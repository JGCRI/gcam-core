# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L132.water_demand_manufacturing
#'
#' Computes manufacturing water withdrawals and consumption by nation/region and historical year
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L132.water_coef_manufacturing_R_W_m3_GJ}. The corresponding file in the
#' original data system was \code{L132.water_demand_manufacturing.R} (water level1).
#' @details Computes manufacturing water withdrawal and consumption coefficients for
#' all regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else group_by inner_join left_join mutate select summarise
#' @importFrom tidyr complete gather
#' @author GPK June 2018
module_water_L132.water_demand_manufacturing <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "water/aquastat_ctry",
             FILE = "water/FAO_industrial_water_AQUASTAT",
             FILE = "water/mfg_water_ratios",
             FILE = "water/mfg_water_mapping",
             FILE = "water/Vassolo_mfg_water",
             "L101.en_bal_EJ_ctry_Si_Fi_Yh_full"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L132.water_km3_ctry_ind_Yh",
             "L132.water_km3_R_ind_Yh"))
  } else if(command == driver.MAKE) {

    year <- value <- GCAM_region_ID <- sector <- fuel <- continent <-
      withdrawals <- consumption <- `water withdrawals` <-
      `water consumption` <- water_type <- coefficient <-
      iso <- energy_EJ <- water_km3 <- Year <- Value <-
      water_km3_aquastat <- scaler <- NULL    # silence package checks.

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    aquastat_ctry <- get_data(all_data, "water/aquastat_ctry")
    FAO_industrial_water_AQUASTAT <- get_data(all_data, "water/FAO_industrial_water_AQUASTAT")
    mfg_water_ratios <- get_data(all_data, "water/mfg_water_ratios")
    mfg_water_mapping <- get_data(all_data, "water/mfg_water_mapping")
    Vassolo_mfg_water <- get_data(all_data, "water/Vassolo_mfg_water")


    L101.en_bal_EJ_ctry_Si_Fi_Yh_full <- get_data(all_data, "L101.en_bal_EJ_ctry_Si_Fi_Yh_full")

    # ===================================================

    # The goal is estimating nation- and region-level manufacturing water withdrawals and consumption by historical year
    # Starting data is continent-scale withdrawals and consumption from the Vassolo and Doll inventory whose base
    # year is 1995. This is downscaled to country and re-aggregated to GCAM region in that year, in order to compute
    # coefficients linking industrial energy to water. Industrial energy is then used to expand the single year to all
    # historical years, with an exogenous cap on withdrawals that is based on FAO Aquastat.

    # The sequence below returns the denominator (energy consumption) by continent for the appropriate year.
    # First, filter IEA country-level energy balance data to the appropriate sectors, fuels, and years.
    # Water demands are assumed to scale with exogenously selected fuel(s)
    L132.in_EJ_cntnt_ind_1995 <- subset(L101.en_bal_EJ_ctry_Si_Fi_Yh_full,
                                        sector %in% water.GCAM_MFG_SECTORS_VASSOLO &
                                          fuel %in% water.GCAM_MFG_FUELS_EFW &
                                          year == 1995) %>%
      group_by(iso) %>%
      summarise(energy_EJ = sum(value)) %>%
      ungroup() %>%
      left_join_error_no_match(mfg_water_mapping, by = "iso") %>%
      group_by(continent) %>%
      summarise(energy_EJ = sum(energy_EJ)) %>%
      ungroup()

    # Computing water-energy coefs from continent-level mfg energy consumption, and mfg water withdrawals and consumption
    # Use inner join to drop the global total from the V&D data, which is not necessary for the calcs
    L132.waterIO_m3GJ_cntnt_ind_1995 <- gather(Vassolo_mfg_water, water_type, water_km3, -continent) %>%
      mutate(water_type = if_else(water_type == "withdrawals", "water withdrawals", "water consumption")) %>%
      inner_join(L132.in_EJ_cntnt_ind_1995, by = "continent") %>%
      mutate(coefficient = water_km3 * CONV_MILLION_M3_KM3 * mfg_water_ratios$`self-to-total-ratio` / energy_EJ) %>%
      select(continent, water_type, coefficient)

    # Applying water demand coefs to historical mfg energy consumption to get mfg water demands by country
    L132.water_km3_ctry_ind_Yh <- subset(L101.en_bal_EJ_ctry_Si_Fi_Yh_full,
                                         sector %in% water.GCAM_MFG_SECTORS_VASSOLO &
                                           fuel %in% water.GCAM_MFG_FUELS_EFW) %>%
      rename(energy_EJ = value) %>%
      left_join_error_no_match(mfg_water_mapping, by = "iso") %>%
      repeat_add_columns(tibble(water_type = c("water withdrawals", "water consumption"))) %>%
      left_join_error_no_match(L132.waterIO_m3GJ_cntnt_ind_1995, by = c("continent", "water_type")) %>%
      mutate(water_km3 = energy_EJ * coefficient) %>%
      group_by(iso, year, water_type) %>%
      summarise(water_km3 = sum(water_km3)) %>%
      ungroup()

    # Capping water demands at an exogenous portion of the FAO estimates of total industrial withdrawals The above
    # method holds the ratio between manufacturing energy and water constant; it assumes that the coef in 1995 works for
    # all other years. However in China (and possibly others) this returns a bottom-up estimate of mfg water withdrawals
    # that exceeds FAO aquastat's total industrial withdrawals. The bottom-up estimate is therefore capped at a portion
    # of the FAO estimate, where available. The table below computes the scalers necessary to keep the manufacturing
    # sector water withdrawals less than this theshold. We use inner_join in order to only compute scalers for countries
    # present in both datasets (i.e., present in FAO Aquastat industrial water, and having > 0 industrial energy in the
    # IEA energy balances). The first year's scaler is set manually for countries with only one observation, as approx()
    # fails w/only 1 obs. Using rule = 2 (fixed extrapolation) because the aquastat years are only a small subset of all
    # historical years.
    L132.scalers_ctry_ind_Yh <-
      left_join_error_no_match(FAO_industrial_water_AQUASTAT, aquastat_ctry[c("aquastat_ctry", "iso")],
                               by = c(Area = "aquastat_ctry")) %>%
      rename(year = Year) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      group_by(iso, year) %>%
      summarise(water_km3_aquastat = sum(Value)) %>%
      ungroup() %>%
      inner_join(subset(L132.water_km3_ctry_ind_Yh, water_type == "water withdrawals"), by = c("iso", "year")) %>%
      mutate(scaler = pmin(water_km3_aquastat * water.MAX_MFG_FRAC_OF_IND / water_km3, 1)) %>%
      select(iso, year, scaler) %>%
      complete(iso = unique(iso), year = HISTORICAL_YEARS) %>%
      group_by(iso) %>%
      mutate(scaler = if_else(year == min(HISTORICAL_YEARS) & is.na(scaler),
                              scaler[year == min(year[!is.na(scaler)])],
                              scaler),
             scaler = approx_fun(year, scaler, rule = 2))

    # Apply the scalers to the country-level estimates of mfg water withdrawals and consumption
    # Not all regions with industrial electricity are in aquastat; just set their scalers to 1 (i.e., keep them)
    L132.water_km3_ctry_ind_Yh <-
      left_join(L132.water_km3_ctry_ind_Yh, L132.scalers_ctry_ind_Yh,
                by = c("iso", "year")) %>%
      mutate(scaler = if_else(is.na(scaler), 1, scaler),
             water_km3 = water_km3 * scaler) %>%
      select(-scaler)

    # Aggregating manufacturing water flow volumes to GCAM regions
    L132.water_km3_R_ind_Yh <-
      left_join_error_no_match(L132.water_km3_ctry_ind_Yh, iso_GCAM_regID[c("iso", "GCAM_region_ID")],
                               by = "iso") %>%
      group_by(GCAM_region_ID, year, water_type) %>%
      summarise(water_km3 = sum(water_km3)) %>%
      ungroup()

    # ===================================================

    L132.water_km3_ctry_ind_Yh %>%
      add_title("Manufacturing water withdrawals by nation and water type") %>%
      add_units("km^3/yr") %>%
      add_comments("Uses continental industrial energy/water ratios from 1995") %>%
      add_comments("to determine water withdrawal and consumption coefficients") %>%
      add_comments("Estimated withdrawal volumes are capped by Aquastat data") %>%
      add_legacy_name("L132.water_km3_ctry_ind_Yh") %>%
      add_precursors("water/aquastat_ctry",
                     "water/FAO_industrial_water_AQUASTAT",
                     "water/mfg_water_ratios",
                     "water/mfg_water_mapping",
                     "water/Vassolo_mfg_water",
                     "L101.en_bal_EJ_ctry_Si_Fi_Yh_full") ->
      L132.water_km3_ctry_ind_Yh

    L132.water_km3_R_ind_Yh %>%
      add_title("Manufacturing water withdrawals by GCAM region and water type") %>%
      add_units("km^3/yr") %>%
      add_comments("Uses continental industrial energy/water ratios from 1995") %>%
      add_comments("to determine water withdrawal and consumption coefficients") %>%
      add_comments("Estimated withdrawal volumes are capped by Aquastat data") %>%
      add_legacy_name("L132.water_km3_R_ind_Yh") %>%
      add_precursors("common/iso_GCAM_regID",
                     "water/aquastat_ctry",
                     "water/FAO_industrial_water_AQUASTAT",
                     "water/mfg_water_ratios",
                     "water/mfg_water_mapping",
                     "water/Vassolo_mfg_water",
                     "L101.en_bal_EJ_ctry_Si_Fi_Yh_full") ->
      L132.water_km3_R_ind_Yh

    return_data(L132.water_km3_ctry_ind_Yh, L132.water_km3_R_ind_Yh)
  } else {
    stop("Unknown command")
  }
}
