#' module_energy_LA154.transportation_UCD
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L154.in_EJ_R_trn_m_sz_tech_F_Yh}, \code{L154.in_EJ_ctry_trn_m_sz_tech_F}, \code{L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y}, \code{L154.loadfactor_R_trn_m_sz_tech_F_Y}, \code{L154.cost_usdvkm_R_trn_m_sz_tech_F_Y}, \code{L154.speed_kmhr_R_trn_m_sz_tech_F_Y}, \code{L154.out_mpkm_R_trn_nonmotor_Yh}. The corresponding file in the
#' original data system was \code{LA154.transportation_UCD.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RH May 2017
#' @export
module_energy_LA154.transportation_UCD <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/mappings/calibrated_techs_trn_agg", #needs source
             FILE = "energy/enduse_fuel_aggregation",
             FILE = "energy/mappings/UCD_ctry", #needs source
             FILE = "energy/mappings/UCD_techs", #needs source
             # This file is currently using a constant to select the correct SSP database
             # All SSP databases will be included in the input files
             # needs source, better description
             FILE = paste0("energy/UCD_trn_data_",energy.TRN_SSP),
             FILE = "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh",
             FILE = "temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
             FILE = "L131.in_EJ_R_Senduse_F_Yh",
             FILE = "L100.Pop_thous_ctry_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L154.in_EJ_R_trn_m_sz_tech_F_Yh",
             "L154.in_EJ_ctry_trn_m_sz_tech_F",
             "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
             "L154.loadfactor_R_trn_m_sz_tech_F_Y",
             "L154.cost_usdvkm_R_trn_m_sz_tech_F_Y",
             "L154.speed_kmhr_R_trn_m_sz_tech_F_Y",
             "L154.out_mpkm_R_trn_nonmotor_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    calibrated_techs_trn_agg <- get_data(all_data, "energy/mappings/calibrated_techs_trn_agg")
    enduse_fuel_aggregation <- get_data(all_data, "energy/enduse_fuel_aggregation")
    UCD_ctry <- get_data(all_data, "energy/mappings/UCD_ctry")
    UCD_techs <- get_data(all_data, "energy/mappings/UCD_techs")
    UCD_trn_data <- get_data(all_data,  paste0("energy/UCD_trn_data_",energy.TRN_SSP)) %>%
      # Might want to gather in a different way
      gather(year, value, `2005`:`2095`) %>%
      mutate(year = as.integer(year))
    L101.in_EJ_ctry_trn_Fi_Yh <- get_data(all_data, "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh") %>%
      # temp-data-inject code
      gather(year, value, starts_with("X")) %>%
      mutate(year = as.integer(substr(year,2,5)))
    L1011.in_EJ_ctry_intlship_TOT_Yh <- get_data(all_data, "temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh") %>%
      # temp-data-inject code
      gather(year, value, starts_with("X")) %>%
      mutate(year = as.integer(substr(year,2,5)))
    L131.in_EJ_R_Senduse_F_Yh <- get_data(all_data, "L131.in_EJ_R_Senduse_F_Yh")
    L100.Pop_thous_ctry_Yh <- get_data(all_data, "L100.Pop_thous_ctry_Yh")
    # ===================================================
    # Part 1: downscaling country-level transportation energy data to UCD transportation technologies
    # NOTE: We are currently aggregating IEA's data on rail and road due to inconsistencies (e.g. no rail in the Middle East)
    %>% filter(iso == "usa", year == 1986, fuel == "refined liquids")
    # First, replace the international shipping data (swapping in EIA for IEA)
    # Only perform this swap for international shipping / refined liquids, and in countries in the EIA database
    L154.in_EJ_ctry_trn_Fi_Yh <- L101.in_EJ_ctry_trn_Fi_Yh %>%
      # left_join used here because we only want to replace certain values
      left_join(L1011.in_EJ_ctry_intlship_TOT_Yh %>% rename(EIA_value = value), by = c("iso","year")) %>%
      mutate(value = if_else(sector == "in_trn_international ship" &
                               fuel == "refined liquids" &
                               !is.na(EIA_value), EIA_value, value),
             sector = sub("in_", "", sector)) %>%
      select(iso, sector, fuel, year, value) %>%
      # Get rid of multiple territories with same country name
      distinct()

    # Need to map sector to UCD_category, calibrated_techs_trn_agg data is too busy
    UCD_category_mapping <- calibrated_techs_trn_agg %>% select(sector, UCD_category) %>% distinct

    # Aggregate to UCD_category in each country/year instead of sector
    L154.in_EJ_ctry_trn_Fi_Yh <- L154.in_EJ_ctry_trn_Fi_Yh %>%
      left_join_error_no_match(UCD_category_mapping, by = "sector") %>%
      group_by(iso, UCD_category, fuel, year) %>%
      summarise(value = sum(value))

    # Aggregating UCD transportation database by the general categories used for the IEA transportation data
    # These will be used to compute shares for allocation of energy to mode/technology/fuel within category/fuel
    L154.in_PJ_Rucd_trn_m_sz_tech_F<- UCD_trn_data %>%
      filter(variable == "energy") %>%
      left_join_error_no_match(UCD_techs, by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel"))

    L154.in_PJ_Rucd_trn_Cat_F <- L154.in_PJ_Rucd_trn_m_sz_tech_F %>%
      # Filtering only to base year for computing shares
      filter(year == energy.UCD_EN_YEAR) %>%
      group_by(UCD_region, UCD_category, fuel) %>%
      summarise(agg = sum(value))

    # Match these energy quantities back into the complete table for computation of shares
    L154.in_PJ_Rucd_trn_m_sz_tech_F <- L154.in_PJ_Rucd_trn_m_sz_tech_F %>%
      filter(year == energy.UCD_EN_YEAR) %>%
      left_join_error_no_match(L154.in_PJ_Rucd_trn_Cat_F, by = c("UCD_region", "UCD_category", "fuel")) %>%
      # If the aggregate is 0 or value is NA, set share to 0, rather than NA
      mutate(UCD_share = if_else(agg == 0 | is.na(value), 0, value/agg))

    # Writing out the UC Davis mode/technology/fuel shares within category/fuel at the country level
    # First, creating a table of desired countries with their UCD regions
    ctry_iso_region <- tibble::tibble(iso = unique(L101.in_EJ_ctry_trn_Fi_Yh$iso)) %>%
      left_join_error_no_match(UCD_ctry, by = "iso")

    L154.share_ctry_trn_m_sz_tech_F <- L154.in_PJ_Rucd_trn_m_sz_tech_F %>%
      repeat_add_columns(ctry_iso_region) %>%
      filter(UCD_region.x == UCD_region.y) %>%
      select(iso, UCD_sector, mode, size.class, UCD_technology,
             UCD_fuel, UCD_category, fuel, UCD_share)

    # Multiplying historical energy by country/category/fuel by shares of country/mode/tech/fuel within country/category/fuel
    # Need a value for each iso, year, UCD category, and fuel combo, even if not currently in L154.in_EJ_ctry_trn_Fi_Yh
     UCD_cat_fuel <- L154.share_ctry_trn_m_sz_tech_F %>%
       select(UCD_category, fuel) %>%
       distinct
     iso_year <- L154.in_EJ_ctry_trn_Fi_Yh %>%
       ungroup %>%
       select(iso, year) %>%
       distinct

     L154.in_EJ_ctry_trn_m_sz_tech_F <- UCD_cat_fuel %>%
       repeat_add_columns(iso_year) %>%
       left_join(L154.share_ctry_trn_m_sz_tech_F, by = c("UCD_category", "fuel", "iso")) %>%
       left_join(L154.in_EJ_ctry_trn_Fi_Yh, by = c("UCD_category", "fuel", "iso", "year")) %>%
       left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
       # Multiply value by share. Set missing values to 0. These are combinations not available in the data from IEA.
       mutate(value = if_else(is.na(value), 0, value*UCD_share)) %>%
       select(iso, UCD_sector, mode, size.class, UCD_technology,
              UCD_fuel, UCD_category, fuel, GCAM_region_ID, year, value)

     # Aggregating by GCAM region
     L154.in_EJ_R_trn_m_sz_tech_F <- L154.in_EJ_ctry_trn_m_sz_tech_F %>%
       group_by(GCAM_region_ID, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, fuel, year) %>%
       summarise(value = sum(value))

     # Aggregating by fuel to calculate scalers
     L154.in_EJ_R_trn_F_Yh_unscaled <- L154.in_EJ_R_trn_m_sz_tech_F %>%
       group_by(GCAM_region_ID, fuel, year) %>%
       summarise(unscaled_value = sum(value))

     L154.in_EJ_R_Strn_F_Yh <- L131.in_EJ_R_Senduse_F_Yh %>%
       filter(grepl("trn",sector))

     # Need to use the "aggregate" fuels
     L154.in_EJ_R_Strn_F_Yh <- L154.in_EJ_R_Strn_F_Yh %>%
       left_join_error_no_match(enduse_fuel_aggregation %>% select(fuel, trn), by = c("fuel")) %>%
       select(-fuel, fuel = trn)

     L154.in_EJ_R_trn_F_Yh <- L154.in_EJ_R_Strn_F_Yh %>%
       group_by(GCAM_region_ID, fuel, year) %>%
       summarise(value = sum(value))

     L154.scalers_R_trn_F_Yh <- L154.in_EJ_R_trn_F_Yh_unscaled %>%
       # Keep NAs and then set to zero
       left_join(L154.in_EJ_R_trn_F_Yh, by = c("GCAM_region_ID", "fuel", "year")) %>%
       mutate(scaled_value = value/unscaled_value) %>%
       replace_na(list(scaled_value = 0)) %>%
       select(-unscaled_value, -value)

     # Multiplying scalers by original estimates
     L154.in_EJ_R_trn_m_sz_tech_F_Yh <- L154.in_EJ_R_trn_m_sz_tech_F %>%
       left_join(L154.scalers_R_trn_F_Yh, by = c("GCAM_region_ID", "fuel", "year")) %>%
       mutate(value = value * scaled_value) %>%
       # Energy is being dropped due to zeroes in the UCD database. Might want to add new techs to the UC Davis database
       replace_na(list(value = 0)) %>%
       select(-scaled_value)

     # Part 2: Downscaling of parameters in the UCD database to the country level
     # 2a: Merging of non-fuel costs to assign each technology with a single cost per vkm
     # Exogenous fixed charge rate to convert $/veh to $/veh/yr
     fcr_veh <- energy.DISCOUNT_RATE_VEH +
       energy.DISCOUNT_RATE_VEH / ( ( ( 1 + energy.DISCOUNT_RATE_VEH ) ^ energy.NPER_AMORT_VEH ) - 1 )

     L154.UCD_trn_vkm_veh <- UCD_trn_data %>%
       filter(variable == "annual travel per vehicle") %>%
       # Dropping UCD technology and UCD fuel because they are "All"
       select(UCD_region, UCD_sector, mode, size.class, year, vkt_veh_yr = value)

     L154.UCD_trn_cost_data <- UCD_trn_data %>%
       filter(grepl("\\$", unit)) %>%
       # Next, match in the number of km per vehicle per year in order to calculate a levelized cost (per vkm)
       mutate(value = if_else(unit == "2005$/veh", value*fcr_veh, value),
              unit = if_else(unit == "2005$/veh", "2005$/veh/yr", unit)) %>%
       left_join(L154.UCD_trn_vkm_veh, by = c("UCD_region", "UCD_sector", "mode", "size.class", "year")) %>%
       mutate(value = if_else(unit == "2005$/veh/yr", value/vkt_veh_yr, value),
              unit = if_else(unit == "2005$/veh/yr", "2005$/vkt", unit)) %>%
       group_by(UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, unit, year) %>%
       summarise(value = sum(value)) %>%
       mutate(variable = "non-fuel costs")

     # Creating tibble with all GCAM years to join with. The values will be filled out using the first available year.
     # Remove years in all GCAM years that are already in UCD database

     all_years <- tibble( year = c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
       filter(!(year %in% unique(UCD_trn_data$year)))


     L154.UCD_trn_data_allyears <- bind_rows(filter( UCD_trn_data, variable %in% c( "intensity", "load factor", "speed" ) ),
                                         L154.UCD_trn_cost_data) %>%
       select(-year, -value) %>%
       distinct() %>%
       repeat_add_columns(all_years) %>%
       mutate(value = NA)

     L154.UCD_trn_data <- bind_rows(filter( UCD_trn_data, variable %in% c( "intensity", "load factor", "speed" ) ),
               L154.UCD_trn_cost_data) %>%
       bind_rows(L154.UCD_trn_data_allyears) %>%
       # Fill out all missing values with the nearest available year that is not missing
       group_by(UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable, unit) %>%
       arrange(UCD_region, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, variable, unit, year) %>%
       mutate(value = if_else(is.na(value), approx_fun(year, value, rule = 2), value))

     #Split into different tables, one per variable
     # L154.UCD_trn_intensity_Y <- subset( L154.UCD_trn_data_Y.melt, variable == "intensity" )
     # L154.UCD_trn_load_Y <- subset( L154.UCD_trn_data_Y.melt, variable == "load factor" )
     # L154.UCD_trn_speed_Y <- subset( L154.UCD_trn_data_Y.melt, variable == "speed" )
     # L154.UCD_trn_nonfuel_Y <- subset( L154.UCD_trn_data_Y.melt, variable == "non-fuel costs" )

     #Aggregate the country-level energy consumption by sector and mode. First need to add in the future years for matching purposes
     L154.in_EJ_ctry_trn_m_sz_tech_F_future_years <- L154.in_EJ_ctry_trn_m_sz_tech_F %>%
       select(-year, -value) %>%
       distinct() %>%
       repeat_add_columns(tibble(year = FUTURE_YEARS, value = NA))

     L154.in_EJ_ctry_trn_m_sz_tech_F_Y <- L154.in_EJ_ctry_trn_m_sz_tech_F %>%
       bind_rows(L154.in_EJ_ctry_trn_m_sz_tech_F_future_years) %>%
       mutate(value = if_else(is.na(value), approx_fun(year, value, rule = 2), value))
     L154.in_EJ_ctry_trn_m_Y <- L154.in_EJ_ctry_trn_m_sz_tech_F_Y %>%
       group_by(iso, UCD_sector, mode, year) %>%
       summarise(value = sum(value))

     # Spreading by variable to join all at once
     L154.UCD_trn_data_variable_spread <- L154.UCD_trn_data %>%
       ungroup %>%
       distinct() %>%
       select(-unit) %>%
       spread(variable, value)

     L154.ALL_ctry_trn_m_sz_tech_F_Y <- L154.in_EJ_ctry_trn_m_sz_tech_F_Y %>%
       left_join_error_no_match(UCD_ctry, by = "iso") %>%
       # The energy weights will be replaced by the energy weights of each mode, as many techs have 0 consumption in the base year
       select(-value) %>%
       left_join_error_no_match(L154.in_EJ_ctry_trn_m_Y, by = c("iso", "UCD_sector", "mode", "year")) %>%
       # Using a floor on the weighting factor to avoid having zero weights for any countries
       mutate(weight_EJ = max(value, energy.MIN_WEIGHT_EJ)) %>%
       # Next, match in the derived variables, specific to each individual country/sector/mode/size.class/tech/fuel, except speed
       # There will be NA non-fuel costs which can be set to zero
       left_join(L154.UCD_trn_data_variable_spread %>% select(-speed),
                 by = c("UCD_sector", "mode", "size.class", "UCD_technology", "UCD_fuel", "year", "UCD_region")) %>%
       replace_na(list(`non-fuel costs` = 0))

     # Adding in speed - this is matched by the mode and (for some) size class. Match size class first
     speed_data <- L154.UCD_trn_data_variable_spread %>%
       select(UCD_sector, mode, size.class, year, UCD_region, speed) %>%
       filter(!is.na(speed))

     L154.ALL_ctry_trn_m_sz_tech_F_Y <- L154.ALL_ctry_trn_m_sz_tech_F_Y %>%
       left_join(speed_data,
                 by = c("UCD_sector", "mode", "size.class", "year", "UCD_region")) %>%
       # For the missing values, join using the mode ID
       left_join_keep_first_only(speed_data,
                 by = c("UCD_sector", "mode", "year", "UCD_region")) %>%
       mutate(speed.x = if_else(is.na(speed.x), speed.y, speed.x)) %>%
       replace_na(list(speed.x = 1))

     # Calculate the weighted volumes. No need to convert units because these will be aggregated and divided by weights
     L154.ALL_R_trn_m_sz_tech_F_Y <- L154.ALL_ctry_trn_m_sz_tech_F_Y %>%
       mutate(Tvkm = weight_EJ / intensity) %>%
       mutate(Tpkm = Tvkm * `load factor`) %>%
       mutate(Tusd = Tvkm * `non-fuel costs`) %>%
       mutate(Thr = Tvkm / speed.x ) %>%
       group_by(UCD_technology,UCD_fuel, UCD_sector, mode, size.class.x, year, GCAM_region_ID) %>%
       summarise(weight_EJ = sum(weight_EJ), Tvkm = sum(Tvkm), Tpkm = sum(Tpkm),Tusd = sum(Tusd), Thr = sum(Thr))

     # Reverse the calculations to calculate the weighted average of each derived variable
     L154.ALL_R_trn_m_sz_tech_F_Y <- L154.ALL_R_trn_m_sz_tech_F_Y %>%
       mutate(intensity_MJvkm = weight_EJ / Tvkm) %>%
       mutate(loadfactor = Tpkm / Tvkm) %>%
       mutate(cost_usdvkm = Tusd / Tvkm) %>%
       mutate(speed_kmhr = Tvkm / Thr ) %>%
       gather(variable, value, 8:16) %>%
       select(GCAM_region_ID, UCD_sector, mode, size.class = size.class.x, UCD_technology, UCD_fuel, variable, year, value)

     # Build the final data frames by variable
     out_var_df <- split(L154.ALL_R_trn_m_sz_tech_F_Y, L154.ALL_R_trn_m_sz_tech_F_Y$variable) %>%
       lapply(function(df) {select(df, -variable)})

     # Part 3: Downscaling of non-motorized transport to the country level, using population
     # why only using 2005 data?
     L154.Pop_thous_Rucd <- L100.Pop_thous_ctry_Yh %>%
       filter(year == energy.UCD_EN_YEAR) %>%
       left_join_error_no_match(UCD_ctry, by = "iso") %>%
       group_by(UCD_region) %>%
       summarise(population = sum(value))

     L154.out_bpkm_Rucd_trn_nonmotor <- UCD_trn_data %>%
       filter(mode %in% c( "Walk", "Cycle" ), variable == "service output") %>%
       left_join_error_no_match(L154.Pop_thous_Rucd, by = "UCD_region") %>%
       mutate(pkm_percap = value * CONV_BIL_MIL * CONV_MIL_THOUS / population) %>%
       select(UCD_region, mode, year, pkm_percap)

     # Compute the nonmotorized service output at the country level, using the historical population
     L154.out_mpkm_ctry_trn_nonmotor <- L100.Pop_thous_ctry_Yh %>%
       repeat_add_columns(tibble(mode = c( "Walk", "Cycle" ))) %>%
       left_join_error_no_match(UCD_ctry %>% select(-country_name), by = "iso")  %>%
       left_join_error_no_match(L154.out_bpkm_Rucd_trn_nonmotor %>% filter(year == energy.UCD_EN_YEAR),
                                by = c("UCD_region", "mode")) %>%
       mutate(value = value * 1/CONV_MIL_THOUS * pkm_percap)

    # Aggregate by GCAM region and write it out
    L154.out_mpkm_R_trn_nonmotor_Yh <- L154.out_mpkm_ctry_trn_nonmotor %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      group_by(GCAM_region_ID, mode, year = year.x) %>%
      summarise(value = sum(value)) %>%
      filter(year %in% HISTORICAL_YEARS)
    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    L154.in_EJ_R_trn_m_sz_tech_F_Yh %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.in_EJ_R_trn_m_sz_tech_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "L131.in_EJ_R_Senduse_F_Yh", "L100.Pop_thous_ctry_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.in_EJ_R_trn_m_sz_tech_F_Yh
    L154.in_EJ_ctry_trn_m_sz_tech_F %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.in_EJ_ctry_trn_m_sz_tech_F") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "L131.in_EJ_R_Senduse_F_Yh", "L100.Pop_thous_ctry_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.in_EJ_ctry_trn_m_sz_tech_F
    out_var_df[["intensity_MJvkm"]] %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "L131.in_EJ_R_Senduse_F_Yh", "L100.Pop_thous_ctry_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y
    out_var_df[["loadfactor"]] %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.loadfactor_R_trn_m_sz_tech_F_Y") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "L131.in_EJ_R_Senduse_F_Yh", "L100.Pop_thous_ctry_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.loadfactor_R_trn_m_sz_tech_F_Y
    out_var_df[["cost_usdvkm"]] %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.cost_usdvkm_R_trn_m_sz_tech_F_Y") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "L131.in_EJ_R_Senduse_F_Yh", "L100.Pop_thous_ctry_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.cost_usdvkm_R_trn_m_sz_tech_F_Y
    out_var_df[["speed_kmhr"]] %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.speed_kmhr_R_trn_m_sz_tech_F_Y") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "L131.in_EJ_R_Senduse_F_Yh", "L100.Pop_thous_ctry_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.speed_kmhr_R_trn_m_sz_tech_F_Y
    L154.out_mpkm_R_trn_nonmotor_Yh %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.out_mpkm_R_trn_nonmotor_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "L131.in_EJ_R_Senduse_F_Yh", "L100.Pop_thous_ctry_Yh") %>%
      #typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.out_mpkm_R_trn_nonmotor_Yh

    return_data(L154.in_EJ_R_trn_m_sz_tech_F_Yh, L154.in_EJ_ctry_trn_m_sz_tech_F, L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y, L154.loadfactor_R_trn_m_sz_tech_F_Y, L154.cost_usdvkm_R_trn_m_sz_tech_F_Y, L154.speed_kmhr_R_trn_m_sz_tech_F_Y, L154.out_mpkm_R_trn_nonmotor_Yh)
  } else {
    stop("Unknown command")
  }
}
