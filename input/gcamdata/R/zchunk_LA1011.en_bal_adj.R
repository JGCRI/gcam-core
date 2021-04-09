# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA1011.en_bal_adj
#'
#' Adjustments to the IEA energy balance for shipping fuel consumption, Russia, and natural
#' gas total primary energy supply (TPES; i.e., consumption).
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1011.en_bal_EJ_R_Si_Fi_Yh}, \code{L1011.in_EJ_ctry_intlship_TOT_Yh}. The
#' corresponding file in the original data system was \code{LA1011.en_bal_adj.R} (energy level1).
#' @details This chunk replaces IEA international shipping fuel consumption estimates with EIA estimates,
#' remaps USSR data to Russia, and removes coal-to-gas from natural gas TPES.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange distinct filter if_else group_by inner_join left_join mutate select summarise
#' @importFrom tidyr complete nesting replace_na
#' @author JDH July 2017
module_energy_LA1011.en_bal_adj <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/calibrated_techs",
             FILE = "energy/EIA_RFO_intlship_kbbld",
             FILE = "energy/EIA_TOT_intlship_kbbld",
             FILE = "energy/mappings/EIA_ctry",
             FILE = "energy/A22.globaltech_coef",
             "L101.en_bal_EJ_R_Si_Fi_Yh_full"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1011.en_bal_EJ_R_Si_Fi_Yh",
             "L1011.in_EJ_ctry_intlship_TOT_Yh"))
  } else if(command == driver.MAKE) {

    # Silence package check
    technology <- minicam.energy.input <- sector <-
      fuel <- supplysector <- subsector <- GCAM_region_ID <- iso <- value.x <-
      year <- value <- Country <- value_TOT <- value_diff <- value_RFO <- value.y <- NULL


    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    EIA_RFO_intlship_kbbld <- get_data(all_data, "energy/EIA_RFO_intlship_kbbld")
    EIA_TOT_intlship_kbbld <- get_data(all_data, "energy/EIA_TOT_intlship_kbbld")
    EIA_ctry <- get_data(all_data, "energy/mappings/EIA_ctry")
    A22.globaltech_coef <- get_data(all_data, "energy/A22.globaltech_coef")
    L101.en_bal_EJ_R_Si_Fi_Yh_full <- get_data(all_data, "L101.en_bal_EJ_R_Si_Fi_Yh_full", strip_attributes = TRUE)

    EIA_RFO_intlship_kbbld %>%
      gather_years -> EIA_RFO_intlship_kbbld

    EIA_TOT_intlship_kbbld %>%
      gather_years -> EIA_TOT_intlship_kbbld

    # ===================================================

    # MODIFICATIONS TO IEA ENERGY BALANCES
    # Replacing IEA estimates of international shipping fuel consumption with EIA estimates
    # (former is known by emissions modeling community as being too low)
    # First, convert available data to EJ per year of total refined liquid products
    EIA_RFO_intlship_kbbld %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      distinct(year) -> EIA_years

    EIA_RFO_intlship_kbbld %>%
      inner_join(EIA_years, by = "year") %>% # inner joining to only keep years from EIA_years
      # Convert from KBBL/day to EJ/yr
      mutate(value = value * CONV_KBBL_BBL * CONV_BBL_TONNE_RFO *
               CONV_TONNE_GJ_RFO * CONV_GJ_EJ / CONV_DAYS_YEAR) -> L1011.in_EJ_ctry_intlship_RFO_Yh

    EIA_RFO_intlship_kbbld %>%
      left_join(EIA_TOT_intlship_kbbld %>%
                  rename(value_TOT = value), by = c("year", "Country")) %>%
      inner_join(EIA_years, by = "year") %>% # inner joining to only keep years from EIA_years
      # Subtract residual fuel oil from total for distillate
      mutate(value_diff = value_TOT - value) %>%
      select(Country, year, value_diff) %>%
      # Convert from KBBL/day to EJ/yr
      mutate(value = value_diff * CONV_KBBL_BBL * CONV_BBL_TONNE_DISTILLATE *
               CONV_TONNE_GJ_RFO * CONV_GJ_EJ / CONV_DAYS_YEAR) %>%
      select(Country, year, value) -> L1011.in_EJ_ctry_intlship_distillate_Yh

    L1011.in_EJ_ctry_intlship_distillate_Yh %>%
      left_join(L1011.in_EJ_ctry_intlship_RFO_Yh %>%
                  rename(value_RFO = value), by = c("year", "Country")) %>%
      # Combine new estimates for RFO and distillate into total refined liquids
      mutate(value = value + value_RFO) %>%
      select(Country, year, value) -> L1011.in_EJ_ctry_intlship_TOT_Yh

    # Several countries blink in and out of the time series.
    # After 2005, Russia's shipping energy use drops off (mostly from RFO);
    # holding constant at 2005 value because we know this is not acurate, and
    # Russia is a large percent of global shipping fuel
    L1011.in_EJ_ctry_intlship_TOT_Yh %>%
      replace_na(list(value=0)) -> L1011.in_EJ_ctry_intlship_TOT_Yh

    L1011.in_EJ_ctry_intlship_TOT_Yh %>%
      filter(Country == "Russia" & year %in% 2005) %>%
      select(value) -> Russia_2005

    L1011.in_EJ_ctry_intlship_TOT_Yh %>%
      mutate(value = replace(value, Country == "Russia" & year %in% 2006:2010,
                             Russia_2005$value)) -> L1011.in_EJ_ctry_intlship_TOT_Yh

    # Russia's energy use prior to 1991 is assigned to Former Soviet Union; need to re-map this to Russia
    # for country-level information being written out
    L1011.in_EJ_ctry_intlship_TOT_Yh %>%
      filter(!(Country == "Russia" & year %in% 1986:1991)) %>%
      mutate(Country = replace(Country, Country == "Former U.S.S.R." & year %in% 1986:1991,
                               "Russia")) -> L1011.in_EJ_ctry_intlship_TOT_Yh

    # Match in the countries and aggregate by region
    L1011.in_EJ_ctry_intlship_TOT_Yh %>%
      left_join(EIA_ctry %>%
                  rename(Country = EIA_ctry), by = "Country") %>%
      left_join(iso_GCAM_regID, by = "iso") %>%
      filter(!(is.na(GCAM_region_ID))) %>%
      select(Country, year, value, iso, GCAM_region_ID) -> L1011.in_EJ_ctry_intlship_TOT_Yh

    # JS 12/2020: Extrapolate EIA data to 2015
    L1011.in_EJ_ctry_intlship_TOT_Yh_2015<-L1011.in_EJ_ctry_intlship_TOT_Yh %>%
      filter(year==2014) %>%
      mutate(year=2015)

    # JS 12/2020: Add 2015 to the dataset and group by iso
    L1011.in_EJ_ctry_intlship_TOT_Yh<-L1011.in_EJ_ctry_intlship_TOT_Yh %>%
      bind_rows(L1011.in_EJ_ctry_intlship_TOT_Yh_2015) %>%
      group_by(year,iso,GCAM_region_ID) %>%
      summarise(value=sum(value)) %>%
      ungroup() %>%
      arrange(iso)

    L1011.in_EJ_ctry_intlship_TOT_Yh %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value)) -> L1011.in_EJ_R_intlship_Yh

    # Filter for historical years
    L101.en_bal_EJ_R_Si_Fi_Yh_full %>%
      filter(year %in% HISTORICAL_YEARS) -> L101.en_bal_EJ_R_Si_Fi_Yh_full

    # Replace the data in the IEA energy balances table
    L101.en_bal_EJ_R_Si_Fi_Yh_full %>%
      left_join(L1011.in_EJ_R_intlship_Yh %>%
                  mutate(sector = "in_trn_international ship",
                         fuel = "refined liquids"),
                by = c("sector", "fuel", "year", "GCAM_region_ID")) %>%
      mutate(value.x = if_else(is.na(value.y), value.x, value.y)) %>%
      select(GCAM_region_ID, sector, fuel, year, value = value.x) -> L1011.en_bal_EJ_R_Si_Fi_Yh

    # Re-calculate TPES after all adjustments are made
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(grepl("(in_|net_)", sector)) %>%
      mutate(sector = "TPES") %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) -> L1011.in_EJ_R_Si_Fi_Yh

    # Replace the TPES data in the IEA energy balances table
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      left_join(L1011.in_EJ_R_Si_Fi_Yh, by = c("sector", "fuel", "year", "GCAM_region_ID")) %>%
      mutate(value.x = if_else(is.na(value.y), value.x, value.y)) %>%
      rename(value = value.x) %>%
      select(GCAM_region_ID, sector, fuel, year, value) -> L1011.en_bal_EJ_R_Si_Fi_Yh

    # This is complicated. Because the output of "gas works gas" that is produced from coal is not
    # distinguished from that produced by other fuels, the process is modeled based on the fuel
    # inputs, and the output fuel is assigned the same name as natural gas. As a result the estimates
    # of TPES at this point for natural gas include both natural gas and gasified coal. This
    # subtraction generally follows the method used in code file L122.

    # Heat production from district heat sector
    A22.globaltech_coef %>%
      gather_years %>%
      # Adding empty historical years to fill in with interpolation
      complete(year = unique(c(HISTORICAL_YEARS, year)),
               nesting(supplysector, subsector, technology, minicam.energy.input)) %>%
      arrange(year) %>%
      group_by(technology, subsector, supplysector, minicam.energy.input) %>%
      # Interpolate to fill in missing globaltech_coef historical years
      mutate(value = approx_fun(year, value)) %>%
      left_join(distinct(calibrated_techs), by = c("supplysector", "subsector", "technology", "minicam.energy.input")) %>%
      select(supplysector, subsector, technology, minicam.energy.input, year, value, sector, fuel) %>%
      filter(supplysector == "gas processing") -> L1011.gasproc_coef

    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "in_gas processing" & fuel == "coal") %>%
      mutate(sector.x = sub("in_", "", sector)) %>%
      left_join(L1011.gasproc_coef %>%
                  rename(sector.x = sector), by = c("sector.x", "fuel", "year")) %>%
      mutate(value = value.x / value.y) %>%
      select(GCAM_region_ID, sector, fuel, year, value) %>%
      # Changing the fuel to gas, sector to TPES so we can easily join it with gas in the next step
      mutate(fuel = "gas",
             sector = "TPES") -> L1011.out_EJ_R_gasproc_coal_Yh

    # Subtract gasified coal from natural gas TPES
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      left_join(L1011.out_EJ_R_gasproc_coal_Yh, by = c("GCAM_region_ID", "sector", "fuel", "year"))%>%
      mutate(value.x = if_else(is.na(value.y), value.x, value.x - value.y)) %>%
      rename(value = value.x) %>%
      select(GCAM_region_ID, sector, fuel, year, value) -> L1011.en_bal_EJ_R_Si_Fi_Yh

    # This is also complicated. In regions with very low natural gas use and high coal-to-gas with very
    # high input-output coefs on coal-to-gas production (South Africa), dividing the coal input by the
    # IO coef may cause gas production in excess of the demands in the region. If this is the case,
    # need to return to original energy balance data and reduce the coal input to gas works (can
    # re-allocate to another sector if desired)

    if(nrow(filter(L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "TPES", fuel == "gas", value < 0)) > 0) {
      stop("Exogenous IO coef on coal input to gas works caused an increase in natural gas beyond the regional TPES of gas")
    }


    # ===================================================

    # Produce outputs

    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      add_title("Energy balances by GCAM region / intermediate sector / intermediate fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Replacing international shipping estimates on IEA energy balances with EIA, ") %>%
      add_comments("subtract coal-to-gas from natural gas TPES") %>%
      add_legacy_name("L1011.en_bal_EJ_R_Si_Fi_Yh") %>%
      add_precursors("L101.en_bal_EJ_R_Si_Fi_Yh_full", "energy/A22.globaltech_coef", "energy/calibrated_techs") ->
      L1011.en_bal_EJ_R_Si_Fi_Yh

     L1011.in_EJ_ctry_intlship_TOT_Yh %>%
      add_title("Liquid fuel consumption by international shipping by country / historical year") %>%
      add_units("EJ") %>%
      add_comments("EIA international shipping data converted to EJ, aggregated by country, ") %>%
      add_comments("adding USSR data to Russia") %>%
      add_legacy_name("L1011.in_EJ_ctry_intlship_TOT_Yh") %>%
      add_precursors("energy/EIA_RFO_intlship_kbbld", "energy/EIA_TOT_intlship_kbbld", "energy/mappings/EIA_ctry", "common/iso_GCAM_regID") ->
      L1011.in_EJ_ctry_intlship_TOT_Yh

    return_data(L1011.en_bal_EJ_R_Si_Fi_Yh, L1011.in_EJ_ctry_intlship_TOT_Yh)
  } else {
    stop("Unknown command")
  }
}
