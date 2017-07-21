#' module_energy_LA1011.en_bal_adj
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1011.en_bal_EJ_R_Si_Fi_Yh}, \code{L1011.in_EJ_ctry_intlship_TOT_Yh}. The corresponding file in the
#' original data system was \code{LA1011.en_bal_adj.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
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

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    EIA_RFO_intlship_kbbld <- get_data(all_data, "energy/EIA_RFO_intlship_kbbld")
    EIA_TOT_intlship_kbbld <- get_data(all_data, "energy/EIA_TOT_intlship_kbbld")
    EIA_ctry <- get_data(all_data, "energy/mappings/EIA_ctry")
    A22.globaltech_coef <- get_data(all_data, "energy/A22.globaltech_coef")
    L101.en_bal_EJ_R_Si_Fi_Yh_full <- get_data(all_data, "L101.en_bal_EJ_R_Si_Fi_Yh_full")

    EIA_RFO_intlship_kbbld %>%
      gather(year, value, -Country) %>%
      mutate(year = sub("X", "", year)) %>%
      mutate(year = as.integer(year)) -> EIA_RFO_intlship_kbbld

    EIA_TOT_intlship_kbbld %>%
      gather(year, value, -Country) %>%
      mutate(year = sub("X", "", year)) %>%
      mutate(year = as.integer(year)) -> EIA_TOT_intlship_kbbld

    L101.en_bal_EJ_R_Si_Fi_Yh_full %>%
      gather(year, value, -GCAM_region_ID, -sector, -fuel) %>%
      mutate(year = sub("X", "", year)) %>%
      mutate(year = as.integer(year)) -> L101.en_bal_EJ_R_Si_Fi_Yh_full
    # ===================================================

    # MODIFICATIONS TO IEA ENERGY BALANCES
    # Subset only the relevant years and combine OECD with non-OECD
    # Replacing IEA estimates of international shipping fuel consumption with EIA estimates
    # First, convert available data to EJ per year of total refined liquid products
    EIA_RFO_intlship_kbbld %>%
      filter(year %in% historical_years) %>%
      select(year) -> EIA_years

    EIA_RFO_intlship_kbbld %>%
      inner_join(EIA_years, by = "year") %>% #inner joining to only keep years from EIA_years
      mutate(value = value * conv_kbbl_bbl * conv_bbl_tonne_RFO *
               conv_tonne_GJ_RFO * conv_GJ_EJ / conv_days_year ) -> L1011.in_EJ_ctry_intlship_RFO_Yh

    EIA_RFO_intlship_kbbld %>%
      left_join(EIA_TOT_intlship_kbbld %>%
                  mutate(value_TOT = value), by = c("year", "Country")) %>%
      inner_join(EIA_years, by = "year") %>%
      mutate(value_diff = value_TOT - value) %>%
      select(Country, year, value_diff) %>%
      rename(value = value_diff) %>%
      mutate(value = value * conv_kbbl_bbl * conv_bbl_tonne_distillate *
               conv_tonne_GJ_RFO * conv_GJ_EJ / conv_days_year) -> L1011.in_EJ_ctry_intlship_distillate_Yh

    L1011.in_EJ_ctry_intlship_distillate_Yh %>%
      left_join(L1011.in_EJ_ctry_intlship_RFO_Yh %>%
                  rename(value_RFO = value), by = c("year", "Country")) %>%
      mutate(value = value + value_RFO) %>%
      select(Country, year, value) -> L1011.in_EJ_ctry_intlship_TOT_Yh

    #Several countries blink in and out of the time series. Only changing Russia because it's the only one that is a really large amount of fuel
    L1011.in_EJ_ctry_intlship_TOT_Yh %>%
      mutate(value = if_else(value = is.na, 0, value)) -> L1011.in_EJ_ctry_intlship_TOT_Yh

    L1011.in_EJ_ctry_intlship_TOT_Yh %>%
      filter(Country == "Russia" & year %in% 2005 ) %>%
      select(value) -> Russia_2005
    L1011.in_EJ_ctry_intlship_TOT_Yh %>%
      filter(Country == "Russia" & year %in% c(2006, 2007, 2008, 2009, 2010)) %>%
      left_join(Russia_2005 %>%
                  rename(value_2005 = value)) %>%
      mutate(value = value_2005) %>%
      select



    L1011.in_EJ_ctry_intlship_TOT_Yh[ is.na( L1011.in_EJ_ctry_intlship_TOT_Yh ) ] <- 0
    L1011.in_EJ_ctry_intlship_TOT_Yh[ L1011.in_EJ_ctry_intlship_TOT_Yh$Country=="Russia", paste( "X", 2006:2010, sep = "" ) ] <-
      L1011.in_EJ_ctry_intlship_TOT_Yh$X2005[ L1011.in_EJ_ctry_intlship_TOT_Yh$Country=="Russia" ]

    # ===================================================

    # Produce outputs

    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1011.en_bal_EJ_R_Si_Fi_Yh") %>%
      add_precursors("L101.en_bal_EJ_R_Si_Fi_Yh_full", "energy/A22.globaltech_coef", "energy/calibrated_techs") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1011.en_bal_EJ_R_Si_Fi_Yh

     L1011.in_EJ_ctry_intlship_TOT_Yh %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1011.in_EJ_ctry_intlship_TOT_Yh") %>%
      add_precursors("energy/EIA_RFO_intlship_kbbld", "energy/EIA_TOT_intlship_kbbld", "energy/mappings/EIA_ctry", "common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1011.in_EJ_ctry_intlship_TOT_Yh

    return_data(L1011.en_bal_EJ_R_Si_Fi_Yh, L1011.in_EJ_ctry_intlship_TOT_Yh)
  } else {
    stop("Unknown command")
  }
}
