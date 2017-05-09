#' module_energy_LA152.transportation
#'
#' Calculate transportation sector energy consumption.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L152.in_EJ_R_trn_F_Yh}. The corresponding file in the
#' original data system was \code{LA152.transportation.R} (energy level1).
#' @details Calculate transportation sector energy consumption by GCAM region, sector, fuel, and year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL April 2017
module_energy_LA152.transportation <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/enduse_fuel_aggregation",
             FILE = "energy/enduse_sector_aggregation",
             FILE = "L131.in_EJ_R_Senduse_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L152.in_EJ_R_trn_F_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    enduse_fuel_aggregation <- get_data(all_data, "energy/enduse_fuel_aggregation")
    enduse_sector_aggregation <- get_data(all_data, "energy/enduse_sector_aggregation")
    L131.in_EJ_R_Senduse_F_Yh <- get_data(all_data, "L131.in_EJ_R_Senduse_F_Yh")

    # Calculation of transportation sector energy consumption
    L131.in_EJ_R_Senduse_F_Yh %>%
      filter(grepl("trn", sector)) %>%
      left_join_error_no_match(enduse_sector_aggregation, by = "sector") %>%
      select(-sector) %>%
      rename(sector = sector_agg) %>%
      left_join_error_no_match(select(enduse_fuel_aggregation, fuel, trn), by = "fuel") %>%
      select(-fuel) %>%
      rename(fuel = trn) %>%
      # Aggregate historical years by region, sector, fuel, and year
      filter(year %in% HISTORICAL_YEARS) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      # Remove "in_" as it's not needed
      mutate(sector = sub("in_", "", sector)) %>%

    # Produce outputs
      add_title("Transportation energy consumption by GCAM region / mode / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Transportation energy consumption aggregated by fuel and sector lookups.") %>%
      add_legacy_name("L152.in_EJ_R_trn_F_Yh") %>%
      add_precursors("energy/enduse_fuel_aggregation",
                     "energy/enduse_sector_aggregation",
                     "L131.in_EJ_R_Senduse_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L152.in_EJ_R_trn_F_Yh

    return_data(L152.in_EJ_R_trn_F_Yh)
  } else {
    stop("Unknown command")
  }
}
