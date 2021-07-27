# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L232.water_demand_industry
#'
#' Computes industrial water withdrawal/consumption coefficients (m3/GJ output) by US state and year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L232.TechCoef_USA}.
#' @details This chunk generates industrial water withdrawal and consumption coefficients by US state and year.
#' Water coefficients are as total withdrawals/consumption divided by industrial sector output (base-service).
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RC March 2019
module_gcamusa_L232.water_demand_industry <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/water_td_sectors",
             FILE = "energy/A32.globaltech_coef",
             "L132.water_km3_state_ind_Yh",
             "L232.StubTechProd_industry_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L232.TechCoef_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    water_sector <- water_type <- region <- supplysector <- subsector <- technology <-
      year <- minicam.energy.input <- coefficient <- market.name <- calOutputValue <-
      water_km3 <- energy_EJ <- state <- NULL  # silence package check notes

    # Load required inputs
    A32.globaltech_coef <- get_data(all_data, "energy/A32.globaltech_coef")
    water_td_sectors <- get_data(all_data, "water/water_td_sectors")
    L132.water_km3_state_ind_Yh <- get_data(all_data, "L132.water_km3_state_ind_Yh")
    L232.StubTechProd_industry_USA <- get_data(all_data, "L232.StubTechProd_industry_USA")

    # First, compute historical coefficients, as total withdrawals/consumption divided by industrial sector output
    # (base-service)
    L132.water_km3_state_ind_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L232.StubTechProd_industry_USA, by = c("state" = "region", "year")) %>%
      rename(energy_EJ = calOutputValue) %>%
      mutate(coefficient = round(water_km3 / energy_EJ, digits = energy.DIGITS_COEFFICIENT)) ->
      L232.water_km3_state_ind_Yh

    # Read in water coefs for all years
    L232.water_km3_state_ind_Yh %>%
      select(region = state, water_type, year, coefficient) %>%
      repeat_add_columns(distinct(A32.globaltech_coef, supplysector, subsector, technology)) %>%
      mutate(water_sector = "Manufacturing",
             minicam.energy.input = set_water_input_name(water_sector, water_type, water_td_sectors),
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) %>%
      # Fill out the values in the final base year to all future years
      group_by(region, supplysector, subsector, technology, minicam.energy.input, market.name) %>%
      complete(year = MODEL_YEARS) %>%
      mutate(coefficient = if_else(year %in% MODEL_FUTURE_YEARS, coefficient[year == max(MODEL_BASE_YEARS)], coefficient)) %>%
      ungroup() ->
      L232.TechCoef_USA

    # add attributes for output
    L232.TechCoef_USA %>%
      add_title("Industrial water demand coefficients by US state, water type, and year") %>%
      add_units("m3/GJ") %>%
      add_comments("Historical coefficients are calculated as industrial water demand divided by industrial sector output") %>%
      add_comments("Future coefficients are set the same as in the final base year") %>%
      add_legacy_name("L232.TechCoef_USA") %>%
      add_precursors("water/water_td_sectors",
                     "energy/A32.globaltech_coef",
                     "L132.water_km3_state_ind_Yh",
                     "L232.StubTechProd_industry_USA") ->
      L232.TechCoef_USA

    return_data(L232.TechCoef_USA)
  } else {
    stop("Unknown command")
  }
}
