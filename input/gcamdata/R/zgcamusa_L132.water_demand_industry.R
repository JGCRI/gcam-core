# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L132.water_demand_industry
#'
#' Computes industrial water withdrawals and consumption by US state and historical year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L132.water_km3_state_ind_Yh}.
#' @details This chunks extends the USGS industrial water withdrawals and consumption data to all historical years. Missing years are
#' filled with interpolated water coefficients and state industrial energy consumption.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RC March 2019
module_gcamusa_L132.water_demand_industry <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/USGS_industry_water_demand_km3",
             "L101.inEIA_EJ_state_S_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L132.water_km3_state_ind_Yh"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    year <- state <- sector <- water_type <- water_km3 <- energy_EJ <- value <- coefficient <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L101.inEIA_EJ_state_S_F <- get_data(all_data, "L101.inEIA_EJ_state_S_F")
    USGS_industry_water_demand_km3 <- get_data(all_data, "gcam-usa/USGS_industry_water_demand_km3") %>%
      gather_years() %>%
      rename(water_km3 = value)

    # ===================================================
    # USGS state industrial water demand are not available for all GCAM historical periods.
    # Here, we first compute the water coefficients for available years, then interpolate the coefficients to all historical years,
    # and finally apply the coefficients to state total industrial energy consumption to compute historical industry water demand.

    # First, calculate total industrial energy consumption by state
    L101.inEIA_EJ_state_S_F %>%
      filter(sector == "industry") %>%
      group_by(state, year) %>%
      summarise(energy_EJ = sum(value)) %>%
      ungroup ->
      L132.in_EJ_state_ind

    # Second, join water demand and energy consumption to calculate coefficients
    USGS_industry_water_demand_km3 %>%
      complete(nesting(state, sector, water_type), year = L132.in_EJ_state_ind$year) %>%
      left_join_error_no_match(L132.in_EJ_state_ind, by = c("state", "year")) %>%
      # Calculate water demand coefficients
      mutate(coefficient = water_km3 / energy_EJ) %>%
      group_by(state, water_type) %>%
      # Third, interpolate water coefficients to all historical years
      mutate(coefficient = approx_fun(year, coefficient, rule = 2),
             # Fourth, Compute water demand for missing years
             water_km3 = energy_EJ * coefficient) %>%
      ungroup %>%
      select(state, sector, water_type, year, water_km3) ->
      L132.water_km3_state_ind_Yh

    ## OUTPUTS
    L132.water_km3_state_ind_Yh %>%
      add_title("Industrial water demand by US state, water type and historical year") %>%
      add_units("km3") %>%
      add_comments("Calculate industrial water coefficients with state water demand and energy consumption data") %>%
      add_comments("Interpolate the coefficients to all historical years") %>%
      add_comments("Apply the coefficients to energy consumption and compute state industrial water demand in all historical years") %>%
      add_legacy_name("L132.water_km3_state_ind_Yh") %>%
      add_precursors("gcam-usa/USGS_industry_water_demand_km3",
                     "L101.inEIA_EJ_state_S_F") ->
      L132.water_km3_state_ind_Yh

    return_data(L132.water_km3_state_ind_Yh)
  } else {
    stop("Unknown command")
  }
}
