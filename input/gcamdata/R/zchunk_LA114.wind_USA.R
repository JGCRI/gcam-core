# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LA114.Wind
#'
#' Compute capacity factors for wind by US state.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L114.CapacityFactor_wind_state}. The corresponding file in the
#' original data system was \code{LA114.Wind.R} (gcam-usa level1).
#' @details Computes capacity factors for wind by US state.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate pull select
#' @importFrom tidyr complete
#' @author ST September 2017
module_gcamusa_LA114.wind <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( FILE = "gcam-usa/us_state_wind",
              "L113.globaltech_OMfixed_ATB",
              "L113.globaltech_OMvar_ATB",
              "L113.globaltech_capital_ATB"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L114.CapacityFactor_wind_state"))
  } else if(command == driver.MAKE) {

    technology <- year <- state <- sector <- capacity.factor <- fuel <- value <- base_cost <-
      region <- NULL  # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    us_state_wind <- get_data(all_data, "gcam-usa/us_state_wind", strip_attributes = TRUE)
    L113.globaltech_capital_ATB <- get_data(all_data, "L113.globaltech_capital_ATB")
    L113.globaltech_OMfixed_ATB <- get_data(all_data, "L113.globaltech_OMfixed_ATB")
    L113.globaltech_OMvar_ATB <- get_data(all_data, "L113.globaltech_OMvar_ATB")

    # ===================================================

    # This function filters A23 tables for wind, then gathers and interpolates...
    # ... to get a single value for wind base cost year. Note that the interpolation is...
    # ... redundant whilst gcamusa.WIND_BASE_COST_YEAR = 2005, ...
    # ... since 2005 is an existing data point in all A23 tables.
    filter_gather_interp_get_cost <- function(x) {
      . <- NULL  # silence package check notes
      x %>% filter(technology == "wind") %>%
        gather_years %>%
        select(year, value) %>%
        complete(year = c(year, gcamusa.WIND_BASE_COST_YEAR)) %>%
        mutate(value = approx_fun(year, value, rule = 2)) %>%
        filter(year == gcamusa.WIND_BASE_COST_YEAR) %>%
        pull(value)
    }

    L113.globaltech_capital_ATB %>% filter_gather_interp_get_cost -> L114.CapCost
    L113.globaltech_OMfixed_ATB %>% filter_gather_interp_get_cost -> L114.OMFixedCost
    L113.globaltech_OMvar_ATB %>% filter_gather_interp_get_cost -> L114.OMVarCost
    # ^^ all above rates are in $1975

    # Get fixed charge rate of capital for wind
    filter(L113.globaltech_capital_ATB, technology == "wind")$fixed.charge.rate -> L114.FixedChargeRate

    # Convert state level wind base cost data to 1975$/GJ, ...
    # ... then compute capacity factor for the base wind turbine in each state
    us_state_wind %>%
      mutate(sector = "electricity generation",
             fuel = "wind",
             base_cost = base_cost * gdp_deflator(1975, 2007) / CONV_KWH_GJ,
             capacity.factor = (L114.CapCost * L114.FixedChargeRate + L114.OMFixedCost) /
               (CONV_KWH_GJ * CONV_YEAR_HOURS) / (base_cost - (L114.OMVarCost / CONV_MWH_GJ))) %>%
      # ^^ capacity factor computed dividing sum of capital and fixed costs (converted to $/GJ) by ...
      # ... base cost minus variable cost (converted to $/GJ)
      rename(state = region) %>%
      select(state, sector, fuel, capacity.factor) %>%
      # add attributes for output...
      add_title("Capacity factor for wind by state") %>%
      add_units("Unitless") %>%
      add_comments("Computed from A23 tables for capital, variable and fixed wind costs") %>%
      add_legacy_name("L114.CapacityFactor_wind_state") %>%
      add_precursors("gcam-usa/us_state_wind",
                     "L113.globaltech_capital_ATB",
                     "L113.globaltech_OMfixed_ATB",
                     "L113.globaltech_OMvar_ATB") ->
      L114.CapacityFactor_wind_state

    return_data(L114.CapacityFactor_wind_state)
  } else {
    stop("Unknown command")
  }
}
