#' module_gcamindia_LA114.wind
#'
#' Compute capacity factors for wind by India state.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L114.india_state_CapacityFactor_wind}.
#' @details Computes capacity factors for wind by India state.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author Malyan_Ankur_CEEW
module_gcamindia_LA114.wind <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( FILE = "gcam-india/A23.india_state_wind",
              FILE = "gcam-india/A23.globaltech_capital_India",
              FILE = "gcam-india/A23.globaltech_OMfixed_India",
              FILE = "gcam-india/A23.globaltech_OMvar_India"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L114.india_state_CapacityFactor_wind"))
  } else if(command == driver.MAKE) {

    technology <- year <- state <- sector <- capacity.factor <- fuel <- value <- base_cost <-
      region <- NULL  # silence package check.

    all_data <- list(...)[[1]]

    # ===================================================
    # Load required inputs
    A23.india_state_wind <- get_data(all_data, "gcam-india/A23.india_state_wind")
    A23.globaltech_capital_India <- get_data(all_data, "gcam-india/A23.globaltech_capital_India")
    A23.globaltech_OMfixed_India <- get_data(all_data, "gcam-india/A23.globaltech_OMfixed_India")
    A23.globaltech_OMvar_India <- get_data(all_data, "gcam-india/A23.globaltech_OMvar_India")

    # ===================================================
    # Calculations and Estimations

    # This function (filter_gather_interp_get_cost) filters A23 tables for wind, then gathers and interpolates...
    # ... to get a single value for wind base cost year. In case the value are missing for 2005 base year, the values...
    # ... interpolated from 2010 and 2015 by the approx fun () function.
    filter_gather_interp_get_cost <- function(x) {
      . <- NULL  # silence package check notes
      x %>% filter(technology == "wind") %>%
        gather_years %>%
        select(year, value) %>%
        complete(year = c(year, gcamindia.WIND_BASE_COST_YEAR)) %>%
        mutate(value = approx_fun(year, value, rule = 2)) %>%
        filter(year == gcamindia.WIND_BASE_COST_YEAR) %>%
        pull(value)
    }

    A23.globaltech_capital_India %>% filter_gather_interp_get_cost -> L114.CapCost_India
    A23.globaltech_OMfixed_India %>% filter_gather_interp_get_cost -> L114.OMFixedCost_India
    A23.globaltech_OMvar_India %>% filter_gather_interp_get_cost -> L114.OMVarCost_India

    # ^^ all above rates are in $1975

    # Get fixed charge rate of capital for wind
    filter(A23.globaltech_capital_India, technology == "wind")$fixed.charge.rate -> L114.FixedChargeRate_India

    #Computing the capacity factor for the base wind turbine in each state
    # Convert state level wind base cost data to 1975$/GJ.
    A23.india_state_wind %>%
      mutate(sector = "electricity generation",
             fuel = "wind",
             base_cost = base_cost * Price_2019INR_1975USD / CONV_KWH_GJ,
             capacity.factor = (L114.CapCost_India * L114.FixedChargeRate_India + L114.OMFixedCost_India) /
               (CONV_KWH_GJ * CONV_YEAR_HOURS) / (base_cost - (L114.OMVarCost_India / CONV_MWH_GJ))) %>%
      # ^^ capacity factor computed dividing sum of capital and fixed costs (converted to $/GJ) by base cost minus variable cost (converted to 1975$/GJ)
      rename(state = region) %>%
      mutate(capacity.factor = if_else(capacity.factor != "Inf", capacity.factor, 0.0)) %>%
      select(state, sector, fuel, capacity.factor) ->
      L114.india_state_CapacityFactor_wind

    # ===================================================
    #Produce Outputs
    L114.india_state_CapacityFactor_wind %>%
      add_title("Capacity factor for wind by state for India") %>%
      add_units("Unitless") %>%
      add_comments("Computed from A23 tables for capital, variable and fixed wind costs specifically for India") %>%
      add_legacy_name("L114.CapacityFactor_wind_state_India") %>%
      add_precursors("gcam-india/A23.india_state_wind",
                     "gcam-india/A23.globaltech_capital_India",
                     "gcam-india/A23.globaltech_OMfixed_India",
                     "gcam-india/A23.globaltech_OMvar_India") ->
      L114.india_state_CapacityFactor_wind

    return_data(L114.india_state_CapacityFactor_wind)
  } else {
    stop("Unknown command")
  }
}
