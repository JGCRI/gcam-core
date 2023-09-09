# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2322.industry_vintage
#'
#' Creates a vintage structure and retirement parameters for U.S. industrial energy use
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2322.StubTechSCurve_industry_USA}, \code{L2322.StubTechProfitShutdown_industry_USA}.
#'  The corresponding file in the original data system was \code{L2322.industry_vintage_USA.R} (gcam-usa level2).
#' @details This chunk creates a vintage structure and retirement parameters for U.S. industrial energy use.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @author KD August 2018; edited MTB October 2018
module_gcamusa_L2322.industry_vintage <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/A32.ind_retirement_USA",
             "L232.StubTechMarket_ind_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2322.StubTechSCurve_industry_USA",
             "L2322.StubTechProfitShutdown_industry_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    A32.ind_retirement_USA <- get_data(all_data, "gcam-usa/A32.ind_retirement_USA", strip_attributes = TRUE)
    L232.StubTechMarket_ind_USA <- get_data(all_data, "L232.StubTechMarket_ind_USA", strip_attributes = TRUE)

    # Silence package checks
    palette <- region <- supplysector <- subsector <- stub.technology <-
      year <- lifetime <- steepness <- half.life <- median.shutdown.point <-
      profit.shutdown.steepness <- NULL

    # ===================================================

    # Assume the same S-curve parameters for the final historical year and all future vintages for all of the states.
    A32.ind_retirement_USA %>%
      repeat_add_columns(tibble("region" =  states_subregions$state)) %>%
      repeat_add_columns(tibble("year" = c(max(MODEL_BASE_YEARS), MODEL_FUTURE_YEARS))) %>%
      select(LEVEL2_DATA_NAMES$StubTechSCurve) ->
      L2322.StubTechSCurve_industry_USA

    # Assume the same shutdown parameters for the final historical year and all future vintages for all of the states.
    A32.ind_retirement_USA %>%
      repeat_add_columns(tibble("region" =  states_subregions$state)) %>%
      repeat_add_columns(tibble("year" = c(max(MODEL_BASE_YEARS), MODEL_FUTURE_YEARS))) %>%
      select(LEVEL2_DATA_NAMES$StubTechProfitShutdown) ->
      L2322.StubTechProfitShutdown_industry_USA

    # ===================================================

    # Produce outputs
    L2322.StubTechSCurve_industry_USA %>%
      add_title("S curve parameters for industry in USA states") %>%
      add_units("steepness = S curve exponent &  half.life = year from 2010 at which half of initial 2010 stock will be retired") %>%
      add_comments("Assume the same S-curve parameters for base year and future vintages in all states for industry.") %>%
      add_legacy_name("L2322.StubTechSCurve_industry_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A32.ind_retirement_USA",
                     "L232.StubTechMarket_ind_USA") ->
      L2322.StubTechSCurve_industry_USA

    L2322.StubTechProfitShutdown_industry_USA %>%
      add_title("Profit shutdown decider parameters for industry in USA states") %>%
      add_units("profit.shutdown.steepness = S curve exponent; median.shutdown.point = profit-ratio at which 50% of vintage is shut down for lack of profitability") %>%
      add_comments("Assume the same profit shutdown parameters for industrial energy use for base year and future vintages in all states") %>%
      same_precursors_as("L2322.StubTechSCurve_industry_USA") ->
      L2322.StubTechProfitShutdown_industry_USA

    return_data(L2322.StubTechSCurve_industry_USA,
                L2322.StubTechProfitShutdown_industry_USA)
  } else {
    stop("Unknown command")
  }
}
