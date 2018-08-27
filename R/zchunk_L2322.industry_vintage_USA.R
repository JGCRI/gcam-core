#' module_gcam.usa_L2322.industry_vintage_USA
#'
#' Selects the subsectors to be removed from the hydrogen sectors for GCAM USA
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2322.StubTechSCurve_industry_USA}. The corresponding file in the
#' original data system was \code{L2322.industry_vintage_USA.R} (gcam-usa level2).
#' @details This chunk selects the subsectors to be removed from the hydrogen sectors in GCAM USA on the national level.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author KD August 2018
module_gcam.usa_L2322.industry_vintage_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/A32.ind_SCurve_USA",
             "L232.StubTechMarket_ind_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2322.StubTechSCurve_industry_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    A32.ind_SCurve_USA <- get_data(all_data, "gcam-usa/A32.ind_SCurve_USA")
    L232.StubTechMarket_ind_USA <- get_data(all_data, "L232.StubTechMarket_ind_USA")


    # ===================================================

    # Assume the same S-curve parameters for the final historical year for all of the states.
    A32.ind_SCurve_USA %>%
      mutate(year = gcamusa.FINAL_HISTORICAL_YEAR) %>%
      repeat_add_columns(tibble("region" =  states_subregions$state)) %>%
      select(region, supplysector, subsector, stub.technology, year, lifetime, steepness, half.life) ->
      L2322.StubTechSCurve_industry_USA

    # Add S-cure paramters to future vintages.
    L2322.StubTechSCurve_industry_USA %>%
      select(-year) %>%
      repeat_add_columns(tibble("year" = gcamusa.FUTURE_MODEL_YEARS)) %>%
      bind_rows(L2322.StubTechSCurve_industry_USA) ->
      L2322.StubTechSCurve_industry_USA


    # ===================================================

    # Produce outputs
    L2322.StubTechSCurve_industry_USA %>%
      add_title("S curve parameters for industry") %>%
      add_units("steepness = S curve exponent &  half.life = year from 2010 at which half of initial 2010 stock will be retired") %>%
      add_comments("Assume the same S-curve parameters for base year and future vintages in all states for industry.") %>%
      add_legacy_name("L2322.StubTechSCurve_industry_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/A32.ind_SCurve_USA",
                     "L232.StubTechMarket_ind_USA") ->
      L2322.StubTechSCurve_industry_USA

    return_data(L2322.StubTechSCurve_industry_USA)
  } else {
    stop("Unknown command")
  }
}
