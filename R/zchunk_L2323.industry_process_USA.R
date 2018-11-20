#' module_gcam.usa_L2323.industry_process_USA
#'
#' Creates industrial processes input for state-level industry sectors in order to track industrial processes emissions.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2324.StubTechCoef_indproc_USA}.
#' The corresponding file in the original data system was \code{L2323.StubTechCoef_indproc_USA} (gcam-usa level2).
#' @details This chunk selects the subsectors to be removed from the hydrogen sectors in GCAM USA on the national level.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author MTB September 2018
module_gcam.usa_L2323.industry_process_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/A32.process_eff_USA",
             "L232.StubTechCoef_industry_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2323.StubTechCoef_indproc_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A32.process_eff_USA <- get_data(all_data, "gcam-usa/A32.process_eff_USA")
    L232.StubTechCoef_industry_USA <- get_data(all_data, "L232.StubTechCoef_industry_USA")

    # Silence package checks
    palette <- region <- supplysector <- subsector <- stub.technology <-
      year <- minicam.energy.input <- coefficient <- market.name <- NULL

    # ===================================================
    # Data Processing

    L232.StubTechCoef_industry_USA %>%
      distinct(region, supplysector, subsector, stub.technology, year) %>%
      left_join(A32.process_eff_USA %>%
                  select(-region),
                by = c("supplysector", "subsector", "stub.technology" = "technology", "year")) %>%
      mutate(market.name = gcamusa.DEFAULT_MARKET) -> L2323.StubTechCoef_indproc_USA

    # ===================================================
    # Produce outputs

    L2323.StubTechCoef_indproc_USA %>%
      add_title("Industrial Processes Coefficients for State-level Industry Sectors") %>%
      add_units("unitless") %>%
      add_comments("Industrial processes coefficients are exogenously specified (A32.process_eff_USA).") %>%
      add_comments("Industrial processes are added as a minicam-energy-input to each state's industry sector with the market set to USA") %>%
      add_comments("This allows industry activity at the state level to drive industrial process emissions; but the emissions are reported at the USA level.") %>%
      add_legacy_name("L2324.StubTechCoef_indproc_USA") %>%
      add_precursors("gcam-usa/A32.process_eff_USA",
                     "L232.StubTechCoef_industry_USA") ->
      L2323.StubTechCoef_indproc_USA

    return_data(L2323.StubTechCoef_indproc_USA)
  } else {
    stop("Unknown command")
  }
}
