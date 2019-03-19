#' module_gcamusa_L232.water.demand.industry_USA
#'
#' Computes industry water withdrawal/consumption coefficients (km3/EJ output) by USA state and year
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L232.TechCoef_USA}. The corresponding file in the
#' original data system was \code{L231.Industry_water.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RC Nov 2018
module_gcamusa_L232.water.demand.industry_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/A03.sector",
             FILE = "gcam-usa/USGS_industry_water_demand_km3",
             "L232.StubTechProd_industry_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L232.TechCoef_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    water_sector <- water_type <- region <- supplysector <- subsector <- technology <-
      year <- minicam.energy.input <- coefficient <- market.name <- calOutputValue <-
      water_km3 <- energy_EJ <- NULL  # silence package check notes

    # Load required inputs
    A03.sector <- get_data(all_data, "water/A03.sector")
    Industry_water_demand <- get_data(all_data, "gcam-usa/USGS_industry_water_demand_km3")
    L232.StubTechProd_industry_USA <- get_data(all_data, "L232.StubTechProd_industry_USA")

    # Calculate coefficients as total withdrawals/consumption divided by industrial sector output (base-service)
    Industry_water_demand %>%
      gather_years(value_col = "water_km3") %>%
      # USGS data only go back to 1985, and use that for 1975
      mutate(year = replace(year, year == min(year), min(MODEL_BASE_YEARS))) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      # Join industry output data
      left_join_error_no_match(L232.StubTechProd_industry_USA %>%
                                 select(region, year, energy_EJ = calOutputValue), by = c("region", "year")) %>%
      # Calculate coefficnets
      mutate(coefficient = round(water_km3 / energy_EJ, digits = energy.DIGITS_COEFFICIENT)) %>%
      mutate(water_sector = "Manufacturing", water_type = minicam.energy.input,
             minicam.energy.input = set_water_input_name(water_sector, water_type, A03.sector),
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
      L232.TechCoef_USA

    # Fill out the values in the final available base year to later base years and all future years
    L232.TechCoef_USA %>%
      group_by(region, supplysector, subsector, technology, minicam.energy.input, market.name) %>%
      complete(year = MODEL_YEARS) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 2)) %>%
      ungroup ->
      L232.TechCoef_USA

    # add attributes for output
    L232.TechCoef_USA %>%
      add_title("Water withdrawal and consumption coefficients for industry by USA state, water type, and year") %>%
      add_units("km3/EJ") %>%
      add_comments("Coefficients are calculated as water withdrawal and consumption divided by industry outputs") %>%
      add_legacy_name("L232.TechCoef_USA") %>%
      add_precursors("water/A03.sector",
                     "gcam-usa/USGS_industry_water_demand_km3",
                     "L232.StubTechProd_industry_USA") ->
      L232.TechCoef_USA

    return_data(L232.TechCoef_USA)
  } else {
    stop("Unknown command")
  }
}
