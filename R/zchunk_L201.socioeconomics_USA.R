#' module_gcam.usa_L201.socioeconomics_USA
#'
#' Interest rate, population, and GDP for GCAM-USA.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.InterestRate_GCAMUSA}, \code{L201.Pop_GCAMUSA}, \code{L201.BaseGDP_GCAMUSA}, \code{L201.LaborForceFillout_GCAMUSA}, \code{L201.LaborProductivity_GCAMUSA}. The corresponding file in the
#' original data system was \code{L201.socioeconomics_USA.R} (gcam-usa level2).
#' @details Interest rate, population, and GDP for GCAM-USA.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RLH October 2017
module_gcam.usa_L201.socioeconomics_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             "L100.Pop_thous_state",
             "L100.GDP_mil90usd_state",
             "L102.pcgdp_thous90USD_GCAM3_ctry_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.InterestRate_GCAMUSA",
             "L201.Pop_GCAMUSA",
             "L201.BaseGDP_GCAMUSA",
             "L201.LaborForceFillout_GCAMUSA",
             "L201.LaborProductivity_GCAMUSA"))
  } else if(command == driver.MAKE) {

    # silence package checks
    year <- value <- state <- totalPop <- baseGDP <- iso <- growth <- timestep <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    L100.Pop_thous_state <- get_data(all_data, "L100.Pop_thous_state")
    L100.GDP_mil90usd_state <- get_data(all_data, "L100.GDP_mil90usd_state")
    L102.pcgdp_thous90USD_GCAM3_ctry_Y <- get_data(all_data, "L102.pcgdp_thous90USD_GCAM3_ctry_Y")

    # ===================================================
    # NOTE: Socioeconomics for grid regions are dealt with in module_gcam.usa_L223.electricity_USA

    # L201.InterestRate: Interest rates by region
    L201.InterestRate <- tibble(region = states_subregions$state, interest.rate = socioeconomics.DEFAULT_INTEREST_RATE)

    # L201.Pop_GCAMUSA: Population by region from the GCAM 3.0 core scenario
    L201.Pop_GCAMUSA <- L100.Pop_thous_state %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(totalPop = value,
             region = state) %>%
      mutate(totalPop = round(totalPop, socioeconomics.POP_DIGITS))

    # L201.BaseGDP_GCAMUSA: Base GDP for GCAM-USA scenario
    L201.BaseGDP_GCAMUSA <- L100.GDP_mil90usd_state %>%
      filter(year == min(MODEL_YEARS)) %>%
      rename(baseGDP = value,
             region = state) %>%
      mutate(baseGDP = round(baseGDP, socioeconomics.GDP_DIGITS)) %>%
      select(-year)

    # L201.LaborForceFillout: Labor force participation and productivity for all scenarios
    # NOTE: No model of labor force used; labor force participation set to a constant
    L201.LaborForceFillout <- tibble(region = states_subregions$state,
                                     year.fillout = min(MODEL_YEARS),
                                     laborforce = socioeconomics.DEFAULT_LABORFORCE)

    # LABOR PRODUCTIVITY GROWTH RATE CALCULATION
    # Labor productivity growth is calculated from the change in per-capita GDP ratio in each time period
    # Calculate the growth rate in per-capita GDP
    L201.LaborProductivity_GCAMUSA <- L102.pcgdp_thous90USD_GCAM3_ctry_Y %>%
      filter(tolower(iso) == "usa",
             year %in% MODEL_YEARS) %>%
      # In order to calculate growth rate we need to know how much GDP grew and number of years between periods
      mutate(growth = value / lag(value),
             timestep = year - lag(year),
             laborproductivity = round(growth ^ (1 / timestep) - 1, socioeconomics.LABOR_PRODUCTIVITY_DIGITS)) %>%
      # Remove the first model year, since it has no previous period to calculate growth rate
      filter(year != min(MODEL_YEARS)) %>%
      # Renaming to region because region column needed for write_to_all_states
      rename(region = iso) %>%
      # NOTE: applying the USA average to all states equally
      write_to_all_states(LEVEL2_DATA_NAMES[["LaborProductivity"]])

    # ===================================================

    # Produce outputs
    L201.InterestRate %>%
      add_title("Interest rates by state") %>%
      add_units("Unitless") %>%
      add_comments("Constant assumed for all states") %>%
      add_legacy_name("L201.InterestRate_GCAMUSA") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L201.InterestRate_GCAMUSA

    L201.Pop_GCAMUSA %>%
      add_title("Population by state") %>%
      add_units("thousand persons") %>%
      add_comments("Data from L100.Pop_thous_state") %>%
      add_legacy_name("L201.Pop_GCAMUSA") %>%
      add_precursors("L100.Pop_thous_state") ->
      L201.Pop_GCAMUSA

    L201.BaseGDP_GCAMUSA %>%
      add_title("Base year GDP by state") %>%
      add_units("million 1990 USD") %>%
      add_comments("Data from L100.GDP_mil90usd_state") %>%
      add_legacy_name("L201.BaseGDP_GCAMUSA") %>%
      add_precursors("L100.GDP_mil90usd_state") ->
      L201.BaseGDP_GCAMUSA

    L201.LaborForceFillout %>%
      add_title("Labor force participation and productivity for all scenarios") %>%
      add_units("Unitless") %>%
      add_comments("Constant value assumed") %>%
      add_legacy_name("L201.LaborForceFillout") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L201.LaborForceFillout_GCAMUSA

    L201.LaborProductivity_GCAMUSA %>%
      add_title("Labor force productivity growth rate for GCAM-USA") %>%
      add_units("Unitless (annual rate of growth)") %>%
      add_comments("Values from L102.pcgdp_thous90USD_GCAM3_ctry_Y used to calculate annual growth") %>%
      add_comments("USA value written to all states") %>%
      add_legacy_name("L201.LaborProductivity_GCAMUSA") %>%
      add_precursors("L102.pcgdp_thous90USD_GCAM3_ctry_Y") ->
      L201.LaborProductivity_GCAMUSA


    return_data(L201.InterestRate_GCAMUSA, L201.Pop_GCAMUSA, L201.BaseGDP_GCAMUSA, L201.LaborForceFillout_GCAMUSA, L201.LaborProductivity_GCAMUSA)
  } else {
    stop("Unknown command")
  }
}
