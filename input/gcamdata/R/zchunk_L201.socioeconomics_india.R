#' module_gcamindia_L201.socioeconomics
#'
#' Interest rate, population, and GDP for GCAM-INDIA.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.InterestRate_GCAMINDIA}, \code{L201.Pop_GCAMINDIA}, \code{L201.BaseGDP_GCAMINDIA}, \code{L201.LaborForceFillout_GCAMINDIA}, \code{L201.LaborProductivity_GCAMINDIA}.
#' @details Interest rate, population, and GDP for GCAM-INDIA.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author Malyan_Ankur_CEEW
module_gcamindia_L201.socioeconomics <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/india_states_subregions",
             "L100.Pop_thous_state_india",
             "L100.GDP_mil90usd_state_india",
             "L102.pcgdp_thous90USD_GCAM3_ctry_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.InterestRate_GCAMINDIA",
             "L201.Pop_GCAMINDIA",
             "L201.BaseGDP_GCAMINDIA",
             "L201.LaborForceFillout_GCAMINDIA",
             "L201.LaborProductivity_GCAMINDIA"))
  } else if(command == driver.MAKE) {

    # silence package checks
    year <- value <- state <- totalPop <- baseGDP <- iso <- growth <- timestep <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    india_states_subregions <- get_data(all_data, "gcam-india/india_states_subregions")
    L100.Pop_thous_state_india <- get_data(all_data, "L100.Pop_thous_state_india")
    L100.GDP_mil90usd_state_india <- get_data(all_data, "L100.GDP_mil90usd_state_india")
    L102.pcgdp_thous90USD_GCAM3_ctry_Y <- get_data(all_data, "L102.pcgdp_thous90USD_GCAM3_ctry_Y")

    # ===================================================

    # L201.InterestRate: Interest rates by region
    L201.InterestRate <- tibble(region = india_states_subregions$state, interest.rate = socioeconomics.DEFAULT_INTEREST_RATE)

    # L201.Pop_GCAMINDIA: Population by region from the GCAM 3.0 core scenario
    L201.Pop_GCAMINDIA <- L100.Pop_thous_state_india %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(totalPop = value,
             region = state) %>%
      mutate(totalPop = round(totalPop, socioeconomics.POP_DIGITS))

    # L201.BaseGDP_GCAMINDIA: Base GDP for GCAM-INDIA scenario
    L201.BaseGDP_GCAMINDIA <- L100.GDP_mil90usd_state_india %>%
      filter(year == min(MODEL_YEARS)) %>%
      rename(baseGDP = value,
             region = state) %>%
      mutate(baseGDP = round(baseGDP, socioeconomics.GDP_DIGITS)) %>%
      select(-year)

    # L201.LaborForceFillout: Labor force participation and productivity for all scenarios
    # NOTE: No model of labor force used; labor force participation set to a constant
    L201.LaborForceFillout <- tibble(region = india_states_subregions$state,
                                     year.fillout = min(MODEL_YEARS),
                                     laborforce = socioeconomics.DEFAULT_LABORFORCE)

    # LABOR PRODUCTIVITY GROWTH RATE CALCULATION
    # Labor productivity growth is calculated from the change in per-capita GDP ratio in each time period
    # Calculate the growth rate in per-capita GDP
    L201.LaborProductivity_GCAMINDIA <- L102.pcgdp_thous90USD_GCAM3_ctry_Y %>%
      filter(tolower(iso) == "ind",
             year %in% MODEL_YEARS) %>%
      # In order to calculate growth rate we need to know how much GDP grew and number of years between periods
      mutate(growth = value / lag(value),
             timestep = year - lag(year),
             laborproductivity = round(growth ^ (1 / timestep) - 1, socioeconomics.LABOR_PRODUCTIVITY_DIGITS)) %>%
      # Remove the first model year, since it has no previous period to calculate growth rate
      filter(year != min(MODEL_YEARS)) %>%
      # Renaming to region because region column needed for write_to_all_states
      rename(region = iso) %>%
      # NOTE: applying the INDIA average to all states equally
      write_to_all_states(LEVEL2_DATA_NAMES[["LaborProductivity"]], region_list = gcamindia.STATES)

    # ===================================================

    # Produce outputs
    L201.InterestRate %>%
      add_title("Interest rates by state") %>%
      add_units("Unitless") %>%
      add_comments("Constant assumed for all states") %>%
      add_legacy_name("L201.InterestRate_GCAMINDIA") %>%
      add_precursors("gcam-india/india_states_subregions") ->
      L201.InterestRate_GCAMINDIA

    L201.Pop_GCAMINDIA %>%
      add_title("Population by state") %>%
      add_units("thousand persons") %>%
      add_comments("Data from L100.Pop_thous_state_india") %>%
      add_legacy_name("L201.Pop_GCAMINDIA") %>%
      add_precursors("L100.Pop_thous_state_india") ->
      L201.Pop_GCAMINDIA

    L201.BaseGDP_GCAMINDIA %>%
      add_title("Base year GDP by state") %>%
      add_units("million 1990 USD") %>%
      add_comments("Data from L100.GDP_mil90usd_state_india") %>%
      add_legacy_name("L201.BaseGDP_GCAMINDIA") %>%
      add_precursors("L100.GDP_mil90usd_state_india") ->
      L201.BaseGDP_GCAMINDIA

    L201.LaborForceFillout %>%
      add_title("Labor force participation and productivity for all scenarios") %>%
      add_units("Unitless") %>%
      add_comments("Constant value assumed") %>%
      add_legacy_name("L201.LaborForceFillout") %>%
      add_precursors("gcam-india/india_states_subregions") ->
      L201.LaborForceFillout_GCAMINDIA

    L201.LaborProductivity_GCAMINDIA %>%
      add_title("Labor force productivity growth rate for GCAM-INDIA") %>%
      add_units("Unitless (annual rate of growth)") %>%
      add_comments("Values from L102.pcgdp_thous90USD_GCAM3_ctry_Y used to calculate annual growth") %>%
      add_comments("INDIA value written to all states") %>%
      add_legacy_name("L201.LaborProductivity_GCAMINDIA") %>%
      add_precursors("L102.pcgdp_thous90USD_GCAM3_ctry_Y") ->
      L201.LaborProductivity_GCAMINDIA


    return_data(L201.InterestRate_GCAMINDIA, L201.Pop_GCAMINDIA, L201.BaseGDP_GCAMINDIA, L201.LaborForceFillout_GCAMINDIA, L201.LaborProductivity_GCAMINDIA)
  } else {
    stop("Unknown command")
  }
}
