# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L201.socioeconomics_USA
#'
#' Interest rate, population, and GDP for GCAM-USA.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.InterestRate_GCAMUSA}, \code{L201.Pop_GCAMUSA}, \code{L201.BaseGDP_GCAMUSA},
#' \code{L201.LaborForceFillout_GCAMUSA}, \code{L201.LaborProductivity_GCAMUSA}, \code{L201.Pop_national_updated_USA},
#' \code{L201.BaseGDP_national_updated_USA}, \code{L201.LaborProductivity_national_updated_USA}.
#' The corresponding file in the original data system was \code{L201.socioeconomics_USA.R} (gcam-usa level2).
#' @details Interest rate, population, and GDP for GCAM-USA.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter lag mutate select rename
#' @author RLH October 2017
module_gcamusa_L201.socioeconomics_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             "L100.Pop_thous_state",
             "L100.GDP_mil90usd_state"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.Pop_GCAMUSA",
             "L201.BaseGDP_GCAMUSA",
             "L201.LaborForceFillout_GCAMUSA",
             "L201.LaborProductivity_GCAMUSA",
             "L201.Pop_national_updated_USA",
             "L201.BaseGDP_national_updated_USA",
             "L201.LaborProductivity_national_updated_USA"))
  } else if(command == driver.MAKE) {

    # silence package checks
    year <- value <- state <- totalPop <- baseGDP <- iso <- growth <- timestep <- region <-
      GDP <- pop <- laborproductivity <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    L100.Pop_thous_state <- get_data(all_data, "L100.Pop_thous_state")
    L100.GDP_mil90usd_state <- get_data(all_data, "L100.GDP_mil90usd_state")

    # ===================================================
    # NOTE: Socioeconomics for grid regions are dealt with in module_gcamusa_L223.electricity_USA

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
    L201.LaborProductivity_GCAMUSA <- L100.GDP_mil90usd_state %>%
      rename(GDP = value) %>%
      filter(year %in% MODEL_YEARS) %>%
      left_join_error_no_match(L100.Pop_thous_state %>%
                                 rename(pop = value),
                               by = c("state", "year")) %>%
      mutate(value = GDP / pop) %>%
      group_by(state) %>%
      # In order to calculate growth rate we need to know how much GDP grew and number of years between periods
      mutate(growth = value / lag(value),
             timestep = year - lag(year),
             laborproductivity = round(growth ^ (1 / timestep) - 1, socioeconomics.LABOR_PRODUCTIVITY_DIGITS)) %>%
      # Remove the first model year, since it has no previous period to calculate growth rate
      ungroup() %>%
      filter(year != min(MODEL_YEARS)) %>%
      select(region = state, year, laborproductivity)


    # Add USA-region udpates
    # Updated USA-region population
    L201.Pop_GCAMUSA %>%
      group_by(year) %>%
      summarise(totalPop = sum(totalPop)) %>%
      ungroup() %>%
      mutate(totalPop = round(totalPop, socioeconomics.POP_DIGITS),
             region = gcam.USA_REGION) %>%
      select(region, year, totalPop) -> L201.Pop_national_updated_USA

    # Updated USA-region base GDP
    L201.BaseGDP_GCAMUSA %>%
      summarise(baseGDP = sum(baseGDP)) %>%
      mutate(baseGDP = round(baseGDP, socioeconomics.GDP_DIGITS),
             region = gcam.USA_REGION) %>%
      select(region, baseGDP) -> L201.BaseGDP_national_updated_USA

    # Updated USA-region labor productivity (GDP)
    L100.GDP_mil90usd_state %>%
      rename(GDP = value) %>%
      filter(year %in% MODEL_YEARS) %>%
      left_join_error_no_match(L100.Pop_thous_state %>%
                                 rename(pop = value),
                               by = c("state", "year")) %>%
      group_by(year) %>%
      summarise(GDP = sum(GDP),
                pop = sum(pop)) %>%
      ungroup() %>%
      mutate(value = GDP / pop,
             region = gcam.USA_REGION) %>%
      group_by(region) %>%
      arrange(year) %>%
      # In order to calculate growth rate we need to know how much GDP grew and number of years between periods
      mutate(growth = value / lag(value),
             timestep = year - lag(year),
             laborproductivity = round(growth ^ (1 / timestep) - 1, socioeconomics.LABOR_PRODUCTIVITY_DIGITS)) %>%
      # Remove the first model year, since it has no previous period to calculate growth rate
      ungroup() %>%
      filter(year != min(MODEL_YEARS)) %>%
      select(region, year, laborproductivity) ->
      L201.LaborProductivity_national_updated_USA


    # ===================================================

    # Produce outputs
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
      add_comments("Values from L100.GDP_mil90usd_state used to calculate annual growth") %>%
      add_legacy_name("L201.LaborProductivity_GCAMUSA") %>%
      add_precursors("L100.GDP_mil90usd_state") ->
      L201.LaborProductivity_GCAMUSA

    L201.Pop_national_updated_USA %>%
      add_title("Updated population for USA region, consistent with sum-of-states") %>%
      add_units("thousand persons") %>%
      add_comments("Updates USA region population to match the 50 state + DC total") %>%
      add_legacy_name("L2011.Pop_updated_USA_national") %>%
      same_precursors_as("L201.Pop_GCAMUSA") ->
      L201.Pop_national_updated_USA

    L201.BaseGDP_national_updated_USA %>%
      add_title("Updated base year GDP for USA region, consistent with sum-of-states") %>%
      add_units("million 1990 USD") %>%
      add_comments("Updates USA region base year GDP to match the 50 state + DC total") %>%
      add_legacy_name("L2011.BaseGDP_updated_USA_national") %>%
      same_precursors_as("L201.BaseGDP_GCAMUSA") ->
      L201.BaseGDP_national_updated_USA

    L201.LaborProductivity_national_updated_USA %>%
      add_title("Updated labor force productivity growth rate for USA region") %>%
      add_units("Unitless (annual rate of growth)") %>%
      add_comments("Annual growth rate calcualted to produce USA region GDP which matches the 50 state + DC total") %>%
      add_legacy_name("L2011.LaborProductivity_updated_USA_national") %>%
      add_precursors("L100.GDP_mil90usd_state") ->
      L201.LaborProductivity_national_updated_USA


    return_data(L201.Pop_GCAMUSA,
                L201.BaseGDP_GCAMUSA,
                L201.LaborForceFillout_GCAMUSA,
                L201.LaborProductivity_GCAMUSA,
                L201.Pop_national_updated_USA,
                L201.BaseGDP_national_updated_USA,
                L201.LaborProductivity_national_updated_USA)
  } else {
    stop("Unknown command")
  }
}
