# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L201.socioeconomics
#'
#' Interest rate, population, and GDP for GCAM-USA.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.InterestRate_GCAMUSA}, \code{L201.Pop_GCAMUSA}, \code{L201.BaseGDP_GCAMUSA},
#' \code{L201.LaborForceFillout_GCAMUSA}, \code{L201.Pop_national_updated_USA},
#' \code{L201.BaseGDP_national_updated_USA}, \code{L201.LaborProductivity_national_updated_USA}.
#' The corresponding file in the original data system was \code{L201.socioeconomics_USA.R} (gcam-usa level2).
#' @details Interest rate, population, and GDP for GCAM-USA.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter lag mutate select rename
#' @author RLH October 2017
module_gcamusa_L201.socioeconomics <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L100.Pop_thous_state",
             "L100.GDP_mil90usd_state"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.Pop_GCAMUSA",
             "L201.GDP_GCAMUSA",
             "L201.Pop_national_updated_USA",
             "L201.GDP_national_updated_USA"))
  } else if(command == driver.MAKE) {

    # silence package checks
    year <- value <- state <- totalPop <- baseGDP <- iso <- growth <- timestep <- region <-
      GDP <- pop <- laborproductivity <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L100.Pop_thous_state <- get_data(all_data, "L100.Pop_thous_state", strip_attributes = TRUE)
    L100.GDP_mil90usd_state <- get_data(all_data, "L100.GDP_mil90usd_state", strip_attributes = TRUE)

    # ===================================================
    # NOTE: Socioeconomics for grid regions are dealt with in module_gcamusa_L223.electricity

    # L201.Pop_GCAMUSA: Population by region from the GCAM 3.0 core scenario
    L201.Pop_GCAMUSA <- L100.Pop_thous_state %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(totalPop = value,
             region = state) %>%
      mutate(totalPop = round(totalPop, socioeconomics.POP_DIGITS))

    # L201.GDP_GCAMUSA: Base GDP for GCAM-USA scenario
    L201.GDP_GCAMUSA <- L100.GDP_mil90usd_state %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(GDP = value,
             region = state) %>%
      mutate(GDP = round(GDP, socioeconomics.GDP_DIGITS))

    # Add USA-region updates
    # Updated USA-region population
    L201.Pop_GCAMUSA %>%
      group_by(year) %>%
      summarise(totalPop = sum(totalPop)) %>%
      ungroup() %>%
      mutate(totalPop = round(totalPop, socioeconomics.POP_DIGITS),
             region = gcam.USA_REGION) %>%
      select(region, year, totalPop) -> L201.Pop_national_updated_USA

    # Updated USA-region base GDP
    L201.GDP_GCAMUSA %>%
      group_by(year) %>%
      summarise(GDP = sum(GDP)) %>%
      ungroup() %>%
      mutate(GDP = round(GDP, socioeconomics.GDP_DIGITS),
             region = gcam.USA_REGION) %>%
      select(region, year, GDP) -> L201.GDP_national_updated_USA


    # ===================================================

    # Produce outputs
    L201.Pop_GCAMUSA %>%
      add_title("Population by state") %>%
      add_units("thousand persons") %>%
      add_comments("Data from L100.Pop_thous_state") %>%
      add_legacy_name("L201.Pop_GCAMUSA") %>%
      add_precursors("L100.Pop_thous_state") ->
      L201.Pop_GCAMUSA

    L201.GDP_GCAMUSA %>%
      add_title("GDP by state and year") %>%
      add_units("million 1990 USD") %>%
      add_comments("Data from L100.GDP_mil90usd_state") %>%
      add_comments("The base GDP which will used when run in fixed or calibrated GDP modes") %>%
      add_comments("When in open GDP mode the actual GDP in modeled years may differ.") %>%
      add_precursors("L100.GDP_mil90usd_state") ->
      L201.GDP_GCAMUSA

    L201.Pop_national_updated_USA %>%
      add_title("Updated population for USA region, consistent with sum-of-states") %>%
      add_units("thousand persons") %>%
      add_comments("Updates USA region population to match the 50 state + DC total") %>%
      add_legacy_name("L2011.Pop_updated_USA_national") %>%
      same_precursors_as("L201.Pop_GCAMUSA") ->
      L201.Pop_national_updated_USA

    L201.GDP_national_updated_USA %>%
      add_title("Updated base year GDP for USA region, consistent with sum-of-states") %>%
      add_units("million 1990 USD") %>%
      add_comments("Updates USA region base year GDP to match the 50 state + DC total") %>%
      same_precursors_as("L201.GDP_GCAMUSA") ->
      L201.GDP_national_updated_USA


    return_data(L201.Pop_GCAMUSA,
                L201.GDP_GCAMUSA,
                L201.Pop_national_updated_USA,
                L201.GDP_national_updated_USA)
  } else {
    stop("Unknown command")
  }
}
