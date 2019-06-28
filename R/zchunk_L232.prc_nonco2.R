# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L232.prc_nonco2
#'
#' Generates input emissions by energy technology, GHG, and base historical year.
#' Writes out max emissions reductions and steepness to all energy technologies and regions.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L232.nonco2_prc}, \code{L232.nonco2_max_reduction}, \code{L232.nonco2_steepness}. The corresponding file in the
#' original data system was \code{L232.prc_nonco2.R} (emissions level2).
#' @details Generates input emissions by energy technology, GHG, and base historical year. Writes out max emissions reductions and steepness to all energy technologies and regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else left_join mutate select
#' @importFrom tidyr gather spread
#' @author RH July 2017
module_emissions_L232.prc_nonco2 <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/A_regions",
             "L131.nonco2_tg_R_prc_S_S_Yh",
             FILE = "emissions/A32.max_reduction",
             FILE = "emissions/A32.steepness"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L232.nonco2_prc",
             "L232.nonco2_max_reduction",
             "L232.nonco2_steepness"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    region <- SO2_name <- year <- Non.CO2 <- supplysector <- subsector <- stub.technology <-
      value <- input.emissions <- variable <- max.reduction <- ctrl.name <- steepness <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_regions <- get_data(all_data, "emissions/A_regions") %>%
      select(region, SO2_name)
    L131.nonco2_tg_R_prc_S_S_Yh <- get_data(all_data, "L131.nonco2_tg_R_prc_S_S_Yh")
    A32.max_reduction <- get_data(all_data, "emissions/A32.max_reduction")
    A32.steepness <- get_data(all_data, "emissions/A32.steepness")

    # ===================================================
    # L232.nonco2_prc: Pollutant emissions for energy technologies in all regions
    # Formatting and rounding L131.nonco2_tg_R_prc_S_S_Yh
    L232.nonco2_prc <- L131.nonco2_tg_R_prc_S_S_Yh %>%
      ungroup() %>%
      filter(year %in% emissions.MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(A_regions, by = "region") %>%
      # Rename to regional SO2
      mutate(Non.CO2 = if_else(Non.CO2 == "SO2", SO2_name, Non.CO2)) %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, input.emissions = value) %>%
      mutate(input.emissions = round(input.emissions, emissions.DIGITS_EMISSIONS))

    # L232.max_reduction: write out maximum reductions to all regions
    L232.max_reduction <- A32.max_reduction %>%
      gather(variable, max.reduction, -supplysector, -subsector, - stub.technology) %>%
      # Repeat reductions for all regions
      repeat_add_columns(GCAM_region_names %>% select(region)) %>%
      left_join_error_no_match(A_regions, by = "region") %>%
      # Rename to regional SO2
      mutate(Non.CO2 = if_else(variable == "SO2", SO2_name, variable)) %>%
      select(region, supplysector, subsector, stub.technology, Non.CO2, max.reduction)

    # L232.nonco2_max_reduction: maximum reduction for energy technologies in all regions
    L232.nonco2_max_reduction <- L232.nonco2_prc %>%
      select(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      repeat_add_columns(tibble(year = as.integer(MODEL_BASE_YEARS))) %>%
      mutate(ctrl.name = "GDP_control") %>%
      # Use left_join because L232.max_reduction is littered with NA values
      left_join(L232.max_reduction, by = c("region", "supplysector", "subsector", "stub.technology", "Non.CO2")) %>%
      na.omit() %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, ctrl.name, max.reduction)

    # L232.steepness: write out steepness to all regions
    L232.steepness <- A32.steepness %>%
      gather(variable, steepness, -supplysector, -subsector, - stub.technology) %>%
      # Repeat steepness for all regions
      repeat_add_columns(GCAM_region_names %>% select(region)) %>%
      left_join_error_no_match(A_regions, by = "region") %>%
      # Rename to regional SO2
      mutate(Non.CO2 = if_else(variable == "SO2", SO2_name, variable)) %>%
      select(region, supplysector, subsector, stub.technology, Non.CO2, steepness)

    # L232.nonco2_steepness: steepness of reduction for energy technologies in all regions
    L232.nonco2_steepness <- L232.nonco2_max_reduction %>%
      select(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      repeat_add_columns(tibble(year = as.integer(MODEL_BASE_YEARS))) %>%
      mutate(ctrl.name = "GDP_control") %>%
      # Use left_join because L232.steepness is littered with NA values
      left_join(L232.steepness, by = c("region", "supplysector", "subsector", "stub.technology", "Non.CO2")) %>%
      na.omit() %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, ctrl.name, steepness)
    # ===================================================

    # Produce outputs
    L232.nonco2_prc %>%
      add_title("Historical GHG emissions by energy technology") %>%
      add_units("Tg") %>%
      add_comments("Filtered and rounded L131.nonco2_tg_R_prc_S_S_Yh") %>%
      add_legacy_name("L232.nonco2_prc") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "L131.nonco2_tg_R_prc_S_S_Yh") ->
      L232.nonco2_prc
    L232.nonco2_max_reduction %>%
      add_title("Maximum GHG reduction by energy technology, region, Non-CO2 gas, and base year") %>%
      add_units("Percentage reduction from baseline") %>%
      add_comments("Applied maximum reductions by technology and gas to all regions") %>%
      add_legacy_name("L232.nonco2_max_reduction") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions",
                     "L131.nonco2_tg_R_prc_S_S_Yh", "emissions/A32.max_reduction") ->
      L232.nonco2_max_reduction
    L232.nonco2_steepness %>%
      add_title("GHG reduction steepness by energy technology, region, Non-CO2 gas, and base year") %>%
      add_units("Unitless") %>%
      add_comments("Applied maximum steepness by technology and gas to all regions") %>%
      add_legacy_name("L232.nonco2_steepness") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions",
                     "L131.nonco2_tg_R_prc_S_S_Yh", "emissions/A32.steepness") ->
      L232.nonco2_steepness

    return_data(L232.nonco2_prc, L232.nonco2_max_reduction, L232.nonco2_steepness)
  } else {
    stop("Unknown command")
  }
}
