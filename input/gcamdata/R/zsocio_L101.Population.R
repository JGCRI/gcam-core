# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L101.Population
#'
#'  Stitch historical and future (by SSP) population
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.Pop_thous_ctry_Yh}, \code{L100.Pop_thous_SSP_ctry_Yfut},
#' \code{L101.Pop_thous_R_Yh}, \code{L101.Pop_thous_SSP_R_Yfut}
#' @details Connect future growth to historical population data and aggregate to GCAM regions
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter full_join if_else group_by left_join mutate order_by select summarize bind_rows
#' @importFrom tidyr complete nesting replace_na spread
#' @author STW May 2017 XZ 2024
module_socio_L101.Population <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      "L100.Pop_thous_ctry_Yh",
      "L100.Pop_thous_SSP_ctry_Yfut_raw")

  MODULE_OUTPUTS <-
    c("L101.Pop_thous_R_Yh",
      "L100.Pop_thous_SSP_ctry_Yfut",
      "L101.Pop_thous_SSP_R_Yfut",
      "L101.Pop_thous_Scen_R_Y")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    ## silence package check.
    Country <- value <- Maddison_ctry <- year <- pop <- Downscale_from <- ratio <-
      iso <- pop_scale <- pop2 <- pop.x <- pop.y <- pop_allocate <- X1900 <-
      X1950 <- X1850 <- X1800 <- X1750 <- X1700 <- pop_ratio <- scg <-
      idn <- mne <- Scenario <- Region <- Sex <- Year <- Value <- MODEL <-
      VARIABLE <- REGION <- SCENARIO <- UNIT <- scenario <- ratio_iso_ssp <-
      year <- GCAM_region_ID <- . <- country_name <- year.y <- year.x <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # (1) future population projections by country and SSP ----

    ## derive growth ratio  ----
    L100.Pop_thous_SSP_ctry_Yfut_raw %>%
      # need to have 2020 in the data as the initial point for interpolation
      # otherwise 2021:2024 could be the same with 2025 (rule = 2 below)
      # hard code 2020 here since socioeconomics.SSP_DB_BASEYEAR is now 2025
      # and base year is 2021
      complete(nesting(scenario, iso),
               year = c(2020:max(FUTURE_YEARS))) %>%
      filter(year %in% c(2020:max(FUTURE_YEARS))) %>%
      group_by(scenario, iso) %>%
      # Data is in five year intervals, so interpolate so get data for the base-year before calculating ratios
      mutate(pop = approx_fun(year, pop, rule = 2),
             ratio_iso_ssp = pop / pop[year == socioeconomics.FINAL_HIST_YEAR]) %>%  # Calculate population ratios to final historical year (2010), no units
      select(-pop) %>%
      filter(year >= socioeconomics.FINAL_HIST_YEAR) %>%
      # Third, project country population values using SSP ratios and final historical year populations.
      # Not all countries in the UN data are in SSP data. Create complete tibble with all UN countries & SSP years.
      ungroup() ->
      L100.Pop_thous_SSP_ctry_Yfut_growth_ratio

    ##  extract the final historical population from UN ----
    pop_final_hist <- filter(L100.Pop_thous_ctry_Yh, year == socioeconomics.FINAL_HIST_YEAR) %>%
      rename(pop_final_hist = value) %>%
      select(-year)

    ## apply the growth ratios
    ##  generate ratios of future population to base year for all SSPs
    # The ratios will be applied to the historical year populations so there are no jumps/inconsistencies.

    L100.Pop_thous_SSP_ctry_Yfut <-
      L100.Pop_thous_SSP_ctry_Yfut_growth_ratio %>%
      complete(scenario = unique(scenario),
               year = unique(year),
               iso = unique(L100.Pop_thous_ctry_Yh$iso)) %>%
      # For these countries, the ratio will be set to 1 (per the old data system).
      replace_na(list(ratio_iso_ssp = 1))  %>%
      ## Note: In the old data system, Taiwan is in this category and has constant population. Issue has been opened to deal with this later. ##
      right_join(pop_final_hist, by = "iso") %>% # Join with final historic period population
      mutate(value = pop_final_hist * ratio_iso_ssp) %>%  # Units are 1000 persons (UN 2010 value is in thousands)
      filter(year != socioeconomics.FINAL_HIST_YEAR) %>% # Keep only SSP future years
      select(-pop_final_hist, -ratio_iso_ssp)

    # (2) Aggregation to GCAM regions ----

    # Historical population by region
    L100.Pop_thous_ctry_Yh %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      filter(year %in% c(socioeconomics.UN_HISTORICAL_YEARS, socioeconomics.MADDISON_HISTORICAL_YEARS)) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      L101.Pop_thous_R_Yh

    # Future population in the SSP scenarios
    L100.Pop_thous_SSP_ctry_Yfut %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      filter(year %in% c(FUTURE_YEARS)) %>%
      group_by(scenario, GCAM_region_ID, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      L101.Pop_thous_SSP_R_Yfut


    # (3) Stitch together history and future population ----
    # Repeat history for all scenarios
    L101.Pop_thous_Scen_R_Y <-
      L101.Pop_thous_R_Yh %>%
      repeat_add_columns(tibble(scenario = unique(L101.Pop_thous_SSP_R_Yfut$scenario))) %>%
      bind_rows(L101.Pop_thous_SSP_R_Yfut)


    # Produce outputs ----
    L100.Pop_thous_SSP_ctry_Yfut %>%
      add_title("SSP population projections by country, 2021-2100") %>%
      add_units("thousand") %>%
      add_comments("Future population calculated as final historical year (2021) population times ratio of SSP future years to SSP 2010") %>%
      add_legacy_name("L100.Pop_thous_SSP_ctry_Yfut") %>%
      add_precursors(MODULE_INPUTS) ->
      L100.Pop_thous_SSP_ctry_Yfut

    L101.Pop_thous_R_Yh %>%
      add_title("Population by region over the historical time period") %>%
      add_units("thousand persons") %>%
      add_comments("Population by region over the historical time period") %>%
      add_legacy_name("L101.Pop_thous_R_Yh") %>%
      add_precursors(MODULE_INPUTS) ->
      L101.Pop_thous_R_Yh

    L101.Pop_thous_SSP_R_Yfut %>%
      add_title("Population by region and SSP in future periods") %>%
      add_units("thousand persons") %>%
      add_comments("Population by region and SSP in future periods") %>%
      add_legacy_name("L101.Pop_thous_SSP_R_Yfut") %>%
      add_precursors(MODULE_INPUTS) ->
      L101.Pop_thous_SSP_R_Yfut

    L101.Pop_thous_Scen_R_Y %>%
      add_title("Population by region over the historical time period and future periods by SSP") %>%
      add_units("thousand persons") %>%
      add_comments("Population by region over the historical time period and future periods by SSP") %>%
      add_legacy_name("L101.Pop_thous_Scen_R_Y") %>%
      add_precursors(MODULE_INPUTS) ->
      L101.Pop_thous_Scen_R_Y


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
