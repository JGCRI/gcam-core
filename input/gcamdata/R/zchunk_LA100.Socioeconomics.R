# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LA100.Socioeconomics
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.pcGDP_thous90usd_state}, \code{L100.GDP_mil90usd_state}, \code{L100.Pop_thous_state}. The corresponding file in the
#' original data system was \code{LA100.Socioeconomics.R} (gcam-usa level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @importFrom dplyr arrange bind_rows bind_rows filter first group_by left_join mutate rename right_join select ungroup
#' @importFrom tidyr complete nesting
#' @author BBL
module_gcamusa_LA100.Socioeconomics <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/Census_pop",
             FILE = "gcam-usa/BEA_GDP_87_96_97USD_state",
             FILE = "gcam-usa/BEA_GDP_97_18_12USD_state",
             FILE = "gcam-usa/NCAR_SSP2_pop_state",
             FILE = "gcam-usa/AEO_2019_regional_pcGDP_ratio",
             "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.pcGDP_thous90usd_state",
             "L100.GDP_mil90usd_state",
             "L100.Pop_thous_state"))
  } else if(command == driver.MAKE) {

    state <- state_name <- year <- value <- Fips <- Area <- population <- iso <- share <-
      pop_ratio <- pop <- State <- State_FIPS <- SSP <- growth_rate_hist <-
      growth_rate_SSP2 <- growth_rate <- lag_pop <- time <- pcGDPratio <- subregion9 <-
      GCAM_region_ID <- scenario <- GDP <- lag_GDP <- NULL
    # silence package check

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    Census_pop <- get_data(all_data, "gcam-usa/Census_pop") %>%
      gather_years("pop")
    BEA_GDP_97_18_12USD_state <- get_data(all_data, "gcam-usa/BEA_GDP_97_18_12USD_state")
    BEA_GDP_87_96_97USD_state <- get_data(all_data, "gcam-usa/BEA_GDP_87_96_97USD_state")
    NCAR_SSP2_pop_state <- get_data(all_data, "gcam-usa/NCAR_SSP2_pop_state")
    AEO_2019_regional_pcGDP_ratio <- get_data(all_data, "gcam-usa/AEO_2019_regional_pcGDP_ratio")
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")

    # ===================================================
    # Data Processing

    # Population time series
    # L100.Pop_USA: Historical population by state from the U.S. Census Bureau
    Census_pop %>%
      mutate(pop = round(pop * CONV_ONES_THOUS, socioeconomics.POP_DIGITS)) -> L100.Pop_USA

    # L100.Pop_SSP2_USA: Updated future year populations for GCAM USA based on
    # 2018 historical data and NCAR downscaled SSP2 data
    # Calculate historical (2010-2018) population growth rates by state
    L100.Pop_USA %>%
      filter(year %in% c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      complete(nesting(state), year = c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      mutate(growth_rate_hist = (pop / lag(pop)) ^ (1 / (year - lag(year))) - 1) %>%
      filter(year >= max(HISTORICAL_YEARS)) -> L100.Pop_future_temp

    # Calculate SSP2 state-level population growth rates
    states_subregions %>%
      select(state, state_name) %>%
      left_join_error_no_match(NCAR_SSP2_pop_state %>%
                                 rename(state_name = State),
                               by = c("state_name")) %>%
      select(-state_name, -State_FIPS, -SSP) %>%
      gather_years("pop") %>%
      # converting people to thousands of people
      mutate(pop = pop * CONV_ONES_THOUS) %>%
      group_by(state) %>%
      complete(nesting(state), year = c(FUTURE_YEARS)) %>%
      mutate(pop = approx_fun(year, pop),
             growth_rate_SSP2 = (pop / lag(pop)) ^ (1 / (year - lag(year))) - 1) %>%
      ungroup() %>%
      filter(year >= gcamusa.SE_NEAR_TERM_YEAR) -> L100.Pop_GR_SSP2

    # Interpolate between historical 2010-2018 growth rates to NCAR 2030 growth rates
    L100.Pop_future_temp %>%
      # left_join_error_no_match thorws error because of NAs in new columns
      # this is becuase some years are (intentionally) missing from RHS
      # thus we use left_join instead
      left_join(L100.Pop_GR_SSP2 %>%
                  select(-pop),
                by = c("state", "year")) %>%
      group_by(state) %>%
      mutate(growth_rate = if_else(is.na(growth_rate_hist), growth_rate_SSP2, growth_rate_hist),
             growth_rate = approx_fun(year, growth_rate)) %>%
      ungroup() %>%
      select(-growth_rate_hist, -growth_rate_SSP2) -> L100.Pop_GR

    L100.Pop_GR %>%
      distinct(year) %>%
      filter(year != min(year)) -> L100.Pop_GR_years
    pop_years <- unique(L100.Pop_GR_years$year)

    for (y in pop_years) {
      # Calculate revised population
      L100.Pop_GR %>%
        group_by(state) %>%
        mutate(time = year - lag(year, n = 1L),
               lag_pop = lag(pop, n = 1L)) %>%
        ungroup() %>%
        filter(year == y) %>%
        mutate(pop = lag_pop * ((1 + growth_rate) ^ time)) -> L100.Pop_GR_temp

      # Add back into table
      L100.Pop_GR %>%
        filter(year != y) %>%
        bind_rows(L100.Pop_GR_temp %>%
                    select(-time, -lag_pop)) %>%
        mutate(year = as.numeric(year)) %>%
        arrange(state, year) -> L100.Pop_GR

    }

    L100.Pop_USA %>%
      bind_rows(L100.Pop_GR %>%
                  filter(!(year %in% L100.Pop_USA$year)) %>%
                  select(-growth_rate)) %>%
      mutate(pop = round(pop, socioeconomics.POP_DIGITS)) %>%
      rename(value = pop) %>%
      arrange(state, year) -> L100.Pop_thous_state


    # Historical per-capita GDP time series
    BEA_GDP_87_96_97USD_state %>%
      select(-Fips) %>%
      gather_years %>%
      PH_year_value_historical %>%
      group_by(Area) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      mutate(value = value * gdp_deflator(1990, 1997)) -> L100.GDP_87_96_90USD_state

    BEA_GDP_97_18_12USD_state %>%
      select(-Fips) %>%
      gather_years %>%
      mutate(value = value * gdp_deflator(1990, 2012)) -> L100.GDP_97_18_90USD_state

    L100.GDP_87_96_90USD_state %>%
      bind_rows(L100.GDP_97_18_90USD_state) %>%
      left_join_error_no_match(states_subregions %>%
                                 select(state, state_name),
                               by = c("Area" = "state_name")) %>%
      select(state, year, value) -> L100.GDP_hist_90USD_state

    L100.GDP_hist_90USD_state %>%
      left_join_error_no_match(L100.Pop_USA, by = c("state", "year")) %>%
      # note:  million $ / thousand persons = thousand $ / person
      mutate(value = value / pop) %>%
      select(-pop) ->
      L100.pcGDP_thous90usd_state


    # GDP time series (historical and future)
    # Labor productivity growth is calculated from the change in per-capita GDP ratio in each time period
    # Calculate the growth rate in per-capita GDP

    L100.pcGDP_thous90usd_state %>%
      group_by(state) %>%
      mutate(pcGDPratio = value / lag(value)) %>%
      ungroup() %>%
      select(state, year, pcGDPratio) -> L100.pcGDPratio_state_hist

    AEO_2019_regional_pcGDP_ratio %>%
      # left_join_error_no_match throws error because rows are duplicated,
      # which is intended because we are mapping values from 9 census
      # divisions to 50 states + DC.  Thus, left_join is used.
      left_join(states_subregions %>%
                  select(state, subregion9),
                by = c("region" = "subregion9")) %>%
      select(state, year, pcGDPratio) -> L100.pcGDPratio_state_AEO

    L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(GCAM_region_ID == gcam.USA_CODE,
             scenario == "gSSP2") %>%
      group_by(GCAM_region_ID, scenario) %>%
      mutate(pcGDPratio = value / lag(value)) %>%
      ungroup() %>%
      repeat_add_columns(tibble::tibble(state = gcamusa.STATES)) %>%
      select(state, year, pcGDPratio) -> L100.pcGDPratio_state_gSSP2

    # Combine annual growth rates into a single table
    L100.pcGDPratio_state_hist %>%
      bind_rows(L100.pcGDPratio_state_AEO %>%
                  # In order to smoothen transitions in GDP growth rate, we iterpolate between
                  # historical (2010-2018 growth) rates to AEO 2030 growth rates
                  # Thus we filter for values at or beyond 2030
                  filter(year >= gcamusa.SE_NEAR_TERM_YEAR),
                L100.pcGDPratio_state_gSSP2 %>%
                  # In order to smoothen transitions in GDP growth rate, we iterpolate between
                  # 2050 state-level values to the USA-region value in 2100
                  # Thus we filter for only the 2100 value here
                  filter(year == max(FUTURE_YEARS))) %>%
      # Smoothen transitions in labor productivity growth rate
      complete(nesting(state), year = c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      filter(year != min(HISTORICAL_YEARS)) %>%
      group_by(state) %>%
      mutate(pcGDPratio = round(approx_fun(year, pcGDPratio), socioeconomics.LABOR_PRODUCTIVITY_DIGITS)) %>%
      ungroup() -> L100.pcGDPratio_state

    # Translate time series of per-capita GDP ratio back into GDP time series
    # Calculate state GDP from parameters
    # Combine parameters into single table
    L100.GDP_hist_90USD_state %>%
      select(state, year, GDP = value) %>%
      complete(nesting(state), year = c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      left_join_error_no_match(L100.Pop_thous_state %>%
                                 rename(pop = value),
                               by = c("state", "year")) %>%
      # left_join(L100.Pop_thous_state, by = c("state", "year")) %>%
      # left_join_error_no_match throws error because there are no per-capita GDP ratios the first year (1971)
      # thus, using left_join
      left_join(L100.pcGDPratio_state, by = c("state", "year")) -> L100.GDP_state

    L100.GDP_state %>%
      filter(is.na(GDP)) %>%
      distinct(year) %>%
      unique %>%
      unlist -> L100.GDP_years

    for (y in L100.GDP_years) {

      # Calculate GDP
      L100.GDP_state %>%
        filter(year %in% c(y, y - 1)) %>%
        group_by(state) %>%
        mutate(lag_pop = lag(pop),
               lag_GDP = lag(GDP)) %>%
        ungroup() %>%
        filter(year == y) %>%
        mutate(GDP = pop * pcGDPratio * (lag_GDP / lag_pop)) -> L100.GDP_state_temp

      # Add back into table
      L100.GDP_state %>%
        filter(year != y) %>%
        bind_rows(L100.GDP_state_temp %>%
                    select(-lag_pop, -lag_GDP)) %>%
        mutate(year = as.integer(year)) %>%
        arrange(state, year) -> L100.GDP_state

    }

    L100.GDP_mil90usd_state <- L100.GDP_state %>%
      select(state, year, value = GDP)

    # ===================================================

    # Produce outputs

    L100.Pop_thous_state %>%
      add_title("Population by state") %>%
      add_units("thousand persons") %>%
      add_comments("State populations from end of history projected into future") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/Census_pop",
                     "gcam-usa/NCAR_SSP2_pop_state") %>%
      add_legacy_name("L100.Pop_thous_state") ->
      L100.Pop_thous_state

    L100.pcGDP_thous90usd_state %>%
      add_title("Per-capita GDP by state") %>%
      add_units("thousand 1990 USD per capita") %>%
      add_comments("") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/Census_pop",
                     "gcam-usa/BEA_GDP_87_96_97USD_state",
                     "gcam-usa/BEA_GDP_97_18_12USD_state") %>%
      add_legacy_name("L100.pcGDP_thous90usd_state") ->
      L100.pcGDP_thous90usd_state

    L100.GDP_mil90usd_state %>%
      add_title("GDP by state") %>%
      add_units("million 1990 USD") %>%
      add_comments("") %>%
      same_precursors_as(L100.pcGDP_thous90usd_state) %>%
      add_precursors("gcam-usa/NCAR_SSP2_pop_state",
                     "gcam-usa/AEO_2019_regional_pcGDP_ratio",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      add_legacy_name("L100.GDP_mil90usd_state") ->
      L100.GDP_mil90usd_state

    return_data(L100.pcGDP_thous90usd_state, L100.GDP_mil90usd_state, L100.Pop_thous_state)
  } else {
    stop("Unknown command")
  }
}
