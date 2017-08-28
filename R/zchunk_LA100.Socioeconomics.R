#' module_gcam.usa_LA100.Socioeconomics
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
#' @import dplyr
#' @importFrom tidyr gather spread
#' @author BBL
module_gcam.usa_LA100.Socioeconomics <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/BEA_pcGDP_09USD_state",
             FILE = "gcam-usa/BEA_pcGDP_97USD_state",
             FILE = "gcam-usa/Census_pop_hist",
             FILE = "gcam-usa/PRIMA_pop",
             "L100.gdp_mil90usd_ctry_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.pcGDP_thous90usd_state",
             "L100.GDP_mil90usd_state",
             "L100.Pop_thous_state"))
  } else if(command == driver.MAKE) {

    state <- state_name <- year <- value <- Fips <- Area <- population <- iso <-
        share <- pop_ratio <- NULL      # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions         <- get_data(all_data, "gcam-usa/states_subregions")
    BEA_pcGDP_09USD_state     <- get_data(all_data, "gcam-usa/BEA_pcGDP_09USD_state")
    BEA_pcGDP_97USD_state     <- get_data(all_data, "gcam-usa/BEA_pcGDP_97USD_state")
    Census_pop_hist           <- get_data(all_data, "gcam-usa/Census_pop_hist")
    PRIMA_pop                 <- get_data(all_data, "gcam-usa/PRIMA_pop")
    L100.gdp_mil90usd_ctry_Yh <- get_data(all_data, "L100.gdp_mil90usd_ctry_Yh")

    # Build historical per-capita GDP time series
    # NOTE: only using these datasets to disaggregate national GDP totals, so no need to convert units or
    # estimate what the actual per-capita GDP trends were in the pre-1987 years that have all missing values

    # Reshape and interpolate input datasets
    states_subregions <- select(states_subregions, state, state_name)

    BEA_pcGDP_97USD_state %>%
      gather(year, value, -Fips, -Area) %>%
      PH_year_value_historical %>%
      group_by(Area) %>%
      mutate(value = approx_fun(year, value, rule = 2)) ->
      BEA_pcGDP_97USD_state

    BEA_pcGDP_09USD_state %>%
      gather(year, value, -Fips, -Area) %>%
      PH_year_value_historical ->
      BEA_pcGDP_09USD_state

    Census_pop_hist %>%
      gather(year, value, -state) %>%
      PH_year_value_historical %>%
      rename(population = value) ->
      Census_pop_hist

    # Bind the '97 and '09 GDP datasets to get a continuous time series
    BEA_pcGDP_97USD_state %>%
      ungroup %>%
      filter(!year %in% unique(BEA_pcGDP_09USD_state$year)) %>%
      bind_rows(BEA_pcGDP_09USD_state) %>%
      # merge with state name/codes
      left_join_error_no_match(states_subregions, by = c("Area" = "state_name")) %>%
      select(state, year, value) %>%
      # merge with census data, and compute total GDP (population * per capita GDP)
      left_join(Census_pop_hist, by = c("state", "year")) %>%
      mutate(value = value * 1e-6 * population) %>%
      arrange(state, year) %>%
      select(-population) %>%
      # compute by-state shares by year
      group_by(year) %>%
      mutate(share = value / sum(value)) %>%
      select(-value) ->
      L100.GDPshare_state

    # Multiply the country-level GDP by the state shares
    L100.gdp_mil90usd_ctry_Yh %>%
      filter(iso == "usa") %>%
      right_join(L100.GDPshare_state, by = c("year")) %>%
      mutate(value = value * share) %>%
      select(-share, -iso) %>%
      add_title("GDP by state") %>%
      add_units("million 1990 USD") %>%
      add_comments("") %>%
      add_precursors("L100.gdp_mil90usd_ctry_Yh",
                     "gcam-usa/BEA_pcGDP_97USD_state",
                     "gcam-usa/BEA_pcGDP_09USD_state",
                     "gcam-usa/Census_pop_hist") %>%
      add_legacy_name("L100.GDP_mil90usd_state") %>%
      # flag that this dataset is in different form from original
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L100.GDP_mil90usd_state

    # Compute per capita GDP by state
    L100.GDP_mil90usd_state %>%
      left_join(Census_pop_hist, by = c("state", "year")) %>%
      mutate(value = value * CONV_MIL_THOUS / population) %>%
      select(-population) %>%
      add_title("Per-capita GDP by state") %>%
      add_units("thousand 1990 USD per capita") %>%
      add_comments("") %>%
      add_precursors("L100.GDP_mil90usd_state") %>%
      add_legacy_name("L100.pcGDP_thous90usd_state") %>%
      # flag that this dataset is in different form from original
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L100.pcGDP_thous90usd_state

    # Future population by scenario. Right now just one scenario.
    PRIMA_pop %>%
      # reshape
      gather(year, population, -state) %>%
      mutate(year = as.numeric(year),
             population = as.numeric(population)) %>%
      # interpolate any missing data from end of history into future
      filter(year %in% c(max(HISTORICAL_YEARS), FUTURE_YEARS)) %>%
      group_by(state) %>%
      mutate(population = approx_fun(year, population)) %>%
      arrange(state, year) %>%
      # compute ratios (change from end of history)
      group_by(state) %>%
      mutate(pop_ratio = population / first(population)) %>%
      arrange(state, year) %>%
      rename(state_name = state) %>%
      left_join_error_no_match(states_subregions, by = "state_name") %>%
      ungroup %>%
      select(-state_name, -population) ->
      L100.Pop_ratio_state

    # Starting from end of history, project state populations into future
    Census_pop_hist %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      select(-year) %>%
      right_join(L100.Pop_ratio_state, by = c("state")) %>%
      filter(year > max(HISTORICAL_YEARS)) %>%
      mutate(population = population * pop_ratio) %>%
      bind_rows(Census_pop_hist) %>%
      mutate(value = population * CONV_ONES_THOUS) %>%
      select(-population, -pop_ratio) %>%
      arrange(state, year) %>%
      add_title("Population by state") %>%
      add_units("thousand persons") %>%
      add_comments("State populations from end of history projected into future") %>%
      add_precursors("L100.gdp_mil90usd_ctry_Yh",
                     "gcam-usa/BEA_pcGDP_97USD_state",
                     "gcam-usa/BEA_pcGDP_09USD_state",
                     "gcam-usa/PRIMA_pop",
                     "gcam-usa/states_subregions") %>%
      add_legacy_name("L100.Pop_thous_state") %>%
      # flag that this dataset is in different form from original
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L100.Pop_thous_state

    return_data(L100.pcGDP_thous90usd_state, L100.GDP_mil90usd_state, L100.Pop_thous_state)
  } else {
    stop("Unknown command")
  }
}



