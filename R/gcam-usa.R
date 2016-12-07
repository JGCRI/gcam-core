# gcam-usa.R




#' module_gcam-usa_LA100.Socioeconomics
#'
#' Construct the \code{gcam-usa} data structures.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}.
#' @author BBL
#' @export
`module_gcam-usa_LA100.Socioeconomics` <- function(command, ...) {
  if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.pcGDP_thous90usd_state",
             "L100.GDP_mil90usd_state",
             "L100.Pop_thous_state"))
  } else if(command == driver.DECLARE_INPUTS) {
    return(c("gcam-usa/states_subregions",
             "gcam-usa/BEA_pcGDP_09USD_state",
             "gcam-usa/BEA_pcGDP_97USD_state",
             "gcam-usa/Census_pop_hist",
             "gcam-usa/PRIMA_pop",
             "L100.gdp_mil90usd_ctry_Yh"))
  } else if(command == driver.MAKE) {
    `gcam-usa_LA100.Socioeconomics_makedata`(...)
  } else {
    stop("Unknown command")
  }
}


#' gcam-usa_LA100.Socioeconomics_makedata
#'
#' @param all_data A named list, holding all data system products so far
#' @return A named list with all gcam-usa data.
#' @importFrom tibble tibble
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select group_by
#' @importFrom tidyr gather spread
#' @export
`gcam-usa_LA100.Socioeconomics_makedata` <- function(all_data) {

  # printlog( "Historical GDP and per-capita GDP by state" )
  #
  # # -----------------------------------------------------------------------------
  # # 1. Read files

  states_subregions         <- get_data(all_data, "gcam-usa/states_subregions")
  BEA_pcGDP_09USD_state     <- get_data(all_data, "gcam-usa/BEA_pcGDP_09USD_state")
  BEA_pcGDP_97USD_state     <- get_data(all_data, "gcam-usa/BEA_pcGDP_97USD_state")
  Census_pop_hist           <- get_data(all_data, "gcam-usa/Census_pop_hist")
  PRIMA_pop                 <- get_data(all_data, "gcam-usa/PRIMA_pop")
  L100.gdp_mil90usd_ctry_Yh <- get_data(all_data, "L100.gdp_mil90usd_ctry_Yh")

  # # Bind the two per-capita GDP data frames to get a continuous time series, and extrapolate
  # printlog( "Building historical per-capita GDP time series" )
  # printlog( "NOTE: only using these datasets to disaggregate national GDP totals, so no need to convert units or" )
  # printlog( "estimate what the actual per-capita GDP trends were in the pre-1987 years that have all missing values")

  # GDP97_years <- as.numeric( substr( names( BEA_pcGDP_97USD_state )[ names( BEA_pcGDP_97USD_state ) %in% X_historical_years ], 2, 5 ) )
  # BEA_pcGDP_97USD_state <- gcam_interp( BEA_pcGDP_97USD_state, GDP97_years, rule = 2 )
  # BEA_pcGDP_97USD_state$state <- states_subregions$state[ match( BEA_pcGDP_97USD_state$Area, states_subregions$state_name ) ]

  # TODO: I bet we'll do this a lot: melt, filter to historic, interpolate
  # Will probably become its own function
  BEA_pcGDP_97USD_state %>%
    gather(year, value, -Fips, -Area) %>%
    mutate(year = as.numeric(year),
           value = as.numeric(value)) %>%
    filter(year %in% HISTORICAL_YEARS) %>%
    group_by(Area) %>%
    mutate(value = approx_fun(year, value, rule = 2)) ->
    BEA_pcGDP_97USD_state

  BEA_pcGDP_09USD_state %>%
    gather(year, value, -Fips, -Area) %>%
    mutate(year = as.numeric(year),
           value = as.numeric(value)) %>%
    filter(year %in% HISTORICAL_YEARS) ->
    BEA_pcGDP_09USD_state

  Census_pop_hist %>%
    gather(year, population, -state) %>%
    mutate(year = as.numeric(year),
           population = as.numeric(population)) %>%
    filter(year %in% HISTORICAL_YEARS) ->
    Census_pop_hist

  # L100.pcGDP_state_unscaled <- data.frame(
  #   BEA_pcGDP_97USD_state[ state ],
  #   BEA_pcGDP_97USD_state[ names( BEA_pcGDP_97USD_state  ) %in% X_historical_years & !names( BEA_pcGDP_97USD_state ) %in% names( BEA_pcGDP_09USD_state ) ],
  #   BEA_pcGDP_09USD_state[ names( BEA_pcGDP_09USD_state  ) %in% X_historical_years ] )
  # L100.GDP_state_unscaled <- L100.pcGDP_state_unscaled
  # L100.GDP_state_unscaled[ X_historical_years ] <- L100.pcGDP_state_unscaled[ X_historical_years ] * 1e-6 *    #avoiding "integer overflow" by dividing by a million
  #   Census_pop_hist[ match( L100.GDP_state_unscaled$state, Census_pop_hist$state ),
  #                    X_historical_years ]
  BEA_pcGDP_97USD_state %>%
    ungroup %>%
    filter(!year %in% unique(BEA_pcGDP_09USD_state$year)) %>%
    bind_rows(BEA_pcGDP_09USD_state) %>%
    left_join(states_subregions[c("state", "state_name")],
              by = c("Area" = "state_name")) %>%
    select(state, year, value) %>%
    left_join(Census_pop_hist, by = c("state", "year")) %>%
    mutate(value = value * 1e-6 * population) %>%
    arrange(state, year) %>%
    select(-population) ->
    L100.GDP_state_unscaled

  # L100.GDPshare_state <- L100.GDP_state_unscaled
  # L100.GDPshare_state[ X_historical_years ] <- sweep( L100.GDP_state_unscaled[ X_historical_years ],
  #                                                     2, colSums( L100.GDP_state_unscaled[ X_historical_years ] ), "/" )
  L100.GDP_state_unscaled %>%
    group_by(year) %>%
    mutate(share = value / sum(value)) %>%
    select(-value) ->
    L100.GDPshare_state

  # # Multiply the country-level GDP by the state shares
  # L100.GDP_mil90usd_state <- apportion_to_states(
  #   nation_data = subset( L100.gdp_mil90usd_ctry_Yh, iso == "usa" ),
  #   state_share_data = L100.GDPshare_state )
  L100.gdp_mil90usd_ctry_Yh %>%
    filter(iso == "usa") %>%
    right_join(L100.GDPshare_state, by = c("year")) %>%
    mutate(value = value * share) %>%
    select(-share, -iso) %>%
    add_dscomments(c("GDP by state", "Unit = million 1990 USD")) %>%
    add_dsflags(FLAG_LONG_NO_X_FORM) ->
    L100.GDP_mil90usd_state

  # L100.pcGDP_thous90usd_state <- L100.GDP_mil90usd_state
  # L100.pcGDP_thous90usd_state[ X_historical_years ] <-
  #   L100.GDP_mil90usd_state[ X_historical_years ] * conv_mil_thous / Census_pop_hist[
  #     match( L100.GDP_mil90usd_state$state, Census_pop_hist$state ),
  #     X_historical_years ]
  L100.GDP_mil90usd_state %>%
    left_join(Census_pop_hist, by = c("state", "year")) %>%
    mutate(value = value * CONV_MIL_THOUS / population) %>%
    select(-population) %>%
    add_dscomments(c("Per-capita GDP by state", "Unit = thousand 1990 USD per capita")) %>%
    add_dsflags(FLAG_LONG_NO_X_FORM) ->
    L100.pcGDP_thous90usd_state

  # # Future population by scenario. Right now just one scenario.
  # L100.Pop_ratio_state <- gcam_interp( PRIMA_pop, c( final_historical_year, future_years ) )
  # L100.Pop_ratio_state[ c( X_final_historical_year, X_future_years ) ] <-
  #   L100.Pop_ratio_state[ c( X_final_historical_year, X_future_years ) ] /
  #   L100.Pop_ratio_state[[ X_final_historical_year ]]
  # L100.Pop_ratio_state$state <- states_subregions$state[ match( L100.Pop_ratio_state$state, states_subregions$state_name ) ]
  PRIMA_pop %>%
    gather(year, population, -state) %>%
    mutate(year = as.numeric(year),
           population = as.numeric(population)) ->
    PRIMA_pop

  PRIMA_pop %>%
    filter(!year %in% c(max(HISTORICAL_YEARS), FUTURE_YEARS)) ->
    PRIMA_pop_old

  PRIMA_pop %>%
    filter(year %in% c(max(HISTORICAL_YEARS), FUTURE_YEARS)) %>%
    group_by(state) %>%
    mutate(population = approx_fun(year, population)) %>%
    arrange(state, year) %>%
    group_by(state) %>%
    mutate(pop_ratio = population / first(population)) %>%
#    bind_rows(PRIMA_pop_old) %>%
    arrange(state, year) %>%
    rename(state_name = state) %>%
    left_join(states_subregions[c("state", "state_name")], by = "state_name") %>%
    ungroup %>%
    select(-state_name, -population) ->
    L100.Pop_ratio_state

  # L100.Pop_thous_state <- data.frame(
  #   Census_pop_hist[ "state" ],
  #   Census_pop_hist[ X_historical_years ] * conv_ones_thous )
  # L100.Pop_thous_state[ X_future_years ] <- L100.Pop_thous_state[[ X_final_historical_year ]] *
  #   L100.Pop_ratio_state[ match( L100.Pop_thous_state$state, L100.Pop_ratio_state$state ),
  #                         X_future_years ]
  Census_pop_hist %>%
    filter(year == max(HISTORICAL_YEARS)) %>%
    select(-year) %>%
    right_join(L100.Pop_ratio_state, by = c("state")) %>%
    filter(year > max(HISTORICAL_YEARS)) %>%
    mutate(population = population * pop_ratio) %>%
    select(-pop_ratio) %>%
    bind_rows(Census_pop_hist) %>%
    mutate(population = population * CONV_ONES_THOUS) %>%
    arrange(state, year) %>%
    add_dsflags(FLAG_NO_OUTPUT) %>%
    add_dscomments(c("Population by state", "Unit = thousand persons")) ->
    L100.Pop_thous_state

  return_data(L100.pcGDP_thous90usd_state,
              L100.GDP_mil90usd_state,
              L100.Pop_thous_state)
}
