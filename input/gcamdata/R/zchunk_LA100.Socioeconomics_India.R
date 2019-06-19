#' module_gcam.india_LA100.Socioeconomics
#'
#' This chunk estimates state-wise per capita GDP, Popualtion, and  total GDP (at 90USD prices) in a time series.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.pcGDP_thous90usd_state_india}, \code{L100.GDP_mil90usd_state_india}, \code{L100.Pop_thous_state_india}. The corresponding file in the
#' @details This chunk includes a series of estimations. First is to fill the data gaps with linear interpolation,
#' followed by estimation of state-wise GDP at 90USD prices using shares and population.
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @import dplyr
#' @importFrom tidyr gather spread
#' @author Malyan_Ankur_CEEW
module_gcam.india_LA100.Socioeconomics <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/States_Subregions_Grid",
             FILE = "gcam-india/SE_SGDP_MOSPI_11INR",
             FILE = "gcam-india/SE_PopH_Census_1961_2011",
             FILE = "gcam-india/SE_PopP_IIASA_2005_2100",
             "L100.gdp_mil90usd_ctry_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.pcGDP_thous90usd_state_india",
             "L100.GDP_mil90usd_state_india",
             "L100.Pop_thous_state_india"))
  } else if(command == driver.MAKE) {

    state <- state_name <- year <- value  <- Area <- population <- iso <-
        share <- pop_ratio <- NULL      # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    States_Subregions_Grid         <- get_data(all_data, "gcam-india/States_Subregions_Grid")
    SE_SGDP_MOSPI_11INR            <- get_data(all_data, "gcam-india/SE_SGDP_MOSPI_11INR")
    SE_PopH_Census_1961_2011       <- get_data(all_data, "gcam-india/SE_PopH_Census_1961_2011")
    SE_PopP_IIASA_2005_2100        <- get_data(all_data, "gcam-india/SE_PopP_IIASA_2005_2100")
    L100.gdp_mil90usd_ctry_Yh      <- get_data(all_data, "L100.gdp_mil90usd_ctry_Yh")


    #Reshaping States and subregion files
    States_Subregions_Grid <- select(States_Subregions_Grid, state, state_name)

    #Interpolating the GDP data to fill the data gaps
    SE_SGDP_MOSPI_11INR %>%
      gather_years %>%
      complete(nesting(state, state_name), year = HISTORICAL_YEARS) %>%
      group_by(state, state_name) %>%
      mutate(value = approx_fun(year, value, rule = 2)) ->
      SE_SGDP_MOSPI_11INR

    #Estimating the shares of state GDP
    SE_SGDP_MOSPI_11INR %>%
      ungroup %>%
      group_by(year) %>%
      mutate(share = value/sum(value)) %>%
      select(-value)-> L100.GDPstate_shares

    #Estimating state wise GDP @1990USD prices
    ##FIltering the data for India
    L100.gdp_mil90usd_ctry_Yh %>%
      filter(iso == "ind") %>% select(year, total = value) ->  L100.gdp_mil90usd_India

    ##Estimation of state GDP using L100.GDPstate_shares
    L100.GDPstate_shares %>%
      left_join_error_no_match(L100.gdp_mil90usd_India, by = "year") %>%
      mutate(value = share *total) %>%
      select(-total, -share, -state_name) %>%
      ungroup %>%
      add_title("GDP by state") %>%
      add_units("million 1990 USD") %>%
      add_comments("State wise GDP Millions@1990USD prices from 1971 to 2010") %>%
      add_precursors("L100.gdp_mil90usd_ctry_Yh",
                     "gcam-india/SE_SGDP_MOSPI_11INR",
                     "gcam-india/SE_PopH_Census_1961_2011") %>%
      add_legacy_name("L100.GDP_mil90usd_state_india")->
      L100.GDP_mil90usd_state_india

    #Reshaping historical population
    SE_PopH_Census_1961_2011 %>%
      gather_years %>%
      PH_year_value_historical %>%
      rename (population = value)->
      SE_PopH_Census_1961_2011

    #Estimating state wise GDP per capita
    L100.GDP_mil90usd_state_india %>%
      left_join(SE_PopH_Census_1961_2011, by = c("state", "year")) %>%
      mutate(value = value * CONV_MIL_THOUS / population) %>%
      select(-population, -state_name) %>%
      add_title("Per-capita GDP by state") %>%
      add_units("thousand 1990 USD per capita") %>%
      add_comments("State wise GDP per capita thousands@1990USD from 1971 to 2010") %>%
      add_precursors("L100.GDP_mil90usd_state_india") %>%
      add_legacy_name("L100.pcGDP_thous90usd_state_india")->
      L100.pcGDP_thous90usd_state_india

    #Future Population by scenario. Right now just one scenario.
    ##Estiamting the state wise population ratio
    SE_PopP_IIASA_2005_2100 %>%
      gather_years(value_col = "population") %>%
      mutate(population = as.numeric(population)) %>%
      complete(nesting(state), year = c(socioeconomics.FINAL_HIST_YEAR, FUTURE_YEARS)) %>%
      group_by(state) %>%
      mutate(population = approx_fun(year, population)) %>%
      arrange(state, year) %>%
      mutate(pop_ratio = population / first(population)) %>%
      arrange(state, year) %>%
      ungroup %>%
      select(-state_name, -population) ->
      L100.Pop_ratio_state

    #Starting from end of history, projecting state populations into future
    SE_PopH_Census_1961_2011 %>%
    filter(year == max(HISTORICAL_YEARS)) %>%
      select(-year) %>%
      right_join(L100.Pop_ratio_state, by = c("state")) %>%
      filter(year > max(HISTORICAL_YEARS)) %>%
      mutate(population = population * pop_ratio) %>%
      bind_rows(SE_PopH_Census_1961_2011) %>%
      mutate(value = population * CONV_ONES_THOUS) %>%
      mutate(population = round(population, 0)) %>%
      select(-population, -pop_ratio, -state_name) %>%
      arrange(state, year) %>%
      ungroup %>%
      add_title("Population by state") %>%
      add_units("thousand persons") %>%
      add_comments("State populations (in thousands) from end of history (2011) projected into future till 2100") %>%
      add_precursors("L100.gdp_mil90usd_ctry_Yh",
                     "gcam-india/SE_SGDP_MOSPI_11INR",
                     "gcam-india/SE_PopP_IIASA_2005_2100",
                     "gcam-india/States_Subregions_Grid") %>%
      add_legacy_name("L100.Pop_thous_state_india")->
      L100.Pop_thous_state_india

    return_data(L100.pcGDP_thous90usd_state_india, L100.GDP_mil90usd_state_india, L100.Pop_thous_state_india)
  } else {
    stop("Unknown command")
  }
}

