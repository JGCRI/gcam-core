#' module_gcamindia_LA100.socioeconomics
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
#'
#'
module_gcamindia_LA100.socioeconomics <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/india_states_subregions",
             FILE = "gcam-india/A10.SE_SGDP_MOSPI_11INR",
             FILE = "gcam-india/A10.SE_PopH_Census_1961_2011",
             FILE = "gcam-india/A10.SE_PopP_IIASA_2005_2100",
             "L100.gdp_mil90usd_ctry_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.pcGDP_thous90usd_state_india",
             "L100.GDP_mil90usd_state_india",
             "L100.Pop_thous_state_india"))
  } else if(command == driver.MAKE) {

    state <- state_name <- year <- value  <- Area <- population <- iso <-
        share <- pop_ratio <- total <- NULL      # silence package check.

    all_data <- list(...)[[1]]

    # ===================================================

    # Load required inputs
    india_states_subregions         <- get_data(all_data, "gcam-india/india_states_subregions")
    A10.SE_SGDP_MOSPI_11INR            <- get_data(all_data, "gcam-india/A10.SE_SGDP_MOSPI_11INR")
    A10.SE_PopH_Census_1961_2011       <- get_data(all_data, "gcam-india/A10.SE_PopH_Census_1961_2011")
    A10.SE_PopP_IIASA_2005_2100        <- get_data(all_data, "gcam-india/A10.SE_PopP_IIASA_2005_2100")
    L100.gdp_mil90usd_ctry_Yh      <- get_data(all_data, "L100.gdp_mil90usd_ctry_Yh")

    # ===================================================

    #L100.gdp_mil90usd_India : State wise GDP at 1990 USD from 1971 to 2010
    #Reshaping States and subregion files
    india_states_subregions <- select(india_states_subregions, state, state_name)

    #Interpolating the GDP data to fill the data gaps and estimating the shares of state GDP
    A10.SE_SGDP_MOSPI_11INR %>%
      gather_years %>%
      complete(nesting(state, state_name), year = HISTORICAL_YEARS) %>%
      group_by(state, state_name) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup %>%
      group_by(year) %>%
      mutate(share = value / sum(value)) %>%
      select(-value) %>%
      ungroup ->
      L100.GDPstate_shares

    #Estimating state wise GDP @1990USD prices
    ##FIltering the data for India
    L100.gdp_mil90usd_ctry_Yh %>%
      filter(iso == "ind") %>%
      select(year, total = value) ->
      L100.gdp_mil90usd_India

    ##Estimation of state GDP using L100.GDPstate_shares
    L100.GDPstate_shares %>%
      left_join_error_no_match(L100.gdp_mil90usd_India, by = "year") %>%
      mutate(value = share * total) %>%
      select(-total, -share, -state_name) ->
      L100.GDP_mil90usd_state_india


    #L100.pcGDP_thous90usd_state_india : State wise per capita GDP at 1990 USD from 1971 to 2010
    #Reshaping historical population
    A10.SE_PopH_Census_1961_2011 %>%
      gather_years %>%
      PH_year_value_historical %>%
      rename(population = value) ->
      A10.SE_PopH_Census_1961_2011

    #Estimating state wise GDP per capita
    L100.GDP_mil90usd_state_india %>%
      left_join(A10.SE_PopH_Census_1961_2011, by = c("state", "year")) %>%
      mutate(value = value * CONV_MIL_THOUS / population) %>%
      select(-population, -state_name) ->
      L100.pcGDP_thous90usd_state_india


    #L100.Pop_thous_state_india : State populations (in thousands) from end of history (2011) projected into future till 2100

    #Method of Estimation:
    #The value of first historical year (2010) of each state is considered as the reference year for that particular state for the future population (data by IIASA) and based on 2010 values
    #the population ratios of future years (from 2011 to 2100) has been estimated. The estimated ratios are applied on the 2010 value of historical census data to make
    #IIASA values uniform with respect to the census values.

    ##Estiamting the state wise population ratio
    A10.SE_PopP_IIASA_2005_2100 %>%
      gather_years(value_col = "population") %>%
      mutate(population = as.numeric(population)) %>%
      #completing the years from 2011 to 2100
      complete(nesting(state), year = c(socioeconomics.FINAL_HIST_YEAR, FUTURE_YEARS)) %>%
      group_by(state) %>%
      mutate(population = approx_fun(year, population)) %>%
      arrange(state, year) %>%
      #estimating population share using 2010 as the reference
      mutate(pop_ratio = population / population[year == socioeconomics.FINAL_HIST_YEAR]) %>%
      ungroup %>%
      select(-state_name, -population) ->
      L100.Pop_ratio_state

    #Starting from end of history, projecting state populations into future
    A10.SE_PopH_Census_1961_2011 %>%
    filter(year == max(HISTORICAL_YEARS)) %>%
      select(-year) %>%
      right_join(L100.Pop_ratio_state, by = c("state")) %>%
      #filtering the data after 2010
      filter(year > max(HISTORICAL_YEARS)) %>%
      #applying the ratios on the population and rounding off the value
      mutate(population = round((population * pop_ratio), socioeconomics.POP_DIGITS)) %>%
      bind_rows(A10.SE_PopH_Census_1961_2011) %>%
      #converting the values into thousands
      mutate(value = population * CONV_ONES_THOUS) %>%
      select(-population, -pop_ratio, -state_name) %>%
      arrange(state, year) %>%
      ungroup ->
      L100.Pop_thous_state_india

    # ===================================================

    #Produce Outputs
    L100.GDP_mil90usd_state_india %>%
      add_title("GDP by state") %>%
      add_units("million 1990 USD") %>%
      add_comments("State wise GDP Millions_1990USD prices from 1971 to 2010") %>%
      add_precursors("L100.gdp_mil90usd_ctry_Yh",
                     "gcam-india/A10.SE_SGDP_MOSPI_11INR",
                     "gcam-india/A10.SE_PopH_Census_1961_2011") %>%
      add_legacy_name("L100.GDP_mil90usd_state_india")->
      L100.GDP_mil90usd_state_india

    L100.pcGDP_thous90usd_state_india %>%
      add_title("Per-capita GDP by state") %>%
      add_units("thousand 1990 USD per capita") %>%
      add_comments("State wise GDP per capita thousands at 1990USD from 1971 to 2010") %>%
      add_precursors("gcam-india/A10.SE_PopH_Census_1961_2011") %>%
      same_precursors_as("L100.GDP_mil90usd_state_india") %>%
      add_legacy_name("L100.pcGDP_thous90usd_state_india") ->
      L100.pcGDP_thous90usd_state_india

    L100.Pop_thous_state_india %>%
      add_title("Population by state") %>%
      add_units("thousand persons") %>%
      add_comments("State populations (in thousands) from end of history (2011) projected into future till 2100") %>%
      add_precursors("gcam-india/A10.SE_SGDP_MOSPI_11INR",
                     "gcam-india/A10.SE_PopP_IIASA_2005_2100",
                     "gcam-india/india_states_subregions") %>%
      add_legacy_name("L100.Pop_thous_state_india") ->
      L100.Pop_thous_state_india

    return_data(L100.pcGDP_thous90usd_state_india, L100.GDP_mil90usd_state_india, L100.Pop_thous_state_india)
  } else {
    stop("Unknown command")
  }
}

