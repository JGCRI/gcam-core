# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L2011.socioeconomics_update_USA
#'
#' Population and labor productivity updates for GCAM-USA.  2015 population & labor productivity updated to match
#' historical values.  Future year population projections updated based on NCAR downsalced SSP2 data. Labor
#' productivity growth rates from 2015 through 2040 harmonized with AEO assumptions. Labor productivity
#' growth rates post-2040 are linearly interpolated from state-level 2040 values to the USA-region value
#' in 2100, so that all states converge on a common labor productivity growth rate in 2100.
#' Finally, the USA-region’s population and labor productivity assumptions were updated to match the
#' sum-of-states population and GDP when running GCAM-USA.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2011.Pop_updated_USA}, \code{L2011.BaseGDP_updated_USA}, \code{L2011.LaborProductivity_updated_USA},
#' \code{L2011.Pop_national_updated_USA}, \code{L2011.BaseGDP_national_updated_USA}, \code{L2011.LaborProductivity_national_updated_USA}.
#' The corresponding file in the original data system was \code{L2011.socioeconomics_USA_update.R} (gcam-usa level2).
#' @details Updated state-level population and labor productivity for GCAM-USA.  Updated USA-region  population and labor productivity
#' to match the sum-of-states population and GDP when running GCAM-USA.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author MTB August 2018
module_gcamusa_L2011.socioeconomics_update_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/Census_pop_hist",
             FILE = "gcam-usa/Census_pop_10_15",
             FILE = "gcam-usa/BEA_GDP_87_96_97USD_state",
             FILE = "gcam-usa/BEA_GDP_97_16_09USD_state",
             FILE = "gcam-usa/AEO_2016_pop_regional",
             FILE = "gcam-usa/AEO_2016_GDP_regional",
             FILE = "gcam-usa/NCAR_SSP2_pop_state",
             "L201.LaborProductivity_SSP2",
             "L201.Pop_GCAMUSA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2011.Pop_updated_USA",
             "L2011.BaseGDP_updated_USA",
             "L2011.LaborProductivity_updated_USA",
             "L2011.Pop_national_updated_USA",
             "L2011.BaseGDP_national_updated_USA",
             "L2011.LaborProductivity_national_updated_USA"))
  } else if(command == driver.MAKE) {

    # silence package checks
    year <- region <- state <- Area <- totalPop <- pop <- pop_ratio <- GDP <- baseGDP <- pcGDP <- pcGDPratio <-
      laborproductivity <- time <- lag_pop <- lag_GDP <- iso <- growth <- timestep <- state_name <- Fips <-
      Year <- Quarter <- census_region <- lp2100 <- lp2040 <- State <- State_FIPS <- SSP <- growth_rate_SSP2 <-
      growth_rate_hist <- growth_rate <- subregion9 <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    Census_pop_hist <- get_data(all_data, "gcam-usa/Census_pop_hist") %>%
      gather_years("totalPop")
    Census_pop_10_15 <- get_data(all_data, "gcam-usa/Census_pop_10_15") %>%
      gather_years("totalPop")
    BEA_GDP_87_96_97USD_state <- get_data(all_data, "gcam-usa/BEA_GDP_87_96_97USD_state")
    BEA_GDP_97_16_09USD_state <- get_data(all_data, "gcam-usa/BEA_GDP_97_16_09USD_state")
    AEO_2016_pop_regional <- get_data(all_data, "gcam-usa/AEO_2016_pop_regional")
    AEO_2016_GDP_regional <- get_data(all_data, "gcam-usa/AEO_2016_GDP_regional")
    NCAR_SSP2_pop_state <- get_data(all_data, "gcam-usa/NCAR_SSP2_pop_state")
    L201.LaborProductivity_SSP2 <- get_data(all_data, "L201.LaborProductivity_SSP2")
    L201.Pop_GCAMUSA <- get_data(all_data, "L201.Pop_GCAMUSA")

    # ===================================================

    # L2011.Pop_USA: Historical population by state from the U.S. Census Bureau, through 2016
    # Merge historical population datasets
    Census_pop_hist %>%
      bind_rows(Census_pop_10_15 %>%
                  anti_join(Census_pop_hist, by = c("state", "year"))) %>%
      mutate(totalPop = round(totalPop * CONV_ONES_THOUS, socioeconomics.POP_DIGITS)) -> L2011.Pop_USA

    # L2011.Pop_updated_USA: Updated future year populations for GCAM USA based on
    # 2015 historical data and NCAR downsalced SSP2 data
    # Calculate historical (2010-2015) population growth rates by state
    L2011.Pop_USA %>%
      rename(region = state) %>%
      filter(year %in% MODEL_YEARS) %>%
      complete(nesting(region), year = c(MODEL_YEARS)) %>%
      mutate(growth_rate_hist = (totalPop / lag(totalPop)) ^ (1 / (year - lag(year))) - 1) %>%
      filter(year >= gcamusa.SE_HIST_YEAR) -> L2011.Pop_future_temp

    # Calculate SSP2 state-level population growth rates
    states_subregions %>%
      select(region = state, state_name) %>%
      left_join_error_no_match(NCAR_SSP2_pop_state %>%
                  rename(state_name = State),
                by = c("state_name")) %>%
      select(-state_name, -State_FIPS, -SSP) %>%
      gather_years("totalPop") %>%
      # converting people to thousands of people
      mutate(totalPop = totalPop * CONV_ONES_THOUS) %>%
      group_by(region) %>%
      complete(nesting(region), year = c(MODEL_FUTURE_YEARS)) %>%
      mutate(totalPop = approx_fun(year, totalPop),
             growth_rate_SSP2 = (totalPop / lag(totalPop)) ^ (1 / (year - lag(year))) - 1) %>%
      ungroup() %>%
      filter(year >= gcamusa.SE_NEAR_TERM_YEAR) -> L2011.Pop_GR_SSP2

    # Interpolate between historical 2010-2015 growth rates to NCAR 2030 growth rates
    L2011.Pop_future_temp %>%
      # left_join_error_no_match thorws error because of NAs in new columns
      # this is becuase some years are (intentionally) missing from RHS
      # thus we use left_join instead
      left_join(L2011.Pop_GR_SSP2 %>%
                  select(-totalPop),
                by = c("region", "year")) %>%
      group_by(region) %>%
      mutate(growth_rate = if_else(is.na(growth_rate_hist), growth_rate_SSP2, growth_rate_hist),
             growth_rate = approx_fun(year, growth_rate)) %>%
      ungroup() %>%
      select(-growth_rate_hist, -growth_rate_SSP2) -> L2011.Pop_GR

    L2011.Pop_GR %>%
      distinct(year) %>%
      filter(year != min(year)) -> L2011.Pop_GR_years
    pop_years <- unique(L2011.Pop_GR_years$year)

    for (y in pop_years) {

      # Calculate revised population
      L2011.Pop_GR %>%
        group_by(region) %>%
        mutate(time = year - lag(year, n = 1L),
               lag_pop = lag(totalPop, n = 1L)) %>%
        ungroup() %>%
        filter(year == y) %>%
        mutate(totalPop = lag_pop * ((1 + growth_rate) ^ time)) -> L2011.Pop_GR_temp

      # Add back into table
      L2011.Pop_GR %>%
        filter(year != y) %>%
        bind_rows(L2011.Pop_GR_temp %>%
                    select(-time, -lag_pop)) %>%
        mutate(year = as.numeric(year)) %>%
        arrange(region, year) -> L2011.Pop_GR

    }

    L2011.Pop_GR %>%
      select(-growth_rate) %>%
      mutate(totalPop = round(totalPop, socioeconomics.POP_DIGITS)) -> L2011.Pop_updated_USA

    # L2011.BaseGDP_USA: GCAM USA base GDP by state
    # NOTE: Bureau of Economic Analysis does not have state-level GDP data available for years prior to 1987.
    # Hence, 1987 state-level GDP are input as GCAM USA base GDP. Labor productivity growth rate for 1990 is
    # calculated as 15 year annualized growth rate off of the assumed 1975 GDP. This assumption implies that
    # our 1975 GDP numbers will be incorrect. However, GDP numbers from 1990 will be correct and match history.

    # Convert 1987 GDP to 1990$ to be input into the model as baseGDP.
    BEA_GDP_87_96_97USD_state %>%
      select(-Fips) %>%
      gather_years("GDP") %>%
      mutate(GDP = as.numeric(GDP),
             GDP = GDP * gdp_deflator(1990, 1997)) %>%
      filter(!is.na(GDP)) %>%
      # NOTE:  having these filters inside the same call [ filter(x, y) ] results in filtering for min(year)
      # in the original tbl_df passed to filter(), not min(year) in the tbl_df after NAs have been filtered out.
      # Thus, using two separate filter calls.
      filter(year == min(year)) %>%
      mutate(year = min(MODEL_BASE_YEARS)) %>%
      select(Area, year, GDP) -> L2011.GDP_state_1975

    L2011.GDP_state_1975 %>%
      mutate(baseGDP = round(GDP, socioeconomics.GDP_DIGITS)) %>%
      left_join_error_no_match(states_subregions %>%
                  select(region = state, state_name),
                by = c("Area" = "state_name")) %>%
      select(region, baseGDP) -> L2011.BaseGDP_updated_USA

    # L2011.pcGDP_state_2015: State per-capita GDP historical data series (through 2015)
    # Convert BEA data to 1990US$.
    BEA_GDP_87_96_97USD_state %>%
      select(-Fips) %>%
      gather_years("GDP") %>%
      mutate(GDP = as.numeric(GDP),
             GDP = GDP * gdp_deflator(1990, 1997)) %>%
      filter(!is.na(GDP)) -> L2011.GDP_state_1996

    BEA_GDP_97_16_09USD_state %>%
      select(-Fips) %>%
      gather_years("GDP") %>%
      mutate(GDP = as.numeric(GDP),
             GDP = GDP * gdp_deflator(1990, 2009)) -> L2011.GDP_state_2016

    # Bind historical datasets, join state abbreviations, join population data, convert to per-capita GDP (thousand 1990$ / person).
    # NOTE:  1975 per-capita GDP is 1987 GDP divided by 1975 population. As explained earlier, this does not affect 1990 numbers
    # since labor productivity growth rates are calculated off of the "wrong" 1975 GDP/capita to match true 1990 GDP.
    # Hence only 1975 GDP would be incorect and we don't care about that anyway.

    L2011.GDP_state_1975 %>%
      bind_rows(L2011.GDP_state_1996,
                L2011.GDP_state_2016) %>%
      mutate(GDP = round(GDP, socioeconomics.GDP_DIGITS)) %>%
      left_join_error_no_match(states_subregions %>%
                  select(state, state_name),
                by = c(Area = "state_name")) %>%
      select(state, year, GDP) %>%
      left_join_error_no_match(L2011.Pop_USA,
                by = c("state", "year")) %>%
      mutate(pcGDP = (GDP / totalPop) * CONV_MIL_THOUS) %>%
      select(state, year, pcGDP) %>%
      # Select model base years + 2015
      filter(year %in% c(MODEL_YEARS),
             year <= gcamusa.SE_HIST_YEAR) -> L2011.pcGDP_state_2015

    # L2011.pcGDP_AEO_reg_2040: Future per-capita GDP (through 2040), based on AEO data (thousand 2005$ / person).
    # AEO GDP data is past 12 months, presented quarterly. Hence we filter quarter 4 (Q4) because that represents
    # the annual GDP for a given year.

    AEO_2016_GDP_regional %>%
      gather(region, GDP, -c(Year, Quarter)) %>%
      filter(Quarter == 4) %>%
      mutate(region = gsub("Mid-Atlantic", "Middle Atlantic", region)) %>%
      select(region, GDP, year = Year) -> GDP_AEO_reg

    # AEO population data is past 12 months, presented quarterly. Filter for Q4.
    AEO_2016_pop_regional %>%
      gather(region, pop, -c(Year, Quarter)) %>%
      filter(Quarter == 4) %>%
      mutate(region = gsub("Mid-Atlantic", "Middle Atlantic", region)) %>%
      select(region, pop, year = Year) -> Pop_AEO_reg

    # Calculate per-capita GDP
    GDP_AEO_reg %>%
      left_join_error_no_match(Pop_AEO_reg, by = c("region", "year")) %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      mutate(pcGDP = (GDP / pop)) %>%
      select(region, year, pcGDP) -> L2011.pcGDP_AEO_reg_2040

    # L2011.LaborProductivity_USA_udpated: Labor productivity growth rate for GCAMUSA
    # Labor productivity growth is calculated from the change in per-capita GDP ratio in each time period
    # Calculate the growth rate in per-capita GDP

    L2011.pcGDP_state_2015 %>%
      group_by(state) %>%
      mutate(pcGDPratio = pcGDP / lag(pcGDP)) %>%
      ungroup() %>%
      select(state, year, pcGDPratio) -> L2011.pcGDPratio_state_2015

    L2011.pcGDP_AEO_reg_2040 %>%
      group_by(region) %>%
      mutate(pcGDPratio = pcGDP / lag(pcGDP)) %>%
      ungroup() %>%
      select(region, year, pcGDPratio) -> L2011.pcGDPratio_AEOreg_2040

    L2011.pcGDPratio_AEOreg_2040 %>%
      # left_join_error_no_match throws error because rows are duplicated,
      # which is intended because we are mapping values from 9 census
      # divisions to 50 states + DC.  Thus, left_join is used.
      left_join(states_subregions %>%
                                 select(state, subregion9),
                               by = c("region" = "subregion9")) %>%
      select(state, year, pcGDPratio) -> L2011.pcGDPratio_state_2040

    # Annualize the ratios to return annual growth rates

    L2011.pcGDPratio_state_2015 %>%
      group_by(state) %>%
      mutate(laborproductivity = (pcGDPratio ^ (1 / (year - lag(year)))) - 1) %>%
      ungroup() %>%
      select(state, year, laborproductivity) %>%
      filter(year != min(MODEL_BASE_YEARS)) -> L2011.LaborProductivity_USA_2015

    L2011.pcGDPratio_state_2040 %>%
      group_by(state) %>%
      mutate(laborproductivity = (pcGDPratio ^ (1 / (year - lag(year)))) - 1) %>%
      ungroup() %>%
      select(state, year, laborproductivity) %>%
      filter(year > gcamusa.SE_HIST_YEAR) -> L2011.LaborProductivity_USA_2040

    # Labor productivity post-2040 is linearly interpolated from 2040 state-level values to the
    # USA-region value in 2100

    L201.LaborProductivity_SSP2 %>%
      filter(region == gcam.USA_REGION,
             year == max(MODEL_FUTURE_YEARS)) %>%
      repeat_add_columns(tibble::tibble(state = gcamusa.STATES)) %>%
      select(state, year, laborproductivity) %>%
      bind_rows(L2011.LaborProductivity_USA_2040 %>%
                  filter(year == gcamusa.AEO_SE_YEAR) %>%
                  select(state, year, laborproductivity)) %>%
      group_by(state) %>%
      complete(nesting(state), year = c(MODEL_FUTURE_YEARS)) %>%
      mutate(laborproductivity = approx_fun(year, laborproductivity)) %>%
      ungroup() %>%
      filter(year > gcamusa.AEO_SE_YEAR)-> L2011.LaborProductivity_USA_EOC

    # Combine annual growth rates into a single table

    states_subregions %>%
      select(state) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      filter(year != min(MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(L2011.LaborProductivity_USA_2015 %>%
                  bind_rows(L2011.LaborProductivity_USA_2040, L2011.LaborProductivity_USA_EOC),
                by = c("state", "year")) %>%
      rename(region = state) %>%
      mutate(laborproductivity = round(laborproductivity, socioeconomics.LABOR_PRODUCTIVITY_DIGITS)) -> L2011.LaborProductivity_updated_USA

    # Smoothen near-term changes in labor productivity growth rate
    # Interpolate between historical (2010-2015 growth) rates to AEO 2030 growth rates

    L2011.LaborProductivity_updated_USA %>%
      filter(year <= gcamusa.SE_HIST_YEAR | year >= gcamusa.SE_NEAR_TERM_YEAR) %>%
      complete(nesting(region), year = c(MODEL_YEARS)) %>%
      filter(year != min(MODEL_YEARS)) %>%
      group_by(region) %>%
      mutate(laborproductivity = approx_fun(year, laborproductivity),
             laborproductivity = round(laborproductivity, socioeconomics.LABOR_PRODUCTIVITY_DIGITS)) %>%
      ungroup() -> L2011.LaborProductivity_updated_USA


    # Add USA-region udpates
    # Updated USA-region population
    L201.Pop_GCAMUSA %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      bind_rows(L2011.Pop_updated_USA) %>%
      group_by(year) %>%
      summarise(totalPop = sum(totalPop)) %>%
      ungroup() %>%
      mutate(totalPop = round(totalPop, socioeconomics.POP_DIGITS),
             region = gcam.USA_REGION) %>%
      select(region, year, totalPop) -> L2011.Pop_national_updated_USA

    # Updated USA-region base GDP
    L2011.BaseGDP_updated_USA %>%
      summarise(baseGDP = sum(baseGDP)) %>%
      mutate(baseGDP = round(baseGDP, socioeconomics.GDP_DIGITS),
             region = gcam.USA_REGION) %>%
      select(region, baseGDP) -> L2011.BaseGDP_national_updated_USA

    # Updated USA-region labor productivity (GDP)
    # First, calculate state GDP from parameters
    # Combine parameters into single table
    L201.Pop_GCAMUSA %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      bind_rows(L2011.Pop_updated_USA) %>%
      # left_join_error_no_match throws error because there are no laborproductivity values for 1975
      # thus, using left_join
      left_join(L2011.LaborProductivity_updated_USA,
                by = c("region", "year")) %>%
      # left_join_error_no_match throws error because baseGDP only has value for 1975
      # thus, using left_join
      left_join(L2011.BaseGDP_updated_USA %>%
                  mutate(year = min(MODEL_BASE_YEARS)),
                by = c("region", "year")) %>%
      rename(GDP = baseGDP) -> L2011.GDP_USA

    L2011.GDP_USA %>%
      distinct(year) %>%
      filter(year != min(MODEL_BASE_YEARS)) %>%
      unique %>%
      unlist -> years

    for (y in years) {

      # Calculate GDP
      L2011.GDP_USA %>%
        group_by(region) %>%
        mutate(time = year - lag(year),
               lag_pop = lag(totalPop),
               lag_GDP = lag(GDP)) %>%
        ungroup() %>%
        filter(year == y) %>%
        mutate(GDP = totalPop * ((1 + laborproductivity)^time) * (lag_GDP / lag_pop)) -> L2011.GDP_USA_temp

      # Add back into table
      L2011.GDP_USA %>%
        filter(year != y) %>%
        bind_rows(L2011.GDP_USA_temp %>%
                    select(-time, -lag_pop, -lag_GDP)) %>%
        mutate(year = as.integer(year)) %>%
        arrange(region, year) -> L2011.GDP_USA

    }

    # Aggregate to USA-region, calculate labor productivity growth
    L2011.GDP_USA %>%
      group_by(year) %>%
      summarise(totalPop = sum(totalPop),
                GDP = sum(GDP)) %>%
      ungroup() %>%
      mutate(region = gcam.USA_REGION,
             pcGDP = GDP / totalPop) %>%
      group_by(region) %>%
      mutate(laborproductivity = round(((pcGDP / lag(pcGDP)) ^ (1 / (year - lag(year)))) - 1,
                                       socioeconomics.LABOR_PRODUCTIVITY_DIGITS)) %>%
      ungroup() %>%
      select(region, year, laborproductivity) %>%
      filter(year != min(MODEL_BASE_YEARS)) -> L2011.LaborProductivity_national_updated_USA

    # ===================================================

    # Produce outputs
    L2011.Pop_updated_USA %>%
      add_title("Updated population by state") %>%
      add_units("thousand persons") %>%
      add_comments("2015 populations from U.S. Census Bureau") %>%
      add_comments("Post-2015 populations based on NCAR SSP2 state-level population growth rates") %>%
      add_legacy_name("L2011.Pop_USA_updated") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/Census_pop_hist",
                     "gcam-usa/Census_pop_10_15",
                     "gcam-usa/NCAR_SSP2_pop_state") ->
      L2011.Pop_updated_USA

    L2011.BaseGDP_updated_USA %>%
      add_title("Updated base year GDP by state") %>%
      add_units("million 1990 USD") %>%
      add_comments("The Bureau of Economic Analysis does not have state-level GDP data available for years prior to 1987.") %>%
      add_comments("Therefore 1987 state-level GDPs are input as GCAM-USA base (1975) GDPs.") %>%
      add_comments("1975 GDP is thus historically incorrect but will yield the correct GDP from 1990 onwards.") %>%
      add_legacy_name("L2011.BaseGDP_USA") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/BEA_GDP_87_96_97USD_state") ->
      L2011.BaseGDP_updated_USA

    L2011.LaborProductivity_updated_USA %>%
      add_title("Updated labor force productivity growth rate by state") %>%
      add_units("Unitless (annual rate of growth)") %>%
      add_comments("Labor productivity growth rates through 2015 were calculated using historical state-level GDP and population data.") %>%
      add_comments("Labor productivity growth rates from 2015 through 2040 were harmonized with AEO 2016 assumptions (at the US Census Division level).") %>%
      add_comments("Labor productivity growth rates post-2040 were linearly interpolated from state-level 2040 values to the USA-region SSP2 value in 2100.") %>%
      add_legacy_name("L2011.LaborProductivity_USA_updated") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/Census_pop_hist",
                     "gcam-usa/Census_pop_10_15",
                     "gcam-usa/BEA_GDP_87_96_97USD_state",
                     "gcam-usa/BEA_GDP_97_16_09USD_state",
                     "gcam-usa/NCAR_SSP2_pop_state",
                     "gcam-usa/AEO_2016_pop_regional",
                     "gcam-usa/AEO_2016_GDP_regional",
                     "L201.LaborProductivity_SSP2",
                     "L201.Pop_GCAMUSA") ->
      L2011.LaborProductivity_updated_USA

    L2011.Pop_national_updated_USA %>%
      add_title("Updated population for USA region, consistent with sum-of-states") %>%
      add_units("thousand persons") %>%
      add_comments("Updates USA region population to match the 50 state + DC total") %>%
      add_comments("2015 populations from U.S. Census Bureau") %>%
      add_legacy_name("L2011.Pop_updated_USA_national") %>%
      same_precursors_as("L2011.Pop_updated_USA") ->
      L2011.Pop_national_updated_USA

    L2011.BaseGDP_national_updated_USA %>%
      add_title("Updated base year GDP for USA region, consistent with sum-of-states") %>%
      add_units("million 1990 USD") %>%
      add_comments("Updates USA region base year GDP to match the 50 state + DC total") %>%
      add_comments("The Bureau of Economic Analysis does not have state-level GDP data available for years prior to 1987.") %>%
      add_comments("Therefore 1987 state-level GDPs are input as GCAM-USA base (1975) GDPs.") %>%
      add_comments("1975 GDP is thus historically incorrect but will yield the correct GDP from 1990 onwards.") %>%
      add_legacy_name("L2011.BaseGDP_updated_USA_national") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/BEA_GDP_87_96_97USD_state") ->
      L2011.BaseGDP_national_updated_USA

    L2011.LaborProductivity_national_updated_USA %>%
      add_title("Updated labor force productivity growth rate for USA region") %>%
      add_units("Unitless (annual rate of growth)") %>%
      add_comments("Annual growth rate calcualted to produce USA region GDP which matches the 50 state + DC total") %>%
      add_legacy_name("L2011.LaborProductivity_updated_USA_national") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/Census_pop_hist",
                     "gcam-usa/Census_pop_10_15",
                     "gcam-usa/BEA_GDP_87_96_97USD_state",
                     "gcam-usa/BEA_GDP_97_16_09USD_state",
                     "gcam-usa/NCAR_SSP2_pop_state",
                     "gcam-usa/AEO_2016_pop_regional",
                     "gcam-usa/AEO_2016_GDP_regional",
                     "L201.LaborProductivity_SSP2",
                     "L201.Pop_GCAMUSA") ->
      L2011.LaborProductivity_national_updated_USA

    return_data(L2011.Pop_updated_USA,
                L2011.BaseGDP_updated_USA,
                L2011.LaborProductivity_updated_USA,
                L2011.Pop_national_updated_USA,
                L2011.BaseGDP_national_updated_USA,
                L2011.LaborProductivity_national_updated_USA)
  } else {
    stop("Unknown command")
  }
}
