# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L102.GDP
#'
#' Prepare historical and future GDP time series.  On the historical side, this
#' amounts to aggregating country-level GDP to GCAM regions.  On the future side
#' we create a time series for each of a variety of future scenarios.  The
#' outputs include GDP, pcGDP, and the PPP-MER conversion factor, all tabulated
#' by GCAM region.
#'
#' The scenarios generated include the SSPs.  GDP outputs are in millions of 1990 USD.
#' Per-capita values are in thousands of 1990 USD.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L102.gdp_mil90usd_Scen_R_Y}, \code{L102.pcgdp_thous90USD_Scen_R_Y}, \code{L102.gdp_mil90usd_GCAM3_R_Y}, \code{L102.gdp_mil90usd_GCAM3_ctry_Y}, \code{L102.pcgdp_thous90USD_GCAM3_R_Y}, \code{L102.pcgdp_thous90USD_GCAM3_ctry_Y}, \code{L102.PPP_MER_R}. The corresponding file in the
#' original data system was \code{L102.GDP.R} (socioeconomics level1).
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter full_join if_else intersect group_by left_join mutate one_of select summarise transmute
#' @importFrom tidyr complete gather nesting replace_na
#' @author RPL March 2017
module_socio_L102.GDP <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      FILE = "socioeconomics/SSP/SSP_database_2024",
      FILE = "socioeconomics/SSP/iso_SSP_regID",
      FILE = "socioeconomics/GDP/GCAM3_GDP",
      "L100.gdp_mil90usd_ctry_Yh",
      "L101.Pop_thous_GCAM3_R_Y",
      "L101.Pop_thous_GCAM3_ctry_Y",
      "L101.Pop_thous_R_Yh",
      "L101.Pop_thous_SSP_R_Yfut")

  MODULE_OUTPUTS <-
    c("L102.gdp_mil90usd_Scen_R_Y",
      "L102.pcgdp_thous90USD_Scen_R_Y",
      "L102.PPP_MER_R",
      "L102.gdp_mil90usd_GCAM3_R_Y",
      "L102.gdp_mil90usd_GCAM3_ctry_Y",
      "L102.pcgdp_thous90USD_GCAM3_R_Y",
      "L102.pcgdp_thous90USD_GCAM3_ctry_Y")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    iso <- GCAM_region_ID <- value <- year <- gdp <- MODEL <- VARIABLE <-
      UNIT <- SCENARIO <- scenario <- gdp.rate <- gdp.ratio <- population <-
      pcgdp <- MER <- PPP <- region_GCAM3 <- agg_val <- share <-
      GCAM3_value <- base <- ratio <- value.x <- value.y <- NULL     # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Step 1: Get historical GDP data & mapping ready ----

    ## iso--region lookup without extraneous data.  We'll use this several times.
    iso_region32_lookup <- select(iso_GCAM_regID, iso, GCAM_region_ID)

    ## Add region IDs to historical GDP and aggregate by region. Hang onto the country-level data
    ## because we will use them again when we make the IMF adjustments
    gdp_mil90usd_ctry <-
      left_join_error_no_match(L100.gdp_mil90usd_ctry_Yh, iso_region32_lookup, by = 'iso') %>%
      rename(gdp = value)

    # Note that we now allow gdp_mil90usd_rgn to have more recent years
    gdp_mil90usd_rgn <-
      gdp_mil90usd_ctry %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(gdp = sum(gdp)) %>%
      ungroup()


    # Step 2: Prepare future drivers of GDP ----
    # Note that the base year or PPP/MER doesn't matter here
    # We will apply growth rates to historical values

    SSP_database_2024 %>%
      # make variable names lower case
      dplyr::rename_all(tolower) %>%
      # remove aggregated regions
      filter(!grepl("\\(|World", region)) %>%
      filter(model == 'OECD ENV-Growth 2023' & variable == 'GDP|PPP') %>%
      left_join_error_no_match(
        iso_SSP_regID %>% distinct(iso, region = ssp_country_name),
        by = "region") %>%
      gather_years()->
      SSP_gdp_0

      # Using the Historical Reference scenario to fill history of SSPs
    SSP_gdp_0 %>%
      filter(scenario != "Historical Reference") %>%
      left_join(
        SSP_gdp_0 %>% filter(scenario == "Historical Reference") %>% select(-scenario) %>%
          rename(hist = value),
        by = c("model", "region", "variable", "unit", "iso", "year")
      ) %>%
      # new ssp data starts 2020 (socioeconomics.SSP_DB_BASEYEAR)
      mutate(value = if_else(year < socioeconomics.SSP_DB_BASEYEAR, hist, value)) %>%
      select(iso, scenario, year, gdp = value) ->
      gdp_bilusd_ctry_Yfut_0

    gdp_bilusd_ctry_Yfut_0 %>%
      left_join_error_no_match(iso_region32_lookup, by = 'iso') %>%
      group_by(scenario, GCAM_region_ID, year) %>%
      summarize(gdp = sum(gdp, na.rm = T)) %>%
      # The steps below write out the data to all future years, starting from the final socio historical year
      complete(nesting(scenario, GCAM_region_ID), year = c(socioeconomics.FINAL_HIST_YEAR, FUTURE_YEARS)) %>%
      group_by(scenario, GCAM_region_ID) %>%
      mutate(gdp = approx_fun(year, gdp)) %>%
      ungroup() ->
      gdp_bilusd_rgn_Yfut
    ## Units are billions of 2017$ but relative ratio will be used when connecting to historical data


    # Step 3 Connect history and future ----

    # all regions currently GDP up to 2023 (FAOSTAT)
    # SSP scenarios use 2020-2100 growth rate from SSP

    ## 3.1 for SSP scenarios ----
    # join.gdp.ts hist and future
    gdp.mil90usd.scen.rgn.yr <-
      join.gdp.ts(
        # hist: gdp_mil90usd_rgn before socioeconomics.SSP_DB_BASEYEAR
        gdp_mil90usd_rgn %>% filter(year <= socioeconomics.SSP_DB_BASEYEAR),
        # future: gdp_bilusd_rgn_Yfut
        gdp_bilusd_rgn_Yfut,
        grouping = 'GCAM_region_ID')

    # Step 5: Additional adjustment  for Venezuela (South Amer North) and Taiwan ----

    # Step 5.1 smoothing GDP when needed (socioeconomics.GDP_Adj_Moving_Average_ISO)

    GDP_Adj_Moving_Average_GCAM_region_ID <-
      iso_GCAM_regID$GCAM_region_ID[iso_GCAM_regID$iso %in% socioeconomics.GDP_ADJ_MOVING_AVERAGE_ISO]

    # use socioeconomics.GDP_Adj_Moving_Average_Duration (15-year) moving average for South Amer North (25)
    gdp.mil90usd.scen.rgn.yr %>%
      filter(GCAM_region_ID %in% GDP_Adj_Moving_Average_GCAM_region_ID) %>%
      group_by(GCAM_region_ID, scenario) %>%
      mutate(gdp = Moving_average(gdp, periods = socioeconomics.GDP_ADG_MOVING_AVERAGE_DURATION)) %>%
      ungroup() %>%
      bind_rows(
        gdp.mil90usd.scen.rgn.yr %>%
          filter(!GCAM_region_ID %in% GDP_Adj_Moving_Average_GCAM_region_ID)) ->
      gdp.mil90usd.scen.rgn.yr_1

    # Step 5.1 no negative gdp growth (per IIASA GDP 2023 scenarios for twn)
    GDP_Adj_No_Neg_Growth_GCAM_region_ID <-
      iso_GCAM_regID$GCAM_region_ID[iso_GCAM_regID$iso %in% socioeconomics.GDP_ADJ_NO_NEG_GROWTH_ISO]

    # No negative after socioeconomics.GDP_Adj_No_Neg_Growth_Year (2025)
    gdp.mil90usd.scen.rgn.yr_1 %>%
      filter(GCAM_region_ID %in%GDP_Adj_No_Neg_Growth_GCAM_region_ID,
             year >= socioeconomics.GDP_ADJ_NO_NEG_GROWTH_YEAR) %>%
      group_by(GCAM_region_ID, scenario) %>%
      mutate(gdp_g = gdp / lag(gdp)) %>%
      replace_na(list(gdp_g = 1)) %>%
      mutate(gdp_g_adj =  pmax(1, gdp_g),
             gdp_cum = cumprod(gdp_g_adj)
             ) %>%
      mutate(gdp = gdp[year == socioeconomics.GDP_ADJ_NO_NEG_GROWTH_YEAR] * gdp_cum) %>% ungroup %>%
      select(names(gdp.mil90usd.scen.rgn.yr_1)) %>%
      bind_rows(
        gdp.mil90usd.scen.rgn.yr_1 %>%
          filter(!(GCAM_region_ID %in% GDP_Adj_No_Neg_Growth_GCAM_region_ID & year >= socioeconomics.GDP_ADJ_NO_NEG_GROWTH_YEAR))
      ) %>%
      arrange(GCAM_region_ID, scenario, year)->
      gdp.mil90usd.scen.rgn.yr


    # *******************----
    # Derive ppp.mer.rgn ----
    ## Construct a table of population by scenario, region, and year.  We have a
    ## table of historical population, and a table of future population by
    ## scenario, both in wide form.  Convert to long form and filter to the years
    ## we need.  Add a scenario column to historical years, and combine the
    ## whole thing into a single table.
    pop.thous.fut <-
      rename(L101.Pop_thous_SSP_R_Yfut, population = value) %>%
      filter(year %in% FUTURE_YEARS)
    pop.thous.hist <-
      rename(L101.Pop_thous_R_Yh, population = value) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      tidyr::crossing(scenario = unique(pop.thous.fut[['scenario']]))
    pop.thous.scen.rgn.yr <-
      bind_rows(pop.thous.hist, pop.thous.fut) %>%
      mutate(year = as.integer(year),
             population = as.numeric(population)) %>%
      select(scenario, GCAM_region_ID, year, population)

    ## calculate per-capita GDP.  This is another final output
    pcgdp.thous90usd.scen.rgn.yr <-
      left_join(gdp.mil90usd.scen.rgn.yr, pop.thous.scen.rgn.yr,
                by = c('scenario', 'GCAM_region_ID', 'year')) %>%
      mutate(pcgdp = gdp / population) %>%
      select(scenario, GCAM_region_ID, year, pcgdp)

    ## Calculate the PPP-MER conversion factor in base year for each region.
    ## Our PPP values are in billions of 2017$, so we make that conversion
    ## here too.
    #kbn 2020-03-26 Using model final base year here below
    PPP.MER.baseyr <- MODEL_FINAL_BASE_YEAR
    mer.rgn <- gdp_mil90usd_ctry %>%
      filter(year == PPP.MER.baseyr) %>%
      group_by(GCAM_region_ID) %>%
      mutate(MER = gdp * gdp_deflator(2017, 1990) * CONV_MIL_BIL) %>%
      summarise(MER = sum(MER))
    ## columns: GCAM_region_ID, MER

    ## The future data is given by SSP scenario, but the final table is scenario
    ## independent, as it should be, since this base year is meant to be a
    ## historical year.  Likewise, the GDP in the PPP/MER base year should also
    ## be scenario-independent, and mostly it is, except for the region containing
    ## the Palestinian Territories, which has four slightly different values across
    ## the 5 SSPs. (It's SSP 2 and 4 that are the same).  The value
    ## actually used in the old data system is the one for SSP1, so that's the
    ## one we'll use here.  Arguably we should average the values over the 5
    ## scenarios, but the differences are only 1 part in 10^4, so we can just
    ## let it slide.
    ppp.rgn <- gdp_bilusd_rgn_Yfut %>%
      ungroup %>%
      filter(year == PPP.MER.baseyr, scenario == 'SSP1') %>%
      rename(PPP = gdp) %>%
      select(GCAM_region_ID, PPP)

    ppp.mer.rgn <-
      left_join_error_no_match(mer.rgn, ppp.rgn, by = 'GCAM_region_ID') %>%
      mutate(PPP_MER = PPP / MER)

    # GDP by GCAM region from GCAM 3.0 GDPs ----
    # Downscaling GCAM 3.0 GDP by GCAM 3.0 region to countries, using SSP2 GDP scenario
    # GDP by GCAM 3.0 region - downscale to country according to actual shares in the historical periods, and SSPbase in the future periods

    GCAM3_GDP <- GCAM3_GDP %>% gather_years %>%
      mutate(value = as.numeric(value))

    # Future GDP

    gdp_bilusd_ctry_Yfut_0 %>% rename(value = gdp) %>%
      # Only Base SSP
      filter(scenario == socioeconomics.BASE_POP_SCEN) %>%
      select(iso, year, value) %>%
      complete(nesting(iso), year = c(socioeconomics.FINAL_HIST_YEAR, FUTURE_YEARS)) %>%
      group_by(iso) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% FUTURE_YEARS) ->
      gdp_bilusd_ctry_Yfut


    # Historical GDP
    gdp_mil90usd_ctry_Yh <- L100.gdp_mil90usd_ctry_Yh %>%
      filter(year %in% HISTORICAL_YEARS,
             iso %in% gdp_bilusd_ctry_Yfut$iso)

    gdp_mil90usd_ctry_Yh <- gdp_mil90usd_ctry_Yh %>%
      # Only take future GDP if iso has historical GDP
      bind_rows(gdp_bilusd_ctry_Yfut %>%
                  filter(iso %in% gdp_mil90usd_ctry_Yh$iso)) %>%
      left_join_error_no_match(iso_GCAM_regID %>%
                                 select(iso, region_GCAM3), by = "iso")

    # Aggregating GDP by GCAM3 region
    gdp_mil90usd_SSPbase_RG3_Y <- gdp_mil90usd_ctry_Yh %>%
      group_by(year, region_GCAM3) %>%
      summarise(agg_val = sum(value, na.rm = T))

    # Calculate shares of each country within its region
    gdpshares_ctryRG3_Y <- gdp_mil90usd_ctry_Yh %>%
      group_by(year, region_GCAM3) %>%
      mutate(share = value / sum(value)) %>%
      ungroup()

    # Interpolate GCAM3 GDP data to all historical and future years
    gdp_mil90usd_GCAM3_RG3_Y <- iso_GCAM_regID %>%
      select(region_GCAM3) %>%
      distinct() %>%
      repeat_add_columns(tibble(year = as.integer(c(HISTORICAL_YEARS, FUTURE_YEARS)))) %>%
      left_join(GCAM3_GDP, by = c("region_GCAM3", "year")) %>%
      # Add in historical values to match in for NA values
      left_join(gdp_mil90usd_SSPbase_RG3_Y, by = c("region_GCAM3", "year")) %>%
      group_by(region_GCAM3) %>%
      # Extending GCAM 3.0 scenario to first historical year using historical GDP ratios by GCAM 3.0 region
      mutate(value = replace(value, year == min(year),
                             value[year == min(GCAM3_GDP$year)] * agg_val[year == min(year)] / agg_val[year == min(GCAM3_GDP$year)]),
             value = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      select(region_GCAM3, year, GCAM3_value = value)

    # Multiply these GDP numbers by the shares of each country within GCAM region
    gdp_mil90usd_GCAM3_ctry_Y <- gdpshares_ctryRG3_Y %>%
      left_join_error_no_match(gdp_mil90usd_GCAM3_RG3_Y, by = c("year", "region_GCAM3")) %>%
      transmute(iso, year, value = share * GCAM3_value)

    # rebasing GDP to 2010 USD at country level
    # Calculate the GDP ratios from the first year in the projections. Use this ratio to project GDP from historical dataset in final historical period
    gdpRatio_GCAM3_ctry_Yfut <- gdp_mil90usd_GCAM3_ctry_Y %>%
      group_by(iso) %>%
      mutate(base = value[year == socioeconomics.FINAL_HIST_YEAR]) %>%
      ungroup() %>%
      filter(year %in% FUTURE_YEARS) %>%
      transmute(iso, year, ratio = value / base)

    # Use these ratios to build the GDP trajectories by old GCAM3
    gdp_mil90usd_GCAM3_ctry_Y <- gdp_mil90usd_ctry_Yh %>%
      left_join(gdpRatio_GCAM3_ctry_Yfut, by = c("iso", "year")) %>%
      group_by(iso) %>%
      mutate(value = if_else(year %in% FUTURE_YEARS, value[year == socioeconomics.FINAL_HIST_YEAR] * ratio, value)) %>%
      ungroup() %>%
      select(iso, year, value)

    # Aggregating by GCAM4 region
    gdp_mil90usd_GCAM3_R_Y <- gdp_mil90usd_GCAM3_ctry_Y %>%
      filter(iso %in% unique(L101.Pop_thous_GCAM3_ctry_Y$iso)) %>%
      left_join_error_no_match(iso_region32_lookup, by = "iso") %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # Calculate per-capita GDP
    pcgdp_thous90USD_GCAM3_R_Y <- gdp_mil90usd_GCAM3_R_Y %>%
      left_join_error_no_match(L101.Pop_thous_GCAM3_R_Y, by = c("year", "GCAM_region_ID")) %>%
      transmute(GCAM_region_ID, year, value = value.x / value.y)

    pcgdp_thous90USD_GCAM3_ctry_Y <- gdp_mil90usd_GCAM3_ctry_Y %>%
      filter(iso %in% unique(L101.Pop_thous_GCAM3_ctry_Y$iso)) %>%
      left_join_error_no_match(L101.Pop_thous_GCAM3_ctry_Y, by = c("year", "iso")) %>%
      transmute(iso, year, value = value.x / value.y)

    # Produce outputs ----
    gdp.mil90usd.scen.rgn.yr %>%
      ungroup %>%
      filter(year %in% c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      rename(value = gdp) %>%
      mutate(year = as.integer(year)) %>%
      add_title("Gross Domestic Product (GDP) by scenario, region, and year.") %>%
      add_units("Millions of 1990 USD (MER)") %>%
      add_comments("SSP projections match historical values and are available beyond") %>%
      add_comments("our final calibration period.") %>%
      add_legacy_name("L102.gdp_mil90usd_Scen_R_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "socioeconomics/SSP/SSP_database_2024",
                     "socioeconomics/SSP/iso_SSP_regID",
                     "L100.gdp_mil90usd_ctry_Yh") ->
      L102.gdp_mil90usd_Scen_R_Y

    pcgdp.thous90usd.scen.rgn.yr %>%
      ungroup %>%
      filter(year %in% c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      rename(value = pcgdp) %>%
      add_title("Gross Domestic Product (GDP) per capita, by scenario, region, and year.") %>%
      add_units("Thousands of 1990 USD (MER)") %>%
      add_comments("Computed as GDP/population.  Values prior to the base year (2010) are ") %>%
      add_comments("historical; values subsequent are from SSP projections.") %>%
      add_legacy_name("L102.pcgdp_thous90USD_Scen_R_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "socioeconomics/SSP/SSP_database_2024",
                     "socioeconomics/SSP/iso_SSP_regID",
                     "L100.gdp_mil90usd_ctry_Yh",
                     "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_SSP_R_Yfut") ->
      L102.pcgdp_thous90USD_Scen_R_Y

    ppp.mer.rgn %>%
      add_title("Purchasing Power Parity (PPP) to Market Exchange Rate (MER) GDP conversions, by region") %>%
      add_units("unitless") %>%
      add_comments("Calculated as GDP(PPP) / GDP(MER) in the base year (2010) for each region.  The") %>%
      add_comments("table also contains PPP-GDP and MER-GDP in billions of 2005 USD for the base year, ") %>%
      add_comments("because they were included in the original table, but I'm not sure they are used for, ") %>%
      add_comments("or useful for, anything besides calculating the ratio.") %>%
      add_legacy_name("L102.PPP_MER_R") %>%
      add_precursors("common/iso_GCAM_regID",
                     "socioeconomics/SSP/SSP_database_2024",
                     "socioeconomics/SSP/iso_SSP_regID",
                     "L100.gdp_mil90usd_ctry_Yh") ->
      L102.PPP_MER_R

    gdp_mil90usd_GCAM3_R_Y %>%
      add_title("GDP by GCAM3 Region") %>%
      add_units("Million 1990 USD") %>%
      add_comments("Scales historical SSP GDP data to GCAM3 GDP data.") %>%
      add_comments("Calculates future GDP based on ratio of GCAM3 future to 2010 value.") %>%
      add_legacy_name("L102.gdp_mil90usd_GCAM3_R_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "socioeconomics/SSP/SSP_database_2024",
                     "socioeconomics/SSP/iso_SSP_regID",
                     "L100.gdp_mil90usd_ctry_Yh",
                     "socioeconomics/GDP/GCAM3_GDP") ->
      L102.gdp_mil90usd_GCAM3_R_Y

    gdp_mil90usd_GCAM3_ctry_Y %>%
      add_title("GCAM3 GDP by Country") %>%
      add_units("Million 1990 USD") %>%
      add_comments("Scales historical SSP GDP data to GCAM3 GDP data.") %>%
      add_comments("Calculates future GDP based on ratio of GCAM3 future to 2010 value.") %>%
      add_legacy_name("L102.gdp_mil90usd_GCAM3_ctry_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "socioeconomics/SSP/SSP_database_2024",
                     "socioeconomics/SSP/iso_SSP_regID",
                     "L100.gdp_mil90usd_ctry_Yh",
                     "socioeconomics/GDP/GCAM3_GDP") ->
      L102.gdp_mil90usd_GCAM3_ctry_Y

    pcgdp_thous90USD_GCAM3_R_Y %>%
      add_title("Per-Capita GDP by GCAM3 Region") %>%
      add_units("Thousand 1990 USD") %>%
      add_comments("L102.gdp_mil90usd_GCAM3_R_Y divided by population from L101.Pop_thous_GCAM3_R_Y") %>%
      add_legacy_name("L102.pcgdp_thous90USD_GCAM3_R_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "socioeconomics/SSP/SSP_database_2024",
                     "socioeconomics/SSP/iso_SSP_regID",
                     "L100.gdp_mil90usd_ctry_Yh",
                     "socioeconomics/GDP/GCAM3_GDP",
                     "L101.Pop_thous_GCAM3_R_Y") ->
      L102.pcgdp_thous90USD_GCAM3_R_Y

    pcgdp_thous90USD_GCAM3_ctry_Y %>%
      add_title("GCAM3 Per-Capita GDP by Country") %>%
      add_units("Thousand 1990 USD") %>%
      add_comments("L102.gdp_mil90usd_GCAM3_ctry_Y divided by population from L101.Pop_thous_GCAM3_ctry_Y") %>%
      add_legacy_name("L102.pcgdp_thous90USD_GCAM3_ctry_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "socioeconomics/SSP/SSP_database_2024",
                     "socioeconomics/SSP/iso_SSP_regID",
                     "L100.gdp_mil90usd_ctry_Yh",
                     "socioeconomics/GDP/GCAM3_GDP",
                     "L101.Pop_thous_GCAM3_ctry_Y") ->
      L102.pcgdp_thous90USD_GCAM3_ctry_Y

    return_data(MODULE_OUTPUTS)

    } else {
    stop("Unknown command")
  }
}
