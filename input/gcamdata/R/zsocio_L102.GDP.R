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
#' the generated outputs: \code{L102.gdp_mil90usd_Scen_R_Y},
#' \code{L102.pcgdp_thous90USD_Scen_R_Y}, \code{L102.pcgdp_thous90USD_ctry_Yh},
#' \code{L102.PPP_MER_R}.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter full_join if_else intersect group_by left_join mutate one_of select summarise transmute
#' @importFrom tidyr complete gather nesting replace_na
#' @author RPL March 2017
module_socio_L102.GDP <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      "L100.gdp_mil90usd_ctry_Yh",
      "L100.GDP_bilusd_SSP_ctry_Yfut_raw",
      "L100.Pop_thous_ctry_Yh",
      "L101.Pop_thous_Scen_R_Y")

  MODULE_OUTPUTS <-
    c("L102.gdp_mil90usd_Scen_R_Y",
      "L102.pcgdp_thous90USD_Scen_R_Y",
      "L102.pcgdp_thous90USD_ctry_Yh",
      "L102.PPP_MER_R")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    iso <- GCAM_region_ID <- value <- year <- gdp <- MODEL <- VARIABLE <-
      UNIT <- SCENARIO <- scenario <- gdp.rate <- gdp.ratio <- population <-
      pcgdp <- MER <- PPP <- agg_val <- share <-
      base <- ratio <- value.x <- value.y <- NULL     # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)



    # 1. Stitch GDP projections to historical values  ----
    ## Step 1: Get historical GDP data & mapping ready ----

    ## iso--region lookup without extraneous data.  We'll use this several times.
    iso_region32_lookup <- select(iso_GCAM_regID, iso, GCAM_region_ID)

    ## Add region IDs to historical GDP and aggregate by region. Hang onto the country-level data
    ## because we will use them again when we make the IMF adjustments
    gdp_mil90usd_ctry <-
      L100.gdp_mil90usd_ctry_Yh %>%
      left_join_error_no_match(iso_region32_lookup, by = 'iso') %>%
      rename(gdp = value)

    # Note that we now allow gdp_mil90usd_rgn to have more recent years
    gdp_mil90usd_rgn <-
      gdp_mil90usd_ctry %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(gdp = sum(gdp)) %>%
      ungroup()


    ## Step 2: Prepare future GDP projections ----
    # Note that the base year or PPP/MER doesn't matter here
    # We will apply growth rates to historical values
    L100.GDP_bilusd_SSP_ctry_Yfut_raw %>%
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


    ## Step 3 Connect history and future ----

    # all regions currently GDP up to 2023 (FAOSTAT)
    # SSP scenarios use 2025-2100 growth rate from SSP

    # join.gdp.ts hist and future
    gdp.mil90usd.scen.rgn.yr <-
      join.gdp.ts(
        # hist: gdp_mil90usd_rgn before socioeconomics.SSP_DB_BASEYEAR
        gdp_mil90usd_rgn,
        # future: gdp_bilusd_rgn_Yfut
        gdp_bilusd_rgn_Yfut,
        grouping = 'GCAM_region_ID')

    ## Step 4: Additional adjustment  for Venezuela (South Amer North) and Taiwan ----

    # Step 4.1 smoothing GDP when needed (socioeconomics.GDP_Adj_Moving_Average_ISO)


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

    # Step 4.2 no negative gdp growth (per IIASA GDP 2023 scenarios for twn)
    GDP_Adj_No_Neg_Growth_GCAM_region_ID <-
      iso_GCAM_regID$GCAM_region_ID[iso_GCAM_regID$iso %in% socioeconomics.GDP_ADJ_NO_NEG_GROWTH_ISO]

    # No negative after socioeconomics.GDP_Adj_No_Neg_Growth_Year (2025)
    gdp.mil90usd.scen.rgn.yr_1 %>%
      filter(GCAM_region_ID %in% GDP_Adj_No_Neg_Growth_GCAM_region_ID,
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
      arrange(GCAM_region_ID, scenario, year) %>%
      filter(year >= min(HISTORICAL_YEARS)) ->
      gdp.mil90usd.scen.rgn.yr



    # 2 create L102.pcgdp_thous90USD_ctry_Yh ----

    L102.pcgdp_thous90USD_ctry_Yh <-
      L100.gdp_mil90usd_ctry_Yh %>%
      rename(gdp = value) %>%
      # left join (not LJENM) here as NA expected (mainly due to tiny island area diff)
      left_join(L100.Pop_thous_ctry_Yh %>%
                  rename(population = value),
        by = c("iso", "year")) %>%
      filter(year <= max(MODEL_BASE_YEARS)) %>%
      mutate(value = gdp / population) %>%
      select(iso, year, value) %>%
      filter(!is.na(value))

    L101.Pop_thous_Scen_R_Y %>%
      filter(year >= min(HISTORICAL_YEARS)) %>%
      rename(population = value) ->
      pop.thous.scen.rgn.yr

    ## calculate per-capita GDP.  This is another final output
    pcgdp.thous90usd.scen.rgn.yr <-
      gdp.mil90usd.scen.rgn.yr %>%
      left_join_error_no_match(pop.thous.scen.rgn.yr,
                               by = c('scenario', 'GCAM_region_ID', 'year')) %>%
      mutate(pcgdp = gdp / population) %>%
      select(scenario, GCAM_region_ID, year, pcgdp)

    # 3. Derive ppp.mer.rgn ----

    ## Calculate the PPP-MER conversion factor in base year for each region.
    ## Our PPP values are in billions of 2017$, so we make that conversion
    ## here too.
    PPP.MER.baseyr <- MODEL_FINAL_BASE_YEAR

    mer.rgn <-
      gdp_mil90usd_ctry %>%
      filter(year == PPP.MER.baseyr) %>%
      group_by(GCAM_region_ID) %>%
      mutate(MER = gdp * CONV_MIL_BIL) %>%
      summarise(MER = sum(MER))

    # PPP dollar year is SSP data base dollar year
    PPP.dollar.year <- 2017

    ppp.rgn <-
      gdp_bilusd_rgn_Yfut %>%
      # any SSP is fine here as historical year is used
      filter(year == PPP.MER.baseyr, scenario == 'SSP1') %>%
      # convert to 1990 $
      mutate(PPP = gdp / gdp_deflator(PPP.dollar.year, 1990) ) %>%
      select(GCAM_region_ID, PPP)

    ppp.mer.rgn <-
      mer.rgn %>%
      left_join_error_no_match(ppp.rgn, by = 'GCAM_region_ID') %>%
      mutate(PPP_MER = PPP / MER)

    assertthat::assert_that(
      abs(ppp.mer.rgn %>% filter(GCAM_region_ID == 1) %>% pull(PPP_MER) - 1) < 0.05,
      msg = "check discrepancy for the US in GDP between PPP and MER" )

    ppp.mer.rgn %>%
      # rescale PPP for all regions by USA PPP_MER
      # to ensure USA PPP_MER == 1
      mutate(PPP = PPP / PPP_MER[GCAM_region_ID == 1],
             PPP_MER = PPP / MER) ->
      ppp.mer.rgn

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
                     "L100.GDP_bilusd_SSP_ctry_Yfut_raw",
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
                     "L100.GDP_bilusd_SSP_ctry_Yfut_raw",
                     "L100.gdp_mil90usd_ctry_Yh",
                     "L101.Pop_thous_Scen_R_Y") ->
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
                     "L100.GDP_bilusd_SSP_ctry_Yfut_raw",
                     "L100.gdp_mil90usd_ctry_Yh") ->
      L102.PPP_MER_R

    L102.pcgdp_thous90USD_ctry_Yh %>%
      add_title("historical Per-Capita GDP by Country") %>%
      add_units("Thousand 1990 USD") %>%
      add_comments("L100.gdp_mil90usd_ctry_Yh divided by population from L100.Pop_thous_ctry_Yh") %>%
      add_legacy_name("L102.pcgdp_thous90USD_ctry_Yh") %>%
      add_precursors("L100.gdp_mil90usd_ctry_Yh",
                     "L100.Pop_thous_ctry_Yh") ->
      L102.pcgdp_thous90USD_ctry_Yh


    return_data(MODULE_OUTPUTS)

    } else {
    stop("Unknown command")
  }
}
