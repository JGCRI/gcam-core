# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L161.nonghg_en_ssp_R_S_T_Y
#'
#' Produce future non-GHG emissions factors by SSP scenario.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L161.SSP15_EF}, \code{L161.SSP2_EF}, \code{L161.SSP34_EF}. The corresponding file in the
#' original data system was \code{L161.nonghg_en_ssp_R_S_T_Y.R} (emissions level1).
#' @details Scales future GAINS, Greenhouse Gas - Air Pollution Interactions and Synergies model, non-GHG emissions factors to L111/L114 base year emissions factors,
#' then applies future emissions factors to some GCAM years based on SSP-specific rules.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by lag left_join mutate order_by select summarise
#' @importFrom tidyr complete gather nesting replace_na
#' @author RLH July 2017
module_emissions_L161.nonghg_en_ssp_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/A_regions",
             FILE = "emissions/mappings/GCAM_sector_tech",
             FILE = "emissions/mappings/GCAM_sector_tech_Revised",
             FILE = "emissions/mappings/gains_to_gcam_sector",
             FILE = "emissions/GAINS_activities",
             FILE = "emissions/GAINS_emissions",
             "L102.pcgdp_thous90USD_Scen_R_Y",
             "L111.nonghg_tgej_R_en_S_F_Yh",
             "L114.bcoc_tgej_R_en_S_F_2000",
             FILE = "emissions/A61_emfact_rules"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L161.SSP15_EF",
             "L161.SSP2_EF",
             "L161.SSP34_EF"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    ACT <- CLE <- CLE_2010 <- CLE_2020 <- CLE_2030 <- CLE_base <- GAINS_region <-
      GCAM_region_ID <- GCAM_tag <- IDYEARS <- IIASA_sector <- MFR <- MFR_2030 <-
      Non.CO2 <- POLL <- SLE <- SLE_2030 <- SSP_group <- TIMER_REGION <- agg_sector <-
      base_value <- emfact <- marker_region_CLE_2020 <- marker_region_CLE_2030 <-
      marker_region_SLE_2030 <- min_CLE_2030_low <- min_CLE_2030_weak_reg <-
      min_SLE_2030_strong_reg <- policy <- prev <- region_grouping <- scaler <-
      scenario <- stub.technology <- subsector <- supplysector <- value <-
      variable <- varyear <- varyearpol <- year <- marker_value <- min_value <-
      min_value <- multiplier <- . <- `2000` <- NULL  # silence package check notes

    all_data <- list(...)[[1]]

    # Load required inputs
    A_regions <- get_data(all_data, "emissions/A_regions")
    GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech")

    if (energy.TRAN_UCD_MODE == "rev.mode"){
      GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech_Revised")

    }


    GAINS_sector <- get_data(all_data, "emissions/mappings/gains_to_gcam_sector")
    GAINS_activities <- get_data(all_data, "emissions/GAINS_activities")
    GAINS_emissions <- get_data(all_data, "emissions/GAINS_emissions") %>%
      # NOTE: these are three different scenarios
      # CLE = current legislation, SLE = stringent legislation, MFR = maximum feasible reductions
      gather(scenario, value, CLE, MFR, SLE)
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")
    L111.nonghg_tgej_R_en_S_F_Yh <- get_data(all_data, "L111.nonghg_tgej_R_en_S_F_Yh")
    L114.bcoc_tgej_R_en_S_F_2000 <- get_data(all_data, "L114.bcoc_tgej_R_en_S_F_2000") %>%
      gather(year, value, `2000`) %>%
      mutate(year = as.integer(year))
    A61_emfact_rules <- get_data(all_data, "emissions/A61_emfact_rules")

    # ===================================================

    # Aggregate GAINS emissions data by GCAM sector
    GAINS_emissions_agg <- GAINS_emissions %>%
      # Change pollutant names
      mutate(POLL = replace(POLL, POLL == "NOX", "NOx"),
             POLL = replace(POLL, POLL == "VOC", "NMVOC")) %>%
      # Use left_join because NAs in GAINS_sector
      left_join(GAINS_sector, by = c("TIMER_SECTOR" = "IIASA_Sector")) %>%
      group_by(TIMER_REGION, agg_sector = GCAM_tag, POLL, IDYEARS, scenario) %>%
      summarise(value = sum(value)) %>%
      na.omit %>%
      ungroup

    # Aggregate GAINS activity data by GCAM sector
    GAINS_activities_agg <- GAINS_activities %>%
      # Use left_join because NAs in GAINS_sector
      left_join(GAINS_sector, by = c("TIMER_SECTOR" = "IIASA_Sector")) %>%
      group_by(TIMER_REGION, agg_sector = GCAM_tag, IDYEARS) %>%
      summarise(ACT = sum(ACT)) %>%
      na.omit %>%
      ungroup

    # Compute emissions factors by dividing GAINS activity by GAINS emissions
    GAINS_emfact <- GAINS_emissions_agg %>%
      left_join_error_no_match(GAINS_activities_agg, by = c("TIMER_REGION", "agg_sector", "IDYEARS")) %>%
      mutate(emfact = value / ACT) %>%
      select(TIMER_REGION, agg_sector, POLL, IDYEARS, scenario, emfact) %>%
      group_by(TIMER_REGION, agg_sector, POLL, IDYEARS) %>%
      # Using CLE scenario for base value
      mutate(CLE_base = emfact[scenario == "CLE"]) %>%
      ungroup() %>%
      # Replace SLE & MFR base year (2005) emissions factors with CLE emissions factors.
      # They don't all start from the same value.
      mutate(emfact = replace(emfact, IDYEARS == emissions.GAINS_BASE_YEAR, CLE_base[IDYEARS == emissions.GAINS_BASE_YEAR])) %>%
      select(-CLE_base)

    # Compute emissions factor scaler.
    # These scalers are relative to the previous time period's numbers.
    GAINS_emfact_scaler <- GAINS_emfact %>%
      group_by(TIMER_REGION, agg_sector, POLL, scenario) %>%
      # Create column of previous time period value
      mutate(prev = lag(emfact, n = 1L, order_by = IDYEARS)) %>%
      ungroup() %>%
      filter(IDYEARS > emissions.GAINS_BASE_YEAR) %>%
      # Divide current value by previous value, not allowing value greater than 1 (emissions factors cannot increase with time)
      mutate(scaler = emfact / prev,
             scaler = replace(scaler, scaler > 1, 1)) %>%
      group_by(TIMER_REGION, agg_sector, POLL, scenario) %>%
      mutate(scaler = cumprod(scaler)) %>%
      ungroup() %>%
      select(GAINS_region = TIMER_REGION, IIASA_sector = agg_sector, Non.CO2 = POLL, scenario, year = IDYEARS, scaler)


    # Determine region groupings
    pcgdp <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      # We are trying to filter to 2010. This code (taking the last historical year) was necessary to
      # pass the timeshift, but is really not what we want to be doing, since the years in this code
      # are fairly set in stone right now
      filter(scenario == "SSP4", year == HISTORICAL_YEARS[length(HISTORICAL_YEARS)]) %>%
      mutate(value = value * gdp_deflator(HISTORICAL_YEARS[length(HISTORICAL_YEARS)], 1990),
             region_grouping = if_else(value >= emissions.LOW_PCGDP, "highmed", "low"))

    # Compute future emissions factors for GAINS scenarios
    emfact_scaled <- L111.nonghg_tgej_R_en_S_F_Yh %>%
      filter(year == emissions.GAINS_BASE_YEAR) %>%
      # Add GAINS regions and sectors
      left_join_error_no_match(A_regions %>% select(GCAM_region_ID, GAINS_region), by = "GCAM_region_ID") %>%
      left_join(GCAM_sector_tech %>% select(supplysector, subsector, stub.technology, IIASA_sector) %>% distinct(),
                by = c("supplysector", "subsector", "stub.technology")) %>%
      # Remove non-IIASA sectors and technologies with 0 emissions factor in base year. No reason to read in future zeroes.
      filter(!is.na(IIASA_sector), value != 0) %>%
      rename(base_year = year, base_value = value) %>%
      left_join(GAINS_emfact_scaler, by = c("GAINS_region", "IIASA_sector", "Non.CO2")) %>%
      # Scale L111/L114 emissions factors to GAINS scalers
      mutate(emfact = base_value * scaler) %>%
      left_join_error_no_match(pcgdp %>% select(GCAM_region_ID, region_grouping), by = "GCAM_region_ID") %>%
      na.omit() %>%
      select(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, GAINS_region, IIASA_sector,
             scenario, year, emfact, region_grouping)

    # Create list of countries with strong regulation based on elec_coal SO2 emissions factor
    coal_so2 <- tibble(GCAM_region_ID = A_regions$GCAM_region_ID) %>%
      left_join(
        emfact_scaled %>%
          filter(year == emissions.GAINS_YEARS[length(emissions.GAINS_YEARS)],
                 IIASA_sector == "elec_coal", Non.CO2 == "SO2", scenario == "CLE"),
        by = "GCAM_region_ID") %>%
      mutate(policy = if_else(emfact <= emissions.COAL_SO2_THRESHOLD, "strong_reg", "weak_reg"),
             policy = replace(policy, region_grouping == "low", "low")) %>%
      # If region is missing a value, assume it is a weak_reg
      replace_na(list(policy = "weak_reg")) %>%
      select(GCAM_region_ID, policy)

    # Group SSPs by whether we process them the same
    SSP_groups <- tibble(SSP_group = c("1&5", "2", "3&4"))

    # Add the rules for each region, gas, technology
    EF_rules <- emfact_scaled %>%
      # This is to have same starting point as old code, which begins with NA omitted 2010 values
      filter(year == emissions.GAINS_YEARS[1]) %>%
      na.omit %>%
      select(-year, -emfact, -scenario) %>%
      distinct() %>%
      # Repeat for future years and SSP groups
      repeat_add_columns(tibble(year = c(2010, 2030, 2050, 2100))) %>%
      repeat_add_columns(SSP_groups) %>%
      # Join with policy type, but only for SSP group 2
      left_join_error_no_match(coal_so2, by = "GCAM_region_ID") %>%
      mutate(policy = replace(policy, SSP_group != "2", NA)) %>%
      # Join with rules-use left_join b/c there are NA values in A61_emfact_rules
      left_join(A61_emfact_rules, by = c("region_grouping", "year", "SSP_group", "policy"))

    # Create a tibble with just marker region values
    # Marker region is Western Europe (13) - some values will be set to its emissions factors in future
    marker_region_df <- emfact_scaled %>%
      filter(GCAM_region_ID == gcam.WESTERN_EUROPE_CODE) %>%
      select(-GCAM_region_ID, -GAINS_region, -region_grouping) %>%
      rename(marker_value = emfact)

    # Combine all emissions factors
    EF_all <- emfact_scaled %>%
      # This is to have same starting point as old code, which begins with NA omitted 2010 values
      filter(year == emissions.GAINS_YEARS[1]) %>%
      na.omit %>%
      select(-year, -emfact) %>%
      left_join(emfact_scaled, by = c("GCAM_region_ID", "Non.CO2", "supplysector", "subsector", "stub.technology",
                                      "GAINS_region", "IIASA_sector", "scenario", "region_grouping")) %>%
      # Add in policy
      left_join_error_no_match(coal_so2, by = "GCAM_region_ID") %>%
      # Use complete to fill out any region/technology/gas combos that do not have emissions factors for all years
      complete(year, nesting(GCAM_region_ID, Non.CO2, supplysector, subsector,
                             stub.technology, GAINS_region, IIASA_sector, scenario, region_grouping, policy)) %>%
      # Calculate minimum by technology/gas
      group_by(Non.CO2, supplysector, subsector, stub.technology, IIASA_sector, scenario, year, policy, region_grouping) %>%
      mutate(min_value = min(emfact, na.rm = TRUE)) %>%
      ungroup %>%
      # Add marker region values
      left_join(marker_region_df, by = c("Non.CO2", "supplysector", "subsector", "stub.technology", "IIASA_sector", "scenario", "year")) %>%
      gather(variable, value, emfact, marker_value, min_value) %>%
      # Add binary columns indicating if value is marker value or minimum value
      mutate(marker_region = if_else(variable == "marker_value", 1, 0),
             min = if_else(variable == "min_value", 1, 0)) %>%
      rename(multiplier_year = year, multiplier_scenario = scenario) %>%
      select(-GAINS_region, -region_grouping, -variable, -policy)

    SSP_EF <- EF_rules %>%
      # Join rules with values
      left_join(EF_all, by = c("GCAM_region_ID", "Non.CO2", "supplysector", "subsector", "stub.technology", "IIASA_sector",
                                    "multiplier_scenario", "multiplier_year", "marker_region", "min")) %>%
      # Multiply non-NA values by multipliers
      mutate(value = replace(value, !is.na(value), multiplier[!is.na(value)] * value[!is.na(value)])) %>%
      select(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology,
             agg_sector = IIASA_sector, year, value, SSP_group) %>%
      group_by(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology,
               agg_sector, SSP_group) %>%
      # Set NA values to previous (non-NA) value
      mutate(value = if_else(is.na(value), lag(value, n = 1L, order_by = year), value),
             value = if_else(is.na(value), lag(value, n = 1L, order_by = year), value),
             value = if_else(is.na(value), lag(value, n = 1L, order_by = year), value),
             # Emission factors cannot increase-if any increases, change it to value from previous time step
             prev = lag(value, n = 1L, order_by = year),
             value = if_else(value > prev & !is.na(prev), prev, value),
             prev = lag(value, n = 1L, order_by = year),
             value = if_else(value > prev & !is.na(prev), prev, value),
             prev = lag(value, n = 1L, order_by = year),
             value = if_else(value > prev & !is.na(prev), prev, value)) %>%
      select(-prev) %>%
      ungroup

    # Split data by SSP grouping
    out_df <- SSP_EF %>%
      split(.$SSP_group) %>%
      lapply(function(df) {
        select(df, -SSP_group)
      })

    # Produce outputs
    out_df[["1&5"]] %>%
      add_title("Emissions factors for SSP 1 and SSP 5") %>%
      add_units("Tg/EJ") %>%
      add_comments("Scales future GAINS emissions factors to L111/L114 base year emissions factors, then applies to GCAM future years") %>%
      add_legacy_name("L161.SSP15_EF") %>%
      add_precursors("emissions/A_regions",
                     "emissions/mappings/GCAM_sector_tech",
                     "emissions/mappings/GCAM_sector_tech_Revised",
                     "emissions/mappings/gains_to_gcam_sector",
                     "emissions/GAINS_activities",
                     "emissions/GAINS_emissions",
                     "L102.pcgdp_thous90USD_Scen_R_Y",
                     "L111.nonghg_tgej_R_en_S_F_Yh",
                     "L114.bcoc_tgej_R_en_S_F_2000",
                     "emissions/A61_emfact_rules") ->
      L161.SSP15_EF

    out_df[["2"]] %>%
      add_title("Emissions factors for SSP 2") %>%
      add_units("Tg/EJ") %>%
      add_comments("Scales future GAINS emissions factors to L111/L114 base year emissions factors, then applies to GCAM future years") %>%
      add_legacy_name("L161.SSP2_EF") %>%
      add_precursors("emissions/A_regions",
                     "emissions/mappings/GCAM_sector_tech",
                     "emissions/mappings/GCAM_sector_tech_Revised",
                     "emissions/mappings/gains_to_gcam_sector",
                     "emissions/GAINS_activities",
                     "emissions/GAINS_emissions",
                     "L102.pcgdp_thous90USD_Scen_R_Y",
                     "L111.nonghg_tgej_R_en_S_F_Yh",
                     "L114.bcoc_tgej_R_en_S_F_2000",
                     "emissions/A61_emfact_rules") ->
      L161.SSP2_EF

    out_df[["3&4"]] %>%
      add_title("Emissions factors for SSP 3 and SSP 4") %>%
      add_units("Tg/EJ") %>%
      add_comments("Scales future GAINS emissions factors to L111/L114 base year emissions factors, then applies to GCAM future years") %>%
      add_legacy_name("L161.SSP34_EF") %>%
      add_precursors("emissions/A_regions",
                     "emissions/mappings/GCAM_sector_tech",
                     "emissions/mappings/GCAM_sector_tech_Revised",
                     "emissions/mappings/gains_to_gcam_sector",
                     "emissions/GAINS_activities",
                     "emissions/GAINS_emissions",
                     "L102.pcgdp_thous90USD_Scen_R_Y",
                     "L111.nonghg_tgej_R_en_S_F_Yh",
                     "L114.bcoc_tgej_R_en_S_F_2000",
                     "emissions/A61_emfact_rules") ->
      L161.SSP34_EF

    return_data(L161.SSP15_EF, L161.SSP2_EF, L161.SSP34_EF)
  } else {
    stop("Unknown command")
  }
}
