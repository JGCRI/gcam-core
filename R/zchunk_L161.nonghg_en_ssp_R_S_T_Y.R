#' module_emissions_L161.nonghg_en_ssp_R_S_T_Y
#'
#' Produces future emissions factors by SSP scenario.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L161.SSP15_EF}, \code{L161.SSP2_EF}, \code{L161.SSP34_EF}. The corresponding file in the
#' original data system was \code{L161.nonghg_en_ssp_R_S_T_Y.R} (emissions level1).
#' @details Scales future GAINS emissions factors to L111/L114 base year emissions factors,
#' then applies future emissions factors to some GCAM years based on SSP-specific rules.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RLH July 2017
module_emissions_L161.nonghg_en_ssp_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/A_regions",
             FILE = "emissions/mappings/GCAM_sector_tech",
             FILE = "emissions/mappings/gains_to_gcam_sector",
             FILE = "emissions/GAINS_activities",
             FILE = "emissions/GAINS_emissions",
             "L102.pcgdp_thous90USD_Scen_R_Y",
             FILE = "temp-data-inject/L111.nonghg_tgej_R_en_S_F_Yh",
             "L114.bcoc_tgej_R_en_S_F_2000"))
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
      variable <- varyear <- varyearpol <- year <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    A_regions <- get_data(all_data, "emissions/A_regions")
    GCAM_sector_tech <- get_data(all_data, "emissions/mappings/GCAM_sector_tech")
    GAINS_sector <- get_data(all_data, "emissions/mappings/gains_to_gcam_sector")
    GAINS_activities <- get_data(all_data, "emissions/GAINS_activities")
    GAINS_emissions <- get_data(all_data, "emissions/GAINS_emissions") %>%
      # NOTE: these are three different scenarios
      # CLE = current legislation, SLE = stringent legislation, MFR = maximum feasible reductions
      gather(variable, value, CLE, MFR, SLE)
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")
    L111.nonghg_tgej_R_en_S_F_Yh <- get_data(all_data, "temp-data-inject/L111.nonghg_tgej_R_en_S_F_Yh") %>%
      # temp-data-inject code
      gather(year, value, starts_with("X")) %>%
      mutate(year = as.integer(substr(year, 2, 5)))
    L114.bcoc_tgej_R_en_S_F_2000 <- get_data(all_data, "L114.bcoc_tgej_R_en_S_F_2000") %>%
      gather(year, value, `2000`) %>%
      mutate(year = as.integer(year))

    # ===================================================

    # Aggregate GAINS emissions data by GCAM sector
    GAINS_emissions_agg <- GAINS_emissions %>%
      # Change pollutant names
      mutate(POLL = replace(POLL, POLL == "NOX", "NOx"),
             POLL = replace(POLL, POLL == "VOC", "NMVOC")) %>%
      # Use left_join because NAs in GAINS_sector
      left_join(GAINS_sector, by = c("TIMER_SECTOR" = "IIASA_Sector")) %>%
      group_by(TIMER_REGION, agg_sector = GCAM_tag, POLL, IDYEARS, variable) %>%
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
      select(TIMER_REGION, agg_sector, POLL, IDYEARS, variable, emfact) %>%
      group_by(TIMER_REGION, agg_sector, POLL, IDYEARS) %>%
      # Replace SLE & MFR base year (2005) emissions factors with CLE emissions factors.
      # They don't all start from the same value.
      mutate(CLE_base = emfact[variable == "CLE"]) %>%
      ungroup() %>%
      mutate(emfact = replace(emfact, IDYEARS == emissions.GAINS_BASE_YEAR, CLE_base[IDYEARS == emissions.GAINS_BASE_YEAR])) %>%
      select(-CLE_base)

    # Compute emissions factor scaler.
    # These scalers are relative to the previous time period's numbers.
    GAINS_emfact_scaler <- GAINS_emfact %>%
      group_by(TIMER_REGION, agg_sector, POLL, variable) %>%
      # Create column of previous time period value
      mutate(prev = lag(emfact, n = 1L, order = IDYEARS)) %>%
      ungroup() %>%
      filter(IDYEARS > emissions.GAINS_BASE_YEAR) %>%
      # Divide current value by previous value, not allowing value greater than 1 (emissions factors cannot increase)
      mutate(scaler = emfact / prev,
             scaler = replace(scaler, scaler > 1, 1)) %>%
      group_by(TIMER_REGION, agg_sector, POLL, variable) %>%
      mutate(scaler = cumprod(scaler)) %>%
      ungroup() %>%
      select(GAINS_region = TIMER_REGION, IIASA_sector = agg_sector, Non.CO2 = POLL, variable, year = IDYEARS, scaler)


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
      # Add in BC/OC emissions factors, assumed that 2005 emissions factors are identical to 2000
      bind_rows(L114.bcoc_tgej_R_en_S_F_2000 %>% mutate(year = emissions.GAINS_BASE_YEAR)) %>%
      # Add GAINS regions and sectors
      left_join_error_no_match(A_regions %>% select(GCAM_region_ID, GAINS_region), by = "GCAM_region_ID") %>%
      left_join(GCAM_sector_tech %>% select(supplysector, subsector, stub.technology, IIASA_sector),
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
             variable, year, emfact, region_grouping)

    # Create list of countries with strong regulation based on elec_coal SO2 emissions factor
    coal_so2 <- tibble(GCAM_region_ID = seq(1, 32)) %>%
      left_join(
        emfact_scaled %>%
          filter(year == "2030", IIASA_sector == "elec_coal", Non.CO2 == "SO2", variable == "CLE"),
        by = "GCAM_region_ID") %>%
      mutate(policy = if_else(emfact <= emissions.COAL_SO2_THRESHOLD, "strong_reg", "weak_reg"),
             policy = replace(policy, region_grouping == "low", "low")) %>%
      replace_na(list(policy = "low")) %>%
      select(GCAM_region_ID, policy)

    # Group SSPs by whether we process them the same
    SSP_groups <- tibble(SSP_group = c("1&5","2","3&4"))
    marker_region <- 13

    # This creates a tibble with the values only from the marker region
    # This will be used to assign to certain future values for certain SSPs
    marker_region_df <- emfact_scaled %>%
      unite(col = varyear, variable, year) %>%
      spread(varyear, emfact) %>%
      filter(GCAM_region_ID == marker_region) %>%
      select(-GCAM_region_ID, -GAINS_region, -region_grouping)

    names(marker_region_df)[grep("CLE|SLE|MFR",names(marker_region_df))] <- paste("marker_region",
                                                                                  names(marker_region_df)[grep("CLE|SLE|MFR",names(marker_region_df))],
                                                                                  sep = "_")

    # This creates a tibble with the lowest emissions factors for certain grouped categories
    # This will be used to assign to certain future values for certain SSPs
    min_EF_policy <- emfact_scaled %>%
      filter(year == emissions.GAINS_YEARS[1]) %>%
      na.omit %>%
      select(-year, -emfact) %>%
      left_join(emfact_scaled, by = c("GCAM_region_ID", "Non.CO2", "supplysector", "subsector", "stub.technology",
                                      "GAINS_region", "IIASA_sector", "variable", "region_grouping")) %>%
      filter(year == emissions.GAINS_YEARS[length(emissions.GAINS_YEARS)]) %>%
      left_join(coal_so2, by = "GCAM_region_ID") %>%
      group_by(Non.CO2, supplysector, subsector, stub.technology, variable, year, region_grouping, policy) %>%
      summarise(emfact = min(emfact, na.rm = TRUE)) %>%
      ungroup %>%
      mutate(variable = paste("min", variable, sep = "_")) %>%
      unite(col = varyearpol, variable, year, policy) %>%
      spread(varyearpol, emfact)


    # Here we assign all future SSP values based on different rules
    # SSP15 HighMed income: 2030 = 0.75*CLE2030; 2050 = SLE2030; 2100 = 0.75*MFR2030
    # SSP15 Low income: 2030 = CLE2030; 2050 = W.Eur CLE2030; 2100 = MFR2030
    # SSP2 HighMed income + strong regulations: 2030 = CLE2030; 2050 = SLE2030; 2100 = min(SLE2030)
    # SSP2 HighMed income + weak regulations: 2030 = CLE2030; 2050 = min(CLE2030); 2100 = W.Eur SLE2030
    # SSP2 Low income: 2030 = CLE2020; 2050 = min(CLE2030); 2100 = SLE2030
    # SSP34 HighMed income: 2030 = CLE2020; 2050 = CLE2030; 2100 = SLE2030
    # SSP34 Low income: 2030 = CLE2010; 2050 = CLE2030; 2100 = W.Eur CLE2020
    SSP_EF <- emfact_scaled %>%
      # This is to have same starting point as old code, which begins with NA omitted 2010 values
      filter(year == emissions.GAINS_YEARS[1]) %>%
      na.omit %>%
      select(-year, -emfact) %>%
      # Rejoining in all data, with no NAs in 2010 values
      left_join(emfact_scaled, by = c("GCAM_region_ID", "Non.CO2", "supplysector", "subsector", "stub.technology",
                                      "GAINS_region", "IIASA_sector", "variable", "region_grouping")) %>%
      unite(col = varyear, variable, year) %>%
      spread(varyear, emfact) %>%
      repeat_add_columns(SSP_groups) %>%
      repeat_add_columns(tibble(year = c(2010, 2030, 2050, 2100))) %>%
      # Adding in policies and other emissions factors
      left_join_error_no_match(coal_so2, by = "GCAM_region_ID") %>%
      left_join(marker_region_df, by = c("Non.CO2", "supplysector", "subsector", "stub.technology", "IIASA_sector")) %>%
      left_join(min_EF_policy, by = c("Non.CO2", "supplysector", "subsector", "stub.technology", "region_grouping")) %>%
      # Initially set all values to CLE_2010, then change all years greater than 2010
      mutate(value = CLE_2010,
             # 1&5, High/Medium Income
             value = replace(value, SSP_group == "1&5" & region_grouping == "highmed" & year == 2030,
                             0.75 * CLE_2030[SSP_group == "1&5" & region_grouping == "highmed" & year == 2030]),
             value = replace(value, SSP_group == "1&5" & region_grouping == "highmed" & year == 2050,
                             SLE_2030[SSP_group == "1&5" & region_grouping == "highmed" & year == 2050]),
             value = replace(value, SSP_group == "1&5" & region_grouping == "highmed" & year == 2100,
                             0.75 * MFR_2030[SSP_group == "1&5" & region_grouping == "highmed" & year == 2100]),
             # 1&5, Low Income
             value = replace(value, SSP_group == "1&5" & region_grouping == "low" & year == 2030,
                             CLE_2030[SSP_group == "1&5" & region_grouping == "low" & year == 2030]),
             value = replace(value, SSP_group == "1&5" & region_grouping == "low" & year == 2050,
                             marker_region_CLE_2030[SSP_group == "1&5" & region_grouping == "low" & year == 2050]),
             value = replace(value, SSP_group == "1&5" & region_grouping == "low" & year == 2100,
                             MFR_2030[SSP_group == "1&5" & region_grouping == "low" & year == 2100]),
             # 2, High/Medium Income, Strong Policies
             value = replace(value, SSP_group == "2" & region_grouping == "highmed" & year == 2030 & policy == "strong_reg",
                             CLE_2030[SSP_group == "2" & region_grouping == "highmed" & year == 2030 & policy == "strong_reg"]),
             value = replace(value, SSP_group == "2" & region_grouping == "highmed" & year == 2050 & policy == "strong_reg",
                             SLE_2030[SSP_group == "2" & region_grouping == "highmed" & year == 2050 & policy == "strong_reg"]),
             value = replace(value, SSP_group == "2" & region_grouping == "highmed" & year == 2100 & policy == "strong_reg",
                             min_SLE_2030_strong_reg[SSP_group == "2" & region_grouping == "highmed" & year == 2100 & policy == "strong_reg"]),
             # 2, High/Medium Income, Weak Policies
             value = replace(value, SSP_group == "2" & region_grouping == "highmed" & year == 2030 & policy == "weak_reg",
                             CLE_2030[SSP_group == "2" & region_grouping == "highmed" & year == 2030 & policy == "weak_reg"]),
             value = replace(value, SSP_group == "2" & region_grouping == "highmed" & year == 2050 & policy == "weak_reg",
                             min_CLE_2030_weak_reg[SSP_group == "2" & region_grouping == "highmed" & year == 2050 & policy == "weak_reg"]),
             value = replace(value, SSP_group == "2" & region_grouping == "highmed" & year == 2100 & policy == "weak_reg",
                             marker_region_SLE_2030[SSP_group == "2" & region_grouping == "highmed" & year == 2100 & policy == "weak_reg"]),
             # 2, Low Income
             value = replace(value, SSP_group == "2" & region_grouping == "low" & year == 2030,
                             CLE_2020[SSP_group == "2" & region_grouping == "low" & year == 2030]),
             value = replace(value, SSP_group == "2" & region_grouping == "low" & year == 2050,
                             min_CLE_2030_low[SSP_group == "2" & region_grouping == "low" & year == 2050]),
             value = replace(value, SSP_group == "2" & region_grouping == "low" & year == 2100,
                             SLE_2030[SSP_group == "2" & region_grouping == "low" & year == 2100]),
             # 3&4, High/Medium Income
             value = replace(value, SSP_group == "3&4" & region_grouping == "highmed" & year == 2030,
                             CLE_2020[SSP_group == "3&4" & region_grouping == "highmed" & year == 2030]),
             value = replace(value, SSP_group == "3&4" & region_grouping == "highmed" & year == 2050,
                             CLE_2030[SSP_group == "3&4" & region_grouping == "highmed" & year == 2050]),
             value = replace(value, SSP_group == "3&4" & region_grouping == "highmed" & year == 2100,
                             SLE_2030[SSP_group == "3&4" & region_grouping == "highmed" & year == 2100]),
             # 3&4, Low Income
             value = replace(value, SSP_group == "3&4" & region_grouping == "low" & year == 2030,
                             CLE_2010[SSP_group == "3&4" & region_grouping == "low" & year == 2030]),
             value = replace(value, SSP_group == "3&4" & region_grouping == "low" & year == 2050,
                             CLE_2030[SSP_group == "3&4" & region_grouping == "low" & year == 2050]),
             value = replace(value, SSP_group == "3&4" & region_grouping == "low" & year == 2100,
                             marker_region_CLE_2020[SSP_group == "3&4" & region_grouping == "low" & year == 2100])
      ) %>%
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
      lapply(function(df){
        select(df, -SSP_group)
      })

    # Region 25 is dropped in old ds because it does not have policy (elec_coal data does not exist)
    # Unsure of what correct behavior is
    if (OLD_DATA_SYSTEM_BEHAVIOR){
      out_df[["2"]] <- out_df[["2"]] %>%
        filter(GCAM_region_ID != 25)
    }
    # ===================================================

    # Produce outputs
    out_df[["1&5"]] %>%
      add_title("Emissions factors for SSP 1 and SSP 5") %>%
      add_units("Tg/EJ") %>%
      add_comments("Scales future GAINS emissions factors to L111/L114 base year emissions factors, then applies to GCAM future years") %>%
      add_legacy_name("L161.SSP15_EF") %>%
      add_precursors("emissions/A_regions",
                     "emissions/mappings/GCAM_sector_tech",
                     "emissions/mappings/gains_to_gcam_sector",
                     "emissions/GAINS_activities",
                     "emissions/GAINS_emissions",
                     "L102.pcgdp_thous90USD_Scen_R_Y",
                     "temp-data-inject/L111.nonghg_tgej_R_en_S_F_Yh",
                     "L114.bcoc_tgej_R_en_S_F_2000") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.SSP15_EF
    out_df[["2"]] %>%
      add_title("Emissions factors for SSP 2") %>%
      add_units("Tg/EJ") %>%
      add_comments("Scales future GAINS emissions factors to L111/L114 base year emissions factors, then applies to GCAM future years") %>%
      add_legacy_name("L161.SSP2_EF") %>%
      add_precursors("emissions/A_regions",
                     "emissions/mappings/GCAM_sector_tech",
                     "emissions/mappings/gains_to_gcam_sector",
                     "emissions/GAINS_activities",
                     "emissions/GAINS_emissions",
                     "L102.pcgdp_thous90USD_Scen_R_Y",
                     "temp-data-inject/L111.nonghg_tgej_R_en_S_F_Yh",
                     "L114.bcoc_tgej_R_en_S_F_2000") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.SSP2_EF
    out_df[["3&4"]] %>%
      add_title("Emissions factors for SSP 3 and SSP 4") %>%
      add_units("Tg/EJ") %>%
      add_comments("Scales future GAINS emissions factors to L111/L114 base year emissions factors, then applies to GCAM future years") %>%
      add_legacy_name("L161.SSP34_EF") %>%
      add_precursors("emissions/A_regions",
                     "emissions/mappings/GCAM_sector_tech",
                     "emissions/mappings/gains_to_gcam_sector",
                     "emissions/GAINS_activities",
                     "emissions/GAINS_emissions",
                     "L102.pcgdp_thous90USD_Scen_R_Y",
                     "temp-data-inject/L111.nonghg_tgej_R_en_S_F_Yh",
                     "L114.bcoc_tgej_R_en_S_F_2000") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.SSP34_EF

    return_data(L161.SSP15_EF, L161.SSP2_EF, L161.SSP34_EF)
  } else {
    stop("Unknown command")
  }
}



