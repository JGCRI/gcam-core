#' module_emissions_L161.nonghg_en_ssp_R_S_T_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L161.SSP15_EF}, \code{L161.SSP2_EF}, \code{L161.SSP34_EF}. The corresponding file in the
#' original data system was \code{L161.nonghg_en_ssp_R_S_T_Y.R} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RLH July 2017
#' @export
module_emissions_L161.nonghg_en_ssp_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/A_regions",
             FILE = "emissions/mappings/GCAM_sector_tech",
             FILE = "emissions/mappings/gains_to_gcam_sector", # source
             FILE = "emissions/GAINS_activities", # source
             FILE = "emissions/GAINS_emissions", # source
             "L102.pcgdp_thous90USD_Scen_R_Y",
             FILE = "temp-data-inject/L111.nonghg_tgej_R_en_S_F_Yh",
             "L114.bcoc_tgej_R_en_S_F_2000"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L161.SSP15_EF",
             "L161.SSP2_EF",
             "L161.SSP34_EF"))
  } else if(command == driver.MAKE) {

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
      # Get rid of any years before base year
      filter(IDYEARS >= emissions.GAINS_BASE_YEAR) %>%
      spread(IDYEARS, emfact) %>%
      na.omit() %>%
      gather(IDYEARS, emfact, matches(YEAR_PATTERN)) %>%
      group_by(TIMER_REGION, agg_sector, POLL, variable) %>%
      # Set scaler equal to emfact.base_value and don't allow any value greater than 1
      mutate(prev = lag(emfact, n = 1L, order = IDYEARS)) %>%
      ungroup() %>%
      filter(IDYEARS > emissions.GAINS_BASE_YEAR) %>%
      mutate(scaler = emfact / prev,
             scaler = replace(scaler, scaler > 1, 1)) %>%
      select(GAINS_region = TIMER_REGION, IIASA_sector = agg_sector, Non.CO2 = POLL, variable, year = IDYEARS, scaler)


    # Determine region groupings
    pcgdp_2010 <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(scenario == "SSP4", year == 2010) %>%
      mutate(value = value * gdp_deflator(2010, 1990),
             region_grouping = if_else(value >= emissions.LOW_PCGDP, "highmed", "low"))

    # Compute future emissions factors for GAINS scenarios
    emfact_scaled <- L111.nonghg_tgej_R_en_S_F_Yh %>%
      filter(year == emissions.GAINS_BASE_YEAR) %>%
      # Add in BC/OC emissions factors, assumed that 2005 emissions factors are identical to 2000
      bind_rows(L114.bcoc_tgej_R_en_S_F_2000 %>% mutate(year = 2005)) %>%
      # Add GAINS regions and sectors
      left_join_error_no_match(A_regions %>% select(GCAM_region_ID, GAINS_region), by = "GCAM_region_ID") %>%
      left_join(GCAM_sector_tech %>% select(supplysector, subsector, stub.technology, IIASA_sector),
                by = c("supplysector", "subsector", "stub.technology")) %>%
      # Remove non-IIASA sectors and technologies with 0 emissions factor in base year. No reason to read in future zeroes.
      filter(!is.na(IIASA_sector), value != 0) %>%
      rename(base_year = year, base_value = value) %>%
      left_join(GAINS_emfact_scaler, by = c("GAINS_region", "IIASA_sector", "Non.CO2")) %>%
      mutate(emfact = base_value * scaler) %>%
      left_join_error_no_match(pcgdp_2010 %>% select(GCAM_region_ID, region_grouping), by = "GCAM_region_ID") %>%
      na.omit() %>%
      select(GCAM_region_ID, Non.CO2, supplysector, subsector, stub.technology, GAINS_region, IIASA_sector,
             variable, year, emfact, region_grouping)

    coal_so2 <- emfact_scaled %>%
      filter(year == "2030", IIASA_sector == "elec_coal", Non.CO2 == "SO2", region_grouping == "highmed") %>%
      group_by(GCAM_region_ID) %>%
      mutate(policy = if_else(emfact[variable == "CLE"] <= emissions.COAL_SO2_THRESHOLD, "strong_reg", "weak_reg")) %>%
      ungroup %>%
      select(GCAM_region_ID, policy) %>%
      distinct()

    # Group SSPs by whether we process them the same
    SSP_groups <- tibble(SSP_group = c("1&5","2","3&4"))
    marker_region <- 13

    marker_region_df <- emfact_scaled %>%
      unite(col = varyear, variable, year) %>%
      spread(varyear, emfact) %>%
      filter(GCAM_region_ID == marker_region) %>%
      select(-GCAM_region_ID, -GAINS_region, -region_grouping)

    names(marker_region_df)[grep("CLE|SLE|MFR",names(marker_region_df))] <- paste("marker_region",
                                                                                  names(marker_region_df)[grep("CLE|SLE|MFR",names(marker_region_df))],
                                                                                  sep = "_")
    min_EF_policy <- emfact_scaled %>%
      filter(year == 2030) %>%
      # Use left_join because not all regions in coal_so2
      left_join(coal_so2, by = "GCAM_region_ID") %>%
      replace_na(list(policy = "low")) %>%
      group_by(Non.CO2, supplysector, subsector, stub.technology, variable, year, region_grouping, policy) %>%
      summarise(emfact = min(emfact)) %>%
      ungroup %>%
      mutate(variable = paste("min", variable, sep = "_")) %>%
      unite(col = varyearpol, variable, year, policy) %>%
      spread(varyearpol, emfact)

    SSP_EF <- emfact_scaled %>%
      unite(col = varyear, variable, year) %>%
      spread(varyear, emfact) %>%
      repeat_add_columns(SSP_groups) %>%
      repeat_add_columns(tibble(year = c(2010, 2030, 2050, 2100))) %>%
      # Use left_join because not all regions in coal_so2
      left_join(coal_so2, by = "GCAM_region_ID") %>%
      left_join(marker_region_df, by = c("Non.CO2", "supplysector", "subsector", "stub.technology", "IIASA_sector")) %>%
      left_join(min_EF_policy, by = c("Non.CO2", "supplysector", "subsector", "stub.technology", "region_grouping")) %>%
      # Initially set all values to CLE_2010, then change all years above 2010
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
                             min_CLE_2030_weak_reg[SSP_group == "2" & region_grouping == "highmed" & year == 2050] & policy == "weak_reg"),
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
      # Set NA values to value 1 or 2 time steps previous
      mutate(value = if_else(is.na(value), lag(value, n = 1L, order_by = year), value),
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

    out_df <- SSP_EF %>%
      split(.$SSP_group) %>%
      lapply(function(df){
        select(df, -SSP_group)
      })

    # NOTE: This code converts gdp using a conv_xxxx_xxxx_USD constant
# Use the `gdp_deflator(year, base_year)` function instead
    # ===================================================

    # Produce outputs
    out_df[["1&5"]] %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
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
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L161.SSP2_EF") %>%
      add_precursors("emissions/A_regions") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.SSP2_EF
    out_df[["3&4"]] %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L161.SSP34_EF") %>%
      add_precursors("emissions/A_regions") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.SSP34_EF

    return_data(L161.SSP15_EF, L161.SSP2_EF, L161.SSP34_EF)
  } else {
    stop("Unknown command")
  }
}



