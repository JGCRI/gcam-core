# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L100.SSP_database
#'
#'  preprocess SSP database for population, GDP, and labor force
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.Pop_thous_SSP_ctry_Yfut_raw},
#'  \code{L100.LaborForce_mil_SSP_ctry_Yfut_raw}, \code{L100.GDP_bilusd_SSP_ctry_Yfut_raw}
#' @details preprocess SSP database for population, GDP, and labor force
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter full_join if_else group_by left_join mutate order_by select summarize bind_rows
#' @importFrom tidyr complete nesting replace_na spread
#' @author XZ 2025
module_socio_L100.SSP_database <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "socioeconomics/SSP/SSP_database_2025",
      FILE = "socioeconomics/SSP/iso_SSP_regID",
      FILE = "socioeconomics/SSP/pop_laborforce_variable",
      FILE = "common/iso_GCAM_regID")

  MODULE_OUTPUTS <-
    c("L100.LaborForce_mil_SSP_ctry_Yfut_raw",
      "L100.Pop_thous_SSP_ctry_Yfut_raw",
      "L100.GDP_bilusd_SSP_ctry_Yfut_raw")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    ## silence package check.
    scenario <- year <- gdp <- GCAM_region_ID <- account <- Region <- region <-
      GDP <- pop <- NULL


    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Note that the SSP database had a region backcasting already to disaggregate
    # historically dissolved countries
    # E.g., no Soviet Union but had Russia since beginning

    # (1) Using SSP database to derive future population and labor force ----
    SSP_database_2025 %>%
      # make variable names lower case
      dplyr::rename_all(tolower) %>%
      # remove aggregated regions
      filter(!grepl("\\(|World", region),
             model == "IIASA-WiC POP 2025") %>%
      left_join_error_no_match(
        iso_SSP_regID %>% distinct(iso, region = ssp_country_name),
        by = "region") %>%
      gather_years() ->
      SSP_pop_0

    # Using the Historical Reference scenario to fill history of SSPs
    SSP_pop_0 %>%
      filter(scenario != "Historical Reference") %>%
      # NA exist in SSP database (no LJENM)
      left_join(
        SSP_pop_0 %>%
          filter(scenario == "Historical Reference") %>% select(-scenario) %>%
          rename(hist = value),
        by = c("model", "region", "variable", "unit", "iso", "year")
      ) %>%
      # new ssp data starts 2025 (socioeconomics.SSP_DB_BASEYEAR)
      mutate(value = if_else(year < socioeconomics.SSP_DB_BASEYEAR, hist, value)) %>%
      select(-hist) ->
      SSP_pop_1

    SSP_pop_1 %>%
      filter(variable == "Population") %>%
      transmute(scenario, iso, var = "pop", unit, year, value) ->
      pop.ssp

    ## (1.1) population ----
    pop.ssp %>%
      select(iso, scenario, year, pop = value) %>%
      add_title("SSP population projections by country, from base year to 2100") %>%
      add_units("thousand") %>%
      add_comments("The implied growth ratios will be applied to historical values from UN or other sources") %>%
      add_legacy_name("L100.Pop_thous_SSP_ctry_Yfut_raw") %>%
      add_precursors("socioeconomics/SSP/SSP_database_2025",
                     "socioeconomics/SSP/iso_SSP_regID") ->
      L100.Pop_thous_SSP_ctry_Yfut_raw


    SSP_pop_1 %>%
      left_join_error_no_match(
        # mapping file
        pop_laborforce_variable, by = "variable") %>%
      filter(laborforce_var == TRUE, year >= socioeconomics.SSP_DB_Labor_StartYear) %>%
      # Note that in historical years, population was differentiated by education only after
      # 2020 when education data was made available in SSP
      # We also do not have that differentiation now (NA could cause issues)
      # Since the population to labor force will have another rescaling when connecting to
      # PWT labor force base values
      # year >= 2020 should have been true already if removed NA earlier
      group_by(scenario, iso, unit, year) %>%
      summarize(value = sum(value, na.rm = T)) %>% ungroup %>%
      mutate(var = "labor.force") ->
      labor.force.ssp

    #include total SSP population (pop) in table
    labor.force.ssp %>%
      bind_rows(pop.ssp) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      select(scenario, iso, GCAM_region_ID, var, year, value, unit) %>%
      arrange(scenario, iso, var, year) ->
      L100.LaborForce_mil_SSP_ctry_Yfut_raw

    ## (1.2) labor force (with population) ----
    L100.LaborForce_mil_SSP_ctry_Yfut_raw %>%
      add_title("Labor Force and Pop by SSP Scenarios") %>%
      add_units("millions") %>%
      add_comments("Total pop and working age population") %>%
      add_legacy_name("L100.LaborForce_mil_SSP_ctry_Yfut_raw") %>%
      add_precursors("common/iso_GCAM_regID",
                     "socioeconomics/SSP/SSP_database_2025",
                     "socioeconomics/SSP/pop_laborforce_variable",
                     "socioeconomics/SSP/iso_SSP_regID") ->
      L100.LaborForce_mil_SSP_ctry_Yfut_raw


    # (2) SSP GDP billions of 2017$ ----

    assertthat::assert_that("billion USD_2017/yr" %in%
                              c(SSP_database_2025 %>%  dplyr::rename_all(tolower) %>%
                                  distinct(unit) %>% pull))

    SSP_database_2025 %>%
      # make variable names lower case
      dplyr::rename_all(tolower) %>%
      # remove aggregated regions
      filter(!grepl("\\(|World", region),
             model == 'OECD ENV-Growth 2025',
             variable == 'GDP|PPP',
             unit == "billion USD_2017/yr") %>%
      left_join_error_no_match(
        iso_SSP_regID %>% distinct(iso, region = ssp_country_name),
        by = "region") %>%
      gather_years()->
      SSP_gdp_0

    assertthat::assert_that(
      c(paste0("SSP", 1:5), "Historical Reference") %in%
        c(SSP_gdp_0 %>% distinct(scenario) %>% pull) %>% all() )

    # Using the Historical Reference scenario to fill history of SSPs
    SSP_gdp_0 %>%
      filter(scenario != "Historical Reference") %>%
      # NA exist in SSP database (no LJENM)
      left_join(
        SSP_gdp_0 %>% filter(scenario == "Historical Reference") %>% select(-scenario) %>%
          rename(hist = value),
        by = c("model", "region", "variable", "unit", "iso", "year")
      ) %>%
      # new ssp data starts 2025 (socioeconomics.SSP_DB_BASEYEAR)
      mutate(value = if_else(year < socioeconomics.SSP_DB_BASEYEAR, hist, value)) %>%
      select(iso, scenario, year, gdp = value) ->
      L100.GDP_bilusd_SSP_ctry_Yfut_raw

    ## Units are billions of 2017$ but relative ratio will be used when connecting to historical data

    L100.GDP_bilusd_SSP_ctry_Yfut_raw %>%
      add_title("SSP GDP projections by country, from base year to 2100") %>%
      add_units("billion 2017$ PPP") %>%
      add_comments("Relative ratio will be used when connecting to historical data") %>%
      add_legacy_name("L100.GDP_bilusd_SSP_ctry_Yfut_raw") %>%
      add_precursors("socioeconomics/SSP/SSP_database_2025",
                     "socioeconomics/SSP/iso_SSP_regID") ->
      L100.GDP_bilusd_SSP_ctry_Yfut_raw


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
