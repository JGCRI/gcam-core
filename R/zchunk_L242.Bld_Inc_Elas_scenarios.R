#' module_socioeconomics_L242.Bld_Inc_Elas_scenarios
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L242.IncomeElasticity_bld_GCAM3}, \code{object}. The corresponding file in the
#' original data system was \code{L242.Bld_Inc_Elas_scenarios.R} (socioeconomics level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RH April 2017
#' @export
module_socioeconomics_L242.Bld_Inc_Elas_scenarios <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A42.demand",
             FILE = "socioeconomics/A42.inc_elas",
             FILE = "L101.Pop_thous_GCAM3_R_Y",
             FILE = "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y",
             FILE = "temp-data-inject/L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L242.IncomeElasticity_bld_GCAM3",
             "L242.IncomeElasticity_bld_gSSP1",
             "L242.IncomeElasticity_bld_gSSP2",
             "L242.IncomeElasticity_bld_gSSP3",
             "L242.IncomeElasticity_bld_gSSP4",
             "L242.IncomeElasticity_bld_gSSP5",
             "L242.IncomeElasticity_bld_SSP1",
             "L242.IncomeElasticity_bld_SSP2",
             "L242.IncomeElasticity_bld_SSP3",
             "L242.IncomeElasticity_bld_SSP4",
             "L242.IncomeElasticity_bld_SSP5"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A42.demand <- get_data(all_data, "energy/A42.demand")
    A42.inc_elas <- get_data(all_data, "socioeconomics/A42.inc_elas")
    L101.Pop_thous_GCAM3_R_Y <- get_data(all_data, "L101.Pop_thous_GCAM3_R_Y")
    L102.gdp_mil90usd_GCAM3_R_Y <- get_data(all_data, "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y") %>%
      gather(year, gdp, starts_with('X')) %>%
      mutate(year = as.integer(substr(year,2,5)))
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "temp-data-inject/L102.pcgdp_thous90USD_Scen_R_Y") %>%
      gather(year, pcgdp_90thousUSD, starts_with('X')) %>%
      mutate(year = as.integer(substr(year,2,5)))

    # ===================================================
    # Joining population and GDP data in order to calculate per capita GDP
    L242.pcgdp_GCAM3_R_Y <- L101.Pop_thous_GCAM3_R_Y %>%
      rename(population = value) %>%
      left_join_error_no_match(L102.gdp_mil90usd_GCAM3_R_Y, by = c("GCAM_region_ID","year")) %>%
      mutate(pcgdp_90thousUSD = gdp/population) %>%
      select(-population, -gdp) %>%
      filter(year %in% FUTURE_YEARS)

    # Linearly interpolate income elasticity for each level of per-capita GDP,
    # using the assumption data
    L242.IncomeElasticity_bld_GCAM3 <- L242.pcgdp_GCAM3_R_Y %>%
      mutate(income.elasticity = approx(x = A42.inc_elas$pcgdp_90thousUSD, y = A42.inc_elas$inc_elas,
                                        xout = pcgdp_90thousUSD,
                                        # Rule 2 means that data outside of the interval of input
                                        # data will be assigned the cloest data extreme
                                        rule = 2)$y %>% round(3),
             energy.final.demand = A42.demand$energy.final.demand) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      ungroup(GCAM_region_ID) %>%
      select(region, energy.final.demand, year, income.elasticity) %>%
      arrange(year)

    # Scenarios gSSPs - same process as above using L1 GDP per capita by SSP scenario file
    # Linearly interpolate income elasticity for each level of per-capita GDP,
    # using the assumption data
    L242.pcgdp_thous90USD_Scen_R_Y <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(year %in% FUTURE_YEARS) %>%
      mutate(income.elasticity = approx(x = A42.inc_elas$pcgdp_90thousUSD, y = A42.inc_elas$inc_elas,
                                        xout = pcgdp_90thousUSD,
                                        # Rule 2 means that data outside of the interval of input
                                        # data will be assigned the cloest data extreme
                                        rule = 2)$y %>% round(3),
             energy.final.demand = A42.demand$energy.final.demand) %>%
      select(scenario, region, energy.final.demand, year, income.elasticity) %>%
      arrange(year)

    for (scen in L242.pcgdp_thous90USD_Scen_R_Y$scenario){
        assign(paste0("L242.IncomeElasticity_bld_",scen),
               L242.pcgdp_thous90USD_Scen_R_Y %>%
                 filter(scen %in% scenario) %>%
                 select(-scenario)
                 )
    }

    # ===================================================

    # Produce outputs

    L242.IncomeElasticity_bld_GCAM3 %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.IncomeElasticity_bld_GCAM3") %>%
      add_precursors("common/GCAM_region_names", "energy/A42.demand", "socioeconomics/A42.inc_elas",
                     "L101.Pop_thous_GCAM3_R_Y","temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y") %>%
      add_flags() ->
      L242.IncomeElasticity_bld_GCAM3

    L242.IncomeElasticity_bld_gSSP1 %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.IncomeElasticity_bld_gSSP1") %>%
      add_precursors("common/GCAM_region_names", "energy/A42.demand", "socioeconomics/A42.inc_elas",
                     "temp-data-inject/L102.pcgdp_thous90USD_Scen_R_Y") ->
      L242.IncomeElasticity_bld_gSSP1

    L242.IncomeElasticity_bld_gSSP2 %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.IncomeElasticity_bld_gSSP1") %>%
      add_precursors("common/GCAM_region_names", "energy/A42.demand", "socioeconomics/A42.inc_elas",
                     "temp-data-inject/L102.pcgdp_thous90USD_Scen_R_Y") ->
      L242.IncomeElasticity_bld_gSSP2

    L242.IncomeElasticity_bld_gSSP3 %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.IncomeElasticity_bld_gSSP1") %>%
      add_precursors("common/GCAM_region_names", "energy/A42.demand", "socioeconomics/A42.inc_elas",
                     "temp-data-inject/L102.pcgdp_thous90USD_Scen_R_Y") ->
      L242.IncomeElasticity_bld_gSSP3

    L242.IncomeElasticity_bld_gSSP4 %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.IncomeElasticity_bld_gSSP1") %>%
      add_precursors("common/GCAM_region_names", "energy/A42.demand", "socioeconomics/A42.inc_elas",
                     "temp-data-inject/L102.pcgdp_thous90USD_Scen_R_Y") ->
      L242.IncomeElasticity_bld_gSSP4

    L242.IncomeElasticity_bld_gSSP5 %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.IncomeElasticity_bld_gSSP1") %>%
      add_precursors("common/GCAM_region_names", "energy/A42.demand", "socioeconomics/A42.inc_elas",
                     "temp-data-inject/L102.pcgdp_thous90USD_Scen_R_Y") ->
      L242.IncomeElasticity_bld_gSSP5

    L242.IncomeElasticity_bld_SSP1 %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.IncomeElasticity_bld_gSSP1") %>%
      add_precursors("common/GCAM_region_names", "energy/A42.demand", "socioeconomics/A42.inc_elas",
                     "temp-data-inject/L102.pcgdp_thous90USD_Scen_R_Y") ->
      L242.IncomeElasticity_bld_SSP1

    L242.IncomeElasticity_bld_SSP2 %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.IncomeElasticity_bld_gSSP1") %>%
      add_precursors("common/GCAM_region_names", "energy/A42.demand", "socioeconomics/A42.inc_elas",
                     "temp-data-inject/L102.pcgdp_thous90USD_Scen_R_Y") ->
      L242.IncomeElasticity_bld_SSP2

    L242.IncomeElasticity_bld_SSP3 %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.IncomeElasticity_bld_gSSP1") %>%
      add_precursors("common/GCAM_region_names", "energy/A42.demand", "socioeconomics/A42.inc_elas",
                     "temp-data-inject/L102.pcgdp_thous90USD_Scen_R_Y") ->
      L242.IncomeElasticity_bld_SSP3

    L242.IncomeElasticity_bld_SSP4 %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.IncomeElasticity_bld_gSSP1") %>%
      add_precursors("common/GCAM_region_names", "energy/A42.demand", "socioeconomics/A42.inc_elas",
                     "temp-data-inject/L102.pcgdp_thous90USD_Scen_R_Y") ->
      L242.IncomeElasticity_bld_SSP4

    L242.IncomeElasticity_bld_SSP5 %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L242.IncomeElasticity_bld_gSSP1") %>%
      add_precursors("common/GCAM_region_names", "energy/A42.demand", "socioeconomics/A42.inc_elas",
                     "temp-data-inject/L102.pcgdp_thous90USD_Scen_R_Y") ->
      L242.IncomeElasticity_bld_SSP5

    return_data(L242.IncomeElasticity_bld_GCAM3,
                L242.IncomeElasticity_bld_gSSP1,
                L242.IncomeElasticity_bld_gSSP2,
                L242.IncomeElasticity_bld_gSSP3,
                L242.IncomeElasticity_bld_gSSP4,
                L242.IncomeElasticity_bld_gSSP5,
                L242.IncomeElasticity_bld_SSP1,
                L242.IncomeElasticity_bld_SSP2,
                L242.IncomeElasticity_bld_SSP3,
                L242.IncomeElasticity_bld_SSP4,
                L242.IncomeElasticity_bld_SSP5)
  } else {
    stop("Unknown command")
  }
}
