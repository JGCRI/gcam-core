#' module_socioeconomics_L252.Trn_Inc_Elas_scenarios
#'
#' Calculates transportation income elasticity for GCAM3 & SSP scenarios using linear interpolation of assumption data
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L252.IncomeElasticity_trn_GCAM3}, \code{object}. The corresponding file in the
#' original data system was \code{L252.Trn_Inc_Elas_scenarios.R} (socioeconomics level2).
#' @details Calculates per-capita GDP for GCAM3. Uses that calculation and per-capita GDP data for SSP scenarios to
#' generate transportation income elasticity for GCAM3 & SSP scenarios using linear interpolation of assumption data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RH May 2017

module_socioeconomics_L252.Trn_Inc_Elas_scenarios <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A52.demand",
             FILE = "socioeconomics/A52.inc_elas",
             "L101.Pop_thous_GCAM3_R_Y",
             FILE = "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y",
             FILE = "temp-data-inject/L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L252.IncomeElasticity_trn_GCAM3",
             "L252.IncomeElasticity_trn_gSSP1",
             "L252.IncomeElasticity_trn_gSSP2",
             "L252.IncomeElasticity_trn_gSSP3",
             "L252.IncomeElasticity_trn_gSSP4",
             "L252.IncomeElasticity_trn_gSSP5",
             "L252.IncomeElasticity_trn_SSP1",
             "L252.IncomeElasticity_trn_SSP2",
             "L252.IncomeElasticity_trn_SSP3",
             "L252.IncomeElasticity_trn_SSP4",
             "L252.IncomeElasticity_trn_SSP5"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A52.demand <- get_data(all_data, "energy/A52.demand")
    A52.inc_elas <- get_data(all_data, "socioeconomics/A52.inc_elas")
    L101.Pop_thous_GCAM3_R_Y <- get_data(all_data, "L101.Pop_thous_GCAM3_R_Y") %>%
      rename(pop = value)
    L102.gdp_mil90usd_GCAM3_R_Y <- get_data(all_data, "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y") %>%
      # Temporary for temp-data-inject data
      gather(year, gdp, starts_with("X")) %>%
      mutate(year = as.integer(substr(year, 2, 5)))
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "temp-data-inject/L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # Temporary for temp-data-inject data
      gather(year, pcgdp_90thousUSD, starts_with("X")) %>%
      mutate(year = as.integer(substr(year, 2, 5)))

    # ===================================================
    # For the GCAM 3.0 scenario, calculate the per-capita GDP
    L252.pcgdp_GCAM3_R_Y <- left_join_error_no_match(L101.Pop_thous_GCAM3_R_Y, L102.gdp_mil90usd_GCAM3_R_Y, by = c("GCAM_region_ID", "year")) %>%
      mutate(pcgdp_90thousUSD = gdp/pop) %>%
      select(-pop, -gdp) %>%
      filter(year %in% FUTURE_YEARS)

    # Linearly interpolate income elasticity at each level of per-capita GDP
    L252.IncomeElasticity_trn_GCAM3 <- L252.pcgdp_GCAM3_R_Y %>%
      mutate(income.elasticity = approx(x = A52.inc_elas$pcgdp_90thousUSD, y = A52.inc_elas$inc_elas,
                                        # Rule 2 means that data outside of the interval of input
                                        # data will be assigned the closest data extreme
                                        xout = pcgdp_90thousUSD, rule = 2 )$y,
             energy.final.demand = A52.demand$energy.final.demand) %>%
      # Add in region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      ungroup(GCAM_region_ID) %>%
      select(-pcgdp_90thousUSD, -GCAM_region_ID)

    # For SSP scenarios, linearly interpolate income elasticity at each level of per-capita GDP
    L252.IncomeElasticity_trn_SSP <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(year %in% FUTURE_YEARS) %>%
      mutate(income.elasticity = approx(x = A52.inc_elas$pcgdp_90thousUSD, y = A52.inc_elas$inc_elas,
                                        # Rule 2 means that data outside of the interval of input
                                        # data will be assigned the closest data extreme
                                        xout = pcgdp_90thousUSD, rule = 2 )$y,
             energy.final.demand = A52.demand$energy.final.demand) %>%
      # Add in region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      ungroup(GCAM_region_ID) %>%
      select(-pcgdp_90thousUSD, -GCAM_region_ID)

    # Split by scenario - turns into list of tibbles
    L252.IncomeElasticity_trn_SSP <- L252.IncomeElasticity_trn_SSP %>%
      split(.$scenario) %>%
      lapply(function(df) {select(df, -scenario)})
    # ===================================================

    # Produce outputs
    L252.IncomeElasticity_trn_GCAM3 %>%
      add_title("Transportation Income Elasticity: GCAM3") %>%
      add_units("Unitless (% change in service demand / % change in income)") %>%
      add_comments("Calculates per-capita GDP using GCAM3 GDP and population") %>%
      add_comments("Transportation income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
      add_legacy_name("L252.IncomeElasticity_trn_GCAM3") %>%
      add_precursors("common/GCAM_region_names", "energy/A52.demand", "socioeconomics/A52.inc_elas",
                     "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "temp-data-inject/L102.pcgdp_thous90USD_Scen_R_Y") ->
      L252.IncomeElasticity_trn_GCAM3
    L252.IncomeElasticity_trn_SSP[['gSSP1']] %>%
      add_title("Transportation Income Elasticity: gSSP1") %>%
      add_units("Unitless (% change in service demand / % change in income)") %>%
      add_comments("Uses previously calculated per-capita GDP assumptions for all SSP scenarios") %>%
      add_comments("Transportation income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
      add_legacy_name("L252.IncomeElasticity_trn_gSSP1") %>%
      add_precursors("common/GCAM_region_names", "energy/A52.demand",
                     "socioeconomics/A52.inc_elas","L101.Pop_thous_GCAM3_R_Y") ->
      L252.IncomeElasticity_trn_gSSP1
    L252.IncomeElasticity_trn_SSP[['gSSP2']] %>%
      add_title("Transportation Income Elasticity: gSSP2") %>%
      add_units("Unitless (% change in service demand / % change in income)") %>%
      add_comments("Uses previously calculated per-capita GDP assumptions for all SSP scenarios") %>%
      add_comments("Transportation income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
      add_legacy_name("L252.IncomeElasticity_trn_gSSP2") %>%
      add_precursors("common/GCAM_region_names", "energy/A52.demand",
                     "socioeconomics/A52.inc_elas","L101.Pop_thous_GCAM3_R_Y") ->
      L252.IncomeElasticity_trn_gSSP2
    L252.IncomeElasticity_trn_SSP[['gSSP3']] %>%
      add_title("Transportation Income Elasticity: gSSP3") %>%
      add_units("Unitless (% change in service demand / % change in income)") %>%
      add_comments("Uses previously calculated per-capita GDP assumptions for all SSP scenarios") %>%
      add_comments("Transportation income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
      add_legacy_name("L252.IncomeElasticity_trn_gSSP3") %>%
      add_precursors("common/GCAM_region_names", "energy/A52.demand",
                     "socioeconomics/A52.inc_elas","L101.Pop_thous_GCAM3_R_Y") ->
      L252.IncomeElasticity_trn_gSSP3
    L252.IncomeElasticity_trn_SSP[['gSSP4']] %>%
      add_title("Transportation Income Elasticity: gSSP4") %>%
      add_units("Unitless (% change in service demand / % change in income)") %>%
      add_comments("Uses previously calculated per-capita GDP assumptions for all SSP scenarios") %>%
      add_comments("Transportation income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
      add_legacy_name("L252.IncomeElasticity_trn_gSSP4") %>%
      add_precursors("common/GCAM_region_names", "energy/A52.demand",
                     "socioeconomics/A52.inc_elas","L101.Pop_thous_GCAM3_R_Y") ->
      L252.IncomeElasticity_trn_gSSP4
    L252.IncomeElasticity_trn_SSP[['gSSP5']] %>%
      add_title("Transportation Income Elasticity: gSSP5") %>%
      add_units("Unitless (% change in service demand / % change in income)") %>%
      add_comments("Uses previously calculated per-capita GDP assumptions for all SSP scenarios") %>%
      add_comments("Transportation income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
      add_legacy_name("L252.IncomeElasticity_trn_gSSP5") %>%
      add_precursors("common/GCAM_region_names", "energy/A52.demand",
                     "socioeconomics/A52.inc_elas","L101.Pop_thous_GCAM3_R_Y") ->
      L252.IncomeElasticity_trn_gSSP5
    L252.IncomeElasticity_trn_SSP[['SSP1']] %>%
      add_title("Transportation Income Elasticity: SSP1") %>%
      add_units("Unitless (% change in service demand / % change in income)") %>%
      add_comments("Uses previously calculated per-capita GDP assumptions for all SSP scenarios") %>%
      add_comments("Transportation income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
      add_legacy_name("L252.IncomeElasticity_trn_SSP1") %>%
      add_precursors("common/GCAM_region_names", "energy/A52.demand",
                     "socioeconomics/A52.inc_elas","L101.Pop_thous_GCAM3_R_Y") ->
      L252.IncomeElasticity_trn_SSP1
    L252.IncomeElasticity_trn_SSP[['SSP2']] %>%
      add_title("Transportation Income Elasticity: SSP2") %>%
      add_units("Unitless (% change in service demand / % change in income)") %>%
      add_comments("Uses previously calculated per-capita GDP assumptions for all SSP scenarios") %>%
      add_comments("Transportation income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
      add_legacy_name("L252.IncomeElasticity_trn_SSP2") %>%
      add_precursors("common/GCAM_region_names", "energy/A52.demand",
                     "socioeconomics/A52.inc_elas","L101.Pop_thous_GCAM3_R_Y") ->
      L252.IncomeElasticity_trn_SSP2
    L252.IncomeElasticity_trn_SSP[['SSP3']] %>%
      add_title("Transportation Income Elasticity: SSP3") %>%
      add_units("Unitless (% change in service demand / % change in income)") %>%
      add_comments("Uses previously calculated per-capita GDP assumptions for all SSP scenarios") %>%
      add_comments("Transportation income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
      add_legacy_name("L252.IncomeElasticity_trn_SSP3") %>%
      add_precursors("common/GCAM_region_names", "energy/A52.demand",
                     "socioeconomics/A52.inc_elas","L101.Pop_thous_GCAM3_R_Y") ->
      L252.IncomeElasticity_trn_SSP3
    L252.IncomeElasticity_trn_SSP[['SSP4']] %>%
      add_title("Transportation Income Elasticity: SSP4") %>%
      add_units("Unitless (% change in service demand / % change in income)") %>%
      add_comments("Uses previously calculated per-capita GDP assumptions for all SSP scenarios") %>%
      add_comments("Transportation income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
      add_legacy_name("L252.IncomeElasticity_trn_SSP4") %>%
      add_precursors("common/GCAM_region_names", "energy/A52.demand",
                     "socioeconomics/A52.inc_elas","L101.Pop_thous_GCAM3_R_Y") ->
      L252.IncomeElasticity_trn_SSP4
    L252.IncomeElasticity_trn_SSP[['SSP5']] %>%
      add_title("Transportation Income Elasticity: SSP5") %>%
      add_units("Unitless (% change in service demand / % change in income)") %>%
      add_comments("Uses previously calculated per-capita GDP assumptions for all SSP scenarios") %>%
      add_comments("Transportation income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
      add_legacy_name("L252.IncomeElasticity_trn_SSP5") %>%
      add_precursors("common/GCAM_region_names", "energy/A52.demand",
                     "socioeconomics/A52.inc_elas","L101.Pop_thous_GCAM3_R_Y") ->
      L252.IncomeElasticity_trn_SSP5

    return_data(L252.IncomeElasticity_trn_GCAM3,
                L252.IncomeElasticity_trn_gSSP1,
                L252.IncomeElasticity_trn_gSSP2,
                L252.IncomeElasticity_trn_gSSP3,
                L252.IncomeElasticity_trn_gSSP4,
                L252.IncomeElasticity_trn_gSSP5,
                L252.IncomeElasticity_trn_SSP1,
                L252.IncomeElasticity_trn_SSP2,
                L252.IncomeElasticity_trn_SSP3,
                L252.IncomeElasticity_trn_SSP4,
                L252.IncomeElasticity_trn_SSP5)
  } else {
    stop("Unknown command")
  }
}
