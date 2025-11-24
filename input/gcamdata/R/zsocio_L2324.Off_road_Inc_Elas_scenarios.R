# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L2324.Off_road_Inc_Elas_scenarios
#'
#' Calculates Off_road income elasticity for each GCAM region by linear interpolation of assumption data
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2324.Off_road_incelas_ssp[1-5]}.
#' @details Takes per-capita GDP from ssp scenarios in each region.
#' Then calculates Off_road income elasticity for each region by linear interpolation of assumption data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter left_join mutate select transmute
#' @importFrom tidyr gather spread
#' @importFrom stats approx
#' @author RH April 2017
module_socio_L2324.Off_road_Inc_Elas_scenarios <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "socioeconomics/A324.inc_elas",
      "L102.pcgdp_thous90USD_Scen_R_Y")

  MODULE_OUTPUTS <-
    c("L2324.IncomeElasticity_Off_road_Scen")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    GCAM_region_ID <- value <- year <- pcgdp_90thousUSD <- scenario <-
      region <- energy.final.demand <- income.elasticity <- . <-
      value.x <- value.y <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    L102.pcgdp_thous90USD_Scen_R_Y <-
      L102.pcgdp_thous90USD_Scen_R_Y %>%
      rename(pcgdp_90thousUSD = value) %>%
      mutate(year = as.integer(year))

    # Linearly interpolate income elasticity for each level of per-capita GDP,
    # using the assumption data
    L102.pcgdp_thous90USD_Scen_R_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      # Using approx rather than approx_fun because data is from assumption file, not in our tibble
      mutate(income.elasticity = approx(x = filter(A324.inc_elas,A324.inc_elas$sector == 'agricultural energy use')$pcgdp_90thousUSD,
                                        y = filter(A324.inc_elas,A324.inc_elas$sector == 'agricultural energy use')$inc_elas,
                                        xout = pcgdp_90thousUSD,
                                        # Rule 2 means that data outside of the interval of input
                                        # data will be assigned the closest data extreme
                                        rule = 2)[['y']] %>% round(3),
             energy.final.demand = "agricultural energy use") %>%
      select(scenario, region, energy.final.demand, year, income.elasticity) %>%
      arrange(year) ->
      agriculture

    L102.pcgdp_thous90USD_Scen_R_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      # Using approx rather than approx_fun because data is from assumption file, not in our tibble
      mutate(income.elasticity = approx(x = filter(A324.inc_elas,A324.inc_elas$sector == 'construction')$pcgdp_90thousUSD,
                                        y = filter(A324.inc_elas,A324.inc_elas$sector == 'construction')$inc_elas,
                                        xout = pcgdp_90thousUSD,
                                        # Rule 2 means that data outside of the interval of input
                                        # data will be assigned the cloest data extreme
                                        rule = 2)[['y']] %>% round(3),
             energy.final.demand = "construction") %>%
      select(scenario, region, energy.final.demand, year, income.elasticity) %>%
      arrange(year) ->
      construction

    L102.pcgdp_thous90USD_Scen_R_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      # Using approx rather than approx_fun because data is from assumption file, not in our tibble
      mutate(income.elasticity = approx(x = filter(A324.inc_elas,A324.inc_elas$sector == 'mining energy use')$pcgdp_90thousUSD,
                                        y = filter(A324.inc_elas,A324.inc_elas$sector == 'mining energy use')$inc_elas,
                                        xout = pcgdp_90thousUSD,
                                        # Rule 2 means that data outside of the interval of input
                                        # data will be assigned the cloest data extreme
                                        rule = 2)[['y']] %>% round(3),
             energy.final.demand = "mining energy use") %>%
      select(scenario, region, energy.final.demand, year, income.elasticity) %>%
      arrange(year) ->
      mining

    L2324.IncomeElasticity_Off_road_Scen <-
      bind_rows(agriculture,construction,mining)

    L2324.IncomeElasticity_Off_road_Scen %>%
      add_title(paste("Off_road Income Elasticity: SSPs")) %>%
      add_legacy_name("L2324.IncomeElasticity_Off_road_Scen") %>%
      add_units("Unitless (% change in service demand / % change in income)") %>%
      add_comments("Uses previously calculated per-capita GDP assumptions for all ssp scenarios") %>%
      add_comments("Off_road income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
      add_precursors("common/GCAM_region_names",
                     "socioeconomics/A324.inc_elas",
                     "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L2324.IncomeElasticity_Off_road_Scen


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
