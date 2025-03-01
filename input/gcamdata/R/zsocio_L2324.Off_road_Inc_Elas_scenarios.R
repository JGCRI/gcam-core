# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L2324.Off_road_Inc_Elas_scenarios
#'
#' Calculates Off_road income elasticity for each GCAM region by linear interpolation of assumption data
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{object}. The corresponding file in the
#' original data system was \code{L2324.Off_road_Inc_Elas_scenarios.R} (socioeconomics level2).
#' @details Takes per-capita GDP from ssp scenarios in each region.
#' Then calculates Off_road income elasticity for each region by linear interpolation of assumption data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter left_join mutate select transmute
#' @importFrom tidyr gather spread
#' @importFrom stats approx
#' @author RH April 2017
module_socio_L2324.Off_road_Inc_Elas_scenarios <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "socioeconomics/A324.inc_elas",
             "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2324.Off_road_incelas_ssp1",
             "L2324.Off_road_incelas_ssp2",
             "L2324.Off_road_incelas_ssp3",
             "L2324.Off_road_incelas_ssp4",
             "L2324.Off_road_incelas_ssp5"))
  } else if(command == driver.MAKE) {

    GCAM_region_ID <- value <- year <- pcgdp_90thousUSD <- scenario <-
      region <- energy.final.demand <- income.elasticity <- . <-
      value.x <- value.y <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    INCOME_ELASTICITY_INPUTS <- c(paste0("SSP", 1:5))

    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A324.inc_elas <- get_data(all_data, "socioeconomics/A324.inc_elas", strip_attributes = TRUE)
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y", strip_attributes = TRUE) %>%
      ungroup() %>%
      rename(pcgdp_90thousUSD = value) %>%
      mutate(year = as.integer(year))


    # ===================================================
    # Linearly interpolate income elasticity for each level of per-capita GDP,
    # using the assumption data
    L102.pcgdp_thous90USD_Scen_R_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      # Using approx rather than approx_fun because data is from assumption file, not in our tibble
      mutate(income.elasticity = approx(x = filter(A324.inc_elas,A324.inc_elas$sector == 'agricultural energy use')$pcgdp_90thousUSD, y = filter(A324.inc_elas,A324.inc_elas$sector == 'agricultural energy use')$inc_elas,
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
      mutate(income.elasticity = approx(x = filter(A324.inc_elas,A324.inc_elas$sector == 'construction')$pcgdp_90thousUSD, y = filter(A324.inc_elas,A324.inc_elas$sector == 'construction')$inc_elas,
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
      mutate(income.elasticity = approx(x = filter(A324.inc_elas,A324.inc_elas$sector == 'mining energy use')$pcgdp_90thousUSD, y = filter(A324.inc_elas,A324.inc_elas$sector == 'mining energy use')$inc_elas,
                                        xout = pcgdp_90thousUSD,
                                        # Rule 2 means that data outside of the interval of input
                                        # data will be assigned the cloest data extreme
                                        rule = 2)[['y']] %>% round(3),
             energy.final.demand = "mining energy use") %>%
      select(scenario, region, energy.final.demand, year, income.elasticity) %>%
      arrange(year) ->
      mining

    L2324.pcgdp_thous90USD_Scen_R_Y <- bind_rows(agriculture,construction,mining)

    # Split by scenario and remove scenario column from each tibble

    L2324.pcgdp_thous90USD_Scen_R_Y <- L2324.pcgdp_thous90USD_Scen_R_Y %>%
      split(.$scenario) %>%
      lapply(function(df) {select(df, -scenario) %>%
          add_units("Unitless (% change in service demand / % change in income)") %>%
          add_comments("Uses previously calculated per-capita GDP assumptions for all ssp scenarios") %>%
          add_comments("Off_road income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
          add_precursors("common/GCAM_region_names", "socioeconomics/A324.inc_elas",
                         "L102.pcgdp_thous90USD_Scen_R_Y") })


    # ===================================================

    # Produce outputs
    MODULE_OUTPUTS <- list()

    for(iei in INCOME_ELASTICITY_INPUTS) {
      output_name <- paste0("L2324.Off_road_incelas_", tolower(iei))

      L2324.pcgdp_thous90USD_Scen_R_Y[[iei]] %>%
        add_title(paste("Off_road Income Elasticity:", iei)) %>%
        add_legacy_name(output_name) %>%
        same_precursors_as(L2324.pcgdp_thous90USD_Scen_R_Y) -> MODULE_OUTPUTS[[output_name]]

      assign(output_name, MODULE_OUTPUTS[[output_name]])
    }

    return_data(L2324.Off_road_incelas_ssp1,
                L2324.Off_road_incelas_ssp2,
                L2324.Off_road_incelas_ssp3,
                L2324.Off_road_incelas_ssp4,
                L2324.Off_road_incelas_ssp5)

  } else {
    stop("Unknown command")
  }
}
