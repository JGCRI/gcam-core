# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L2326.aluminum_Inc_Elas_scenarios
#'
#' Calculates aluminum income elasticity for each GCAM region by linear interpolation of assumption data
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{object}. The corresponding file in the
#' original data system was \code{L2326.aluminum_Inc_Elas_scenarios.R} (socioeconomics level2).
#' @details Takes per-capita GDP from ssp scenarios in each region.
#' Then calculates aluminum income elasticity for each region by linear interpolation of assumption data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter left_join mutate select transmute
#' @importFrom tidyr gather spread
#' @importFrom stats approx
#' @author Yang Liu  Dec 2019
module_socio_L2326.aluminum_Inc_Elas_scenarios <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "socioeconomics/A326.inc_elas",
             FILE = "socioeconomics/A326.inc_elas_parameter",
             "L102.pcgdp_thous90USD_Scen_R_Y",
             "L101.Pop_thous_SSP_R_Yfut",
             "L101.Pop_thous_R_Yh",
             "L1326.out_Mt_R_aluminum_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2326.aluminum_incelas_ssp1",
             "L2326.aluminum_incelas_ssp2",
             "L2326.aluminum_incelas_ssp3",
             "L2326.aluminum_incelas_ssp4",
             "L2326.aluminum_incelas_ssp5"))
  } else if(command == driver.MAKE) {

    GCAM_region_ID <- value <- year <- pcgdp_90thousUSD <- scenario <-
        region <- energy.final.demand <- income.elasticity <- . <-
      value.x <- value.y <- sector <- pcgdp_90thousUSD_Yfut <- population <-
      a <- b <- m <- per_capita_aluminum <- aluminum_pro <- pcgdp_90thousUSD_before <-
      aluminum_pro_before <- population_before <- aluminum_hist <- inc_elas <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    INCOME_ELASTICITY_INPUTS <- c(paste0("SSP", 1:5))

    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A326.inc_elas <- get_data(all_data, "socioeconomics/A326.inc_elas", strip_attributes = TRUE)
    A326.inc_elas_parameter  <- get_data(all_data, "socioeconomics/A326.inc_elas_parameter", strip_attributes = TRUE)
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y", strip_attributes = TRUE) %>%
      ungroup() %>%
      rename(pcgdp_90thousUSD = value) %>%
      mutate(year = as.integer(year)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")

    L101.Pop_thous_R_Yh <- get_data(all_data, "L101.Pop_thous_R_Yh", strip_attributes = TRUE) %>%
      rename(population = value) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")

    L101.Pop_thous_SSP_R_Yfut <- get_data(all_data, "L101.Pop_thous_SSP_R_Yfut", strip_attributes = TRUE) %>%
      ungroup() %>%
      rename(population = value) %>%
      mutate(year = as.integer(year)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")

    aluminum_pro_Yfut <- get_data(all_data, "L1326.out_Mt_R_aluminum_Yh") %>%
      filter(sector == "Aluminum") %>%
      group_by(GCAM_region_ID,year) %>%
      summarise(aluminum_hist = sum(value))%>%
      ungroup() %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      mutate(year = min(MODEL_FUTURE_YEARS))

    pcgdp_Yfut <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      mutate(pcgdp_90thousUSD_Yfut = pcgdp_90thousUSD, year = min(MODEL_FUTURE_YEARS)) %>%
      select(scenario, GCAM_region_ID, year, pcgdp_90thousUSD_Yfut)

    # unclear why base year is being filtered from FUTURE population, it returns
    # empty tibble. Just filtering by the first future year. Can't use
    # L101.Pop_thous_R_Yh because it doesn't have scenarios
    population_Yfut <- L101.Pop_thous_SSP_R_Yfut %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      mutate(population_Yfut = population) %>%
      select(scenario, GCAM_region_ID, year, population_Yfut)


    # ===================================================
    # Create one population dataset to pass timeshift tests
    # This is required because L101.Pop_thous_SSP_R_Yfut uses FUTURE_YEARS,
    # but here we want to create a dataset from MODEL_FUTURE_YEARS, which may start before FUTURE_YEARS
    L101_Pop_hist_and_fut <- L101.Pop_thous_R_Yh %>%
      repeat_add_columns(distinct(L101.Pop_thous_SSP_R_Yfut, scenario)) %>%
      bind_rows(L101.Pop_thous_SSP_R_Yfut)

    #First calculate the per capita aluminum consumption
    L2326.pcgdp_thous90USD_Scen_R_Y  <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(year %in%  MODEL_FUTURE_YEARS) %>%
      left_join_error_no_match(L101_Pop_hist_and_fut, by = c("scenario", "GCAM_region_ID", "year", "region")) %>%
      left_join_error_no_match(A326.inc_elas_parameter, by = c( "region")) %>%
      # TODO-BYU: (year - MODEL_FINAL_BASE_YEAR) needs to be confirmed by Pralit as he is working on updating this function
      mutate(per_capita_aluminum = a * exp(b/(pcgdp_90thousUSD * 1000 * gdp_deflator(2005, 1990))) * (1-m) ^ (year - MODEL_FINAL_BASE_YEAR),
             aluminum_pro = per_capita_aluminum * population * 0.000001)


    #Rebuild a new tibble save the previous year value
    L2326.pcgdp_thous90USD_Scen_R_Y_5_before <- L2326.pcgdp_thous90USD_Scen_R_Y %>%
      # essentially year + 5, but gets the next year dynamically without assuming + 5 year increments
      mutate(year = sapply(year, function(y) MODEL_FUTURE_YEARS[which(MODEL_FUTURE_YEARS > y)[1]]),
             pcgdp_90thousUSD_before = pcgdp_90thousUSD,
             aluminum_pro_before = aluminum_pro,
             population_before = aluminum_pro) %>%
      select(scenario, GCAM_region_ID, region, year, pcgdp_90thousUSD_before, aluminum_pro_before, population_before)

    L2326.pcgdp_thous90USD_Scen_R_Y <- L2326.pcgdp_thous90USD_Scen_R_Y %>%
      left_join(L2326.pcgdp_thous90USD_Scen_R_Y_5_before, by = c("scenario", "GCAM_region_ID", "year","region"))%>%
      # Add base year data (that we copied forward to first model future year)
      left_join(aluminum_pro_Yfut, by = c("GCAM_region_ID", "year")) %>%
      left_join(pcgdp_Yfut, by = c("scenario", "GCAM_region_ID", "year")) %>%
      left_join(population_Yfut, by = c("scenario", "GCAM_region_ID", "year")) %>%
      mutate(pcgdp_90thousUSD_before = replace_na(pcgdp_90thousUSD_before,0),aluminum_pro_before  = replace_na(aluminum_pro_before ,0),population_before  = replace_na(population ,0),
             aluminum_hist  = replace_na(aluminum_hist,0),pcgdp_90thousUSD_Yfut = replace_na(pcgdp_90thousUSD_Yfut,0),population_Yfut = replace_na(population_Yfut,0),
             pcgdp_90thousUSD_before = pcgdp_90thousUSD_before + pcgdp_90thousUSD_Yfut, aluminum_pro_before = aluminum_pro_before + aluminum_hist, population_before = population_before + population_Yfut,
             #cal
             inc_elas = log((aluminum_pro/population) / (aluminum_pro_before/population_before)) /log(pcgdp_90thousUSD/pcgdp_90thousUSD_before),
             income.elasticity = inc_elas,energy.final.demand = "aluminum") %>%
      select(scenario, region, energy.final.demand, year, income.elasticity) %>%
      arrange(year) %>%
      #replace those huge number
      mutate(income.elasticity = replace(income.elasticity,income.elasticity > 3 , 3),
             income.elasticity = replace(income.elasticity,income.elasticity < -3,-3))

    # Split by scenario and remove scenario column from each tibble
    L2326.pcgdp_thous90USD_Scen_R_Y <- L2326.pcgdp_thous90USD_Scen_R_Y %>%
      split(.$scenario) %>%
      lapply(function(df) {select(df, -scenario) %>%
          add_units("Unitless (% change in service demand / % change in income)") %>%
          add_comments("Uses previously calculated per-capita GDP assumptions for all ssp scenarios") %>%
          add_comments("aluminum income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
          add_precursors("common/GCAM_region_names", "socioeconomics/A326.inc_elas","socioeconomics/A326.inc_elas_parameter",
                         "L102.pcgdp_thous90USD_Scen_R_Y") })


    # ===================================================

    # Produce outputs
    MODULE_OUTPUTS <- list()

    for(iei in INCOME_ELASTICITY_INPUTS) {
      output_name <- paste0("L2326.aluminum_incelas_", tolower(iei))

      L2326.pcgdp_thous90USD_Scen_R_Y[[iei]] %>%
        add_title(paste("aluminum Income Elasticity:", iei)) %>%
        add_legacy_name(output_name) %>%
        add_precursors("socioeconomics/A326.inc_elas_parameter",
                       "L102.pcgdp_thous90USD_Scen_R_Y","L101.Pop_thous_SSP_R_Yfut",
                       "L101.Pop_thous_R_Yh") -> MODULE_OUTPUTS[[output_name]]

      assign(output_name, MODULE_OUTPUTS[[output_name]])
    }

    return_data(L2326.aluminum_incelas_ssp1,
                L2326.aluminum_incelas_ssp2,
                L2326.aluminum_incelas_ssp3,
                L2326.aluminum_incelas_ssp4,
                L2326.aluminum_incelas_ssp5)

  } else {
    stop("Unknown command")
  }
}
