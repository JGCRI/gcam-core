# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L2323.iron_steel_Inc_Elas_scenarios
#'
#' Calculates iron and steel income elasticity for each GCAM region by linear interpolation of assumption data
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{object}. The corresponding file in the
#' original data system was \code{L2323.iron_steel_Inc_Elas_scenarios.R} (socioeconomics level2).
#' @details Takes per-capita GDP from ssp scenarios in each region.
#' Then calculates iron_steel income elasticity for each region by linear interpolation of assumption data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter left_join mutate select transmute
#' @importFrom tidyr gather spread
#' @importFrom stats approx
#' @author RH April 2017
module_socio_L2323.iron_steel_Inc_Elas_scenarios <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "socioeconomics/A323.inc_elas",
             FILE = "socioeconomics/A323.inc_elas_parameter",
             "L102.pcgdp_thous90USD_Scen_R_Y",
             "L101.Pop_thous_SSP_R_Yfut",
             "L101.Pop_thous_R_Yh",
             "LB1092.Tradebalance_iron_steel_Mt_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2323.iron_steel_incelas_ssp1",
             "L2323.iron_steel_incelas_ssp2",
             "L2323.iron_steel_incelas_ssp3",
             "L2323.iron_steel_incelas_ssp4",
             "L2323.iron_steel_incelas_ssp5"))
  } else if(command == driver.MAKE) {

    GCAM_region_ID <- value <- year <- pcgdp_90thousUSD <- scenario <-
      region <- energy.final.demand <- income.elasticity <- . <-
      value.x <- value.y <- pcgdp_90thousUSD_2015 <- a <- b <- m <-
      per_capita_steel <- population <- steel_cons <- pcgdp_90thousUSD_before <-
      steel_cons_before <- steel_hist <- inc_elas <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    INCOME_ELASTICITY_INPUTS <- c(paste0("SSP", 1:5))

    LB1092.Tradebalance_iron_steel_Mt_R_Y <- get_data(all_data, "LB1092.Tradebalance_iron_steel_Mt_R_Y")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A323.inc_elas <- get_data(all_data, "socioeconomics/A323.inc_elas", strip_attributes = TRUE)
    A323.inc_elas_parameter  <- get_data(all_data, "socioeconomics/A323.inc_elas_parameter", strip_attributes = TRUE)
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

    steel_cons_Yfut <- LB1092.Tradebalance_iron_steel_Mt_R_Y %>%
      filter(metric=="consumption_reval")%>%
      group_by(GCAM_region,year) %>%
      summarise(steel_hist = sum(value))%>%
      rename(region=GCAM_region)%>%
      left_join(GCAM_region_names,by=c("region"))%>%
      ungroup() %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      mutate(year = min(MODEL_FUTURE_YEARS)) %>%
      select(GCAM_region_ID,year,steel_hist)

    pcgdp_Yfut <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      mutate(pcgdp_90thousUSD_Yfut = pcgdp_90thousUSD, year = min(MODEL_FUTURE_YEARS)) %>%
      select(scenario, GCAM_region_ID, year,pcgdp_90thousUSD_Yfut)



    # ===================================================
    # Create one population dataset to pass timeshift tests
    # This is required because L101.Pop_thous_SSP_R_Yfut uses FUTURE_YEARS,
    # but here we want to create a dataset from MODEL_FUTURE_YEARS, which may start before FUTURE_YEARS
    L101_Pop_hist_and_fut <- L101.Pop_thous_R_Yh %>%
      repeat_add_columns(distinct(L101.Pop_thous_SSP_R_Yfut, scenario)) %>%
      bind_rows(L101.Pop_thous_SSP_R_Yfut)

    #First calculate the per capita steel consumption
    L2323.pcgdp_thous90USD_Scen_R_Y  <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(year %in%  MODEL_FUTURE_YEARS) %>%
      left_join_error_no_match(L101_Pop_hist_and_fut, by = c("scenario", "GCAM_region_ID", "year", "region")) %>%
      left_join_error_no_match(A323.inc_elas_parameter, by = c( "region")) %>%
      mutate(per_capita_steel = a * exp(b/(pcgdp_90thousUSD * 1000)) * (1-m) ^ (year - 1990),
             steel_cons = per_capita_steel * population * 0.000001)


    #Rebuild a new tibble save the previous year value
    L2323.pcgdp_thous90USD_Scen_R_Y_5_before <- L2323.pcgdp_thous90USD_Scen_R_Y %>%
      # essentially year + 5, but gets the next year dynamically without assuming + 5 year increments
      mutate(year = sapply(year, function(y) MODEL_FUTURE_YEARS[which(MODEL_FUTURE_YEARS > y)[1]]),
             pcgdp_90thousUSD_before = pcgdp_90thousUSD,
             steel_cons_before = steel_cons) %>%
      select(scenario,GCAM_region_ID,region, year, pcgdp_90thousUSD_before, steel_cons_before)

    L2323.pcgdp_thous90USD_Scen_R_Y <- L2323.pcgdp_thous90USD_Scen_R_Y %>%
      left_join(L2323.pcgdp_thous90USD_Scen_R_Y_5_before, by = c("scenario", "GCAM_region_ID", "year","region"))%>%
      #Add 2015 data
      left_join(steel_cons_Yfut, by = c("GCAM_region_ID", "year")) %>%
      left_join(pcgdp_Yfut, by = c("scenario", "GCAM_region_ID", "year")) %>%
      mutate(pcgdp_90thousUSD_before = replace_na(pcgdp_90thousUSD_before,0),
             steel_cons_before  = replace_na(steel_cons_before ,0),
             steel_hist  = replace_na(steel_hist,0),
             pcgdp_90thousUSD_Yfut = replace_na(pcgdp_90thousUSD_Yfut,0),
             pcgdp_90thousUSD_before = pcgdp_90thousUSD_before + pcgdp_90thousUSD_Yfut,
             steel_cons_before = steel_cons_before + steel_hist,
             #cal
             inc_elas = log(steel_cons / steel_cons_before)/log(pcgdp_90thousUSD/pcgdp_90thousUSD_before),
             income.elasticity = inc_elas,energy.final.demand = "regional iron and steel") %>%
      select(scenario, region, energy.final.demand, year, income.elasticity) %>%
      arrange(year) %>%
      #replace those huge number
      mutate(income.elasticity = replace(income.elasticity,income.elasticity > 10 , 10),
             income.elasticity = replace(income.elasticity,income.elasticity < -10,-10))


    # Split by scenario and remove scenario column from each tibble
    L2323.pcgdp_thous90USD_Scen_R_Y <- L2323.pcgdp_thous90USD_Scen_R_Y %>%
      split(.$scenario) %>%
      lapply(function(df) {select(df, -scenario) %>%
          add_units("Unitless (% change in service demand / % change in income)") %>%
          add_comments("Uses previously calculated per-capita GDP assumptions for all ssp scenarios") %>%
          add_comments("iron_steel income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
          add_precursors("common/GCAM_region_names", "socioeconomics/A323.inc_elas","socioeconomics/A323.inc_elas_parameter",
                         "L102.pcgdp_thous90USD_Scen_R_Y") })


    # ===================================================

    # Produce outputs
    MODULE_OUTPUTS <- list()

    for(iei in INCOME_ELASTICITY_INPUTS) {
      output_name <- paste0("L2323.iron_steel_incelas_", tolower(iei))

      L2323.pcgdp_thous90USD_Scen_R_Y[[iei]] %>%
        add_title(paste("iron_steel Income Elasticity:", iei)) %>%
        add_legacy_name(output_name) %>%
        add_precursors("socioeconomics/A323.inc_elas_parameter",
                       "L102.pcgdp_thous90USD_Scen_R_Y","L101.Pop_thous_SSP_R_Yfut",
                       "L101.Pop_thous_R_Yh") -> MODULE_OUTPUTS[[output_name]]

      assign(output_name, MODULE_OUTPUTS[[output_name]])
    }

    return_data(L2323.iron_steel_incelas_ssp1,
                L2323.iron_steel_incelas_ssp2,
                L2323.iron_steel_incelas_ssp3,
                L2323.iron_steel_incelas_ssp4,
                L2323.iron_steel_incelas_ssp5)

  } else {
    stop("Unknown command")
  }
}
