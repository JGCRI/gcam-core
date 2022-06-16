# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socioeconomics_L2326.aluminum_Inc_Elas_scenarios
#'
#' Calculates aluminum income elasticity for each GCAM region by linear interpolation of assumption data
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2326.aluminum_incelas_gcam3}, \code{object}. The corresponding file in the
#' original data system was \code{L2326.aluminum_Inc_Elas_scenarios.R} (socioeconomics level2).
#' @details Takes per-capita GDP from ssp scenarios in each region.
#' Then calculates aluminum income elasticity for each region by linear interpolation of assumption data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter left_join mutate select transmute
#' @importFrom tidyr gather spread
#' @importFrom stats approx
#' @author Yang Liu  Dec 2019
module_socioeconomics_L2326.aluminum_Inc_Elas_scenarios <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "socioeconomics/A326.inc_elas",
             FILE = "socioeconomics/A326.inc_elas_parameter",
             "L102.pcgdp_thous90USD_Scen_R_Y",
             "L102.pcgdp_thous90USD_GCAM3_R_Y",
             "L101.Pop_thous_Scen_R_Yfut",
             "L101.Pop_thous_GCAM3_R_Y",
             "L102.gdp_mil90usd_GCAM3_R_Y",
             "L1326.out_Mt_R_aluminum_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2326.aluminum_incelas_gssp1",
             "L2326.aluminum_incelas_gssp2",
             "L2326.aluminum_incelas_gssp3",
             "L2326.aluminum_incelas_gssp4",
             "L2326.aluminum_incelas_gssp5",
             "L2326.aluminum_incelas_ssp1",
             "L2326.aluminum_incelas_ssp2",
             "L2326.aluminum_incelas_ssp3",
             "L2326.aluminum_incelas_ssp4",
             "L2326.aluminum_incelas_ssp5",
             "L2326.aluminum_incelas_gcam3"))
  } else if(command == driver.MAKE) {

    GCAM_region_ID <- value <- year <- pcgdp_90thousUSD <- scenario <-
        region <- energy.final.demand <- income.elasticity <- . <-
      value.x <- value.y <- sector <- pcgdp_90thousUSD_2015 <- population <-
      a <- b <- m <- per_capita_aluminum <- aluminum_pro <- pcgdp_90thousUSD_before <-
      aluminum_pro_before <- population_before <- aluminum_hist <- inc_elas <- NULL # silence package check.

    all_data <- list(...)[[1]]
    # Load required inputs
    COV_1990USD_2005USD = 1.383

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A326.inc_elas <- get_data(all_data, "socioeconomics/A326.inc_elas", strip_attributes = TRUE)
    A326.inc_elas_parameter  <- get_data(all_data, "socioeconomics/A326.inc_elas_parameter", strip_attributes = TRUE)
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y", strip_attributes = TRUE) %>%
      ungroup() %>%
      rename(pcgdp_90thousUSD = value) %>%
      mutate(year = as.integer(year)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")

    L102.pcgdp_thous90USD_GCAM3_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_GCAM3_R_Y", strip_attributes = TRUE) %>%
      ungroup() %>%
      rename(pcgdp_90thousUSD = value) %>%
      mutate(year = as.integer(year)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")

    L101.Pop_thous_Scen_R_Yfut <- get_data(all_data, "L101.Pop_thous_Scen_R_Yfut", strip_attributes = TRUE) %>%
      ungroup() %>%
      rename(population = value) %>%
      mutate(year = as.integer(year)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")

    L101.Pop_thous_GCAM3_R_Y <- get_data(all_data, "L101.Pop_thous_GCAM3_R_Y", strip_attributes = TRUE) %>%
      ungroup() %>%
      rename(population = value) %>%
      mutate(year = as.integer(year)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")

    L102.gdp_mil90usd_GCAM3_R_Y <- get_data(all_data, "L102.gdp_mil90usd_GCAM3_R_Y", strip_attributes = TRUE) %>%
      ungroup() %>%
      rename(pcgdp_90thousUSD = value) %>%
      mutate(year = as.integer(year)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")

    aluminum_pro_2015 <- get_data(all_data, "L1326.out_Mt_R_aluminum_Yh") %>%
      filter(sector == "Aluminum") %>%
      group_by(GCAM_region_ID,year) %>%
      summarise(aluminum_hist = sum(value))%>%
      ungroup() %>%
      filter(year == 2015) %>%
      mutate(year = 2020)

    pcgdp_2015 <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(year == 2015) %>%
      mutate(pcgdp_90thousUSD_2015 = pcgdp_90thousUSD,year = 2020 ) %>%
      select(scenario, GCAM_region_ID, year,pcgdp_90thousUSD_2015)

    pcgdp_2015_GCAM3 <- L102.pcgdp_thous90USD_GCAM3_R_Y %>%
      filter(year == 2015) %>%
      mutate(pcgdp_90thousUSD_2015 = pcgdp_90thousUSD,year = 2020 ) %>%
      select( GCAM_region_ID, year,pcgdp_90thousUSD_2015)

    population_2015 <- L101.Pop_thous_Scen_R_Yfut %>%
      filter(year == 2015) %>%
      mutate(population_2015 = population ,year = 2020 ) %>%
      select(scenario, GCAM_region_ID, year,population_2015 )

    population_2015_GCAM3 <- L101.Pop_thous_GCAM3_R_Y %>%
      filter(year == 2015) %>%
      mutate(population_2015 = population ,year = 2020 ) %>%
      select( GCAM_region_ID, year,population_2015 )

    # ===================================================
    #First calculate the per capita aluminum consumption
    L2326.pcgdp_thous90USD_Scen_R_Y  <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(year %in%  MODEL_FUTURE_YEARS) %>%
      left_join_error_no_match(L101.Pop_thous_Scen_R_Yfut, by = c("scenario", "GCAM_region_ID", "year", "region")) %>%
      left_join_error_no_match(A326.inc_elas_parameter, by = c( "region")) %>%
      mutate(per_capita_aluminum = a * exp(b/(pcgdp_90thousUSD * 1000 * COV_1990USD_2005USD)) * (1-m) ^ (year- 2015),
             aluminum_pro = per_capita_aluminum * population*0.000001)


    #Rebuild a new tibble save the previous year value
    L2326.pcgdp_thous90USD_Scen_R_Y_5_before <- L2326.pcgdp_thous90USD_Scen_R_Y %>%
      mutate(year= year+5,pcgdp_90thousUSD_before = pcgdp_90thousUSD ,aluminum_pro_before = aluminum_pro,population_before = aluminum_pro) %>%
      select(scenario,GCAM_region_ID,region, year,pcgdp_90thousUSD_before,aluminum_pro_before,population_before )

    L2326.pcgdp_thous90USD_Scen_R_Y <- L2326.pcgdp_thous90USD_Scen_R_Y %>%
      left_join(L2326.pcgdp_thous90USD_Scen_R_Y_5_before, by = c("scenario", "GCAM_region_ID", "year","region"))%>%
      #Add 2015 data
      left_join(aluminum_pro_2015, by = c("GCAM_region_ID", "year")) %>%
      left_join(pcgdp_2015, by = c("scenario", "GCAM_region_ID", "year")) %>%
      left_join(population_2015, by = c("scenario", "GCAM_region_ID", "year")) %>%
      mutate(pcgdp_90thousUSD_before = replace_na(pcgdp_90thousUSD_before,0),aluminum_pro_before  = replace_na(aluminum_pro_before ,0),population_before  = replace_na(population ,0),
             aluminum_hist  = replace_na(aluminum_hist,0),pcgdp_90thousUSD_2015 = replace_na(pcgdp_90thousUSD_2015,0),population_2015 = replace_na(population_2015,0),
             pcgdp_90thousUSD_before = pcgdp_90thousUSD_before + pcgdp_90thousUSD_2015, aluminum_pro_before = aluminum_pro_before + aluminum_hist, population_before = population_before + population_2015,
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

    ####Cal gcam3
    #First calculate the per capita aluminum consumption
    L2326.pcgdp_thous90USD_GCAM3_R_Y  <- L102.gdp_mil90usd_GCAM3_R_Y%>%
      filter(year %in%  MODEL_FUTURE_YEARS) %>%
      left_join_error_no_match(L101.Pop_thous_GCAM3_R_Y, by = c("GCAM_region_ID", "year", "region")) %>%
      left_join_error_no_match(A326.inc_elas_parameter, by = c( "region")) %>%
      mutate(per_capita_aluminum = a * exp(b/(pcgdp_90thousUSD * 1000 * COV_1990USD_2005USD)) * (1-m) ^ (year- 2015),
             aluminum_pro = per_capita_aluminum * population*0.000001)


    #Rebuild a new tibble save the previous year value
    L2326.pcgdp_thous90USD_GCAM3_R_Y_5_before <- L2326.pcgdp_thous90USD_GCAM3_R_Y %>%
      mutate(year= year+5,pcgdp_90thousUSD_before = pcgdp_90thousUSD ,aluminum_pro_before = aluminum_pro) %>%
      select(scenario,GCAM_region_ID,region, year,pcgdp_90thousUSD_before,aluminum_pro_before )

    L2326.aluminum_incelas_gcam3 <- L2326.pcgdp_thous90USD_GCAM3_R_Y %>%
      left_join(L2326.pcgdp_thous90USD_GCAM3_R_Y_5_before, by = c("GCAM_region_ID", "year","region"))%>%
      #Add 2015 data
      left_join(aluminum_pro_2015, by = c("GCAM_region_ID", "year")) %>%
      left_join(pcgdp_2015_GCAM3, by = c( "GCAM_region_ID", "year")) %>%
      left_join(population_2015_GCAM3, by = c( "GCAM_region_ID", "year")) %>%
      mutate(pcgdp_90thousUSD_before = replace_na(pcgdp_90thousUSD_before,0),aluminum_pro_before  = replace_na(aluminum_pro_before ,0),population_before  = replace_na(population ,0),
             aluminum_hist  = replace_na(aluminum_hist,0),pcgdp_90thousUSD_2015 = replace_na(pcgdp_90thousUSD_2015,0),population_2015 = replace_na(population_2015,0),
             pcgdp_90thousUSD_before = pcgdp_90thousUSD_before + pcgdp_90thousUSD_2015, aluminum_pro_before = aluminum_pro_before + aluminum_hist,
             #cal
             inc_elas = log((aluminum_pro/population) / (aluminum_pro_before/population_before)) /log(pcgdp_90thousUSD/pcgdp_90thousUSD_before),
             income.elasticity = inc_elas,energy.final.demand = "aluminum") %>%
      select(region, energy.final.demand, year, income.elasticity) %>%
      arrange(year) %>%
      #replace those huge number
      mutate(income.elasticity = replace(income.elasticity,income.elasticity > 3 , 3),
             income.elasticity = replace(income.elasticity,income.elasticity < -3,-3))

    # ===================================================

    # Produce outputs
    L2326.pcgdp_thous90USD_Scen_R_Y[["gSSP1"]] %>%
      add_title("aluminum Income Elasticity: gssp1") %>%
      add_legacy_name("L2326.aluminum_incelas_gssp1")%>%
      add_precursors("L102.pcgdp_thous90USD_Scen_R_Y","L101.Pop_thous_Scen_R_Yfut","socioeconomics/A326.inc_elas_parameter") ->
      L2326.aluminum_incelas_gssp1

    L2326.pcgdp_thous90USD_Scen_R_Y[["gSSP2"]] %>%
      add_title("aluminum Income Elasticity: gssp2") %>%
      add_legacy_name("L2326.aluminum_incelas_gssp2")%>%
      add_precursors("L102.pcgdp_thous90USD_Scen_R_Y","L101.Pop_thous_Scen_R_Yfut","socioeconomics/A326.inc_elas_parameter") ->
      L2326.aluminum_incelas_gssp2

    L2326.pcgdp_thous90USD_Scen_R_Y[["gSSP3"]] %>%
      add_title("aluminum Income Elasticity: gssp3") %>%
      add_legacy_name("L2326.aluminum_incelas_gssp3") ->
      L2326.aluminum_incelas_gssp3

    L2326.pcgdp_thous90USD_Scen_R_Y[["gSSP4"]] %>%
      add_title("aluminum Income Elasticity: gssp4") %>%
      add_legacy_name("L2326.aluminum_incelas_gssp4")%>%
      add_precursors("L102.pcgdp_thous90USD_Scen_R_Y","L101.Pop_thous_Scen_R_Yfut","socioeconomics/A326.inc_elas_parameter") ->
      L2326.aluminum_incelas_gssp4

    L2326.pcgdp_thous90USD_Scen_R_Y[["gSSP5"]] %>%
      add_title("aluminum Income Elasticity: gssp5") %>%
      add_legacy_name("L2326.aluminum_incelas_gssp5")%>%
      add_precursors("L102.pcgdp_thous90USD_Scen_R_Y","L101.Pop_thous_Scen_R_Yfut","socioeconomics/A326.inc_elas_parameter") ->
      L2326.aluminum_incelas_gssp5

    L2326.pcgdp_thous90USD_Scen_R_Y[["SSP1"]] %>%
      add_title("aluminum Income Elasticity: ssp1") %>%
      add_legacy_name("L2326.aluminum_incelas_ssp1")%>%
      add_precursors("L102.pcgdp_thous90USD_Scen_R_Y","L101.Pop_thous_Scen_R_Yfut","socioeconomics/A326.inc_elas_parameter") ->
      L2326.aluminum_incelas_ssp1

    L2326.pcgdp_thous90USD_Scen_R_Y[["SSP2"]] %>%
      add_title("aluminum Income Elasticity: ssp2") %>%
      add_legacy_name("L2326.aluminum_incelas_ssp2")%>%
      add_precursors("L102.pcgdp_thous90USD_Scen_R_Y","L101.Pop_thous_Scen_R_Yfut","socioeconomics/A326.inc_elas_parameter") ->
      L2326.aluminum_incelas_ssp2

    L2326.pcgdp_thous90USD_Scen_R_Y[["SSP3"]] %>%
      add_title("aluminum Income Elasticity: ssp3") %>%
      add_legacy_name("L2326.aluminum_incelas_ssp3")%>%
      add_precursors("L102.pcgdp_thous90USD_Scen_R_Y","L101.Pop_thous_Scen_R_Yfut","socioeconomics/A326.inc_elas_parameter") ->
      L2326.aluminum_incelas_ssp3

    L2326.pcgdp_thous90USD_Scen_R_Y[["SSP4"]] %>%
      add_title("aluminum Income Elasticity: ssp4") %>%
      add_legacy_name("L2326.aluminum_incelas_ssp4")%>%
      add_precursors("L102.pcgdp_thous90USD_Scen_R_Y","L101.Pop_thous_Scen_R_Yfut","socioeconomics/A326.inc_elas_parameter") ->
      L2326.aluminum_incelas_ssp4

    L2326.pcgdp_thous90USD_Scen_R_Y[["SSP5"]] %>%
      add_title("aluminum Income Elasticity: ssp5") %>%
      add_legacy_name("L2326.aluminum_incelas_ssp5")%>%
      add_precursors("L102.pcgdp_thous90USD_Scen_R_Y","L101.Pop_thous_Scen_R_Yfut","socioeconomics/A326.inc_elas_parameter") ->
      L2326.aluminum_incelas_ssp5

    L2326.aluminum_incelas_gcam3 %>%
      add_title("aluminum Income Elasticity: gcam3") %>%
      add_units("Unitless (% change in service demand / % change in income)") %>%
      add_comments("Uses previously calculated per-capita GDP assumptions of aluminum elastciity") %>%
      add_comments("aluminum income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
      add_legacy_name("L2326.aluminum_incelas_gcam3") %>%
      add_precursors("common/GCAM_region_names", "socioeconomics/A326.inc_elas",
                     "L101.Pop_thous_GCAM3_R_Y", "L102.gdp_mil90usd_GCAM3_R_Y",
                     "L1326.out_Mt_R_aluminum_Yh","L102.pcgdp_thous90USD_GCAM3_R_Y") ->
      L2326.aluminum_incelas_gcam3

    return_data(L2326.aluminum_incelas_gssp1,
                L2326.aluminum_incelas_gssp2,
                L2326.aluminum_incelas_gssp3,
                L2326.aluminum_incelas_gssp4,
                L2326.aluminum_incelas_gssp5,
                L2326.aluminum_incelas_ssp1,
                L2326.aluminum_incelas_ssp2,
                L2326.aluminum_incelas_ssp3,
                L2326.aluminum_incelas_ssp4,
                L2326.aluminum_incelas_ssp5,
                L2326.aluminum_incelas_gcam3)
  } else {
    stop("Unknown command")
  }
}
