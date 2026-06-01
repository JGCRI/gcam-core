# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L232.Inc_Elas_scenarios
#'
#' Derive income elasticity scenarios for detailed industries cement/Off_road/chemical/aluminum/paper/iron and steel
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs
#' @details Takes per-capita GDP from ssp scenarios in each region.
#' Then derive income elasticity scenarios for detailed industries cement/Off_road/chemical/aluminum/paper/iron and steel
#' for each region by linear interpolation of assumption data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter left_join mutate select transmute
#' @importFrom tidyr gather spread
#' @importFrom stats approx
#' @author RH April 2017 xz 2025
module_socio_L232.Inc_Elas_scenarios <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "socioeconomics/A321.inc_elas_output",
      FILE = "socioeconomics/A323.inc_elas_parameter",
      FILE = "socioeconomics/A324.inc_elas",
      FILE = "socioeconomics/A325.inc_elas",
      FILE = "socioeconomics/A326.inc_elas_parameter",
      FILE = "socioeconomics/A327.inc_elas",
      FILE = "socioeconomics/A32.inc_elas_output",
      FILE = "energy/A32.demand",
      FILE = "energy/A321.demand",
      "L102.pcgdp_thous90USD_Scen_R_Y",
      "L101.Pop_thous_SSP_R_Yfut",
      "L101.Pop_thous_R_Yh",
      "L2321.BaseService_cement",
      "LB1092.Tradebalance_iron_steel_Mt_R_Y",
      "L1326.out_Mt_R_aluminum_Yh",
      "L232.BaseService_ind")

  MODULE_OUTPUTS <-
    c("L2321.IncomeElasticity_cement_Scen",
      "L2323.IncomeElasticity_iron_steel_Scen",
      "L2324.IncomeElasticity_Off_road_Scen",
      "L2325.IncomeElasticity_chemical_Scen",
      "L2326.IncomeElasticity_aluminum_Scen",
      "L2327.IncomeElasticity_paper_Scen",
      "L232.IncomeElasticity_ind_Scen")

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


    # prepare pc income
    L102.pcgdp_thous90USD_Scen_R_Y <-
      L102.pcgdp_thous90USD_Scen_R_Y %>%
      rename(pcgdp_90thousUSD = value) %>%
      mutate(year = as.integer(year)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")

    L101.Pop_thous_R_Yh <- L101.Pop_thous_R_Yh %>%
      rename(population = value) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")

    L101.Pop_thous_SSP_R_Yfut <- L101.Pop_thous_SSP_R_Yfut %>%
      ungroup() %>%
      rename(population = value) %>%
      mutate(year = as.integer(year)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")

    pcgdp_Yfut <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      mutate(pcgdp_90thousUSD_Yfut = pcgdp_90thousUSD, year = min(MODEL_FUTURE_YEARS)) %>%
      select(scenario, GCAM_region_ID, year, pcgdp_90thousUSD_Yfut)

    ## Create one population dataset to pass timeshift tests
    # This is required because L101.Pop_thous_SSP_R_Yfut uses FUTURE_YEARS,
    # but here we want to create a dataset from MODEL_FUTURE_YEARS, which may start before FUTURE_YEARS
    L101_Pop_hist_and_fut <- L101.Pop_thous_R_Yh %>%
      repeat_add_columns(distinct(L101.Pop_thous_SSP_R_Yfut, scenario)) %>%
      bind_rows(L101.Pop_thous_SSP_R_Yfut)

    # L232 .IncomeElasticity_cement_scen: income elasticity of cement (scenario-specific)
    #  calculate the per-capita GDP pathways of every GDP scenario and combine
    L102.pcgdp_thous90USD_Scen_R_Y %>%
      rename(value = pcgdp_90thousUSD) %>%
      filter(year %in% c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS)) %>%
      # Per-capita GDP ratios, which are used in the equation for demand growth
      group_by(GCAM_region_ID, scenario) %>%
      mutate(temp_lag = lag(value, 1),
             value = value / temp_lag) %>%
      ungroup %>%
      select(-temp_lag) %>%
      filter(year %in% MODEL_FUTURE_YEARS) ->
      L232.pcgdpRatio_ALL_R_Y # intermediate tibble



    # cement ----

    # Calculate the cement output as the base-year cement output times the GDP ratio raised to the income elasticity
    # The income elasticity is looked up based on the prior year's output
    L232.pcgdpRatio_ALL_R_Y %>%
      select(GCAM_region_ID, scenario) %>%
      distinct %>%
      left_join_error_no_match(GCAM_region_names, by = 'GCAM_region_ID') %>%
      mutate(year = MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L2321.BaseService_cement, by = c("year", "region")) %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh, by = c("year", "GCAM_region_ID", "region")) %>%
      mutate(value = base.service * CONV_MIL_THOUS / population) %>%
      select(-base.service, -energy.final.demand, -population) ->
      L2321.Output_cement # intermediate tibble

    # At each time, the output is equal to the prior period's output times the GDP ratio, raised to the elasticity
    # that corresponds to the output that was observed in the prior time period. This method prevents (ideally) runaway
    # production/consumption.
    elast_years <- c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS)
    for(i in seq_along(elast_years)[-1]) {
      L2321.Output_cement %>%
        filter(year == elast_years[i - 1]) %>%
        # strick left join fails timeshift test due to NAs in L102.pcgdp_thous90USD_Scen_R_Y under timeshift mode
        left_join(filter(L232.pcgdpRatio_ALL_R_Y, year == elast_years[i]), by = c("GCAM_region_ID", "scenario", "region")) ->
        intermediate

      intermediate %>%
        mutate(parameter = approx(x = A321.inc_elas_output[["pc.output_t"]],
                                  y = A321.inc_elas_output[["inc_elas"]],
                                  xout = intermediate[["value.x"]],
                                  rule = 2)[['y']],
               value = value.x * value.y ^ parameter,
               year = elast_years[i]) %>%
        select(GCAM_region_ID, scenario, region, year, value) %>%
        bind_rows(L2321.Output_cement) ->
        L2321.Output_cement
    }

    # Now that we have cement output, we can back out the appropriate income elasticities
    L2321.Output_cement %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      mutate(value = approx( x = A321.inc_elas_output[["pc.output_t"]],
                             y = A321.inc_elas_output[["inc_elas"]],
                             xout = value, rule = 2)[["y"]],
             value = round(value, energy.DIGITS_INCELAS_IND)) %>%
      rename(income.elasticity = value) %>%
      mutate(energy.final.demand = A321.demand[["energy.final.demand"]]) ->
      L2321.IncomeElasticity_cement_Scen # intermediate tibble

    L2321.IncomeElasticity_cement_Scen %>%
      add_title(paste("Income elasticity of cement - SSPs")) %>%
      add_units("Unitless") %>%
      add_comments("First calculate cement output as the base-year cement output times the GDP ratio raised to the income elasticity") %>%
      add_comments("Then back out the appropriate income elasticities from cement output") %>%
      add_legacy_name("L2321.IncomeElasticity_cement_Scen") %>%
      add_precursors("L101.Pop_thous_R_Yh", "L102.pcgdp_thous90USD_Scen_R_Y",
                     "L2321.BaseService_cement",
                     "common/GCAM_region_names",
                     "energy/A321.demand",
                     "socioeconomics/A321.inc_elas_output") ->
      L2321.IncomeElasticity_cement_Scen

    assertthat::assert_that(
      L2321.IncomeElasticity_cement_Scen %>%
        rename(value = income.elasticity) %>%
        filter(year == MODEL_SCENARIO_ALIGN_YEAR) %>%
        group_by(region, year, energy.final.demand) %>%
        summarize(max = max(value), min = min(value), .groups = "drop") %>%
        filter(max != min) %>%
        nrow() == 0,
      msg = paste0("Values in ", MODEL_SCENARIO_ALIGN_YEAR, "across scenarios not fully aligned")
    )


    # iron steel ----

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


    #First calculate the per capita steel consumption
    L2323.pcgdp_thous90USD_Scen_R_Y  <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
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

    L2323.pcgdp_thous90USD_Scen_R_Y %>%
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
             income.elasticity = replace(income.elasticity,income.elasticity < -10,-10)) ->
      L2323.IncomeElasticity_iron_steel_Scen


    # Produce outputs
    L2323.IncomeElasticity_iron_steel_Scen %>%
      add_title("iron_steel Income Elasticity: SSPs") %>%
      add_legacy_name("L2323.IncomeElasticity_iron_steel_Scen") %>%
      add_units("Unitless (% change in service demand / % change in income)") %>%
      add_comments("Uses previously calculated per-capita GDP assumptions for all ssp scenarios") %>%
      add_comments("iron_steel income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
      add_precursors("common/GCAM_region_names",
                     "socioeconomics/A323.inc_elas_parameter",
                     "L101.Pop_thous_SSP_R_Yfut",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y",
                     "LB1092.Tradebalance_iron_steel_Mt_R_Y") ->
      L2323.IncomeElasticity_iron_steel_Scen

    assertthat::assert_that(
      L2323.IncomeElasticity_iron_steel_Scen %>%
        rename(value = income.elasticity) %>%
        filter(year == MODEL_SCENARIO_ALIGN_YEAR) %>%
        group_by(region, year, energy.final.demand) %>%
        summarize(max = max(value), min = min(value), .groups = "drop") %>%
        filter(max != min) %>%
        nrow() == 0,
      msg = paste0("Values in ", MODEL_SCENARIO_ALIGN_YEAR, "across scenarios not fully aligned")
    )

    # off road ----

    # Linearly interpolate income elasticity for each level of per-capita GDP,
    # using the assumption data
    L102.pcgdp_thous90USD_Scen_R_Y %>%
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

    assertthat::assert_that(
      L2324.IncomeElasticity_Off_road_Scen %>%
        rename(value = income.elasticity) %>%
        filter(year == MODEL_SCENARIO_ALIGN_YEAR) %>%
        group_by(region, year, energy.final.demand) %>%
        summarize(max = max(value), min = min(value), .groups = "drop") %>%
        filter(max != min) %>%
        nrow() == 0,
      msg = paste0("Values in ", MODEL_SCENARIO_ALIGN_YEAR, "across scenarios not fully aligned")
    )


    # chemicals ----

    A325.inc_elas %>% filter(sector == "chemical") ->
      inc_elas_chemical

    # Linearly interpolate income elasticity for each level of per-capita GDP,
    # using the assumption data
    L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      # find the corresponding interpolated value of income elasticity from the inc_elas_chemical table.
      mutate(income.elasticity = approx(x = inc_elas_chemical$pcgdp_90thousUSD,
                                        y = inc_elas_chemical$inc_elas,xout = pcgdp_90thousUSD,
                                        # Rule 2 means that data outside of the interval of input
                                        # data will be assigned the cloest data extreme
                                        rule = 2)[['y']] %>% round(3),
             energy.final.demand = "chemical") %>%
      select(scenario, region, energy.final.demand, year, income.elasticity) %>%
      arrange(year) ->
      L2325.IncomeElasticity_chemical_Scen

    L2325.IncomeElasticity_chemical_Scen %>%
      add_title(paste("chemical Income Elasticity: SSPs")) %>%
      add_units("Unitless (% change in service demand / % change in income)") %>%
      add_comments("Uses previously calculated per-capita GDP assumptions for all ssp scenarios") %>%
      add_comments("chemical income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
      add_legacy_name("L2325.IncomeElasticity_chemical_Scen") %>%
      add_precursors("common/GCAM_region_names",
                     "socioeconomics/A325.inc_elas",
                     "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L2325.IncomeElasticity_chemical_Scen

    assertthat::assert_that(
      L2325.IncomeElasticity_chemical_Scen %>%
        rename(value = income.elasticity) %>%
        filter(year == MODEL_SCENARIO_ALIGN_YEAR) %>%
        group_by(region, year, energy.final.demand) %>%
        summarize(max = max(value), min = min(value), .groups = "drop") %>%
        filter(max != min) %>%
        nrow() == 0,
      msg = paste0("Values in ", MODEL_SCENARIO_ALIGN_YEAR, "across scenarios not fully aligned")
    )


    # aluminum ----

    aluminum_pro_Yfut <- L1326.out_Mt_R_aluminum_Yh %>%
      filter(sector == "Aluminum") %>%
      group_by(GCAM_region_ID,year) %>%
      summarise(aluminum_hist = sum(value))%>%
      ungroup() %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      mutate(year = min(MODEL_FUTURE_YEARS))


    # unclear why base year is being filtered from FUTURE population, it returns
    # empty tibble. Just filtering by the first future year. Can't use
    # L101.Pop_thous_R_Yh because it doesn't have scenarios
    population_Yfut <- L101.Pop_thous_SSP_R_Yfut %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      mutate(population_Yfut = population) %>%
      select(scenario, GCAM_region_ID, year, population_Yfut)


    #First calculate the per capita aluminum consumption
    L2326.pcgdp_thous90USD_Scen_R_Y  <-
      L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      left_join_error_no_match(L101_Pop_hist_and_fut, by = c("scenario", "GCAM_region_ID", "year", "region")) %>%
      left_join_error_no_match(A326.inc_elas_parameter, by = c( "region")) %>%
      # TODO-BYU: (year - MODEL_FINAL_BASE_YEAR) needs to be confirmed by Pralit as he is working on updating this function
      mutate(per_capita_aluminum = a * exp(b/(pcgdp_90thousUSD * 1000 * gdp_deflator(2005, 1990))) * (1-m) ^ (year - MODEL_FINAL_BASE_YEAR),
             aluminum_pro = per_capita_aluminum * population * 0.000001)


    #Rebuild a new tibble save the previous year value
    L2326.pcgdp_thous90USD_Scen_R_Y_5_before <-
      L2326.pcgdp_thous90USD_Scen_R_Y %>%
      # essentially year + 5, but gets the next year dynamically without assuming + 5 year increments
      mutate(year = sapply(year, function(y) MODEL_FUTURE_YEARS[which(MODEL_FUTURE_YEARS > y)[1]]),
             pcgdp_90thousUSD_before = pcgdp_90thousUSD,
             aluminum_pro_before = aluminum_pro,
             population_before = aluminum_pro) %>%
      select(scenario, GCAM_region_ID, region, year, pcgdp_90thousUSD_before, aluminum_pro_before, population_before)

    L2326.pcgdp_thous90USD_Scen_R_Y %>%
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
             income.elasticity = replace(income.elasticity,income.elasticity < -3,-3)) ->
      L2326.IncomeElasticity_aluminum_Scen

    L2326.IncomeElasticity_aluminum_Scen %>%
      add_title(paste("aluminum Income Elasticity: SSPs")) %>%
      add_units("Unitless (% change in service demand / % change in income)") %>%
      add_comments("Uses previously calculated per-capita GDP assumptions for all ssp scenarios") %>%
      add_comments("aluminum income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
      add_legacy_name("L2326.IncomeElasticity_aluminum_Scen") %>%
      add_precursors("common/GCAM_region_names", "socioeconomics/A326.inc_elas_parameter",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L1326.out_Mt_R_aluminum_Yh") ->
      L2326.IncomeElasticity_aluminum_Scen

    assertthat::assert_that(
      L2326.IncomeElasticity_aluminum_Scen %>%
        rename(value = income.elasticity) %>%
        filter(year == MODEL_SCENARIO_ALIGN_YEAR) %>%
        group_by(region, year, energy.final.demand) %>%
        summarize(max = max(value), min = min(value), .groups = "drop") %>%
        filter(max != min) %>%
        nrow() == 0,
      msg = paste0("Values in ", MODEL_SCENARIO_ALIGN_YEAR, "across scenarios not fully aligned")
    )


    # paper ----

    A327.inc_elas %>% filter(sector == "paper") ->
      inc_elas_paper

    # Linearly interpolate income elasticity for each level of per-capita GDP,
    # using the assumption data
    L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      # Using approx rather than approx_fun because data is from assumption file, not in our tibble
      mutate(income.elasticity = approx(x = inc_elas_paper$pcgdp_90thousUSD,
                                        y = inc_elas_paper$inc_elas,
                                        xout = pcgdp_90thousUSD,
                                        # Rule 2 means that data outside of the interval of input
                                        # data will be assigned the cloest data extreme
                                        rule = 2)[['y']] %>% round(3),
             energy.final.demand = "paper") %>%
      select(scenario, region, energy.final.demand, year, income.elasticity) %>%
      arrange(year) ->
      L2327.IncomeElasticity_paper_Scen

    L2327.IncomeElasticity_paper_Scen %>%
      add_title("Paper Income Elasticity: SSPs") %>%
      add_units("Unitless (% change in service demand / % change in income)") %>%
      add_comments("Uses previously calculated per-capita GDP assumptions for all ssp scenarios") %>%
      add_comments("Paper income elasticity for each GCAM region generated by linear interpolation of assumption data") %>%
      add_legacy_name("L2327.IncomeElasticity_paper_Scen") %>%
      add_precursors("common/GCAM_region_names",
                     "socioeconomics/A327.inc_elas",
                     "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L2327.IncomeElasticity_paper_Scen

    assertthat::assert_that(
      L2327.IncomeElasticity_paper_Scen %>%
        rename(value = income.elasticity) %>%
        filter(year == MODEL_SCENARIO_ALIGN_YEAR) %>%
        group_by(region, year, energy.final.demand) %>%
        summarize(max = max(value), min = min(value), .groups = "drop") %>%
        filter(max != min) %>%
        nrow() == 0,
      msg = paste0("Values in ", MODEL_SCENARIO_ALIGN_YEAR, "across scenarios not fully aligned")
    )


    # other industry ----

    # Calculate the industrial output as the base-year industrial output times the GDP ratio raised to the income elasticity
    # The income elasticity is looked up based on the prior year's output
    L232.pcgdpRatio_ALL_R_Y %>%
      select(GCAM_region_ID, scenario) %>%
      distinct %>%
      left_join_error_no_match(GCAM_region_names, by = 'GCAM_region_ID') %>%
      mutate(year = MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L232.BaseService_ind, by = c("year", "region")) %>%
      left_join_error_no_match(L101.Pop_thous_R_Yh, by = c("year", "GCAM_region_ID", "region")) %>%
      mutate(value = base.service * CONV_BIL_THOUS / population) %>%
      select(-base.service, -energy.final.demand, -population) ->
      L232.Output_ind

    # At each time, the output is equal to the prior period's output times the GDP ratio, raised to the elasticity
    # that corresponds to the output that was observed in the prior time period. This method prevents (ideally) runaway
    # industrial production.
    elast_years <- c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS)
    for(i in seq_along(elast_years)[-1]) {
      L232.Output_ind %>%
        filter(year == elast_years[i - 1]) %>%
        left_join(filter(L232.pcgdpRatio_ALL_R_Y, year == elast_years[i]), by = c("GCAM_region_ID", "region", "scenario")) %>% # strick left join fails timeshift test due to NAs in L102.pcgdp_thous90USD_Scen_R_Y under timeshift mode
        mutate(parameter = approx(x = A32.inc_elas_output[["pc.output_GJ"]],
                                  y = A32.inc_elas_output[["inc_elas"]],
                                  xout = value.x,
                                  rule = 2)[['y']],
               value = value.x * value.y ^ parameter,
               year = elast_years[i]) %>%
        select(GCAM_region_ID, scenario, region, year, value) %>%
        bind_rows(L232.Output_ind) ->
        L232.Output_ind
    }

    # Now that we have industrial output, we can back out the appropriate income elasticities
    L232.Output_ind %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      mutate(value = approx( x = A32.inc_elas_output[["pc.output_GJ"]],
                             y = A32.inc_elas_output[["inc_elas"]],
                             xout = value,
                             rule = 2)[["y"]]) %>%
      mutate(value = round(value, energy.DIGITS_INCELAS_IND)) %>%
      rename(income.elasticity = value) %>%
      mutate(energy.final.demand = A32.demand[["energy.final.demand"]]) ->
      L232.IncomeElasticity_ind_Scen # intermediate tibble

    # KVC: SSP1 needs lower income elasticities. Storyline has limited growth in energy-related industries
    # because of warm fuzzy feelings about environment. We are hard-coding this for a while.
    L232.IncomeElasticity_ind_Scen %>%
      filter(scenario == "SSP1") %>%
      mutate(income.elasticity = if_else(year > MODEL_SCENARIO_ALIGN_YEAR, income.elasticity * 0.75, income.elasticity)) %>%
      bind_rows(filter(L232.IncomeElasticity_ind_Scen, scenario != "SSP1")) ->
      L232.IncomeElasticity_ind_Scen

    L232.IncomeElasticity_ind_Scen %>%
      add_title("Income elasticity of other industry - SSPs") %>%
      add_units("Unitless") %>%
      add_comments("First calculate industrial output as the base-year industrial output times the GDP ratio raised to the income elasticity") %>%
      add_comments("Then back out the appropriate income elasticities from industrial output") %>%
      add_comments("Note lower income elasticities for SSP1 are hard-coded.") %>%
      add_legacy_name("L232.IncomeElasticity_ind_Scen") %>%
      add_precursors("L101.Pop_thous_R_Yh", "L102.pcgdp_thous90USD_Scen_R_Y", "common/GCAM_region_names",
                     "L232.BaseService_ind", "energy/A32.demand", "socioeconomics/A32.inc_elas_output") ->
      L232.IncomeElasticity_ind_Scen

    assertthat::assert_that(
      L232.IncomeElasticity_ind_Scen %>%
        rename(value = income.elasticity) %>%
        filter(year == MODEL_SCENARIO_ALIGN_YEAR) %>%
        group_by(region, year, energy.final.demand) %>%
        summarize(max = max(value), min = min(value), .groups = "drop") %>%
        filter(max != min) %>%
        nrow() == 0,
      msg = paste0("Values in ", MODEL_SCENARIO_ALIGN_YEAR, "across scenarios not fully aligned")
    )

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
