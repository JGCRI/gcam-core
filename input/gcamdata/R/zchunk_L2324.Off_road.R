# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2324.Off_road
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for Off_road-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2324.SectorLogitTables[[ curr_table ]]$data}, \code{L2324.Supplysector_Off_road}, \code{L2324.FinalEnergyKeyword_Off_road},
#' \code{L2324.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2324.SubsectorLogit_Off_road}, \code{L2324.SubsectorShrwtFllt_Off_road},
#' \code{L2324.SubsectorInterp_Off_road}, \code{L2324.StubTech_Off_road}, \code{L2324.GlobalTechShrwt_Off_road}, \code{L2324.GlobalTechCoef_Off_road},
#' \code{L2324.GlobalTechCost_Off_road}, \code{L2324.GlobalTechCapture_Off_road}, \code{L2324.StubTechProd_Off_road}, \code{L2324.StubTechCalInput_Off_road},
#' \code{L2324.StubTechCoef_Off_road}, \code{L2324.PerCapitaBased_Off_road}, \code{L2324.BaseService_Off_road}, \code{L2324.PriceElasticity_Off_road},\code{L2324.GlobalTechCSeq_ind},
#' \code{object}. The corresponding file in the
#' original data system was \code{L2324.Off_road.R} (energy level2).
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for Off_road sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr gather spread
#' @author Yang Liu Sep 2019, Molly Charles 2020-21, 2022 modifications from Jay Fuhrman, Siddarth Durga, Page Kyle
module_energy_L2324.Off_road <- function(command, ...) {

  INCOME_ELASTICITY_OUTPUTS <- c("GCAM3",
                                 paste0("gSSP", 1:5),
                                 paste0("SSP", 1:5))

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A_regions",
             FILE = "energy/A324.sector",
             FILE = "energy/A324.subsector_interp",
             FILE = "energy/A324.nonenergy_Cseq",
             FILE = "energy/A324.subsector_logit",
             FILE = "energy/A324.subsector_shrwt",
             FILE = "energy/A324.globaltech_coef",
             FILE = "energy/A324.globaltech_eff",
             FILE = "energy/A324.globaltech_cost",
             FILE = "energy/A324.globaltech_retirement",
             FILE = "energy/A324.globaltech_shrwt",
             FILE = "energy/A324.globaltech_interp",
             FILE = "energy/A324.demand",
             "L1324.in_EJ_R_Off_road_F_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2324.Supplysector_Off_road",
             "L2324.FinalEnergyKeyword_Off_road",
             "L2324.SubsectorLogit_Off_road",
             "L2324.SubsectorShrwtFllt_Off_road",
             "L2324.SubsectorInterp_Off_road",
             "L2324.StubTech_Off_road",
             "L2324.GlobalTechShrwt_Off_road",
             "L2324.GlobalTechInterp_Off_road",
             "L2324.GlobalTechCoef_Off_road",
             "L2324.GlobalTechEff_Off_road",
             "L2324.GlobalTechCost_Off_road",
             "L2324.GlobalTechCSeq_ind",
			 "L2324.GlobalTechShutdown_Off_road",
             "L2324.GlobalTechSCurve_Off_road",
             "L2324.GlobalTechLifetime_Off_road",
             "L2324.GlobalTechProfitShutdown_Off_road",
             "L2324.StubTechCalInput_Off_road",
             "L2324.StubTechCoef_Off_road",
             "L2324.StubTechProd_Off_road",
             "L2324.PerCapitaBased_Off_road",
             "L2324.BaseService_Off_road",
			       "L2324.PriceElasticity_Off_road"))
  } else if(command == driver.MAKE) {


    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A324.sector <- get_data(all_data, "energy/A324.sector", strip_attributes = TRUE)
    A_regions <- get_data(all_data, "energy/A_regions")
    A324.subsector_interp <- get_data(all_data, "energy/A324.subsector_interp", strip_attributes = TRUE)
    A324.subsector_logit <- get_data(all_data, "energy/A324.subsector_logit", strip_attributes = TRUE)
    A324.subsector_shrwt <- get_data(all_data, "energy/A324.subsector_shrwt", strip_attributes = TRUE)
    A324.globaltech_coef <- get_data(all_data, "energy/A324.globaltech_coef", strip_attributes = TRUE)
    A324.globaltech_eff <- get_data(all_data, "energy/A324.globaltech_eff", strip_attributes = TRUE)
    A324.globaltech_cost <- get_data(all_data, "energy/A324.globaltech_cost", strip_attributes = TRUE)
	  A324.globaltech_retirement <- get_data(all_data, "energy/A324.globaltech_retirement", strip_attributes = TRUE)
	  A324.nonenergy_Cseq <- get_data(all_data, "energy/A324.nonenergy_Cseq", strip_attributes = TRUE)
    A324.globaltech_shrwt <- get_data(all_data, "energy/A324.globaltech_shrwt", strip_attributes = TRUE)
    A324.globaltech_interp <- get_data(all_data, "energy/A324.globaltech_interp", strip_attributes = TRUE)
    A324.demand <- get_data(all_data, "energy/A324.demand", strip_attributes = TRUE)
    L1324.in_EJ_R_Off_road_F_Y <- get_data(all_data, "L1324.in_EJ_R_Off_road_F_Y")
    # ===================================================
    # 0. Give binding for variable names used in pipeline
    has_district_heat <- year <- value <- GCAM_region_ID <- sector <- fuel <- year.fillout <- to.value <-
      technology <- supplysector <- subsector <- minicam.energy.input <- coefficient <-
      remove.fraction <- minicam.non.energy.input <- input.cost  <- calibration <- calOutputValue <- subs.share.weight <- region <-
      calibrated.value <- . <- scenario <- temp_lag <- base.service <- energy.final.demand <-
      value.x <- value.y <- parameter <- to.year <- from.year <- efficiency <- year.x <- year.y <-
      sector.name <- subsector.name <- stub.technology <- calOutputValue.x <- calOutputValue.y <- output_tot <-
      market.name <- terminal_coef <- share.weight <- interpolation.function <- NULL

    # ===================================================
    # 1. Perform computations
    # Create tables to delete technologies and subsectors in regions where heat is not modeled as a fuel
    has_not_heat <- filter(A_regions, has_district_heat == 0) # intermediate tibble

    calibrated_techs %>%
      filter(sector %in% c("in_industry_agriculture", "mining energy use", "construction energy use") & fuel == "heat") %>%
      select(supplysector, subsector, technology) %>%
      repeat_add_columns(tibble(GCAM_region_ID = has_not_heat[["GCAM_region_ID"]])) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L2324.rm_heat_techs_R # intermediate tibble

    # 1a. Supplysector information
    # L2324.Supplysector_Off_road: Supply sector information for Off_road sector
    A324.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L2324.Supplysector_Off_road

    # L2324.FinalEnergyKeyword_Off_road: Supply sector keywords for Off_road sector
    A324.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]], GCAM_region_names) %>%
      na.omit ->
      L2324.FinalEnergyKeyword_Off_road

    # 1b. Subsector information
    # L2324.SubsectorLogit_Off_road: Subsector logit exponents of Off_road sector
    A324.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L2324.SubsectorLogit_Off_road

    # and L2324.SubsectorShrwtFllt_Off_road: Subsector shareweights of Off_road sector
    A324.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) ->
#      anti_join(L2324.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2324.SubsectorShrwtFllt_Off_road

    #change the share weight in regions where baseyear biomass share weight is 1
    L2324.SubsectorShrwtFllt_Off_road %>%
      filter(region %in% energy.OFF_ROAD.BIOMASS_GROWTH,subsector=="biomass",supplysector == "agricultural energy use") %>%
      mutate(year.fillout = 2020,share.weight=0.15) ->
      L2324.SubsectorShrwtFllt_Off_road_add

    L2324.SubsectorShrwtFllt_Off_road %>%
      mutate(year.fillout = as.numeric(year.fillout)) %>%
      bind_rows(L2324.SubsectorShrwtFllt_Off_road_add) ->
      L2324.SubsectorShrwtFllt_Off_road

    # L2324.SubsectorInterp_Off_road: Subsector shareweight interpolation of Off_road sector
    A324.subsector_interp %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names) ->
      #anti_join(L2324.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2324.SubsectorInterp_Off_road

    #change interplate for the regions where baseyear biomass share weight is 1
    L2324.SubsectorInterp_Off_road %>%
      filter(region %in% energy.OFF_ROAD.BIOMASS_GROWTH,subsector=="biomass",supplysector == "agricultural energy use") %>%
      mutate(from.year=2015,to.year = 2020,interpolation.function="linear") ->
      L2324.SubsectorInterp_Off_road_add

    L2324.SubsectorInterp_Off_road_add %>%
      mutate(from.year=2020,to.year = 2100,interpolation.function="linear") ->
      L2324.SubsectorInterp_Off_road_add2

    L2324.SubsectorInterp_Off_road %>%
      anti_join(L2324.SubsectorInterp_Off_road_add, by = c("region","supplysector","subsector")) %>%
      mutate(to.year = as.numeric(to.year),from.year = as.numeric(from.year)) %>%
      bind_rows(L2324.SubsectorInterp_Off_road_add,L2324.SubsectorInterp_Off_road_add2) ->
      L2324.SubsectorInterp_Off_road

    # L2324.GlobalTechEff_ind: Energy inputs and efficiency of global Off_road energy use and feedstocks technologies
    A324.globaltech_eff %>%
      gather_years(value_col = "efficiency") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input,  year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(efficiency = approx_fun(year, efficiency, rule = 1),
             efficiency = round(efficiency, energy.DIGITS_EFFICIENCY)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) ->
      L2324.globaltech_eff.long # intermediate tibble

    L2324.globaltech_eff.long %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechEff"]]) ->
      L2324.GlobalTechEff_Off_road

    # Coefficients on global Off_road sector technologies (not energy-use or feedstocks)
    # L2324.GlobalTechCoef_road: Energy inputs and coefficients of global off-road technologies
    A324.globaltech_coef %>%
      rename(coefficient = "terminal_coef") %>%
      repeat_add_columns(tibble(year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS))) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>% # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L2324.GlobalTechCoef_Off_road

    # 1c. Technology information
    # L2324.StubTech_Off_road: Identification of stub technologies of Off_road
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A324.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      anti_join(L2324.rm_heat_techs_R, by = c("region", "technology")) %>% # Remove non-existent heat subsectors from each region
      rename(stub.technology = technology) ->
      L2324.StubTech_Off_road

    # L2324.GlobalTechShrwt_Off_road: Shareweights of global Off_road technologies
    A324.globaltech_shrwt %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "share.weight") ->
      L2324.GlobalTechShrwt_Off_road


    # L2324.GlobalTechCost_Off_road: Non-energy costs of global Off_road manufacturing technologies
    A324.globaltech_cost %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.non.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, value, rule = 1),
             input.cost = round(input.cost, energy.DIGITS_COST)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]]) ->
      L2324.GlobalTechCost_Off_road # intermediate tibble

    A324.nonenergy_Cseq %>%
      repeat_add_columns(tibble(year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS))) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCSeq"]]) ->
      L2324.GlobalTechCSeq_ind

    # Retirement information
    A324.globaltech_retirement %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L2324.globaltech_retirement_base

    # Copies first future year retirment information into all future years and appends back onto base year
    L2324.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) ->
      L2324.globaltech_retirement_future

    # filters base years from original and then appends future years
    L2324.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      bind_rows(L2324.globaltech_retirement_future) ->
      L2324.globaltech_retirement

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # This section checks L2324.globaltech_retirement for each of these functions and creates a separate level 2 file for each
    # All of these options have different headers, and all are allowed
    if(any(!is.na(L2324.globaltech_retirement$shutdown.rate))) {
      L2324.globaltech_retirement %>%
        filter(!is.na(L2324.globaltech_retirement$shutdown.rate)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "shutdown.rate") ->
        L2324.GlobalTechShutdown_Off_road
    }

    if(any(!is.na(L2324.globaltech_retirement$half.life))) {
      L2324.globaltech_retirement %>%
        filter(!is.na(L2324.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
        L2324.GlobalTechSCurve_Off_road
    }

    # L2324.GlobalTechLifetime_Off_road: Global tech lifetime
    if(any(is.na(L2324.globaltech_retirement$shutdown.rate) & is.na(L2324.globaltech_retirement$half.life))) {
      L2324.globaltech_retirement %>%
        filter(is.na(L2324.globaltech_retirement$shutdown.rate) & is.na(L2324.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime") ->
        L2324.GlobalTechLifetime_Off_road
    }

    # L2324.GlobalTechProfitShutdown_Off_road: Global tech profit shutdown decider and parameters
    if(any(!is.na(L2324.globaltech_retirement$median.shutdown.point))) {
      L2324.globaltech_retirement %>%
        filter(!is.na(L2324.globaltech_retirement$median.shutdown.point)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
        L2324.GlobalTechProfitShutdown_Off_road
    }

    # L2324.StubTechCoef_Off_road
    L2324.GlobalTechEff_Off_road %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology,
             coefficient = efficiency) %>%
      mutate(efficiency = NULL) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["StubTechCoef"]]), GCAM_region_names) %>%
      mutate(market.name =NULL,coefficient = NULL) ->
      #anti_join(L2324.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2324.Off_road_tmp


    # L2324.StubTechCalInput_Off_road: calibrated Off_road input
    L1324.in_EJ_R_Off_road_F_Y %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      complete(nesting(fuel,year,sector),region = GCAM_region_names$region) %>%
      mutate(GCAM_region_ID = NULL,value = replace_na(value,0)) %>%
      left_join(GCAM_region_names,by = c("region")) %>%
      left_join(select(calibrated_techs, sector, fuel, supplysector, subsector, technology), by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) ->
      L2324.in_EJ_R_Off_road_F_Y_tmp # intermediate tibble


    L2324.in_EJ_R_Off_road_F_Y_tmp %>%
      mutate(value = if_else(subsector ==  'mobile' & fuel == 'refined liquids', value * energy.LIQUID_FUEL_MOBILE_FRAC, #assign 80% of liquid fuels consumption to vehicles and the remainder to stationary equipment
                            if_else(subsector == 'stationary' & fuel == 'refined liquids', value * (1 - energy.LIQUID_FUEL_MOBILE_FRAC), value))) -> L2324.in_EJ_R_Off_road_F_Y_tmp

    L2324.in_EJ_R_Off_road_F_Y_tmp %>%
      left_join_error_no_match(distinct(select(A324.globaltech_eff, subsector, technology, minicam.energy.input)),
                               by = c("subsector", "stub.technology" = "technology")) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year) ->
      L2324.StubTechCalInput_Off_road_tmp

    L2324.Off_road_tmp %>%
      left_join(L2324.StubTechCalInput_Off_road_tmp,
                by = c("region", "supplysector", "subsector", "stub.technology", "year", "minicam.energy.input")) %>%
      mutate(fuel = NULL,sector = NULL, value = NULL,GCAM_region_ID  = NULL,calibrated.value = replace_na(calibrated.value,0),
             share.weight.year = year,
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      set_subsector_shrwt(value_col = "calibrated.value") %>%
      filter(!(region %in% L2324.rm_heat_techs_R$region & stub.technology == 'heat')) %>% #remove heat technology from regions that have no distict heat
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) ->
      L2324.StubTechCalInput_Off_road

     # L2324.StubTechProd_Off_road: calibrated output of Off_road sector
     # First, calculate service output by technology, for energy-use and feedstocks
    L2324.in_EJ_R_Off_road_F_Y_tmp %>%
       left_join_error_no_match(select(L2324.globaltech_eff.long, sector.name, subsector.name, technology, year,efficiency),
                                by = c("supplysector" = "sector.name", "subsector" = "subsector.name",
                                       "stub.technology" = "technology", "year")) %>%
       mutate(calOutputValue = round(value * efficiency, energy.DIGITS_CALOUTPUT)) ->
       L2324.out_EJ_R_Off_road_serv_F_Yh # intermediate tibble

     # intermediate tibble to extract Off_road names
     A324.globaltech_shrwt %>%
       filter(supplysector == "construction") %>%
       select(supplysector, subsector, technology) ->
       L2324.Off_road_names

     # Aggregate service output by region. This is the output of the Off_road sector in each region.
     L2324.out_EJ_R_Off_road_serv_F_Yh %>%
       filter(supplysector != "agricultural energy use" & supplysector != "mining energy use") %>%
       group_by(region, GCAM_region_ID, year) %>%
       summarise(calOutputValue = sum(calOutputValue)) %>%
       ungroup %>%
       mutate(supplysector = L2324.Off_road_names[["supplysector"]],
              subsector = L2324.Off_road_names[["subsector"]],
              stub.technology = L2324.Off_road_names[["technology"]],
              share.weight.year = year,
              subs.share.weight = if_else(calOutputValue > 0, 1, 0),
              tech.share.weight = subs.share.weight) %>%
       select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
       L2324.StubTechProd_Off_road

     # L2324.StubTechCoef_Off_road: calibrated output of Off_road sector
     # Next, aggregate service output by sector to calculate the portion of each input
     L2324.out_EJ_R_Off_road_serv_F_Yh %>%
       filter(supplysector != "agricultural energy use" & supplysector != "mining energy use") %>%
       group_by(region, GCAM_region_ID, supplysector, year) %>%
       summarise(calOutputValue = sum(calOutputValue)) %>%
       ungroup %>%
       left_join_error_no_match(select(L2324.StubTechProd_Off_road, calOutputValue, region, year),
                                by = c("region", "year")) %>%
       rename(calOutputValue = calOutputValue.x,
              output_tot = calOutputValue.y) %>%
       mutate(coefficient = replace_na(calOutputValue / output_tot,0)) %>%
       rename(minicam.energy.input = supplysector) %>%
       mutate(supplysector = L2324.Off_road_names[["supplysector"]],
              subsector = L2324.Off_road_names[["subsector"]],
              stub.technology = L2324.Off_road_names[["technology"]],
              market.name = region) %>%
       select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
       L2324.StubTechCoef_Off_road_base # intermediate tibble?

     # This set of coefficients covers only the base years; the first "future" period will default to the global tech coefficient
     # Instead, interpolate the coefficients to these global default values in a specified period
     L2324.StubTechCoef_Off_road_base %>%
       complete(nesting(region, supplysector, subsector, stub.technology, minicam.energy.input, market.name),
                year = unique(c(MODEL_YEARS, energy.INDCOEF_CONVERGENCE_YR))) %>%
       left_join(select(A324.globaltech_coef, supplysector, subsector, technology, minicam.energy.input, terminal_coef),
                 by = c("supplysector", "subsector", stub.technology = "technology", "minicam.energy.input")) %>%
       mutate(coefficient = if_else(year == energy.INDCOEF_CONVERGENCE_YR, terminal_coef, coefficient)) %>%
       select(-terminal_coef) %>%
       group_by(region, supplysector, subsector, stub.technology, minicam.energy.input) %>%
       mutate(coefficient = round(approx_fun(year, coefficient,rule = 2), energy.DIGITS_COEFFICIENT)) %>%
       ungroup() %>%
       filter(year %in% MODEL_YEARS) ->   # drop the terminal coef year if it's outside of the model years
       L2324.StubTechCoef_Off_road


    # L2324.PerCapitaBased_Off_road: per-capita based flag for Off_road exports final demand
    A324.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PerCapitaBased"]], GCAM_region_names)  ->
      L2324.PerCapitaBased_Off_road


    # L2324.BaseService_Off_road: base-year service output of Off_road
    L2324.out_EJ_R_Off_road_serv_F_Yh %>%
      filter(supplysector == "agricultural energy use" | supplysector == "mining energy use") %>%
      group_by(region, GCAM_region_ID, supplysector, year) %>%
      summarise(base.service = sum(calOutputValue)) %>%
      ungroup %>%
      mutate(energy.final.demand = supplysector,supplysector = NULL,GCAM_region_ID = NULL) ->
      L2324.BaseService_Off_road_tmp

    L2324.StubTechProd_Off_road %>%
      select(region, year, supplysector,base.service = calOutputValue) %>%
      rename(energy.final.demand = supplysector) %>%
      group_by(region,year,energy.final.demand) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup() %>%
      bind_rows(L2324.BaseService_Off_road_tmp) ->
      L2324.BaseService_Off_road

    #For regions with 0 in base year, modify Subsector shareweight and interpolation
    L2324.out_EJ_R_Off_road_serv_F_Yh %>%
      group_by(region,supplysector, GCAM_region_ID, year) %>%
      summarise(value = sum(calOutputValue)) %>%
      ungroup %>%
      select(region, year, supplysector,value) %>%
      filter(value == 0)  ->
      nobaseyear

    L2324.SubsectorShrwtFllt_Off_road %>%
      left_join(nobaseyear, by = c("region", "supplysector")) %>%
      mutate(value = replace_na(value,1),share.weight = if_else(value ==0,0.5,share.weight),year = NULL,value = NULL) ->
      L2324.SubsectorShrwtFllt_Off_road


    L2324.SubsectorInterp_Off_road %>%
      left_join(nobaseyear, by = c("region", "supplysector")) %>%
      mutate(value = replace_na(value,1),interpolation.function = if_else(value ==0,"linear",interpolation.function),year = NULL,value = NULL) ->
      L2324.SubsectorInterp_Off_road

    L2324.GlobalTechInterp_Off_road <- A324.globaltech_interp %>%
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = subsector)


    # L2324.PriceElasticity_Off_road: price elasticity
    A324.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PriceElasticity"]][LEVEL2_DATA_NAMES[["PriceElasticity"]] != "year"], GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["PriceElasticity"]]) ->
      L2324.PriceElasticity_Off_road

    # ===================================================
    # Produce outputs
    L2324.Supplysector_Off_road %>%
      add_title("Supply sector information for Off_road sector") %>%
      add_units("NA") %>%
      add_comments("For Off_road sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A324.sector is expended into all GCAM regions") %>%
      add_legacy_name("L2324.Supplysector_Off_road") %>%
      add_precursors("energy/A324.sector", "common/GCAM_region_names") ->
      L2324.Supplysector_Off_road

    L2324.FinalEnergyKeyword_Off_road %>%
      add_title("Supply sector keywords for Off_road sector") %>%
      add_units("NA") %>%
      add_comments("For Off_road sector, the supply sector final energy keywords from A324.sector are expended into all GCAM regions") %>%
      add_legacy_name("L2324.FinalEnergyKeyword_Off_road") %>%
      add_precursors("energy/A324.sector", "common/GCAM_region_names") ->
      L2324.FinalEnergyKeyword_Off_road

    L2324.SubsectorLogit_Off_road %>%
      add_title("Subsector logit exponents of Off_road sector") %>%
      add_units("Unitless") %>%
      add_comments("For Off_road sector, the subsector logit exponents from A324.subsector_logit are expanded into all GCAM regions") %>%
      add_legacy_name("L2324.SubsectorLogit_Off_road") %>%
      add_precursors("energy/A324.subsector_logit", "energy/A_regions", "common/GCAM_region_names") ->
      L2324.SubsectorLogit_Off_road

    L2324.SubsectorShrwtFllt_Off_road %>%
      add_title("Subsector shareweights of Off_road sector") %>%
      add_units("unitless") %>%
      add_comments("For Off_road sector, the subsector shareweights from A324.subsector_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2324.SubsectorShrwtFllt_Off_road") %>%
      add_precursors("energy/A324.subsector_shrwt", "energy/A_regions", "common/GCAM_region_names") ->
      L2324.SubsectorShrwtFllt_Off_road

    L2324.SubsectorInterp_Off_road %>%
      add_title("Subsector shareweight interpolation of Off_road sector") %>%
      add_units("NA") %>%
      add_comments("For Off_road sector, the subsector shareweight interpolation function infromation from A324.subsector_interp is expanded into all GCAM regions") %>%
      add_legacy_name("L2324.SubsectorInterp_Off_road") %>%
      add_precursors("energy/A324.subsector_interp", "energy/A_regions", "common/GCAM_region_names") ->
      L2324.SubsectorInterp_Off_road

    L2324.GlobalTechInterp_Off_road %>%
      add_title("Technology shareweight interpolation of Off_road sector") %>%
      add_units("NA") %>%
      add_comments("Rules from global technology database are applied to all regions") %>%
      add_precursors("energy/A324.globaltech_interp") ->
      L2324.GlobalTechInterp_Off_road


    L2324.StubTech_Off_road %>%
      add_title("Identification of stub technologies of Off_road") %>%
      add_units("NA") %>%
      add_comments("For Off_road sector, the stub technologies from A324.globaltech_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2324.StubTech_Off_road") %>%
      add_precursors("energy/A324.globaltech_shrwt", "energy/A_regions", "common/GCAM_region_names") ->
      L2324.StubTech_Off_road

    L2324.GlobalTechShrwt_Off_road %>%
      add_title("Shareweights of global Off_road technologies") %>%
      add_units("Unitless") %>%
      add_comments("For Off_road sector, the share weights from A324.globaltech_shrwt are interpolated into all base years and future years") %>%
      add_legacy_name("L2324.GlobalTechShrwt_Off_road") %>%
      add_precursors("energy/A324.globaltech_shrwt") ->
      L2324.GlobalTechShrwt_Off_road

    L2324.GlobalTechCoef_Off_road %>%
      add_title("Energy inputs and coefficients of Off_road technologies") %>%
      add_units("Unitless") %>%
      add_comments("For Off_road sector, the energy use coefficients from A324.globaltech_coef are interpolated into all model years") %>%
      add_legacy_name("L2324.GlobalTechCoef_Off_road") %>%
      add_precursors("energy/A324.globaltech_coef") ->
      L2324.GlobalTechCoef_Off_road

    L2324.GlobalTechEff_Off_road %>%
      add_title("Energy inputs and efficiency of global Off_road energy use and feedstocks technologies") %>%
      add_units("Unitless") %>%
      add_comments("For Off_road sector, the efficiency values from A324.globaltech_eff are interpolated into all base years and future years") %>%
      add_legacy_name("L2324.GlobalTechEff_Off_road") %>%
      add_precursors("energy/A324.globaltech_eff") ->
      L2324.GlobalTechEff_Off_road

    L2324.GlobalTechCost_Off_road %>%
      add_title("Non-energy costs of global Off_road manufacturing technologies") %>%
      add_units("1975$/kg for supplysector Off_road; 1975$/GJ for supplysector process heat Off_road") %>%
      add_comments("For Off_road sector, the Non-energy costs of global Off_road manufacturing technologies are calculated then adjusted with CCS to include CO2 capture costs") %>%
      add_legacy_name("L2324.GlobalTechCost_Off_road") %>%
      add_precursors("energy/A324.globaltech_cost", "energy/A324.globaltech_coef") ->
      L2324.GlobalTechCost_Off_road

    L2324.GlobalTechCSeq_ind %>%
      add_title("CO2 capture fractions from global electricity generation technologies") %>%
      add_units("Unitless") %>%
      add_comments("Remove fractions from A324.nonenergy_Cseq are expanded into all model years") %>%
      add_legacy_name("L2324.GlobalTechCSeq_ind") %>%
      add_precursors("energy/A324.nonenergy_Cseq") ->
      L2324.GlobalTechCSeq_ind

    if(exists("L2324.GlobalTechShutdown_Off_road")) {
      L2324.GlobalTechShutdown_Off_road %>%
        add_title("Global tech lifetime for techs with shutdown rate") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that uses a phased retirement function") %>%
        add_legacy_name("L2324.GlobalTechShutdown_Off_road") %>%
        add_precursors("energy/A324.globaltech_retirement") ->
        L2324.GlobalTechShutdown_Off_road
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2324.GlobalTechShutdown_Off_road") ->
        L2324.GlobalTechShutdown_Off_road
    }

    if(exists("L2324.GlobalTechSCurve_Off_road")) {
      L2324.GlobalTechSCurve_Off_road %>%
        add_title("Global tech lifetime for techs with s-curve retirement function") %>%
        add_units("Lifetime in years, half-life in years") %>%
        add_comments("Filters for any technology that uses an S-curve retirement function") %>%
        add_legacy_name("L2324.GlobalTechSCurve_Off_road") %>%
        add_precursors("energy/A324.globaltech_retirement") ->
        L2324.GlobalTechSCurve_Off_road
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2324.GlobalTechSCurve_Off_road") ->
        L2324.GlobalTechSCurve_Off_road
    }

    if(exists("L2324.GlobalTechLifetime_Off_road")) {
      L2324.GlobalTechLifetime_Off_road %>%
        add_title("Global tech lifetime for any technology with no retirement function") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that has no phased or S-curve retirement function, empty by default.") %>%
        add_legacy_name("L2324.GlobalTechLifetime_Off_road") %>%
        add_precursors("energy/A324.globaltech_retirement") ->
        L2324.GlobalTechLifetime_Off_road
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2324.GlobalTechLifetime_Off_road") ->
        L2324.GlobalTechLifetime_Off_road
    }

    if(exists("L2324.GlobalTechProfitShutdown_Off_road")) {
      L2324.GlobalTechProfitShutdown_Off_road %>%
        add_title("Global tech profit shutdown decider and parameters") %>%
        add_units("Unitless, used to determine shape of the function defining the relationship between shutdown rate and profitability") %>%
        add_comments("Filters for any technologies that use a profit-based shutdown parameter") %>%
        add_legacy_name("L2324.GlobalTechProfitShutdown_Off_road") %>%
        add_precursors("energy/A324.globaltech_retirement") ->
        L2324.GlobalTechProfitShutdown_Off_road
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2324.GlobalTechProfitShutdown_Off_road") ->
        L2324.GlobalTechProfitShutdown_Off_road
    }


    L2324.StubTechCalInput_Off_road %>%
       add_title("calibrated Off_road production") %>%
       add_units("EJ") %>%
       add_comments("Values are calculated using L1324.in_EJ_R_Off_road_F_Y then added GCAM region information and supplysector, subsector, technology, and input information") %>%
       add_legacy_name("L2324.StubTechCalInput_Off_road") %>%
       add_precursors("energy/calibrated_techs", "L1324.in_EJ_R_Off_road_F_Y", "common/GCAM_region_names","energy/A324.globaltech_eff") ->
       L2324.StubTechCalInput_Off_road


    L2324.StubTechProd_Off_road %>%
      add_title("Calibrated output of Off_road sector") %>%
      add_units("EJ") %>%
      add_comments("Service output values are calculated by technology, for energy-use and feedstocks then aggregated by region") %>%
      add_legacy_name("L2324.StubTechProd_Off_road") %>%
      add_precursors("energy/calibrated_techs", "L1324.in_EJ_R_Off_road_F_Y", "common/GCAM_region_names","energy/A324.globaltech_eff","energy/A324.globaltech_eff") ->
      L2324.StubTechProd_Off_road

    L2324.StubTechCoef_Off_road %>%
      add_title("region-specific coefficients of Off_road production technologies") %>%
      add_units("unitless") %>%
      add_comments("Coefficients") %>%
      add_legacy_name("L2324.StubTechCoef_Off_road") %>%
      add_precursors("energy/calibrated_techs", "common/GCAM_region_names","energy/A324.globaltech_coef","energy/A324.globaltech_eff") ->
      L2324.StubTechCoef_Off_road

      L2324.PerCapitaBased_Off_road %>%
      add_title("per-capita based flag for Off_road exports final demand") %>%
      add_units("NA") %>%
      add_comments("Per-capita based flags for Off_road from A324.demand are expanded into all GCAM regions") %>%
      add_legacy_name("L2324.PerCapitaBased_Off_road") %>%
      add_precursors("energy/A324.demand", "common/GCAM_region_names") ->
      L2324.PerCapitaBased_Off_road

    L2324.BaseService_Off_road %>%
      add_title("base-year service output of Off_road") %>%
      add_units("EJ") %>%
      add_comments("Transformed from L2324.StubTechProd_Off_road by adding energy.final.demand") %>%
      add_legacy_name("L2324.BaseService_Off_road") %>%
      add_precursors("energy/A324.demand","L1324.in_EJ_R_Off_road_F_Y", "energy/calibrated_techs", "common/GCAM_region_names","energy/A324.globaltech_eff") ->
      L2324.BaseService_Off_road

    L2324.PriceElasticity_Off_road %>%
      add_title("price elasticity for Off_road") %>%
      add_units("Unitless") %>%
      add_comments("The elasticity values from A324.demand are expanded into all GCAM_regions") %>%
      add_legacy_name("L2324.PriceElasticity_Off_road") %>%
      add_precursors("energy/A324.demand", "common/GCAM_region_names") ->
      L2324.PriceElasticity_Off_road

      return_data(L2324.Supplysector_Off_road, L2324.FinalEnergyKeyword_Off_road, L2324.SubsectorLogit_Off_road,
                  L2324.SubsectorShrwtFllt_Off_road, L2324.SubsectorInterp_Off_road, L2324.GlobalTechInterp_Off_road,
                  L2324.StubTech_Off_road, L2324.GlobalTechShrwt_Off_road,L2324.GlobalTechShutdown_Off_road,
                  L2324.GlobalTechSCurve_Off_road, L2324.GlobalTechLifetime_Off_road, L2324.GlobalTechProfitShutdown_Off_road,
                  L2324.GlobalTechCoef_Off_road, L2324.GlobalTechEff_Off_road,L2324.GlobalTechCost_Off_road,
                  L2324.StubTechCalInput_Off_road,L2324.StubTechCoef_Off_road,
                  L2324.StubTechProd_Off_road,L2324.GlobalTechCSeq_ind,
                  L2324.PerCapitaBased_Off_road, L2324.BaseService_Off_road,
                  L2324.PriceElasticity_Off_road)


  } else {
    stop("Unknown command")
  }
}
