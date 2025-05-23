# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2323.iron_steel
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for iron and steel-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2323.SectorLogitTables[[ curr_table ]]$data}, \code{L2323.Supplysector_iron_steel}, \code{L2323.FinalEnergyKeyword_iron_steel},
#' \code{L2323.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2323.SubsectorLogit_iron_steel}, \code{L2323.SubsectorShrwtFllt_iron_steel},
#' \code{L2323.SubsectorInterp_iron_steel}, \code{L2323.StubTech_iron_steel}, \code{L2323.GlobalTechShrwt_iron_steel}, \code{L2323.GlobalTechCoef_iron_steel},
#' \code{L2323.GlobalTechCost_iron_steel}, \code{L2323.GlobalTechCapture_iron_steel}, \code{L2323.StubTechProd_iron_steel}, \code{L2323.StubTechCalInput_iron_steel},
#' \code{L2323.StubTechCoef_iron_steel}, \code{L2323.StubTechCost_iron_steel}, \code{L2323.PerCapitaBased_iron_steel}, \code{L2323.BaseService_iron_steel}, \code{L2323.PriceElasticity_iron_steel},
#' \code{object}. The corresponding file in the
#' original data system was \code{L2323.iron_steel.R} (energy level2).
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for iron and steel sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr gather spread
#' @author Yang Liu Sep 2019, Siddarth Durga April 2023
module_energy_L2323.iron_steel <- function(command, ...) {

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A323.sector",
             FILE = "energy/A323.subsector_logit",
             FILE = "energy/A323.subsector_shrwt",
             FILE = "energy/A323.globaltech_coef",
             FILE = "energy/A323.globaltech_cost",
             FILE = "energy/A323.globaltech_shrwt",
             FILE = "energy/A323.globaltech_co2capture",
             FILE = "energy/A323.globaltech_retirement",
             FILE = "energy/A323.demand",
             FILE = "energy/A323.capital_cost_adders",
             FILE = "energy/A323.ccs_adders",
             FILE = "energy/TZ_steel_production_costs",
             FILE = "energy/mappings/TZ_steel_cost_gcam_mapping",
             FILE = "energy/mappings/TZ_steel_cost_oecd_mapping",
			       "L1323.out_Mt_R_iron_steel_Yh",
             "L1323.IO_GJkg_R_iron_steel_F_Yh",
			       "L1323.SubsectorInterp_iron_steel",
			       "LB1092.Tradebalance_iron_steel_Mt_R_Y"
			       ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2323.Supplysector_iron_steel",
             "L2323.FinalEnergyKeyword_iron_steel",
             "L2323.SubsectorLogit_iron_steel",
             "L2323.SubsectorShrwtFllt_iron_steel",
             "L2323.SubsectorInterp_iron_steel",
             "L2323.StubTech_iron_steel",
             "L2323.GlobalTechShrwt_iron_steel",
             "L2323.GlobalTechCoef_iron_steel",
             "L2323.GlobalTechCost_iron_steel",
             "L2323.GlobalTechTrackCapital_iron_steel",
             "L2323.GlobalTechCapture_iron_steel",
             "L2323.GlobalTechShutdown_en",
             "L2323.GlobalTechSCurve_en",
             "L2323.GlobalTechLifetime_en",
             "L2323.GlobalTechProfitShutdown_en",
             "L2323.StubTechProd_iron_steel",
             #"L2323.StubTechCalInput_iron_steel",
             "L2323.StubTechCoef_iron_steel",
             "L2323.StubTechCost_iron_steel",
             "L2323.PerCapitaBased_iron_steel",
             "L2323.BaseService_iron_steel",
			       "L2323.PriceElasticity_iron_steel"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A323.sector <- get_data(all_data, "energy/A323.sector", strip_attributes = TRUE)
    A323.subsector_logit <- get_data(all_data, "energy/A323.subsector_logit", strip_attributes = TRUE)
    A323.subsector_shrwt <- get_data(all_data, "energy/A323.subsector_shrwt", strip_attributes = TRUE)
    A323.globaltech_coef <- get_data(all_data, "energy/A323.globaltech_coef", strip_attributes = TRUE)
    A323.globaltech_cost <- get_data(all_data, "energy/A323.globaltech_cost", strip_attributes = TRUE)
    A323.globaltech_shrwt <- get_data(all_data, "energy/A323.globaltech_shrwt", strip_attributes = TRUE)
    A323.globaltech_co2capture <- get_data(all_data, "energy/A323.globaltech_co2capture", strip_attributes = TRUE)
    A323.globaltech_retirement <- get_data(all_data, "energy/A323.globaltech_retirement", strip_attributes = TRUE)
    A323.demand <- get_data(all_data, "energy/A323.demand", strip_attributes = TRUE)
    A323.capital_cost_adders <- get_data(all_data, "energy/A323.capital_cost_adders", strip_attributes = TRUE)
    A323.ccs_adders <- get_data(all_data, "energy/A323.ccs_adders", strip_attributes = TRUE)
    L1323.out_Mt_R_iron_steel_Yh <- get_data(all_data, "L1323.out_Mt_R_iron_steel_Yh", strip_attributes = TRUE)
    L1323.IO_GJkg_R_iron_steel_F_Yh <- get_data(all_data, "L1323.IO_GJkg_R_iron_steel_F_Yh",strip_attributes = TRUE)
    L1323.SubsectorInterp_iron_steel <- get_data(all_data,"L1323.SubsectorInterp_iron_steel",strip_attributes = TRUE)
    LB1092.Tradebalance_iron_steel_Mt_R_Y <- get_data(all_data, "LB1092.Tradebalance_iron_steel_Mt_R_Y",strip_attributes = TRUE)
    TZ_steel_production_costs <- get_data(all_data, "energy/TZ_steel_production_costs", strip_attributes = TRUE)
    TZ_steel_cost_gcam_mapping <- get_data(all_data, "energy/mappings/TZ_steel_cost_gcam_mapping", strip_attributes = TRUE)
    TZ_steel_cost_oecd_mapping <- get_data(all_data, "energy/mappings/TZ_steel_cost_oecd_mapping", strip_attributes = TRUE)


    # ===================================================
    # Give binding for variable names used in pipeline
    year <- value <- GCAM_region_ID <- sector <- fuel <- year.fillout <- to.value <-
      technology <- supplysector <- subsector <- minicam.energy.input <- coefficient <-
      remove.fraction <- minicam.non.energy.input <- input.cost  <- calibration <- calOutputValue <- subs.share.weight <- region <-
      calibrated.value <- . <- scenario <- temp_lag <- base.service <- energy.final.demand <-
      value.x <- value.y <- parameter <- year.x <- year.y <- tech.share.weight <- stub.technology <-
      market.name <- sector.name <- subsector.name <- terminal_coef <- share.weight.year <- coeff <-
      Plant_ID <- Model_Year <- Primary_Production_Route <- Category <- Component <- Value <-
      Unit <- Country <- capital_cost_frac <- ccs_cost_adder <- NULL

    # ====================================================================
    # 0. Calculate the regional steel production costs by stub technology
    # The TZ steel cost database contains plant-level steel production costs across major producing countries
    # The costs are aggregated by primary production route, averaged across OECD and non-OECD regions, and mapped to the GCAM regions
    # ====================================================================

      TZ_steel_production_costs %>%
        # filter non-energy costs for GCAM model years
        filter(Model_Year %in% c(MODEL_BASE_YEARS)) %>%
        rename(year=Model_Year,subsector= Primary_Production_Route,value=Value)%>%
        # remove energy and total costs from data
        filter(!(Category%in%c("Total","Energy"))) %>%
        # remove coke from BF-BOF raw materials cost
        # GCAM estimates cost of coke in energy costs
        filter(!(Component%in%c("Coke"))) %>%
        # map TZ countries with oecd classification
        left_join(TZ_steel_cost_oecd_mapping,by=c("Country"))-> TZ_steel_production_costs

    # Function to aggregate steel production data by Country or OECD regions
    aggregate_steel_production_costs <- function(data,agg_region) {

        TZ_steel_production_costs %>%
        # estimate total costs by Plant ID across countries
        group_by(subsector,Plant_ID,year)%>%
        mutate(value=sum(value)) %>%
        distinct(Plant_ID,subsector,year,{{agg_region}},value)%>%
        # calculate mean costs by country for EAF and BF-BOF
        group_by({{agg_region}},subsector,year)%>%
        summarize(value=mean(value)) %>%
        mutate(subsector=if_else(subsector=="EAF","EAF with scrap","BLASTFUR")) -> all_steel_production_costs

      return(all_steel_production_costs)
    }

    # Calculate the average steel production costs by sub sector for OECD and non-OECD countries
    oecd_steel_production_costs <- aggregate_steel_production_costs(data=TZ_steel_production_costs,agg_region=OECD_mapping) %>%
      rename(Country=OECD_mapping)

    # Calculate average steel production costs by sub sector for countries in transition zero database and combine
    # with OECD average data
    all_steel_production_costs <- bind_rows(aggregate_steel_production_costs(data=TZ_steel_production_costs,agg_region=Country),
                                            oecd_steel_production_costs)

    #add capital costs and CCS costs to estimate total production costs
    all_steel_production_costs <-  TZ_steel_cost_gcam_mapping%>%
      left_join(all_steel_production_costs,by=c("Country")) %>%
      mutate(supplysector="iron and steel")%>%
      left_join(A323.capital_cost_adders,by=c("subsector","supplysector"))%>%
      #multiply total OPEX, labor, and raw materials costs with capital cost fraction from IEA
      mutate(value=value+(value*capital_cost_frac))%>%
      left_join(A323.ccs_adders,by=c("subsector","supplysector"))%>%
      mutate(value=((value/CONV_T_KG)+ccs_cost_adder)*gdp_deflator(1975,base_year=2015))%>%
      select(supplysector,subsector,region,year,stub.technology,minicam.non.energy.input,value)

    #complete subsector and technology nesting across model years
    all_steel_production_costs %>%
      rename(input.cost=value)%>%
      complete(nesting(supplysector, subsector, region,stub.technology,input.cost, minicam.non.energy.input), year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS))%>%
      select(region,supplysector, subsector, stub.technology,year,minicam.non.energy.input, input.cost) %>%
      mutate(input.cost = round(input.cost, energy.DIGITS_COST)) %>%
      ungroup() %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCost"]]) ->
      L2323.StubTechCost_iron_steel

    # ============================================================================
    # ============================================================================

    # L2323.GlobalTechCost_iron_steel: Non-energy costs of global iron_steel manufacturing technologies
    A323.globaltech_cost %>%
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
      L2323.GlobalTechCost_iron_steel

    # ===================================================
    # 1. Perform computations
    # Create tables to delete technologies and subsectors in regions where heat is not modeled as a fuel
    # 1a. Supplysector information
    # L2323.Supplysector_iron_steel: Supply sector information for iron and steel sector
    A323.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L2323.Supplysector_iron_steel

    # L2323.FinalEnergyKeyword_iron_steel: Supply sector keywords for iron and steel sector
    A323.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]], GCAM_region_names) %>%
      na.omit ->
      L2323.FinalEnergyKeyword_iron_steel

    # 1b. Subsector information
    # L2323.SubsectorLogit_iron_steel: Subsector logit exponents of iron and steel sector
    A323.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L2323.SubsectorLogit_iron_steel

    # and L2323.SubsectorShrwtFllt_iron_steel: Subsector shareweights of iron and steel sector
    A323.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) ->
      L2323.SubsectorShrwtFllt_iron_steel

    # L2323.SubsectorInterp_iron_steel: Subsector shareweight interpolation of iron and steel sector
    L2323.SubsectorInterp_iron_steel <- L1323.SubsectorInterp_iron_steel

    # 1c. Technology information
    # L2323.StubTech_iron_steel: Identification of stub technologies of iron_steel
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A323.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L2323.StubTech_iron_steel

    # L2323.GlobalTechShrwt_iron_steel: Shareweights of global iron_steel technologies
    A323.globaltech_shrwt %>%
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
      L2323.GlobalTechShrwt_iron_steel

    # L2323.GlobalTechCoef_iron_steel: Energy inputs and coefficients of iron_steel technologies
    A323.globaltech_coef %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, value, rule = 1),
             coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L2323.GlobalTechCoef_iron_steel

    # Carbon capture rates from technologies with CCS
    # L2323.GlobalTechCapture_iron_steel: CO2 capture fractions from global iron_steel production technologies with CCS
    # No need to consider historical periods or intermittent technologies here
    A323.globaltech_co2capture %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(year, value, rule = 1),
             remove.fraction = round(remove.fraction, energy.DIGITS_REMOVE.FRACTION)) %>%
      ungroup %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "remove.fraction") %>%
      mutate(storage.market = energy.CO2.STORAGE.MARKET) ->
      L2323.GlobalTechCapture_iron_steel

    # Retirement information
    A323.globaltech_retirement %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L2323.globaltech_retirement_base

    # Copies first future year retirment information into all future years and appends back onto base year
    L2323.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) ->
      L2323.globaltech_retirement_future

    # filters base years from original and then appends future years
    L2323.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      bind_rows(L2323.globaltech_retirement_future) ->
      L2323.globaltech_retirement

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # This section checks L2323.globaltech_retirement for each of these functions and creates a separate level 2 file for each
    # All of these options have different headers, and all are allowed
    if(any(!is.na(L2323.globaltech_retirement$shutdown.rate))) {
      L2323.globaltech_retirement %>%
        filter(!is.na(L2323.globaltech_retirement$shutdown.rate)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "shutdown.rate") ->
        L2323.GlobalTechShutdown_en
    }

    if(any(!is.na(L2323.globaltech_retirement$half.life))) {
      L2323.globaltech_retirement %>%
        filter(!is.na(L2323.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
        L2323.GlobalTechSCurve_en
    }

    # L2323.GlobalTechLifetime_en: Global tech lifetime
    if(any(is.na(L2323.globaltech_retirement$shutdown.rate) & is.na(L2323.globaltech_retirement$half.life))) {
      L2323.globaltech_retirement %>%
        filter(is.na(L2323.globaltech_retirement$shutdown.rate) & is.na(L2323.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime") ->
        L2323.GlobalTechLifetime_en
    }

    # L2323.GlobalTechProfitShutdown_en: Global tech profit shutdown decider and parameters
    if(any(!is.na(L2323.globaltech_retirement$median.shutdown.point))) {
      L2323.globaltech_retirement %>%
        filter(!is.na(L2323.globaltech_retirement$median.shutdown.point)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
        L2323.GlobalTechProfitShutdown_en
    }

    # L2323.GlobalTechCost_iron_steel: Non-energy costs of global iron_steel manufacturing technologies
    A323.globaltech_cost %>%
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
      L2323.GlobalTechCost_iron_steel

    FCR <- (socioeconomics.DEFAULT_INTEREST_RATE * (1+socioeconomics.DEFAULT_INTEREST_RATE)^socioeconomics.INDUSTRY_CAP_PAYMENTS) /
      ((1+socioeconomics.DEFAULT_INTEREST_RATE)^socioeconomics.INDUSTRY_CAP_PAYMENTS -1)
    L2323.GlobalTechCost_iron_steel %>%
      # note: the units for iron and steel output / costs will yield a dollar
      # amount of million 1975$, the rest of the capital market will be in billion 1975$
      # so we need to include the unit conversion here to make it consistent
      mutate(capital.coef = socioeconomics.INDUSTRY_CAPITAL_RATIO / FCR / 1000,
             tracking.market = socioeconomics.EN_CAPITAL_MARKET_NAME,
             # vintaging is active so no need for depreciation
             depreciation.rate = 0) %>%
      select(LEVEL2_DATA_NAMES[['GlobalTechTrackCapital']]) ->
      L2323.GlobalTechTrackCapital_iron_steel

    # Calibration and region-specific data
    # L2323.StubTechProd_iron_steel: calibrated iron_steel production
    calibrated_techs %>%
      filter(calibration == "output") %>% # Only take the tech IDs where the calibration is identified as output
      select(sector, supplysector, subsector, technology) %>%
      distinct ->
      calibrated_techs_export # temporary tibble

    L1323.out_Mt_R_iron_steel_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT), sector = "iron and steel") %>%
      left_join(calibrated_techs_export %>% select(sector,supplysector, subsector,technology), by = c("sector","subsector")) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(stub.technology = technology,
             share.weight.year = year,
             subs.share.weight = 1,
             tech.share.weight = if_else(subsector == technology , 1, 0),
             calOutputValue = calOutputValue * tech.share.weight) %>%
      unique() %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2323.StubTechProd_iron_steel

    # L2323.StubTechCoef_iron_steel: region-specific coefficients of iron_steel production technologies
    # Take this as a given in all years for which data is available
    calibrated_techs %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      distinct ->
      calibrated_techs_export # temporary tibble

    L1323.IO_GJkg_R_iron_steel_F_Yh %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(calibrated_techs, by = c("supplysector", "subsector", "technology", "fuel")) %>%
      mutate(coefficient = round(coefficient, energy.DIGITS_COEFFICIENT),
             stub.technology = technology,
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L2323.StubTechCoef_iron_steel_tmp

    # Instead, interpolate the coefficients to these global default values in a specified period
    L2323.StubTechCoef_iron_steel_tmp %>%
      complete(nesting(region, supplysector, subsector, stub.technology, minicam.energy.input, market.name),
               year = unique(c(MODEL_YEARS, energy.INDCOEF_CONVERGENCE_YR))) %>%
      left_join(select(L2323.GlobalTechCoef_iron_steel %>% rename(terminal_coef = coefficient,supplysector = sector.name,subsector = subsector.name),
                       supplysector, subsector, technology, minicam.energy.input, terminal_coef, year),
                by = c("supplysector", "subsector", stub.technology = "technology", "minicam.energy.input","year")) %>%
      left_join(L2323.StubTechCoef_iron_steel_tmp %>%mutate(coeff = coefficient,coefficient=NULL),
                 by = c("region", "supplysector", "subsector", "stub.technology", "minicam.energy.input", "market.name", "year")) %>%
      left_join(L2323.StubTechProd_iron_steel %>% select(-share.weight.year,-subs.share.weight,-tech.share.weight),
                by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
      mutate(coefficient = if_else(year > MODEL_FINAL_BASE_YEAR , coeff, coefficient),
             #coefficient = if_else(year > MODEL_FINAL_BASE_YEAR & stub.technology == energy.IRON_STEEL.DEFAULT_COEF[1] , terminal_coef, coefficient),
             coefficient = if_else(year > MODEL_FINAL_BASE_YEAR & minicam.energy.input == energy.IRON_STEEL.DEFAULT_COEF[2] , terminal_coef, coefficient),
             coefficient = if_else(year > MODEL_FINAL_BASE_YEAR & minicam.energy.input == energy.IRON_STEEL.DEFAULT_COEF[3] , terminal_coef, coefficient)) %>%
      select(-terminal_coef,-coeff,-calOutputValue) %>%
      group_by(region, supplysector, subsector, stub.technology, minicam.energy.input) %>%
      mutate(coefficient = round(approx_fun(year, coefficient,rule = 2), energy.DIGITS_COEFFICIENT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) ->   # drop the terminal coef year if it's outside of the model years
      L2323.StubTechCoef_iron_steel

    # L2323.PerCapitaBased_iron_steel: per-capita based flag for iron_steel exports final demand
    A323.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PerCapitaBased"]], GCAM_region_names)  ->
      L2323.PerCapitaBased_iron_steel

    # L2323.BaseService_iron_steel: base-year service output of iron and steel demand (not production)
    LB1092.Tradebalance_iron_steel_Mt_R_Y %>%
      filter(metric %in% c("consumption_reval"),
             year %in% c(MODEL_BASE_YEARS),GCAM_region != "NA")%>%
      mutate(value=round(value, energy.DIGITS_CALOUTPUT))%>%
      distinct(GCAM_region,year,value)%>%
      rename(region=GCAM_region,calOutputValue=value)%>%
      select(region, year, base.service = calOutputValue) %>%
      mutate(energy.final.demand = A323.demand[["energy.final.demand"]]) %>%
      ungroup()-> L2323.BaseService_iron_steel

    # L2323.PriceElasticity_iron_steel: price elasticity
    A323.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PriceElasticity"]][LEVEL2_DATA_NAMES[["PriceElasticity"]] != "year"], GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["PriceElasticity"]]) ->
      L2323.PriceElasticity_iron_steel

    # ===================================================
    # Produce outputs
    L2323.Supplysector_iron_steel %>%
      add_title("Supply sector information for iron and steel sector") %>%
      add_units("NA") %>%
      add_comments("For iron and steel sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A323.sector is expended into all GCAM regions") %>%
      add_legacy_name("L2323.Supplysector_iron_steel") %>%
      add_precursors("energy/A323.sector", "common/GCAM_region_names") ->
      L2323.Supplysector_iron_steel

    L2323.FinalEnergyKeyword_iron_steel %>%
      add_title("Supply sector keywords for iron and steel sector") %>%
      add_units("NA") %>%
      add_comments("For iron and steel sector, the supply sector final energy keywords from A323.sector are expended into all GCAM regions") %>%
      add_legacy_name("L2323.FinalEnergyKeyword_iron_steel") %>%
      add_precursors("energy/A323.sector", "common/GCAM_region_names") ->
      L2323.FinalEnergyKeyword_iron_steel

    L2323.SubsectorLogit_iron_steel %>%
      add_title("Subsector logit exponents of iron and steel sector") %>%
      add_units("Unitless") %>%
      add_comments("For iron and steel sector, the subsector logit exponents from A323.subsector_logit are expanded into all GCAM regions") %>%
      add_legacy_name("L2323.SubsectorLogit_iron_steel") %>%
      add_precursors("energy/A323.subsector_logit", "common/GCAM_region_names") ->
      L2323.SubsectorLogit_iron_steel

    L2323.SubsectorShrwtFllt_iron_steel %>%
      add_title("Subsector shareweights of iron and steel sector") %>%
      add_units("unitless") %>%
      add_comments("For iron and steel sector, the subsector shareweights from A323.subsector_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2323.SubsectorShrwtFllt_iron_steel") %>%
      add_precursors("energy/A323.subsector_shrwt", "common/GCAM_region_names") ->
      L2323.SubsectorShrwtFllt_iron_steel

    L2323.SubsectorInterp_iron_steel %>%
      add_title("Subsector shareweight interpolation of iron and steel sector") %>%
      add_units("NA") %>%
      add_comments("For iron and steel sector, the subsector shareweight interpolation function infromation from A323.subsector_interp is expanded into all GCAM regions") %>%
      add_legacy_name("L2323.SubsectorInterp_iron_steel") %>%
      add_precursors("L1323.SubsectorInterp_iron_steel") ->
      L2323.SubsectorInterp_iron_steel

    L2323.StubTech_iron_steel %>%
      add_title("Identification of stub technologies of iron and steel") %>%
      add_units("NA") %>%
      add_comments("For iron and steel sector, the stub technologies from A323.globaltech_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2323.StubTech_iron_steel") %>%
      add_precursors("energy/A323.globaltech_shrwt", "common/GCAM_region_names") ->
      L2323.StubTech_iron_steel

    L2323.GlobalTechShrwt_iron_steel %>%
      add_title("Shareweights of global iron and steel technologies") %>%
      add_units("Unitless") %>%
      add_comments("For iron and steel sector, the share weights from A323.globaltech_shrwt are interpolated into all base years and future years") %>%
      add_legacy_name("L2323.GlobalTechShrwt_iron_steel") %>%
      add_precursors("energy/A323.globaltech_shrwt") ->
      L2323.GlobalTechShrwt_iron_steel

    L2323.GlobalTechCoef_iron_steel %>%
      add_title("Energy inputs and coefficients of iron and steel technologies") %>%
      add_units("scrap input is unitless (Mt scrap per Mt steel); all others are GJ per kg (EJ of energy per Mt of steel)") %>%
      add_comments("For iron and steel sector, the energy use coefficients from A323.globaltech_coef are interpolated into all model years") %>%
      add_legacy_name("L2323.GlobalTechCoef_iron_steel") %>%
      add_precursors("energy/A323.globaltech_coef") ->
      L2323.GlobalTechCoef_iron_steel

    L2323.GlobalTechCost_iron_steel %>%
      add_title("Non-energy costs of global iron and steel manufacturing technologies") %>%
      add_units("1975$/kg for supplysector iron_steel; 1975$/GJ for supplysector process heat iron_steel") %>%
      add_comments("For iron and steel sector, the Non-energy costs of global iron and steel manufacturing technologies are calculated then adjusted with CCS to include CO2 capture costs") %>%
      add_legacy_name("L2323.GlobalTechCost_iron_steel") %>%
      add_precursors("energy/A323.globaltech_cost", "energy/A323.globaltech_co2capture", "energy/A323.globaltech_coef") ->
      L2323.GlobalTechCost_iron_steel

    L2323.GlobalTechTrackCapital_iron_steel %>%
      add_title("Convert non-energy inputs to track the annual capital investments.") %>%
      add_units(("Coefficients")) %>%
      add_comments("Track capital investments for purposes of macro economic calculations") %>%
      same_precursors_as(L2323.GlobalTechCost_iron_steel) ->
      L2323.GlobalTechTrackCapital_iron_steel

    L2323.StubTechCost_iron_steel %>%
      add_title("Regionally varying non-energy costs of iron and steel manufacturing technologies") %>%
      add_units("1975$/kg for supplysector iron_steel") %>%
      add_comments("For iron and steel sector, the non-energy costs of regional iron and steel manufacturing technologies are obtained from transition zero global cost tracker database and then adjusted with capital costs and CCS to include CO2 capture costs") %>%
      add_legacy_name("L2323.StubTechCost_iron_steel") %>%
      add_precursors("energy/A323.capital_cost_adders", "energy/A323.ccs_adders", "energy/TZ_steel_production_costs","energy/mappings/TZ_steel_cost_gcam_mapping",
                     "energy/mappings/TZ_steel_cost_oecd_mapping") ->
      L2323.StubTechCost_iron_steel

    L2323.GlobalTechCapture_iron_steel %>%
      add_title("CO2 capture fractions from global iron snf steel production technologies with CCS") %>%
      add_units("Unitless") %>%
      add_comments("For iron and steel sector, the remove fractions from A323.globaltech_co2capture are interpolated into all model years") %>%
      add_legacy_name("L2323.GlobalTechCapture_iron_steel") %>%
      add_precursors("energy/A323.globaltech_co2capture") ->
      L2323.GlobalTechCapture_iron_steel

    if(exists("L2323.GlobalTechShutdown_en")) {
      L2323.GlobalTechShutdown_en %>%
        add_title("Global tech lifetime for techs with shutdown rate") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that uses a phased retirement function") %>%
        add_legacy_name("L2323.GlobalTechShutdown_en") %>%
        add_precursors("energy/A323.globaltech_retirement") ->
        L2323.GlobalTechShutdown_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2323.GlobalTechShutdown_en") ->
        L2323.GlobalTechShutdown_en
    }

    if(exists("L2323.GlobalTechSCurve_en")) {
      L2323.GlobalTechSCurve_en %>%
        add_title("Global tech lifetime for techs with s-curve retirement function") %>%
        add_units("Lifetime in years, half-life in years") %>%
        add_comments("Filters for any technology that uses an S-curve retirement function") %>%
        add_legacy_name("L2323.GlobalTechSCurve_en") %>%
        add_precursors("energy/A323.globaltech_retirement") ->
        L2323.GlobalTechSCurve_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2323.GlobalTechSCurve_en") ->
        L2323.GlobalTechSCurve_en
    }

    if(exists("L2323.GlobalTechLifetime_en")) {
      L2323.GlobalTechLifetime_en %>%
        add_title("Global tech lifetime for any technology with no retirement function") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that has no phased or S-curve retirement function, empty by default.") %>%
        add_legacy_name("L2323.GlobalTechLifetime_en") %>%
        add_precursors("energy/A323.globaltech_retirement") ->
        L2323.GlobalTechLifetime_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2323.GlobalTechLifetime_en") ->
        L2323.GlobalTechLifetime_en
    }

    if(exists("L2323.GlobalTechProfitShutdown_en")) {
      L2323.GlobalTechProfitShutdown_en %>%
        add_title("Global tech profit shutdown decider and parameters") %>%
        add_units("Unitless, used to determine shape of the function defining the relationship between shutdown rate and profitability") %>%
        add_comments("Filters for any technologies that use a profit-based shutdown parameter") %>%
        add_legacy_name("L2323.GlobalTechProfitShutdown_en") %>%
        add_precursors("energy/A323.globaltech_retirement") ->
        L2323.GlobalTechProfitShutdown_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2323.GlobalTechProfitShutdown_en") ->
        L2323.GlobalTechProfitShutdown_en
    }


    L2323.StubTechProd_iron_steel %>%
      add_title("calibrated iron and steel production") %>%
      add_units("Mt") %>%
      add_comments("Values are calculated using L1323.out_Mt_R_iron_steel_Yh then added GCAM region information and supplysector, subsector, and technology information") %>%
      add_legacy_name("L2323.StubTechProd_iron_steel") %>%
      add_precursors("energy/calibrated_techs", "L1323.out_Mt_R_iron_steel_Yh", "common/GCAM_region_names") ->
      L2323.StubTechProd_iron_steel

    L2323.StubTechCoef_iron_steel %>%
      add_title("region-specific coefficients of iron and steel production technologies") %>%
      add_units("scrap input is unitless (Mt scrap per Mt steel); all others are GJ per kg (EJ of energy per Mt of steel)") %>%
      add_comments("Coefficients are calculated using L1323.IO_GJkg_R_iron_steel_F_Yh") %>%
      add_legacy_name("L2323.StubTechCoef_iron_steel") %>%
      add_precursors("energy/calibrated_techs", "L1323.IO_GJkg_R_iron_steel_F_Yh", "common/GCAM_region_names") ->
      L2323.StubTechCoef_iron_steel

    L2323.PerCapitaBased_iron_steel %>%
      add_title("per-capita based flag for iron and steel exports final demand") %>%
      add_units("NA") %>%
      add_comments("Per-capita based flags for iron and steel from A323.demand are expanded into all GCAM regions") %>%
      add_legacy_name("L2323.PerCapitaBased_iron_steel") %>%
      add_precursors("energy/A323.demand", "common/GCAM_region_names") ->
      L2323.PerCapitaBased_iron_steel

    L2323.BaseService_iron_steel %>%
      add_title("base-year service output of iron and steel") %>%
      add_units("Mt") %>%
      add_comments("Transformed from L2323.StubTechProd_iron_steel by adding energy.final.demand from A323.demand") %>%
      add_legacy_name("L2323.BaseService_iron_steel") %>%
      add_precursors("energy/A323.demand", "energy/calibrated_techs", "L1323.out_Mt_R_iron_steel_Yh", "common/GCAM_region_names","LB1092.Tradebalance_iron_steel_Mt_R_Y") ->
      L2323.BaseService_iron_steel

    L2323.PriceElasticity_iron_steel %>%
      add_title("price elasticity for iron and steel") %>%
      add_units("Unitless") %>%
      add_comments("The elasticity values from A323.demand are expanded into all GCAM_regions") %>%
      add_legacy_name("L2323.PriceElasticity_iron_steel") %>%
      add_precursors("energy/A323.demand", "common/GCAM_region_names") ->
      L2323.PriceElasticity_iron_steel

      return_data(L2323.Supplysector_iron_steel, L2323.FinalEnergyKeyword_iron_steel, L2323.SubsectorLogit_iron_steel,
                  L2323.SubsectorShrwtFllt_iron_steel,L2323.SubsectorInterp_iron_steel,
                  L2323.StubTech_iron_steel, L2323.GlobalTechShrwt_iron_steel,
                  L2323.GlobalTechCoef_iron_steel, L2323.GlobalTechCost_iron_steel, L2323.GlobalTechCapture_iron_steel, L2323.GlobalTechShutdown_en,
                  L2323.GlobalTechSCurve_en, L2323.GlobalTechLifetime_en, L2323.GlobalTechProfitShutdown_en,
                  L2323.StubTechProd_iron_steel, L2323.StubTechCoef_iron_steel,
                  L2323.PerCapitaBased_iron_steel, L2323.BaseService_iron_steel,
                  L2323.PriceElasticity_iron_steel,L2323.StubTechCost_iron_steel,
                  L2323.GlobalTechTrackCapital_iron_steel)
  } else {
    stop("Unknown command")
  }
}
