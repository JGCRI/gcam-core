# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2326.aluminum
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for aluminum-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2326.SectorLogitTables[[ curr_table ]]$data}, \code{L2326.Supplysector_aluminum}, \code{L2326.FinalEnergyKeyword_aluminum},
#' \code{L2326.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2326.SubsectorLogit_aluminum}, \code{L2326.SubsectorShrwtFllt_aluminum},
#' \code{L2326.SubsectorInterp_aluminum}, \code{L2326.StubTech_aluminum}, \code{L2326.GlobalTechShrwt_aluminum}, \code{L2326.GlobalTechCoef_aluminum},
#' \code{L2326.GlobalTechCost_aluminum}, \code{L2326.GlobalTechCapture_aluminum}, \code{L2326.StubTechProd_aluminum}, \code{L2326.StubTechCalInput_aluminum},
#' \code{L2326.StubTechCoef_aluminum}, \code{L2326.PerCapitaBased_aluminum}, \code{L2326.BaseService_aluminum}, \code{L2326.PriceElasticity_aluminum},
#' \code{L2326.GlobalTechSecOut_aluminum},
#' \code{object}. The corresponding file in the
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for aluminum sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr gather spread
#' @author Yang Liu Dec 2019
module_energy_L2326.aluminum <- function(command, ...) {

  INCOME_ELASTICITY_OUTPUTS <- c("GCAM3",
                                 paste0("gSSP", 1:5),
                                 paste0("SSP", 1:5))

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A_regions",
             FILE = "energy/A326.sector",
             FILE = "energy/A23.chp_elecratio",
             FILE = "energy/A326.subsector_interp",
             FILE = "energy/A326.subsector_logit",
             FILE = "energy/A326.subsector_shrwt",
             FILE = "energy/A326.globaltech_coef",
             FILE = "energy/A326.globaltech_co2capture",
             FILE = "energy/A326.globaltech_cost",
             FILE = "energy/A326.globaltech_shrwt",
			 FILE = "energy/A326.globaltech_retirement",
             FILE = "energy/A326.demand",
             "L1326.in_EJ_R_aluminum_Yh",
             "L1326.out_Mt_R_aluminum_Yh",
             "L1326.IO_GJkg_R_aluminum_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2326.Supplysector_aluminum",
             "L2326.FinalEnergyKeyword_aluminum",
             "L2326.SubsectorLogit_aluminum",
             "L2326.SubsectorShrwtFllt_aluminum",
             "L2326.SubsectorInterp_aluminum",
             "L2326.StubTech_aluminum",
             "L2326.GlobalTechShrwt_aluminum",
             "L2326.GlobalTechCoef_aluminum",
             "L2326.GlobalTechCost_aluminum",
             "L2326.GlobalTechCapture_aluminum",
			       "L2326.GlobalTechShutdown_aluminum",
             "L2326.GlobalTechSCurve_aluminum",
             "L2326.GlobalTechLifetime_aluminum",
             "L2326.GlobalTechProfitShutdown_aluminum",
             "L2326.StubTechProd_aluminum",
             "L2326.StubTechCalInput_aluminum",
             "L2326.StubTechCoef_aluminum",
             "L2326.PerCapitaBased_aluminum",
             "L2326.BaseService_aluminum",
			       "L2326.PriceElasticity_aluminum",
			       "L2326.GlobalTechSecOut_aluminum"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A_regions <- get_data(all_data, "energy/A_regions")
    A326.sector <- get_data(all_data, "energy/A326.sector", strip_attributes = TRUE)
    A326.subsector_interp <- get_data(all_data, "energy/A326.subsector_interp", strip_attributes = TRUE)
    A326.subsector_logit <- get_data(all_data, "energy/A326.subsector_logit", strip_attributes = TRUE)
    A326.subsector_shrwt <- get_data(all_data, "energy/A326.subsector_shrwt", strip_attributes = TRUE)
    A326.globaltech_coef <- get_data(all_data, "energy/A326.globaltech_coef", strip_attributes = TRUE)
    A326.globaltech_co2capture <- get_data(all_data, "energy/A326.globaltech_co2capture", strip_attributes = TRUE)
	  A326.globaltech_retirement <- get_data(all_data, "energy/A326.globaltech_retirement", strip_attributes = TRUE)
    A326.globaltech_cost <- get_data(all_data, "energy/A326.globaltech_cost", strip_attributes = TRUE)
    A326.globaltech_shrwt <- get_data(all_data, "energy/A326.globaltech_shrwt", strip_attributes = TRUE)
    A326.demand <- get_data(all_data, "energy/A326.demand", strip_attributes = TRUE)
    A23.chp_elecratio  <- get_data(all_data, "energy/A23.chp_elecratio", strip_attributes = TRUE)
    L1326.in_EJ_R_aluminum_Yh <- get_data(all_data, "L1326.in_EJ_R_aluminum_Yh")
    L1326.out_Mt_R_aluminum_Yh <- get_data(all_data, "L1326.out_Mt_R_aluminum_Yh")
    L1326.IO_GJkg_R_aluminum_F_Yh<- get_data(all_data, "L1326.IO_GJkg_R_aluminum_F_Yh")
    # ===================================================
    # 0. Give binding for variable names used in pipeline
    has_district_heat <- year <- value <- GCAM_region_ID <- sector <- fuel <- year.fillout <- to.value <-
      technology <- supplysector <- subsector <- minicam.energy.input <- coefficient <-
      remove.fraction <- minicam.non.energy.input <- input.cost  <- calibration <- calOutputValue <- subs.share.weight <- region <-
      calibrated.value <- . <- scenario <- temp_lag <- base.service <- energy.final.demand <- output.ratio <-
      value.x <- value.y <- parameter <- secondary.output <- elec_ratio <- year.x <- year.y <- output.ratio.x <-
      output.ratio.y <- sector.name <- subsector.name <- stub.technology <- market.name <- terminal_coef <- NULL

    # ===================================================
    # 1. Perform computations
    # Create tables to delete technologies and subsectors in regions where heat is not modeled as a fuel
    has_not_heat <- filter(A_regions, has_district_heat == 0) # intermediate tibble

    calibrated_techs %>%
      filter(sector %in% c("Alumina") & fuel == "heat") %>%
      select(supplysector, subsector, technology) %>%
      repeat_add_columns(tibble(GCAM_region_ID = has_not_heat[["GCAM_region_ID"]])) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L2326.rm_heat_techs_R # intermediate tibble

    # 1a. Supplysector information
    # L2326.Supplysector_aluminum: Supply sector information for aluminum sector
    A326.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L2326.Supplysector_aluminum

    # L2326.FinalEnergyKeyword_aluminum: Supply sector keywords for aluminum sector
    A326.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]], GCAM_region_names) %>%
      na.omit ->
      L2326.FinalEnergyKeyword_aluminum

    # 1b. Subsector information
    # L2326.SubsectorLogit_aluminum: Subsector logit exponents of aluminum sector
    A326.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) %>%
      anti_join(L2326.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2326.SubsectorLogit_aluminum

    # and L2326.SubsectorShrwtFllt_aluminum: Subsector shareweights of aluminum sector
    A326.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) %>%
      anti_join(L2326.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2326.SubsectorShrwtFllt_aluminum

    # L2326.SubsectorInterp_aluminum: Subsector shareweight interpolation of aluminum sector
    A326.subsector_interp %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names) %>%
      anti_join(L2326.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2326.SubsectorInterp_aluminum

    # 1c. Technology information
    # L2326.StubTech_aluminum: Identification of stub technologies of aluminum
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A326.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      anti_join(L2326.rm_heat_techs_R, by = c("region", "subsector")) %>% # Remove non-existent heat subsectors from each region
      rename(stub.technology = technology) ->
      L2326.StubTech_aluminum

    # L2326.GlobalTechShrwt_aluminum: Shareweights of global aluminum technologies
    A326.globaltech_shrwt %>%
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
      L2326.GlobalTechShrwt_aluminum

    # L2326.GlobalTechCoef_aluminum: Energy inputs and efficiency of global aluminum energy use and feedstocks technologies
    A326.globaltech_coef %>%
      gather_years(value_col = "coefficient") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input, secondary.output),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, secondary.output, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input, secondary.output) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 1),
             coefficient = round(coefficient, energy.DIGITS_EFFICIENCY)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) ->
      L2326.globaltech_coef.long # intermediate tibble

    L2326.globaltech_coef.long %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L2326.GlobalTechCoef_aluminum


    # Secondary outputs of cogen technologies: these are input as a ratio
    # L2326.GlobalTechSecOut_ind: Secondary output ratios of aluminum cogeneration technologies
    A326.globaltech_coef %>%
      gather_years(value_col = "coefficient") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input, secondary.output),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, secondary.output, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input, secondary.output) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 1),
             coefficient = round(coefficient, energy.DIGITS_EFFICIENCY)) %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      filter(!is.na(secondary.output)) %>%
      left_join_error_no_match(A23.chp_elecratio, by = c("subsector" = "fuel")) %>%
      mutate(output.ratio = elec_ratio * coefficient,
             output.ratio = round(output.ratio, energy.DIGITS_EFFICIENCY)) %>%
      # NOTE: holding the output ratio constant over time in future periods
      left_join_error_no_match(select(filter(., year == max(MODEL_BASE_YEARS)), -coefficient, -elec_ratio),
                               by = c("supplysector", "subsector", "technology", "minicam.energy.input", "secondary.output")) %>%
      mutate(output.ratio = if_else(year.x %in% MODEL_BASE_YEARS, output.ratio.x, output.ratio.y)) %>%
      ungroup %>%
      rename(year = year.x,
             sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechSecOut"]]) ->
      L2326.GlobalTechSecOut_aluminum

    # Carbon capture rates from technologies with CCS
    # L2326.GlobalTechCapture_aluminum: CO2 capture fractions from global aluminum production technologies with CCS
    # No need to consider historical periods or intermittent technologies here
    A326.globaltech_co2capture %>%
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
      L2326.GlobalTechCapture_aluminum

    # Retirement information
    A326.globaltech_retirement %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L2326.globaltech_retirement_base

    # Copies first future year retirment information into all future years and appends back onto base year
    L2326.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) ->
      L2326.globaltech_retirement_future

    # filters base years from original and then appends future years
    L2326.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      bind_rows(L2326.globaltech_retirement_future) ->
      L2326.globaltech_retirement

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # This section checks L2326.globaltech_retirement for each of these functions and creates a separate level 2 file for each
    # All of these options have different headers, and all are allowed
    if(any(!is.na(L2326.globaltech_retirement$shutdown.rate))) {
      L2326.globaltech_retirement %>%
        filter(!is.na(L2326.globaltech_retirement$shutdown.rate)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "shutdown.rate") ->
        L2326.GlobalTechShutdown_aluminum
    }

    if(any(!is.na(L2326.globaltech_retirement$half.life))) {
      L2326.globaltech_retirement %>%
        filter(!is.na(L2326.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
        L2326.GlobalTechSCurve_aluminum
    }

    # L2326.GlobalTechLifetime_aluminum: Global tech lifetime
    if(any(is.na(L2326.globaltech_retirement$shutdown.rate) & is.na(L2326.globaltech_retirement$half.life))) {
      L2326.globaltech_retirement %>%
        filter(is.na(L2326.globaltech_retirement$shutdown.rate) & is.na(L2326.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime") ->
        L2326.GlobalTechLifetime_aluminum
    }

    # L2326.GlobalTechProfitShutdown_aluminum: Global tech profit shutdown decider and parameters
    if(any(!is.na(L2326.globaltech_retirement$median.shutdown.point))) {
      L2326.globaltech_retirement %>%
        filter(!is.na(L2326.globaltech_retirement$median.shutdown.point)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
        L2326.GlobalTechProfitShutdown_aluminum
    }

    # L2326.GlobalTechCost_aluminum: Non-energy costs of global aluminum manufacturing technologies
    A326.globaltech_cost %>%
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
      L2326.GlobalTechCost_aluminum # intermediate tibble

    # L2326.StubTechCalInput_aluminum: calibrated input of aluminum energy use technologies (including cogen)
    L2326.GlobalTechCoef_aluminum %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["StubTechCoef"]]), GCAM_region_names) %>%
      mutate(market.name =NULL,coefficient = NULL) %>%
      anti_join(L2326.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2326.aluminum_tmp

    # L2326.StubTechCalInput_aluminum: calibrated aluminum input
     calibrated_techs %>%
       filter(calibration == "input") %>% # Only take the tech IDs where the calibration is identified as input
       select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
       distinct ->
       calibrated_techs_export # temporary tibble

    L1326.in_EJ_R_aluminum_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      complete(nesting(fuel,year,sector),GCAM_region_ID = GCAM_region_names$GCAM_region_ID) %>%
      filter(fuel != 'heat' | (fuel == 'heat' & value>0) ) %>%
      filter(fuel != 'electricity') %>%
      mutate(value = replace_na(value,0)) %>%
      left_join(GCAM_region_names,by = c("GCAM_region_ID")) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      left_join(calibrated_techs_export, by = c("fuel", "sector")) %>%
      mutate(stub.technology = technology,
      technology = NULL)   ->
      L2326.StubTechCalInput_aluminum_tmp

    L2326.aluminum_tmp %>%
      filter(supplysector != "aluminum") %>%
      left_join(L2326.StubTechCalInput_aluminum_tmp,
                by = c("region", "supplysector", "subsector", "stub.technology", "year", "minicam.energy.input")) %>%
      mutate(fuel = NULL,sector = NULL, value = NULL,GCAM_region_ID  = NULL,calibrated.value = replace_na(calibrated.value,0),
             share.weight.year = year) %>%
      rename(calOutputValue = calibrated.value) %>%  # temporary column name change to accommodate function set_subsector_shrwt
      set_subsector_shrwt %>%
      rename(calibrated.value = calOutputValue) %>% # temporary column name changeto accommodate function set_subsector_shrwt
      mutate(tech.share.weight = if_else(calibrated.value>0 , 1, 0)) %>%
      anti_join(L2326.rm_heat_techs_R, by = c("region", "subsector")) %>% # Remove non-existent heat subsectors from each region
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) ->
      L2326.StubTechCalInput_aluminum

    # L2326.StubTechCoef_aluminum
    calibrated_techs %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      distinct ->
      calibrated_techs_export # temporary tibble

    #Add coefficients for aluminum and alumina
    L1326.out_Mt_R_aluminum_Yh %>%
      filter(sector == "Alumina") %>%
      mutate(sector = "Aluminum") %>%
      left_join(L1326.out_Mt_R_aluminum_Yh %>%
                                 filter(sector == "Aluminum"), by = c("GCAM_region_ID", "year", "sector")) %>%
      mutate(value = replace_na(value.x/value.y,0), value.x = NULL,value.y = NULL,fuel = "alumina") ->
      L1326.IO_GJkg_R_aluminum_F_Yh_alumina


    L1326.IO_GJkg_R_aluminum_F_Yh %>%
      bind_rows(L1326.IO_GJkg_R_aluminum_F_Yh_alumina) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      complete(nesting(fuel,year,sector),GCAM_region_ID = GCAM_region_names$GCAM_region_ID) %>%
      filter(fuel != 'heat' | (fuel == 'heat' & value>0) ) %>%
      mutate(value = replace_na(value,0)) %>%
      left_join(GCAM_region_names,by = c("GCAM_region_ID")) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      left_join(calibrated_techs_export, by = c("fuel", "sector")) %>%
      anti_join(L2326.rm_heat_techs_R, by = c("region", "subsector")) %>% # Remove non-existent heat subsectors from each region
      mutate(coefficient = round(value, energy.DIGITS_COEFFICIENT),
             stub.technology = technology,
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L2326.StubTechCoef_aluminum_tmp

    L2326.aluminum_tmp %>%
      left_join(L2326.StubTechCoef_aluminum_tmp,
                by = c("region", "supplysector", "subsector", "stub.technology", "year", "minicam.energy.input")) %>%
      mutate(GCAM_region_ID  = NULL,coefficient = replace_na(coefficient,0),
             share.weight.year = year,market.name = region) %>%
      anti_join(L2326.rm_heat_techs_R, by = c("region", "subsector")) %>% # Remove non-existent heat subsectors from each region
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L2326.StubTechCoef_aluminum

    # Instead, interpolate the coefficients to these global default values in a specified period
    L2326.StubTechCoef_aluminum %>%
      complete(nesting(region, supplysector, subsector, stub.technology, minicam.energy.input, market.name),
               year = unique(c(MODEL_YEARS, energy.INDCOEF_CONVERGENCE_YR))) %>%
      left_join(select(L2326.GlobalTechCoef_aluminum %>% rename(terminal_coef = coefficient,supplysector = sector.name,subsector = subsector.name),
                       supplysector, subsector, technology, minicam.energy.input, terminal_coef, year),
                by = c("supplysector", "subsector", stub.technology = "technology", "minicam.energy.input","year")) %>%
      mutate(coefficient = if_else(year == 2010 & is.na(coefficient), terminal_coef, coefficient),
             coefficient = if_else(year == 2015 & coefficient ==0, terminal_coef, coefficient)) %>%
      select(-terminal_coef) %>%
      group_by(region, supplysector, subsector, stub.technology, minicam.energy.input) %>%
      mutate(coefficient = round(approx_fun(year, coefficient,rule = 2), energy.DIGITS_COEFFICIENT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) ->   # drop the terminal coef year if it's outside of the model years
      L2326.StubTechCoef_aluminum


    # Calibration and region-specific data
    # L2326.StubTechProd_aluminum: calibrated aluminum output
    calibrated_techs %>%
     filter(calibration == "output") %>% # Only take the tech IDs where the calibration is identified as output
      select(fuel, sector, supplysector, subsector, technology,minicam.energy.input) %>%
      distinct ->
      calibrated_techs_export # temporary tibble

    L1326.out_Mt_R_aluminum_Yh %>%
      mutate(fuel = "electricity") %>%
      complete(nesting(fuel,region,sector),year = MODEL_BASE_YEARS) %>%
      complete(nesting(fuel,year,sector),GCAM_region_ID = GCAM_region_names$GCAM_region_ID) %>%
      mutate(calOutputValue = replace_na(value,0),value = NULL) %>%
      left_join(GCAM_region_names,by = c("GCAM_region_ID")) %>%
      left_join(calibrated_techs_export, by = c("fuel", "sector"))  %>%
      filter(!is.na(supplysector)) %>%
      mutate(sector = NULL, fuel = NULL,stub.technology = technology, technology = NULL,minicam.energy.input = NULL) ->
      L2326.StubTechProd_aluminum_tmp

    L2326.GlobalTechShrwt_aluminum %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["StubTechShrwt"]]), GCAM_region_names) %>%
      mutate(market.name =NULL,coefficient = NULL) %>%
      anti_join(L2326.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2326.aluminum_out_tmp

    L2326.aluminum_out_tmp %>%
      filter(supplysector == "aluminum") %>%
      left_join(L2326.StubTechProd_aluminum_tmp,
                by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
      mutate(GCAM_region_ID  = NULL,calOutputValue = replace_na(calOutputValue,0),
             share.weight.year = year) %>%
      set_subsector_shrwt %>%
      mutate(tech.share.weight = if_else(subs.share.weight > 0, 1, 0)) %>%
      anti_join(L2326.rm_heat_techs_R, by = c("region", "subsector")) %>% # Remove non-existent heat subsectors from each region
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2326.StubTechProd_aluminum


    # L2326.PerCapitaBased_aluminum: per-capita based flag for aluminum exports final demand
    A326.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PerCapitaBased"]], GCAM_region_names)  ->
      L2326.PerCapitaBased_aluminum

    # L2326.BaseService_aluminum: base-year service output of aluminum
    L2326.StubTechProd_aluminum %>%
      select(region, year, base.service = calOutputValue) %>%
      mutate(energy.final.demand = A326.demand[["energy.final.demand"]]) %>%
      group_by(region,year,energy.final.demand) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup() ->
      L2326.BaseService_aluminum

    # L2326.PriceElasticity_aluminum: price elasticity
    A326.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PriceElasticity"]][LEVEL2_DATA_NAMES[["PriceElasticity"]] != "year"], GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["PriceElasticity"]]) ->
      L2326.PriceElasticity_aluminum

    # ===================================================
    # Produce outputs
    L2326.Supplysector_aluminum %>%
      add_title("Supply sector information for aluminum sector") %>%
      add_units("NA") %>%
      add_comments("For aluminum sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A326.sector is expended into all GCAM regions") %>%
      add_legacy_name("L2326.Supplysector_aluminum") %>%
      add_precursors("energy/A326.sector", "common/GCAM_region_names") ->
      L2326.Supplysector_aluminum

    L2326.FinalEnergyKeyword_aluminum %>%
      add_title("Supply sector keywords for aluminum sector") %>%
      add_units("NA") %>%
      add_comments("For aluminum sector, the supply sector final energy keywords from A326.sector are expended into all GCAM regions") %>%
      add_legacy_name("L2326.FinalEnergyKeyword_aluminum") %>%
      add_precursors("energy/A326.sector", "common/GCAM_region_names") ->
      L2326.FinalEnergyKeyword_aluminum

    L2326.SubsectorLogit_aluminum %>%
      add_title("Subsector logit exponents of aluminum sector") %>%
      add_units("Unitless") %>%
      add_comments("For aluminum sector, the subsector logit exponents from A326.subsector_logit are expanded into all GCAM regions") %>%
      add_legacy_name("L2326.SubsectorLogit_aluminum") %>%
      add_precursors("energy/A326.subsector_logit", "energy/A_regions","common/GCAM_region_names") ->
      L2326.SubsectorLogit_aluminum

    L2326.SubsectorShrwtFllt_aluminum %>%
      add_title("Subsector shareweights of aluminum sector") %>%
      add_units("unitless") %>%
      add_comments("For aluminum sector, the subsector shareweights from A326.subsector_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2326.SubsectorShrwtFllt_aluminum") %>%
      add_precursors("energy/A326.subsector_shrwt", "energy/A_regions","common/GCAM_region_names") ->
      L2326.SubsectorShrwtFllt_aluminum

    L2326.SubsectorInterp_aluminum %>%
      add_title("Subsector shareweight interpolation of aluminum sector") %>%
      add_units("NA") %>%
      add_comments("For aluminum sector, the subsector shareweight interpolation function infromation from A326.subsector_interp is expanded into all GCAM regions") %>%
      add_legacy_name("L2326.SubsectorInterp_aluminum") %>%
      add_precursors("energy/A326.subsector_interp", "energy/A_regions","common/GCAM_region_names") ->
      L2326.SubsectorInterp_aluminum

    L2326.StubTech_aluminum %>%
      add_title("Identification of stub technologies of aluminum") %>%
      add_units("NA") %>%
      add_comments("For aluminum sector, the stub technologies from A326.globaltech_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2326.StubTech_aluminum") %>%
      add_precursors("energy/A326.globaltech_shrwt","energy/A_regions", "common/GCAM_region_names") ->
      L2326.StubTech_aluminum

    L2326.GlobalTechShrwt_aluminum %>%
      add_title("Shareweights of global aluminum technologies") %>%
      add_units("Unitless") %>%
      add_comments("For aluminum sector, the share weights from A326.globaltech_shrwt are interpolated into all base years and future years") %>%
      add_legacy_name("L2326.GlobalTechShrwt_aluminum") %>%
      add_precursors("energy/A326.globaltech_shrwt") ->
      L2326.GlobalTechShrwt_aluminum

    L2326.GlobalTechCoef_aluminum %>%
      add_title("Energy inputs and coefficients of aluminum technologies") %>%
      add_units("Unitless") %>%
      add_comments("For aluminum sector, the energy use coefficients from A326.globaltech_coef are interpolated into all model years") %>%
      add_legacy_name("L2326.GlobalTechCoef_aluminum") %>%
      add_precursors("energy/A326.globaltech_coef") ->
      L2326.GlobalTechCoef_aluminum

    L2326.GlobalTechCost_aluminum %>%
      add_title("Non-energy costs of global aluminum manufacturing technologies") %>%
      add_units("1975$/kg for supplysector aluminum; 1975$/GJ for supplysector process heat aluminum") %>%
      add_comments("For aluminum sector, the Non-energy costs of global aluminum manufacturing technologies are calculated then adjusted with CCS to include CO2 capture costs") %>%
      add_legacy_name("L2326.GlobalTechCost_aluminum") %>%
      add_precursors("energy/A326.globaltech_cost", "energy/A326.globaltech_coef") ->
      L2326.GlobalTechCost_aluminum


    if(exists("L2326.GlobalTechShutdown_aluminum")) {
      L2326.GlobalTechShutdown_aluminum %>%
        add_title("Global tech lifetime for techs with shutdown rate") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that uses a phased retirement function") %>%
        add_legacy_name("L2326.GlobalTechShutdown_aluminum") %>%
        add_precursors("energy/A326.globaltech_retirement") ->
        L2326.GlobalTechShutdown_aluminum
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2326.GlobalTechShutdown_aluminum") ->
        L2326.GlobalTechShutdown_aluminum
    }

    if(exists("L2326.GlobalTechSCurve_aluminum")) {
      L2326.GlobalTechSCurve_aluminum %>%
        add_title("Global tech lifetime for techs with s-curve retirement function") %>%
        add_units("Lifetime in years, half-life in years") %>%
        add_comments("Filters for any technology that uses an S-curve retirement function") %>%
        add_legacy_name("L2326.GlobalTechSCurve_aluminum") %>%
        add_precursors("energy/A326.globaltech_retirement") ->
        L2326.GlobalTechSCurve_aluminum
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2326.GlobalTechSCurve_aluminum") ->
        L2326.GlobalTechSCurve_aluminum
    }

    if(exists("L2326.GlobalTechLifetime_aluminum")) {
      L2326.GlobalTechLifetime_aluminum %>%
        add_title("Global tech lifetime for any technology with no retirement function") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that has no phased or S-curve retirement function, empty by default.") %>%
        add_legacy_name("L2326.GlobalTechLifetime_aluminum") %>%
        add_precursors("energy/A326.globaltech_retirement") ->
        L2326.GlobalTechLifetime_aluminum
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2326.GlobalTechLifetime_aluminum") ->
        L2326.GlobalTechLifetime_aluminum
    }

    if(exists("L2326.GlobalTechProfitShutdown_aluminum")) {
      L2326.GlobalTechProfitShutdown_aluminum %>%
        add_title("Global tech profit shutdown decider and parameters") %>%
        add_units("Unitless, used to determine shape of the function defining the relationship between shutdown rate and profitability") %>%
        add_comments("Filters for any technologies that use a profit-based shutdown parameter") %>%
        add_legacy_name("L2326.GlobalTechProfitShutdown_aluminum") %>%
        add_precursors("energy/A326.globaltech_retirement") ->
        L2326.GlobalTechProfitShutdown_aluminum
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2326.GlobalTechProfitShutdown_aluminum") ->
        L2326.GlobalTechProfitShutdown_aluminum
    }

    L2326.GlobalTechCapture_aluminum %>%
      add_title("CO2 capture fractions from global IRONSTL production technologies with CCS") %>%
      add_units("Unitless") %>%
      add_comments("For aluminum sector, the remove fractions from A326.globaltech_co2capture are interpolated into all model years") %>%
      add_legacy_name("L2326.GlobalTechCapture_aluminum") %>%
      add_precursors("energy/A326.globaltech_co2capture") ->
      L2326.GlobalTechCapture_aluminum


    L2326.StubTechProd_aluminum %>%
      add_title("calibrated aluminum production") %>%
      add_units("EJ") %>%
      add_comments("Values are calculated using L1326.in_EJ_R_aluminum_Yh, then added GCAM region information and supplysector, subsector, and technology information") %>%
      add_legacy_name("L2326.StubTechProd_aluminum") %>%
      add_precursors("energy/calibrated_techs",  "common/GCAM_region_names") ->
      L2326.StubTechProd_aluminum

    L2326.StubTechCalInput_aluminum %>%
       add_title("calibrated aluminum production") %>%
       add_units("EJ") %>%
       add_comments("Values are calculated using L1326.in_EJ_R_aluminum_Yh then added GCAM region information and supplysector, subsector, technology, and input information") %>%
       add_legacy_name("L2326.StubTechCalInput_aluminum") %>%
       add_precursors("energy/calibrated_techs", "L1326.in_EJ_R_aluminum_Yh", "common/GCAM_region_names") ->
       L2326.StubTechCalInput_aluminum

    L2326.StubTechCoef_aluminum %>%
      add_title("region-specific coefficients of aluminum production technologies") %>%
      add_units("unitless") %>%
      add_comments("Coefficients from literature") %>%
      add_legacy_name("L2326.StubTechCoef_aluminum") %>%
      add_precursors("energy/calibrated_techs", "common/GCAM_region_names","energy/A326.globaltech_coef", "L1326.out_Mt_R_aluminum_Yh", "L1326.IO_GJkg_R_aluminum_F_Yh") ->
      L2326.StubTechCoef_aluminum

      L2326.PerCapitaBased_aluminum %>%
      add_title("per-capita based flag for aluminum exports final demand") %>%
      add_units("NA") %>%
      add_comments("Per-capita based flags for aluminum from A326.demand are expanded into all GCAM regions") %>%
      add_legacy_name("L2326.PerCapitaBased_aluminum") %>%
      add_precursors("energy/A326.demand", "common/GCAM_region_names") ->
      L2326.PerCapitaBased_aluminum

    L2326.BaseService_aluminum %>%
      add_title("base-year service output of aluminum") %>%
      add_units("EJ") %>%
      add_comments("Transformed from L2326.StubTechProd_aluminum by adding energy.final.demand") %>%
      add_legacy_name("L2326.BaseService_aluminum") %>%
      add_precursors("energy/A326.demand","L1326.in_EJ_R_aluminum_Yh", "energy/calibrated_techs", "common/GCAM_region_names") ->
      L2326.BaseService_aluminum

    L2326.PriceElasticity_aluminum %>%
      add_title("price elasticity for aluminum") %>%
      add_units("Unitless") %>%
      add_comments("The elasticity values from A326.demand are expanded into all GCAM_regions") %>%
      add_legacy_name("L2326.PriceElasticity_aluminum") %>%
      add_precursors("energy/A326.demand", "common/GCAM_region_names") ->
      L2326.PriceElasticity_aluminum

    L2326.GlobalTechSecOut_aluminum %>%
      add_title("Secondary output ratios of aluminum cogeneration technologies") %>%
      add_units("Unitless") %>%
      add_comments("Secondary output ratios are calculated as electricity ratio (Assumed CHP electricity output per unit fuel input) over efficiency") %>%
      add_legacy_name("L2326.GlobalTechSecOut_aluminum") %>%
      add_precursors("energy/A23.chp_elecratio", "energy/A326.globaltech_coef") ->
      L2326.GlobalTechSecOut_aluminum

      return_data(L2326.Supplysector_aluminum, L2326.FinalEnergyKeyword_aluminum, L2326.SubsectorLogit_aluminum,
                  L2326.SubsectorShrwtFllt_aluminum, L2326.SubsectorInterp_aluminum,
                  L2326.StubTech_aluminum, L2326.GlobalTechShrwt_aluminum,
                  L2326.GlobalTechCoef_aluminum, L2326.GlobalTechCost_aluminum,
                  L2326.StubTechCalInput_aluminum,L2326.StubTechCoef_aluminum,L2326.GlobalTechShutdown_aluminum,
                  L2326.GlobalTechSCurve_aluminum, L2326.GlobalTechLifetime_aluminum, L2326.GlobalTechProfitShutdown_aluminum,
                  L2326.PerCapitaBased_aluminum, L2326.BaseService_aluminum,L2326.StubTechProd_aluminum,
                  L2326.PriceElasticity_aluminum,L2326.GlobalTechCapture_aluminum,L2326.GlobalTechSecOut_aluminum)


  } else {
    stop("Unknown command")
  }
}
