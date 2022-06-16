# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2325.chemical
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for chemical-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2325.SectorLogitTables[[ curr_table ]]$data}, \code{L2325.Supplysector_chemical}, \code{L2325.FinalEnergyKeyword_chemical},
#' \code{L2325.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2325.SubsectorLogit_chemical}, \code{L2325.SubsectorShrwtFllt_chemical},
#' \code{L2325.SubsectorInterp_chemical}, \code{L2325.StubTech_chemical}, \code{L2325.GlobalTechShrwt_chemical}, \code{L2325.GlobalTechCoef_chemical},
#' \code{L2325.GlobalTechCost_chemical}, \code{L2325.GlobalTechCapture_chemical}, \code{L2325.StubTechProd_chemical}, \code{L2325.StubTechCalInput_chemical},
#' \code{L2325.StubTechCoef_chemical}, \code{L2325.PerCapitaBased_chemical}, \code{L2325.BaseService_chemical}, \code{L2325.PriceElasticity_chemical},
#' \code{L2325.GlobalTechCapture_chemical}, \code{L2325.GlobalTechEff_chemical},\code{L2325.GlobalTechCSeq_ind},
#' \code{object}. The corresponding file in the
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for chemical sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr gather spread
#' @author Yang Liu Dec 2019
module_energy_L2325.chemical <- function(command, ...) {

  INCOME_ELASTICITY_OUTPUTS <- c("GCAM3",
                                 paste0("gSSP", 1:5),
                                 paste0("SSP", 1:5))

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A_regions",
             FILE = "energy/A325.sector",
             FILE = "energy/A23.chp_elecratio",
             FILE = "energy/A325.subsector_interp",
             FILE = "energy/A325.nonenergy_Cseq",
             FILE = "energy/A325.subsector_logit",
             FILE = "energy/A325.subsector_shrwt",
             FILE = "energy/A325.globaltech_coef",
             FILE = "energy/A325.globaltech_eff",
             FILE = "energy/A325.globaltech_co2capture",
             FILE = "energy/A325.globaltech_cost",
             FILE = "energy/A325.globaltech_retirement",
             FILE = "energy/A325.globaltech_shrwt",
             FILE = "energy/A325.demand",
             "L1325.in_EJ_R_chemical_F_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2325.Supplysector_chemical",
             "L2325.FinalEnergyKeyword_chemical",
             "L2325.SubsectorLogit_chemical",
             "L2325.SubsectorShrwtFllt_chemical",
             "L2325.SubsectorInterp_chemical",
             "L2325.StubTech_chemical",
             "L2325.GlobalTechShrwt_chemical",
             "L2325.GlobalTechCoef_chemical",
             "L2325.GlobalTechEff_chemical",
             "L2325.GlobalTechCost_chemical",
             "L2325.GlobalTechCapture_chemical",
             "L2325.GlobalTechCSeq_ind",
			 "L2325.GlobalTechShutdown_chemical",
             "L2325.GlobalTechSCurve_chemical",
             "L2325.GlobalTechLifetime_chemical",
             "L2325.GlobalTechProfitShutdown_chemical",
             "L2325.StubTechProd_chemical",
             "L2325.StubTechCalInput_chemical",
             "L2325.StubTechCoef_chemical",
             "L2325.PerCapitaBased_chemical",
             "L2325.BaseService_chemical",
			 "L2325.PriceElasticity_chemical",
			 "L2325.GlobalTechSecOut_chemical"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A_regions <- get_data(all_data, "energy/A_regions")
    A325.sector <- get_data(all_data, "energy/A325.sector", strip_attributes = TRUE)
    A325.subsector_interp <- get_data(all_data, "energy/A325.subsector_interp", strip_attributes = TRUE)
    A325.subsector_logit <- get_data(all_data, "energy/A325.subsector_logit", strip_attributes = TRUE)
    A325.subsector_shrwt <- get_data(all_data, "energy/A325.subsector_shrwt", strip_attributes = TRUE)
    A325.globaltech_coef <- get_data(all_data, "energy/A325.globaltech_coef", strip_attributes = TRUE)
    A325.globaltech_eff <- get_data(all_data, "energy/A325.globaltech_eff", strip_attributes = TRUE)
    A325.globaltech_co2capture <- get_data(all_data, "energy/A325.globaltech_co2capture", strip_attributes = TRUE)
    A325.globaltech_cost <- get_data(all_data, "energy/A325.globaltech_cost", strip_attributes = TRUE)
	  A325.globaltech_retirement <- get_data(all_data, "energy/A325.globaltech_retirement", strip_attributes = TRUE)
	  A325.nonenergy_Cseq <- get_data(all_data, "energy/A325.nonenergy_Cseq", strip_attributes = TRUE)
    A325.globaltech_shrwt <- get_data(all_data, "energy/A325.globaltech_shrwt", strip_attributes = TRUE)
    A325.demand <- get_data(all_data, "energy/A325.demand", strip_attributes = TRUE)
    A23.chp_elecratio  <- get_data(all_data, "energy/A23.chp_elecratio", strip_attributes = TRUE)
    L1325.in_EJ_R_chemical_F_Y <- get_data(all_data, "L1325.in_EJ_R_chemical_F_Y")
    # ===================================================
    # 0. Give binding for variable names used in pipeline
    has_district_heat <- year <- value <- GCAM_region_ID <- sector <- fuel <- year.fillout <- to.value <-
      technology <- supplysector <- subsector <- minicam.energy.input <- coefficient <-
      remove.fraction <- minicam.non.energy.input <- input.cost  <- calibration <- calOutputValue <- subs.share.weight <- region <-
      calibrated.value <- . <- scenario <- temp_lag <- base.service <- energy.final.demand <-
      value.x <- value.y <- parameter <- secondary.output <- efficiency <- elec_ratio <- output.ratio <-
      year.x <- year.y <- output.ratio.x <- output.ratio.y <- sector.name <- subsector.name <-
      calOutputValue.x <- calOutputValue.y <- output_tot <- stub.technology <- market.name <- terminal_coef <-
      share.weight <- interpolation.function <- NULL

    # ===================================================
    # 1. Perform computations
    # Create tables to delete technologies and subsectors in regions where heat is not modeled as a fuel
    has_not_heat <- filter(A_regions, has_district_heat == 0) # intermediate tibble

    calibrated_techs %>%
      filter(grepl("chemical energy", sector) & fuel == "heat") %>%
      select(supplysector, subsector, technology) %>%
      repeat_add_columns(tibble(GCAM_region_ID = has_not_heat[["GCAM_region_ID"]])) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L2325.rm_heat_techs_R # intermediate tibble

    # 1a. Supplysector information
    # L2325.Supplysector_chemical: Supply sector information for chemical sector
    A325.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L2325.Supplysector_chemical

    # L2325.FinalEnergyKeyword_chemical: Supply sector keywords for chemical sector
    A325.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]], GCAM_region_names) %>%
      na.omit ->
      L2325.FinalEnergyKeyword_chemical

    # 1b. Subsector information
    # L2325.SubsectorLogit_chemical: Subsector logit exponents of chemical sector
    A325.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) %>%
      anti_join(L2325.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2325.SubsectorLogit_chemical

    # and L2325.SubsectorShrwtFllt_chemical: Subsector shareweights of chemical sector
    A325.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) %>%
      anti_join(L2325.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2325.SubsectorShrwtFllt_chemical

    # L2325.SubsectorInterp_chemical: Subsector shareweight interpolation of chemical sector
    A325.subsector_interp %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names) %>%
      anti_join(L2325.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2325.SubsectorInterp_chemical

    # 1c. Technology information
    # L2325.StubTech_chemical: Identification of stub technologies of chemical
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A325.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      anti_join(L2325.rm_heat_techs_R, by = c("region", "subsector")) %>% # Remove non-existent heat subsectors from each region
      rename(stub.technology = technology) ->
      L2325.StubTech_chemical

    # L2325.GlobalTechShrwt_chemical: Shareweights of global chemical technologies
    A325.globaltech_shrwt %>%
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
      L2325.GlobalTechShrwt_chemical

    # L2325.GlobalTechEff_ind: Energy inputs and efficiency of global chemical energy use and feedstocks technologies
    A325.globaltech_eff %>%
      gather_years(value_col = "efficiency") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input, secondary.output),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, secondary.output, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input, secondary.output) %>%
      mutate(efficiency = approx_fun(year, efficiency, rule = 1),
             efficiency = round(efficiency, energy.DIGITS_EFFICIENCY)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector,
             subsector.name = subsector) ->
      L2325.globaltech_eff.long # intermediate tibble

    L2325.globaltech_eff.long %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechEff"]]) ->
      L2325.GlobalTechEff_chemical

    # Coefficients on global chemical sector technologies (not energy-use or feedstocks)
    # L2325.GlobalTechCoef_ind: Energy inputs and coefficients of global chemical technologies
    A325.globaltech_coef %>%
      rename(coefficient = "terminal_coef") %>%
      repeat_add_columns(tibble(year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS))) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>% # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L2325.GlobalTechCoef_chemical

    # Secondary outputs of cogen technologies: these are input as a ratio
    # L2325.GlobalTechSecOut_ind: Secondary output ratios of chemical cogeneration technologies
    A325.globaltech_eff %>%
      gather_years(value_col = "efficiency") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input, secondary.output),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, secondary.output, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input, secondary.output) %>%
      mutate(efficiency = approx_fun(year, efficiency, rule = 1),
             efficiency = round(efficiency, energy.DIGITS_EFFICIENCY)) %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      filter(!is.na(secondary.output)) %>%
      left_join_error_no_match(A23.chp_elecratio, by = c("subsector" = "fuel")) %>%
      mutate(output.ratio = elec_ratio / efficiency,
             output.ratio = round(output.ratio, energy.DIGITS_EFFICIENCY)) %>%
      # NOTE: holding the output ratio constant over time in future periods
      left_join_error_no_match(select(filter(., year == max(MODEL_BASE_YEARS)), -efficiency, -elec_ratio),
                               by = c("supplysector", "subsector", "technology", "minicam.energy.input", "secondary.output")) %>%
      mutate(output.ratio = if_else(year.x %in% MODEL_BASE_YEARS, output.ratio.x, output.ratio.y)) %>%
      ungroup %>%
      rename(year = year.x,
             sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechSecOut"]]) ->
      L2325.GlobalTechSecOut_chemical

    # Carbon capture rates from technologies with CCS
    # L2325.GlobalTechCapture_chemical: CO2 capture fractions from global chemical production technologies with CCS
    # No need to consider historical periods or intermittent technologies here
    A325.globaltech_co2capture %>%
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
      L2325.GlobalTechCapture_chemical

    # L2325.GlobalTechCost_chemical: Non-energy costs of global chemical manufacturing technologies
    A325.globaltech_cost %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.non.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, value, rule = 2),
             input.cost = round(input.cost, energy.DIGITS_COST)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]]) ->
      L2325.GlobalTechCost_chemical # intermediate tibble

    A325.nonenergy_Cseq %>%
      repeat_add_columns(tibble(year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS))) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCSeq"]]) ->
      L2325.GlobalTechCSeq_ind

    # Retirement information
    A325.globaltech_retirement %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L2325.globaltech_retirement_base

    # Copies first future year retirment information into all future years and appends back onto base year
    L2325.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) ->
      L2325.globaltech_retirement_future

    # filters base years from original and then appends future years
    L2325.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      bind_rows(L2325.globaltech_retirement_future) ->
      L2325.globaltech_retirement

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # This section checks L2325.globaltech_retirement for each of these functions and creates a separate level 2 file for each
    # All of these options have different headers, and all are allowed
    if(any(!is.na(L2325.globaltech_retirement$shutdown.rate))) {
      L2325.globaltech_retirement %>%
        filter(!is.na(L2325.globaltech_retirement$shutdown.rate)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "shutdown.rate") ->
        L2325.GlobalTechShutdown_chemical
    }

    if(any(!is.na(L2325.globaltech_retirement$half.life))) {
      L2325.globaltech_retirement %>%
        filter(!is.na(L2325.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
        L2325.GlobalTechSCurve_chemical
    }

    # L2325.GlobalTechLifetime_chemical: Global tech lifetime
    if(any(is.na(L2325.globaltech_retirement$shutdown.rate) & is.na(L2325.globaltech_retirement$half.life))) {
      L2325.globaltech_retirement %>%
        filter(is.na(L2325.globaltech_retirement$shutdown.rate) & is.na(L2325.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime") ->
        L2325.GlobalTechLifetime_chemical
    }

    # L2325.GlobalTechProfitShutdown_chemical: Global tech profit shutdown decider and parameters
    if(any(!is.na(L2325.globaltech_retirement$median.shutdown.point))) {
      L2325.globaltech_retirement %>%
        filter(!is.na(L2325.globaltech_retirement$median.shutdown.point)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
        L2325.GlobalTechProfitShutdown_chemical
    }


    # Calibration and region-specific data
    # L2325.StubTechCalInput_indenergy: calibrated input of industrial energy use technologies (including cogen)
    L2325.GlobalTechEff_chemical %>%
      rename(supplysector = sector.name,
           subsector = subsector.name,
           stub.technology = technology,
           coefficient = efficiency) %>%
      mutate(efficiency = NULL) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["StubTechCoef"]]), GCAM_region_names) %>%
      mutate(market.name =NULL,coefficient = NULL) %>%
      anti_join(L2325.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2325.chemical_tmp

    L1325.in_EJ_R_chemical_F_Y %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      complete(nesting(fuel,year,sector),region = GCAM_region_names$region) %>%
      mutate(GCAM_region_ID = NULL,value = replace_na(value,0)) %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      left_join(select(calibrated_techs, sector, fuel, supplysector, subsector, technology), by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) ->
      L2325.in_EJ_R_chemical_F_Yh # intermediate tibble

    L2325.in_EJ_R_chemical_F_Yh %>%
      left_join_error_no_match(distinct(select(A325.globaltech_eff, subsector, technology, minicam.energy.input)),
                               by = c("subsector", "stub.technology" = "technology")) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year) ->
      L2325.StubTechCalInput_chemical_tmp

    L2325.chemical_tmp %>%
      left_join(L2325.StubTechCalInput_chemical_tmp,
                by = c("region", "supplysector", "subsector", "stub.technology", "year", "minicam.energy.input")) %>%
      mutate(fuel = NULL,sector = NULL, value = NULL,GCAM_region_ID  = NULL,calibrated.value = replace_na(calibrated.value,0),
             share.weight.year = year) %>%
      rename(calOutputValue = calibrated.value) %>%  # temporary column name change to accommodate function set_subsector_shrwt
      set_subsector_shrwt %>%
      rename(calibrated.value = calOutputValue) %>% # temporary column name change to accommodate function set_subsector_shrwt
      mutate(tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) ->
      L2325.StubTechCalInput_chemical


    # L2325.StubTechProd_chemical: calibrated output of chemical sector
    # First, calculate service output by technology, for energy-use and feedstocks
    L2325.in_EJ_R_chemical_F_Yh %>%
      left_join_error_no_match(select(L2325.globaltech_eff.long, sector.name, subsector.name, technology, year,efficiency),
                               by = c("supplysector" = "sector.name", "subsector" = "subsector.name",
                                      "stub.technology" = "technology", "year")) %>%
      mutate(calOutputValue = round(value * efficiency, energy.DIGITS_CALOUTPUT)) ->
      L2325.out_EJ_R_ind_serv_F_Yh # intermediate tibble

    # intermediate tibble to extract chemical names
    A325.globaltech_shrwt %>%
      filter(supplysector == "chemical") %>%
      select(supplysector, subsector, technology) ->
      L2325.chemical_names

    # Aggregate service output by region. This is the output of the industrial sector in each region.
    L2325.out_EJ_R_ind_serv_F_Yh %>%
      group_by(region, GCAM_region_ID, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup %>%
      mutate(supplysector = L2325.chemical_names[["supplysector"]],
             subsector = L2325.chemical_names[["subsector"]],
             stub.technology = L2325.chemical_names[["technology"]],
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2325.StubTechProd_chemical

    # L2325.StubTechCoef_chemical: calibrated output of industrial sector
    # Next, aggregate service output by sector to calculate the portion of each input
    L2325.out_EJ_R_ind_serv_F_Yh %>%
      group_by(region, GCAM_region_ID, supplysector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup %>%
      left_join_error_no_match(select(L2325.StubTechProd_chemical, calOutputValue, region, year),
                               by = c("region", "year")) %>%
      rename(calOutputValue = calOutputValue.x,
             output_tot = calOutputValue.y) %>%
      mutate(coefficient = replace_na(calOutputValue / output_tot,0)) %>%
      rename(minicam.energy.input = supplysector) %>%
      mutate(supplysector = L2325.chemical_names[["supplysector"]],
             subsector = L2325.chemical_names[["subsector"]],
             stub.technology = L2325.chemical_names[["technology"]],
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L2325.StubTechCoef_chemical_base # intermediate tibble?

    # This set of coefficients covers only the base years; the first "future" period will default to the global tech coefficient
    # Instead, interpolate the coefficients to these global default values in a specified period
    L2325.StubTechCoef_chemical_base %>%
      complete(nesting(region, supplysector, subsector, stub.technology, minicam.energy.input, market.name),
               year = unique(c(MODEL_YEARS, energy.INDCOEF_CONVERGENCE_YR))) %>%
      left_join(select(A325.globaltech_coef, supplysector, subsector, technology, minicam.energy.input, terminal_coef),
                by = c("supplysector", "subsector", stub.technology = "technology", "minicam.energy.input")) %>%
      mutate(coefficient = if_else(year == energy.INDCOEF_CONVERGENCE_YR, terminal_coef, coefficient)) %>%
      select(-terminal_coef) %>%
      group_by(region, supplysector, subsector, stub.technology, minicam.energy.input) %>%
      mutate(coefficient = round(approx_fun(year, coefficient,rule = 2), energy.DIGITS_COEFFICIENT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) ->   # drop the terminal coef year if it's outside of the model years
      L2325.StubTechCoef_chemical


    # L2325.PerCapitaBased_chemical: per-capita based flag for chemical exports final demand
    A325.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PerCapitaBased"]], GCAM_region_names)  ->
      L2325.PerCapitaBased_chemical


    # L2325.BaseService_chemical: base-year service output of chemical
    L2325.StubTechProd_chemical %>%
      select(region, year, supplysector,base.service = calOutputValue) %>%
      rename(energy.final.demand = supplysector) %>%
      group_by(region,year,energy.final.demand) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup()  ->
      L2325.BaseService_chemical

    #For regions with 0 in base year, modify Subsector shareweight and interpolation
    L2325.out_EJ_R_ind_serv_F_Yh %>%
      group_by(region,supplysector, GCAM_region_ID, year) %>%
      summarise(value = sum(calOutputValue)) %>%
      ungroup %>%
      select(region, year, supplysector,value) %>%
      filter(value == 0, year == MODEL_FINAL_BASE_YEAR)  ->
      nobaseyear

    L2325.SubsectorShrwtFllt_chemical %>%
      left_join(nobaseyear, by = c("region", "supplysector")) %>%
      mutate(value = replace_na(value,1),share.weight = if_else(value ==0,0.5,share.weight),year = NULL,value = NULL) ->
      L2325.SubsectorShrwtFllt_chemical


    L2325.SubsectorInterp_chemical %>%
      left_join(nobaseyear, by = c("region", "supplysector")) %>%
      mutate(value = replace_na(value,1),interpolation.function = if_else(value ==0,"linear",interpolation.function),year = NULL,value = NULL) ->
      L2325.SubsectorInterp_chemical

    # L2325.PriceElasticity_chemical: price elasticity
    A325.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PriceElasticity"]][LEVEL2_DATA_NAMES[["PriceElasticity"]] != "year"], GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["PriceElasticity"]]) ->
      L2325.PriceElasticity_chemical

    # ===================================================
    # Produce outputs
    L2325.Supplysector_chemical %>%
      add_title("Supply sector information for chemical sector") %>%
      add_units("NA") %>%
      add_comments("For chemical sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A325.sector is expended into all GCAM regions") %>%
      add_legacy_name("L2325.Supplysector_chemical") %>%
      add_precursors("energy/A325.sector", "common/GCAM_region_names") ->
      L2325.Supplysector_chemical

    L2325.FinalEnergyKeyword_chemical %>%
      add_title("Supply sector keywords for chemical sector") %>%
      add_units("NA") %>%
      add_comments("For chemical sector, the supply sector final energy keywords from A325.sector are expended into all GCAM regions") %>%
      add_legacy_name("L2325.FinalEnergyKeyword_chemical") %>%
      add_precursors("energy/A325.sector", "common/GCAM_region_names") ->
      L2325.FinalEnergyKeyword_chemical

    L2325.SubsectorLogit_chemical %>%
      add_title("Subsector logit exponents of chemical sector") %>%
      add_units("Unitless") %>%
      add_comments("For chemical sector, the subsector logit exponents from A325.subsector_logit are expanded into all GCAM regions") %>%
      add_legacy_name("L2325.SubsectorLogit_chemical") %>%
      add_precursors("energy/A325.subsector_logit", "energy/A_regions","common/GCAM_region_names") ->
      L2325.SubsectorLogit_chemical

    L2325.SubsectorShrwtFllt_chemical %>%
      add_title("Subsector shareweights of chemical sector") %>%
      add_units("unitless") %>%
      add_comments("For chemical sector, the subsector shareweights from A325.subsector_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2325.SubsectorShrwtFllt_chemical") %>%
      add_precursors("energy/A325.subsector_shrwt", "energy/A_regions","common/GCAM_region_names") ->
      L2325.SubsectorShrwtFllt_chemical

    L2325.SubsectorInterp_chemical %>%
      add_title("Subsector shareweight interpolation of chemical sector") %>%
      add_units("NA") %>%
      add_comments("For chemical sector, the subsector shareweight interpolation function infromation from A325.subsector_interp is expanded into all GCAM regions") %>%
      add_legacy_name("L2325.SubsectorInterp_chemical") %>%
      add_precursors("energy/A325.subsector_interp", "energy/A_regions", "common/GCAM_region_names") ->
      L2325.SubsectorInterp_chemical

    L2325.StubTech_chemical %>%
      add_title("Identification of stub technologies of chemical") %>%
      add_units("NA") %>%
      add_comments("For chemical sector, the stub technologies from A325.globaltech_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2325.StubTech_chemical") %>%
      add_precursors("energy/A325.globaltech_shrwt", "energy/A_regions","common/GCAM_region_names") ->
      L2325.StubTech_chemical

    L2325.GlobalTechShrwt_chemical %>%
      add_title("Shareweights of global chemical technologies") %>%
      add_units("Unitless") %>%
      add_comments("For chemical sector, the share weights from A325.globaltech_shrwt are interpolated into all base years and future years") %>%
      add_legacy_name("L2325.GlobalTechShrwt_chemical") %>%
      add_precursors("energy/A325.globaltech_shrwt") ->
      L2325.GlobalTechShrwt_chemical

    L2325.GlobalTechCoef_chemical %>%
      add_title("Energy inputs and coefficients of chemical technologies") %>%
      add_units("Unitless") %>%
      add_comments("For chemical sector, the energy use coefficients from A325.globaltech_coef are interpolated into all model years") %>%
      add_legacy_name("L2325.GlobalTechCoef_chemical") %>%
      add_precursors("energy/A325.globaltech_coef") ->
      L2325.GlobalTechCoef_chemical


    if(exists("L2325.GlobalTechShutdown_chemical")) {
      L2325.GlobalTechShutdown_chemical %>%
        add_title("Global tech lifetime for techs with shutdown rate") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that uses a phased retirement function") %>%
        add_legacy_name("L2325.GlobalTechShutdown_chemical") %>%
        add_precursors("energy/A325.globaltech_retirement") ->
        L2325.GlobalTechShutdown_chemical
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2325.GlobalTechShutdown_chemical") ->
        L2325.GlobalTechShutdown_chemical
    }

    if(exists("L2325.GlobalTechSCurve_chemical")) {
      L2325.GlobalTechSCurve_chemical %>%
        add_title("Global tech lifetime for techs with s-curve retirement function") %>%
        add_units("Lifetime in years, half-life in years") %>%
        add_comments("Filters for any technology that uses an S-curve retirement function") %>%
        add_legacy_name("L2325.GlobalTechSCurve_chemical") %>%
        add_precursors("energy/A325.globaltech_retirement") ->
        L2325.GlobalTechSCurve_chemical
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2325.GlobalTechSCurve_chemical") ->
        L2325.GlobalTechSCurve_chemical
    }

    if(exists("L2325.GlobalTechLifetime_chemical")) {
      L2325.GlobalTechLifetime_chemical %>%
        add_title("Global tech lifetime for any technology with no retirement function") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that has no phased or S-curve retirement function, empty by default.") %>%
        add_legacy_name("L2325.GlobalTechLifetime_chemical") %>%
        add_precursors("energy/A325.globaltech_retirement") ->
        L2325.GlobalTechLifetime_chemical
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2325.GlobalTechLifetime_chemical") ->
        L2325.GlobalTechLifetime_chemical
    }

    if(exists("L2325.GlobalTechProfitShutdown_chemical")) {
      L2325.GlobalTechProfitShutdown_chemical %>%
        add_title("Global tech profit shutdown decider and parameters") %>%
        add_units("Unitless, used to determine shape of the function defining the relationship between shutdown rate and profitability") %>%
        add_comments("Filters for any technologies that use a profit-based shutdown parameter") %>%
        add_legacy_name("L2325.GlobalTechProfitShutdown_chemical") %>%
        add_precursors("energy/A325.globaltech_retirement") ->
        L2325.GlobalTechProfitShutdown_chemical
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2325.GlobalTechProfitShutdown_chemical") ->
        L2325.GlobalTechProfitShutdown_chemical
    }

    L2325.GlobalTechCost_chemical %>%
      add_title("Non-energy costs of global chemical manufacturing technologies") %>%
      add_units("1975$/kg for supplysector chemical; 1975$/GJ for supplysector process heat chemical") %>%
      add_comments("For chemical sector, the Non-energy costs of global chemical manufacturing technologies are calculated then adjusted with CCS to include CO2 capture costs") %>%
      add_legacy_name("L2325.GlobalTechCost_chemical") %>%
      add_precursors("energy/A325.globaltech_cost", "energy/A325.globaltech_coef") ->
      L2325.GlobalTechCost_chemical

    L2325.GlobalTechEff_chemical %>%
      add_title("Energy inputs and efficiency of global chemical energy use and feedstocks technologies") %>%
      add_units("Unitless") %>%
      add_comments("For chemical sector, the efficiency values from A325.globaltech_eff are interpolated into all base years and future years") %>%
      add_legacy_name("L2325.GlobalTechEff_chemical") %>%
      add_precursors("energy/A325.globaltech_eff") ->
      L2325.GlobalTechEff_chemical

    L2325.GlobalTechCapture_chemical %>%
      add_title("CO2 capture fractions from global IRONSTL production technologies with CCS") %>%
      add_units("Unitless") %>%
      add_comments("For chemical sector, the remove fractions from A325.globaltech_co2capture are interpolated into all model years") %>%
      add_legacy_name("L2325.GlobalTechCapture_chemical") %>%
      add_precursors("energy/A325.globaltech_co2capture") ->
      L2325.GlobalTechCapture_chemical


    L2325.StubTechProd_chemical %>%
      add_title("calibrated chemical production") %>%
      add_units("EJ") %>%
      add_comments("Values are calculated using L1325.in_EJ_R_chemical_F_Y, then added GCAM region information and supplysector, subsector, and technology information") %>%
      add_legacy_name("L2325.StubTechProd_chemical") %>%
      add_precursors("energy/calibrated_techs",  "common/GCAM_region_names") ->
      L2325.StubTechProd_chemical

    L2325.StubTechCalInput_chemical %>%
       add_title("calibrated chemical production") %>%
       add_units("EJ") %>%
       add_comments("Values are calculated using L1325.in_EJ_R_chemical_F_Y then added GCAM region information and supplysector, subsector, technology, and input information") %>%
       add_legacy_name("L2325.StubTechCalInput_chemical") %>%
       add_precursors("energy/calibrated_techs", "L1325.in_EJ_R_chemical_F_Y", "common/GCAM_region_names") ->
       L2325.StubTechCalInput_chemical

    L2325.StubTechCoef_chemical %>%
      add_title("region-specific coefficients of chemical production technologies") %>%
      add_units("unitless") %>%
      add_comments("Coefficients from literature") %>%
      add_legacy_name("L2325.StubTechCoef_chemical") %>%
      add_precursors("energy/calibrated_techs", "common/GCAM_region_names","energy/A325.globaltech_coef") ->
      L2325.StubTechCoef_chemical

      L2325.PerCapitaBased_chemical %>%
      add_title("per-capita based flag for chemical exports final demand") %>%
      add_units("NA") %>%
      add_comments("Per-capita based flags for chemical from A325.demand are expanded into all GCAM regions") %>%
      add_legacy_name("L2325.PerCapitaBased_chemical") %>%
      add_precursors("energy/A325.demand", "common/GCAM_region_names") ->
      L2325.PerCapitaBased_chemical

    L2325.GlobalTechCSeq_ind %>%
      add_title("CO2 capture fractions from global electricity generation technologies") %>%
      add_units("Unitless") %>%
      add_comments("Remove fractions from A325.nonenergy_Cseq are expanded into all model years") %>%
      add_legacy_name("L2325.GlobalTechCSeq_ind") %>%
      add_precursors("energy/A325.nonenergy_Cseq") ->
      L2325.GlobalTechCSeq_ind

    L2325.BaseService_chemical %>%
      add_title("base-year service output of chemical") %>%
      add_units("EJ") %>%
      add_comments("Transformed from L2325.StubTechProd_chemical by adding energy.final.demand") %>%
      add_legacy_name("L2325.BaseService_chemical") %>%
      add_precursors("energy/A325.demand","L1325.in_EJ_R_chemical_F_Y", "energy/calibrated_techs", "common/GCAM_region_names") ->
      L2325.BaseService_chemical

    L2325.PriceElasticity_chemical %>%
      add_title("price elasticity for chemical") %>%
      add_units("Unitless") %>%
      add_comments("The elasticity values from A325.demand are expanded into all GCAM_regions") %>%
      add_legacy_name("L2325.PriceElasticity_chemical") %>%
      add_precursors("energy/A325.demand", "common/GCAM_region_names") ->
      L2325.PriceElasticity_chemical

    L2325.GlobalTechSecOut_chemical %>%
      add_title("Secondary output ratios of chemical cogeneration technologies") %>%
      add_units("Unitless") %>%
      add_comments("Secondary output ratios are calculated as electricity ratio (Assumed CHP electricity output per unit fuel input) over efficiency") %>%
      add_legacy_name("L2325.GlobalTechSecOut_chemical") %>%
      add_precursors("energy/A23.chp_elecratio", "energy/A325.globaltech_eff") ->
      L2325.GlobalTechSecOut_chemical

      return_data(L2325.Supplysector_chemical, L2325.FinalEnergyKeyword_chemical, L2325.SubsectorLogit_chemical,
                  L2325.SubsectorShrwtFllt_chemical, L2325.SubsectorInterp_chemical,
                  L2325.StubTech_chemical, L2325.GlobalTechShrwt_chemical,L2325.GlobalTechCSeq_ind,
                  L2325.GlobalTechCoef_chemical, L2325.GlobalTechCost_chemical,L2325.GlobalTechShutdown_chemical,
                  L2325.GlobalTechSCurve_chemical, L2325.GlobalTechLifetime_chemical, L2325.GlobalTechProfitShutdown_chemical,
                  L2325.StubTechCalInput_chemical,L2325.StubTechCoef_chemical,L2325.StubTechProd_chemical,
                  L2325.PerCapitaBased_chemical, L2325.BaseService_chemical,L2325.GlobalTechSecOut_chemical,
                  L2325.PriceElasticity_chemical,L2325.GlobalTechCapture_chemical,L2325.GlobalTechEff_chemical)


  } else {
    stop("Unknown command")
  }
}
