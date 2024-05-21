# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2322.Fert
#'
#' Provide supply sector information/keywords, subsector shareweights, global technology lifetime,
#' energy inputs and coefficients, global fertilizer manufacturing technologies, etc. for the fertilizer sector.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:  \code{L2322.Supplysector_Fert}, \code{L2322.FinalEnergyKeyword_Fert}, \code{L2322.SubsectorLogit_Fert},
#' \code{L2322.SubsectorShrwtFllt_Fert}, \code{L2322.SubsectorInterp_Fert}, \code{L2322.StubTech_Fert}, \code{L2322.GlobalTechShrwt_Fert},
#' \code{L2322.GlobalTechCoef_Fert}, \code{L2322.GlobalTechCost_Fert}, \code{L2322.GlobalTechCapture_Fert}, \code{L2322.GlobalTechSCurve_Fert},
#' \code{L2322.GlobalTechProfitShutdown_Fert}, \code{L2322.StubTechProd_FertProd}, \code{L2322.StubTechCoef_Fert}. The corresponding file in the
#' original data system was \code{L2322.Fert.R} (energy level2).
#' @details This chunk provides supply sector information/keywords, subsector shareweights, global technology lifetime,
#' energy inputs and coefficients, global fertilizer manufacturing technologies, etc. for the fertilizer sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select
#' @importFrom tidyr complete nesting
#' @author LF September 2017
module_energy_L2322.Fert <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A322.sector",
             FILE = "energy/A322.subsector_interp",
             FILE = "energy/A322.subsector_logit",
             FILE = "energy/A322.subsector_shrwt",
             FILE = "energy/A322.globaltech_coef",
             FILE = "energy/A322.globaltech_shrwt",
             FILE = "energy/A322.globaltech_co2capture",
             FILE = "energy/A322.globaltech_retirement",
             "L1322.Fert_Prod_MtNH3_R_F_Y",
             "L1322.IO_R_Fert_F_Yh",
             "L1322.Fert_NEcost_75USDkgNH3_F",
             "L142.ag_Fert_NetExp_MtN_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2322.Supplysector_Fert",
             "L2322.SectorUseTrialMarket_tra",
             "L2322.FinalEnergyKeyword_Fert",
             "L2322.SubsectorLogit_Fert",
             "L2322.SubsectorShrwtFllt_Fert",
             "L2322.SubsectorInterp_Fert",
             "L2322.StubTech_Fert",
             "L2322.GlobalTechShrwt_Fert",
             "L2322.TechShrwt_TradedFert",
             "L2322.GlobalTechCoef_Fert",
             "L2322.TechCoef_TradedFert",
             "L2322.StubTechMarket_FertImports",
             "L2322.GlobalTechCost_Fert",
             "L2322.GlobalTechCapture_Fert",
             "L2322.GlobalTechSCurve_Fert",
             "L2322.GlobalTechProfitShutdown_Fert",
             "L2322.StubTechProd_FertProd",
             "L2322.StubTechCoef_Fert",
             "L2322.Production_FertExport",
             "L2322.StubTechProd_FertImport",
             "L2322.StubTechProd_FertDomCons",
             "L2322.StubTechProd_NtoAg"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A322.sector <- get_data(all_data, "energy/A322.sector", strip_attributes = TRUE)
    A322.subsector_interp <- get_data(all_data, "energy/A322.subsector_interp", strip_attributes = TRUE)
    A322.subsector_logit <- get_data(all_data, "energy/A322.subsector_logit", strip_attributes = TRUE)
    A322.subsector_shrwt <- get_data(all_data, "energy/A322.subsector_shrwt", strip_attributes = TRUE)
    A322.globaltech_coef <- get_data(all_data, "energy/A322.globaltech_coef")
    A322.globaltech_shrwt <- get_data(all_data, "energy/A322.globaltech_shrwt", strip_attributes = TRUE)
    A322.globaltech_co2capture <- get_data(all_data, "energy/A322.globaltech_co2capture")
    A322.globaltech_retirement <- get_data(all_data, "energy/A322.globaltech_retirement", strip_attributes = TRUE)
    L1322.Fert_Prod_MtNH3_R_F_Y <- get_data(all_data, "L1322.Fert_Prod_MtNH3_R_F_Y", strip_attributes = TRUE)
    L1322.IO_R_Fert_F_Yh <- get_data(all_data, "L1322.IO_R_Fert_F_Yh", strip_attributes = TRUE)
    L1322.Fert_NEcost_75USDkgNH3_F <- get_data(all_data, "L1322.Fert_NEcost_75USDkgNH3_F")
    L142.ag_Fert_NetExp_MtN_R_Y <- get_data(all_data, "L142.ag_Fert_NetExp_MtN_R_Y", strip_attributes = TRUE)

    # ===================================================
    # 0. Give binding for variable names used in pipeline

    year.fillout <- to.value <- technology <- year <-
      share.weight <- supplysector <- subsector <- coefficient <- minicam.energy.input <-
      NEcost_75USDkgNH3 <- input.cost <- remove.fraction <- half.life <- median.shutdown.point <-
      value <- calOutputValue <- sector <- fuel <- subs.share.weight <- region <- fixedOutput <- . <- NULL

    # ===================================================
    # 1. Perform computations
    # Create tables to delete technologies and subsectors in regions where heat is not modeled as a fuel
    # 1a. Supplysector information
    # L2322.Supplysector_Fert: Supply sector information for fertilizer sector
    A322.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME),
                           GCAM_region_names,
                           has_traded = TRUE) ->
      L2322.Supplysector_Fert

    # L2322.SectorUseTrialMarket_tra: Create solved markets for the traded sectors
    L2322.SectorUseTrialMarket_tra <- filter(A322.sector, traded == 1) %>%
      mutate(region = gcam.USA_REGION,
             use.trial.market = 1) %>%
      select(LEVEL2_DATA_NAMES[["SectorUseTrialMarket"]])

    # L2322.FinalEnergyKeyword_Fert: Supply sector keywords for fertilizer sector
    A322.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]],
                           GCAM_region_names,
                           has_traded = TRUE) %>%
      na.omit ->
      L2322.FinalEnergyKeyword_Fert

    # 2b. Subsector information
    # L2322.SubsectorLogit_Fert: Subsector logit exponents of fertilizer sector
    A322.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME),
                           GCAM_region_names,
                           has_traded = TRUE) ->
      L2322.SubsectorLogit_Fert

    # L2322.SubsectorShrwtFllt_Fert: Subsector shareweights of fertilizer sector
    A322.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]],
                           GCAM_region_names,
                           has_traded = TRUE) ->
      L2322.SubsectorShrwtFllt_Fert

    # L2322.SubsectorInterp_Fert: Subsector shareweight interpolation of fertilizer sector
    A322.subsector_interp %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]],
                           GCAM_region_names,
                           has_traded = TRUE) ->
      L2322.SubsectorInterp_Fert

    # 2c. Technology information
    # L2322.StubTech_Fert: Identification of stub technologies of fertilizer sector
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    # Traded technologies are not represented as stub technologies and are dropped here
    A322.globaltech_shrwt %>%
      filter(traded == 0) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]],
                           GCAM_region_names,
                           has_traded = FALSE) %>%
      rename(stub.technology = technology) ->
      L2322.StubTech_Fert

    # L2322.GlobalTechShrwt_Fert: Shareweights of global fertilizer sector technologies
    # Exclude traded commodities from the global technology database
    A322.globaltech_shrwt %>%
      filter(traded == 0) %>%
      select(-traded) %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]]) ->
      L2322.GlobalTechShrwt_Fert

    # Traded technologies are assigned to all regions within a given region
    A322.globaltech_shrwt %>%
      filter(traded == 1) %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(supplysector, subsector, technology, traded), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["TechShrwt"]],
                           GCAM_region_names = GCAM_region_names,
                           has_traded = TRUE) ->
      L2322.TechShrwt_TradedFert

    # L2322.GlobalTechCoef_Fert: Energy inputs and coefficients of global fertilizer energy use and feedstocks technologies
    # Traded (export) technologies are not assigned to the global technology database
    A322.globaltech_coef %>%
      filter(traded == 0) %>%
      gather_years(value_col = "coefficient") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 1),
             coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L2322.GlobalTechCoef_Fert

    # Traded (export) technologies are treated as standard technologies
    A322.globaltech_coef %>%
      filter(traded == 1) %>%
      gather_years(value_col = "coefficient") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input, traded), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 1),
             coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["TechCoef"]],
                           GCAM_region_names = GCAM_region_names,
                           has_traded = TRUE,
                           set_market = TRUE) ->
      L2322.TechCoef_TradedFert

    # Market-names of import technologies are assigned to the USA
    L2322.GlobalTechCoef_Fert %>%
      filter(grepl("import", subsector.name)) %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["StubTechMarket"]],
                           GCAM_region_names,
                           has_traded = FALSE) %>%
      mutate(market.name = gcam.USA_REGION) ->
      L2322.StubTechMarket_FertImports

    # Costs of global technologies
    # L2322.GlobalTechCost_Fert: Non-energy costs of global fertilizer manufacturing technologies
    L2322.GlobalTechCoef_Fert %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]]) %>%
      mutate(minicam.non.energy.input = "non-energy") %>%
      left_join(L1322.Fert_NEcost_75USDkgNH3_F, by = c('technology' = 'fuel')) %>% # expecting NAs in the joined tibble
      rename(input.cost = NEcost_75USDkgNH3) %>%
      mutate(input.cost = round(input.cost, energy.DIGITS_COST)) %>%
      na.omit -> # Export technologies have no cost assigned. Just drop the object
      L2322.GlobalTechCost_Fert

    # Carbon capture rates from technologies with CCS
    # L2322.GlobalTechCapture_Fert: CO2 capture fractions from global fertilizer production technologies with CCS
    ## No need to consider historical periods or intermittent technologies here
    A322.globaltech_co2capture %>%
      gather_years(value_col = "remove.fraction") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(year, remove.fraction, rule = 1),
             remove.fraction = round(remove.fraction, energy.DIGITS_REMOVE.FRACTION)) %>%
      ungroup %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      mutate(storage.market = "carbon-storage") ->
      L2322.GlobalTechCapture_Fert

    # Retirement information
    A322.globaltech_retirement %>%
      set_years %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      A322.globaltech_retirement_with_years

    # Copy the data in the last base year period through to the end year
    A322.globaltech_retirement_with_years %>%
      filter(year == max(MODEL_BASE_YEARS)) ->
      A322.globaltech_retirement_max_baseyear

    A322.globaltech_retirement_with_years %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(A322.globaltech_retirement_max_baseyear) ->
      L2322.globaltech_retirement

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # All of these options have different headers, and all are allowed

    # L2322.GlobalTechSCurve_Fert: Global tech lifetime and s-curve retirement function
    L2322.globaltech_retirement %>%
      filter(!is.na(half.life)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
      L2322.GlobalTechSCurve_Fert

    # L2322.GlobalTechProfitShutdown_Fert: Global tech profit shutdown decider.
    L2322.globaltech_retirement %>%
      filter(!is.na(median.shutdown.point)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
      L2322.GlobalTechProfitShutdown_Fert

    # Calibration and region-specific data
    # L2322.StubTechProd_FertProd: calibrated output of fertilizer production technologies
    L1322.Fert_Prod_MtNH3_R_F_Y %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(calOutputValue = value) %>%
      mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = 'GCAM_region_ID') %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, supplysector, subsector, technology), by = c("sector", "fuel") ) %>%
      rename(stub.technology = technology) %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2322.StubTechProd_FertProd

    # L2322.StubTechCoef_Fert: calibrated base-year coefficients of fertilizer production technologies
    L1322.IO_R_Fert_F_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(coefficient = value) %>%
      mutate(coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      filter(coefficient != 0) %>% # Where 0, drop from this table (to revert to assumed defaults)
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, supplysector, subsector, technology, minicam.energy.input), by = c("sector", "fuel")) %>%
      mutate(stub.technology = technology, market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L2322.StubTechCoef_Fert

    # Ammonia Exports = NetExports where positive
    L142.ag_Fert_NetExp_MtN_R_Y %>%
      select(GCAM_region_ID, year, calOutputValue = value) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = if_else(calOutputValue < 0, 0,
                                      round(calOutputValue / CONV_NH3_N, energy.DIGITS_CALOUTPUT))) %>%   # Convert N export to NH3
      left_join_error_no_match(GCAM_region_names,
                               by = "GCAM_region_ID") %>%
      rename(market.name = region) %>%
      left_join_error_no_match(L2322.TechCoef_TradedFert,
                               by = c("market.name", "year")) %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) ->
      L2322.Production_FertExport

    # Ammonia Imports = NetExports where negative
    L142.ag_Fert_NetExp_MtN_R_Y %>%
      select(GCAM_region_ID, year, calOutputValue = value) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = if_else(calOutputValue > 0, 0,
                                      round(calOutputValue * -1 / CONV_NH3_N, energy.DIGITS_CALOUTPUT))) %>%   # Convert N import to NH3
      left_join_error_no_match(GCAM_region_names,
                               by = "GCAM_region_ID") %>%
      left_join_error_no_match(filter(L2322.StubTech_Fert, grepl("imported", subsector)),
                                      by = "region") %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2322.StubTechProd_FertImport

    # Ammonia Consumption of Domestic Production = Production - Exports
    L2322.Production_FertExport %>%
      mutate(region = substr(subsector, 1, regexpr("traded", subsector, fixed = T) - 2)) %>%
      select(region, year, Exports = calOutputValue) ->
      L2322.AmmoniaExports

    L2322.StubTechProd_FertProd %>%
      group_by(region, year) %>%
      summarise(Production = sum(calOutputValue)) %>%
      ungroup() %>%
      left_join_error_no_match(L2322.AmmoniaExports, by = c("region", "year")) %>%
      mutate(calOutputValue = Production - Exports) %>%
      left_join_error_no_match(filter(L2322.StubTech_Fert, grepl("domestic", subsector)),
                               by = "region") %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2322.StubTechProd_FertDomCons

    # Calibrated flow of ammonia to agricultural "N fertilizer"
    # The input of ammonia to N fertilizer is equal to the sum of consumption of domestic production plus imports,
    # times the NH3-to-N stoichiometric mass ratio
    L2322.StubTechProd_FertDomCons %>%
      select(region, year, DomConsumption = calOutputValue) %>%
      left_join_error_no_match(select(L2322.StubTechProd_FertImport,
                                      region, year, Imports = calOutputValue),
                               by = c("region", "year")) %>%
      mutate(calOutputValue = round((DomConsumption + Imports) * CONV_NH3_N,
                                    energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(filter(L2322.StubTech_Fert, supplysector == aglu.FERT_NAME),
                               by = "region") %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2322.StubTechProd_NtoAg

    # ===================================================
    # Produce outputs

    L2322.Supplysector_Fert %>%
      add_title("Supply sector information for fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("For fertilizer sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A322.sector is expended into all GCAM regions") %>%
      add_legacy_name("L2322.Supplysector_Fert") %>%
      add_precursors("common/GCAM_region_names", "energy/A322.sector") ->
      L2322.Supplysector_Fert

    L2322.SectorUseTrialMarket_tra %>%
      add_title("Supplysector flag indicating to make trial markets for traded ammonia") %>%
      add_units("NA") %>%
      add_comments("This helps model solution") %>%
      add_precursors("energy/A322.sector") ->
      L2322.SectorUseTrialMarket_tra

    L2322.FinalEnergyKeyword_Fert %>%
      add_title("Supply sector keywords for fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("For fertilizer sector, the supply sector final energy keywords from A322.sector are expended into all GCAM regions") %>%
      add_legacy_name("L2322.FinalEnergyKeyword_Fert") %>%
      add_precursors("common/GCAM_region_names", "energy/A322.sector") ->
      L2322.FinalEnergyKeyword_Fert

    L2322.SubsectorLogit_Fert %>%
      add_title("Subsector logit exponents of fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("For fertilizer sector, the subsector logit exponents from A322.subsector_logit are expanded into all GCAM regions") %>%
      add_legacy_name("L2322.SubsectorLogit_Fert") %>%
      add_precursors("energy/A322.subsector_logit", "common/GCAM_region_names") ->
      L2322.SubsectorLogit_Fert

    L2322.SubsectorShrwtFllt_Fert %>%
      add_title("Subsector shareweights of fertilizer") %>%
      add_units("Unitless") %>%
      add_comments("For fertilizer sector, the subsector shareweights from A322.subsector_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2322.SubsectorShrwtFllt_Fert") %>%
      add_precursors("energy/A322.subsector_shrwt", "common/GCAM_region_names") ->
      L2322.SubsectorShrwtFllt_Fert

    L2322.SubsectorInterp_Fert %>%
      add_title("Subsector shareweight interpolation of fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("For fertilizer sector, the subsector shareweight interpolation function infromation from A322.subsector_interp is expanded into all GCAM regions") %>%
      add_legacy_name("L2322.SubsectorInterp_Fert") %>%
      add_precursors("energy/A322.subsector_interp", "common/GCAM_region_names") ->
      L2322.SubsectorInterp_Fert

    L2322.StubTech_Fert %>%
      add_title("Stub-technology (coal, coal CCS, and etc.) for fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("For fertilizer sector, the stub technologies from A322.globaltech_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2322.StubTech_Fert") %>%
      add_precursors("energy/A322.globaltech_shrwt", "common/GCAM_region_names") ->
      L2322.StubTech_Fert

    L2322.GlobalTechShrwt_Fert %>%
      add_title("Shareweights of global fertilizer sector technologies") %>%
      add_units("Unitless") %>%
      add_comments("For fertilizer sector, the share weights from A322.globaltech_shrwt are interpolated into all base years and future years") %>%
      add_legacy_name("L2322.GlobalTechShrwt_Fert") %>%
      add_precursors("energy/A322.globaltech_shrwt") ->
      L2322.GlobalTechShrwt_Fert

    L2322.TechShrwt_TradedFert %>%
      add_title("Shareweights of traded fertilizer sector technologies") %>%
      add_units("Unitless") %>%
      add_comments("For fertilizer sector, the share weights from A322.globaltech_shrwt are interpolated into all base years and future years") %>%
      add_precursors("common/GCAM_region_names") %>%
      same_precursors_as(L2322.GlobalTechShrwt_Fert) ->
      L2322.TechShrwt_TradedFert

    L2322.GlobalTechCoef_Fert %>%
      add_title("Energy inputs and coefficients of global fertilizer energy use and feedstocks technologies") %>%
      add_units("GJ/kg NH3 for ammonia supplysector; Unitless for regional ammonia; kgNH3/kgN for N fertilizer") %>%
      add_comments("For fertilizer sector, the energy use coefficients from A322.globaltech_coef are interpolated into all model years") %>%
      add_legacy_name("energy/A322.globaltech_coef") %>%
      add_precursors("energy/A322.globaltech_coef") ->
      L2322.GlobalTechCoef_Fert

    L2322.TechCoef_TradedFert %>%
      add_title("Energy inputs, coefficients, and market names of traded fertilizer technologies") %>%
      add_units("unitless IO") %>%
      add_comments("Traded technologies are not assigned to the global technology database") %>%
      add_precursors("common/GCAM_region_names") %>%
      same_precursors_as(L2322.GlobalTechCoef_Fert) ->
      L2322.TechCoef_TradedFert

    L2322.StubTechMarket_FertImports %>%
      add_title("Market-name of ammonia import technologies in each region") %>%
      add_units("unitless IO") %>%
      add_comments("Import markets are cleared within the USA region") %>%
      add_precursors("common/GCAM_region_names") %>%
      same_precursors_as(L2322.GlobalTechCoef_Fert) ->
      L2322.StubTechMarket_FertImports

    L2322.GlobalTechCost_Fert %>%
      add_title("Non-energy costs of global fertilizer manufacturing technologies") %>%
      add_units("1975 USD/kgNH3") %>%
      add_comments("For fertilizer sector, the non-energy costs of global fertilizer manufacturing technologies are calculated using values from L1322.Fert_NEcost_75USDkgNH3_F") %>%
      add_legacy_name("L2322.GlobalTechCost_Fert") %>%
      add_precursors("L1322.Fert_NEcost_75USDkgNH3_F") ->
      L2322.GlobalTechCost_Fert

    L2322.GlobalTechCapture_Fert %>%
      add_title("CO2 capture fractions from global fertilizer production technologies with CCS") %>%
      add_units("Unitless") %>%
      add_comments("For fertilizer sector, the remove fractions from A322.globaltech_co2capture are interpolated into all model years") %>%
      add_legacy_name("L2322.GlobalTechCapture_Fert") %>%
      add_precursors("energy/A322.globaltech_co2capture") ->
      L2322.GlobalTechCapture_Fert

    L2322.GlobalTechSCurve_Fert %>%
      add_title("Global tech lifetime and s-curve retirement function") %>%
      add_units("year for lifetime and halflife; Unitless for steepness") %>%
      add_comments("The values are extracted from L2322.globaltech_retirement for entries that half life value is not NA") %>%
      add_legacy_name("L2322.GlobalTechSCurve_Fert") %>%
      add_precursors("energy/A322.globaltech_retirement") ->
      L2322.GlobalTechSCurve_Fert

    L2322.GlobalTechProfitShutdown_Fert %>%
      add_title("Global tech profit shutdown decider") %>%
      add_units("Unitless") %>%
      add_comments("The values are extracted from L2322.globaltech_retirement for entries that median shutdown point is not NA") %>%
      add_legacy_name("L2322.GlobalTechProfitShutdown_Fert") %>%
      add_precursors("energy/A322.globaltech_retirement") ->
      L2322.GlobalTechProfitShutdown_Fert

    L2322.StubTechProd_FertProd %>%
      add_title("calibrated output of fertilizer technologies") %>%
      add_units("Mt N") %>%
      add_comments("Values are calculated using L1322.Fert_Prod_MtNH3_R_F_Y then added GCAM region information") %>%
      add_legacy_name("L2322.StubTechProd_Fert") %>%
      add_precursors("L1322.Fert_Prod_MtNH3_R_F_Y", "common/GCAM_region_names", "energy/calibrated_techs") ->
      L2322.StubTechProd_FertProd

    L2322.StubTechCoef_Fert %>%
      add_title("calibrated base-year coefficients of fertilizer production technologies") %>%
      add_units("Unitless") %>%
      add_comments("Coefficients are calculated using L1322.IO_R_Fert_F_Yh") %>%
      add_legacy_name("L2322.StubTechCoef_Fert") %>%
      add_precursors("L1322.IO_R_Fert_F_Yh", "common/GCAM_region_names", "energy/calibrated_techs") ->
      L2322.StubTechCoef_Fert

    L2322.Production_FertExport %>%
      add_title("calibrated base-year exports of ammonia") %>%
      add_units("Mt NH3") %>%
      add_comments("Calibrated exports of ammonia") %>%
      same_precursors_as(L2322.TechCoef_TradedFert) %>%
      add_precursors("L142.ag_Fert_NetExp_MtN_R_Y") ->
      L2322.Production_FertExport

    L2322.StubTechProd_FertImport %>%
      add_title("calibrated base-year imports of ammonia") %>%
      add_units("Mt NH3") %>%
      add_comments("Calibrated imports of ammonia") %>%
      add_precursors("L142.ag_Fert_NetExp_MtN_R_Y",
                     "common/GCAM_region_names",
                     "energy/A322.globaltech_shrwt") ->
      L2322.StubTechProd_FertImport

    L2322.StubTechProd_FertDomCons %>%
      add_title("calibrated base-year consumption of domestically produced ammonia") %>%
      add_units("Mt NH3") %>%
      add_comments("Calculated as production minus exports") %>%
      add_precursors("L142.ag_Fert_NetExp_MtN_R_Y",
                     "common/GCAM_region_names",
                     "energy/A322.globaltech_shrwt") ->
      L2322.StubTechProd_FertDomCons

    L2322.StubTechProd_NtoAg %>%
      add_title("calibrated base-year flow of synthetic nitrogen to the agricultural sector") %>%
      add_units("Mt NH3") %>%
      add_comments("Calculated as ammonia consumption of domestic production plus imports, times the N/NH3 mass ratio") %>%
      same_precursors_as(L2322.StubTechProd_FertDomCons) ->
      L2322.StubTechProd_NtoAg


    return_data(L2322.Supplysector_Fert, L2322.SectorUseTrialMarket_tra,
                L2322.FinalEnergyKeyword_Fert, L2322.SubsectorLogit_Fert,
                L2322.SubsectorShrwtFllt_Fert, L2322.SubsectorInterp_Fert,
                L2322.StubTech_Fert, L2322.GlobalTechShrwt_Fert, L2322.TechShrwt_TradedFert,
                L2322.GlobalTechCoef_Fert, L2322.TechCoef_TradedFert, L2322.StubTechMarket_FertImports,
                L2322.GlobalTechCost_Fert, L2322.GlobalTechCapture_Fert, L2322.GlobalTechSCurve_Fert,
                L2322.GlobalTechProfitShutdown_Fert, L2322.StubTechProd_FertProd, L2322.StubTechCoef_Fert,
                L2322.Production_FertExport, L2322.StubTechProd_FertImport, L2322.StubTechProd_FertDomCons,
                L2322.StubTechProd_NtoAg)
  } else {
    stop("Unknown command")
  }
}
