# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L222.en_transformation
#'
#' Prepare the assumptions and calibrated outputs for energy transformation supplysectors, subsectors, and technologies.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L222.SectorLogitTables[[ curr_table ]]$data}, \code{L222.Supplysector_en},
#' \code{L222.SubsectorLogitTables[[ curr_table ]]$data}, \code{L222.SubsectorLogit_en}, \code{L222.SubsectorShrwt_en},
#'  \code{L222.SubsectorShrwtFllt_en}, \code{L222.SubsectorInterp_en}, \code{L222.SubsectorInterpTo_en},
#'  \code{L222.StubTech_en}, \code{L222.GlobalTechInterp_en}, \code{L222.GlobalTechCoef_en},
#'  \code{L222.GlobalTechCost_en}, \code{L222.GlobalTechShrwt_en}, \code{L222.GlobalTechCapture_en},
#'  \code{L222.GlobalTechShutdown_en}, \code{L222.GlobalTechSCurve_en}, \code{L222.GlobalTechLifetime_en},
#'  \code{L222.GlobalTechProfitShutdown_en}, \code{L222.StubTechProd_gasproc}, \code{L222.StubTechProd_refining},
#'   \code{L222.StubTechCoef_refining}, \code{L222.GlobalTechCost_low_en}. The corresponding file in the
#' original data system was \code{L222.en_transformation.R} (energy level2).
#' @details This chunk sets up the energy transformation global technology database as well as writing out assumptions to all regions for shareweights and logits.
#' Calibrated outputs for gas processing and oil refining as well as I:O coefficients are interpolated from historical values to base model years.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select semi_join
#' @importFrom tidyr complete nesting
#' @author CWR Sept 2017
module_energy_L222.en_transformation <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A_regions",
             FILE = "energy/A22.sector",
             FILE = "energy/A22.subsector_logit",
             FILE = "energy/A22.subsector_shrwt",
             FILE = "energy/A22.subsector_interp",
             FILE = "energy/A22.globaltech_coef",
             FILE = "energy/A22.globaltech_cost",
             # Note: Low indicates low tech. Costs are actually higher than core
             FILE = "energy/A22.globaltech_cost_low",
             FILE = "energy/A22.globaltech_shrwt",
             FILE = "energy/A22.globaltech_interp",
             FILE = "energy/A22.globaltech_co2capture",
             FILE = "energy/A22.globaltech_retirement",
             FILE = "energy/A22.globaltech_keyword",
             "L122.out_EJ_R_gasproc_F_Yh",
             "L122.out_EJ_R_refining_F_Yh",
             "L122.IO_R_oilrefining_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L222.Supplysector_en",
             "L222.SectorUseTrialMarket_en",
             "L222.SubsectorLogit_en",
             "L222.SubsectorShrwt_en",
             "L222.SubsectorShrwtFllt_en",
             "L222.SubsectorInterp_en",
             "L222.SubsectorInterpTo_en",
             "L222.StubTech_en",
             "L222.GlobalTechInterp_en",
             "L222.GlobalTechCoef_en",
             "L222.GlobalTechCost_en",
             "L222.GlobalTechShrwt_en",
             "L222.GlobalTechCapture_en",
             "L222.GlobalTechShutdown_en",
             "L222.GlobalTechSCurve_en",
             "L222.GlobalTechLifetime_en",
             "L222.GlobalTechProfitShutdown_en",
             "L222.GlobalTechKeyword_en",
             "L222.StubTechProd_gasproc",
             "L222.StubTechProd_refining",
             "L222.StubTechCoef_refining",
             "L222.GlobalTechCost_low_en"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silencing global variable package checks
    GCAM_region_ID <- calOutputValue <- coefficient <- fuel <- half.life <- input.cost <- lifetime <-
    median.shutdown.point <- minicam.energy.input <- minicam.non.energy.input <- object <-
    profit.shutdown.steepness <- region <- remove.fraction <- sector <- sector.name <- share.weight <-
    shutdown.rate <- steepness <- stub.technology <- subsector <- subsector.name <- supplysector <-
    technology <- to.value <- value <- year <- year.fillout <- year.share.weight <- year.x <- year.y <-
      primary.consumption <- NULL

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs", strip_attributes = TRUE)
    A_regions <- get_data(all_data, "energy/A_regions")
    A22.sector <- get_data(all_data, "energy/A22.sector", strip_attributes = TRUE)
    A22.subsector_logit <- get_data(all_data, "energy/A22.subsector_logit", strip_attributes = TRUE)
    A22.subsector_shrwt <- get_data(all_data, "energy/A22.subsector_shrwt", strip_attributes = TRUE)
    A22.subsector_interp <- get_data(all_data, "energy/A22.subsector_interp", strip_attributes = TRUE)
    A22.globaltech_coef <- get_data(all_data, "energy/A22.globaltech_coef", strip_attributes = TRUE)
    A22.globaltech_cost <- get_data(all_data, "energy/A22.globaltech_cost")
    A22.globaltech_cost_low  <- get_data(all_data, "energy/A22.globaltech_cost_low")
    A22.globaltech_shrwt <- get_data(all_data, "energy/A22.globaltech_shrwt", strip_attributes = TRUE)
    A22.globaltech_interp <- get_data(all_data, "energy/A22.globaltech_interp", strip_attributes = TRUE)
    A22.globaltech_co2capture <- get_data(all_data, "energy/A22.globaltech_co2capture")
    A22.globaltech_retirement <- get_data(all_data, "energy/A22.globaltech_retirement", strip_attributes = TRUE)
    A22.globaltech_keyword <- get_data(all_data, "energy/A22.globaltech_keyword", strip_attributes = TRUE)
    L122.out_EJ_R_gasproc_F_Yh <- get_data(all_data, "L122.out_EJ_R_gasproc_F_Yh")
    L122.out_EJ_R_refining_F_Yh <- get_data(all_data, "L122.out_EJ_R_refining_F_Yh", strip_attributes = TRUE)
    L122.IO_R_oilrefining_F_Yh <- get_data(all_data, "L122.IO_R_oilrefining_F_Yh")

    # ===================================================

    # 2. Build tables for CSVs
    # 2a. Supplysector information
    # L222.Supplysector_en: Supply sector information for energy transformation sectors

    A22.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L222.Supplysector_en
    # 2b. Subsector information
    # L222.SubsectorLogit_en: Subsector logit exponents of energy transformation sectors

    # Create a trial market to help with simultaneities related to refining
    L222.SectorUseTrialMarket_en <- filter(L222.Supplysector_en, supplysector == "refining") %>%
      select(region, supplysector) %>%
      mutate(use.trial.market = 1)

    A22.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L222.SubsectorLogit_en

    if(any(!is.na(A22.subsector_shrwt$year))) {
      A22.subsector_shrwt %>%
        filter(!is.na(year)) %>%
        write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorShrwt"]]), GCAM_region_names) ->
        L222.SubsectorShrwt_en
    }

    if(any(!is.na(A22.subsector_shrwt$year.fillout))) {
      A22.subsector_shrwt %>%
        filter(!is.na(year.fillout)) %>%
        write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]), GCAM_region_names) ->
        L222.SubsectorShrwtFllt_en
      }

    # L222.SubsectorInterp_en and L222.SubsectorInterpTo_en: Subsector shareweight interpolation of energy transformation sectors

    if(any(is.na(A22.subsector_interp$to.value))) {
    A22.subsector_interp %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorInterp"]]), GCAM_region_names) ->
      L222.SubsectorInterp_en
    }

    if(any(!is.na(A22.subsector_interp$to.value))) {
    A22.subsector_interp %>%
      filter(!is.na(to.value)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]]), GCAM_region_names) ->
      L222.SubsectorInterpTo_en
    }

    # 2c. Technology information

    # L222.StubTech_en: Identification of stub technologies of energy transformation
    # set up filter to keep all non-first gen bio techs in L222.StubTech_en
    firstgenbio_techs <- c("corn ethanol", "sugarbeet ethanol", "sugar cane ethanol", "biodiesel")

    # create list of regional stub.technologies
    A22.globaltech_shrwt %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Tech"]]), GCAM_region_names) %>%
      rename(stub.technology = technology) %>%
      # Drops region x technology combinations that are not applicable, i.e. any first gen bio techs not listed for a region in A_regions
      filter(!(stub.technology %in% firstgenbio_techs) | paste(region, stub.technology) %in%
               c(paste(A_regions$region, A_regions$ethanol), paste(A_regions$region, A_regions$biodiesel))) ->
      L222.StubTech_en

    # L222.GlobalTechInterp_en: Technology shareweight interpolation of energy transformation sectors
    A22.globaltech_interp %>%
      set_years() %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      # included to strip attributes from assumption file
      mutate(sector.name = sector.name) ->
      L222.GlobalTechInterp_en

    # L222.GlobalTechCoef_en: Energy inputs and coefficients of global technologies for energy transformation

    A22.globaltech_coef %>%
      gather_years(value_col = "coefficient") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      mutate(coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) ->
      L222.GlobalTechCoef_en
    # reorders columns to match expected model interface input
    L222.GlobalTechCoef_en <- L222.GlobalTechCoef_en[LEVEL2_DATA_NAMES[["GlobalTechCoef"]]]

    # L222.GlobalTechCost_en: Costs of global technologies for energy transformation
    A22.globaltech_cost %>%
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector, input.cost = value) %>%
      mutate(input.cost = round(input.cost, energy.DIGITS_COST)) ->
      L222.GlobalTechCost_en
    # reorders columns to match expected model interface input
    L222.GlobalTechCost_en <- L222.GlobalTechCost_en[LEVEL2_DATA_NAMES[["GlobalTechCost"]]]

    # L222.GlobalTechCost_low_en: Costs of global technologies for energy transformation -- low tech/high cost option
    A22.globaltech_cost_low %>%
      gather_years(value_col = "input.cost") %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input), year = c(year, MODEL_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, input.cost, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      mutate(input.cost = round(input.cost, energy.DIGITS_COST)) ->
      L222.GlobalTechCost_low_en
    # reorders columns to match expected model interface input
    L222.GlobalTechCost_low_en <- L222.GlobalTechCost_low_en[LEVEL2_DATA_NAMES[["GlobalTechCost"]]]

    # L222.GlobalTechShrwt_en: Shareweights of global technologies for energy transformation
    A22.globaltech_shrwt %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L222.GlobalTechShrwt_en
    # reorders columns to match expected model interface input
    L222.GlobalTechShrwt_en <- L222.GlobalTechShrwt_en[c(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "share.weight")]

    # L222.GlobalTechCapture_en: CO2 capture fractions from global technologies for energy transformation
    # No need to consider historical periods here
    A22.globaltech_co2capture %>%
      gather_years(value_col = "remove.fraction") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(year, remove.fraction, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      # Rounds the fraction to two digits and adds the name of the carbon storage market
      mutate(remove.fraction = round(remove.fraction, energy.DIGITS_REMOVE.FRACTION), storage.market = energy.CO2.STORAGE.MARKET) ->
      L222.GlobalTechCapture_en
    # reorders columns to match expected model interface input
    L222.GlobalTechCapture_en <- L222.GlobalTechCapture_en[c(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "remove.fraction", "storage.market")]

    # Retirement information
    A22.globaltech_retirement %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L222.globaltech_retirement_base

    # Copies first future year retirment information into all future years and appends back onto base year
    L222.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) ->
      L222.globaltech_retirement_future

    # filters base years from original and then appends future years
    L222.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      bind_rows(L222.globaltech_retirement_future) ->
      L222.globaltech_retirement

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # This section checks L222.globaltech_retirement for each of these functions and creates a separate level 2 file for each
    # All of these options have different headers, and all are allowed
    if(any(!is.na(L222.globaltech_retirement$shutdown.rate))) {
      L222.globaltech_retirement %>%
        filter(!is.na(L222.globaltech_retirement$shutdown.rate)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "shutdown.rate") ->
        L222.GlobalTechShutdown_en
    }

    if(any(!is.na(L222.globaltech_retirement$half.life))) {
      L222.globaltech_retirement %>%
        filter(!is.na(L222.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
        L222.GlobalTechSCurve_en
    }

    # L222.GlobalTechLifetime_en: Global tech lifetime
    if(any(is.na(L222.globaltech_retirement$shutdown.rate) & is.na(L222.globaltech_retirement$half.life))) {
      L222.globaltech_retirement %>%
        filter(is.na(L222.globaltech_retirement$shutdown.rate) & is.na(L222.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime") ->
        L222.GlobalTechLifetime_en
    }

    # L222.GlobalTechProfitShutdown_en: Global tech profit shutdown decider and parameters
    if(any(!is.na(L222.globaltech_retirement$median.shutdown.point))) {
      L222.globaltech_retirement %>%
        filter(!is.na(L222.globaltech_retirement$median.shutdown.point)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
        L222.GlobalTechProfitShutdown_en
    }

    # L222.GlobalTechKeyword_en: Global tech primary energy keywords to ensure first gen
    # biomass gets picked up by the primary energy queries
    A22.globaltech_keyword %>%
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, primary.consumption, year) %>%
      filter(year %in% MODEL_YEARS) ->
      L222.GlobalTechKeyword_en

    #2d. Calibration and region-specific data
    #  generate base year calibrated outputs of gas processing by interpolating from historical values
    L122.out_EJ_R_gasproc_F_Yh %>%
      complete(nesting(GCAM_region_ID, sector, fuel), year = c(year, MODEL_BASE_YEARS)) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      # append region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L222.out_EJ_R_gasproc_F_Yh_base

    # append matching calibrated technology sector/subsector/technology to calibrated outputs of gas processing
    calibrated_techs %>%
      semi_join(L222.out_EJ_R_gasproc_F_Yh_base, by = c("sector", "fuel")) %>%
      select(sector, fuel, supplysector, subsector, technology) %>%
      # left_join because the join changes the number of rows, multiple matches in gasproc for every calibrated tech.
      left_join(L222.out_EJ_R_gasproc_F_Yh_base, by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) ->
      L222.out_EJ_R_gasproc_F_Yh

    # L222.StubTechProd_gasproc: calibrated output of gas processing technologies -- writes to all regions, adds calibrated tech info
    A22.globaltech_coef %>%
      filter(supplysector == "gas processing") %>%
      write_to_all_regions(c("region", "supplysector", "subsector", "technology", "minicam.energy.input"), GCAM_region_names) %>%
      rename(stub.technology = technology) %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(L222.out_EJ_R_gasproc_F_Yh, by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
      # rounds outputs and adds year column for shareweights
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT), year.share.weight = year) %>%
      select(region, supplysector, subsector, stub.technology, year, calOutputValue, year.share.weight) %>%
      # sets shareweight to 1 if output exists, otherwise 0
      mutate(share.weight = if_else(calOutputValue > 0, 1, 0), subs.share.weight = share.weight) ->
      L222.StubTechProd_gasproc
    # reorders columns to match expected model interface input
    L222.StubTechProd_gasproc <- L222.StubTechProd_gasproc[c(LEVEL2_DATA_NAMES[["StubTechYr"]], "calOutputValue", "year.share.weight", "subs.share.weight", "share.weight")]

    # Oil refining calibrated output by technology
    # interpolates values of IO coefficients for base years from historical values
    L122.out_EJ_R_refining_F_Yh %>%
      complete(nesting(GCAM_region_ID, sector, fuel), year = c(year, MODEL_BASE_YEARS)) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      # append region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L222.out_EJ_R_refining_F_Yh

    # L222.StubTechProd_refining: calibrated output of refining technologies
    # matches calibrated tech info (sector, subsector, stub.technology) to refining outputs for base years and adds to output file
    calibrated_techs %>%
      semi_join(L222.out_EJ_R_refining_F_Yh, by = c("sector", "fuel")) %>%
      select(sector, fuel, supplysector, subsector, technology) %>%
      rename(stub.technology = technology) %>%
      left_join(L222.out_EJ_R_refining_F_Yh, by = c("sector", "fuel")) %>%
      # rounds and renames outputs and adds year column for shareweights
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT), year.share.weight = year) %>%
      select(-sector, -GCAM_region_ID, -fuel, -value) %>%
      # sets shareweight to 1 if output exists, otherwise 0
      mutate(share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      set_subsector_shrwt() ->
      L222.StubTechProd_refining
    # reorders columns to match expected model interface input
    L222.StubTechProd_refining <- L222.StubTechProd_refining[c(LEVEL2_DATA_NAMES[["StubTechYr"]], "calOutputValue", "year.share.weight", "subs.share.weight", "share.weight")]

    # L222.StubTechCoef_refining: calibrated input-output coefficients of oil refining by region and input
    # interpolates values of IO coefficients for base years from historical values
    L122.IO_R_oilrefining_F_Yh %>%
      complete(nesting(GCAM_region_ID, sector, fuel), year = c(year, MODEL_BASE_YEARS)) %>%
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      # append region names
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L222.IO_R_oilrefining_F_Yh

    # matches calibrated tech info (sector, subsector, stub.technology) to input-output coefficients for base years
    calibrated_techs %>%
      semi_join(L222.IO_R_oilrefining_F_Yh, by = c("sector", "fuel")) %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      left_join(L222.IO_R_oilrefining_F_Yh, by = c("sector", "fuel")) %>%
      rename(stub.technology = technology)  %>%
      # rounds and renames outputs and adds market name
      mutate(coefficient = round(value, energy.DIGITS_COEFFICIENT), market.name = region) %>%
      select(-sector, -GCAM_region_ID, -fuel, -value) ->
      L222.StubTechCoef_refining
    # reorders columns to match expected model interface input
    L222.StubTechCoef_refining <- L222.StubTechCoef_refining[c(LEVEL2_DATA_NAMES[["StubTechYr"]], "minicam.energy.input", "coefficient", "market.name")]

    # ===================================================

    # Produce outputs

    L222.Supplysector_en %>%
      add_title("Supply sector information for energy transformation sectors") %>%
      add_units("Unitless") %>%
      add_comments("Written to all regions from A22.sector") %>%
      add_legacy_name("L222.Supplysector_en") %>%
      add_precursors("energy/A22.sector", "common/GCAM_region_names") ->
      L222.Supplysector_en

    L222.SectorUseTrialMarket_en %>%
      add_title("Refining sector trial markets") %>%
      add_units("unitless") %>%
      add_comments("Trial market in the refining sector helps the model solve the simultaneities associated with refining") %>%
      same_precursors_as(L222.Supplysector_en) ->
      L222.SectorUseTrialMarket_en

    L222.SubsectorLogit_en %>%
      add_title("Subsector logit exponents of energy transformation sectors") %>%
      add_units("Unitless") %>%
      add_comments("Written to all regions from A22.subsector_logit") %>%
      add_legacy_name("L222.SubsectorLogit_en") %>%
      add_precursors("energy/A22.subsector_logit", "common/GCAM_region_names") ->
      L222.SubsectorLogit_en

    if(exists("L222.SubsectorShrwt_en")) {
    L222.SubsectorShrwt_en %>%
      add_title("Subsector shareweights of energy transformation sectors") %>%
      add_units("Unitless") %>%
      add_comments("Conditionally created from the subset of A22.subsector_shrwt with values in column 'year'.") %>%
      add_comments("Does not exist by default") %>%
      add_legacy_name("L222.SubsectorShrwt_en") %>%
      add_precursors("energy/A22.subsector_shrwt", "common/GCAM_region_names") ->
      L222.SubsectorShrwt_en
  } else {
    missing_data() %>%
      add_legacy_name("energy/L222.SubsectorShrwt_en") ->
      L222.SubsectorShrwt_en
  }

    if(exists("L222.SubsectorShrwtFllt_en")) {
    L222.SubsectorShrwtFllt_en %>%
      add_title("Subsector shareweights of energy transformation sectors") %>%
      add_units("Unitless") %>%
      add_comments("Conditionally created from the subset of A22.subsector_shrwt with values in column 'year.fillout'.") %>%
      add_comments("by default contains all values from A22.subsector_shrwt") %>%
      add_legacy_name("L222.SubsectorShrwtFllt_en") %>%
      add_precursors("energy/A22.subsector_shrwt", "common/GCAM_region_names") ->
      L222.SubsectorShrwtFllt_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L222.SubsectorShrwt_en") ->
        L222.SubsectorShrwtFllt_en
    }

    if(exists("L222.SubsectorInterp_en")) {
    L222.SubsectorInterp_en %>%
      add_title("Subsector shareweight interpolation rules of energy transformation sectors") %>%
      add_units("Unitless") %>%
        add_comments("Conditionally created from the subset of A22.subsector_interp used to define regional shareweights interpolated to a year") %>%
        add_comments("by default contains all of A22.subsector_interp.") %>%
      add_legacy_name("L222.SubsectorInterp_en") %>%
      add_precursors("energy/A22.subsector_interp", "common/GCAM_region_names") ->
      L222.SubsectorInterp_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L222.SubsectorInterp_en") ->
        L222.SubsectorInterp_en
    }

    if(exists("L222.SubsectorInterpTo_en")) {
    L222.SubsectorInterpTo_en %>%
      add_title("Subsector shareweights interpolation rules of energy transformation sectors") %>%
      add_units("Unitless") %>%
      add_comments("If A22.subsector_interp is altered to interpolated to a to.value instead of a to.year, will contain any resulting interpolation information") %>%
      add_legacy_name("L222.SubsectorInterpTo_en") %>%
      add_precursors("energy/A22.subsector_interp", "common/GCAM_region_names") ->
      L222.SubsectorInterpTo_en
  } else {
    missing_data() %>%
      add_legacy_name("energy/L222.SubsectorInterpTo_en") ->
      L222.SubsectorInterpTo_en
  }

    L222.StubTech_en %>%
      add_title("Identification of stub technologies of energy transformation") %>%
      add_units("Unitless") %>%
      add_comments("Writes out subset of stub technologies to all regions where those technologies exist") %>%
      add_comments("removes some first gen bio techs from regions where they do not exist") %>%
      add_legacy_name("L222.StubTech_en") %>%
      add_precursors("energy/A22.globaltech_shrwt", "energy/A_regions") ->
      L222.StubTech_en

    L222.GlobalTechInterp_en %>%
      add_title("Technology shareweight interpolation of energy transformation sectors") %>%
      add_units("Unitless") %>%
      add_comments("fills out model years in A22.globaltech_interp") %>%
      add_legacy_name("L222.GlobalTechInterp_en") %>%
      add_precursors("energy/A22.globaltech_interp") ->
      L222.GlobalTechInterp_en

    L222.GlobalTechCoef_en %>%
      add_title("Energy inputs and coefficients of global technologies for energy transformation") %>%
      add_units("Coefficients") %>%
      add_comments("Historical and future values interpolated from coefficients in A22.globaltech_coef") %>%
      add_legacy_name("L222.GlobalTechCoef_en") %>%
      add_precursors("energy/A22.globaltech_coef") ->
      L222.GlobalTechCoef_en

    L222.GlobalTechCost_en %>%
      add_title("Costs of global technologies for energy transformation") %>%
      add_units("1975USD/GJ") %>%
      add_comments("Values interpolated to model years from assumptions in A22.globaltech_cost") %>%
      add_legacy_name("L222.GlobalTechCost_en") %>%
      add_precursors("energy/A22.globaltech_cost") ->
      L222.GlobalTechCost_en

    L222.GlobalTechShrwt_en %>%
      add_title("Shareweights of global technologies for energy transformation") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights interpolated to model years from assumptions in A22.globaltech_shrwt") %>%
      add_legacy_name("L222.GlobalTechShrwt_en") %>%
      add_precursors("energy/A22.globaltech_shrwt") ->
      L222.GlobalTechShrwt_en

    L222.GlobalTechCapture_en %>%
      add_title("CO2 capture fractions from global technologies for energy transformation") %>%
      add_units("Unitless") %>%
      add_comments("Fraction of CO2 captured by global CCS tech in energy transformation interpolated from assumptions in A22.globaltech_co2capture") %>%
      add_legacy_name("L222.GlobalTechCapture_en") %>%
      add_precursors("energy/A22.globaltech_co2capture") ->
      L222.GlobalTechCapture_en

    if(exists("L222.GlobalTechShutdown_en")) {
    L222.GlobalTechShutdown_en %>%
      add_title("Global tech lifetime for techs with shutdown rate") %>%
      add_units("Lifetime in years") %>%
      add_comments("Filters for any technology that uses a phased retirement function") %>%
      add_legacy_name("L222.GlobalTechShutdown_en") %>%
      add_precursors("energy/A22.globaltech_retirement") ->
      L222.GlobalTechShutdown_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L222.GlobalTechShutdown_en") ->
        L222.GlobalTechShutdown_en
    }

    if(exists("L222.GlobalTechSCurve_en")) {
    L222.GlobalTechSCurve_en %>%
      add_title("Global tech lifetime for techs with s-curve retirement function") %>%
      add_units("Lifetime in years, half-life in years") %>%
      add_comments("Filters for any technology that uses an S-curve retirement function") %>%
      add_legacy_name("L222.GlobalTechSCurve_en") %>%
      add_precursors("energy/A22.globaltech_retirement") ->
      L222.GlobalTechSCurve_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L222.GlobalTechSCurve_en") ->
        L222.GlobalTechSCurve_en
    }

    if(exists("L222.GlobalTechLifetime_en")) {
    L222.GlobalTechLifetime_en %>%
      add_title("Global tech lifetime for any technology with no retirement function") %>%
      add_units("Lifetime in years") %>%
      add_comments("Filters for any technology that has no phased or S-curve retirement function, empty by default.") %>%
      add_legacy_name("L222.GlobalTechLifetime_en") %>%
      add_precursors("energy/A22.globaltech_retirement") ->
      L222.GlobalTechLifetime_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L222.GlobalTechLifetime_en") ->
        L222.GlobalTechLifetime_en
    }

    if(exists("L222.GlobalTechProfitShutdown_en")) {
    L222.GlobalTechProfitShutdown_en %>%
      add_title("Global tech profit shutdown decider and parameters") %>%
      add_units("Unitless, used to determine shape of the function defining the relationship between shutdown rate and profitability") %>%
      add_comments("Filters for any technologies that use a profit-based shutdown parameter") %>%
      add_legacy_name("L222.GlobalTechProfitShutdown_en") %>%
      add_precursors("energy/A22.globaltech_retirement") ->
      L222.GlobalTechProfitShutdown_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L222.GlobalTechProfitShutdown_en") ->
        L222.GlobalTechProfitShutdown_en
    }

    L222.GlobalTechKeyword_en %>%
        add_title("Global tech keywords") %>%
        add_units("NA") %>%
        add_comments("Primary energy keywords to ensure first get bio fuels get picked up in") %>%
        add_comments("the primary energy queries") %>%
        add_precursors("energy/A22.globaltech_keyword") ->
        L222.GlobalTechKeyword_en

    L222.StubTechProd_gasproc %>%
      add_title("Historical calibrated output of gas processing technologies") %>%
      add_units("EJ") %>%
      add_comments("Historical values of output of gas processing for base model years by region") %>%
      add_legacy_name("L222.StubTechProd_gasproc") %>%
      add_precursors("energy/A22.globaltech_coef", "L122.out_EJ_R_gasproc_F_Yh", "energy/calibrated_techs", "common/GCAM_region_names") ->
      L222.StubTechProd_gasproc

    L222.StubTechProd_refining %>%
      add_title("Historical calibrated output of refining technologies") %>%
      add_units("EJ") %>%
      add_comments("Historical values of output for liquid refining for base model years by region") %>%
      add_legacy_name("L222.StubTechProd_refining") %>%
      add_precursors("L122.out_EJ_R_refining_F_Yh", "energy/calibrated_techs", "common/GCAM_region_names") ->
      L222.StubTechProd_refining

    L222.StubTechCoef_refining %>%
      add_title("Calibrated input-output coefficients of oil refining") %>%
      add_units("unitless ratio") %>%
      add_comments("Historical values of ratio of inputs to outputs in the oil refining sector by region") %>%
      add_legacy_name("L222.StubTechCoef_refining") %>%
      add_precursors("L122.IO_R_oilrefining_F_Yh", "energy/calibrated_techs", "common/GCAM_region_names") ->
      L222.StubTechCoef_refining

    L222.GlobalTechCost_low_en %>%
      add_title("Low-tech costs of global technologies for energy transformation") %>%
      add_units("1975 USD/GJ") %>%
      add_comments("Low tech cost projections interpolated to model years from pessmistic cost assumptions contained in A22.globaltech_cost_low") %>%
      add_legacy_name("L222.GlobalTechCost_low_en") %>%
      add_precursors("energy/A22.globaltech_cost_low") ->
      L222.GlobalTechCost_low_en

    return_data(L222.Supplysector_en, L222.SectorUseTrialMarket_en, L222.SubsectorLogit_en, L222.SubsectorShrwt_en,
                L222.SubsectorShrwtFllt_en, L222.SubsectorInterp_en, L222.SubsectorInterpTo_en,
                L222.StubTech_en, L222.GlobalTechInterp_en, L222.GlobalTechCoef_en, L222.GlobalTechCost_en,
                L222.GlobalTechShrwt_en, L222.GlobalTechCapture_en, L222.GlobalTechShutdown_en,
                L222.GlobalTechSCurve_en, L222.GlobalTechLifetime_en, L222.GlobalTechProfitShutdown_en,
                L222.StubTechProd_gasproc, L222.StubTechProd_refining, L222.StubTechCoef_refining,
                L222.GlobalTechCost_low_en, L222.GlobalTechKeyword_en)
  } else {
    stop("Unknown command")
  }
}
