#' module_energy_L2322.Fert
#'
#' The chunk provides supply sector information/keywords, subsector shareweights, global technology lifetime, energy inputs and coefficients, global fertilizer manufacturing technologies, and etc. for fertilizer sector.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2322.SectorLogitTables[[ curr_table ]]$data}, \code{L2322.Supplysector_Fert}, \code{L2322.FinalEnergyKeyword_Fert}, \code{L2322.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2322.SubsectorLogit_Fert}, \code{L2322.SubsectorShrwt_Fert}, \code{L2322.SubsectorShrwtFllt_Fert}, \code{L2322.SubsectorInterp_Fert}, \code{L2322.SubsectorInterpTo_Fert}, \code{L2322.StubTech_Fert}, \code{L2322.GlobalTechShrwt_Fert}, \code{L2322.GlobalTechCoef_Fert}, \code{L2322.GlobalTechCost_Fert}, \code{L2322.GlobalTechCapture_Fert}, \code{L2322.GlobalTechShutdown_Fert}, \code{L2322.GlobalTechSCurve_Fert}, \code{L2322.GlobalTechLifetime_Fert}, \code{L2322.GlobalTechProfitShutdown_Fert}, \code{L2322.StubTechProd_Fert}, \code{L2322.StubTechCoef_Fert}, \code{L2322.StubTechFixOut_Fert_imp}, \code{L2322.StubTechFixOut_Fert_exp}, \code{L2322.PerCapitaBased_Fert}, \code{L2322.BaseService_Fert}. The corresponding file in the
#' original data system was \code{L2322.Fert.R} (energy level2).
#' @details The chunk provides supply sector information/keywords, subsector shareweights, global technology lifetime, energy inputs and coefficients, global fertilizer manufacturing technologies, and etc. for fertilizer sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
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
             FILE = "energy/A322.globaltech_renew",
             FILE = "energy/A322.globaltech_retirement",
             "L1322.Fert_Prod_MtN_R_F_Y",
             "L1322.IO_R_Fert_F_Yh",
             "L1322.Fert_NEcost_75USDkgN_F",
             "L142.ag_Fert_NetExp_MtN_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2322.Supplysector_Fert",
             "L2322.FinalEnergyKeyword_Fert",
             "L2322.SubsectorLogit_Fert",
             "L2322.SubsectorShrwt_Fert",
             "L2322.SubsectorShrwtFllt_Fert",
             "L2322.SubsectorInterp_Fert",
             "L2322.SubsectorInterpTo_Fert",
             "L2322.StubTech_Fert",
             "L2322.GlobalTechShrwt_Fert",
             "L2322.GlobalTechCoef_Fert",
             "L2322.GlobalTechCost_Fert",
             "L2322.GlobalTechCapture_Fert",
             "L2322.GlobalTechShutdown_Fert",
             "L2322.GlobalTechSCurve_Fert",
             "L2322.GlobalTechLifetime_Fert",
             "L2322.GlobalTechProfitShutdown_Fert",
             "L2322.StubTechProd_Fert",
             "L2322.StubTechCoef_Fert",
             "L2322.StubTechFixOut_Fert_imp",
             "L2322.StubTechFixOut_Fert_exp",
             "L2322.PerCapitaBased_Fert",
             "L2322.BaseService_Fert"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A322.sector <- get_data(all_data, "energy/A322.sector")
    A322.subsector_interp <- get_data(all_data, "energy/A322.subsector_interp")
    A322.subsector_logit <- get_data(all_data, "energy/A322.subsector_logit")
    A322.subsector_shrwt <- get_data(all_data, "energy/A322.subsector_shrwt")
    A322.globaltech_coef <- get_data(all_data, "energy/A322.globaltech_coef")
    A322.globaltech_shrwt <- get_data(all_data, "energy/A322.globaltech_shrwt")
    A322.globaltech_co2capture <- get_data(all_data, "energy/A322.globaltech_co2capture")
    A322.globaltech_renew <- get_data(all_data, "energy/A322.globaltech_renew")
    A322.globaltech_retirement <- get_data(all_data, "energy/A322.globaltech_retirement")
    L1322.Fert_Prod_MtN_R_F_Y <- get_data(all_data, "L1322.Fert_Prod_MtN_R_F_Y")
    L1322.IO_R_Fert_F_Yh <- get_data(all_data, "L1322.IO_R_Fert_F_Yh")
    L1322.Fert_NEcost_75USDkgN_F <- get_data(all_data, "L1322.Fert_NEcost_75USDkgN_F")
    L142.ag_Fert_NetExp_MtN_R_Y <- get_data(all_data, "L142.ag_Fert_NetExp_MtN_R_Y")

    # ===================================================
    # 0. Give binding for variable names used in pipeline

    LEVEL2_DATA_NAMES <- year.fillout <- to.value <- technology <- year <-
      share.weight <- supplysector <- subsector <- coefficient <- minicam.energy.input <-
      NEcost_75USDkgN <- input.cost <- remove.fraction <- half.life <- median.shutdown.point <-
      value <- calOutputValue <- sector <- fuel <- subs.share.weight <- region <- fixedOutput <- . <- NULL

    # ===================================================
    # 1. Perform computations
    # Create tables to delete technologies and subsectors in regions where heat is not modeled as a fuel
    # 1a. Supplysector information
    # L2322.Supplysector_Fert: Supply sector information for fertilizer sector
    A322.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Supplysector"]], GCAM_region_names) ->
      L2322.Supplysector_Fert

    # L2322.FinalEnergyKeyword_Fert: Supply sector keywords for fertilizer sector
    A322.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]], GCAM_region_names) %>%
      na.omit ->
      L2322.FinalEnergyKeyword_Fert

    # 2b. Subsector information
    # L2322.SubsectorLogit_Fert: Subsector logit exponents of fertilizer sector
    A322.subsector_logit %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorLogit"]], GCAM_region_names) ->
      L2322.SubsectorLogit_Fert

    # L2322.SubsectorShrwt_Fert and L2322.SubsectorShrwtFllt_Fert: Subsector shareweights of fertilizer sector
    tibble(x = NA) %>%
      add_units("None") %>%
      add_comments("Not generated") %>%
      add_flags(FLAG_NO_TEST) ->
      L2322.SubsectorShrwt_Fert

    A322.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) ->
      L2322.SubsectorShrwtFllt_Fert

    # L2322.SubsectorInterp_Fert and L2322.SubsectorInterpTo_Fert: Subsector shareweight interpolation of fertilizer sector
    A322.subsector_interp %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names) ->
      L2322.SubsectorInterp_Fert

    tibble(x = NA) %>%
      add_units("None") %>%
      add_comments("Not generated") %>%
      add_flags(FLAG_NO_TEST) ->
      L2322.SubsectorInterpTo_Fert

    # 2c. Technology information
    # L2322.StubTech_Fert: Identification of stub technologies of fertilizer sector
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A322.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L2322.StubTech_Fert

    # L2322.GlobalTechShrwt_Fert: Shareweights of global fertilizer sector technologies
    # Notes of the workflow: The pipeline below functions as the interpolate_and_melt
    #                        function in the old data system. The pipeline first extracts the years
    #                        needed in interpolation then constructs the layout with proper id tags (columns)
    #                        for interpolation years, and finally performs interpolation using 'rule=1' as in
    #                        old data system. For more information about 'rule' please see ?approx_fun.
    #                        Same workflow applies at several places in this chunk.
    A322.globaltech_shrwt %>%
      gather(year, share.weight, matches(YEAR_PATTERN)) %>%
      mutate(year = as.numeric(year)) ->
      A322.globaltech_shrwt_long

    df_years <- unique(A322.globaltech_shrwt_long$year)
    int_years <- setdiff(c(BASE_YEARS, FUTURE_YEARS), df_years)

    A322.globaltech_shrwt_long %>%
      select(-share.weight, -year) %>%
      unique() %>%
      repeat_add_columns(tibble(year = int_years)) %>%
      bind_rows(A322.globaltech_shrwt_long) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(BASE_YEARS, FUTURE_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L2322.GlobalTechShrwt_Fert

    # L2322.GlobalTechCoef_Fert: Energy inputs and coefficients of global fertilizer energy use and feedstocks technologies
    A322.globaltech_coef %>%
      gather(year, coefficient, matches(YEAR_PATTERN)) %>%
      mutate(year = as.numeric(year)) ->
      A322.globaltech_coef_long

    df_years <- unique(A322.globaltech_coef_long$year)
    int_years <- setdiff(c(BASE_YEARS, FUTURE_YEARS), df_years)

    A322.globaltech_coef_long %>%
      select(-coefficient, -year) %>%
      unique() %>%
      repeat_add_columns(tibble(year = int_years)) %>%
      bind_rows(A322.globaltech_coef_long) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 1)) %>%
      mutate(coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      ungroup %>%
      filter(year %in% c(BASE_YEARS, FUTURE_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L2322.GlobalTechCoef_Fert

    # Costs of global technologies
    # L2322.GlobalTechCost_Fert: Non-energy costs of global fertilizer manufacturing technologies
    L2322.GlobalTechCoef_Fert %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]]) %>%
      mutate(minicam.non.energy.input = "non-energy") %>%
      left_join(L1322.Fert_NEcost_75USDkgN_F, by = c('technology' = 'fuel') ) %>%
      rename(input.cost = NEcost_75USDkgN) %>%
      mutate(input.cost = round(input.cost, energy.DIGITS_COST)) %>%
      na.omit -> #Export technologies have no cost assigned. Just drop the object
      L2322.GlobalTechCost_Fert

    # Carbon capture rates from technologies with CCS
    # L2322.GlobalTechCapture_Fert: CO2 capture fractions from global fertilizer production technologies with CCS
    ## No need to consider historical periods or intermittent technologies here
    A322.globaltech_co2capture %>%
      gather(year, remove.fraction, matches(YEAR_PATTERN)) %>%
      mutate(year = as.numeric(year)) ->
      A322.globaltech_co2capture_long

    df_years <- unique(A322.globaltech_co2capture_long$year)
    int_years <- setdiff(FUTURE_YEARS, df_years)

    A322.globaltech_co2capture_long %>%
      select(-remove.fraction, -year) %>%
      unique() %>%
      repeat_add_columns(tibble(year = int_years)) %>%
      bind_rows(A322.globaltech_co2capture_long) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(year, remove.fraction, rule = 1)) %>%
      mutate(remove.fraction = round(remove.fraction, energy.DIGITS_REMOVE.FRACTION)) %>%
      ungroup %>%
      filter(year %in% FUTURE_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      mutate(storage.market = "carbon-storage") ->
      L2322.GlobalTechCapture_Fert

    # Retirement information
    A322.globaltech_retirement %>%
      set_years %>%
      mutate(year = as.numeric(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      A322.globaltech_retirement_with_years

    # Copy the data in the first future period through to the end year
    A322.globaltech_retirement_with_years %>%
      filter(year == max(BASE_YEARS)) ->
      A322.globaltech_retirement_max_baseyear

    A322.globaltech_retirement_with_years %>%
      filter(year == min(FUTURE_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = FUTURE_YEARS)) %>%
      bind_rows(A322.globaltech_retirement_max_baseyear) ->
      L2322.globaltech_retirement

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # All of these options have different headers, and all are allowed

    # L2322.GlobalTechShutdown_Fert: Global tech lifetime and shutdown rate
    tibble(x = NA) %>%
      add_units("None") %>%
      add_comments("Not generated") %>%
      add_flags(FLAG_NO_TEST) ->
      L2322.GlobalTechShutdown_Fert

    # L2322.GlobalTechSCurve_Fert: Global tech lifetime and s-curve retirement function
    L2322.globaltech_retirement %>%
      filter(!is.na(half.life)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
      L2322.GlobalTechSCurve_Fert

    # L2322.GlobalTechLifetime_Fert: Global tech lifetime
    tibble(x = NA) %>%
      add_units("None") %>%
      add_comments("Not generated") %>%
      add_flags(FLAG_NO_TEST) ->
      L2322.GlobalTechLifetime_Fert

    # L2322.GlobalTechProfitShutdown_Fert: Global tech profit shutdown decider.
    L2322.globaltech_retirement %>%
      filter(!is.na(median.shutdown.point)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
      L2322.GlobalTechProfitShutdown_Fert

    # Calibration and region-specific data
    # L2322.StubTechProd_Fert: calibrated output of fertilizer technologies
    L1322.Fert_Prod_MtN_R_F_Y %>%
      filter(year %in% BASE_YEARS) %>%
      rename(calOutputValue = value) %>%
      mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = 'GCAM_region_ID') %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, supplysector, subsector, technology), by = c("sector", "fuel") ) %>%
      rename(stub.technology = technology) %>%
      mutate(share.weight.year = year) %>%
      mutate(subs.share.weight = 0) %>%
      mutate(subs.share.weight = replace(subs.share.weight, which(calOutputValue > 0), 1)) %>%
      mutate(tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2322.StubTechProd_Fert

    # L2322.StubTechCoef_Fert: calibrated base-year coefficients of fertilizer production technologies
    L1322.IO_R_Fert_F_Yh %>%
      filter(year %in% BASE_YEARS) %>%
      rename(coefficient = value) %>%
      mutate(coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      filter(coefficient != 0) %>% # Where 0, drop from this table (to revert to assumed defaults)
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, supplysector, subsector, technology, minicam.energy.input), by = c("sector", "fuel")) %>%
      mutate(stub.technology = technology) %>%
      mutate(market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L2322.StubTechCoef_Fert

    # L2322.StubTechFixOut_Fert_imp: fixed output of import technology (fixed imports)
    # Imports are negative net exports
    L142.ag_Fert_NetExp_MtN_R_Y %>%
      filter(year %in% BASE_YEARS) %>%
      rename(fixedOutput = value) %>%
      left_join_error_no_match(GCAM_region_names, by = 'GCAM_region_ID') %>%
      mutate(supplysector = A322.globaltech_renew[["supplysector"]], subsector = A322.globaltech_renew[["subsector"]], stub.technology = A322.globaltech_renew[["technology"]]) %>%
      mutate(fixedOutput = pmax(0, -1 * fixedOutput)) %>%
      mutate(fixedOutput = round(fixedOutput, energy.DIGITS_CALOUTPUT)) %>%
      mutate(share.weight.year = year) %>%
      mutate(subs.share.weight = 0) %>%
      mutate(tech.share.weight = 0) %>%
      bind_rows(repeat_add_columns(select(filter(., year == max(BASE_YEARS)), -year), tibble(year = FUTURE_YEARS))) %>%
      select(LEVEL2_DATA_NAMES[["StubTechFixOut"]])-> #Repeat final year to all future years and rbind
      L2322.StubTechFixOut_Fert_imp

    # L2322.StubTechFixOut_Fert_exp: fixed output of import technology (fixed imports)
    # Exports are positive net exports
    L142.ag_Fert_NetExp_MtN_R_Y %>%
      filter(year %in% BASE_YEARS) %>%
      rename(fixedOutput = value) %>%
      mutate(fixedOutput = round(fixedOutput, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID" ) %>%
      mutate(supplysector = filter(A322.globaltech_shrwt, grepl( "Exports", supplysector))[["supplysector"]]) %>%
      mutate(subsector = filter(A322.globaltech_shrwt, grepl( "Exports", supplysector))[["subsector"]]) %>%
      mutate(stub.technology = filter(A322.globaltech_shrwt, grepl( "Exports", supplysector))[["technology"]]) %>%
      mutate(fixedOutput = pmax(0, fixedOutput)) %>%
      mutate(share.weight.year = year) %>%
      mutate(subs.share.weight = 0) %>%
      mutate(tech.share.weight = 0) ->
      L2322.StubTechFixOut_Fert_exp_base

    L2322.StubTechFixOut_Fert_exp_base %>%
      bind_rows(repeat_add_columns(select(filter(., year == max(BASE_YEARS)), -year), tibble(year = FUTURE_YEARS))) %>% # Repeat final year to all future years and rbind
      select(LEVEL2_DATA_NAMES[["StubTechFixOut"]]) ->
      L2322.StubTechFixOut_Fert_exp

    # L2322.PerCapitaBased_Fert: per-capita based flag for fertilizer exports final demand
    tibble(region = GCAM_region_names[["region"]]) %>%
      mutate(energy.final.demand = filter(A322.globaltech_shrwt, grepl( "Exports", supplysector))[["supplysector"]]) %>%
      mutate(perCapitaBased = 0) ->
      L2322.PerCapitaBased_Fert

    # L2322.BaseService_Fert: base-year service output of fertilizer exports final demand
    # Base service is equal to the output of the exports supplysector
    tibble(region = L2322.StubTechFixOut_Fert_exp_base[["region"]]) %>%
      mutate(energy.final.demand = L2322.StubTechFixOut_Fert_exp_base[["supplysector"]]) %>%
      mutate(year = L2322.StubTechFixOut_Fert_exp_base[["year"]]) %>%
      mutate(base.service = L2322.StubTechFixOut_Fert_exp_base[["fixedOutput"]]) ->
      L2322.BaseService_Fert

    # ===================================================
    # Produce outputs

    L2322.Supplysector_Fert %>%
      add_title("Supply sector information for fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("Expanded fertilizer sector supply sector information for all GCAM regions") %>%
      add_legacy_name("L2322.Supplysector_Fert") %>%
      add_precursors("common/GCAM_region_names", "energy/A322.sector") ->
      L2322.Supplysector_Fert

    L2322.FinalEnergyKeyword_Fert %>%
      add_title("Supply sector keywords for fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("Expanded fertilizer sector supply sector keywords for all GCAM regions") %>%
      add_legacy_name("L2322.FinalEnergyKeyword_Fert") %>%
      add_precursors("common/GCAM_region_names", "energy/A322.sector") ->
      L2322.FinalEnergyKeyword_Fert

    L2322.SubsectorLogit_Fert %>%
      add_title("Subsector logit exponents of fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("Expanded fertilizer sector subsector logit exponents for all GCAM regions") %>%
      add_legacy_name("L2322.SubsectorLogit_Fert") %>%
      add_precursors("energy/A322.subsector_logit", "common/GCAM_region_names") ->
      L2322.SubsectorLogit_Fert

    L2322.SubsectorShrwt_Fert %>%
      add_title("Subsector shareweights of fertilizer") %>%
      add_units("NA") %>%
      add_comments("Created as empty table") %>%
      add_legacy_name("L2322.SubsectorShrwt_Fert") %>%
      add_precursors("energy/A322.subsector_shrwt", 'common/GCAM_region_names') ->
      L2322.SubsectorShrwt_Fert

    L2322.SubsectorShrwtFllt_Fert %>%
      add_title("Subsector shareweights of fertilizer") %>%
      add_units("Unitless") %>%
      add_comments("Expanded Subsector shareweights of fertilizer to all GCAM regions") %>%
      add_legacy_name("L2322.SubsectorShrwtFllt_Fert") %>%
      add_precursors("energy/A322.subsector_shrwt", "common/GCAM_region_names") ->
      L2322.SubsectorShrwtFllt_Fert

    L2322.SubsectorInterp_Fert %>%
      add_title("Subsector shareweight interpolation of fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("Expanded subsector shareweight interpolation of fertilizer sector to all GCAM regions") %>%
      add_legacy_name("L2322.SubsectorInterp_Fert") %>%
      add_precursors("energy/A322.subsector_interp", "common/GCAM_region_names") ->
      L2322.SubsectorInterp_Fert

    L2322.SubsectorInterpTo_Fert %>%
      add_title("Subsector shareweight interpolation of fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("Created as empty table") %>%
      add_legacy_name("L2322.SubsectorInterpTo_Fert") %>%
      add_precursors("energy/A322.subsector_interp", "common/GCAM_region_names") ->
      L2322.SubsectorInterpTo_Fert

    L2322.StubTech_Fert %>%
      add_title("Identification of stub technologies of fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("Expanded identification of stub technologies of fertilizer sector to all GCAM regions") %>%
      add_legacy_name("L2322.StubTech_Fert") %>%
      add_precursors("energy/A322.globaltech_shrwt", "common/GCAM_region_names") ->
      L2322.StubTech_Fert

    L2322.GlobalTechShrwt_Fert %>%
      add_title("Shareweights of global fertilizer sector technologies") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_legacy_name("L2322.GlobalTechShrwt_Fert") %>%
      add_precursors("energy/A322.globaltech_shrwt") ->
      L2322.GlobalTechShrwt_Fert

    L2322.GlobalTechCoef_Fert %>%
      add_title("Energy inputs and coefficients of global fertilizer energy use and feedstocks technologies") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_legacy_name("energy/A322.globaltech_coef") %>%
      add_precursors("L1322.Fert_NEcost_75USDkgN_F") ->
      L2322.GlobalTechCoef_Fert

    L2322.GlobalTechCost_Fert %>%
      add_title("Non-energy costs of global fertilizer manufacturing technologies") %>%
      add_units("1975USDkgN") %>%
      add_comments("Claculated cost from L1322.Fert_NEcost_75USDkgN_F") %>%
      add_legacy_name("L2322.GlobalTechCost_Fert") %>%
      add_precursors("L1322.Fert_NEcost_75USDkgN_F") ->
      L2322.GlobalTechCost_Fert

    L2322.GlobalTechCapture_Fert %>%
      add_title("CO2 capture fractions from global fertilizer production technologies with CCS") %>%
      add_units("Unitless") %>%
      add_comments("Interpolated orginal data into all model years") %>%
      add_legacy_name("L2322.GlobalTechCapture_Fert") %>%
      add_precursors("energy/A322.globaltech_co2capture") ->
      L2322.GlobalTechCapture_Fert

    L2322.GlobalTechShutdown_Fert %>%
      add_title("Global tech lifetime and shutdown rate") %>%
      add_units("NA") %>%
      add_comments("Created as empty table") %>%
      add_legacy_name("L2322.GlobalTechShutdown_Fert") %>%
      add_precursors("energy/A322.globaltech_retirement")->
      L2322.GlobalTechShutdown_Fert

    L2322.GlobalTechSCurve_Fert %>%
      add_title("Global tech lifetime and s-curve retirement function") %>%
      add_units("year for lifetime and halflife; Unitless for steepness") %>%
      add_comments("Extracted from L2322.globaltech_retirement") %>%
      add_legacy_name("L2322.GlobalTechSCurve_Fert") %>%
      add_precursors("energy/A322.globaltech_retirement") ->
      L2322.GlobalTechSCurve_Fert

    L2322.GlobalTechLifetime_Fert %>%
      add_title("Global tech lifetime") %>%
      add_units("NA") %>%
      add_comments("Created as empty table") %>%
      add_legacy_name("L2322.GlobalTechLifetime_Fert") %>%
      add_precursors("energy/A322.globaltech_retirement") ->
      L2322.GlobalTechLifetime_Fert

    L2322.GlobalTechProfitShutdown_Fert %>%
      add_title("Global tech profit shutdown decider") %>%
      add_units("Unitless") %>%
      add_comments("Extracted from L2322.globaltech_retirement") %>%
      add_legacy_name("L2322.GlobalTechProfitShutdown_Fert") %>%
      add_precursors("energy/A322.globaltech_retirement") ->
      L2322.GlobalTechProfitShutdown_Fert

    L2322.StubTechProd_Fert %>%
      add_title("calibrated output of fertilizer technologies") %>%
      add_units("MtN") %>%
      add_comments("Calculated from L1322.Fert_Prod_MtN_R_F_Y") %>%
      add_legacy_name("L2322.StubTechProd_Fert") %>%
      add_precursors("L1322.Fert_Prod_MtN_R_F_Y", "common/GCAM_region_names") ->
      L2322.StubTechProd_Fert

    L2322.StubTechCoef_Fert %>%
      add_title("calibrated base-year coefficients of fertilizer production technologies") %>%
      add_units("Unitless") %>%
      add_comments("Calculated from L1322.IO_R_Fert_F_Yh") %>%
      add_legacy_name("L2322.StubTechCoef_Fert") %>%
      add_precursors("L1322.IO_R_Fert_F_Yh", "common/GCAM_region_names") ->
      L2322.StubTechCoef_Fert

    L2322.StubTechFixOut_Fert_imp %>%
      add_title("fixed output of import technology (fixed imports)") %>%
      add_units("Unitless") %>%
      add_comments("Calculated from L142.ag_Fert_NetExp_MtN_R_Y") %>%
      add_legacy_name("L2322.StubTechFixOut_Fert_imp") %>%
      add_precursors("L142.ag_Fert_NetExp_MtN_R_Y", "common/GCAM_region_names", "energy/A322.globaltech_renew") ->
      L2322.StubTechFixOut_Fert_imp

    L2322.StubTechFixOut_Fert_exp %>%
      add_title("fixed output of import technology (fixed imports)") %>%
      add_units("Unitless") %>%
      add_comments("Calculated from ") %>%
      add_legacy_name("L2322.StubTechFixOut_Fert_exp") %>%
      add_precursors("L142.ag_Fert_NetExp_MtN_R_Y", "common/GCAM_region_names", "energy/A322.globaltech_shrwt") ->
      L2322.StubTechFixOut_Fert_exp

    L2322.PerCapitaBased_Fert %>%
      add_title("per-capita based flag for fertilizer exports final demand") %>%
      add_units("Unitless") %>%
      add_comments("Created based onA322.globaltech_shrwt") %>%
      add_legacy_name("L2322.PerCapitaBased_Fert") %>%
      add_precursors("common/GCAM_region_names", "energy/A322.globaltech_shrwt") ->
      L2322.PerCapitaBased_Fert

    L2322.BaseService_Fert %>%
      add_title("base-year service output of fertilizer exports final demand") %>%
      add_units("MtN") %>%
      add_comments("Created based on L2322.StubTechFixOut_Fert_exp_base") %>%
      add_legacy_name("L2322.BaseService_Fert") %>%
      add_precursors("L142.ag_Fert_NetExp_MtN_R_Y", "common/GCAM_region_names", "energy/A322.globaltech_shrwt") ->
      L2322.BaseService_Fert

    return_data(L2322.Supplysector_Fert, L2322.FinalEnergyKeyword_Fert, L2322.SubsectorLogit_Fert,
                L2322.SubsectorShrwt_Fert, L2322.SubsectorShrwtFllt_Fert, L2322.SubsectorInterp_Fert,
                L2322.SubsectorInterpTo_Fert, L2322.StubTech_Fert, L2322.GlobalTechShrwt_Fert,
                L2322.GlobalTechCoef_Fert, L2322.GlobalTechCost_Fert, L2322.GlobalTechCapture_Fert,
                L2322.GlobalTechShutdown_Fert, L2322.GlobalTechSCurve_Fert, L2322.GlobalTechLifetime_Fert,
                L2322.GlobalTechProfitShutdown_Fert, L2322.StubTechProd_Fert, L2322.StubTechCoef_Fert,
                L2322.StubTechFixOut_Fert_imp, L2322.StubTechFixOut_Fert_exp, L2322.PerCapitaBased_Fert,
                L2322.BaseService_Fert)
  } else {
    stop("Unknown command")
  }
}
