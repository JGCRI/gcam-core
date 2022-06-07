# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L232.other_industry
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for industry-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L232.SectorLogitTables[[ curr_table ]]$data},
#' \code{L232.Supplysector_ind}, \code{L232.SubsectorLogitTables[[ curr_table ]]$data},
#' \code{L232.SubsectorLogit_ind}, \code{L232.FinalEnergyKeyword_ind},
#' \code{L232.SubsectorShrwtFllt_ind}, \code{L232.SubsectorInterp_ind},
#' \code{L232.StubTech_ind}, \code{L232.GlobalTechShrwt_ind}, \code{L232.StubTechInterp_ind},
#' \code{L232.GlobalTechEff_ind}, \code{L232.GlobalTechCoef_ind}, \code{L232.GlobalTechCost_ind},
#' \code{L232.GlobalTechSecOut_ind}, \code{L232.GlobalTechCSeq_ind},
#' \code{L232.StubTechCalInput_indenergy}, \code{L232.StubTechCalInput_indfeed},
#' \code{L232.StubTechProd_industry}, \code{L232.StubTechCoef_industry},
#' \code{L232.FuelPrefElast_indenergy}, \code{L232.PerCapitaBased_ind},
#' \code{L232.PriceElasticity_ind}, \code{L232.BaseService_ind},
#' \code{L232.IncomeElasticity_ind_gcam3}, \code{L232.IncomeElasticity_ind_gssp1},
#' \code{L232.IncomeElasticity_ind_gssp2}, \code{L232.IncomeElasticity_ind_gssp3},
#' \code{L232.IncomeElasticity_ind_gssp4}, \code{L232.IncomeElasticity_ind_gssp5},
#' \code{L232.IncomeElasticity_ind_ssp1}, \code{L232.IncomeElasticity_ind_ssp2},
#' \code{L232.IncomeElasticity_ind_ssp3}, \code{L232.IncomeElasticity_ind_ssp4},
#' \code{L232.IncomeElasticity_ind_ssp5}, \code{object}. The corresponding file in the
#' original data system was \code{L232.industry.R} (energy level2).
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, supplysector/subsector share weights, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join arrange bind_rows distinct filter if_else group_by lag left_join mutate right_join select summarise
#' @importFrom tidyr complete nesting
#' @author LF October 2017
module_energy_L232.other_industry <- function(command, ...) {

  INCOME_ELASTICITY_OUTPUTS <- c("GCAM3",
                                 paste0("gSSP", 1:5),
                                 paste0("SSP", 1:5))

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A_regions",
             FILE = "energy/A23.chp_elecratio",
             FILE = "energy/A32.sector",
             FILE = "energy/A32.subsector_interp",
             FILE = "energy/A32.subsector_logit",
             FILE = "energy/A32.subsector_shrwt",
             FILE = "energy/A32.globaltech_coef",
             FILE = "energy/A32.globaltech_cost",
             FILE = "energy/A32.globaltech_eff",
             FILE = "energy/A32.globaltech_shrwt",
             FILE = "energy/A32.globaltech_interp",
             FILE = "energy/A32.nonenergy_Cseq",
             FILE = "energy/A32.fuelprefElasticity",
             FILE = "energy/A32.globaltech_retirement",
             FILE = "energy/A32.demand",
             "L123.in_EJ_R_indchp_F_Yh",
             "L1326.in_EJ_R_indenergy_F_Yh",
             "L1324.in_EJ_R_indfeed_F_Yh",
             FILE = "socioeconomics/A32.inc_elas_output",
             "L101.Pop_thous_GCAM3_R_Y",
             "L102.pcgdp_thous90USD_GCAM3_R_Y",
             "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L232.Supplysector_ind",
             "L232.SubsectorLogit_ind",
             "L232.FinalEnergyKeyword_ind",
             "L232.SubsectorShrwtFllt_ind",
             "L232.SubsectorInterp_ind",
             "L232.StubTech_ind",
             "L232.GlobalTechShrwt_ind",
             "L232.StubTechInterp_ind",
             "L232.GlobalTechEff_ind",
             "L232.GlobalTechCoef_ind",
             "L232.GlobalTechCost_ind",
             "L232.GlobalTechSecOut_ind",
             "L232.GlobalTechCSeq_ind",
             "L232.StubTechCalInput_indenergy",
             "L232.StubTechCalInput_indfeed",
             "L232.StubTechProd_industry",
             "L232.StubTechCoef_industry",
             "L232.GlobalTechShutdown_en",
             "L232.GlobalTechSCurve_en",
             "L232.GlobalTechLifetime_en",
             "L232.GlobalTechProfitShutdown_en",
             "L232.FuelPrefElast_indenergy",
             "L232.PerCapitaBased_ind",
             "L232.PriceElasticity_ind",
             "L232.BaseService_ind",
             paste("L232.IncomeElasticity_ind", tolower(INCOME_ELASTICITY_OUTPUTS), sep = "_")))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs", strip_attributes = TRUE)
    A_regions <- get_data(all_data, "energy/A_regions")
    A23.chp_elecratio <- get_data(all_data, "energy/A23.chp_elecratio")
    A32.sector <- get_data(all_data, "energy/A32.sector", strip_attributes = TRUE)
    A32.subsector_interp <- get_data(all_data, "energy/A32.subsector_interp", strip_attributes = TRUE)
    A32.subsector_logit <- get_data(all_data, "energy/A32.subsector_logit", strip_attributes = TRUE)
    A32.subsector_shrwt <- get_data(all_data, "energy/A32.subsector_shrwt", strip_attributes = TRUE)
    A32.globaltech_coef <- get_data(all_data, "energy/A32.globaltech_coef", strip_attributes = TRUE)
    A32.globaltech_cost <- get_data(all_data, "energy/A32.globaltech_cost")
    A32.globaltech_eff <- get_data(all_data, "energy/A32.globaltech_eff")
    A32.globaltech_shrwt <- get_data(all_data, "energy/A32.globaltech_shrwt", strip_attributes = TRUE)
    A32.globaltech_interp <- get_data(all_data, "energy/A32.globaltech_interp", strip_attributes = TRUE)
    A32.globaltech_retirement <- get_data(all_data, "energy/A32.globaltech_retirement", strip_attributes = TRUE)
    A32.nonenergy_Cseq <- get_data(all_data, "energy/A32.nonenergy_Cseq", strip_attributes = TRUE)
    A32.fuelprefElasticity <- get_data(all_data, "energy/A32.fuelprefElasticity")
    A32.demand <- get_data(all_data, "energy/A32.demand")
    L123.in_EJ_R_indchp_F_Yh <- get_data(all_data, "L123.in_EJ_R_indchp_F_Yh")
    L1324.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1326.in_EJ_R_indenergy_F_Yh")
    L1324.in_EJ_R_indfeed_F_Yh <- get_data(all_data, "L1324.in_EJ_R_indfeed_F_Yh", strip_attributes = TRUE)
    A32.inc_elas_output <- get_data(all_data, "socioeconomics/A32.inc_elas_output")
    L101.Pop_thous_GCAM3_R_Y <- get_data(all_data, "L101.Pop_thous_GCAM3_R_Y")
    L102.pcgdp_thous90USD_GCAM3_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_GCAM3_R_Y")
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")

    # ===================================================
    # 0. Give binding for variable names used in pipeline
    has_district_heat <- sector <- fuel <- supplysector <- subsector <-
      technology <- year.fillout <- to.value <- year <- share.weight <-
      efficiency <- minicam.energy.input <- secondary.output <- coefficient <-
      elec_ratio <- output.ratio <- . <- year.x <- output.ratio.x <- output.ratio.y <-
      input.cost <- minicam.non.energy.input <- GCAM_region_ID <- value <-
      calibrated.value <- sector.name <- subsector.name <- region <-
      calOutputValue <- subs.share.weight <- calOutputValue.x <- calOutputValue.y <-
      output_tot <- value.x <- value.y <- total <- fuelprefElasticity <-
      terminal_coef <- criteria <- scenario <- temp_lag <- base.service <- energy.final.demand <-
      parameter <- income.elasticity <- L232.IncomeElasticity_ind_gcam3 <-
      L232.IncomeElasticity_ind_gssp1 <- L232.IncomeElasticity_ind_gssp2 <-
      L232.IncomeElasticity_ind_gssp3 <- L232.IncomeElasticity_ind_gssp4 <-
      L232.IncomeElasticity_ind_gssp5 <- L232.IncomeElasticity_ind_ssp1 <-
      L232.IncomeElasticity_ind_ssp2 <- L232.IncomeElasticity_ind_ssp3 <-
      L232.IncomeElasticity_ind_ssp4 <- L232.IncomeElasticity_ind_ssp5 <-
      market.name <- stub.technology <- year.y <- NULL

    # ===================================================
    # 1. Perform computations
    # Create tables to delete technologies and subsectors in regions where heat is not modeled as a fuel
    has_not_heat <- filter(A_regions, has_district_heat == 0) # intermediate tibble

    calibrated_techs %>%
      filter(grepl("industry", sector) & fuel == "heat") %>%
      select(supplysector, subsector, technology) %>%
      repeat_add_columns(tibble(GCAM_region_ID = has_not_heat[["GCAM_region_ID"]])) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L232.rm_heat_techs_R # intermediate tibble

    # 1a. Supplysector information
    # L232.Supplysector_ind: Supply sector information for industry sector
    A32.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L232.Supplysector_ind

    # L232.FinalEnergyKeyword_ind: Supply sector keywords for industry sector
    A32.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]], GCAM_region_names) %>%
      na.omit ->
      L232.FinalEnergyKeyword_ind

    # 1b. Subsector information
    # L232.SubsectorLogit_ind: Subsector logit exponents of industry sector
    A32.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) %>%
      anti_join(L232.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L232.SubsectorLogit_ind

    # L232.SubsectorShrwtFllt_ind: Subsector shareweights of industry sector
    A32.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) %>%
      anti_join(L232.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat technologies from each region
      L232.SubsectorShrwtFllt_ind

    # L232.SubsectorInterp_ind: Subsector shareweight interpolation of industry sector
    A32.subsector_interp %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names) %>%
      anti_join(L232.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat technologies from each region
      L232.SubsectorInterp_ind

    # 1c. Technology information
    # L232.StubTech_ind: Identification of stub technologies of industrial sector
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A32.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      anti_join(L232.rm_heat_techs_R, by = c("region", "technology")) %>% # Remove non-existent heat technologies from each region
      rename(stub.technology = technology) ->
      L232.StubTech_ind

    # L232.GlobalTechShrwt_ind: Shareweights of global industrial sector technologies
    A32.globaltech_shrwt %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "share.weight") ->
      L232.GlobalTechShrwt_ind

    # L232.StubTechInterp_ind: Shareweight interpolation of global industrial sector technologies
    A32.globaltech_interp %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["TechInterp"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L232.StubTechInterp_ind

    # L232.GlobalTechEff_ind: Energy inputs and efficiency of global industrial energy use and feedstocks technologies
    A32.globaltech_eff %>%
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
      L232.globaltech_eff.long # intermediate tibble

    L232.globaltech_eff.long %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechEff"]]) ->
      L232.GlobalTechEff_ind

    # Coefficients on global industry sector technologies (not energy-use or feedstocks)
    # L232.GlobalTechCoef_ind: Energy inputs and coefficients of global industry technologies
    A32.globaltech_coef %>%
      rename(coefficient = "terminal_coef") %>%
      repeat_add_columns(tibble(year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS))) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>% # Assign the columns "sector.name" and "subsector.name", consistent with the location info of a global technology
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L232.GlobalTechCoef_ind

    # Secondary outputs of cogen technologies: these are input as a ratio
    # L232.GlobalTechSecOut_ind: Secondary output ratios of industrial cogeneration technologies
    A32.globaltech_eff %>%
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
      L232.GlobalTechSecOut_ind

    # Costs of global technologies
    # L232.GlobalTechCost_ind: Capital costs of global industrial technologies
    A32.globaltech_cost %>%
      gather_years(value_col = "input.cost") %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.non.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, input.cost, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]]) ->
      L232.GlobalTechCost_ind

    # Carbon capture from feedstock carbon sequestration
    # L232.GlobalTechCSeq_ind: CO2 capture fractions from global electricity generation technologies
    ## No need to consider historical periods or intermittent technologies here
    A32.nonenergy_Cseq %>%
      repeat_add_columns(tibble(year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS))) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCSeq"]]) ->
      L232.GlobalTechCSeq_ind

    # Calibration and region-specific data
    # L232.StubTechCalInput_indenergy: calibrated input of industrial energy use technologies (including cogen)
    L1324.in_EJ_R_indenergy_F_Yh %>%
      bind_rows(L123.in_EJ_R_indchp_F_Yh) %>%
      complete(nesting(GCAM_region_ID, sector, fuel), year = c(year, MODEL_BASE_YEARS)) %>%
      arrange(GCAM_region_ID, sector, fuel, year) %>%
      group_by(GCAM_region_ID, sector, fuel) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(select(calibrated_techs, sector, fuel, supplysector, subsector, technology), by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) ->
      L232.in_EJ_R_indenergy_F_Yh # intermediate tibble

    L232.in_EJ_R_indenergy_F_Yh %>%
      left_join_error_no_match(distinct(select(A32.globaltech_eff, subsector, technology, minicam.energy.input)),
                               by = c("subsector", "stub.technology" = "technology")) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year) %>%
      rename(calOutputValue = calibrated.value) %>%  # temporary column name change to accommodate function set_subsector_shrwt
      set_subsector_shrwt %>%
      rename(calibrated.value = calOutputValue) %>% # temporary column name change to accommodate function set_subsector_shrwt
      mutate(tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) ->
      L232.StubTechCalInput_indenergy

    # L232.StubTechCalInput_indfeed: calibrated input of industrial feedstock technologies
    L1324.in_EJ_R_indfeed_F_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(distinct(select(calibrated_techs, sector, fuel, supplysector, subsector, technology)),
                               by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) ->
      L232.in_EJ_R_indfeed_F_Yh # intermediate tibble

    L232.in_EJ_R_indfeed_F_Yh %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], "value") %>%
      left_join(distinct(select(A32.globaltech_eff, subsector, technology, minicam.energy.input)),
                by = c("subsector", "stub.technology" = "technology")) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      select(-value) %>%
      mutate(share.weight.year = year) %>%
      rename(calOutputValue = calibrated.value) %>%  # temporary column name change to accommodate function set_subsector_shrwt
      set_subsector_shrwt %>%
      rename(calibrated.value = calOutputValue) %>% # temporary column name change to accommodate function set_subsector_shrwt
      mutate(tech.share.weight = if_else(calibrated.value > 0, 1, 0)) ->
      L232.StubTechCalInput_indfeed

    # L232.StubTechProd_industry: calibrated output of industrial sector
    # First, calculate service output by technology, for energy-use and feedstocks
    L232.in_EJ_R_indenergy_F_Yh %>%
      bind_rows(L232.in_EJ_R_indfeed_F_Yh) %>%
      left_join_error_no_match(select(L232.globaltech_eff.long, sector.name, subsector.name, technology, year,efficiency),
                               by = c("supplysector" = "sector.name", "subsector" = "subsector.name",
                                      "stub.technology" = "technology", "year")) %>%
      mutate(calOutputValue = round(value * efficiency, energy.DIGITS_CALOUTPUT)) ->
      L232.out_EJ_R_ind_serv_F_Yh # intermediate tibble

    # intermediate tibble to extract industry names
    A32.globaltech_shrwt %>%
      filter(supplysector == "other industry") %>%
      select(supplysector, subsector, technology) ->
      L232.industry_names

    # Aggregate service output by region. This is the output of the industrial sector in each region.
    L232.out_EJ_R_ind_serv_F_Yh %>%
      group_by(region, GCAM_region_ID, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup %>%
      mutate(supplysector = L232.industry_names[["supplysector"]],
             subsector = L232.industry_names[["subsector"]],
             stub.technology = L232.industry_names[["technology"]],
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L232.StubTechProd_industry

    # L232.StubTechCoef_industry: calibrated output of industrial sector
    # Next, aggregate service output by sector to calculate the portion of each input
    L232.out_EJ_R_ind_serv_F_Yh %>%
      group_by(region, GCAM_region_ID, supplysector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup %>%
      left_join_error_no_match(select(L232.StubTechProd_industry, calOutputValue, region, year),
                               by = c("region", "year")) %>%
      rename(calOutputValue = calOutputValue.x,
             output_tot = calOutputValue.y) %>%
      mutate(coefficient = calOutputValue / output_tot) %>%
      rename(minicam.energy.input = supplysector) %>%
      mutate(supplysector = L232.industry_names[["supplysector"]],
             subsector = L232.industry_names[["subsector"]],
             stub.technology = L232.industry_names[["technology"]],
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L232.StubTechCoef_industry_base # intermediate tibble?

    # This set of coefficients covers only the base years; the first "future" period will default to the global tech coefficient
    # Instead, interpolate the coefficients to these global default values in a specified period
    L232.StubTechCoef_industry_base %>%
      complete(nesting(region, supplysector, subsector, stub.technology, minicam.energy.input, market.name),
               year = unique(c(MODEL_YEARS, energy.INDCOEF_CONVERGENCE_YR))) %>%
      left_join(select(A32.globaltech_coef, supplysector, subsector, technology, minicam.energy.input, terminal_coef),
                by = c("supplysector", "subsector", stub.technology = "technology", "minicam.energy.input")) %>%
      mutate(coefficient = if_else(year == energy.INDCOEF_CONVERGENCE_YR, terminal_coef, coefficient)) %>%
      select(-terminal_coef) %>%
      group_by(region, supplysector, subsector, stub.technology, minicam.energy.input) %>%
      mutate(coefficient = round(approx_fun(year, coefficient), energy.DIGITS_COEFFICIENT)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) ->   # drop the terminal coef year if it's outside of the model years
      L232.StubTechCoef_industry

    # L232.FuelPrefElast_indenergy: fuel preference elasticities of industrial energy use
    # First, calculate the fuel shares allocated to each fuel
    L232.in_EJ_R_indenergy_F_Yh %>%
      group_by(region, GCAM_region_ID, supplysector, subsector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup ->
      L232.indenergy_fuel_shares # intermediate tibble

    L232.indenergy_fuel_shares %>%
      group_by(region, GCAM_region_ID, supplysector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      right_join(L232.indenergy_fuel_shares, by = c("region", "GCAM_region_ID", "supplysector", "year")) %>%
      rename(total = value.x,
             value = value.y) %>%
      mutate(share = value / total) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      mutate(fuelprefElasticity = 0) ->
      L232.indenergy_fuel_shares

    # Set fuel preference elasticities as indicated by exogenous rules
    # For each row of L232.indenergy_fuel_shares, we check for any entries in A32.fuelprefElasticity
    # that have the same `subsector` entry, and apply the rule specified (the share in a 'greater than'
    # entry is only used if the share value is larger; similar logic applies to 'lesser than' entries).
    # This is an inefficient way to do this, but the datasets involved are small
    A32.fuelprefElasticity.gt <- filter(A32.fuelprefElasticity, criteria == "greater than")
    for(j in seq_len(nrow(L232.indenergy_fuel_shares))) {
      for(i in seq_len(nrow(A32.fuelprefElasticity.gt))) {
        L232.indenergy_fuel_shares$fuelprefElasticity[j][
          L232.indenergy_fuel_shares$subsector[j] == A32.fuelprefElasticity.gt$subsector[i] &
            L232.indenergy_fuel_shares$share[j] > A32.fuelprefElasticity.gt$share[i] ] <-
          A32.fuelprefElasticity.gt$fuelprefElasticity[i]
      }
    }

    A32.fuelprefElasticity.lt <- filter(A32.fuelprefElasticity, criteria == "lesser than")
    for(j in seq_len(nrow(L232.indenergy_fuel_shares))) {
      for(i in seq_len(nrow(A32.fuelprefElasticity.lt))) {
        L232.indenergy_fuel_shares$fuelprefElasticity[j][
          L232.indenergy_fuel_shares$subsector[j] == A32.fuelprefElasticity.lt$subsector[i] &
            L232.indenergy_fuel_shares$share[j] < A32.fuelprefElasticity.lt$share[i] ] <-
          A32.fuelprefElasticity.lt$fuelprefElasticity[i]
      }
    }

    # The fuel preference elasticities only matter in future periods. Fill out from the first future model time period
    L232.indenergy_fuel_shares %>%
      mutate(year.fillout = min(MODEL_FUTURE_YEARS)) %>%
      filter(fuelprefElasticity != 0) %>%
      select(LEVEL2_DATA_NAMES[["FuelPrefElast"]]) ->
      L232.FuelPrefElast_indenergy

    # L232.PerCapitaBased_ind: per-capita based flag for industry final demand
    tibble(region = GCAM_region_names[["region"]]) %>%
      mutate(energy.final.demand = A32.demand[["energy.final.demand"]],
             perCapitaBased = A32.demand[["perCapitaBased"]]) ->
      L232.PerCapitaBased_ind

    # Retirement information
    A32.globaltech_retirement %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L232.globaltech_retirement_base

    # Copies first future year retirment information into all future years and appends back onto base year
    L232.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) ->
      L232.globaltech_retirement_future

    # filters base years from original and then appends future years
    L232.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      bind_rows(L232.globaltech_retirement_future) ->
      L232.globaltech_retirement

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # This section checks L232.globaltech_retirement for each of these functions and creates a separate level 2 file for each
    # All of these options have different headers, and all are allowed
    if(any(!is.na(L232.globaltech_retirement$shutdown.rate))) {
      L232.globaltech_retirement %>%
        filter(!is.na(L232.globaltech_retirement$shutdown.rate)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "shutdown.rate") ->
        L232.GlobalTechShutdown_en
    }

    if(any(!is.na(L232.globaltech_retirement$half.life))) {
      L232.globaltech_retirement %>%
        filter(!is.na(L232.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
        L232.GlobalTechSCurve_en
    }

    # L232.GlobalTechLifetime_en: Global tech lifetime
    if(any(is.na(L232.globaltech_retirement$shutdown.rate) & is.na(L232.globaltech_retirement$half.life))) {
      L232.globaltech_retirement %>%
        filter(is.na(L232.globaltech_retirement$shutdown.rate) & is.na(L232.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime") ->
        L232.GlobalTechLifetime_en
    }

    # L232.GlobalTechProfitShutdown_en: Global tech profit shutdown decider and parameters
    if(any(!is.na(L232.globaltech_retirement$median.shutdown.point))) {
      L232.globaltech_retirement %>%
        filter(!is.na(L232.globaltech_retirement$median.shutdown.point)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
        L232.GlobalTechProfitShutdown_en
    }

    # L232.PriceElasticity_ind: price elasticity of industry final demand
    # Price elasticities are only applied to future periods. Application in base years will cause solution failure
    tibble(region = rep(GCAM_region_names[["region"]], times = length(MODEL_FUTURE_YEARS))) %>%
      mutate(energy.final.demand = A32.demand[["energy.final.demand"]],
             year = sort(rep(MODEL_FUTURE_YEARS, times = nrow(GCAM_region_names))),
             price.elasticity = A32.demand[["price.elasticity"]]) ->
      L232.PriceElasticity_ind

    # L232.BaseService_ind: base-year service output of industry final demand
    # Base service is equal to the output of the industry supplysector
    L232.StubTechProd_industry %>%
      select(region, year, calOutputValue) %>%
      rename(base.service = calOutputValue) %>%
      mutate(energy.final.demand = A32.demand[["energy.final.demand"]]) ->
      L232.BaseService_ind

    # L232.IncomeElasticity_ind_scen: income elasticity of industry (scenario-specific)
    L102.pcgdp_thous90USD_GCAM3_R_Y %>%
      # Combine GCAM 3.0 with the SSPs, and subset only the relevant years
      mutate(scenario = 'GCAM3') %>%
      bind_rows(L102.pcgdp_thous90USD_Scen_R_Y) %>%
      filter(year %in% c(max(MODEL_BASE_YEARS), MODEL_FUTURE_YEARS)) %>%
      # Per-capita GDP ratios, which are used in the equation for demand growth
      group_by(GCAM_region_ID, scenario) %>%
      mutate(temp_lag = lag(value, 1),
             value = value / temp_lag) %>%
      ungroup %>%
      select(-temp_lag) %>%
      filter(year %in% MODEL_FUTURE_YEARS) ->
      L232.pcgdpRatio_ALL_R_Y # intermediate tibble

    # Calculate the industrial output as the base-year industrial output times the GDP ratio raised to the income elasticity
    # The income elasticity is looked up based on the prior year's output
    L232.pcgdpRatio_ALL_R_Y %>%
      select(GCAM_region_ID, scenario) %>%
      distinct %>%
      left_join_error_no_match(GCAM_region_names, by = 'GCAM_region_ID') %>%
      mutate(year = max(MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(L232.BaseService_ind, by = c("year", "region")) %>%
      left_join_error_no_match(L101.Pop_thous_GCAM3_R_Y, by = c("year", "GCAM_region_ID")) %>%
      mutate(value = base.service * CONV_BIL_THOUS / value) %>%
      select(-base.service, -energy.final.demand) ->
      L232.Output_ind

    # At each time, the output is equal to the prior period's output times the GDP ratio, raised to the elasticity
    # that corresponds to the output that was observed in the prior time period. This method prevents (ideally) runaway
    # industrial production.
    elast_years <- c(max(MODEL_BASE_YEARS), MODEL_FUTURE_YEARS)
    for(i in seq_along(elast_years)[-1]) {
      L232.Output_ind %>%
        filter(year == elast_years[i - 1]) %>%
        left_join(filter(L232.pcgdpRatio_ALL_R_Y, year == elast_years[i]), by = c("GCAM_region_ID", "scenario")) %>% # strick left join fails timeshift test due to NAs in L102.pcgdp_thous90USD_Scen_R_Y under timeshift mode
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
      L232.IncomeElasticity_ind # intermediate tibble

    # KVC: SSP1 needs lower income elasticities. Storyline has limited growth in energy-related industries
    # because of warm fuzzy feelings about environment. We are hard-coding this for a while.
    L232.IncomeElasticity_ind %>%
      filter(scenario == "SSP1") %>%
      mutate(income.elasticity = income.elasticity * 0.75) %>%
      bind_rows(filter(L232.IncomeElasticity_ind, scenario != "SSP1")) ->
      L232.IncomeElasticity_ind

    # ===================================================
    # Produce outputs

    # Extract GCAM3, SSP, and gSSP data and assign to separate tables
    for(ieo in INCOME_ELASTICITY_OUTPUTS) {
      L232.IncomeElasticity_ind %>%
        filter(scenario == ieo) %>%
        select(LEVEL2_DATA_NAMES[["IncomeElasticity"]]) %>%
        add_title(paste("Income elasticity of industry -", ieo)) %>%
        add_units("Unitless") %>%
        add_comments("First calculate industrial output as the base-year industrial output times the GDP ratio raised to the income elasticity") %>%
        add_comments("Then back out the appropriate income elasticities from industrial output") %>%
        add_comments("Note lower income elasticities for SSP1 are hard-coded.") %>%
        add_legacy_name(paste0("L232.IncomeElasticity_ind_", tolower(ieo))) %>%
        add_precursors("L102.pcgdp_thous90USD_GCAM3_R_Y", "L102.pcgdp_thous90USD_Scen_R_Y", "common/GCAM_region_names", "L1326.in_EJ_R_indenergy_F_Yh", "L123.in_EJ_R_indchp_F_Yh", "energy/calibrated_techs", "L1324.in_EJ_R_indfeed_F_Yh", "energy/A32.globaltech_eff", "energy/A32.globaltech_shrwt", "energy/A32.demand", "L101.Pop_thous_GCAM3_R_Y", "socioeconomics/A32.inc_elas_output") ->
        x
      assign(paste0("L232.IncomeElasticity_ind_", tolower(ieo)), x)
    }

    L232.Supplysector_ind %>%
      add_title("Supply sector information for industry sector") %>%
      add_units("NA") %>%
      add_comments("For industry sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A32.sector is expended into all GCAM regions") %>%
      add_legacy_name("L232.Supplysector_ind") %>%
      add_precursors("common/GCAM_region_names", "energy/A32.sector") ->
      L232.Supplysector_ind

    L232.SubsectorLogit_ind %>%
      add_title("Subsector logit exponents of industry sector") %>%
      add_units("Unitless") %>%
      add_comments("For industry sector, the subsector logit exponents from A32.subsector_logit are expanded into all GCAM regions with non-existent heat subsectors removed") %>%
      add_legacy_name("L232.SubsectorLogit_ind") %>%
      add_precursors("energy/A32.subsector_logit", "common/GCAM_region_names", "energy/A_regions", "energy/calibrated_techs") ->
      L232.SubsectorLogit_ind

    L232.FinalEnergyKeyword_ind %>%
      add_title("Supply sector keywords for industry sector") %>%
      add_units("NA") %>%
      add_comments("Set supply sector keywords for industry sector for all GCAM regions") %>%
      add_legacy_name("L232.FinalEnergyKeyword_ind") %>%
      add_precursors("energy/A32.sector", "common/GCAM_region_names") ->
      L232.FinalEnergyKeyword_ind

    L232.SubsectorShrwtFllt_ind %>%
      add_title("Subsector shareweights of industry sector") %>%
      add_units("Unitless") %>%
      add_comments("For industry sector, the subsector shareweights from A32.subsector_shrwt are expanded into all GCAM regions with non-existent heat technologies") %>%
      add_legacy_name("L232.SubsectorShrwtFllt_ind") %>%
      add_precursors("energy/A32.subsector_shrwt", "common/GCAM_region_names", "energy/A_regions", "energy/calibrated_techs") ->
      L232.SubsectorShrwtFllt_ind

    L232.SubsectorInterp_ind %>%
      add_title("Subsector shareweight interpolation of industry sector") %>%
      add_units("NA") %>%
      add_comments("For industry sector, the subsector shareweight interpolation function infromation from A32.subsector_interp is expanded into all GCAM regions with non-existent heat technologies removed") %>%
      add_legacy_name("L232.SubsectorInterp_ind") %>%
      add_precursors("energy/A32.subsector_interp", "common/GCAM_region_names", "energy/A_regions", "energy/calibrated_techs") ->
      L232.SubsectorInterp_ind

    L232.StubTech_ind %>%
      add_title("Identification of stub technologies of industrial sector") %>%
      add_units("NA") %>%
      add_comments("For industry sector, the stub technologies from A32.globaltech_shrwt are expanded into all GCAM regions with non-existent heat technologies removed") %>%
      add_legacy_name("L232.StubTech_ind") %>%
      add_precursors("energy/A32.globaltech_shrwt", "common/GCAM_region_names", "energy/A_regions", "energy/calibrated_techs") ->
      L232.StubTech_ind

    L232.GlobalTechShrwt_ind %>%
      add_title("Shareweights of global industrial sector technologies") %>%
      add_units("Unitless") %>%
      add_comments("For industry sector, the share weights from A32.globaltech_shrwt are interpolated into all base years and future years") %>%
      add_legacy_name("L232.GlobalTechShrwt_ind") %>%
      add_precursors("energy/A32.globaltech_shrwt") ->
      L232.GlobalTechShrwt_ind

    L232.StubTechInterp_ind %>%
      add_title("Shareweight interpolation of global industrial sector technologies") %>%
      add_units("NA") %>%
      add_comments("For industry sector, the interpolation function from A32.globaltech_interp are expanded into all GCAM regions") %>%
      add_legacy_name("L232.StubTechInterp_ind") %>%
      add_precursors("energy/A32.globaltech_interp", "common/GCAM_region_names") ->
      L232.StubTechInterp_ind

    L232.GlobalTechEff_ind %>%
      add_title("Energy inputs and efficiency of global industrial energy use and feedstocks technologies") %>%
      add_units("Unitless") %>%
      add_comments("For industry sector, the efficiency values from A32.globaltech_eff are interpolated into all base years and future years") %>%
      add_legacy_name("L232.GlobalTechEff_ind") %>%
      add_precursors("energy/A32.globaltech_eff") ->
      L232.GlobalTechEff_ind

    L232.GlobalTechCoef_ind %>%
      add_title("Energy inputs and coefficients of global industry technologies") %>%
      add_units("Unitless") %>%
      add_comments("For industry sector, the coefficients from A32.globaltech_coef are interpolated into all base years and future years") %>%
      add_legacy_name("L232.GlobalTechCoef_ind") %>%
      add_precursors("energy/A32.globaltech_coef") ->
      L232.GlobalTechCoef_ind

    L232.GlobalTechCost_ind %>%
      add_title("Capital costs of global industrial technologies") %>%
      add_units("1975$/GJ") %>%
      add_comments("For industry sector, the capital costs from A32.globaltech_cost are interpolated into all base years and future years") %>%
      add_legacy_name("L232.GlobalTechCost_ind") %>%
      add_precursors("energy/A32.globaltech_cost") ->
      L232.GlobalTechCost_ind

    L232.GlobalTechSecOut_ind %>%
      add_title("Secondary output ratios of industrial cogeneration technologies") %>%
      add_units("Unitless") %>%
      add_comments("Secondary output ratios are calculated as electricity ratio (Assumed CHP electricity output per unit fuel input) over efficiency") %>%
      add_legacy_name("L232.GlobalTechSecOut_ind") %>%
      add_precursors("energy/A23.chp_elecratio", "energy/A32.globaltech_eff") ->
      L232.GlobalTechSecOut_ind

    L232.GlobalTechCSeq_ind %>%
      add_title("CO2 capture fractions from global electricity generation technologies") %>%
      add_units("Unitless") %>%
      add_comments("Remove fractions from A32.nonenergy_Cseq are expanded into all model years") %>%
      add_legacy_name("L232.GlobalTechCSeq_ind") %>%
      add_precursors("energy/A32.nonenergy_Cseq") ->
      L232.GlobalTechCSeq_ind

    L232.StubTechCalInput_indenergy %>%
      add_title("Calibrated input of industrial energy use technologies (including cogen)") %>%
      add_units("EJ") %>%
      add_comments("Calibrated input of industrial energy use values are calculated using L1324.in_EJ_R_indenergy_F_Yh then added information such as subsector, technology, minicam.energy.input, calibration, tech.share.weight, and etc.") %>%
      add_legacy_name("L232.StubTechCalInput_indenergy") %>%
      add_precursors("L1326.in_EJ_R_indenergy_F_Yh", "energy/calibrated_techs", "energy/A32.globaltech_eff") ->
      L232.StubTechCalInput_indenergy

    L232.StubTechCalInput_indfeed %>%
      add_title("Calibrated input of industrial feedstock technologies") %>%
      add_units("EJ") %>%
      add_comments("calibrated input of industrial feedstock technologies values are calculated using L1324.in_EJ_R_indfeed_F_Yh then added information such as subsector, technology, minicam.energy.input, calibration, tech.share.weight, and etc.") %>%
      add_legacy_name("L232.StubTechCalInput_indfeed") %>%
      add_precursors("L1324.in_EJ_R_indfeed_F_Yh", "common/GCAM_region_names", "energy/calibrated_techs", "energy/A32.globaltech_eff") ->
      L232.StubTechCalInput_indfeed

    L232.StubTechProd_industry %>%
      add_title("Calibrated output of industrial sector") %>%
      add_units("EJ") %>%
      add_comments("Service output values are calculated by technology, for energy-use and feedstocks then aggregated by region") %>%
      add_legacy_name("L232.StubTechProd_industry") %>%
      add_precursors("L1326.in_EJ_R_indenergy_F_Yh", "L123.in_EJ_R_indchp_F_Yh", "common/GCAM_region_names", "energy/calibrated_techs", "L1324.in_EJ_R_indfeed_F_Yh", "energy/A32.globaltech_eff", "energy/A32.globaltech_shrwt") ->
      L232.StubTechProd_industry

    L232.StubTechCoef_industry %>%
      add_title("Calibrated output of industrial sector") %>%
      add_units("Unitless") %>%
      add_comments("Service output values were first aggregated by sector to calculate the portion of each input as coefficients, then the coefficients were interpolated to cover last base year, future years and industry coefficient convergence year") %>%
      add_legacy_name("L232.StubTechCoef_industry") %>%
      add_precursors("L1326.in_EJ_R_indenergy_F_Yh", "L123.in_EJ_R_indchp_F_Yh", "common/GCAM_region_names", "energy/calibrated_techs", "L1324.in_EJ_R_indfeed_F_Yh", "energy/A32.globaltech_eff", "energy/A32.globaltech_shrwt") ->
      L232.StubTechCoef_industry

    L232.FuelPrefElast_indenergy %>%
      add_title("Fuel preference elasticities of industrial energy use") %>%
      add_units("Unitless") %>%
      add_comments("First, calculate the fuel shares allocated to each fuel, then set fuel preference elasticities as indicated by exogenous rules(A32.fuelprefElasticity), lastly  fill out elasticities from the first future model time period") %>%
      add_legacy_name("L232.FuelPrefElast_indenergy") %>%
      add_precursors("L1326.in_EJ_R_indenergy_F_Yh", "L123.in_EJ_R_indchp_F_Yh", "common/GCAM_region_names", "energy/calibrated_techs", "energy/A32.fuelprefElasticity") ->
      L232.FuelPrefElast_indenergy

    L232.PerCapitaBased_ind %>%
      add_title("Per-capita based flag for industry final demand") %>%
      add_units("NA") %>%
      add_comments("Extracted per-capita based flag for industry final demand from A32.demand") %>%
      add_legacy_name("L232.PerCapitaBased_ind") %>%
      add_precursors("common/GCAM_region_names", "energy/A32.demand") ->
      L232.PerCapitaBased_ind

    L232.PriceElasticity_ind %>%
      add_title("Price elasticity of industry final demand") %>%
      add_units("Unitless") %>%
      add_comments("Extracted price elasticity of industry final demand from A32.demand") %>%
      add_comments("Price elasticities are only applied to future periods. Application in base years will cause solution failure") %>%
      add_legacy_name("L232.PriceElasticity_ind") %>%
      add_precursors("common/GCAM_region_names", "energy/A32.demand") ->
      L232.PriceElasticity_ind

    if(exists("L232.GlobalTechShutdown_en")) {
      L232.GlobalTechShutdown_en %>%
        add_title("Global tech lifetime for techs with shutdown rate") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that uses a phased retirement function") %>%
        add_legacy_name("L232.GlobalTechShutdown_en") %>%
        add_precursors("energy/A32.globaltech_retirement") ->
        L232.GlobalTechShutdown_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L232.GlobalTechShutdown_en") ->
        L232.GlobalTechShutdown_en
    }

    if(exists("L232.GlobalTechSCurve_en")) {
      L232.GlobalTechSCurve_en %>%
        add_title("Global tech lifetime for techs with s-curve retirement function") %>%
        add_units("Lifetime in years, half-life in years") %>%
        add_comments("Filters for any technology that uses an S-curve retirement function") %>%
        add_legacy_name("L232.GlobalTechSCurve_en") %>%
        add_precursors("energy/A32.globaltech_retirement") ->
        L232.GlobalTechSCurve_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L232.GlobalTechSCurve_en") ->
        L232.GlobalTechSCurve_en
    }

    if(exists("L232.GlobalTechLifetime_en")) {
      L232.GlobalTechLifetime_en %>%
        add_title("Global tech lifetime for any technology with no retirement function") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that has no phased or S-curve retirement function, empty by default.") %>%
        add_legacy_name("L232.GlobalTechLifetime_en") %>%
        add_precursors("energy/A32.globaltech_retirement") ->
        L232.GlobalTechLifetime_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L232.GlobalTechLifetime_en") ->
        L232.GlobalTechLifetime_en
    }

    if(exists("L232.GlobalTechProfitShutdown_en")) {
      L232.GlobalTechProfitShutdown_en %>%
        add_title("Global tech profit shutdown decider and parameters") %>%
        add_units("Unitless, used to determine shape of the function defining the relationship between shutdown rate and profitability") %>%
        add_comments("Filters for any technologies that use a profit-based shutdown parameter") %>%
        add_legacy_name("L232.GlobalTechProfitShutdown_en") %>%
        add_precursors("energy/A32.globaltech_retirement") ->
        L232.GlobalTechProfitShutdown_en
    } else {
      missing_data() %>%
        add_legacy_name("energy/L232.GlobalTechProfitShutdown_en") ->
        L232.GlobalTechProfitShutdown_en
    }

    L232.BaseService_ind %>%
      add_title("Base-year service output of industry final demand") %>%
      add_units("EJ") %>%
      add_comments("Extracted base-year service output of industry final demand from L232.StubTechProd_industry") %>%
      add_legacy_name("L232.BaseService_ind") %>%
      add_precursors("L1326.in_EJ_R_indenergy_F_Yh", "L123.in_EJ_R_indchp_F_Yh", "common/GCAM_region_names", "energy/calibrated_techs", "L1324.in_EJ_R_indfeed_F_Yh", "energy/A32.globaltech_eff", "energy/A32.globaltech_shrwt", "energy/A32.demand") ->
      L232.BaseService_ind

    return_data(L232.Supplysector_ind, L232.SubsectorLogit_ind, L232.FinalEnergyKeyword_ind,
                L232.SubsectorShrwtFllt_ind, L232.SubsectorInterp_ind,
                L232.StubTech_ind, L232.GlobalTechShrwt_ind,
                L232.StubTechInterp_ind, L232.GlobalTechEff_ind, L232.GlobalTechCoef_ind,
                L232.GlobalTechCost_ind, L232.GlobalTechSecOut_ind, L232.GlobalTechCSeq_ind,
                L232.StubTechCalInput_indenergy, L232.StubTechCalInput_indfeed, L232.StubTechProd_industry,
                L232.StubTechCoef_industry, L232.FuelPrefElast_indenergy, L232.PerCapitaBased_ind,
                L232.PriceElasticity_ind, L232.BaseService_ind, L232.GlobalTechShutdown_en,
                L232.GlobalTechSCurve_en, L232.GlobalTechLifetime_en, L232.GlobalTechProfitShutdown_en,
                L232.IncomeElasticity_ind_gcam3, L232.IncomeElasticity_ind_gssp1,
                L232.IncomeElasticity_ind_gssp2, L232.IncomeElasticity_ind_gssp3,
                L232.IncomeElasticity_ind_gssp4, L232.IncomeElasticity_ind_gssp5,
                L232.IncomeElasticity_ind_ssp1, L232.IncomeElasticity_ind_ssp2,
                L232.IncomeElasticity_ind_ssp3, L232.IncomeElasticity_ind_ssp4,
                L232.IncomeElasticity_ind_ssp5)
  } else {
    stop("Unknown command")
  }
}
