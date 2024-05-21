# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2327.paper
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for paper-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2327.SectorLogitTables[[ curr_table ]]$data}, \code{L2327.Supplysector_paper}, \code{L2327.FinalEnergyKeyword_paper},
#' \code{L2327.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2327.SubsectorLogit_paper}, \code{L2327.SubsectorShrwtFllt_paper},
#' \code{L2327.SubsectorInterp_paper}, \code{L2327.StubTech_paper}, \code{L2327.GlobalTechShrwt_paper}, \code{L2327.GlobalTechCoef_paper},
#' \code{L2327.GlobalTechCost_paper}, \code{L2327.GlobalTechCapture_paper}, \code{L2327.StubTechProd_paper}, \code{L2327.StubTechCalInput_paper_heat},
#' \code{L2327.StubTechCoef_paper}, \code{L2327.PerCapitaBased_paper}, \code{L2327.BaseService_paper}, \code{L2327.PriceElasticity_paper},
#' \code{L2327.GlobalTechSecOut_paper},
#' \code{object}. The corresponding file in the
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for paper sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr gather spread
#' @author MMC July 2022
module_energy_L2327.paper <- function(command, ...) {

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A_regions",
             FILE = "energy/A327.sector",
             FILE = "energy/A23.chp_elecratio",
             FILE = "energy/A327.subsector_interp",
             FILE = "energy/A327.subsector_logit",
             FILE = "energy/A327.subsector_shrwt",
             FILE = "energy/A327.globaltech_coef",
             FILE = "energy/A327.globaltech_co2capture",
             FILE = "energy/A327.globaltech_cost",
             FILE = "energy/A327.globaltech_shrwt",
             FILE = "energy/A327.globaltech_retirement",
             FILE = "energy/A327.demand",
             FILE = "energy/A327.subsector_interp_adj_future_years",
             FILE = "energy/A327.subsector_shrwt_adj_future_years",
             "L1327.in_EJ_R_paper_F_Yh",
             "L1327.out_Mt_R_paper_Yh",
             "L1327.IO_GJkg_R_paper_F_Yh",
             "L1327.elec_noheat_adj_shwt_R",
             "L203.Supplysector_demand",
             "L203.PerCapitaBased"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2327.Supplysector_paper",
             "L2327.FinalEnergyKeyword_paper",
             "L2327.SubsectorLogit_paper",
             "L2327.SubsectorShrwtFllt_paper",
             "L2327.SubsectorInterp_paper",
             "L2327.StubTech_paper",
             "L2327.GlobalTechShrwt_paper",
             "L2327.GlobalTechCoef_paper",
             "L2327.GlobalTechCost_paper",
             "L2327.GlobalTechTrackCapital_paper",
             "L2327.GlobalTechCapture_paper",
             "L2327.GlobalTechShutdown_paper",
             "L2327.GlobalTechSCurve_paper",
             "L2327.GlobalTechLifetime_paper",
             "L2327.GlobalTechProfitShutdown_paper",
             "L2327.StubTechProd_paper",
             "L2327.StubTechCalInput_paper_heat",
             "L2327.StubTechCoef_paper",
             "L2327.PerCapitaBased_paper",
             "L2327.BaseService_paper",
             "L2327.PriceElasticity_paper",
             "L2327.GlobalTechSecOut_paper",
             "L2327.DeleteSupplysector_PaperAgDemand",
             "L2327.DeleteFinalDemand_PaperAgDemand",
             "L2327.DeleteSupplysector_PaperAgDemand_USA",
             "L2327.DeleteFinalDemand_PaperAgDemand_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs", strip_attributes = TRUE)
    A_regions <- get_data(all_data, "energy/A_regions", strip_attributes = TRUE)
    A327.sector <- get_data(all_data, "energy/A327.sector", strip_attributes = TRUE)
    A327.subsector_interp <- get_data(all_data, "energy/A327.subsector_interp", strip_attributes = TRUE)
    A327.subsector_logit <- get_data(all_data, "energy/A327.subsector_logit", strip_attributes = TRUE)
    A327.subsector_shrwt <- get_data(all_data, "energy/A327.subsector_shrwt", strip_attributes = TRUE)
    A327.globaltech_coef <- get_data(all_data, "energy/A327.globaltech_coef", strip_attributes = TRUE)
    A327.globaltech_co2capture <- get_data(all_data, "energy/A327.globaltech_co2capture", strip_attributes = TRUE)
    A327.globaltech_retirement <- get_data(all_data, "energy/A327.globaltech_retirement", strip_attributes = TRUE)
    A327.globaltech_cost <- get_data(all_data, "energy/A327.globaltech_cost", strip_attributes = TRUE)
    A327.globaltech_shrwt <- get_data(all_data, "energy/A327.globaltech_shrwt", strip_attributes = TRUE)
    A327.demand <- get_data(all_data, "energy/A327.demand", strip_attributes = TRUE)
    A23.chp_elecratio  <- get_data(all_data, "energy/A23.chp_elecratio", strip_attributes = TRUE)
    L1327.in_EJ_R_paper_F_Yh <- get_data(all_data, "L1327.in_EJ_R_paper_F_Yh", strip_attributes = TRUE)
    L1327.out_Mt_R_paper_Yh <- get_data(all_data, "L1327.out_Mt_R_paper_Yh", strip_attributes = TRUE)
    L1327.IO_GJkg_R_paper_F_Yh <- get_data(all_data, "L1327.IO_GJkg_R_paper_F_Yh", strip_attributes = TRUE)
    L1327.elec_noheat_adj_shwt_R <- get_data(all_data, "L1327.elec_noheat_adj_shwt_R", strip_attributes = TRUE)
    A327.subsector_interp_adj_future_years <- get_data(all_data, "energy/A327.subsector_interp_adj_future_years", strip_attributes = TRUE)
    A327.subsector_shrwt_adj_future_years <- get_data(all_data, "energy/A327.subsector_shrwt_adj_future_years", strip_attributes = TRUE)
    L203.Supplysector_demand <- get_data(all_data, "L203.Supplysector_demand", strip_attributes = TRUE)
    L203.PerCapitaBased <- get_data(all_data, "L203.PerCapitaBased", strip_attributes = TRUE)

    # ===================================================
    # 0. Give binding for variable names used in pipeline
    has_district_heat <- year <- value <- GCAM_region_ID <- sector <- fuel <- year.fillout <- to.value <-
      technology <- supplysector <- subsector <- minicam.energy.input <- coefficient <-
      remove.fraction <- minicam.non.energy.input <- input.cost  <- calibration <- calOutputValue <- subs.share.weight <- region <-
      calibrated.value <- . <- scenario <- temp_lag <- base.service <- energy.final.demand <- output.ratio <-
      shutdown.rate <- half.life <- median.shutdown.point <- share.weight.year <- tech.share.weight <-
      value.x <- value.y <- parameter <- secondary.output <- elec_ratio <- year.x <- year.y <- output.ratio.x <-
      output.ratio.y <- sector.name <- subsector.name <- stub.technology <- market.name <- terminal_coef <- NULL

    # ===================================================
    # 1. Perform computations
    has_not_heat <- filter(A_regions, has_district_heat == 0) # intermediate tibble

    calibrated_techs %>%
      filter(sector == "paper" & subsector == "heat") %>%
      select(supplysector, subsector, technology) %>%
      repeat_add_columns(tibble(GCAM_region_ID = has_not_heat[["GCAM_region_ID"]])) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L2327.rm_heat_techs_R # intermediate tibble

    # 1a. Supplysector information
    # L2327.Supplysector_paper: Supply sector information for paper sector
    A327.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L2327.Supplysector_paper

    # L2327.FinalEnergyKeyword_paper: Supply sector keywords for paper sector
    A327.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]], GCAM_region_names) %>%
      na.omit ->
      L2327.FinalEnergyKeyword_paper

    # 1b. Subsector information
    # L2327.SubsectorLogit_paper: Subsector logit exponents of paper sector
    A327.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) %>%
      anti_join(L2327.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2327.SubsectorLogit_paper

    # and L2327.SubsectorShrwtFllt_paper: Subsector shareweights of paper sector
    A327.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) %>%
      anti_join(L2327.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2327.SubsectorShrwtFllt_paper


    # Adjust process heat subsector share weights for regions with electricity but no other fuels - set to 1 after base year
    L1327.elec_noheat_adj_shwt_R %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      select(-year) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") ->
      reg_adj_subsector_shwt

    A327.subsector_shrwt_adj_future_years %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) %>%
      filter(region %in% reg_adj_subsector_shwt[["region"]]) %>%
      anti_join(L2327.rm_heat_techs_R, by = c("region", "subsector")) ->
      L2327.SubsectorShrwtFllt_paper_adj_noheat

    # L2327.SubsectorInterp_paper: Subsector shareweight interpolation of paper sector
    A327.subsector_interp %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names) %>%
      anti_join(L2327.rm_heat_techs_R, by = c("region", "subsector")) -> # Remove non-existent heat subsectors from each region
      L2327.SubsectorInterp_paper

    # Adjust interpolation rule for regions with electricity but no other fuels - allow interpolation to 1 in 2020 for process heat subsectors
    A327.subsector_interp_adj_future_years %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names) %>%
      filter(region %in% reg_adj_subsector_shwt[["region"]]) %>%
      anti_join(L2327.rm_heat_techs_R, by = c("region", "subsector")) ->
      L2327.SubsectorInterp_paper_adj_noheat

    L2327.SubsectorInterp_paper %>%
      anti_join(L2327.SubsectorInterp_paper_adj_noheat, by = c("region", "supplysector", "subsector")) %>%
      bind_rows(L2327.SubsectorInterp_paper_adj_noheat) ->
      L2327.SubsectorInterp_paper


    # 1c. Technology information
    # L2327.StubTech_paper: Identification of stub technologies of paper and paperboard
    # Note: assuming that technology list in the share weight table includes the full set (any others would default to a 0 shareweight)
    A327.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      anti_join(L2327.rm_heat_techs_R, by = c("region", "subsector")) %>% # Remove non-existent heat subsectors from each region
      rename(stub.technology = technology) ->
      L2327.StubTech_paper

    # L2327.GlobalTechShrwt_paper: Share weights of global paper technologies
    A327.globaltech_shrwt %>%
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
      L2327.GlobalTechShrwt_paper

    # L2327.GlobalTechCoef_paper: Energy inputs and efficiency of global paper energy use and feedstocks technologies
    A327.globaltech_coef %>%
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
      L2327.globaltech_coef.long # intermediate tibble

    L2327.globaltech_coef.long %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L2327.GlobalTechCoef_paper

    # Secondary outputs of cogen technologies: these are input as a ratio
    # L2327.GlobalTechSecOut_ind: Secondary output ratios of paper cogeneration technologies
    A327.globaltech_coef %>%
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
      L2327.GlobalTechSecOut_paper

    # Carbon capture rates from technologies with CCS
    # L2327.GlobalTechCapture_paper: CO2 capture fractions from global paper production technologies with CCS
    # No need to consider historical periods or intermittent technologies here
    A327.globaltech_co2capture %>%
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
      L2327.GlobalTechCapture_paper

    # Retirement information
    A327.globaltech_retirement %>%
      set_years() %>%
      mutate(year = as.integer(year)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) ->
      L2327.globaltech_retirement_base

    # Copies first future year retirement information into all future years and appends back onto base year
    L2327.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(-year.x) %>%
      rename(year = year.y) ->
      L2327.globaltech_retirement_future

    # filters base years from original and then appends future years
    L2327.globaltech_retirement_base %>%
      mutate(year = as.integer(year)) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      bind_rows(L2327.globaltech_retirement_future) ->
      L2327.globaltech_retirement

    # Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # This section checks L2327.globaltech_retirement for each of these functions and creates a separate level 2 file for each
    # All of these options have different headers, and all are allowed
    if(any(!is.na(L2327.globaltech_retirement$shutdown.rate))) {
      L2327.globaltech_retirement %>%
        filter(!is.na(L2327.globaltech_retirement$shutdown.rate)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "shutdown.rate") ->
        L2327.GlobalTechShutdown_paper
    }

    if(any(!is.na(L2327.globaltech_retirement$half.life))) {
      L2327.globaltech_retirement %>%
        filter(!is.na(L2327.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life") ->
        L2327.GlobalTechSCurve_paper
    }

    # L2327.GlobalTechLifetime_paper: Global tech lifetime
    if(any(is.na(L2327.globaltech_retirement$shutdown.rate) & is.na(L2327.globaltech_retirement$half.life))) {
      L2327.globaltech_retirement %>%
        filter(is.na(L2327.globaltech_retirement$shutdown.rate) & is.na(L2327.globaltech_retirement$half.life)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime") ->
        L2327.GlobalTechLifetime_paper
    }

    # L2327.GlobalTechProfitShutdown_paper: Global tech profit shutdown decider and parameters
    if(any(!is.na(L2327.globaltech_retirement$median.shutdown.point))) {
      L2327.globaltech_retirement %>%
        filter(!is.na(L2327.globaltech_retirement$median.shutdown.point)) %>%
        select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ->
        L2327.GlobalTechProfitShutdown_paper
    }

    # L2327.GlobalTechCost_paper: Non-energy costs of global paper manufacturing technologies
    A327.globaltech_cost %>%
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
      L2327.GlobalTechCost_paper # intermediate tibble

    FCR <- (socioeconomics.DEFAULT_INTEREST_RATE * (1+socioeconomics.DEFAULT_INTEREST_RATE)^socioeconomics.INDUSTRY_CAP_PAYMENTS) /
      ((1+socioeconomics.DEFAULT_INTEREST_RATE)^socioeconomics.INDUSTRY_CAP_PAYMENTS -1)
    L2327.GlobalTechCost_paper %>%
      # we only want to track investments in energy, otherwise we double accounting with materials
      filter(sector.name != "paper") %>%
      mutate(capital.coef = socioeconomics.INDUSTRY_CAPITAL_RATIO / FCR,
             tracking.market = socioeconomics.EN_CAPITAL_MARKET_NAME,
             # vintaging is active so no need for depreciation
             depreciation.rate = 0) %>%
      select(LEVEL2_DATA_NAMES[['GlobalTechTrackCapital']]) ->
      L2327.GlobalTechTrackCapital_paper

    # L2327.StubTechCoef_paper: region-specific coefficients of paper production technologies
    # Take this as a given in all years for which data is available
    calibrated_techs %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      distinct ->
      calibrated_techs_export # temporary tibble

    L1327.IO_GJkg_R_paper_F_Yh %>%
      filter(year %in% HISTORICAL_YEARS[HISTORICAL_YEARS %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)]) %>%
      mutate(coefficient = round(value, energy.DIGITS_COEFFICIENT)) %>%
      rename(supplysector = sector) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(calibrated_techs_export, by = c("supplysector", "fuel")) %>%
      mutate(stub.technology = technology,
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) %>%
      # Fill out the values in the final base year to all future years
      group_by(region, supplysector, subsector, stub.technology, minicam.energy.input, market.name) %>%
      complete(year = MODEL_YEARS) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule=2)) %>%
      ungroup() ->
      L2327.StubTechCoef_paper


    # L2327.StubTechCalInput_paper_heat: calibrated paper inputs to heat production (including cogen)
    L2327.GlobalTechCoef_paper %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["StubTechCoef"]]), GCAM_region_names) %>%
      mutate(market.name =NULL,coefficient = NULL) %>%
      anti_join(L2327.rm_heat_techs_R, by = c("region", "subsector")) %>% # Remove non-existent heat subsectors from each region
      filter(!(supplysector %in% L2327.StubTechCoef_paper[["supplysector"]])) -> # filter for process heat only
      L2327.paper_heat_tmp # tibble of all region/year/technology combinations for process heat


    calibrated_techs %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      distinct ->
      calibrated_techs_export # temporary tibble


    L1327.in_EJ_R_paper_F_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(supplysector = if_else(fuel == 'electricity', 'paper',
                                    if_else(grepl("biomass", fuel), "waste biomass for paper",
                                            "process heat paper"))) %>%
      # assign electricity consumption to paper supply (so it doesn't get double-counted as electric technology in process heat),
      # assign biomass to waste biomass energy, all other fuels to process heat
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(calibrated_techs_export, by = c("sector", "supplysector", "fuel")) %>%
      mutate(stub.technology = technology, technology = NULL, GCAM_region_ID = NULL, fuel = NULL, sector = NULL) %>%
      # This table should only be the technologies for producing heat - drop the electricity inputs to the paper production technology
      filter(!(supplysector %in% L2327.StubTechCoef_paper[["supplysector"]]))  ->
      L2327.StubTechCalInput_paper_heat_tmp


    L2327.paper_heat_tmp %>%
      left_join(L2327.StubTechCalInput_paper_heat_tmp, by = c("region", "supplysector", "subsector", "stub.technology", "year", "minicam.energy.input")) %>%
      # using left_join here because 0 values are NA
      mutate(value = replace_na(value, 0),
             calibrated.value = round(value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      rename(calOutputValue = calibrated.value) %>%  # temporary column name change to accommodate function set_subsector_shrwt
      set_subsector_shrwt %>%
      rename(calibrated.value = calOutputValue) %>% # temporary column name changeto accommodate function set_subsector_shrwt
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) ->
      L2327.StubTechCalInput_paper_heat

    # L2327.StubTechProd_paper: calibrated paper production
    calibrated_techs %>%
      filter(calibration == "output") %>% # Only take the tech IDs where the calibration is identified as output
      select(sector, supplysector, subsector, technology) %>%
      distinct ->
      calibrated_techs_export # temporary tibble

    L1327.out_Mt_R_paper_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(calibrated_techs_export, by = "sector") %>%
      mutate(stub.technology = technology,
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2327.StubTechProd_paper

    # L2327.PerCapitaBased_paper: per-capita based flag for paper exports final demand
    A327.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PerCapitaBased"]], GCAM_region_names)  ->
      L2327.PerCapitaBased_paper

    # L2327.BaseService_paper: base-year service output of paper
    L2327.StubTechProd_paper %>%
      select(region, year, base.service = calOutputValue) %>%
      mutate(energy.final.demand = A327.demand[["energy.final.demand"]]) %>%
      group_by(region,year,energy.final.demand) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup() ->
      L2327.BaseService_paper

    # L2327.PriceElasticity_paper: price elasticity
    A327.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PriceElasticity"]][LEVEL2_DATA_NAMES[["PriceElasticity"]] != "year"], GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["PriceElasticity"]]) ->
      L2327.PriceElasticity_paper


    # Delete NonFoodDemand_woodpulp sectors in all regions (energy-final-demands and supplysectors)
    # This is replaced with woodpulp_energy
    L203.Supplysector_demand %>%
      filter(supplysector == aglu.PAPER_DELETE_AG_DEMAND) %>%
      select(LEVEL2_DATA_NAMES[["DeleteSupplysector"]]) ->
      L2327.DeleteSupplysector_PaperAgDemand

    L203.PerCapitaBased %>%
      filter(energy.final.demand == aglu.PAPER_DELETE_AG_DEMAND) %>%
      select(LEVEL2_DATA_NAMES[["DeleteFinalDemand"]]) ->
      L2327.DeleteFinalDemand_PaperAgDemand

    # For GCAM-USA, keep NonFoodDemand_woodpulp for USA region since paper industry will be deleted
    L2327.DeleteSupplysector_PaperAgDemand %>%
      filter(!region == gcam.USA_REGION) ->
      L2327.DeleteSupplysector_PaperAgDemand_USA

    L2327.DeleteFinalDemand_PaperAgDemand %>%
      filter(!region == gcam.USA_REGION) ->
      L2327.DeleteFinalDemand_PaperAgDemand_USA

    # =======================================================
    # Produce outputs
    L2327.Supplysector_paper %>%
      add_title("Supply sector information for paper sector") %>%
      add_units("NA") %>%
      add_comments("For paper sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A327.sector is expended into all GCAM regions") %>%
      add_legacy_name("L2327.Supplysector_paper") %>%
      add_precursors("energy/A327.sector", "common/GCAM_region_names") ->
      L2327.Supplysector_paper

    L2327.FinalEnergyKeyword_paper %>%
      add_title("Supply sector keywords for paper sector") %>%
      add_units("NA") %>%
      add_comments("For paper sector, the supply sector final energy keywords from A327.sector are expended into all GCAM regions") %>%
      add_legacy_name("L2327.FinalEnergyKeyword_paper") %>%
      add_precursors("energy/A327.sector", "common/GCAM_region_names") ->
      L2327.FinalEnergyKeyword_paper

    L2327.SubsectorLogit_paper %>%
      add_title("Subsector logit exponents of paper sector") %>%
      add_units("Unitless") %>%
      add_comments("For paper sector, the subsector logit exponents from A327.subsector_logit are expanded into all GCAM regions") %>%
      add_legacy_name("L2327.SubsectorLogit_paper") %>%
      add_precursors("energy/A327.subsector_logit", "energy/A_regions","common/GCAM_region_names") ->
      L2327.SubsectorLogit_paper

    L2327.SubsectorShrwtFllt_paper %>%
      add_title("Subsector shareweights of paper sector") %>%
      add_units("unitless") %>%
      add_comments("For paper sector, the subsector shareweights from A327.subsector_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2327.SubsectorShrwtFllt_paper") %>%
      add_precursors("energy/A327.subsector_shrwt", "energy/A_regions","common/GCAM_region_names",
                     "energy/A327.subsector_shrwt_adj_future_years", "L1327.elec_noheat_adj_shwt_R") ->
      L2327.SubsectorShrwtFllt_paper

    L2327.SubsectorInterp_paper %>%
      add_title("Subsector shareweight interpolation of paper sector") %>%
      add_units("NA") %>%
      add_comments("For paper sector, the subsector shareweight interpolation function infromation from A327.subsector_interp is expanded into all GCAM regions") %>%
      add_legacy_name("L2327.SubsectorInterp_paper") %>%
      add_precursors("energy/A327.subsector_interp", "energy/A_regions","common/GCAM_region_names",
                     "energy/A327.subsector_interp_adj_future_years", "L1327.elec_noheat_adj_shwt_R") ->
      L2327.SubsectorInterp_paper

    L2327.StubTech_paper %>%
      add_title("Identification of stub technologies of paper") %>%
      add_units("NA") %>%
      add_comments("For paper sector, the stub technologies from A327.globaltech_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2327.StubTech_paper") %>%
      add_precursors("energy/A327.globaltech_shrwt","energy/A_regions", "common/GCAM_region_names") ->
      L2327.StubTech_paper

    L2327.GlobalTechShrwt_paper %>%
      add_title("Shareweights of global paper technologies") %>%
      add_units("Unitless") %>%
      add_comments("For paper sector, the share weights from A327.globaltech_shrwt are interpolated into all base years and future years") %>%
      add_legacy_name("L2327.GlobalTechShrwt_paper") %>%
      add_precursors("energy/A327.globaltech_shrwt") ->
      L2327.GlobalTechShrwt_paper

    L2327.GlobalTechCoef_paper %>%
      add_title("Energy inputs and coefficients of paper technologies") %>%
      add_units("Unitless") %>%
      add_comments("For paper sector, the energy use coefficients from A327.globaltech_coef are interpolated into all model years") %>%
      add_legacy_name("L2327.GlobalTechCoef_paper") %>%
      add_precursors("energy/A327.globaltech_coef") ->
      L2327.GlobalTechCoef_paper

    L2327.GlobalTechCost_paper %>%
      add_title("Non-energy costs of global paper manufacturing technologies") %>%
      add_units("1975$/kg for supplysector paper; 1975$/GJ for supplysector process heat paper") %>%
      add_comments("For paper sector, the Non-energy costs of global paper manufacturing technologies are calculated then adjusted with CCS to include CO2 capture costs") %>%
      add_legacy_name("L2327.GlobalTechCost_paper") %>%
      add_precursors("energy/A327.globaltech_cost", "energy/A327.globaltech_coef") ->
      L2327.GlobalTechCost_paper

    L2327.GlobalTechTrackCapital_paper %>%
      add_title("Convert non-energy inputs to track the annual capital investments.") %>%
      add_units(("Coefficients")) %>%
      add_comments("Track capital investments for purposes of macro economic calculations") %>%
      same_precursors_as(L2327.GlobalTechCost_paper) ->
      L2327.GlobalTechTrackCapital_paper


    if(exists("L2327.GlobalTechShutdown_paper")) {
      L2327.GlobalTechShutdown_paper %>%
        add_title("Global tech lifetime for techs with shutdown rate") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that uses a phased retirement function") %>%
        add_legacy_name("L2327.GlobalTechShutdown_paper") %>%
        add_precursors("energy/A327.globaltech_retirement") ->
        L2327.GlobalTechShutdown_paper
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2327.GlobalTechShutdown_paper") ->
        L2327.GlobalTechShutdown_paper
    }

    if(exists("L2327.GlobalTechSCurve_paper")) {
      L2327.GlobalTechSCurve_paper %>%
        add_title("Global tech lifetime for techs with s-curve retirement function") %>%
        add_units("Lifetime in years, half-life in years") %>%
        add_comments("Filters for any technology that uses an S-curve retirement function") %>%
        add_legacy_name("L2327.GlobalTechSCurve_paper") %>%
        add_precursors("energy/A327.globaltech_retirement") ->
        L2327.GlobalTechSCurve_paper
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2327.GlobalTechSCurve_paper") ->
        L2327.GlobalTechSCurve_paper
    }

    if(exists("L2327.GlobalTechLifetime_paper")) {
      L2327.GlobalTechLifetime_paper %>%
        add_title("Global tech lifetime for any technology with no retirement function") %>%
        add_units("Lifetime in years") %>%
        add_comments("Filters for any technology that has no phased or S-curve retirement function, empty by default.") %>%
        add_legacy_name("L2327.GlobalTechLifetime_paper") %>%
        add_precursors("energy/A327.globaltech_retirement") ->
        L2327.GlobalTechLifetime_paper
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2327.GlobalTechLifetime_paper") ->
        L2327.GlobalTechLifetime_paper
    }

    if(exists("L2327.GlobalTechProfitShutdown_paper")) {
      L2327.GlobalTechProfitShutdown_paper %>%
        add_title("Global tech profit shutdown decider and parameters") %>%
        add_units("Unitless, used to determine shape of the function defining the relationship between shutdown rate and profitability") %>%
        add_comments("Filters for any technologies that use a profit-based shutdown parameter") %>%
        add_legacy_name("L2327.GlobalTechProfitShutdown_paper") %>%
        add_precursors("energy/A327.globaltech_retirement") ->
        L2327.GlobalTechProfitShutdown_paper
    } else {
      missing_data() %>%
        add_legacy_name("energy/L2327.GlobalTechProfitShutdown_paper") ->
        L2327.GlobalTechProfitShutdown_paper
    }

    L2327.GlobalTechCapture_paper %>%
      add_title("CO2 capture fractions from global paper production technologies with CCS") %>%
      add_units("Unitless") %>%
      add_comments("For paper sector, the remove fractions from A327.globaltech_co2capture are interpolated into all model years") %>%
      add_legacy_name("L2327.GlobalTechCapture_paper") %>%
      add_precursors("energy/A327.globaltech_co2capture") ->
      L2327.GlobalTechCapture_paper


    L2327.StubTechProd_paper %>%
      add_title("calibrated paper production") %>%
      add_units("EJ") %>%
      add_comments("Values are calculated using L1327.out_Mt_R_paper_Yh, then added GCAM region information and supplysector, subsector, and technology information") %>%
      add_legacy_name("L2327.StubTechProd_paper") %>%
      add_precursors("energy/calibrated_techs",  "common/GCAM_region_names", "L1327.out_Mt_R_paper_Yh") ->
      L2327.StubTechProd_paper

    L2327.StubTechCalInput_paper_heat %>%
      add_title("calibrated paper production") %>%
      add_units("EJ") %>%
      add_comments("Values are calculated using L1327.in_EJ_R_paper_F_Yh then added GCAM region information and supplysector, subsector, technology, and input information") %>%
      add_legacy_name("L2327.StubTechCalInput_paper") %>%
      add_precursors("energy/calibrated_techs", "L1327.in_EJ_R_paper_F_Yh", "common/GCAM_region_names") ->
      L2327.StubTechCalInput_paper_heat

    L2327.StubTechCoef_paper %>%
      add_title("region-specific coefficients of paper production technologies") %>%
      add_units("unitless") %>%
      add_comments("Coefficients calculated based on energy from regional energy (IEA) and production (FAO) data") %>%
      add_legacy_name("L2327.StubTechCoef_paper") %>%
      add_precursors("energy/calibrated_techs", "common/GCAM_region_names", "L1327.IO_GJkg_R_paper_F_Yh") ->
      L2327.StubTechCoef_paper

    L2327.PerCapitaBased_paper %>%
      add_title("per-capita based flag for paper exports final demand") %>%
      add_units("NA") %>%
      add_comments("Per-capita based flags for paper from A327.demand are expanded into all GCAM regions") %>%
      add_legacy_name("L2327.PerCapitaBased_paper") %>%
      add_precursors("energy/A327.demand", "common/GCAM_region_names") ->
      L2327.PerCapitaBased_paper

    L2327.BaseService_paper %>%
      add_title("base-year service output of paper") %>%
      add_units("EJ") %>%
      add_comments("Transformed from L2327.StubTechProd_paper by adding energy.final.demand") %>%
      add_legacy_name("L2327.BaseService_paper") %>%
      add_precursors("energy/A327.demand","L1327.out_Mt_R_paper_Yh", "energy/calibrated_techs", "common/GCAM_region_names") ->
      L2327.BaseService_paper

    L2327.PriceElasticity_paper %>%
      add_title("price elasticity for paper") %>%
      add_units("Unitless") %>%
      add_comments("The elasticity values from A327.demand are expanded into all GCAM_regions") %>%
      add_legacy_name("L2327.PriceElasticity_paper") %>%
      add_precursors("energy/A327.demand", "common/GCAM_region_names") ->
      L2327.PriceElasticity_paper

    L2327.GlobalTechSecOut_paper %>%
      add_title("Secondary output ratios of paper cogeneration technologies") %>%
      add_units("Unitless") %>%
      add_comments("Secondary output ratios are calculated as electricity ratio (Assumed CHP electricity output per unit fuel input) over efficiency") %>%
      add_legacy_name("L2327.GlobalTechSecOut_paper") %>%
      add_precursors("energy/A23.chp_elecratio", "energy/A327.globaltech_coef") ->
      L2327.GlobalTechSecOut_paper

    L2327.DeleteSupplysector_PaperAgDemand %>%
      add_title("Delete forest supplysector used in paper industry") %>%
      add_units("Unitless") %>%
      add_comments("Supplysector is replaced with paper industry demand") %>%
      add_legacy_name("L2327.DeleteSupplysector_PaperAgDemand") %>%
      add_precursors("L203.Supplysector_demand") ->
      L2327.DeleteSupplysector_PaperAgDemand

    L2327.DeleteFinalDemand_PaperAgDemand %>%
      add_title("Delete forest final demand used in paper industry") %>%
      add_units("Unitless") %>%
      add_comments("Final energy demand is replaced with paper industry demand") %>%
      add_legacy_name("L2327.DeleteFinalDemand_PaperAgDemand") %>%
      add_precursors("L203.PerCapitaBased") ->
      L2327.DeleteFinalDemand_PaperAgDemand

    L2327.DeleteSupplysector_PaperAgDemand_USA %>%
      add_title("Delete forest supplysector used in paper industry") %>%
      add_units("Unitless") %>%
      add_comments("Supplysector is replaced with paper industry demand") %>%
      add_legacy_name("L2327.DeleteSupplysector_PaperAgDemand_USA") %>%
      add_precursors("L203.Supplysector_demand") ->
      L2327.DeleteSupplysector_PaperAgDemand_USA

    L2327.DeleteFinalDemand_PaperAgDemand_USA %>%
      add_title("Delete forest final demand used in paper industry") %>%
      add_units("Unitless") %>%
      add_comments("Final energy demand is replaced with paper industry demand") %>%
      add_legacy_name("L2327.DeleteFinalDemand_PaperAgDemand_USA") %>%
      add_precursors("L203.PerCapitaBased") ->
      L2327.DeleteFinalDemand_PaperAgDemand_USA


    return_data(L2327.Supplysector_paper, L2327.FinalEnergyKeyword_paper, L2327.SubsectorLogit_paper, L2327.SubsectorShrwtFllt_paper,
                L2327.SubsectorInterp_paper, L2327.StubTech_paper, L2327.GlobalTechShrwt_paper, L2327.GlobalTechCoef_paper,
                L2327.GlobalTechCost_paper, L2327.GlobalTechCapture_paper, L2327.GlobalTechShutdown_paper, L2327.GlobalTechSCurve_paper,
                L2327.GlobalTechLifetime_paper, L2327.GlobalTechProfitShutdown_paper, L2327.StubTechProd_paper,
                L2327.StubTechCalInput_paper_heat, L2327.StubTechCoef_paper, L2327.PerCapitaBased_paper,
                L2327.BaseService_paper, L2327.PriceElasticity_paper, L2327.GlobalTechSecOut_paper,
                L2327.DeleteSupplysector_PaperAgDemand, L2327.DeleteFinalDemand_PaperAgDemand,
                L2327.DeleteSupplysector_PaperAgDemand_USA, L2327.DeleteFinalDemand_PaperAgDemand_USA,
                L2327.GlobalTechTrackCapital_paper)

  } else {
    stop("Unknown command")
  }
}

