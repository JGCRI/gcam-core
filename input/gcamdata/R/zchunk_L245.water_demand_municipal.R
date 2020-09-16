# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L245.water_demand_municipal
#'
#' Expands municipal water information (cost, efficiency, coefficients) across regions and model years
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L245.Supplysector}, \code{L245.SubsectorLogit}, \code{L245.SubsectorShrwtFllt}, \code{L245.TechShrwt}, \code{L245.TechCoef}, \code{L245.TechCost}, \code{L245.PerCapitaBased}, \code{L245.BaseService}, \code{L245.IncomeElasticity}, \code{L245.PriceElasticity}, \code{L245.aeei}. The corresponding file in the
#' original data system was \code{L245.water_demand_municipal.R} (water level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_cols filter if_else group_by left_join mutate one_of select
#' @importFrom tidyr complete
#' @author ST August 2017
module_water_L245.water_demand_municipal <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/water_td_sectors",
             FILE = "water/A45.sector",
             FILE = "water/A45.tech_cost",
             FILE = "water/A45.demand",
             "L145.municipal_water_R_W_Yh_km3",
             "L145.municipal_water_cost_R_75USD_m3",
             "L145.municipal_water_eff_R_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L245.Supplysector",
             "L245.SubsectorLogit",
             "L245.SubsectorShrwtFllt",
             "L245.TechShrwt",
             "L245.TechCoef",
             "L245.TechCost",
             "L245.PerCapitaBased",
             "L245.BaseService",
             "L245.IncomeElasticity",
             "L245.PriceElasticity",
             "L245.aeei"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    logit.type <- GCAM_region_ID <- region <- water_type <- coefficient <-
      water_sector <- year <- value <- efficiency <- withdrawals <-
      supplysector <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    water_td_sectors <- get_data(all_data, "water/water_td_sectors")
    A45.sector <- get_data(all_data, "water/A45.sector", strip_attributes = TRUE)
    A45.tech_cost <- get_data(all_data, "water/A45.tech_cost")
    A45.demand <- get_data(all_data, "water/A45.demand")
    L145.municipal_water_R_W_Yh_km3 <- get_data(all_data, "L145.municipal_water_R_W_Yh_km3")
    L145.municipal_water_cost_R_75USD_m3 <- get_data(all_data, "L145.municipal_water_cost_R_75USD_m3")
    L145.municipal_water_eff_R_Yh <- get_data(all_data, "L145.municipal_water_eff_R_Yh")

    # ===================================================

    # join up all the assumptions into a single tibble and expand for all regions
    A45.sector %>%
      left_join_error_no_match(A45.tech_cost, by = "supplysector") %>%
      bind_cols(A45.demand) %>%
      mutate(logit.year.fillout = MODEL_YEARS[1]) %>%
      repeat_add_columns(GCAM_region_names) %>%
      mutate(logit.type = NA) ->
      L245.assumptions_all

    L245.assumptions_all %>%
      select(LEVEL2_DATA_NAMES$Supplysector, logit.type) ->
      L245.Supplysector  # Supply sector information

    L245.assumptions_all %>%
      select(LEVEL2_DATA_NAMES$SubsectorLogit, logit.type) ->
      L245.SubsectorLogit  # Subsector logit detail

    L245.assumptions_all %>%
      mutate(year.fillout = MODEL_YEARS[1],
             share.weight = 1) %>%
      # ^^ share weights are 1 due to no competition
      select(LEVEL2_DATA_NAMES$SubsectorShrwtFllt) ->
      L245.SubsectorShrwtFllt  # Subsector shareweights

    L245.assumptions_all %>%
      mutate(share.weight = 1) %>%
      # ^^ share weights are 1 due to no competition
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES$TechShrwt) ->
      L245.TechShrwt

    # Expand the municipal water efficiencies to all model years
    # Keep historical years prior to extrapolating in case there are historical years that aren't model years
    L245.municipal_water_eff_R_Y <-
      group_by(L145.municipal_water_eff_R_Yh, GCAM_region_ID) %>%
      complete(year = sort(unique(c(HISTORICAL_YEARS, MODEL_YEARS)))) %>%
      mutate(efficiency = approx_fun(year, efficiency, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS)

    # This is confusing; in GCAM consumptive uses are represented as a fractional input on the withdrawal volumes, so
    # the model parameter is an input-output coefficient rather than an efficiency
    L245.assumptions_all %>%
      select(GCAM_region_ID, one_of(LEVEL2_DATA_NAMES$Tech)) %>%
      # ^^ unrestricted left_join allows row expansion for all model years
      left_join(L245.municipal_water_eff_R_Y, by = "GCAM_region_ID") %>%
      mutate(market.name = region) %>%
      # ^^ repeats tibble for consumption and withdrawal coefficients
      repeat_add_columns(tibble(water_type = c("water consumption", "water withdrawals"))) %>%
      # ^^ withdrawal coefficient is 1; consumption coefficient is fraction of withdrawal
      mutate(coefficient = if_else(water_type == "water withdrawals", 1,
                                   round(efficiency, water.DIGITS_MUNI_WATER)),
             water_sector = "Municipal",
             minicam.energy.input = set_water_input_name(water_sector, water_type, water_td_sectors)) %>%
      select(LEVEL2_DATA_NAMES$TechCoef) ->
      L245.TechCoef  # municipal water technology withdrawals and consumption efficiencies

    L245.assumptions_all %>%
      left_join_error_no_match(L145.municipal_water_cost_R_75USD_m3, by = "GCAM_region_ID") %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES$TechCost) ->
      L245.TechCost  # Municipal water non-energy cost

    L245.assumptions_all %>%
      select(LEVEL2_DATA_NAMES$PerCapitaBased) ->
      L245.PerCapitaBased  # used to set final demand as per-capita based

    L245.assumptions_all %>%
      left_join(L145.municipal_water_R_W_Yh_km3, by = "GCAM_region_ID") %>%
      # ^^ non-restrictive join used to allow expansion across multiple years
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(base.service = withdrawals) %>%
      select(LEVEL2_DATA_NAMES$BaseService) ->
      L245.BaseService  # municipal water withdrawals for base years

    L245.assumptions_all %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES$IncomeElasticity) ->
      L245.IncomeElasticity  # income elasticity projections

    L245.assumptions_all %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES$PriceElasticity) ->
      L245.PriceElasticity  # price elasticity projections

    L245.assumptions_all %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES$aeei) ->
      L245.aeei  # demand efficiency projections


    #==== OUTPUT ===========

    L245.Supplysector %>%
      add_title("Sector information (municipal water)") %>%
      add_units("Unitless") %>%
      add_comments("A selection of columns from the input water assumptions") %>%
      add_legacy_name("L245.Supplysector") %>%
      add_precursors("common/GCAM_region_names",
                     "water/A45.sector",
                     "water/A45.tech_cost",
                     "water/A45.demand") ->
      L245.Supplysector

    L245.SubsectorLogit %>%
      add_title("Subsector logit detail (municipal water)") %>%
      add_units("Unitless") %>%
      add_comments("A selection of columns from the input tibbles") %>%
      add_legacy_name("L245.SubsectorLogit") %>%
      add_precursors("common/GCAM_region_names",
                     "water/A45.sector",
                     "water/A45.tech_cost") ->
      L245.SubsectorLogit

    L245.SubsectorShrwtFllt %>%
      add_title("Subsector shareweights (municipal water)") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights of 1 for all regions due to no competition)") %>%
      add_legacy_name("L245.SubsectorShrwtFllt") %>%
      add_precursors("common/GCAM_region_names",
                     "water/A45.sector",
                     "water/A45.tech_cost") ->
      L245.SubsectorShrwtFllt

    L245.TechShrwt %>%
      add_title("Technology share weights (municipal water)") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights of 1 for all regions/years due to no competition") %>%
      add_legacy_name("L245.TechShrwt") %>%
      add_precursors("common/GCAM_region_names",
                     "water/A45.sector",
                     "water/A45.tech_cost") ->
      L245.TechShrwt

    L245.TechCoef %>%
      add_title("Municipal water technology consumption efficiencies") %>%
      add_units("Unitless") %>%
      add_comments("Withdrawal efficiencies bound to consumption and appropriate minicam.energy.input appended") %>%
      add_legacy_name("L245.TechCoef") %>%
      add_precursors("common/GCAM_region_names",
                     "water/water_td_sectors",
                     "water/A45.sector",
                     "water/A45.tech_cost",
                     "L145.municipal_water_eff_R_Yh") ->
      L245.TechCoef

    L245.TechCost %>%
      add_title("Municipal water non-energy costs") %>%
      add_units("1975USD/m3") %>%
      add_comments("Costs joined to sector infromation and expanded for all model years") %>%
      add_legacy_name("L245.TechCost") %>%
      add_precursors("common/GCAM_region_names",
                     "water/A45.sector",
                     "water/A45.tech_cost",
                     "water/A45.demand",
                     "L145.municipal_water_cost_R_75USD_m3") ->
      L245.TechCost

    L245.PerCapitaBased %>%
      add_title("Per-capital based final energy demand switch (municipal water)") %>%
      add_units("NA") %>%
      add_comments("final energy demand category repeated for all regions") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L245.PerCapitaBased") %>%
      add_precursors("common/GCAM_region_names",
                     "water/A45.demand") ->
      L245.PerCapitaBased

    L245.BaseService %>%
      add_title("Municipal water withdrawals for base years") %>%
      add_units("km^3") %>%
      add_comments("Withdrawals filtered for base years and combined with demand category") %>%
      add_legacy_name("L245.BaseService") %>%
      add_precursors("common/GCAM_region_names",
                     "water/A45.demand",
                     "L145.municipal_water_R_W_Yh_km3") ->
      L245.BaseService

    L245.IncomeElasticity %>%
      add_title("Income elasticity projections (municipal water)") %>%
      add_units("Unitless") %>%
      add_comments("Regional income elasticity repeated out for future years") %>%
      add_legacy_name("L245.IncomeElasticity") %>%
      add_precursors("common/GCAM_region_names",
                     "water/A45.demand")  ->
      L245.IncomeElasticity

    L245.PriceElasticity %>%
      add_title("Price elasticity projections (municipal water)") %>%
      add_units("Unitless") %>%
      add_comments("Regional price elasticity repeated out for all years") %>%
      add_legacy_name("L245.PriceElasticity") %>%
      add_precursors("common/GCAM_region_names",
                     "water/A45.demand")  ->
      L245.PriceElasticity

    L245.aeei %>%
      add_title("Demand efficiency projections (municipal water)") %>%
      add_units("Unitless") %>%
      add_comments("Regional efficiency repeated out for all years") %>%
      add_legacy_name("L245.aeei") %>%
      add_precursors("common/GCAM_region_names",
                     "water/A45.demand") ->
      L245.aeei

    return_data(L245.Supplysector, L245.SubsectorLogit, L245.SubsectorShrwtFllt, L245.TechShrwt, L245.TechCoef, L245.TechCost, L245.PerCapitaBased, L245.BaseService, L245.IncomeElasticity, L245.PriceElasticity, L245.aeei)
  } else {
    stop("Unknown command")
  }
}
