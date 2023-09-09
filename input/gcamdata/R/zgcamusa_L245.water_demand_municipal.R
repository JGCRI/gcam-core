# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L245.water_demand_municipal
#'
#' Genereate GCAM-USA municipal water sector input files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L245.Supplysector_USA}, \code{L245.SubsectorLogit_USA}, \code{L245.SubsectorShrwtFllt_USA},
#' \code{L245.TechShrwt_USA}, \code{L245.TechCoef_USA}, \code{L245.TechCost_USA}, \code{L245.PerCapitaBased_USA},
#' \code{L245.BaseService_USA}, \code{L245.IncomeElasticity_USA}, \code{L245.PriceElasticity_USA}, \code{L245.aeei_USA}.
#' @details This chunk generates the input files of municipal water information (cost, efficiency, coefficients)
#' across US states and model years.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RC March 2019
module_gcamusa_L245.water_demand_municipal <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/A45.sector",
             FILE = "water/A45.tech_cost",
             FILE = "water/A45.demand",
             FILE = "water/water_td_sectors",
             "L145.municipal_water_state_W_Yh_km3",
             "L145.municipal_water_cost_state_75USD_m3",
             "L145.municipal_water_eff_state_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L245.Supplysector_USA",
             "L245.SubsectorLogit_USA",
             "L245.SubsectorShrwtFllt_USA",
             "L245.TechShrwt_USA",
             "L245.TechCoef_USA",
             "L245.TechCost_USA",
             "L245.DeleteSupplysector_USA",
             "L245.DeleteFinalDemand_USA",
             "L245.PerCapitaBased_USA",
             "L245.BaseService_USA",
             "L245.IncomeElasticity_USA",
             "L245.PriceElasticity_USA",
             "L245.aeei_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    logit.type <- state <- water_type <- coefficient <-
      water_sector <- year <- value <- efficiency <- withdrawals <-
      region <- supplysector <- NULL  # silence package check notes

    # Load required inputs
    A45.sector <- get_data(all_data, "water/A45.sector", strip_attributes = TRUE)
    A45.tech_cost <- get_data(all_data, "water/A45.tech_cost", strip_attributes = TRUE)
    A45.demand <- get_data(all_data, "water/A45.demand", strip_attributes = TRUE)
    water_td_sectors <- get_data(all_data, "water/water_td_sectors")
    L145.municipal_water_state_W_Yh_km3 <- get_data(all_data, "L145.municipal_water_state_W_Yh_km3")
    L145.municipal_water_cost_state_75USD_m3 <- get_data(all_data, "L145.municipal_water_cost_state_75USD_m3")
    L145.municipal_water_eff_state_Yh <- get_data(all_data, "L145.municipal_water_eff_state_Yh")

    # ===================================================

    # Join up all the assumptions into a single tibble and expand for all US states
    A45.sector %>%
      left_join_error_no_match(A45.tech_cost, by = "supplysector") %>%
      #First build on single row of all municipal assumptions.
      bind_cols(A45.demand) %>%
      mutate(logit.year.fillout = min(MODEL_YEARS)) %>%
      #Expand assumptions in a single tibble across all states
      repeat_add_columns(tibble(region = gcamusa.STATES)) %>%
      mutate(logit.type = gcamusa.DEFAULT_LOGIT_TYPE) ->
      L245.assumptions_all

    # Supply sector information
    L245.assumptions_all %>%
      select(LEVEL2_DATA_NAMES[["Supplysector"]], logit.type) ->
      L245.Supplysector_USA

    # Subsector logit detail
    L245.assumptions_all %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]], logit.type) ->
      L245.SubsectorLogit_USA

    # Subsector shareweights
    L245.assumptions_all %>%
      # share weights are 1 due to no competition
      mutate(year.fillout = min(MODEL_YEARS),
             share.weight = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) ->
      L245.SubsectorShrwtFllt_USA

    # Technology shareweights
    L245.assumptions_all %>%
      # share weights are 1 due to no competition
      mutate(share.weight = gcamusa.DEFAULT_SHAREWEIGHT) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]]) ->
      L245.TechShrwt_USA

    # Expand the municipal water efficiencies to all model years
    # Keep historical years prior to extrapolating in case there are historical years that aren't model years
    L145.municipal_water_eff_state_Yh %>%
      group_by(state) %>%
      complete(year = sort(unique(c(HISTORICAL_YEARS, MODEL_YEARS)))) %>%
      #Make sure no unwanted years are present
      filter(year %in% unique(c(HISTORICAL_YEARS, MODEL_YEARS))) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) ->
      L245.municipal_water_eff_state_Y

    # Technology coefficients: pass-through demands for withdrawals, some fraction is consumed
    # In GCAM consumptive uses are represented as a fractional input on the withdrawal volumes, so
    # the model parameter is an input-output coefficient rather than an efficiency
    L245.assumptions_all %>%
      select(region, one_of(LEVEL2_DATA_NAMES$Tech)) %>%
      # Use unrestricted left_join allows row expansion for all model years
      left_join(L245.municipal_water_eff_state_Y, by = c("region" = "state")) %>%
      mutate(market.name = region) %>%
      # ^^ repeats tibble for consumption and withdrawal coefficients
      repeat_add_columns(tibble(water_type = water.MAPPED_WATER_TYPES)) %>%
      # ^^ withdrawal coefficient is 1; consumption coefficient is fraction of withdrawal
      mutate(coefficient = gcamusa.DEFAULT_COEFFICIENT,
             coefficient = replace(coefficient, water_type == "water consumption",
                                   round(value[water_type == "water consumption"], water.DIGITS_MUNI_WATER)),
             water_sector = gcamusa.MUNICIPAL_SECTOR,
             minicam.energy.input = set_water_input_name(water_sector, water_type, water_td_sectors)) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
      L245.TechCoef_USA  # municipal water technology withdrawals and consumption efficiencies

    # Delete the USA region so that the modeled state level data can override it
    L245.TechCoef_USA %>%
      select(region, supplysector) %>%
      mutate(region = gcam.USA_REGION,
             energy.final.demand = supplysector) %>%
      unique() ->
      L245.DeleteSupplysector_USA

    tibble(region = gcam.USA_REGION,
           energy.final.demand = A45.sector$supplysector) ->
      L245.DeleteFinalDemand_USA


    # Municipal water non-energy cost
    L245.assumptions_all %>%
      left_join_error_no_match(L145.municipal_water_cost_state_75USD_m3, by = c("region" = "state")) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["TechCost"]]) ->
      L245.TechCost_USA

    # Set final demand as per-capita based
    L245.assumptions_all %>%
      select(LEVEL2_DATA_NAMES[["PerCapitaBased"]]) ->
      L245.PerCapitaBased_USA

    # Municipal water withdrawals for base years
    L245.assumptions_all %>%
      # Use non-restrictive join used to allow expansion across multiple years
      left_join(L145.municipal_water_state_W_Yh_km3, by = c("region" = "state")) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(base.service = value) %>%
      select(LEVEL2_DATA_NAMES[["BaseService"]]) ->
      L245.BaseService_USA

    # Income elasticity projections
    L245.assumptions_all %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["IncomeElasticity"]]) ->
      L245.IncomeElasticity_USA

    # Price elasticity projections
    L245.assumptions_all %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["PriceElasticity"]]) ->
      L245.PriceElasticity_USA

    # Demand efficiency projections
    L245.assumptions_all %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["aeei"]]) ->
      L245.aeei_USA


    # ===================================================
    # Produce outputs

    L245.Supplysector_USA %>%
      add_title("Sector information (municipal water) for US states") %>%
      add_units("Unitless") %>%
      add_comments("A selection of columns from the input water assumptions") %>%
      add_legacy_name("L245.Supplysector_USA") %>%
      add_precursors("water/A45.sector",
                     "water/A45.tech_cost",
                     "water/A45.demand") ->
      L245.Supplysector_USA

    L245.SubsectorLogit_USA %>%
      add_title("Subsector logit detail (municipal water) for US states") %>%
      add_units("Unitless") %>%
      add_comments("A selection of columns from the input water assumptions") %>%
      add_legacy_name("L245.SubsectorLogit_USA") %>%
      add_precursors("water/A45.sector",
                     "water/A45.tech_cost") ->
      L245.SubsectorLogit_USA

    L245.SubsectorShrwtFllt_USA %>%
      add_title("Subsector shareweights (municipal water) for US states") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights of 1 for all states due to no competition)") %>%
      add_legacy_name("L245.SubsectorShrwtFllt_USA") %>%
      add_precursors("water/A45.sector",
                     "water/A45.tech_cost") ->
      L245.SubsectorShrwtFllt_USA

    L245.TechShrwt_USA %>%
      add_title("Technology share weights (municipal water) for US states") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights of 1 for all states/years due to no competition") %>%
      add_legacy_name("L245.TechShrwt_USA") %>%
      add_precursors("water/A45.sector",
                     "water/A45.tech_cost") ->
      L245.TechShrwt_USA

    L245.TechCoef_USA %>%
      add_title("Municipal water technology consumption efficiencies for US states") %>%
      add_units("Unitless") %>%
      add_comments("Withdrawal efficiencies bound to consumption and appropriate minicam.energy.input appended") %>%
      add_legacy_name("L245.TechCoef_USA") %>%
      add_precursors("water/A45.sector",
                     "water/A45.tech_cost",
                     "water/water_td_sectors",
                     "L145.municipal_water_eff_state_Yh") ->
      L245.TechCoef_USA

    L245.TechCost_USA %>%
      add_title("Municipal water non-energy costs for US states") %>%
      add_units("1975USD/m3") %>%
      add_comments("Costs joined to sector infromation and expanded for all model years") %>%
      add_legacy_name("L245.TechCost_USA") %>%
      add_precursors("water/A45.sector",
                     "water/A45.tech_cost",
                     "water/A45.demand",
                     "L145.municipal_water_cost_state_75USD_m3") ->
      L245.TechCost_USA

    L245.DeleteSupplysector_USA %>%
      add_title("Remove municipal water withdrawal and consumption of the USA region") %>%
      add_units("Uniteless") %>%
      add_comments("Remove the USA municipal demands to make way for state level") %>%
      add_legacy_name("L2232.DeleteSupplysector_USA") ->
      L245.DeleteSupplysector_USA

    L245.DeleteFinalDemand_USA %>%
      add_title("Remove municipal water withdrawal and consumption of the USA region") %>%
      add_units("Uniteless") %>%
      add_comments("Remove the USA municipal demands to make way for state level") %>%
      add_legacy_name("L2232.DeleteSupplysector_USA") ->
      L245.DeleteFinalDemand_USA

    L245.PerCapitaBased_USA %>%
      add_title("Per-capital based final energy demand switch (municipal water) for US states") %>%
      add_units("NA") %>%
      add_comments("Final energy demand category repeated for all states") %>%
      add_legacy_name("L245.PerCapitaBased_USA") %>%
      add_precursors("water/A45.demand") ->
      L245.PerCapitaBased_USA

    L245.BaseService_USA %>%
      add_title("Municipal water withdrawals for US states and base years") %>%
      add_units("km3") %>%
      add_comments("Withdrawals filtered for base years and combined with demand category") %>%
      add_legacy_name("L245.BaseService_USA") %>%
      add_precursors("water/A45.demand",
                     "L145.municipal_water_state_W_Yh_km3") ->
      L245.BaseService_USA

    L245.IncomeElasticity_USA %>%
      add_title("Income elasticity projections (municipal water) for US states") %>%
      add_units("Unitless") %>%
      add_comments("State income elasticity repeated out for future years") %>%
      add_legacy_name("L245.IncomeElasticity_USA") %>%
      add_precursors("water/A45.demand")  ->
      L245.IncomeElasticity_USA

    L245.PriceElasticity_USA %>%
      add_title("Price elasticity projections (municipal water) for US states") %>%
      add_units("Unitless") %>%
      add_comments("State price elasticity repeated out for all years") %>%
      add_legacy_name("L245.PriceElasticity_USA") %>%
      add_precursors("water/A45.demand")  ->
      L245.PriceElasticity_USA

    L245.aeei_USA %>%
      add_title("Demand efficiency projections (municipal water) for US state") %>%
      add_units("Unitless") %>%
      add_comments("State efficiency repeated out for all years") %>%
      add_legacy_name("L245.aeei_USA") %>%
      add_precursors("water/A45.demand") ->
      L245.aeei_USA

    return_data(L245.Supplysector_USA, L245.SubsectorLogit_USA, L245.SubsectorShrwtFllt_USA,
                L245.TechShrwt_USA, L245.TechCoef_USA, L245.TechCost_USA, L245.DeleteSupplysector_USA, L245.DeleteFinalDemand_USA, L245.PerCapitaBased_USA,
                L245.BaseService_USA, L245.IncomeElasticity_USA, L245.PriceElasticity_USA, L245.aeei_USA)
  } else {
    stop("Unknown command")
  }
}
