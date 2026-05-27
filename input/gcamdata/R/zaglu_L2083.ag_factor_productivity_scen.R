# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L2083.ag_factor_productivity_scen
#'
#' Construct XML data structure for \code{ag_input_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs.
#' @details This chunk compute IO coefficients for labor and capital uses in Ag and forest
#' Adjust nonLandVariableCost to remove the explicitly computed labor and capital costs.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else left_join mutate select
#' @importFrom tidyr replace_na gather
#' @importFrom tibble tibble
#' @author XZ DS 2025

module_aglu_L2083.ag_factor_productivity_scen <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "socioeconomics/A_labor_supply_sector",
      "L2082.AgCoef_laborcapital_ag_irr_mgmt_hist",
      "L2082.AgCoef_laborcapital_bio_irr_mgmt_hist",
      "L2082.StubTechCoef_laborcapital_an_hist",
      "L2082.AgCoef_laborcapital_for_hist",
      "L2053.AgLand_BiophysicalProducitivityRatio_ag_R_C_Y_scen")

  MODULE_OUTPUTS <-
    c("L2083.AgLand_BiophysicalProducitivityRatio_ag_R_Y_scen",
      "L2083.AgLabor_ProducitivityRatio_R_Y_scen",
      "L2083.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA",
      "L2083.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA",
      "L2083.StubTechCoef_laborcapital_an_tfp_MA",
      "L2083.AgCoef_laborcapital_for_tfp_MA")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # bind future years
    L2082.AgCoef_laborcapital_ag_irr_mgmt_hist %>%
      filter(year %in% MODEL_FINAL_BASE_YEAR) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(L2082.AgCoef_laborcapital_ag_irr_mgmt_hist) ->
      L2082.AgCoef_laborcapital_ag_irr_mgmt

    # bind future years
    L2082.AgCoef_laborcapital_bio_irr_mgmt_hist %>%
      filter(year %in% MODEL_FINAL_BASE_YEAR) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(L2082.AgCoef_laborcapital_bio_irr_mgmt_hist) ->
      L2082.AgCoef_laborcapital_bio_irr_mgmt

    # bind future years w/o tfp
    L2082.StubTechCoef_laborcapital_an_hist %>%
      filter(year %in% MODEL_FINAL_BASE_YEAR) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(L2082.StubTechCoef_laborcapital_an_hist) ->
      L2082.StubTechCoef_laborcapital_an

    # bind future years
    L2082.AgCoef_laborcapital_for_hist %>%
      filter(year %in% MODEL_FINAL_BASE_YEAR) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(L2082.AgCoef_laborcapital_for_hist) ->
      L2082.AgCoef_laborcapital_for


    # Adding future productivity growth --------

    # Get land productivity and aggregate to regional values

    L2053.AgLand_BiophysicalProducitivityRatio_ag_R_C_Y_scen %>%
      filter(scenario == "ref") %>%
      # remove fodder crops (no good data/assumptions)
      filter(!grepl("Fodder", GCAM_commodity, ignore.case = T)) %>%
      group_by(scenario, region, year) %>%
      summarize(Yield = weighted.mean(Yield, w = LandWeight, na.rm = T),
                .groups = "drop") %>%
      group_by_at(vars(-year, -Yield)) %>%
      mutate(value = Yield/first(Yield)) %>%
      ungroup() %>%
      select(-Yield) ->
      L2083.AgLand_BiophysicalProducitivityRatio_ag_R_Y_scen

    # get regional labor supply info
    assertthat::assert_that(
      identical(
        GCAM_region_names %>% distinct(GCAM_region_ID, region) %>% arrange,
        A_labor_supply_sector %>% distinct(GCAM_region_ID, region) %>% arrange) )

    LaborSupply_Reg <- A_labor_supply_sector %>%
      distinct(region, region_AgProdGrowth)

    # Assumptions for deriving Ag labor productivity relative to land
    # For regions with high growth: labor is 3X land in growth rate
    # For other regions: labor is 2X land in growth rate
    # The assumption was base on SSP2 land productivity
    # Will examine alternative scenarios


    L2083.AgLand_BiophysicalProducitivityRatio_ag_R_Y_scen %>%
      filter(scenario == "ref") %>%
      mutate(scenario = "CORE") %>%
      group_by_at(vars(-year,-value)) %>%
      # inter period growth rate
      mutate(value = value / lag(value) - 1)  %>%
      replace_na(list(value = 0)) %>%
      # derive scenario of 2X and 5X land productivity growth
      mutate(Productivity_Land_2X = cumprod(value * 2 + 1),
             Productivity_Land_3X = cumprod(value * 3 + 1),
             Productivity_Land_4X = cumprod(value * 4 + 1)) %>%
      ungroup %>%
      left_join_error_no_match(
        LaborSupply_Reg, by = "region") %>%
      # for high labor productivity growth regions, using 4x, low using 2x other 3x
      transmute(
        scenario, region, year,
        productivity = case_when(
          region_AgProdGrowth == "High" ~ Productivity_Land_4X,
          region_AgProdGrowth == "Low" ~ Productivity_Land_2X,
          TRUE ~ Productivity_Land_3X) ) ->
      L2083.AgLabor_ProducitivityRatio_R_Y_scen_original

    # using exponential growth here
    # land productivity assumption could be too concave (sometimes kink in 2050)
    alpha <- 0.7 # parameter in the power function to reshape log linear to make it less convex (vs log linear)
    # when alpha is 1 it becomes log linear
    L2083.AgLand_BiophysicalProducitivityRatio_ag_R_Y_scen %>%
      filter(scenario == "ref") %>%
      mutate(scenario = "CORE") %>%
      group_by_at(vars(-year, -value)) %>%
      mutate(CumYearStep = year - first(year),
             AnnualGrowth = value ^(1/CumYearStep) - 1,
             AnnualGrwoth2100 = AnnualGrowth[year == 2100],
             LandProductivity_smoothed = (1 + AnnualGrwoth2100)^CumYearStep,
             LandProductivity_smoothed2X = (1 + AnnualGrwoth2100 *2)^CumYearStep,
             LandProductivity_smoothed3X = (1 + AnnualGrwoth2100 *3)^CumYearStep,
             LandProductivity_smoothed4X = (1 + AnnualGrwoth2100 *4)^CumYearStep,
             # using generalized methods here; power function to reshape log linear to less convex
             CumTimeProgress_S = (year - first(year)) / (2100 - first(year)),
             LandProductivity_shaped   = (LandProductivity_smoothed[year == 2100])^(CumTimeProgress_S^alpha),
             LandProductivity_shaped2X = (LandProductivity_smoothed2X[year == 2100])^(CumTimeProgress_S^alpha),
             LandProductivity_shaped3X = (LandProductivity_smoothed3X[year == 2100])^(CumTimeProgress_S^alpha),
             LandProductivity_shaped4X = (LandProductivity_smoothed4X[year == 2100])^(CumTimeProgress_S^alpha)) %>%
      select(-CumYearStep, -AnnualGrowth, -AnnualGrwoth2100, -CumTimeProgress_S) %>%
      ungroup() %>%
      left_join_error_no_match(
        LaborSupply_Reg, by = "region") %>%
      # for high labor productivity growth regions, using 4x, low using 2x other 3x
      transmute(
        scenario, region, year,
        productivity = case_when(
          region_AgProdGrowth == "High" ~ LandProductivity_shaped4X,
          region_AgProdGrowth == "Low" ~ LandProductivity_shaped2X,
          TRUE ~ LandProductivity_smoothed3X) ) ->
      L2083.AgLabor_ProducitivityRatio_R_Y_scen_smoothed_reshaped

    L2083.AgLabor_ProducitivityRatio_R_Y_scen <-
      L2083.AgLabor_ProducitivityRatio_R_Y_scen_smoothed_reshaped

    # create value of 1 for historical periods
    L2083.AgLabor_ProducitivityRatio_R_Y_scen %>%
      filter(year == min(MODEL_FUTURE_YEARS)) %>%
      mutate(productivity = 1) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      bind_rows(
        L2083.AgLabor_ProducitivityRatio_R_Y_scen %>%
          filter(year >= min(MODEL_FUTURE_YEARS)) ) %>%
      # no negative growth
      mutate(productivity = ifelse(productivity < 1, 1, productivity),
             LaborIO_Scaler = 1 / productivity) %>%
      filter(scenario == "CORE") ->
      L2083.AgLaborIOCoef_Scaler_DerivedBasedOnLand


    L2083.AgLaborIOCoef_Scaler <-
      L2083.AgLaborIOCoef_Scaler_DerivedBasedOnLand

    L2082.AgCoef_laborcapital_ag_irr_mgmt %>%
      left_join_error_no_match(L2083.AgLaborIOCoef_Scaler, by = c("region", "year")) %>%
      mutate(coefficient =
               if_else(minicam.energy.input == socioeconomics.LABOR_AG_MARKET_NAME,
                      coefficient * LaborIO_Scaler,
                      coefficient)) %>%
      # apply tfp to labor only: coefficient includes both labor and capital IO
      ungroup() %>%
      select(colnames(L2082.AgCoef_laborcapital_ag_irr_mgmt)) ->
      L2083.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA

    L2082.AgCoef_laborcapital_bio_irr_mgmt %>%
      left_join_error_no_match(L2083.AgLaborIOCoef_Scaler, by = c("region", "year")) %>%
      mutate(coefficient =
               if_else(minicam.energy.input == socioeconomics.LABOR_AG_MARKET_NAME,
                      coefficient * LaborIO_Scaler,
                      coefficient)) %>% # apply tfp to labor only: coefficient includes both labor and capital IO
      ungroup() %>%
      select(colnames(L2082.AgCoef_laborcapital_bio_irr_mgmt)) ->
      L2083.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA

    L2082.StubTechCoef_laborcapital_an %>%
      left_join_error_no_match(L2083.AgLaborIOCoef_Scaler, by = c("region", "year")) %>%
      mutate(coefficient =
               if_else(minicam.energy.input == socioeconomics.LABOR_AG_MARKET_NAME,
                      coefficient * LaborIO_Scaler,
                      coefficient)) %>% # apply tfp to labor only: coefficient includes both labor and capital IO
      ungroup() %>%
      select(colnames(L2082.StubTechCoef_laborcapital_an)) ->
      L2083.StubTechCoef_laborcapital_an_tfp_MA

    L2082.AgCoef_laborcapital_for %>%
      left_join_error_no_match(L2083.AgLaborIOCoef_Scaler, by = c("region", "year")) %>%
      mutate(coefficient =
               if_else(minicam.energy.input == socioeconomics.LABOR_AG_MARKET_NAME,
                      coefficient * LaborIO_Scaler,
                      coefficient)) %>% # apply tfp to labor only: coefficient includes both labor and capital IO
      ungroup() %>%
      select(colnames(L2082.AgCoef_laborcapital_for)) ->
      L2083.AgCoef_laborcapital_for_tfp_MA

    # Produce outputs ----

    L2083.AgLand_BiophysicalProducitivityRatio_ag_R_Y_scen %>%
      add_title("Regional aggregated agricultural land productivity ratio, relative to final base year, aggregrated using initial land weights by scenario") %>%
      add_units("Unitless") %>%
      add_legacy_name("L2083.AgLand_BiophysicalProducitivityRatio_ag_R_Y_scen") %>%
      add_precursors("L2053.AgLand_BiophysicalProducitivityRatio_ag_R_C_Y_scen") ->
      L2083.AgLand_BiophysicalProducitivityRatio_ag_R_Y_scen

    L2083.AgLabor_ProducitivityRatio_R_Y_scen %>%
      add_title("Agricultural labor productivity ratio relative to final base year by scenario") %>%
      add_units("Unitless") %>%
      add_legacy_name("L2083.AgLabor_ProducitivityRatio_R_Y_scen") %>%
      add_precursors("L2083.AgLand_BiophysicalProducitivityRatio_ag_R_Y_scen",
                     "common/GCAM_region_names",
                     "socioeconomics/A_labor_supply_sector") ->
      L2083.AgLabor_ProducitivityRatio_R_Y_scen


    L2083.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA %>%
      mutate(price.unit.conversion = if_else(minicam.energy.input == socioeconomics.LABOR_AG_MARKET_NAME, 1 / gdp_deflator(1990, 1975) / 1000, 1)) %>%  # (1990 $/ppl) to (thou 1975 $/ppl)
      add_title("labor and capital coefficients for agricultural technologies with MA tfp") %>%
      add_units("index") %>%
      add_comments("Note: we are using theta to differentiate coefficient across four management technologies (irrigated, rainfed, hi and lo") %>%
      add_legacy_name("L2083.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA") %>%
      add_precursors(MODULE_INPUTS) ->
      L2083.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA

    L2083.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA %>%
      mutate(price.unit.conversion = if_else(minicam.energy.input == socioeconomics.LABOR_AG_MARKET_NAME, 1 / gdp_deflator(1990, 1975) / 1000, 1)) %>%  # (1990 $/ppl) to (thou 1975 $/ppl)
      add_title("labor and capital coefficients for bioenergy technologies with MA tfp") %>%
      add_units("index") %>%
      add_comments("Compute bioenergy labor and capital coefficients") %>%
      add_legacy_name("L2083.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA") %>%
      add_precursors(MODULE_INPUTS) ->
      L2083.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA

    L2083.StubTechCoef_laborcapital_an_tfp_MA %>%
      mutate(price.unit.conversion = if_else(minicam.energy.input == socioeconomics.LABOR_AG_MARKET_NAME, 1 / gdp_deflator(1990, 1975) / 1000, 1)) %>%  # (1990 $/ppl) to (thou 1975 $/ppl)
      add_title("labor and capital coefficients for livestock technologies with MA tfp") %>%
      add_units("index") %>%
      add_comments("labor and capital coefficients for livestock sectors except fish") %>%
      add_legacy_name("L2082.StubTechCoef_laborcapital_tfp_MA") %>%
      add_precursors(MODULE_INPUTS) ->
      L2083.StubTechCoef_laborcapital_an_tfp_MA

    L2083.AgCoef_laborcapital_for_tfp_MA %>%
      mutate(price.unit.conversion = if_else(minicam.energy.input == socioeconomics.LABOR_AG_MARKET_NAME, 1 / gdp_deflator(1990, 1975) / 1000, 1)) %>%  # (1990 $/ppl) to (thou 1975 $/ppl)
      add_title("labor and capital coefficients for forest technologies with MA tfp") %>%
      add_units("index") %>%
      add_comments("Compute bioenergy labor and capital coefficients") %>%
      add_legacy_name("L2083.AgCoef_laborcapital_for_tfp_MA") %>%
      add_precursors(MODULE_INPUTS) ->
      L2083.AgCoef_laborcapital_for_tfp_MA

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
