# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L2053.ag_land_prodchange
#'
#' Specify future agricultural productivity changes for all technologies.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2053.AgProdChange_ag_irr_ref}, \code{L2053.AgProdChange_bio_irr_ref},
#' \code{L2053.AgProdChange_irr_high}, \code{L2053.AgProdChange_irr_low}, \code{L2053.AgProdChange_irr_ssp4}.
#' @details This chunk calculates future productivity change of crops and biomass for all technologies along reference, high, low and SSP4 scenarios.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter mutate one_of pull select
#' @importFrom tidyr replace_na separate
#' @author RC July 2017 XZ 2025
module_aglu_L2053.ag_land_prodchange <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "water/basin_to_country_mapping",
      "L162.ag_YieldRate_R_C_Y_GLU_irr",
      "L162.bio_YieldRate_R_Y_GLU_irr",
      "L201.AgYield_bio_grass",
      "L201.AgYield_bio_tree",
      "L102.pcgdp_thous90USD_Scen_R_Y",
      "L181.LC_bm2_R_C_Yh_GLU_irr_level",
      "L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level")

  MODULE_OUTPUTS <-
    c("L2053.AgProdChange_ag_irr_ref",
      "L2053.AgProdChange_bio_irr_ref",
      "L2053.AgProdChange_irr_high",
      "L2053.AgProdChange_irr_low",
      "L2053.AgProdChange_irr_ssp4",
      "L2053.AgLand_BiophysicalProducitivityRatio_ag_R_C_Y_scen")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    names_AgTech <- names_AgCost <- names_AgProdChange <- GLU <- GLU_name <- IRR_RFD <-
      MGMT <- AgSupplySector <- AgSupplySubsector <- AgProductionTechnology <-
      AgProdChange <- nonLandVariableCost <- high_reg <- low_reg <- region <-
      GCAM_region_ID <- year <- value <- GCAM_commodity <- Cost_75USDkg <-
      Irr_Rfd <- scenario <- calPrice <- cost_PrP_ratio <- . <- GCAM_subsector <- NULL  # silence package check notes

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Future agricultural productivity changes ----
    # Specify reference scenario agricultural productivity change for crops (not incl biomass)
    L162.ag_YieldRate_R_C_Y_GLU_irr %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      mutate(AgProdChange = round(value, digits = aglu.DIGITS_AGPRODCHANGE)) %>%
      # If the final calibration year is less than the final historical year,
      # this method will return Inf for crops that are 0 in one year,
      # and non-zero in subsequent years (e.g. Korea and FSU FodderGrass).
      # Set the Inf to 0, and keep the technologies out.
      mutate(AgProdChange = replace(AgProdChange, AgProdChange == Inf, 0)) %>%
      # Copy costs to high and low management levels
      repeat_add_columns(tibble(MGMT = c("hi", "lo"))) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      # Add sector, subsector, technology names
      mutate(AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_subsector, GLU_name, sep = aglu.CROP_GLU_DELIMITER),
             AgProductionTechnology = paste(paste(AgSupplySubsector, Irr_Rfd, sep = aglu.IRR_DELIMITER),
                                            MGMT, sep = aglu.MGMT_DELIMITER)) %>%
      select(all_of(LEVEL2_DATA_NAMES[["AgProdChange"]])) ->
      L2053.AgProdChange_ag_irr_ref

    # Specify reference scenario agricultural productivity change for biomass
    L162.bio_YieldRate_R_Y_GLU_irr %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      mutate(AgProdChange = round(value, digits = aglu.DIGITS_AGPRODCHANGE)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) ->
      L2051.AgProdChange_bio_irr_ref

    names_AgTech <- LEVEL2_DATA_NAMES[["AgTech"]]
    # Use the yield table to determine where bioenergy crops are being read in, get both grass and tree crops
    L201.AgYield_bio_grass %>%
      select(all_of(names_AgTech)) %>%
      unique() %>%
      bind_rows(unique(select(L201.AgYield_bio_tree, one_of(names_AgTech)))) %>%
      # Copy to all future years
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      # Copy to both irrigated and rainfed technologies
      repeat_add_columns(tibble(IRR_RFD = c("IRR", "RFD"))) %>%
      # Separate the AgProductionTechnology variable to get GLU names for matching in the yield change rates
      separate(AgProductionTechnology, c("biomass", "GLU_name"), sep = aglu.CROP_GLU_DELIMITER) %>%
      # Map in yield change rates, the same values for bioenergy crops are applied equally to grass and tree crops.
      # NA expected and replaced later
      left_join(L2051.AgProdChange_bio_irr_ref %>%
                  select(region, GLU_name, biomass = GCAM_subsector, IRR_RFD = Irr_Rfd, year, AgProdChange),
                by = c("region", "biomass", "GLU_name", "year", "IRR_RFD")) %>%

      # Note: Grass crops are available in any land use regions with crop production, and tree crops are available in any region with forests.
      # Because the yield growth rates are based on crops, some places that have forests but no cropland will not have yield improvement rates.
      # These regions are assumed minor agriculturally and as such not assigned yield improvement for tree-based bioenergy crops.
      replace_na(list(AgProdChange = 0)) %>%
      # Copy coefficients to high and low management levels
      repeat_add_columns(tibble(MGMT = c("hi", "lo"))) %>%
      # Revise technology names to add all technologies
      mutate(AgProductionTechnology = paste(paste(AgSupplySubsector, IRR_RFD, sep = aglu.CROP_GLU_DELIMITER),
                                            MGMT, sep = aglu.MGMT_DELIMITER)) %>%
      select(all_of(LEVEL2_DATA_NAMES[["AgProdChange"]])) ->
      L2053.AgProdChange_bio_irr_ref

    # align parameter in MODEL_SCENARIO_ALIGN_YEAR across scenarios

    # Specify the scenario with high agricultural productivity change (not incl biomass)
    L2053.AgProdChange_ag_irr_ref %>%
      # Use the high growth multiplier
      mutate(AgProdChange = if_else(year >= MODEL_SCENARIO_ALIGN_YEAR,
                                    AgProdChange * aglu.HI_PROD_GROWTH_MULT,
                                    AgProdChange)) ->
      L2053.AgProdChange_irr_high

    # Specify the scenario with low agricultural productivity change (not incl biomass)
    L2053.AgProdChange_ag_irr_ref %>%
      # Use the low growth multiplier
      mutate(AgProdChange = if_else(year >= MODEL_SCENARIO_ALIGN_YEAR,
                                    AgProdChange * aglu.LOW_PROD_GROWTH_MULT,
                                    AgProdChange)) ->
      L2053.AgProdChange_irr_low

    # Specify the SSP4 scenario with diverging agricultural productivity change
    # between high, median, and low income regions (not incl biomass)

    # Get the region list of high income countries
    get_ssp_regions(L102.pcgdp_thous90USD_Scen_R_Y, GCAM_region_names, "high") ->
      high_reg
    # Get the region list of low income countries
    get_ssp_regions(L102.pcgdp_thous90USD_Scen_R_Y, GCAM_region_names, "low") ->
      low_reg

    # Assign the reference agricultural productivity change to median income countries,
    # high change to high income regions, and low change to low income regions
    L2053.AgProdChange_ag_irr_ref %>%
      filter(!region %in% c(high_reg, low_reg)) %>%
      bind_rows(filter(L2053.AgProdChange_irr_high, region %in% high_reg),
                filter(L2053.AgProdChange_irr_low, region %in% low_reg)) ->
      L2053.AgProdChange_irr_ssp4



    # Derive biophysical land productivity (for later use in labor productivity derivation) ----

    # Bind all Ag prod change (annual yield growth rate)
    bind_rows(
      L2053.AgProdChange_ag_irr_ref %>% mutate(scenario = "ref"),
      L2053.AgProdChange_irr_high %>% mutate(scenario = "high"),
      L2053.AgProdChange_irr_low %>% mutate(scenario = "low"),
      L2053.AgProdChange_irr_ssp4 %>% mutate(scenario = "regional")) ->
      L2053.AgProdChange_ag_irr_scen

    # Convert back to ratio
    bind_rows(
      # bind final base year
      L2053.AgProdChange_ag_irr_scen %>%
        filter(year == first(MODEL_FUTURE_YEARS)) %>%
        mutate(year = MODEL_FINAL_BASE_YEAR, AgProdChange = 0),
      L2053.AgProdChange_ag_irr_scen) %>%
      # calculate Yield ratio per year
      group_by_at(vars(-year, -AgProdChange)) %>%
      mutate(timestep = if_else(year == MODEL_FINAL_BASE_YEAR, 0, year - lag(year)),
             YieldRatio = (1 + AgProdChange)^timestep,
             YieldRatio = cumprod(YieldRatio)) %>%
      ungroup() %>%
      select(-AgProdChange, -timestep) ->
      L2053.Ag_YieldRatio_cumulative_ag_irr_mgmt_scen

    ## Get initial land weights ----
    L181.LC_bm2_R_C_Yh_GLU_irr_level %>%
      rename(area = value) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(
        L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level %>% rename(yield = value),
        by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU", "Irr_Rfd", "year", "level") ) %>%
      rename(MGMT = level) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      mutate(AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_subsector, GLU_name, sep = aglu.CROP_GLU_DELIMITER),
             AgProductionTechnology = paste(paste(AgSupplySubsector, toupper(Irr_Rfd), sep = aglu.IRR_DELIMITER),
                                            MGMT, sep = aglu.MGMT_DELIMITER)) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology,
             LandWeight = area, IntialYield = yield) ->
      L2053.Ag_LandWeight_Yield_FBY_ag_irr_mgmt

    L2053.Ag_YieldRatio_cumulative_ag_irr_mgmt_scen %>%
      left_join_error_no_match(
        L2053.Ag_LandWeight_Yield_FBY_ag_irr_mgmt,
        by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology")) %>%
      group_by_at(vars(scenario, region, GCAM_commodity = AgSupplySector, year)) %>%
      summarize(Yield = weighted.mean(YieldRatio * IntialYield,
                                      w = LandWeight, na.rm = T),
                LandWeight = sum(LandWeight, na.rm = T), .groups = "drop") %>%
      group_by_at(vars(-year, -Yield, -LandWeight)) %>%
      mutate(YieldRatio = Yield/first(Yield)) %>%
      ungroup() ->
      L2053.AgLand_BiophysicalProducitivityRatio_ag_R_C_Y_scen

    # Produce outputs ----
    L2053.AgLand_BiophysicalProducitivityRatio_ag_R_C_Y_scen %>%
      add_title("Agricultural land productivity ratio relative to final base year aggregrated using initial land weights by scenario") %>%
      add_units("Unitless") %>%
      add_comments("The land productivity will be used to imply labor productivity") %>%
      add_legacy_name("L2053.AgLand_BiophysicalProducitivityRatio_ag_R_C_Y_scen") %>%
      add_precursors("L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level",
                     "L181.LC_bm2_R_C_Yh_GLU_irr_level",
                     "L2053.AgProdChange_ag_irr_ref",
                     "L2053.AgProdChange_irr_high",
                     "L2053.AgProdChange_irr_low",
                     "L2053.AgProdChange_irr_ssp4") ->
      L2053.AgLand_BiophysicalProducitivityRatio_ag_R_C_Y_scen

    L2053.AgProdChange_ag_irr_ref %>%
      add_title("Reference agricultural productivity change of crops by region / crop / GLU / technology") %>%
      add_units("Unitless") %>%
      add_comments("The same productivity change are assigned to both high and low management") %>%
      add_legacy_name("L2053.AgProdChange_ag_irr_ref") %>%
      add_precursors("water/basin_to_country_mapping",
                     "common/GCAM_region_names",
                     "L162.ag_YieldRate_R_C_Y_GLU_irr") ->
      L2053.AgProdChange_ag_irr_ref

    L2053.AgProdChange_bio_irr_ref %>%
      add_title("Reference agricultural productivity change of biomass crops by region / crop / GLU / technology") %>%
      add_units("Unitless") %>%
      add_comments("The same productivity change are assigned to both high and low management") %>%
      add_legacy_name("L2053.AgProdChange_bio_irr_ref") %>%
      add_precursors("common/GCAM_region_names",
                     "L162.bio_YieldRate_R_Y_GLU_irr",
                     "L201.AgYield_bio_grass",
                     "L201.AgYield_bio_tree") ->
      L2053.AgProdChange_bio_irr_ref

    L2053.AgProdChange_irr_high %>%
      add_title("High agricultural productivity change of crops by region / crop / GLU / technology") %>%
      add_units("Unitless") %>%
      add_comments("Multiply reference productivity change with the high growth rate multiplier") %>%
      add_comments("The same productivity change are assigned to both high and low management") %>%
      add_legacy_name("L2053.AgProdChange_irr_high") %>%
      same_precursors_as("L2053.AgProdChange_ag_irr_ref") %>%
      add_precursors("L102.pcgdp_thous90USD_Scen_R_Y") ->
      L2053.AgProdChange_irr_high

    L2053.AgProdChange_irr_low %>%
      add_title("Low agricultural productivity change of crops by region / crop / GLU / technology") %>%
      add_units("Unitless") %>%
      add_comments("Multiply reference productivity change with the low growth rate multiplier") %>%
      add_comments("The same productivity change are assigned to both high and low management") %>%
      add_legacy_name("L2053.AgProdChange_irr_low") %>%
      same_precursors_as("L2053.AgProdChange_ag_irr_ref") %>%
      add_precursors("L102.pcgdp_thous90USD_Scen_R_Y") ->
      L2053.AgProdChange_irr_low

    L2053.AgProdChange_irr_ssp4 %>%
      add_title("SSP4 agricultural productivity change of crops by region / crop / GLU / technology") %>%
      add_units("Unitless") %>%
      add_comments("Assign reference productivity change to median income regions") %>%
      add_comments("Assign high productivity change to high income regions") %>%
      add_comments("Assign low productivity change to low income regions") %>%
      add_comments("Region groups by income level are based on the 2010 GDP per capita") %>%
      add_legacy_name("L2053.AgProdChange_irr_ssp4") %>%
      same_precursors_as("L2053.AgProdChange_ag_irr_ref") %>%
      add_precursors("L102.pcgdp_thous90USD_Scen_R_Y") ->
      L2053.AgProdChange_irr_ssp4

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
