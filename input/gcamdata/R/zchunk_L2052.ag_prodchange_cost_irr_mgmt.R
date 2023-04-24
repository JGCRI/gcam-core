# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L2052.ag_prodchange_cost_irr_mgmt
#'
#' Specify production costs and future agricultural productivity changes for all technologies.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2052.AgCost_ag_irr_mgmt}, \code{L2052.AgCost_bio_irr_mgmt}, \code{L2052.AgCost_For}, \code{L2052.AgProdChange_ag_irr_ref}, \code{L2052.AgProdChange_bio_irr_ref}, \code{L2052.AgProdChange_irr_high}, \code{L2052.AgProdChange_irr_low}, \code{L2052.AgProdChange_irr_ssp4}. The corresponding file in the
#' original data system was \code{L2052.ag_prodchange_cost_irr_mgmt.R} (aglu level2).
#' @details This chunk maps the production costs of crops, biomass and forest to all four technologies (irrigated / rainfed; high / low),
#' and calculates future productivity change of crops and biomass for all technologies along reference, high, low and SSP4 scenarios.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter mutate one_of pull select
#' @importFrom tidyr replace_na separate
#' @author RC July 2017
module_aglu_L2052.ag_prodchange_cost_irr_mgmt <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "water/basin_to_country_mapping",
      "L123.For_Yield_m3m2_R_GLU",
      "L161.ag_irrProd_Mt_R_C_Y_GLU",
      "L161.ag_rfdProd_Mt_R_C_Y_GLU",
      "L162.ag_YieldRate_R_C_Y_GLU_irr",
      "L162.bio_YieldRate_R_Y_GLU_irr",
      "L164.ag_Cost_75USDkg_C",
      "L1321.ag_prP_R_C_75USDkg",
      "L2012.AgSupplySector",
      "L201.AgYield_bio_grass",
      "L201.AgYield_bio_tree",
      "L102.pcgdp_thous90USD_Scen_R_Y",
      "L1321.expP_R_F_75USDm3")

  MODULE_OUTPUTS <-
    c("L2052.AgCost_ag_irr_mgmt",
      "L2052.AgCalMinProfitRate",
      "L2052.AgCost_bio_irr_mgmt",
      "L2052.AgCost_For",
      "L2052.AgProdChange_ag_irr_ref",
      "L2052.AgProdChange_bio_irr_ref",
      "L2052.AgProdChange_irr_high",
      "L2052.AgProdChange_irr_low",
      "L2052.AgProdChange_irr_ssp4")

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

    # Define column names
    names_AgTech <- LEVEL2_DATA_NAMES[["AgTech"]]
    names_AgCost <- LEVEL2_DATA_NAMES[["AgCost"]]
    names_AgProdChange <- LEVEL2_DATA_NAMES[["AgProdChange"]]

    # Production costs ----
    # Assign nonLandVariableCost of crop production, assuming the same level to all four technologies
    # Start with the L161 production tables to specify which region / GLU / crop will need costs assigned
    L161.ag_irrProd_Mt_R_C_Y_GLU %>%
      mutate(IRR_RFD = "IRR") %>%
      bind_rows(mutate(L161.ag_rfdProd_Mt_R_C_Y_GLU, IRR_RFD = "RFD")) %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, IRR_RFD) %>%
      unique() %>%
      # Map in costs data, same level for irrigated and rainfed
      left_join_error_no_match(L164.ag_Cost_75USDkg_C, by = "GCAM_commodity") %>%
      mutate(nonLandVariableCost = round(Cost_75USDkg, aglu.DIGITS_CALPRICE)) %>%
      # Copy costs to high and low management levels
      repeat_add_columns(tibble(MGMT = c("hi", "lo"))) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      # Add sector, subsector, technology names
      mutate(AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_subsector, GLU_name, sep = aglu.CROP_GLU_DELIMITER),
             AgProductionTechnology = paste(paste(AgSupplySubsector, IRR_RFD, sep = aglu.IRR_DELIMITER),
                                            MGMT, sep = aglu.MGMT_DELIMITER)) %>%
      # Copy costs to all model years
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(names_AgCost) ->
      L2052.AgCost_ag_irr_mgmt

    # 2/14/2019 ag trade modification (GPK): These costs need to be modified in order to accommodate any crops with
    # regional markets, whose prices differ from the default (USA-based) assumptions for global markets. Failure to
    # take this into account will result in inconsistency between cost assumptions (which are based on the USA) and
    # crop prices, which may return distorted and potentially negative profit rates.

    # 9/23/2019 modification - the producer price data in L132 comes from a direct query of the "United States of
    # America" country in FAOSTAT, whereas the L1321 and L2012 data come from a global query with regions mapped
    # according to AGLU_ctry. For this reason, the latter datasets include Puerto Rico within the USA region. This
    # causes a mismatch in computing costs and prices, and can lead to negative profit rates for crops that (a) tend to
    # be grown in Puerto Rico, and (b) have lower producer prices in Puerto Rico than in the USA.

    # Specifically, the method applies the cost:price ratio of each crop in the USA to each crop in all regions
    # L132.ag_an_For_Prices %>% filter(GCAM_commodity %in% L164.ag_Cost_75USDkg_C$GCAM_commodity) %>% mutate(region = gcam.USA_REGION)
    # L132.ag_an_For_Prices is replaced with L1321.ag_prP_R_C_75USDkg for consistency
    L1321.ag_prP_R_C_75USDkg %>%
      filter(region == gcam.USA_REGION) %>%
      filter(GCAM_commodity %in% L164.ag_Cost_75USDkg_C$GCAM_commodity) %>%
      select(region, GCAM_commodity, calPrice = value) %>%
      left_join_error_no_match(L164.ag_Cost_75USDkg_C, by = "GCAM_commodity") %>%
      mutate(cost_PrP_ratio = Cost_75USDkg / calPrice) %>%
      select(AgSupplySector = GCAM_commodity, cost_PrP_ratio) ->
        L2052.AgCostRatio_USA

    L2052.AgCost_ag_irr_mgmt <- left_join_error_no_match(L2052.AgCost_ag_irr_mgmt, L2052.AgCostRatio_USA,
                                     by = "AgSupplySector") %>%
      left_join_error_no_match(select(L2012.AgSupplySector, region, AgSupplySector, calPrice),
                               by = c("region", "AgSupplySector")) %>%
      mutate(nonLandVariableCost = round(calPrice * cost_PrP_ratio, aglu.DIGITS_CALPRICE)) %>%
      select(LEVEL2_DATA_NAMES[["AgCost"]])

    # Assign nonLandVariableCost of bioenergy production, assuming the same level to all four technologies
    # Start with the yield table to determine where bioenergy crops are being read in, get both grass and tree crops
    L201.AgYield_bio_grass %>%
      select(names_AgTech) %>%
      unique() %>%
      bind_rows(unique(select(L201.AgYield_bio_tree, one_of(names_AgTech)))) %>%
      mutate(nonLandVariableCost = aglu.BIO_GRASS_COST_75USD_GJ,
             nonLandVariableCost = replace(nonLandVariableCost, grepl("Tree", AgProductionTechnology),
                                           aglu.BIO_TREE_COST_75USD_GJ)) %>%
      # Copy coefficients to all four technologies
      repeat_add_columns(tibble(IRR_RFD = c("IRR", "RFD"))) %>%
      repeat_add_columns(tibble(MGMT = c("hi", "lo"))) %>%
      # Revise technology names, adding info of irr/rfd and hi/lo
      mutate(AgProductionTechnology = paste(AgProductionTechnology, IRR_RFD, MGMT, sep = "_")) %>%
      # Copy costs to all model years
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(names_AgCost) ->
      L2052.AgCost_bio_irr_mgmt

    # Assign nonLandVariableCost of forest production
    # Start with the yield table to determine where forest are being read in
    # Differentiate regional cost for forest using aglu.FOR_COST_SHARE in constants.R
    L123.For_Yield_m3m2_R_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GLU) %>%
      unique() %>%
      # Copy costs to all model years
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      # Add sector, subsector, technology names
      mutate(AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_commodity, GLU_name, sep = aglu.CROP_GLU_DELIMITER),
             AgProductionTechnology = AgSupplySubsector) %>%
      left_join(L1321.expP_R_F_75USDm3, by = c("GCAM_region_ID", "GCAM_commodity", "region")) %>%
                  mutate(nonLandVariableCost = value * aglu.FOR_COST_SHARE) %>%
      select(names_AgCost) ->
      L2052.AgCost_For


    # Generate min profit for ag sector ----
    # This will be read in as a threshold to calculate implicit land subsidy in GCAM
    # mainly to avoid negative profits & also to handel inconsistency nonland costs calculations
    L2052.AgCost_ag_irr_mgmt %>%
      left_join_error_no_match(select(L2012.AgSupplySector, region, AgSupplySector, calPrice),
                               by = c("region", "AgSupplySector")) %>%
      mutate(Profit = calPrice - nonLandVariableCost) ->
      L2052.UnAdjProfits

    # The min profit is uniform across regions & crops (not including bio & forest)
    # Check min profit by ag sector here. This could be considered later when needed
    # L2052.UnAdjProfits %>%
    #   group_by(AgSupplySector) %>%
    #   summarize(cal.min.profit.rate = min(Profit)) %>%
    #   ungroup() %>%
    #   left_join_error_no_match(L2052.UnAdjProfits %>% select(region, AgSupplySector) %>% distinct(),
    #                            ., by=c("AgSupplySector")) %>%
    #   select(LEVEL2_DATA_NAMES[['AgCalMinProfitRate']]) ->
    #   L2052.AgCalMinProfitRate

    L2052.UnAdjProfits %>%
      select(region, AgSupplySector) %>%
      distinct() %>%
      mutate(cal.min.profit.rate = min(L2052.UnAdjProfits$Profit)) ->
      L2052.AgCalMinProfitRate


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
      select(names_AgProdChange) ->
      L2052.AgProdChange_ag_irr_ref

    # Specify reference scenario agricultural productivity change for biomass
    L162.bio_YieldRate_R_Y_GLU_irr %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      mutate(AgProdChange = round(value, digits = aglu.DIGITS_AGPRODCHANGE)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) ->
      L2051.AgProdChange_bio_irr_ref

    # Use the yield table to determine where bioenergy crops are being read in, get both grass and tree crops
    L201.AgYield_bio_grass %>%
      select(names_AgTech) %>%
      unique() %>%
      bind_rows(unique(select(L201.AgYield_bio_tree, one_of(names_AgTech)))) %>%
      # Copy to all future years
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      # Copy to both irrigated and rainfed technologies
      repeat_add_columns(tibble(IRR_RFD = c("IRR", "RFD"))) %>%
      # Separate the AgProductionTechnology variable to get GLU names for matching in the yield change rates
      separate(AgProductionTechnology, c("biomass", "GLU_name"), sep = aglu.CROP_GLU_DELIMITER) %>%
      # Map in yield change rates, the same values for bioenergy crops are applied equally to grass and tree crops.
      left_join(L2051.AgProdChange_bio_irr_ref[c("region", "GLU_name", "Irr_Rfd", "year", "AgProdChange")],
                by = c("region", "GLU_name", "IRR_RFD" = "Irr_Rfd", "year")) %>%
      # Note: Grass crops are available in any land use regions with crop production, and tree crops are available in any region with forests.
      # Because the yield growth rates are based on crops, some places that have forests but no cropland will not have yield improvement rates.
      # These regions are assumed minor agriculturally and as such not assigned yield improvement for tree-based bioenergy crops.
      replace_na(list(AgProdChange = 0)) %>%
      # Copy coefficients to high and low management levels
      repeat_add_columns(tibble(MGMT = c("hi", "lo"))) %>%
      # Revise technology names to add all technologies
      mutate(AgProductionTechnology = paste(paste(AgSupplySubsector, IRR_RFD, sep = aglu.CROP_GLU_DELIMITER),
                                            MGMT, sep = aglu.MGMT_DELIMITER)) %>%
      select(names_AgProdChange) ->
      L2052.AgProdChange_bio_irr_ref

    # Specify the scenario with high agricultural productivity change (not incl biomass)
    L2052.AgProdChange_ag_irr_ref %>%
      # Use the high growth multiplier
      mutate(AgProdChange = AgProdChange * aglu.HI_PROD_GROWTH_MULT) ->
      L2052.AgProdChange_irr_high

    # Specify the scenario with low agricultural productivity change (not incl biomass)
    L2052.AgProdChange_ag_irr_ref %>%
      # Use the low growth multiplier
      mutate(AgProdChange = AgProdChange * aglu.LOW_PROD_GROWTH_MULT) ->
      L2052.AgProdChange_irr_low

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
    L2052.AgProdChange_ag_irr_ref %>%
      filter(!region %in% c(high_reg, low_reg)) %>%
      bind_rows(filter(L2052.AgProdChange_irr_high, region %in% high_reg),
                filter(L2052.AgProdChange_irr_low, region %in% low_reg)) ->
      L2052.AgProdChange_irr_ssp4

    # Produce outputs ----
    L2052.AgCost_ag_irr_mgmt %>%
      add_title("Non-land variable costs of crops prodction by region / crop / GLU / technology") %>%
      add_units("1975$ per kg") %>%
      add_comments("The same costs are assigned to all four technologies") %>%
      add_legacy_name("L2052.AgCost_ag_irr_mgmt") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "L161.ag_rfdProd_Mt_R_C_Y_GLU",
                     "L164.ag_Cost_75USDkg_C",
                     "L1321.ag_prP_R_C_75USDkg",
                     "L2012.AgSupplySector") ->
      L2052.AgCost_ag_irr_mgmt

    L2052.AgCalMinProfitRate %>%
      add_title("Set minimum calibration profit rate") %>%
      add_units("1975$ per kg") %>%
      add_comments("Enables the calculation of an implicit subsidy to ensure that calibration profit rates") %>%
      add_comments("do not fall below the threshold value provided in this output regardless of dynamic costs") %>%
      add_comments("calculated such as fertilizer or water costs.") %>%
      add_precursors("L2012.AgSupplySector", "L2052.AgCost_ag_irr_mgmt") ->
      L2052.AgCalMinProfitRate

    L2052.AgCost_bio_irr_mgmt %>%
      add_title("Non-land variable costs of biomass crops production by region / crop / GLU / technology") %>%
      add_units("1975$ per kg") %>%
      add_comments("The same costs are assigned to all four technologies") %>%
      add_legacy_name("L2052.AgCost_bio_irr_mgmt") %>%
      add_precursors("L201.AgYield_bio_grass",
                     "L201.AgYield_bio_tree") ->
      L2052.AgCost_bio_irr_mgmt

    L2052.AgCost_For %>%
      add_title("Non-land variable costs of forest prodction by region / GLU") %>%
      add_units("1975$ per kg") %>%
      add_comments("Technologies are not specified for forest") %>%
      add_legacy_name("L2052.AgCost_For") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L123.For_Yield_m3m2_R_GLU",
                     "L1321.expP_R_F_75USDm3") ->
      L2052.AgCost_For

    L2052.AgProdChange_ag_irr_ref %>%
      add_title("Reference agricultural productivity change of crops by region / crop / GLU / technology") %>%
      add_units("Unitless") %>%
      add_comments("The same productivity change are assigned to both high and low management") %>%
      add_legacy_name("L2052.AgProdChange_ag_irr_ref") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L162.ag_YieldRate_R_C_Y_GLU_irr") ->
      L2052.AgProdChange_ag_irr_ref

    L2052.AgProdChange_bio_irr_ref %>%
      add_title("Reference agricultural productivity change of biomass crops by region / crop / GLU / technology") %>%
      add_units("Unitless") %>%
      add_comments("The same productivity change are assigned to both high and low management") %>%
      add_legacy_name("L2052.AgProdChange_bio_irr_ref") %>%
      add_precursors("L162.bio_YieldRate_R_Y_GLU_irr",
                     "L201.AgYield_bio_grass",
                     "L201.AgYield_bio_tree") ->
      L2052.AgProdChange_bio_irr_ref

    L2052.AgProdChange_irr_high %>%
      add_title("High agricultural productivity change of crops by region / crop / GLU / technology") %>%
      add_units("Unitless") %>%
      add_comments("Multiply reference productivity change with the high growth rate multiplier") %>%
      add_comments("The same productivity change are assigned to both high and low management") %>%
      add_legacy_name("L2052.AgProdChange_irr_high") %>%
      same_precursors_as("L2052.AgProdChange_ag_irr_ref") %>%
      add_precursors("L102.pcgdp_thous90USD_Scen_R_Y") ->
      L2052.AgProdChange_irr_high

    L2052.AgProdChange_irr_low %>%
      add_title("Low agricultural productivity change of crops by region / crop / GLU / technology") %>%
      add_units("Unitless") %>%
      add_comments("Multiply reference productivity change with the low growth rate multiplier") %>%
      add_comments("The same productivity change are assigned to both high and low management") %>%
      add_legacy_name("L2052.AgProdChange_irr_low") %>%
      same_precursors_as("L2052.AgProdChange_ag_irr_ref") %>%
      add_precursors("L102.pcgdp_thous90USD_Scen_R_Y") ->
      L2052.AgProdChange_irr_low

    L2052.AgProdChange_irr_ssp4 %>%
      add_title("SSP4 agricultural productivity change of crops by region / crop / GLU / technology") %>%
      add_units("Unitless") %>%
      add_comments("Assign reference productivity change to median income regions") %>%
      add_comments("Assign high productivity change to high income regions") %>%
      add_comments("Assign low productivity change to low income regions") %>%
      add_comments("Region groups by income level are based on the 2010 GDP per capita") %>%
      add_legacy_name("L2052.AgProdChange_irr_ssp4") %>%
      same_precursors_as("L2052.AgProdChange_ag_irr_ref") %>%
      add_precursors("L102.pcgdp_thous90USD_Scen_R_Y") ->
      L2052.AgProdChange_irr_ssp4

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
