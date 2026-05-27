# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L2052.ag_nonlandcost_irr_mgmt
#'
#' Specify production costs and future agricultural productivity changes for all technologies.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2052.AgCost_ag_irr_mgmt}, \code{L2052.AgCost_bio_irr_mgmt}, \code{L2052.AgCost_For},
#' \code{L2052.AgCalMinProfitRate}
#' @details This chunk maps the production costs of crops, biomass and forest to all four technologies (irrigated / rainfed; high / low),
#' and calculates future productivity change of crops and biomass for all technologies along reference, high, low and SSP4 scenarios.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter mutate one_of pull select
#' @importFrom tidyr replace_na separate
#' @author RC July 2017 XZ 2023 2025
module_aglu_L2052.ag_nonlandcost_irr_mgmt <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "water/basin_to_country_mapping",
      "L123.For_Yield_m3m2_R_GLU",
      "L161.ag_irrProd_Mt_R_C_Y_GLU",
      "L161.ag_rfdProd_Mt_R_C_Y_GLU",
      "L201.AgYield_bio_grass",
      "L201.AgYield_bio_tree",
      "L164.ag_Cost_75USDkg_C",
      "L1321.ag_prP_R_C_75USDkg",
      "L2012.AgSupplySector",
      "L1321.expP_R_F_75USDm3",
      "L100.GTAPCostShare_AgLU_reg_comm")

  MODULE_OUTPUTS <-
    c("L2052.AgCost_ag_irr_mgmt",
      "L2052.AgCalMinProfitRate",
      "L2052.AgCost_bio_irr_mgmt",
      "L2052.AgCost_For")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    names_AgTech <- names_AgCost <- GLU <- GLU_name <- IRR_RFD <-
      MGMT <- AgSupplySector <- AgSupplySubsector <- AgProductionTechnology <-
      AgProdChange <- nonLandVariableCost <- high_reg <- low_reg <- region <-
      GCAM_region_ID <- year <- value <- GCAM_commodity <- Cost_75USDkg <-
      Irr_Rfd <- scenario <- calPrice <- cost_PrP_ratio <- . <- GCAM_subsector <- NULL  # silence package check notes

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Get GTAP cost shares ready ----
    L100.GTAPCostShare_AgLU_reg_comm %>%
      # complete year (historical) and simply fill missing
      complete(nesting(GCAM_region_ID, GCAM_commodity, input), year = HISTORICAL_YEARS) %>%
      group_by(GCAM_region_ID, GCAM_commodity, input) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_BASE_YEARS) ->
      L2052.AgCostShare_reg

    assertthat::assert_that(
      L164.ag_Cost_75USDkg_C %>% distinct(GCAM_commodity) %>% pull %in%
        (L2052.AgCostShare_reg %>% distinct(GCAM_commodity) %>% pull) %>%
        all)

    L2052.AgCostShare_reg %>%
      right_join(L164.ag_Cost_75USDkg_C %>% distinct(GCAM_commodity), by = "GCAM_commodity") %>%
      # do not remove self use (AgLU) and water cost
      # filter(!input %in% c("Water", "AgLU")) %>%
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%
      mutate(value = value / sum(value)) %>% ungroup() %>%
      filter(input == "Land") %>%
      mutate(value = 1 - value) %>%
      select(-input) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L2052.AgCostShare_reg_nonLand

    L2052.AgCostShare_reg_nonLand %>%
      bind_rows(
        L2052.AgCostShare_reg_nonLand %>%
          filter(year == dplyr::last(MODEL_BASE_YEARS)) %>%
          select(-year) %>%
          repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS))
      ) ->
      L2052.AgCostShare_reg_nonLand_yr

    # Define column names
    names_AgTech <- LEVEL2_DATA_NAMES[["AgTech"]]
    names_AgCost <- LEVEL2_DATA_NAMES[["AgCost"]]

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
      mutate(cost_PrP_ratio_USDA = Cost_75USDkg / calPrice) %>%
      select(AgSupplySector = GCAM_commodity, cost_PrP_ratio_USDA) ->
      L2052.AgCostRatio_USA

    L2052.AgCost_ag_irr_mgmt <- left_join_error_no_match(L2052.AgCost_ag_irr_mgmt, L2052.AgCostRatio_USA,
                                     by = "AgSupplySector") %>%
      left_join_error_no_match(select(L2012.AgSupplySector, region, AgSupplySector, calPrice),
                               by = c("region", "AgSupplySector")) %>%
      left_join_error_no_match(L2052.AgCostShare_reg_nonLand_yr %>%
                                 select(region, AgSupplySector = GCAM_commodity, cost_PrP_ratio_GTAP = value, year),
                               by = c("region", "AgSupplySector", "year")) %>%
      # Note that GTAP approach/data is used. If using USDA: nonLandVariableCost = calPrice * cost_PrP_ratio_USDA
      mutate(nonLandVariableCost = calPrice * cost_PrP_ratio_GTAP,
             nonLandVariableCost = round(nonLandVariableCost, aglu.DIGITS_CALPRICE)) %>%
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

    assertthat::assert_that(
      L1321.expP_R_F_75USDm3 %>% distinct(GCAM_commodity) %>% pull %in%
        (L2052.AgCostShare_reg %>% distinct(GCAM_commodity) %>% pull) %>%
        all)

    L2052.AgCostShare_reg %>%
      filter(GCAM_commodity %in% unique(L1321.expP_R_F_75USDm3$GCAM_commodity)) %>%
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%
      mutate(value = value / sum(value)) %>% ungroup() %>%
      filter(input == "Resource")  %>%
      mutate(value = 1 - value) %>%
      select(-input) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L2052.For_CostShare_reg_nonLand

    L2052.For_CostShare_reg_nonLand %>%
      bind_rows(
        L2052.For_CostShare_reg_nonLand %>%
          filter(year == dplyr::last(MODEL_BASE_YEARS)) %>%
          select(-year) %>%
          repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS))
      ) %>%
      rename(NonLandShare = value) ->
      L2052.For_CostShare_reg_nonLand_Y


    # Start with the yield table to determine where forest are being read in
    # Differentiate regional cost for forest using aglu.FOR_COST_SHARE in constants.R
    L123.For_Yield_m3m2_R_GLU %>%
      select(GCAM_region_ID, Land_Type, GLU) %>%
      unique() %>%
      # Copy costs to all model years
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(tibble(GCAM_commodity = aglu.FOREST_SUPPLY_SECTOR)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      # Add sector, subsector, technology names
      mutate(AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(Land_Type, GLU_name, sep = aglu.CROP_GLU_DELIMITER),
             AgProductionTechnology = AgSupplySubsector) %>%
      left_join(L1321.expP_R_F_75USDm3,
                by = c("GCAM_region_ID", "GCAM_commodity", "region")) %>%
      left_join_error_no_match(L2052.For_CostShare_reg_nonLand_Y,
                               by = c("GCAM_region_ID", "region", "year", "GCAM_commodity") ) %>%
      mutate(nonLandVariableCost = NonLandShare * value) %>%
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
      mutate(cal.min.profit.rate = min(L2052.UnAdjProfits[year = max(MODEL_BASE_YEARS)]$Profit)) ->
      L2052.AgCalMinProfitRate


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
                     "L2012.AgSupplySector",
                     "L100.GTAPCostShare_AgLU_reg_comm") ->
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

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
