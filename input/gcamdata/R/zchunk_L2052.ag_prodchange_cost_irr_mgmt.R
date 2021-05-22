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
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             "L123.For_Yield_m3m2_R_GLU",
             "L161.ag_irrProd_Mt_R_C_Y_GLU",
             "L161.ag_rfdProd_Mt_R_C_Y_GLU",
             "L162.ag_YieldRate_R_C_Y_GLU_irr",
             "L162.bio_YieldRate_R_Y_GLU_irr",
             "L164.ag_Cost_75USDkg_C",
             "L132.ag_an_For_Prices",
             "L2012.AgSupplySector",
             "L201.AgYield_bio_grass",
             "L201.AgYield_bio_tree",
             "L102.pcgdp_thous90USD_Scen_R_Y",
             "L1321.expP_R_F_75USDm3"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2052.AgCost_ag_irr_mgmt",
             "L2052.AgCost_bio_irr_mgmt",
             "L2052.AgCost_For",
             "L2052.AgProdChange_ag_irr_ref",
             "L2052.AgProdChange_bio_irr_ref",
             "L2052.AgProdChange_irr_high",
             "L2052.AgProdChange_irr_low",
             "L2052.AgProdChange_irr_ssp4"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    names_AgTech <- names_AgCost <- names_AgProdChange <- GLU <- GLU_name <- IRR_RFD <-
      MGMT <- AgSupplySector <- AgSupplySubsector <- AgProductionTechnology <-
      AgProdChange <- nonLandVariableCost <- high_reg <- low_reg <- region <-
      GCAM_region_ID <- year <- value <- GCAM_commodity <- Cost_75USDkg <-
      Irr_Rfd <- scenario <- calPrice <- cost_PrP_ratio <- . <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping", strip_attributes = TRUE)
    L123.For_Yield_m3m2_R_GLU <- get_data(all_data, "L123.For_Yield_m3m2_R_GLU", strip_attributes = TRUE)
    L161.ag_irrProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_irrProd_Mt_R_C_Y_GLU", strip_attributes = TRUE)
    L161.ag_rfdProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_rfdProd_Mt_R_C_Y_GLU", strip_attributes = TRUE)
    L162.ag_YieldRate_R_C_Y_GLU_irr <- get_data(all_data, "L162.ag_YieldRate_R_C_Y_GLU_irr", strip_attributes = TRUE)
    L162.bio_YieldRate_R_Y_GLU_irr <- get_data(all_data, "L162.bio_YieldRate_R_Y_GLU_irr", strip_attributes = TRUE)
    L164.ag_Cost_75USDkg_C <- get_data(all_data, "L164.ag_Cost_75USDkg_C", strip_attributes = TRUE)
    L132.ag_an_For_Prices <- get_data(all_data, "L132.ag_an_For_Prices", strip_attributes = TRUE)
    L2012.AgSupplySector <- get_data(all_data, "L2012.AgSupplySector", strip_attributes = TRUE)
    L201.AgYield_bio_grass <- get_data(all_data, "L201.AgYield_bio_grass", strip_attributes = TRUE)
    L201.AgYield_bio_tree <- get_data(all_data, "L201.AgYield_bio_tree", strip_attributes = TRUE)
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y", strip_attributes = TRUE)
    L1321.expP_R_F_75USDm3 <- get_data(all_data, "L1321.expP_R_F_75USDm3", strip_attributes = TRUE)

    # Define column names
    names_AgTech <- LEVEL2_DATA_NAMES[["AgTech"]]
    names_AgCost <- LEVEL2_DATA_NAMES[["AgCost"]]
    names_AgProdChange <- LEVEL2_DATA_NAMES[["AgProdChange"]]

    # Production costs
    # Assign nonLandVariableCost of crop production, assuming the same level to all four technologies
    # Start with the L161 production tables to specify which region / GLU / crop will need costs assigned
    L161.ag_irrProd_Mt_R_C_Y_GLU %>%
      mutate(IRR_RFD = "IRR") %>%
      bind_rows(mutate(L161.ag_rfdProd_Mt_R_C_Y_GLU, IRR_RFD = "RFD")) %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, IRR_RFD) %>%
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
             AgSupplySubsector = paste(GCAM_commodity, GLU_name, sep = "_"),
             AgProductionTechnology = paste(GCAM_commodity, GLU_name, IRR_RFD, MGMT, sep = "_")) %>%
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
    L132.ag_an_For_Prices %>% filter(GCAM_commodity %in% L164.ag_Cost_75USDkg_C$GCAM_commodity) %>%
      mutate(region = gcam.USA_REGION) %>%
      select(region, GCAM_commodity, calPrice) %>%
      left_join_error_no_match(L164.ag_Cost_75USDkg_C, by = "GCAM_commodity") %>%
      mutate(cost_PrP_ratio = Cost_75USDkg / calPrice) %>%
      select(AgSupplySector = GCAM_commodity, cost_PrP_ratio)->L2052.AgCostRatio_USA

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
             AgSupplySubsector = paste(GCAM_commodity, GLU_name, sep = "_"),
             AgProductionTechnology = paste(GCAM_commodity, GLU_name, sep = "_")) %>%
      left_join(L132.ag_an_For_Prices, by = "GCAM_commodity") %>%
      left_join(L1321.expP_R_F_75USDm3, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
                  mutate(nonLandVariableCost = if_else(is.na(value),
                                                       calPrice * aglu.FOR_COST_SHARE,
                                                       value * aglu.FOR_COST_SHARE) ) %>%
      select(names_AgCost) ->
      L2052.AgCost_For
    # Future agricultural productivity changes
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
             AgSupplySubsector = paste(GCAM_commodity, GLU_name, sep = "_"),
             AgProductionTechnology = paste(GCAM_commodity, GLU_name, Irr_Rfd, MGMT, sep = "_")) %>%
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
      separate(AgProductionTechnology, c("biomass", "GLU_name"), sep = "_") %>%
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
      mutate(AgProductionTechnology = paste(AgSupplySubsector, IRR_RFD, MGMT, sep = "_")) %>%
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
    L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(scenario == "SSP4" & year == 2010) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # Calculate GDP per capita in 2010 USD
      mutate(value = value / gdp_deflator(1990, 2010)) %>%
      select(region, value) ->
      L225.pcgdp_2010

    # Get the region list of high income countries
    L225.pcgdp_2010 %>%
      filter(value > aglu.HIGH_GROWTH_PCGDP) %>%
      select(region) %>%
      # Convert tibble to vector
      pull(region) ->
      high_reg
    # Get the region list of low income countries
    L225.pcgdp_2010 %>%
      filter(value < aglu.LOW_GROWTH_PCGDP) %>%
      select(region) %>%
      # Convert tibble to vector
      pull(region) ->
      low_reg

    # Assign the reference agricultural productivity change to median income countries,
    # high change to high income regions, and low change to low income regions
    L2052.AgProdChange_ag_irr_ref %>%
      filter(!region %in% c(high_reg, low_reg)) %>%
      bind_rows(filter(L2052.AgProdChange_irr_high, region %in% high_reg),
                filter(L2052.AgProdChange_irr_low, region %in% low_reg)) ->
      L2052.AgProdChange_irr_ssp4

    # Produce outputs
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
                     "L132.ag_an_For_Prices",
                     "L2012.AgSupplySector") ->
      L2052.AgCost_ag_irr_mgmt

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

    return_data(L2052.AgCost_ag_irr_mgmt, L2052.AgCost_bio_irr_mgmt,
                L2052.AgCost_For,
                L2052.AgProdChange_ag_irr_ref, L2052.AgProdChange_bio_irr_ref,
                L2052.AgProdChange_irr_high, L2052.AgProdChange_irr_low,
                L2052.AgProdChange_irr_ssp4)
  } else {
    stop("Unknown command")
  }
}
