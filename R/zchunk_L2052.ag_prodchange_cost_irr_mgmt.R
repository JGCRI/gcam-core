#' module_aglu_L2052.ag_prodchange_cost_irr_mgmt
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2052.AgCost_ag_irr_mgmt}, \code{L2052.AgCost_bio_irr_mgmt}, \code{L2052.AgCost_For}, \code{L2052.AgProdChange_ag_irr_ref}, \code{L2052.AgProdChange_bio_irr_ref}, \code{L2052.AgProdChange_irr_high}, \code{L2052.AgProdChange_irr_low}, \code{L2052.AgProdChange_irr_ssp4}. The corresponding file in the
#' original data system was \code{L2052.ag_prodchange_cost_irr_mgmt.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2052.ag_prodchange_cost_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "temp-data-inject/L123.For_Yield_m3m2_R_GLU",
             FILE = "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
             FILE = "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU",
             "L162.ag_YieldRate_R_C_Y_GLU_irr",
             "L162.bio_YieldRate_R_Y_GLU_irr",
             "L164.ag_Cost_75USDkg_C",
             FILE = "temp-data-inject/L201.AgYield_bio_grass",
             FILE = "temp-data-inject/L201.AgYield_bio_tree",
             "L102.pcgdp_thous90USD_Scen_R_Y"))
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
      MGMT <- AgSupplySector <- AgSupplySubsector <- AgProductionTechnology <- AgProdChange <-
      nonLandVariableCost <- high_reg <- low_reg <- region <- GCAM_region_ID <-
      year <- value <- GCAM_commodity <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    L123.For_Yield_m3m2_R_GLU <- get_data(all_data, "temp-data-inject/L123.For_Yield_m3m2_R_GLU") %>%
      gather(year, value, -GCAM_region_ID, -GCAM_commodity, -GLU) %>%  # temporary
      mutate(year = as.integer(substr(year, 2, 5)))
    L161.ag_irrProd_Mt_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU") %>%
      gather(year, value, -GCAM_region_ID, -GCAM_commodity, -GLU) %>%  # temporary
      mutate(year = as.integer(substr(year, 2, 5)))
    L161.ag_rfdProd_Mt_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      gather(year, value, -GCAM_region_ID, -GCAM_commodity, -GLU) %>%  # temporary
      mutate(year = as.integer(substr(year, 2, 5)))
    L162.ag_YieldRate_R_C_Y_GLU_irr <- get_data(all_data, "L162.ag_YieldRate_R_C_Y_GLU_irr")
    L162.bio_YieldRate_R_Y_GLU_irr <- get_data(all_data, "L162.bio_YieldRate_R_Y_GLU_irr")
    L164.ag_Cost_75USDkg_C <- get_data(all_data, "L164.ag_Cost_75USDkg_C")
    L201.AgYield_bio_grass <- get_data(all_data, "temp-data-inject/L201.AgYield_bio_grass")
    L201.AgYield_bio_tree <- get_data(all_data, "temp-data-inject/L201.AgYield_bio_tree")
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")

    # Define column names
    names_AgTech <- LEVEL2_DATA_NAMES[["AgTech"]]
    names_AgCost <- LEVEL2_DATA_NAMES[["AgCost"]]
    names_AgProdChange <- LEVEL2_DATA_NAMES[["AgProdChange"]]

    # COSTS
    # Costs of crop prodcution : combined with the corresponding chunk L2051
    # Use the L161 production tables to specify which region x glu x crop will need costs assigned
    L161.ag_irrProd_Mt_R_C_Y_GLU %>%
      mutate(IRR_RFD = "IRR") %>%
      bind_rows(mutate(L161.ag_rfdProd_Mt_R_C_Y_GLU, IRR_RFD = "RFD")) %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, IRR_RFD) %>%
      unique() %>%
      left_join_error_no_match(L164.ag_Cost_75USDkg_C, by = "GCAM_commodity") %>%
      mutate(nonLandVariableCost = round(Cost_75USDkg, aglu.DIGITS_CALPRICE)) %>%
      # Copy costs to high and low management levels
      repeat_add_columns(tibble::tibble(MGMT = c("hi", "lo"))) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      # Add sector, subsector, technology names
      mutate(AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_commodity, GLU_name, sep = "_"),
             AgProductionTechnology = paste(GCAM_commodity, GLU_name, IRR_RFD, MGMT, sep = "_")) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      select(one_of(names_AgCost)) ->
      L2052.AgCost_ag_irr_mgmt

    # Costs of bioenergy production: combined with the corresponding chunks L205 and L2051
    # Use the yield table from level1 to determine where bioenergy crops are being read in, and merge with the tech table to get both grass and tree crops
    L201.AgYield_bio_grass %>%
      select(one_of(names_AgTech)) %>%
      unique() %>%
      bind_rows(unique(select(L201.AgYield_bio_tree, one_of(names_AgTech)))) %>%
      mutate(nonLandVariableCost = aglu.BIO_GRASS_COST_75USD_GJ,
             nonLandVariableCost = replace(nonLandVariableCost, grepl("tree", AgProductionTechnology),
                                           aglu.BIO_TREE_COST_75USD_GJ)) %>%
      # Copy coefficients to all four technologies
      repeat_add_columns(tibble::tibble(IRR_RFD = c("IRR", "RFD"))) %>%
      repeat_add_columns(tibble::tibble(MGMT = c("hi", "lo"))) %>%
      # Revise technology names
      mutate(AgProductionTechnology = paste(AgProductionTechnology, IRR_RFD, MGMT, sep = "_")) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      select(one_of(names_AgCost)) ->
      L2052.AgCost_bio_irr_mgmt

    # Costs of forest production: from the corresponding chunk L205
    L123.For_Yield_m3m2_R_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GLU) %>%
      unique() %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      mutate(nonLandVariableCost = aglu.FOR_COST_75USDM3) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      # Add sector, subsector, technology names
      mutate(AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_commodity, GLU_name, sep = "_"),
             AgProductionTechnology = paste(GCAM_commodity, GLU_name, sep = "_")) %>%
      select(one_of(names_AgCost)) ->
      L2052.AgCost_For

    # Ag production change
    # Reference scenario ag prod change (not incl biomass)
    L162.ag_YieldRate_R_C_Y_GLU_irr %>%
      filter(year %in% FUTURE_YEARS) %>%
      mutate(AgProdChange = round(value, digits = aglu.DIGITS_AGPRODCHANGE)) %>%
      # If the final calibration year is less than the final historical year, this method will return Inf for crops that are 0 in one year
      # and non-zero in subsequent years. e.g. Korea and FSU FodderGrass.
      # Setting the agprodchange to 0, and keeping these techs out.
      mutate(AgProdChange = replace(AgProdChange, AgProdChange == Inf, 0)) %>%
      # Copy costs to high and low management levels
      repeat_add_columns(tibble::tibble(MGMT = c("hi", "lo"))) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      # Add sector, subsector, technology names
      mutate(AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_commodity, GLU_name, sep = "_"),
             AgProductionTechnology = paste(GCAM_commodity, GLU_name, Irr_Rfd, MGMT, sep = "_")) %>%
      select(one_of(names_AgProdChange)) ->
      L2052.AgProdChange_ag_irr_ref

    # Reference scenario ag prod change for biomass crops
    L162.bio_YieldRate_R_Y_GLU_irr %>%
      filter(year %in% FUTURE_YEARS) %>%
      mutate(AgProdChange = round(value, digits = aglu.DIGITS_AGPRODCHANGE)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) ->
      L2051.AgProdChange_bio_irr_ref

    L201.AgYield_bio_grass %>%
      select(one_of(names_AgTech)) %>%
      unique() %>%
      bind_rows(unique(select(L201.AgYield_bio_tree, one_of(names_AgTech)))) %>%
      repeat_add_columns(tibble::tibble(year = FUTURE_YEARS)) %>%
      repeat_add_columns(tibble::tibble(IRR_RFD = c("IRR", "RFD"))) %>%
      separate(AgProductionTechnology, c("biomass", "type", "GLU_name"), sep = "_") %>%
      left_join(L2051.AgProdChange_bio_irr_ref[c("region", "GLU_name", "Irr_Rfd", "year", "AgProdChange")],
                by = c("region", "GLU_name", "IRR_RFD" = "Irr_Rfd", "year")) %>%
      # Note: the ag prod change values for bioenergy crops are applied equally to grass and tree crops.
      # Grass crops are available in any land use regions with crop production, and tree crops are available in any region with forests.
      # Because the yield growth rates are based on crops, some places that have forests but no cropland will not have yield improvement rates.
      # These regions are assumed minor agriculturally and as such not assigned yield improvement for tree-based bioenergy crops.
      replace_na(list(AgProdChange = 0)) %>%
      # Copy coefficients to all four technologies
      repeat_add_columns(tibble::tibble(MGMT = c("hi", "lo"))) %>%
      # Revise technology names
      mutate(AgProductionTechnology = paste(AgSupplySubsector, IRR_RFD, MGMT, sep = "_")) %>%
      select(one_of(names_AgProdChange)) ->
      L2052.AgProdChange_bio_irr_ref

    # High ag prod change (not incl biomass)
    L2052.AgProdChange_ag_irr_ref %>%
      mutate(AgProdChange = AgProdChange * aglu.HI_PROD_GROWTH_MULT) ->
      L2052.AgProdChange_irr_high

    # Low ag prod change (not incl biomass)
    L2052.AgProdChange_ag_irr_ref %>%
      mutate(AgProdChange = AgProdChange * aglu.LOW_PROD_GROWTH_MULT) ->
      L2052.AgProdChange_irr_low

    # SSP4 ag prod change (not incl biomass)
     L102.pcgdp_thous90USD_Scen_R_Y %>%
       filter(scenario == "SSP4" & year == 2010) %>%
       left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
       mutate(value = value / gdp_deflator(1990, 2010)) %>%
       select(region, value) ->
       L225.pcgdp_2010

     L225.pcgdp_2010 %>%
       filter(value > aglu.HIGH_GROWTH_PCGDP) %>%
       select(region) %>%
       .[["region"]] -> high_reg
     L225.pcgdp_2010 %>%
       filter(value < aglu.LOW_GROWTH_PCGDP) %>%
       select(region) %>%
       .[["region"]] -> low_reg

     L2052.AgProdChange_ag_irr_ref %>%
       filter(!region %in% c(high_reg, low_reg)) %>%
       bind_rows(filter(L2052.AgProdChange_irr_high, region %in% high_reg),
                 filter(L2052.AgProdChange_irr_low, region %in% low_reg)) ->
       L2052.AgProdChange_irr_ssp4

    # Produce outputs
    L2052.AgCost_ag_irr_mgmt %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2052.AgCost_ag_irr_mgmt") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU",
                     "L164.ag_Cost_75USDkg_C") ->
      L2052.AgCost_ag_irr_mgmt

    L2052.AgCost_bio_irr_mgmt %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2052.AgCost_bio_irr_mgmt") %>%
      add_precursors("temp-data-inject/L201.AgYield_bio_grass",
                     "temp-data-inject/L201.AgYield_bio_tree") ->
      L2052.AgCost_bio_irr_mgmt

    L2052.AgCost_For %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2052.AgCost_For") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "temp-data-inject/L123.For_Yield_m3m2_R_GLU") ->
      L2052.AgCost_For

    L2052.AgProdChange_ag_irr_ref %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2052.AgProdChange_ag_irr_ref") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L162.ag_YieldRate_R_C_Y_GLU_irr") ->
      L2052.AgProdChange_ag_irr_ref

    L2052.AgProdChange_bio_irr_ref %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2052.AgProdChange_bio_irr_ref") %>%
      add_precursors("L162.bio_YieldRate_R_Y_GLU_irr",
                     "temp-data-inject/L201.AgYield_bio_grass",
                     "temp-data-inject/L201.AgYield_bio_tree") ->
      L2052.AgProdChange_bio_irr_ref

    L2052.AgProdChange_irr_high %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2052.AgProdChange_irr_high") %>%
      same_precursors_as("L2052.AgProdChange_ag_irr_ref") %>%
      add_precursors("L102.pcgdp_thous90USD_Scen_R_Y") %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L2052.AgProdChange_irr_high

    L2052.AgProdChange_irr_low %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2052.AgProdChange_irr_low") %>%
      same_precursors_as("L2052.AgProdChange_ag_irr_ref") %>%
      add_precursors("L102.pcgdp_thous90USD_Scen_R_Y") ->
      L2052.AgProdChange_irr_low

    L2052.AgProdChange_irr_ssp4 %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2052.AgProdChange_irr_ssp4") %>%
      same_precursors_as("L2052.AgProdChange_ag_irr_ref") %>%
      add_precursors("L102.pcgdp_thous90USD_Scen_R_Y") %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L2052.AgProdChange_irr_ssp4

    return_data(L2052.AgCost_ag_irr_mgmt, L2052.AgCost_bio_irr_mgmt, L2052.AgCost_For, L2052.AgProdChange_ag_irr_ref, L2052.AgProdChange_bio_irr_ref, L2052.AgProdChange_irr_high, L2052.AgProdChange_irr_low, L2052.AgProdChange_irr_ssp4)
  } else {
    stop("Unknown command")
  }
}
