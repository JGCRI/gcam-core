# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L221.land_input_1
#'
#' Produce L221.LN0_Logit, L221.LN0_Land, L221.LN0_SoilTimeScale, L221.LN1_ValueLogit,
#' L221.LN1_HistUnmgdAllocation, L221.LN1_UnmgdAllocation, and L221.LN1_UnmgdCarbon.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{curr_table$data}, \code{L221.LN0_Logit}, \code{L221.LN0_Land}, \code{L221.LN0_SoilTimeScale}, \code{curr_table$data}, \code{L221.LN1_ValueLogit}, \code{L221.LN1_HistUnmgdAllocation}, \code{L221.LN1_UnmgdAllocation}, \code{L221.LN1_UnmgdCarbon}. The corresponding file in the
#' original data system was \code{L221.land_input_1.R} (aglu level2).
#' @details
#' \itemize{
#' \item{"L221.LN0_Logit Logit exponent of the top-level (zero) land nest by region.
#' AgLu regions are given externally defined constant logit information."}
#' \item{"L221.LN0_Land Total regional land allocation in top level nest.
#' L125 land cover data is assigned proper GCAM region names and nest id."}
#' \item{"L221.LN0_SoilTimeScale Soil time scale by region.
#' External soil time scale assumptions for each AGLU region."}
#' \item{"L221.LN1_ValueLogit Unmanaged land value by region and GLU, and logit exponent of first nest.
#' L131 land value data is joined with LandNode Logit assumptions.A minimum value is set to ensure all
#' regions have a nonzero land value."}
#' \item{"L221.LN1_HistUnmgdAllocation Historical land cover fora unmanaged land (LT_GLU) in the first nest by region.
#' Historical land cover for unmanaged land in the first nest, from L125 land cover data."}
#' \item{"L221.LN1_UnmgdAllocation Land cover in the model base periods for unmanaged land (LT_GLU) in the first nest by region.
#' Land cover in the model base periods for unmanaged land in the first nest, from L125 land cover data."}
#' \item{"L221.LN1_UnmgdCarbon Carbon content for unmanaged land (LT_GLU) in first nest by region.
#' Carbon content info for unmanaged land in the first nest including soil and vegetative carbon,
#' from L125 land cover data, L121 carbon content data, and GCAMLandLeaf_CdensityLT assumptions."}
#' }
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter left_join mutate select
#' @author ACS August 2017 XZ (2022)
module_aglu_L221.land_input_1 <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "water/basin_to_country_mapping",
      FILE = "aglu/GCAMLandLeaf_CdensityLT",
      FILE = "aglu/A_LandNode_logit",
      FILE = "aglu/A_LandLeaf_Unmgd1",
      FILE = "aglu/A_LT_Mapping",
      FILE = "aglu/A_soil_time_scale_R",
      "L121.CarbonContent_kgm2_R_LT_GLU",
      "L125.LC_bm2_R_LT_Yh_GLU",
      "L125.LC_bm2_R",
      "L131.LV_USD75_m2_R_GLU",
      "L120.LC_soil_veg_carbon_GLU",
      # Inputs for calculating cropland rental profits
      "L2252.LN5_MgdAllocation_crop",
      "L2012.AgProduction_ag_irr_mgmt",
      "L2012.AgSupplySector",
      "L2052.AgCost_ag_irr_mgmt")

  MODULE_OUTPUTS <-
    c("L221.LN0_Logit",
      "L221.LN0_Land",
      "L221.LN0_SoilTimeScale",
      "L221.LN1_ValueLogit",
      "L221.LN1_HistUnmgdAllocation",
      "L221.LN1_UnmgdAllocation",
      "L221.LN1_UnmgdCarbon")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data,
                  MODULE_INPUTS[!MODULE_INPUTS %in% c("L120.LC_soil_veg_carbon_GLU",
                                                       "L121.CarbonContent_kgm2_R_LT_GLU")],
                          strip_attributes = TRUE)


    # If the carbon data source is set to moirai, use the spatially distinct carbon values. If not, use the Houghton values.
    if(aglu.CARBON_DATA_SOURCE =="moirai"){

      L121.CarbonContent_kgm2_R_LT_GLU <- get_data(all_data, "L120.LC_soil_veg_carbon_GLU")
    }else{
      L121.CarbonContent_kgm2_R_LT_GLU <- get_data(all_data, "L121.CarbonContent_kgm2_R_LT_GLU")

    }


    # silence package check notes
    GCAM_commodity <- GCAM_region_ID <- region <- value <- year <- GLU <- GLU_name <- GLU_code <-
      LandLeaf <- Land_Type <- LandNode <- LandNode1 <- LandNode2 <- LandNode3 <- UnmanagedLandLeaf <-
      logit.year.fillout <- logit.exponent <- logit.type <- soilTimeScale <- `mature age` <- mature.age <-
      soil_c <- veg_c <- LC_bm2 <- LV_milUSD75 <- LV_USD75_bm2 <- LV_USD75_m2 <- HarvCropLand_bm2 <-
      unManagedLandValue <- LandAllocatorRoot <- hist.veg.carbon.density <- hist.soil.carbon.density <-
      veg.carbon.density <- soil.carbon.density <- allocation <- Land_Type.y <- mature.age.year.fillout <-
      min.veg.carbon.density <- min.soil.carbon.density <- . <- NULL


    # 1. Process inputs ----

    # Replace GLU names and Add region names
    L121.CarbonContent_kgm2_R_LT_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) %>%
      rename(mature.age = `mature age`) ->
      L121.CarbonContent_kgm2_R_LT_GLU

    L125.LC_bm2_R_LT_Yh_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) ->
      L125.LC_bm2_R_LT_Yh_GLU

    L131.LV_USD75_m2_R_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) %>%
      ungroup ->
      L131.LV_USD75_m2_R_GLU

    # Add region names to inputs
    L125.LC_bm2_R %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L125.LC_bm2_R

    A_soil_time_scale_R %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      A_soil_time_scale_R

    # Convert land value to  1975$ per thousand km2 and calculate minimum land value,
    # setting a minimum threshold on the land values to ensure that no land use regions get a value of zero
    # min_LV_USD75_bm2 is picked based on data and represents a very conservative value (better than zero)
    min_LV_USD75_bm2 = 200000
    L131.LV_USD75_m2_R_GLU %>%
      mutate(LV_USD75_bm2 = round(LV_USD75_m2 * CONV_BM2_M2, aglu.DIGITS_LAND_VALUE)) %>%
      select(region, GLU, LV_USD75_bm2) %>%
      filter(LV_USD75_bm2 > min_LV_USD75_bm2) ->
      L131.LV_USD75_bm2_R_GLU

    # Get a regional median value to fill in zero or NA later ----
    # Previously min value was used which could underestimate the marginal land conversion cost

    # One motivation of this change was to fix the Southeast Asia Hong basin issue
    # Note that the data source and the method for generating
    # unmanaged land rental profit should be revisited (XZ)
    L131.LV_USD75_bm2_R_GLU %>%
      group_by(region) %>%
      summarize(reg_med_LV_USD75_bm2 = median(LV_USD75_bm2)) %>%
      ungroup() %>%
      complete(region = GCAM_region_names$region) %>%
      replace_na(list(reg_med_LV_USD75_bm2 = 0)) %>%
      mutate(reg_med_LV_USD75_bm2 = if_else(reg_med_LV_USD75_bm2 == 0,
                                            median(reg_med_LV_USD75_bm2), reg_med_LV_USD75_bm2)) ->
      med_LV_USD75_bm2


    # 2. Build tables ----

    # Build L221.LN0_Logit: Logit exponent of the top-level (zero) land nest
    L125.LC_bm2_R %>%
      select(region) %>%
      mutate(LandAllocatorRoot = "root",
             logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = aglu.N0_LOGIT_EXP,
             logit.type = NA) ->
      L221.LN0_Logit


    # Build L221.LN0_Land: Total regional land allocation
    L125.LC_bm2_R %>%
      mutate(LandAllocatorRoot = "root",
            year.fillout = min(MODEL_BASE_YEARS)) %>%
      rename(landAllocation = LC_bm2) %>%
      select(-GCAM_region_ID) ->
      L221.LN0_Land


    # Build L221.LN0_SoilTimeScale: Soil time scale by region
    A_soil_time_scale_R %>%
      filter(!(region %in% aglu.NO_AGLU_REGIONS)) %>%
      select(-GCAM_region_ID) %>%
      mutate(LandAllocatorRoot = "root") ->
      L221.LN0_SoilTimeScale


    # Build L221.LN1_ValueLogit: Unmanaged land value by region and AEZ, and logit exponent of first nest
    L125.LC_bm2_R_LT_Yh_GLU %>%
      left_join(select(A_LT_Mapping, Land_Type, LandNode1), by = "Land_Type") %>%
      select(region, GLU, LandNode1) %>%
      distinct %>%
      na.omit %>%
      mutate(LandAllocatorRoot = "root",
             logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.type = NA) %>%
      # add logit exponents
      # This is where logit types would be added as well, but currently omitting to allow left_join_error_no_match_use
      left_join_error_no_match(select(A_LandNode_logit, LandNode, logit.exponent), by = c("LandNode1" = "LandNode")) %>%
      # add land value, maintaining any NA's to be replaced with minimum
      left_join(select(L131.LV_USD75_bm2_R_GLU, region, GLU, LV_USD75_bm2), by = c("region", "GLU")) %>%
      rename(unManagedLandValue = LV_USD75_bm2) %>%
      left_join_error_no_match(med_LV_USD75_bm2, by = "region") %>%
      # update land value with minimum to make sure every region-glu has a nonzero land value
      mutate(unManagedLandValue = if_else(is.na(unManagedLandValue) | unManagedLandValue == 0,
                                          reg_med_LV_USD75_bm2, unManagedLandValue),
             LandNode1 = paste(LandNode1, GLU, sep = aglu.CROP_GLU_DELIMITER)) %>%
      select(region, LandAllocatorRoot, LandNode1, unManagedLandValue, logit.year.fillout, logit.exponent, logit.type) ->
      L221.LN1_ValueLogit

    # 3. Adding options of using GCAM data for unmanaged land rental prices ----

    # Adding a module-specific variable UsingGCAMCroplandRentalProfit
    # If TRUE, calculating a mean rental profit for cropland for all water basins and use that as unmanaged land rental prices

    UsingGCAMCroplandRentalProfit <- TRUE
    # Otherwise, the default values from GTAP (year 2000) will be used
    # The variable is only added here since it is module-specific
    # The original GTAP approach can be superseded. Keeping them now, but they can be removed later.

    if (UsingGCAMCroplandRentalProfit == TRUE) {

      # 3.1. Calculate rental profits for cropland

      # Formula:
      # Rental profit (1975$ per thousand km2)
      # Rental profit = (P - NLC) * yield
      # Units
      # (1975$/kg - 1975$/kg) * kg/thousand km2 or
      # (1975$/kg - 1975$/kg) * Mt * 10^9/bm2 or
      # (1975$/t - 1975$/t) * t /bm2

      # Calculate P - NLC
      # Note that water cost is not included in NLC; it is ignored here
      # L2052 cost should be the NLC for this calculation
      L2052.AgCost_ag_irr_mgmt %>%
        left_join_error_no_match(select(L2012.AgSupplySector, region, AgSupplySector, calPrice),
                                 by = c("region", "AgSupplySector")) %>%
        #(P - NLC)
        mutate(Profit_USDPerKg = calPrice - nonLandVariableCost) ->
        L2052.UnAdjProfits

      # Calculate yield and rental profit at the technology level
      L2252.LN5_MgdAllocation_crop %>%
        rename(Area_bm2 = allocation) %>%
        left_join(
          L2012.AgProduction_ag_irr_mgmt %>%
            select(region, year, LandLeaf = AgProductionTechnology, Prod_Mt = calOutputValue),
          by = c("region", "LandLeaf", "year")
        ) %>%
        left_join(
          L2052.UnAdjProfits %>%
            select(region, year, LandLeaf = AgProductionTechnology, Profit_USDPerKg),
          by = c("region", "LandLeaf", "year")) %>%
        # (P - NLC) * yield
        mutate(RentalProfit = Profit_USDPerKg * CONV_T_KG * Prod_Mt / CONV_T_MT / Area_bm2) ->
        Cropland_RentalProfit_GLU_C_Y_IRR_MGMT

      # Calculate rental profit (weighted average) at the regional level
      Cropland_RentalProfit_GLU_C_Y_IRR_MGMT %>%
        group_by(region, year) %>%
        summarize(unManagedLandValue = weighted.mean(RentalProfit, w = Area_bm2)) %>%
        ungroup() %>%
        filter(year == max(MODEL_BASE_YEARS)) %>%
        select(-year) -> Cropland_RentalProfit_R

      # Calculate rental profit (weighted average) at the basin level
      Cropland_RentalProfit_GLU_C_Y_IRR_MGMT %>%
        group_by(region, LandAllocatorRoot, LandNode1, year) %>%
        summarize(unManagedLandValue = weighted.mean(RentalProfit, w = Area_bm2)) %>%
        ungroup() %>%
        filter(year == max(MODEL_BASE_YEARS)) %>%
        select(-year) -> Cropland_RentalProfit_GLU


      # Update the value ----
      L221.LN1_ValueLogit %>%
        rename(unManagedLandValue_GTAP = unManagedLandValue) %>%
        left_join(Cropland_RentalProfit_GLU, by = c("region", "LandAllocatorRoot", "LandNode1")) %>%
        # join regional rental profit and use to fill in NA at basin level
        left_join_error_no_match(Cropland_RentalProfit_R %>% rename(unManagedLandValue_R = unManagedLandValue),
                                 by = c("region")) %>%
        mutate(unManagedLandValue = if_else(is.na(unManagedLandValue), unManagedLandValue_R, unManagedLandValue)) %>%
        # The difference can be analysed here
        # In short, much smaller variation with the new data
        select(names(L221.LN1_ValueLogit)) ->
        L221.LN1_ValueLogit
    }


    # Land use history
    # Build a temporary table of Land Cover allocated for Unmanaged Land, and then split into different
    # output tables by years. It is also used for the Carbon Content output table
    L125.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type %in% unique(A_LandLeaf_Unmgd1$UnmanagedLandLeaf),
             year %in% c(aglu.LAND_HISTORY_YEARS, MODEL_BASE_YEARS)) %>%
      mutate(allocation = round(value, aglu.DIGITS_LAND_USE)) %>%
      select(-value) %>%
      mutate(LandNode1 = paste(Land_Type, GLU, sep = aglu.CROP_GLU_DELIMITER),
             UnmanagedLandLeaf = LandNode1,
             LandAllocatorRoot = "root") %>%
      select(region, GLU, LandAllocatorRoot, LandNode1, UnmanagedLandLeaf, year, allocation, Land_Type) ->
      L221.LC_bm2_R_Unmgd1_Yh_GLU

    # Historical land cover, unmanaged land in the first nest
    L221.LC_bm2_R_Unmgd1_Yh_GLU %>%
      filter(year %in% aglu.LAND_HISTORY_YEARS) %>%
      select(-Land_Type, -GLU) ->
      L221.LN1_HistUnmgdAllocation

    # Land cover in the model base periods, unmanaged land in the first nest
    L221.LC_bm2_R_Unmgd1_Yh_GLU %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(-Land_Type, -GLU) ->
      L221.LN1_UnmgdAllocation


    # Carbon contents and mature ages
    # Carbon content info, unmanaged land in the first nest
    L221.LC_bm2_R_Unmgd1_Yh_GLU %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      select(-year, -allocation) %>%
      left_join_error_no_match(GCAMLandLeaf_CdensityLT, by = c("Land_Type" = "LandLeaf")) %>%
      rename(Cdensity_LT = Land_Type.y) %>%
      add_carbon_info(carbon_info_table = L121.CarbonContent_kgm2_R_LT_GLU) %>%
      select(LEVEL2_DATA_NAMES[["LN1_UnmgdCarbon"]]) ->
      L221.LN1_UnmgdCarbon

    LEVEL2_DATA_NAMES$LN1_Delete


    # 3. Produce outputs
    L221.LN0_Logit %>%
      add_title("Logit exponent of the top-level (zero) land nest by region") %>%
      add_units("NA") %>%
      add_comments("AgLu regions are given externally defined constant logit information.") %>%
      add_legacy_name("L221.LN0_Logit") %>%
      add_precursors("common/GCAM_region_names",
                     "L125.LC_bm2_R") ->
      L221.LN0_Logit
    L221.LN0_Land %>%
      add_title("Total regional land allocation in top level nest") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("L125 land cover data is assigned proper GCAM region names and nest id.") %>%
      add_legacy_name("L221.LN0_Land") %>%
      add_precursors("common/GCAM_region_names",
                     "L125.LC_bm2_R") ->
      L221.LN0_Land
    L221.LN0_SoilTimeScale %>%
      add_title("Soil time scale by region") %>%
      add_units("rate") %>%
      add_comments("External soil time scale assumptions for each AGLU region") %>%
      add_legacy_name("L221.LN0_SoilTimeScale") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_soil_time_scale_R") ->
      L221.LN0_SoilTimeScale
    L221.LN1_ValueLogit %>%
      add_title("Unmanaged land value by region and GLU, and logit exponent of first nest") %>%
      add_units("1975USD/thousand square kilometers") %>%
      add_comments("L131 land value data is joined with LandNode Logit assumptions.") %>%
      add_comments("A minimum value is set to ensure all regions have a nonzero land value.") %>%
      add_legacy_name("L221.LN1_ValueLogit") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LT_Mapping",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L131.LV_USD75_m2_R_GLU",
                     "L2252.LN5_MgdAllocation_crop",
                     "L2012.AgProduction_ag_irr_mgmt",
                     "L2012.AgSupplySector",
                     "L2052.AgCost_ag_irr_mgmt") ->
      L221.LN1_ValueLogit
    L221.LN1_HistUnmgdAllocation %>%
      add_title("Historical land cover fora unmanaged land (LT_GLU) in the first nest by region") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Historical land cover for unmanaged land in the first nest, from L125 land cover data") %>%
      add_legacy_name("L221.LN1_HistUnmgdAllocation") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandLeaf_Unmgd1",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L131.LV_USD75_m2_R_GLU") ->
      L221.LN1_HistUnmgdAllocation
    L221.LN1_UnmgdAllocation %>%
      add_title("Land cover in the model base periods for unmanaged land (LT_GLU) in the first nest by region") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Land cover in the model base periods for unmanaged land in the first nest, from L125 land cover data") %>%
      add_legacy_name("L221.LN1_UnmgdAllocation") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandLeaf_Unmgd1",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L131.LV_USD75_m2_R_GLU") ->
      L221.LN1_UnmgdAllocation
    L221.LN1_UnmgdCarbon %>%
      add_title("Carbon content for unmanaged land (LT_GLU) in first nest by region") %>%
      add_units("Varies") %>%
      add_comments("Carbon content info for unmanaged land in the first nest including soil and vegetative carbon, ") %>%
      add_comments("from L125 land cover data, L121 carbon content data, and GCAMLandLeaf_CdensityLT assumptions.") %>%
      add_legacy_name("L221.LN1_UnmgdCarbon") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandLeaf_Unmgd1",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L120.LC_soil_veg_carbon_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L131.LV_USD75_m2_R_GLU") ->
      L221.LN1_UnmgdCarbon

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
