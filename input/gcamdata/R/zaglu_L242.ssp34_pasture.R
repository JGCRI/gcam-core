# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L242.ssp34_pasture
#'
#' Construct the managed and unmanaged pasture allocation datasets for SSP3 and SSP4
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L242.LN2_HistUnmgdAllocation_SSP34}, \code{L242.LN2_UnmgdAllocation_SSP34}, \code{L242.LN2_HistMgdAllocation_SSP34}, \code{L242.LN2_MgdAllocation_SSP34}. The corresponding file in the
#' original data system was \code{L242.ssp34_pasture.R} (aglu level2).
#' @details For unmanaged and managed pasture, adjust data so that their ratio is 0.25 and add node leaf names.
#' We do this for both historical and model base periods, isolating low-growth regions (defined by \code{aglu.LOW_GROWTH_PCGDP}).
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter left_join mutate pull select
#' @author BBL June 2017
module_aglu_L242.ssp34_pasture <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "aglu/GCAMLandLeaf_CdensityLT",
             FILE = "aglu/A_LandLeaf_Unmgd2",
             FILE = "aglu/A_LandLeaf2",
             "L125.LC_bm2_R_LT_Yh_GLU",
             "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L242.LN2_HistUnmgdAllocation_SSP34",
             "L242.LN2_UnmgdAllocation_SSP34",
             "L242.LN2_HistMgdAllocation_SSP34",
             "L242.LN2_MgdAllocation_SSP34"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    Land_Type <- value <- year <- region <- LandAllocatorRoot <- LandNode1 <- LandNode2 <-
      UnmanagedLandLeaf <- allocation <- GCAM_region_ID <- GLU <- value.x <- value.y <-
      LandLeaf <- scenario <- . <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    GCAMLandLeaf_CdensityLT <- get_data(all_data, "aglu/GCAMLandLeaf_CdensityLT")
    A_LandLeaf_Unmgd2 <- get_data(all_data, "aglu/A_LandLeaf_Unmgd2")
    A_LandLeaf2 <- get_data(all_data, "aglu/A_LandLeaf2")
    get_data(all_data, "L125.LC_bm2_R_LT_Yh_GLU", strip_attributes = TRUE) %>%
      replace_GLU(basin_to_country_mapping) ->
      L125.LC_bm2_R_LT_Yh_GLU
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")

    # Build tables

    FRACT_UNMGD_TO_MGD <- 0.25

    # LAND ALLOCATION AND LAND USE HISTORY
    # Subset the relevant land types from table with land cover by all land types
    # Unmanaged land (lines 43-58 in old file)
    L125.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type == "UnmanagedPasture") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # Adjust land cover downwards so more pasture is in the managed category
      mutate(allocation = value * (1 - FRACT_UNMGD_TO_MGD)) %>%
      add_node_leaf_names(nesting_table = A_LandLeaf_Unmgd2, leaf_name = "UnmanagedLandLeaf", "LandNode1", "LandNode2") ->
      L242.LC_bm2_R_Unmgd2_Yh_GLU.mlt

    # Historical land cover, unmanaged land in the second nest
    L242.LC_bm2_R_Unmgd2_Yh_GLU.mlt %>%
      filter(year %in% aglu.LAND_HISTORY_YEARS) %>%
      select(region, LandAllocatorRoot, LandNode1, LandNode2, UnmanagedLandLeaf, year, allocation) ->
      L242.LN2_HistUnmgdAllocation_ALL

    # Model base period land cover, unmanaged land in the second nest
    L242.LC_bm2_R_Unmgd2_Yh_GLU.mlt %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(region, LandAllocatorRoot, LandNode1, LandNode2, UnmanagedLandLeaf, year, allocation) ->
      L242.LN2_UnmgdAllocation_ALL

    # Managed land (60-75)
    L125.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type == "Pasture") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(select(L242.LC_bm2_R_Unmgd2_Yh_GLU.mlt, GCAM_region_ID, year, GLU, value), by = c("GCAM_region_ID", "year", "GLU")) %>%
      mutate(allocation = value.x + FRACT_UNMGD_TO_MGD * value.y) %>%
      select(GCAM_region_ID, Land_Type, GLU, year, value = value.x, region, allocation) %>%
      add_node_leaf_names(nesting_table = A_LandLeaf2, leaf_name = "LandLeaf", "LandNode1", "LandNode2") ->
      L242.LC_bm2_R_Mgd2_Yh_GLU.mlt

    # Historical land cover, managed land in the second nest
    L242.LC_bm2_R_Mgd2_Yh_GLU.mlt %>%
      filter(year %in% aglu.LAND_HISTORY_YEARS) %>%
      select(region, LandAllocatorRoot, LandNode1, LandNode2, LandLeaf, year, allocation) ->
      L242.LN2_HistMgdAllocation_ALL

    # Model base period land cover, managed land in the second nest
    L242.LC_bm2_R_Mgd2_Yh_GLU.mlt %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(region, LandAllocatorRoot, LandNode1, LandNode2, LandLeaf, year, allocation) ->
      L242.LN2_MgdAllocation_ALL

    # Create SSP4 pasture inputs, isolating poor (low GDP) regions for subsequent filtering
    get_ssp_regions(L102.pcgdp_thous90USD_Scen_R_Y, GCAM_region_names, "low") ->
      low_gdp_regions

    # Produce outputs

    L242.LN2_HistUnmgdAllocation_ALL %>%
      filter(region %in% low_gdp_regions) %>%
      add_title("Historical unmanaged pasture allocation for low-GDP regions") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("For unmanaged and managed pasture, adjust data so that their ratio is 0.25; add node leaf names.") %>%
      add_comments("Do this for both historical and model base periods, isolating low-GDP regions.") %>%
      add_legacy_name("L242.LN2_HistUnmgdAllocation_SSP34") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandLeaf_Unmgd2",
                     "aglu/A_LandLeaf2",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L242.LN2_HistUnmgdAllocation_SSP34

    L242.LN2_UnmgdAllocation_ALL %>%
      filter(region %in% low_gdp_regions) %>%
      add_title("Model period unmanaged pasture allocation for low-GDP regions") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("For unmanaged and managed pasture, adjust data so that their ratio is 0.25; add node leaf names.") %>%
      add_comments("Do this for both historical and model base periods, isolating low-GDP regions.") %>%
      add_legacy_name("L242.LN2_UnmgdAllocation_SSP34") %>%
      same_precursors_as(L242.LN2_HistUnmgdAllocation_SSP34) ->
      L242.LN2_UnmgdAllocation_SSP34

    L242.LN2_HistMgdAllocation_ALL %>%
      filter(region %in% low_gdp_regions) %>%
      add_title("Historical managed pasture allocation for low-GDP regions") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("For unmanaged and managed pasture, adjust data so that their ratio is 0.25; add node leaf names.") %>%
      add_comments("Do this for both historical and model base periods, isolating low-GDP regions.") %>%
      add_legacy_name("L242.LN2_HistMgdAllocation_SSP34") %>%
      same_precursors_as(L242.LN2_HistUnmgdAllocation_SSP34) ->
      L242.LN2_HistMgdAllocation_SSP34

    L242.LN2_MgdAllocation_ALL %>%
      filter(region %in% low_gdp_regions) %>%
      add_title("Model period managed pasture allocation for low-GDP regions") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("For unmanaged and managed pasture, adjust data so that their ratio is 0.25; add node leaf names.") %>%
      add_comments("Do this for both historical and model base periods, isolating low-GDP regions.") %>%
      add_legacy_name("L242.LN2_MgdAllocation_SSP34") %>%
      same_precursors_as(L242.LN2_HistUnmgdAllocation_SSP34) ->
      L242.LN2_MgdAllocation_SSP34

    return_data(L242.LN2_HistUnmgdAllocation_SSP34, L242.LN2_UnmgdAllocation_SSP34, L242.LN2_HistMgdAllocation_SSP34, L242.LN2_MgdAllocation_SSP34)
  } else {
    stop("Unknown command")
  }
}
