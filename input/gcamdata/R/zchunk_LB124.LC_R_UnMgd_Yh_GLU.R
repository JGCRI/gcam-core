# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LB124.LC_R_UnMgd_Yh_GLU
#'
#' Adjust land cover data for unmanaged land types (shrub, grass, unmgd pasture, unmgd forest) in L120.LC_bm2_R_LT_Yh_GLU to deduct
#' the extra cropland needs described in L122.LC_bm2_R_ExtraCropLand_Yh_GLU proportionally according to the relative share of each unmanaged
#' land type.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L124.LC_bm2_R_Shrub_Yh_GLU_adj}, \code{L124.LC_bm2_R_Grass_Yh_GLU_adj}, \code{L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj}, \code{L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj}. The corresponding file in the
#' original data system was \code{LB124.LC_R_UnMgd_Yh_GLU.R} (aglu level1).
#' @details Initial unmanaged land cover estimates from L120.LC_bm2_R_LT_Yh_GLU for each unmanaged land type (shrub, grass,
#' unmgd pasture, unmgd forest) are aggregated to produce the total amount of unmanaged land in each region-GLU-year. This
#' is then used with the extra cropland calculated in L122.LC_bm2_R_ExtraCropLand_Yh_GLU to create an adjustment factor
#' for each region-GLU-year. This adjustment factor is applied to each unmanaged land type in each region-GLU-year so that
#' the extra cropland is deducted from each unmanaged land type according to its relative share.
#'
#' Initial unmanaged pasture area in each region-glu-year is calculated as total pasture are in L120.LC_bm2_R_LT_Yh_GLU
#' minus managed pasture area in L123.LC_bm2_R_MgdPast_Yh_GLU.
#' Initial unmanaged forest area in each region-glu-year is calculated as total forest are in L120.LC_bm2_R_LT_Yh_GLU
#' minus managed forest area in L123.LC_bm2_R_MgdFor_Yh_GLU.
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by left_join mutate select summarise
#' @importFrom tidyr replace_na
#' @author ACS August 2017
module_aglu_LB124.LC_R_UnMgd_Yh_GLU <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L120.LC_bm2_R_LT_Yh_GLU",
              "L122.LC_bm2_R_ExtraCropLand_Yh_GLU",
              "L123.LC_bm2_R_MgdPast_Yh_GLU",
              "L123.LC_bm2_R_MgdFor_Yh_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L124.LC_bm2_R_Shrub_Yh_GLU_adj",
             "L124.LC_bm2_R_Grass_Yh_GLU_adj",
             "L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj",
             "L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L120.LC_bm2_R_LT_Yh_GLU <- get_data(all_data, "L120.LC_bm2_R_LT_Yh_GLU", strip_attributes = TRUE)
    L122.LC_bm2_R_ExtraCropLand_Yh_GLU <- get_data(all_data, "L122.LC_bm2_R_ExtraCropLand_Yh_GLU")
    L123.LC_bm2_R_MgdPast_Yh_GLU <- get_data(all_data, "L123.LC_bm2_R_MgdPast_Yh_GLU")
    L123.LC_bm2_R_MgdFor_Yh_GLU <- get_data(all_data, "L123.LC_bm2_R_MgdFor_Yh_GLU")

    # silence package check notes
    GCAM_region_ID <- value <- year <- GLU <-  Land_Type <- TotPasture <-
      MgdPasture <- TotForest <- MgdForest <- TotUnmgdLand <- ExtraCropland <- adjustmentRatio <-
      . <- NULL

    # Perform Computations:

    # Calculate initial estimates of shrubland and grassland,
    # taken directly from SAGE minus HYDE and WDPA by subsetting
    # unadjusted land cover data
    L120.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type == "Shrubland") ->
      L124.LC_bm2_R_Shrub_Yh_GLU

    L120.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type == "Grassland") ->
      L124.LC_bm2_R_Grass_Yh_GLU


    # Calculate initial estimate of unmanaged pasture = total
    # pasture from Hyde minus managed pasture
    L120.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type == "Pasture") %>%
      rename(TotPasture = value) %>%
      left_join_error_no_match(L123.LC_bm2_R_MgdPast_Yh_GLU, by = c("GCAM_region_ID", "GLU", "Land_Type", "year")) %>%
      rename(MgdPasture = value) %>%
      # have to use value instead of more informative name so that binding all unmanaged land later goes smoothly:
      mutate(value = TotPasture - MgdPasture,
             Land_Type = "UnmanagedPasture") %>%
      select(-TotPasture, -MgdPasture) ->
      L124.LC_bm2_R_UnMgdPast_Yh_GLU


    # Calculate initial estimate of unmanaged forest = total
    # forest from SAGE/Hyde minus managed forest
    L120.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type == "Forest") %>%
      rename(TotForest = value) %>%
      left_join_error_no_match(L123.LC_bm2_R_MgdFor_Yh_GLU, by = c("GCAM_region_ID", "GLU", "Land_Type", "year")) %>%
      rename(MgdForest = value) %>%
      # have to use value instead of more informative name so that binding all unmanaged land later goes smoothly:
      mutate(value = TotForest - MgdForest,
             Land_Type = "UnmanagedForest") %>%
      select(-TotForest, -MgdForest) ->
      L124.LC_bm2_R_UnMgdFor_Yh_GLU


    # Combine all unmanaged land types into a single table for processing in multiple subsequent pipelines
    bind_rows(L124.LC_bm2_R_Grass_Yh_GLU,
              L124.LC_bm2_R_Shrub_Yh_GLU,
              L124.LC_bm2_R_UnMgdFor_Yh_GLU,
              L124.LC_bm2_R_UnMgdPast_Yh_GLU) ->
      L124.LC_bm2_R_LTunmgd_Yh_GLU


    # The initial estimates of shrubland, grassland, unmanaged pasture,
    # and unmanaged forest must have land deducted from them to cover
    # the "extra" cropland calculated in chunk LB122, setting a max
    # harvested:cropped ratio and resulting in table
    # L122.LC_bm2_R_ExtraCropLand_Yh_GLU.
    #
    # Calculate the adjustment ratio for this deduction.
    # adjustment ratio = (Total Unmgd Land - Extra Cropland) / Total Unmgd Land
    # First, calculate the total amount of unmanaged land:
    L124.LC_bm2_R_LTunmgd_Yh_GLU %>%
      group_by(GCAM_region_ID, GLU, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      mutate(Land_Type = "All_Unmanaged") %>%
      rename(TotUnmgdLand = value) %>%
      # Then, Calculate adjustment ratio = (Total Unmgd Land - Extra Cropland) / Total Unmgd Land
      # to get adjusted total unmanaged land, keeping NAs for later processing:
      left_join(select(L122.LC_bm2_R_ExtraCropLand_Yh_GLU, -Land_Type), by = c("GCAM_region_ID", "GLU", "year")) %>%
      rename(ExtraCropland = value) %>%
      mutate(adjustmentRatio = (TotUnmgdLand - ExtraCropland) / TotUnmgdLand) %>%
      replace_na(list(adjustmentRatio = 1)) %>%
      select(-TotUnmgdLand, -ExtraCropland, -Land_Type) ->
      L124.LC_UnMgdAdj_R_Yh_GLU

    # Check that enough unmanaged land is available for the cropland expansion in all regions/GLUs
    if(any(L124.LC_UnMgdAdj_R_Yh_GLU$adjustmentRatio < 0)) {
      stop("Increase in cropland exceeds available unmanaged land")
    }


    # Apply the adjusment ratio to the different land types
    L124.LC_bm2_R_LTunmgd_Yh_GLU %>%
      left_join_error_no_match(L124.LC_UnMgdAdj_R_Yh_GLU, by = c("GCAM_region_ID", "GLU", "year")) %>%
      mutate(value = value * adjustmentRatio) %>%
      select(-adjustmentRatio) ->
      L124.LC_bm2_R_LTunmgd_Yh_GLU_adj


    # Produce outputs
    L124.LC_bm2_R_LTunmgd_Yh_GLU_adj %>%
      filter(Land_Type == "Shrubland") %>%
      add_title("Shrub land cover by GCAM region / historical year / GLU") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Initial unmanaged land cover estimates from L120.LC_bm2_R_LT_Yh_GLU are aggregated to produce") %>%
      add_comments("the total amount of unmanaged land in each region-GLU-year. This is then used with") %>%
      add_comments("the extra cropland calculated in L122.LC_bm2_R_ExtraCropLand_Yh_GLU to create an adjustment") %>%
      add_comments("factor for each region-GLU-year. This adjustment factor is applied to each unmanaged land") %>%
      add_comments("type in each region-GLU-year so that the extra cropland is deducted from each unmanaged land") %>%
      add_comments("type according to its relative share.") %>%
      add_legacy_name("L124.LC_bm2_R_Shrub_Yh_GLU_adj") %>%
      add_precursors("L120.LC_bm2_R_LT_Yh_GLU",
                     "L122.LC_bm2_R_ExtraCropLand_Yh_GLU",
                     "L123.LC_bm2_R_MgdPast_Yh_GLU",
                     "L123.LC_bm2_R_MgdFor_Yh_GLU") ->
      L124.LC_bm2_R_Shrub_Yh_GLU_adj

    L124.LC_bm2_R_LTunmgd_Yh_GLU_adj %>%
      filter(Land_Type == "Grassland") %>%
      add_title("Grass land cover by GCAM region / historical year / GLU") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Initial unmanaged land cover estimates from L120.LC_bm2_R_LT_Yh_GLU are aggregated to produce") %>%
      add_comments("the total amount of unmanaged land in each region-GLU-year. This is then used with") %>%
      add_comments("the extra cropland calculated in L122.LC_bm2_R_ExtraCropLand_Yh_GLU to create an adjustment") %>%
      add_comments("factor for each region-GLU-year. This adjustment factor is applied to each unmanaged land") %>%
      add_comments("type in each region-GLU-year so that the extra cropland is deducted from each unmanaged land") %>%
      add_comments("type according to its relative share.") %>%
      add_legacy_name("L124.LC_bm2_R_Grass_Yh_GLU_adj") %>%
      same_precursors_as(L124.LC_bm2_R_Shrub_Yh_GLU_adj) ->
      L124.LC_bm2_R_Grass_Yh_GLU_adj

    L124.LC_bm2_R_LTunmgd_Yh_GLU_adj %>%
      filter(Land_Type == "UnmanagedPasture") %>%
      add_title("Unmanaged Pasture land cover by GCAM region / historical year / GLU") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Initial unmanaged pasture area in each region-glu-year is calculated as total pasture are in ") %>%
      add_comments("L120.LC_bm2_R_LT_Yh_GLU minus managed pasture area in L123.LC_bm2_R_MgdPast_Yh_GLU.") %>%
      add_comments("Initial unmanaged land cover estimates from L120.LC_bm2_R_LT_Yh_GLU are aggregated to produce") %>%
      add_comments("the total amount of unmanaged land in each region-GLU-year. This is then used with") %>%
      add_comments("the extra cropland calculated in L122.LC_bm2_R_ExtraCropLand_Yh_GLU to create an adjustment") %>%
      add_comments("factor for each region-GLU-year. This adjustment factor is applied to each unmanaged land") %>%
      add_comments("type in each region-GLU-year so that the extra cropland is deducted from each unmanaged land") %>%
      add_comments("type according to its relative share.") %>%
      add_legacy_name("L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj") %>%
      same_precursors_as(L124.LC_bm2_R_Shrub_Yh_GLU_adj) ->
      L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj

    L124.LC_bm2_R_LTunmgd_Yh_GLU_adj %>%
      filter(Land_Type == "UnmanagedForest") %>%
      add_title("Unmanaged Forest land cover by GCAM region / historical year / GLU") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Initial unmanaged forest area in each region-glu-year is calculated as total forest are in ") %>%
      add_comments("L120.LC_bm2_R_LT_Yh_GLU minus managed forest area in L123.LC_bm2_R_MgdFor_Yh_GLU.") %>%
      add_comments("Initial unmanaged land cover estimates from L120.LC_bm2_R_LT_Yh_GLU are aggregated to produce") %>%
      add_comments("the total amount of unmanaged land in each region-GLU-year. This is then used with") %>%
      add_comments("the extra cropland calculated in L122.LC_bm2_R_ExtraCropLand_Yh_GLU to create an adjustment") %>%
      add_comments("factor for each region-GLU-year. This adjustment factor is applied to each unmanaged land") %>%
      add_comments("type in each region-GLU-year so that the extra cropland is deducted from each unmanaged land") %>%
      add_comments("type according to its relative share.") %>%
      add_legacy_name("L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj") %>%
      same_precursors_as(L124.LC_bm2_R_Shrub_Yh_GLU_adj) ->
      L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj

    return_data(L124.LC_bm2_R_Shrub_Yh_GLU_adj, L124.LC_bm2_R_Grass_Yh_GLU_adj, L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj, L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj)
  } else {
    stop("Unknown command")
  }
}
