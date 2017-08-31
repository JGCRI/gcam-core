#' module_aglu_L223.land_input_3
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{curr_table$data}, \code{L223.LN3_Logit}, \code{L223.LN3_LeafGhostShare}, \code{L223.LN3_LeafIsGhostShareRel}, \code{L223.LN3_HistUnmgdAllocation}, \code{L223.LN3_UnmgdAllocation}, \code{L223.NodeEquiv}, \code{L223.LN3_NoEmissCarbon}, \code{L223.LN3_NodeCarbon}, \code{L223.LN3_HistMgdAllocation_noncrop}, \code{L223.LN3_MgdAllocation_noncrop}, \code{L223.LN3_HistMgdAllocation_crop}, \code{L223.LN3_MgdAllocation_crop}, \code{L223.LN3_HistMgdAllocation_bio}, \code{L223.LN3_MgdAllocation_bio}, \code{L223.LN3_UnmgdCarbon}, \code{L223.LN3_MgdCarbon_noncrop}, \code{L223.LN3_MgdCarbon_crop}, \code{L223.LN3_MgdCarbon_bio}, \code{curr_table$data}, \code{L223.LN1_Logit_prot}, \code{L223.LN3_HistUnmgdAllocation_noprot}, \code{L223.LN3_UnmgdAllocation_noprot}, \code{L223.LN1_HistUnmgdAllocation_prot}, \code{L223.LN1_UnmgdAllocation_prot}, \code{L223.LN1_UnmgdCarbon_prot}. The corresponding file in the
#' original data system was \code{L223.land_input_3.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L223.land_input_3 <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "aglu/GCAMLandLeaf_CdensityLT",
             FILE = "aglu/A_bio_ghost_share",
             FILE = "aglu/A_Fodderbio_chars",
             FILE = "aglu/A_LT_Mapping",
             FILE = "aglu/A_LandNode_logit",
             FILE = "aglu/A_LandLeaf_Unmgd3",
             FILE = "aglu/A_LandLeaf3",
             "L111.ag_resbio_R_C",
             "L121.CarbonContent_kgm2_R_LT_GLU",
             "L122.ag_EcYield_kgm2_R_C_Y_GLU",
             "L125.LC_bm2_R_LT_Yh_GLU",
             #"L201.AgYield_bio_grass",
             #"L201.AgYield_bio_tree"
             "L2012.AgYield_bio_ref"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L223.LN3_Logit",
             "L223.LN3_LeafGhostShare",
             "L223.LN3_LeafIsGhostShareRel",
             "L223.LN3_HistUnmgdAllocation",
             "L223.LN3_UnmgdAllocation",
             "L223.NodeEquiv",
             "L223.LN3_NoEmissCarbon",
             "L223.LN3_NodeCarbon",
             "L223.LN3_HistMgdAllocation_noncrop",
             "L223.LN3_MgdAllocation_noncrop",
             "L223.LN3_HistMgdAllocation_crop",
             "L223.LN3_MgdAllocation_crop",
             "L223.LN3_HistMgdAllocation_bio",
             "L223.LN3_MgdAllocation_bio",
             "L223.LN3_UnmgdCarbon",
             "L223.LN3_MgdCarbon_noncrop",
             "L223.LN3_MgdCarbon_crop",
             "L223.LN3_MgdCarbon_bio",
             "L223.LN1_Logit_prot",
             "L223.LN3_HistUnmgdAllocation_noprot",
             "L223.LN3_UnmgdAllocation_noprot",
             "L223.LN1_HistUnmgdAllocation_prot",
             "L223.LN1_UnmgdAllocation_prot",
             "L223.LN1_UnmgdCarbon_prot"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    GCAMLandLeaf_CdensityLT <- get_data(all_data, "aglu/GCAMLandLeaf_CdensityLT")
    A_bio_ghost_share <- get_data(all_data, "aglu/A_bio_ghost_share")
    A_Fodderbio_chars <- get_data(all_data, "aglu/A_Fodderbio_chars")
    A_LT_Mapping <- get_data(all_data, "aglu/A_LT_Mapping")
    A_LandNode_logit <- get_data(all_data, "aglu/A_LandNode_logit")
    A_LandLeaf_Unmgd3 <- get_data(all_data, "aglu/A_LandLeaf_Unmgd3")
    A_LandLeaf3 <- get_data(all_data, "aglu/A_LandLeaf3")
    L111.ag_resbio_R_C <- get_data(all_data, "L111.ag_resbio_R_C")
    L121.CarbonContent_kgm2_R_LT_GLU <- get_data(all_data, "L121.CarbonContent_kgm2_R_LT_GLU")
    L122.ag_EcYield_kgm2_R_C_Y_GLU <- get_data(all_data, "L122.ag_EcYield_kgm2_R_C_Y_GLU")
    L125.LC_bm2_R_LT_Yh_GLU <- get_data(all_data, "L125.LC_bm2_R_LT_Yh_GLU")
    L2012.AgYield_bio_ref <- get_data(all_data, "L2012.AgYield_bio_ref")

    # silence package check notes
    GCAM_commodity <- GCAM_region_ID <- region <- value <- year <- GLU <- GLU_name <- GLU_code <-
      LandLeaf <- Land_Type <- LandNode <- LandNode1 <- LandNode2 <- LandNode3 <- UnmanagedLandLeaf <-
      logit.year.fillout <- logit.exponent <- logit.type <- soilTimeScale <- `mature age` <- mature.age <-
      soil_c <- veg_c <- LC_bm2 <- LV_milUSD75 <- LV_USD75_bm2 <- LV_USD75_m2 <- HarvCropLand_bm2 <-
      unManagedLandValue <- LandAllocatorRoot <- hist.veg.carbon.density <- hist.soil.carbon.density <-
      veg.carbon.density <- soil.carbon.density <- allocation <- Land_Type.y <- mature.age.year.fillout <-
      min.veg.carbon.density <- min.soil.carbon.density <- . <- NULL


    # Process inputs
    # Replace GLU names and Add region names as needed
    L111.ag_resbio_R_C %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L111.ag_resbio_R_C

    L121.CarbonContent_kgm2_R_LT_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) %>%
      rename(mature.age = `mature age`) ->
      L121.CarbonContent_kgm2_R_LT_GLU

    L122.ag_EcYield_kgm2_R_C_Y_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) ->
      L122.ag_EcYield_kgm2_R_C_Y_GLU

    L125.LC_bm2_R_LT_Yh_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) ->
      L125.LC_bm2_R_LT_Yh_GLU


    # Build Tables






    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN3_Logit") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN3_Logit

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN3_LeafGhostShare") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN3_LeafGhostShare

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN3_LeafIsGhostShareRel") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN3_LeafIsGhostShareRel

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN3_HistUnmgdAllocation") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN3_HistUnmgdAllocation

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN3_UnmgdAllocation") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN3_UnmgdAllocation

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.NodeEquiv") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.NodeEquiv

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN3_NoEmissCarbon") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN3_NoEmissCarbon

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN3_NodeCarbon") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN3_NodeCarbon

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN3_HistMgdAllocation_noncrop") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN3_HistMgdAllocation_noncrop

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN3_MgdAllocation_noncrop") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN3_MgdAllocation_noncrop

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN3_HistMgdAllocation_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN3_HistMgdAllocation_crop

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN3_MgdAllocation_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN3_MgdAllocation_crop

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN3_HistMgdAllocation_bio") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN3_HistMgdAllocation_bio

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN3_MgdAllocation_bio") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN3_MgdAllocation_bio

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN3_UnmgdCarbon") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN3_UnmgdCarbon

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN3_MgdCarbon_noncrop") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN3_MgdCarbon_noncrop

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN3_MgdCarbon_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN3_MgdCarbon_crop

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN3_MgdCarbon_bio") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN3_MgdCarbon_bio

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN1_Logit_prot") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN1_Logit_prot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN3_HistUnmgdAllocation_noprot") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN3_HistUnmgdAllocation_noprot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN3_UnmgdAllocation_noprot") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN3_UnmgdAllocation_noprot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN1_HistUnmgdAllocation_prot") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN1_HistUnmgdAllocation_prot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN1_UnmgdAllocation_prot") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN1_UnmgdAllocation_prot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.LN1_UnmgdCarbon_prot") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LT_Mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd3",
                     "aglu/A_LandLeaf3",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L2012.AgYield_bio_ref") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L223.LN1_UnmgdCarbon_prot

    return_data(L223.LN3_Logit, L223.LN3_LeafGhostShare, L223.LN3_LeafIsGhostShareRel, L223.LN3_HistUnmgdAllocation, L223.LN3_UnmgdAllocation, L223.NodeEquiv, L223.LN3_NoEmissCarbon, L223.LN3_NodeCarbon, L223.LN3_HistMgdAllocation_noncrop, L223.LN3_MgdAllocation_noncrop, L223.LN3_HistMgdAllocation_crop, L223.LN3_MgdAllocation_crop, L223.LN3_HistMgdAllocation_bio, L223.LN3_MgdAllocation_bio, L223.LN3_UnmgdCarbon, L223.LN3_MgdCarbon_noncrop, L223.LN3_MgdCarbon_crop, L223.LN3_MgdCarbon_bio, curr_table$data, L223.LN1_Logit_prot, L223.LN3_HistUnmgdAllocation_noprot, L223.LN3_UnmgdAllocation_noprot, L223.LN1_HistUnmgdAllocation_prot, L223.LN1_UnmgdAllocation_prot, L223.LN1_UnmgdCarbon_prot)
  } else {
    stop("Unknown command")
  }
}
