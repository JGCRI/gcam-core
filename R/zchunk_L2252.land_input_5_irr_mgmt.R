#' module_aglu_L2252.land_input_5_irr_mgmt
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{curr_table$data}, \code{L2252.LN5_Logit}, \code{L2252.LN5_HistMgdAllocation_crop}, \code{L2252.LN5_MgdAllocation_crop}, \code{L2252.LN5_HistMgdAllocation_bio}, \code{L2252.LN5_MgdAllocation_bio}, \code{L2252.LN5_MgdCarbon_crop}, \code{L2252.LN5_MgdCarbon_bio}, \code{L2252.LN5_LeafGhostShare}, \code{L2252.LN5_NodeGhostShare}. The corresponding file in the
#' original data system was \code{L2252.land_input_5_irr_mgmt.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2252.land_input_5_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             "L181.LandShare_R_bio_GLU_irr",
             "L181.LC_bm2_R_C_Yh_GLU_irr_level",
             "L181.YieldMult_R_bio_GLU_irr",
             "L2241.LN4_Logit",
             "L2241.LN4_HistMgdAllocation_crop",
             "L2241.LN4_MgdAllocation_crop",
             "L2241.LN4_HistMgdAllocation_bio",
             "L2241.LN4_MgdAllocation_bio",
             "L2241.LN4_MgdCarbon_crop",
             "L2241.LN4_MgdCarbon_bio",
             "L2241.LN4_LeafGhostShare",
             "L2012.AgProduction_ag_irr_mgmt"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2252.LN5_Logit",
             "L2252.LN5_HistMgdAllocation_crop",
             "L2252.LN5_MgdAllocation_crop",
             "L2252.LN5_HistMgdAllocation_bio",
             "L2252.LN5_MgdAllocation_bio",
             "L2252.LN5_MgdCarbon_crop",
             "L2252.LN5_MgdCarbon_bio",
             "L2252.LN5_LeafGhostShare",
             "L2252.LN5_NodeGhostShare"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    L181.LandShare_R_bio_GLU_irr <- get_data(all_data, "L181.LandShare_R_bio_GLU_irr")
    L181.LC_bm2_R_C_Yh_GLU_irr_level <- get_data(all_data, "L181.LC_bm2_R_C_Yh_GLU_irr_level")
    L181.YieldMult_R_bio_GLU_irr <- get_data(all_data, "L181.YieldMult_R_bio_GLU_irr")
    L2241.LN4_Logit <- get_data(all_data, "L2241.LN4_Logit")
    L2241.LN4_HistMgdAllocation_crop <- get_data(all_data, "L2241.LN4_HistMgdAllocation_crop")
    L2241.LN4_MgdAllocation_crop <- get_data(all_data, "L2241.LN4_MgdAllocation_crop")
    L2241.LN4_HistMgdAllocation_bio <- get_data(all_data, "L2241.LN4_HistMgdAllocation_bio")
    L2241.LN4_MgdAllocation_bio <- get_data(all_data, "L2241.LN4_MgdAllocation_bio")
    L2241.LN4_MgdCarbon_crop <- get_data(all_data, "L2241.LN4_MgdCarbon_crop")
    L2241.LN4_MgdCarbon_bio <- get_data(all_data, "L2241.LN4_MgdCarbon_bio")
    L2241.LN4_LeafGhostShare <- get_data(all_data, "L2241.LN4_LeafGhostShare")
    L2012.AgProduction_ag_irr_mgmt <- get_data(all_data, "L2012.AgProduction_ag_irr_mgmt")


    # silence package check notes
    GCAM_commodity <- GCAM_region_ID <- region <- value <- year <- GLU <- GLU_name <- GLU_code <-
      LandLeaf <- Land_Type <- LandNode <- LandNode1 <- LandNode2 <- LandNode3 <- UnmanagedLandLeaf <-
      logit.year.fillout <- logit.exponent <- logit.type <- soilTimeScale <- `mature age` <- mature.age <-
      soil_c <- veg_c <- LC_bm2 <- LV_milUSD75 <- LV_USD75_bm2 <- LV_USD75_m2 <- HarvCropLand_bm2 <-
      unManagedLandValue <- LandAllocatorRoot <- hist.veg.carbon.density <- hist.soil.carbon.density <-
      veg.carbon.density <- soil.carbon.density <- allocation <- Land_Type.y <- mature.age.year.fillout <-
      min.veg.carbon.density <- min.soil.carbon.density <- . <- NULL


    # 1. Process inputs

    # Replace GLU names and Add region names
    L181.LandShare_R_bio_GLU_irr %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) ->
      L181.LandShare_R_bio_GLU_irr

    L181.LC_bm2_R_C_Yh_GLU_irr_level %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) ->
      L181.LC_bm2_R_C_Yh_GLU_irr_level

    L181.YieldMult_R_bio_GLU_irr %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L181.YieldMult_R_bio_GLU_irr

    # Write a function to carry LN4 information down to LN5
    convert_LN4_to_LN5 <- function( data, names ){
      data %>%
        repeat_add_columns(tibble::tibble(level = c( "lo", "hi" ))) %>%
        mutate(LandNode5 = LandLeaf,
               LandLeaf = paste(data_new$LandNode5, data_new[[lvl]], sep = aglu.MGMT_DELIMITER)) ->
        data_new
      # data_new <- repeat_and_add_vector( data, lvl, c( "lo", "hi" ) )
      # data_new$LandNode5 <- data_new$LandLeaf
      # data_new$LandLeaf <- paste( data_new$LandNode5, data_new[[lvl]], sep = mgmt_delimiter )

      data_new <- data_new[names]
      return( data_new )
    }





    # The methods in this code file will be to start with existing (landnode4) tables, and add another level of detail
    # Some tables will require post-hoc adjustments, and for others the information simply needs to be passed down to another level


    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2252.LN5_Logit") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L181.LandShare_R_bio_GLU_irr",
                     "L181.LC_bm2_R_C_Yh_GLU_irr_level",
                     "L181.YieldMult_R_bio_GLU_irr",
                     "L2241.LN4_Logit",
                     "L2241.LN4_HistMgdAllocation_crop",
                     "L2241.LN4_MgdAllocation_crop",
                     "L2241.LN4_HistMgdAllocation_bio",
                     "L2241.LN4_MgdAllocation_bio",
                     "L2241.LN4_MgdCarbon_crop",
                     "L2241.LN4_MgdCarbon_bio",
                     "L2241.LN4_LeafGhostShare",
                     "L2012.AgProduction_ag_irr_mgmt") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2252.LN5_Logit

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2252.LN5_HistMgdAllocation_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L181.LandShare_R_bio_GLU_irr",
                     "L181.LC_bm2_R_C_Yh_GLU_irr_level",
                     "L181.YieldMult_R_bio_GLU_irr",
                     "L2241.LN4_Logit",
                     "L2241.LN4_HistMgdAllocation_crop",
                     "L2241.LN4_MgdAllocation_crop",
                     "L2241.LN4_HistMgdAllocation_bio",
                     "L2241.LN4_MgdAllocation_bio",
                     "L2241.LN4_MgdCarbon_crop",
                     "L2241.LN4_MgdCarbon_bio",
                     "L2241.LN4_LeafGhostShare",
                     "L2012.AgProduction_ag_irr_mgmt") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2252.LN5_HistMgdAllocation_crop

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2252.LN5_MgdAllocation_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L181.LandShare_R_bio_GLU_irr",
                     "L181.LC_bm2_R_C_Yh_GLU_irr_level",
                     "L181.YieldMult_R_bio_GLU_irr",
                     "L2241.LN4_Logit",
                     "L2241.LN4_HistMgdAllocation_crop",
                     "L2241.LN4_MgdAllocation_crop",
                     "L2241.LN4_HistMgdAllocation_bio",
                     "L2241.LN4_MgdAllocation_bio",
                     "L2241.LN4_MgdCarbon_crop",
                     "L2241.LN4_MgdCarbon_bio",
                     "L2241.LN4_LeafGhostShare",
                     "L2012.AgProduction_ag_irr_mgmt") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2252.LN5_MgdAllocation_crop

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2252.LN5_HistMgdAllocation_bio") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L181.LandShare_R_bio_GLU_irr",
                     "L181.LC_bm2_R_C_Yh_GLU_irr_level",
                     "L181.YieldMult_R_bio_GLU_irr",
                     "L2241.LN4_Logit",
                     "L2241.LN4_HistMgdAllocation_crop",
                     "L2241.LN4_MgdAllocation_crop",
                     "L2241.LN4_HistMgdAllocation_bio",
                     "L2241.LN4_MgdAllocation_bio",
                     "L2241.LN4_MgdCarbon_crop",
                     "L2241.LN4_MgdCarbon_bio",
                     "L2241.LN4_LeafGhostShare",
                     "L2012.AgProduction_ag_irr_mgmt") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2252.LN5_HistMgdAllocation_bio

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2252.LN5_MgdAllocation_bio") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L181.LandShare_R_bio_GLU_irr",
                     "L181.LC_bm2_R_C_Yh_GLU_irr_level",
                     "L181.YieldMult_R_bio_GLU_irr",
                     "L2241.LN4_Logit",
                     "L2241.LN4_HistMgdAllocation_crop",
                     "L2241.LN4_MgdAllocation_crop",
                     "L2241.LN4_HistMgdAllocation_bio",
                     "L2241.LN4_MgdAllocation_bio",
                     "L2241.LN4_MgdCarbon_crop",
                     "L2241.LN4_MgdCarbon_bio",
                     "L2241.LN4_LeafGhostShare",
                     "L2012.AgProduction_ag_irr_mgmt") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2252.LN5_MgdAllocation_bio

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2252.LN5_MgdCarbon_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L181.LandShare_R_bio_GLU_irr",
                     "L181.LC_bm2_R_C_Yh_GLU_irr_level",
                     "L181.YieldMult_R_bio_GLU_irr",
                     "L2241.LN4_Logit",
                     "L2241.LN4_HistMgdAllocation_crop",
                     "L2241.LN4_MgdAllocation_crop",
                     "L2241.LN4_HistMgdAllocation_bio",
                     "L2241.LN4_MgdAllocation_bio",
                     "L2241.LN4_MgdCarbon_crop",
                     "L2241.LN4_MgdCarbon_bio",
                     "L2241.LN4_LeafGhostShare",
                     "L2012.AgProduction_ag_irr_mgmt") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2252.LN5_MgdCarbon_crop

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2252.LN5_MgdCarbon_bio") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L181.LandShare_R_bio_GLU_irr",
                     "L181.LC_bm2_R_C_Yh_GLU_irr_level",
                     "L181.YieldMult_R_bio_GLU_irr",
                     "L2241.LN4_Logit",
                     "L2241.LN4_HistMgdAllocation_crop",
                     "L2241.LN4_MgdAllocation_crop",
                     "L2241.LN4_HistMgdAllocation_bio",
                     "L2241.LN4_MgdAllocation_bio",
                     "L2241.LN4_MgdCarbon_crop",
                     "L2241.LN4_MgdCarbon_bio",
                     "L2241.LN4_LeafGhostShare",
                     "L2012.AgProduction_ag_irr_mgmt") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2252.LN5_MgdCarbon_bio

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2252.LN5_LeafGhostShare") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L181.LandShare_R_bio_GLU_irr",
                     "L181.LC_bm2_R_C_Yh_GLU_irr_level",
                     "L181.YieldMult_R_bio_GLU_irr",
                     "L2241.LN4_Logit",
                     "L2241.LN4_HistMgdAllocation_crop",
                     "L2241.LN4_MgdAllocation_crop",
                     "L2241.LN4_HistMgdAllocation_bio",
                     "L2241.LN4_MgdAllocation_bio",
                     "L2241.LN4_MgdCarbon_crop",
                     "L2241.LN4_MgdCarbon_bio",
                     "L2241.LN4_LeafGhostShare",
                     "L2012.AgProduction_ag_irr_mgmt") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2252.LN5_LeafGhostShare

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2252.LN5_NodeGhostShare") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L181.LandShare_R_bio_GLU_irr",
                     "L181.LC_bm2_R_C_Yh_GLU_irr_level",
                     "L181.YieldMult_R_bio_GLU_irr",
                     "L2241.LN4_Logit",
                     "L2241.LN4_HistMgdAllocation_crop",
                     "L2241.LN4_MgdAllocation_crop",
                     "L2241.LN4_HistMgdAllocation_bio",
                     "L2241.LN4_MgdAllocation_bio",
                     "L2241.LN4_MgdCarbon_crop",
                     "L2241.LN4_MgdCarbon_bio",
                     "L2241.LN4_LeafGhostShare",
                     "L2012.AgProduction_ag_irr_mgmt") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2252.LN5_NodeGhostShare

    return_data(L2252.LN5_Logit, L2252.LN5_HistMgdAllocation_crop, L2252.LN5_MgdAllocation_crop, L2252.LN5_HistMgdAllocation_bio, L2252.LN5_MgdAllocation_bio, L2252.LN5_MgdCarbon_crop, L2252.LN5_MgdCarbon_bio, L2252.LN5_LeafGhostShare, L2252.LN5_NodeGhostShare)
  } else {
    stop("Unknown command")
  }
}
