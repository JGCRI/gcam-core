#' module_aglu_L2231.land_input_3_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2231.LN3_LogitTables[[ curr_table_name ]]}, \code{L2231.LN3_Logit}, \code{L2231.LN3_HistUnmgdAllocation}, \code{L2231.LN3_UnmgdAllocation}, \code{L2231.NodeEquiv}, \code{L2231.LN3_NoEmissCarbon}, \code{L2231.LN3_NodeCarbon}, \code{L2231.LN3_HistMgdAllocation_noncrop}, \code{L2231.LN3_MgdAllocation_noncrop}, \code{L2231.LN3_UnmgdCarbon}, \code{L2231.LN3_MgdCarbon_noncrop}. The corresponding file in the
#' original data system was \code{L2231.land_input_3_irr.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2231.land_input_3_irr_DISABLED <- function(command, ...) {
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
             "L2012.AgYield_bio_ref"))
 #      "L223.LN3_Logit",
 # "L223.LN3_HistUnmgdAllocation",
 # "L223.LN3_UnmgdAllocation",
 # "L223.NodeEquiv",
 # "L223.LN3_NoEmissCarbon",
 # "L223.LN3_NodeCarbon",
 # "L223.LN3_HistMgdAllocation_noncrop",
 # "L223.LN3_MgdAllocation_noncrop",
 # "L223.LN3_UnmgdCarbon",
 # "L223.LN3_MgdCarbon_noncrop"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2231.LN3_LogitTables[[ curr_table_name ]]",
"L2231.LN3_Logit",
"L2231.LN3_HistUnmgdAllocation",
"L2231.LN3_UnmgdAllocation",
"L2231.NodeEquiv",
"L2231.LN3_NoEmissCarbon",
"L2231.LN3_NodeCarbon",
"L2231.LN3_HistMgdAllocation_noncrop",
"L2231.LN3_MgdAllocation_noncrop",
"L2231.LN3_UnmgdCarbon",
"L2231.LN3_MgdCarbon_noncrop"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    # L223.LN3_Logit <- get_data(all_data, "L223.LN3_Logit")
    # L223.LN3_HistUnmgdAllocation <- get_data(all_data, "L223.LN3_HistUnmgdAllocation")
    # L223.LN3_UnmgdAllocation <- get_data(all_data, "L223.LN3_UnmgdAllocation")
    # L223.NodeEquiv <- get_data(all_data, "L223.NodeEquiv")
    # L223.LN3_NoEmissCarbon <- get_data(all_data, "L223.LN3_NoEmissCarbon")
    # L223.LN3_NodeCarbon <- get_data(all_data, "L223.LN3_NodeCarbon")
    # L223.LN3_HistMgdAllocation_noncrop <- get_data(all_data, "L223.LN3_HistMgdAllocation_noncrop")
    # L223.LN3_MgdAllocation_noncrop <- get_data(all_data, "L223.LN3_MgdAllocation_noncrop")
    # L223.LN3_UnmgdCarbon <- get_data(all_data, "L223.LN3_UnmgdCarbon")
    # L223.LN3_MgdCarbon_noncrop <- get_data(all_data, "L223.LN3_MgdCarbon_noncrop")



    # Produce outputs
 #    tibble() %>%
 #   add_title("descriptive title of data") %>%
 # add_units("units") %>%
 # add_comments("comments describing how data generated") %>%
 # add_comments("can be multiple lines") %>%
 # add_legacy_name("L2231.LN3_LogitTables[[ curr_table_name ]]") %>%
 # add_precursors("precursor1", "precursor2", "etc") %>%
 # # typical flags, but there are others--see `constants.R`
 # add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
 #   L2231.LN3_LogitTables[[ curr_table_name ]]
tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.LN3_Logit") %>%
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
  # typical flags, but there are others--see `constant.R`
  add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
  L2231.LN3_Logit


tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.LN3_HistUnmgdAllocation") %>%
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
  L2231.LN3_HistUnmgdAllocation


tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.LN3_UnmgdAllocation") %>%
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
  L2231.LN3_UnmgdAllocation


tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.NodeEquiv") %>%
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
  L2231.NodeEquiv


tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.LN3_NoEmissCarbon") %>%
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
  L2231.LN3_NoEmissCarbon


tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.LN3_NodeCarbon") %>%
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
  L2231.LN3_NodeCarbon


tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.LN3_HistMgdAllocation_noncrop") %>%
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
  L2231.LN3_HistMgdAllocation_noncrop


tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.LN3_MgdAllocation_noncrop") %>%
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
  L2231.LN3_MgdAllocation_noncrop


tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.LN3_UnmgdCarbon") %>%
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
  L2231.LN3_UnmgdCarbon


tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.LN3_MgdCarbon_noncrop") %>%
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
  L2231.LN3_MgdCarbon_noncrop


    return_data(#L2231.LN3_LogitTables[[ curr_table_name ]],
                L2231.LN3_Logit, L2231.LN3_HistUnmgdAllocation, L2231.LN3_UnmgdAllocation, L2231.NodeEquiv, L2231.LN3_NoEmissCarbon, L2231.LN3_NodeCarbon, L2231.LN3_HistMgdAllocation_noncrop, L2231.LN3_MgdAllocation_noncrop, L2231.LN3_UnmgdCarbon, L2231.LN3_MgdCarbon_noncrop)
  } else {
    stop("Unknown command")
  }
}



