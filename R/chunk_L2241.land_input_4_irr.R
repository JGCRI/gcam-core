#' module_aglu_L2241.land_input_4_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{curr_table$data}, \code{L2241.LN4_Logit}, \code{L2241.LN4_HistMgdAllocation_crop}, \code{L2241.LN4_MgdAllocation_crop}, \code{L2241.LN4_HistMgdAllocation_bio}, \code{L2241.LN4_MgdAllocation_bio}, \code{L2241.LN4_MgdCarbon_crop}, \code{L2241.LN4_MgdCarbon_bio}, \code{L2241.LN4_LeafGhostShare}, \code{L2241.LN4_NodeGhostShare}, \code{L2241.LN4_NodeIsGhostShareRel}. The corresponding file in the
#' original data system was \code{L2241.land_input_4_irr.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2241.land_input_4_irr_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "water/basin_to_country_mapping",
FILE = "aglu/A_Fodderbio_chars",
FILE = "aglu/A_LandNode_logit_irr",
FILE = "aglu/A_LandNode4_irr",
FILE = "aglu/A_LandLeaf4_irr",
FILE = "aglu/A_bio_ghost_share",
FILE = "aglu/GCAMLandLeaf_CdensityLT",
 "L111.ag_resbio_R_C",
 "L121.CarbonContent_kgm2_R_LT_GLU",
 "L171.ag_irrEcYield_kgm2_R_C_Y_GLU",
 "L171.ag_rfdEcYield_kgm2_R_C_Y_GLU",
 "L223.LN3_HistMgdAllocation_bio",
 "L223.LN3_MgdAllocation_bio",
 "L223.LN3_MgdAllocation_crop",
 "L223.LN3_LeafGhostShare",
 "L223.LN3_LeafIsGhostShareRel",
 "L2011.AgYield_bio_grass_irr",
 "L2011.AgYield_bio_tree_irr"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "curr_table$data",
XML = "L2241.LN4_Logit",
XML = "L2241.LN4_HistMgdAllocation_crop",
XML = "L2241.LN4_MgdAllocation_crop",
XML = "L2241.LN4_HistMgdAllocation_bio",
XML = "L2241.LN4_MgdAllocation_bio",
XML = "L2241.LN4_MgdCarbon_crop",
XML = "L2241.LN4_MgdCarbon_bio",
XML = "L2241.LN4_LeafGhostShare",
XML = "L2241.LN4_NodeGhostShare",
XML = "L2241.LN4_NodeIsGhostShareRel"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
  A_Fodderbio_chars <- get_data(all_data, "aglu/A_Fodderbio_chars")
  A_LandNode_logit_irr <- get_data(all_data, "aglu/A_LandNode_logit_irr")
  A_LandNode4_irr <- get_data(all_data, "aglu/A_LandNode4_irr")
  A_LandLeaf4_irr <- get_data(all_data, "aglu/A_LandLeaf4_irr")
  A_bio_ghost_share <- get_data(all_data, "aglu/A_bio_ghost_share")
  GCAMLandLeaf_CdensityLT <- get_data(all_data, "aglu/GCAMLandLeaf_CdensityLT")
  L111.ag_resbio_R_C <- get_data(all_data, "L111.ag_resbio_R_C")
  L121.CarbonContent_kgm2_R_LT_GLU <- get_data(all_data, "L121.CarbonContent_kgm2_R_LT_GLU")
  L171.ag_irrEcYield_kgm2_R_C_Y_GLU <- get_data(all_data, "L171.ag_irrEcYield_kgm2_R_C_Y_GLU")
  L171.ag_rfdEcYield_kgm2_R_C_Y_GLU <- get_data(all_data, "L171.ag_rfdEcYield_kgm2_R_C_Y_GLU")
  L223.LN3_HistMgdAllocation_bio <- get_data(all_data, "L223.LN3_HistMgdAllocation_bio")
  L223.LN3_MgdAllocation_bio <- get_data(all_data, "L223.LN3_MgdAllocation_bio")
  L223.LN3_MgdAllocation_crop <- get_data(all_data, "L223.LN3_MgdAllocation_crop")
  L223.LN3_LeafGhostShare <- get_data(all_data, "L223.LN3_LeafGhostShare")
  L223.LN3_LeafIsGhostShareRel <- get_data(all_data, "L223.LN3_LeafIsGhostShareRel")
  L2011.AgYield_bio_grass_irr <- get_data(all_data, "L2011.AgYield_bio_grass_irr")
  L2011.AgYield_bio_tree_irr <- get_data(all_data, "L2011.AgYield_bio_tree_irr")

    # ===================================================
    # TRANSLATED PROCESSING CODE GOES HERE...
    #
    # If you find a mistake/thing to update in the old code and
    # fixing it will change the output data, causing the tests to fail,
    # (i) open an issue on GitHub, (ii) consult with colleagues, and
    # then (iii) code a fix:
    #
    # if(OLD_DATA_SYSTEM_BEHAVIOR) {
    #   ... code that replicates old, incorrect behavior
    # } else {
    #   ... new code with a fix
    # }
    #
    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("curr_table$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   curr_table$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2241.LN4_Logit") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2241.LN4_Logit
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2241.LN4_HistMgdAllocation_crop") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2241.LN4_HistMgdAllocation_crop
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2241.LN4_MgdAllocation_crop") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2241.LN4_MgdAllocation_crop
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2241.LN4_HistMgdAllocation_bio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2241.LN4_HistMgdAllocation_bio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2241.LN4_MgdAllocation_bio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2241.LN4_MgdAllocation_bio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2241.LN4_MgdCarbon_crop") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2241.LN4_MgdCarbon_crop
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2241.LN4_MgdCarbon_bio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2241.LN4_MgdCarbon_bio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2241.LN4_LeafGhostShare") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2241.LN4_LeafGhostShare
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2241.LN4_NodeGhostShare") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2241.LN4_NodeGhostShare
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2241.LN4_NodeIsGhostShareRel") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2241.LN4_NodeIsGhostShareRel

    return_data(curr_table$data, L2241.LN4_Logit, L2241.LN4_HistMgdAllocation_crop, L2241.LN4_MgdAllocation_crop, L2241.LN4_HistMgdAllocation_bio, L2241.LN4_MgdAllocation_bio, L2241.LN4_MgdCarbon_crop, L2241.LN4_MgdCarbon_bio, L2241.LN4_LeafGhostShare, L2241.LN4_NodeGhostShare, L2241.LN4_NodeIsGhostShareRel)
  } else {
    stop("Unknown command")
  }
}



