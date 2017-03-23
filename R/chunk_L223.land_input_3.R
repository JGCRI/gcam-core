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
module_aglu_L223.land_input_3_DISABLED <- function(command, ...) {
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
 "L201.AgYield_bio_grass",
 "L201.AgYield_bio_tree"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "curr_table$data",
XML = "L223.LN3_Logit",
XML = "L223.LN3_LeafGhostShare",
XML = "L223.LN3_LeafIsGhostShareRel",
XML = "L223.LN3_HistUnmgdAllocation",
XML = "L223.LN3_UnmgdAllocation",
XML = "L223.NodeEquiv",
XML = "L223.LN3_NoEmissCarbon",
XML = "L223.LN3_NodeCarbon",
XML = "L223.LN3_HistMgdAllocation_noncrop",
XML = "L223.LN3_MgdAllocation_noncrop",
XML = "L223.LN3_HistMgdAllocation_crop",
XML = "L223.LN3_MgdAllocation_crop",
XML = "L223.LN3_HistMgdAllocation_bio",
XML = "L223.LN3_MgdAllocation_bio",
XML = "L223.LN3_UnmgdCarbon",
XML = "L223.LN3_MgdCarbon_noncrop",
XML = "L223.LN3_MgdCarbon_crop",
XML = "L223.LN3_MgdCarbon_bio",
XML = "curr_table$data",
XML = "L223.LN1_Logit_prot",
XML = "L223.LN3_HistUnmgdAllocation_noprot",
XML = "L223.LN3_UnmgdAllocation_noprot",
XML = "L223.LN1_HistUnmgdAllocation_prot",
XML = "L223.LN1_UnmgdAllocation_prot",
XML = "L223.LN1_UnmgdCarbon_prot"))
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
  L201.AgYield_bio_grass <- get_data(all_data, "L201.AgYield_bio_grass")
  L201.AgYield_bio_tree <- get_data(all_data, "L201.AgYield_bio_tree")

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
 add_legacy_name("L223.LN3_Logit") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN3_Logit
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN3_LeafGhostShare") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN3_LeafGhostShare
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN3_LeafIsGhostShareRel") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN3_LeafIsGhostShareRel
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN3_HistUnmgdAllocation") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN3_HistUnmgdAllocation
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN3_UnmgdAllocation") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN3_UnmgdAllocation
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.NodeEquiv") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.NodeEquiv
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN3_NoEmissCarbon") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN3_NoEmissCarbon
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN3_NodeCarbon") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN3_NodeCarbon
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN3_HistMgdAllocation_noncrop") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN3_HistMgdAllocation_noncrop
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN3_MgdAllocation_noncrop") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN3_MgdAllocation_noncrop
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN3_HistMgdAllocation_crop") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN3_HistMgdAllocation_crop
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN3_MgdAllocation_crop") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN3_MgdAllocation_crop
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN3_HistMgdAllocation_bio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN3_HistMgdAllocation_bio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN3_MgdAllocation_bio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN3_MgdAllocation_bio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN3_UnmgdCarbon") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN3_UnmgdCarbon
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN3_MgdCarbon_noncrop") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN3_MgdCarbon_noncrop
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN3_MgdCarbon_crop") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN3_MgdCarbon_crop
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN3_MgdCarbon_bio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN3_MgdCarbon_bio
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
 add_legacy_name("L223.LN1_Logit_prot") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN1_Logit_prot
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN3_HistUnmgdAllocation_noprot") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN3_HistUnmgdAllocation_noprot
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN3_UnmgdAllocation_noprot") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN3_UnmgdAllocation_noprot
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN1_HistUnmgdAllocation_prot") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN1_HistUnmgdAllocation_prot
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN1_UnmgdAllocation_prot") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN1_UnmgdAllocation_prot
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L223.LN1_UnmgdCarbon_prot") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L223.LN1_UnmgdCarbon_prot

    return_data(curr_table$data, L223.LN3_Logit, L223.LN3_LeafGhostShare, L223.LN3_LeafIsGhostShareRel, L223.LN3_HistUnmgdAllocation, L223.LN3_UnmgdAllocation, L223.NodeEquiv, L223.LN3_NoEmissCarbon, L223.LN3_NodeCarbon, L223.LN3_HistMgdAllocation_noncrop, L223.LN3_MgdAllocation_noncrop, L223.LN3_HistMgdAllocation_crop, L223.LN3_MgdAllocation_crop, L223.LN3_HistMgdAllocation_bio, L223.LN3_MgdAllocation_bio, L223.LN3_UnmgdCarbon, L223.LN3_MgdCarbon_noncrop, L223.LN3_MgdCarbon_crop, L223.LN3_MgdCarbon_bio, curr_table$data, L223.LN1_Logit_prot, L223.LN3_HistUnmgdAllocation_noprot, L223.LN3_UnmgdAllocation_noprot, L223.LN1_HistUnmgdAllocation_prot, L223.LN1_UnmgdAllocation_prot, L223.LN1_UnmgdCarbon_prot)
  } else {
    stop("Unknown command")
  }
}



