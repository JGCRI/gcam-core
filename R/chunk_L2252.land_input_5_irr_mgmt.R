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
module_aglu_L2252.land_input_5_irr_mgmt_DISABLED <- function(command, ...) {
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
    return(c(XML = "curr_table$data",
XML = "L2252.LN5_Logit",
XML = "L2252.LN5_HistMgdAllocation_crop",
XML = "L2252.LN5_MgdAllocation_crop",
XML = "L2252.LN5_HistMgdAllocation_bio",
XML = "L2252.LN5_MgdAllocation_bio",
XML = "L2252.LN5_MgdCarbon_crop",
XML = "L2252.LN5_MgdCarbon_bio",
XML = "L2252.LN5_LeafGhostShare",
XML = "L2252.LN5_NodeGhostShare"))
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
 add_legacy_name("L2252.LN5_Logit") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2252.LN5_Logit
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2252.LN5_HistMgdAllocation_crop") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2252.LN5_HistMgdAllocation_crop
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2252.LN5_MgdAllocation_crop") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2252.LN5_MgdAllocation_crop
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2252.LN5_HistMgdAllocation_bio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2252.LN5_HistMgdAllocation_bio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2252.LN5_MgdAllocation_bio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2252.LN5_MgdAllocation_bio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2252.LN5_MgdCarbon_crop") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2252.LN5_MgdCarbon_crop
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2252.LN5_MgdCarbon_bio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2252.LN5_MgdCarbon_bio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2252.LN5_LeafGhostShare") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2252.LN5_LeafGhostShare
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2252.LN5_NodeGhostShare") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2252.LN5_NodeGhostShare

    return_data(curr_table$data, L2252.LN5_Logit, L2252.LN5_HistMgdAllocation_crop, L2252.LN5_MgdAllocation_crop, L2252.LN5_HistMgdAllocation_bio, L2252.LN5_MgdAllocation_bio, L2252.LN5_MgdCarbon_crop, L2252.LN5_MgdCarbon_bio, L2252.LN5_LeafGhostShare, L2252.LN5_NodeGhostShare)
  } else {
    stop("Unknown command")
  }
}



