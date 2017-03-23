#' module_aglu_L2011.ag_For_Past_bio_input_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2011.AgSectorLogitTables[[ curr_table ]]$data}, \code{L2011.AgSupplySector}, \code{L2011.AgSubsectorLogitTables[[ curr_table ]]$data}, \code{L2011.AgSupplySubsector}, \code{L2011.AgProduction_ag_irr}, \code{L2011.AgProduction_For}, \code{L2011.AgProduction_Past}, \code{L2011.AgHAtoCL_irr}, \code{L2011.AgYield_bio_grass_irr}, \code{L2011.AgYield_bio_tree_irr}. The corresponding file in the
#' original data system was \code{L2011.ag_For_Past_bio_input_irr.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2011.ag_For_Past_bio_input_irr_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "water/basin_to_country_mapping",
FILE = "aglu/A_agSupplySector",
 "L161.ag_irrProd_Mt_R_C_Y_GLU",
 "L161.ag_rfdProd_Mt_R_C_Y_GLU",
 "L163.ag_irrBioYield_GJm2_R_GLU",
 "L163.ag_rfdBioYield_GJm2_R_GLU",
 "L201.AgSupplySector",
 "L201.AgSupplySubsector",
 "L201.AgProduction_For",
 "L201.AgProduction_Past",
 "L201.AgHAtoCL",
 "L201.AgYield_bio_grass",
 "L201.AgYield_bio_tree"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L2011.AgSectorLogitTables[[ curr_table ]]$data",
XML = "L2011.AgSupplySector",
XML = "L2011.AgSubsectorLogitTables[[ curr_table ]]$data",
XML = "L2011.AgSupplySubsector",
XML = "L2011.AgProduction_ag_irr",
XML = "L2011.AgProduction_For",
XML = "L2011.AgProduction_Past",
XML = "L2011.AgHAtoCL_irr",
XML = "L2011.AgYield_bio_grass_irr",
XML = "L2011.AgYield_bio_tree_irr"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
  A_agSupplySector <- get_data(all_data, "aglu/A_agSupplySector")
  L161.ag_irrProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_irrProd_Mt_R_C_Y_GLU")
  L161.ag_rfdProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_rfdProd_Mt_R_C_Y_GLU")
  L163.ag_irrBioYield_GJm2_R_GLU <- get_data(all_data, "L163.ag_irrBioYield_GJm2_R_GLU")
  L163.ag_rfdBioYield_GJm2_R_GLU <- get_data(all_data, "L163.ag_rfdBioYield_GJm2_R_GLU")
  L201.AgSupplySector <- get_data(all_data, "L201.AgSupplySector")
  L201.AgSupplySubsector <- get_data(all_data, "L201.AgSupplySubsector")
  L201.AgProduction_For <- get_data(all_data, "L201.AgProduction_For")
  L201.AgProduction_Past <- get_data(all_data, "L201.AgProduction_Past")
  L201.AgHAtoCL <- get_data(all_data, "L201.AgHAtoCL")
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
 add_legacy_name("L2011.AgSectorLogitTables[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2011.AgSectorLogitTables[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2011.AgSupplySector") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2011.AgSupplySector
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2011.AgSubsectorLogitTables[[ curr_table ]]$data") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2011.AgSubsectorLogitTables[[ curr_table ]]$data
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2011.AgSupplySubsector") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2011.AgSupplySubsector
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2011.AgProduction_ag_irr") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2011.AgProduction_ag_irr
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2011.AgProduction_For") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2011.AgProduction_For
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2011.AgProduction_Past") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2011.AgProduction_Past
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2011.AgHAtoCL_irr") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2011.AgHAtoCL_irr
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2011.AgYield_bio_grass_irr") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2011.AgYield_bio_grass_irr
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2011.AgYield_bio_tree_irr") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2011.AgYield_bio_tree_irr

    return_data(L2011.AgSectorLogitTables[[ curr_table ]]$data, L2011.AgSupplySector, L2011.AgSubsectorLogitTables[[ curr_table ]]$data, L2011.AgSupplySubsector, L2011.AgProduction_ag_irr, L2011.AgProduction_For, L2011.AgProduction_Past, L2011.AgHAtoCL_irr, L2011.AgYield_bio_grass_irr, L2011.AgYield_bio_tree_irr)
  } else {
    stop("Unknown command")
  }
}



