#' module_aglu_L205.ag_prodchange_cost_input
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L205.AgProdChange_ag_ref}, \code{L205.AgProdChange_bio_ref}, \code{L205.AgCost_ag}, \code{L205.AgCost_bio}, \code{L205.AgCost_Past}, \code{L205.AgCost_For}, \code{L205.AgProdChange_high}, \code{L205.AgProdChange_ag_ref}, \code{L205.AgProdChange_low}, \code{L205.AgProdChange_ssp4}, \code{L205.AgProdChange_high}. The corresponding file in the
#' original data system was \code{L205.ag_prodchange_cost_input.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L205.ag_prodchange_cost_input_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "water/basin_to_country_mapping",
 "L103.ag_Prod_Mt_R_C_Y_GLU",
 "L112.ag_YieldRate_R_C_Y_GLU",
 "L112.bio_YieldRate_R_Y_GLU",
 "L113.ag_bioYield_GJm2_R_GLU",
 "L122.ag_EcYield_kgm2_R_C_Y_GLU",
 "L123.For_Yield_m3m2_R_GLU",
 "L123.LC_bm2_R_MgdPast_Yh_GLU",
 "L133.ag_Cost_75USDkg_C",
 "L201.AgYield_bio_grass",
 "L201.AgYield_bio_tree",
 "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L205.AgProdChange_ag_ref",
XML = "L205.AgProdChange_bio_ref",
XML = "L205.AgCost_ag",
XML = "L205.AgCost_bio",
XML = "L205.AgCost_Past",
XML = "L205.AgCost_For",
XML = "L205.AgProdChange_high",
XML = "L205.AgProdChange_ag_ref",
XML = "L205.AgProdChange_low",
XML = "L205.AgProdChange_ssp4",
XML = "L205.AgProdChange_high"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
  L103.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y_GLU")
  L112.ag_YieldRate_R_C_Y_GLU <- get_data(all_data, "L112.ag_YieldRate_R_C_Y_GLU")
  L112.bio_YieldRate_R_Y_GLU <- get_data(all_data, "L112.bio_YieldRate_R_Y_GLU")
  L113.ag_bioYield_GJm2_R_GLU <- get_data(all_data, "L113.ag_bioYield_GJm2_R_GLU")
  L122.ag_EcYield_kgm2_R_C_Y_GLU <- get_data(all_data, "L122.ag_EcYield_kgm2_R_C_Y_GLU")
  L123.For_Yield_m3m2_R_GLU <- get_data(all_data, "L123.For_Yield_m3m2_R_GLU")
  L123.LC_bm2_R_MgdPast_Yh_GLU <- get_data(all_data, "L123.LC_bm2_R_MgdPast_Yh_GLU")
  L133.ag_Cost_75USDkg_C <- get_data(all_data, "L133.ag_Cost_75USDkg_C")
  L201.AgYield_bio_grass <- get_data(all_data, "L201.AgYield_bio_grass")
  L201.AgYield_bio_tree <- get_data(all_data, "L201.AgYield_bio_tree")
  L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")

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
 add_legacy_name("L205.AgProdChange_ag_ref") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L205.AgProdChange_ag_ref
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L205.AgProdChange_bio_ref") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L205.AgProdChange_bio_ref
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L205.AgCost_ag") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L205.AgCost_ag
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L205.AgCost_bio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L205.AgCost_bio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L205.AgCost_Past") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L205.AgCost_Past
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L205.AgCost_For") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L205.AgCost_For
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L205.AgProdChange_high") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L205.AgProdChange_high
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L205.AgProdChange_ag_ref") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L205.AgProdChange_ag_ref
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L205.AgProdChange_low") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L205.AgProdChange_low
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L205.AgProdChange_ssp4") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L205.AgProdChange_ssp4
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L205.AgProdChange_high") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L205.AgProdChange_high

    return_data(L205.AgProdChange_ag_ref, L205.AgProdChange_bio_ref, L205.AgCost_ag, L205.AgCost_bio, L205.AgCost_Past, L205.AgCost_For, L205.AgProdChange_high, L205.AgProdChange_ag_ref, L205.AgProdChange_low, L205.AgProdChange_ssp4, L205.AgProdChange_high)
  } else {
    stop("Unknown command")
  }
}



