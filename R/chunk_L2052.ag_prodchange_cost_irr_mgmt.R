#' module_aglu_L2052.ag_prodchange_cost_irr_mgmt
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2052.AgCost_ag_irr_mgmt}, \code{L2052.AgCost_bio_irr_mgmt}, \code{L2052.AgCost_For}, \code{L2052.AgProdChange_ag_irr_ref}, \code{L2052.AgProdChange_bio_irr_ref}, \code{L2052.AgProdChange_irr_high}, \code{L2052.AgProdChange_ag_irr_ref}, \code{L2052.AgProdChange_irr_low}, \code{L2052.AgProdChange_irr_ssp4}, \code{L2052.AgProdChange_irr_high}. The corresponding file in the
#' original data system was \code{L2052.ag_prodchange_cost_irr_mgmt.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2052.ag_prodchange_cost_irr_mgmt_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L2051.AgCost_ag_irr",
 "L2051.AgCost_bio_irr",
 "L2051.AgCost_For",
 "i"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L2052.AgCost_ag_irr_mgmt",
XML = "L2052.AgCost_bio_irr_mgmt",
XML = "L2052.AgCost_For",
XML = "L2052.AgProdChange_ag_irr_ref",
XML = "L2052.AgProdChange_bio_irr_ref",
XML = "L2052.AgProdChange_irr_high",
XML = "L2052.AgProdChange_ag_irr_ref",
XML = "L2052.AgProdChange_irr_low",
XML = "L2052.AgProdChange_irr_ssp4",
XML = "L2052.AgProdChange_irr_high"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      L2051.AgCost_ag_irr <- get_data(all_data, "L2051.AgCost_ag_irr")
  L2051.AgCost_bio_irr <- get_data(all_data, "L2051.AgCost_bio_irr")
  L2051.AgCost_For <- get_data(all_data, "L2051.AgCost_For")
  i <- get_data(all_data, "i")

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
 add_legacy_name("L2052.AgCost_ag_irr_mgmt") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2052.AgCost_ag_irr_mgmt
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2052.AgCost_bio_irr_mgmt") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2052.AgCost_bio_irr_mgmt
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2052.AgCost_For") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2052.AgCost_For
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2052.AgProdChange_ag_irr_ref") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2052.AgProdChange_ag_irr_ref
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2052.AgProdChange_bio_irr_ref") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2052.AgProdChange_bio_irr_ref
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2052.AgProdChange_irr_high") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2052.AgProdChange_irr_high
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2052.AgProdChange_ag_irr_ref") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2052.AgProdChange_ag_irr_ref
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2052.AgProdChange_irr_low") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2052.AgProdChange_irr_low
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2052.AgProdChange_irr_ssp4") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2052.AgProdChange_irr_ssp4
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2052.AgProdChange_irr_high") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2052.AgProdChange_irr_high

    return_data(L2052.AgCost_ag_irr_mgmt, L2052.AgCost_bio_irr_mgmt, L2052.AgCost_For, L2052.AgProdChange_ag_irr_ref, L2052.AgProdChange_bio_irr_ref, L2052.AgProdChange_irr_high, L2052.AgProdChange_ag_irr_ref, L2052.AgProdChange_irr_low, L2052.AgProdChange_irr_ssp4, L2052.AgProdChange_irr_high)
  } else {
    stop("Unknown command")
  }
}



