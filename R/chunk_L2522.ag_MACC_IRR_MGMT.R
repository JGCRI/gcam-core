#' module_emissions_L2522.ag_MACC_IRR_MGMT
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2522.MAC_an}, \code{L2522.AgMAC}, \code{L2522.MAC_Ag_TC_SSP1}, \code{L2522.MAC_An_TC_SSP1}, \code{L2522.MAC_Ag_TC_SSP2}, \code{L2522.MAC_An_TC_SSP2}, \code{L2522.MAC_Ag_TC_SSP5}, \code{L2522.MAC_An_TC_SSP5}. The corresponding file in the
#' original data system was \code{L2522.ag_MACC_IRR_MGMT.R} (emissions level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L2522.ag_MACC_IRR_MGMT_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/A_MACC_TechChange",
 "L2521.MAC_an",
 "L2521.AgMAC"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L2522.MAC_an",
XML = "L2522.AgMAC",
XML = "L2522.MAC_Ag_TC_SSP1",
XML = "L2522.MAC_An_TC_SSP1",
XML = "L2522.MAC_Ag_TC_SSP2",
XML = "L2522.MAC_An_TC_SSP2",
XML = "L2522.MAC_Ag_TC_SSP5",
XML = "L2522.MAC_An_TC_SSP5"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      A_MACC_TechChange <- get_data(all_data, "emissions/A_MACC_TechChange")
  L2521.MAC_an <- get_data(all_data, "L2521.MAC_an")
  L2521.AgMAC <- get_data(all_data, "L2521.AgMAC")

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
 add_legacy_name("L2522.MAC_an") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2522.MAC_an
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2522.AgMAC") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2522.AgMAC
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2522.MAC_Ag_TC_SSP1") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2522.MAC_Ag_TC_SSP1
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2522.MAC_An_TC_SSP1") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2522.MAC_An_TC_SSP1
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2522.MAC_Ag_TC_SSP2") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2522.MAC_Ag_TC_SSP2
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2522.MAC_An_TC_SSP2") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2522.MAC_An_TC_SSP2
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2522.MAC_Ag_TC_SSP5") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2522.MAC_Ag_TC_SSP5
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2522.MAC_An_TC_SSP5") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2522.MAC_An_TC_SSP5

    return_data(L2522.MAC_an, L2522.AgMAC, L2522.MAC_Ag_TC_SSP1, L2522.MAC_An_TC_SSP1, L2522.MAC_Ag_TC_SSP2, L2522.MAC_An_TC_SSP2, L2522.MAC_Ag_TC_SSP5, L2522.MAC_An_TC_SSP5)
  } else {
    stop("Unknown command")
  }
}



