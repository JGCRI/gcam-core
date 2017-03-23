#' module_gcam.usa_L201.socioeconomics_USA
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.InterestRate}, \code{L201.Pop_GCAMUSA}, \code{L201.BaseGDP_GCAMUSA}, \code{L201.LaborForceFillout}, \code{L201.LaborProductivity_GCAMUSA}. The corresponding file in the
#' original data system was \code{L201.socioeconomics_USA.R} (gcam-usa level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_L201.socioeconomics_USA_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
 "L100.pcGDP_thous90usd_state",
 "L100.Pop_thous_state",
 "L100.GDP_mil90usd_state",
 "L102.pcgdp_thous90USD_GCAM3_ctry_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L201.InterestRate",
XML = "L201.Pop_GCAMUSA",
XML = "L201.BaseGDP_GCAMUSA",
XML = "L201.LaborForceFillout",
XML = "L201.LaborProductivity_GCAMUSA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
  L100.pcGDP_thous90usd_state <- get_data(all_data, "L100.pcGDP_thous90usd_state")
  L100.Pop_thous_state <- get_data(all_data, "L100.Pop_thous_state")
  L100.GDP_mil90usd_state <- get_data(all_data, "L100.GDP_mil90usd_state")
  L102.pcgdp_thous90USD_GCAM3_ctry_Y <- get_data(all_data, "L102.pcgdp_thous90USD_GCAM3_ctry_Y")

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
 add_legacy_name("L201.InterestRate") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L201.InterestRate
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L201.Pop_GCAMUSA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L201.Pop_GCAMUSA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L201.BaseGDP_GCAMUSA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L201.BaseGDP_GCAMUSA
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L201.LaborForceFillout") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L201.LaborForceFillout
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L201.LaborProductivity_GCAMUSA") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L201.LaborProductivity_GCAMUSA

    return_data(L201.InterestRate, L201.Pop_GCAMUSA, L201.BaseGDP_GCAMUSA, L201.LaborForceFillout, L201.LaborProductivity_GCAMUSA)
  } else {
    stop("Unknown command")
  }
}



