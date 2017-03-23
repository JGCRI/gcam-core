#' module_emissions_L232.prc_nonco2
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L232.nonco2_prc}, \code{L232.nonco2_max_reduction}, \code{L232.nonco2_steepness}. The corresponding file in the
#' original data system was \code{L232.prc_nonco2.R} (emissions level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L232.prc_nonco2_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "emissions/A_regions",
 "L131.nonco2_tg_R_prc_S_S_Yh",
FILE = "emissions/A32.max_reduction",
FILE = "emissions/A32.steepness"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L232.nonco2_prc",
XML = "L232.nonco2_max_reduction",
XML = "L232.nonco2_steepness"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  A_regions <- get_data(all_data, "emissions/A_regions")
  L131.nonco2_tg_R_prc_S_S_Yh <- get_data(all_data, "L131.nonco2_tg_R_prc_S_S_Yh")
  A32.max_reduction <- get_data(all_data, "emissions/A32.max_reduction")
  A32.steepness <- get_data(all_data, "emissions/A32.steepness")

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
 add_legacy_name("L232.nonco2_prc") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L232.nonco2_prc
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L232.nonco2_max_reduction") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L232.nonco2_max_reduction
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L232.nonco2_steepness") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L232.nonco2_steepness

    return_data(L232.nonco2_prc, L232.nonco2_max_reduction, L232.nonco2_steepness)
  } else {
    stop("Unknown command")
  }
}



