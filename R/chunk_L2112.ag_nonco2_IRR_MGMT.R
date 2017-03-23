#' module_emissions_L2112.ag_nonco2_IRR_MGMT
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{object}, \code{L2112.AnEmissions}, \code{L2112.AnNH3Emissions}, \code{L2112.AWBEmissions}, \code{L2112.AGREmissions}. The corresponding file in the
#' original data system was \code{L2112.ag_nonco2_IRR_MGMT.R} (emissions level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L2112.ag_nonco2_IRR_MGMT_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "emissions/A_regions",
 "L2111.AWBEmissions",
 "L2111.AGREmissions",
 "L2111.AGRBio",
 "L2111.AWB_BCOC_EmissCoeff",
 "L2111.nonghg_max_reduction",
 "L2111.nonghg_steepness",
 "L2012.AgProduction_ag_irr_mgmt",
 "L2111.AnEmissions",
 "L2111.AnNH3Emissions"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "object",
XML = "L2112.AnEmissions",
XML = "L2112.AnNH3Emissions",
XML = "L2112.AWBEmissions",
XML = "L2112.AGREmissions"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  A_regions <- get_data(all_data, "emissions/A_regions")
  L2111.AWBEmissions <- get_data(all_data, "L2111.AWBEmissions")
  L2111.AGREmissions <- get_data(all_data, "L2111.AGREmissions")
  L2111.AGRBio <- get_data(all_data, "L2111.AGRBio")
  L2111.AWB_BCOC_EmissCoeff <- get_data(all_data, "L2111.AWB_BCOC_EmissCoeff")
  L2111.nonghg_max_reduction <- get_data(all_data, "L2111.nonghg_max_reduction")
  L2111.nonghg_steepness <- get_data(all_data, "L2111.nonghg_steepness")
  L2012.AgProduction_ag_irr_mgmt <- get_data(all_data, "L2012.AgProduction_ag_irr_mgmt")
  L2111.AnEmissions <- get_data(all_data, "L2111.AnEmissions")
  L2111.AnNH3Emissions <- get_data(all_data, "L2111.AnNH3Emissions")

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
 add_legacy_name("object") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   object
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2112.AnEmissions") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2112.AnEmissions
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2112.AnNH3Emissions") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2112.AnNH3Emissions
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2112.AWBEmissions") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2112.AWBEmissions
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2112.AGREmissions") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2112.AGREmissions

    return_data(object, L2112.AnEmissions, L2112.AnNH3Emissions, L2112.AWBEmissions, L2112.AGREmissions)
  } else {
    stop("Unknown command")
  }
}



