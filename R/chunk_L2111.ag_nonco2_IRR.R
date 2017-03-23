#' module_emissions_L2111.ag_nonco2_IRR
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2111.AWBEmissions}, \code{L2111.AGREmissions}, \code{L2111.AnEmissions}, \code{L2111.AnNH3Emissions}, \code{L2111.AGRBio}, \code{L2111.AWB_BCOC_EmissCoeff}, \code{L2111.nonghg_max_reduction}, \code{L2111.nonghg_steepness}. The corresponding file in the
#' original data system was \code{L2111.ag_nonco2_IRR.R} (emissions level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L2111.ag_nonco2_IRR_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "water/basin_to_country_mapping",
FILE = "emissions/A_regions",
 "L1211.nonco2_tg_R_awb_C_Y_GLU_IRR",
 "L1221.ghg_tg_R_agr_C_Y_GLU_IRR",
 "L211.AnEmissions",
 "L211.AnNH3Emissions",
 "L211.AGRBio",
 "L211.AWB_BCOC_EmissCoeff",
 "L211.nonghg_max_reduction",
 "L211.nonghg_steepness"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L2111.AWBEmissions",
XML = "L2111.AGREmissions",
XML = "L2111.AnEmissions",
XML = "L2111.AnNH3Emissions",
XML = "L2111.AGRBio",
XML = "L2111.AWB_BCOC_EmissCoeff",
XML = "L2111.nonghg_max_reduction",
XML = "L2111.nonghg_steepness"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
  A_regions <- get_data(all_data, "emissions/A_regions")
  L1211.nonco2_tg_R_awb_C_Y_GLU_IRR <- get_data(all_data, "L1211.nonco2_tg_R_awb_C_Y_GLU_IRR")
  L1221.ghg_tg_R_agr_C_Y_GLU_IRR <- get_data(all_data, "L1221.ghg_tg_R_agr_C_Y_GLU_IRR")
  L211.AnEmissions <- get_data(all_data, "L211.AnEmissions")
  L211.AnNH3Emissions <- get_data(all_data, "L211.AnNH3Emissions")
  L211.AGRBio <- get_data(all_data, "L211.AGRBio")
  L211.AWB_BCOC_EmissCoeff <- get_data(all_data, "L211.AWB_BCOC_EmissCoeff")
  L211.nonghg_max_reduction <- get_data(all_data, "L211.nonghg_max_reduction")
  L211.nonghg_steepness <- get_data(all_data, "L211.nonghg_steepness")

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
 add_legacy_name("L2111.AWBEmissions") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2111.AWBEmissions
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2111.AGREmissions") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2111.AGREmissions
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2111.AnEmissions") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2111.AnEmissions
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2111.AnNH3Emissions") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2111.AnNH3Emissions
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2111.AGRBio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2111.AGRBio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2111.AWB_BCOC_EmissCoeff") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2111.AWB_BCOC_EmissCoeff
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2111.nonghg_max_reduction") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2111.nonghg_max_reduction
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L2111.nonghg_steepness") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L2111.nonghg_steepness

    return_data(L2111.AWBEmissions, L2111.AGREmissions, L2111.AnEmissions, L2111.AnNH3Emissions, L2111.AGRBio, L2111.AWB_BCOC_EmissCoeff, L2111.nonghg_max_reduction, L2111.nonghg_steepness)
  } else {
    stop("Unknown command")
  }
}



