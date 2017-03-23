#' module_emissions_L211.ag_nonco2
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L211.AWBEmissions}, \code{L211.AGREmissions}, \code{L211.AnEmissions}, \code{L211.AnNH3Emissions}, \code{L211.AGRBio}, \code{L211.AWB_BCOC_EmissCoeff}, \code{L211.nonghg_max_reduction}, \code{L211.nonghg_steepness}. The corresponding file in the
#' original data system was \code{L211.ag_nonco2.R} (emissions level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L211.ag_nonco2_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "water/basin_to_country_mapping",
FILE = "emissions/A_regions",
FILE = "aglu/A_agSupplySector",
 "L103.ag_Prod_Mt_R_C_Y_GLU",
 "L205.AgCost_bio",
 "L113.ghg_tg_R_an_C_Sys_Fd_Yh",
 "L115.nh3_tg_R_an_C_Sys_Fd_Yh",
 "L121.nonco2_tg_R_awb_C_Y_GLU",
 "L122.ghg_tg_R_agr_C_Y_GLU",
 "L123.bcoc_tgmt_R_awb_2000",
FILE = "emissions/A11.max_reduction",
FILE = "emissions/A11.steepness"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L211.AWBEmissions",
XML = "L211.AGREmissions",
XML = "L211.AnEmissions",
XML = "L211.AnNH3Emissions",
XML = "L211.AGRBio",
XML = "L211.AWB_BCOC_EmissCoeff",
XML = "L211.nonghg_max_reduction",
XML = "L211.nonghg_steepness"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
  A_regions <- get_data(all_data, "emissions/A_regions")
  A_agSupplySector <- get_data(all_data, "aglu/A_agSupplySector")
  L103.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y_GLU")
  L205.AgCost_bio <- get_data(all_data, "L205.AgCost_bio")
  L113.ghg_tg_R_an_C_Sys_Fd_Yh <- get_data(all_data, "L113.ghg_tg_R_an_C_Sys_Fd_Yh")
  L115.nh3_tg_R_an_C_Sys_Fd_Yh <- get_data(all_data, "L115.nh3_tg_R_an_C_Sys_Fd_Yh")
  L121.nonco2_tg_R_awb_C_Y_GLU <- get_data(all_data, "L121.nonco2_tg_R_awb_C_Y_GLU")
  L122.ghg_tg_R_agr_C_Y_GLU <- get_data(all_data, "L122.ghg_tg_R_agr_C_Y_GLU")
  L123.bcoc_tgmt_R_awb_2000 <- get_data(all_data, "L123.bcoc_tgmt_R_awb_2000")
  A11.max_reduction <- get_data(all_data, "emissions/A11.max_reduction")
  A11.steepness <- get_data(all_data, "emissions/A11.steepness")

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
 add_legacy_name("L211.AWBEmissions") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L211.AWBEmissions
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L211.AGREmissions") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L211.AGREmissions
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L211.AnEmissions") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L211.AnEmissions
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L211.AnNH3Emissions") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L211.AnNH3Emissions
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L211.AGRBio") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L211.AGRBio
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L211.AWB_BCOC_EmissCoeff") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L211.AWB_BCOC_EmissCoeff
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L211.nonghg_max_reduction") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L211.nonghg_max_reduction
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L211.nonghg_steepness") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L211.nonghg_steepness

    return_data(L211.AWBEmissions, L211.AGREmissions, L211.AnEmissions, L211.AnNH3Emissions, L211.AGRBio, L211.AWB_BCOC_EmissCoeff, L211.nonghg_max_reduction, L211.nonghg_steepness)
  } else {
    stop("Unknown command")
  }
}



