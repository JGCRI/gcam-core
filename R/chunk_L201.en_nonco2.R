#' module_emissions_L201.en_nonco2
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.en_pol_emissions}, \code{L201.en_ghg_emissions}, \code{L201.en_bcoc_emissions}, \code{L201.nonghg_max_reduction}, \code{L201.nonghg_steepness}, \code{L201.nonghg_max_reduction_res}, \code{L201.nonghg_steepness_res}, \code{L201.nonghg_res}, \code{L201.ghg_res}. The corresponding file in the
#' original data system was \code{L201.en_nonco2.R} (emissions level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L201.en_nonco2_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "emissions/A_regions",
FILE = "energy/A_regions",
 "L111.nonghg_tg_R_en_S_F_Yh",
 "L111.nonghg_tgej_R_en_S_F_Yh",
 "L112.ghg_tg_R_en_S_F_Yh",
 "L112.ghg_tgej_R_en_S_F_Yh",
 "L114.bcoc_tgej_R_en_S_F_2000",
 "L151.nonghg_ctrl_R_en_S_T",
FILE = "emissions/A51.steepness",
 "L244.DeleteThermalService"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "L201.en_pol_emissions",
XML = "L201.en_ghg_emissions",
XML = "L201.en_bcoc_emissions",
XML = "L201.nonghg_max_reduction",
XML = "L201.nonghg_steepness",
XML = "L201.nonghg_max_reduction_res",
XML = "L201.nonghg_steepness_res",
XML = "L201.nonghg_res",
XML = "L201.ghg_res"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  A_regions <- get_data(all_data, "emissions/A_regions")
  A_regions <- get_data(all_data, "energy/A_regions")
  L111.nonghg_tg_R_en_S_F_Yh <- get_data(all_data, "L111.nonghg_tg_R_en_S_F_Yh")
  L111.nonghg_tgej_R_en_S_F_Yh <- get_data(all_data, "L111.nonghg_tgej_R_en_S_F_Yh")
  L112.ghg_tg_R_en_S_F_Yh <- get_data(all_data, "L112.ghg_tg_R_en_S_F_Yh")
  L112.ghg_tgej_R_en_S_F_Yh <- get_data(all_data, "L112.ghg_tgej_R_en_S_F_Yh")
  L114.bcoc_tgej_R_en_S_F_2000 <- get_data(all_data, "L114.bcoc_tgej_R_en_S_F_2000")
  L151.nonghg_ctrl_R_en_S_T <- get_data(all_data, "L151.nonghg_ctrl_R_en_S_T")
  A51.steepness <- get_data(all_data, "emissions/A51.steepness")
  L244.DeleteThermalService <- get_data(all_data, "L244.DeleteThermalService")

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
 add_legacy_name("L201.en_pol_emissions") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L201.en_pol_emissions
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L201.en_ghg_emissions") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L201.en_ghg_emissions
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L201.en_bcoc_emissions") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L201.en_bcoc_emissions
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L201.nonghg_max_reduction") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L201.nonghg_max_reduction
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L201.nonghg_steepness") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L201.nonghg_steepness
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L201.nonghg_max_reduction_res") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L201.nonghg_max_reduction_res
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L201.nonghg_steepness_res") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L201.nonghg_steepness_res
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L201.nonghg_res") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L201.nonghg_res
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L201.ghg_res") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST) %>%
  add_xml_data() ->
   L201.ghg_res

    return_data(L201.en_pol_emissions, L201.en_ghg_emissions, L201.en_bcoc_emissions, L201.nonghg_max_reduction, L201.nonghg_steepness, L201.nonghg_max_reduction_res, L201.nonghg_steepness_res, L201.nonghg_res, L201.ghg_res)
  } else {
    stop("Unknown command")
  }
}



