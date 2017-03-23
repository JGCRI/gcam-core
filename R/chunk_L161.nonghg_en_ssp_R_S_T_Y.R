#' module_emissions_L161.nonghg_en_ssp_R_S_T_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L161.SSP15_EF}, \code{L161.SSP2_EF}, \code{L161.SSP34_EF}. The corresponding file in the
#' original data system was \code{L161.nonghg_en_ssp_R_S_T_Y.R} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L161.nonghg_en_ssp_R_S_T_Y_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/A_regions",
FILE = "emissions/GCAM_sector_tech",
FILE = "emissions/gains_to_gcam_sector",
FILE = "emissions/GAINS_activities",
FILE = "emissions/GAINS_emissions",
 "L102.pcgdp_thous90USD_Scen_R_Y",
 "L111.nonghg_tgej_R_en_S_F_Yh",
 "L114.bcoc_tgej_R_en_S_F_2000"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L161.SSP15_EF",
"L161.SSP2_EF",
"L161.SSP34_EF"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      A_regions <- get_data(all_data, "emissions/A_regions")
  GCAM_sector_tech <- get_data(all_data, "emissions/GCAM_sector_tech")
  gains_to_gcam_sector <- get_data(all_data, "emissions/gains_to_gcam_sector")
  GAINS_activities <- get_data(all_data, "emissions/GAINS_activities")
  GAINS_emissions <- get_data(all_data, "emissions/GAINS_emissions")
  L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")
  L111.nonghg_tgej_R_en_S_F_Yh <- get_data(all_data, "L111.nonghg_tgej_R_en_S_F_Yh")
  L114.bcoc_tgej_R_en_S_F_2000 <- get_data(all_data, "L114.bcoc_tgej_R_en_S_F_2000")

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
 add_legacy_name("L161.SSP15_EF") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L161.SSP15_EF
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L161.SSP2_EF") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L161.SSP2_EF
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L161.SSP34_EF") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L161.SSP34_EF

    return_data(L161.SSP15_EF, L161.SSP2_EF, L161.SSP34_EF)
  } else {
    stop("Unknown command")
  }
}



