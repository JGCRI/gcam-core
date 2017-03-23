#' module_energy_LA1011.en_bal_adj
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1011.en_bal_EJ_R_Si_Fi_Yh}, \code{L1011.in_EJ_ctry_intlship_TOT_Yh}. The corresponding file in the
#' original data system was \code{LA1011.en_bal_adj.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_LA1011.en_bal_adj_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
FILE = "energy/calibrated_techs",
FILE = "energy/EIA_RFO_intlship_kbbld",
FILE = "energy/EIA_TOT_intlship_kbbld",
FILE = "energy/EIA_ctry",
FILE = "energy/A22.globaltech_coef",
 "L101.en_bal_EJ_R_Si_Fi_Yh_full"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1011.en_bal_EJ_R_Si_Fi_Yh",
"L1011.in_EJ_ctry_intlship_TOT_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
  calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
  EIA_RFO_intlship_kbbld <- get_data(all_data, "energy/EIA_RFO_intlship_kbbld")
  EIA_TOT_intlship_kbbld <- get_data(all_data, "energy/EIA_TOT_intlship_kbbld")
  EIA_ctry <- get_data(all_data, "energy/EIA_ctry")
  A22.globaltech_coef <- get_data(all_data, "energy/A22.globaltech_coef")
  L101.en_bal_EJ_R_Si_Fi_Yh_full <- get_data(all_data, "L101.en_bal_EJ_R_Si_Fi_Yh_full")

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
 add_legacy_name("L1011.en_bal_EJ_R_Si_Fi_Yh") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L1011.en_bal_EJ_R_Si_Fi_Yh
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L1011.in_EJ_ctry_intlship_TOT_Yh") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L1011.in_EJ_ctry_intlship_TOT_Yh

    return_data(L1011.en_bal_EJ_R_Si_Fi_Yh, L1011.in_EJ_ctry_intlship_TOT_Yh)
  } else {
    stop("Unknown command")
  }
}



