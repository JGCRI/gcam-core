#' module_gcam.usa_LA154.Transport
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L154.in_EJ_state_trn_m_sz_tech_F}, \code{L154.out_mpkm_state_trn_nonmotor_Yh}, \code{L154.in_EJ_state_trn_F}. The corresponding file in the
#' original data system was \code{LA154.Transport.R} (gcam-usa level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_LA154.Transport_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/trnUCD_EIA_mapping",
 "L154.in_EJ_R_trn_m_sz_tech_F_Yh",
 "L154.out_mpkm_R_trn_nonmotor_Yh",
 "L100.Pop_thous_state",
 "L101.inEIA_EJ_state_S_F",
 "L101.EIA_use_all_Bbtu"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L154.in_EJ_state_trn_m_sz_tech_F",
"L154.out_mpkm_state_trn_nonmotor_Yh",
"L154.in_EJ_state_trn_F"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      trnUCD_EIA_mapping <- get_data(all_data, "gcam-usa/trnUCD_EIA_mapping")
  L154.in_EJ_R_trn_m_sz_tech_F_Yh <- get_data(all_data, "L154.in_EJ_R_trn_m_sz_tech_F_Yh")
  L154.out_mpkm_R_trn_nonmotor_Yh <- get_data(all_data, "L154.out_mpkm_R_trn_nonmotor_Yh")
  L100.Pop_thous_state <- get_data(all_data, "L100.Pop_thous_state")
  L101.inEIA_EJ_state_S_F <- get_data(all_data, "L101.inEIA_EJ_state_S_F")
  L101.EIA_use_all_Bbtu <- get_data(all_data, "L101.EIA_use_all_Bbtu")

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
 add_legacy_name("L154.in_EJ_state_trn_m_sz_tech_F") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L154.in_EJ_state_trn_m_sz_tech_F
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L154.out_mpkm_state_trn_nonmotor_Yh") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L154.out_mpkm_state_trn_nonmotor_Yh
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L154.in_EJ_state_trn_F") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L154.in_EJ_state_trn_F

    return_data(L154.in_EJ_state_trn_m_sz_tech_F, L154.out_mpkm_state_trn_nonmotor_Yh, L154.in_EJ_state_trn_F)
  } else {
    stop("Unknown command")
  }
}



