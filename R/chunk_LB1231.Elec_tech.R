#' module_gcam.usa_LB1231.Elec_tech
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1231.in_EJ_state_elec_F_tech}, \code{L1231.out_EJ_state_elec_F_tech}. The corresponding file in the
#' original data system was \code{LB1231.Elec_tech.R} (gcam-usa level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_LB1231.Elec_tech_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L123.in_EJ_R_elec_F_Yh",
 "L123.out_EJ_R_elec_F_Yh",
 "L1231.in_EJ_R_elec_F_tech_Yh",
 "L1231.out_EJ_R_elec_F_tech_Yh",
 "L123.in_EJ_state_elec_F",
 "L123.out_EJ_state_elec_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1231.in_EJ_state_elec_F_tech",
"L1231.out_EJ_state_elec_F_tech"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      L123.in_EJ_R_elec_F_Yh <- get_data(all_data, "L123.in_EJ_R_elec_F_Yh")
  L123.out_EJ_R_elec_F_Yh <- get_data(all_data, "L123.out_EJ_R_elec_F_Yh")
  L1231.in_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.in_EJ_R_elec_F_tech_Yh")
  L1231.out_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.out_EJ_R_elec_F_tech_Yh")
  L123.in_EJ_state_elec_F <- get_data(all_data, "L123.in_EJ_state_elec_F")
  L123.out_EJ_state_elec_F <- get_data(all_data, "L123.out_EJ_state_elec_F")

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
 add_legacy_name("L1231.in_EJ_state_elec_F_tech") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L1231.in_EJ_state_elec_F_tech
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L1231.out_EJ_state_elec_F_tech") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L1231.out_EJ_state_elec_F_tech

    return_data(L1231.in_EJ_state_elec_F_tech, L1231.out_EJ_state_elec_F_tech)
  } else {
    stop("Unknown command")
  }
}



