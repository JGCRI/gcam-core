#' module_gcam.usa_LA1321.Cement
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1321.out_Mt_state_cement_Yh}, \code{L1321.IO_GJkg_state_cement_F_Yh}, \code{L1321.in_EJ_state_cement_F_Y}. The corresponding file in the
#' original data system was \code{LA1321.Cement.R} (gcam-usa level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_LA1321.Cement_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/Census_ind_VoS_state",
             FILE = "temp-data-inject/L1321.out_Mt_R_cement_Yh",
             FILE = "temp-data-inject/L1321.IO_GJkg_R_cement_F_Yh",
             FILE = "temp-data-inject/L1321.in_EJ_R_cement_F_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1321.out_Mt_state_cement_Yh",
             "L1321.IO_GJkg_state_cement_F_Yh",
             "L1321.in_EJ_state_cement_F_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    Census_ind_VoS_state <- get_data(all_data, "gcam-usa/Census_ind_VoS_state")
    L1321.out_Mt_R_cement_Yh <- get_data(all_data, "temp-data-inject/L1321.out_Mt_R_cement_Yh")
    L1321.IO_GJkg_R_cement_F_Yh <- get_data(all_data, "temp-data-inject/L1321.IO_GJkg_R_cement_F_Yh")
    L1321.in_EJ_R_cement_F_Y <- get_data(all_data, "temp-data-inject/L1321.in_EJ_R_cement_F_Y")

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
    #
    # NOTE: there are 'match' calls in this code. You probably want to use left_join_error_no_match
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # NOTE: This code uses repeat_and_add_vector
    # This function can be removed; see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
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
      add_legacy_name("L1321.out_Mt_state_cement_Yh") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1321.out_Mt_state_cement_Yh

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1321.IO_GJkg_state_cement_F_Yh") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1321.IO_GJkg_state_cement_F_Yh

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1321.in_EJ_state_cement_F_Y") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1321.in_EJ_state_cement_F_Y

    return_data(L1321.out_Mt_state_cement_Yh, L1321.IO_GJkg_state_cement_F_Yh, L1321.in_EJ_state_cement_F_Y)
  } else {
    stop("Unknown command")
  }
}
