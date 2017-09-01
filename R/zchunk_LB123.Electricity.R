#' module_gcam.usa_LB123.Electricity
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L123.in_EJ_state_elec_F}, \code{L123.out_EJ_state_elec_F}, \code{L123.in_EJ_state_ownuse_elec}, \code{L123.out_EJ_state_ownuse_elec}. The corresponding file in the
#' original data system was \code{LB123.Electricity.R} (gcam-usa level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_LB123.Electricity_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/NREL_us_re_technical_potential",
             "L123.in_EJ_R_elec_F_Yh",
             "L123.out_EJ_R_elec_F_Yh",
             FILE = "gcam-usa/EIA_elect_td_ownuse_prices",
             "L126.in_EJ_R_elecownuse_F_Yh",
             "L126.out_EJ_R_elecownuse_F_Yh",
             "L101.inEIA_EJ_state_S_F",
             "L132.out_EJ_state_indchp_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L123.in_EJ_state_elec_F",
             "L123.out_EJ_state_elec_F",
             "L123.in_EJ_state_ownuse_elec",
             "L123.out_EJ_state_ownuse_elec"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    NREL_us_re_technical_potential <- get_data(all_data, "gcam-usa/NREL_us_re_technical_potential")
    L123.in_EJ_R_elec_F_Yh <- get_data(all_data, "L123.in_EJ_R_elec_F_Yh")
    L123.out_EJ_R_elec_F_Yh <- get_data(all_data, "L123.out_EJ_R_elec_F_Yh")
    EIA_elect_td_ownuse_prices <- get_data(all_data, "gcam-usa/EIA_elect_td_ownuse_prices")
    L126.in_EJ_R_elecownuse_F_Yh <- get_data(all_data, "L126.in_EJ_R_elecownuse_F_Yh")
    L126.out_EJ_R_elecownuse_F_Yh <- get_data(all_data, "L126.out_EJ_R_elecownuse_F_Yh")
    L101.inEIA_EJ_state_S_F <- get_data(all_data, "L101.inEIA_EJ_state_S_F")
    L132.out_EJ_state_indchp_F <- get_data(all_data, "L132.out_EJ_state_indchp_F")

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
      add_legacy_name("L123.in_EJ_state_elec_F") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L123.in_EJ_state_elec_F

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L123.out_EJ_state_elec_F") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L123.out_EJ_state_elec_F

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L123.in_EJ_state_ownuse_elec") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L123.in_EJ_state_ownuse_elec

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L123.out_EJ_state_ownuse_elec") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L123.out_EJ_state_ownuse_elec

    return_data(L123.in_EJ_state_elec_F, L123.out_EJ_state_elec_F, L123.in_EJ_state_ownuse_elec, L123.out_EJ_state_ownuse_elec)
  } else {
    stop("Unknown command")
  }
}
