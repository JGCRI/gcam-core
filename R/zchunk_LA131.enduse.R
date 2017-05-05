#' module_energy_LA131.enduse
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L131.in_EJ_R_Senduse_F_Yh}, \code{L131.share_R_Senduse_heat_Yh}. The corresponding file in the
#' original data system was \code{LA131.enduse.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_LA131.enduse_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A_regions",
             FILE = "energy/enduse_sector_aggregation",
             "L1011.en_bal_EJ_R_Si_Fi_Yh",
             "L121.in_EJ_R_unoil_F_Yh",
             "L122.in_EJ_R_refining_F_Yh",
             "L124.in_EJ_R_heat_F_Yh",
             "L124.out_EJ_R_heat_F_Yh",
             "L124.out_EJ_R_heatfromelec_F_Yh",
             "L126.out_EJ_R_electd_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L131.in_EJ_R_Senduse_F_Yh",
             "L131.share_R_Senduse_heat_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_regions <- get_data(all_data, "energy/A_regions")
    enduse_sector_aggregation <- get_data(all_data, "energy/enduse_sector_aggregation")
    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1011.en_bal_EJ_R_Si_Fi_Yh")
    L121.in_EJ_R_unoil_F_Yh <- get_data(all_data, "L121.in_EJ_R_unoil_F_Yh")
    L122.in_EJ_R_refining_F_Yh <- get_data(all_data, "L122.in_EJ_R_refining_F_Yh")
    L124.in_EJ_R_heat_F_Yh <- get_data(all_data, "L124.in_EJ_R_heat_F_Yh")
    L124.out_EJ_R_heat_F_Yh <- get_data(all_data, "L124.out_EJ_R_heat_F_Yh")
    L124.out_EJ_R_heatfromelec_F_Yh <- get_data(all_data, "L124.out_EJ_R_heatfromelec_F_Yh")
    L126.out_EJ_R_electd_F_Yh <- get_data(all_data, "L126.out_EJ_R_electd_F_Yh")

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
    # NOTE: This code uses vecpaste
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
      add_legacy_name("L131.in_EJ_R_Senduse_F_Yh") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L131.in_EJ_R_Senduse_F_Yh
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L131.share_R_Senduse_heat_Yh") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L131.share_R_Senduse_heat_Yh

    return_data(L131.in_EJ_R_Senduse_F_Yh, L131.share_R_Senduse_heat_Yh)
  } else {
    stop("Unknown command")
  }
}
