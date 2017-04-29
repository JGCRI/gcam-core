#' module_water_L1233.Elec_water
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1233.out_EJ_R_elec_F_tech_Yh_cool}, \code{L1233.in_EJ_R_elec_F_tech_Yh_cool}, \code{L1233.wdraw_km3_R_elec}, \code{L1233.wcons_km3_R_elec}, \code{L1233.shrwt_R_elec_cool_Yf}. The corresponding file in the
#' original data system was \code{L1233.Elec_water.R} (water level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_water_L1233.Elec_water_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/calibrated_techs",
             FILE = "energy/enduse_fuel_aggregation",
             "L101.en_bal_EJ_ctry_Si_Fi_Yh_full",
             "L1231.in_EJ_R_elec_F_tech_Yh",
             "L1231.out_EJ_R_elec_F_tech_Yh",
             FILE = "water/A23.CoolingSystemShares_RG3",
             FILE = "water/elec_tech_water_map",
             FILE = "water/Macknick_elec_water_m3MWh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1233.out_EJ_R_elec_F_tech_Yh_cool",
             "L1233.in_EJ_R_elec_F_tech_Yh_cool",
             "L1233.wdraw_km3_R_elec",
             "L1233.wcons_km3_R_elec",
             "L1233.shrwt_R_elec_cool_Yf"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    enduse_fuel_aggregation <- get_data(all_data, "energy/enduse_fuel_aggregation")
    L101.en_bal_EJ_ctry_Si_Fi_Yh_full <- get_data(all_data, "L101.en_bal_EJ_ctry_Si_Fi_Yh_full")
    L1231.in_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.in_EJ_R_elec_F_tech_Yh")
    L1231.out_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.out_EJ_R_elec_F_tech_Yh")
    A23.CoolingSystemShares_RG3 <- get_data(all_data, "water/A23.CoolingSystemShares_RG3")
    elec_tech_water_map <- get_data(all_data, "water/elec_tech_water_map")
    Macknick_elec_water_m3MWh <- get_data(all_data, "water/Macknick_elec_water_m3MWh")

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
      add_legacy_name("L1233.out_EJ_R_elec_F_tech_Yh_cool") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1233.out_EJ_R_elec_F_tech_Yh_cool
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1233.in_EJ_R_elec_F_tech_Yh_cool") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1233.in_EJ_R_elec_F_tech_Yh_cool
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1233.wdraw_km3_R_elec") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1233.wdraw_km3_R_elec
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1233.wcons_km3_R_elec") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1233.wcons_km3_R_elec
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1233.shrwt_R_elec_cool_Yf") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1233.shrwt_R_elec_cool_Yf

    return_data(L1233.out_EJ_R_elec_F_tech_Yh_cool, L1233.in_EJ_R_elec_F_tech_Yh_cool, L1233.wdraw_km3_R_elec, L1233.wcons_km3_R_elec, L1233.shrwt_R_elec_cool_Yf)
  } else {
    stop("Unknown command")
  }
}
