#' module_energy_LB1322.Fert
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1322.Fert_Prod_MtN_R_F_Y}, \code{L1322.IO_R_Fert_F_Yh}, \code{L1322.in_EJ_R_indenergy_F_Yh}, \code{L1322.in_EJ_R_indfeed_F_Yh}, \code{L1322.Fert_NEcost_75USDkgN_F}. The corresponding file in the
#' original data system was \code{LB1322.Fert.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_LB1322.Fert_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/IEA_ctry",
             FILE = "energy/IEA_Fert_fuel_data",
             FILE = "energy/H2A_Prod_Tech",
             "L142.ag_Fert_Prod_MtN_ctry_Y",
             FILE = "energy/A10.rsrc_info",
             FILE = "energy/A21.globaltech_cost",
             FILE = "energy/A22.globaltech_cost",
             "L1321.in_EJ_R_indenergy_F_Yh",
             "L132.in_EJ_R_indfeed_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1322.Fert_Prod_MtN_R_F_Y",
             "L1322.IO_R_Fert_F_Yh",
             "L1322.in_EJ_R_indenergy_F_Yh",
             "L1322.in_EJ_R_indfeed_F_Yh",
             "L1322.Fert_NEcost_75USDkgN_F"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    IEA_ctry <- get_data(all_data, "energy/IEA_ctry")
    IEA_Fert_fuel_data <- get_data(all_data, "energy/IEA_Fert_fuel_data")
    H2A_Prod_Tech <- get_data(all_data, "energy/H2A_Prod_Tech")
    L142.ag_Fert_Prod_MtN_ctry_Y <- get_data(all_data, "L142.ag_Fert_Prod_MtN_ctry_Y")
    A10.rsrc_info <- get_data(all_data, "energy/A10.rsrc_info")
    A21.globaltech_cost <- get_data(all_data, "energy/A21.globaltech_cost")
    A22.globaltech_cost <- get_data(all_data, "energy/A22.globaltech_cost")
    L1321.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1321.in_EJ_R_indenergy_F_Yh")
    L132.in_EJ_R_indfeed_F_Yh <- get_data(all_data, "L132.in_EJ_R_indfeed_F_Yh")

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
    # NOTE: This code uses translate_to_full_table
    # This function can be removed; see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
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
      add_legacy_name("L1322.Fert_Prod_MtN_R_F_Y") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1322.Fert_Prod_MtN_R_F_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1322.IO_R_Fert_F_Yh") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1322.IO_R_Fert_F_Yh
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1322.in_EJ_R_indenergy_F_Yh") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1322.in_EJ_R_indenergy_F_Yh
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1322.in_EJ_R_indfeed_F_Yh") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1322.in_EJ_R_indfeed_F_Yh
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1322.Fert_NEcost_75USDkgN_F") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1322.Fert_NEcost_75USDkgN_F

    return_data(L1322.Fert_Prod_MtN_R_F_Y, L1322.IO_R_Fert_F_Yh, L1322.in_EJ_R_indenergy_F_Yh, L1322.in_EJ_R_indfeed_F_Yh, L1322.Fert_NEcost_75USDkgN_F)
  } else {
    stop("Unknown command")
  }
}
