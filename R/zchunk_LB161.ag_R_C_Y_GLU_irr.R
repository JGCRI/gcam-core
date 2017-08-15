#' module_aglu_LB161.ag_R_C_Y_GLU_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L161.ag_irrProd_Mt_R_C_Y_GLU}, \code{L161.ag_rfdProd_Mt_R_C_Y_GLU}, \code{L161.ag_irrHA_bm2_R_C_Y_GLU}, \code{L161.ag_rfdHA_bm2_R_C_Y_GLU}, \code{L161.ag_irrYield_kgm2_R_C_Y_GLU}, \code{L161.ag_rfdYield_kgm2_R_C_Y_GLU}, \code{L161.ag_irrHA_frac_R_C_GLU}. The corresponding file in the
#' original data system was \code{LB161.ag_R_C_Y_GLU_irr.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB161.ag_R_C_Y_GLU_irr_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L103.ag_Prod_Mt_R_C_Y_GLU",
              "L103.ag_HA_bm2_R_C_Y_GLU",
              "L152.ag_irrProd_Mt_R_C_GLU",
              "L152.ag_irrHA_bm2_R_C_GLU",
              "L152.ag_rfdProd_Mt_R_C_GLU",
              "L152.ag_rfdHA_bm2_R_C_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L161.ag_irrProd_Mt_R_C_Y_GLU",
             "L161.ag_rfdProd_Mt_R_C_Y_GLU",
             "L161.ag_irrHA_bm2_R_C_Y_GLU",
             "L161.ag_rfdHA_bm2_R_C_Y_GLU",
             "L161.ag_irrYield_kgm2_R_C_Y_GLU",
             "L161.ag_rfdYield_kgm2_R_C_Y_GLU",
             "L161.ag_irrHA_frac_R_C_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L103.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y_GLU")
    L103.ag_HA_bm2_R_C_Y_GLU <- get_data(all_data, "L103.ag_HA_bm2_R_C_Y_GLU")
    L152.ag_irrProd_Mt_R_C_GLU <- get_data(all_data, "L152.ag_irrProd_Mt_R_C_GLU")
    L152.ag_irrHA_bm2_R_C_GLU <- get_data(all_data, "L152.ag_irrHA_bm2_R_C_GLU")
    L152.ag_rfdProd_Mt_R_C_GLU <- get_data(all_data, "L152.ag_rfdProd_Mt_R_C_GLU")
    L152.ag_rfdHA_bm2_R_C_GLU <- get_data(all_data, "L152.ag_rfdHA_bm2_R_C_GLU")

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
    # NOTE: there are `merge` calls in this code. Be careful!
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
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
      add_legacy_name("L161.ag_irrProd_Mt_R_C_Y_GLU") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.ag_irrProd_Mt_R_C_Y_GLU

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.ag_rfdProd_Mt_R_C_Y_GLU

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L161.ag_irrHA_bm2_R_C_Y_GLU") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.ag_irrHA_bm2_R_C_Y_GLU

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L161.ag_rfdHA_bm2_R_C_Y_GLU") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.ag_rfdHA_bm2_R_C_Y_GLU

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L161.ag_irrYield_kgm2_R_C_Y_GLU") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.ag_irrYield_kgm2_R_C_Y_GLU

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L161.ag_rfdYield_kgm2_R_C_Y_GLU") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.ag_rfdYield_kgm2_R_C_Y_GLU

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L161.ag_irrHA_frac_R_C_GLU") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L161.ag_irrHA_frac_R_C_GLU

    return_data(L161.ag_irrProd_Mt_R_C_Y_GLU, L161.ag_rfdProd_Mt_R_C_Y_GLU, L161.ag_irrHA_bm2_R_C_Y_GLU, L161.ag_rfdHA_bm2_R_C_Y_GLU, L161.ag_irrYield_kgm2_R_C_Y_GLU, L161.ag_rfdYield_kgm2_R_C_Y_GLU, L161.ag_irrHA_frac_R_C_GLU)
  } else {
    stop("Unknown command")
  }
}
