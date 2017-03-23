#' module_gcam.usa_LB126.Gas_ElecTD
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L126.out_EJ_state_pipeline_gas}, \code{L126.in_EJ_state_pipeline_gas}, \code{L126.out_EJ_state_gasproc_F}, \code{L126.in_EJ_state_gasproc_F}, \code{L126.out_EJ_state_td_elec}, \code{L126.in_EJ_state_td_elec}. The corresponding file in the
#' original data system was \code{LB126.Gas_ElecTD.R} (gcam-usa level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_LB126.Gas_ElecTD_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L122.in_EJ_R_gasproc_F_Yh",
 "L122.out_EJ_R_gasproc_F_Yh",
 "L126.in_EJ_R_gaspipe_F_Yh",
 "L126.out_EJ_R_gaspipe_F_Yh",
 "L126.IO_R_electd_F_Yh",
 "L101.EIA_use_all_Bbtu",
 "L101.inEIA_EJ_state_S_F",
 "L122.in_EJ_state_refining_F",
 "L123.out_EJ_state_elec_F",
 "L123.out_EJ_state_ownuse_elec",
 "L132.in_EJ_state_indchp_F",
 "L132.in_EJ_state_indfeed_F",
 "L132.in_EJ_state_indnochp_F",
 "L1321.in_EJ_state_cement_F_Y",
 "L1322.in_EJ_state_Fert_Yh",
 "L142.in_EJ_state_bld_F",
 "L154.in_EJ_state_trn_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L126.out_EJ_state_pipeline_gas",
"L126.in_EJ_state_pipeline_gas",
"L126.out_EJ_state_gasproc_F",
"L126.in_EJ_state_gasproc_F",
"L126.out_EJ_state_td_elec",
"L126.in_EJ_state_td_elec"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      L122.in_EJ_R_gasproc_F_Yh <- get_data(all_data, "L122.in_EJ_R_gasproc_F_Yh")
  L122.out_EJ_R_gasproc_F_Yh <- get_data(all_data, "L122.out_EJ_R_gasproc_F_Yh")
  L126.in_EJ_R_gaspipe_F_Yh <- get_data(all_data, "L126.in_EJ_R_gaspipe_F_Yh")
  L126.out_EJ_R_gaspipe_F_Yh <- get_data(all_data, "L126.out_EJ_R_gaspipe_F_Yh")
  L126.IO_R_electd_F_Yh <- get_data(all_data, "L126.IO_R_electd_F_Yh")
  L101.EIA_use_all_Bbtu <- get_data(all_data, "L101.EIA_use_all_Bbtu")
  L101.inEIA_EJ_state_S_F <- get_data(all_data, "L101.inEIA_EJ_state_S_F")
  L122.in_EJ_state_refining_F <- get_data(all_data, "L122.in_EJ_state_refining_F")
  L123.out_EJ_state_elec_F <- get_data(all_data, "L123.out_EJ_state_elec_F")
  L123.out_EJ_state_ownuse_elec <- get_data(all_data, "L123.out_EJ_state_ownuse_elec")
  L132.in_EJ_state_indchp_F <- get_data(all_data, "L132.in_EJ_state_indchp_F")
  L132.in_EJ_state_indfeed_F <- get_data(all_data, "L132.in_EJ_state_indfeed_F")
  L132.in_EJ_state_indnochp_F <- get_data(all_data, "L132.in_EJ_state_indnochp_F")
  L1321.in_EJ_state_cement_F_Y <- get_data(all_data, "L1321.in_EJ_state_cement_F_Y")
  L1322.in_EJ_state_Fert_Yh <- get_data(all_data, "L1322.in_EJ_state_Fert_Yh")
  L142.in_EJ_state_bld_F <- get_data(all_data, "L142.in_EJ_state_bld_F")
  L154.in_EJ_state_trn_F <- get_data(all_data, "L154.in_EJ_state_trn_F")

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
 add_legacy_name("L126.out_EJ_state_pipeline_gas") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L126.out_EJ_state_pipeline_gas
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L126.in_EJ_state_pipeline_gas") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L126.in_EJ_state_pipeline_gas
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L126.out_EJ_state_gasproc_F") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L126.out_EJ_state_gasproc_F
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L126.in_EJ_state_gasproc_F") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L126.in_EJ_state_gasproc_F
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L126.out_EJ_state_td_elec") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L126.out_EJ_state_td_elec
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L126.in_EJ_state_td_elec") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L126.in_EJ_state_td_elec

    return_data(L126.out_EJ_state_pipeline_gas, L126.in_EJ_state_pipeline_gas, L126.out_EJ_state_gasproc_F, L126.in_EJ_state_gasproc_F, L126.out_EJ_state_td_elec, L126.in_EJ_state_td_elec)
  } else {
    stop("Unknown command")
  }
}



