#' module_energy_LA144.building_det_flsp
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L144.flsp_bm2_R_res_Yh}, \code{L144.flsp_bm2_R_comm_Yh}, \code{L144.flspPrice_90USDm2_R_bld_Yh}. The corresponding file in the
#' original data system was \code{LA144.building_det_flsp.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_LA144.building_det_flsp_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
FILE = "energy/A44.flsp_bm2_state_res",
FILE = "energy/A44.flsp_bm2_state_comm",
FILE = "energy/A44.pcflsp_default",
FILE = "energy/A44.HouseholdSize",
FILE = "energy/CEDB_ResFloorspace_chn",
FILE = "energy/Other_pcflsp_m2_ctry_Yh",
FILE = "energy/IEA_PCResFloorspace",
FILE = "energy/Odyssee_ResFloorspacePerHouse",
 "L100.Pop_thous_ctry_Yh",
 "L102.gdp_mil90usd_GCAM3_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L144.flsp_bm2_R_res_Yh",
"L144.flsp_bm2_R_comm_Yh",
"L144.flspPrice_90USDm2_R_bld_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
  A44.flsp_bm2_state_res <- get_data(all_data, "energy/A44.flsp_bm2_state_res")
  A44.flsp_bm2_state_comm <- get_data(all_data, "energy/A44.flsp_bm2_state_comm")
  A44.pcflsp_default <- get_data(all_data, "energy/A44.pcflsp_default")
  A44.HouseholdSize <- get_data(all_data, "energy/A44.HouseholdSize")
  CEDB_ResFloorspace_chn <- get_data(all_data, "energy/CEDB_ResFloorspace_chn")
  Other_pcflsp_m2_ctry_Yh <- get_data(all_data, "energy/Other_pcflsp_m2_ctry_Yh")
  IEA_PCResFloorspace <- get_data(all_data, "energy/IEA_PCResFloorspace")
  Odyssee_ResFloorspacePerHouse <- get_data(all_data, "energy/Odyssee_ResFloorspacePerHouse")
  L100.Pop_thous_ctry_Yh <- get_data(all_data, "L100.Pop_thous_ctry_Yh")
  L102.gdp_mil90usd_GCAM3_R_Y <- get_data(all_data, "L102.gdp_mil90usd_GCAM3_R_Y")

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
 add_legacy_name("L144.flsp_bm2_R_res_Yh") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L144.flsp_bm2_R_res_Yh
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L144.flsp_bm2_R_comm_Yh") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L144.flsp_bm2_R_comm_Yh
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L144.flspPrice_90USDm2_R_bld_Yh") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L144.flspPrice_90USDm2_R_bld_Yh

    return_data(L144.flsp_bm2_R_res_Yh, L144.flsp_bm2_R_comm_Yh, L144.flspPrice_90USDm2_R_bld_Yh)
  } else {
    stop("Unknown command")
  }
}



