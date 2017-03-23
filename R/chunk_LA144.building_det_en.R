#' module_energy_LA144.building_det_en
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L144.end_use_eff}, \code{L144.shell_eff_R_Y}, \code{L144.in_EJ_R_bld_serv_F_Yh}, \code{L144.NEcost_75USDGJ}, \code{L144.internal_gains}, \code{L144.base_service_EJ_serv}. The corresponding file in the
#' original data system was \code{LA144.building_det_en.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_LA144.building_det_en_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "common/iso_GCAM_regID",
FILE = "energy/A_regions",
FILE = "energy/calibrated_techs_bld_det",
FILE = "energy/A44.cost_efficiency",
FILE = "energy/A44.internal_gains",
FILE = "energy/A44.share_serv_fuel",
FILE = "energy/A44.shell_eff_mult_RG3",
FILE = "energy/A44.tech_eff_mult_RG3",
FILE = "energy/A44.USA_TechChange",
 "L101.in_EJ_ctry_bld_Fi_Yh",
 "L142.in_EJ_R_bld_F_Yh",
 "L143.HDDCDD_scen_RG3_Y",
 "L143.HDDCDD_scen_ctry_Y",
FILE = "socioeconomics/GCAM3_pcGDP",
 "L102.pcgdp_thous90USD_GCAM3_ctry_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L144.end_use_eff",
"L144.shell_eff_R_Y",
"L144.in_EJ_R_bld_serv_F_Yh",
"L144.NEcost_75USDGJ",
"L144.internal_gains",
"L144.base_service_EJ_serv"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
  A_regions <- get_data(all_data, "energy/A_regions")
  calibrated_techs_bld_det <- get_data(all_data, "energy/calibrated_techs_bld_det")
  A44.cost_efficiency <- get_data(all_data, "energy/A44.cost_efficiency")
  A44.internal_gains <- get_data(all_data, "energy/A44.internal_gains")
  A44.share_serv_fuel <- get_data(all_data, "energy/A44.share_serv_fuel")
  A44.shell_eff_mult_RG3 <- get_data(all_data, "energy/A44.shell_eff_mult_RG3")
  A44.tech_eff_mult_RG3 <- get_data(all_data, "energy/A44.tech_eff_mult_RG3")
  A44.USA_TechChange <- get_data(all_data, "energy/A44.USA_TechChange")
  L101.in_EJ_ctry_bld_Fi_Yh <- get_data(all_data, "L101.in_EJ_ctry_bld_Fi_Yh")
  L142.in_EJ_R_bld_F_Yh <- get_data(all_data, "L142.in_EJ_R_bld_F_Yh")
  L143.HDDCDD_scen_RG3_Y <- get_data(all_data, "L143.HDDCDD_scen_RG3_Y")
  L143.HDDCDD_scen_ctry_Y <- get_data(all_data, "L143.HDDCDD_scen_ctry_Y")
  GCAM3_pcGDP <- get_data(all_data, "socioeconomics/GCAM3_pcGDP")
  L102.pcgdp_thous90USD_GCAM3_ctry_Y <- get_data(all_data, "L102.pcgdp_thous90USD_GCAM3_ctry_Y")

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
 add_legacy_name("L144.end_use_eff") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L144.end_use_eff
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L144.shell_eff_R_Y") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L144.shell_eff_R_Y
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L144.in_EJ_R_bld_serv_F_Yh") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L144.in_EJ_R_bld_serv_F_Yh
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L144.NEcost_75USDGJ") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L144.NEcost_75USDGJ
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L144.internal_gains") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L144.internal_gains
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L144.base_service_EJ_serv") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L144.base_service_EJ_serv

    return_data(L144.end_use_eff, L144.shell_eff_R_Y, L144.in_EJ_R_bld_serv_F_Yh, L144.NEcost_75USDGJ, L144.internal_gains, L144.base_service_EJ_serv)
  } else {
    stop("Unknown command")
  }
}



