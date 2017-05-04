#' module_energy_LA154.transportation_UCD
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L154.in_EJ_R_trn_m_sz_tech_F_Yh}, \code{L154.in_EJ_ctry_trn_m_sz_tech_F}, \code{L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y}, \code{L154.loadfactor_R_trn_m_sz_tech_F_Y}, \code{L154.cost_usdvkm_R_trn_m_sz_tech_F_Y}, \code{L154.speed_kmhr_R_trn_m_sz_tech_F_Y}, \code{L154.out_mpkm_R_trn_nonmotor_Yh}. The corresponding file in the
#' original data system was \code{LA154.transportation_UCD.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RH May 2017
#' @export
module_energy_LA154.transportation_UCD <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/mappings/calibrated_techs_trn_agg", #needs source
             FILE = "energy/enduse_fuel_aggregation",
             FILE = "energy/mappings/UCD_ctry", #needs source
             FILE = "energy/mappings/UCD_techs", #needs source
             # This file is currently using a constant to select the correct SSP database
             # All SSP databases will be included in the input files
             # needs source, better description
             FILE = paste0("energy/UCD_trn_data_",energy.TRN_SSP),
             FILE = "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh",
             FILE = "temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
             FILE = "temp-data-inject/L131.in_EJ_R_Senduse_F_Yh",
             FILE = "temp-data-inject/L100.Pop_thous_ctry_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L154.in_EJ_R_trn_m_sz_tech_F_Yh",
             "L154.in_EJ_ctry_trn_m_sz_tech_F",
             "L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y",
             "L154.loadfactor_R_trn_m_sz_tech_F_Y",
             "L154.cost_usdvkm_R_trn_m_sz_tech_F_Y",
             "L154.speed_kmhr_R_trn_m_sz_tech_F_Y",
             "L154.out_mpkm_R_trn_nonmotor_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    calibrated_techs_trn_agg <- get_data(all_data, "energy/mappings/calibrated_techs_trn_agg")
    enduse_fuel_aggregation <- get_data(all_data, "energy/enduse_fuel_aggregation")
    UCD_ctry <- get_data(all_data, "energy/mappings/UCD_ctry")
    UCD_techs <- get_data(all_data, "energy/mappings/UCD_techs")

    # Russell - it looks like skeleton script got messed up on next line
    # Probably a loop in original file that loads stuff dynamically
    UCD_trn_data_ <- get_data(all_data,  paste0("energy/UCD_trn_data_",energy.TRN_SSP))
    L101.in_EJ_ctry_trn_Fi_Yh <- get_data(all_data, "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh")
    L1011.in_EJ_ctry_intlship_TOT_Yh <- get_data(all_data, "temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh")
    L131.in_EJ_R_Senduse_F_Yh <- get_data(all_data, "temp-data-inject/L131.in_EJ_R_Senduse_F_Yh")
    L100.Pop_thous_ctry_Yh <- get_data(all_data, "temp-data-inject/L100.Pop_thous_ctry_Yh")

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
      add_legacy_name("L154.in_EJ_R_trn_m_sz_tech_F_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "temp-data-inject/L131.in_EJ_R_Senduse_F_Yh", "temp-data-inject/L100.Pop_thous_ctry_Yh") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.in_EJ_R_trn_m_sz_tech_F_Yh
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.in_EJ_ctry_trn_m_sz_tech_F") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "temp-data-inject/L131.in_EJ_R_Senduse_F_Yh", "temp-data-inject/L100.Pop_thous_ctry_Yh")  %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.in_EJ_ctry_trn_m_sz_tech_F
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "temp-data-inject/L131.in_EJ_R_Senduse_F_Yh", "temp-data-inject/L100.Pop_thous_ctry_Yh")  %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.loadfactor_R_trn_m_sz_tech_F_Y") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "temp-data-inject/L131.in_EJ_R_Senduse_F_Yh", "temp-data-inject/L100.Pop_thous_ctry_Yh")  %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.loadfactor_R_trn_m_sz_tech_F_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.cost_usdvkm_R_trn_m_sz_tech_F_Y") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "temp-data-inject/L131.in_EJ_R_Senduse_F_Yh", "temp-data-inject/L100.Pop_thous_ctry_Yh")  %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.cost_usdvkm_R_trn_m_sz_tech_F_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.speed_kmhr_R_trn_m_sz_tech_F_Y") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "temp-data-inject/L131.in_EJ_R_Senduse_F_Yh", "temp-data-inject/L100.Pop_thous_ctry_Yh")  %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.speed_kmhr_R_trn_m_sz_tech_F_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.out_mpkm_R_trn_nonmotor_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "energy/mappings/calibrated_techs_trn_agg", "energy/enduse_fuel_aggregation",
                     "energy/mappings/UCD_ctry","energy/mappings/UCD_techs",paste0("energy/UCD_trn_data_",energy.TRN_SSP),
                     "temp-data-inject/L101.in_EJ_ctry_trn_Fi_Yh","temp-data-inject/L1011.in_EJ_ctry_intlship_TOT_Yh",
                     "temp-data-inject/L131.in_EJ_R_Senduse_F_Yh", "temp-data-inject/L100.Pop_thous_ctry_Yh")  %>%
      #typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.out_mpkm_R_trn_nonmotor_Yh

    return_data(L154.in_EJ_R_trn_m_sz_tech_F_Yh, L154.in_EJ_ctry_trn_m_sz_tech_F, L154.intensity_MJvkm_R_trn_m_sz_tech_F_Y, L154.loadfactor_R_trn_m_sz_tech_F_Y, L154.cost_usdvkm_R_trn_m_sz_tech_F_Y, L154.speed_kmhr_R_trn_m_sz_tech_F_Y, L154.out_mpkm_R_trn_nonmotor_Yh)
  } else {
    stop("Unknown command")
  }
}
