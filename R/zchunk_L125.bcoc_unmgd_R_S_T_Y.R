#' module_emissions_L125.bcoc_unmgd_R_S_T_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L125.bcoc_tgbkm2_R_grass_2000}, \code{L125.bcoc_tgbkm2_R_forest_2000}, \code{L125.deforest_coefs_bcoc}. The corresponding file in the
#' original data system was \code{L125.bcoc_unmgd_R_S_T_Y.R} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L125.bcoc_unmgd_R_S_T_Y_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/EDGAR_nation",
             FILE = "common/iso_GCAM_regID",
             "L124.LC_bm2_R_Grass_Yh_GLU_adj",
             "L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj",
             FILE = "emissions/RCP_BC_2000",
             FILE = "emissions/RCP_OC_2000",
             FILE = "emissions/GFED_ForestFire_BC",
             FILE = "emissions/GFED_Deforest_BC",
             FILE = "emissions/GFED_ForestFire_OC",
             FILE = "emissions/GFED_Deforest_OC"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L125.bcoc_tgbkm2_R_grass_2000",
             "L125.bcoc_tgbkm2_R_forest_2000",
             "L125.deforest_coefs_bcoc"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    EDGAR_nation <- get_data(all_data, "emissions/EDGAR_nation")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    L124.LC_bm2_R_Grass_Yh_GLU_adj <- get_data(all_data, "L124.LC_bm2_R_Grass_Yh_GLU_adj")
    L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj <- get_data(all_data, "L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj")
    RCP_BC_2000 <- get_data(all_data, "emissions/RCP_BC_2000")
    RCP_OC_2000 <- get_data(all_data, "emissions/RCP_OC_2000")
    GFED_ForestFire_BC <- get_data(all_data, "emissions/GFED_ForestFire_BC")
    GFED_Deforest_BC <- get_data(all_data, "emissions/GFED_Deforest_BC")
    GFED_ForestFire_OC <- get_data(all_data, "emissions/GFED_ForestFire_OC")
    GFED_Deforest_OC <- get_data(all_data, "emissions/GFED_Deforest_OC")

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
      add_legacy_name("L125.bcoc_tgbkm2_R_grass_2000") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L125.bcoc_tgbkm2_R_grass_2000
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L125.bcoc_tgbkm2_R_forest_2000") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L125.bcoc_tgbkm2_R_forest_2000
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L125.deforest_coefs_bcoc") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L125.deforest_coefs_bcoc

    return_data(L125.bcoc_tgbkm2_R_grass_2000, L125.bcoc_tgbkm2_R_forest_2000, L125.deforest_coefs_bcoc)
  } else {
    stop("Unknown command")
  }
}
