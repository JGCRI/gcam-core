#' module_emissions_L124.nonco2_unmgd_R_S_T_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L124.nonco2_tg_R_grass_Y_GLU}, \code{L124.nonco2_tg_R_forest_Y_GLU}, \code{L124.deforest_coefs}. The corresponding file in the
#' original data system was \code{L124.nonco2_unmgd_R_S_T_Y.R} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L124.nonco2_unmgd_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EDGAR/EDGAR_sector",
             FILE = "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj",
             FILE = "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj",
             "EDGAR_gases",
             FILE = "emissions/GFED/GFED_ForestFire_SO2",
             FILE = "emissions/GFED/GFED_Deforest_SO2",
             FILE = "emissions/GFED/GFED_ForestFire_CO",
             FILE = "emissions/GFED/GFED_Deforest_CO",
             FILE = "emissions/GFED/GFED_ForestFire_NOx",
             FILE = "emissions/GFED/GFED_Deforest_NOx"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L124.nonco2_tg_R_grass_Y_GLU",
             "L124.nonco2_tg_R_forest_Y_GLU",
             "L124.deforest_coefs"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR/EDGAR_sector")
    L124.LC_bm2_R_Grass_Yh_GLU_adj <- get_data(all_data, "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj")
    L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj <- get_data(all_data, "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj")
    EDGAR_gases <- get_data(all_data, "EDGAR_gases")
    GFED_ForestFire_SO2 <- get_data(all_data, "emissions/GFED/GFED_ForestFire_SO2")
    GFED_Deforest_SO2 <- get_data(all_data, "emissions/GFED/GFED_Deforest_SO2")
    GFED_ForestFire_CO <- get_data(all_data, "emissions/GFED/GFED_ForestFire_CO")
    GFED_Deforest_CO <- get_data(all_data, "emissions/GFED/GFED_Deforest_CO")
    GFED_ForestFire_NOx <- get_data(all_data, "emissions/GFED/GFED_ForestFire_NOx")
    GFED_Deforest_NOx <- get_data(all_data, "emissions/GFED/GFED_Deforest_NOx")

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
      add_legacy_name("L124.nonco2_tg_R_grass_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "emissions/EDGAR/EDGAR_sector", "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj", "EDGAR_gases") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L124.nonco2_tg_R_grass_Y_GLU
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L124.nonco2_tg_R_forest_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "emissions/EDGAR/EDGAR_sector", "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj", "EDGAR_gases",
                     "emissions/GFED/GFED_ForestFire_SO2", "emissions/GFED/GFED_Deforest_SO2", "emissions/GFED/GFED_ForestFire_CO",
                     "emissions/GFED/GFED_Deforest_CO", "emissions/GFED/GFED_ForestFire_NOx", "emissions/GFED/GFED_Deforest_NOx") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L124.nonco2_tg_R_forest_Y_GLU
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L124.deforest_coefs") %>%
      same_precursors_as("L124.nonco2_tg_R_forest_Y_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L124.deforest_coefs

    return_data(L124.nonco2_tg_R_grass_Y_GLU, L124.nonco2_tg_R_forest_Y_GLU, L124.deforest_coefs)
  } else {
    stop("Unknown command")
  }
}
