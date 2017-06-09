#' module_aglu_L2062.ag_Fert_irr_mgmt
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2062.AgCoef_Fert_ag_irr_mgmt}, \code{L2062.AgCoef_Fert_bio_irr_mgmt}, \code{L2062.AgCost_ag_irr_mgmt_adj}, \code{L2062.AgCost_bio_irr_mgmt_adj}. The corresponding file in the
#' original data system was \code{L2062.ag_Fert_irr_mgmt.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2062.ag_Fert_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( FILE = "common/GCAM_region_names",
              FILE = "water/basin_to_country_mapping",
              FILE = "aglu/A_Fodderbio_chars",
              FILE = "temp-data-inject/L142.ag_Fert_IO_R_C_Y_GLU",
              FILE = "temp-data-inject/L205.AgCost_ag",
              FILE = "temp-data-inject/L205.AgCost_bio"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2062.AgCoef_Fert_ag_irr_mgmt",
             "L2062.AgCoef_Fert_bio_irr_mgmt",
             "L2062.AgCost_ag_irr_mgmt_adj",
             "L2062.AgCost_bio_irr_mgmt_adj"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    A_Fodderbio_chars <- get_data(all_data, "aglu/A_Fodderbio_chars")
    L142.ag_Fert_IO_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L142.ag_Fert_IO_R_C_Y_GLU")
    L205.AgCost_ag <- get_data(all_data, "temp-data-inject/L205.AgCost_ag")
    L205.AgCost_bio <- get_data(all_data, "temp-data-inject/L205.AgCost_bio")

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
      add_legacy_name("L2062.AgCoef_Fert_ag_irr_mgmt") %>%
      add_precursors("common/GCAM_region_names", "water/basin_to_country_mapping", "aglu/A_Fodderbio_chars",
                     "temp-data-inject/L142.ag_Fert_IO_R_C_Y_GLU", "temp-data-inject/L205.AgCost_ag", "temp-data-inject/L205.AgCost_bio") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2062.AgCoef_Fert_ag_irr_mgmt
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2062.AgCoef_Fert_bio_irr_mgmt") %>%
      add_precursors("common/GCAM_region_names", "water/basin_to_country_mapping", "aglu/A_Fodderbio_chars",
                     "temp-data-inject/L142.ag_Fert_IO_R_C_Y_GLU", "temp-data-inject/L205.AgCost_ag", "temp-data-inject/L205.AgCost_bio") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2062.AgCoef_Fert_bio_irr_mgmt
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2062.AgCost_ag_irr_mgmt_adj") %>%
      add_precursors("common/GCAM_region_names", "water/basin_to_country_mapping", "aglu/A_Fodderbio_chars",
                     "temp-data-inject/L142.ag_Fert_IO_R_C_Y_GLU", "temp-data-inject/L205.AgCost_ag", "temp-data-inject/L205.AgCost_bio") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2062.AgCost_ag_irr_mgmt_adj
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2062.AgCost_bio_irr_mgmt_adj") %>%
      add_precursors("common/GCAM_region_names", "water/basin_to_country_mapping", "aglu/A_Fodderbio_chars",
                     "temp-data-inject/L142.ag_Fert_IO_R_C_Y_GLU", "temp-data-inject/L205.AgCost_ag", "temp-data-inject/L205.AgCost_bio") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2062.AgCost_bio_irr_mgmt_adj

    return_data(L2062.AgCoef_Fert_ag_irr_mgmt, L2062.AgCoef_Fert_bio_irr_mgmt, L2062.AgCost_ag_irr_mgmt_adj, L2062.AgCost_bio_irr_mgmt_adj)
  } else {
    stop("Unknown command")
  }
}
