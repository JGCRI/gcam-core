#' module_aglu_L201.ag_For_Past_bio_input
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.AgSectorLogitTables[[ curr_table ]]$data}, \code{L201.AgSupplySector}, \code{L201.AgSubsectorLogitTables[[ curr_table ]]$data}, \code{L201.AgSupplySubsector}, \code{L201.AgProduction_ag}, \code{L201.AgProduction_For}, \code{L201.AgProduction_Past}, \code{L201.AgHAtoCL}, \code{L201.AgYield_bio_grass}, \code{L201.AgYield_bio_tree}. The corresponding file in the
#' original data system was \code{L201.ag_For_Past_bio_input.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L201.ag_For_Past_bio_input_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "aglu/A_agSupplySector",
             FILE = "aglu/A_agSupplySubsector",
             "L103.ag_Prod_Mt_R_C_Y_GLU",
             "L113.ag_bioYield_GJm2_R_GLU",
             "L122.ag_HA_to_CropLand_R_Y_GLU",
             "L123.ag_Prod_Mt_R_Past_Y_GLU",
             "L123.For_Prod_bm3_R_Y_GLU",
             "L123.For_Yield_m3m2_R_GLU",
             "L132.ag_an_For_Prices"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.AgSectorLogitTables[[ curr_table ]]$data",
             "L201.AgSupplySector",
             "L201.AgSubsectorLogitTables[[ curr_table ]]$data",
             "L201.AgSupplySubsector",
             "L201.AgProduction_ag",
             "L201.AgProduction_For",
             "L201.AgProduction_Past",
             "L201.AgHAtoCL",
             "L201.AgYield_bio_grass",
             "L201.AgYield_bio_tree"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    A_agSupplySector <- get_data(all_data, "aglu/A_agSupplySector")
    A_agSupplySubsector <- get_data(all_data, "aglu/A_agSupplySubsector")
    L103.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y_GLU")
    L113.ag_bioYield_GJm2_R_GLU <- get_data(all_data, "L113.ag_bioYield_GJm2_R_GLU")
    L122.ag_HA_to_CropLand_R_Y_GLU <- get_data(all_data, "L122.ag_HA_to_CropLand_R_Y_GLU")
    L123.ag_Prod_Mt_R_Past_Y_GLU <- get_data(all_data, "L123.ag_Prod_Mt_R_Past_Y_GLU")
    L123.For_Prod_bm3_R_Y_GLU <- get_data(all_data, "L123.For_Prod_bm3_R_Y_GLU")
    L123.For_Yield_m3m2_R_GLU <- get_data(all_data, "L123.For_Yield_m3m2_R_GLU")
    L132.ag_an_For_Prices <- get_data(all_data, "L132.ag_an_For_Prices")

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
      add_legacy_name("L201.AgSectorLogitTables[[ curr_table ]]$data") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.AgSectorLogitTables[[ curr_table ]]$data
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.AgSupplySector") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.AgSupplySector
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.AgSubsectorLogitTables[[ curr_table ]]$data") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.AgSubsectorLogitTables[[ curr_table ]]$data
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.AgSupplySubsector") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.AgSupplySubsector
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.AgProduction_ag") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.AgProduction_ag
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.AgProduction_For") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.AgProduction_For
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.AgProduction_Past") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.AgProduction_Past
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.AgHAtoCL") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.AgHAtoCL
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.AgYield_bio_grass") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.AgYield_bio_grass
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.AgYield_bio_tree") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.AgYield_bio_tree

    return_data(L201.AgSectorLogitTables[[ curr_table ]]$data, L201.AgSupplySector, L201.AgSubsectorLogitTables[[ curr_table ]]$data, L201.AgSupplySubsector, L201.AgProduction_ag, L201.AgProduction_For, L201.AgProduction_Past, L201.AgHAtoCL, L201.AgYield_bio_grass, L201.AgYield_bio_tree)
  } else {
    stop("Unknown command")
  }
}
