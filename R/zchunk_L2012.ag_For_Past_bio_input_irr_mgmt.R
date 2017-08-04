#' module_aglu_L2012.ag_For_Past_bio_input_irr_mgmt
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2012.AgSectorLogitTables[[ curr_table ]]$data}, \code{L2012.AgSupplySector}, \code{L2012.AgSubsectorLogitTables[[ curr_table ]]$data}, \code{L2012.AgSupplySubsector}, \code{L2012.AgProduction_ag_irr_mgmt}, \code{L2012.AgProduction_For}, \code{L2012.AgProduction_Past}, \code{L2012.AgHAtoCL_irr_mgmt}, \code{L2012.AgYield_bio_ref}. The corresponding file in the
#' original data system was \code{L2012.ag_For_Past_bio_input_irr_mgmt.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2012.ag_For_Past_bio_input_irr_mgmt_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "aglu/A_agSupplySector",
             "L181.ag_Prod_Mt_R_C_Y_GLU_irr_level",
             "L181.YieldMult_R_bio_GLU_irr",
             "L2011.AgSupplySector",
             "L2011.AgSupplySubsector",
             "L2011.AgProduction_ag_irr",
             "L2011.AgProduction_For",
             "L2011.AgProduction_Past",
             "L2011.AgHAtoCL_irr",
             "L2011.AgYield_bio_grass_irr",
             "L2011.AgYield_bio_tree_irr"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2012.AgSupplySector",
             "L2012.AgSupplySubsector",
             "L2012.AgProduction_ag_irr_mgmt",
             "L2012.AgProduction_For",
             "L2012.AgProduction_Past",
             "L2012.AgHAtoCL_irr_mgmt",
             "L2012.AgYield_bio_ref"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    A_agSupplySector <- get_data(all_data, "aglu/A_agSupplySector")
    L181.ag_Prod_Mt_R_C_Y_GLU_irr_level <- get_data(all_data, "L181.ag_Prod_Mt_R_C_Y_GLU_irr_level")
    L181.YieldMult_R_bio_GLU_irr <- get_data(all_data, "L181.YieldMult_R_bio_GLU_irr")
    L2011.AgSupplySector <- get_data(all_data, "L2011.AgSupplySector")
    L2011.AgSupplySubsector <- get_data(all_data, "L2011.AgSupplySubsector")
    L2011.AgProduction_ag_irr <- get_data(all_data, "L2011.AgProduction_ag_irr")
    L2011.AgProduction_For <- get_data(all_data, "L2011.AgProduction_For")
    L2011.AgProduction_Past <- get_data(all_data, "L2011.AgProduction_Past")
    L2011.AgHAtoCL_irr <- get_data(all_data, "L2011.AgHAtoCL_irr")
    L2011.AgYield_bio_grass_irr <- get_data(all_data, "L2011.AgYield_bio_grass_irr")
    L2011.AgYield_bio_tree_irr <- get_data(all_data, "L2011.AgYield_bio_tree_irr")

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
      add_legacy_name("L2012.AgSupplySector") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2012.AgSupplySector
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgSubsectorLogitTables[[ curr_table ]]$data") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2012.AgSubsectorLogitTables[[ curr_table ]]$data
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgSupplySubsector") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2012.AgSupplySubsector
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgProduction_ag_irr_mgmt") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2012.AgProduction_ag_irr_mgmt
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgProduction_For") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2012.AgProduction_For
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgProduction_Past") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2012.AgProduction_Past
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgHAtoCL_irr_mgmt") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2012.AgHAtoCL_irr_mgmt
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgYield_bio_ref") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2012.AgYield_bio_ref

    return_data(L2012.AgSupplySector, L2012.AgSubsectorLogitTables[[ curr_table ]]$data, L2012.AgSupplySubsector, L2012.AgProduction_ag_irr_mgmt, L2012.AgProduction_For, L2012.AgProduction_Past, L2012.AgHAtoCL_irr_mgmt, L2012.AgYield_bio_ref)
  } else {
    stop("Unknown command")
  }
}
