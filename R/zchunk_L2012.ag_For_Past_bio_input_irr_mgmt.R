#' module_aglu_L2012.ag_For_Past_bio_input_irr_mgmt
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2012.AgSupplySector}, \code{L2012.AgSupplySubsector}, \code{L2012.AgProduction_ag_irr_mgmt}, \code{L2012.AgProduction_For}, \code{L2012.AgProduction_Past}, \code{L2012.AgHAtoCL_irr_mgmt}, \code{L2012.AgYield_bio_ref}. The corresponding file in the
#' original data system was \code{L2012.ag_For_Past_bio_input_irr_mgmt.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2012.ag_For_Past_bio_input_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "aglu/A_agSupplySector",
             FILE = "aglu/A_agSupplySubsector",
             "L103.ag_Prod_Mt_R_C_Y_GLU",
             "L113.ag_bioYield_GJm2_R_GLU",
             "L122.ag_HA_to_CropLand_R_Y_GLU",
             FILE = "temp-data-inject/L123.ag_Prod_Mt_R_Past_Y_GLU",
             FILE = "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU",
             FILE = "temp-data-inject/L123.For_Yield_m3m2_R_GLU",
             "L132.ag_an_For_Prices",
             FILE = "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
             FILE = "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU",
             "L163.ag_irrBioYield_GJm2_R_GLU",
             "L163.ag_rfdBioYield_GJm2_R_GLU",
             "L181.ag_Prod_Mt_R_C_Y_GLU_irr_level",
             "L181.YieldMult_R_bio_GLU_irr"))
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
    A_AgSupplySubsector <- get_data(all_data, "aglu/A_agSupplySubsector")
    L103.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y_GLU")
    L113.ag_bioYield_GJm2_R_GLU <- get_data(all_data, "L113.ag_bioYield_GJm2_R_GLU")
    L122.ag_HA_to_CropLand_R_Y_GLU <- get_data(all_data, "L122.ag_HA_to_CropLand_R_Y_GLU")
    L123.ag_Prod_Mt_R_Past_Y_GLU <- get_data(all_data, "temp-data-inject/L123.ag_Prod_Mt_R_Past_Y_GLU")
    L123.For_Prod_bm3_R_Y_GLU <- get_data(all_data, "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU")
    L123.For_Yield_m3m2_R_GLU <- get_data(all_data, "temp-data-inject/L123.For_Yield_m3m2_R_GLU")
    L132.ag_an_For_Prices <- get_data(all_data, "L132.ag_an_For_Prices" )
    L161.ag_irrProd_Mt_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU")
    L161.ag_rfdProd_Mt_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU")
    L163.ag_irrBioYield_GJm2_R_GLU <- get_data(all_data, "L163.ag_irrBioYield_GJm2_R_GLU")
    L163.ag_rfdBioYield_GJm2_R_GLU <- get_data(all_data, "L163.ag_rfdBioYield_GJm2_R_GLU")
    L181.ag_Prod_Mt_R_C_Y_GLU_irr_level <- get_data(all_data, "L181.ag_Prod_Mt_R_C_Y_GLU_irr_level")
    L181.YieldMult_R_bio_GLU_irr <- get_data(all_data, "L181.YieldMult_R_bio_GLU_irr")

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
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_agSupplySector",
                     "L132.ag_an_For_Prices") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2012.AgSupplySector
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgSupplySubsector") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_agSupplySubsector",
                     "L103.ag_Prod_Mt_R_C_Y_GLU",
                     "temp-data-inject/L123.ag_Prod_Mt_R_Past_Y_GLU",
                     "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2012.AgSupplySubsector
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgProduction_ag_irr_mgmt") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_agSupplySector",
                     "aglu/A_agSupplySubsector",
                     "L103.ag_Prod_Mt_R_C_Y_GLU",
                     "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU",
                     "L181.ag_Prod_Mt_R_C_Y_GLU_irr_level") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2012.AgProduction_ag_irr_mgmt
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgProduction_For") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_agSupplySector",
                     "aglu/A_agSupplySubsector",
                     "temp-data-inject/L123.For_Prod_bm3_R_Y_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2012.AgProduction_For
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgProduction_Past") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_agSupplySector",
                     "aglu/A_agSupplySubsector",
                     "temp-data-inject/L123.ag_Prod_Mt_R_Past_Y_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2012.AgProduction_Past
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgHAtoCL_irr_mgmt") %>%
      same_precursors_as("L2012.AgProduction_ag_irr_mgmt") %>%
      add_precursors("L122.ag_HA_to_CropLand_R_Y_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2012.AgHAtoCL_irr_mgmt
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2012.AgYield_bio_ref") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_agSupplySector",
                     "aglu/A_agSupplySubsector",
                     "L113.ag_bioYield_GJm2_R_GLU",
                     "temp-data-inject/L123.For_Yield_m3m2_R_GLU",
                     "L163.ag_irrBioYield_GJm2_R_GLU",
                     "L163.ag_rfdBioYield_GJm2_R_GLU",
                     "L181.YieldMult_R_bio_GLU_irr") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2012.AgYield_bio_ref

    return_data(L2012.AgSupplySector, L2012.AgSupplySubsector, L2012.AgProduction_ag_irr_mgmt, L2012.AgProduction_For, L2012.AgProduction_Past, L2012.AgHAtoCL_irr_mgmt, L2012.AgYield_bio_ref)
  } else {
    stop("Unknown command")
  }
}
