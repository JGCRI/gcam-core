#' module_aglu_LB181.ag_R_C_Y_GLU_irr_mgmt
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L181.LC_bm2_R_C_Yh_GLU_irr_level}, \code{L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level}, \code{L181.ag_Prod_Mt_R_C_Y_GLU_irr_level}, \code{L181.YieldMult_R_bio_GLU_irr}, \code{L181.LandShare_R_bio_GLU_irr}. The corresponding file in the
#' original data system was \code{LB181.ag_R_C_Y_GLU_irr_mgmt.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB181.ag_R_C_Y_GLU_irr_mgmt_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             "Mueller_yield_levels",
             FILE = "aglu/Muller_crops",
             FILE = "aglu/FAO_ag_items_PRODSTAT",
             "L151.ag_irrHA_ha_ctry_crop",
             "L151.ag_rfdHA_ha_ctry_crop",
             "L151.ag_irrProd_t_ctry_crop",
             "L151.ag_rfdProd_t_ctry_crop",
             "L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU",
             "L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU",
             "L171.ag_irrEcYield_kgm2_R_C_Y_GLU",
             "L171.ag_rfdEcYield_kgm2_R_C_Y_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L181.LC_bm2_R_C_Yh_GLU_irr_level",
             "L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level",
             "L181.ag_Prod_Mt_R_C_Y_GLU_irr_level",
             "L181.YieldMult_R_bio_GLU_irr",
             "L181.LandShare_R_bio_GLU_irr"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    Mueller_yield_levels <- get_data(all_data, "Mueller_yield_levels")
    Muller_crops <- get_data(all_data, "aglu/Muller_crops")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO_ag_items_PRODSTAT")
    L151.ag_irrHA_ha_ctry_crop <- get_data(all_data, "L151.ag_irrHA_ha_ctry_crop")
    L151.ag_rfdHA_ha_ctry_crop <- get_data(all_data, "L151.ag_rfdHA_ha_ctry_crop")
    L151.ag_irrProd_t_ctry_crop <- get_data(all_data, "L151.ag_irrProd_t_ctry_crop")
    L151.ag_rfdProd_t_ctry_crop <- get_data(all_data, "L151.ag_rfdProd_t_ctry_crop")
    L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU <- get_data(all_data, "L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU")
    L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU <- get_data(all_data, "L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU")
    L171.ag_irrEcYield_kgm2_R_C_Y_GLU <- get_data(all_data, "L171.ag_irrEcYield_kgm2_R_C_Y_GLU")
    L171.ag_rfdEcYield_kgm2_R_C_Y_GLU <- get_data(all_data, "L171.ag_rfdEcYield_kgm2_R_C_Y_GLU")

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
      add_legacy_name("L181.LC_bm2_R_C_Yh_GLU_irr_level") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L181.LC_bm2_R_C_Yh_GLU_irr_level
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L181.ag_Prod_Mt_R_C_Y_GLU_irr_level") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L181.ag_Prod_Mt_R_C_Y_GLU_irr_level
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L181.YieldMult_R_bio_GLU_irr") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L181.YieldMult_R_bio_GLU_irr
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L181.LandShare_R_bio_GLU_irr") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L181.LandShare_R_bio_GLU_irr

    return_data(L181.LC_bm2_R_C_Yh_GLU_irr_level, L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level, L181.ag_Prod_Mt_R_C_Y_GLU_irr_level, L181.YieldMult_R_bio_GLU_irr, L181.LandShare_R_bio_GLU_irr)
  } else {
    stop("Unknown command")
  }
}
