#' module_aglu_LB171.LC_R_Cropland_Yh_GLU_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU}, \code{L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU}, \code{L171.ag_irrEcYield_kgm2_R_C_Y_GLU}, \code{L171.ag_rfdEcYield_kgm2_R_C_Y_GLU}. The corresponding file in the
#' original data system was \code{LB171.LC_R_Cropland_Yh_GLU_irr.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB171.LC_R_Cropland_Yh_GLU_irr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
              FILE = "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
              FILE = "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU",
              FILE = "temp-data-inject/L161.ag_irrHA_frac_R_C_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU",
             "L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU",
             "L171.ag_irrEcYield_kgm2_R_C_Y_GLU",
             "L171.ag_rfdEcYield_kgm2_R_C_Y_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU <- get_data(all_data, "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU")
    L161.ag_irrProd_Mt_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU") %>%
      # The following two lines of code will be removed later, when we're using 'real' data
      gather(year, value, -GCAM_region_ID, -GCAM_commodity, -GLU) %>%   # reshape
      mutate(year = as.integer(substr(year, 2, 5)))    # change Xyear to year

    L161.ag_rfdProd_Mt_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      # The following two lines of code will be removed later, when we're using 'real' data
      gather(year, value, -GCAM_region_ID, -GCAM_commodity, -GLU) %>%   # reshape
      mutate(year = as.integer(substr(year, 2, 5)))    # change Xyear to year

    L161.ag_irrHA_frac_R_C_GLU <- get_data(all_data, "temp-data-inject/L161.ag_irrHA_frac_R_C_GLU") # No year in this data

    # ===================================================
    L161.ag_irrHA_frac_R_C_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, irrHA_frac) %>%
      mutate(rfd_share = 1 - irrHA_frac) ->
    L171.ag_irrHA_frac_R_C_GLU

    # Downscaling cropland by region, crop, and GLU to irrigated/rainfed according to irrigated/rainfed shares in base year
    # NOTE: Assuming the same irrigated:rainfed share in all historical periods (due to lack of data indicating otherwise)
    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      left_join_error_no_match(L171.ag_irrHA_frac_R_C_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      mutate(value = value * irrHA_frac,
             # Where values are missing, assume rainfed. This is left as a check; there are none currently (5/10/17).
             value = if_else(is.na(value), 0, value)) %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, year, value) ->
      L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU

    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      rename(land = value) %>%
      left_join_error_no_match(L171.ag_irrHA_frac_R_C_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      mutate(value = land * rfd_share,
             # For the rainfed cropland table, missing values default to cropland quantities; presently all zero (5/10/17).
             value = if_else(is.na(value), land, value)) %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, year, value) ->
      L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU

    # Calculating economic yields as production divided by cropland
    L161.ag_irrProd_Mt_R_C_Y_GLU %>%
      rename(prod = value) %>%
      left_join_error_no_match(L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU", "year")) %>%
      mutate(value = prod / value,
             value = if_else(is.na(value), 0, value)) %>%
      select(-prod) ->
      L171.ag_irrEcYield_kgm2_R_C_Y_GLU

    L161.ag_rfdProd_Mt_R_C_Y_GLU %>%
      rename(prod = value) %>%
      left_join_error_no_match(L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU", "year")) %>%
      mutate(value = prod / value,
             value = if_else(is.na(value), 0, value)) %>%
      select(-prod) ->
      L171.ag_rfdEcYield_kgm2_R_C_Y_GLU

    # Produce outputs
    L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU") %>%
      add_precursors("L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
                     "temp-data-inject/L161.ag_irrHA_frac_R_C_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_PROTECT_FLOAT) ->
      L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU

    L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU") %>%
      add_precursors("L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
                     "temp-data-inject/L161.ag_irrHA_frac_R_C_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_PROTECT_FLOAT) ->
      L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU

    L171.ag_irrEcYield_kgm2_R_C_Y_GLU %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L171.ag_irrEcYield_kgm2_R_C_Y_GLU") %>%
      add_precursors("temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L171.ag_irrEcYield_kgm2_R_C_Y_GLU

    L171.ag_rfdEcYield_kgm2_R_C_Y_GLU %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L171.ag_rfdEcYield_kgm2_R_C_Y_GLU") %>%
      add_precursors("temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L171.ag_rfdEcYield_kgm2_R_C_Y_GLU

    return_data(L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU, L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU, L171.ag_irrEcYield_kgm2_R_C_Y_GLU, L171.ag_rfdEcYield_kgm2_R_C_Y_GLU)
  } else {
    stop("Unknown command")
  }
}
