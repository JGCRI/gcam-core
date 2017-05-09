#' module_aglu_LB171.LC_R_Cropland_Yh_GLU_irr
#'
#' Calculate irrigated/rainfed harvested cropland cover and economic yields by GCAM region / commodity / year / GLU.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU}, \code{L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU}, \code{L171.ag_irrEcYield_kgm2_R_C_Y_GLU}, \code{L171.ag_rfdEcYield_kgm2_R_C_Y_GLU}. The corresponding file in the
#' original data system was \code{LB171.LC_R_Cropland_Yh_GLU_irr.R} (aglu level1).
#' @details This chunk downscales cropland by GCAM region / commodity / year / GLU to irrigated/rainfed according to irrigated/rainfed shares in the base year, and calculates the economic yields as production divided by cropland.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RC May 2017
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

    # First, calculate the share of irrigated vs. rainfed cropland in the base year by GCAM region, commodity and GLU.
    L161.ag_irrHA_frac_R_C_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, irrHA_frac) %>%
      # Get the share of rainfed cropland,
      mutate(rfd_share = 1 - irrHA_frac) ->
    L171.ag_irrHA_frac_R_C_GLU

    # Second, downscale total cropland to irrigated and rainfed by GCAM region, commodity, year and GLU.
    # Apply the base year share of irrigated vs. rainfed cropland to all historial periods (due to lack of data indicating otherwise).
    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      # Match the base year share by GCAM region, commodity, year and GLU.
      left_join_error_no_match(L171.ag_irrHA_frac_R_C_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      # Calculate the irrigated cropland cover in all historical periods.
      mutate(value = value * irrHA_frac,
             # Where values are missing, assume none irrigated (all rainfed). This is left as a check; there are no missing currently (5/10/17).
             value = if_else(is.na(value), 0, value)) %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, year, value) ->
      L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU

    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      # Total cropland cover, will be used for where rainfed share is missing
      rename(total = value) %>%
      # Match the base year share by GCAM region, commodity, year and GLU.
      left_join_error_no_match(L171.ag_irrHA_frac_R_C_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      # Calculate the rainfed cropland cover in all historical periods.
      mutate(value = total * rfd_share,
             # For the rainfed cropland table, missing values default to total cropland quantities; there are no missing currently (5/10/17).
             value = if_else(is.na(value), total, value)) %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, year, value) ->
      L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU

    # Calculate economic yields for irrigated crops, as production divided by cropland
    L161.ag_irrProd_Mt_R_C_Y_GLU %>%
      # Irrigated production in Mt by GCAM region, commodity, year, and GLU
      rename(prod = value) %>%
      # Match with the irrigated cropland cover in bm2 by GCAM region, commodity, year, and GLU
      left_join_error_no_match(L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU", "year")) %>%
      # Calculate economic yields as irrigated production divided by irrigated cropland, Mt/bm2 = kg/m2
      mutate(value = prod / value,
             # Replace missing value with zero
             value = if_else(is.na(value), 0, value)) %>%
      select(-prod) ->
      L171.ag_irrEcYield_kgm2_R_C_Y_GLU

    # Calculate economic yields for rainfed crops, as production divided by cropland
    L161.ag_rfdProd_Mt_R_C_Y_GLU %>%
      # Rainfed production in Mt by GCAM region, commodity, year, and GLU
      rename(prod = value) %>%
      # Match with the rainfed cropland cover in bm2 by GCAM region, commodity, year, and GLU
      left_join_error_no_match(L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU", "year")) %>%
      # Calculate economic yields as rainfed production divided by rainfed cropland, Mt/bm2 = kg/m2
      mutate(value = prod / value,
             # Replace missing value with zero
             value = if_else(is.na(value), 0, value)) %>%
      select(-prod) ->
      L171.ag_rfdEcYield_kgm2_R_C_Y_GLU

    # Produce outputs
    L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU %>%
      add_title("Irrigated harvested cropland cover by GCAM region / commodity / year / GLU") %>%
      add_units("bm2") %>%
      add_comments("Irrigated cropland cover is downscaled from total harvested cropland by GCAM region / commodity / year / GLU.") %>%
      add_comments("The share of irrigated cropland in the base year is applied to all historical periods.") %>%
      add_legacy_name("L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU") %>%
      add_precursors("L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
                     "temp-data-inject/L161.ag_irrHA_frac_R_C_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_PROTECT_FLOAT) ->
      L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU

    L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU %>%
      add_title("Rainfed harvested cropland cover by GCAM region / commodity / year / GLU") %>%
      add_units("bm2") %>%
      add_comments("Rainfed cropland cover is downscaled from total harvested cropland by GCAM region / commodity / year / GLU.") %>%
      add_comments("The share of rainfed cropland in the base year is applied to all historical periods.") %>%
      add_legacy_name("L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU") %>%
      add_precursors("L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
                     "temp-data-inject/L161.ag_irrHA_frac_R_C_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_PROTECT_FLOAT) ->
      L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU

    L171.ag_irrEcYield_kgm2_R_C_Y_GLU %>%
      add_title("Adjusted economic yield for irrigated crops by GCAM region / commodity / year / GLU") %>%
      add_units("kg/m2") %>%
      add_comments("Adjusted economic yield for irrigated crops are calculated as irrigated crop production devided by irrigated cropland cover.") %>%
      add_legacy_name("L171.ag_irrEcYield_kgm2_R_C_Y_GLU") %>%
      add_precursors("temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L171.ag_irrEcYield_kgm2_R_C_Y_GLU

    L171.ag_rfdEcYield_kgm2_R_C_Y_GLU %>%
      add_title("Adjusted economic yield for rainfed crops by GCAM region / commodity / year / GLU") %>%
      add_units("kg/m2") %>%
      add_comments("Adjusted economic yield for rainfed crops are calculated as rainfed crop production devided by rainfed cropland cover.") %>%
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
