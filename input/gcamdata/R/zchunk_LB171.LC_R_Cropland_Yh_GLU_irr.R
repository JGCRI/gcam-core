# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

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
#' @details This chunk downscales total harvested cropland by GCAM region / commodity / year / GLU to irrigated/rainfed according to irrigated/rainfed shares in the base year, and calculates the economic yields as production divided by cropland.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter full_join if_else group_by left_join mutate select
#' @importFrom tidyr nesting
#' @author RC May 2017
module_aglu_LB171.LC_R_Cropland_Yh_GLU_irr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
              "L161.ag_irrProd_Mt_R_C_Y_GLU",
              "L161.ag_rfdProd_Mt_R_C_Y_GLU",
              "L161.ag_irrHA_frac_R_C_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU",
             "L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU",
             "L171.ag_irrEcYield_kgm2_R_C_Y_GLU",
             "L171.ag_rfdEcYield_kgm2_R_C_Y_GLU"))
  } else if(command == driver.MAKE) {

    year <- value <- GCAM_region_ID <- GCAM_commodity <- GLU <- irrHA_frac <-
      irr.harvarea <- rfd_share <- rfd.harvarea <- prod.irr <- irr.yld <-
      prod.rfd <- rfd.yld <- GCAM_subsector <- NULL     # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU <- get_data(all_data, "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU", strip_attributes = TRUE)
    L161.ag_irrProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_irrProd_Mt_R_C_Y_GLU", strip_attributes = TRUE)
    L161.ag_rfdProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_rfdProd_Mt_R_C_Y_GLU", strip_attributes = TRUE)
    L161.ag_irrHA_frac_R_C_GLU <- get_data(all_data, "L161.ag_irrHA_frac_R_C_GLU", strip_attributes = TRUE)

    # First, calculate the share of irrigated vs. rainfed cropland in the base year by GCAM region, commodity and GLU.
    L161.ag_irrHA_frac_R_C_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, irrHA_frac) %>%
      # Get the share of rainfed cropland
      mutate(rfd_share = 1 - irrHA_frac) ->
      L171.ag_irrHA_frac_R_C_GLU

    # Second, downscale total harvested cropland to irrigated and rainfed by GCAM region, commodity, year and GLU.
    # Apply the ~2000-era share of irrigated vs. rainfed cropland to all historial periods (due to lack of data indicating otherwise).
    # Use left_join to allow missing values (for areas like islands excluded from MIRCA inventory) where production is assumed 100% rainfed
    IrrRfdCropland <-
      L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      left_join(L171.ag_irrHA_frac_R_C_GLU,
                               by = c("GCAM_region_ID", "GCAM_commodity",
                                      "GCAM_subsector", "GLU")) %>%
      mutate(irrHA_frac = if_else(is.na(irrHA_frac), 0, irrHA_frac),
             rfd_share = if_else(is.na(rfd_share), 1, rfd_share),
             irr.harvarea = value * irrHA_frac,
             rfd.harvarea = value * rfd_share)

    ## Extend to cover all years 1700-2010.
    idvars <- c('GCAM_region_ID', 'GCAM_commodity', 'GCAM_subsector', 'GLU', 'year')
    allyr <- seq(min(HISTORICAL_YEARS), max(HISTORICAL_YEARS))
    IrrRfdCropland %>%
      tidyr::expand(nesting(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU),
                    year = allyr) %>%
      left_join(IrrRfdCropland, by=idvars) %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%
      mutate(irr.harvarea = approx_fun(year, irr.harvarea, rule = 2),
             rfd.harvarea = approx_fun(year, rfd.harvarea, rule = 2)) %>%
      ungroup -> IrrRfdCropland.interp

    ## Compute economic yield for each category as production divided by
    ## harvested area.
    prod.both <-
      full_join(rename(L161.ag_rfdProd_Mt_R_C_Y_GLU, prod.rfd = value),
                rename(L161.ag_irrProd_Mt_R_C_Y_GLU, prod.irr = value),
                by = idvars) %>%
      filter(year %in% c(HISTORICAL_YEARS, FUTURE_YEARS))

    ecyield.both <- left_join_error_no_match(prod.both, IrrRfdCropland.interp,
                                             by = idvars) %>%
      mutate(irr.yld = prod.irr / irr.harvarea,
             irr.yld = replace(irr.yld, is.na(irr.yld), 0.0),
             rfd.yld = prod.rfd / rfd.harvarea,
             rfd.yld = replace(rfd.yld, is.na(rfd.yld), 0.0))


    ## Produce outputs.  Note that for harvested area, we use the values without
    ## interpolation, stored in IrrRfdCropland (as opposed to IrrRfdCropland.interp)
    select(IrrRfdCropland, GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year,
           value = irr.harvarea) %>%
      add_title("Irrigated harvested cropland cover by GCAM region / commodity / year / GLU") %>%
      add_units("bm2") %>%
      add_comments("Irrigated cropland cover is downscaled from total harvested cropland by GCAM region / commodity / year / GLU.") %>%
      add_comments("The share of irrigated cropland in the base year is applied to all historical periods.") %>%
      add_legacy_name("L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU") %>%
      add_precursors("L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
                     "L161.ag_irrHA_frac_R_C_GLU") ->
      L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU

    select(IrrRfdCropland, GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year,
           value = rfd.harvarea) %>%
      add_title("Rainfed harvested cropland cover by GCAM region / commodity / year / GLU") %>%
      add_units("bm2") %>%
      add_comments("Rainfed cropland cover is downscaled from total harvested cropland by GCAM region / commodity / year / GLU.") %>%
      add_comments("The share of rainfed cropland in the base year is applied to all historical periods.") %>%
      add_legacy_name("L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU") %>%
      add_precursors("L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
                     "L161.ag_irrHA_frac_R_C_GLU") ->
      L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU

    select(ecyield.both, GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, value =
             irr.yld) %>%
      add_title("Adjusted economic yield for irrigated crops by GCAM region / commodity / year / GLU") %>%
      add_units("kg/m2") %>%
      add_comments("Adjusted economic yield for irrigated crops are calculated as irrigated crop production devided by irrigated cropland cover.") %>%
      add_legacy_name("L171.ag_irrEcYield_kgm2_R_C_Y_GLU") %>%
      add_precursors("L161.ag_irrProd_Mt_R_C_Y_GLU") ->
      L171.ag_irrEcYield_kgm2_R_C_Y_GLU

    select(ecyield.both, GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, value =
             rfd.yld) %>%
      add_title("Adjusted economic yield for rainfed crops by GCAM region / commodity / year / GLU") %>%
      add_units("kg/m2") %>%
      add_comments("Adjusted economic yield for rainfed crops are calculated as rainfed crop production devided by rainfed cropland cover.") %>%
      add_legacy_name("L171.ag_rfdEcYield_kgm2_R_C_Y_GLU") %>%
      add_precursors("L161.ag_rfdProd_Mt_R_C_Y_GLU") ->
      L171.ag_rfdEcYield_kgm2_R_C_Y_GLU

    return_data(L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU, L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU, L171.ag_irrEcYield_kgm2_R_C_Y_GLU, L171.ag_rfdEcYield_kgm2_R_C_Y_GLU)
  } else {
    stop("Unknown command")
  }
}
