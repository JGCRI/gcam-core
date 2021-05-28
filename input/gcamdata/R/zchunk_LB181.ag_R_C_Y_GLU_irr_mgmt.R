# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LB181.ag_R_C_Y_GLU_irr_mgmt
#'
#' Calculates the economic yields, cropland cover and production by GCAM region / commodity / year / GLU / irrigation / mgmt level.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L181.LC_bm2_R_C_Yh_GLU_irr_level}, \code{L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level}, \code{L181.ag_Prod_Mt_R_C_Y_GLU_irr_level}, \code{L181.YieldMult_R_bio_GLU_irr}, \code{L181.LandShare_R_bio_GLU_irr}. The corresponding file in the
#' original data system was \code{LB181.ag_R_C_Y_GLU_irr_mgmt.R} (aglu level1).
#' @details This chunk calulates the economic yields, cropland cover and production by GCAM region / commodity / year / GLU / irrigation / mgmt level.
#' Currently the yield multipliers by high and low yield management are set at the same value for all region / commodity / year / GLU / irrigation,
#' and the land share by high and low yield management is 50 percent by each. But this chunk is also a placeholder for a generic method of calculating specific
#' yield mutipliers and land shares for each region / commodity / GLU / irrigation level.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter left_join mutate select
#' @importFrom tidyr gather
#' @author RC May 2017
module_aglu_LB181.ag_R_C_Y_GLU_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU",
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

    EcYield_kgm2_hi <- EcYield_kgm2_lo <- GCAM_commodity <- GCAM_region_ID <- GLU <-
      Irr_Rfd <- LC_bm2_hi <- LC_bm2_lo <- landshare_hi <- landshare_lo <- level <- value <-
      year <- yield <- yieldmult_hi <- yieldmult_lo <- GCAM_subsector <- NULL  # silence package check notes

    # Load required inputs
    L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU <- get_data(all_data, "L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU")
    L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU <- get_data(all_data, "L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU")
    L171.ag_irrEcYield_kgm2_R_C_Y_GLU <- get_data(all_data, "L171.ag_irrEcYield_kgm2_R_C_Y_GLU")
    L171.ag_rfdEcYield_kgm2_R_C_Y_GLU <- get_data(all_data, "L171.ag_rfdEcYield_kgm2_R_C_Y_GLU")

    # In order to calculate weighted yield levels for aggregation, we don't want to be using the raw yields, as our
    # GCAM commodities may include a blend of heterogeneous yielding commodities. For example, cucumber yields are in
    # excess of 400 tonnes/hectare in some places, whereas pulses tend to be about 2. In a non-indexed aggregation,
    # the cucumbers would be the only crop that matters for the final yields, and the yield of the "high" technology
    # would not be representative of a biophysically attainable yield for the commodity class as a whole.

    # Therefore, apply yield multipliers to the baseline historical economic yields.
    # Multipliers are applied to economic yields (kg/m2/yr, not kg/m2/harvest), and shares are applied to land areas.

    # First, calculate the new EcYields as the former yields times the yield mults, for high and low
    L171.ag_rfdEcYield_kgm2_R_C_Y_GLU %>%
      mutate(Irr_Rfd = "rfd") %>%
      # Combine rainfed and irrigated data
      bind_rows(mutate(L171.ag_irrEcYield_kgm2_R_C_Y_GLU, Irr_Rfd = "irr")) %>%
      filter(year %in% aglu.AGLU_HISTORICAL_YEARS) %>%
      # SET THE SAME YIELD MULTIPLIERS EVERYWHERE, 1 plus or minus an adj fraction.
      mutate(yieldmult_hi = 1 + aglu.MGMT_YIELD_ADJ, yieldmult_lo = 1 - aglu.MGMT_YIELD_ADJ,
             # high and low yields are now calculated as the observed yield times the multipliers
             EcYield_kgm2_lo = value * yieldmult_lo, EcYield_kgm2_hi = value * yieldmult_hi) %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, Irr_Rfd, year, EcYield_kgm2_hi, EcYield_kgm2_lo) %>%
      gather(level, value, -GCAM_region_ID, -GCAM_commodity, -GCAM_subsector, -GLU, -Irr_Rfd, -year) %>%
      mutate(level = sub("EcYield_kgm2_", "", level)) ->
      L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level

    # Second, apply land shares to disaggregate low- and high-input land
    L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU %>%
      mutate(Irr_Rfd = "rfd") %>%
      # Combine rainfed and irrigated data
      bind_rows(mutate(L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU, Irr_Rfd = "irr")) %>%
      filter(year %in% aglu.LAND_COVER_YEARS) %>%
      # SET THE SAME YIELD MULTIPLIERS EVERYWHERE, 1 plus or minus an adj fraction.
      mutate(yieldmult_hi = 1 + aglu.MGMT_YIELD_ADJ, yieldmult_lo = 1 - aglu.MGMT_YIELD_ADJ,
             # Calculate the land shares to allocate to low, and high is the rest (currently the shares are set at 0.5/0.5 to all)
             landshare_lo = (1 - yieldmult_hi) / (yieldmult_lo - yieldmult_hi), landshare_hi = 1 - landshare_lo,
             # low- and high-input land are calculated as the total times the shares
             LC_bm2_lo = value * landshare_lo, LC_bm2_hi = value * landshare_hi) %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, Irr_Rfd, year, LC_bm2_hi, LC_bm2_lo) %>%
      gather(level, value, -GCAM_region_ID, -GCAM_commodity, -GCAM_subsector, -GLU, -Irr_Rfd, -year) %>%
      mutate(level = sub("LC_bm2_", "", level)) ->
      L181.LC_bm2_R_C_Yh_GLU_irr_level

    # Third, calculate production: economic yield times land area
    L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level %>%
      rename(yield = value) %>%
      left_join_error_no_match(L181.LC_bm2_R_C_Yh_GLU_irr_level,
                               by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector",
                                      "GLU", "Irr_Rfd", "year", "level")) %>%
      # apply land area rounding (and cutoff) for production consistency
      mutate(value = round(value, digits = aglu.DIGITS_LAND_USE) * yield) %>%
      select(-yield) ->
      L181.ag_Prod_Mt_R_C_Y_GLU_irr_level

    # Calculate bioenergy yield levels
    L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level %>%
      # Only where production (and harvested area) is non-zero
      filter(value > 0) %>%
      select(GCAM_region_ID, GLU, Irr_Rfd) %>%
      unique() %>%
      # SET THE SAME YIELD MULTIPLIERS EVERYWHERE
      mutate(yieldmult_hi = 1 + aglu.MGMT_YIELD_ADJ, yieldmult_lo = 1 - aglu.MGMT_YIELD_ADJ) ->
      L181.YieldMult_R_bio_GLU_irr

    # Calculate bioenergy land shares
    L181.LC_bm2_R_C_Yh_GLU_irr_level %>%
      filter(value > 0) %>%
      select(GCAM_region_ID, GLU, Irr_Rfd) %>%
      unique() %>%
      # SET THE SAME LADN SHARE
      mutate(landshare_lo = 0.5, landshare_hi = 0.5) ->
      L181.LandShare_R_bio_GLU_irr

    # Produce outputs
    L181.LC_bm2_R_C_Yh_GLU_irr_level %>%
      add_title("Cropland cover by GCAM region / commodity / year / GLU / irrigation / mgmt level") %>%
      add_units("bm2") %>%
      add_comments("Cropland cover by high and low management levels are currently set at the share of 50% by each.") %>%
      add_legacy_name("L181.LC_bm2_R_C_Yh_GLU_irr_level") %>%
      add_precursors("L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU",
                     "L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU") ->
      L181.LC_bm2_R_C_Yh_GLU_irr_level

    L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level %>%
      add_title("Economic yield by GCAM region / commodity / year / GLU / irrigation / mgmt level") %>%
      add_units("kg/bm2") %>%
      add_comments("Economic yields are calculated as the observed yields times a mutiplier for high or low mgmt level.") %>%
      add_comments("Currently the same yield mutipliers are set for all region/commodity/GLU/irrigation.") %>%
      add_legacy_name("L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level") %>%
      add_precursors("L171.ag_irrEcYield_kgm2_R_C_Y_GLU",
                     "L171.ag_rfdEcYield_kgm2_R_C_Y_GLU") ->
      L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level

    L181.ag_Prod_Mt_R_C_Y_GLU_irr_level %>%
      add_title("Agricultural production by GCAM region / commodity / year / GLU / irrigation / mgmt level") %>%
      add_units("Mt") %>%
      add_comments("Agricultural production are calculated as the economic yield times cropland cover.") %>%
      add_legacy_name("L181.ag_Prod_Mt_R_C_Y_GLU_irr_level") %>%
      add_precursors()  ->
      L181.ag_Prod_Mt_R_C_Y_GLU_irr_level

    L181.YieldMult_R_bio_GLU_irr %>%
      add_title("Yield multipliers for bioenergy by region / GLU / irrigation / mgmt level") %>%
      add_units("Unitless") %>%
      add_comments("Yield mutipliers for bioenergy high and low management level are set at the same values for all region/commodity/GLU/irrigation.") %>%
      add_legacy_name("L181.YieldMult_R_bio_GLU_irr") %>%
      add_precursors() ->
      L181.YieldMult_R_bio_GLU_irr

    L181.LandShare_R_bio_GLU_irr %>%
      add_title("Ghost land shares for bioenergy by region / GLU / irrigation / mgmt level") %>%
      add_units("Unitless") %>%
      add_comments("Ghost land shares for bioenergy by high and low management levels are currently set at the share of 50% by each.") %>%
      add_legacy_name("L181.LandShare_R_bio_GLU_irr") %>%
      add_precursors() ->
      L181.LandShare_R_bio_GLU_irr

    return_data(L181.LC_bm2_R_C_Yh_GLU_irr_level, L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level, L181.ag_Prod_Mt_R_C_Y_GLU_irr_level, L181.YieldMult_R_bio_GLU_irr, L181.LandShare_R_bio_GLU_irr)
  } else {
    stop("Unknown command")
  }
}
