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
module_aglu_LB181.ag_R_C_Y_GLU_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/LDS/Mueller_yield_levels",
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
    Mueller_yield_levels <- get_data(all_data, "aglu/LDS/Mueller_yield_levels")
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

    # Yield multiplier that goes from the observed yield to the "hi" and "lo" yields
    # observed plus or minus observed times this number
    MGMT_YIELD_ADJ <- 0.1
    MIN_YIELD_ADJ <- 0.05
    MAX_BIO_MULTI_HI <- 3
    
    # Combine country level irrigated and rainfed files
    # Harvest area by ctry / GLU / crop / irr
    L151.ag_irrHA_ha_ctry_crop %>%
      ungroup() %>%
      left_join_error_no_match(L151.ag_rfdHA_ha_ctry_crop, by = c("iso", "GLU", "GTAP_crop")) %>%
      gather(Irr_Rfd, HA_ha, -iso, -GTAP_crop, -GLU) %>%
      mutate(Irr_Rfd = sub("HA", "", Irr_Rfd)) ->
      L181.ag_HA_ha_ctry_crop_irr

    # Production by ctry / GLU / crop / irr
    L151.ag_irrProd_t_ctry_crop %>%
      ungroup() %>%
      left_join_error_no_match(L151.ag_rfdProd_t_ctry_crop, by = c("iso", "GLU", "GTAP_crop")) %>%
      gather(Irr_Rfd, Prod_t, -iso, -GTAP_crop, -GLU) %>%
      mutate(Irr_Rfd = sub("Prod", "", Irr_Rfd)) ->
      L181.ag_Prod_t_ctry_crop_irr

    # Calulate yields by ctry / GLU / crop / irr, as production divided by harvest area
    L181.ag_HA_ha_ctry_crop_irr %>%
      left_join_error_no_match(L181.ag_Prod_t_ctry_crop_irr, by = c("iso", "GLU", "GTAP_crop", "Irr_Rfd")) %>%
      mutate(yield_tha = Prod_t / HA_ha ) ->
      L181.ag_Yield_tha_ctry_crop_irr

    # Match in Mueller database low and high yields for each available crop and country
    # First prepare the data for matching in
    Mueller_yield_levels %>%
      na.omit(  ) %>%
      rename(iso = Country) %>%
      filter(iso != "mne") %>%                                   # Drop mne
      mutate(iso = replace(iso, iso == "srb", "scg"),            # Replace Serbia iso from srb to scg
             iso = replace(iso, iso == "twn", "chn"),            # Taiwan (re-set to China)
             GLU = paste0("GLU", sprintf("%03d", Basin))) %>%    # GLU ID
      left_join_error_no_match(Muller_crops, by = "crop") ->     # Match Mueller crop
      L181.Mueller_yield_levels

    # Separate low yields - 2nd percentile
    L181.Mueller_yield_levels %>%
      filter(yield_level == "_02ndpercentileyield") ->
      L181.Mueller_yield_levels_lo

    # Separate high yields - 95th percentile, and rainfed yield ceilings
    L181.Mueller_yield_levels %>%
      filter(yield_level %in% c("_95thpercentileyield", "_rainfedyieldceilings"))%>%
      mutate(Irr_Rfd = "irr",
             Irr_Rfd = replace(Irr_Rfd, yield_level == "_rainfedyieldceilings", "rfd")) ->
      L181.Mueller_yield_levels_hi

    # Match low and high yields by crop / ctry / GLU / irr, adjust where the observed yields were not within the bound
    L181.ag_Yield_tha_ctry_crop_irr %>%
      # Subset only the crops, countries, and GLUs from the GTAP database that are represented in the Mueller data
      semi_join(L181.Mueller_yield_levels, by = c("iso", "GTAP_crop", "GLU")) %>%
      # Only use data where production (and harvested area) is non-zero
      filter(HA_ha > 0) %>%
      # Match in Mueller's low yields, use the 2nd percentile average to estimate the "lower" yielding technology
      left_join_keep_first_only(select(L181.Mueller_yield_levels_lo, iso, GTAP_crop, GLU, average), by = c("iso", "GTAP_crop", "GLU")) %>%
      rename(lo = average) %>%
      # Where Muller's 2nd percentile averages are higher than observed yields, use observed times an adjustment fraction
      mutate(lo = replace(lo, lo > yield_tha * (1 - MIN_YIELD_ADJ), yield_tha[lo > yield_tha * (1 - MIN_YIELD_ADJ)] * (1 - MIN_YIELD_ADJ))) %>%
      # Match in Mueller's high yields, use the 95th percentile average to estimate the "higher" yielding technology
      left_join_keep_first_only(select(L181.Mueller_yield_levels_hi, iso, GTAP_crop, GLU, Irr_Rfd, average),
                                by = c("iso", "GTAP_crop", "Irr_Rfd", "GLU")) %>%
      rename(hi = average) ->
      L181.Mueller_ag_Yield_tha_irr

    # Many of the crops that have no rainfed yield ceiling information available.
    # Use the high irrigated yields - 95th percentile, de-rated by some fraction, for ctry/GLU/crops where rainfed yield ceilings were not reported.
    # First calculate an irrigated:rainfed maximum yield derating factor.
    L181.Mueller_ag_Yield_tha_irr %>%
      filter(Irr_Rfd == "irr") %>%     # Get irrigated yields
      rename(hi_irr = hi) %>%          # Get high irrigated yields
      mutate(Irr_Rfd = "rfd") %>%      # Change the category to rainfed, to match the rainfed yield ceiling data
      left_join_keep_first_only(select(L181.Mueller_yield_levels_hi, iso, GTAP_crop, GLU, Irr_Rfd, average),
                                by = c("iso", "GTAP_crop", "Irr_Rfd", "GLU")) %>%
      rename(hi_rfd = average) %>%
      # Use harvest area to weight the rainfed to irrigated high yields ratio
      mutate(wt_derating = HA_ha * hi_rfd / hi_irr) %>%
      na.omit() %>%
      group_by(iso, GLU) %>%
      summarise(HA_ha = sum(HA_ha), wt_derating= sum(wt_derating)) %>%
      ungroup %>%
      # Calculate the area-weighted irrigated to rainfed yield derating factor by ctry / GLU
      mutate(derating = wt_derating / HA_ha) ->
      L181.RfdDerating_ctry_GLU

    # Use the derating factors and high yields to calculate the rainfel yield ceilings for missing values
    L181.Mueller_ag_Yield_tha_irr %>%
      # Join the derating factors, which creates NA, use left_join instead of left_join_error_no_match
      left_join(select(L181.RfdDerating_ctry_GLU, iso, GLU, derating), by = c("iso", "GLU")) %>%
      left_join_keep_first_only(select(L181.Mueller_yield_levels_hi, iso, GTAP_crop, GLU, average), by = c("iso", "GTAP_crop", "GLU")) %>%
      # Re-set derating factor to 1 for irrigated crops, just to make sure that none of these get derated
      mutate(derating = replace(derating, Irr_Rfd == "irr", 1),
             # Set missing derating factors to zero. These will be over-written later
             derating = replace(derating, is.na(derating), 0),
             # When rainfel yield ceilings,
             hi = replace(hi, is.na(hi), average[is.na(hi)] * derating[is.na(hi)]),
             # Where Muller's 95th percentile / rainfed ceiling averages are less than observed yields, use observed times (1+adjustment fraction)
             hi = replace(hi, hi < (yield_tha * (1 + MIN_YIELD_ADJ)), yield_tha[hi < yield_tha * (1 + MIN_YIELD_ADJ)] * (1 + MIN_YIELD_ADJ))) %>%
      select(-derating, -average) ->
      L181.Mueller_ag_Yield_tha_irr

    # Calculate lo and hi yields of crops and regions in the GTAP database that aren't represented in the Mueller database
    # Apply a generic functional form in order to get "hi" yields of crops not covered by Mueller
    # Separately subset the crops that are not in the Muller data. Yield multipliers and land shares will be assigned from the above calc
    L181.ag_Yield_tha_ctry_crop_irr %>%
      anti_join(L181.Mueller_yield_levels, by = c("iso", "GTAP_crop", "GLU")) %>%
      filter(HA_ha > 0) ->
      L181.noMueller_ag_Yield_tha_irr

    # First step is to figure out, for the 17 available crops, how far each land use region's 95th percentile yields
    # are from the global maximum 95th percentile yields. This gives a climate-based index of yields that can be
    # applied to the global 95th percentile yields of each of the non-covered crops
    L181.Mueller_ag_Yield_tha_irr %>%
      group_by(GTAP_crop, Irr_Rfd) %>%
      summarise(maxYield = max(hi)) ->
      L181.Mueller_YieldMax_irr

    L181.Mueller_ag_Yield_tha_irr %>%
      left_join_error_no_match(L181.Mueller_YieldMax_irr, by = c("GTAP_crop", "Irr_Rfd")) %>%
      mutate(wt_YieldIndex = hi * HA_ha / maxYield) %>%
      group_by(iso, GLU, Irr_Rfd) %>%
      summarise(HA_ha = sum(HA_ha), wt_YieldIndex = sum(wt_YieldIndex)) %>%
      ungroup %>%
      mutate(YieldIndex = wt_YieldIndex / HA_ha) ->
      L181.YieldIndex_ctry_GLU_irr

    # Next, figure out the 95th percentile observed yield across all regions and agricultural regions (intersection of country and GLU)
    # Using 95th percentile because many of the crops have absurdly high maximum values (e.g., cucumbers with >500t/ha)
    # Final "hi" yields will be max of( ( average of 95th percentile and observed ) and ( observed plus min-yield-adj ) )
    L181.noMueller_ag_Yield_tha_irr %>%
      group_by(GTAP_crop) %>%
      summarise(maxYield_C = quantile(yield_tha, probs = 0.95)) ->
      L181.noMueller_maxYield_crop

    L181.noMueller_ag_Yield_tha_irr %>%
      mutate(lo = yield_tha / 2) %>%
      left_join_error_no_match(L181.noMueller_maxYield_crop, by = "GTAP_crop") %>%
      # Not all countries, basins, and irrigation levels are necessarily represented in the yield index data
      left_join(select(L181.YieldIndex_ctry_GLU_irr, -wt_YieldIndex, -HA_ha), by = c("iso", "GLU", "Irr_Rfd")) %>%
      # Just set them to zero, and the yields will be re-set to the observed plus the min adjustment factor
      replace_na(list(YieldIndex = 0)) %>%
      mutate(hi = pmax(maxYield_C * YieldIndex, yield_tha * (1 + MIN_YIELD_ADJ))) %>%
      select(-maxYield_C, -YieldIndex) %>%
      # Bind this back in with the dataset that only has Muller crops
      bind_rows(L181.Mueller_ag_Yield_tha_irr) ->
      L181.ag_Yield_tha_ctry_crop_irr_mgmt

    # In order to calculate weighted yield levels for aggregation, we don't want to be using the raw yields, as our
    # GCAM commodities may include a blend of heterogeneous yielding commodities. For example, cucumber yields are in
    # excess of 400 tonnes/hectare in some places, whereas pulses tend to be about 2. In a non-indexed aggregation,
    # the cucumbers would be the only crop that matters for the final yields, and the yield of the "high" technology
    # would not be representative of a biophysically attainable yield for the commodity class as a whole. Instead,
    # each crop's low and high yields are simply indexed to the observed yield, and these multipliers are weighted
    # by harvested area and aggregated.

    # Calculating multipliers from observed to lo and to hi yields, in order to aggregate by GCAM regions and commodities
    L181.ag_Yield_tha_ctry_crop_irr_mgmt %>%
      mutate(wt_yieldmult_lo = (lo / yield_tha) * HA_ha, wt_yieldmult_hi = (hi / yield_tha) * HA_ha) %>%
      #This is now ready to have GCAM commodities and regions matched in, and to then be aggregated, weighted by production (observed, lo, and hi)
      left_join_error_no_match(select(iso_GCAM_regID, GCAM_region_ID, iso), by = "iso") %>%
      left_join(select(FAO_ag_items_PRODSTAT, GCAM_commodity, GTAP_crop), by = "GTAP_crop") %>%
      filter(!is.na(GCAM_commodity)) %>%
      group_by(GCAM_region_ID, GCAM_commodity, GLU, Irr_Rfd) %>%
      summarise(HA_ha = sum(HA_ha), wt_yieldmult_lo = sum(wt_yieldmult_lo), wt_yieldmult_hi = sum(wt_yieldmult_hi)) %>%
      ungroup %>%
      mutate(yieldmult_lo = wt_yieldmult_lo / HA_ha, yieldmult_hi = wt_yieldmult_hi / HA_ha) ->
      L181.YieldLevels_R_C_GLU_irr

    # Method two: HACK TO SET THE SAME YIELD MULTIPLIERS EVERYWHERE
    # first, set the yield multiplier that goes from the observed to the "hi". "lo" will be the reciprocal of this
    L181.YieldLevels_R_C_GLU_irr %>%
      mutate(yieldmult_lo = 1 - MGMT_YIELD_ADJ, yieldmult_hi = 1 + MGMT_YIELD_ADJ) ->
      L181.YieldLevels_R_C_GLU_irr

    # Applying yield multipliers to the baseline historical economic yields
    # Multipliers are applied to economic yields (kg/m2/yr, not kg/m2/harvest), and shares are applied to land areas.
    # Production is calculated as land area times yield
    # First, calculate the new EcYields as the former yields times the yield mults, for hi and lo
    # EcYields are done first because a feasibility check will re-write some of the multipliers
    L171.ag_rfdEcYield_kgm2_R_C_Y_GLU %>%
      mutate(Irr_Rfd = "rfd") %>%
      bind_rows(mutate(L171.ag_irrEcYield_kgm2_R_C_Y_GLU, Irr_Rfd = "irr")) %>%
      filter(year %in% AGLU_HISTORICAL_YEARS) %>%
      # Match in the multipliers for the commodities that are matched
      left_join(select(L181.YieldLevels_R_C_GLU_irr, -wt_yieldmult_hi, -wt_yieldmult_lo, -HA_ha),
                by = c("GCAM_region_ID", "GCAM_commodity", "GLU", "Irr_Rfd")) %>%
      # Where land allocation and therefore yields are zero, set the multipliers to zero as well
      mutate(yieldmult_lo = replace(yieldmult_lo, value == 0, 0), yieldmult_hi = replace(yieldmult_hi, value == 0, 0),
             # Any remaining missing values would be minor combinations of region / crop / GLU / irrigation.
             # These would be included in Monfreda/LDS/FAO/MIRCA, for commodities considered by Mueller, but not in the final Mueller aggregation
             # Set the multipliers to 1, no missing currently 05/17
             yieldmult_lo = replace(yieldmult_lo, is.na(yieldmult_lo), 1), yieldmult_hi = replace(yieldmult_hi, is.na(yieldmult_lo), 1),
             # Hi and lo yields are now calculated as the observed yield times the multipliers
             EcYield_kgm2_lo = value * yieldmult_lo, EcYield_kgm2_hi = value * yieldmult_hi) ->
      L181.ag_EcYield_kgm2_R_C_Y_GLU_irr

    # Calculating land shares to each technology in to return correct average yields
    # Calculate the land shares to allocate to lo and hi
    L181.YieldLevels_R_C_GLU_irr %>%
      mutate(landshare_lo = (1 - yieldmult_hi) / (yieldmult_lo - yieldmult_hi), landshare_hi = 1 - landshare_lo) ->
      L181.YieldLevels_R_C_GLU_irr

    # Applying land shares to disaggregate lo- and hi-input land
    L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU %>%
      mutate(Irr_Rfd = "rfd") %>%
      bind_rows(mutate(L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU, Irr_Rfd = "irr")) %>%
      filter(year %in% aglu.LAND_COVER_YEARS) %>%
      # Match in the shares for the commodities that are matched
      left_join(select(L181.YieldLevels_R_C_GLU_irr, GCAM_region_ID, GCAM_commodity, GLU, Irr_Rfd, landshare_lo, landshare_hi),
                               by = c("GCAM_region_ID", "GCAM_commodity", "GLU", "Irr_Rfd")) %>%
      # Where land allocation is zero, set the shares to zero as well
      mutate(landshare_lo = replace(landshare_lo, value == 0, 0), landshare_hi = replace(landshare_hi, value == 0, 0),
             # As above, any remaining missing values would be minor combinations of region / crop / GLU / irrigation.
             # Leaving this step here in case observations are included in Monfreda/LDS/FAO/MIRCA crop data, for commodities considered by Mueller,
             # but not reported in the Mueller aggregation
             # Set the shares to 0.5 hi/lo, as no further information is available
             landshare_lo = replace(landshare_lo, is.na(landshare_lo), 0.5), landshare_hi = replace(landshare_hi, is.na(landshare_lo), 0.5),
             LC_bm2_lo = value * landshare_lo, LC_bm2_hi = value * landshare_hi) ->
      L181.LC_bm2_R_C_Yh_GLU_irr

    # Land: cast into two dataframes and bind together to be written out
    L181.LC_bm2_R_C_Yh_GLU_irr %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, Irr_Rfd, year, LC_bm2_hi, LC_bm2_lo) %>%
      gather(level, value, -GCAM_region_ID, -GCAM_commodity, -GLU, -Irr_Rfd, -year) %>%
      mutate(level = sub("LC_bm2_", "", level)) ->
      L181.LC_bm2_R_C_Yh_GLU_irr_level

    # Yields: cast into two dataframes and bind together to be written out
    L181.ag_EcYield_kgm2_R_C_Y_GLU_irr %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, Irr_Rfd, year, EcYield_kgm2_hi, EcYield_kgm2_lo) %>%
      gather(level, value, -GCAM_region_ID, -GCAM_commodity, -GLU, -Irr_Rfd, -year) %>%
      mutate(level = sub("EcYield_kgm2_", "", level)) ->
      L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level

    # Calculate production: economic yield times land area
    L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level %>%
      rename(yield = value) %>%
      left_join_error_no_match(L181.LC_bm2_R_C_Yh_GLU_irr_level,
                               by = c("GCAM_region_ID", "GCAM_commodity", "GLU", "Irr_Rfd", "year", "level")) %>%
      mutate(value = yield * value) %>%
      select(-yield) ->
      L181.ag_Prod_Mt_R_C_Y_GLU_irr_level

    # Calculating bioenergy yield levels
    # Method one:
    # For bioenergy yields, calculate a generic weighted lo, observed, and hi yield across all crops
    L181.YieldLevels_R_C_GLU_irr %>%
      group_by(GCAM_region_ID, GLU, Irr_Rfd) %>%
      summarise(HA_ha = sum(HA_ha), wt_yieldmult_lo = sum(wt_yieldmult_lo), wt_yieldmult_hi = sum(wt_yieldmult_hi)) %>%
      ungroup %>%
      mutate(yieldmult_lo = wt_yieldmult_lo / HA_ha, yieldmult_hi = wt_yieldmult_hi / HA_ha,
             # don't let the bioenergy "hi" yield multipliers exceed some exogenous threshold
             yieldmult_hi = pmin(yieldmult_hi, MAX_BIO_MULTI_HI)) ->
      L181.YieldLevels_R_GLU_irr

    # Method two: APPLYING HACK TO SET THE SAME YIELD MULTIPLIERS EVERYWHERE
    L181.YieldLevels_R_GLU_irr %>%
      mutate(yieldmult_hi = 1 + MGMT_YIELD_ADJ, yieldmult_lo = 1 - MGMT_YIELD_ADJ) %>%
      select(-wt_yieldmult_lo, -wt_yieldmult_hi, -HA_ha) ->
      L181.YieldMult_R_bio_GLU_irr

    # Calculating bioenergy land shares
    # For bioenergy ghost shares, write out the table of land shares
    L181.YieldLevels_R_GLU_irr %>%
      mutate(landshare_lo = (1 - yieldmult_hi) / (yieldmult_lo - yieldmult_hi), landshare_hi = 1 - landshare_lo) %>%
      select(GCAM_region_ID, GLU, Irr_Rfd, landshare_lo, landshare_hi) ->
    L181.LandShare_R_bio_GLU_irr

    # Method two: HACK
    L181.LandShare_R_bio_GLU_irr %>%
      mutate(landshare_lo = 0.5, landshare_hi = 0.5) ->
      L181.LandShare_R_bio_GLU_irr

    # Produce outputs
    L181.LC_bm2_R_C_Yh_GLU_irr_level %>%
      add_title("Cropland cover by GCAM region / commodity / year / GLU / irrigation / mgmt level") %>%
      add_units("bm2") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L181.LC_bm2_R_C_Yh_GLU_irr_level") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/LDS/Mueller_yield_levels",
                     "aglu/Muller_crops",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "L151.ag_irrHA_ha_ctry_crop",
                     "L151.ag_rfdHA_ha_ctry_crop",
                     "L151.ag_irrProd_t_ctry_crop",
                     "L151.ag_rfdProd_t_ctry_crop",
                     "L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU",
                     "L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L181.LC_bm2_R_C_Yh_GLU_irr_level

    L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level %>%
      add_title("Economic yield by GCAM region / commodity / year / GLU / irrigation / mgmt level") %>%
      add_units("kg/bm2") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/LDS/Mueller_yield_levels",
                     "aglu/Muller_crops",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "L151.ag_irrHA_ha_ctry_crop",
                     "L151.ag_rfdHA_ha_ctry_crop",
                     "L151.ag_irrProd_t_ctry_crop",
                     "L151.ag_rfdProd_t_ctry_crop",
                     "L171.ag_irrEcYield_kgm2_R_C_Y_GLU",
                     "L171.ag_rfdEcYield_kgm2_R_C_Y_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level

    L181.ag_Prod_Mt_R_C_Y_GLU_irr_level %>%
      add_title("Agricultural production by GCAM region / commodity / year / GLU / irrigation / mgmt level") %>%
      add_units("Mt") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L181.ag_Prod_Mt_R_C_Y_GLU_irr_level") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/LDS/Mueller_yield_levels",
                     "aglu/Muller_crops",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "L151.ag_irrHA_ha_ctry_crop",
                     "L151.ag_rfdHA_ha_ctry_crop",
                     "L151.ag_irrProd_t_ctry_crop",
                     "L151.ag_rfdProd_t_ctry_crop",
                     "L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU",
                     "L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU",
                     "L171.ag_irrEcYield_kgm2_R_C_Y_GLU",
                     "L171.ag_rfdEcYield_kgm2_R_C_Y_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L181.ag_Prod_Mt_R_C_Y_GLU_irr_level

    L181.YieldMult_R_bio_GLU_irr %>%
      add_title("Yield multipliers for bioenergy by region / GLU / irrigation / mgmt level") %>%
      add_units("Unitless") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L181.YieldMult_R_bio_GLU_irr") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/LDS/Mueller_yield_levels",
                     "aglu/Muller_crops",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "L151.ag_irrHA_ha_ctry_crop",
                     "L151.ag_rfdHA_ha_ctry_crop",
                     "L151.ag_irrProd_t_ctry_crop",
                     "L151.ag_rfdProd_t_ctry_crop",
                     "L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU",
                     "L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU",
                     "L171.ag_irrEcYield_kgm2_R_C_Y_GLU",
                     "L171.ag_rfdEcYield_kgm2_R_C_Y_GLU") ->
      L181.YieldMult_R_bio_GLU_irr

    L181.LandShare_R_bio_GLU_irr %>%
      add_title("Ghost land shares for bioenergy by region / GLU / irrigation / mgmt level") %>%
      add_units("Unitless") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L181.LandShare_R_bio_GLU_irr") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/LDS/Mueller_yield_levels",
                     "aglu/Muller_crops",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "L151.ag_irrHA_ha_ctry_crop",
                     "L151.ag_rfdHA_ha_ctry_crop",
                     "L151.ag_irrProd_t_ctry_crop",
                     "L151.ag_rfdProd_t_ctry_crop",
                     "L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU",
                     "L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU",
                     "L171.ag_irrEcYield_kgm2_R_C_Y_GLU",
                     "L171.ag_rfdEcYield_kgm2_R_C_Y_GLU") ->
      L181.LandShare_R_bio_GLU_irr

    return_data(L181.LC_bm2_R_C_Yh_GLU_irr_level, L181.ag_EcYield_kgm2_R_C_Y_GLU_irr_level, L181.ag_Prod_Mt_R_C_Y_GLU_irr_level, L181.YieldMult_R_bio_GLU_irr, L181.LandShare_R_bio_GLU_irr)
  } else {
    stop("Unknown command")
  }
}
