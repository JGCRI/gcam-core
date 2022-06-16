# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LB162.ag_prodchange_R_C_Y_GLU_irr
#'
#' This module calculates the first level production/yield change assumptions that are exogenous to GCAM. These rates are calculated for each commodity
#' at the region-glu-irrigation level in each model year, including the calibration year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L162.ag_YieldRatio_R_C_Ysy_GLU_irr}, \code{L162.ag_YieldRate_R_C_Y_GLU_irr}, \code{L162.bio_YieldRate_R_Y_GLU_irr}. The corresponding file in the
#' original data system was \code{LB162.ag_prodchange_R_C_Y_GLU_irr.R} (aglu level1).
#' @details The CROSIT agriculture database is processed and reconciled with LDS and GCAM data system production and harvested area information at the irrigation
#' level. Yield Ratios are calculated as future year reconciled production / base year reconciled production. The biomass yield ratio in each year is taken to
#' be the median of all other commodities at the region-glu-irrigation level. The yield ratios are used to calculate annual yield rate assumptions
#' (Yield Rate(year i) = Yield Ratio(year i) / Yield ratio(year i-1) ). Externally defined default yield rates are used to fill in missing data at the GCAM
#' region-commodity-glu-irrigation level for all model years.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter first group_by left_join mutate select semi_join summarise
#' @importFrom tidyr complete nesting
#' @importFrom tibble tibble
#' @author ACS June 2017
module_aglu_LB162.ag_prodchange_R_C_Y_GLU_irr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/A_defaultYieldRate",
             FILE = "aglu/AGLU_ctry",
             FILE = "aglu/FAO/FAO_ag_CROSIT",
             FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
             "L151.ag_irrHA_ha_ctry_crop",
             "L151.ag_rfdHA_ha_ctry_crop",
             "L161.ag_irrProd_Mt_R_C_Y_GLU",
             "L161.ag_rfdProd_Mt_R_C_Y_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L162.ag_YieldRatio_R_C_Ysy_GLU_irr",
             "L162.ag_YieldRate_R_C_Y_GLU_irr",
             "L162.bio_YieldRate_R_Y_GLU_irr"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- value <- GCAM_region_ID <- GCAM_commodity <- GLU <- CROSIT_country_ID <- CROSIT_ctry <-
      CROSIT_crop <- CROSIT_cropID <- country_ID <- crop_ID <- Prod_kt_irrigated <- HA_kha_irrigated <-
      Yield_kgHa_irrigated <- Prod_kt_rainfed <- HA_kha_rainfed <- Yield_kgHa_rainfed <- Irr_Rfd <-
      yield_kgHa <- iso <- irrHA <- rfdHA <- GTAP_crop <- HA <- Mult <- Prod_mod <- YieldRatio <-
      timestep <- lagyear <- YieldRatio_lag <- YieldRate <- defaultRate <- GCAM_subsector <- NULL  # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    A_defaultYieldRate <- get_data(all_data, "aglu/A_defaultYieldRate")
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    FAO_ag_CROSIT <- get_data(all_data, "aglu/FAO/FAO_ag_CROSIT")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_items_PRODSTAT")
    L151.ag_irrHA_ha_ctry_crop <- get_data(all_data, "L151.ag_irrHA_ha_ctry_crop")
    L151.ag_rfdHA_ha_ctry_crop <- get_data(all_data, "L151.ag_rfdHA_ha_ctry_crop")
    L161.ag_irrProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_irrProd_Mt_R_C_Y_GLU")
    L161.ag_rfdProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_rfdProd_Mt_R_C_Y_GLU")

    # Perform calculations

    # Prepare CROSIT database by replacing country and crop IDs with names from
    # AGLU_ctry and FAO_ag_items_PRODSTAT respectively. Because these are larger tables
    # with data for multiple uses, a call to dplyr::distinct is used to reduce to only
    # CROSIT-relevant countries and crops. This allows for left_join_error_no_match to
    # be used.
    # Some regions have 0 production but positive harvested area and a non zero yield -
    # use the yields to recalculate production = yield * harvested area when yields are
    # available; when yields are not available, reset harvested area to 0.
    FAO_ag_CROSIT %>%
      left_join_error_no_match(dplyr::distinct(select(AGLU_ctry, CROSIT_country_ID, CROSIT_ctry)),
                               by = c("country_ID" = "CROSIT_country_ID")) %>%
      left_join_error_no_match(dplyr::distinct(select(FAO_ag_items_PRODSTAT, CROSIT_crop, CROSIT_cropID)),
                               by = c("crop_ID" = "CROSIT_cropID")) %>%
      # Process 0 production / nonzero HA and yield cases
      group_by(country_ID, crop_ID, year) %>%
      mutate(Prod_kt_irrigated = replace(Prod_kt_irrigated,
                                         Prod_kt_irrigated == 0 & HA_kha_irrigated != 0,
                                         Yield_kgHa_irrigated * HA_kha_irrigated * CONV_KG_T),
             Prod_kt_rainfed = replace(Prod_kt_rainfed,
                                       Prod_kt_rainfed == 0 & HA_kha_rainfed != 0,
                                       Yield_kgHa_rainfed * HA_kha_rainfed * CONV_KG_T),
             HA_kha_irrigated = replace(HA_kha_irrigated,
                                        Prod_kt_irrigated == 0 & HA_kha_irrigated != 0,
                                        0),
             HA_kha_rainfed = replace(HA_kha_rainfed,
                                      Prod_kt_rainfed == 0 & HA_kha_rainfed != 0,
                                      0)) %>%
      ungroup() ->
      FAO_ag_CROSIT


    # Use the CROSIT database to prepare a table of yields by country, crop, irrigation, and year,
    # and interpolate to fill in all of the specified agricultural production years, aglu.SPEC_AG_PROD_YEARS
    # Finally, Calculate yield multipliers from the base year <=> first year of aglu.SPEC_AG_PROD_YEARS
    # For each country, crop, irrigation, the multiplier for each year is
    # yield in that year / yield in base year.

    # pull off irrigated table of yield, country, crop; append irrigation information
    FAO_ag_CROSIT %>%
      select(CROSIT_ctry, CROSIT_crop, year, Yield_kgHa_irrigated) %>%
      mutate(Irr_Rfd = "IRR") %>%
      rename(yield_kgHa = Yield_kgHa_irrigated) ->
      L162.ag_irrYield_kgHa_Rcrs_Ccrs_Y

    # Do same for rainfed and bind the irrigated table.
    # Then complete missing agricultural years from aglu.SPEC_AG_PROD_YEARS and interpolate
    # to fill in yields. Keep only the years in aglu.SPEC_AG_PROD_YEARS
    # Finally, Calculate yield multipliers.
    FAO_ag_CROSIT %>%
      select(CROSIT_ctry, CROSIT_crop, year, Yield_kgHa_rainfed) %>%
      mutate(Irr_Rfd = "RFD") %>%
      rename(yield_kgHa = Yield_kgHa_rainfed) %>%
      bind_rows(L162.ag_irrYield_kgHa_Rcrs_Ccrs_Y) %>%
      # add the missing aglu.SPEC_AG_PROD_YEARS and interpolate the yields
      complete(year = c(year, aglu.SPEC_AG_PROD_YEARS) ,
                      CROSIT_ctry, CROSIT_crop, Irr_Rfd) %>%
      select(CROSIT_ctry, CROSIT_crop, Irr_Rfd, year, yield_kgHa) %>%
      arrange(year) %>%
      group_by(CROSIT_ctry, CROSIT_crop, Irr_Rfd) %>%
      mutate(yield_kgHa = approx_fun(year, yield_kgHa)) %>%
      ungroup() %>% filter(year %in% aglu.SPEC_AG_PROD_YEARS) %>%
      # calculate yield multipliers
      group_by(CROSIT_ctry, CROSIT_crop, Irr_Rfd) %>%
      mutate(Mult = yield_kgHa / first(yield_kgHa)) %>%
      ungroup() %>%
      select(-yield_kgHa) %>%
      # Drop the NaN's = crops with zero base year production / harvested area
      na.omit() ->
      L162.ag_Yieldmult_Rcrs_Ccrs_Y_irr


    # We apply the above yield multipliers to crop-specific changes in harvested area.
    # This removes bias from changes in composition of GCAM commodities in the FAO projections.
    # These yield multipliers are now ready to be matched into the GTAP/LDS-based table of
    # country x crop x zone harvested area in the base year, L151.ag_irrHA/rfdHA...
    #
    # First, merge the separate rainfed and irrigated harvested area datasets to simplify processing.
    # Then join CROSIT country and crop identifiers. This intermediate table is used in multiple
    # subsequent pipelines.
    #
    # CROSIT_ctry identifiers come from the AGLU_ctry table, which needs preprocessing to avoid unwanted
    # extra rows due to Yugoslav FSR.

    # deal with unfilled Yugoslavia entries in AGLU_ctry without impacting other code chunks.
    AGLU_ctry %>%
      select(iso, CROSIT_ctry) %>%
      filter(!is.na(CROSIT_ctry)) %>%
      dplyr::distinct() ->
      AGLU_ctry_iso_CROSIT

    L151.ag_irrHA_ha_ctry_crop %>%
      mutate(Irr_Rfd = "IRR") %>%
      rename(HA = irrHA) ->
      L151.ag_irrHA_ha_ctry_crop

    L151.ag_rfdHA_ha_ctry_crop %>%
      mutate(Irr_Rfd = "RFD") %>%
      rename(HA = rfdHA) %>%
      bind_rows(L151.ag_irrHA_ha_ctry_crop) %>%
      # Join CROSIT country and crop information and aggregate
      # keeping NA's for later processing
      left_join(AGLU_ctry_iso_CROSIT, by = "iso") %>%
      left_join(dplyr::distinct(select(FAO_ag_items_PRODSTAT, CROSIT_crop, GTAP_crop)),
                by = "GTAP_crop") ->
      L162.ag_HA_ha_ctry_crop_irr


    # Aggregate the above table of LDS area to CROSIT country and crop levels.
    # Then, filter to only the country-crop-irrigation combinations present in the CROSIT multiplier
    # table, L162.ag_Yieldmult_Rcrs_Ccrs_Y_irr.
    # Repeat the resulting tibble for all aglu.SPEC_AG_PROD_YEARS, and join in the yield multipliers.
    # This results in a table of harvested area and yield multipliers by CROSIT country and crop, glu,
    # and irrigation for each aglu.SPEC_AG_PROD_YEARS year, based on LDS harvested area tables L151....
    #
    # Compositional shifts of CROSIT commodities within GCAM commodities do not translate to modified yields.
    # This is because Harvested area multipliers are 1 in all periods, meaning the yield multipliers are
    # equivalent to production multipliers and subsequent aggregation to the GCAM commodity level is not
    # area weighted.
    L162.ag_HA_ha_ctry_crop_irr %>%
      group_by(CROSIT_ctry, CROSIT_crop, GLU, Irr_Rfd) %>%
      summarise(HA = sum(HA)) %>%
      na.omit() %>%
      ungroup() %>%
      # Filter to country-crop-irrigation combos in yield multiplier table
      semi_join(select(L162.ag_Yieldmult_Rcrs_Ccrs_Y_irr, CROSIT_ctry, CROSIT_crop, Irr_Rfd),
                by = c("CROSIT_ctry", "CROSIT_crop", "Irr_Rfd")) %>%
      # repeat for all years in aglu.SPEC_AG_PROD_YEARS and join Yield multipliers for each year
      repeat_add_columns(tibble(year = aglu.SPEC_AG_PROD_YEARS)) %>%
      left_join_error_no_match(L162.ag_Yieldmult_Rcrs_Ccrs_Y_irr,
                               by = c("CROSIT_ctry", "CROSIT_crop", "Irr_Rfd", "year")) ->
      L162.ag_HA_ha_Rcrs_Ccrs_Ysy_GLU_irr



    # Calculating the adjusted production / harvested area in each time period, for each GCAM region / commodity / GLU.
    # Starting from full GTAP table for composite regions and commodities in the CROSIT database, L162.ag_HA_ha_ctry_crop_irr,
    # repeating for all years in aglu.SPEC_AG_PROD_YEARS.
    # Then join in yield multipliers from CROSIT L162.ag_HA_ha_Rcrs_Ccrs_Ysy_GLU_irr, GCAM region and GCAM commodity information.
    # Calculate modified production, Prod_mod = base year harvested area HA * yearly yield multipliers Mult. Production and
    # Harvested area are then aggregated to the GCAM region - commodity level so that Aggregated Yield can be correctly calculated.
    # The YieldRatio = Prod_mod/HA is then calculated and output for each GCAM region-commodity-glu-irrigation-year.
    #
    # The YieldRatio calculated in the following pipeline is so named to reflect that "the value was the yield the given year divided
    # by the yield in the base year"; in other words, YieldRatio = Prod_mod / HA rather than Yield = Prod / HA.
    # And modified Production, Prod_mod = Harvested Area * Yield Multiplier -> Prod_mod is productivity growth weighted by
    # harvested area, and so the YieldRatio is distinct from Yield.

    # preprocess table of multipliers before joining, restricting to the CROSIT country-crop-glu-irrigation present in
    # the full GTAP table,  L162.ag_HA_ha_ctry_crop_irr
    L162.ag_HA_ha_Rcrs_Ccrs_Ysy_GLU_irr %>%
      select(CROSIT_ctry, CROSIT_crop, Irr_Rfd, year, Mult) %>%
      semi_join(L162.ag_HA_ha_ctry_crop_irr, by = c("CROSIT_ctry", "CROSIT_crop", "Irr_Rfd")) %>%
      dplyr::distinct() ->
      CROSIT_mult

    L162.ag_HA_ha_ctry_crop_irr %>%
      na.omit() %>%
      repeat_add_columns(tibble(year = aglu.SPEC_AG_PROD_YEARS)) %>%
      left_join(CROSIT_mult, by = c("CROSIT_ctry", "CROSIT_crop", "Irr_Rfd", "year")) %>%
      na.omit() %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      left_join_error_no_match(select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity, GCAM_subsector), by = "GTAP_crop") %>%
      # Multiply base-year harvested area by the future productivity multipliers to calculate prod_mod and aggregate
      mutate(Prod_mod = HA * Mult) %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year, GLU, Irr_Rfd) %>%
      summarise(HA = sum(HA), Prod_mod = sum(Prod_mod)) %>%
      ungroup() %>%
      # Calculate YieldRatio = Prod_mod/HA by region-commodity-glu-irrigation-year; subset and output the YieldRatios
      mutate(YieldRatio = Prod_mod / HA) %>%
      na.omit() %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year, GLU, Irr_Rfd, YieldRatio) ->
      L162.ag_YieldRatio_R_C_Ysy_GLU_irr

    # Create a comparable table of YieldRatio for each year by GCAM region / commodity / GLU for biomass.
    # The biomass YieldRatio in each year is taken to be the median of YieldRatios for all commodities that year.
    # Then bind to the table of yield ratios for other commodities
    L162.ag_YieldRatio_R_C_Ysy_GLU_irr %>%
      group_by(GCAM_region_ID, year, GLU, Irr_Rfd) %>%
      summarise(YieldRatio = median(YieldRatio)) %>%
      ungroup() %>%
      mutate(GCAM_commodity = "biomass") %>%
      repeat_add_columns(tibble(GCAM_subsector = c("biomassGrass", "biomassTree"))) %>%
      bind_rows(L162.ag_YieldRatio_R_C_Ysy_GLU_irr) ->
      L162.agBio_YieldRatio_R_C_Ysy_GLU_irr


    # Translate these yield ratios to annual improvement rates.
    # The rate in year i is defined as
    # [(ratio_i / ratio_{i-1}) ^ (1/(year_i - year_{i-1}) ] - 1.
    #
    # To perform this calculation in a pipeline, we form two intermediate tables.
    # First, of aglu.SPEC_AG_PROD_YEARS and the corresponding timestep for each.
    # Second, of lagged YieldRatios and corresponding time steps,
    # Where lagyear represents the year a ratio is subtracted from (ie lagyear = 2010 indicates this ratio
    # is subtracted from the 2010 ratio.)
    # This allows the same calculation to be performed even if aglu.SPEC_AG_PROD_YEARS changes.
    tibble(year = aglu.SPEC_AG_PROD_YEARS, timestep = c(diff(aglu.SPEC_AG_PROD_YEARS), max(aglu.SPEC_AG_PROD_YEARS) + 1)) ->
      timesteps

    L162.agBio_YieldRatio_R_C_Ysy_GLU_irr %>%
      left_join_error_no_match(timesteps, by = "year") %>%
      mutate(lagyear = year + timestep,
             # There is no lag for aglu.SPEC_AG_PROD_YEARS[1] but there is for a year not in SPEC_AG_PROD_YEARS
             # aglu.SPEC_AG_PROD_YEARS[1] gets left alone, so for lagyear = not in aglu.SPEC_AG_PROD_YEAR, overwrite
             # the ratio to be 0.5, the timestep to be 1, and lagyear = SPEC_AG_PROD_YEAR[1]. This allows
             # the same pipeline to be used for all aglu.SPEC_AG_PROD_YEARS
             YieldRatio = replace(YieldRatio,
                                  ! lagyear %in% aglu.SPEC_AG_PROD_YEARS,
                                  0.5),
             timestep = replace(timestep,
                                ! lagyear %in% aglu.SPEC_AG_PROD_YEARS,
                                1),
             lagyear = replace(lagyear,
                               ! lagyear %in% aglu.SPEC_AG_PROD_YEARS,
                               first(aglu.SPEC_AG_PROD_YEARS))) %>%
      select(-year) %>%
      rename(YieldRatio_lag = YieldRatio) ->
      L162.agBio_YieldRatio_lag

    # Join the YieldRatio_lag table to the YieldRatio table to calculate the annual rates.
    L162.agBio_YieldRatio_R_C_Ysy_GLU_irr %>%
      left_join_error_no_match(L162.agBio_YieldRatio_lag, by = c("GCAM_region_ID", "GLU", "Irr_Rfd", "GCAM_commodity", "GCAM_subsector", "year" = "lagyear")) %>%
      mutate(YieldRate = (YieldRatio / YieldRatio_lag) ^ (1 / timestep) - 1) %>%
      select(-YieldRatio, -YieldRatio_lag, -timestep) ->
      L162.agBio_YieldRate_R_C_Ysy_GLU_irr


    # Match These Annual Improvement Rates, L162.agBio_YieldRate_R_C_Ysy_GLU_irr, into a table of existing crop yields.
    #
    # Step 1: make a table of default improvement rates by interpolating available rates to relevant years.
    A_defaultYieldRate %>%
      gather_years %>%
      complete(year = unique(c(year, max(HISTORICAL_YEARS), FUTURE_YEARS)),
                      GCAM_commodity) %>%
      arrange(year) %>%
      group_by(GCAM_commodity) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% unique(c(max(HISTORICAL_YEARS), FUTURE_YEARS))) %>%
      rename(defaultRate = value) ->
      L162.defaultYieldRate

    # Step 2: The GCAM region-commodity-glu-irrigation combinations contained in L161.ag_irrProd_Mt_R_C_Y_GLU, L161.ag_rfdProd_Mt_R_C_Y_GLU
    # represent all relevant combinations, minus biomass.
    # Get the set of possible combinations, and add biomass for each GCAM region-GLU-irrigation combo.
    # Then join in the YieldRates from L162.agBio_YieldRate_R_C_Ysy_GLU_irr.

    # get set of all relevent GCAM Region-Commodity-GLU-Irrigation combos (except biomass)
    L161.ag_irrProd_Mt_R_C_Y_GLU %>%
      mutate(Irr_Rfd = "IRR") %>%
      bind_rows(mutate(L161.ag_rfdProd_Mt_R_C_Y_GLU, Irr_Rfd = "RFD")) %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, Irr_Rfd) %>%
      dplyr::distinct() ->
      L162.ag_Prod_Mt_R_C_Y_GLU_irr

    # add biomass in each region-glu-irrigation combo and join to other commodities.
    # Then join in yield ratesfrom L162.agBio_YieldRate_R_C_Ysy_GLU_irr.
    L162.ag_Prod_Mt_R_C_Y_GLU_irr %>%
      select(GCAM_region_ID, GLU, Irr_Rfd) %>%
      dplyr::distinct() %>%
      mutate(GCAM_commodity = "biomass") %>%
      repeat_add_columns(tibble(GCAM_subsector = c("biomassGrass", "biomassTree"))) %>%
      bind_rows(L162.ag_Prod_Mt_R_C_Y_GLU_irr) %>%
      # Join the agBio Yield Rates
      left_join(L162.agBio_YieldRate_R_C_Ysy_GLU_irr, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU", "Irr_Rfd")) %>%
      # NA's include NA years, address
      tidyr::complete(year = aglu.SPEC_AG_PROD_YEARS, nesting(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, Irr_Rfd)) %>%
      filter(!is.na(year)) ->
      # store in a table for further processing
      L162.agbio_YieldRate_R_C_Y_GLU_irr

    # Step 3: For combinations not covered by L162.agBio_YieldRate_R_C_Ysy_GLU_irr, fill in the values from the default table in
    # Step 1.

    # Subset to only the complete cases - group by region-commodity-glu-irrigation and keep only the members with non-na entries
    # for every year
    L162.agbio_YieldRate_R_C_Y_GLU_irr %>%
      # isolate the incomplete rows, wipe out their existing data, and pull in default yield rates
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, Irr_Rfd) %>%
      filter(!any(is.na(YieldRate))) %>%
      ungroup() ->
      L162.agbio_YieldRate_R_C_Y_GLU_irr_completecases

    # Isolate the incomplete cases, fill in default yield rates for each year, and join to the complete cases
    L162.agbio_YieldRate_R_C_Y_GLU_irr %>%
      # isolate the incomplete rows, wipe out their existing data, and pull in default yield rates
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, Irr_Rfd) %>%
      filter(any(is.na(YieldRate))) %>%
      ungroup() %>%
      select(-YieldRate) %>%
      left_join_error_no_match(L162.defaultYieldRate, by = c("year", "GCAM_commodity")) %>%
      rename(YieldRate = defaultRate) %>%
      # incorporate back into main data frame
      bind_rows(L162.agbio_YieldRate_R_C_Y_GLU_irr_completecases) ->
      L162.agbio_YieldRate_R_C_Y_GLU_irr

    # Step 4: Expand to future years by applying the default rate in each year
    L162.agbio_YieldRate_R_C_Y_GLU_irr %>%
      complete(year = c(max(HISTORICAL_YEARS),FUTURE_YEARS), nesting(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, Irr_Rfd)) %>%
      left_join_error_no_match(L162.defaultYieldRate, by = c("year", "GCAM_commodity")) %>%
      # replace NA's - which correspond to years we just filled in - with the default yield for that year we just joined
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, Irr_Rfd, year) %>%
      mutate(YieldRate = replace(YieldRate,
                                 is.na(YieldRate),
                                 defaultRate)) %>%
      ungroup() %>%
      select(-defaultRate) ->
      L162.agbio_YieldRate_R_C_Y_GLU_irr

    # Step 5: Separate out into tables for biomass and non-biomass quantities for writing outputs.
    # Then rename columns of output tables to value for testing.
    # In old DS, L162.ag_YieldRatio_R_C_Ysy_GLU_irr is long-form with the informative name YieldRatio.
    # Therefore here, L162.ag_YieldRatio_R_C_Ysy_GLU_irr can be left alone.
    # Old L162.ag_YieldRate_R_C_Y_GLU_irr and L162.bio_YieldRate_R_Y_GLU_irr are in wide-form, so new
    # L162.ag_YieldRate_R_C_Y_GLU_irr and L162.bio_YieldRate_R_Y_GLU_irr need column names of value rather
    # than the informative YieldRate used so far for readability. They also need appropriate flags.
    L162.agbio_YieldRate_R_C_Y_GLU_irr %>%
      filter(GCAM_commodity == "biomass") %>%
      rename(value = YieldRate) ->
      L162.bio_YieldRate_R_Y_GLU_irr

    L162.agbio_YieldRate_R_C_Y_GLU_irr %>%
      filter(GCAM_commodity != "biomass") %>%
      rename(value = YieldRate) ->
      L162.ag_YieldRate_R_C_Y_GLU_irr

    # Produce outputs
    L162.ag_YieldRatio_R_C_Ysy_GLU_irr %>%
      add_title("Yield change ratios from final historical year by GCAM region / commodity / future year / GLU / irrigation") %>%
      add_units("Unitless") %>%
      add_comments("Future year production multipliers are calculated at the CROSIT country-crop level as future production / base-year production.") %>%
      add_comments("These are used to aggregate to the GCAM region-commodity-GLU level and calculate future year yield ratios.") %>%
      add_legacy_name("L162.ag_YieldRatio_R_C_Ysy_GLU_irr") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/A_defaultYieldRate",
                     "aglu/AGLU_ctry",
                     "aglu/FAO/FAO_ag_CROSIT",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "L151.ag_irrHA_ha_ctry_crop",
                     "L151.ag_rfdHA_ha_ctry_crop",
                     "L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "L161.ag_rfdProd_Mt_R_C_Y_GLU") ->
      L162.ag_YieldRatio_R_C_Ysy_GLU_irr

    L162.ag_YieldRate_R_C_Y_GLU_irr %>%
      add_title("Yield change rates by GCAM region / commodity / future year / GLU / irrigation") %>%
      add_units("Annual rate") %>%
      add_comments("Yield Ratios are used to calculate Yield Rates for each GCAM region-commodity-GLU for externally specified agricultural") %>%
      add_comments("production years. Externally provided default Yield Rates are used to fill in missing information and to extend from ") %>%
      add_comments("specified years to all future years.") %>%
      add_legacy_name("L162.ag_YieldRate_R_C_Y_GLU_irr") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/A_defaultYieldRate",
                     "aglu/AGLU_ctry",
                     "aglu/FAO/FAO_ag_CROSIT",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "L151.ag_irrHA_ha_ctry_crop",
                     "L151.ag_rfdHA_ha_ctry_crop",
                     "L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "L161.ag_rfdProd_Mt_R_C_Y_GLU") ->
      L162.ag_YieldRate_R_C_Y_GLU_irr

    L162.bio_YieldRate_R_Y_GLU_irr %>%
      add_title("Biomass yield change rates by GCAM region / commodity / future year / GLU / irrigation") %>%
      add_units("Annual rate") %>%
      add_comments("Biomass Yield Ratios are the median ratio of all other commodities at the region-GLU-irrigation level in each year and are used to") %>%
      add_comments("calculate Yield Rates for each GCAM region-commodity-GLU for externally specified agricultural") %>%
      add_comments("production years. Externally provided default Yield Rates are used to fill in missing information and to extend from ") %>%
      add_comments("specified years to all future years.") %>%
      add_legacy_name("L162.bio_YieldRate_R_Y_GLU_irr") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/A_defaultYieldRate",
                     "aglu/AGLU_ctry",
                     "aglu/FAO/FAO_ag_CROSIT",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "L151.ag_irrHA_ha_ctry_crop",
                     "L151.ag_rfdHA_ha_ctry_crop",
                     "L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "L161.ag_rfdProd_Mt_R_C_Y_GLU") ->
      L162.bio_YieldRate_R_Y_GLU_irr

    return_data(L162.ag_YieldRatio_R_C_Ysy_GLU_irr, L162.ag_YieldRate_R_C_Y_GLU_irr, L162.bio_YieldRate_R_Y_GLU_irr)
  } else {
    stop("Unknown command")
  }
}
