#' module_aglu_LB162.ag_prodchange_R_C_Y_GLU_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L162.ag_YieldRatio_R_C_Ysy_GLU_irr}, \code{L162.ag_YieldRate_R_C_Y_GLU_irr}, \code{L162.bio_YieldRate_R_Y_GLU_irr}. The corresponding file in the
#' original data system was \code{LB162.ag_prodchange_R_C_Y_GLU_irr.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB162.ag_prodchange_R_C_Y_GLU_irr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/A_defaultYieldRate",
             FILE = "aglu/AGLU_ctry",
             FILE = "aglu/FAO_ag_CROSIT",
             FILE = "aglu/FAO_ag_items_PRODSTAT",
             "L151.ag_irrHA_ha_ctry_crop",
             "L151.ag_rfdHA_ha_ctry_crop",
             FILE = "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
             FILE = "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L162.ag_YieldRatio_R_C_Ysy_GLU_irr",
             "L162.ag_YieldRate_R_C_Y_GLU_irr",
             "L162.bio_YieldRate_R_Y_GLU_irr"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    A_defaultYieldRate <- get_data(all_data, "aglu/A_defaultYieldRate")
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    FAO_ag_CROSIT <- get_data(all_data, "aglu/FAO_ag_CROSIT")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO_ag_items_PRODSTAT")
    L151.ag_irrHA_ha_ctry_crop <- get_data(all_data, "L151.ag_irrHA_ha_ctry_crop")
    L151.ag_rfdHA_ha_ctry_crop <- get_data(all_data, "L151.ag_rfdHA_ha_ctry_crop")
    L161.ag_irrProd_Mt_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU")
    L161.ag_rfdProd_Mt_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU")
    # These lines are only while using temp-data-inject:
    L161.ag_irrProd_Mt_R_C_Y_GLU %>%
      gather(year, value, -GCAM_region_ID, -GCAM_commodity, -GLU) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L161.ag_irrProd_Mt_R_C_Y_GLU
    L161.ag_rfdProd_Mt_R_C_Y_GLU %>%
      gather(year, value, -GCAM_region_ID, -GCAM_commodity, -GLU) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L161.ag_rfdProd_Mt_R_C_Y_GLU


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
                                         Yield_kgHa_irrigated * HA_kha_irrigated * CONV_KG_T)) %>%
      mutate(Prod_kt_rainfed = replace(Prod_kt_rainfed,
                                         Prod_kt_rainfed == 0 & HA_kha_rainfed != 0,
                                         Yield_kgHa_rainfed * HA_kha_rainfed * CONV_KG_T)) %>%
      mutate(HA_kha_irrigated = replace(HA_kha_irrigated,
                                        Prod_kt_irrigated == 0 & HA_kha_irrigated != 0,
                                        0)) %>%
      mutate(HA_kha_rainfed = replace(HA_kha_rainfed,
                                      Prod_kt_rainfed == 0 & HA_kha_rainfed != 0,
                                        0)) %>%
      ungroup() ->
      FAO_ag_CROSIT


    # Use the CROSIT database to prepare a table of yields by country, crop, irrigation, and year,
    # and interpolate to fill in all of the specified agricultural production years, SPEC_AG_PROD_YEARS.
    # Finally, Calculate yield multipliers from the base year <=> first year of SPEC_AG_PROD_YEARS.
    # For each country, crop, irrigation, the multiplier for each year is
    # yield in that year / yield in base year.

    # pull off irrigated table of yield, country, crop; append irrigation information
    FAO_ag_CROSIT %>%
      select(CROSIT_ctry, CROSIT_crop, year, Yield_kgHa_irrigated) %>%
      mutate(Irr_Rfd = "IRR") %>%
      rename(yield_kgHa = Yield_kgHa_irrigated) ->
      L162.ag_irrYield_kgHa_Rcrs_Ccrs_Y

    # Do same for rainfed and bind the irrigated table.
    # Then complete missing agricultural years from SPEC_AG_PROD_YEARS and interpolate
    # to fill in yields. Keep only the years in SPEC_AG_PROD_YEARS.
    # Finally, Calculate yield multipliers.
    FAO_ag_CROSIT %>%
      select(CROSIT_ctry, CROSIT_crop, year, Yield_kgHa_rainfed) %>%
      mutate(Irr_Rfd = "RFD") %>%
      rename(yield_kgHa = Yield_kgHa_rainfed) %>%
      bind_rows(L162.ag_irrYield_kgHa_Rcrs_Ccrs_Y ) %>%
      # add the missing SPEC_AG_PROD_YEARS and interpolate the yields
      tidyr::complete(year = c(year, SPEC_AG_PROD_YEARS) ,
                      CROSIT_ctry, CROSIT_crop, Irr_Rfd) %>%
      select(CROSIT_ctry, CROSIT_crop, Irr_Rfd, year, yield_kgHa) %>%
      arrange(year) %>%
      group_by(CROSIT_ctry, CROSIT_crop, Irr_Rfd) %>%
      mutate(yield_kgHa = approx_fun(year, yield_kgHa)) %>%
      ungroup() %>% filter(year %in% SPEC_AG_PROD_YEARS) %>%
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
      L162.ag_HA_ha_ctry_crop_irr # agrees to 10e-8


    # Aggregate the above table of LDS area to CROSIT country and crop levels.
    # Then, filter to only the country-crop-irrigation combinations present in the CROSIT multiplier
    # table, L162.ag_Yieldmult_Rcrs_Ccrs_Y_irr.
    # Repeat the resulting tibble for all SPEC_AG_PROD_YEARS, and join in the yield multipliers.
    # This results in a table of harvested area and yield multipliers by CROSIT country and crop, glu,
    # and irrigation for each SPEC_AG_PROD_YEARS year, based on LDS harvested area tables L151....
    #
    # In this method, compositional shifts of CROSIT commodities within GCAM commodities do not
    # translate to modified yields. Harvested area multipliers are 1 in all periods.
    L162.ag_HA_ha_ctry_crop_irr %>%
      group_by(CROSIT_ctry, CROSIT_crop, GLU, Irr_Rfd) %>%
      summarise(HA = sum(HA)) %>%
      na.omit() %>%
      ungroup() %>%
      # Filter to country-crop-irrigation combos in yield multiplier table
      semi_join(select(L162.ag_Yieldmult_Rcrs_Ccrs_Y_irr, CROSIT_ctry, CROSIT_crop, Irr_Rfd),
                by = c("CROSIT_ctry", "CROSIT_crop", "Irr_Rfd")) %>%
      # repeat for all years in SPEC_AG_PROD_YEARS and join Yield multipliers for each year
      repeat_add_columns(tibble::tibble(year = SPEC_AG_PROD_YEARS)) %>%
      left_join_error_no_match(L162.ag_Yieldmult_Rcrs_Ccrs_Y_irr,
                               by = c("CROSIT_ctry", "CROSIT_crop", "Irr_Rfd", "year")) ->
      L162.ag_HA_ha_Rcrs_Ccrs_Ysy_GLU_irr # agree perfectly



    # Calculating the adjusted production / harvested area in each time period, for each GCAM region / commodity / GLU.
    # Starting from full GTAP table for composite regions and commodities in the CROSIT database, L162.ag_HA_ha_ctry_crop_irr,
    # repeating for all years in SPEC_AG_PROD_YEARS.
    # Then join in yield multipliers from CROSIT L162.ag_HA_ha_Rcrs_Ccrs_Ysy_GLU_irr, GCAM region and GCAM commodity information.
    # Calculate modified production, Prod_mod = base year harvested area HA * yearly yield multipliers Mult. Production and
    # Harvested area are then aggregated to the GCAM region - commodity level so that Aggregated Yield can be correctly calculated.
    # The YieldRatio = Prod_mod/HA is then calculated and output for each GCAM region-commodity-glu-irrigation-year.
    #
    # There is a small error in the old data system. The intention is to match in multipliers by CROSIT_ctry, CROSIT_crop, irrigation,
    # and year. The old DS only does so by CROSIT_ctry, CROSIT_crop, and year.

    if(OLD_DATA_SYSTEM_BEHAVIOR) {
      # preprocess table of multipliers before joining, restricting to the CROSIT country-crop-glu-irrigation present in
      # the full GTAP table,  L162.ag_HA_ha_ctry_crop_irr
      L162.ag_HA_ha_Rcrs_Ccrs_Ysy_GLU_irr %>%
        select(CROSIT_ctry, CROSIT_crop, year, Mult) %>%
        semi_join(L162.ag_HA_ha_ctry_crop_irr, by = c("CROSIT_ctry", "CROSIT_crop")) %>%
        dplyr::distinct() ->
        CROSIT_mult #NO NA multipliers, as in old

      L162.ag_HA_ha_ctry_crop_irr %>%
        na.omit() %>%
        repeat_add_columns(tibble::tibble(year = SPEC_AG_PROD_YEARS)) %>% # agree fine to here; it's the next join
        left_join_keep_first_only(CROSIT_mult, by = c("CROSIT_ctry", "CROSIT_crop", "year")) %>%
        na.omit() %>%
        left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
        left_join_error_no_match(select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity), by = "GTAP_crop") %>%
        #THINK GOOD TO HERE
        # Multiply base-year harvested area by the future productivity multipliers to calculate prod_mod and aggregate
        mutate(Prod_mod = HA * Mult)  %>%
        group_by(GCAM_region_ID, GCAM_commodity, year, GLU, Irr_Rfd) %>%
        summarise(HA = sum(HA), Prod_mod = sum(Prod_mod)) %>%
        ungroup() %>%
        # Calculate YieldRatio = Prod_mod/HA by region-commodity-glu-irrigation-year; subset and output the YieldRatios
        mutate(YieldRatio = Prod_mod / HA) %>%
        na.omit() %>%
        select(GCAM_region_ID, GCAM_commodity, year, GLU, Irr_Rfd, YieldRatio) ->
        L162.ag_YieldRatio_R_C_Ysy_GLU_irr
    } else {
      # preprocess table of multipliers before joining, restricting to the CROSIT country-crop-glu-irrigation present in
      # the full GTAP table,  L162.ag_HA_ha_ctry_crop_irr
      L162.ag_HA_ha_Rcrs_Ccrs_Ysy_GLU_irr %>%
        select(CROSIT_ctry, CROSIT_crop, Irr_Rfd, year, Mult) %>%
        semi_join(L162.ag_HA_ha_ctry_crop_irr, by = c("CROSIT_ctry", "CROSIT_crop", "Irr_Rfd")) %>%
        dplyr::distinct() ->
        CROSIT_mult #NO NA multipliers, as in old

      L162.ag_HA_ha_ctry_crop_irr %>%
        na.omit() %>%
        repeat_add_columns(tibble::tibble(year = SPEC_AG_PROD_YEARS)) %>% # agree fine to here; it's the next join
        left_join(CROSIT_mult, by = c("CROSIT_ctry", "CROSIT_crop","Irr_Rfd", "year")) %>%
        na.omit() %>%
        left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
        left_join_error_no_match(select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity), by = "GTAP_crop") %>%
        #THINK GOOD TO HERE
        # Multiply base-year harvested area by the future productivity multipliers to calculate prod_mod and aggregate
        mutate(Prod_mod = HA * Mult)  %>%
        group_by(GCAM_region_ID, GCAM_commodity, year, GLU, Irr_Rfd) %>%
        summarise(HA = sum(HA), Prod_mod = sum(Prod_mod)) %>%
        ungroup() %>%
        # Calculate YieldRatio = Prod_mod/HA by region-commodity-glu-irrigation-year; subset and output the YieldRatios
        mutate(YieldRatio = Prod_mod / HA) %>%
        na.omit() %>%
        select(GCAM_region_ID, GCAM_commodity, year, GLU, Irr_Rfd, YieldRatio) ->
        L162.ag_YieldRatio_R_C_Ysy_GLU_irr
    }




    # Create a comparable table of YieldRatio for each year by GCAM region / commodity / GLU for biomass.
    # The biomass YieldRatio in each year is taken to be the median of YieldRatios for all commodities that year.
    # Then bind to the table of yield ratios for other commodities
    L162.ag_YieldRatio_R_C_Ysy_GLU_irr %>%
      group_by(GCAM_region_ID, year, GLU, Irr_Rfd) %>%
      summarise(YieldRatio = median(YieldRatio)) %>%
      ungroup() %>%
      mutate(GCAM_commodity = "biomass") %>%
      bind_rows(L162.ag_YieldRatio_R_C_Ysy_GLU_irr) ->
      L162.agBio_YieldRatio_R_C_Ysy_GLU_irr


    # Translate these yield ratios to annual improvement rates.
    # The rate in year i is defined as
    # [(ratio_i / ratio_{i-1}) ^ (1/(year_i - year_{i-1}) ] - 1.
    #
    # To perform this calculation in a pipeline, we form two intermediate tables.
    # First, of SPEC_AG_PROD_YEARS and the corresponding timestep for each.
    # Second, of lagged YieldRatios and corresponding time steps,
    # Where lagyear represents the year a ratio is subtracted from (ie lagyear = 2010 indicates this ratio
    # is subtracted from the 2010 ratio.)
    # This allows the same calculation to be performed even if SPEC_AG_PROD_YEARS changes.
    tibble::tibble(year = SPEC_AG_PROD_YEARS, timestep = c(diff(SPEC_AG_PROD_YEARS), max(SPEC_AG_PROD_YEARS) + 1)) ->
      timesteps

    L162.agBio_YieldRatio_R_C_Ysy_GLU_irr %>%
      left_join_error_no_match(timesteps, by = "year") %>%
      mutate(lagyear = year + timestep)  %>%
      # There is no lag for SPEC_AG_PROD_YEARS[1] but there is for a year not in SPEC_AG_PROD_YEARS
      # SPEC_AG_PROD_YEARS[1] gets left alone, so for lagyear = not in SPEC_AG_PROD_YEAR, overwrite
      # the ratio to be 0.52, the timestep to be 1, and lagyear = SPEC_AG_PROD_YEAR[1]. This allows
      # the same pipeline to be used for all SPEC_AG_PROD_YEARS
      mutate(YieldRatio = replace(YieldRatio,
                                  ! lagyear %in% SPEC_AG_PROD_YEARS,
                                  0.5),
             timestep = replace(timestep,
                                ! lagyear %in% SPEC_AG_PROD_YEARS,
                                1),
             lagyear = replace(lagyear,
                               ! lagyear %in% SPEC_AG_PROD_YEARS,
                               first(SPEC_AG_PROD_YEARS))) %>%
      select(-year) %>%
      rename(YieldRatio_lag = YieldRatio) ->
      L162.agBio_YieldRatio_lag

    # Join the YieldRatio_lag table to the YieldRatio table to calculate the annual rates.
    L162.agBio_YieldRatio_R_C_Ysy_GLU_irr %>%
      left_join_error_no_match(L162.agBio_YieldRatio_lag, by = c("GCAM_region_ID", "GLU", "Irr_Rfd", "GCAM_commodity", "year" = "lagyear")) %>%
      mutate(YieldRate = (YieldRatio / YieldRatio_lag) ^ (1 / timestep) - 1) %>%
      select(-YieldRatio, -YieldRatio_lag, -timestep) ->
      L162.agBio_YieldRate_R_C_Ysy_GLU_irr


    # Match These Annual Improvement Rates, L162.agBio_YieldRate_R_C_Ysy_GLU_irr, into a table of existing crop yields.
    #
    # Step 1: make a table of default improvement rates by interpolating available rates to relevant years.
    A_defaultYieldRate %>%
      gather(year, value, -GCAM_commodity) %>%
      mutate(year = as.integer(year)) %>%
      tidyr::complete(year = unique(c(year, max(HISTORICAL_YEARS), FUTURE_YEARS)),
                      GCAM_commodity) %>%
      arrange(year) %>%
      group_by(GCAM_commodity) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% unique(c(max(HISTORICAL_YEARS), FUTURE_YEARS))) %>%
      rename(defaultRate = value) ->
      L162.defaultYieldRate

    # Step 2: The GCAM region-commodity-glu-irrigation combinations contained in L161.ag_irrProd_Mt_R_C_Y_GLU, L161.ag_rfdProd_Mt_R_C_Y_GLU
    # represent all relevant combinations.
    # Get the set of possible combinations and join in the YieldRates from L162.agBio_YieldRate_R_C_Ysy_GLU_irr.
    # For combinations not covered by L162.agBio_YieldRate_R_C_Ysy_GLU_irr, fill in the values from the default table above.
    L161.ag_irrProd_Mt_R_C_Y_GLU %>%
      mutate(Irr_Rfd = "IRR") %>%
      bind_rows(mutate(L161.ag_rfdProd_Mt_R_C_Y_GLU, Irr_Rfd = "RFD"))  %>%
      select(GCAM_region_ID, GCAM_commodity, GLU, Irr_Rfd) %>%
      dplyr::distinct() ->
      L162.ag_Prod_Mt_R_C_Y_GLU_irr

    L162.ag_Prod_Mt_R_C_Y_GLU_irr %>%
      left_join(L162.agBio_YieldRate_R_C_Ysy_GLU_irr, by = c("GCAM_region_ID", "GCAM_commodity", "GLU", "Irr_Rfd")) %>%
      # NA's include NA years, address
      tidyr::complete(year = unique(c(max(HISTORICAL_YEARS), FUTURE_YEARS)),
                      nesting(GCAM_region_ID, GCAM_commodity, GLU, Irr_Rfd)) %>%
      # join in default Rates and use to replace NA YieldRates
      left_join_error_no_match(L162.defaultYieldRate, by = c("GCAM_commodity", "year")) %>%
      group_by(GCAM_region_ID, GCAM_commodity, GLU, Irr_Rfd) %>%
      mutate(YieldRate = replace(YieldRate,
                                 is.na(YieldRate),
                                 defaultRate)) %>%
      ungroup() %>%
      select(-defaultRate) ->
      L162.ag_YieldRate_R_C_Y_GLU_irr


    # Step 3: Expand to future years



















    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L162.ag_YieldRatio_R_C_Ysy_GLU_irr") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/A_defaultYieldRate",
                     "aglu/AGLU_ctry",
                     "aglu/FAO_ag_CROSIT",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "L151.ag_irrHA_ha_ctry_crop",
                     "L151.ag_rfdHA_ha_ctry_crop",
                     "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L162.ag_YieldRatio_R_C_Ysy_GLU_irr
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L162.ag_YieldRate_R_C_Y_GLU_irr") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/A_defaultYieldRate",
                     "aglu/AGLU_ctry",
                     "aglu/FAO_ag_CROSIT",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "L151.ag_irrHA_ha_ctry_crop",
                     "L151.ag_rfdHA_ha_ctry_crop",
                     "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L162.ag_YieldRate_R_C_Y_GLU_irr
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L162.bio_YieldRate_R_Y_GLU_irr") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/A_defaultYieldRate",
                     "aglu/AGLU_ctry",
                     "aglu/FAO_ag_CROSIT",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "L151.ag_irrHA_ha_ctry_crop",
                     "L151.ag_rfdHA_ha_ctry_crop",
                     "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L162.bio_YieldRate_R_Y_GLU_irr

    return_data(L162.ag_YieldRatio_R_C_Ysy_GLU_irr, L162.ag_YieldRate_R_C_Y_GLU_irr, L162.bio_YieldRate_R_Y_GLU_irr)
  } else {
    stop("Unknown command")
  }
}
