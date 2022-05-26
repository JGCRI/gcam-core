# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LB112.ag_prodchange_R_C_Y_GLU
#'
#' Calculate yield change ratios and yield change rates by GCAM commodity / region / future years.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L112.ag_YieldRatio_R_C_Ysy_GLU}, \code{L112.ag_YieldRate_R_C_Y_GLU}, \code{L112.bio_YieldRate_R_Y_GLU}. The corresponding file in the
#' original data system was \code{LB112.ag_prodchange_R_C_Y_GLU.R} (aglu level1).
#' @details This chunk first calculates the yield ratios relative to the base year by GCAM commodity / region / future years to 2050,
#' and calculates the annual productivity change rates by GCAM commodity / region / future years to 2100.
#' Yield improvement rates are based on FAO estimates up to 2050 and the default agriculture productivity change assumptions beyond 2050.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join bind_rows filter full_join if_else group_by left_join mutate pull select summarise
#' @importFrom stats median
#' @author RC April 2017
module_aglu_LB112.ag_prodchange_R_C_Y_GLU <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/A_defaultYieldRate",
             FILE = "aglu/AGLU_ctry",
             FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
             FILE = "aglu/FAO/FAO_ag_CROSIT",
             "L100.LDS_ag_HA_ha",
             "L101.ag_Prod_Mt_R_C_Y_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L112.ag_YieldRatio_R_C_Ysy_GLU",
             "L112.ag_YieldRate_R_C_Y_GLU",
             "L112.bio_YieldRate_R_Y_GLU"))
  } else if(command == driver.MAKE) {

    CROSIT_ctry <- CROSIT_country_ID <- CROSIT_crop <- CROSIT_cropID <- year <-
        HA_kha <- Yield_kgHa <- Prod_kt <- HA_kha_rainfed <- Yield_kgHa_rainfed <-
        NULL
    Prod_kt_rainfed <- HA_kha_irrigated <- Yield_kgHa_irrigated <-
        Prod_kt_irrigated <- NULL
    Yield_base <- iso <- GTAP_crop <- . <- GLU <- value <- CROSIT <- Mult <-
        GCAM_region_ID <- GCAM_commodity <- Prod_mod <- YieldRatio <-
        GCAM_subsector <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    A_defaultYieldRate <- get_data(all_data, "aglu/A_defaultYieldRate")
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_items_PRODSTAT")
    FAO_ag_CROSIT <- get_data(all_data, "aglu/FAO/FAO_ag_CROSIT")
    L100.LDS_ag_HA_ha <- get_data(all_data, "L100.LDS_ag_HA_ha")
    L101.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L101.ag_Prod_Mt_R_C_Y_GLU", strip_attributes = TRUE)

    # Initial preparation of CROSIT database
    # Mapping file for CROSIT country name and country ID
    AGLU_ctry %>%
      select(CROSIT_ctry, CROSIT_country_ID) %>%
      unique() %>%
      na.omit() -> ctry_id

    # Mapping file for CROSIT crop name and crop ID
    FAO_ag_items_PRODSTAT %>%
      select(CROSIT_crop, CROSIT_cropID) %>%
      unique() %>%
      na.omit() -> crop_id

    # Build a table with all specified ag productivity years, prepare for interpolation
    FAO_ag_CROSIT %>%
      left_join_error_no_match(ctry_id, by = c("country_ID" = "CROSIT_country_ID")) %>%
      left_join_error_no_match(crop_id, by = c("crop_ID" = "CROSIT_cropID")) %>%
      select(CROSIT_ctry, CROSIT_crop) %>%
      unique() %>%
      repeat_add_columns(tibble(aglu.SPEC_AG_PROD_YEARS)) -> interp.tbl

    # Intial CROSIT data clean-up and interpolation of yields
    FAO_ag_CROSIT %>%
      # Replace country IDs with names
      left_join_error_no_match(ctry_id, by = c("country_ID" = "CROSIT_country_ID")) %>%
      # Replace crop IDs with names
      left_join_error_no_match(crop_id, by = c("crop_ID" = "CROSIT_cropID")) %>%
      # NOTE: Only use total production and harvested area from CROSIT (not separating rainfed and irrigated).
      select(CROSIT_ctry, CROSIT_crop, year, HA_kha, Yield_kgHa, Prod_kt,
             -HA_kha_rainfed, -Yield_kgHa_rainfed, -Prod_kt_rainfed, -HA_kha_irrigated, -Yield_kgHa_irrigated, -Prod_kt_irrigated) %>%
      # Convert the needed numeric variables from integer to double for calculation
      mutate(HA_kha = as.double(HA_kha),
             Yield_kgHa = as.double(Yield_kgHa),
             Prod_kt = as.double(Prod_kt)) %>%
      # Some regions have 0 production with positive harvested area and a reported yield.
      # Re-calculate production where area and yields are available.
      mutate(Prod_kt = if_else((Prod_kt == 0 & HA_kha != 0), HA_kha * Yield_kgHa * 0.001, Prod_kt),
             # Where no production or yields are given, re-set harvested area to 0.
             HA_kha = if_else((Prod_kt == 0 & HA_kha != 0), 0, HA_kha),
             # Eritrea has non-sensical data in the base year for wheat, and >60x yield growth to 2050.
             # Change the yield and production in the base year
             Yield_kgHa = if_else((CROSIT_ctry == "ERIT" & CROSIT_crop == "WHEA" & year == 2005), 1400, Yield_kgHa),
             Prod_kt = if_else((CROSIT_ctry == "ERIT" & CROSIT_crop == "WHEA" & year == 2005), 16 * 1.4, Prod_kt)) %>%
      # CROSIT yield data are available in 2005, 2030, and 2050. The followings are to fill the years in between by five-year step.
      # Join the table with all specified ag productivity years.
      full_join(interp.tbl, by = c("CROSIT_ctry", "CROSIT_crop", "year" = "aglu.SPEC_AG_PROD_YEARS")) %>%
      # Interpolate for each country and crop combination
      group_by(CROSIT_ctry, CROSIT_crop) %>%
      # Interpolate to all specified agricultural productivity years from 2010 to 2050 by five-year step.
      mutate(Yield_kgHa = approx_fun(year, Yield_kgHa)) %>%
      ungroup() %>%
      # Drop any years not included (i.e.2005)
      filter(year %in% aglu.SPEC_AG_PROD_YEARS) %>%
      # Drop the unneeded production and harvest area varaibles
      select(-HA_kha, -Prod_kt) -> Yield

    # Calculate CROSIT multipliers (yield ratios) from the base year to all specified years, by country and crop
    Yield %>%
      # Select the base year (2010)
      filter(year == min(aglu.SPEC_AG_PROD_YEARS)) %>%
      select(-year) %>%
      # Change the yield column name for next-step calculations
      rename(Yield_base = Yield_kgHa) %>%
      # Join the interpolated yield table
      full_join(Yield, by = c("CROSIT_ctry", "CROSIT_crop")) %>%
      # Calculate the yield ratios relative to base year
      mutate(Mult = Yield_kgHa / Yield_base) %>%
      select(-Yield_kgHa, -Yield_base) %>%
      # Drop the NaN's (crops with zero base year production / harvested area)
      na.omit() ->
      L112.ag_Yieldmult

    # These yield multipliers are now ready to be matched into the GTAP/LDS-based table of country x crop x zone harvested area in the base year
    # NOTE: We apply the yield ratio to crop-specific changes in harvested area. This removes bias from changes in composition of GCAM commodities in the FAO projections.

    # First, match in the CROSIT region and commodity names for aggregation
    # Mapping file for CROSIT country name and iso
    AGLU_ctry %>%
      select(CROSIT_ctry, iso) %>%
      unique() %>%
      na.omit() -> ctry_id

    # Mapping file for CROSIT crop name and GTAP crop name
    FAO_ag_items_PRODSTAT %>%
      select(CROSIT_crop, GTAP_crop) %>%
      unique() %>%
      na.omit() -> crop_id

    # List of all crop x commodity combinations in the CROSIT yield multiplier data
    L112.ag_Yieldmult %>%
      mutate(CROSIT_id = paste(CROSIT_ctry, CROSIT_crop, sep = "_")) %>%
      pull(CROSIT_id) %>%
      unique() -> CROSIT_id

    # Aggregate LDS harvested area by CROSIT country and crop, and match in the productivity multipliers
    L100.LDS_ag_HA_ha %>%
      # Replace CROSIT country names with iso (this creates NAs, and use left_join instead of left_join_error_no_match).
      left_join(ctry_id, by = "iso") %>%
      # Replace CROSIT crop names with GTAP crop names (this creates NAs, and use left_join instead of left_join_error_no_match).
      left_join(crop_id, by = "GTAP_crop") %>%
      # Drop the NAs.
      na.omit() %>%
      # Aggregate by CROSIT region and commodity
      group_by(CROSIT_ctry, CROSIT_crop, GLU) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      # Get a variable to identify all crop x commodity combinations in the LDS harvested area data
      mutate(CROSIT = paste(CROSIT_ctry, CROSIT_crop, sep = "_")) %>%
      # Drop all crop x commodity combinations not present in the CROSIT yield multiplier data
      filter(CROSIT %in% CROSIT_id) %>%
      # Repeat GTAP GLU production / harvested area data by the number of years in the specified agricultural productivity set
      repeat_add_columns(tibble(aglu.SPEC_AG_PROD_YEARS)) %>%
      # Rename the year vector
      rename(year = aglu.SPEC_AG_PROD_YEARS) %>%
      # Match CROSIT yield multipliers into GTAP/LDS harvested area database
      left_join_error_no_match(L112.ag_Yieldmult, by = c("CROSIT_ctry", "CROSIT_crop", "year")) ->
      L112.ag_HA

    # Calculate the adjusted production in each time period, for each CROSIT country / crop / GLU, and aggregate by GCAM region and commodity.
    # Multiply yield multipliers through future time periods to get the projected production, assuming constant land area
    # NOTE: Need to start from full GTAP tables for composite regions and commodities in the CROSIT database
    L100.LDS_ag_HA_ha %>%
      # Replace CROSIT country names with iso (this creates NAs, and use left_join instead of left_join_error_no_match).
      left_join(ctry_id, by = "iso") %>%
      # Replace CROSIT crop names with GTAP crop names (this creates NAs, and use left_join instead of left_join_error_no_match).
      left_join(crop_id, by = "GTAP_crop") %>%
      # Drop the NAs.
      na.omit() %>%
      # Repeat GTAP GLU production / harvested area data by the number of years in the specified agricultural productivity set
      repeat_add_columns(tibble(aglu.SPEC_AG_PROD_YEARS)) %>%
      # Rename the year vector
      rename(year = aglu.SPEC_AG_PROD_YEARS) %>%
      # Match in the CROSIT-GTAP/LDS matched multipliers (this creates NAs, and use left_join instead of left_join_error_no_match).
      left_join(unique(select(L112.ag_HA, CROSIT_ctry, CROSIT_crop, year, Mult)), by = c("CROSIT_ctry", "CROSIT_crop", "year")) %>%
      # Drop the NAs.
      na.omit() %>%
      # Match in GCAM regions
      left_join_error_no_match(select(iso_GCAM_regID, GCAM_region_ID, iso), by = "iso") %>%
      # Match in GCAM commodities
      left_join_error_no_match(select(FAO_ag_items_PRODSTAT, GCAM_commodity, GCAM_subsector, GTAP_crop), by = "GTAP_crop") %>%
      # Multiply base-year harvested area by the future productivity multipliers to approximate future production change
      mutate(Prod_mod = value * Mult) %>%
      # Aggregate by GCAM region / zone / commodity / year
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year, GLU) %>%
      # Aggregate future production change and base-year harvest area (to approxinate base-year production)
      summarise(value = sum(value), Prod_mod = sum(Prod_mod)) %>%
      ungroup() %>%
      # Calculate the yield ratio as future production divided by base-year production to calculate change in productivity from the base year
      mutate(YieldRatio = Prod_mod / value,
             year = as.integer(year)) %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year, GLU, YieldRatio) ->
      L112.ag_YieldRatio_R_C_Ysy_GLU

    # Reference bioenergy scenario: using median improvement rates from main agricultural crops
    L112.ag_YieldRatio_R_C_Ysy_GLU %>%
      group_by(GCAM_region_ID, year, GLU) %>%
      # Write out the median improvement rate across all crops
      summarise(YieldRatio = median(YieldRatio)) %>%
      ungroup() %>%
      # Use as the default rate for biomass
      mutate(GCAM_commodity = "biomass") %>%
      repeat_add_columns(tibble(GCAM_subsector = c("biomassGrass", "biomassTree"))) %>%
      bind_rows(L112.ag_YieldRatio_R_C_Ysy_GLU) ->
      L112.agBio_YieldRatio

    # Calculate the yield change rates for all specified ag productivity years
    # First prepare a yield rate table of 2010 values for binding rows in the loop
    L112.agBio_YieldRatio %>%
      filter(year == min(aglu.SPEC_AG_PROD_YEARS)) %>%
      rename(value = YieldRatio) -> L112.agBio_YieldRate

    # For each model timestep between 2010 to 2050, calculate the annual yield change rate based on the yield ratios
    assert_that(length(aglu.SPEC_AG_PROD_YEARS) > 1)
    for(i in 2:length(aglu.SPEC_AG_PROD_YEARS)) {
      timestep <- aglu.SPEC_AG_PROD_YEARS[i] - aglu.SPEC_AG_PROD_YEARS[i-1]

      # Yield ratios in the last model time period
      L112.agBio_YieldRatio %>%
        filter(year == aglu.SPEC_AG_PROD_YEARS[i-1]) %>%
        select(-year) %>%
        # Change the yield ratio column name for next-step calculations
        rename(YieldRatio.last = YieldRatio) -> YieldRatio.last

      L112.agBio_YieldRatio %>%
        # Yield ratios in this model time period
        filter(year == aglu.SPEC_AG_PROD_YEARS[i]) %>%
        # Join the last period ratios
        full_join(YieldRatio.last, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
        # Translate from yield ratios to annual improvement rates for this period
        mutate(value = (YieldRatio / YieldRatio.last) ^ (1 / timestep) - 1) %>%
        select(-YieldRatio, -YieldRatio.last) %>%
        # Bind all time periods in this loop
        bind_rows(L112.agBio_YieldRate) ->
        L112.agBio_YieldRate
    }

    # Fill out yield change rates to all future years to 2100 and all crops with base-year production
    # Build a table with all future year, prepare for interpolation
    A_defaultYieldRate %>%
      select(GCAM_commodity) %>%
      repeat_add_columns(tibble(FUTURE_YEARS)) %>%
      rename(year = FUTURE_YEARS) -> interp.future

    # Make a table of default yield improvement rates
    A_defaultYieldRate %>%
      gather_years %>%
      # Join the table with future years.
      full_join(interp.future, by = c("GCAM_commodity", "year")) %>%
      # Interpolate for each GCAM commodity
      group_by(GCAM_commodity) %>%
      # Interpolate to all future years from 2005 to 2100 by five-year step.
      mutate(value = approx_fun(year, value, rule = 2)) ->
      L112.defaultYieldRate

    # Next fill out missing years / commodity / region, using the default ag productivity change assumptions
    # For 2010-2050, there are a number of region-commodity-GLU combinations missing the FAO estimates entirely;
    #                and also 4 other cases missing 2030 beyond;
    #                for these incomplete cases, we use default yield change rates to fill out all years 2010-2050.
    # For 2050 beyond, all region-commodity-GLU combinations use the default yield rates assumptions

    # First, incomplete cases of ag commodities, use default yield change rates across 2010-2100
    # Get all GCAM region x commodity x GLU combinations in the production table
    L101.ag_Prod_Mt_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%
      unique() %>%
      # Join the yield change rates of all specified ag productivity years 2010-2050 (this creates NAs, use left_join instead of left_join_error_no_match)
      left_join(L112.agBio_YieldRate, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      # Find the missing cases
      filter(is.na(value)) %>%
      # Identify the imcomplete cases, will use the default yield rates across 2010-2100
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%
      unique() %>%
      # Join the default ag producivity improvement assumptions
      left_join(L112.defaultYieldRate, by = c("GCAM_commodity")) %>%
      # Keep future years (2015-2100) and 2010
      filter(year %in% FUTURE_YEARS | year %in% aglu.SPEC_AG_PROD_YEARS) %>%
      mutate(year = as.integer(year)) ->
      ag_YieldRate_incomplete.cases

    # Second, complete cases of ag commodities, yield change rates based on FAO estimates for 2010-2050
    L101.ag_Prod_Mt_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%
      unique() %>%
      # Join the yield change rates for all specified ag productivity years from 2010 to 2050 (this creates NAs, use left_join instead of left_join_error_no_match)
      left_join(L112.agBio_YieldRate, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      # Drop all the incomplete region-commodity-GLU combinations
      anti_join(ag_YieldRate_incomplete.cases, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      mutate(year = as.integer(year)) ->
      ag_YieldRate_complete.cases

    # Third, complete cases of ag commodities, use default yield rates beyond 2050 only
    ag_YieldRate_complete.cases %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%
      unique() %>%
      # Join the default ag producivity improvement assumptions for 2050 beyond
      left_join(L112.defaultYieldRate, by = c("GCAM_commodity")) %>%
      # Keep future years (2055-2100)
      filter(year %in% FUTURE_YEARS & (!(year %in% aglu.SPEC_AG_PROD_YEARS))) %>%
      # Combine complete cases 2010-2050, and imcomplete cases of ag commodities
      bind_rows(ag_YieldRate_complete.cases, ag_YieldRate_incomplete.cases) %>%
      mutate(year = as.integer(year)) %>%
      ungroup() ->
      L112.ag_YieldRate_R_C_Y_GLU

    # Do the same for biomass yield change rates
    # First, incomplete cases of biomass, use default yield change rates across 2010-2100
    # Get all combinations of region-GLU in ag production table
    L101.ag_Prod_Mt_R_C_Y_GLU %>%
      select(GCAM_region_ID, GLU) %>%
      unique() %>%
      mutate(GCAM_commodity = "biomass") %>%
      repeat_add_columns(tibble(GCAM_subsector = c("biomassGrass", "biomassTree"))) %>%
      # Join the yield change rates for all specified ag productivity years from 2010 to 2050 (this creates NAs, use left_join instead of left_join_error_no_match)
      left_join(L112.agBio_YieldRate, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      # Find the missing cases
      filter(is.na(value)) %>%
      # Identify the imcomplete cases, will use the default yield rates across 2010-2100
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%
      # Join the default ag producivity improvement assumptions
      left_join(L112.defaultYieldRate, by = c("GCAM_commodity")) %>%
      # Keep future years (2015-2100) and 2010
      filter(year %in% FUTURE_YEARS | year %in% aglu.SPEC_AG_PROD_YEARS) %>%
      mutate(year = as.integer(year)) ->
      bio_YieldRate_incomplete.cases

    # Second, complete cases of biomass, yield change rates based on FAO estimates for 2010-2050
    L101.ag_Prod_Mt_R_C_Y_GLU %>%
      select(GCAM_region_ID, GLU) %>%
      unique() %>%
      mutate(GCAM_commodity = "biomass") %>%
      repeat_add_columns(tibble(GCAM_subsector = c("biomassGrass", "biomassTree"))) %>%
      # Join the yield change rates for all specified ag productivity years from 2010 to 2050 (this creates NAs, use left_join instead of left_join_error_no_match)
      left_join(L112.agBio_YieldRate, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      # Drop all the incomplete region-commodity-GLU combinations
      anti_join(bio_YieldRate_incomplete.cases, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      mutate(year = as.integer(year)) ->
      bio_YieldRate_complete.cases

    # Third, complete cases of biomass, use default yield rates beyond 2050 only
    bio_YieldRate_complete.cases %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%
      unique() %>%
      # Join the default ag producivity improvement assumptions for 2050 beyond
      left_join(L112.defaultYieldRate, by = c("GCAM_commodity")) %>%
      # Keep future years (2055-2100)
      filter(year %in% FUTURE_YEARS & (!(year %in% aglu.SPEC_AG_PROD_YEARS))) %>%
      # Combine complete cases 2010-2050, and imcomplete cases of ag commodities
      bind_rows(bio_YieldRate_complete.cases, bio_YieldRate_incomplete.cases) %>%
      mutate(year = as.integer(year)) %>%
      ungroup() ->
      L112.bio_YieldRate_R_Y_GLU

    # Produce outputs
    L112.ag_YieldRatio_R_C_Ysy_GLU %>%
      add_title("Yield change ratios from final historical year by GCAM region / commodity / future year (specified)") %>%
      add_units("Unitless") %>%
      add_comments("Interpolate FAO CROSIT yields in 2005, 2030 and 2050 to all years from 2010 to 2050 by five-year step.") %>%
      add_comments("Calculate the yield ratios in each time period relative to the base year of 2010.") %>%
      add_legacy_name("L112.ag_YieldRatio_R_C_Ysy_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/AGLU_ctry",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "aglu/FAO/FAO_ag_CROSIT",
                     "L100.LDS_ag_HA_ha") ->
      L112.ag_YieldRatio_R_C_Ysy_GLU

    L112.ag_YieldRate_R_C_Y_GLU %>%
      add_title("Yield change rates by GCAM region / commodity / future year") %>%
      add_units("Annual rate") %>%
      add_comments("Annual productivity change rates in each time period up to 2050 are translated from the yield ratios based on FAO estimates.") %>%
      add_comments("Change rates beyond 2050 to 2100 (or when FAO estimates are missing) are based on default agriculture productivity change assumptions.") %>%
      add_legacy_name("L112.ag_YieldRate_R_C_Y_GLU") %>%
      add_precursors("aglu/A_defaultYieldRate",
                     "L101.ag_Prod_Mt_R_C_Y_GLU") ->
      L112.ag_YieldRate_R_C_Y_GLU

    L112.bio_YieldRate_R_Y_GLU %>%
      add_title("Biomass yield change rates by GCAM region / future year") %>%
      add_units("Annual rate") %>%
      add_comments("Annual productivity change rates in each time period up to 2050 are translated from the average yield ratios across all crops.") %>%
      add_comments("Change rates beyond 2050 to 2100 (or when FAO estimates are missing) are based on default agriculture productivity change assumptions.") %>%
      add_legacy_name("L112.bio_YieldRate_R_Y_GLU") %>%
      add_precursors("aglu/A_defaultYieldRate",
                     "L101.ag_Prod_Mt_R_C_Y_GLU") ->
      L112.bio_YieldRate_R_Y_GLU

    return_data(L112.ag_YieldRatio_R_C_Ysy_GLU, L112.ag_YieldRate_R_C_Y_GLU, L112.bio_YieldRate_R_Y_GLU)
  } else {
    stop("Unknown command")
  }
}
