#' module_aglu_LB112.ag_prodchange_R_C_Y_GLU
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L112.ag_YieldRatio_R_C_Ysy_GLU}, \code{L112.ag_YieldRate_R_C_Y_GLU}, \code{L112.bio_YieldRate_R_Y_GLU}. The corresponding file in the
#' original data system was \code{LB112.ag_prodchange_R_C_Y_GLU.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB112.ag_prodchange_R_C_Y_GLU <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/A_defaultYieldRate",
             FILE = "aglu/AGLU_ctry",
             FILE = "aglu/FAO_ag_items_PRODSTAT",
             FILE = "aglu/FAO_ag_CROSIT",
             "L100.LDS_ag_HA_ha",
             "L103.ag_Prod_Mt_R_C_Y_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L112.ag_YieldRatio_R_C_Ysy_GLU",
             "L112.ag_YieldRate_R_C_Y_GLU",
             "L112.bio_YieldRate_R_Y_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    A_defaultYieldRate <- get_data(all_data, "aglu/A_defaultYieldRate")
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO_ag_items_PRODSTAT")
    FAO_ag_CROSIT <- get_data(all_data, "aglu/FAO_ag_CROSIT")
    L100.LDS_ag_HA_ha <- get_data(all_data, "L100.LDS_ag_HA_ha")
    L103.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y_GLU")

    # ===================================================
    # Initial preparation of CROSIT database - replace country and crop IDs with names
    AGLU_ctry %>%
      select(CROSIT_ctry, CROSIT_country_ID) %>%
      unique() %>%
      na.omit() -> ctry_id

    FAO_ag_items_PRODSTAT %>%
      select(CROSIT_crop, CROSIT_cropID) %>%
      unique() %>%
      na.omit() -> crop_id

    # Build the table for interpolation
    FAO_ag_CROSIT %>%
      left_join_error_no_match(ctry_id, by = c("country_ID" = "CROSIT_country_ID")) %>%
      left_join_error_no_match(crop_id, by = c("crop_ID" = "CROSIT_cropID")) %>%
      select(CROSIT_ctry, CROSIT_crop) %>%
      unique() %>%
      repeat_add_columns(tibble(SPEC_AG_PROD_YEARS)) -> interp.tbl

    FAO_ag_CROSIT %>%
      left_join_error_no_match(ctry_id, by = c("country_ID" = "CROSIT_country_ID")) %>%
      left_join_error_no_match(crop_id, by = c("crop_ID" = "CROSIT_cropID")) %>%
      # NOTE: Only use total production and harvested area from CROSIT (not separating rainfed and irrigated).
      select(CROSIT_ctry, CROSIT_crop, year, HA_kha, Yield_kgHa, Prod_kt,
             -HA_kha_rainfed, -Yield_kgHa_rainfed, -Prod_kt_rainfed, -HA_kha_irrigated, -Yield_kgHa_irrigated, -Prod_kt_irrigated) %>%
      # Convert the needed numeric variables from integer to double
      mutate(HA_kha = as.double(HA_kha),
             Yield_kgHa = as.double(Yield_kgHa),
             Prod_kt = as.double(Prod_kt)) %>%
      # Intial data clean-up: some regions have 0 production with positive harvested area and a reported yield.
      # Re-calculate production where yields are available.
      mutate(Prod_kt = if_else((Prod_kt == 0 & HA_kha != 0), HA_kha * Yield_kgHa * 0.001, Prod_kt),
             # Where no yields are given, re-set harvested area to 0
             HA_kha = if_else((Prod_kt == 0 & HA_kha != 0), 0, HA_kha),
             # Eritrea has non-sensical data in the base year for wheat, and >60x yield growth to 2050.
             # Change the yield and production in the base year
             Yield_kgHa = if_else((CROSIT_ctry == "ERIT" & CROSIT_crop == "WHEA" & year == 2005), 1400, Yield_kgHa),
             Prod_kt = if_else((CROSIT_ctry == "ERIT" & CROSIT_crop == "WHEA" & year == 2005), 16 * 1.4, Prod_kt)) %>%
      # Interpolate yield
      select(-HA_kha, -Prod_kt) %>%
      full_join(interp.tbl, by = c("CROSIT_ctry", "CROSIT_crop", "year" = "SPEC_AG_PROD_YEARS")) %>%
      # Column year needs to be numeric for interpolation
      mutate(year = as.numeric(year)) %>%
      group_by(CROSIT_ctry, CROSIT_crop) %>%
      # Interpolate to specified agricultural productivity years
      mutate(Yield_kgHa = approx_fun(year, Yield_kgHa)) %>%
      ungroup() %>%
      # Drop any years not specified
      filter(year %in% SPEC_AG_PROD_YEARS) -> Yield

    # Calculating CROSIT multipliers from the base year to all specified years, for yield
    Yield %>%
      filter(year == min(SPEC_AG_PROD_YEARS)) %>%
      select(-year) %>%
      rename(Yield_base = Yield_kgHa) %>%
      full_join(Yield, by = c("CROSIT_ctry", "CROSIT_crop")) %>%
      mutate(Mult = Yield_kgHa / Yield_base) %>%
      select(-Yield_kgHa, -Yield_base) %>%
      # Drop the NaN's (crops with zero base year production / harvested area)
      na.omit() -> L112.ag_Yieldmult

    # NOTE: We apply the yield ratio to crop-specific changes in harvested area
    # This removes bias from changes in composition of GCAM commodities in the FAO projections
    # These yield multipliers are now ready to be matched into the GTAP/LDS-based table of country x crop x zone harvested area in the base year

    # First, match in the CROSIT region and commodity names for aggregation
    AGLU_ctry %>%
      select(CROSIT_ctry, iso) %>%
      unique() %>%
      na.omit() -> ctry_id

    FAO_ag_items_PRODSTAT %>%
      select(CROSIT_crop, GTAP_crop) %>%
      unique() %>%
      na.omit() -> crop_id

    # Drop all crop x commodity combinations not present in the CROSIT yield multiplier data
    L112.ag_Yieldmult %>%
      mutate(CROSIT_id = paste(CROSIT_ctry, CROSIT_crop, sep = "_")) %>%
      .[["CROSIT_id"]] %>%
      unique() -> CROSIT_id

    # Aggregating LDS harvested area by CROSIT region and crop" )
    L100.LDS_ag_HA_ha %>%
      left_join(ctry_id, by = "iso") %>%
      left_join(crop_id, by = "GTAP_crop") %>%
      na.omit() %>%
      # Aggregate by CROSIT region and commodity
      group_by(CROSIT_ctry, CROSIT_crop, GLU) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(CROSIT = paste(CROSIT_ctry, CROSIT_crop, sep = "_")) %>%
      filter(CROSIT %in% CROSIT_id) %>%
      # Repeat by the number of years in the specified agricultural productivity set, and add a year vector
      # Repeating GTAP GLU production / harvested area data by number of model timesteps
      repeat_add_columns(tibble(SPEC_AG_PROD_YEARS)) %>%
      rename(year = SPEC_AG_PROD_YEARS) %>%
      # Match in the productivity multipliers
      # Matching CROSIT yield multipliers into GTAP/LDS harvested area database
      left_join_error_no_match(L112.ag_Yieldmult, by = c("CROSIT_ctry", "CROSIT_crop", "year")) -> L112.ag_HA

    # Multiply through to get the projected production (assuming constant land area
    # Calculating the adjusted production in each time period, for each CROSIT country / crop / GLU
    # NOTE: Need to start from full GTAP tables for composite regions and commodities in the CROSIT database
    L100.LDS_ag_HA_ha %>%
      left_join(ctry_id, by = "iso") %>%
      left_join(crop_id, by = "GTAP_crop") %>%
      na.omit() %>%
      repeat_add_columns(tibble(SPEC_AG_PROD_YEARS)) %>%
      rename(year = SPEC_AG_PROD_YEARS) %>%
      # Match in multipliers (from CROSIT database)
      left_join(unique(select(L112.ag_HA, CROSIT_ctry, CROSIT_crop, year, Mult)), by = c("CROSIT_ctry", "CROSIT_crop", "year")) %>%
      na.omit() %>%
      # Match in GCAM regions
      left_join_error_no_match(select(iso_GCAM_regID, GCAM_region_ID, iso), by = "iso") %>%
      # Match in GCAM commodities
      left_join_error_no_match(select(FAO_ag_items_PRODSTAT, GCAM_commodity, GTAP_crop), by = "GTAP_crop") %>%
      # Multiply base-year harvested area by the future productivity multipliers
      mutate(Prod_mod = value * Mult) %>%
      # Aggregate by GCAM region and commodity
      # Aggregating by GCAM region / zone / commodity / year to calculate change in productivity from the base year
      group_by(GCAM_region_ID, GCAM_commodity, year, GLU) %>%
      summarise(value = sum(value), Prod_mod = sum(Prod_mod)) %>%
      ungroup() %>%
      # Calculate the yield ratio as future production divided by base-year production
      mutate(YieldRatio = Prod_mod / value,
             year = as.integer(year)) %>%
      select(GCAM_region_ID, GCAM_commodity, year, GLU, YieldRatio) -> L112.ag_YieldRatio_R_C_Ysy_GLU

    # Write out the median improvement rate across all crops, to use as the default rate for biomass
    L112.ag_YieldRatio_R_C_Ysy_GLU %>%
      group_by(GCAM_region_ID, year, GLU) %>%
      # Reference bioenergy scenario: using median improvement rates from main agricultural crops
      summarise(YieldRatio = median(YieldRatio)) %>%
      ungroup() %>%
      mutate(GCAM_commodity = "biomass") %>%
      bind_rows(L112.ag_YieldRatio_R_C_Ysy_GLU) ->
      L112.agBio_YieldRatio

    # Fill this out to all future years and all crops with base-year production, using the default ag prod change assumptions
    # Translating from yield ratios to annual improvement rates
    L112.agBio_YieldRatio %>%
      mutate(YieldRate = NA) %>%
      select(-YieldRatio) -> L112.agBio_YieldRate

    for(i in 2:length(SPEC_AG_PROD_YEARS)) {
      timestep <- SPEC_AG_PROD_YEARS[i] - SPEC_AG_PROD_YEARS[i-1]

      L112.agBio_YieldRatio %>%
        filter(year == SPEC_AG_PROD_YEARS[i-1]) %>%
        select(-year) %>%
        rename(YieldRatio.i = YieldRatio) -> YieldRatio.i

      L112.agBio_YieldRatio %>%
        filter(year == SPEC_AG_PROD_YEARS[i]) %>%
        full_join(YieldRatio.i, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
        mutate(YieldRate = YieldRatio / YieldRatio.i ^ (1 / timestep) - 1) %>%
        select(-YieldRatio, -YieldRatio.i) %>%
        bind_rows(L112.agBio_YieldRate) %>%
        na.omit() ->
        L112.agBio_YieldRate
    }

    # Match these annual improvement rates into a table of existing crop yields

    # Build the table for all future years interpolation
    A_defaultYieldRate %>%
      select(GCAM_commodity) %>%
      repeat_add_columns(tibble(FUTURE_YEARS)) %>%
      rename(year = FUTURE_YEARS) -> interp.future

    # First, make a table of default yield improvement rates
    A_defaultYieldRate %>%
      gather(year, value, -GCAM_commodity) %>%
      mutate(year = as.integer(year)) %>%
      full_join(interp.future, by = c("GCAM_commodity", "year")) %>%
      group_by(GCAM_commodity) %>%
      mutate(Yield.default = approx_fun(year, value)) %>%
      ungroup() -> L112.defaultYieldRate

    # Fill this out to all future years
    L103.ag_Prod_Mt_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GLU) %>%
      unique() %>%
      left_join(L112.agBio_YieldRate, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      full_join(L112.defaultYieldRate, by = c("GCAM_commodity", "year")) %>%
      filter(year %in% FUTURE_YEARS | year %in% SPEC_AG_PROD_YEARS) %>%
      mutate(value = if_else(is.na(YieldRate), Yield.default, YieldRate),
             year = as.integer(year)) %>%
      select(-Yield.default, -YieldRate) %>%
      ungroup() ->
      L112.ag_YieldRate_R_C_Y_GLU

    # Writing out biomass yields
    L103.ag_Prod_Mt_R_C_Y_GLU %>%
      select(GCAM_region_ID, GLU) %>%
      unique() %>%
      left_join(filter(L112.agBio_YieldRate, GCAM_commodity == "biomass"), by = c("GCAM_region_ID", "GLU"))%>%
      full_join(L112.defaultYieldRate, by = c("GCAM_commodity", "year")) %>%
      filter(year %in% FUTURE_YEARS | year %in% SPEC_AG_PROD_YEARS) %>%
      mutate(value = if_else(is.na(YieldRate), Yield.default, YieldRate),
             year = as.integer(year)) %>%
      select(-Yield.default, -YieldRate) %>%
      ungroup() ->
      L112.bio_YieldRate_R_Y_GLU

    # ===================================================

    # Produce outputs
    L112.ag_YieldRatio_R_C_Ysy_GLU %>%
      add_title("Yield change ratios from final historical year by GCAM region / commodity (specified) / future year (specified)") %>%
      add_units("Unitless") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L112.ag_YieldRatio_R_C_Ysy_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/A_defaultYieldRate",
                     "aglu/AGLU_ctry",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "aglu/FAO_ag_CROSIT",
                     "L100.LDS_ag_HA_ha",
                     "L103.ag_Prod_Mt_R_C_Y_GLU") ->
      L112.ag_YieldRatio_R_C_Ysy_GLU

    L112.ag_YieldRate_R_C_Y_GLU %>%
      add_title("Yield change rates by GCAM region / commodity / future year") %>%
      add_units("Annual rate") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L112.ag_YieldRate_R_C_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/A_defaultYieldRate",
                     "aglu/AGLU_ctry",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "aglu/FAO_ag_CROSIT",
                     "L100.LDS_ag_HA_ha",
                     "L103.ag_Prod_Mt_R_C_Y_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L112.ag_YieldRate_R_C_Y_GLU

    L112.bio_YieldRate_R_Y_GLU %>%
      add_title("Biomass yield change rates by GCAM region / future year") %>%
      add_units("Annual rate") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L112.bio_YieldRate_R_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/A_defaultYieldRate",
                     "aglu/AGLU_ctry",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "aglu/FAO_ag_CROSIT",
                     "L100.LDS_ag_HA_ha",
                     "L103.ag_Prod_Mt_R_C_Y_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L112.bio_YieldRate_R_Y_GLU

    return_data(L112.ag_YieldRatio_R_C_Ysy_GLU, L112.ag_YieldRate_R_C_Y_GLU, L112.bio_YieldRate_R_Y_GLU)
  } else {
    stop("Unknown command")
  }
}



