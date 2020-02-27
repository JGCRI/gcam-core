# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LA101.ag_FAO_R_C_Y
#'
#' Aggregate FAO food consumption, ag production, and harvested area
#' data to GCAM regions and GCAM commodities.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L101.ag_Food_Mt_R_C_Y}, \code{L101.ag_Food_Pcal_R_C_Y}, \code{L101.ag_kcalg_R_C_Y}, \code{L101.ag_HA_bm2_R_C_Y}, \code{L101.ag_Prod_Mt_R_C_Y}. The corresponding file in the
#' original data system was \code{LA101.ag_FAO_R_C_Y.R} (aglu level1).
#' @details This chunk aggregates FAO food consumption, agricultural production,
#' and harvested area data up to GCAM commodities and GCAM regions. Data is converted
#' from FAO units (tons, hectares) to GCAM units (Mt, Pcal, billion km2). Note that
#' FAO's alfalfa production in the USA is divided by 4 "for consistency with USDA".
#' Note (August 2018 GPK revision) - The FAO production and harvested area are disaggregated
#' to basin PRIOR to aggregation by GCAM region. This reduces the bias from using a single
#' year (around 2000) to disaggregate to basin, in multi-country regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join arrange bind_rows distinct filter full_join if_else group_by inner_join left_join mutate right_join select summarise
#' @importFrom tidyr complete drop_na replace_na
#' @author KVC March 2017 (revised August 2018 by GPK)
module_aglu_LA101.ag_FAO_R_C_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
             FILE = "aglu/FAO/FAO_ag_items_cal_SUA",
             FILE = "aglu/LDS/LDS_land_types",
             "L100.FAO_ag_Food_t",
             "L100.FAO_ag_HA_ha",
             "L100.FAO_ag_Prod_t",
             "L100.LDS_ag_HA_ha",
             "L100.LDS_ag_prod_t",
             "L100.Land_type_area_ha"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.ag_Food_Mt_R_C_Y",
             "L101.ag_Food_Pcal_R_C_Y",
             "L101.ag_kcalg_R_C_Y",
             "L101.ag_HA_bm2_R_C_Y",
             "L101.ag_HA_bm2_R_C_Y_GLU",
             "L101.ag_Prod_Mt_R_C_Y",
             "L101.ag_Prod_Mt_R_C_Y_GLU",
             "L101.ag_Yield_kgm2_R_C_Y_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    GLU <- default_share_GLU <- HA_share_GLU <- prod_share_GLU <-
      Category <- LT_SAGE <- LT_HYDE <- countries <- country.codes <-
      item.codes <- element <- element.codes <- GCAM_commodity <-
      value <- GCAM_region_ID <- year <- Mcal_t <- value.y <- value.x <-
      item <- iso <- production <- harvested.area <- NULL # silence package check.

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_items_PRODSTAT")
    FAO_ag_items_cal_SUA <- get_data(all_data, "aglu/FAO/FAO_ag_items_cal_SUA")
    LDS_land_types <- get_data(all_data, "aglu/LDS/LDS_land_types")
    L100.FAO_ag_Food_t <- get_data(all_data, "L100.FAO_ag_Food_t")
    L100.FAO_ag_HA_ha <- get_data(all_data, "L100.FAO_ag_HA_ha")
    L100.FAO_ag_Prod_t <- get_data(all_data, "L100.FAO_ag_Prod_t")
    L100.LDS_ag_HA_ha <- get_data(all_data, "L100.LDS_ag_HA_ha")
    L100.LDS_ag_prod_t <- get_data(all_data, "L100.LDS_ag_prod_t")
    L100.Land_type_area_ha <- get_data(all_data, "L100.Land_type_area_ha")

    # Process FAO food consumption data (tons): remove unnecessary columns, convert units, aggregate to region and commodity
    L100.FAO_ag_Food_t %>%
      select(-countries, -country.codes, -item.codes, -element, -element.codes) %>%                               # Remove unnecessary columns
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%                                                   # Map in ISO codes
      left_join(FAO_ag_items_cal_SUA, by = "item") %>%                                                           # Map in GCAM commodities
      filter(!is.na(GCAM_commodity)) %>%                                                                          # Remove commodities not included in GCAM
      mutate(value = value * CONV_TON_MEGATON) %>%                                                               # Convert from tons to Mt
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%                                                          # Group by region, commodity, year
      summarise(value = sum(value)) %>%                                                                           # Aggregate then map to appropriate data frame
      ungroup() %>%                                                                                               # Ungroup before complete
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(value = 0)) ->                                                   # Fill in missing region/commodity combinations with 0
      L101.ag_Food_Mt_R_C_Y

    # Process FAO food consumption data (Pcal): remove unnecessary columns, convert units, aggregate to region and commodity
    L100.FAO_ag_Food_t %>%
      select(-countries, -country.codes, -item.codes, -element, -element.codes) %>%                               # Remove unnecessary columns
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%                                                   # Map in ISO codes
      left_join(FAO_ag_items_cal_SUA, by = "item") %>%                                                           # Map in GCAM commodities
      filter(!is.na(GCAM_commodity)) %>%                                                                          # Remove commodities not included in GCAM
      mutate(value = value * Mcal_t * CONV_MCAL_PCAL) %>%                                                        # Convert from tons to Pcal
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%                                                          # Group by region, commodity, year
      summarise(value = sum(value)) %>%                                                                           # Aggregate then map to appropriate data frame
      ungroup() %>%                                                                                               # Ungroup before complete
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(value = 0)) ->                                                   # Fill in missing region/commodity combinations with 0
      L101.ag_Food_Pcal_R_C_Y

    # Calculate average caloric content of consumed commodities (kcal/g)
    L101.ag_Food_Pcal_R_C_Y %>%
      left_join(L101.ag_Food_Mt_R_C_Y, by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%                  # Join food in Mt to food in Pcal
      mutate(value = if_else(value.y == 0, 1, value.x / value.y)) %>%                                          # Calculate average caloric content, set NA values to 1
      select(-value.x, -value.y) ->                                                                             # Remove extra columns
      L101.ag_kcalg_R_C_Y

    # Clean FAO production and harvested area tables
    L100.FAO_ag_HA_ha %>%
      select(-countries, -country.codes, -item.codes, -element, -element.codes) %>%                             # Remove unnecessary columns
      mutate(iso = if_else(iso %in% c("srb", "mne"), "scg", iso)) %>%                                           # Re-map Serbia (srb) and Montenegro (mne) to the iso code for the single country (scg)
      group_by(item, iso, year) %>%                                                                             # scg is the iso code in the Monfreda data because its base year is ~2000
      summarise(value = sum(value)) ->                                                                          # Aggregate by ISO, item, and region (some iso codes apply to multiple lines in data)
      FAO_ag_HA_ha

    L100.FAO_ag_Prod_t %>%
      select(-countries, -country.codes, -item.codes, -element, -element.codes) %>%                             # Remove unnecessary columns
      mutate(value = if_else(iso == "usa" & item == "Alfalfa for forage and silage",
                             value / 4, value),                                                                 # Divide USA Alfalfa production by 4 "for consistency with USDA"
             iso = if_else(iso %in% c("srb", "mne"), "scg", iso)) %>%                                           # Re-map Serbia (srb) and Montenegro (mne) to the iso code for the single country (scg)
      group_by(item, iso, year) %>%                                                                             # scg is the iso code in the Monfreda data because its base year is ~2000
      summarise(value = sum(value)) ->                                                                          # Aggregate by ISO, item, and region (some iso codes apply to multiple lines in data)
      FAO_ag_Prod_t

    # Set production to zero when harvested area is zero and vice versa
    FAO_ag_HA_ha %>% ungroup %>%
      inner_join(FAO_ag_Prod_t, by = c("iso", "item", "year")) %>%                                              # Join production and harvested area
      rename(harvested.area = value.x, production = value.y) %>%                                              # Rename variables
      mutate(harvested.area = if_else(production == 0, 0, harvested.area),                                  # Set harvested area to zero if production is zero
             production = if_else(harvested.area == 0, 0, production)) ->                                       # Set production to zero if harvested area is zero
      FAO_PRODSTAT_MERGED

    # Next we downscale the data from countries to basins, using the basin-within-country shares
    # of each GCAM commodity in the Monfreda (pre-processed by LDS) data on harvested area and production
    # Note - using GCAM commodities rather than specific crops in this task. This avoids dropping data, particularly
    # for the grass fodder crops which are poorly matched with the FAO data.
    L100.LDS_ag_HA_ha %>%
      left_join_error_no_match(L100.LDS_ag_prod_t,
                               by = c("iso", "GLU", "GTAP_crop")) %>%                                           # Join the Monfreda/LDS datasets of production and harvested area
      rename(harvested.area = value.x,
             production = value.y) %>%
      left_join(FAO_ag_items_PRODSTAT[c("GTAP_crop", "GCAM_commodity")], by = "GTAP_crop") %>%                  # Join in the GCAM commodities and aggregate.
      drop_na() %>%   # drop any crops not considered in GCAM
      group_by(iso, GCAM_commodity, GLU) %>%
      summarise(harvested.area = sum(harvested.area),
                production = sum(production)) %>%
      ungroup() %>%
      group_by(iso, GCAM_commodity) %>%
      mutate(HA_share_GLU = harvested.area / sum(harvested.area),                                               # Compute the shares of country/crop/GLU within country/crop
             prod_share_GLU = production / sum(production)) %>%
      ungroup() ->
      LDS_ctry_crop_SHARES


    # Compute default basin-within-country shares to be used where FAOSTAT has data but LDS/Monfreda does not.
    # These shares are computed from the harvested area of all crops available in Monfreda.
    # Harvested area is used to avoid compositional bias from different crop types in different basins.
    LDS_ctry_crop_SHARES %>%
      group_by(iso, GLU) %>%
      summarise(harvested.area = sum(harvested.area)) %>%
      ungroup() %>%
      group_by(iso) %>%
      mutate(default_share_GLU = harvested.area / sum(harvested.area)) %>%
      ungroup() %>%
      select(iso, GLU, default_share_GLU) ->
      LDS_ctry_SHARES

    # only take the columns required for later steps in the LDS_ctry_crop_SHARES data table
    LDS_ctry_crop_SHARES <- select(LDS_ctry_crop_SHARES, iso, GLU, GCAM_commodity, HA_share_GLU, prod_share_GLU)

    # FAO_PRODSTAT_DOWNSCALED: FAO Prodstat data aggregated by GCAM commodity and downscaled to GLU.
    FAO_PRODSTAT_MERGED %>%
      left_join_error_no_match(distinct(select(FAO_ag_items_PRODSTAT, item, GCAM_commodity)), by = "item",      # distinct() to avoid duplicating data for items with multiple rows in FAO_ag_items_PRODSTAT
                               ignore_columns = "GCAM_commodity") %>%                                           # ignore GCAM_commodity column to avoid error in ljenm (this column has NA for FAO items not modeled in GCAM)
      filter(!is.na(GCAM_commodity)) %>%                                                                        # Remove commodities not included in GCAM
      group_by(iso, GCAM_commodity, year) %>%
      summarise(harvested.area = sum(harvested.area),
                production = sum(production)) %>%
      ungroup() ->
      FAO_PRODSTAT_DOWNSCALED

    # First group: crops and countries in BOTH datasetes (LDS/Monfreda and FAOSTAT)
    FAO_PRODSTAT_DOWNSCALED %>%
      right_join(LDS_ctry_crop_SHARES, by = c("iso", "GCAM_commodity")) %>%                                      # use right_join to exclude crops and countries not in the LDS data
      drop_na() %>%                                                                                               # NAs are observations in LDS but not FAOSTAT. These are dropped.
      mutate(harvested.area = harvested.area * HA_share_GLU,                                                      # multiply through by shares of GLU within country and crop
             production = production * prod_share_GLU) %>%
      select(-HA_share_GLU, -prod_share_GLU) ->
      FAO_PRODSTAT_DOWNSCALED_matches

    # Second group: country/crop observations missing in LDS/Monfreda where some crops for the country are available
    FAO_PRODSTAT_DOWNSCALED %>%
      anti_join(LDS_ctry_crop_SHARES, by = c("iso", "GCAM_commodity")) %>%                                        # Filter the dataset to only observations where the country and crop couldn't be matched
      full_join(LDS_ctry_SHARES, by = "iso") %>%
      drop_na() %>%                                                                                               # Drop places where entire country is not available in LDS/Monfreda data
      mutate(harvested.area = harvested.area * default_share_GLU,                                                 # multiply through by shares of GLU within country and crop
             production = production * default_share_GLU) %>%
      select(-default_share_GLU) ->
      FAO_PRODSTAT_DOWNSCALED_cropNA

    # Third group: country/crop observations in countries excluded from Monfreda/LDS, but that have cropland in Hyde.
    # These use the Hyde data to downscale to basin. In this method, there is a fourth group that is dropped entirely:
    # countries with data in FAOSTAT, but excluded from Monfreda/LDS and that have no cropland in Hyde.
    # First, compute the cropland basin-within-country from the Hyde countries, for only the countries that are in FAOSTAT but not Monfreda/LDS
    L100.Land_type_area_ha %>%
      filter(iso %in% FAO_PRODSTAT_DOWNSCALED$iso &                                                              # countries in FAOSTAT
               !iso %in% LDS_ctry_SHARES$iso) %>%                                                                # but not in Monfreda/LDS
      left_join_error_no_match(select(LDS_land_types, Category, LT_SAGE, LT_HYDE), by = c(land_code = "Category")) %>%
      filter(LT_HYDE == "Cropland",
             LT_SAGE != "Unknown",                                                                               # Do not assign crop production to lands that are "unknown" in SAGE as these have no land allocation in GCAM.
             year == 2000) %>%
      group_by(iso, GLU) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      group_by(iso) %>%
      mutate(default_share_GLU = value / sum(value)) %>%
      ungroup() %>%
      select(iso, GLU, default_share_GLU) ->
      Hyde_cropland_share_basin

    FAO_PRODSTAT_DOWNSCALED %>%
      filter(iso %in% Hyde_cropland_share_basin$iso) %>%                                                         # Filter to the same set of countries as above (in FAOSTAT, not Monfreda/LDS, with cropland in Hyde)
      full_join(Hyde_cropland_share_basin, by = "iso") %>%
      mutate(harvested.area = harvested.area * default_share_GLU,                                                # multiply through by shares of GLU within country and crop
             production = production * default_share_GLU) %>%
      select(-default_share_GLU) ->
      FAO_PRODSTAT_DOWNSCALED_countryNA

    # FAO downscaled data: bind the three groups together
    FAO_PRODSTAT_DOWNSCALED <- bind_rows(FAO_PRODSTAT_DOWNSCALED_matches,
                                         FAO_PRODSTAT_DOWNSCALED_cropNA,
                                         FAO_PRODSTAT_DOWNSCALED_countryNA)

    # Process FAO production data: convert units, aggregate to region, commodity, and GLU
    FAO_PRODSTAT_DOWNSCALED %>%
      select(iso, GCAM_commodity, GLU, year, production) %>%                                                    # Select relevant columns (not harvested.area)
      rename(value = production) %>%                                                                            # Rename column since tests are expecting "value"
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%                                                  # Map in ISO codes
      group_by(GCAM_region_ID, GCAM_commodity, GLU, year) %>%                                                   # Group by region, commodity, GLU, year
      summarise(value = sum(value)) %>%                                                                         # Aggregate then map to appropriate data frame
      mutate(value = value * CONV_TON_MEGATON) %>%                                                              # Convert from tons to Mt
      ungroup() ->                                                                                              # Ungroup before complete
      L101.ag_Prod_Mt_R_C_Y_GLU

    # Also write out the production volumes without basin-level detail (by region, crop, year)
    L101.ag_Prod_Mt_R_C_Y_GLU %>%
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(value = 0))  ->                                                # Fill in missing region/commodity combinations with 0
      L101.ag_Prod_Mt_R_C_Y

    # Now, Process FAO harvested area data: convert units, aggregate to region, commodity, and GLU
    FAO_PRODSTAT_DOWNSCALED %>%
      select(iso, GCAM_commodity, GLU, year, harvested.area) %>%                                              # Select relevant columns (not production)
      rename(value = harvested.area) %>%                                                                      # Rename column since tests are expecting "value"
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%                                               # Map in ISO codes
      group_by(GCAM_region_ID, GCAM_commodity, GLU, year) %>%                                                      # Group by region, commodity, GLU, year
      summarise(value = sum(value)) %>%                                                                       # Aggregate then map to appropriate data frame
      mutate(value = value * CONV_HA_BM2) %>%                                                                # Convert from hectares to billion m2
      ungroup() ->                                                                                           # Ungroup before complete
      L101.ag_HA_bm2_R_C_Y_GLU

    L101.ag_HA_bm2_R_C_Y_GLU %>%
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(value = 0)) ->                                               # Fill in missing region/commodity combinations with 0
      L101.ag_HA_bm2_R_C_Y

    # Calculate initial yield estimates in kilograms per square meter by region, crop, year, and GLU
    # Yield in kilograms per square meter
    L101.ag_Prod_Mt_R_C_Y_GLU %>%
      left_join(L101.ag_HA_bm2_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU", "year")) %>%
      mutate(value = value.x / value.y) %>%
      replace_na(list(value = 0)) %>%
      select(-value.x, -value.y) %>%
      arrange(GLU) ->  # so we match old d.s. order
      L101.ag_Yield_kgm2_R_C_Y_GLU

    # Produce outputs
    L101.ag_Food_Mt_R_C_Y %>%
      add_title("FAO food consumption by GCAM region, commodity, and year") %>%
      add_units("Mt/yr") %>%
      add_comments("Aggregates FAO data by GCAM region, commodity, and year") %>%
      add_comments("Data is also converted from tons to Mt") %>%
      add_legacy_name("L101.ag_Food_Mt_R_C_Y") %>%
      add_precursors("L100.FAO_ag_Food_t", "aglu/FAO/FAO_ag_items_cal_SUA", "common/iso_GCAM_regID") ->
      L101.ag_Food_Mt_R_C_Y
    L101.ag_Food_Pcal_R_C_Y %>%
      add_title("FAO food consumption by GCAM region, commodity, and year") %>%
      add_units("Pcal/yr") %>%
      add_comments("Aggregates FAO data by GCAM region, commodity, and year") %>%
      add_comments("Data is also converted from tons to Pcal") %>%
      add_legacy_name("L101.ag_Food_Pcal_R_C_Y") %>%
      add_precursors("L100.FAO_ag_Food_t", "aglu/FAO/FAO_ag_items_cal_SUA", "common/iso_GCAM_regID") ->
      L101.ag_Food_Pcal_R_C_Y
    L101.ag_kcalg_R_C_Y %>%
      add_title("Weighted average commodity caloric content by GCAM region, commodity, and year") %>%
      add_units("kcal/g") %>%
      add_comments("Combines the L101.ag_Food_Mt_R_C_Y and L101.ag_Food_Pcal_R_C_Y data frames") %>%
      add_legacy_name("L101.ag_kcalg_R_C_Y") %>%
      add_precursors("L100.FAO_ag_Food_t", "aglu/FAO/FAO_ag_items_cal_SUA", "common/iso_GCAM_regID") ->
      L101.ag_kcalg_R_C_Y

    L101.ag_HA_bm2_R_C_Y_GLU %>%
      add_title("Harvested area by GCAM region, commodity, year, and GLU") %>%
      add_units("billion km2") %>%
      add_comments("FAO data downscaled to GLU then aggregated by GCAM region, commodity, and GLU") %>%
      add_comments("Data was also converted from HA to billion km2") %>%
      add_legacy_name("L103.ag_HA_bm2_R_C_Y_GLU") %>%
      add_precursors("L100.FAO_ag_HA_ha", "aglu/FAO/FAO_ag_items_PRODSTAT", "L100.LDS_ag_HA_ha", "common/iso_GCAM_regID",
                     "aglu/LDS/LDS_land_types", "L100.Land_type_area_ha") ->
      L101.ag_HA_bm2_R_C_Y_GLU
    L101.ag_HA_bm2_R_C_Y %>%
      add_title("Harvested area by GCAM region, commodity, and year") %>%
      add_units("billion km2") %>%
      add_comments("FAO data downscaled to GLU then aggregated by GCAM region and commodity") %>%
      add_comments("Data was also converted from HA to billion km2") %>%
      add_comments("Country/crop combinations with zero production were assigned zero harvested area") %>%
      add_legacy_name("L101.ag_HA_bm2_R_C_Y") %>%
      same_precursors_as(L101.ag_HA_bm2_R_C_Y_GLU) ->
      L101.ag_HA_bm2_R_C_Y
    L101.ag_Prod_Mt_R_C_Y_GLU %>%
      add_title("Agricultural production by GCAM region, commodity, year, and GLU") %>%
      add_units("Mt/yr") %>%
      add_comments("FAO data downscaled to GLU then aggregated by GCAM region, commodity, and GLU") %>%
      add_comments("Data was also converted from tons to Mt") %>%
      add_comments("USA alfalfa production was divided by 4 for consistency with USDA") %>%
      add_comments("Country/crop combinations with zero harvested area were assigned zero production") %>%
      add_legacy_name("L101.ag_Prod_Mt_R_C_Y_GLU") %>%
      add_precursors("L100.FAO_ag_Prod_t", "aglu/FAO/FAO_ag_items_PRODSTAT", "L100.LDS_ag_prod_t", "common/iso_GCAM_regID",
                     "aglu/LDS/LDS_land_types", "L100.Land_type_area_ha") ->
      L101.ag_Prod_Mt_R_C_Y_GLU
    L101.ag_Prod_Mt_R_C_Y %>%
      add_title("Agricultural production by GCAM region, commodity, and year") %>%
      add_units("Mt/yr") %>%
      add_comments("FAO data downscaled to GLU then aggregated by GCAM region and commodity") %>%
      add_comments("Data was also converted from tons to Mt") %>%
      add_comments("USA alfalfa production was divided by 4 for consistency with USDA") %>%
      add_comments("Country/crop combinations with zero harvested area were assigned zero production") %>%
      add_legacy_name("L101.ag_Prod_Mt_R_C_Y") %>%
      same_precursors_as(L101.ag_Prod_Mt_R_C_Y_GLU) ->
      L101.ag_Prod_Mt_R_C_Y
    L101.ag_Yield_kgm2_R_C_Y_GLU %>%
      add_title("Unadjusted agronomic yield by GCAM region / commodity / year / GLU") %>%
      add_units("kg/m2") %>%
      add_comments("Agricultural yield computed based on production and harvested area") %>%
      add_legacy_name("L103.ag_Yield_kgm2_R_C_Y_GLU") %>%
      same_precursors_as(L101.ag_Prod_Mt_R_C_Y_GLU) ->
      L101.ag_Yield_kgm2_R_C_Y_GLU

    return_data(L101.ag_Food_Mt_R_C_Y, L101.ag_Food_Pcal_R_C_Y, L101.ag_kcalg_R_C_Y, L101.ag_HA_bm2_R_C_Y_GLU,
                L101.ag_HA_bm2_R_C_Y, L101.ag_Prod_Mt_R_C_Y_GLU, L101.ag_Prod_Mt_R_C_Y, L101.ag_Yield_kgm2_R_C_Y_GLU)
  } else {
    stop("Unknown command")
  }
}
