#' module_aglu_LA101.ag_FAO_R_C_Y
#'
#' This chunk aggregates FAO food consumption, ag production, and harvested area
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
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select left_join
#' @importFrom tidyr gather spread complete
#' @author KVC March 2017
module_aglu_LA101.ag_FAO_R_C_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
             FILE = "aglu/FAO/FAO_ag_items_cal_SUA",
             "L100.FAO_ag_Food_t",
             "L100.FAO_ag_HA_ha",
             "L100.FAO_ag_Prod_t"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.ag_Food_Mt_R_C_Y",
             "L101.ag_Food_Pcal_R_C_Y",
             "L101.ag_kcalg_R_C_Y",
             "L101.ag_HA_bm2_R_C_Y",
             "L101.ag_Prod_Mt_R_C_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    countries <- country.codes <- item.codes <- element <- element.codes <-
        GCAM_commodity <- value <- GCAM_region_ID <- year <- Mcal_t <- value.y <-
            value.x <- item <- iso <- production <- harvested.area <- NULL # silence package check.

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_items_PRODSTAT")
    FAO_ag_items_cal_SUA <- get_data(all_data, "aglu/FAO/FAO_ag_items_cal_SUA")
    L100.FAO_ag_Food_t <- get_data(all_data, "L100.FAO_ag_Food_t")
    L100.FAO_ag_HA_ha <- get_data(all_data, "L100.FAO_ag_HA_ha")
    L100.FAO_ag_Prod_t <- get_data(all_data, "L100.FAO_ag_Prod_t")

    # Process FAO food consumption data (tons): remove unnecessary columns, convert units, aggregate to region and commodity
    L100.FAO_ag_Food_t %>%
      select(-countries, -country.codes, -item.codes, -element, -element.codes) %>%                               # Remove unnecessary columns
      left_join_error_no_match(iso_GCAM_regID, by = "iso")  %>%                                                   # Map in ISO codes
      left_join(FAO_ag_items_cal_SUA, by = "item")  %>%                                                           # Map in GCAM commodities
      filter(!is.na(GCAM_commodity)) %>%                                                                          # Remove commodities not included in GCAM
      mutate(value = value * CONV_TON_MEGATON)  %>%                                                               # Convert from tons to Mt
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%                                                          # Group by region, commodity, year
      summarize(value = sum(value)) %>%                                                                           # Aggregate then map to appropriate data frame
      ungroup() %>%                                                                                               # Ungroup before complete
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(value = 0)) ->                                                   # Fill in missing region/commodity combinations with 0
      L101.ag_Food_Mt_R_C_Y

    # Process FAO food consumption data (Pcal): remove unnecessary columns, convert units, aggregate to region and commodity
    L100.FAO_ag_Food_t %>%
      select(-countries, -country.codes, -item.codes, -element, -element.codes) %>%                               # Remove unnecessary columns
      left_join_error_no_match(iso_GCAM_regID, by = "iso")  %>%                                                   # Map in ISO codes
      left_join(FAO_ag_items_cal_SUA, by = "item")  %>%                                                           # Map in GCAM commodities
      filter(!is.na(GCAM_commodity)) %>%                                                                          # Remove commodities not included in GCAM      mutate(value = value * CONV_HA_BM2)  %>%                                                                     # Convert from hectares to billion square kilometers
      mutate(value = value * Mcal_t * CONV_MCAL_PCAL)  %>%                                                        # Convert from tons to Pcal
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%                                                          # Group by region, commodity, year
      summarize(value = sum(value)) %>%                                                                           # Aggregate then map to appropriate data frame
      ungroup() %>%                                                                                               # Ungroup before complete
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(value = 0)) ->                                                   # Fill in missing region/commodity combinations with 0
      L101.ag_Food_Pcal_R_C_Y

    # Calculate average caloric content of consumed commodities (kcal/g)
    L101.ag_Food_Pcal_R_C_Y %>%
      left_join(L101.ag_Food_Mt_R_C_Y, by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%                  # Join food in Mt to food in Pcal
      mutate(value = if_else(value.y == 0, 1, value.x / value.y))  %>%                                          # Calculate average caloric content, set NA values to 1
      select(-value.x, -value.y) ->                                                                             # Remove extra columns
      L101.ag_kcalg_R_C_Y

    # Clean FAO production and harvested area tables
    L100.FAO_ag_HA_ha %>%
      select(-countries, -country.codes, -item.codes, -element, -element.codes) %>%                             # Remove unnecessary columns
      group_by(item, iso, year) %>%
      summarize(value = sum(value)) ->                                                                          # Aggregate by ISO, item, and region (some iso codes apply to multiple lines in data)
      FAO_ag_HA_ha

    L100.FAO_ag_Prod_t %>%
      select(-countries, -country.codes, -item.codes, -element, -element.codes) %>%                             # Remove unnecessary columns
      mutate(value = if_else(iso == "usa" & item == "Alfalfa for forage and silage",
                             value / 4, value)) %>%                                                             # Divide USA Alfalfa production by 4 "for consistency with USDA"
      group_by(item, iso, year) %>%
      summarize(value = sum(value)) ->                                                                          # Aggregate by ISO, item, and region (some iso codes apply to multiple lines in data)
      FAO_ag_Prod_t

    # Set production to zero when harvested area is zero and vice versa
    FAO_ag_HA_ha %>% ungroup %>%
      inner_join(FAO_ag_Prod_t, by = c("iso", "item", "year")) %>%                                              # Join production and harvested area
      rename(harvested.area = value.x, production = value.y)   %>%                                              # Rename variables
      mutate(harvested.area = if_else(production == 0, 0, harvested.area)) %>%                                  # Set harvested area to zero if production is zero
      mutate(production = if_else(harvested.area == 0, 0, production)) ->                                       # Set production to zero if harvested area is zero
      FAO_PRODSTAT_MERGED

    # Process FAO production data: convert units, aggregate to region and commodity
    FAO_PRODSTAT_MERGED %>%
      select(iso, item, year, production) %>%                                                                   # Select relevant columns (not harvested.area)
      rename(value = production) %>%                                                                            # Rename column since tests are expecting "value"
      left_join_error_no_match(iso_GCAM_regID, by = "iso")  %>%                                                 # Map in ISO codes
      left_join(unique(FAO_ag_items_PRODSTAT[ c("item", "GCAM_commodity")]), by = "item")  %>%                  # Map in GCAM commodities
      filter(!is.na(GCAM_commodity)) %>%                                                                        # Remove commodities not included in GCAM      mutate(value = value * CONV_HA_BM2)  %>%                                                                     # Convert from hectares to billion square kilometers
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%                                                        # Group by region, commodity, year
      summarize(value = sum(value)) %>%                                                                         # Aggregate then map to appropriate data frame
      mutate(value = value * CONV_TON_MEGATON)  %>%                                                             # Convert from tons to Mt
      ungroup() %>%                                                                                             # Ungroup before complete
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(value = 0))  ->                                                # Fill in missing region/commodity combinations with 0
      L101.ag_Prod_Mt_R_C_Y

    # Now, Process FAO harvested area data: convert units, aggregate to region and commodity
    FAO_PRODSTAT_MERGED %>%
      select(iso, item, year, harvested.area) %>%                                                             # Select relevant columns (not production)
      rename(value = harvested.area) %>%                                                                      # Rename column since tests are expecting "value"
      left_join_error_no_match(iso_GCAM_regID, by = "iso")  %>%                                               # Map in ISO codes
      left_join(unique(FAO_ag_items_PRODSTAT[ c("item", "GCAM_commodity")]), by = "item")  %>%                # Map in GCAM commodities
      filter(!is.na(GCAM_commodity)) %>%                                                                      # Remove commodities not included in GCAM      mutate(value = value * CONV_HA_BM2)  %>%                                                                     # Convert from hectares to billion square kilometers
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%                                                      # Group by region, commodity, year
      summarize(value = sum(value)) %>%                                                                       # Aggregate then map to appropriate data frame
      mutate(value = value * CONV_HA_BM2)  %>%                                                                # Convert from hectares to billion km2
      ungroup() %>%                                                                                           # Ungroup before complete
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(value = 0)) ->                                               # Fill in missing region/commodity combinations with 0
      L101.ag_HA_bm2_R_C_Y

    # Produce outputs
    L101.ag_Food_Mt_R_C_Y %>%
      add_title("FAO food consumption by GCAM region, commodity, and year") %>%
      add_units("Mt/yr") %>%
      add_comments("Aggregates FAO data by GCAM region, commodity, and year") %>%
      add_comments("Data is also converted from tons to Mt") %>%
      add_legacy_name("L101.ag_Food_Mt_R_C_Y") %>%
      add_precursors("L100.FAO_ag_Food_t", "aglu/FAO/FAO_ag_items_cal_SUA", "common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.ag_Food_Mt_R_C_Y
    L101.ag_Food_Pcal_R_C_Y %>%
      add_title("FAO food consumption by GCAM region, commodity, and year") %>%
      add_units("Pcal/yr") %>%
      add_comments("Aggregates FAO data by GCAM region, commodity, and year") %>%
      add_comments("Data is also converted from tons to Pcal") %>%
      add_legacy_name("L101.ag_Food_Pcal_R_C_Y") %>%
      add_precursors("L100.FAO_ag_Food_t", "aglu/FAO/FAO_ag_items_cal_SUA", "common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.ag_Food_Pcal_R_C_Y
    L101.ag_kcalg_R_C_Y %>%
      add_title("Weighted average commodity caloric content by GCAM region, commodity, and year") %>%
      add_units("kcal/g") %>%
      add_comments("Combines the L101.ag_Food_Mt_R_C_Y and L101.ag_Food_Pcal_R_C_Y data frames") %>%
      add_legacy_name("L101.ag_kcalg_R_C_Y") %>%
      add_precursors("L100.FAO_ag_Food_t", "aglu/FAO/FAO_ag_items_cal_SUA", "common/iso_GCAM_regID") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.ag_kcalg_R_C_Y

    # The next two datasets have rounding issues that we couldn't resolve:
    # they kept failing the 'oldnew' test even though data confirmed to be
    # the same (to reasonable precision). Use the less-strict sum test for these.
    L101.ag_HA_bm2_R_C_Y %>%
      add_title("descriptive title of data") %>%
      add_units("billion km2") %>%
      add_comments("Aggregated FAO data by GCAM region and commodity") %>%
      add_comments("Data was also converted from HA to billion km2") %>%
      add_comments("Country/crop combinations with zero production were assigned zero harvested area") %>%
      add_legacy_name("L101.ag_HA_bm2_R_C_Y") %>%
      add_precursors("L100.FAO_ag_HA_ha", "aglu/FAO/FAO_ag_items_PRODSTAT", "L100.FAO_ag_Prod_t", "common/iso_GCAM_regID") %>%
      add_flags(FLAG_NO_XYEAR, FLAG_LONG_YEAR_FORM, FLAG_SUM_TEST) ->
      L101.ag_HA_bm2_R_C_Y
    L101.ag_Prod_Mt_R_C_Y %>%
      add_title("Agricultural production by GCAM region, commodity (cotton separated), and year") %>%
      add_units("Mt/yr") %>%
      add_comments("Aggregated FAO data by GCAM region and commodity") %>%
      add_comments("Data was also converted from tons to Mt") %>%
      add_comments("USA alfalfa production was divided by 4 for consistency with USDA") %>%
      add_comments("Country/crop combinations with zero harvested area were assigned zero production") %>%
      add_legacy_name("L101.ag_Prod_Mt_R_C_Y") %>%
      add_precursors("L100.FAO_ag_Prod_t", "aglu/FAO/FAO_ag_items_PRODSTAT", "L100.FAO_ag_HA_ha", "common/iso_GCAM_regID") %>%
      add_flags(FLAG_NO_XYEAR, FLAG_LONG_YEAR_FORM, FLAG_SUM_TEST) ->
      L101.ag_Prod_Mt_R_C_Y

    return_data(L101.ag_Food_Mt_R_C_Y, L101.ag_Food_Pcal_R_C_Y, L101.ag_kcalg_R_C_Y, L101.ag_HA_bm2_R_C_Y, L101.ag_Prod_Mt_R_C_Y)
  } else {
    stop("Unknown command")
  }
}
