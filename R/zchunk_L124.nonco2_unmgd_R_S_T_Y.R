#' module_emissions_L124.nonco2_unmgd_R_S_T_Y
#'
#' Calculate non-CO2 emissions from unmanaged lands (savanna burning, forest fires, deforestation)
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L124.nonco2_tg_R_grass_Y_GLU}, \code{L124.nonco2_tg_R_forest_Y_GLU}, \code{L124.deforest_coefs}. The corresponding file in the
#' original data system was \code{L124.nonco2_unmgd_R_S_T_Y.R} (emissions level1).
#' @details Calculate non-CO2 emissions from unmanaged lands (savanna burning, forest fires, deforestation).
#' Downscale EDGAR regional emissions to GLU using shares of land area in each GLU within each region.
#' Divide forest-related emissions into deforestation and forest fires using GFED data
#' Compute global average deforestation emissions coefficients using deforestation from 2000 to 2005.
#' Note: File does not calculate emissions from BC/OC (separate chunk) or NH3 (seemingly omitted)
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author KVC May 2017
#' @export
module_emissions_L124.nonco2_unmgd_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EDGAR/EDGAR_sector",
             FILE = "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj",
             FILE = "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj",
             "EDGAR_gases",
             FILE = "emissions/GFED/GFED_ForestFire_SO2",
             FILE = "emissions/GFED/GFED_Deforest_SO2",
             FILE = "emissions/GFED/GFED_ForestFire_CO",
             FILE = "emissions/GFED/GFED_Deforest_CO",
             FILE = "emissions/GFED/GFED_ForestFire_NOx",
             FILE = "emissions/GFED/GFED_Deforest_NOx"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L124.nonco2_tg_R_grass_Y_GLU",
             "L124.nonco2_tg_R_forest_Y_GLU",
             "L124.deforest_coefs"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- value <- GCAM_region_ID <- Land_Type <- GLU <- agg_sector <-
      sector <- IPCC <- Non.CO2 <- iso <- tail <- land_share <- Country <-
      type <- ForestFire <- Deforest <- PctForestFire <- PctDeforest <-
      technology <- year1 <- year2 <- emissions <- NULL  # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR/EDGAR_sector")
    L124.LC_bm2_R_Grass_Yh_GLU_adj <- get_data(all_data, "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj")
    L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj <- get_data(all_data, "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj")
    EDGAR_gases <- get_data(all_data, "EDGAR_gases")
    GFED_ForestFire_SO2 <- get_data(all_data, "emissions/GFED/GFED_ForestFire_SO2")
    GFED_Deforest_SO2 <- get_data(all_data, "emissions/GFED/GFED_Deforest_SO2")
    GFED_ForestFire_CO <- get_data(all_data, "emissions/GFED/GFED_ForestFire_CO")
    GFED_Deforest_CO <- get_data(all_data, "emissions/GFED/GFED_Deforest_CO")
    GFED_ForestFire_NOx <- get_data(all_data, "emissions/GFED/GFED_ForestFire_NOx")
    GFED_Deforest_NOx <- get_data(all_data, "emissions/GFED/GFED_Deforest_NOx")

    # Reformat temporary data -- Note: this can be removed after the upstream files are done
    L124.LC_bm2_R_Grass_Yh_GLU_adj %>%
      gather(year, value, -GCAM_region_ID, -Land_Type, -GLU) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L124.LC_bm2_R_Grass_Yh_GLU_adj

    L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj %>%
      gather(year, value, -GCAM_region_ID, -Land_Type, -GLU) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj

    # Prepare EDGAR emissions for use
    # Map in region ID and sector name; extrapolate to 2010; aggregate to sector & region; convert from Gg to Tg
    EDGAR_gases %>%
      left_join(EDGAR_sector, by = "IPCC") %>%                                          # Add GCAM sector from the sector mapping
      rename(sector = agg_sector) %>%
      filter(sector %in% c("grassland", "forest")) %>%                                  # Filter for the two sectors we use in this file.
      standardize_iso(col = "ISO_A3") %>%
      change_iso_code('rou', 'rom') %>%                                                 # Switch Romania iso code to its pre-2002 value
      left_join(iso_GCAM_regID, by = "iso") %>%                                         # Map in GCAM regions
      select(IPCC, Non.CO2, sector, iso, GCAM_region_ID, year, value) ->
      EDGAR_history

    if(OLD_DATA_SYSTEM_BEHAVIOR) {
      # The old data system processed data in wide format. There was a na.omit() in the code that effectively removed
      # any region/sector combination where the time series was incomplete. We'd like to keep this information.
      # Additionally, the old data system never included NH3 in the processing. The emissions are small
      # but we should keep them.
      EDGAR_history %>%
        filter(Non.CO2 != "NH3" ) %>%                                                    # Remove NH3 for consistency with old data
        filter(year <= 2008) %>%                                                         # Old data didn't care if post-2008 data was missing so remove it here
        spread(year, value) %>%                                                          # Convert to wide format
        na.omit() %>%                                                                    # Remove any row with an NA (i.e., incomplete time series)
        gather(year, value, -GCAM_region_ID, -iso, -sector, -Non.CO2, -IPCC) %>%         # Convert back to long format
        mutate(year = as.integer( year )) ->                                             # Convert year back to integer form (not sure why this changes type)
        EDGAR_history

    } else {
      EDGAR_history %>%
        na.omit() ->
        EDGAR_history
    }

    # Prepare EDGAR emissions for use (continued)
    EDGAR_history %>%
      # Extrapolate using average of last five years --- 2010 is problematic here because of deforestation (i.e., we don't want to rely on 2005 as last year of data)
      filter(year %in% tail(emissions.EDGAR_YEARS,5)) %>%
      group_by(GCAM_region_ID, iso, sector, Non.CO2, IPCC) %>%
      summarize(value = mean(value)) %>%
      mutate(year = as.integer(max(BASE_YEARS))) %>%
      bind_rows(EDGAR_history) %>%                                                      # Bind extrapolated 2010 data to the rest of the EDGAR data
      group_by(GCAM_region_ID, sector, Non.CO2, year) %>%                               # Aggregate by region, sector, gas, and year
      summarize(value = sum(value)) %>%                                                 # This will sum over IPCC category (e.g., grassland = grassland fires + savanna burning)
      mutate(value = value * CONV_GG_TG) ->                                             # Convert to Tg
      EDGAR_history

    # Part 1: Grassland burning
    # Downscale regional grassland burning emissions to GLU based on the share of land in each GLU
    L124.LC_bm2_R_Grass_Yh_GLU_adj %>%
      group_by(GCAM_region_ID, year) %>%
      mutate(land_share = value / sum(value)) %>%                                                           # Compute the share of regional grassland in each GLU
      select(-value) %>%
      # There are regions (e.g., region #3) where we have grassland area, but no emissions. Use inner join to remove
      inner_join(filter(EDGAR_history, sector == "grassland"), by = c("GCAM_region_ID", "year")) %>%         # Map in EDGAR grassland emissions
      mutate(value = value * land_share) %>%                                                                # Compute emissions by GLU using EDGAR totals and land shares
      ungroup() %>%
      select(-sector, -land_share) ->
      L124.nonco2_tg_R_grass_Y_GLU

    # Part 2: Forest fires and deforestation
    # Calculate share of forest emissions from forest fires versus deforestation using GFED data.
    # Bind all GFED data together, aggregate by GCAM region/gas/year, calculate share of forest fire versus deforestation
    bind_rows(mutate(GFED_ForestFire_CO, Non.CO2 = "CO", type = "ForestFire"),
              mutate(GFED_Deforest_CO, Non.CO2 = "CO", type = "Deforest"),
              mutate(GFED_ForestFire_SO2, Non.CO2 = "SO2", type = "ForestFire"),
              mutate(GFED_Deforest_SO2, Non.CO2 = "SO2", type = "Deforest"),
              mutate(GFED_ForestFire_NOx, Non.CO2 = "NOx", type = "ForestFire"),
              mutate(GFED_Deforest_NOx, Non.CO2 = "NOx", type = "Deforest")) %>%
      # NMVOC is split into lots of inventories, so using CO for now.
      bind_rows(mutate(GFED_ForestFire_CO, Non.CO2 = "NMVOC", type = "ForestFire"),
                mutate(GFED_Deforest_CO, Non.CO2 = "NMVOC", type = "Deforest")) %>%
      # Use CO for other missing gases. Previous code did this implicitly in lines 162-166
      bind_rows(mutate(GFED_ForestFire_CO, Non.CO2 = "CH4", type = "ForestFire"),
                mutate(GFED_Deforest_CO, Non.CO2 = "CH4", type = "Deforest"),
                mutate(GFED_ForestFire_CO, Non.CO2 = "N2O", type = "ForestFire"),
                mutate(GFED_Deforest_CO, Non.CO2 = "N2O", type = "Deforest")) %>%
      gather(year, value, -Country, -Non.CO2, -type) %>%                                             # Convert from wide to long
      mutate(year = as.integer(year)) %>%
      spread(type, value) %>%                                                                        # Spread data so deforestation and forest fires are in columns
      standardize_iso(col = "Country") %>%
      change_iso_code('rou', 'rom') %>%                                                              # Convert Romania iso code to pre-2002 value
      left_join(iso_GCAM_regID, by = "iso") %>%
      # There are a set of iso codes in the GFED data that don't exist in the GCAM region mapping. Remove those now.
      na.omit() %>%
      group_by(GCAM_region_ID, Non.CO2, year ) %>%
      summarize(ForestFire = sum(ForestFire), Deforest = sum(Deforest) ) %>%                         # Aggregate emissions by region, gas, and year
      mutate(PctForestFire = ForestFire / (ForestFire + Deforest)) %>%                               # Compute share of emissions from forest fires
      # There are regions where GFED data is zero for both forest fires and deforestation, leading to NAs
      # Assume those missing values are places with 100% forest fires since these are easier to model in GCAM
      replace_na(list(PctForestFire = 1)) %>%
      mutate(PctDeforest = 1 - PctForestFire) %>%                                                    # Compute share of emissions from deforestation
      select(-ForestFire, -Deforest) ->
      FireShares_R_G_Y

    # Downscale regional forest burning emissions to GLU based on the share of land in each GLU
    # Use GFED to separate into forest fires and deforestation, which have different drivers in GCAM
    L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj %>%
      group_by(GCAM_region_ID, year) %>%
      mutate(land_share = value / sum(value)) %>%                                                      # Compute share of regional forest area in each GLU
      na.omit() %>%
      select(-value) %>%
      # There are places with land area but no emissions and vice versa. Use an inner_join to only get places with both.
      # Note: this means that some regions get zero emissions coefficients in the historic period (future deforestation emissions coefs are defined below)
      inner_join(filter(EDGAR_history, sector == "forest"), by = c("GCAM_region_ID", "year")) %>%       # Map in EDGAR emissions information
      mutate(value = value * land_share) %>%                                                           # Compute forest emissions from EDGAR totals and land shares
      select(-sector, -land_share) %>%
      left_join(FireShares_R_G_Y, by = c("GCAM_region_ID", "Non.CO2", "year")) %>%                     # Map in GFED fire shares
      # Assume missing values mean 100% forest fires since these are easier to model in GCAM
      replace_na(list(PctForestFire = 1)) %>%
      replace_na(list(PctDeforest = 0)) %>%
      mutate(ForestFire = value * PctForestFire) %>%                                                   # Compute forest fire emissions
      mutate(Deforest = value * PctDeforest) %>%                                                       # Compute deforestation emissions
      ungroup() %>%
      select(-value, -PctForestFire, -PctDeforest) %>%
      gather(technology, value, -GCAM_region_ID, -GLU, -Land_Type, -Non.CO2, -year) ->
      L124.nonco2_tg_R_forest_Y_GLU

    # Compute global average deforestation emissions coefficients
    # These coefficients are used for future model time periods.
    # Compute total change in forest area from 2000 to 2005, total global emissions, and average annualized coefficients (emissions / change in land area / number of years)
    gas_list <- tibble(Non.CO2 = unique(L124.nonco2_tg_R_forest_Y_GLU$Non.CO2))                       # Set up list of gases (we'll use this to add columns later)
    L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj %>%
      filter(year %in% emissions.DEFOREST_COEF_YEARS) %>%                                             # Get years that we'll use for deforestation calculation (as of 5/14/17 this was 2000 & 2005)
      mutate(year = if_else(year == min(emissions.DEFOREST_COEF_YEARS), "year1", "year2")) %>%        # Rename years so we can use them as column headings (this also makes this robust to changes in years later)
      spread(year, value) %>%                                                                         # Spread so years are separate columns
      mutate(driver = (year1 - year2) / (emissions.DEFOREST_COEF_YEARS[2] - emissions.DEFOREST_COEF_YEARS[1])) %>%    # Compute average annual deforestation rates (change in forest area / number of years)
      mutate(driver = if_else(driver < 0, 0, driver)) %>%                                             # Deforestation emissions only happen if forest area decreases
      repeat_add_columns(gas_list) %>%                                                                # Add in rows for all required emissions
      left_join(filter(L124.nonco2_tg_R_forest_Y_GLU,                                                 # Map in EDGAR deforestation emissions for the final deforestation year (as of 5/14/17 this was 2005)
                       year == emissions.DEFOREST_COEF_YEARS[2],
                       technology == "Deforest"), by = c( "GCAM_region_ID", "Land_Type", "GLU", "Non.CO2")) %>%
      mutate(technology = if_else(is.na(technology), "Deforest", technology)) %>%                     # Make sure the technology name is "Deforest" (not sure why I have to do this but it is required)
      replace_na(list(value = 0)) %>%                                                                 # Note: "value" are the emissions calculated above
      mutate(value = if_else(driver == 0, 0, value)) %>%                                              # Zero out emissions in places where there wasn't any deforestation
      group_by(Land_Type, technology, Non.CO2) %>%
      summarize(driver = sum(driver), emissions = sum(value)) %>%                                     # Calculate global total emissions and deforestation
      mutate(emiss.coef = emissions / driver) %>%                                                     # Calculate average annual deforestation emissions coefficients
      ungroup() %>%
      na.omit() ->
      L124.deforest_coefs

    # Produce outputs
    L124.nonco2_tg_R_grass_Y_GLU %>%
      add_title("Grassland fire emissions by GCAM region, gas, and historical year") %>%
      add_units("Tg/yr") %>%
      add_comments("EDGAR grassland emissions are downscaled to GLU using shares of grassland area.") %>%
      add_legacy_name("L124.nonco2_tg_R_grass_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "emissions/EDGAR/EDGAR_sector", "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj", "EDGAR_gases") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_SUM_TEST) ->
      L124.nonco2_tg_R_grass_Y_GLU
    L124.nonco2_tg_R_forest_Y_GLU %>%
      add_title("Forest fire and deforestation emissions by GCAM region, gas, and historical year") %>%
      add_units("Tg/yr") %>%
      add_comments("EDGAR forest emissions are downscaled to GLU using shares of forest area.") %>%
      add_comments("These emissions are then separated into forest fire and deforestation using GFED data.") %>%
      add_legacy_name("L124.nonco2_tg_R_forest_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "emissions/EDGAR/EDGAR_sector", "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj", "EDGAR_gases",
                     "emissions/GFED/GFED_ForestFire_SO2", "emissions/GFED/GFED_Deforest_SO2", "emissions/GFED/GFED_ForestFire_CO",
                     "emissions/GFED/GFED_Deforest_CO", "emissions/GFED/GFED_ForestFire_NOx", "emissions/GFED/GFED_Deforest_NOx") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L124.nonco2_tg_R_forest_Y_GLU
    L124.deforest_coefs %>%
      add_title("Default deforestation coefficients by Non-CO2 species") %>%
      add_units("Tg/yr") %>%
      add_comments("Global average deforestation coefficients are calculated from global emissions and global deforestation.") %>%
      add_legacy_name("L124.deforest_coefs") %>%
      same_precursors_as("L124.nonco2_tg_R_forest_Y_GLU") ->
      L124.deforest_coefs

    return_data(L124.nonco2_tg_R_grass_Y_GLU, L124.nonco2_tg_R_forest_Y_GLU, L124.deforest_coefs)
  } else {
    stop("Unknown command")
  }
}
