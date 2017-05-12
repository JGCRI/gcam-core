#' module_emissions_L124.nonco2_unmgd_R_S_T_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L124.nonco2_tg_R_grass_Y_GLU}, \code{L124.nonco2_tg_R_forest_Y_GLU}, \code{L124.deforest_coefs}. The corresponding file in the
#' original data system was \code{L124.nonco2_unmgd_R_S_T_Y.R} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
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
      left_join(EDGAR_sector, by = "IPCC") %>%
      rename(sector = agg_sector) %>%
      mutate(iso = tolower( ISO_A3 ), ISO_A3 = NULL) %>%
      change_iso_code('rou', 'rom') %>%
      left_join(iso_GCAM_regID, by = "iso") %>%
      select(IPCC, Non.CO2, sector, iso, GCAM_region_ID, year, value) ->
      EDGAR_history

    if(OLD_DATA_SYSTEM_BEHAVIOR) {
      #   ... code that replicates old, incorrect behavior
      EDGAR_history %>%
        filter(Non.CO2 != "NH3" & year <= 2008 ) %>%
        spread(year, value) %>%
        na.omit() %>%
        gather(year, value, -GCAM_region_ID, -iso, -sector, -Non.CO2, -IPCC) %>%
        mutate(year = as.integer( year )) ->
        EDGAR_history

    } else {
      #   ... new code with a fix
    }

    EDGAR_history %>%
      filter(year %in% 2004:2008) %>%                             # Extrapolate using average of last five years --- 2010 is problematic here because of deforestation
      group_by(GCAM_region_ID, iso, sector, Non.CO2, IPCC) %>%
      summarize(value = mean(value)) %>%
      mutate(year = as.integer(2010)) %>%
      bind_rows(EDGAR_history) %>%
      group_by(GCAM_region_ID, sector, Non.CO2, year) %>%
      summarize(value = sum(value)) %>%
      na.omit() %>%
      mutate(value = value * CONV_GG_TG) ->
      EDGAR_history

    # Part 1: Grassland burning
    # Downscale regional grassland burning emissions to GLU based on the share of land in each GLU
    L124.LC_bm2_R_Grass_Yh_GLU_adj %>%
      group_by(GCAM_region_ID, year) %>%
      mutate(land_share = value / sum(value)) %>%
      select(-value) %>%
      left_join(filter(EDGAR_history, sector == "grassland"), by = c("GCAM_region_ID", "year")) %>%
      mutate(value = value * land_share) %>%
      na.omit() %>%
      select(-sector, -land_share) ->
      L124.nonco2_tg_R_grass_Y_GLU

    # Part 2: Forest fires and deforestation
    # Downscale regional forest burning emissions to GLU based on the share of land in each GLU
    # Use GFED to separate into forest fires and deforestation, which have different drivers in GCAM
    bind_rows(mutate(GFED_ForestFire_CO, Non.CO2 = "CO", type = "ForestFire"),
              mutate(GFED_Deforest_CO, Non.CO2 = "CO", type = "Deforest"),
              mutate(GFED_ForestFire_SO2, Non.CO2 = "SO2", type = "ForestFire"),
              mutate(GFED_Deforest_SO2, Non.CO2 = "SO2", type = "Deforest"),
              mutate(GFED_ForestFire_NOx, Non.CO2 = "NOx", type = "ForestFire"),
              mutate(GFED_Deforest_NOx, Non.CO2 = "NOx", type = "Deforest")) %>%
      # # NMVOC is split into lots of inventories, so using CO for now. TODO: Get NMVOC data
      bind_rows(mutate(GFED_ForestFire_CO, Non.CO2 = "NMVOC", type = "ForestFire"),
                mutate(GFED_Deforest_CO, Non.CO2 = "NMVOC", type = "Deforest")) %>%
      # Previous code did this implicitly in lines 162-166
      bind_rows(mutate(GFED_ForestFire_CO, Non.CO2 = "CH4", type = "ForestFire"),
                mutate(GFED_Deforest_CO, Non.CO2 = "CH4", type = "Deforest"),
                mutate(GFED_ForestFire_CO, Non.CO2 = "N2O", type = "ForestFire"),
                mutate(GFED_Deforest_CO, Non.CO2 = "N2O", type = "Deforest")) %>%
      gather(year, value, -Country, -Non.CO2, -type) %>%
      mutate(year = as.integer(year)) %>%
      spread(type, value) %>%
      mutate(iso = tolower( Country ), Country = NULL) %>%
      change_iso_code('rou', 'rom') %>%
      left_join(iso_GCAM_regID, by = "iso") %>%
      group_by(GCAM_region_ID, Non.CO2, year ) %>%
      summarize(ForestFire = sum(ForestFire), Deforest = sum(Deforest) ) %>%
      na.omit() %>%
      mutate(PctForestFire = ForestFire / (ForestFire + Deforest)) %>%
      mutate(PctForestFire = if_else( is.na(PctForestFire), 1, PctForestFire )) %>%
      mutate(PctDeforest = 1 - PctForestFire) %>%
      select(-ForestFire, -Deforest) ->
      FireShares_R_G_Y

    L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj %>%
      group_by(GCAM_region_ID, year) %>%
      mutate(land_share = value / sum(value)) %>%
      select(-value) %>%
      left_join(filter(EDGAR_history, sector == "forest"), by = c("GCAM_region_ID", "year")) %>%
      mutate(value = value * land_share) %>%
      na.omit() %>%
      select(-sector, -land_share) %>%
      left_join(FireShares_R_G_Y, by = c("GCAM_region_ID", "Non.CO2", "year")) %>%
      # If data is missing, assume all emissions are assigned to forest fires. These are easier to calibrate in GCAM.
      mutate(PctForestFire = if_else( is.na(PctForestFire), 1, PctForestFire)) %>%
      mutate(PctDeforest = if_else( is.na(PctDeforest), 0, PctDeforest)) %>%
      mutate(ForestFire = value * PctForestFire) %>%
      mutate(Deforest = value * PctDeforest) %>%
      select(-value, -PctForestFire, -PctDeforest) %>%
      gather(technology, value, -GCAM_region_ID, -GLU, -Land_Type, -Non.CO2, -year) ->
      L124.nonco2_tg_R_forest_Y_GLU

    # # Compute driver data to write out some average coefficients. Note that the driver can't be negative, and is annualized, so divide by timestep
    # L124.deforest_driver <- L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj[ c( R_LT_GLU, X_Deforest_coef_years ) ]
    # L124.deforest_driver$driver <- pmax(
    #   L124.deforest_driver[[X_Deforest_coef_years[1] ]] - L124.deforest_driver[[ X_Deforest_coef_years[2] ]],
    #   0 ) / ( Deforest_coef_years[2] - Deforest_coef_years[1] )
    #
    # #Repeat by species, and match in the emissions quantities
    # L124.deforest_coefs_R_GLU <- repeat_and_add_vector( L124.deforest_driver[ c( R_LT_GLU, "driver" ) ],
    #                                                     "Non.CO2",
    #                                                     unique( L124.nonco2_tg_R_forest_Y_GLU$Non.CO2 ) )
    # L124.deforest_coefs_R_GLU$technology <- "Deforest"
    # L124.deforest_coefs_R_GLU$emissions <- L124.nonco2_tg_R_forest_Y_GLU[[ X_Deforest_coef_years[2 ] ]][
    #   match( vecpaste( L124.deforest_coefs_R_GLU[ c( R_LT_GLU, "Non.CO2", "technology" ) ] ),
    #          vecpaste( L124.nonco2_tg_R_forest_Y_GLU[ c( R_LT_GLU, "Non.CO2", "technology" ) ] ) ) ]
    #
    # # where the driver is (net) zero, re-set the emissions to zero to remove them from the calculation
    # L124.deforest_coefs_R_GLU$emissions[ L124.deforest_coefs_R_GLU$driver == 0 ] <- 0
    # L124.deforest_coefs_R_GLU$emissions[ is.na( L124.deforest_coefs_R_GLU$emissions ) ] <- 0
    #
    # #Aggregate the totals to compute the default coefficients
    # L124.deforest_coefs <- aggregate( L124.deforest_coefs_R_GLU[ c( "driver", "emissions" ) ],
    #                                   by = L124.deforest_coefs_R_GLU[ c( LT, "technology", "Non.CO2" ) ],
    #                                   sum )
    # L124.deforest_coefs$emiss.coef <- with( L124.deforest_coefs, emissions / driver )
    #

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    L124.nonco2_tg_R_grass_Y_GLU %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L124.nonco2_tg_R_grass_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "emissions/EDGAR/EDGAR_sector", "temp-data-inject/L124.LC_bm2_R_Grass_Yh_GLU_adj", "EDGAR_gases") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L124.nonco2_tg_R_grass_Y_GLU
    L124.nonco2_tg_R_forest_Y_GLU %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L124.nonco2_tg_R_forest_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "emissions/EDGAR/EDGAR_sector", "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj", "EDGAR_gases",
                     "emissions/GFED/GFED_ForestFire_SO2", "emissions/GFED/GFED_Deforest_SO2", "emissions/GFED/GFED_ForestFire_CO",
                     "emissions/GFED/GFED_Deforest_CO", "emissions/GFED/GFED_ForestFire_NOx", "emissions/GFED/GFED_Deforest_NOx") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L124.nonco2_tg_R_forest_Y_GLU
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L124.deforest_coefs") %>%
      same_precursors_as("L124.nonco2_tg_R_forest_Y_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
      L124.deforest_coefs

    return_data(L124.nonco2_tg_R_grass_Y_GLU, L124.nonco2_tg_R_forest_Y_GLU, L124.deforest_coefs)
  } else {
    stop("Unknown command")
  }
}
