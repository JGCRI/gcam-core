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
    # First, compute the share of each region's land in each GLU
    # Then, merge in total regional emissions from EDGAR
    L124.LC_bm2_R_Grass_Yh_GLU_adj %>%
      group_by(GCAM_region_ID, year) %>%
      mutate(land_share = value / sum(value)) %>%
      select(-value) %>%
      left_join(filter(EDGAR_history, sector == "grassland"), by = c("GCAM_region_ID", "year")) %>%
      mutate(value = value * land_share) %>%
      na.omit() %>%
      select(-sector, -land_share) ->
      L124.nonco2_tg_R_grass_Y_GLU


    #
    # printlog( "Compute grassland emissions by GCAM region and GLU" )
    # L124.nonco2_tg_R_grass_Y_GLU.melt <- L124.LCshare_grass_R_Y_GLU.melt[ c( R_LT_Y_GLU, "land_share" ) ]
    # L124.nonco2_tg_R_grass_Y_GLU.melt <- repeat_and_add_vector( L124.nonco2_tg_R_grass_Y_GLU.melt, "Non.CO2", unique( L124.EDGAR_grass.melt$Non.CO2 ) )
    # L124.nonco2_tg_R_grass_Y_GLU.melt$total_emiss <- L124.EDGAR_grass.melt$value[
    #   match( vecpaste( L124.nonco2_tg_R_grass_Y_GLU.melt[ c( R_Y, "Non.CO2" ) ] ),
    #          vecpaste( L124.EDGAR_grass.melt[ c( R_Y, "Non.CO2" ) ] ) ) ]
    # L124.nonco2_tg_R_grass_Y_GLU.melt$emissions <- with( L124.nonco2_tg_R_grass_Y_GLU.melt, total_emiss * land_share )
    # L124.nonco2_tg_R_grass_Y_GLU.melt <- na.omit( L124.nonco2_tg_R_grass_Y_GLU.melt)
    #
    # #Reshape
    # L124.nonco2_tg_R_grass_Y_GLU <- dcast( L124.nonco2_tg_R_grass_Y_GLU.melt, GCAM_region_ID + Land_Type + Non.CO2 + GLU ~ year, value.var = c( "emissions" ) )


    # Part 2: Forest fires and deforestation
    # First, compute the share of each region's land in each GLU

    # L124.forest_bm2_R_Y <- aggregate( L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj[ X_historical_years ],
    #                                   by = L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj[ R ], sum )
    # L124.forest_bm2_R_Y <- melt( L124.forest_bm2_R_Y, id.vars = R, variable.name = Y )
    # L124.LCshare_forest_R_Y_GLU.melt <- melt( L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj,
    #                                           id.vars = R_LT_GLU, measure.vars = X_historical_years, variable.name = Y )
    # L124.LCshare_forest_R_Y_GLU.melt$total_land <- L124.forest_bm2_R_Y$value[
    #   match( vecpaste( L124.LCshare_forest_R_Y_GLU.melt[ R_Y ] ),
    #          vecpaste( L124.forest_bm2_R_Y[ R_Y ] ) ) ]
    # L124.LCshare_forest_R_Y_GLU.melt$land_share <- with( L124.LCshare_forest_R_Y_GLU.melt, value / total_land )
    #
    # printlog( "Compute share of forest fire vs deforestation from GFED")
    # GFED_ForestFire_SO2$Non.CO2 <- "SO2"
    # GFED_Deforest_SO2$Non.CO2 <- "SO2"
    # GFED_ForestFire_CO$Non.CO2 <- "CO"
    # GFED_Deforest_CO$Non.CO2 <- "CO"
    # GFED_ForestFire_NOx$Non.CO2 <- "NOx"
    # GFED_Deforest_NOx$Non.CO2 <- "NOx"
    #
    # # NMVOC is split into lots of inventories, so using CO for now. TODO: Get NMVOC data
    # GFED_ForestFire_NMVOC <- GFED_ForestFire_CO
    # GFED_ForestFire_NMVOC$Non.CO2 <- "NMVOC"
    # GFED_Deforest_NMVOC <- GFED_Deforest_CO
    # GFED_Deforest_NMVOC$Non.CO2 <- "NMVOC"
    #
    # L124.GFED_ForestFire <- rbind( GFED_ForestFire_SO2, GFED_ForestFire_CO, GFED_ForestFire_NOx, GFED_ForestFire_NMVOC )
    # L124.GFED_Deforest <- rbind( GFED_Deforest_SO2, GFED_Deforest_CO, GFED_Deforest_NOx, GFED_Deforest_NMVOC )
    #
    # L124.GFED_ForestFire.melt <- melt( L124.GFED_ForestFire,
    #                                    id.vars=c( "Country", "Non.CO2" ), variable.name = Y, value.name = "ForestFire" )
    # L124.GFED_Deforest.melt <- melt( L124.GFED_Deforest,
    #                                  id.vars=c( "Country", "Non.CO2" ), variable.name = Y, value.name = "Deforest" )
    #
    # L124.GFED_ALL.melt <- L124.GFED_ForestFire.melt
    # L124.GFED_ALL.melt$Deforest <- L124.GFED_Deforest.melt$Deforest[
    #   match( vecpaste( L124.GFED_ALL.melt[ c( "Country", "Non.CO2", Y )]),
    #          vecpaste( L124.GFED_Deforest.melt[ c( "Country", "Non.CO2", Y )] ))]
    #
    # #Aggregate by region, gas, and year
    # L124.GFED_ALL.melt$iso <- EDGAR_nation$iso[ match( L124.GFED_ALL.melt$Country, EDGAR_nation$ISO_A3 )]
    # L124.GFED_ALL.melt$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L124.GFED_ALL.melt$iso, iso_GCAM_regID$iso )]
    # L124.GFED_ALL.melt <- aggregate( L124.GFED_ALL.melt[ c( "ForestFire", "Deforest" )],
    #                                  by = L124.GFED_ALL.melt[ c( "GCAM_region_ID", "Non.CO2", Y )], sum )
    #
    # L124.GFED_ALL.melt$PctForestFire <- L124.GFED_ALL.melt$ForestFire / ( L124.GFED_ALL.melt$ForestFire + L124.GFED_ALL.melt$Deforest )
    # L124.GFED_ALL.melt$PctForestFire[ is.na( L124.GFED_ALL.melt$PctForestFire ) ] <- 1
    # L124.GFED_ALL.melt$PctDeforest <- 1 - L124.GFED_ALL.melt$PctForestFire
    #
    #
    # printlog( "Compute forest emissions by GCAM region and GLU" )
    # L124.nonco2_tg_R_forest_Y_GLU.melt <- L124.LCshare_forest_R_Y_GLU.melt[ c( R_LT_Y_GLU, "land_share" )]
    # L124.nonco2_tg_R_forest_Y_GLU.melt <- repeat_and_add_vector( L124.nonco2_tg_R_forest_Y_GLU.melt, "Non.CO2", unique( L124.EDGAR_forest.melt$Non.CO2 ) )
    # L124.nonco2_tg_R_forest_Y_GLU.melt$total_emiss <- L124.EDGAR_forest.melt$value[
    #   match( vecpaste( L124.nonco2_tg_R_forest_Y_GLU.melt[ c( R_Y, "Non.CO2" ) ] ),
    #          vecpaste( L124.EDGAR_forest.melt[ c( R_Y, "Non.CO2" ) ] ) ) ]
    # L124.nonco2_tg_R_forest_Y_GLU.melt$emissions <- with( L124.nonco2_tg_R_forest_Y_GLU.melt, total_emiss * land_share )
    # L124.nonco2_tg_R_forest_Y_GLU.melt <- na.omit( L124.nonco2_tg_R_forest_Y_GLU.melt )
    #
    # #Split into ForestFire and Deforest
    # L124.nonco2_tg_R_forest_Y_GLU.melt$PctForestFire <- L124.GFED_ALL.melt$PctForestFire[
    #   match( vecpaste( L124.nonco2_tg_R_forest_Y_GLU.melt[ c( R_Y, "Non.CO2" ) ] ),
    #          vecpaste( L124.GFED_ALL.melt[ c( R_Y, "Non.CO2" ) ] ) ) ]
    #
    # # Because the GFED data only have a subset of emissions species available, just use these to infer the other species (rather than default to 100% FF)
    # L124.nonco2_tg_R_forest_Y_GLU.melt$PctForestFire[ is.na( L124.nonco2_tg_R_forest_Y_GLU.melt$PctForestFire ) ] <- L124.GFED_ALL.melt$PctForestFire[
    #   match( vecpaste( L124.nonco2_tg_R_forest_Y_GLU.melt[ is.na( L124.nonco2_tg_R_forest_Y_GLU.melt$PctForestFire ), c( R_Y ) ] ),
    #          vecpaste( L124.GFED_ALL.melt[ c( R_Y ) ] ) ) ]
    # L124.nonco2_tg_R_forest_Y_GLU.melt$PctDeforest <- 1 - L124.nonco2_tg_R_forest_Y_GLU.melt$PctForestFire
    #
    # # If data is missing, assume all emissions are assigned to forest fires. These are easier to calibrate in GCAM.
    # L124.nonco2_tg_R_forest_Y_GLU.melt$PctForestFire[ is.na( L124.nonco2_tg_R_forest_Y_GLU.melt$PctForestFire ) ] <- 1.0
    # L124.nonco2_tg_R_forest_Y_GLU.melt$PctDeforest[ is.na( L124.nonco2_tg_R_forest_Y_GLU.melt$PctDeforest ) ] <- 0.0
    #
    # L124.nonco2_tg_R_forest_Y_GLU.melt$ForestFire <- with( L124.nonco2_tg_R_forest_Y_GLU.melt, emissions * PctForestFire )
    # L124.nonco2_tg_R_forest_Y_GLU.melt$Deforest <- with( L124.nonco2_tg_R_forest_Y_GLU.melt, emissions * PctDeforest )
    #
    # L124.nonco2_tg_R_forest_Y_GLU.melt <- L124.nonco2_tg_R_forest_Y_GLU.melt[ c( R_LT_Y_GLU, "Non.CO2", "ForestFire", "Deforest" ) ]
    # L124.nonco2_tg_R_forest_Y_GLU.melt <- melt( L124.nonco2_tg_R_forest_Y_GLU.melt,
    #                                             measure.vars = c( "ForestFire", "Deforest" ), variable.name = "technology" )
    #
    # #Reshape
    # L124.nonco2_tg_R_forest_Y_GLU <- dcast( L124.nonco2_tg_R_forest_Y_GLU.melt,
    #                                         GCAM_region_ID + Land_Type + Non.CO2 + GLU + technology ~ year, value.var = c( "value" ) )
    #
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
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L124.nonco2_tg_R_forest_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "emissions/EDGAR/EDGAR_sector", "temp-data-inject/L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj", "EDGAR_gases",
                     "emissions/GFED/GFED_ForestFire_SO2", "emissions/GFED/GFED_Deforest_SO2", "emissions/GFED/GFED_ForestFire_CO",
                     "emissions/GFED/GFED_Deforest_CO", "emissions/GFED/GFED_ForestFire_NOx", "emissions/GFED/GFED_Deforest_NOx") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST) ->
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
