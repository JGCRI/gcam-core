# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LB120.LC_GIS_R_LTgis_Yh_GLU
#'
#' Land cover by GCAM region / aggregate land type / historical year / GLU.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L120.LC_bm2_R_LT_Yh_GLU}, \code{L120.LC_bm2_R_UrbanLand_Yh_GLU}, \code{L120.LC_bm2_R_Tundra_Yh_GLU}, \code{L120.LC_bm2_R_RckIceDsrt_Yh_GLU}, \code{L120.LC_bm2_ctry_LTsage_GLU}, \code{L120.LC_bm2_ctry_LTpast_GLU}. The corresponding file in the
#' original data system was \code{LB120.LC_GIS_R_LTgis_Yh_GLU.R} (aglu level1).
#' @details Aggregate the \code{L100.Land_type_area_ha} dataset, interpolate land use historical
#' years, and split into various sub-categories. Missing values are set to zero because the GLU files don't include
#' zero values (i.e. they only report nonzero land use combinations).
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange distinct filter group_by left_join mutate select summarise
#' @importFrom tidyr complete nesting spread
#' @author BBL April 2017
module_aglu_LB120.LC_GIS_R_LTgis_Yh_GLU <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/LDS/LDS_land_types",
             FILE = "aglu/SAGE_LT",
             "L100.Land_type_area_ha",
             FILE = "aglu/LDS/L123.LC_bm2_R_MgdFor_Yh_GLU_beforeadjust"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L120.LC_bm2_R_LT_Yh_GLU",
             "L120.LC_bm2_R_UrbanLand_Yh_GLU",
             "L120.LC_bm2_R_Tundra_Yh_GLU",
             "L120.LC_bm2_R_RckIceDsrt_Yh_GLU",
             "L120.LC_bm2_ctry_LTsage_GLU",
             "L120.LC_bm2_ctry_LTpast_GLU",
             "L120.LC_prot_land_frac_GLU"))
  } else if(command == driver.MAKE) {

    iso <- GCAM_region_ID <- Land_Type <- year <- GLU <- Area_bm2 <- LT_HYDE <-
      land_code <- LT_SAGE <- variable <- value <- Forest <- MgdFor <- Grassland <-
      Shrubland <- Pasture <- nonForScaler <- ForScaler <- `mature age` <- Status <- prot_status <- prot_frac <-
      non_prot_frac <- c_type <- Category <- `soil_c (0-100 cms)` <- `veg_c (above ground biomass)` <- `veg_c (below ground biomass)` <-
      soil_c <- vegc_ag <- vegc_bg <- land_area <- veg_c <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs

    get_data(all_data, "common/iso_GCAM_regID") %>%
      select(iso, GCAM_region_ID) ->
      iso_GCAM_regID
    LDS_land_types <- get_data(all_data, "aglu/LDS/LDS_land_types")
    SAGE_LT <- get_data(all_data, "aglu/SAGE_LT")
    L123.LC_bm2_R_MgdFor_Yh_GLU_beforeadjust <- get_data(all_data, "aglu/LDS/L123.LC_bm2_R_MgdFor_Yh_GLU_beforeadjust")
    L100.Land_type_area_ha <- get_data(all_data, "L100.Land_type_area_ha")

    # Perform computations

    land.type <-
      L100.Land_type_area_ha %>%
      ## Add data for GCAM region ID and GLU
      left_join_error_no_match(distinct(iso_GCAM_regID, iso, .keep_all = TRUE), by = "iso") %>%
      ## Add vectors for land type (SAGE, HYDE, and WDPA)
      left_join_error_no_match(LDS_land_types, by = c("land_code" = "Category")) %>%
      left_join(SAGE_LT, by = "LT_SAGE") %>%  # includes NAs
      rename(LT_SAGE_5 = Land_Type) %>%
      ## Drop all rows with missing values (inland bodies of water)
      na.omit

    ##calculate protection_shares

    land.type %>%
      mutate(prot_status = if_else( Status %in% aglu.NONPROTECT_LAND_STATUS, "Non-protected" ,"Protected")) %>%
      filter(LT_HYDE %in% c("Unmanaged","Pasture")) %>%
      left_join(SAGE_LT, by = "LT_SAGE") %>%  # includes NAs
      ## Drop all rows with missing values (inland bodies of water)
      na.omit() %>%
      # Note that Pasture is a land use type in moirai as opposed to a land cover type whereas in GCAM, it is treated as a separate land type.
      # Therefore, we set the land type to Pasture based on the land use type so that we can map the same to the appropriate land  types in GCAM.
      mutate(Land_Type= if_else(LT_HYDE=="Pasture","Pasture",Land_Type)) %>%
      group_by(GCAM_region_ID, year, GLU, Land_Type) %>%
      mutate (Tot_land = sum(value)) %>%
      ungroup() %>%
      filter(prot_status ==  "Protected" ) %>%
      group_by(GCAM_region_ID, year, GLU, Land_Type) %>%
      mutate(value= sum(value)) %>%
      ungroup() %>%
      select(GCAM_region_ID, year, GLU, Tot_land, value, Land_Type) %>%
      distinct() %>%
      mutate(prot_frac = value/Tot_land, non_prot_frac = 1 -(value/Tot_land)) %>%
      select(GCAM_region_ID, year, GLU,prot_frac, non_prot_frac,Land_Type) -> L120.LC_prot_land_frac_GLU

    if(aglu.PROTECTION_DATA_SOURCE_DEFAULT == TRUE){

      L120.LC_prot_land_frac_GLU %>%
          mutate(prot_frac = aglu.PROTECT_DEFAULT,
                 non_prot_frac = 1-aglu.PROTECT_DEFAULT) -> L120.LC_prot_land_frac_GLU
    }



    ## Reset WDPA classification to "Non-protected" where HYDE classification
    ## is cropland, pasture, or urban land
    hyde <- land.type$LT_HYDE
    ltype <- land.type$LT_SAGE_5

    #land.type$LT_WDPA <- replace(hyde, hyde != "Unmanaged", "Non-protected")

    land.type$Land_Type <-
      ltype %>%
      replace(hyde=='Cropland', 'Cropland') %>%
      replace(hyde=='Pasture', 'Pasture') %>%
      replace(hyde=='UrbanLand', 'UrbanLand')

    land.type$Area_bm2 <- land.type$value * CONV_HA_BM2
    L100.Land_type_area_ha <- land.type # Rename to the convention used in the
    # rest of the module

    # LAND COVER FOR LAND ALLOCATION
    # Aggregate into GCAM regions and land types
    # Part 1: Land cover by GCAM land category in all model history/base years
    # Collapse land cover into GCAM regions and aggregate land types
    L100.Land_type_area_ha %>%
      group_by(GCAM_region_ID, Land_Type, year, GLU) %>%
      summarise(Area_bm2 = sum(Area_bm2)) %>%
      ungroup %>%
      # Missing values should be set to 0 before interpolation, so that in-between years are interpolated correctly
      # We do his because Alan Di Vittorio (see sources above) isn't writing out all possible combinations of
      # country, GLU, year (of which there are 30), and land use category (of which there are also about 30).
      # If something isn't written out by the LDS, that is because it is a zero; this step back-fills the zeroes.
      complete(nesting(GCAM_region_ID, Land_Type, GLU), year, fill = list(Area_bm2 = 0)) %>%
      # Expand to all combinations with land cover years
      complete(nesting(GCAM_region_ID, Land_Type, GLU), year = unique(c(year, aglu.LAND_COVER_YEARS))) %>%
      group_by(GCAM_region_ID, Land_Type, GLU) %>%
      # Interpolate
      mutate(Area_bm2 = approx_fun(year, Area_bm2)) %>%
      ungroup %>%
      filter(year %in% aglu.LAND_COVER_YEARS) %>%
      arrange(GCAM_region_ID, Land_Type, GLU, year) %>%
      rename(value = Area_bm2) %>%
      mutate(year = as.integer(year)) ->
      L120.LC_bm2_R_LT_Yh_GLU

    # scale forest to avoid negative unmanaged forest area which caused issue for yield in Pakistan and African regions
    # L123.LC_bm2_R_MgdFor_Yh_GLU_beforeadjust, pulled from L123.LC_bm2_R_MgdFor_Yh_GLU before managed forest scaling, was used here.
    L120.LC_bm2_R_LT_Yh_GLU %>%
      left_join(L120.LC_bm2_R_LT_Yh_GLU %>%
                  spread(Land_Type, value, fill = 0) %>%
                  left_join(L123.LC_bm2_R_MgdFor_Yh_GLU_beforeadjust %>% select(-Land_Type),
			by = c("GCAM_region_ID", "GLU", "year")) %>%
                  mutate(nonForScaler =
                           if_else((Forest - MgdFor) < 0 & Forest > 0,
                                   1 + (Forest - MgdFor)/(Grassland + Shrubland + Pasture), 1),
                         ForScaler = if_else((Forest - MgdFor) < 0 & Forest > 0,  MgdFor/Forest ,1)) %>%
                  select(GCAM_region_ID, GLU, year, nonForScaler, ForScaler),
                by = c("GCAM_region_ID", "GLU", "year") ) %>%
      mutate(value = if_else(Land_Type %in% c("Grassland", "Shrubland" , "Pasture"),
                             value * nonForScaler,
                             if_else(Land_Type == "Forest", value * ForScaler, value) )) %>%
      select(-nonForScaler, -ForScaler) ->
      L120.LC_bm2_R_LT_Yh_GLU

    # Subset the land types that are not further modified
    L120.LC_bm2_R_UrbanLand_Yh_GLU <- filter(L120.LC_bm2_R_LT_Yh_GLU, Land_Type == "UrbanLand")
    L120.LC_bm2_R_Tundra_Yh_GLU <- filter(L120.LC_bm2_R_LT_Yh_GLU, Land_Type == "Tundra")
    L120.LC_bm2_R_RckIceDsrt_Yh_GLU <- filter(L120.LC_bm2_R_LT_Yh_GLU, Land_Type == "RockIceDesert")

    # LAND COVER FOR CARBON CONTENT CALCULATION
    # Compile data for land carbon content calculation on unmanaged lands
    # Note: not just using the final year, as some land use types may have gone to zero over the historical period.
    # Instead, use the mean of the available years within our "historical" years

    # The HYDE data are provided in increments of 10 years, so any GCAM model time period
    # or carbon cycle year that ends in a 5 (e.g., 1975) is computed as an average of
    # surrounding time periods. For most of the years that we want, we aren't doing any real
    # averaging or interpolation.
    L100.Land_type_area_ha %>%
      filter(LT_HYDE == "Unmanaged") %>%
      group_by(iso, GCAM_region_ID, GLU, land_code, LT_SAGE, Land_Type) %>%
      summarise(Area_bm2 = mean(Area_bm2)) %>%
      ungroup ->
      L120.LC_bm2_ctry_LTsage_GLU

    # Compile data for land carbon content calculation on pasture lands
    L100.Land_type_area_ha %>%
      filter(LT_HYDE == "Pasture") %>%
      group_by(iso, GCAM_region_ID, GLU, land_code, LT_SAGE, Land_Type) %>%
      summarise(Area_bm2 = mean(Area_bm2)) %>%
      ungroup ->
      L120.LC_bm2_ctry_LTpast_GLU


    # Produce outputs
    L120.LC_bm2_R_LT_Yh_GLU %>%
      add_title("Land cover by GCAM region / aggregate land type / historical year / GLU") %>%
      add_units("bm2") %>%
      add_comments("Land types from SAGE, HYDE, WDPA merged and reconciled; missing zeroes backfilled; interpolated to AGLU land cover years") %>%
      add_legacy_name("L120.LC_bm2_R_LT_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT", "L100.Land_type_area_ha",
                     "aglu/LDS/L123.LC_bm2_R_MgdFor_Yh_GLU_beforeadjust") ->
      L120.LC_bm2_R_LT_Yh_GLU

    L120.LC_bm2_R_UrbanLand_Yh_GLU %>%
      add_title("Urban land cover by GCAM region / historical year / GLU") %>%
      add_units("bm2") %>%
      add_comments("Land types from SAGE, HYDE, WDPA merged and reconciled; missing zeroes backfilled; interpolated to AGLU land cover years") %>%
      add_legacy_name("L120.LC_bm2_R_UrbanLand_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT", "L100.Land_type_area_ha") ->
      L120.LC_bm2_R_UrbanLand_Yh_GLU

    L120.LC_bm2_R_Tundra_Yh_GLU %>%
      add_title("Tundra land cover by GCAM region / historical year / GLU") %>%
      add_units("bm2") %>%
      add_comments("Land types from SAGE, HYDE, WDPA merged and reconciled; missing zeroes backfilled; interpolated to AGLU land cover years") %>%
      add_legacy_name("L120.LC_bm2_R_Tundra_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT", "L100.Land_type_area_ha") ->
      L120.LC_bm2_R_Tundra_Yh_GLU

    L120.LC_bm2_R_RckIceDsrt_Yh_GLU %>%
      add_title("Rock/ice/desert land cover by GCAM region / historical year / GLU") %>%
      add_units("bm2") %>%
      add_comments("Land types from SAGE, HYDE, WDPA merged and reconciled; missing zeroes backfilled; interpolated to AGLU land cover years") %>%
      add_legacy_name("L120.LC_bm2_R_RckIceDsrt_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT", "L100.Land_type_area_ha") ->
      L120.LC_bm2_R_RckIceDsrt_Yh_GLU

    L120.LC_bm2_ctry_LTsage_GLU %>%
      add_title("Unmanaged land cover by country / SAGE15 land type / GLU") %>%
      add_units("bm2") %>%
      add_comments("Land types from SAGE, HYDE, WDPA merged and reconciled; missing zeroes backfilled; interpolated to AGLU land cover years") %>%
      add_comments("Mean computed for HYDE 'Unmanaged' over available historical years") %>%
      add_legacy_name("L120.LC_bm2_ctry_LTsage_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT", "L100.Land_type_area_ha") ->
      L120.LC_bm2_ctry_LTsage_GLU

    L120.LC_bm2_ctry_LTpast_GLU %>%
      add_title("Pasture land cover by country / SAGE15 land type / GLU") %>%
      add_units("bm2") %>%
      add_comments("Land types from SAGE, HYDE, WDPA merged and reconciled; missing zeroes backfilled; interpolated to AGLU land cover years") %>%
      add_comments("Mean computed for HYDE 'Pasture' over available historical years") %>%
      add_legacy_name("L120.LC_bm2_ctry_LTpast_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT", "L100.Land_type_area_ha") ->
      L120.LC_bm2_ctry_LTpast_GLU

    L120.LC_prot_land_frac_GLU %>%
      add_title("protected and unprotected fractions by year,GLU, land type.") %>%
      add_units("fraction") %>%
      add_comments("Land types from SAGE, HYDE, WDPA merged and reconciled; missing zeroes backfilled; interpolated to AGLU land cover years") %>%
      add_legacy_name("L120.LC_prot_land_frac_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT", "L100.Land_type_area_ha") ->
      L120.LC_prot_land_frac_GLU


    return_data(L120.LC_bm2_R_LT_Yh_GLU, L120.LC_bm2_R_UrbanLand_Yh_GLU, L120.LC_bm2_R_Tundra_Yh_GLU, L120.LC_bm2_R_RckIceDsrt_Yh_GLU, L120.LC_bm2_ctry_LTsage_GLU, L120.LC_bm2_ctry_LTpast_GLU, L120.LC_prot_land_frac_GLU)
  } else {
    stop("Unknown command")
  }
}
