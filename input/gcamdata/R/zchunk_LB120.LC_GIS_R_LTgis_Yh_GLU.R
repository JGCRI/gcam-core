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
#' @importFrom stats quantile
#' @author BBL April 2017
module_aglu_LB120.LC_GIS_R_LTgis_Yh_GLU <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/LDS/LDS_land_types",
             FILE = "aglu/SAGE_LT",
             FILE = "aglu/Various_CarbonData_LTsage",
             "L100.Land_type_area_ha",
             "L100.Ref_veg_carbon_Mg_per_ha",
             FILE = "aglu/LDS/L123.LC_bm2_R_MgdFor_Yh_GLU_beforeadjust"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L120.LC_bm2_R_LT_Yh_GLU",
             "L120.LC_bm2_R_UrbanLand_Yh_GLU",
             "L120.LC_bm2_R_Tundra_Yh_GLU",
             "L120.LC_bm2_R_RckIceDsrt_Yh_GLU",
             "L120.LC_bm2_ctry_LTsage_GLU",
             "L120.LC_bm2_ctry_LTpast_GLU",
             "L120.LC_prot_land_frac_GLU",
             "L120.LC_soil_veg_carbon_GLU"))
  } else if(command == driver.MAKE) {

    iso <- GCAM_region_ID <- Land_Type <- year <- GLU <- Area_bm2 <- LT_HYDE <-
      land_code <- LT_SAGE <- variable <- value <- Forest <- MgdFor <- Grassland <-
      Shrubland <- Pasture <- nonForScaler <- ForScaler <- `mature age` <- Status <- prot_status <- prot_frac <-
      non_prot_frac <- c_type <- Category <- `soil_c (0-100 cms)` <- `veg_c (above ground biomass)` <- `veg_c (below ground biomass)` <-
      soil_c <- vegc_ag <- vegc_bg <- land_area <- veg_c <- Tot_land <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs

    get_data(all_data, "common/iso_GCAM_regID") %>%
      select(iso, GCAM_region_ID) ->
      iso_GCAM_regID
    LDS_land_types <- get_data(all_data, "aglu/LDS/LDS_land_types")
    SAGE_LT <- get_data(all_data, "aglu/SAGE_LT")
    L123.LC_bm2_R_MgdFor_Yh_GLU_beforeadjust <- get_data(all_data, "aglu/LDS/L123.LC_bm2_R_MgdFor_Yh_GLU_beforeadjust")
    L100.Land_type_area_ha <- get_data(all_data, "L100.Land_type_area_ha")
    L100.Ref_veg_carbon_Mg_per_ha <- get_data(all_data, "L100.Ref_veg_carbon_Mg_per_ha")
    Various_CarbonData_LTsage <- get_data(all_data,"aglu/Various_CarbonData_LTsage") %>%
      filter(variable %in% c("mature age","soil_c","veg_c")) %>%
      select(LT_SAGE,variable,value) %>%
      distinct() %>%
      spread(variable,value) %>%
      select(LT_SAGE,`mature age`,soil_c_houghton=soil_c,veg_c_houghton=veg_c)

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

    ##calculate soil and veg carbon

    L100.Ref_veg_carbon_Mg_per_ha %>%
      select(iso, GLU, land_code, c_type, !!(as.name(aglu.CARBON_STATE))) %>%
      left_join_error_no_match(distinct(iso_GCAM_regID, iso, .keep_all = TRUE), by = "iso") %>%
      left_join(LDS_land_types %>% rename(land_code = Category), by = c("land_code")) %>%
      left_join(SAGE_LT, by = "LT_SAGE") %>% # includes NAs
      ## Drop all rows with missing values (inland bodies of water)
      na.omit() %>%
      spread(c_type, !!(as.name(aglu.CARBON_STATE))) %>%
      rename(soil_c = `soil_c (0-30 cms)`, vegc_ag = `veg_c (above ground biomass)`, vegc_bg = `veg_c (below ground biomass)`) %>%
      select(iso,GCAM_region_ID, GLU, Land_Type, soil_c, vegc_ag, vegc_bg, land_code)  %>%
      distinct() -> L120.LC_soil_veg_carbon_GLU_agg

    L100.Land_type_area_ha %>%
      ## Add data for GCAM region ID and GLU
      left_join_error_no_match(distinct(iso_GCAM_regID, iso, .keep_all = TRUE), by = "iso") %>%
      ## Add vectors for land type (SAGE, HYDE, and WDPA)
      left_join_error_no_match(LDS_land_types, by = c("land_code" = "Category")) %>%
      filter(LT_HYDE== "Unmanaged") %>%
      left_join(SAGE_LT, by = "LT_SAGE") %>%
      ## Drop all rows with missing values (inland bodies of water)
      na.omit() %>%
      # moirai only outputs carbon values from unmanaged land. Therefore, we remove pastures, urbanland and cropland from the below. We continue to calculate the carbon values for these land types using the Houghton structure.
      left_join_error_no_match(Various_CarbonData_LTsage %>%  filter(!LT_SAGE %in% c("Pasture","UrbanLand","Cropland")) %>% mutate(LT_SAGE = gsub(" ","",LT_SAGE)), by= c("LT_SAGE")) %>%
      mutate(`mature age` = if_else(is.na(`mature age`),1,`mature age`)) %>%
      complete(nesting(GCAM_region_ID, Land_Type, GLU,iso,land_code), year, fill = list(value = 0)) %>%
      complete(nesting(GCAM_region_ID, Land_Type, GLU,iso,land_code), year = unique(c(year, aglu.LAND_COVER_YEARS))) %>%
      filter(year == MODEL_CARBON_YEAR) %>%
      select(-year) %>%
      select(iso, GCAM_region_ID,GLU, Land_Type, value, land_code, `mature age`,soil_c_houghton, veg_c_houghton) %>%
      rename(land_area= value) %>%
      distinct() %>%
      mutate(`mature age` = if_else(is.na(`mature age`),aglu.DEFAULT_MATURITY_AGE_ALL_LAND,`mature age`))->Land_for_carbon

    Land_for_carbon %>%
      left_join(L120.LC_soil_veg_carbon_GLU_agg, by=c("iso", "GCAM_region_ID", "Land_Type", "GLU", "land_code")) %>%
      mutate(soil_c = if_else(is.na(soil_c),soil_c_houghton,if_else(soil_c==0,soil_c_houghton,soil_c)),
             vegc_ag = if_else(is.na(vegc_ag),veg_c_houghton,if_else(vegc_ag ==0,veg_c_houghton,vegc_ag)),
             vegc_bg = if_else(is.na(vegc_bg),0,vegc_bg),
             soil_c = if_else(is.na(soil_c),0,soil_c),
             vegc_ag = if_else(is.na(vegc_ag),0,vegc_ag)) %>%
      group_by(GCAM_region_ID, Land_Type, GLU) %>%
      #Note that soil and vegetation carbon units are in Mgc/ha. These are therefore converted to kg/m2 using CONV_THA_KGM2.
      #We compute a weighted average using land area as a weight.
      mutate( soil_c = (sum(land_area * soil_c)/sum(land_area))*CONV_THA_KGM2,
              veg_c = (sum(land_area * (vegc_ag+ vegc_bg))/sum(land_area))*CONV_THA_KGM2,
              `mature age` = sum(`mature age` * land_area )/sum(land_area)) %>%
      ungroup() %>%
      mutate(soil_c = if_else(is.na(soil_c),0,soil_c),
             veg_c = if_else(is.na(veg_c),0,veg_c),
             `mature age` = if_else(is.na(`mature age`),aglu.DEFAULT_MATURITY_AGE_ALL_LAND,`mature age`)) %>%
      select(GCAM_region_ID, Land_Type, GLU,soil_c,veg_c,`mature age`) %>%
      distinct() %>%
      #Add adjustment for Tundra. Our Tundra values are unreliable. Use Houghton for those,
      mutate(`mature age` = if_else(Land_Type == "Tundra", aglu.DEFAULT_TUNDRA_AGE, `mature age`))->L120.LC_soil_veg_carbon_GLU_all_cat


    #Compute Cropland carbon
    L100.Land_type_area_ha %>%
      ## Add data for GCAM region ID and GLU
      left_join_error_no_match(distinct(iso_GCAM_regID, iso, .keep_all = TRUE), by = "iso") %>%
      ## Add vectors for land type (SAGE, HYDE, and WDPA)
      left_join_error_no_match(LDS_land_types, by = c("land_code" = "Category")) %>%
      filter(LT_HYDE== "Cropland") %>%
      left_join(SAGE_LT, by = "LT_SAGE") %>%
      ## Drop all rows with missing values (inland bodies of water)
      na.omit() %>%
      # moirai only outputs carbon values from unmanaged land. Therefore, we remove pastures, urbanland and cropland from the below. We continue to calculate the carbon values for these land types using the Houghton structure.
      left_join_error_no_match(Various_CarbonData_LTsage %>%  filter(!LT_SAGE %in% c("Pasture","UrbanLand","Unmanaged")) %>% mutate(LT_SAGE = gsub(" ","",LT_SAGE)), by= c("LT_SAGE")) %>%
      mutate(`mature age` = if_else(is.na(`mature age`),1,`mature age`)) %>%
      complete(nesting(GCAM_region_ID, Land_Type, GLU,iso,land_code), year, fill = list(value = 0)) %>%
      complete(nesting(GCAM_region_ID, Land_Type, GLU,iso,land_code), year = unique(c(year, aglu.LAND_COVER_YEARS))) %>%
      filter(year == MODEL_CARBON_YEAR) %>%
      select(-year) %>%
      select(iso, GCAM_region_ID,GLU, Land_Type, value, land_code, `mature age`) %>%
      rename(land_area= value) %>%
      distinct() %>%
      mutate(`mature age` = if_else(is.na(`mature age`),aglu.DEFAULT_MATURITY_AGE_ALL_LAND,`mature age`))->Land_for_Crop_carbon

    L120.LC_soil_veg_carbon_GLU_all_cat %>%
      group_by(GCAM_region_ID,Land_Type,GLU) %>%
      mutate(soil_c= mean(soil_c),
             veg_c= mean(veg_c)) %>%
      ungroup() %>%
      select(GCAM_region_ID,Land_Type,GLU,soil_c,veg_c) %>%
      distinct()->L120.LC_soil_veg_carbon_mean_LT_GLU_reg



    Land_for_Crop_carbon %>%
      left_join_keep_first_only(L120.LC_soil_veg_carbon_mean_LT_GLU_reg, by=c("GLU", "GCAM_region_ID", "Land_Type")) %>%
      mutate(soil_c = if_else(is.na(soil_c),aglu.DEFAULT_SOIL_CARBON_CROPLAND,soil_c),
             veg_c = if_else(is.na(veg_c),aglu.DEFAULT_VEG_CARBON_CROPLAND,veg_c)) %>%
      mutate(Land_Type = "Cropland") %>%
      group_by(GCAM_region_ID, Land_Type, GLU) %>%
      #Note that soil and vegetation carbon units are in Mgc/ha. These are therefore converted to kg/m2 using CONV_THA_KGM2.
      #We compute a weighted average using land area as a weight.
      mutate( soil_c = (sum(land_area * soil_c)/sum(land_area))*0.7,
              veg_c = aglu.DEFAULT_VEG_CARBON_CROPLAND,
              `mature age` = 1) %>%
      ungroup() %>%
      mutate(soil_c = if_else(is.na(soil_c),aglu.DEFAULT_SOIL_CARBON_CROPLAND,soil_c),
             veg_c = if_else(is.na(veg_c),aglu.DEFAULT_VEG_CARBON_CROPLAND,veg_c),
             `mature age` = if_else(is.na(`mature age`),1,`mature age`)) %>%
      select(GCAM_region_ID, Land_Type, GLU,soil_c,veg_c,`mature age`) %>%
      distinct() ->L120.LC_soil_veg_carbon_GLU_crop






    # Pasture carbon is the same as grassland carbon values. But since the grassland values are subject to uncertainty, we make sure the values are below the mean of
    # all Grassland values for soil and vegetation.
    L120.LC_soil_veg_carbon_GLU_all_cat %>%
      select(-soil_c,-veg_c,-`mature age`,-Land_Type) %>%
      distinct() %>%
      left_join(L120.LC_soil_veg_carbon_GLU_all_cat %>% filter(Land_Type == aglu.GRASSLAND_NODE_NAMES), by =c("GCAM_region_ID","GLU")) %>%
      #Reducing soil carbon on pastures by a factor. This is because these pastures have been grazed in the past, so will not have same carbon as undisturbed grasslands.
      mutate(Land_Type = aglu.PASTURE_NODE_NAMES,
             soil_c = if_else(is.na(soil_c), aglu.DEFAULT_SOIL_CARBON_PASTURE*aglu.CSOIL_MULT_UNMGDPAST_MGDPAST,if_else(soil_c==0,aglu.DEFAULT_SOIL_CARBON_PASTURE*aglu.CSOIL_MULT_UNMGDPAST_MGDPAST,
                                                                                      soil_c*aglu.CSOIL_MULT_UNMGDPAST_MGDPAST)),
             veg_c = if_else(is.na(veg_c), aglu.DEFAULT_VEG_CARBON_PASTURE,if_else(veg_c==0,aglu.DEFAULT_VEG_CARBON_PASTURE,
                                                                                   veg_c)),
             `mature age` = if_else(is.na(`mature age`),aglu.DEFAULT_MATURITY_AGE_PASTURE,if_else(
               `mature age` ==1 , aglu.DEFAULT_MATURITY_AGE_PASTURE , `mature age`)))->L120.LC_soil_veg_carbon_GLU_pasture




    # Note that we set the default maturity age for Urban Land to 1 based on Houghton values.

    L120.LC_soil_veg_carbon_GLU_all_cat %>%
      select(-soil_c,-veg_c,-`mature age`,-Land_Type) %>%
      distinct() %>%
      mutate(Land_Type = paste0("UrbanLand"),soil_c = aglu.DEFAULT_SOIL_CARBON_URBANLAND, veg_c = aglu.DEFAULT_VEG_CARBON_URBANLAND, `mature age`= 1)->L120.LC_soil_veg_carbon_GLU_urban

    L120.LC_soil_veg_carbon_GLU <- bind_rows(L120.LC_soil_veg_carbon_GLU_all_cat,
                                             L120.LC_soil_veg_carbon_GLU_pasture,
                                             L120.LC_soil_veg_carbon_GLU_crop,
                                             L120.LC_soil_veg_carbon_GLU_urban)




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

    L120.LC_soil_veg_carbon_GLU %>%
      add_title("Spatially distinct soil and vegetation carbon by GLU") %>%
      add_units("kg/m2") %>%
      add_comments("Land types from SAGE, HYDE, WDPA merged and reconciled; missing zeroes backfilled; interpolated to AGLU land cover years. Soil carbon is at a depth of 0-30 cms and vegetation carbon is a combination of above and below ground biomass.") %>%
      add_legacy_name("L120.LC_soil_veg_carbon_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT", "L100.Land_type_area_ha","L100.Ref_veg_carbon_Mg_per_ha","aglu/Various_CarbonData_LTsage")->L120.LC_soil_veg_carbon_GLU

    return_data(L120.LC_bm2_R_LT_Yh_GLU, L120.LC_bm2_R_UrbanLand_Yh_GLU, L120.LC_bm2_R_Tundra_Yh_GLU, L120.LC_bm2_R_RckIceDsrt_Yh_GLU, L120.LC_bm2_ctry_LTsage_GLU, L120.LC_bm2_ctry_LTpast_GLU, L120.LC_prot_land_frac_GLU, L120.LC_soil_veg_carbon_GLU)
  } else {
    stop("Unknown command")
  }
}
