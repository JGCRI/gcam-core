# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L121.Carbon_LT
#'
#' Compute natural vegetation, managed land, and pasture carbon density, mature age, and yield.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L121.CarbonContent_kgm2_R_LT_GLU}, \code{L121.Yield_kgm2_R_Past_GLU}. The corresponding file in the
#' original data system was \code{LB121.Carbon_LT.R} (aglu level1).
#' @details Combine Houghton et al. (1999 and similar) carbon density, mature age, and pasture yield data with
#' SAGE data to produce tables giving, for each GCAM region and land use type, data on these variables.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select summarise
#' @importFrom tidyr spread
#' @importFrom stats weighted.mean
#' @author BBL April 2017
module_aglu_L121.Carbon_LT <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/SAGE_LT",
      FILE = "aglu/Various_CarbonData_LTsage",
      FILE = "aglu/Various_Tree_C_yield_ratios",
      FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
      FILE = "common/iso_GCAM_regID",
      "L100.LDS_ag_HA_ha",
      "L100.LDS_ag_prod_t",
      "L120.LC_bm2_R_LT_Yh_GLU",
      "L120.LC_bm2_ctry_LTsage_GLU",
      "L120.LC_bm2_ctry_LTpast_GLU",
      "L120.LC_soil_veg_carbon_GLU")

  MODULE_OUTPUTS <-
    c("L121.CarbonContent_kgm2_R_LT_GLU",
      "L121.Yield_kgm2_R_Past_GLU",
      "L121.CarbonContent_kgm2_R_TreeCrop_GLU")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    unit <- value <- Source <- variable <- pasture_yield <- GCAM_region_ID <-
        Land_Type <- GLU <- `mature age` <- Area_bm2 <- veg_c <- soil_c <-
        year <- NULL                    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Convert characteristics by land type to correct units (kgC/m2)
    Various_CarbonData_LTsage %>%
      mutate(value = if_else(unit == "tC/ha", value * CONV_THA_KGM2, value)) %>%
      select(-Source, -unit) %>%
      spread(variable, value) ->
      L121.Various_CarbonData_LTsage

    # Need to get rid of the spaces in the table for matching
    L121.Various_CarbonData_LTsage$LT_SAGE <- gsub(" ", "", L121.Various_CarbonData_LTsage$LT_SAGE)

    # Match carbon contents and mature age by land cover, by SAGE land type
    L120.LC_bm2_ctry_LTsage_GLU %>%
      left_join(L121.Various_CarbonData_LTsage, by = "LT_SAGE") %>%
      select(-pasture_yield) %>%
      # Aggregate by GCAM region and GCAM land use type, using area-weighted mean
      group_by(GCAM_region_ID, Land_Type, GLU) %>%
      summarise(`mature age` = weighted.mean(`mature age`, Area_bm2),
                veg_c = weighted.mean(veg_c, Area_bm2),
                soil_c = weighted.mean(soil_c, Area_bm2)) %>%
      ungroup ->
      L121.CarbonContent_kgm2_R_LTnatveg_GLU

    # Urban land and cropland
    L120.LC_bm2_R_LT_Yh_GLU %>%
      spread(year, value) %>%
      filter(Land_Type %in% c("Cropland", "UrbanLand")) %>%
      select(GCAM_region_ID, Land_Type, GLU) %>%
      left_join_error_no_match(select(L121.Various_CarbonData_LTsage, -pasture_yield),
                               by = c("Land_Type" = "LT_SAGE")) ->
      L121.CarbonContent_kgm2_R_LTmgd_GLU

    #If using moirai as carbon data source use the moirai carbon data to get pasture carbon.Set the pasture yield here equal to vegetative carbon content.
    #This will be divided by the cellulose content below to get the true pasture yield.

    # Xin: 11/24/2022 0.6 was used uniformly across regions as annual pasture carbon yield
    # Two issues:
    # 1. this value could be quite different across regions
    # 2. grazing intensity is not considered in anyways
    # as a result, we could underestimate managed pasture area by overestimating yield
    # This is now changed to regional values using veg_c from moirai
    if(aglu.CARBON_DATA_SOURCE == "moirai"){

      L120.LC_soil_veg_carbon_GLU %>%
        filter(Land_Type == "Pasture") %>%
        # Note that using veg_c / `mature age` underestimate the annual pasture yield
        # Since grazing pasture likely is in high yield periods.
        # However, this partly account for the grazing intensity. I.e., not all grass grown is eaten (see more below)
        mutate(pasture_yield = veg_c / `mature age`,
               # Using Q-40 value across regions as min
               pasture_yield_Low = quantile(pasture_yield, 0.40),
               pasture_yield = pmax(pasture_yield, pasture_yield_Low)) %>%
        # Taiwan has cropland area issue need to have a relatively large pasture yield here
        # Otherwise, unmanaged land/cropland area adjustments in LB124 gives error
        # So use the original value for Taiwan
        mutate(pasture_yield = if_else(!GCAM_region_ID %in% c(iso_GCAM_regID %>% filter(iso == "twn") %>%
                                                               pull(GCAM_region_ID)),
                                       pasture_yield, 0.6))->
        L121.CarbonContent_kgm2_R_LTpast_GLU


    }else{

    # Pasture carbon contents
    L120.LC_bm2_ctry_LTpast_GLU %>%
      left_join_error_no_match(L121.Various_CarbonData_LTsage, by = "LT_SAGE") %>%
      # pasture lands may involve clearing; it is unclear to what extent this takes place but we'll use the carbon
      # contents of savanna as a maximum possible value for the vegetative carbon content of pasture lands
      mutate(veg_c = pmin(veg_c, L121.Various_CarbonData_LTsage$veg_c[L121.Various_CarbonData_LTsage$LT_SAGE == "Savanna"])) %>%

      # Aggregate by GCAM region and GCAM land use type, using area-weighted mean
      group_by(GCAM_region_ID, Land_Type, GLU) %>%
      summarise(`mature age` = weighted.mean(`mature age`, Area_bm2),
                veg_c = weighted.mean(veg_c, Area_bm2),
                soil_c = weighted.mean(soil_c, Area_bm2),
                pasture_yield = weighted.mean(pasture_yield, Area_bm2)) %>%
      ungroup ->
      L121.CarbonContent_kgm2_R_LTpast_GLU}

    # Tree crop carbon contents: estimate from the yield
    L100.LDS_ag_prod_t %>%
      inner_join(select(Various_Tree_C_yield_ratios, GTAP_crop, C_per_output, max_veg_c_tha),
                 by = "GTAP_crop") %>%
      left_join_error_no_match(L100.LDS_ag_HA_ha,
                               by = c("iso", "GTAP_crop", "GLU"),
                               suffix = c(".prod", ".HA")) %>%
      mutate(Cdensity_tha = value.prod * C_per_output / value.HA,
             Cdensity_tha = if_else(Cdensity_tha > max_veg_c_tha, max_veg_c_tha, Cdensity_tha),
             Cstock_tC = value.HA * Cdensity_tha) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID),
                               by = "iso") %>%
      left_join_error_no_match(select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity, GCAM_subsector),
                               by = "GTAP_crop") %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%
      summarise(Cstock_tC = sum(Cstock_tC),
                HA_ha = sum(value.HA)) %>%
      ungroup() %>%
      mutate(Cdensity_kgm2 = Cstock_tC / HA_ha * CONV_THA_KGM2) %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, Cdensity_kgm2) ->
      L121.CarbonContent_kgm2_R_TreeCrop_GLU

    # Combine natural vegetation and managed land use tables
    bind_rows(L121.CarbonContent_kgm2_R_LTnatveg_GLU,
              L121.CarbonContent_kgm2_R_LTpast_GLU,
              L121.CarbonContent_kgm2_R_LTmgd_GLU) %>%
      select(GCAM_region_ID, Land_Type, GLU, `mature age`, soil_c, veg_c) %>%
      mutate(`mature age` = round(`mature age`)) %>%
      arrange(GCAM_region_ID, GLU, Land_Type) %>%
      add_title("Natural vegetation and managed land age and carbon density") %>%
      add_units("Years (mature age) and kgC/m2 (others)") %>%
      add_comments("From matching Houghton (1999) and SAGE data by GCAM region and land use type") %>%
      add_legacy_name("L121.CarbonContent_kgm2_R_LT_GLU") %>%
      add_precursors("aglu/SAGE_LT", "aglu/Various_CarbonData_LTsage",
                     "L120.LC_bm2_R_LT_Yh_GLU", "L120.LC_bm2_ctry_LTsage_GLU", "L120.LC_bm2_ctry_LTpast_GLU") ->
      L121.CarbonContent_kgm2_R_LT_GLU

    # Grazing intensity: the proportion of annual aboveground net primary productivity removed by grazing livestock
    # Grazing intensity, globally is around 13% in 2010.
    # See Wolf et al. (2021) https://www.mdpi.com/2072-4292/13/17/3430
    # To be conservative, we use a uniform 70% value here for now
    GrazingIntensity = 0.7
    # Because 1. the C yield method above may under estimate the yield
    # 2. we are representing relatively more managed high-quality pasture
    # 3. the world average pasture dry matter yield is ~ 3 DM t/ha
    # 4. the future productivity increase has not been considered yet
    # This value should be revisited later & differentiated by regions


    # Pasture yields are separate
    L121.CarbonContent_kgm2_R_LTpast_GLU %>%
      select(GCAM_region_ID, Land_Type, GLU, pasture_yield) %>%
      mutate(pasture_yield = pasture_yield * GrazingIntensity / aglu.CCONTENT_CELLULOSE) %>%
      add_title("Pasture land age, carbon density, and yield") %>%
      add_units("Years (mature age), kgC/m2 (carbon), and kg/m2 (pasture yield)") %>%
      add_comments("From matching Houghton (1999) and SAGE data by GCAM region and land use type") %>%
      add_legacy_name("L121.Yield_kgm2_R_Past_GLU") %>%
      add_precursors("aglu/SAGE_LT", "aglu/Various_CarbonData_LTsage",
                     "L120.LC_bm2_R_LT_Yh_GLU", "L120.LC_bm2_ctry_LTsage_GLU", "L120.LC_bm2_ctry_LTpast_GLU","L120.LC_soil_veg_carbon_GLU") ->
      L121.Yield_kgm2_R_Past_GLU

    L121.CarbonContent_kgm2_R_TreeCrop_GLU %>%
      add_title("Tree crop carbon contents by GCAM region, commodity/subsector, and GLU") %>%
      add_units("kgC/m2") %>%
      add_comments("Calculated from exogenous assumptions relating yield of each tree crop species to its standing stock of carbon") %>%
      add_precursors("aglu/Various_Tree_C_yield_ratios", "L100.LDS_ag_prod_t", "L100.LDS_ag_HA_ha",
                     "common/iso_GCAM_regID", "aglu/FAO/FAO_ag_items_PRODSTAT") ->
      L121.CarbonContent_kgm2_R_TreeCrop_GLU

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
