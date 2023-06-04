# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L2252.land_input_5_irr_mgmt
#'
#' Produce the inputs for the lowest level of the land nest, including disaggregated crop technologies:
#' L2252.LN5_Logit, L2252.LN5_HistMgdAllocation_crop, L2252.LN5_MgdAllocation_crop,
#' L2252.LN5_HistMgdAllocation_bio, L2252.LN5_MgdAllocation_bio, L2252.LN5_MgdCarbon_crop,
#' L2252.LN5_MgdCarbon_bio, L2252.LN5_LeafGhostShare, L2252.LN5_NodeGhostShare
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{curr_table$data}, \code{L2252.LN5_Logit}, \code{L2252.LN5_HistMgdAllocation_crop}, \code{L2252.LN5_MgdAllocation_crop}, \code{L2252.LN5_HistMgdAllocation_bio}, \code{L2252.LN5_MgdAllocation_bio}, \code{L2252.LN5_MgdCarbon_crop}, \code{L2252.LN5_MgdCarbon_bio}, \code{L2252.LN5_LeafGhostShare}, \code{L2252.LN5_NodeGhostShare}. The corresponding file in the
#' original data system was \code{L2252.land_input_5_irr_mgmt.R} (aglu level2).
#' @details
#' \itemize{
#' \item{"L2252.LN5_Logit: Logit exponent of the fifth land nest by region. AgLU regions are given externally defined constant logit information."}
#' \item{"L2252.LN5_HistMgdAllocation_crop: Historical land cover for managed crop land (LT_GLU) in the fifth nest, from L181 land cover data."}
#' \item{"L2252.LN5_MgdAllocation_crop: Land cover in the model base periods for managed crop land (LT_GLU) in the fifth nest, from L181 land cover data."}
#' \item{"L2252.LN5_HistMgdAllocation_bio: Historical land cover for managed bio land (LT_GLU) in the fifth nest by region,
#' generated directly from nest 4 files."}
#' \item{"L2252.LN5_MgdAllocation_bio: Land cover in the model base periods for managed bio land (LT_GLU) in the fifth nest by region,
#' generated directly from nest 4 files."}
#' \item{"L2252.LN5_MgdCarbon_crop: Carbon content info for managed crop land (LT_GLU) in the fifth nest including soil and vegetative carbon,
#' generated directly from nest 4 files"}
#' \item{"L2252.LN5_MgdCarbon_bio: Carbon content info for biofuel managed land (LT_GLU) in the fifth nest including soil and vegetative carbon,
#' from L181 yield multiplier data and L2241.LN4_MgdCarbon_bio."}
#' \item{"L2252.LN5_LeafGhostShare: Ghost share of the landleaf in the fifth nest by region. Ghost shares are inferred
#' from average land shares allocated to hi-input versus lo-input in L181.LandShare, across all crops"}
#' \item{"L2252.LN5_NodeGhostShare: Ghost share of the nest 4 nodes (irrigated versus rainfed)."}
#' }
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter full_join if_else group_by left_join mutate select semi_join summarize
#' @importFrom tidyr replace_na separate
#' @importFrom tibble tibble
#' @author ACS September 2017
module_aglu_L2252.land_input_5_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "aglu/GCAMLandLeaf_CdensityLT",
             FILE = "aglu/A_Fodderbio_chars",
             FILE = "aglu/A_LandLeaf3",
             "L171.ag_irrEcYield_kgm2_R_C_Y_GLU",
             "L171.ag_rfdEcYield_kgm2_R_C_Y_GLU",
             "L181.LandShare_R_bio_GLU_irr",
             "L181.LC_bm2_R_C_Yh_GLU_irr_level",
             "L2242.LN4_Logit",
             "L111.ag_resbio_R_C",
             "L121.CarbonContent_kgm2_R_LT_GLU",
             "L120.LC_soil_veg_carbon_GLU",
             "L121.CarbonContent_kgm2_R_TreeCrop_GLU",
             "L2012.AgYield_bio_ref",
             "L2012.AgProduction_ag_irr_mgmt"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2252.LN5_Logit",
             "L2252.LN5_HistMgdAllocation_crop",
             "L2252.LN5_MgdAllocation_crop",
             "L2252.LN5_HistMgdAllocation_bio",
             "L2252.LN5_MgdAllocation_bio",
             "L2252.LN5_MgdCarbon_crop",
             "L2252.LN5_MgdCarbon_bio",
             "L2252.LN5_LeafGhostShare",
             "L2252.LN5_NodeGhostShare"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    GCAMLandLeaf_CdensityLT <- get_data(all_data, "aglu/GCAMLandLeaf_CdensityLT")
    A_Fodderbio_chars <- get_data(all_data, "aglu/A_Fodderbio_chars")
    A_LandLeaf3 <- get_data(all_data, "aglu/A_LandLeaf3")
    L181.LandShare_R_bio_GLU_irr <- get_data(all_data, "L181.LandShare_R_bio_GLU_irr")
    L181.LC_bm2_R_C_Yh_GLU_irr_level <- get_data(all_data, "L181.LC_bm2_R_C_Yh_GLU_irr_level")
    L2242.LN4_Logit <- get_data(all_data, "L2242.LN4_Logit", strip_attributes = TRUE)
    L111.ag_resbio_R_C <- get_data(all_data, "L111.ag_resbio_R_C")
    L121.CarbonContent_kgm2_R_LT_GLU <- get_data(all_data, "L121.CarbonContent_kgm2_R_LT_GLU")
    L121.CarbonContent_kgm2_R_TreeCrop_GLU <- get_data(all_data, "L121.CarbonContent_kgm2_R_TreeCrop_GLU")
    L171.ag_irrEcYield_kgm2_R_C_Y_GLU <- get_data(all_data, "L171.ag_irrEcYield_kgm2_R_C_Y_GLU", strip_attributes = TRUE)
    L171.ag_rfdEcYield_kgm2_R_C_Y_GLU <- get_data(all_data, "L171.ag_rfdEcYield_kgm2_R_C_Y_GLU", strip_attributes = TRUE)
    L2012.AgYield_bio_ref <- get_data(all_data, "L2012.AgYield_bio_ref")
    L2012.AgProduction_ag_irr_mgmt <- get_data(all_data, "L2012.AgProduction_ag_irr_mgmt")

    # silence package check notes
    GCAM_commodity <- GCAM_region_ID <- region <- value <- year <- GLU <- GLU_name <- GLU_code <-
      LandLeaf <- Land_Type <- LandNode <- LandNode1 <- LandNode2 <- LandNode3 <- UnmanagedLandLeaf <-
      logit.year.fillout <- logit.exponent <- logit.type <- soilTimeScale <- `mature age` <- mature.age <-
      soil_c <- veg_c <- LC_bm2 <- LV_milUSD75 <- LV_USD75_bm2 <- LV_USD75_m2 <- HarvCropLand_bm2 <-
      unManagedLandValue <- LandAllocatorRoot <- hist.veg.carbon.density <- hist.soil.carbon.density <-
      veg.carbon.density <- soil.carbon.density <- allocation <- Land_Type.y <- mature.age.year.fillout <-
      min.veg.carbon.density <- min.soil.carbon.density <- LandNode4 <- LandNode5 <- level <-
      calOutputValue <- AgProductionTechnology <- Irr_Rfd <- variable <- yieldmult <- tmp <- crop1 <-
      crop2 <- landshare <- lev <- . <- ghost.unnormalized.share <- HarvestIndex.x <- HarvestIndex.y <-
      Root_Shoot.x <- Root_Shoot.y <- WaterContent.x <- WaterContent.y <- yield <- HarvestIndex <-
      WaterContent <- Root_Shoot <- total_land <- gcam5_hist.veg.carbon.density <- new_hist.veg.carbon.density <-
      gcam5_veg.carbon.density <- new_veg.carbon.density <- dif_hist.veg <- dif_veg<- id <- GCAM_subsector <- NULL

    # 1. Process inputs

    if(aglu.CARBON_DATA_SOURCE=="moirai"){

      L121.CarbonContent_kgm2_R_LT_GLU <- get_data(all_data, "L120.LC_soil_veg_carbon_GLU")
    }else{
      L121.CarbonContent_kgm2_R_LT_GLU <- get_data(all_data, "L121.CarbonContent_kgm2_R_LT_GLU")

    }

    # Replace GLU names and Add region names
    L181.LandShare_R_bio_GLU_irr %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) ->
      L181.LandShare_R_bio_GLU_irr

    L181.LC_bm2_R_C_Yh_GLU_irr_level %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) ->
      L181.LC_bm2_R_C_Yh_GLU_irr_level

    L121.CarbonContent_kgm2_R_TreeCrop_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) %>%
      select(region, GCAM_commodity, GCAM_subsector, GLU, Tree_Cdensity_kgm2 = Cdensity_kgm2) ->
      L121.CarbonContent_kgm2_R_TreeCrop_GLU

    L121.CarbonContent_kgm2_R_LT_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) %>%
      rename(mature.age = `mature age`) ->
      L121.CarbonContent_kgm2_R_LT_GLU

    L111.ag_resbio_R_C %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L111.ag_resbio_R_C

    L171.ag_irrEcYield_kgm2_R_C_Y_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) %>%
      mutate(Irr_Rfd = "IRR") ->
      L171.ag_irrEcYield_kgm2_R_C_Y_GLU

    L171.ag_rfdEcYield_kgm2_R_C_Y_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) %>%
      mutate(Irr_Rfd = "RFD") %>%
      bind_rows(L171.ag_irrEcYield_kgm2_R_C_Y_GLU) %>%
      filter(year == max(MODEL_BASE_YEARS)) ->
      L171.ag_EcYield_kgm2_R_C_Y_GLU

    # convert_LN4_to_LN5
    # A function to carry LN4 information down to LN5
    convert_LN4_to_LN5 <- function(data, names) {
      data %>%
        repeat_add_columns(tibble(level = c("lo", "hi"))) %>%
        mutate(LandNode5 = LandLeaf,
               LandLeaf = paste(LandNode5, level, sep = aglu.MGMT_DELIMITER)) ->
        data_new
      data_new <- data_new[names]

      return(data_new)
    } # end convert_LN4_to_LN5

    # remove_zero_production_land_leafs
    # A function to remove land leafs for each region-year whose production, read in from
    # a provided table, is 0
    remove_zero_production_land_leafs <- function(land, prod) {

      # remove 0 production region-years and add an id combining
      # region, AgProductionTechnology, year info"
      prod %>%
        filter(calOutputValue > 0) %>%
        mutate(id = paste0(region, AgProductionTechnology, year)) %>%
        select(id) %>%
        distinct() ->
        prod1

      # give land a corresponding id and filter to the land ids that
      # occur in prod1
      land %>%
        mutate(id = paste0(region, LandLeaf, year)) %>%
        semi_join(prod1, by = "id") %>%
        select(-id)
    } # end remove_zero_production_land_leafs

    # 2. Build tables
    #
    # The methods in this code file will be to start with existing (landnode4) tables, and add another level of detail for management levels
    # (nitrogen application), hi and lo.

    # L2252.LN5_Logit: Logit exponent between lo and hi managed techs for each crop-irrigation type combo
    # ie competition between Corn_IRR_hi and Corn_Irr_lo.
    L2242.LN4_Logit %>%
      repeat_add_columns(tibble(Irr_Rfd = c("IRR", "RFD"))) %>%
      mutate(LandNode5 = paste(LandNode4, Irr_Rfd, sep = aglu.IRR_DELIMITER),
             logit.exponent = aglu.MGMT_LOGIT_EXP,
             logit.type = aglu.MGMT_LOGIT_TYPE) %>%
      select(LEVEL2_DATA_NAMES[["LN5_Logit"]], LOGIT_TYPE_COLNAME) ->
      L2252.LN5_Logit

    # Create an intermediary table of land allocation for each landleaf (= crop-glu-irr-mgmt)
    # in each region-year. This is used for both HistMgdAllocation and MgdAllocation for crops.
    L181.LC_bm2_R_C_Yh_GLU_irr_level %>%
      mutate(Irr_Rfd = toupper(Irr_Rfd),
             LandLeaf = paste(paste(paste(GCAM_subsector, GLU, sep = aglu.CROP_GLU_DELIMITER),
                                    Irr_Rfd, sep = aglu.IRR_DELIMITER),
                              level, sep = aglu.MGMT_DELIMITER),
             value = round(value, aglu.DIGITS_LAND_USE)) %>%
      rename(allocation = value) %>%
      select(region, year, LandLeaf, allocation) ->
      L2252.LC_bm2_R_C_Yh_GLU_irr_mgmt

    # Use L2252.LN5_Logit to get names of LandNodes, copy to all years
    # Add land leafs using `convert_LN4_to_LN5`, then join land allocation information
    L2252.LN5_Logit %>%
      rename(LandLeaf = LandNode5) %>%
      select(-logit.year.fillout, -logit.exponent, -logit.type) %>%
      repeat_add_columns(tibble(year = aglu.LAND_COVER_YEARS)) %>%
      mutate(allocation = -1) %>%
      convert_LN4_to_LN5(names = LEVEL2_DATA_NAMES[["LN5_HistMgdAllocation"]]) %>%
      select(-allocation) %>%
      # Biomass leafs don't have historical land area, but we want to keep them, so using full_join
      full_join(L2252.LC_bm2_R_C_Yh_GLU_irr_mgmt, by = c("region", "year", "LandLeaf")) %>%
      replace_na(list(allocation = 0)) ->
      ALL_LAND_ALLOCATION

    # L2252.LN5_HistMgdAllocation_crop: historical cropland allocation
    # in the fifth land nest ie for each crop-irr-mgmt combo in each region-glu-year.
    ALL_LAND_ALLOCATION %>%
      filter(!grepl("biomassGrass", LandLeaf) & !grepl("biomassTree", LandLeaf)) %>%
      filter(year %in% aglu.LAND_HISTORY_YEARS) ->
      L2252.LN5_HistMgdAllocation_crop

    # L2252.LN5_MgdAllocation_crop: cropland allocation
    # in the fifth land nest ie for each crop-irr-mgmt combo in each region-glu-year.
    ALL_LAND_ALLOCATION %>%
      filter(!grepl("biomassGrass", LandLeaf), !grepl("biomassTree", LandLeaf)) %>%
      filter(year %in% MODEL_BASE_YEARS)  %>%
      remove_zero_production_land_leafs(prod = L2012.AgProduction_ag_irr_mgmt) ->
      L2252.LN5_MgdAllocation_crop

    # L2252.LN5_HistMgdAllocation_bio
    ALL_LAND_ALLOCATION %>%
      filter(grepl("biomassGrass", LandLeaf) | grepl("biomassTree", LandLeaf)) %>%
      filter(year %in% aglu.LAND_HISTORY_YEARS) ->
      L2252.LN5_HistMgdAllocation_bio

    # L2252.LN5_MgdAllocation_bio
    ALL_LAND_ALLOCATION %>%
      filter(grepl("biomassGrass", LandLeaf) | grepl("biomassTree", LandLeaf)) %>%
      filter(year %in% MODEL_BASE_YEARS) ->
      L2252.LN5_MgdAllocation_bio


    # L2252.LN5_MgdCarbon_crop: Carbon content info, managed land in the fifth nest, cropland (no bio)
    # Soil will use default values, but vegetation will be replaced by bottom-up estimates
    L171.ag_EcYield_kgm2_R_C_Y_GLU %>%
      rename(yield = value) %>%
      left_join_error_no_match(GCAMLandLeaf_CdensityLT, by = c("GCAM_subsector" = "LandLeaf")) %>%
      rename(Cdensity_LT = Land_Type) %>%
      add_carbon_info(carbon_info_table = L121.CarbonContent_kgm2_R_LT_GLU) %>%
      # Replacing missing values in places with harvested area and production but no assigned cropland
      # If regions have harvested area and production but no cropland assigned in Hyde, we take "unmanaged" land and re-assign it to cropland.
      # however this re-assignment is done after computing carbon contents, so such regions will have missing values at this point.
      # because at this point cropland carbon contents are not derived from underlying vegetation, all are equal and we can just inherit the
      # values from any other region. No need to do this w veg at this point b/c it will be replaced later
      mutate(soil.carbon.density = if_else(is.na(soil.carbon.density), mean(soil.carbon.density, na.rm = TRUE), soil.carbon.density),
             hist.soil.carbon.density = if_else(is.na(hist.soil.carbon.density),
                                                mean(hist.soil.carbon.density, na.rm = TRUE), hist.soil.carbon.density),
             mature.age = if_else(is.na(mature.age), mean(mature.age, na.rm = TRUE), mature.age),
             mature.age = if_else(grepl("Tree", GCAM_subsector), aglu.TREECROP_MATURE_AGE, mature.age)) %>%
      # Map in yield -- this will be used to compute vegetation carbon
      # Map in information for calculation of cropland vegetation carbon; note there will be NAs since Fodder crops are missing
      left_join(L111.ag_resbio_R_C, by = c("region", "GCAM_commodity")) %>%
      left_join(A_Fodderbio_chars, by = c("GCAM_commodity")) %>%
      left_join(L121.CarbonContent_kgm2_R_TreeCrop_GLU, by = c("region", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      mutate(HarvestIndex.x = if_else(is.na(HarvestIndex.x), HarvestIndex.y, HarvestIndex.x),
             Root_Shoot.x = if_else(is.na(Root_Shoot.x), Root_Shoot.y, Root_Shoot.x),
             WaterContent.x = if_else(is.na(WaterContent.x), WaterContent.y, WaterContent.x)) %>%
      select(-HarvestIndex.y, -Root_Shoot.y, -WaterContent.y) %>%
      # Calculate vegetation carbon density based on the yields and crop characteristics
      mutate(hist.veg.carbon.density = round( yield / (HarvestIndex.x) * (1 + Root_Shoot.x) * (1 - WaterContent.x) *
                                                aglu.CCONTENT_CELLULOSE * aglu.CCONV_PEAK_AVG,  aglu.DIGITS_C_DENSITY_CROP),
             # For tree crops, replace the values calculated above with tree-specific carbon contents elsewhere calculated
             hist.veg.carbon.density = if_else(is.na(Tree_Cdensity_kgm2), hist.veg.carbon.density,
                                               round(Tree_Cdensity_kgm2, aglu.DIGITS_C_DENSITY_CROP)),
             # Replace missing values with the default values
             hist.veg.carbon.density = if_else(is.na(hist.veg.carbon.density), veg.carbon.density,
                                               hist.veg.carbon.density),
             veg.carbon.density = hist.veg.carbon.density) %>%
      left_join(A_LandLeaf3, by = c("GCAM_subsector" = "LandLeaf")) %>%
      mutate(LandAllocatorRoot = "root",
             LandNode1 = paste(LandNode1, GLU, sep = "_"),
             LandNode2 = paste(LandNode2, GLU, sep = "_"),
             LandNode3 = paste(LandNode3, GLU, sep = "_"),
             LandNode4 = paste(GCAM_subsector, GLU, sep = "_"),
             LandLeaf = paste(LandNode4, Irr_Rfd, sep = "_")) %>%
      convert_LN4_to_LN5(names = LEVEL2_DATA_NAMES[["LN5_MgdCarbon"]]) ->
      L2252.LN5_MgdCarbon_crop

    L2012.AgYield_bio_ref %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(region, AgProductionTechnology, yield) %>%
      separate(AgProductionTechnology, c("GCAM_subsector", "GLU", "Irr_Rfd", "level")) %>%
      left_join_error_no_match(GCAMLandLeaf_CdensityLT, by = c("GCAM_subsector" = "LandLeaf")) %>%
      rename(Cdensity_LT = Land_Type) %>%
      add_carbon_info(carbon_info_table = L121.CarbonContent_kgm2_R_LT_GLU) %>%
      # There may missing values, where the assigned land type (LT) from which to get the carbon content didn't actually exist. Re-set to defaults.
      mutate(soil.carbon.density = if_else(is.na(soil.carbon.density), mean(soil.carbon.density, na.rm = TRUE), soil.carbon.density),
             hist.soil.carbon.density = if_else(is.na(hist.soil.carbon.density),
                                                mean(hist.soil.carbon.density, na.rm = TRUE), hist.soil.carbon.density),
             mature.age = round(if_else(is.na(mature.age), mean(mature.age, na.rm = TRUE), mature.age)),
             # Convert biomass yield from GJ/m2 to kg/m2
             yield = yield / (aglu.BIO_ENERGY_CONTENT_GJT * CONV_KG_T)) %>%
      # Map in the harvest index, water content, and root-shoot ratio
      left_join(A_Fodderbio_chars, by=c(GCAM_subsector = "GCAM_commodity")) %>%
      # Calculate the veg carbon content. Assume that the crop is perennial so the root portion doesn't get multiplied by the peak->avg conversion
      mutate(hist.veg.carbon.density = yield / (HarvestIndex) * (1 - WaterContent) * aglu.CCONTENT_CELLULOSE * aglu.CCONV_PEAK_AVG +
               yield / (HarvestIndex) * (1 - WaterContent) * Root_Shoot * aglu.CCONTENT_CELLULOSE,
             hist.veg.carbon.density = round(hist.veg.carbon.density, aglu.DIGITS_C_DENSITY),
             veg.carbon.density = hist.veg.carbon.density) %>%
      left_join(A_LandLeaf3, by = c("GCAM_subsector" = "LandLeaf")) %>%
      mutate(LandAllocatorRoot = "root",
             LandNode1 = paste(LandNode1, GLU, sep = "_"),
             LandNode2 = paste(LandNode2, GLU, sep = "_"),
             LandNode3 = paste(LandNode3, GLU, sep = "_"),
             LandNode4 = paste(GCAM_subsector, GLU, sep = "_"),
             LandNode5 = paste(LandNode4, Irr_Rfd, sep = "_"),
             LandLeaf = paste(LandNode5, level, sep = "_")) %>%
      select(c(LEVEL2_DATA_NAMES[["LN5_MgdCarbon"]], "GLU", "Irr_Rfd", "level")) %>%
      mutate(Irr_Rfd = tolower(Irr_Rfd)) ->
      L2252.LN5_MgdCarbon_bio

    # L2252.LN5_LeafGhostShare: Ghost share of the new landleaf (lo-input versus hi-input)
    # NOTE: The ghost shares are inferred from average land shares allocated to hi-input
    # versus lo-input, across all crops
    #
    # First, prep landshares for easier joining to L2241.LN4_LeafGhostShare data
    L181.LandShare_R_bio_GLU_irr %>%
      gather(variable, landshare, -GCAM_region_ID, -region, -GLU, -Irr_Rfd) %>%
      separate(variable, c("variable", "level")) %>%
      select(-GCAM_region_ID, -variable) %>%
      mutate(Irr_Rfd = toupper(Irr_Rfd)) ->
      L2252.LandShare_R_bio_GLU_irr

    L2252.LN5_MgdAllocation_bio %>%
      distinct(region, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, LandNode4, LandNode5, LandLeaf) %>%
      separate(LandLeaf, c("crop1", "GLU", "Irr_Rfd", "level"), remove = FALSE) %>%
      select(-crop1)  %>%
      # use left_join to keep NA's for further manipulation
      left_join(L2252.LandShare_R_bio_GLU_irr, by = c("region", "GLU", "Irr_Rfd", "level")) %>%
      mutate(ghost.unnormalized.share = round(landshare, aglu.DIGITS_GHOSTSHARE), year = aglu.BIO_START_YEAR) %>%
      select(-landshare) %>%
      # For bio techs with no ghost share info, set lo- and hi-input techs to 0.5
      replace_na(replace = list(ghost.unnormalized.share = 0.5)) %>%
      select(c(LEVEL2_DATA_NAMES[["LN5_LeafGhostShare"]], "GLU", "Irr_Rfd", "level")) ->
      L2252.LN5_LeafGhostShare

    # Calculate share of irrigated vs rainfed land
    # First, multiply irrigated land by aglu.IRR_GHOST_SHARE_MULT
    L181.LC_bm2_R_C_Yh_GLU_irr_level %>%
      mutate(value = if_else(Irr_Rfd == "irr", value * aglu.IRR_GHOST_SHARE_MULT, value)) ->
      ADJ_LAND_COVER

    # Next, compute total land by GLU, using adjust levels
    ADJ_LAND_COVER %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      group_by(region, GLU) %>%
      summarize(total_land = sum(value)) %>%
      ungroup() ->
      TOTAL_GLU_LAND

    # Finally, compute share of adjusted land cover that is irrigated or rainfed
    ADJ_LAND_COVER %>%
      mutate(Irr_Rfd = toupper(Irr_Rfd)) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      group_by(region, GLU, Irr_Rfd) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      left_join(TOTAL_GLU_LAND, by = c("region", "GLU")) %>%
      mutate(landshare = value / total_land) %>%
      select(-value, -total_land) ->
      LANDSHARE_IRR_RFD

    # L2252.LN5_NodeGhostShare: Ghost share of the new nodes (irrigated versus rainfed)
    L2252.LN5_LeafGhostShare %>%
      distinct(region, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, LandNode4, LandNode5, year, GLU, Irr_Rfd) %>%
      left_join(LANDSHARE_IRR_RFD, by = c("region", "GLU", "Irr_Rfd")) %>%
      mutate(ghost.unnormalized.share = round(landshare, aglu.DIGITS_LAND_USE)) %>%
      select(-landshare) %>%
      # For bio techs with no ghost share info, set irr to 0 and rfd to 1
      mutate(ghost.unnormalized.share = if_else(is.na(ghost.unnormalized.share) & Irr_Rfd == "RFD", 1, ghost.unnormalized.share),
             ghost.unnormalized.share = if_else(is.na(ghost.unnormalized.share) & Irr_Rfd == "IRR", 0, ghost.unnormalized.share)) %>%
      select(c(LEVEL2_DATA_NAMES[["LN5_NodeGhostShare"]])) ->
      L2252.LN5_NodeGhostShare

    # Produce outputs
    L2252.LN5_Logit %>%
      add_title("Logit exponent of the fifth land nest by region") %>%
      add_units("NA") %>%
      add_comments("Logit exponent of the fifth land nest by region. AgLU regions") %>%
      add_comments("are given externally defined constant logit information.") %>%
      add_legacy_name("L2252.LN5_Logit") %>%
      add_precursors("L2242.LN4_Logit") ->
      L2252.LN5_Logit

    L2252.LN5_HistMgdAllocation_crop %>%
      add_title("Historical land cover for managed crop land (LT_GLU) in the fifth nest by region.") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Historical land cover for managed crop land (LT_GLU) in the fifth nest, from L181 land cover data.") %>%
      add_legacy_name("L2252.LN5_HistMgdAllocation_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L181.LC_bm2_R_C_Yh_GLU_irr_level",
                     "L2242.LN4_Logit") ->
      L2252.LN5_HistMgdAllocation_crop

    L2252.LN5_MgdAllocation_crop %>%
      add_title("Land cover in the model base periods for managed crop land (LT_GLU) in the fifth nest by region.") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Land cover in the model base periods for managed crop land (LT_GLU) in the fifth nest, from L181 land cover data.") %>%
      add_comments("Filter to remove zero production leafs, using L2012.AgProduction_ag_irr_mgmt") %>%
      add_legacy_name("L2252.LN5_MgdAllocation_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L181.LC_bm2_R_C_Yh_GLU_irr_level",
                     "L2242.LN4_Logit",
                     "L2012.AgProduction_ag_irr_mgmt") ->
      L2252.LN5_MgdAllocation_crop

    L2252.LN5_HistMgdAllocation_bio %>%
      add_title("Historical land cover for managed bio land (LT_GLU) in the fifth nest by region.") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Historical land cover for managed bio land (LT_GLU) in the fifth nest by region, ") %>%
      add_comments("generated directly from nest 4 files.") %>%
      add_legacy_name("L2252.LN5_HistMgdAllocation_bio") %>%
      same_precursors_as("L2252.LN5_HistMgdAllocation_crop") ->
      L2252.LN5_HistMgdAllocation_bio

    L2252.LN5_MgdAllocation_bio %>%
      add_title("Land cover in the model base periods for managed bio land (LT_GLU) in the fifth nest by region.") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Land cover in the model base periods for managed bio land (LT_GLU) in the fifth nest by region, ") %>%
      add_comments("generated directly from nest 4 files.") %>%
      add_legacy_name("L2252.LN5_MgdAllocation_bio") %>%
      same_precursors_as("L2252.LN5_HistMgdAllocation_crop") ->
      L2252.LN5_MgdAllocation_bio

    L2252.LN5_MgdCarbon_crop %>%
      add_title("Carbon content for managed crop land (LT_GLU) in fifth nest by region.") %>%
      add_units("Varies") %>%
      add_comments("Carbon content info for managed crop land (LT_GLU) in the fifth nest including soil and vegetative carbon, ") %>%
      add_comments("calculated from yield and other biomass characteristics (e.g., root-shoot, harvest index, water content).") %>%
      add_legacy_name("L2252.LN5_MgdCarbon_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LandLeaf3",
                     "L171.ag_irrEcYield_kgm2_R_C_Y_GLU",
                     "L171.ag_rfdEcYield_kgm2_R_C_Y_GLU",
                     "L111.ag_resbio_R_C",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L120.LC_soil_veg_carbon_GLU",
                     "L121.CarbonContent_kgm2_R_TreeCrop_GLU") ->
      L2252.LN5_MgdCarbon_crop

    L2252.LN5_MgdCarbon_bio %>%
      add_title("Carbon content for biofuel managed land (LT_GLU) in fifth nest by region.") %>%
      add_units("Varies") %>%
      add_comments("Carbon content info for biofuel managed land (LT_GLU) in the fifth nest including soil and vegetative carbon, ") %>%
      add_comments("calculated from yield and other biomass characteristics (e.g., root-shoot, harvest index, water content).") %>%
      add_legacy_name("L2252.LN5_MgdCarbon_bio") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_Fodderbio_chars",
                     "aglu/A_LandLeaf3",
                     "L2012.AgYield_bio_ref") ->
      L2252.LN5_MgdCarbon_bio



    L2252.LN5_LeafGhostShare %>%
      mutate(Irr_Rfd = tolower(Irr_Rfd)) %>%
      add_title("Ghost share of the landleaf in the fifth nest by region (lo-input versus hi-input)") %>%
      add_units("NA") %>%
      add_comments("Ghost share of the landleaf in the fifth nest by region. Ghost shares are inferred") %>%
      add_comments(" from average land shares allocated to hi-input versus lo-input in L181.LandShare, across all crops") %>%
      add_legacy_name("L2252.LN5_LeafGhostShare") %>%
      same_precursors_as("L2252.LN5_MgdAllocation_bio") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L181.LandShare_R_bio_GLU_irr") ->
      L2252.LN5_LeafGhostShare

    L2252.LN5_NodeGhostShare %>%
      add_title("Ghost share of the nest 4 nodes (irrigated versus rainfed)") %>%
      add_units("NA") %>%
      add_comments("Ghost share of the nest 4 nodes (irrigated versus rainfed).") %>%
      add_legacy_name("L2252.LN5_NodeGhostShare") %>%
      same_precursors_as("L2252.LN5_LeafGhostShare") %>%
      add_precursors("L181.LC_bm2_R_C_Yh_GLU_irr_level") ->
      L2252.LN5_NodeGhostShare

    return_data(L2252.LN5_Logit, L2252.LN5_HistMgdAllocation_crop, L2252.LN5_MgdAllocation_crop, L2252.LN5_HistMgdAllocation_bio, L2252.LN5_MgdAllocation_bio, L2252.LN5_MgdCarbon_crop, L2252.LN5_MgdCarbon_bio, L2252.LN5_LeafGhostShare, L2252.LN5_NodeGhostShare)
  } else {
    stop("Unknown command")
  }
}
