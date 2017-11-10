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
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author ACS September 2017
module_aglu_L2252.land_input_5_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             "L181.LandShare_R_bio_GLU_irr",
             "L181.LC_bm2_R_C_Yh_GLU_irr_level",
             "L181.YieldMult_R_bio_GLU_irr",
             FILE = "temp-data-inject/L2241.LN4_Logit",
             FILE = "temp-data-inject/L2241.LN4_HistMgdAllocation_crop",
             FILE = "temp-data-inject/L2241.LN4_MgdAllocation_crop",
             FILE = "temp-data-inject/L2241.LN4_HistMgdAllocation_bio",
             FILE = "temp-data-inject/L2241.LN4_MgdAllocation_bio",
             FILE = "temp-data-inject/L2241.LN4_MgdCarbon_crop",
             FILE = "temp-data-inject/L2241.LN4_MgdCarbon_bio",
             FILE = "temp-data-inject/L2241.LN4_LeafGhostShare",
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
    L181.LandShare_R_bio_GLU_irr <- get_data(all_data, "L181.LandShare_R_bio_GLU_irr")
    L181.LC_bm2_R_C_Yh_GLU_irr_level <- get_data(all_data, "L181.LC_bm2_R_C_Yh_GLU_irr_level")
    L181.YieldMult_R_bio_GLU_irr <- get_data(all_data, "L181.YieldMult_R_bio_GLU_irr")
    L2241.LN4_Logit <- get_data(all_data, "temp-data-inject/L2241.LN4_Logit")
    L2241.LN4_HistMgdAllocation_crop <- get_data(all_data, "temp-data-inject/L2241.LN4_HistMgdAllocation_crop")
    L2241.LN4_MgdAllocation_crop <- get_data(all_data, "temp-data-inject/L2241.LN4_MgdAllocation_crop")
    L2241.LN4_HistMgdAllocation_bio <- get_data(all_data, "temp-data-inject/L2241.LN4_HistMgdAllocation_bio")
    L2241.LN4_MgdAllocation_bio <- get_data(all_data, "temp-data-inject/L2241.LN4_MgdAllocation_bio")
    L2241.LN4_MgdCarbon_crop <- get_data(all_data, "temp-data-inject/L2241.LN4_MgdCarbon_crop")
    L2241.LN4_MgdCarbon_bio <- get_data(all_data, "temp-data-inject/L2241.LN4_MgdCarbon_bio")
    L2241.LN4_LeafGhostShare <- get_data(all_data, "temp-data-inject/L2241.LN4_LeafGhostShare")
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
      crop2 <- landshare <- lev <- . <- NULL


    # 1. Process inputs

    # Replace GLU names and Add region names
    L181.LandShare_R_bio_GLU_irr %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) ->
      L181.LandShare_R_bio_GLU_irr

    L181.LC_bm2_R_C_Yh_GLU_irr_level %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) ->
      L181.LC_bm2_R_C_Yh_GLU_irr_level

    # The old data system does not correct the GLU names for this input.
    # Consequently, in the original L2252, the match on line 119 introduces
    # hist.veg.carbon.density = NA for all region-glu-irr-mgmt because it
    # is being told to match GLU008 style names with ArkWhtRedR style names
    # so there are no matches.
    # As a result, there is no difference in hist.veg.carbon.density for
    # hi vs lo nests.
    if(OLD_DATA_SYSTEM_BEHAVIOR) {
      L181.YieldMult_R_bio_GLU_irr %>%
        left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
        L181.YieldMult_R_bio_GLU_irr
    } else {
      L181.YieldMult_R_bio_GLU_irr %>%
        left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
        replace_GLU(map = basin_to_country_mapping) ->
        L181.YieldMult_R_bio_GLU_irr
    }

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

    # L2252.LN5_Logit: Logit exponent between lo and hi managed techs for each crop-irrigationtype combo
    # ie competition between Corn_IRR_hi and Corn_Irr_lo.
    L2241.LN4_Logit %>%
      repeat_add_columns(tibble::tibble(Irr_Rfd = c("IRR", "RFD"))) %>%
      mutate(LandNode5 = paste(LandNode4, Irr_Rfd, sep = aglu.IRR_DELIMITER),
             logit.exponent = aglu.MGMT_LOGIT_EXP,
             logit.type = aglu.MGMT_LOGIT_TYPE) %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["LN5_Logit"]], "logit.type"))) ->
      L2252.LN5_Logit


    # create an intermediary table of land allocation for each landleaf (= crop-glu-irr-mgmt)
    # in each region-year. This is used for both HistMgdAllocation and MgdAllocation for crops.
    L181.LC_bm2_R_C_Yh_GLU_irr_level %>%
      mutate(Irr_Rfd = toupper(Irr_Rfd),
             LandLeaf = paste(paste(paste(GCAM_commodity, GLU, sep = aglu.CROP_GLU_DELIMITER),
                                    Irr_Rfd, sep = aglu.IRR_DELIMITER),
                              level, sep = aglu.MGMT_DELIMITER),
             value = round(value, aglu.DIGITS_LAND_USE)) %>%
      rename(allocation = value) %>%
      select(region, year, LandLeaf, allocation) ->
      L2252.LC_bm2_R_C_Yh_GLU_irr_mgmt


    # L2252.LN5_HistMgdAllocation_crop: historical cropland allocation
    # in the fifth land nest ie for each crop-irr-mgmt combo in each region-glu-year.
    L2241.LN4_HistMgdAllocation_crop %>%
      convert_LN4_to_LN5(names = LEVEL2_DATA_NAMES[["LN5_HistMgdAllocation"]]) %>%
      select(-allocation) %>%
      left_join_error_no_match(L2252.LC_bm2_R_C_Yh_GLU_irr_mgmt, by = c("region", "year", "LandLeaf")) ->
      L2252.LN5_HistMgdAllocation_crop


    # L2252.LN5_MgdAllocation_crop: cropland allocation
    # in the fifth land nest ie for each crop-irr-mgmt combo in each region-glu-year.
    L2241.LN4_MgdAllocation_crop %>%
      convert_LN4_to_LN5(names = LEVEL2_DATA_NAMES[["LN5_MgdAllocation"]]) %>%
      select(-allocation) %>%
      left_join_error_no_match(L2252.LC_bm2_R_C_Yh_GLU_irr_mgmt, by = c("region", "year", "LandLeaf")) %>%
      remove_zero_production_land_leafs(prod = L2012.AgProduction_ag_irr_mgmt) ->
      L2252.LN5_MgdAllocation_crop


    # Several outputs are unchanged from their L2241 form. They just undergo relabeling and the addition of
    # hi/lo management information:
    #
    # L2252.LN5_HistMgdAllocation_bio
    L2241.LN4_HistMgdAllocation_bio %>%
      convert_LN4_to_LN5(names = LEVEL2_DATA_NAMES[["LN5_HistMgdAllocation"]]) ->
      L2252.LN5_HistMgdAllocation_bio

    # L2252.LN5_MgdAllocation_bio
    L2241.LN4_MgdAllocation_bio %>%
      convert_LN4_to_LN5(names = LEVEL2_DATA_NAMES[["LN5_MgdAllocation"]]) ->
      L2252.LN5_MgdAllocation_bio

    # L2252.LN5_MgdCarbon_crop
    L2241.LN4_MgdCarbon_crop %>%
      convert_LN4_to_LN5(names = LEVEL2_DATA_NAMES[["LN5_MgdCarbon"]]) ->
      L2252.LN5_MgdCarbon_crop


    # L2252.LN5_MgdCarbon_bio
    # Undergoes relabelling and the addition of hi/lo mgmt information;
    # vegetative carbon content is multiplied for the hi/lo yield
    # multipliers as well.
    #
    # First, prep multipliers for easier joining to relabeled L2241 data.
    L181.YieldMult_R_bio_GLU_irr %>%
      gather(variable, yieldmult, -GCAM_region_ID, -region, -GLU, -Irr_Rfd) %>%
      separate(variable, c("variable", "level")) %>%
      select(-GCAM_region_ID, -variable) %>%
      mutate(Irr_Rfd = toupper(Irr_Rfd)) ->
      L2252.YieldMult_R_bio_GLU_irr

    # Second, relabel L2241 data and join the multiplier information
    L2241.LN4_MgdCarbon_bio %>%
      convert_LN4_to_LN5(names = LEVEL2_DATA_NAMES[["LN5_MgdCarbon"]]) %>%
      mutate(tmp = LandLeaf) %>%
      separate(tmp, c("crop1", "crop2", "GLU", "Irr_Rfd", "level")) %>%
      select(-crop1, -crop2) %>%
      # some region-glu-irr-mgmt have no info, so less restrictive join and overwrite NAs
      left_join(L2252.YieldMult_R_bio_GLU_irr, by = c("region", "GLU", "Irr_Rfd", "level")) %>%
      # For places with no yieldmult, set hist.veg.carbon.density equal to veg.carbon.density;
      # otherwise, hist.veg.carbon.density = veg.carbon.density * yieldmult.
      mutate(hist.veg.carbon.density = if_else(is.na(yieldmult),
                                             veg.carbon.density,
                                             round(veg.carbon.density * yieldmult, aglu.DIGITS_C_DENSITY)),
             veg.carbon.density = hist.veg.carbon.density) %>%
      select(-yieldmult) ->
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

    # Second, relabel L2241 data and join the land share information
    L2241.LN4_LeafGhostShare %>%
      convert_LN4_to_LN5(names = c(LEVEL2_DATA_NAMES[["LN5_LeafGhostShare"]], "level")) %>%
      mutate(tmp = LandLeaf) %>%
      separate(tmp, c("crop1", "crop2", "GLU", "Irr_Rfd", "lev")) %>%
      select(-lev, -crop1, -crop2) %>%
      # use left_join to keep NA's for further manipulation
      left_join(L2252.LandShare_R_bio_GLU_irr, by = c("region", "GLU", "Irr_Rfd", "level")) %>%
      mutate(ghost.unnormalized.share = round(landshare, aglu.DIGITS_LAND_USE)) %>%
      select(-landshare) %>%
      # For bio techs with no ghost share info, set lo- and hi-input techs to 0.5
      replace_na(replace = list(ghost.unnormalized.share = 0.5)) ->
      L2252.LN5_LeafGhostShare


    # L2252.LN5_NodeGhostShare: Ghost share of the new nodes (irrigated versus rainfed)
    L2241.LN4_LeafGhostShare %>%
      rename(LandNode5 = LandLeaf) ->
      L2252.LN5_NodeGhostShare


    # Produce outputs
    L2252.LN5_Logit %>%
      add_title("Logit exponent of the fifth land nest by region") %>%
      add_units("NA") %>%
      add_comments("Logit exponent of the fifth land nest by region. AgLU regions") %>%
      add_comments("are given externally defined constant logit information.") %>%
      add_legacy_name("L2252.LN5_Logit") %>%
      add_precursors("temp-data-inject/L2241.LN4_Logit") ->
      L2252.LN5_Logit

    L2252.LN5_HistMgdAllocation_crop %>%
      add_title("Historical land cover for managed crop land (LT_GLU) in the fifth nest by region.") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Historical land cover for managed crop land (LT_GLU) in the fifth nest, from L181 land cover data.") %>%
      add_legacy_name("L2252.LN5_HistMgdAllocation_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L181.LC_bm2_R_C_Yh_GLU_irr_level",
                     "temp-data-inject/L2241.LN4_HistMgdAllocation_crop") %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L2252.LN5_HistMgdAllocation_crop

    L2252.LN5_MgdAllocation_crop %>%
      add_title("Land cover in the model base periods for managed crop land (LT_GLU) in the fifth nest by region.") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Land cover in the model base periods for managed crop land (LT_GLU) in the fifth nest, from L181 land cover data.") %>%
      add_legacy_name("L2252.LN5_MgdAllocation_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L181.LC_bm2_R_C_Yh_GLU_irr_level",
                     "temp-data-inject/L2241.LN4_MgdAllocation_crop",
                     "L2012.AgProduction_ag_irr_mgmt") ->
      L2252.LN5_MgdAllocation_crop

    L2252.LN5_HistMgdAllocation_bio %>%
      add_title("Historical land cover for managed bio land (LT_GLU) in the fifth nest by region.") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Historical land cover for managed bio land (LT_GLU) in the fifth nest by region,") %>%
      add_comments("generated directly from nest 4 files.") %>%
      add_legacy_name("L2252.LN5_HistMgdAllocation_bio") %>%
      add_precursors("temp-data-inject/L2241.LN4_HistMgdAllocation_bio") ->
      L2252.LN5_HistMgdAllocation_bio

    L2252.LN5_MgdAllocation_bio %>%
      add_title("Land cover in the model base periods for managed bio land (LT_GLU) in the fifth nest by region.") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Land cover in the model base periods for managed bio land (LT_GLU) in the fifth nest by region,") %>%
      add_comments("generated directly from nest 4 files.") %>%
      add_legacy_name("L2252.LN5_MgdAllocation_bio") %>%
      add_precursors("temp-data-inject/L2241.LN4_MgdAllocation_bio") ->
      L2252.LN5_MgdAllocation_bio

    L2252.LN5_MgdCarbon_crop %>%
      add_title("Carbon content for managed crop land (LT_GLU) in fifth nest by region.") %>%
      add_units("Varies") %>%
      add_comments("Carbon content info for managed crop land (LT_GLU) in the fifth nest including soil and vegetative carbon,") %>%
      add_comments("generated directly from nest 4 files.") %>%
      add_legacy_name("L2252.LN5_MgdCarbon_crop") %>%
      add_precursors("temp-data-inject/L2241.LN4_MgdCarbon_crop") ->
      L2252.LN5_MgdCarbon_crop

    L2252.LN5_MgdCarbon_bio %>%
      mutate(Irr_Rfd = tolower(Irr_Rfd)) %>%
      add_title("Carbon content for biofuel managed land (LT_GLU) in fifth nest by region.") %>%
      add_units("Varies") %>%
      add_comments("Carbon content info for biofuel managed land (LT_GLU) in the fifth nest including soil and vegetative carbon,") %>%
      add_comments("from L181 yield multiplier data and L2241.LN4_MgdCarbon_bio.")%>%
      add_legacy_name("L2252.LN5_MgdCarbon_bio") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L181.YieldMult_R_bio_GLU_irr",
                     "temp-data-inject/L2241.LN4_MgdCarbon_bio") ->
      L2252.LN5_MgdCarbon_bio

    L2252.LN5_LeafGhostShare %>%
      mutate(Irr_Rfd = tolower(Irr_Rfd)) %>%
      add_title("Ghost share of the landleaf in the fifth nest by region (lo-input versus hi-input)") %>%
      add_units("NA") %>%
      add_comments("Ghost share of the landleaf in the fifth nest by region. Ghost shares are inferred") %>%
      add_comments(" from average land shares allocated to hi-input versus lo-input in L181.LandShare, across all crops") %>%
      add_legacy_name("L2252.LN5_LeafGhostShare") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L181.LandShare_R_bio_GLU_irr",
                     "temp-data-inject/L2241.LN4_LeafGhostShare") ->
      L2252.LN5_LeafGhostShare

    L2252.LN5_NodeGhostShare %>%
      # add_title("Ghost share of the nest 4 nodes (irrigated versus rainfed)") %>%
      add_units("NA") %>%
      add_comments("Ghost share of the nest 4 nodes (irrigated versus rainfed).") %>%
      add_legacy_name("L2252.LN5_NodeGhostShare") %>%
      add_precursors("temp-data-inject/L2241.LN4_LeafGhostShare") ->
      L2252.LN5_NodeGhostShare

    return_data(L2252.LN5_Logit, L2252.LN5_HistMgdAllocation_crop, L2252.LN5_MgdAllocation_crop, L2252.LN5_HistMgdAllocation_bio, L2252.LN5_MgdAllocation_bio, L2252.LN5_MgdCarbon_crop, L2252.LN5_MgdCarbon_bio, L2252.LN5_LeafGhostShare, L2252.LN5_NodeGhostShare)
  } else {
    stop("Unknown command")
  }
}
