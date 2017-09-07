#' module_aglu_L2231.land_input_3_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2231.LN3_LogitTables[[ curr_table_name ]]}, \code{L2231.LN3_Logit}, \code{L2231.LN3_HistUnmgdAllocation}, \code{L2231.LN3_UnmgdAllocation}, \code{L2231.NodeEquiv}, \code{L2231.LN3_NoEmissCarbon}, \code{L2231.LN3_NodeCarbon}, \code{L2231.LN3_HistMgdAllocation_noncrop}, \code{L2231.LN3_MgdAllocation_noncrop}, \code{L2231.LN3_UnmgdCarbon}, \code{L2231.LN3_MgdCarbon_noncrop}. The corresponding file in the
#' original data system was \code{L2231.land_input_3_irr.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2231.land_input_3_irr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "aglu/GCAMLandLeaf_CdensityLT",
             FILE = "aglu/A_bio_ghost_share",
             FILE = "aglu/A_Fodderbio_chars",
             FILE = "aglu/A_LT_Mapping",
             FILE = "aglu/A_LandNode_logit",
             FILE = "aglu/A_LandLeaf_Unmgd3",
             FILE = "aglu/A_LandLeaf3",
             "L111.ag_resbio_R_C",
             "L121.CarbonContent_kgm2_R_LT_GLU",
             "L122.ag_EcYield_kgm2_R_C_Y_GLU",
             "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
             "L125.LC_bm2_R_LT_Yh_GLU",
             FILE = "temp-data-inject/L201.AgYield_bio_grass",
             FILE = "temp-data-inject/L201.AgYield_bio_tree",
             "L2012.AgYield_bio_ref"))
 #      "L223.LN3_Logit",
 # "L223.LN3_HistUnmgdAllocation",
 # "L223.LN3_UnmgdAllocation",
 # "L223.NodeEquiv",
 # "L223.LN3_NoEmissCarbon",
 # "L223.LN3_NodeCarbon",
 # "L223.LN3_HistMgdAllocation_noncrop",
 # "L223.LN3_MgdAllocation_noncrop",
 # "L223.LN3_UnmgdCarbon",
 # "L223.LN3_MgdCarbon_noncrop"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2231.LN3_LogitTables[[ curr_table_name ]]",
             "L2231.LN3_Logit",
             "L2231.LN3_HistUnmgdAllocation",
             "L2231.LN3_UnmgdAllocation",
             "L2231.NodeEquiv",
             "L2231.LN3_NoEmissCarbon",
             "L2231.LN3_NodeCarbon",
             "L2231.LN3_HistMgdAllocation_noncrop",
             "L2231.LN3_MgdAllocation_noncrop",
             "L2231.LN3_UnmgdCarbon",
             "L2231.LN3_MgdCarbon_noncrop"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    GCAMLandLeaf_CdensityLT <- get_data(all_data, "aglu/GCAMLandLeaf_CdensityLT")
    A_bio_ghost_share <- get_data(all_data, "aglu/A_bio_ghost_share")
    A_Fodderbio_chars <- get_data(all_data, "aglu/A_Fodderbio_chars")
    A_LT_Mapping <- get_data(all_data, "aglu/A_LT_Mapping")
    A_LandNode_logit <- get_data(all_data, "aglu/A_LandNode_logit")
    A_LandLeaf_Unmgd3 <- get_data(all_data, "aglu/A_LandLeaf_Unmgd3")
    A_LandLeaf3 <- get_data(all_data, "aglu/A_LandLeaf3")
    L111.ag_resbio_R_C <- get_data(all_data, "L111.ag_resbio_R_C")
    L121.CarbonContent_kgm2_R_LT_GLU <- get_data(all_data, "L121.CarbonContent_kgm2_R_LT_GLU")
    L122.ag_EcYield_kgm2_R_C_Y_GLU <- get_data(all_data, "L122.ag_EcYield_kgm2_R_C_Y_GLU")
    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU <- get_data(all_data, "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU")
    L125.LC_bm2_R_LT_Yh_GLU <- get_data(all_data, "L125.LC_bm2_R_LT_Yh_GLU")
    if(OLD_DATA_SYSTEM_BEHAVIOR){
      L201.AgYield_bio_grass <- get_data(all_data, "temp-data-inject/L201.AgYield_bio_grass")
      L201.AgYield_bio_tree  <- get_data(all_data, "temp-data-inject/L201.AgYield_bio_tree")
    }
    L2012.AgYield_bio_ref  <- get_data(all_data, "L2012.AgYield_bio_ref")

    # silence package check notes
    GCAM_commodity <- GCAM_region_ID <- region <- value <- year <- GLU <- GLU_name <- GLU_code <-
      LandLeaf <- Land_Type <- LandNode <- LandNode1 <- LandNode2 <- LandNode3 <- UnmanagedLandLeaf <-
      logit.year.fillout <- logit.exponent <- logit.type <- soilTimeScale <- `mature age` <- mature.age <-
      soil_c <- veg_c <- LC_bm2 <- LV_milUSD75 <- LV_USD75_bm2 <- LV_USD75_m2 <- HarvCropLand_bm2 <-
      unManagedLandValue <- LandAllocatorRoot <- hist.veg.carbon.density <- hist.soil.carbon.density <-
      veg.carbon.density <- soil.carbon.density <- allocation <- Land_Type.y <- mature.age.year.fillout <-
      min.veg.carbon.density <- min.soil.carbon.density <- . <- NULL


    # Process inputs
    # Replace GLU names and Add region names as needed
    L111.ag_resbio_R_C %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L111.ag_resbio_R_C

    L121.CarbonContent_kgm2_R_LT_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) %>%
      rename(mature.age = `mature age`) ->
      L121.CarbonContent_kgm2_R_LT_GLU

    # L122.ag_EcYield_kgm2_R_C_Y_GLU %>%
    #   left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
    #   replace_GLU(map = basin_to_country_mapping) ->
    #   L122.ag_EcYield_kgm2_R_C_Y_GLU
    #
    # L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
    #   left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
    #   replace_GLU(map = basin_to_country_mapping) ->
    #   L122.LC_bm2_R_HarvCropLand_C_Yh_GLU

    L125.LC_bm2_R_LT_Yh_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) ->
      L125.LC_bm2_R_LT_Yh_GLU

    # Load required inputs
    # L223.LN3_Logit <- get_data(all_data, "L223.LN3_Logit")
    # L223.LN3_HistUnmgdAllocation <- get_data(all_data, "L223.LN3_HistUnmgdAllocation")
    # L223.LN3_UnmgdAllocation <- get_data(all_data, "L223.LN3_UnmgdAllocation")
    # L223.NodeEquiv <- get_data(all_data, "L223.NodeEquiv")
    # L223.LN3_NoEmissCarbon <- get_data(all_data, "L223.LN3_NoEmissCarbon")
    # L223.LN3_NodeCarbon <- get_data(all_data, "L223.LN3_NodeCarbon")
    # L223.LN3_HistMgdAllocation_noncrop <- get_data(all_data, "L223.LN3_HistMgdAllocation_noncrop")
    # L223.LN3_MgdAllocation_noncrop <- get_data(all_data, "L223.LN3_MgdAllocation_noncrop")
    # L223.LN3_UnmgdCarbon <- get_data(all_data, "L223.LN3_UnmgdCarbon")
    # L223.LN3_MgdCarbon_noncrop <- get_data(all_data, "L223.LN3_MgdCarbon_noncrop")

    # Build table

    # L223.LN3_Logit: Logit exponent of the third nest.
    # First, Determine the node combinations applicable at this level.
    # Then, match in logit exponents based on the land node 2.
    # Finally, append GLU names and keep only relevant columns
    L125.LC_bm2_R_LT_Yh_GLU %>%
      select(region, GLU, Land_Type) %>%
      # not all land types have node matches, so use left_join
      left_join(select(A_LT_Mapping, Land_Type, LandNode1, LandNode2, LandNode3),
                               by = "Land_Type") %>%
      select(-Land_Type) %>%
      distinct() %>%
      na.omit() %>%
      mutate(LandAllocatorRoot = "root",
             logit.year.fillout = min(BASE_YEARS)) %>%
      # logit.type is NA by default, so left_join
      left_join(select(A_LandNode_logit, logit.exponent, logit.type, LandNode), by = c("LandNode3" = "LandNode")) %>%
      append_GLU(var1 = "LandNode1", var2 = "LandNode2", var3 = "LandNode3") %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["LN3_Logit"]], "logit.type"))) ->
      L223.LN3_Logit


    # # L223.LN3_leaf_bio: Biomass leaves and nodes, matching those created in L201.
    # ### how to handle old vs new? need L201 to match old ds but only have L2012 with different irr_mgmt cases
    # ### shouldn't want temp-data-inject forever? I think L2012 will have to be updated to produce both types of
    # ### outputs? irr_mgmt and not ie L201 and L2012.
    # bind_rows(L201.AgYield_bio_grass, L201.AgYield_bio_tree) %>%
    #   select(region, AgProductionTechnology) %>%
    #   distinct() %>%
    #   # recreate remove_GLU function in old DS with a separate and unite (the latter due to biomass_grass, biomass_tree):
    #   separate(AgProductionTechnology, c("tech1", "tech2", "GLU"), sep = "_") %>%
    #   unite(AgProductionTechnology, tech1, tech2, sep = aglu.CROP_DELIMITER) %>%
    #   # not all land types have node matches, so use left_join
    #   left_join(select(A_LT_Mapping, Land_Type, LandNode1, LandNode2, LandNode3),
    #             by = c("AgProductionTechnology" ="Land_Type")) %>%
    #   append_GLU(var1 = "LandNode1", var2 = "LandNode2", var3 = "LandNode3", var4 = "AgProductionTechnology") %>%
    #   mutate(LandLeaf = AgProductionTechnology) ->
    #   L223.LN3_leaf_bio


    # # L223.LN3_LeafGhostShare: Default shares for new technologies in specified years
    # # Default shares do not interpolate in the model, so write it out in all model future years (starting with first bio year)
    # ### uses the L201
    # L223.LN3_leaf_bio %>%
    #   mutate(LandAllocatorRoot = "root") %>%
    #   repeat_add_columns(tibble::tibble(year = FUTURE_YEARS)) %>%
    #   filter(year >= aglu.BIO_START_YEAR) %>%
    #   # left join to keep NA's for interpolation in next step:
    #   left_join(A_bio_ghost_share, by = "year") %>%
    #   mutate(ghost.unnormalized.share = approx_fun(year, ghost.share, rule = 2)) %>%
    #   select(one_of(c(LEVEL2_DATA_NAMES[["LN3_LeafGhostShare"]]))) ->
    #   L223.LN3_LeafGhostShare
    #
    #
    # # L223.LN3_LeafIsGhostShareRel: relative information about the ghost share
    # ### uses the L201
    # L223.LN3_leaf_bio %>%
    #   mutate(LandAllocatorRoot = "root",
    #          is.ghost.share.relative = 1) %>%
    #   select(one_of(c(LEVEL2_DATA_NAMES[["LN3_LeafIsGhostShareRel"]]))) ->
    #   L223.LN3_LeafIsGhostShareRel


    # Land Use History

    # Unmanaged land tables
    #
    # These tables are formed from a master table, made by filtering and adding
    # node_leaf_names in L125.LC.
    L125.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type %in% A_LandLeaf_Unmgd3$UnmanagedLandLeaf,
             year %in% c(LAND_HISTORY_YEARS, BASE_YEARS)) %>%
      mutate(allocation = round(value, aglu.DIGITS_LAND_USE)) %>%
      add_node_leaf_names(nesting_table = A_LandLeaf_Unmgd3, leaf_name = "UnmanagedLandLeaf",
                          LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3") ->
      L223.LC_bm2_R_Unmgd3_Yh_GLU

    # L223.LN3_HistUnmgdAllocation: Historical land cover, unmanaged land in the third nest
    # L223.LN3_UnmgdAllocation: Model base period land cover, unmanaged land in the third nest
    # Formed from filtering the master table by different years.
    L223.LC_bm2_R_Unmgd3_Yh_GLU %>%
      filter(year %in% LAND_HISTORY_YEARS) %>%
      select(one_of(LEVEL2_DATA_NAMES[["LN3_HistUnmgdAllocation"]])) ->
      L223.LN3_HistUnmgdAllocation

    L223.LC_bm2_R_Unmgd3_Yh_GLU %>%
      filter(year %in% BASE_YEARS) %>%
      select(one_of(LEVEL2_DATA_NAMES[["LN3_UnmgdAllocation"]])) ->
      L223.LN3_UnmgdAllocation


    # Managed land - non-crop (forest)
    #
    # These tables are formed from a master table, made by filtering and adding
    # node_leaf_names in L125.LC.
    L125.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type %in% A_LandLeaf3$LandLeaf,
             year %in% c(LAND_HISTORY_YEARS, BASE_YEARS)) %>%
      mutate(allocation = round(value, aglu.DIGITS_LAND_USE)) %>%
      add_node_leaf_names(nesting_table = A_LandLeaf3, leaf_name = "LandLeaf",
                          LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3") ->
      L223.LC_bm2_R_Mgd3_Yh_GLU

    # L223.LN3_HistMgdAllocation_noncrop: Historical land cover, managed land in the third nest (noncrop)
    # L223.LN3_MgdAllocation_noncrop: Model base period land cover, managed land in the third nest (noncrop)
    # Formed from filtering the master table by different years.
    L223.LC_bm2_R_Mgd3_Yh_GLU %>%
      filter(year %in% LAND_HISTORY_YEARS) %>%
      select(one_of(LEVEL2_DATA_NAMES[["LN3_HistMgdAllocation"]])) ->
      L223.LN3_HistMgdAllocation_noncrop

    L223.LC_bm2_R_Mgd3_Yh_GLU %>%
      filter(year %in% BASE_YEARS) %>%
      select(one_of(LEVEL2_DATA_NAMES[["LN3_MgdAllocation"]])) ->
      L223.LN3_MgdAllocation_noncrop


    # L223.LN3_UnmgdCarbon: Carbon content info, unmanaged land in the third nest
    L223.LC_bm2_R_Unmgd3_Yh_GLU %>%
      filter(year == max(BASE_YEARS)) %>%
      left_join_error_no_match(select(GCAMLandLeaf_CdensityLT, Land_Type, LandLeaf), by = c("Land_Type" = "LandLeaf")) %>%
      rename(Cdensity_LT = Land_Type.y) %>%
      add_carbon_info(carbon_info_table = L121.CarbonContent_kgm2_R_LT_GLU) %>%
      replace_na()
      select(one_of(LEVEL2_DATA_NAMES[["LN2_UnmgdCarbon"]])) ->


    L2231.NodeEquiv

    L2231.LN3_NoEmissCarbon


    L2231.LN3_NodeCarbon



    L2231.LN3_MgdCarbon_noncrop


    # # Managed land - crop
    # #
    # # These tables are formed from a master table, made by filtering and adding
    # # node_leaf_names in L122.LC
    # L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
    #   group_by(GCAM_region_ID, region, GLU, GCAM_commodity) %>%
    #   mutate(rowSum = sum(value)) %>%
    #   ungroup %>%
    #   filter(rowSum != 0,
    #          year %in% c(LAND_HISTORY_YEARS, BASE_YEARS)) %>%
    #   select(-rowSum) %>%
    #   mutate(allocation = round(value, aglu.DIGITS_LAND_USE)) %>%
    #   add_node_leaf_names(nesting_table = A_LandLeaf3, leaf_name = "LandLeaf",
    #                       LT_name = "GCAM_commodity",
    #                       LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3") ->
    #   L223.LC_bm2_R_HarvCropLand_C_Yh_GLU
    #
    # # L223.LN3_HistMgdAllocation_crop: Historical land cover, managed land in the third nest, cropland
    # # L223.LN3_MgdAllocation_crop: Model base year land cover, managed land in the third nest, cropland
    # L223.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
    #   filter(year %in% LAND_HISTORY_YEARS) %>%
    #   select(one_of(LEVEL2_DATA_NAMES[["LN3_HistMgdAllocation"]])) ->
    #   L223.LN3_HistMgdAllocation_crop
    #
    # L223.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
    #   filter(year %in% BASE_YEARS) %>%
    #   select(one_of(LEVEL2_DATA_NAMES[["LN3_MgdAllocation"]])) ->
    #   L223.LN3_MgdAllocation_crop
    #
    #
    # # L223.LN3_HistMgdAllocation_bio: Historical land cover, managed land in the third nest, bioenergy
    # ### Uses L201
    # L223.LN3_leaf_bio %>%
    #   mutate(LandAllocatorRoot = "root",
    #          allocation = 0) %>%
    #   repeat_add_columns(tibble::tibble(year = LAND_HISTORY_YEARS)) %>%
    #   select(one_of(LEVEL2_DATA_NAMES[["LN3_HistMgdAllocation"]])) ->
    #   L223.LN3_HistMgdAllocation_bio
    #
    # # L223.LN3_MgdAllocation_bio: Model base year land cover, managed land in the third nest, bioenergy
    # ### uses L201
    # L223.LN3_leaf_bio %>%
    #   mutate(LandAllocatorRoot = "root",
    #          allocation = 0) %>%
    #   repeat_add_columns(tibble::tibble(year = BASE_YEARS)) %>%
    #   select(one_of(LEVEL2_DATA_NAMES[["LN3_MgdAllocation"]])) ->
    #   L223.LN3_MgdAllocation_bio


    # Produce outputs
 #    tibble() %>%
 #   add_title("descriptive title of data") %>%
 # add_units("units") %>%
 # add_comments("comments describing how data generated") %>%
 # add_comments("can be multiple lines") %>%
 # add_legacy_name("L2231.LN3_LogitTables[[ curr_table_name ]]") %>%
 # add_precursors("precursor1", "precursor2", "etc") %>%
 # # typical flags, but there are others--see `constants.R`
 # add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
 #   L2231.LN3_LogitTables[[ curr_table_name ]]
tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.LN3_Logit") %>%
  add_precursors("common/GCAM_region_names",
                 "water/basin_to_country_mapping",
                 "aglu/GCAMLandLeaf_CdensityLT",
                 "aglu/A_bio_ghost_share",
                 "aglu/A_Fodderbio_chars",
                 "aglu/A_LT_Mapping",
                 "aglu/A_LandNode_logit",
                 "aglu/A_LandLeaf_Unmgd3",
                 "aglu/A_LandLeaf3",
                 "L111.ag_resbio_R_C",
                 "L121.CarbonContent_kgm2_R_LT_GLU",
                 "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                 "L125.LC_bm2_R_LT_Yh_GLU",
                 "L2012.AgYield_bio_ref") %>%
  # typical flags, but there are others--see `constant.R`
  add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
  L2231.LN3_Logit


tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.LN3_HistUnmgdAllocation") %>%
  add_precursors("common/GCAM_region_names",
                 "water/basin_to_country_mapping",
                 "aglu/GCAMLandLeaf_CdensityLT",
                 "aglu/A_bio_ghost_share",
                 "aglu/A_Fodderbio_chars",
                 "aglu/A_LT_Mapping",
                 "aglu/A_LandNode_logit",
                 "aglu/A_LandLeaf_Unmgd3",
                 "aglu/A_LandLeaf3",
                 "L111.ag_resbio_R_C",
                 "L121.CarbonContent_kgm2_R_LT_GLU",
                 "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                 "L125.LC_bm2_R_LT_Yh_GLU",
                 "L2012.AgYield_bio_ref") %>%
  # typical flags, but there are others--see `constants.R`
  add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
  L2231.LN3_HistUnmgdAllocation


tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.LN3_UnmgdAllocation") %>%
 add_precursors("common/GCAM_region_names",
                "water/basin_to_country_mapping",
                "aglu/GCAMLandLeaf_CdensityLT",
                "aglu/A_bio_ghost_share",
                "aglu/A_Fodderbio_chars",
                "aglu/A_LT_Mapping",
                "aglu/A_LandNode_logit",
                "aglu/A_LandLeaf_Unmgd3",
                "aglu/A_LandLeaf3",
                "L111.ag_resbio_R_C",
                "L121.CarbonContent_kgm2_R_LT_GLU",
                "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                "L125.LC_bm2_R_LT_Yh_GLU",
                "L2012.AgYield_bio_ref") %>%
  # typical flags, but there are others--see `constants.R`
  add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
  L2231.LN3_UnmgdAllocation


tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.NodeEquiv") %>%
  add_precursors("common/GCAM_region_names",
                 "water/basin_to_country_mapping",
                 "aglu/GCAMLandLeaf_CdensityLT",
                 "aglu/A_bio_ghost_share",
                 "aglu/A_Fodderbio_chars",
                 "aglu/A_LT_Mapping",
                 "aglu/A_LandNode_logit",
                 "aglu/A_LandLeaf_Unmgd3",
                 "aglu/A_LandLeaf3",
                 "L111.ag_resbio_R_C",
                 "L121.CarbonContent_kgm2_R_LT_GLU",
                 "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                 "L125.LC_bm2_R_LT_Yh_GLU",
                 "L2012.AgYield_bio_ref") %>%
  # typical flags, but there are others--see `constants.R`
  add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
  L2231.NodeEquiv


tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.LN3_NoEmissCarbon") %>%
  add_precursors("common/GCAM_region_names",
                 "water/basin_to_country_mapping",
                 "aglu/GCAMLandLeaf_CdensityLT",
                 "aglu/A_bio_ghost_share",
                 "aglu/A_Fodderbio_chars",
                 "aglu/A_LT_Mapping",
                 "aglu/A_LandNode_logit",
                 "aglu/A_LandLeaf_Unmgd3",
                 "aglu/A_LandLeaf3",
                 "L111.ag_resbio_R_C",
                 "L121.CarbonContent_kgm2_R_LT_GLU",
                 "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                 "L125.LC_bm2_R_LT_Yh_GLU",
                 "L2012.AgYield_bio_ref") %>%
  # typical flags, but there are others--see `constants.R`
  add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
  L2231.LN3_NoEmissCarbon


tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.LN3_NodeCarbon") %>%
  add_precursors("common/GCAM_region_names",
                 "water/basin_to_country_mapping",
                 "aglu/GCAMLandLeaf_CdensityLT",
                 "aglu/A_bio_ghost_share",
                 "aglu/A_Fodderbio_chars",
                 "aglu/A_LT_Mapping",
                 "aglu/A_LandNode_logit",
                 "aglu/A_LandLeaf_Unmgd3",
                 "aglu/A_LandLeaf3",
                 "L111.ag_resbio_R_C",
                 "L121.CarbonContent_kgm2_R_LT_GLU",
                 "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                 "L125.LC_bm2_R_LT_Yh_GLU",
                 "L2012.AgYield_bio_ref") %>%
  # typical flags, but there are others--see `constants.R`
  add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
  L2231.LN3_NodeCarbon


tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.LN3_HistMgdAllocation_noncrop") %>%
  add_precursors("common/GCAM_region_names",
                 "water/basin_to_country_mapping",
                 "aglu/GCAMLandLeaf_CdensityLT",
                 "aglu/A_bio_ghost_share",
                 "aglu/A_Fodderbio_chars",
                 "aglu/A_LT_Mapping",
                 "aglu/A_LandNode_logit",
                 "aglu/A_LandLeaf_Unmgd3",
                 "aglu/A_LandLeaf3",
                 "L111.ag_resbio_R_C",
                 "L121.CarbonContent_kgm2_R_LT_GLU",
                 "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                 "L125.LC_bm2_R_LT_Yh_GLU",
                 "L2012.AgYield_bio_ref") %>%
  # typical flags, but there are others--see `constants.R`
  add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
  L2231.LN3_HistMgdAllocation_noncrop


tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.LN3_MgdAllocation_noncrop") %>%
  add_precursors("common/GCAM_region_names",
                 "water/basin_to_country_mapping",
                 "aglu/GCAMLandLeaf_CdensityLT",
                 "aglu/A_bio_ghost_share",
                 "aglu/A_Fodderbio_chars",
                 "aglu/A_LT_Mapping",
                 "aglu/A_LandNode_logit",
                 "aglu/A_LandLeaf_Unmgd3",
                 "aglu/A_LandLeaf3",
                 "L111.ag_resbio_R_C",
                 "L121.CarbonContent_kgm2_R_LT_GLU",
                 "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                 "L125.LC_bm2_R_LT_Yh_GLU",
                 "L2012.AgYield_bio_ref") %>%
  # typical flags, but there are others--see `constants.R`
  add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
  L2231.LN3_MgdAllocation_noncrop


tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.LN3_UnmgdCarbon") %>%
  add_precursors("common/GCAM_region_names",
                 "water/basin_to_country_mapping",
                 "aglu/GCAMLandLeaf_CdensityLT",
                 "aglu/A_bio_ghost_share",
                 "aglu/A_Fodderbio_chars",
                 "aglu/A_LT_Mapping",
                 "aglu/A_LandNode_logit",
                 "aglu/A_LandLeaf_Unmgd3",
                 "aglu/A_LandLeaf3",
                 "L111.ag_resbio_R_C",
                 "L121.CarbonContent_kgm2_R_LT_GLU",
                 "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                 "L125.LC_bm2_R_LT_Yh_GLU",
                 "L2012.AgYield_bio_ref") %>%
  # typical flags, but there are others--see `constants.R`
  add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
  L2231.LN3_UnmgdCarbon


tibble() %>%
  add_title("descriptive title of data") %>%
  add_units("units") %>%
  add_comments("comments describing how data generated") %>%
  add_comments("can be multiple lines") %>%
  add_legacy_name("L2231.LN3_MgdCarbon_noncrop") %>%
  add_precursors("common/GCAM_region_names",
                 "water/basin_to_country_mapping",
                 "aglu/GCAMLandLeaf_CdensityLT",
                 "aglu/A_bio_ghost_share",
                 "aglu/A_Fodderbio_chars",
                 "aglu/A_LT_Mapping",
                 "aglu/A_LandNode_logit",
                 "aglu/A_LandLeaf_Unmgd3",
                 "aglu/A_LandLeaf3",
                 "L111.ag_resbio_R_C",
                 "L121.CarbonContent_kgm2_R_LT_GLU",
                 "L122.ag_EcYield_kgm2_R_C_Y_GLU",
                 "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
                 "L125.LC_bm2_R_LT_Yh_GLU",
                 "L2012.AgYield_bio_ref") %>%
  # typical flags, but there are others--see `constants.R`
  add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
  L2231.LN3_MgdCarbon_noncrop


    return_data(#L2231.LN3_LogitTables[[ curr_table_name ]],
                L2231.LN3_Logit, L2231.LN3_HistUnmgdAllocation, L2231.LN3_UnmgdAllocation, L2231.NodeEquiv, L2231.LN3_NoEmissCarbon, L2231.LN3_NodeCarbon, L2231.LN3_HistMgdAllocation_noncrop, L2231.LN3_MgdAllocation_noncrop, L2231.LN3_UnmgdCarbon, L2231.LN3_MgdCarbon_noncrop)
  } else {
    stop("Unknown command")
  }
}



