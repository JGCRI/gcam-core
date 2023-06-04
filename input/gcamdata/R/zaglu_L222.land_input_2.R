# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L222.land_input_2
#'
#' Produce L222.LN2_Logit, L222.LN2_HistUnmgdAllocation, L222.LN2_UnmgdAllocation,
#' L222.LN2_HistMgdAllocation, L222.LN2_MgdAllocation, L222.LN2_UnmgdCarbon, L222.LN2_MgdCarbon,
#' L222.LN2_HistUnmgdAllocation_noprot, L222.LN2_UnmgdAllocation_noprot, L222.LN1_Logit_prot,
#' L222.LN1_HistUnmgdAllocation_prot, L222.LN1_UnmgdAllocation_prot, L222.LN1_UnmgdCarbon_prot
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{curr_table$data}, \code{L222.LN2_Logit}, \code{L222.LN2_HistUnmgdAllocation}, \code{L222.LN2_UnmgdAllocation}, \code{L222.LN2_HistMgdAllocation}, \code{L222.LN2_MgdAllocation}, \code{L222.LN2_UnmgdCarbon}, \code{L222.LN2_MgdCarbon}, \code{L222.LN2_HistUnmgdAllocation_noprot}, \code{L222.LN2_UnmgdAllocation_noprot}, \code{L222.LN2_UnmgdCarbon}, \code{curr_table$data}, \code{L222.LN1_Logit_prot}, \code{L222.LN1_HistUnmgdAllocation_prot}, \code{L222.LN1_UnmgdAllocation_prot}, \code{L222.LN1_UnmgdCarbon_prot}. The corresponding file in the
#' original data system was \code{L222.land_input_2.R} (aglu level2).
#' @details
#'  \itemize{
#' \item{"L222.LN2_Logit: Logit exponent of the second land nest by region.
#' AgLU regions are given externally defined constant logit information."}
#' \item{"L222.LN2_HistUnmgdAllocation: Historical land cover for unmanaged land (LT_GLU) in the second nest by region.
#' Historical land cover for unmanaged land in the second nest, from L125 land cover data."}
#' \item{"L222.LN2_UnmgdAllocation: Land cover in the model base periods for unmanaged land (LT_GLU) in the second nest by region.
#' Land cover in the model base periods for unmanaged land in the second nest, from L125 land cover data."}
#' \item{"L222.LN2_HistMgdAllocation: Historical land cover for managed land (LT_GLU) in the second nest by region.
#' Historical land cover for managed land in the second nest, from L125 land cover data."}
#' \item{"L222.LN2_MgdAllocation: Land cover in the model base periods for managed land (LT_GLU) in the second nest by region.
#' Land cover in the model base periods for managed land in the second nest, from L125 land cover data."}
#' \item{"L222.LN2_UnmgdCarbon: Carbon content for unmanaged land (LT_GLU) in second nest by region.
#' Carbon content info for unmanaged land in the second nest including soil and vegetative carbon,
#' from L125 land cover data, L121 carbon content data, and GCAMLandLeaf_CdensityLT assumptions."}
#' \item{"L222.LN2_MgdCarbon: Carbon content for managed land (LT_GLU) in second nest by region.
#' Carbon content info for managed land in the second nest including soil and vegetative carbon,
#' from L125 land cover data, L121 carbon content data, and GCAMLandLeaf_CdensityLT assumptions."}
#' \item{"L222.LN2_HistUnmgdAllocation_noprot: Historical land cover for unprotected unmanaged land (LT_GLU) in the second nest by region.
#' Historical land cover for unprotected unmanaged land in the second nest, from L222.LN2_HistUnmgdAllocation."}
#' \item{" L222.LN2_UnmgdAllocation_noprot: Land cover in the model base periods for unprotected unmanaged land (LT_GLU) in the second nest by region.
#' Land cover in the model base periods for unprotected unmanaged land in the second nest, from L222.LN2_UnmgdAllocation."}
#' \item{"L222.LN1_HistUnmgdAllocation_prot: Historical land cover for protected unmanaged land (LT_GLU) in the first nest by region.
#' Historical land cover for protected unmanaged land in the first nest, from L222.LN2_HistUnmgdAllocation."}
#' \item{"L222.LN1_UnmgdAllocation_prot: Land cover in the model base periods for protected unmanaged land (LT_GLU) in the first nest by region.
#' Land cover in the model base periods for protected unmanaged land in the first nest, from L222.LN2_UnmgdAllocation."}
#' \item{"L222.LN1_UnmgdCarbon_prot: Carbon content for protected unmanaged land (LT_GLU) in first nest by region.
#' Carbon content info for protected unmanaged land in the first nest including soil and vegetative carbon, from L222.LN2_UnmgdCarbon."}
#' \item{"L222.LN1_Logit_prot: Logit exponent of protected land in the first nest by region.
#' AgLU regions are given externally defined constant logit information. From L222.LN1_UnmgdAllocation_prot"}
#' }
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter left_join mutate select
#' @author ACS August 2017
module_aglu_L222.land_input_2 <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "aglu/GCAMLandLeaf_CdensityLT",
             FILE = "aglu/A_LandNode_logit",
             FILE = "aglu/A_LandLeaf_Unmgd2",
             FILE = "aglu/A_LandLeaf2",
             FILE = "aglu/A_LT_Mapping",
             "L121.CarbonContent_kgm2_R_LT_GLU",
             "L125.LC_bm2_R_LT_Yh_GLU",
             "L120.LC_prot_land_frac_GLU",
             "L120.LC_soil_veg_carbon_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L222.LN2_Logit",
             "L222.LN2_HistUnmgdAllocation",
             "L222.LN2_UnmgdAllocation",
             "L222.LN2_HistMgdAllocation",
             "L222.LN2_MgdAllocation",
             "L222.LN2_UnmgdCarbon",
             "L222.LN2_MgdCarbon",
             "L222.LN2_HistUnmgdAllocation_noprot",
             "L222.LN2_UnmgdAllocation_noprot",
             "L222.LN1_Logit_prot",
             "L222.LN1_HistUnmgdAllocation_prot",
             "L222.LN1_UnmgdAllocation_prot",
             "L222.LN1_UnmgdCarbon_prot"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    GCAMLandLeaf_CdensityLT <- get_data(all_data, "aglu/GCAMLandLeaf_CdensityLT")
    A_LandNode_logit <- get_data(all_data, "aglu/A_LandNode_logit")
    A_LandLeaf_Unmgd2 <- get_data(all_data, "aglu/A_LandLeaf_Unmgd2")
    A_LandLeaf2 <- get_data(all_data, "aglu/A_LandLeaf2")
    A_LT_Mapping <- get_data(all_data, "aglu/A_LT_Mapping")
    L121.CarbonContent_kgm2_R_LT_GLU <- get_data(all_data, "L121.CarbonContent_kgm2_R_LT_GLU")

    L125.LC_bm2_R_LT_Yh_GLU <- get_data(all_data, "L125.LC_bm2_R_LT_Yh_GLU", strip_attributes = TRUE)
    L120.LC_prot_land_frac_GLU <- get_data(all_data, "L120.LC_prot_land_frac_GLU",strip_attributes = TRUE)

    # If the carbon data source is set to moirai, use the spatially distinct carbon values. If not, use the Houghton values.
    if(aglu.CARBON_DATA_SOURCE=="moirai"){

      L121.CarbonContent_kgm2_R_LT_GLU <- get_data(all_data, "L120.LC_soil_veg_carbon_GLU")
    }else{
      L121.CarbonContent_kgm2_R_LT_GLU <- get_data(all_data, "L121.CarbonContent_kgm2_R_LT_GLU")

    }


    # This chunk just deals with the Pasture nodes. Therefore we filter the protected area fractions just for Pastures.
    L120.LC_prot_land_frac_GLU <- L120.LC_prot_land_frac_GLU %>%  filter(Land_Type == aglu.PASTURE_NODE_NAMES,year == MODEL_FINAL_BASE_YEAR) %>% select(-Land_Type,-year)
    # silence package check notes
    GCAM_commodity <- GCAM_region_ID <- region <- value <- year <- GLU <- GLU_name <- GLU_code <-
      LandLeaf <- Land_Type <- LandNode <- LandNode1 <- LandNode2 <- LandNode3 <- UnmanagedLandLeaf <-
      logit.year.fillout <- logit.exponent <- logit.type <- soilTimeScale <- `mature age` <- mature.age <-
      soil_c <- veg_c <- LC_bm2 <- LV_milUSD75 <- LV_USD75_bm2 <- LV_USD75_m2 <- HarvCropLand_bm2 <-
      unManagedLandValue <- LandAllocatorRoot <- hist.veg.carbon.density <- hist.soil.carbon.density <-
      veg.carbon.density <- soil.carbon.density <- allocation <- Land_Type.y <- mature.age.year.fillout <-
      min.veg.carbon.density <- min.soil.carbon.density <- prot_frac <- . <- NULL


    # 1. Process inputs

    # Replace GLU names and Add region names
    L121.CarbonContent_kgm2_R_LT_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) %>%
      rename(mature.age = `mature age`) ->
      L121.CarbonContent_kgm2_R_LT_GLU

    L125.LC_bm2_R_LT_Yh_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) ->
      L125.LC_bm2_R_LT_Yh_GLU


    # 2. Build tables

    # L222.LN2_Logit: Logit exponent of the second land nest (down two nests from the top-level nest)
    L125.LC_bm2_R_LT_Yh_GLU %>%
      # Determine which node combinations apply at this level.
      # not all land types have node matches, so use left_join
      left_join(select(A_LT_Mapping, LandNode1, LandNode2, Land_Type), by = "Land_Type") %>%
      select(region, GLU, LandNode1, LandNode2) %>%
      distinct() %>%
      na.omit %>%
      # Match in logit exponents based on the land node 2
      mutate(LandAllocatorRoot = "root",
             logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      # logit.type is NA by default, so left_join
      left_join(select(A_LandNode_logit, logit.exponent, logit.type, LandNode), by = c("LandNode2" = "LandNode")) %>%
      append_GLU("LandNode1", "LandNode2") %>%
      select(LEVEL2_DATA_NAMES[["LN2_Logit"]], LOGIT_TYPE_COLNAME) ->
      L222.LN2_Logit


    # Unmanaged land tables
    #
    # These tables are formed from a master table, made by filtering and adding
    # node_leaf_names in L125.LC.
    L125.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type %in% A_LandLeaf_Unmgd2$UnmanagedLandLeaf,
             year %in% c(aglu.LAND_HISTORY_YEARS, MODEL_BASE_YEARS)) %>%
      mutate(allocation = round(value, aglu.DIGITS_LAND_USE)) %>%
      add_node_leaf_names(nesting_table = A_LandLeaf_Unmgd2, leaf_name = "UnmanagedLandLeaf",
                          LN1 = "LandNode1", LN2 = "LandNode2") ->
      L222.LC_bm2_R_Unmgd2_Yh_GLU


    # L222.LN2_HistUnmgdAllocation: Historical land cover, unmanaged land in the second nest
    # L222.LN2_UnmgdAllocation: Model base period land cover, unmanaged land in the second nest
    # Formed from filtering the master table by different years.
    L222.LC_bm2_R_Unmgd2_Yh_GLU %>%
      filter(year %in% aglu.LAND_HISTORY_YEARS) %>%
      select(LEVEL2_DATA_NAMES[["LN2_HistUnmgdAllocation"]],GCAM_region_ID,GLU) %>%
      left_join_error_no_match(basin_to_country_mapping %>% rename(GLU=GLU_name) %>%  select(GLU_code , GLU),by=c("GLU"))->
      L222.LN2_HistUnmgdAllocation

    L222.LC_bm2_R_Unmgd2_Yh_GLU %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(LEVEL2_DATA_NAMES[["LN2_UnmgdAllocation"]],GCAM_REGION_ID,GLU) %>%
      left_join_error_no_match(basin_to_country_mapping %>% rename(GLU=GLU_name) %>%  select(GLU_code , GLU),by=c("GLU"))->
      L222.LN2_UnmgdAllocation


    # Unmanaged, unprotected land allocation tables
    # L222.LN2_HistUnmgdAllocation_noprot: historical unmanaged land cover, no protect
    # L222.LN2_UnmgdAllocation_noprot: unmanaged land cover, no protect
    L222.LN2_HistUnmgdAllocation %>%
      left_join(L120.LC_prot_land_frac_GLU %>%  rename(GLU_code =GLU), by= c("GCAM_region_ID","GLU_code")) %>%
      mutate(prot_frac = if_else(is.na(prot_frac),aglu.PROTECT_DEFAULT,prot_frac),
             allocation = (1 - prot_frac) * allocation) %>%
      select(LEVEL2_DATA_NAMES[["LN2_HistUnmgdAllocation"]])->
      L222.LN2_HistUnmgdAllocation_noprot

    L222.LN2_UnmgdAllocation %>%
      left_join(L120.LC_prot_land_frac_GLU %>%  rename(GLU_code =GLU), by= c("GCAM_region_ID","GLU_code"))%>%
      mutate(prot_frac = if_else(is.na(prot_frac),aglu.PROTECT_DEFAULT,prot_frac),
             allocation = (1 - prot_frac) * allocation) %>%
      select(LEVEL2_DATA_NAMES[["LN2_UnmgdAllocation"]])-> L222.LN2_UnmgdAllocation_noprot



    # Unmanaged, protected land allocation and logit tables.
    # Protected land files have different names and a different nesting structure.
    #
    # L222.LN1_HistUnmgdAllocation_prot: historical unmanaged land cover, protected
    # L222.LN1_UnmgdAllocation_prot: unmanaged land cover, protected
    L222.LN2_HistUnmgdAllocation %>%
      left_join(L120.LC_prot_land_frac_GLU %>%  rename(GLU_code =GLU), by= c("GCAM_region_ID","GLU_code")) %>%
      mutate(prot_frac = if_else(is.na(prot_frac),aglu.PROTECT_DEFAULT,prot_frac),
             UnmanagedLandLeaf = paste0("Protected", UnmanagedLandLeaf),
             LandNode1 = UnmanagedLandLeaf,
             allocation = prot_frac * allocation) %>%
      select(LEVEL2_DATA_NAMES[["LN2_HistUnmgdAllocation"]]) %>%
      select(-LandNode2) ->
      L222.LN1_HistUnmgdAllocation_prot

    L222.LN2_UnmgdAllocation %>%
      left_join(L120.LC_prot_land_frac_GLU %>%  rename(GLU_code =GLU), by= c("GCAM_region_ID","GLU_code")) %>%
      mutate(prot_frac = if_else(is.na(prot_frac),aglu.PROTECT_DEFAULT,prot_frac),
             UnmanagedLandLeaf = paste0("Protected", UnmanagedLandLeaf),
             LandNode1 = UnmanagedLandLeaf,
             allocation = prot_frac * allocation) %>%
      select(LEVEL2_DATA_NAMES[["LN2_UnmgdAllocation"]]) %>%
      select(-LandNode2) ->
      L222.LN1_UnmgdAllocation_prot

    # L222.LN1_Logit_prot: Logit
    L222.LN1_UnmgdAllocation_prot %>%
      mutate(unManagedLandValue = aglu.UNMANAGED_LAND_VALUE,
             logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = aglu.LN1_PROTUNMGD_LOGIT_EXP,
             logit.type = aglu.LN1_PROTUNMGD_LOGIT_TYPE) %>%
      select(region, LandAllocatorRoot, LandNode1, unManagedLandValue, logit.year.fillout, logit.exponent, logit.type) ->
      L222.LN1_Logit_prot


    # L222.LN2_UnmgdCarbon: Carbon content info, unmanaged land in the second nest
    # Add carbon content info to the master  table to give the
    # carbon content infor for unmanaged land in the second nest.
    L222.LC_bm2_R_Unmgd2_Yh_GLU %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(-year, -allocation) %>%
      left_join_error_no_match(GCAMLandLeaf_CdensityLT, by = c("Land_Type" = "LandLeaf")) %>%
      rename(Cdensity_LT = Land_Type.y) %>%
      add_carbon_info(carbon_info_table = L121.CarbonContent_kgm2_R_LT_GLU) %>%
      select(LEVEL2_DATA_NAMES[["LN2_UnmgdCarbon"]]) ->
      L222.LN2_UnmgdCarbon

    # L222.LN1_UnmgdCarbon_prot: unmanaged carbon info, protected land in the first nest.
    # Manipulate unmanaged land carbon info to cover specifically protected land
    L222.LN2_UnmgdCarbon %>%
      mutate(UnmanagedLandLeaf = paste0("Protected", UnmanagedLandLeaf),
             LandNode1 = UnmanagedLandLeaf) %>%
      select(-LandNode2) ->
      L222.LN1_UnmgdCarbon_prot


    # Managed land allocation tables
    #
    # These tables are formed from a master table, made by filtering and adding
    # node_leaf_names in L125.LC.
    L125.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type %in% A_LandLeaf2$LandLeaf,
             year %in% c(aglu.LAND_HISTORY_YEARS, MODEL_BASE_YEARS)) %>%
      mutate(allocation = round(value, aglu.DIGITS_LAND_USE)) %>%
      add_node_leaf_names(nesting_table = A_LandLeaf2, leaf_name = "LandLeaf",
                          LN1 = "LandNode1", LN2 = "LandNode2") ->
      L222.LC_bm2_R_Mgd2_Yh_GLU


    # L222.LN2_HistMgdAllocation: Historical land cover, managed land in the second nest
    # L222.LN2_MgdAllocation: Model base period land cover, managed land in the second nest
    # Formed from filtering the master table by different years.
    L222.LC_bm2_R_Mgd2_Yh_GLU %>%
      filter(year %in% aglu.LAND_HISTORY_YEARS) %>%
      select(LEVEL2_DATA_NAMES[["LN2_HistMgdAllocation"]]) ->
      L222.LN2_HistMgdAllocation

    L222.LC_bm2_R_Mgd2_Yh_GLU %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(LEVEL2_DATA_NAMES[["LN2_MgdAllocation"]]) ->
      L222.LN2_MgdAllocation


    # L222.LN2_MgdCarbon: Carbon content info, managed land in the second nest
    # Add carbon content info to the master  table to give the
    # carbon content infor for managed land in the second nest.
    L222.LC_bm2_R_Mgd2_Yh_GLU %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(-year, -allocation) %>%
      left_join_error_no_match(GCAMLandLeaf_CdensityLT, by = c("Land_Type" = "LandLeaf")) %>%
      rename(Cdensity_LT = Land_Type.y) %>%
      add_carbon_info(carbon_info_table = L121.CarbonContent_kgm2_R_LT_GLU) %>%
      reduce_mgd_carbon() %>%
      select(LEVEL2_DATA_NAMES[["LN2_MgdCarbon"]]) ->
      L222.LN2_MgdCarbon

    L222.LN2_HistUnmgdAllocation %>%
      select(LEVEL2_DATA_NAMES[["LN2_HistUnmgdAllocation"]]) -> L222.LN2_HistUnmgdAllocation

    L222.LN2_UnmgdAllocation %>%
      select(LEVEL2_DATA_NAMES[["LN2_UnmgdAllocation"]]) -> L222.LN2_UnmgdAllocation

    # 3. Produce outputs

    L222.LN2_Logit %>%
      add_title("Logit exponent of the second land nest by region") %>%
      add_units("NA") %>%
      add_comments("Logit exponent of the second land nest by region. AgLU regions") %>%
      add_comments("are given externally defined constant logit information.") %>%
      add_legacy_name("L222.LN2_Logit") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LT_Mapping",
                     "L125.LC_bm2_R_LT_Yh_GLU") ->
      L222.LN2_Logit

    L222.LN2_HistUnmgdAllocation %>%
      add_title("Historical land cover for unmanaged land (LT_GLU) in the second nest by region.") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Historical land cover for unmanaged land (LT_GLU) in the second nest, from L125 land cover data.") %>%
      add_legacy_name("L222.LN2_HistUnmgdAllocation") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_LandLeaf_Unmgd2",
                     "L125.LC_bm2_R_LT_Yh_GLU") ->
      L222.LN2_HistUnmgdAllocation

    L222.LN2_UnmgdAllocation %>%
      add_title("Land cover in the model base periods for unmanaged land (LT_GLU) in the second nest by region") %>%
      add_units("billion square meters (bm2) ") %>%
      add_comments("Land cover in the model base periods for unmanaged land (LT_GLU) in the second nest, from L125 land cover data.") %>%
      add_legacy_name("L222.LN2_UnmgdAllocation") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_LandLeaf_Unmgd2",
                     "L125.LC_bm2_R_LT_Yh_GLU") ->
      L222.LN2_UnmgdAllocation

    L222.LN2_HistMgdAllocation %>%
      add_title("Historical land cover for managed land (LT_GLU) in the second nest by region.") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Historical land cover for managed land (LT_GLU) in the second nest, from L125 land cover data.") %>%
      add_legacy_name("L222.LN2_HistMgdAllocation") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_LandLeaf2",
                     "L125.LC_bm2_R_LT_Yh_GLU") ->
      L222.LN2_HistMgdAllocation

    L222.LN2_MgdAllocation %>%
      add_title("Land cover in the model base periods for managed land (LT_GLU) in the second nest by region") %>%
      add_units("billion square meters (bm2) ") %>%
      add_comments("Land cover in the model base periods for managed land (LT_GLU) in the second nest, from L125 land cover data.") %>%
      add_legacy_name("L222.LN2_MgdAllocation") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_LandLeaf2",
                     "L125.LC_bm2_R_LT_Yh_GLU") ->
      L222.LN2_MgdAllocation

    L222.LN2_UnmgdCarbon %>%
      add_title("Carbon content for unmanaged land (LT_GLU) in second nest by region.") %>%
      add_units("Varies") %>%
      add_comments("Carbon content info for unmanaged land (LT_GLU) in the second nest including soil and vegetative carbon, ") %>%
      add_comments("from L125 land cover data, L121 carbon content data, and GCAMLandLeaf_CdensityLT assumptions") %>%
      add_legacy_name("L222.LN2_UnmgdCarbon") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandLeaf_Unmgd2",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L120.LC_soil_veg_carbon_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU") ->
      L222.LN2_UnmgdCarbon

    L222.LN2_MgdCarbon %>%
      add_title("Carbon content for managed land (LT_GLU) in second nest by region.") %>%
      add_units("Varies") %>%
      add_comments("Carbon content info for managed land (LT_GLU) in the second nest including soil and vegetative carbon, ") %>%
      add_comments("from L125 land cover data, L121 carbon content data, and GCAMLandLeaf_CdensityLT assumptions.") %>%
      add_legacy_name("L222.LN2_MgdCarbon") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandLeaf2",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L120.LC_soil_veg_carbon_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU") ->
      L222.LN2_MgdCarbon

    L222.LN2_HistUnmgdAllocation_noprot %>%
      add_title("Historical land cover for unprotected unmanaged land (LT_GLU) in the second nest by region") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Historical land cover for unprotected unmanaged land (LT_GLU) in the second nest, from L222.LN2_HistUnmgdAllocation.") %>%
      add_legacy_name("L222.LN2_HistUnmgdAllocation_noprot") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_LandLeaf_Unmgd2",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L120.LC_prot_land_frac_GLU") ->
      L222.LN2_HistUnmgdAllocation_noprot

    L222.LN2_UnmgdAllocation_noprot %>%
      add_title("Land cover in the model base periods for unprotected unmanaged land (LT_GLU) in the second nest by region.") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Land cover in the model base periods for unprotected unmanaged land (LT_GLU) in the second nest, from L222.LN2_UnmgdAllocation.") %>%
      add_legacy_name("L222.LN2_UnmgdAllocation_noprot") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_LandLeaf_Unmgd2",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L120.LC_prot_land_frac_GLU") ->
      L222.LN2_UnmgdAllocation_noprot

    L222.LN1_Logit_prot %>%
      add_title("Logit exponent of protected land in the first nest by region") %>%
      add_units("NA") %>%
      add_comments("Logit exponent of protected land in the first nest by region. AgLU regions ") %>%
      add_comments("are given externally defined constant logit information. From L222.LN1_UnmgdAllocation_prot") %>%
      add_legacy_name("L222.LN1_Logit_prot") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_LandLeaf_Unmgd2",
                     "L125.LC_bm2_R_LT_Yh_GLU") ->
      L222.LN1_Logit_prot

    L222.LN1_HistUnmgdAllocation_prot %>%
      add_title("Historical land cover for protected unmanaged land in the first nest by region") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Historical land cover for protected unmanaged land (LT_GLU) in the first nest, from L222.LN2_HistUnmgdAllocation.") %>%
      add_legacy_name("L222.LN1_HistUnmgdAllocation_prot") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_LandLeaf_Unmgd2",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L120.LC_prot_land_frac_GLU") ->
      L222.LN1_HistUnmgdAllocation_prot

    L222.LN1_UnmgdAllocation_prot %>%
      add_title("Land cover in the model base periods for protected unmanaged land in the first nest by region.") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Land cover in the model base periods for protected unmanaged land (LT_GLU)  in the first nest, from L222.LN2_UnmgdAllocation.") %>%
      add_legacy_name("L222.LN1_UnmgdAllocation_prot") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_LandLeaf_Unmgd2",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L120.LC_prot_land_frac_GLU") ->
      L222.LN1_UnmgdAllocation_prot

    L222.LN1_UnmgdCarbon_prot %>%
      add_title("Carbon content for protected unmanaged land in first nest by region.") %>%
      add_units("Varies") %>%
      add_comments("Carbon content info for protected unmanaged land (LT_GLU)  in the first nest including soil and vegetative carbon, from L222.LN2_UnmgdCarbon.") %>%
      add_legacy_name("L222.LN1_UnmgdCarbon_prot") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandLeaf_Unmgd2",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L120.LC_soil_veg_carbon_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU") ->
      L222.LN1_UnmgdCarbon_prot

    return_data(L222.LN2_Logit, L222.LN2_HistUnmgdAllocation, L222.LN2_UnmgdAllocation,
                L222.LN2_HistMgdAllocation, L222.LN2_MgdAllocation, L222.LN2_UnmgdCarbon, L222.LN2_MgdCarbon,
                L222.LN2_HistUnmgdAllocation_noprot, L222.LN2_UnmgdAllocation_noprot, L222.LN1_Logit_prot,
                L222.LN1_HistUnmgdAllocation_prot, L222.LN1_UnmgdAllocation_prot, L222.LN1_UnmgdCarbon_prot)
  } else {
    stop("Unknown command")
  }
}
