#' module_aglu_L2231.land_input_3_irr
#'
#' Produce L2231.LN3_Logit, L2231.LN3_HistUnmgdAllocation, L2231.LN3_UnmgdAllocation,
#' L2231.LN3_HistMgdAllocation_noncrop, L2231.LN3_MgdAllocation_noncrop, L2231.LN3_UnmgdCarbon,
#' L2231.LN3_MgdCarbon_noncrop, L2231.NodeEquiv, L2231.LN3_NoEmissCarbon, L2231.LN3_NodeCarbon, and
#' protected lands related outputs: L2231.LN3_HistUnmgdAllocation_noprot, L2231.LN3_UnmgdAllocation_noprot,
#' L2231.LN1_HistUnmgdAllocation_prot, L2231.LN1_UnmgdAllocation_prot, L2231.LN1_UnmgdCarbon_prot,
#' L2231.LN1_Logit_prot.
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2231.LN3_LogitTables[[ curr_table_name ]]}, \code{L2231.LN3_Logit}, \code{L2231.LN3_HistUnmgdAllocation}, \code{L2231.LN3_UnmgdAllocation}, \code{L2231.NodeEquiv}, \code{L2231.LN3_NoEmissCarbon}, \code{L2231.LN3_NodeCarbon}, \code{L2231.LN3_HistMgdAllocation_noncrop}, \code{L2231.LN3_MgdAllocation_noncrop}, \code{L2231.LN3_UnmgdCarbon}, \code{L2231.LN3_MgdCarbon_noncrop}. The corresponding file in the
#' original data system was \code{L2231.land_input_3_irr.R} (aglu level2).
#' @details
#' \itemize{
#' \item{"L2231.LN3_Logit: Logit exponent of the third land nest by region.
#' AgLU regions are given externally defined constant logit information."}
#' \item{"L2231.LN3_HistUnmgdAllocation: Historical land cover for unmanaged land (LT_GLU) in the third nest by region.
#' Historical land cover for unmanaged land in the third nest, from L125 land cover data."}
#' \item{"L2231.LN3_UnmgdAllocation: Land cover in the model base periods for unmanaged land (LT_GLU) in the third nest by region.
#' Land cover in the model base periods for unmanaged land in the third nest, from L125 land cover data."}
#' \item{"L2231.LN3_HistMgdAllocation_noncrop: Historical land cover for non-crop managed land (LT_GLU) in the third nest by region.
#' Historical land cover for non-crop managed land in the third nest, from L125 land cover data."}
#' \item{"L2231.LN3_MgdAllocation_noncrop,: Land cover in the model base periods for non-crop managed land (LT_GLU) in the third nest by region.
#' Land cover in the model base periods for non-crop managed land in the third nest, from L125 land cover data."}
#' \item{"L2231.LN3_UnmgdCarbon: Carbon content for unmanaged land (LT_GLU) in third nest by region.
#' Carbon content info for unmanaged land in the third nest including soil and vegetative carbon,
#' from L125 land cover data, L121 carbon content data, and GCAMLandLeaf_CdensityLT assumptions."}
#' \item{"L2231.LN3_MgdCarbon_noncrop: Carbon content for non-crop managed land (LT_GLU) in third nest by region.
#' Carbon content info for non-crop managed land in the third nest including soil and vegetative carbon,
#' from L125 land cover data, L121 carbon content data, and GCAMLandLeaf_CdensityLT assumptions."}
#' \item{"L2231.NodeEquiv: Node tag equivalence list to minimize extra tables to read in same parameters;
#' manually formed with no inputs."}
#' \item{"L2231.LN3_NoEmissCarbon: Sets the no-emiss-carbon-calc as the type of carbon to use in forest leaves by region."}
#' \item{"L2231.LN3_NodeCarbon: Sets the node-carbon-calc to drive the carbon calc between forest leaves, by region, and
#' places the node carbon calc in the node just above the leaves."}
#' \item{"L2231.LN3_HistUnmgdAllocation_noprot: Historical unmanaged land data from L223.LN3_HistUnmgdAllocation is multiplied
#' by specified fraction to give unprotected land allocation in the third nest. OtherArableLands omitted."}
#' \item{"L2231.LN3_UnmgdAllocation_noprot: Unmanaged land data from L223.LN3_UnmgdAllocation is multiplied by specified
#' fraction to give "unprotected land allocation in the third nest. OtherArableLands omitted."}
#' \item{"L2231.LN1_HistUnmgdAllocation_prot: Historical unmanaged land data from L223.LN3_HistUnmgdAllocation is multiplied by
#' specified fraction to give protected land allocation in the first nest. OtherArableLands omitted."}
#' \item{"L2231.LN1_UnmgdAllocation_prot: Unmanaged land data from L223.LN3_UnmgdAllocation is multiplied by specified fraction
#' to give protected land allocation in the first nest. OtherArableLands omitted."}
#' \item{" L2231.LN1_UnmgdCarbon_prot: Carbon content info for protected unmanaged land (LT_GLU) in the first nest including
#' soil and vegetative carbon."}
#' \item{"L2231.LN1_Logit_prot: Logit info for protected lands in the first land nest by region"}
#' }
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author ACS September 2017
module_aglu_L2231.land_input_3_irr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "aglu/GCAMLandLeaf_CdensityLT",
             FILE = "aglu/A_LT_Mapping",
             FILE = "aglu/A_LandNode_logit",
             FILE = "aglu/A_LandLeaf_Unmgd3",
             FILE = "aglu/A_LandLeaf3",
             "L121.CarbonContent_kgm2_R_LT_GLU",
             "L125.LC_bm2_R_LT_Yh_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2231.LN3_Logit",
             "L2231.LN3_HistUnmgdAllocation",
             "L2231.LN3_UnmgdAllocation",
             "L2231.NodeEquiv",
             "L2231.LN3_NoEmissCarbon",
             "L2231.LN3_NodeCarbon",
             "L2231.LN3_HistMgdAllocation_noncrop",
             "L2231.LN3_MgdAllocation_noncrop",
             "L2231.LN3_UnmgdCarbon",
             "L2231.LN3_MgdCarbon_noncrop",
             # Protected land related outputs:
             "L2231.LN3_HistUnmgdAllocation_noprot",
             "L2231.LN3_UnmgdAllocation_noprot",
             "L2231.LN1_HistUnmgdAllocation_prot",
             "L2231.LN1_UnmgdAllocation_prot",
             "L2231.LN1_UnmgdCarbon_prot",
             "L2231.LN1_Logit_prot"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    GCAMLandLeaf_CdensityLT <- get_data(all_data, "aglu/GCAMLandLeaf_CdensityLT")
    A_LT_Mapping <- get_data(all_data, "aglu/A_LT_Mapping")
    A_LandNode_logit <- get_data(all_data, "aglu/A_LandNode_logit")
    A_LandLeaf_Unmgd3 <- get_data(all_data, "aglu/A_LandLeaf_Unmgd3")
    A_LandLeaf3 <- get_data(all_data, "aglu/A_LandLeaf3")
    L121.CarbonContent_kgm2_R_LT_GLU <- get_data(all_data, "L121.CarbonContent_kgm2_R_LT_GLU")
    L125.LC_bm2_R_LT_Yh_GLU <- get_data(all_data, "L125.LC_bm2_R_LT_Yh_GLU")

    # silence package check notes
    GCAM_commodity <- GCAM_region_ID <- region <- value <- year <- GLU <- GLU_name <- GLU_code <-
      LandLeaf <- Land_Type <- LandNode <- LandNode1 <- LandNode2 <- LandNode3 <- UnmanagedLandLeaf <-
      logit.year.fillout <- logit.exponent <- logit.type <- soilTimeScale <- `mature age` <- mature.age <-
      soil_c <- veg_c <- LC_bm2 <- LV_milUSD75 <- LV_USD75_bm2 <- LV_USD75_m2 <- HarvCropLand_bm2 <-
      unManagedLandValue <- LandAllocatorRoot <- hist.veg.carbon.density <- hist.soil.carbon.density <-
      veg.carbon.density <- soil.carbon.density <- allocation <- Land_Type.y <- mature.age.year.fillout <-
      min.veg.carbon.density <- min.soil.carbon.density <- no.emiss.carbon.calc <- . <- NULL


    # Process inputs
    # Replace GLU names and Add region names as needed
    L121.CarbonContent_kgm2_R_LT_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) %>%
      rename(mature.age = `mature age`) ->
      L121.CarbonContent_kgm2_R_LT_GLU

    L125.LC_bm2_R_LT_Yh_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) ->
      L125.LC_bm2_R_LT_Yh_GLU


    # Build tables

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
      select(LEVEL2_DATA_NAMES[["LN3_Logit"]], "logit.type") ->
      L223.LN3_Logit


    # Land Use History

    # Unmanaged land tables
    #
    # These tables are formed from a master table, made by filtering and adding
    # node_leaf_names in L125.LC.
    L125.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type %in% A_LandLeaf_Unmgd3$UnmanagedLandLeaf,
             year %in% c(aglu.LAND_HISTORY_YEARS, BASE_YEARS)) %>%
      mutate(allocation = round(value, aglu.DIGITS_LAND_USE)) %>%
      add_node_leaf_names(nesting_table = A_LandLeaf_Unmgd3, leaf_name = "UnmanagedLandLeaf",
                          LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3") ->
      L223.LC_bm2_R_Unmgd3_Yh_GLU

    # L223.LN3_HistUnmgdAllocation: Historical land cover, unmanaged land in the third nest
    # L223.LN3_UnmgdAllocation: Model base period land cover, unmanaged land in the third nest
    # Formed from filtering the master table by different years.
    L223.LC_bm2_R_Unmgd3_Yh_GLU %>%
      filter(year %in% aglu.LAND_HISTORY_YEARS) %>%
      select(LEVEL2_DATA_NAMES[["LN3_HistUnmgdAllocation"]]) ->
      L223.LN3_HistUnmgdAllocation

    L223.LC_bm2_R_Unmgd3_Yh_GLU %>%
      filter(year %in% BASE_YEARS) %>%
      select(LEVEL2_DATA_NAMES[["LN3_UnmgdAllocation"]]) ->
      L223.LN3_UnmgdAllocation


    # Managed land - non-crop (forest)
    # This code will cover any land types we wish to make a `LandLeaf` at level 3 in the future.
    #
    # These tables are formed from a master table, made by filtering and adding
    # node_leaf_names in L125.LC.
    L125.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type %in% A_LandLeaf3$LandLeaf,
             year %in% c(aglu.LAND_HISTORY_YEARS, BASE_YEARS)) %>%
      mutate(allocation = round(value, aglu.DIGITS_LAND_USE)) %>%
      add_node_leaf_names(nesting_table = A_LandLeaf3, leaf_name = "LandLeaf",
                          LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3") ->
      L223.LC_bm2_R_Mgd3_Yh_GLU

    # L223.LN3_HistMgdAllocation_noncrop: Historical land cover, managed land in the third nest (noncrop)
    # L223.LN3_MgdAllocation_noncrop: Model base period land cover, managed land in the third nest (noncrop)
    # Formed from filtering the master table by different years.
    L223.LC_bm2_R_Mgd3_Yh_GLU %>%
      filter(year %in% aglu.LAND_HISTORY_YEARS) %>%
      select(LEVEL2_DATA_NAMES[["LN3_HistMgdAllocation"]]) ->
      L223.LN3_HistMgdAllocation_noncrop

    L223.LC_bm2_R_Mgd3_Yh_GLU %>%
      filter(year %in% BASE_YEARS) %>%
      select(LEVEL2_DATA_NAMES[["LN3_MgdAllocation"]]) ->
      L223.LN3_MgdAllocation_noncrop


    # L223.LN3_UnmgdCarbon: Carbon content info, unmanaged land in the third nest
    L223.LC_bm2_R_Unmgd3_Yh_GLU %>%
      filter(year == max(BASE_YEARS)) %>%
      left_join_error_no_match(select(GCAMLandLeaf_CdensityLT, Land_Type, LandLeaf), by = c("Land_Type" = "LandLeaf")) %>%
      rename(Cdensity_LT = Land_Type.y) %>%
      add_carbon_info(carbon_info_table = L121.CarbonContent_kgm2_R_LT_GLU) %>%
      select(LEVEL2_DATA_NAMES[["LN3_UnmgdCarbon"]]) ->
      L223.LN3_UnmgdCarbon

    # If any regions are zero in all periods, they will return missing values here. The specific value doesn't matter because
    # these land use types will be zero in all periods, but there is an assert in the code prohibiting mature age from being zero.
    L223.LN3_UnmgdCarbon[is.na(L223.LN3_UnmgdCarbon)] <- 1

    # L223.LN3_MgdCarbon_noncrop: Carbon content info, managed land in the third nest, non-crop (forest)
    L223.LC_bm2_R_Mgd3_Yh_GLU %>%
      filter(year == max(BASE_YEARS)) %>%
      left_join_error_no_match(select(GCAMLandLeaf_CdensityLT, Land_Type, LandLeaf), by = c("Land_Type" = "LandLeaf")) %>%
      rename(Cdensity_LT = Land_Type.y) %>%
      add_carbon_info(carbon_info_table = L121.CarbonContent_kgm2_R_LT_GLU) %>%
      select(LEVEL2_DATA_NAMES[["LN3_MgdCarbon"]]) ->
      L223.LN3_MgdCarbon_noncrop

    # L223.LN3_NoEmissCarbon: Set the no-emiss-carbon-calc as the type of carbon to use in forest leaves.
    # Set Forests to use node-carbon-calc at the node level and no-emiss-carbon-calc at the leaf
    # to allow them to switch between each other without lots of emissions+uptake.
    L223.LN3_UnmgdCarbon %>%
      filter(grepl("Forest", UnmanagedLandLeaf)) %>%
      select(region, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, UnmanagedLandLeaf) %>%
      rename(LandLeaf = UnmanagedLandLeaf) ->
      L223.LN3_NoEmissCarbon

    L223.LN3_MgdCarbon_noncrop %>%
      select(region, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, LandLeaf) %>%
      bind_rows(L223.LN3_NoEmissCarbon, .) %>%
      # Need to create an empty tag, to do so we have to have a whitespace column and an
      # extra column with extraneous data
      mutate(no.emiss.carbon.calc = " ",
             extra = "junk") ->
      L223.LN3_NoEmissCarbon

    # L223.LN3_NodeCarbon: Set the node-carbon-calc to drive the carbon calc between forest leaves
    L223.LN3_NoEmissCarbon %>%
      select(-LandLeaf) %>%
      distinct() %>%
      rename(node.carbon.calc = no.emiss.carbon.calc) ->
      L223.LN3_NodeCarbon

    # L223.NodeEquiv: Node tag equivalence list to minimize extra tables to read in same params
    # TODO: better place for these?  they are related to headers since they list tag names
    tibble(group.name = c("Leaf", "CarbonCalc"),
           tag1 = c("LandLeaf", "land-carbon-densities"),
           tag2 = c("UnmanagedLandLeaf", "no-emiss-carbon-calc")) ->
      L223.NodeEquiv


    # UNPROTECTED LANDS in the third nest
    # Note that OtherArableLand is specifically NOT adjusted.

    # function to process unprotected lands
    # remove OtherArableLand types from UnmanageLandLeaf and adjust allocation
    # grepl is used in filtering out OtherArableLand from UnmanagedLandLeaf
    # because entries in UnmanagedLandLeaf are formatted as landtype_basin.
    create_noprot_unmgd <- function(data){
      data %>%
        filter(!grepl("OtherArable", UnmanagedLandLeaf)) %>%
        mutate(allocation = (1 - aglu.PROTECT_LAND_FRACT) * allocation)
    } # end create_noprot


    # L223.LN3_HistUnmgdAllocation_noprot: historical unmanaged land cover, no protect
    L223.LN3_HistUnmgdAllocation_noprot <- create_noprot_unmgd(L223.LN3_HistUnmgdAllocation)


    # L223.LN3_UnmgdAllocation_noprot: unmanaged land cover, no protect
    L223.LN3_UnmgdAllocation_noprot <- create_noprot_unmgd(L223.LN3_UnmgdAllocation)


    # PROTECTED LANDS in the first nest
    # modified land allocations, different names, different nesting structure

    # function to process protected lands
    create_prot_unmgd <- function(data){
      data %>%
        filter(!grepl("OtherArable", UnmanagedLandLeaf)) %>%
        mutate(UnmanagedLandLeaf = paste0("Protected", UnmanagedLandLeaf),
               LandNode1 = UnmanagedLandLeaf,
               LandNode2 = NULL,
               LandNode3 = NULL,
               allocation = aglu.PROTECT_LAND_FRACT * allocation)
    }


    # L223.LN1_HistUnmgdAllocation_prot: historical unmanaged land cover, protected
    L223.LN1_HistUnmgdAllocation_prot <- create_prot_unmgd(L223.LN3_HistUnmgdAllocation)


    # L223.LN1_UnmgdAllocation_prot: unmanaged land cover, protected
    L223.LN1_UnmgdAllocation_prot <- create_prot_unmgd(L223.LN3_UnmgdAllocation)


    # L223.LN1_UnmgdCarbon_prot: unmanaged carbon info, protected
    L223.LN3_UnmgdCarbon %>%
      filter(!grepl("OtherArable", UnmanagedLandLeaf)) %>%
      mutate(UnmanagedLandLeaf = paste0("Protected", UnmanagedLandLeaf),
             LandNode1 = UnmanagedLandLeaf,
             LandNode2 = NULL,
             LandNode3 = NULL) ->
      L223.LN1_UnmgdCarbon_prot


    # L223.LN1_Logit_prot: Logit for protected land in the first nest.
    L223.LN1_UnmgdAllocation_prot %>%
      mutate(unManagedLandValue = aglu.UNMANAGED_LAND_VALUE,
             logit.year.fillout = min(BASE_YEARS),
             logit.exponent = aglu.LN1_PROTUNMGD_LOGIT_EXP,
             logit.type = aglu.LN1_PROTUNMGD_LOGIT_TYPE) %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["LN1_ValueLogit"]], "logit.type"))) ->
      L223.LN1_Logit_prot


    # Produce outputs
    L223.LN3_Logit %>%
      add_title("Logit exponent of the third land nest by region") %>%
      add_units("NA") %>%
      add_comments("Logit exponent of the third land nest by region. AgLU regions") %>%
      add_comments("are given externally defined constant logit information.") %>%
      add_legacy_name("L2231.LN3_Logit") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LT_Mapping",
                     "L125.LC_bm2_R_LT_Yh_GLU") ->
      L2231.LN3_Logit

    L223.LN3_HistUnmgdAllocation %>%
      add_title("Historical land cover for unmanaged land (LT_GLU) in the third nest by region.") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Historical land cover for unmanaged land (LT_GLU) in the third nest, from L125 land cover data.") %>%
      add_legacy_name("L2231.LN3_HistUnmgdAllocation") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_LandLeaf_Unmgd3",
                     "L125.LC_bm2_R_LT_Yh_GLU")  ->
      L2231.LN3_HistUnmgdAllocation

    L223.LN3_UnmgdAllocation %>%
      add_title("Land cover in the model base periods for unmanaged land (LT_GLU) in the third nest by region") %>%
      add_units("billion square meters (bm2) ") %>%
      add_comments("Land cover in the model base periods for unmanaged land (LT_GLU) in the third nest, from L125 land cover data.") %>%
      add_legacy_name("L2231.LN3_UnmgdAllocation") %>%
      same_precursors_as("L2231.LN3_HistUnmgdAllocation") ->
      L2231.LN3_UnmgdAllocation

    L223.NodeEquiv %>%
      add_title("Node tag equivalence list to minimize extra tables to read in same params") %>%
      add_units("NA") %>%
      add_comments("Manually formed with no inputs") %>%
      add_legacy_name("L2231.NodeEquiv") ->
      L2231.NodeEquiv

    L223.LN3_NoEmissCarbon %>%
      add_title("Sets the no-emiss-carbon-calc as the type of carbon to use in forest leaves") %>%
      add_units("NA") %>%
      add_comments("Sets the no-emiss-carbon-calc as the type of carbon to use in forest leaves, by region.") %>%
      add_legacy_name("L2231.LN3_NoEmissCarbon") %>%
      same_precursors_as("L2231.LN3_UnmgdCarbon") ->
      L2231.LN3_NoEmissCarbon

    L223.LN3_NodeCarbon %>%
      add_title("Sets the node-carbon-calc to drive the carbon calc between forest leaves") %>%
      add_units("NA") %>%
      add_comments("Sets the node-carbon-calc to drive the carbon calc between forest leaves, by region,") %>%
      add_comments("and places the node carbon calc in the node just above the leaves.") %>%
      add_legacy_name("L2231.LN3_NodeCarbon") %>%
      same_precursors_as("L2231.LN3_NoEmissCarbon")->
      L2231.LN3_NodeCarbon

    L223.LN3_HistMgdAllocation_noncrop %>%
      add_title("Historical land cover for non-crop (forest) managed land (LT_GLU) in the third nest by region.") %>%
      add_units("billion square meters (bm2)") %>%
      add_comments("Historical land cover for non-crop (forest) managed land (LT_GLU) in the third nest, from L125 land cover data.") %>%
      add_legacy_name("L2231.LN3_HistMgdAllocation_noncrop") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/A_LandLeaf3",
                     "L125.LC_bm2_R_LT_Yh_GLU") ->
      L2231.LN3_HistMgdAllocation_noncrop

    L223.LN3_MgdAllocation_noncrop %>%
      add_title("Land cover in the model base periods for non-crop (forest) managed land (LT_GLU) in the third nest by region") %>%
      add_units("billion square meters (bm2) ") %>%
      add_comments("Land cover in the model base periods for non-crop (forest) managed land (LT_GLU) in the third nest, from L125 land cover data.") %>%
      add_legacy_name("L2231.LN3_MgdAllocation_noncrop") %>%
      same_precursors_as("L2231.LN3_HistMgdAllocation_noncrop") ->
      L2231.LN3_MgdAllocation_noncrop

    L223.LN3_UnmgdCarbon %>%
      add_title("Carbon content for unmanaged land (LT_GLU) in third nest by region.") %>%
      add_units("Varies") %>%
      add_comments("Carbon content info for unmanaged land (LT_GLU) in the third nest including soil and vegetative carbon,") %>%
      add_comments("from L125 land cover data, L121 carbon content data, and GCAMLandLeaf_CdensityLT assumptions") %>%
      add_legacy_name("L2231.LN3_UnmgdCarbon") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandLeaf_Unmgd3",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU") ->
      L2231.LN3_UnmgdCarbon

    L223.LN3_MgdCarbon_noncrop %>%
      add_title("Carbon content for non-crop (forest) managed land (LT_GLU) in third nest by region.") %>%
      add_units("Varies") %>%
      add_comments("Carbon content info for non-crop (forest) managed land (LT_GLU) in the third nest including soil and vegetative carbon,") %>%
      add_comments("from L125 land cover data, L121 carbon content data, and GCAMLandLeaf_CdensityLT assumptions.") %>%
      add_legacy_name("L2231.LN3_MgdCarbon_noncrop") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandLeaf3",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU") ->
      L2231.LN3_MgdCarbon_noncrop


    # Protected lands related outputs:
    L223.LN3_HistUnmgdAllocation_noprot %>%
      add_title("Unprotected historical unmanged lands in the third nest by basin.") %>%
      add_units("thou km2") %>%
      add_comments("Historical unmanaged land data from L223.LN3_HistUnmgdAllocation is multiplied by ") %>%
      add_comments("specified fraction to give unprotected land allocation in the third nest. OtherArableLands omitted.") %>%
      add_legacy_name("L223.LN3_HistUnmgdAllocation_noprot") %>%
      same_precursors_as(L223.LN3_HistUnmgdAllocation) %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L2231.LN3_HistUnmgdAllocation_noprot

    L223.LN3_UnmgdAllocation_noprot %>%
      add_title("Unprotected unmanged lands in the third nest by basin.") %>%
      add_units("thou km2") %>%
      add_comments("Unmanaged land data from L223.LN3_UnmgdAllocation is multiplied by specified fraction to give ") %>%
      add_comments("unprotected land allocation in the third nest. OtherArableLands omitted.") %>%
      add_legacy_name("L223.LN3_UnmgdAllocation_noprot") %>%
      same_precursors_as(L223.LN3_UnmgdAllocation) %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L2231.LN3_UnmgdAllocation_noprot

    L223.LN1_HistUnmgdAllocation_prot %>%
      add_title("Protected historical unmanged lands in the first nest by basin.") %>%
      add_units("thou km2") %>%
      add_comments("Historical unmanaged land data from L223.LN3_HistUnmgdAllocation is multiplied by ") %>%
      add_comments("specified fraction to give protected land allocation in the first nest. OtherArableLands omitted.") %>%
      add_legacy_name("L223.LN1_HistUnmgdAllocation_prot") %>%
      same_precursors_as(L223.LN3_HistUnmgdAllocation) %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L2231.LN1_HistUnmgdAllocation_prot

    L223.LN1_UnmgdAllocation_prot %>%
      add_title("Protected unmanged lands in the first nest by basin.") %>%
      add_units("thou km2") %>%
      add_comments("Unmanaged land data from L223.LN3_UnmgdAllocation is multiplied by specified fraction to give ") %>%
      add_comments("protected land allocation in the first nest. OtherArableLands omitted.") %>%
      add_legacy_name("L223.LN1_UnmgdAllocation_prot") %>%
      same_precursors_as(L223.LN3_UnmgdAllocation) %>%
      add_flags(FLAG_PROTECT_FLOAT) ->
      L2231.LN1_UnmgdAllocation_prot

    L223.LN1_UnmgdCarbon_prot %>%
      add_title("Carbon content for protected unmanaged land (LT_GLU) in first nest by region.") %>%
      add_units("Varies") %>%
      add_comments("Carbon content info for protected unmanaged land (LT_GLU) in the first nest including soil and vegetative carbon.") %>%
      add_legacy_name("L223.LN1_UnmgdCarbon_prot") %>%
      same_precursors_as(L223.LN3_UnmgdCarbon) ->
      L2231.LN1_UnmgdCarbon_prot

    L223.LN1_Logit_prot %>%
      add_title("Logit info for protected lands in the first land nest by region") %>%
      add_units("NA") %>%
      add_comments("Logit exponent of the first land nest by region. AgLU regions") %>%
      add_comments("are given externally defined constant logit information.") %>%
      add_legacy_name("L223.LN1_Logit_prot") %>%
      same_precursors_as(L2231.LN1_UnmgdAllocation_prot) ->
      L2231.LN1_Logit_prot


    return_data(L2231.LN3_Logit, L2231.LN3_HistUnmgdAllocation, L2231.LN3_UnmgdAllocation, L2231.NodeEquiv, L2231.LN3_NoEmissCarbon, L2231.LN3_NodeCarbon, L2231.LN3_HistMgdAllocation_noncrop, L2231.LN3_MgdAllocation_noncrop, L2231.LN3_UnmgdCarbon, L2231.LN3_MgdCarbon_noncrop,
                # protected land outputs:
                L2231.LN3_HistUnmgdAllocation_noprot, L2231.LN3_UnmgdAllocation_noprot, L2231.LN1_HistUnmgdAllocation_prot, L2231.LN1_UnmgdAllocation_prot, L2231.LN1_UnmgdCarbon_prot, L2231.LN1_Logit_prot)
  } else {
    stop("Unknown command")
  }
}
