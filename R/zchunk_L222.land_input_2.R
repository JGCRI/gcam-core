#' module_aglu_L222.land_input_2
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{curr_table$data}, \code{L222.LN2_Logit}, \code{L222.LN2_HistUnmgdAllocation}, \code{L222.LN2_UnmgdAllocation}, \code{L222.LN2_HistMgdAllocation}, \code{L222.LN2_MgdAllocation}, \code{L222.LN2_UnmgdCarbon}, \code{L222.LN2_MgdCarbon}, \code{L222.LN2_HistUnmgdAllocation_noprot}, \code{L222.LN2_UnmgdAllocation_noprot}, \code{L222.LN2_UnmgdCarbon}, \code{curr_table$data}, \code{L222.LN1_Logit_prot}, \code{L222.LN1_HistUnmgdAllocation_prot}, \code{L222.LN1_UnmgdAllocation_prot}, \code{L222.LN1_UnmgdCarbon_prot}. The corresponding file in the
#' original data system was \code{L222.land_input_2.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
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
             "L125.LC_bm2_R_LT_Yh_GLU"))
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
             #"L222.LN2_UnmgdCarbon",
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
    L125.LC_bm2_R_LT_Yh_GLU <- get_data(all_data, "L125.LC_bm2_R_LT_Yh_GLU")

    # silence package check notes
    GCAM_commodity <- GCAM_region_ID <- region <- value <- year <- GLU <- GLU_name <- GLU_code <-
      LandLeaf <- Land_Type <- LandNode <- LandNode1 <- LandNode2 <- LandNode3 <- UnmanagedLandLeaf <-
      logit.year.fillout <- logit.exponent <- logit.type <- soilTimeScale <- `mature age` <- mature.age <-
      soil_c <- veg_c <- LC_bm2 <- LV_milUSD75 <- LV_USD75_bm2 <- LV_USD75_m2 <- HarvCropLand_bm2 <-
      unManagedLandValue <- LandAllocatorRoot <- hist.veg.carbon.density <- hist.soil.carbon.density <-
      veg.carbon.density <- soil.carbon.density <- allocation <- Land_Type.y <- mature.age.year.fillout <-
      min.veg.carbon.density <- min.soil.carbon.density <- . <- NULL


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

    # L222.LN2_Logit: Logit exponent of the second nest
    L125.LC_bm2_R_LT_Yh_GLU %>%
      # Determine which node combinations apply at this level.
      # not all land types have node matches, so use left_join
      left_join(select(A_LT_Mapping, LandNode1, LandNode2, Land_Type), by = "Land_Type") %>%
      select(region, GLU, LandNode1, LandNode2) %>%
      distinct() %>%
      na.omit %>%
      # Match in logit exponents based on the land node 2
      mutate(LandAllocatorRoot = "root",
             logit.year.fillout = min(BASE_YEARS)) %>%
      # logit.type is NA by default, so left_join
      left_join(select(A_LandNode_logit, logit.exponent, logit.type, LandNode), by = c("LandNode2" = "LandNode")) %>%
      append_GLU("LandNode1", "LandNode2") %>%
      select(one_of(LEVEL2_DATA_NAMES[["LN2_Logit"]]))->
      L222.LN2_Logit


    # L222.LN2_HistUnmgdAllocation: Historical land cover, unmanaged land in the second nest
    # L222.LN2_UnmgdAllocation: Model base period land cover, unmanaged land in the second nest
    # These tables are formed by filtering and adding node_leaf_names in L125.LC, and then
    # separating the result out by historical years and by model base years.
    L125.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type %in% A_LandLeaf_Unmgd2$UnmanagedLandLeaf,
             year %in% c(LAND_HISTORY_YEARS, BASE_YEARS)) %>%
      mutate(allocation = round(value, aglu.DIGITS_LAND_USE)) %>%
      add_node_leaf_names(nesting_table = A_LandLeaf_Unmgd2, leaf_name = "UnmanagedLandLeaf",
                          LN1 = "LandNode1", LN2 = "LandNode2") ->
      L222.LC_bm2_R_Unmgd2_Yh_GLU

    L222.LC_bm2_R_Unmgd2_Yh_GLU %>%
      filter(year %in% LAND_HISTORY_YEARS) %>%
      select(one_of(LEVEL2_DATA_NAMES[["LN2_HistUnmgdAllocation"]])) ->
      L222.LN2_HistUnmgdAllocation

    L222.LC_bm2_R_Unmgd2_Yh_GLU %>%
      filter(year %in% BASE_YEARS) %>%
      select(one_of(LEVEL2_DATA_NAMES[["LN2_UnmgdAllocation"]])) ->
      L222.LN2_UnmgdAllocation


    # L222.LN2_HistMgdAllocation: Historical land cover, managed land in the second nest
    # L222.LN2_MgdAllocation: Model base period land cover, managed land in the second nest
    # These tables are formed by filtering and adding node_leaf_names in L125.LC, and then
    # separating the result out by historical years and by model base years.
    L125.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type %in% A_LandLeaf2$LandLeaf,
             year %in% c(LAND_HISTORY_YEARS, BASE_YEARS)) %>%
      mutate(allocation = round(value, aglu.DIGITS_LAND_USE)) %>%
      add_node_leaf_names(nesting_table = A_LandLeaf_Unmgd2, leaf_name = "LandLeaf",
                          LN1 = "LandNode1", LN2 = "LandNode2") ->
      L222.LC_bm2_R_Mgd2_Yh_GLU

    L222.LC_bm2_R_Mgd2_Yh_GLU %>%
      filter(year %in% LAND_HISTORY_YEARS) %>%
      select(one_of(LEVEL2_DATA_NAMES[["LN2_HistMgdAllocation"]])) ->
      L222.LN2_HistMgdAllocation

    L222.LC_bm2_R_Mgd2_Yh_GLU %>%
      filter(year %in% BASE_YEARS) %>%
      select(one_of(LEVEL2_DATA_NAMES[["LN2_MgdAllocation"]])) ->
      L222.LN2_MgdAllocation


    # L222.LN2_UnmgdCarbon: Carbon content info, unmanaged land in the second nest
    # Add carbon content info to the unmanaged land allocation table to give the
    # carbon content infor for unmanaged land in the second nest.
    L222.LC_bm2_R_Unmgd2_Yh_GLU %>%
      filter(year == max(BASE_YEARS)) %>%
      select(-year, -allocation) %>%
      left_join_error_no_match(GCAMLandLeaf_CdensityLT, by = c("Land_Type" = "LandLeaf")) %>%
      rename(Cdensity_LT = Land_Type.y) %>%
      add_carbon_info(., carbon_info_table = L121.CarbonContent_kgm2_R_LT_GLU) %>%
      select(one_of(LEVEL2_DATA_NAMES[["LN2_UnmgdCarbon"]])) ->
      L221.LN2_UnmgdCarbon






    # 3. Produce outputs

    L222.LN2_Logit %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN2_Logit") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd2",
                     "aglu/A_LandLeaf2",
                     "aglu/A_LT_Mapping",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU") ->
      L222.LN2_Logit

    L222.LN2_HistUnmgdAllocation %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN2_HistUnmgdAllocation") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd2",
                     "aglu/A_LandLeaf2",
                     "aglu/A_LT_Mapping",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU") ->
      L222.LN2_HistUnmgdAllocation

    L222.LN2_UnmgdAllocation %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN2_UnmgdAllocation") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd2",
                     "aglu/A_LandLeaf2",
                     "aglu/A_LT_Mapping",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU") ->
      L222.LN2_UnmgdAllocation

    L222.LN2_HistMgdAllocation %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN2_HistMgdAllocation") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd2",
                     "aglu/A_LandLeaf2",
                     "aglu/A_LT_Mapping",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU") ->
      L222.LN2_HistMgdAllocation

    L222.LN2_MgdAllocation %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN2_MgdAllocation") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd2",
                     "aglu/A_LandLeaf2",
                     "aglu/A_LT_Mapping",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU") ->
      L222.LN2_MgdAllocation

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN2_UnmgdCarbon") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd2",
                     "aglu/A_LandLeaf2",
                     "aglu/A_LT_Mapping",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN2_UnmgdCarbon

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN2_MgdCarbon") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd2",
                     "aglu/A_LandLeaf2",
                     "aglu/A_LT_Mapping",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN2_MgdCarbon

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN2_HistUnmgdAllocation_noprot") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd2",
                     "aglu/A_LandLeaf2",
                     "aglu/A_LT_Mapping",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN2_HistUnmgdAllocation_noprot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN2_UnmgdAllocation_noprot") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd2",
                     "aglu/A_LandLeaf2",
                     "aglu/A_LT_Mapping",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN2_UnmgdAllocation_noprot

    # tibble() %>%
    #   add_title("descriptive title of data") %>%
    #   add_units("units") %>%
    #   add_comments("comments describing how data generated") %>%
    #   add_comments("can be multiple lines") %>%
    #   add_legacy_name("L222.LN2_UnmgdCarbon") %>%
    #   add_precursors("common/GCAM_region_names",
    #                  "water/basin_to_country_mapping",
    #                  "aglu/GCAMLandLeaf_CdensityLT",
    #                  "aglu/A_LandNode_logit",
    #                  "aglu/A_LandLeaf_Unmgd2",
    #                  "aglu/A_LandLeaf2",
    #                  "aglu/A_LT_Mapping",
    #                  "L121.CarbonContent_kgm2_R_LT_GLU",
    #                  "L125.LC_bm2_R_LT_Yh_GLU") %>%
    #   # typical flags, but there are others--see `constants.R`
    #   add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    #   L222.LN2_UnmgdCarbon

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN1_Logit_prot") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd2",
                     "aglu/A_LandLeaf2",
                     "aglu/A_LT_Mapping",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN1_Logit_prot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN1_HistUnmgdAllocation_prot") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd2",
                     "aglu/A_LandLeaf2",
                     "aglu/A_LT_Mapping",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN1_HistUnmgdAllocation_prot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN1_UnmgdAllocation_prot") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd2",
                     "aglu/A_LandLeaf2",
                     "aglu/A_LT_Mapping",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN1_UnmgdAllocation_prot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.LN1_UnmgdCarbon_prot") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd2",
                     "aglu/A_LandLeaf2",
                     "aglu/A_LT_Mapping",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L222.LN1_UnmgdCarbon_prot

    return_data(L222.LN2_Logit, L222.LN2_HistUnmgdAllocation, L222.LN2_UnmgdAllocation,
                L222.LN2_HistMgdAllocation, L222.LN2_MgdAllocation, L222.LN2_UnmgdCarbon, L222.LN2_MgdCarbon,
                L222.LN2_HistUnmgdAllocation_noprot, L222.LN2_UnmgdAllocation_noprot, L222.LN1_Logit_prot,
                L222.LN1_HistUnmgdAllocation_prot, L222.LN1_UnmgdAllocation_prot, L222.LN1_UnmgdCarbon_prot)
  } else {
    stop("Unknown command")
  }
}
