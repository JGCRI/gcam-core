#' module_aglu_L221.land_input_1
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{curr_table$data}, \code{L221.LN0_Logit}, \code{L221.LN0_Land}, \code{L221.LN0_SoilTimeScale}, \code{curr_table$data}, \code{L221.LN1_ValueLogit}, \code{L221.LN1_HistUnmgdAllocation}, \code{L221.LN1_UnmgdAllocation}, \code{L221.LN1_UnmgdCarbon}. The corresponding file in the
#' original data system was \code{L221.land_input_1.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L221.land_input_1 <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "aglu/GCAMLandLeaf_CdensityLT",
             FILE = "aglu/A_LandNode_logit",
             FILE = "aglu/A_LandLeaf_Unmgd1",
             FILE = "aglu/A_LT_Mapping",
             FILE = "aglu/A_soil_time_scale_R",
             "L121.CarbonContent_kgm2_R_LT_GLU",
             "L125.LC_bm2_R_LT_Yh_GLU",
             "L125.LC_bm2_R",
             "L131.LV_USD75_m2_R_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(#"curr_table$data",
             "L221.LN0_Logit",
             "L221.LN0_Land",
             "L221.LN0_SoilTimeScale",
             "L221.LN1_ValueLogit",
             "L221.LN1_HistUnmgdAllocation",
             "L221.LN1_UnmgdAllocation",
             "L221.LN1_UnmgdCarbon"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    GCAMLandLeaf_CdensityLT <- get_data(all_data, "aglu/GCAMLandLeaf_CdensityLT")
    A_LandNode_logit <- get_data(all_data, "aglu/A_LandNode_logit")
    A_LandLeaf_Unmgd1 <- get_data(all_data, "aglu/A_LandLeaf_Unmgd1")
    A_LT_Mapping <- get_data(all_data, "aglu/A_LT_Mapping")
    A_soil_time_scale_R <- get_data(all_data, "aglu/A_soil_time_scale_R")
    L121.CarbonContent_kgm2_R_LT_GLU <- get_data(all_data, "L121.CarbonContent_kgm2_R_LT_GLU")
    L125.LC_bm2_R_LT_Yh_GLU <- get_data(all_data, "L125.LC_bm2_R_LT_Yh_GLU")
    L125.LC_bm2_R <- get_data(all_data, "L125.LC_bm2_R")
    L131.LV_USD75_m2_R_GLU <- get_data(all_data, "L131.LV_USD75_m2_R_GLU")


    # 1. Process inputs

    # Replace GLU names and Add region names
    L121.CarbonContent_kgm2_R_LT_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) ->
      L121.CarbonContent_kgm2_R_LT_GLU

    L125.LC_bm2_R_LT_Yh_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) ->
      L125.LC_bm2_R_LT_Yh_GLU

    L131.LV_USD75_m2_R_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) %>%
      ungroup ->
      L131.LV_USD75_m2_R_GLU

    # Add region names to inputs
    L125.LC_bm2_R %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L125.LC_bm2_R

    A_soil_time_scale_R %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      A_soil_time_scale_R

    # Convert land value to  1975$ per thousand km2 and calculate minimum land value,
    # setting a minimum threshold on the land values to ensure that no land use regions get a value of zero
    L131.LV_USD75_m2_R_GLU %>%
      mutate(LV_USD75_bm2 = round(LV_USD75_m2 * CONV_BM2_M2, aglu.DIGITS_LAND_VALUE)) ->
      L131.LV_USD75_bm2_R_GLU

    L131.LV_USD75_bm2_R_GLU %>%
      select(LV_USD75_bm2) %>%
      filter(LV_USD75_bm2 > 0) %>%
      min ->
      min_LV_USD75_bm2


    # 2. Perform computations

    # Build L221.LN0_Logit: Logit exponent of the top-level (zero) land nest
    L125.LC_bm2_R %>%
      select(region) %>%
      mutate(LandAllocatorRoot = "root",
             logit.year.fillout = min(BASE_YEARS),
             logit.exponent = aglu.N0_LOGIT_EXP,
             logit.type = aglu.N0_LOGIT_TYPE) ->
      L221.LN0_Logit


    # Build L221.LN0_Land: Total regional land allocation
    L125.LC_bm2_R %>%
      mutate(LandAllocatorRoot = "root",
            year.fillout = min(BASE_YEARS)) %>%
      rename(landAllocation = LC_bm2) ->
      L221.LN0_Land


    # Build L221.LN0_SoilTimeScale: Soil time scale by region
    A_soil_time_scale_R %>%
      filter(!(region %in% aglu.NO_AGLU_REGIONS)) %>%
      select(-GCAM_region_ID) %>%
      mutate(LandAllocatorRoot = "root") ->
      L221.LN0_SoilTimeScale


    # Build L221.LN1_ValueLogit: Unmanaged land value by region and AEZ, and logit exponent of first nest
    L125.LC_bm2_R_LT_Yh_GLU %>%
      left_join(select(A_LT_Mapping, Land_Type, LandNode1), by = "Land_Type") %>%
      select(region, GLU, LandNode1) %>%
      distinct %>%
      na.omit %>%
      mutate(LandAllocatorRoot = "root",
             logit.year.fillout = min(BASE_YEARS))  %>%
      # add exponents and logit types
      left_join(A_LandNode_logit, by = c("LandNode1" = "LandNode")) %>%
      # add land value
      left_join(select(L131.LV_USD75_bm2_R_GLU, region, GLU, LV_USD75_bm2), by = c("region", "GLU")) %>%
      rename(unManagedLandValue = LV_USD75_bm2) %>%
      # update land value with minimum to make sure every region-glu has a nonzero land value
      mutate(unManagedLandValue = replace(unManagedLandValue,
                                          is.na(unManagedLandValue) | unManagedLandValue == 0,
                                          min_LV_USD75_bm2),
             LandNode1 = paste(LandNode1, GLU, sep = aglu.CROP_GLU_DELIMITER),
             logit.type = aglu.N0_LOGIT_TYPE) %>%
      select(region, LandAllocatorRoot, LandNode1, unManagedLandValue, logit.year.fillout, logit.exponent, logit.type) ->
      L221.LN1_ValueLogit








    # 3. Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.LN0_Logit") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd1",
                     "aglu/A_LT_Mapping",
                     "aglu/A_soil_time_scale_R",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L125.LC_bm2_R",
                     "L131.LV_USD75_m2_R_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.LN0_Logit
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.LN0_Land") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd1",
                     "aglu/A_LT_Mapping",
                     "aglu/A_soil_time_scale_R",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L125.LC_bm2_R",
                     "L131.LV_USD75_m2_R_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.LN0_Land
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.LN0_SoilTimeScale") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd1",
                     "aglu/A_LT_Mapping",
                     "aglu/A_soil_time_scale_R",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L125.LC_bm2_R",
                     "L131.LV_USD75_m2_R_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.LN0_SoilTimeScale
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.LN1_ValueLogit") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd1",
                     "aglu/A_LT_Mapping",
                     "aglu/A_soil_time_scale_R",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L125.LC_bm2_R",
                     "L131.LV_USD75_m2_R_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.LN1_ValueLogit
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.LN1_HistUnmgdAllocation") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd1",
                     "aglu/A_LT_Mapping",
                     "aglu/A_soil_time_scale_R",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L125.LC_bm2_R",
                     "L131.LV_USD75_m2_R_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.LN1_HistUnmgdAllocation
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.LN1_UnmgdAllocation") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd1",
                     "aglu/A_LT_Mapping",
                     "aglu/A_soil_time_scale_R",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L125.LC_bm2_R",
                     "L131.LV_USD75_m2_R_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.LN1_UnmgdAllocation
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L221.LN1_UnmgdCarbon") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "aglu/GCAMLandLeaf_CdensityLT",
                     "aglu/A_LandNode_logit",
                     "aglu/A_LandLeaf_Unmgd1",
                     "aglu/A_LT_Mapping",
                     "aglu/A_soil_time_scale_R",
                     "L121.CarbonContent_kgm2_R_LT_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU",
                     "L125.LC_bm2_R",
                     "L131.LV_USD75_m2_R_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L221.LN1_UnmgdCarbon

    return_data(L221.LN0_Logit, L221.LN0_Land, L221.LN0_SoilTimeScale,
                L221.LN1_ValueLogit, L221.LN1_HistUnmgdAllocation, L221.LN1_UnmgdAllocation, L221.LN1_UnmgdCarbon)
  } else {
    stop("Unknown command")
  }
}
