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

    # Replace GLU names
    L121.CarbonContent_kgm2_R_LT_GLU %>%
      replace_GLU(map = basin_to_country_mapping) ->
      L121.CarbonContent_kgm2_R_LT_GLU

    L125.LC_bm2_R_LT_Yh_GLU %>%
      replace_GLU(map = basin_to_country_mapping) ->
      L125.LC_bm2_R_LT_Yh_GLU

    L131.LV_USD75_m2_R_GLU %>%
      replace_GLU(map = basin_to_country_mapping) ->
      L131.LV_USD75_m2_R_GLU

    # Add region names to inputs
    L125.LC_bm2_R %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L125.LC_bm2_R


    # Perform computations

    # Create a template table of combinations of region, GLUs, land use types
    L125.LC_bm2_R_LT_Yh_GLU %>%
      left_join(select(A_LT_Mapping, Land_Type, LandNode1), by = "Land_Type") %>%
      select(GCAM_region_ID, GLU, LandNode1) %>%
      distinct %>%
      na.omit ->
      L221.LN1


    # Calculate logit exponent of the top-level (zero) land nest.

    # Prepare the table of identifying information to be filled in
    L125.LC_bm2_R %>%
      select(region) %>%
      mutate(LandAllocatorRoot = "root",
             logit.year.fillout = min(BASE_YEARS),
             logit.exponent = aglu.N0_LOGIT_EXP,
             logit.type = aglu.N0_LOGIT_TYPE) %>%
      get_logit_fn_tables(names_LN0_LogitType,
                          base.header="LN0_Logit_", include.equiv.table=T, write.all.regions=F) ->
      L221.LN0_Logit






    # Produce outputs
    # tibble() %>%
    #   add_title("descriptive title of data") %>%
    #   add_units("units") %>%
    #   add_comments("comments describing how data generated") %>%
    #   add_comments("can be multiple lines") %>%
    #   add_legacy_name("curr_table$data") %>%
    #   add_precursors("precursor1", "precursor2", "etc") %>%
    #   # typical flags, but there are others--see `constants.R`
    #   add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    #   curr_table$data
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
    # tibble() %>%
    #   add_title("descriptive title of data") %>%
    #   add_units("units") %>%
    #   add_comments("comments describing how data generated") %>%
    #   add_comments("can be multiple lines") %>%
    #   add_legacy_name("curr_table$data") %>%
    #   add_precursors("precursor1", "precursor2", "etc") %>%
    #   # typical flags, but there are others--see `constants.R`
    #   add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
    #   curr_table$data
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

    return_data(# curr_table$data,
                L221.LN0_Logit, L221.LN0_Land, L221.LN0_SoilTimeScale,
                # curr_table$data,
                L221.LN1_ValueLogit, L221.LN1_HistUnmgdAllocation, L221.LN1_UnmgdAllocation, L221.LN1_UnmgdCarbon)
  } else {
    stop("Unknown command")
  }
}
