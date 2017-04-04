#' module_aglu_LB120.LC_GIS_R_LTgis_Yh_GLU
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L120.LC_bm2_R_LT_Yh_GLU}, \code{L120.LC_bm2_R_UrbanLand_Yh_GLU}, \code{L120.LC_bm2_R_Tundra_Yh_GLU}, \code{L120.LC_bm2_R_RckIceDsrt_Yh_GLU}, \code{L120.LC_bm2_ctry_LTsage_GLU}, \code{L120.LC_bm2_ctry_LTpast_GLU}. The corresponding file in the
#' original data system was \code{LB120.LC_GIS_R_LTgis_Yh_GLU.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL April 2017
#' @export
module_aglu_LB120.LC_GIS_R_LTgis_Yh_GLU <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/LDS/LDS_land_types",
             FILE = "aglu/SAGE_LT",
             "L100.Land_type_area_ha"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L120.LC_bm2_R_LT_Yh_GLU",
             "L120.LC_bm2_R_UrbanLand_Yh_GLU",
             "L120.LC_bm2_R_Tundra_Yh_GLU",
             "L120.LC_bm2_R_RckIceDsrt_Yh_GLU",
             "L120.LC_bm2_ctry_LTsage_GLU",
             "L120.LC_bm2_ctry_LTpast_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs

    get_data(all_data, "common/iso_GCAM_regID") %>%
      select(iso, GCAM_region_ID) ->
      iso_GCAM_regID
    LDS_land_types <- get_data(all_data, "aglu/LDS/LDS_land_types")
    SAGE_LT <- get_data(all_data, "aglu/SAGE_LT")
    L100.Land_type_area_ha <- get_data(all_data, "L100.Land_type_area_ha")

    # Perform computations

    L100.Land_type_area_ha %>%
      # Add data for GCAM region ID and GLU
      left_join_error_no_match(distinct(iso_GCAM_regID, iso, .keep_all = TRUE), by = "iso") %>%
      # Add vectors for land type ( SAGE, HYDE, and WDPA )
      left_join_error_no_match(LDS_land_types, by = c("land_code" = "Category")) %>%
      left_join(SAGE_LT, by = "LT_SAGE") %>%  # includes NAs
      rename(LT_SAGE_5 = Land_Type) %>%
      # Drop all rows with missing values (inland bodies of water)
      na.omit %>%
      # Reset WDPA classification to "Non-protected" where HYDE classification is cropland, pasture, or urban land
      mutate(LT_WDPA = if_else(LT_HYDE!="Unmanaged", "Non-protected", "Unmanaged"),
             Land_Type = LT_SAGE_5,
             # These multi-tiered classifications will be used for C contents, but for all land cover processing, collapse into GCAM land types
             Land_Type = if_else(LT_HYDE == "Cropland", "Cropland", Land_Type),
             Land_Type = if_else(LT_HYDE == "Pasture", "Pasture", Land_Type),
             Land_Type = if_else(LT_HYDE == "UrbanLand", "UrbanLand", Land_Type),
             # Area in thousand square kilometers (bm2)
             Area_bm2 =  value * CONV_HA_BM2) ->
      L100.Land_type_area_ha

    # LAND COVER FOR LAND ALLOCATION
    # Aggregate into GCAM regions and land types. This table is incomplete (missing non-existent combinations), indicated by LCi
    # Part 1: Land cover by GCAM land category in all model history/base years
    # Collapsing land cover into GCAM regions and aggregate land types
    L100.Land_type_area_ha %>%
      group_by(GCAM_region_ID, Land_Type, year, GLU) %>%
      summarise(Area_bm2 = sum(Area_bm2)) %>%
      # TODO: is this corrrect?
      # Missing values should be set to 0 before interpolation, so that in-between years are interpolated correctly
      # Note that without this, some groups have all NAs and can't be interpolated
      mutate(Area_bm2 = if_else(is.na(Area_bm2), 0, Area_bm2)) ->
      L120.LC_bm2_R_LT_Yh_GLU

    # Interpolation step
    L120.LC_bm2_R_LT_Yh_GLU %>%
      ungroup %>%
      # Expand to all combinations using more years. Note the `ungroup` call above!!
      complete(nesting(GCAM_region_ID, Land_Type, GLU), year = unique(c(year, aglu.LAND_COVER_YEARS))) %>%
      arrange(GCAM_region_ID, Land_Type, GLU, year) %>%
      group_by(GCAM_region_ID, Land_Type, GLU) %>%
      mutate(Area_bm2 = approx_fun(year, Area_bm2)) %>%
      # Replicate old behavior, replacing NAs with zeroes
      mutate(Area_bm2 = if_else(is.na(Area_bm2), 0, Area_bm2)) ->
      L120.LC_bm2_R_LT_Yh_GLU

    # Subset the land types that are not further modified, and write them out
    L120.LC_bm2_R_UrbanLand_Yh_GLU <- filter(L120.LC_bm2_R_LT_Yh_GLU, Land_Type =="UrbanLand")
    L120.LC_bm2_R_Tundra_Yh_GLU <- filter(L120.LC_bm2_R_LT_Yh_GLU, Land_Type =="Tundra")
    L120.LC_bm2_R_RckIceDsrt_Yh_GLU <- filter(L120.LC_bm2_R_LT_Yh_GLU, Land_Type =="RockIceDesert")


    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L120.LC_bm2_R_LT_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT") %>%
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L120.LC_bm2_R_LT_Yh_GLU
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L120.LC_bm2_R_UrbanLand_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT") %>%
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L120.LC_bm2_R_UrbanLand_Yh_GLU
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L120.LC_bm2_R_Tundra_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT") %>%
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L120.LC_bm2_R_Tundra_Yh_GLU
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L120.LC_bm2_R_RckIceDsrt_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT") %>%
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L120.LC_bm2_R_RckIceDsrt_Yh_GLU
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L120.LC_bm2_ctry_LTsage_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT") %>%
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L120.LC_bm2_ctry_LTsage_GLU
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L120.LC_bm2_ctry_LTpast_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT") %>%
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L120.LC_bm2_ctry_LTpast_GLU

    return_data(L120.LC_bm2_R_LT_Yh_GLU, L120.LC_bm2_R_UrbanLand_Yh_GLU, L120.LC_bm2_R_Tundra_Yh_GLU, L120.LC_bm2_R_RckIceDsrt_Yh_GLU, L120.LC_bm2_ctry_LTsage_GLU, L120.LC_bm2_ctry_LTpast_GLU)
  } else {
    stop("Unknown command")
  }
}



