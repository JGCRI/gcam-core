#' module_energy_LA116.geo
#'
#' Generate geothermal (hydrothermal and engineered geothermal systems (EGS)) supply curves by GCAM region
#' Supply curves developed for the 14 GCAM 3.0 regions are downscaled to the country level on the basis
#' of land area, and aggregated to the current GCAM regions. The data source for the supply curves is
#' Hannam et al. 2009: http://www.pnl.gov/main/publications/external/technical_reports/PNNL-19231.pdf
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L116.RsrcCurves_EJ_R_geo}, \code{L116.RsrcCurves_EJ_R_EGS}. The corresponding file in the
#' original data system was \code{LA116.geo.R} (energy level1).
#' @details This code chunk assigns geothermal electric resources to GCAM regions. Because the underlying
#' source data were developed for the 14 regions of GCAM 3.0 and prior, they are first
#' downscaled to the nation level on the basis of land area, and aggregated to the current GCAM regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author GPK April 2017
module_energy_LA116.geo <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/LDS/Land_type_area_ha",
             FILE = "energy/A16.geo_curves",
             FILE = "energy/A16.EGS_curves"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L116.RsrcCurves_EJ_R_geo",
             "L116.RsrcCurves_EJ_R_EGS"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    Land_type_area_ha <- get_data(all_data, "aglu/LDS/Land_type_area_ha")
    A16.geo_curves <- get_data(all_data, "energy/A16.geo_curves")
    A16.EGS_curves <- get_data(all_data, "energy/A16.EGS_curves")

    # The method below assumes that the geothermal supply curves (hydrothermal and EGS) have
    # only one price point per grade (i.e., "grade 2" has the same price in all regions). This
    # is the standard in GCAM as long as the model has existed, but still this needs to be
    # confirmed in this initial check

    if( nrow(unique(A16.geo_curves[ c("grade", "extractioncost")])) > length( unique( A16.geo_curves$grade ) ) ){
      stop( "The geothermal (hydrothermal) supply curves have regionally differentiated price points" )
    }
    if( nrow(unique(A16.EGS_curves[ c("grade", "extractioncost")])) > length( unique( A16.EGS_curves$grade ) ) ){
      stop( "The geothermal (EGS) supply curves have regionally differentiated price points" )
    }

    # Downscale GCAM 3.0 geothermal resources to countries on the basis of land area
    # Calculate land area shares of countries within region_GCAM3
    # if the last of the HISTORICAL_YEARS is in the land area data set, use that,
    # otherwise just use the most recent year in the land area data
    if(max(HISTORICAL_YEARS) %in% Land_type_area_ha$year) {
      geo_land_year = max(HISTORICAL_YEARS)
    } else {
      geo_land_year = max(Land_type_area_ha$year)
    }

    Land_type_area_ha %>%
      filter(year == geo_land_year) %>%
      group_by( iso ) %>%
      summarise(value = as.numeric(sum(value))) %>%
      left_join_error_no_match(iso_GCAM_regID[c("iso", "region_GCAM3", GCAM_REGION_ID)], by = "iso") ->
      L116.land_ctry

    L116.land_ctry %>%
      group_by(region_GCAM3) %>%
      summarise(sumvalue = sum(value)) ->
      L116.land_rg3

    L116.land_ctry %>%
      full_join(L116.land_rg3, by = "region_GCAM3") %>%
      mutate(share = value / sumvalue) %>%
      select( -value, -sumvalue ) ->
      L116.land_share_ctry_rg3

    # Repeat land area shares by the number of grades in the hydrothermal geothermal resource assumptions
    # and multiply by the available resource quantities
    L116.land_share_ctry_rg3 %>%
      repeat_add_columns(unique(A16.geo_curves["grade"] ) ) %>%
      left_join_error_no_match(A16.geo_curves, by = c( "region_GCAM3", "grade" ) ) %>%
      mutate(available = share * available) ->
      L116.geothermal_ctry

    # Aggregate country-level hydrothermal geothermal resource supply curves by GCAM region
    L116.geothermal_ctry %>%
      group_by_(GCAM_REGION_ID, "resource", "subresource", "grade", "extractioncost") %>%
      summarise(available = sum(available)) ->
      L116.geothermal_rgn

    # Specify the names of the resource table that will be written out
    L116.table.names <- c( GCAM_REGION_ID, "resource", "subresource", "grade", "available", "extractioncost" )
    as_tibble( L116.geothermal_rgn[ L116.table.names ]) %>%
      # Documentation
      add_title("Hydrothermal geothermal supply curves by GCAM region") %>%
      add_units("EJ/yr") %>%
      add_comments("Downscale GCAM 3.0 geothermal supply curves to countries on land area basis; aggregate to GCAM regions") %>%
      add_legacy_name("L116.RsrcCurves_EJ_R_geo") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/LDS/Land_type_area_ha",
                     "energy/A16.geo_curves") ->
      L116.RsrcCurves_EJ_R_geo

    # Repeat land area shares by the number of grades in the EGS geothermal resource assumptions
    # and multiply by the available resource quantities
    L116.land_share_ctry_rg3 %>%
      repeat_add_columns(unique(A16.EGS_curves["grade"] ) ) %>%
      left_join_error_no_match(A16.EGS_curves, by = c( "region_GCAM3", "grade" ) ) %>%
      mutate(available = share * available) ->
      L116.EGS_ctry

    # Aggregate country-level EGS geothermal resource supply curves by GCAM region
    L116.EGS_ctry %>%
      group_by_(GCAM_REGION_ID, "resource", "subresource", "grade", "extractioncost") %>%
      summarise(available = sum(available)) ->
      L116.EGS_rgn

    as_tibble( L116.EGS_rgn[ L116.table.names ]) %>%
      # Documentation
      add_title("Enhanced Geothermal Systems (EGS) supply curves by GCAM region") %>%
      add_units("EJ/yr") %>%
      add_comments("Downscale GCAM 3.0 EGS supply curves to countries on land area basis; aggregate to GCAM regions") %>%
      add_legacy_name("L116.RsrcCurves_EJ_R_EGS") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/LDS/Land_type_area_ha",
                     "energy/A16.EGS_curves") ->
      L116.RsrcCurves_EJ_R_EGS

    return_data(L116.RsrcCurves_EJ_R_geo, L116.RsrcCurves_EJ_R_EGS)
  } else {
    stop("Unknown command")
  }
}
