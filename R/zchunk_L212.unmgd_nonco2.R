#' module_emissions_L212.unmgd_nonco2
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L212.AgSupplySectorLogitTables[[ curr_table ]]$data}, \code{L212.AgSupplySector}, \code{L212.SubsectorLogitTables[[ curr_table ]]$data}, \code{L212.AgSupplySubsector}, \code{L212.ItemName}, \code{L212.GRASSEmissions}, \code{L212.FORESTEmissions_FF}, \code{L212.FORESTEmissions_D}, \code{L212.GRASSEmissionsFactors_BCOC}, \code{L212.FORESTEmissionsFactors_BCOC_FF}, \code{L212.FORESTEmissionsFactors_BCOC_D}, \code{L212.FORESTEmissionsFactors_future}, \code{L212.ItemName_prot}, \code{L212.GRASSEmissions_prot}, \code{L212.GRASSEmissions_noprot}, \code{L212.FORESTEmissions_FF_prot}, \code{L212.FORESTEmissions_FF_noprot}, \code{L212.FORESTEmissions_D_prot}, \code{L212.FORESTEmissions_D_noprot}, \code{L212.GRASSEmissionsFactors_BCOC_prot}, \code{L212.GRASSEmissionsFactors_BCOC_noprot}, \code{L212.FORESTEmissionsFactors_BCOC_FF_prot}, \code{L212.FORESTEmissionsFactors_BCOC_FF_noprot}, \code{L212.FORESTEmissionsFactors_BCOC_D_prot}, \code{L212.FORESTEmissionsFactors_BCOC_D_noprot}. The corresponding file in the
#' original data system was \code{L212.unmgd_nonco2.R} (emissions level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RLH August 2017
#' @export
module_emissions_L212.unmgd_nonco2 <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "emissions/A_regions",
             "L124.nonco2_tg_R_grass_Y_GLU",
             "L124.nonco2_tg_R_forest_Y_GLU",
             "L124.deforest_coefs",
             "L125.bcoc_tgbkm2_R_grass_2000",
             "L125.bcoc_tgbkm2_R_forest_2000",
             "L125.deforest_coefs_bcoc"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L212.AgSupplySector",
             "L212.AgSupplySubsector",
             "L212.ItemName",
             "L212.GRASSEmissions",
             "L212.FORESTEmissions_FF",
             "L212.FORESTEmissions_D",
             "L212.GRASSEmissionsFactors_BCOC",
             "L212.FORESTEmissionsFactors_BCOC_FF",
             "L212.FORESTEmissionsFactors_BCOC_D",
             "L212.FORESTEmissionsFactors_future",
             "L212.ItemName_prot",
             "L212.GRASSEmissions_prot",
             "L212.GRASSEmissions_noprot",
             "L212.FORESTEmissions_FF_prot",
             "L212.FORESTEmissions_FF_noprot",
             "L212.FORESTEmissions_D_prot",
             "L212.FORESTEmissions_D_noprot",
             "L212.GRASSEmissionsFactors_BCOC_prot",
             "L212.GRASSEmissionsFactors_BCOC_noprot",
             "L212.FORESTEmissionsFactors_BCOC_FF_prot",
             "L212.FORESTEmissionsFactors_BCOC_FF_noprot",
             "L212.FORESTEmissionsFactors_BCOC_D_prot",
             "L212.FORESTEmissionsFactors_BCOC_D_noprot"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    A_regions <- get_data(all_data, "emissions/A_regions")
    L124.nonco2_tg_R_grass_Y_GLU <- get_data(all_data, "L124.nonco2_tg_R_grass_Y_GLU")
    L124.nonco2_tg_R_forest_Y_GLU <- get_data(all_data, "L124.nonco2_tg_R_forest_Y_GLU")
    L124.deforest_coefs <- get_data(all_data, "L124.deforest_coefs")
    L125.bcoc_tgbkm2_R_grass_2000 <- get_data(all_data, "L125.bcoc_tgbkm2_R_grass_2000")
    L125.bcoc_tgbkm2_R_forest_2000 <- get_data(all_data, "L125.bcoc_tgbkm2_R_forest_2000")
    L125.deforest_coefs_bcoc <- get_data(all_data, "L125.deforest_coefs_bcoc")

    # ===================================================
    # Unmanaged land sector info
    # L212.AgSupplySector: Sector info for unmanaged land technology
    # NOTE: only making unmanaged land sectors in regions that have aglu related emissions
    # also note, not including the BC/OC tables here, which are from a different data source, and have no GLU detail
    # if regions/LTs have default BC/OC emissions factors but don't have any other gases (from EDGAR), they are dropped
    L212.regions <- unique(c(L124.nonco2_tg_R_grass_Y_GLU$GCAM_region_ID, L124.nonco2_tg_R_forest_Y_GLU$GCAM_region_ID))
    L212.regions <- GCAM_region_names %>%
      filter(GCAM_region_ID %in% L212.regions) %>%
      select(region)

    L212.AgSupplySector <- L212.regions %>%
      mutate(AgSupplySector = "UnmanagedLand",
             output.unit = "none",
             input.unit = "thous km2",
             price.unit = "none",
             calPrice = -1,
             market = region,
             logit.year.fillout = min(BASE_YEARS),
             logit.exponent = -3)

    # L212.ItemName: Land item to relate emissions to
    # Note: only making unmanaged land technologies in the regions/glus with the corresponding land use type available
    L212.ItemName_R_LT_GLU <- bind_rows(unique(L124.nonco2_tg_R_grass_Y_GLU[c("GCAM_region_ID", "Land_Type", "GLU")]),
                                     unique(L124.nonco2_tg_R_forest_Y_GLU[c("GCAM_region_ID", "Land_Type", "GLU")]))

    L212.ItemName <- tibble(AgSupplySector = "UnmanagedLand",
                            AgSupplySubsector = c("ForestFire", "Deforest", "GrasslandFires"),
                            itemName = c("UnmanagedForest", "UnmanagedForest", "Grassland")) %>%
      left_join(L212.ItemName_R_LT_GLU, by = c("itemName" = "Land_Type")) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(AgSupplySubsector = paste(AgSupplySubsector, GLU, sep = "_"),
             UnmanagedLandTechnology = AgSupplySubsector,
             itemName = paste(itemName, GLU, sep = "_")) %>%
      repeat_add_columns(tibble(year = BASE_YEARS)) %>%
      select(region, AgSupplySector, AgSupplySubsector, UnmanagedLandTechnology, year, itemName)

    # Grassland emissions
    # L212.GrassEmissions: Grassland fire emissions in all regions
    L212.GRASSEmissions <- L124.nonco2_tg_R_grass_Y_GLU %>%
      filter(year %in% emissions.MODEL_BASE_YEARS) %>%
      mutate(value = round(value, emissions.DIGITS_EMISSIONS),
             AgSupplySector = "UnmanagedLand",
             AgSupplySubsector = paste( "GrasslandFires", GLU, sep = "_"),
             UnmanagedLandTechnology = AgSupplySubsector) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, AgSupplySector, AgSupplySubsector, UnmanagedLandTechnology, year, Non.CO2, input.emissions = value)

    # Grassland emissions factors for BC/OC
    # L212.GrassEmissions: Grassland fire emissions factors for BC/OC in all regions
    L212.GRASSEmissionsFactors_BCOC <- L212.GRASSEmissions %>%
      select(region, AgSupplySector, AgSupplySubsector, UnmanagedLandTechnology, year) %>%
      distinct() %>%
      repeat_add_columns(tibble(Non.CO2 = unique(L125.bcoc_tgbkm2_R_grass_2000$Non.CO2))) %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      left_join_error_no_match(L125.bcoc_tgbkm2_R_grass_2000, by = c("Non.CO2", "GCAM_region_ID")) %>%
      mutate(em_factor = round(em_factor, emissions.DIGITS_EMISSIONS)) %>%
      select(-GCAM_region_ID, -Land_Type)

    # Forest fire emissions
    # L212.ForestEmissions: Forest fire emissions in all regions
    # Interpolate and add region name
    L212.FOREST <- L124.nonco2_tg_R_forest_Y_GLU %>%
      filter(year %in% emissions.MODEL_BASE_YEARS) %>%
      mutate(value = round(value, emissions.DIGITS_EMISSIONS),
             AgSupplySector = "UnmanagedLand",
             AgSupplySubsector = paste(technology, GLU, sep = "_"),
             UnmanagedLandTechnology = AgSupplySubsector) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # remove technology later
      select(region, AgSupplySector, AgSupplySubsector, UnmanagedLandTechnology, technology, year, Non.CO2, input.emissions = value)

    # Forest fire emissions factors for BC/OC
    # L212.ForestEmissions: Forest fire emissions factors for BC/OC in all regions
    # Interpolate and add region name
    L212.FORESTEmissionsFactors_BCOC <- L212.FOREST %>%
      select(region, AgSupplySector, AgSupplySubsector, UnmanagedLandTechnology, technology, year) %>%
      distinct()


    # Split into ForestFire & Deforest -- they have different drivers and this is easier to do now
    L212.FORESTEmissionsFactors_BCOC_FF <- subset( L212.FORESTEmissionsFactors_BCOC, technology == "ForestFire" )[
      c( names_UnmgdTech, Y, "Non.CO2", "emiss.coef" ) ]
    L212.FORESTEmissionsFactors_BCOC_D <- subset( L212.FORESTEmissionsFactors_BCOC, technology == "Deforest" )[
      c( names_UnmgdTech, Y, "Non.CO2", "emiss.coef" ) ]

    # ===================================================
    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.AgSupplySector") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.AgSupplySector

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.AgSupplySubsector") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.AgSupplySubsector

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.ItemName") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.ItemName

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.GRASSEmissions") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.GRASSEmissions

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissions_FF") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissions_FF

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissions_D") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissions_D

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.GRASSEmissionsFactors_BCOC") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.GRASSEmissionsFactors_BCOC

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_BCOC_FF") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissionsFactors_BCOC_FF

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_BCOC_D") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissionsFactors_BCOC_D

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_future") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissionsFactors_future

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.ItemName_prot") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.ItemName_prot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.GRASSEmissions_prot") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.GRASSEmissions_prot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.GRASSEmissions_noprot") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.GRASSEmissions_noprot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissions_FF_prot") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissions_FF_prot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissions_FF_noprot") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissions_FF_noprot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissions_D_prot") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissions_D_prot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissions_D_noprot") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissions_D_noprot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.GRASSEmissionsFactors_BCOC_prot") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.GRASSEmissionsFactors_BCOC_prot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.GRASSEmissionsFactors_BCOC_noprot") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.GRASSEmissionsFactors_BCOC_noprot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_BCOC_FF_prot") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissionsFactors_BCOC_FF_prot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_BCOC_FF_noprot") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissionsFactors_BCOC_FF_noprot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_BCOC_D_prot") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissionsFactors_BCOC_D_prot

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_BCOC_D_noprot") %>%
      add_precursors("common/GCAM_region_names") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L212.FORESTEmissionsFactors_BCOC_D_noprot

    return_data(L212.AgSupplySector, L212.AgSupplySubsector, L212.ItemName, L212.GRASSEmissions, L212.FORESTEmissions_FF, L212.FORESTEmissions_D, L212.GRASSEmissionsFactors_BCOC, L212.FORESTEmissionsFactors_BCOC_FF, L212.FORESTEmissionsFactors_BCOC_D, L212.FORESTEmissionsFactors_future, L212.ItemName_prot, L212.GRASSEmissions_prot, L212.GRASSEmissions_noprot, L212.FORESTEmissions_FF_prot, L212.FORESTEmissions_FF_noprot, L212.FORESTEmissions_D_prot, L212.FORESTEmissions_D_noprot, L212.GRASSEmissionsFactors_BCOC_prot, L212.GRASSEmissionsFactors_BCOC_noprot, L212.FORESTEmissionsFactors_BCOC_FF_prot, L212.FORESTEmissionsFactors_BCOC_FF_noprot, L212.FORESTEmissionsFactors_BCOC_D_prot, L212.FORESTEmissionsFactors_BCOC_D_noprot)
  } else {
    stop("Unknown command")
  }
}
