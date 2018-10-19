#' module_water_L201.water.resources.constrainted
#'
#' Constrained surface and groudwater.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.RenewRsrcCurves_calib},
#' \code{201.GrdRenewRsrcMax_runoff}, \code{L201.DepRsrcCurves_ground}. The corresponding file in the
#' original data system was \code{L102.water.supply.unlimited.R} (water level1).
#' @details  Genereates water withdrawal resource input files for region + basin which includes runoff and groundwater.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author ST Oct 2018
module_water_L201.water.resources.constrainted <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/basin_ID",
             FILE = "common/GCAM_region_names",
             FILE = "water/basin_water_demand_1990_2010",
             "L100.runoff_accessible",
             "L100.runoff_max_bm3",
             "L101.groundwater_depletion_bm3",
             "L101.groundwater_grades_constrained_bm3",
             "L101.groundwater_grades_uniform_bm3",
             "L103.water_mapping_R_GLU_B_W_Ws_share",
             "L103.water_mapping_R_B_W_Ws_share"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.NodeEquiv",
             "L201.DeleteUnlimitRsrc",
             "L201.Rsrc",
             "L201.RsrcPrice",
             "L201.RenewRsrcCurves_uncalibrated",
             "L201.GrdRenewRsrcMax_runoff",
             "L201.DepRsrcCurves_ground_uniform",
             "L201.RenewRsrcCurves_calib",
             "L201.DepRsrcCurves_ground"))
  } else if(command == driver.MAKE) {

    region <- NULL                      # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    gcam_regions <- get_data(all_data, "common/GCAM_region_names")
    basin_ids <- get_data(all_data, "water/basin_ID")
    water_mapping_R_GLU_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_GLU_B_W_Ws_share")
    water_mapping_R_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_B_W_Ws_share")


    # build tables with all possible technologies

    # create node equivalence lists to allow use of same subresource headers
    # ... regardless of what type the actual resources is

    tibble(group.name = "Resources", tag1 = "resource",
           tag2 = "depresource", tag3 = "renewresource",
           tag4 = "unlimited.resource") ->
      L201.NodeEquiv

    # create full set of region/basin combinations
    bind_rows(water_mapping_R_GLU_B_W_Ws_share %>%
                rename(basin_id = GLU),
              water_mapping_R_B_W_Ws_share) %>%
      select(-water_sector, -share) %>% unique() %>%
      filter(water_type == "water withdrawals") %>%
      left_join(basin_ids, by = "basin_id") %>%
      left_join(gcam_regions, by = "GCAM_region_ID") ->
      L201.region_basin

    # create the delete for the unlimited resource markets for withdrawals
    L201.region_basin %>%
      arrange(region) %>%
      mutate(unlimited.resource = paste0("-", water_type)) %>%
      select(region, unlimited.resource) ->
      L201.DeleteUnlimitRsrc

    # create resource markets for water withdrawals
    L201.region_basin %>%
      arrange(region) %>%
      mutate(resource = paste0("-", water_type),
             output.unit = water.WATER_UNITS_QUANTITY,
             price.unit = water.WATER_UNITS_PRICE) %>%
      rename(market = basin_id) %>%
      select(region, resource, output.unit, price.unit, market) ->
      L201.Rsrc


    # ... TBC













    # ===================================================

    # Produce outputs

    L102.unlimited_mapped_water_price_B_W_Y_75USDm3 %>%
      add_title("Water price assumptions for mapped water types") %>%
      add_units("1975$/m3") %>%
      add_comments("Nominal default water prices") %>%
      add_legacy_name("L102.unlimited_mapped_water_price_R_W_Y_75USDm3") %>%
      add_precursors("water/basin_ID") ->
      L102.unlimited_mapped_water_price_B_W_Y_75USDm3


    return_data()


  } else {
    stop("Unknown command")
  }
}
