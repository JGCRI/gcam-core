# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L102.water_supply_unlimited
#'
#' Build out unlimited water prices using defaults in Constants.R.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L102.unlimited_mapped_water_price_B_W_Y_75USDm3},
#' \code{L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3}. The corresponding file in the
#' original data system was \code{L102.water_supply_unlimited.R} (water level1).
#' @details  Generates unlimited water supply for all combinations of basin / region and water_type.
#' @importFrom assertthat assert_that
#' @importFrom dplyr case_when filter mutate select
#' @author ST Oct 2018
module_water_L102.water_supply_unlimited <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/basin_to_country_mapping",
             FILE = "common/GCAM_region_names"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L102.unlimited_mapped_water_price_B_W_Y_75USDm3",
             "L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3"))
  } else if(command == driver.MAKE) {

    region <- water_type <- NULL                      # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")

    # get vector of nonmapped water types
    water.ALL_WATER_TYPES[!(water.ALL_WATER_TYPES %in% water.MAPPED_WATER_TYPES)] ->
      nonmapped_water_types

    # mapped water types
    expand.grid(GCAM_basin_ID = basin_to_country_mapping[["GCAM_basin_ID"]],
                year = MODEL_YEARS,
                water_type = water.MAPPED_WATER_TYPES) %>%
      as_tibble() %>%
      mutate(price = case_when(
        water_type == "water consumption" ~ water.DEFAULT_UNLIMITED_WATER_PRICE,
        water_type == "water withdrawals" ~ water.DEFAULT_UNLIMITED_WITHD_WATER_PRICE
      )) ->
      L102.unlimited_mapped_water_price_B_W_Y_75USDm3

    # nonmapped water types
    expand.grid(GCAM_region_ID = GCAM_region_names[["GCAM_region_ID"]],
                year = MODEL_YEARS,
                water_type = nonmapped_water_types) %>%
      as_tibble() %>%
      mutate(price = water.DEFAULT_UNLIMITED_WATER_PRICE) ->
      L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3

    # ===================================================

    # Produce outputs

    L102.unlimited_mapped_water_price_B_W_Y_75USDm3 %>%
      mutate(water_type = as.character(water_type)) %>%
      add_title("Water price assumptions for mapped water types") %>%
      add_units("1975$/m3") %>%
      add_comments("Nominal default water prices") %>%
      add_legacy_name("L102.unlimited_mapped_water_price_R_W_Y_75USDm3") %>%
      add_precursors("water/basin_to_country_mapping") ->
      L102.unlimited_mapped_water_price_B_W_Y_75USDm3

    L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3 %>%
      mutate(water_type = as.character(water_type)) %>%
      add_title("Water price assumptions for nonmapped water types") %>%
      add_units("1975$/m3") %>%
      add_comments("Nominal default water prices") %>%
      add_legacy_name("L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3") %>%
      add_precursors("common/GCAM_region_names") ->
      L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3

      return_data(L102.unlimited_mapped_water_price_B_W_Y_75USDm3,
                  L102.unlimited_nonmapped_water_price_R_W_Y_75USDm3)


  } else {
    stop("Unknown command")
  }
}
