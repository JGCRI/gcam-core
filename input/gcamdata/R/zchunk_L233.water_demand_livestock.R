# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L233.water_demand_livestock
#'
#' Generates water coefficients for region-specific livestock for model years.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L233.TechCoef}. The corresponding file in the
#' original data system was \code{L233.water_demand_livestock.R} (water level2).
#' @details This chunk generates water coefficients (amount of water needed to produce one unit of livestock, unit: m^3/Mt)
#' for region-specific livestock for#' model years (1975, 1990,2005, 2010....2100), with the information of supplysector,
#' subsector,technology, and energy input.
#' @importFrom assertthat assert_that
#' @importFrom dplyr inner_join left_join mutate select
#' @author YL July 2017
module_water_L233.water_demand_livestock <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/water_td_sectors",
             FILE = "aglu/A_an_technology",
             "L133.water_demand_livestock_R_C_W_km3_Mt"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L233.TechCoef"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    GCAM_commodity <- supplysector <- subsector <- technology <- water_sector <-
      water_type <- region <- GCAM_region_ID <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    water_td_sectors <- get_data(all_data, "water/water_td_sectors")
    A_an_technology <- get_data(all_data, "aglu/A_an_technology")
    L133.water_demand_livestock_R_C_W_km3_Mt <- get_data(all_data, "L133.water_demand_livestock_R_C_W_km3_Mt", strip_attributes = TRUE)

    # Just read in water coefficients for all years
    L133.water_demand_livestock_R_C_W_km3_Mt %>%
      rename(supplysector = GCAM_commodity) %>%
      inner_join(select(A_an_technology, supplysector, subsector, technology), by = "supplysector") %>%
      mutate(water_sector = "Livestock",
             minicam.energy.input = set_water_input_name(water_sector, water_type, water_td_sectors)) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(market.name = region) %>%
      # Set the coef for all years
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(-GCAM_region_ID, -water_type, -water_sector) %>%

      # ===================================================

    # Produce outputs
    add_title("water coefficient for region-specific livestock") %>%
      add_units("m^3/Mt") %>%
      add_comments("The data is generated through:
                   1) read in the file L133.water_demand_livestock_R_C_W_km3_Mt and left join with the file A_an_technology;
                   2) generate new fields water_sector and minicam.energy.input:
                   3) left join with GCAM_region_names;
                   4) replicate the water coefficients for all MODEL_YEARS") %>%
      add_legacy_name("L233.TechCoef") %>%
      add_precursors("common/GCAM_region_names", "water/water_td_sectors", "aglu/A_an_technology",
                     "L133.water_demand_livestock_R_C_W_km3_Mt") ->
      L233.TechCoef

    return_data(L233.TechCoef)
  } else {
    stop("Unknown command")
  }
}
