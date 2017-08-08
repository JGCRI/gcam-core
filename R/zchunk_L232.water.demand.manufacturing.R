#' module_water_L232.water.demand.manufacturing
#'
#' Maps sector names to manufacturing water withdrawal/consumption coefficients and expands to all regions and years
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L232.TechCoef}. The corresponding file in the
#' original data system was \code{L232.water.demand.manufacturing.R} (water level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author ST August 2017
module_water_L232.water.demand.manufacturing <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/A03.sector",
             FILE = "energy/A32.globaltech_coef",
             "L132.water_coef_manufacturing_R_W_m3_GJ"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L232.TechCoef"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    water_sector <- water_type <- region <- supplysector <- subsector <- technology <-
      year <- minicam.energy.input <- coefficient <- market.name <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A03.sector <- get_data(all_data, "water/A03.sector")
    A32.globaltech_coef <- get_data(all_data, "energy/A32.globaltech_coef")
    L132.water_coef_manufacturing_R_W_m3_GJ <- get_data(all_data, "L132.water_coef_manufacturing_R_W_m3_GJ")

    L132.water_coef_manufacturing_R_W_m3_GJ %>%
      mutate(supplysector = dplyr::first(A32.globaltech_coef$supplysector),
             subsector = dplyr::first(A32.globaltech_coef$subsector),
             technology = dplyr::first(A32.globaltech_coef$technology),
             # ^^ set variables equal to first row of A23.globaltech_coef (all are named simply "industry")
             water_sector = "Manufacturing",
             # (below) map in the the appropriate minicam.energy.input name...
             # for manufacturing sector and withdrawal/consumption partitioning
             minicam.energy.input =
               set_water_input_name(water_sector, water_type, A03.sector)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(market.name = region) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # ^^ expand for all model years
      select(region, supplysector, subsector, technology, year,
             minicam.energy.input, coefficient, market.name) %>%
      # add attributes for output
      add_title("Water withdrawal and consumption coefficients for manufacturing") %>%
      add_units("m3/GJ") %>%
      add_comments("Coeffients mapped to sector names and expanded to all regions and years") %>%
      add_legacy_name("L232.TechCoef") %>%
      add_precursors("common/GCAM_region_names",
                     "water/A03.sector",
                     "energy/A32.globaltech_coef",
                     "L132.water_coef_manufacturing_R_W_m3_GJ") ->
      L232.TechCoef

    return_data(L232.TechCoef)
  } else {
    stop("Unknown command")
  }
}
