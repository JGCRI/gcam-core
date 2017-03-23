#' module_water_L132.water.demand.manufacturing
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L132.water_coef_manufacturing_R_W_m3_GJ}. The corresponding file in the
#' original data system was \code{L132.water.demand.manufacturing.R} (water level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_water_L132.water.demand.manufacturing_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
 "L1322.in_EJ_R_indenergy_F_Yh",
 "L1322.in_EJ_R_indfeed_F_Yh",
FILE = "water/manufacturing_water_mapping",
FILE = "water/manufacturing_water_data",
FILE = "water/manufacturing_water_ratios"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L132.water_coef_manufacturing_R_W_m3_GJ"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  L1322.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indenergy_F_Yh")
  L1322.in_EJ_R_indfeed_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indfeed_F_Yh")
  manufacturing_water_mapping <- get_data(all_data, "water/manufacturing_water_mapping")
  manufacturing_water_data <- get_data(all_data, "water/manufacturing_water_data")
  manufacturing_water_ratios <- get_data(all_data, "water/manufacturing_water_ratios")

    # ===================================================
    # TRANSLATED PROCESSING CODE GOES HERE...
    #
    # If you find a mistake/thing to update in the old code and
    # fixing it will change the output data, causing the tests to fail,
    # (i) open an issue on GitHub, (ii) consult with colleagues, and
    # then (iii) code a fix:
    #
    # if(OLD_DATA_SYSTEM_BEHAVIOR) {
    #   ... code that replicates old, incorrect behavior
    # } else {
    #   ... new code with a fix
    # }
    #
    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L132.water_coef_manufacturing_R_W_m3_GJ") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L132.water_coef_manufacturing_R_W_m3_GJ

    return_data(L132.water_coef_manufacturing_R_W_m3_GJ)
  } else {
    stop("Unknown command")
  }
}



