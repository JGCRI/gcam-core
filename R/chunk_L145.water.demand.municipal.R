#' module_water_L145.water.demand.municipal
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L145.municipal_water_R_W_Yh_km3}, \code{L145.municipal_water_cost_R_75USD_m3}, \code{L145.municipal_water_eff_R_Y}. The corresponding file in the
#' original data system was \code{L145.water.demand.municipal.R} (water level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_water_L145.water.demand.municipal_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
FILE = "common/GCAM_region_names",
FILE = "aglu/AGLU_ctry",
FILE = "water/FAO_municipal_water_AQUASTAT",
FILE = "water/IBNET_municipal_water_cost_USDm3",
FILE = "water/municipal_water_use_efficiency",
FILE = "water/manufacturing_water_mapping"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L145.municipal_water_R_W_Yh_km3",
"L145.municipal_water_cost_R_75USD_m3",
"L145.municipal_water_eff_R_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
  GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
  FAO_municipal_water_AQUASTAT <- get_data(all_data, "water/FAO_municipal_water_AQUASTAT")
  IBNET_municipal_water_cost_USDm3 <- get_data(all_data, "water/IBNET_municipal_water_cost_USDm3")
  municipal_water_use_efficiency <- get_data(all_data, "water/municipal_water_use_efficiency")
  manufacturing_water_mapping <- get_data(all_data, "water/manufacturing_water_mapping")

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
 add_legacy_name("L145.municipal_water_R_W_Yh_km3") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L145.municipal_water_R_W_Yh_km3
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L145.municipal_water_cost_R_75USD_m3") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L145.municipal_water_cost_R_75USD_m3
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L145.municipal_water_eff_R_Y") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L145.municipal_water_eff_R_Y

    return_data(L145.municipal_water_R_W_Yh_km3, L145.municipal_water_cost_R_75USD_m3, L145.municipal_water_eff_R_Y)
  } else {
    stop("Unknown command")
  }
}



