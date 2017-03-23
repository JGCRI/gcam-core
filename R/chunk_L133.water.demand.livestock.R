#' module_water_L133.water.demand.livestock
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L133.water_demand_livestock_R_C_W_km3_Mt}. The corresponding file in the
#' original data system was \code{L133.water.demand.livestock.R} (water level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_water_L133.water.demand.livestock_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
 "L105.an_Prod_Mt_R_C_Y",
FILE = "water/LivestockWaterFootprint_MH2010",
FILE = "water/FAO_an_items_Stocks",
 "L100.FAO_an_Stocks",
 "L100.FAO_an_Dairy_Stocks"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L133.water_demand_livestock_R_C_W_km3_Mt"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
  L105.an_Prod_Mt_R_C_Y <- get_data(all_data, "L105.an_Prod_Mt_R_C_Y")
  LivestockWaterFootprint_MH2010 <- get_data(all_data, "water/LivestockWaterFootprint_MH2010")
  FAO_an_items_Stocks <- get_data(all_data, "water/FAO_an_items_Stocks")
  L100.FAO_an_Stocks <- get_data(all_data, "L100.FAO_an_Stocks")
  L100.FAO_an_Dairy_Stocks <- get_data(all_data, "L100.FAO_an_Dairy_Stocks")

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
 add_legacy_name("L133.water_demand_livestock_R_C_W_km3_Mt") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L133.water_demand_livestock_R_C_W_km3_Mt

    return_data(L133.water_demand_livestock_R_C_W_km3_Mt)
  } else {
    stop("Unknown command")
  }
}



