#' module_gcam.usa_LA101.EIA_SEDS
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L101.EIA_use_all_Bbtu}, \code{L101.inEIA_EJ_state_S_F}. The corresponding file in the
#' original data system was \code{LA101.EIA_SEDS.R} (gcam-usa level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_LA101.EIA_SEDS_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/EIA_SEDS_fuels",
FILE = "gcam-usa/EIA_SEDS_sectors",
FILE = "gcam-usa/EIA_use_all_Bbtu",
FILE = "gcam-usa/A_fuel_conv"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.EIA_use_all_Bbtu",
"L101.inEIA_EJ_state_S_F"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      EIA_SEDS_fuels <- get_data(all_data, "gcam-usa/EIA_SEDS_fuels")
  EIA_SEDS_sectors <- get_data(all_data, "gcam-usa/EIA_SEDS_sectors")
  EIA_use_all_Bbtu <- get_data(all_data, "gcam-usa/EIA_use_all_Bbtu")
  A_fuel_conv <- get_data(all_data, "gcam-usa/A_fuel_conv")

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
 add_legacy_name("L101.EIA_use_all_Bbtu") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L101.EIA_use_all_Bbtu
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L101.inEIA_EJ_state_S_F") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L101.inEIA_EJ_state_S_F

    return_data(L101.EIA_use_all_Bbtu, L101.inEIA_EJ_state_S_F)
  } else {
    stop("Unknown command")
  }
}



