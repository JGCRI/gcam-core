#' module_energy_L202.Ccoef
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L202.CarbonCoef}. The corresponding file in the
#' original data system was \code{L202.Ccoef.R} (energy level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_energy_L202.Ccoef_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/A_PrimaryFuelCCoef",
             FILE = "emissions/fuel_to_Ccoef",
             "L102.Ccoef_kgCGJ_F_Yh",
             "L102.Ccoef_kgCGJ_R_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L202.CarbonCoef"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_PrimaryFuelCCoef <- get_data(all_data, "emissions/A_PrimaryFuelCCoef")
    fuel_to_Ccoef <- get_data(all_data, "emissions/fuel_to_Ccoef")
    L102.Ccoef_kgCGJ_F_Yh <- get_data(all_data, "L102.Ccoef_kgCGJ_F_Yh")
    L102.Ccoef_kgCGJ_R_F_Yh <- get_data(all_data, "L102.Ccoef_kgCGJ_R_F_Yh")

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
    #
    # NOTE: there are 'match' calls in this code. You probably want to use left_join_error_no_match
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
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
      add_legacy_name("L202.CarbonCoef") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L202.CarbonCoef

    return_data(L202.CarbonCoef)
  } else {
    stop("Unknown command")
  }
}
