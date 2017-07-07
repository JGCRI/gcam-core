#' module_emissions_L232.prc_nonco2
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L232.nonco2_prc}, \code{L232.nonco2_max_reduction}, \code{L232.nonco2_steepness}. The corresponding file in the
#' original data system was \code{L232.prc_nonco2.R} (emissions level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RH July 2017
#' @export
module_emissions_L232.prc_nonco2 <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/A_regions",
             "L131.nonco2_tg_R_prc_S_S_Yh",
             FILE = "emissions/A32.max_reduction", # Source and Units
             FILE = "emissions/A32.steepness")) # Source and Units
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L232.nonco2_prc",
             "L232.nonco2_max_reduction",
             "L232.nonco2_steepness"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_regions <- get_data(all_data, "emissions/A_regions")
    L131.nonco2_tg_R_prc_S_S_Yh <- get_data(all_data, "L131.nonco2_tg_R_prc_S_S_Yh")
    A32.max_reduction <- get_data(all_data, "emissions/A32.max_reduction")
    A32.steepness <- get_data(all_data, "emissions/A32.steepness")

    # ===================================================

    # ===================================================

    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.nonco2_prc") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "L131.nonco2_tg_R_prc_S_S_Yh",
                     "emissions/A32.max_reduction", "emissions/A32.steepness") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.nonco2_prc
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.nonco2_max_reduction") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "L131.nonco2_tg_R_prc_S_S_Yh",
                     "emissions/A32.max_reduction", "emissions/A32.steepness") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.nonco2_max_reduction
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L232.nonco2_steepness") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "L131.nonco2_tg_R_prc_S_S_Yh",
                     "emissions/A32.max_reduction", "emissions/A32.steepness") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L232.nonco2_steepness

    return_data(L232.nonco2_prc, L232.nonco2_max_reduction, L232.nonco2_steepness)
  } else {
    stop("Unknown command")
  }
}
