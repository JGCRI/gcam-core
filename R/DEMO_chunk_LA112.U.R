#' module_energy_LA112.U_DEMO_DISABLED
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L112.RsrcCurves_Mt_R_U}. The corresponding file in the
#' original data system was \code{LA112.U.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @import dplyr
#' @importFrom tidyr gather spread
#' @author Author name(s)
#' @export
module_energy_LA112.U_DEMO_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A12.U_curves",
             "L113.RsrcCurves_EJ_R_MSW"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L112.RsrcCurves_Mt_R_U_DEMO"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    # DEMO: these are file inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A12.U_curves <- get_data(all_data, "energy/A12.U_curves")
    # DEMO: this data is produced elsewhere in the data system. It's guaranteed
    # that we won't be called until this is available
    L113.RsrcCurves_EJ_R_MSW <- get_data(all_data, "L113.RsrcCurves_EJ_R_MSW")

    # ===================================================
    # TRANSLATED PROCESSING CODE GOES HERE...
    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Remove FLAG_NO_TEST when an output dataset is ready
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # If no precursors (very rare) don't call `add_precursor` at all
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_legacy_name("L112.RsrcCurves_Mt_R_U_DEMO") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L112.RsrcCurves_Mt_R_U_DEMO

    return_data(L112.RsrcCurves_Mt_R_U_DEMO)
  } else {
    stop("Unknown command")
  }
}



