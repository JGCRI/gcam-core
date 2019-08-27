# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA112.U
#'
#' Assigning global uranium supply curve to GCAM_region_ID 1.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L112.RsrcCurves_Mt_R_U}. The corresponding file in the
#' original data system was \code{LA112.U.R} (energy level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom tidyr gather spread
#' @author HM April 2017
module_energy_LA112.U <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A12.U_curves"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L112.RsrcCurves_Mt_R_U"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A12.U_curves <- get_data(all_data, "energy/A12.U_curves")

    # Uranium supply curves, reprinting from input
    # Currently not built up from inventory data; just using GCAM 3.0 values
    # These were not disaggregated to regions in GCAM 3.0. Keeping this convention for now.
    # NOTE: Assigning global uranium supply curve to USA: GCAM_region_ID 1

    tibble(GCAM_region_ID = 1,
           resource = A12.U_curves$resource,
           subresource = A12.U_curves$subresource,
           grade = A12.U_curves$grade,
           extractioncost = A12.U_curves$extractioncost,
           available = A12.U_curves$available) %>%

      # Historical uranium prices (currently assumed at global level, so no level 1 processing necessary)

      # Produce outputs
      add_title("Uranium resource curves for global supply") %>%
      add_units("1975$/kgU; MtU") %>%
      add_comments("Uranium supply and extraction costs imported from GCAM 3.0 data") %>%
      add_comments("All global supply is assigned to a single region") %>%
      add_legacy_name("L112.RsrcCurves_Mt_R_U") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A12.U_curves") ->
      L112.RsrcCurves_Mt_R_U

    return_data(L112.RsrcCurves_Mt_R_U)
  } else {
    stop("Unknown command")
  }
}
