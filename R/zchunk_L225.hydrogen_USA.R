# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcam.usa_L225.hydrogen_USA
#'
#' Selects the subsectors to be removed from the hydrogen sectors for GCAM USA
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L225.DeleteSubsector_h2_USA}. The corresponding file in the
#' original data system was \code{L225.hydrogen_USA.R} (gcam-usa level2).
#' @details This chunk selects the subsectors to be removed from the hydrogen sectors in GCAM USA on the national level.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author KD September 2017
module_gcamusa_L225.hydrogen_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L225.SubsectorLogit_h2"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L225.DeleteSubsector_h2_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region <- subsector <- supplysector <- NULL  # silence package check notes

    # Load required inputs
    L225.SubsectorLogit_h2 <- get_data(all_data, "L225.SubsectorLogit_h2")

    # ===================================================
    # This chunk selects the subsectors to be removed from the
    # hydrogen sectors in GCAM USA on the national level.

    # Since there is no basis for inter-state competition in the hydrogen sector
    # keep the logit exponents for hydrogen at the national level for GCAM USA.
    # Select the wind, solar, and electricity subsectors because these resources do
    # not exists in the national level in GCAM USA.
    L225.SubsectorLogit_h2 %>%
      # Copy the region column to remove the attributes from the data frame.
      mutate(region = region) %>%
      filter(region == gcam.USA_REGION, subsector %in% c("wind", "solar", "electricity")) %>%
      select(region, supplysector, subsector) ->
      L225.DeleteSubsector_h2_USA

    # ===================================================

    # Produce outputs
    L225.DeleteSubsector_h2_USA %>%
      add_title("Subsector logit exponents of hydrogen sectors in the U.S. to be removed") %>%
      add_units("Unitless") %>%
      add_comments("Select the national subsector logit exponents to be excluded from GCAM USA") %>%
      add_legacy_name("L225.DeleteSubsector_h2_USA") %>%
      add_precursors("L225.SubsectorLogit_h2") ->
      L225.DeleteSubsector_h2_USA

    return_data(L225.DeleteSubsector_h2_USA)
  } else {
    stop("Unknown command")
  }
}
