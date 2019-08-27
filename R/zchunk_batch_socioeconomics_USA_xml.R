# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_socioeconomics_USA_xml
#'
#' Construct XML data structure for \code{socioeconomics_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{socioeconomics_USA.xml}. The corresponding file in the
#' original data system was \code{batch_socioeconomics_USA.xml} (gcamusa XML).
module_gcamusa_batch_socioeconomics_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.Pop_GCAMUSA",
             "L201.BaseGDP_GCAMUSA",
             "L201.LaborForceFillout_GCAMUSA",
             "L201.LaborProductivity_GCAMUSA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "socioeconomics_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L201.Pop_GCAMUSA <- get_data(all_data, "L201.Pop_GCAMUSA")
    L201.BaseGDP_GCAMUSA <- get_data(all_data, "L201.BaseGDP_GCAMUSA")
    L201.LaborForceFillout_GCAMUSA <- get_data(all_data, "L201.LaborForceFillout_GCAMUSA")
    L201.LaborProductivity_GCAMUSA <- get_data(all_data, "L201.LaborProductivity_GCAMUSA")

    # ===================================================

    # Produce outputs
    create_xml("socioeconomics_USA.xml") %>%
      add_xml_data(L201.Pop_GCAMUSA, "Pop") %>%
      add_xml_data(L201.BaseGDP_GCAMUSA, "BaseGDP") %>%
      add_xml_data(L201.LaborForceFillout_GCAMUSA, "LaborForceFillout") %>%
      add_xml_data(L201.LaborProductivity_GCAMUSA, "LaborProductivity") %>%
      add_precursors("L201.Pop_GCAMUSA", "L201.BaseGDP_GCAMUSA", "L201.LaborForceFillout_GCAMUSA", "L201.LaborProductivity_GCAMUSA") ->
      socioeconomics_USA.xml

    return_data(socioeconomics_USA.xml)
  } else {
    stop("Unknown command")
  }
}
