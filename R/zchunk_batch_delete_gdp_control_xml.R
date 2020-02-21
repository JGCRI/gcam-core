# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_batch_delete_gdp_control_xml
#'
#' Construct XML data structure for \code{delete_gdp_control.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{delete_gdp_control.xml}. The corresponding file in the
#' original data system was \code{batch_delete_gdp_control_xml.R} (emissions XML).
module_emissions_batch_delete_gdp_control_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L251.ctrl.delete"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "delete_gdp_control.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L251.ctrl.delete <- get_data(all_data, "L251.ctrl.delete")

    # ===================================================

    # Produce outputs
    create_xml("delete_gdp_control.xml") %>%
      add_xml_data(L251.ctrl.delete, "DelEmCtrl") %>%
      add_precursors("L251.ctrl.delete") ->
      delete_gdp_control.xml

    return_data(delete_gdp_control.xml)
  } else {
    stop("Unknown command")
  }
}
