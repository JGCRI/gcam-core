# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_transport_emissions_USA_xml
#'
#' Construct XML data structure for \code{transport_emissions_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transport_emissions_USA.xml}. The corresponding file in the
#' original data system was \code{transport_emissions_USA.xml} (gcamusa XML).

module_gcamusa_batch_transport_emissions_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L271.nonco2_trn_tech_coeff_USA",
             "L271.nonco2_trn_emiss_control_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "transport_emissions_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L271.nonco2_trn_tech_coeff_USA <- get_data(all_data, "L271.nonco2_trn_tech_coeff_USA")
    L271.nonco2_trn_emiss_control_USA <- get_data(all_data, "L271.nonco2_trn_emiss_control_USA")

    # ===================================================

    # Produce outputs
    create_xml("transport_emissions_USA.xml") %>%
      add_xml_data(L271.nonco2_trn_tech_coeff_USA, "TrnOutputEmissCoeff") %>%
      add_xml_data(L271.nonco2_trn_emiss_control_USA, "LinearCtrlInc") %>%
      add_precursors("L271.nonco2_trn_tech_coeff_USA",
                     "L271.nonco2_trn_emiss_control_USA") ->
      transport_emissions_USA.xml

    return_data(transport_emissions_USA.xml)
  } else {
    stop("Unknown command")
  }
}
