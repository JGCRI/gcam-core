# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_indenergy_emissions_USA_xml
#'
#' Construct XML data structure for \code{indenergy_emissions_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{indenergy_emissions_USA.xml}. The corresponding file in the
#' original data system was \code{indenergy_emissions_USA.xml} (gcamusa XML)

module_gcamusa_batch_indenergy_emissions_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L275.nonghg_indenergy_tech_coeff_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "indenergy_emissions_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L275.nonghg_indenergy_tech_coeff_USA <- get_data(all_data, "L275.nonghg_indenergy_tech_coeff_USA")

    # ===================================================

    # Produce outputs
    create_xml("indenergy_emissions_USA.xml") %>%
      add_xml_data(L275.nonghg_indenergy_tech_coeff_USA, "InputEmissCoeff") %>%
      add_precursors("L275.nonghg_indenergy_tech_coeff_USA") ->
      indenergy_emissions_USA.xml

    return_data(indenergy_emissions_USA.xml)
  } else {
    stop("Unknown command")
  }
}
