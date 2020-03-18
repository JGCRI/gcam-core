# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_elec_hydro_USA_xml
#'
#' Construct XML data structure for \code{elec_hydro_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{elec_hydro_USA.xml}.
#' The corresponding file in the original data system was \code{batch_elec_hydro_USA.xml} (gcamusa XML batch).
module_gcamusa_batch_elec_hydro_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2242.StubTechFixOut_hydro_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "elec_hydro_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2242.StubTechFixOut_hydro_USA <- get_data(all_data, "L2242.StubTechFixOut_hydro_USA")

    # ===================================================

    # Produce outputs
    create_xml("elec_hydro_USA.xml") %>%
      add_xml_data_generate_levels(L2242.StubTechFixOut_hydro_USA%>% rename(stub.technology = technology), "StubTechFixOut","subsector","nesting-subsector",1,FALSE) %>%
      add_precursors("L2242.StubTechFixOut_hydro_USA") ->
      elec_hydro_USA.xml

    return_data(elec_hydro_USA.xml)
  } else {
    stop("Unknown command")
  }
}
