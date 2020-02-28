# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_coal_delay_USA_xml
#'
#' Construct XML data structure for \code{coal_delay_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{coal_delay_USA.xml}.
module_gcamusa_batch_coal_delay_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2231.StubTechShrwt_coal_delay_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "coal_delay_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2231.StubTechShrwt_coal_delay_USA <- get_data(all_data, "L2231.StubTechShrwt_coal_delay_USA")

    # Produce outputs
    create_xml("coal_delay_USA.xml") %>%
      add_xml_data(L2231.StubTechShrwt_coal_delay_USA, "StubTechShrwt") %>%
      add_precursors("L2231.StubTechShrwt_coal_delay_USA") ->
      coal_delay_USA.xml

    return_data(coal_delay_USA.xml)
  } else {
    stop("Unknown command")
  }
}
