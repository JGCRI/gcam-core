# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_coal_delay_xml
#'
#' Construct XML data structure for \code{coal_delay_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{coal_delay_USA.xml}.
module_gcamusa_coal_delay_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2231.SubsectorShrwt_coal_delay_elecS_cool_USA",
             "L2231.StubTechShrwt_coal_delay_nongen_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "coal_delay_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2231.SubsectorShrwt_coal_delay_elecS_cool_USA <- get_data(all_data, "L2231.SubsectorShrwt_coal_delay_elecS_cool_USA")
    L2231.StubTechShrwt_coal_delay_nongen_USA <- get_data(all_data, "L2231.StubTechShrwt_coal_delay_nongen_USA")

    # Produce outputs
    create_xml("coal_delay_USA.xml") %>%
      add_xml_data(L2231.StubTechShrwt_coal_delay_nongen_USA, "StubTechShrwt") %>%
      add_xml_data_generate_levels(L2231.SubsectorShrwt_coal_delay_elecS_cool_USA, "SubsectorShrwt", "subsector", "nesting-subsector", 1, FALSE) %>%
      add_precursors("L2231.StubTechShrwt_coal_delay_nongen_USA",
                     "L2231.SubsectorShrwt_coal_delay_elecS_cool_USA") ->
      coal_delay_USA.xml

    return_data(coal_delay_USA.xml)
  } else {
    stop("Unknown command")
  }
}
