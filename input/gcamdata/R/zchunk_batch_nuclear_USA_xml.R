# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_nuclear_USA_xml
#'
#' Construct XML data structure for \code{nuclear_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{nuclear_USA.xml}.
#' The corresponding file in the original data system was \code{batch_nuclear_USA.xml} (gcamusa XML batch).
module_gcamusa_batch_nuclear_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2244.StubTechSCurve_nuc_gen2_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "nuclear_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2244.StubTechSCurve_nuc_gen2_USA <- get_data(all_data, "L2244.StubTechSCurve_nuc_gen2_USA")

    # ===================================================

    # Produce outputs
    create_xml("nuclear_USA.xml") %>%
      add_xml_data_generate_levels(L2244.StubTechSCurve_nuc_gen2_USA%>% rename(stub.technology = technology), "StubTechSCurve","subsector","nesting-subsector",1,FALSE) %>%
      add_precursors("L2244.StubTechSCurve_nuc_gen2_USA") ->
      nuclear_USA.xml

    return_data(nuclear_USA.xml)
  } else {
    stop("Unknown command")
  }
}
