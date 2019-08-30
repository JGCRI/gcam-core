# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_HDDCDD_constdds_USA_xml
#'
#' Construct XML data structure for \code{HDDCDD_constdds_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{HDDCDD_constdds_USA.xml}.
module_gcamusa_batch_HDDCDD_constdds_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L244.HDDCDD_constdds_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "HDDCDD_constdds_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L244.HDDCDD_constdds_USA <- get_data(all_data, "L244.HDDCDD_constdds_USA")

    # Produce outputs
    create_xml("HDDCDD_constdds_USA.xml") %>%
      add_xml_data(L244.HDDCDD_constdds_USA, "HDDCDD") %>%
      add_precursors("L244.HDDCDD_constdds_USA") ->
      HDDCDD_constdds_USA.xml

    return_data(HDDCDD_constdds_USA.xml)
  } else {
    stop("Unknown command")
  }
}
