# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_climate_xml_magicc_xml
#'
#' Construct XML data structure for \code{magicc.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{magicc.xml}. The corresponding file in the
#' original data system was \code{batch_magicc.xml.R} (climate_xml XML).
module_climate_xml_magicc_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L200.MAGICC"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "magicc.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L200.MAGICC <- get_data(all_data, "L200.MAGICC")

    # ===================================================

    # Produce outputs
    create_xml("magicc.xml") %>%
      add_xml_data(L200.MAGICC, "MAGICC") %>%
      add_precursors("L200.MAGICC") ->
      magicc.xml

    return_data(magicc.xml)
  } else {
    stop("Unknown command")
  }
}
