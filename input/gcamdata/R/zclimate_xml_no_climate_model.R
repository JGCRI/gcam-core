# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_climate_no_climate_model_xml
#'
#' Construct XML data structure for \code{no_climate_model.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{no_climate_model.xml}, \code{disable_climate_model.xml}.
module_climate_no_climate_model_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c())
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "no_climate_model.xml",
             XML = "disable_climate_model.xml"))
  } else if(command == driver.MAKE) {

    # ===================================================

    no_climate_data <- tibble(no.climate.model = 1)
    delete_climate <- tibble(delete = 1)

    # Produce outputs
    create_xml("no_climate_model.xml") %>%
      add_xml_data(no_climate_data, "NoClimateModel") ->
      no_climate_model.xml

    create_xml("disable_climate_model.xml") %>%
      add_xml_data(delete_climate, "DeleteMAGICC") %>%
      add_xml_data(delete_climate, "DeleteHector") %>%
      add_xml_data(no_climate_data, "NoClimateModel") ->
      disable_climate_model.xml

    return_data(no_climate_model.xml, disable_climate_model.xml)
  } else {
    stop("Unknown command")
  }
}

