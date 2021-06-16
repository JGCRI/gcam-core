# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_batch_EFW_input_coefs_xml
#'
#' Construct XML data structure for \code{EFW_input_coefs.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{EFW_input_coefs.xml}.
module_water_batch_EFW_input_coefs_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L270.TechCoef_EFW"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "EFW_input_coefs.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L270.TechCoef_EFW <- get_data(all_data, "L270.TechCoef_EFW")

    # ===================================================

    # Produce outputs
    create_xml("EFW_input_coefs.xml") %>%
      add_xml_data(L270.TechCoef_EFW, "TechCoef") %>%
      add_precursors("L270.TechCoef_EFW") ->
      EFW_input_coefs.xml

    return_data(EFW_input_coefs.xml)
  } else {
    stop("Unknown command")
  }
}
