# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_batch_electricity_water_coefs_xml
#'
#' Construct XML data structure for \code{electricity_water_coefs.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electricity_water_coefs.xml}. The corresponding file in the
#' original data system was \code{batch_electricity_water_coefs.xml.R} (water XML).
module_water_batch_electricity_water_coefs_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2233.GlobalTechCoef_elec_cool",
              "L2233.GlobalIntTechCoef_elec_cool"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electricity_water_coefs.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2233.GlobalTechCoef_elec_cool <- get_data(all_data, "L2233.GlobalTechCoef_elec_cool")
    L2233.GlobalIntTechCoef_elec_cool <- get_data(all_data, "L2233.GlobalIntTechCoef_elec_cool")

    # ===================================================

    # Produce outputs
    create_xml("electricity_water_coefs.xml") %>%
      add_xml_data(L2233.GlobalTechCoef_elec_cool, "GlobalTechCoef") %>%
      add_xml_data(L2233.GlobalIntTechCoef_elec_cool, "GlobalIntTechCoef") %>%
      add_precursors("L2233.GlobalTechCoef_elec_cool", "L2233.GlobalIntTechCoef_elec_cool") ->
      electricity_water_coefs.xml

    return_data(electricity_water_coefs.xml)
  } else {
    stop("Unknown command")
  }
}
