# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_geo_tech_adv_xml
#'
#' Construct XML data structure for \code{geo_tech_adv.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{geo_tech_adv.xml}. The corresponding file in the
#' original data system was \code{batch_geo_tech_adv_xml.R} (energy XML).
module_energy_geo_tech_adv_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L223.GlobalTechCapital_geo_adv"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "geo_tech_adv.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L223.GlobalTechCapital_geo_adv <- get_data(all_data, "L223.GlobalTechCapital_geo_adv")

    # ===================================================

    # Produce outputs
    create_xml("geo_tech_adv.xml") %>%
      add_xml_data(L223.GlobalTechCapital_geo_adv, "GlobalTechCapital") %>%
      add_precursors("L223.GlobalTechCapital_geo_adv") ->
      geo_tech_adv.xml

    return_data(geo_tech_adv.xml)
  } else {
    stop("Unknown command")
  }
}
