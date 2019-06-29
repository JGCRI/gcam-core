# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_en_transformation_low_xml
#'
#' Construct XML data structure for \code{en_transformation_low.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_transformation_low.xml}. The corresponding file in the
#' original data system was \code{batch_en_transformation_low.xml.R} (energy XML).
module_energy_batch_en_transformation_low_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L222.GlobalTechCost_low_en"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_transformation_low.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L222.GlobalTechCost_low_en <- get_data(all_data, "L222.GlobalTechCost_low_en")

    # ===================================================

    # Produce outputs
    create_xml("en_transformation_low.xml") %>%
      add_xml_data(L222.GlobalTechCost_low_en, "GlobalTechCost") %>%
      add_precursors("L222.GlobalTechCost_low_en") ->
      en_transformation_low.xml

    return_data(en_transformation_low.xml)
  } else {
    stop("Unknown command")
  }
}
