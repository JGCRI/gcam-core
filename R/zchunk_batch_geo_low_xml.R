#' module_energy_batch_geo_low_xml
#'
#' Construct XML data structure for \code{geo_low.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{geo_low.xml}. The corresponding file in the
#' original data system was \code{batch_geo_low_xml.R} (energy XML).
module_energy_batch_geo_low_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L223.GlobalTechCapital_geo_low"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "geo_low.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L223.GlobalTechCapital_geo_low <- get_data(all_data, "L223.GlobalTechCapital_geo_low")

    # ===================================================

    # Produce outputs
    create_xml("geo_low.xml") %>%
      add_xml_data(L223.GlobalTechCapital_geo_low,"GlobalTechCapital") %>%
      add_precursors("L223.GlobalTechCapital_geo_low") ->
      geo_low.xml

    return_data(geo_low.xml)
  } else {
    stop("Unknown command")
  }
}
