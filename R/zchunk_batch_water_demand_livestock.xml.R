#' module_water_batch_water_demand_livestock.xml
#'
#' Construct XML data structure for \code{water_demand_livestock.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_demand_livestock.xml}. The corresponding file in the
#' original data system was \code{batch_water_demand_livestock.xml.R} (water XML).
module_water_batch_water_demand_livestock.xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L233.TechCoef"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_demand_livestock.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L233.TechCoef <- get_data(all_data, "L233.TechCoef")

    # ===================================================

    # Produce outputs
    create_xml("water_demand_livestock.xml") %>%
      add_xml_data(L233.TechCoef,"TechCoef") %>%
      add_precursors("L233.TechCoef") ->
      water_demand_livestock.xml

    return_data(water_demand_livestock.xml)
  } else {
    stop("Unknown command")
  }
}
