#' module_water_batch_water_demand_primary_xml
#'
#' Construct XML data structure for \code{water_demand_primary.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_demand_primary.xml}. The corresponding file in the
#' original data system was \code{batch_water_demand_primary.xml.R} (water XML).
module_water_batch_water_demand_primary_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L210.TechCoef"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_demand_primary.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L210.TechCoef <- get_data(all_data, "L210.TechCoef")

    # ===================================================

    # Produce outputs
    create_xml("water_demand_primary.xml") %>%
      add_xml_data(L210.TechCoef, "TechCoef") %>%
      add_precursors("L210.TechCoef") ->
      water_demand_primary.xml

    return_data(water_demand_primary.xml)
  } else {
    stop("Unknown command")
  }
}
