# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_water_demand_industry_USA_xml
#'
#' Construct XML data structure for \code{water_demand_industry_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_demand_industry_USA.xml}.
module_gcamusa_batch_water_demand_industry_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L232.TechCoef_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_demand_industry_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L232.TechCoef_USA <- get_data(all_data, "L232.TechCoef_USA")

    # ===================================================

    # Produce outputs
    create_xml("water_demand_industry_USA.xml") %>%
      add_xml_data(L232.TechCoef_USA, "TechCoef") %>%
      add_precursors("L232.TechCoef_USA") ->
      water_demand_industry_USA.xml

    return_data(water_demand_industry_USA.xml)
  } else {
    stop("Unknown command")
  }
}
