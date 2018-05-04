#' module_water_batch_water_demand_industry_xml
#'
#' Construct XML data structure for \code{water_demand_industry.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_demand_industry.xml}. The corresponding file in the
#' original data system was \code{batch_water_demand_industry.xml.R} (water XML).
module_water_batch_water_demand_industry_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L232.TechCoef"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_demand_industry.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L232.TechCoef <- get_data(all_data, "L232.TechCoef")

    # ===================================================

    # Produce outputs
    create_xml("water_demand_industry.xml") %>%
      add_xml_data(L232.TechCoef, "TechCoef") %>%
      add_precursors("L232.TechCoef") ->
      water_demand_industry.xml

    return_data(water_demand_industry.xml)
  } else {
    stop("Unknown command")
  }
}
