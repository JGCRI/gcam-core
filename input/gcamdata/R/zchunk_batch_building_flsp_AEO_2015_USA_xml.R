#' module_gcamusa_batch_building_flsp_AEO_2015_USA_xml
#'
#' Construct XML data structure for \code{building_flsp_AEO_2015_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' The generated outputs: \code{building_flsp_AEO_2015_USA.xml}.
#' The corresponding file in the original data system was \code{batch_building_flsp_USA_QER.xml} (gcamusa XML batch).
module_gcamusa_batch_building_flsp_AEO_2015_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L244.Floorspace_AEO_2015_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_flsp_AEO_2015_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L244.Floorspace_AEO_2015_USA <- get_data(all_data, "L244.Floorspace_AEO_2015_USA")

    # ===================================================

    # Produce outputs
    create_xml("building_flsp_AEO_2015_USA.xml") %>%
      add_xml_data(L244.Floorspace_AEO_2015_USA, "Floorspace") %>%
      add_precursors("L244.Floorspace_AEO_2015_USA") ->
      building_flsp_AEO_2015_USA.xml

    return_data(building_flsp_AEO_2015_USA.xml)
  } else {
    stop("Unknown command")
  }
}
