#' module_energy_batch_resources_SSP_xml
#'
#' Construct XML data structure for \code{resources_SSP5.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{resources_SSP5.xml}. The corresponding file in the
#' original data system was \code{batch_resources_SSP5_xml.R} (energy XML).
module_energy_batch_resources_SSP_xml_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L210.DepRsrcTechChange_SSP5",
             "L210.DepRsrcEnvironCost_SSP5"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "resources_SSP5.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L210.DepRsrcTechChange_SSP5 <- get_data(all_data, "L210.DepRsrcTechChange_SSP5")
    L210.DepRsrcEnvironCost_SSP5 <- get_data(all_data, "L210.DepRsrcEnvironCost_SSP5")

    # ===================================================

    # Produce outputs
    create_xml("resources_SSP5.xml") %>%
      add_xml_data(L210.DepRsrcTechChange_SSP5,"DepRsrcTechChange") %>%
      add_xml_data(L210.DepRsrcEnvironCost_SSP5,"DepRsrcEnvironCost") %>%
      add_precursors("L210.DepRsrcTechChange_SSP5", "L210.DepRsrcEnvironCost_SSP5") ->
      resources_SSP5.xml

    return_data(resources_SSP5.xml)
  } else {
    stop("Unknown command")
  }
}
