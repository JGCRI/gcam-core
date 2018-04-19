#' module_emissions_batch_all_fgas_emissions_xml
#'
#' Construct XML data structure for \code{all_fgas_emissions.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{all_fgas_emissions.xml}. The corresponding file in the
#' original data system was \code{batch_all_fgas_emissions.xml} (emissions XML).
module_emissions_batch_all_fgas_emissions_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L241.hfc_all",
             "L241.pfc_all",
             "L241.hfc_future",
             "L241.fgas_all_units",
             "L252.MAC_higwp"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "all_fgas_emissions.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L241.hfc_all <- get_data(all_data, "L241.hfc_all")
    L241.pfc_all <- get_data(all_data, "L241.pfc_all")
    L241.hfc_future <- get_data(all_data, "L241.hfc_future")
    L241.fgas_all_units <- get_data(all_data, "L241.fgas_all_units")
    L252.MAC_higwp <- get_data(all_data, "L252.MAC_higwp")

    # ===================================================

    # Produce outputs
    create_xml("all_fgas_emissions.xml") %>%
      add_xml_data(L241.hfc_all, "StbTechOutputEmissions") %>%
      add_xml_data(L241.pfc_all, "StbTechOutputEmissions") %>%
      add_xml_data(L241.hfc_future, "OutputEmissCoeff") %>%
      add_xml_data(L241.fgas_all_units, "StubTechEmissUnits") %>%
      add_xml_data(L252.MAC_higwp, "MAC") %>%
      add_precursors("L241.hfc_all", "L241.pfc_all",
                     "L241.hfc_future", "L241.fgas_all_units","L252.MAC_higwp") ->
      all_fgas_emissions.xml

    return_data(all_fgas_emissions.xml)
  } else {
    stop("Unknown command")
  }
}
