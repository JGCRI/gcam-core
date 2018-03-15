#' module_water_batch_water_elec_emissions_xml
#'
#' Construct XML data structure for \code{water_elec_emissions.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_elec_emissions.xml}. The corresponding file in the
#' original data system was \code{batch_water_elec_emissions.xml.R} (water XML).
module_water_batch_water_elec_emissions_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2233.InputEmissCoeff_hist_elecPassthru",
              "L2233.InputEmissCoeff_fut_elecPassthru"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_elec_emissions.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2233.InputEmissCoeff_hist_elecPassthru <- get_data(all_data, "L2233.InputEmissCoeff_hist_elecPassthru")
    L2233.InputEmissCoeff_fut_elecPassthru <- get_data(all_data, "L2233.InputEmissCoeff_fut_elecPassthru")

    # ===================================================

    # Produce outputs
    create_xml("water_elec_emissions.xml") %>%
      add_xml_data(L2233.InputEmissCoeff_hist_elecPassthru,"InputEmissCoeff") %>%
      add_xml_data(L2233.InputEmissCoeff_fut_elecPassthru,"InputEmissCoeff") %>%
      add_precursors("L2233.InputEmissCoeff_hist_elecPassthru", "L2233.InputEmissCoeff_fut_elecPassthru") ->
      water_elec_emissions.xml

    return_data(water_elec_emissions.xml)
  } else {
    stop("Unknown command")
  }
}
