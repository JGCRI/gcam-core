#' module_energy_batch_Ccoef_xml
#'
#' Construct XML data structure for \code{Ccoef.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{Ccoef.xml}. The corresponding file in the
#' original data system was \code{batch_Ccoef_xml.R} (energy XML).
module_energy_batch_Ccoef_xml_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L202.CarbonCoef"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "Ccoef.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L202.CarbonCoef <- get_data(all_data, "L202.CarbonCoef")

    # ===================================================

    # Produce outputs
    create_xml("Ccoef.xml") %>%
      add_xml_data(L202.CarbonCoef, "CarbonCoef") %>%
      add_precursors("L202.CarbonCoef") ->
      Ccoef.xml

    return_data(Ccoef.xml)
  } else {
    stop("Unknown command")
  }
}
