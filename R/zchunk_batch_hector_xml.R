#' module_climate_xml_batch_hector_xml
#'
#' Construct XML data structure for \code{hector.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{hector.xml}. The corresponding file in the
#' original data system was \code{batch_hector.xml.R} (climate_xml XML).
module_climate_xml_batch_hector_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L200.hector"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "hector.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L200.hector <- get_data(all_data, "L200.hector")

    # ===================================================

    # Produce outputs
    create_xml("hector.xml") %>%
      add_xml_data(L200.hector,"hector") %>%
      add_precursors("L200.hector") ->
      hector.xml

    return_data(hector.xml)
  } else {
    stop("Unknown command")
  }
}
