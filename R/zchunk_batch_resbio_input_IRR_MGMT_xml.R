#' module_aglu_batch_resbio_input_IRR_MGMT_xml
#'
#' Construct XML data structure for \code{resbio_input_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{resbio_input_IRR_MGMT.xml}. The corresponding file in the
#' original data system was \code{batch_resbio_input_IRR_MGMT_xml.R} (aglu XML).
module_aglu_batch_resbio_input_IRR_MGMT_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L2042.AgResBioCurve_For",
              "L2042.GlobalResBio_Mill",
              "L2042.StubResBioCurve_Mill",
              "L2042.AgResBio_ag_irr_mgmt",
              "L2042.AgResBioCurve_ag_irr_mgmt"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "resbio_input_IRR_MGMT.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2042.AgResBioCurve_For <- get_data(all_data, "L2042.AgResBioCurve_For")
    L2042.GlobalResBio_Mill <- get_data(all_data, "L2042.GlobalResBio_Mill")
    L2042.StubResBioCurve_Mill <- get_data(all_data, "L2042.StubResBioCurve_Mill")
    L2042.AgResBio_ag_irr_mgmt <- get_data(all_data, "L2042.AgResBio_ag_irr_mgmt")
    L2042.AgResBioCurve_ag_irr_mgmt <- get_data(all_data, "L2042.AgResBioCurve_ag_irr_mgmt")

    # ===================================================

    # Produce outputs
    create_xml("resbio_input_IRR_MGMT.xml") %>%
      add_xml_data(L2042.AgResBioCurve_For,"AgResBioCurve") %>%
      add_xml_data(L2042.GlobalResBio_Mill,"GlobalResBio") %>%
      add_xml_data(L2042.StubResBioCurve_Mill,"StubResBioCurve") %>%
      add_xml_data(L2042.AgResBio_ag_irr_mgmt,"AgResBio") %>%
      add_xml_data(L2042.AgResBioCurve_ag_irr_mgmt,"AgResBioCurve") %>%
      add_precursors("L2042.AgResBioCurve_For", "L2042.GlobalResBio_Mill", "L2042.StubResBioCurve_Mill", "L2042.AgResBio_ag_irr_mgmt", "L2042.AgResBioCurve_ag_irr_mgmt") ->
      resbio_input_IRR_MGMT.xml

    return_data(resbio_input_IRR_MGMT.xml)
  } else {
    stop("Unknown command")
  }
}
