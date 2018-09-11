#' module_gcamusa_batch_industry_process_USA_xml
#'
#' Construct XML data structure for \code{industry_process_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' The generated outputs: \code{industry_process_USA.xml}.
#' The corresponding file in the original data system was \code{batch_industry_process_USA.xml} (gcamusa XML batch).
module_gcamusa_batch_industry_process_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2323.StubTechCoef_indproc_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "industry_process_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2323.StubTechCoef_indproc_USA <- get_data(all_data, "L2323.StubTechCoef_indproc_USA")

    # ===================================================

    # Produce outputs
    create_xml("industry_process_USA.xml") %>%
      add_xml_data(L2323.StubTechCoef_indproc_USA, "StubTechCoef") %>%
      add_precursors("L2323.StubTechCoef_indproc_USA") ->
      industry_process_USA.xml

    return_data(industry_process_USA.xml)
  } else {
    stop("Unknown command")
  }
}
