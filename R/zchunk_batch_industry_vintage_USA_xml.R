# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_industry_vintage_USA_xml
#'
#' Construct XML data structure for \code{industry_vintage_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{industry_vintage_USA.xml}. The corresponding file in the
#' original data system was \code{batch_industry_vintage_USA.xml.R} (gcamusa XML).
module_gcamusa_batch_industry_vintage_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2322.StubTechSCurve_industry_USA",
             "L2322.StubTechProfitShutdown_industry_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "industry_vintage_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2322.StubTechSCurve_industry_USA <- get_data(all_data, "L2322.StubTechSCurve_industry_USA")
    L2322.StubTechProfitShutdown_industry_USA <- get_data(all_data, "L2322.StubTechProfitShutdown_industry_USA")

    # ===================================================

    # Produce outputs
    create_xml("industry_vintage_USA.xml") %>%
      add_xml_data(L2322.StubTechSCurve_industry_USA, "StubTechSCurve") %>%
      add_xml_data(L2322.StubTechProfitShutdown_industry_USA, "StubTechProfitShutdown") %>%
      add_precursors("L2322.StubTechSCurve_industry_USA",
                     "L2322.StubTechProfitShutdown_industry_USA") ->
      industry_vintage_USA.xml

    return_data(industry_vintage_USA.xml)
  } else {
    stop("Unknown command")
  }
}
