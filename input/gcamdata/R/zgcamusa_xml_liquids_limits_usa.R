# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_liquids_limits_usa_xml
#'
#' Construct XML data structure for \code{liquids_limits.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{liquids_limits_USA.xml}.
module_gcamusa_liquids_limits_usa_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L270.CreditMkt_USA",
              "L270.CreditOutput_USA",
              "L270.CreditInput_elecS_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "liquids_limits_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L270.CreditMkt_USA <- get_data(all_data, "L270.CreditMkt_USA")
    L270.CreditOutput_USA <- get_data(all_data, "L270.CreditOutput_USA")
    L270.CreditInput_elecS_USA <- get_data(all_data, "L270.CreditInput_elecS_USA")

    # ===================================================

    # Produce outputs
    create_xml("liquids_limits_USA.xml") %>%
      add_xml_data(L270.CreditMkt_USA, "PortfolioStd") %>%
      add_xml_data(L270.CreditOutput_USA, "GlobalTechRESSecOut") %>%
      add_xml_data(L270.CreditInput_elecS_USA, "GlobalTechCoef") %>%
      add_precursors("L270.CreditMkt_USA",
                     "L270.CreditOutput_USA",
                     "L270.CreditInput_elecS_USA") ->
      liquids_limits_USA.xml

    return_data(liquids_limits_USA.xml)
  } else {
    stop("Unknown command")
  }
}
