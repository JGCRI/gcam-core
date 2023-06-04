# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_negative_emissions_budget_usa_xml
#'
#' Construct XML data structure for \code{paste0("negative_emissions_budget_USA_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5)), ".xml")}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{paste0("negative_emissions_budget_USA_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5)), ".xml")}.
module_gcamusa_negative_emissions_budget_usa_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L270.NegEmissBudget_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("XML" = "negative_emissions_budget_USA.xml"))
  } else if(command == driver.MAKE) {

    . <- NULL # silence package check note

    all_data <- list(...)[[1]]

    # Load required inputs
    L270.NegEmissBudget_USA <- get_data(all_data, "L270.NegEmissBudget_USA")

    # ===================================================

    create_xml("negative_emissions_budget_USA.xml") %>%
      add_xml_data(L270.NegEmissBudget_USA, "PortfolioStd") %>%
      add_precursors("L270.NegEmissBudget_USA") ->
      negative_emissions_budget_USA.xml

    return_data(negative_emissions_budget_USA.xml)
  } else {
    stop("Unknown command")
  }
}
