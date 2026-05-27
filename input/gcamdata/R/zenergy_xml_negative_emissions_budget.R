# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_negative_emissions_budget_xml
#'
#' Construct XML data structure for \code{paste0("negative_emissions_budget.xml")}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{negative_emissions_budget.xml}. The corresponding file in the
#' original data system was \code{L270.limits.R} (energy XML).
module_energy_negative_emissions_budget_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L270.CTaxInput",
      "L270.LandRootNegEmissMkt",
      "L270.NegEmissBudgetMaxPrice",
      "L270.NegEmissBudgetDefaultPrice",
      "L270.NegEmissBudget",
      "L270.NegEmissBudgetFraction")

  MODULE_OUTPUTS <-
    c(XML = "negative_emissions_budget.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    . <- NULL # silence package check note

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # ===================================================

    create_xml("negative_emissions_budget.xml") %>%
      add_xml_data(L270.CTaxInput, "GlobalTechCTaxInput") %>%
      add_xml_data(L270.LandRootNegEmissMkt, "LandRootNegEmissMkt") %>%
      add_xml_data(L270.NegEmissBudgetMaxPrice, "PortfolioStdMaxPrice") %>%
      add_xml_data(L270.NegEmissBudgetDefaultPrice, "PortfolioStdFixedTax") %>%
      add_xml_data(L270.NegEmissBudget, "PortfolioStd") %>%
      add_xml_data(L270.NegEmissBudgetFraction, "NegEmissBudgetFraction") %>%
      add_precursors(MODULE_INPUTS) ->
      negative_emissions_budget.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
