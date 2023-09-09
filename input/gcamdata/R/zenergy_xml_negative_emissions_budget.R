# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_negative_emissions_budget_xml
#'
#' Construct XML data structure for \code{paste0("negative_emissions_budget_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5)), ".xml")}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{paste0("negative_emissions_budget_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5)), ".xml")}. The corresponding file in the
#' original data system was \code{L270.limits.R} (energy XML).
module_energy_negative_emissions_budget_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L270.CTaxInput",
             "L270.LandRootNegEmissMkt",
              "L270.NegEmissBudgetMaxPrice",
              "L270.NegEmissBudgetDefaultPrice",
              "L270.NegEmissBudget",
              "L270.NegEmissBudgetFraction"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("XML" = "negative_emissions_budget.xml"))
  } else if(command == driver.MAKE) {

    . <- NULL # silence package check note

    all_data <- list(...)[[1]]

    # Load required inputs
    L270.CTaxInput <- get_data(all_data, "L270.CTaxInput")
    L270.LandRootNegEmissMkt <- get_data(all_data, "L270.LandRootNegEmissMkt")
    L270.NegEmissBudgetMaxPrice <- get_data(all_data, "L270.NegEmissBudgetMaxPrice")
    L270.NegEmissBudgetDefaultPrice <- get_data(all_data, "L270.NegEmissBudgetDefaultPrice")
    L270.NegEmissBudget <- get_data(all_data, "L270.NegEmissBudget")
    L270.NegEmissBudgetFraction <- get_data(all_data, "L270.NegEmissBudgetFraction")

    # ===================================================

    create_xml("negative_emissions_budget.xml") %>%
      add_xml_data(L270.CTaxInput, "GlobalTechCTaxInput") %>%
      add_xml_data(L270.LandRootNegEmissMkt, "LandRootNegEmissMkt") %>%
      add_xml_data(L270.NegEmissBudgetMaxPrice, "PortfolioStdMaxPrice") %>%
      add_xml_data(L270.NegEmissBudgetDefaultPrice, "PortfolioStdFixedTax") %>%
      add_xml_data(L270.NegEmissBudget, "PortfolioStd") %>%
      add_xml_data(L270.NegEmissBudgetFraction, "NegEmissBudgetFraction") %>%
      add_precursors("L270.CTaxInput", "L270.LandRootNegEmissMkt", "L270.NegEmissBudgetMaxPrice", "L270.NegEmissBudget", "L270.NegEmissBudgetFraction",
                     "L270.NegEmissBudgetDefaultPrice") ->
      negative_emissions_budget.xml

    return_data(negative_emissions_budget.xml)
  } else {
    stop("Unknown command")
  }
}
