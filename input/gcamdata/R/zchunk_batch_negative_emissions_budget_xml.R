# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_negative_emissions_budget_xml
#'
#' Construct XML data structure for \code{paste0("negative_emissions_budget_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5)), ".xml")}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{paste0("negative_emissions_budget_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5)), ".xml")}. The corresponding file in the
#' original data system was \code{L270.limits.R} (energy XML).
module_energy_batch_negative_emissions_budget_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L270.CTaxInput",
             "L270.LandRootNegEmissMkt",
              "L270.NegEmissBudgetMaxPrice",
              paste0("L270.NegEmissBudget_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5))) ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    xml_files = paste0("negative_emissions_budget_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5)), ".xml")
    names(xml_files) <- rep("XML", length(xml_files))
    return(xml_files)
  } else if(command == driver.MAKE) {

    . <- NULL # silence package check note

    all_data <- list(...)[[1]]

    # Load required inputs
    L270.CTaxInput <- get_data(all_data, "L270.CTaxInput")
    L270.LandRootNegEmissMkt <- get_data(all_data, "L270.LandRootNegEmissMkt")
    L270.NegEmissBudgetMaxPrice <- get_data(all_data, "L270.NegEmissBudgetMaxPrice")

    # ===================================================

    scenarios <- c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5))
    xml_files = paste0("negative_emissions_budget_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5)), ".xml")

    # Produce outputs
    ret_data <- c()
    curr_env <- environment()
    for(scen in scenarios) {
      curr_data_name <- paste0("L270.NegEmissBudget_", scen)
      curr_xml_name <- paste0("negative_emissions_budget_", scen, ".xml")
      create_xml(curr_xml_name) %>%
        add_xml_data(L270.CTaxInput, "GlobalTechCTaxInput") %>%
        add_xml_data(L270.LandRootNegEmissMkt, "LandRootNegEmissMkt") %>%
        add_xml_data(L270.NegEmissBudgetMaxPrice, "PortfolioStdMaxPrice") %>%
        add_xml_data(get_data(all_data, curr_data_name), "PortfolioStd") %>%
        add_precursors("L270.CTaxInput", "L270.LandRootNegEmissMkt", "L270.NegEmissBudgetMaxPrice", curr_data_name) %>%
        assign(curr_xml_name, ., envir = curr_env)

      ret_data <- c(ret_data, curr_xml_name)
    }

    # Call return_data but we need to jump through some hoops since we generated the
    # xml from the scenarios so we will generate the call to return_data
    ret_data %>%
      paste(collapse = ", ") %>%
      paste0("return_data(", ., ")") %>%
      parse(text = .) %>%
      eval()
  } else {
    stop("Unknown command")
  }
}
