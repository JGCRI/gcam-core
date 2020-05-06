# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_negative_emissions_budget_usa_xml
#'
#' Construct XML data structure for \code{paste0("negative_emissions_budget_USA_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5)), ".xml")}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{paste0("negative_emissions_budget_USA_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5)), ".xml")}.
module_gcamusa_batch_negative_emissions_budget_usa_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( paste0("L270.NegEmissBudget_USA_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5))),
           "L270.NegEmissBudgetMaxPrice_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    xml_files = paste0("negative_emissions_budget_USA_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5)), ".xml")
    names(xml_files) <- rep("XML", length(xml_files))
    return(xml_files)
  } else if(command == driver.MAKE) {

    . <- NULL # silence package check note

    all_data <- list(...)[[1]]

    # Load required inputs
    L270.NegEmissBudgetMaxPrice_USA <- get_data(all_data, "L270.NegEmissBudgetMaxPrice_USA")

    # ===================================================

    scenarios <- c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5))
    xml_files = paste0("negative_emissions_budget_USA_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5)), ".xml")

    # Produce outputs
    ret_data <- c()
    curr_env <- environment()
    for(scen in scenarios) {
      curr_data_name <- paste0("L270.NegEmissBudget_USA_", scen)
      curr_xml_name <- paste0("negative_emissions_budget_USA_", scen, ".xml")
      create_xml(curr_xml_name) %>%
        add_xml_data(get_data(all_data, curr_data_name), "PortfolioStd") %>%
        add_xml_data(L270.NegEmissBudgetMaxPrice_USA, "PortfolioStdMaxPrice") %>%
        add_precursors(curr_data_name,
                       "L270.NegEmissBudgetMaxPrice_USA") %>%
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
