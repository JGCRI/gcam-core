# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_water_demand_municipal_xml
#'
#' Construct XML data structure for \code{water_demand_municipal.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_demand_municipal_USA.xml}.
module_gcamusa_batch_water_demand_municipal_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L245.Supplysector_USA",
              "L245.SubsectorLogit_USA",
              "L245.SubsectorShrwtFllt_USA",
              "L245.TechShrwt_USA",
              "L245.TechCoef_USA",
              "L245.TechCost_USA",
              "L245.DeleteSupplysector_USA",
              "L245.DeleteFinalDemand_USA",
              "L245.PerCapitaBased_USA",
              "L245.BaseService_USA",
              "L245.IncomeElasticity_USA",
              "L245.PriceElasticity_USA",
              "L245.aeei_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_demand_municipal_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L245.Supplysector_USA <- get_data(all_data, "L245.Supplysector_USA")
    L245.SubsectorLogit_USA <- get_data(all_data, "L245.SubsectorLogit_USA")
    L245.SubsectorShrwtFllt_USA <- get_data(all_data, "L245.SubsectorShrwtFllt_USA")
    L245.TechShrwt_USA <- get_data(all_data, "L245.TechShrwt_USA")
    L245.TechCoef_USA <- get_data(all_data, "L245.TechCoef_USA")
    L245.TechCost_USA <- get_data(all_data, "L245.TechCost_USA")
    L245.DeleteSupplysector_USA <- get_data(all_data, "L245.DeleteSupplysector_USA")
    L245.DeleteFinalDemand_USA <- get_data(all_data, "L245.DeleteFinalDemand_USA")
    L245.PerCapitaBased_USA <- get_data(all_data, "L245.PerCapitaBased_USA")
    L245.BaseService_USA <- get_data(all_data, "L245.BaseService_USA")
    L245.IncomeElasticity_USA <- get_data(all_data, "L245.IncomeElasticity_USA")
    L245.PriceElasticity_USA <- get_data(all_data, "L245.PriceElasticity_USA")
    L245.aeei_USA <- get_data(all_data, "L245.aeei_USA")

    # ===================================================

    # Produce outputs
    create_xml("water_demand_municipal_USA.xml") %>%
      add_logit_tables_xml(L245.Supplysector_USA, "Supplysector") %>%
      add_logit_tables_xml(L245.SubsectorLogit_USA, "SubsectorLogit") %>%
      add_xml_data(L245.SubsectorShrwtFllt_USA, "SubsectorShrwtFllt") %>%
      add_xml_data(L245.TechShrwt_USA, "TechShrwt") %>%
      add_xml_data(L245.TechCoef_USA, "TechCoef") %>%
      add_xml_data(L245.TechCost_USA, "TechCost") %>%
      add_xml_data(L245.DeleteSupplysector_USA, "DeleteSupplysector") %>%
      add_xml_data(L245.DeleteFinalDemand_USA, "DeleteFinalDemand") %>%
      add_xml_data(L245.PerCapitaBased_USA, "PerCapitaBased") %>%
      add_xml_data(L245.BaseService_USA, "BaseService") %>%
      add_xml_data(L245.IncomeElasticity_USA, "IncomeElasticity") %>%
      add_xml_data(L245.PriceElasticity_USA, "PriceElasticity") %>%
      add_xml_data(L245.aeei_USA, "aeei") %>%
      add_precursors("L245.Supplysector_USA", "L245.SubsectorLogit_USA", "L245.SubsectorShrwtFllt_USA",
                     "L245.TechShrwt_USA", "L245.TechCoef_USA", "L245.TechCost_USA", "L245.DeleteSupplysector_USA","L245.DeleteFinalDemand_USA", "L245.PerCapitaBased_USA",
                     "L245.BaseService_USA", "L245.IncomeElasticity_USA", "L245.PriceElasticity_USA", "L245.aeei_USA") ->
      water_demand_municipal_USA.xml

    return_data(water_demand_municipal_USA.xml)
  } else {
    stop("Unknown command")
  }
}
