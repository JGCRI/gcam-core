#' module_water_batch_water_demand_municipal_xml
#'
#' Construct XML data structure for \code{water_demand_municipal.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_demand_municipal.xml}. The corresponding file in the
#' original data system was \code{batch_water_demand_municipal.xml.R} (water XML).
module_water_batch_water_demand_municipal_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L245.Supplysector",
              "L245.SubsectorLogit",
              "L245.SubsectorShrwtFllt",
              "L245.TechShrwt",
              "L245.TechCoef",
              "L245.TechCost",
              "L245.PerCapitaBased",
              "L245.BaseService",
              "L245.IncomeElasticity",
              "L245.PriceElasticity",
              "L245.aeei"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_demand_municipal.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L245.Supplysector <- get_data(all_data, "L245.Supplysector")
    L245.SubsectorLogit <- get_data(all_data, "L245.SubsectorLogit")
    L245.SubsectorShrwtFllt <- get_data(all_data, "L245.SubsectorShrwtFllt")
    L245.TechShrwt <- get_data(all_data, "L245.TechShrwt")
    L245.TechCoef <- get_data(all_data, "L245.TechCoef")
    L245.TechCost <- get_data(all_data, "L245.TechCost")
    L245.PerCapitaBased <- get_data(all_data, "L245.PerCapitaBased")
    L245.BaseService <- get_data(all_data, "L245.BaseService")
    L245.IncomeElasticity <- get_data(all_data, "L245.IncomeElasticity")
    L245.PriceElasticity <- get_data(all_data, "L245.PriceElasticity")
    L245.aeei <- get_data(all_data, "L245.aeei")

    # ===================================================

    # Produce outputs
    create_xml("water_demand_municipal.xml") %>%
      add_logit_tables_xml(L245.Supplysector,"Supplysector") %>%
      add_logit_tables_xml(L245.SubsectorLogit,"SubsectorLogit") %>%
      add_xml_data(L245.SubsectorShrwtFllt,"SubsectorShrwtFllt") %>%
      add_xml_data(L245.TechShrwt,"TechShrwt") %>%
      add_xml_data(L245.TechCoef,"TechCoef") %>%
      add_xml_data(L245.TechCost,"TechCost") %>%
      add_xml_data(L245.PerCapitaBased,"PerCapitaBased") %>%
      add_xml_data(L245.BaseService,"BaseService") %>%
      add_xml_data(L245.IncomeElasticity,"IncomeElasticity") %>%
      add_xml_data(L245.PriceElasticity,"PriceElasticity") %>%
      add_xml_data(L245.aeei,"aeei") %>%
      add_precursors("L245.Supplysector", "L245.SubsectorLogit", "L245.SubsectorShrwtFllt", "L245.TechShrwt", "L245.TechCoef", "L245.TechCost", "L245.PerCapitaBased", "L245.BaseService", "L245.IncomeElasticity", "L245.PriceElasticity", "L245.aeei") ->
      water_demand_municipal.xml

    return_data(water_demand_municipal.xml)
  } else {
    stop("Unknown command")
  }
}
