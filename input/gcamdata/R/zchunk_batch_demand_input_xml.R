#' module_aglu_batch_demand_input_xml
#'
#' Construct XML data structure for \code{demand_input.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{demand_input.xml}. The corresponding file in the
#' original data system was \code{batch_demand_input_xml.R} (aglu XML).
module_aglu_batch_demand_input_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L203.Supplysector_demand",
              "L203.SubsectorAll_demand",
              "L203.StubTech_demand",
              "L203.GlobalTechCoef_demand",
              "L203.GlobalTechShrwt_demand",
              "L203.StubTechProd_food",
              "L203.StubTechProd_nonfood_crop",
              "L203.StubTechProd_nonfood_meat",
              "L203.StubTechProd_For",
              "L203.StubTechFixOut_exp",
              "L203.StubCalorieContent",
              "L203.PerCapitaBased",
              "L203.BaseService",
              "L203.IncomeElasticity",
              "L203.PriceElasticity",
              "L203.SubregionalShares",
              "L203.DemandFunction_food",
              "L203.DemandStapleParams",
              "L203.DemandNonStapleParams",
              "L203.DemandStapleRegBias",
              "L203.DemandNonStapleRegBias",
              "L203.StapleBaseService",
              "L203.NonStapleBaseService"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "demand_input.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L203.Supplysector_demand <- get_data(all_data, "L203.Supplysector_demand")
    L203.SubsectorAll_demand <- get_data(all_data, "L203.SubsectorAll_demand")
    L203.StubTech_demand <- get_data(all_data, "L203.StubTech_demand")
    L203.GlobalTechCoef_demand <- get_data(all_data, "L203.GlobalTechCoef_demand")
    L203.GlobalTechShrwt_demand <- get_data(all_data, "L203.GlobalTechShrwt_demand")
    L203.StubTechProd_food <- get_data(all_data, "L203.StubTechProd_food")
    L203.StubTechProd_nonfood_crop <- get_data(all_data, "L203.StubTechProd_nonfood_crop")
    L203.StubTechProd_nonfood_meat <- get_data(all_data, "L203.StubTechProd_nonfood_meat")
    L203.StubTechProd_For <- get_data(all_data, "L203.StubTechProd_For")
    L203.StubTechFixOut_exp <- get_data(all_data, "L203.StubTechFixOut_exp")
    L203.StubCalorieContent <- get_data(all_data, "L203.StubCalorieContent")
    L203.PerCapitaBased <- get_data(all_data, "L203.PerCapitaBased")
    L203.BaseService <- get_data(all_data, "L203.BaseService")
    L203.IncomeElasticity <- get_data(all_data, "L203.IncomeElasticity")
    L203.PriceElasticity <- get_data(all_data, "L203.PriceElasticity")
    L203.SubregionalShares <- get_data(all_data, "L203.SubregionalShares")
    L203.DemandFunction_food <- get_data(all_data, "L203.DemandFunction_food")
    L203.DemandStapleParams <- get_data(all_data, "L203.DemandStapleParams")
    L203.DemandNonStapleParams <- get_data(all_data, "L203.DemandNonStapleParams")
    L203.DemandStapleRegBias <- get_data(all_data, "L203.DemandStapleRegBias")
    L203.DemandNonStapleRegBias <- get_data(all_data, "L203.DemandNonStapleRegBias")
    L203.StapleBaseService <- get_data(all_data, "L203.StapleBaseService")
    L203.NonStapleBaseService <- get_data(all_data, "L203.NonStapleBaseService")

    # ===================================================

    # Produce outputs
    create_xml("demand_input.xml") %>%
      add_logit_tables_xml(L203.Supplysector_demand, "Supplysector") %>%
      add_logit_tables_xml(L203.SubsectorAll_demand, "SubsectorAll", "SubsectorLogit") %>%
      add_xml_data(L203.StubTech_demand, "StubTech") %>%
      add_xml_data(L203.GlobalTechCoef_demand, "GlobalTechCoef") %>%
      add_xml_data(L203.GlobalTechShrwt_demand, "GlobalTechShrwt") %>%
      add_xml_data(L203.StubTechProd_food, "StubTechProd") %>%
      add_xml_data(L203.StubTechProd_nonfood_crop, "StubTechProd") %>%
      add_xml_data(L203.StubTechProd_nonfood_meat, "StubTechProd") %>%
      add_xml_data(L203.StubTechProd_For, "StubTechProd") %>%
      add_xml_data(L203.StubTechFixOut_exp, "StubTechFixOut") %>%
      add_xml_data(L203.StubCalorieContent, "StubCalorieContent") %>%
      add_xml_data(L203.PerCapitaBased, "PerCapitaBased") %>%
      add_xml_data(L203.BaseService, "BaseService") %>%
      add_xml_data(L203.IncomeElasticity, "IncomeElasticity") %>%
      add_xml_data(L203.PriceElasticity, "PriceElasticity") %>%
      add_xml_data(L203.SubregionalShares, "SubregionalShares") %>%
      add_xml_data(L203.DemandFunction_food, "DemandFunction_food") %>%
      add_xml_data(L203.DemandStapleParams, "DemandStapleParams") %>%
      add_xml_data(L203.DemandNonStapleParams, "DemandNonStapleParams") %>%
      add_xml_data(L203.DemandStapleRegBias, "DemandStapleRegBias") %>%
      add_xml_data(L203.DemandNonStapleRegBias, "DemandNonStapleRegBias") %>%
      add_xml_data(L203.StapleBaseService, "StapleBaseService") %>%
      add_xml_data(L203.NonStapleBaseService, "NonStapleBaseService") %>%
      add_precursors("L203.Supplysector_demand", "L203.SubsectorAll_demand", "L203.StubTech_demand",
                     "L203.GlobalTechCoef_demand", "L203.GlobalTechShrwt_demand", "L203.StubTechProd_food",
                     "L203.StubTechProd_nonfood_crop", "L203.StubTechProd_nonfood_meat",
                     "L203.StubTechProd_For", "L203.StubTechFixOut_exp", "L203.StubCalorieContent", "L203.PerCapitaBased",
                     "L203.BaseService", "L203.IncomeElasticity", "L203.PriceElasticity",
                     "L203.SubregionalShares", "L203.DemandFunction_food", "L203.DemandStapleParams",
                     "L203.DemandNonStapleParams", "L203.DemandStapleRegBias", "L203.DemandNonStapleRegBias",
                     "L203.StapleBaseService", "L203.NonStapleBaseService") ->
      demand_input.xml

    return_data(demand_input.xml)
  } else {
    stop("Unknown command")
  }
}



