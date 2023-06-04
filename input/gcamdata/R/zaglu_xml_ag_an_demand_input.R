# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_ag_an_demand_input_xml
#'
#' Construct XML data structure for \code{ag_an_demand_input.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_an_demand_input.xml}. The corresponding file in the
#' original data system was \code{batch_demand_input_xml.R} (aglu XML).
module_aglu_ag_an_demand_input_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L203.Supplysector_demand",
      "L203.NestingSubsectorAll_demand_food",
      "L203.SubsectorAll_demand_food",
      "L203.SubsectorAll_demand_nonfood",
      "L203.StubTech_demand_food",
      "L203.StubTech_demand_nonfood",
      "L203.GlobalTechCoef_demand",
      "L203.GlobalTechShrwt_demand",
      "L203.StubTechProd_food",
      "L203.StubTechProd_nonfood_crop",
      "L203.StubTechProd_nonfood_meat",
      "L203.StubTechProd_For",
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
      "L203.NonStapleBaseService",
      "L203.GlobalTechInterp_demand")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ag_an_demand_input.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # ===================================================

    # Produce outputs
    create_xml("ag_an_demand_input.xml") %>%
      add_logit_tables_xml(L203.Supplysector_demand, "Supplysector") %>%
      add_logit_tables_xml_generate_levels(L203.SubsectorAll_demand_food,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTech_demand_food, "StubTech","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubTechProd_food, "StubTechProd", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L203.StubCalorieContent, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_logit_tables_xml(L203.NestingSubsectorAll_demand_food, "SubsectorAll", "SubsectorLogit") %>%
      add_logit_tables_xml(L203.SubsectorAll_demand_nonfood, "SubsectorAll", "SubsectorLogit") %>%
      add_xml_data(L203.StubTech_demand_nonfood, "StubTech") %>%
      add_xml_data(L203.GlobalTechCoef_demand, "GlobalTechCoef") %>%
      add_xml_data(L203.GlobalTechShrwt_demand, "GlobalTechShrwt") %>%
      add_xml_data(L203.GlobalTechInterp_demand, "GlobalTechInterp") %>%
      add_xml_data(L203.StubTechProd_nonfood_crop, "StubTechProd") %>%
      add_xml_data(L203.StubTechProd_nonfood_meat, "StubTechProd") %>%
      add_xml_data(L203.StubTechProd_For, "StubTechProd") %>%
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
      add_precursors("L203.Supplysector_demand","L203.NestingSubsectorAll_demand_food", "L203.SubsectorAll_demand_food",
                     "L203.SubsectorAll_demand_nonfood", "L203.StubTech_demand_food", "L203.StubTech_demand_nonfood",
                     "L203.GlobalTechCoef_demand", "L203.GlobalTechShrwt_demand", "L203.GlobalTechInterp_demand",
                     "L203.StubTechProd_food", "L203.StubTechProd_nonfood_crop", "L203.StubTechProd_nonfood_meat",
                     "L203.StubTechProd_For", "L203.StubCalorieContent", "L203.PerCapitaBased",
                     "L203.BaseService", "L203.IncomeElasticity", "L203.PriceElasticity",
                     "L203.SubregionalShares", "L203.DemandFunction_food", "L203.DemandStapleParams",
                     "L203.DemandNonStapleParams", "L203.DemandStapleRegBias", "L203.DemandNonStapleRegBias",
                     "L203.StapleBaseService", "L203.NonStapleBaseService") ->
      ag_an_demand_input.xml

    return_data("ag_an_demand_input.xml")
  } else {
    stop("Unknown command")
  }
}



