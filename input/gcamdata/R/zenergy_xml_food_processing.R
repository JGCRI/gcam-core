# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_food_processing_xml
#'
#' Construct XML data structure for \code{food_processing.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{food_processing.xml}. The corresponding file in the
#' original data system was \code{batch_food_processing_xml.R} (energy XML).
module_energy_food_processing_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2328.Supplysector_food",
             "L2328.FinalEnergyKeyword_food",
             "L2328.SubsectorLogit_food",
             "L2328.SubsectorShrwtFllt_food",
             "L2328.SubsectorInterp_food",
             "L2328.StubTech_food",
             "L2328.GlobalTechShrwt_food",
             "L2328.GlobalTechCoef_food",
             "L2328.GlobalTechCost_food",
             "L2328.StubTechCost_food",
             "L2328.GlobalTechTrackCapital_food",
             "L2328.GlobalTechSCurve_food",
             "L2328.GlobalTechProfitShutdown_food",
             "L2328.StubTechProd_food",
             "L2328.StubTechCalInput_food_heat",
             "L2328.StubTechCoef_food",
             "L2328.GlobalTechSecOut_food",
             "L2328.StubCalorieContent",
             "L2328.StubCaloriePriceConv"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "food_processing.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2328.Supplysector_food <- get_data(all_data, "L2328.Supplysector_food")
    L2328.FinalEnergyKeyword_food <- get_data(all_data, "L2328.FinalEnergyKeyword_food")
    L2328.SubsectorLogit_food <- get_data(all_data, "L2328.SubsectorLogit_food")
    L2328.SubsectorShrwtFllt_food <- get_data(all_data, "L2328.SubsectorShrwtFllt_food")
    L2328.SubsectorInterp_food <- get_data(all_data, "L2328.SubsectorInterp_food")
    L2328.StubTech_food <- get_data(all_data, "L2328.StubTech_food")
    L2328.GlobalTechShrwt_food <- get_data(all_data, "L2328.GlobalTechShrwt_food")
    L2328.GlobalTechCoef_food <- get_data(all_data, "L2328.GlobalTechCoef_food")
    L2328.GlobalTechCost_food <- get_data(all_data, "L2328.GlobalTechCost_food")
    L2328.StubTechCost_food <- get_data(all_data, "L2328.StubTechCost_food")
    L2328.GlobalTechTrackCapital_food <- get_data(all_data, "L2328.GlobalTechTrackCapital_food")
    L2328.GlobalTechSCurve_food <- get_data(all_data, "L2328.GlobalTechSCurve_food")
    L2328.GlobalTechProfitShutdown_food <- get_data(all_data, "L2328.GlobalTechProfitShutdown_food")
    L2328.StubTechProd_food <- get_data(all_data, "L2328.StubTechProd_food")
    L2328.StubTechCalInput_food_heat <- get_data(all_data, "L2328.StubTechCalInput_food_heat")
    L2328.StubTechCoef_food <- get_data(all_data, "L2328.StubTechCoef_food")
    L2328.GlobalTechSecOut_food <- get_data(all_data, "L2328.GlobalTechSecOut_food")
    L2328.StubCalorieContent <- get_data(all_data, "L2328.StubCalorieContent")
    L2328.StubCaloriePriceConv <- get_data(all_data, "L2328.StubCaloriePriceConv")

    # ===================================================

    # Produce outputs
    create_xml("food_processing.xml") %>%
      add_logit_tables_xml(L2328.Supplysector_food, "Supplysector") %>%
      add_xml_data(L2328.FinalEnergyKeyword_food, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2328.SubsectorLogit_food, "SubsectorLogit") %>%
      add_xml_data(L2328.SubsectorShrwtFllt_food, "SubsectorShrwtFllt") %>%
      add_xml_data(L2328.SubsectorInterp_food, "SubsectorInterp") %>%
      add_xml_data(L2328.StubTech_food, "StubTech") %>%
      add_xml_data(L2328.GlobalTechShrwt_food, "GlobalTechShrwt") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L2328.GlobalTechCoef_food, "GlobalTechCoef") %>%
      add_xml_data(L2328.GlobalTechSCurve_food, "GlobalTechSCurve") %>%
      add_xml_data(L2328.GlobalTechProfitShutdown_food, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2328.GlobalTechTrackCapital_food, "GlobalTechTrackCapital") %>%
      add_xml_data(L2328.GlobalTechCost_food, "GlobalTechCost") %>%
      add_xml_data(L2328.StubTechCost_food, "StubTechCost") %>%
      add_xml_data(L2328.StubTechProd_food, "StubTechProd") %>%
      add_xml_data(L2328.StubTechCalInput_food_heat, "StubTechCalInput") %>%
      add_xml_data(L2328.StubTechCoef_food, "StubTechCoef") %>%
      add_xml_data(L2328.GlobalTechSecOut_food, "GlobalTechSecOut") %>%
      add_xml_data_generate_levels(L2328.StubCalorieContent, "StubCalorieContent", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2328.StubCaloriePriceConv, "StubCaloriePriceConv", "subsector","nesting-subsector",1,FALSE) %>%
      add_precursors("L2328.Supplysector_food", "L2328.FinalEnergyKeyword_food", "L2328.SubsectorLogit_food",
                     "L2328.SubsectorShrwtFllt_food", "L2328.SubsectorInterp_food", "L2328.StubTech_food",
                     "L2328.GlobalTechSCurve_food", "L2328.GlobalTechProfitShutdown_food", "L2328.GlobalTechShrwt_food",
                     "L2328.GlobalTechCoef_food", "L2328.GlobalTechCost_food", "L2328.StubTechCost_food",
                     "L2328.StubTechProd_food", "L2328.StubTechCalInput_food_heat", "L2328.StubTechCoef_food",
                     "L2328.GlobalTechSecOut_food", "L2328.StubCalorieContent", "L2328.StubCaloriePriceConv",
                     "L2328.GlobalTechTrackCapital_food") ->
      food_processing.xml

    return_data(food_processing.xml)
  } else {
    stop("Unknown command")
  }
}
