# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_iron_steel_xml
#'
#' Construct XML data structure for \code{iron_steel.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{iron_steel.xml}. The corresponding file in the
#' original data system was \code{batch_iron_steel_xml.R} (energy XML).
module_energy_batch_iron_steel_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2323.Supplysector_iron_steel",
             "L2323.FinalEnergyKeyword_iron_steel",
             "L2323.SubsectorLogit_iron_steel",
             "L2323.SubsectorShrwtFllt_iron_steel",
             "L2323.SubsectorInterp_iron_steel",
             "L2323.StubTech_iron_steel",
             "L2323.GlobalTechShrwt_iron_steel",
             "L2323.GlobalTechCoef_iron_steel",
             "L2323.GlobalTechCost_iron_steel",
             "L2323.GlobalTechCapture_iron_steel",
             "L2323.GlobalTechSCurve_en",
             "L2323.GlobalTechProfitShutdown_en",
             "L2323.StubTechProd_iron_steel",
             "L2323.StubTechCoef_iron_steel",
             "L2323.PerCapitaBased_iron_steel",
             "L2323.BaseService_iron_steel",
             "L2323.PriceElasticity_iron_steel"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "iron_steel.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2323.Supplysector_iron_steel <- get_data(all_data, "L2323.Supplysector_iron_steel")
    L2323.FinalEnergyKeyword_iron_steel <- get_data(all_data, "L2323.FinalEnergyKeyword_iron_steel")
    L2323.SubsectorLogit_iron_steel <- get_data(all_data, "L2323.SubsectorLogit_iron_steel")
    L2323.SubsectorShrwtFllt_iron_steel <- get_data(all_data, "L2323.SubsectorShrwtFllt_iron_steel")
    L2323.SubsectorInterp_iron_steel <- get_data(all_data, "L2323.SubsectorInterp_iron_steel")
    L2323.StubTech_iron_steel <- get_data(all_data, "L2323.StubTech_iron_steel")
    L2323.GlobalTechShrwt_iron_steel <- get_data(all_data, "L2323.GlobalTechShrwt_iron_steel")
    L2323.GlobalTechCoef_iron_steel <- get_data(all_data, "L2323.GlobalTechCoef_iron_steel")
    L2323.GlobalTechCost_iron_steel <- get_data(all_data, "L2323.GlobalTechCost_iron_steel")
    L2323.GlobalTechCapture_iron_steel <- get_data(all_data, "L2323.GlobalTechCapture_iron_steel")
    L2323.GlobalTechSCurve_en <- get_data(all_data, "L2323.GlobalTechSCurve_en")
    L2323.GlobalTechProfitShutdown_en <- get_data(all_data, "L2323.GlobalTechProfitShutdown_en")
    L2323.StubTechProd_iron_steel <- get_data(all_data, "L2323.StubTechProd_iron_steel")
    L2323.StubTechCoef_iron_steel <- get_data(all_data, "L2323.StubTechCoef_iron_steel")
    L2323.PerCapitaBased_iron_steel <- get_data(all_data, "L2323.PerCapitaBased_iron_steel")
    L2323.BaseService_iron_steel <- get_data(all_data, "L2323.BaseService_iron_steel")
    L2323.PriceElasticity_iron_steel <- get_data(all_data, "L2323.PriceElasticity_iron_steel")
    # ===================================================

    # Produce outputs
    create_xml("iron_steel.xml") %>%
      add_logit_tables_xml(L2323.Supplysector_iron_steel, "Supplysector") %>%
      add_xml_data(L2323.FinalEnergyKeyword_iron_steel, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2323.SubsectorLogit_iron_steel, "SubsectorLogit") %>%
      add_xml_data(L2323.SubsectorShrwtFllt_iron_steel, "SubsectorShrwtFllt") %>%
      add_xml_data(L2323.SubsectorInterp_iron_steel, "SubsectorInterp") %>%
      add_xml_data(L2323.StubTech_iron_steel, "StubTech") %>%
      add_xml_data(L2323.GlobalTechShrwt_iron_steel, "GlobalTechShrwt") %>%
      add_xml_data(L2323.GlobalTechCoef_iron_steel, "GlobalTechCoef") %>%
      add_xml_data(L2323.GlobalTechCost_iron_steel, "GlobalTechCost") %>%
      add_xml_data(L2323.GlobalTechCapture_iron_steel, "GlobalTechCapture") %>%
      add_xml_data(L2323.GlobalTechSCurve_en, "GlobalTechSCurve") %>%
      add_xml_data(L2323.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2323.StubTechProd_iron_steel, "StubTechProd") %>%
      add_xml_data(L2323.StubTechCoef_iron_steel, "StubTechCoef") %>%
      add_xml_data(L2323.PerCapitaBased_iron_steel, "PerCapitaBased") %>%
      add_xml_data(L2323.BaseService_iron_steel, "BaseService") %>%
      add_xml_data(L2323.PriceElasticity_iron_steel, "PriceElasticity") %>%
      add_precursors("L2323.Supplysector_iron_steel", "L2323.FinalEnergyKeyword_iron_steel", "L2323.SubsectorLogit_iron_steel",
                     "L2323.SubsectorShrwtFllt_iron_steel",
                     "L2323.SubsectorInterp_iron_steel",
                     "L2323.StubTech_iron_steel",
                     "L2323.GlobalTechShrwt_iron_steel", "L2323.GlobalTechCoef_iron_steel", "L2323.GlobalTechCost_iron_steel",
                     "L2323.GlobalTechCapture_iron_steel", "L2323.GlobalTechSCurve_en",
                     "L2323.GlobalTechProfitShutdown_en", "L2323.StubTechProd_iron_steel",
                     "L2323.StubTechCoef_iron_steel", "L2323.PerCapitaBased_iron_steel", "L2323.BaseService_iron_steel",
                     "L2323.PriceElasticity_iron_steel") ->
      iron_steel.xml
    return_data(iron_steel.xml)
  } else {
    stop("Unknown command")
  }
}

