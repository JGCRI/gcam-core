# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_cement_xml
#'
#' Construct XML data structure for \code{cement.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{cement.xml}. The corresponding file in the
#' original data system was \code{batch_cement_xml.R} (energy XML).
module_energy_batch_cement_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2321.Supplysector_cement",
             "L2321.FinalEnergyKeyword_cement",
             "L2321.SubsectorLogit_cement",
             # "L2321.SubsectorShrwt_cement",
             "L2321.SubsectorShrwtFllt_cement",
             "L2321.SubsectorInterp_cement",
             # "L2321.SubsectorInterpTo_cement",
             "L2321.StubTech_cement",
             "L2321.GlobalTechShrwt_cement",
             "L2321.GlobalTechCoef_cement",
             "L2321.GlobalTechCost_cement",
             "L2321.GlobalTechCapture_cement",
			 "L2321.GlobalTechSCurve_en",
             "L2321.GlobalTechProfitShutdown_en",
             "L2321.StubTechProd_cement",
             "L2321.StubTechCalInput_cement_heat",
             "L2321.StubTechCoef_cement",
             "L2321.PerCapitaBased_cement",
             "L2321.BaseService_cement",
             "L2321.PriceElasticity_cement"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "cement.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2321.Supplysector_cement <- get_data(all_data, "L2321.Supplysector_cement")
    L2321.FinalEnergyKeyword_cement <- get_data(all_data, "L2321.FinalEnergyKeyword_cement")
    L2321.SubsectorLogit_cement <- get_data(all_data, "L2321.SubsectorLogit_cement")
    #    L2321.SubsectorShrwt_cement <- get_data(all_data, "L2321.SubsectorShrwt_cement")
    L2321.SubsectorShrwtFllt_cement <- get_data(all_data, "L2321.SubsectorShrwtFllt_cement")
    L2321.SubsectorInterp_cement <- get_data(all_data, "L2321.SubsectorInterp_cement")
    #    L2321.SubsectorInterpTo_cement <- get_data(all_data, "L2321.SubsectorInterpTo_cement")
    L2321.StubTech_cement <- get_data(all_data, "L2321.StubTech_cement")
    L2321.GlobalTechShrwt_cement <- get_data(all_data, "L2321.GlobalTechShrwt_cement")
    L2321.GlobalTechCoef_cement <- get_data(all_data, "L2321.GlobalTechCoef_cement")
    L2321.GlobalTechCost_cement <- get_data(all_data, "L2321.GlobalTechCost_cement")
    L2321.GlobalTechCapture_cement <- get_data(all_data, "L2321.GlobalTechCapture_cement")
	L2321.GlobalTechSCurve_en <- get_data(all_data, "L2321.GlobalTechSCurve_en")
    L2321.GlobalTechProfitShutdown_en <- get_data(all_data, "L2321.GlobalTechProfitShutdown_en")
    L2321.StubTechProd_cement <- get_data(all_data, "L2321.StubTechProd_cement")
    L2321.StubTechCalInput_cement_heat <- get_data(all_data, "L2321.StubTechCalInput_cement_heat")
    L2321.StubTechCoef_cement <- get_data(all_data, "L2321.StubTechCoef_cement")
    L2321.PerCapitaBased_cement <- get_data(all_data, "L2321.PerCapitaBased_cement")
    L2321.BaseService_cement <- get_data(all_data, "L2321.BaseService_cement")
    L2321.PriceElasticity_cement <- get_data(all_data, "L2321.PriceElasticity_cement")

    # ===================================================

    # Produce outputs
    create_xml("cement.xml") %>%
      add_logit_tables_xml(L2321.Supplysector_cement, "Supplysector") %>%
      add_xml_data(L2321.FinalEnergyKeyword_cement, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2321.SubsectorLogit_cement, "SubsectorLogit") %>%
      #      add_xml_data(L2321.SubsectorShrwt_cement, "SubsectorShrwt") %>%
      add_xml_data(L2321.SubsectorShrwtFllt_cement, "SubsectorShrwtFllt") %>%
      add_xml_data(L2321.SubsectorInterp_cement, "SubsectorInterp") %>%
      #      add_xml_data(L2321.SubsectorInterpTo_cement, "SubsectorInterpTo") %>%
      add_xml_data(L2321.StubTech_cement, "StubTech") %>%
      add_xml_data(L2321.GlobalTechShrwt_cement, "GlobalTechShrwt") %>%
      add_xml_data(L2321.GlobalTechCoef_cement, "GlobalTechCoef") %>%
	  add_xml_data(L2321.GlobalTechSCurve_en, "GlobalTechSCurve") %>%
      add_xml_data(L2321.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2321.GlobalTechCost_cement, "GlobalTechCost") %>%
      add_xml_data(L2321.GlobalTechCapture_cement, "GlobalTechCapture") %>%
      add_xml_data(L2321.StubTechProd_cement, "StubTechProd") %>%
      add_xml_data(L2321.StubTechCalInput_cement_heat, "StubTechCalInput") %>%
      add_xml_data(L2321.StubTechCoef_cement, "StubTechCoef") %>%
      add_xml_data(L2321.PerCapitaBased_cement, "PerCapitaBased") %>%
      add_xml_data(L2321.BaseService_cement, "BaseService") %>%
      add_xml_data(L2321.PriceElasticity_cement, "PriceElasticity") %>%
      add_precursors("L2321.Supplysector_cement", "L2321.FinalEnergyKeyword_cement", "L2321.SubsectorLogit_cement",
                     # "L2321.SubsectorShrwt_cement",
                     "L2321.SubsectorShrwtFllt_cement",
                     "L2321.SubsectorInterp_cement",
                     # "L2321.SubsectorInterpTo_cement",
                     "L2321.StubTech_cement","L2321.GlobalTechSCurve_en", "L2321.GlobalTechProfitShutdown_en",
                     "L2321.GlobalTechShrwt_cement", "L2321.GlobalTechCoef_cement", "L2321.GlobalTechCost_cement",
                     "L2321.GlobalTechCapture_cement", "L2321.StubTechProd_cement", "L2321.StubTechCalInput_cement_heat",
                     "L2321.StubTechCoef_cement", "L2321.PerCapitaBased_cement", "L2321.BaseService_cement",
                     "L2321.PriceElasticity_cement") ->
      cement.xml

    return_data(cement.xml)
  } else {
    stop("Unknown command")
  }
}
