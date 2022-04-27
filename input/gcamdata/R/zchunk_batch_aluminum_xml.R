# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_aluminum_xml
#'
#' Construct XML data structure for \code{aluminum.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{aluminum.xml}. The corresponding file in the
#' original data system was \code{batch_aluminum_xml.R} (energy XML).
module_energy_batch_aluminum_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2326.Supplysector_aluminum",
             "L2326.FinalEnergyKeyword_aluminum",
             "L2326.SubsectorLogit_aluminum",
             "L2326.SubsectorShrwtFllt_aluminum",
             "L2326.SubsectorInterp_aluminum",
             "L2326.StubTech_aluminum",
             "L2326.GlobalTechShrwt_aluminum",
             "L2326.GlobalTechCoef_aluminum",
             "L2326.GlobalTechCost_aluminum",
			 "L2326.GlobalTechSCurve_aluminum",
             "L2326.GlobalTechProfitShutdown_aluminum",
             "L2326.StubTechProd_aluminum",
             "L2326.StubTechCalInput_aluminum",
             "L2326.StubTechCoef_aluminum",
             "L2326.PerCapitaBased_aluminum",
             "L2326.BaseService_aluminum",
             "L2326.PriceElasticity_aluminum",
             "L2326.GlobalTechCapture_aluminum"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "aluminum.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2326.Supplysector_aluminum <- get_data(all_data, "L2326.Supplysector_aluminum")
    L2326.FinalEnergyKeyword_aluminum <- get_data(all_data, "L2326.FinalEnergyKeyword_aluminum")
    L2326.SubsectorLogit_aluminum <- get_data(all_data, "L2326.SubsectorLogit_aluminum")
    L2326.SubsectorShrwtFllt_aluminum <- get_data(all_data, "L2326.SubsectorShrwtFllt_aluminum")
    L2326.SubsectorInterp_aluminum <- get_data(all_data, "L2326.SubsectorInterp_aluminum")
    L2326.StubTech_aluminum <- get_data(all_data, "L2326.StubTech_aluminum")
    L2326.GlobalTechShrwt_aluminum <- get_data(all_data, "L2326.GlobalTechShrwt_aluminum")
    L2326.GlobalTechCoef_aluminum <- get_data(all_data, "L2326.GlobalTechCoef_aluminum")
    L2326.GlobalTechCost_aluminum <- get_data(all_data, "L2326.GlobalTechCost_aluminum")
	  L2326.GlobalTechSCurve_aluminum <- get_data(all_data, "L2326.GlobalTechSCurve_aluminum")
    L2326.GlobalTechProfitShutdown_aluminum <- get_data(all_data, "L2326.GlobalTechProfitShutdown_aluminum")
    L2326.GlobalTechCapture_aluminum <- get_data(all_data, "L2326.GlobalTechCapture_aluminum")
    L2326.StubTechProd_aluminum <- get_data(all_data, "L2326.StubTechProd_aluminum")
    L2326.StubTechCalInput_aluminum <- get_data(all_data, "L2326.StubTechCalInput_aluminum")
    L2326.StubTechCoef_aluminum <- get_data(all_data, "L2326.StubTechCoef_aluminum")
    L2326.PerCapitaBased_aluminum <- get_data(all_data, "L2326.PerCapitaBased_aluminum")
    L2326.BaseService_aluminum <- get_data(all_data, "L2326.BaseService_aluminum")
    L2326.PriceElasticity_aluminum <- get_data(all_data, "L2326.PriceElasticity_aluminum")
    # ===================================================

    # Produce outputs
    create_xml("aluminum.xml") %>%
      add_logit_tables_xml(L2326.Supplysector_aluminum, "Supplysector") %>%
      add_xml_data(L2326.FinalEnergyKeyword_aluminum, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2326.SubsectorLogit_aluminum, "SubsectorLogit") %>%
      add_xml_data(L2326.SubsectorShrwtFllt_aluminum, "SubsectorShrwtFllt") %>%
      add_xml_data(L2326.SubsectorInterp_aluminum, "SubsectorInterp") %>%
      add_xml_data(L2326.StubTech_aluminum, "StubTech") %>%
      add_xml_data(L2326.GlobalTechShrwt_aluminum, "GlobalTechShrwt") %>%
      add_xml_data(L2326.GlobalTechCoef_aluminum, "GlobalTechCoef") %>%
      add_xml_data(L2326.GlobalTechCost_aluminum, "GlobalTechCost") %>%
	    add_xml_data(L2326.GlobalTechSCurve_aluminum, "GlobalTechSCurve") %>%
      add_xml_data(L2326.GlobalTechProfitShutdown_aluminum, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2326.GlobalTechCapture_aluminum, "GlobalTechCapture") %>%
      add_xml_data(L2326.StubTechProd_aluminum, "StubTechProd") %>%
      add_xml_data(L2326.StubTechCalInput_aluminum, "StubTechCalInput") %>%
      add_xml_data(L2326.StubTechCoef_aluminum, "StubTechCoef") %>%
      add_xml_data(L2326.PerCapitaBased_aluminum, "PerCapitaBased") %>%
      add_xml_data(L2326.BaseService_aluminum, "BaseService") %>%
      add_xml_data(L2326.PriceElasticity_aluminum, "PriceElasticity") %>%
      add_precursors("L2326.Supplysector_aluminum", "L2326.FinalEnergyKeyword_aluminum", "L2326.SubsectorLogit_aluminum",
                     "L2326.SubsectorShrwtFllt_aluminum",
                     "L2326.SubsectorInterp_aluminum","L2326.StubTechProd_aluminum",
                     "L2326.StubTech_aluminum","L2326.StubTechCoef_aluminum",
                     "L2326.GlobalTechShrwt_aluminum", "L2326.GlobalTechCoef_aluminum", "L2326.GlobalTechCost_aluminum",
                     "L2326.GlobalTechProfitShutdown_aluminum", "L2326.GlobalTechSCurve_aluminum",
                     "L2326.StubTechCalInput_aluminum","L2326.GlobalTechCapture_aluminum",
                     "L2326.PerCapitaBased_aluminum", "L2326.BaseService_aluminum",
                     "L2326.PriceElasticity_aluminum") ->
      aluminum.xml

    return_data(aluminum.xml)
  } else {
    stop("Unknown command")
  }
}

