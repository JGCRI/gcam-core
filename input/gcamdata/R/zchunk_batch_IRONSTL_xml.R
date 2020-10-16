# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_IRONSTL_xml
#'
#' Construct XML data structure for \code{IRONSTL.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{IRONSTL.xml}. The corresponding file in the
#' original data system was \code{batch_IRONSTL_xml.R} (energy XML).
module_energy_batch_IRONSTL_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2323.Supplysector_IRONSTL",
             "L2323.FinalEnergyKeyword_IRONSTL",
             "L2323.SubsectorLogit_IRONSTL",
             # "L2323.SubsectorShrwt_IRONSTL",
             "L2323.SubsectorShrwtFllt_IRONSTL",
             "L2323.SubsectorInterp_IRONSTL",
             # "L2323.SubsectorInterp_IRONSTL",
             "L2323.StubTech_IRONSTL",
             "L2323.GlobalTechShrwt_IRONSTL",
             "L2323.GlobalTechCoef_IRONSTL",
             "L2323.GlobalTechCost_IRONSTL",
             "L2323.GlobalTechCapture_IRONSTL",
             "L2323.GlobalTechSCurve_en",
             "L2323.GlobalTechProfitShutdown_en",
             "L2323.StubTechProd_IRONSTL",
             #"L2323.StubTechCalInput_IRONSTL",
             "L2323.StubTechCoef_IRONSTL",
             "L2323.PerCapitaBased_IRONSTL",
             "L2323.BaseService_IRONSTL",
             "L2323.PriceElasticity_IRONSTL"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "IRONSTL.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2323.Supplysector_IRONSTL <- get_data(all_data, "L2323.Supplysector_IRONSTL")
    L2323.FinalEnergyKeyword_IRONSTL <- get_data(all_data, "L2323.FinalEnergyKeyword_IRONSTL")
    L2323.SubsectorLogit_IRONSTL <- get_data(all_data, "L2323.SubsectorLogit_IRONSTL")
    #    L2323.SubsectorShrwt_IRONSTL <- get_data(all_data, "L2323.SubsectorShrwt_IRONSTL")
    L2323.SubsectorShrwtFllt_IRONSTL <- get_data(all_data, "L2323.SubsectorShrwtFllt_IRONSTL")
    #L2323.SubsectorInterp_IRONSTL <- get_data(all_data, "L2323.SubsectorInterp_IRONSTL")
    L2323.SubsectorInterp_IRONSTL <- get_data(all_data, "L2323.SubsectorInterp_IRONSTL")
    L2323.StubTech_IRONSTL <- get_data(all_data, "L2323.StubTech_IRONSTL")
    L2323.GlobalTechShrwt_IRONSTL <- get_data(all_data, "L2323.GlobalTechShrwt_IRONSTL")
    L2323.GlobalTechCoef_IRONSTL <- get_data(all_data, "L2323.GlobalTechCoef_IRONSTL")
    L2323.GlobalTechCost_IRONSTL <- get_data(all_data, "L2323.GlobalTechCost_IRONSTL")
    L2323.GlobalTechCapture_IRONSTL <- get_data(all_data, "L2323.GlobalTechCapture_IRONSTL")
    L2323.GlobalTechSCurve_en <- get_data(all_data, "L2323.GlobalTechSCurve_en")
    L2323.GlobalTechProfitShutdown_en <- get_data(all_data, "L2323.GlobalTechProfitShutdown_en")
    L2323.StubTechProd_IRONSTL <- get_data(all_data, "L2323.StubTechProd_IRONSTL")
    #L2323.StubTechCalInput_IRONSTL <- get_data(all_data, "L2323.StubTechCalInput_IRONSTL")
    L2323.StubTechCoef_IRONSTL <- get_data(all_data, "L2323.StubTechCoef_IRONSTL")
    L2323.PerCapitaBased_IRONSTL <- get_data(all_data, "L2323.PerCapitaBased_IRONSTL")
    L2323.BaseService_IRONSTL <- get_data(all_data, "L2323.BaseService_IRONSTL")
    L2323.PriceElasticity_IRONSTL <- get_data(all_data, "L2323.PriceElasticity_IRONSTL")
    # ===================================================

    # Produce outputs
    create_xml("IRONSTL.xml") %>%
      add_logit_tables_xml(L2323.Supplysector_IRONSTL, "Supplysector") %>%
      add_xml_data(L2323.FinalEnergyKeyword_IRONSTL, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2323.SubsectorLogit_IRONSTL, "SubsectorLogit") %>%
      #      add_xml_data(L2323.SubsectorShrwt_IRONSTL, "SubsectorShrwt") %>%
      add_xml_data(L2323.SubsectorShrwtFllt_IRONSTL, "SubsectorShrwtFllt") %>%
      #     add_xml_data(L2323.SubsectorInterp_IRONSTL, "SubsectorInterp") %>%
      add_xml_data(L2323.SubsectorInterp_IRONSTL, "SubsectorInterp") %>%
      add_xml_data(L2323.StubTech_IRONSTL, "StubTech") %>%
      add_xml_data(L2323.GlobalTechShrwt_IRONSTL, "GlobalTechShrwt") %>%
      add_xml_data(L2323.GlobalTechCoef_IRONSTL, "GlobalTechCoef") %>%
      add_xml_data(L2323.GlobalTechCost_IRONSTL, "GlobalTechCost") %>%
      add_xml_data(L2323.GlobalTechCapture_IRONSTL, "GlobalTechCapture") %>%
      add_xml_data(L2323.GlobalTechSCurve_en, "GlobalTechSCurve") %>%
      add_xml_data(L2323.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2323.StubTechProd_IRONSTL, "StubTechProd") %>%
      #add_xml_data(L2323.StubTechCalInput_IRONSTL, "StubTechCalInput") %>%
      add_xml_data(L2323.StubTechCoef_IRONSTL, "StubTechCoef") %>%
      add_xml_data(L2323.PerCapitaBased_IRONSTL, "PerCapitaBased") %>%
      add_xml_data(L2323.BaseService_IRONSTL, "BaseService") %>%
      add_xml_data(L2323.PriceElasticity_IRONSTL, "PriceElasticity") %>%
      add_precursors("L2323.Supplysector_IRONSTL", "L2323.FinalEnergyKeyword_IRONSTL", "L2323.SubsectorLogit_IRONSTL",
                     # "L2323.SubsectorShrwt_IRONSTL",
                     "L2323.SubsectorShrwtFllt_IRONSTL",
                     "L2323.SubsectorInterp_IRONSTL",
                     # "L2323.SubsectorInterp_IRONSTL",
                     "L2323.StubTech_IRONSTL",
                     "L2323.GlobalTechShrwt_IRONSTL", "L2323.GlobalTechCoef_IRONSTL", "L2323.GlobalTechCost_IRONSTL",
                     "L2323.GlobalTechCapture_IRONSTL", "L2323.GlobalTechSCurve_en",
                     "L2323.GlobalTechProfitShutdown_en",
                     "L2323.StubTechCoef_IRONSTL", "L2323.PerCapitaBased_IRONSTL", "L2323.BaseService_IRONSTL",
                     "L2323.PriceElasticity_IRONSTL") ->
      IRONSTL.xml
	#, "L2323.StubTechCalInput_IRONSTL"
    return_data(IRONSTL.xml)
  } else {
    stop("Unknown command")
  }
}

