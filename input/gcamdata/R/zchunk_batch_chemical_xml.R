# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_chemical_xml
#'
#' Construct XML data structure for \code{chemical.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{chemical.xml}. The corresponding file in the
#' original data system was \code{batch_chemical_xml.R} (energy XML).
module_energy_batch_chemical_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2325.Supplysector_chemical",
             "L2325.FinalEnergyKeyword_chemical",
             "L2325.SubsectorLogit_chemical",
             "L2325.SubsectorShrwtFllt_chemical",
             "L2325.SubsectorInterp_chemical",
             "L2325.StubTech_chemical",
             "L2325.GlobalTechShrwt_chemical",
             "L2325.GlobalTechCoef_chemical",
             "L2325.GlobalTechCost_chemical",
			 "L2325.GlobalTechSCurve_chemical",
			 "L2325.GlobalTechCSeq_ind",
             "L2325.GlobalTechProfitShutdown_chemical",
             "L2325.StubTechProd_chemical",
             "L2325.StubTechCalInput_chemical",
             "L2325.StubTechCoef_chemical",
             "L2325.PerCapitaBased_chemical",
             "L2325.BaseService_chemical",
             "L2325.PriceElasticity_chemical",
             "L2325.GlobalTechCapture_chemical",
             "L2325.GlobalTechEff_chemical",
             "L2325.GlobalTechSecOut_chemical"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "chemical.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2325.Supplysector_chemical <- get_data(all_data, "L2325.Supplysector_chemical")
    L2325.FinalEnergyKeyword_chemical <- get_data(all_data, "L2325.FinalEnergyKeyword_chemical")
    L2325.SubsectorLogit_chemical <- get_data(all_data, "L2325.SubsectorLogit_chemical")
    L2325.SubsectorShrwtFllt_chemical <- get_data(all_data, "L2325.SubsectorShrwtFllt_chemical")
    L2325.SubsectorInterp_chemical <- get_data(all_data, "L2325.SubsectorInterp_chemical")
    L2325.StubTech_chemical <- get_data(all_data, "L2325.StubTech_chemical")
    L2325.GlobalTechShrwt_chemical <- get_data(all_data, "L2325.GlobalTechShrwt_chemical")
    L2325.GlobalTechEff_chemical <- get_data(all_data, "L2325.GlobalTechEff_chemical")
    L2325.GlobalTechCoef_chemical <- get_data(all_data, "L2325.GlobalTechCoef_chemical")
    L2325.GlobalTechCost_chemical <- get_data(all_data, "L2325.GlobalTechCost_chemical")
	  L2325.GlobalTechSCurve_chemical <- get_data(all_data, "L2325.GlobalTechSCurve_chemical")
    L2325.GlobalTechProfitShutdown_chemical <- get_data(all_data, "L2325.GlobalTechProfitShutdown_chemical")
    L2325.GlobalTechCapture_chemical <- get_data(all_data, "L2325.GlobalTechCapture_chemical")
    L2325.StubTechProd_chemical <- get_data(all_data, "L2325.StubTechProd_chemical")
    L2325.StubTechCalInput_chemical <- get_data(all_data, "L2325.StubTechCalInput_chemical")
    L2325.StubTechCoef_chemical <- get_data(all_data, "L2325.StubTechCoef_chemical")
    L2325.PerCapitaBased_chemical <- get_data(all_data, "L2325.PerCapitaBased_chemical")
    L2325.BaseService_chemical <- get_data(all_data, "L2325.BaseService_chemical")
    L2325.PriceElasticity_chemical <- get_data(all_data, "L2325.PriceElasticity_chemical")
    L2325.GlobalTechSecOut_chemical <- get_data(all_data, "L2325.GlobalTechSecOut_chemical")
    L2325.GlobalTechCSeq_ind <- get_data(all_data, "L2325.GlobalTechCSeq_ind")
    # ===================================================

    # Produce outputs
    create_xml("chemical.xml") %>%
      add_logit_tables_xml(L2325.Supplysector_chemical, "Supplysector") %>%
      add_xml_data(L2325.FinalEnergyKeyword_chemical, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2325.SubsectorLogit_chemical, "SubsectorLogit") %>%
      add_xml_data(L2325.SubsectorShrwtFllt_chemical, "SubsectorShrwtFllt") %>%
      add_xml_data(L2325.SubsectorInterp_chemical, "SubsectorInterp") %>%
      add_xml_data(L2325.StubTech_chemical, "StubTech") %>%
      add_xml_data(L2325.GlobalTechShrwt_chemical, "GlobalTechShrwt") %>%
      add_xml_data(L2325.GlobalTechEff_chemical, "GlobalTechEff") %>%
      add_xml_data(L2325.GlobalTechCoef_chemical, "GlobalTechCoef") %>%
      add_xml_data(L2325.GlobalTechCost_chemical, "GlobalTechCost") %>%
	    add_xml_data(L2325.GlobalTechSCurve_chemical, "GlobalTechSCurve") %>%
      add_xml_data(L2325.GlobalTechProfitShutdown_chemical, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2325.GlobalTechCSeq_ind, "GlobalTechCSeq") %>%
      add_xml_data(L2325.GlobalTechCapture_chemical, "GlobalTechCapture") %>%
      add_xml_data(L2325.StubTechProd_chemical, "StubTechProd") %>%
      add_xml_data(L2325.StubTechCalInput_chemical, "StubTechCalInput") %>%
      add_xml_data(L2325.StubTechCoef_chemical, "StubTechCoef") %>%
      add_xml_data(L2325.PerCapitaBased_chemical, "PerCapitaBased") %>%
      add_xml_data(L2325.BaseService_chemical, "BaseService") %>%
      add_xml_data(L2325.PriceElasticity_chemical, "PriceElasticity") %>%
      add_xml_data(L2325.GlobalTechSecOut_chemical, "GlobalTechSecOut") %>%
      add_precursors("L2325.Supplysector_chemical", "L2325.FinalEnergyKeyword_chemical", "L2325.SubsectorLogit_chemical",
                     "L2325.SubsectorShrwtFllt_chemical","L2325.GlobalTechEff_chemical",
                     "L2325.SubsectorInterp_chemical","L2325.StubTechProd_chemical",
                     "L2325.StubTech_chemical","L2325.StubTechCoef_chemical","L2325.GlobalTechCSeq_ind",
                     "L2325.GlobalTechShrwt_chemical", "L2325.GlobalTechCoef_chemical", "L2325.GlobalTechCost_chemical",
                     "L2325.GlobalTechProfitShutdown_chemical", "L2325.GlobalTechSCurve_chemical",
                     "L2325.StubTechCalInput_chemical","L2325.GlobalTechCapture_chemical",
                     "L2325.PerCapitaBased_chemical", "L2325.BaseService_chemical",
                     "L2325.PriceElasticity_chemical","L2325.GlobalTechSecOut_chemical") ->
      chemical.xml

    return_data(chemical.xml)
  } else {
    stop("Unknown command")
  }
}

