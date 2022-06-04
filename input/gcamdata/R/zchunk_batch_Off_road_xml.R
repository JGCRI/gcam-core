# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_Off_road_xml
#'
#' Construct XML data structure for \code{Off_road.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{Off_road.xml}. The corresponding file in the
#' original data system was \code{batch_Off_road_xml.R} (energy XML).
module_energy_batch_Off_road_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2324.Supplysector_Off_road",
             "L2324.FinalEnergyKeyword_Off_road",
             "L2324.SubsectorLogit_Off_road",
             "L2324.SubsectorShrwtFllt_Off_road",
             "L2324.SubsectorInterp_Off_road",
             "L2324.StubTech_Off_road",
             "L2324.GlobalTechInterp_Off_road",
             "L2324.GlobalTechShrwt_Off_road",
             "L2324.GlobalTechCoef_Off_road",
             "L2324.GlobalTechEff_Off_road",
             "L2324.GlobalTechCost_Off_road",
			        "L2324.GlobalTechSCurve_Off_road",
			        "L2324.GlobalTechCSeq_ind",
             "L2324.GlobalTechProfitShutdown_Off_road",
             "L2324.StubTechProd_Off_road",
             "L2324.StubTechCalInput_Off_road",
             "L2324.StubTechCoef_Off_road",
             "L2324.PerCapitaBased_Off_road",
             "L2324.BaseService_Off_road",
             "L2324.PriceElasticity_Off_road"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "Off_road.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2324.Supplysector_Off_road <- get_data(all_data, "L2324.Supplysector_Off_road")
    L2324.FinalEnergyKeyword_Off_road <- get_data(all_data, "L2324.FinalEnergyKeyword_Off_road")
    L2324.SubsectorLogit_Off_road <- get_data(all_data, "L2324.SubsectorLogit_Off_road")
    L2324.SubsectorShrwtFllt_Off_road <- get_data(all_data, "L2324.SubsectorShrwtFllt_Off_road")
    L2324.SubsectorInterp_Off_road <- get_data(all_data, "L2324.SubsectorInterp_Off_road")
    L2324.StubTech_Off_road <- get_data(all_data, "L2324.StubTech_Off_road")
    L2324.GlobalTechInterp_Off_road <- get_data(all_data, "L2324.GlobalTechInterp_Off_road")
    L2324.GlobalTechShrwt_Off_road <- get_data(all_data, "L2324.GlobalTechShrwt_Off_road")
    L2324.GlobalTechCoef_Off_road <- get_data(all_data, "L2324.GlobalTechCoef_Off_road")
    L2324.GlobalTechEff_Off_road <- get_data(all_data, "L2324.GlobalTechEff_Off_road")
    L2324.GlobalTechCost_Off_road <- get_data(all_data, "L2324.GlobalTechCost_Off_road")
	  L2324.GlobalTechSCurve_Off_road <- get_data(all_data, "L2324.GlobalTechSCurve_Off_road")
    L2324.GlobalTechProfitShutdown_Off_road <- get_data(all_data, "L2324.GlobalTechProfitShutdown_Off_road")
    L2324.StubTechProd_Off_road <- get_data(all_data, "L2324.StubTechProd_Off_road")
    L2324.StubTechCalInput_Off_road <- get_data(all_data, "L2324.StubTechCalInput_Off_road")
    L2324.StubTechCoef_Off_road <- get_data(all_data, "L2324.StubTechCoef_Off_road")
    L2324.PerCapitaBased_Off_road <- get_data(all_data, "L2324.PerCapitaBased_Off_road")
    L2324.BaseService_Off_road <- get_data(all_data, "L2324.BaseService_Off_road")
    L2324.PriceElasticity_Off_road <- get_data(all_data, "L2324.PriceElasticity_Off_road")
    L2324.GlobalTechCSeq_ind <-  get_data(all_data, "L2324.GlobalTechCSeq_ind")
    # ===================================================

    # Produce outputs
    create_xml("Off_road.xml") %>%
      add_logit_tables_xml(L2324.Supplysector_Off_road, "Supplysector") %>%
      add_xml_data(L2324.FinalEnergyKeyword_Off_road, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2324.SubsectorLogit_Off_road, "SubsectorLogit") %>%
      add_xml_data(L2324.SubsectorShrwtFllt_Off_road, "SubsectorShrwtFllt") %>%
      add_xml_data(L2324.SubsectorInterp_Off_road, "SubsectorInterp") %>%
      add_xml_data(L2324.StubTech_Off_road, "StubTech") %>%
      add_xml_data(L2324.GlobalTechInterp_Off_road, "GlobalTechInterp") %>%
      add_xml_data(L2324.GlobalTechShrwt_Off_road, "GlobalTechShrwt") %>%
      add_xml_data(L2324.GlobalTechCoef_Off_road, "GlobalTechCoef") %>%
      add_xml_data(L2324.GlobalTechEff_Off_road, "GlobalTechEff") %>%
      add_xml_data(L2324.GlobalTechCost_Off_road, "GlobalTechCost") %>%
	    add_xml_data(L2324.GlobalTechSCurve_Off_road, "GlobalTechSCurve") %>%
      add_xml_data(L2324.GlobalTechProfitShutdown_Off_road, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2324.StubTechProd_Off_road, "StubTechProd") %>%
      add_xml_data(L2324.StubTechCalInput_Off_road, "StubTechCalInput") %>%
      add_xml_data(L2324.StubTechCoef_Off_road, "StubTechCoef") %>%
      add_xml_data(L2324.PerCapitaBased_Off_road, "PerCapitaBased") %>%
      add_xml_data(L2324.BaseService_Off_road, "BaseService") %>%
      add_xml_data(L2324.PriceElasticity_Off_road, "PriceElasticity") %>%
      add_xml_data(L2324.GlobalTechCSeq_ind, "GlobalTechCSeq") %>%
      add_precursors("L2324.Supplysector_Off_road", "L2324.FinalEnergyKeyword_Off_road", "L2324.SubsectorLogit_Off_road",
                     "L2324.SubsectorShrwtFllt_Off_road",
                     "L2324.SubsectorInterp_Off_road", "L2324.GlobalTechInterp_Off_road",
                     "L2324.StubTech_Off_road","L2324.StubTechCoef_Off_road","L2324.GlobalTechCSeq_ind",
                     "L2324.GlobalTechProfitShutdown_Off_road", "L2324.GlobalTechSCurve_Off_road",
                     "L2324.GlobalTechShrwt_Off_road", "L2324.GlobalTechCoef_Off_road", "L2324.GlobalTechCost_Off_road",
                     "L2324.StubTechCalInput_Off_road","L2324.StubTechProd_Off_road","L2324.GlobalTechEff_Off_road",
                     "L2324.PerCapitaBased_Off_road", "L2324.BaseService_Off_road",
                     "L2324.PriceElasticity_Off_road") ->
      Off_road.xml

    return_data(Off_road.xml)
  } else {
    stop("Unknown command")
  }
}

