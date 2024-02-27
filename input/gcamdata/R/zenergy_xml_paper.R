# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_paper_xml
#'
#' Construct XML data structure for \code{paper.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{paper.xml}. The corresponding file in the
#' original data system was \code{batch_paper_xml.R} (energy XML).
module_energy_paper_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2327.Supplysector_paper",
             "L2327.FinalEnergyKeyword_paper",
             "L2327.SubsectorLogit_paper",
             "L2327.SubsectorShrwtFllt_paper",
             "L2327.SubsectorInterp_paper",
             "L2327.StubTech_paper",
             "L2327.GlobalTechShrwt_paper",
             "L2327.GlobalTechCoef_paper",
             "L2327.GlobalTechCost_paper",
             "L2327.GlobalTechTrackCapital_paper",
             "L2327.GlobalTechCapture_paper",
             "L2327.GlobalTechSCurve_paper",
             "L2327.GlobalTechProfitShutdown_paper",
             "L2327.StubTechProd_paper",
             "L2327.StubTechCalInput_paper_heat",
             "L2327.StubTechCoef_paper",
             "L2327.PerCapitaBased_paper",
             "L2327.BaseService_paper",
             "L2327.PriceElasticity_paper",
             "L2327.GlobalTechSecOut_paper"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "paper.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2327.Supplysector_paper <- get_data(all_data, "L2327.Supplysector_paper")
    L2327.FinalEnergyKeyword_paper <- get_data(all_data, "L2327.FinalEnergyKeyword_paper")
    L2327.SubsectorLogit_paper <- get_data(all_data, "L2327.SubsectorLogit_paper")
    L2327.SubsectorShrwtFllt_paper <- get_data(all_data, "L2327.SubsectorShrwtFllt_paper")
    L2327.SubsectorInterp_paper <- get_data(all_data, "L2327.SubsectorInterp_paper")
    L2327.StubTech_paper <- get_data(all_data, "L2327.StubTech_paper")
    L2327.GlobalTechShrwt_paper <- get_data(all_data, "L2327.GlobalTechShrwt_paper")
    L2327.GlobalTechCoef_paper <- get_data(all_data, "L2327.GlobalTechCoef_paper")
    L2327.GlobalTechCost_paper <- get_data(all_data, "L2327.GlobalTechCost_paper")
    L2327.GlobalTechTrackCapital_paper <- get_data(all_data, "L2327.GlobalTechTrackCapital_paper")
    L2327.GlobalTechCapture_paper <- get_data(all_data, "L2327.GlobalTechCapture_paper")
    L2327.GlobalTechSCurve_paper <- get_data(all_data, "L2327.GlobalTechSCurve_paper")
    L2327.GlobalTechProfitShutdown_paper <- get_data(all_data, "L2327.GlobalTechProfitShutdown_paper")
    L2327.StubTechProd_paper <- get_data(all_data, "L2327.StubTechProd_paper")
    L2327.StubTechCalInput_paper_heat <- get_data(all_data, "L2327.StubTechCalInput_paper_heat")
    L2327.StubTechCoef_paper <- get_data(all_data, "L2327.StubTechCoef_paper")
    L2327.PerCapitaBased_paper <- get_data(all_data, "L2327.PerCapitaBased_paper")
    L2327.BaseService_paper <- get_data(all_data, "L2327.BaseService_paper")
    L2327.PriceElasticity_paper <- get_data(all_data, "L2327.PriceElasticity_paper")
    L2327.GlobalTechSecOut_paper <- get_data(all_data, "L2327.GlobalTechSecOut_paper")

    # ===================================================

    # Produce outputs
    create_xml("paper.xml") %>%
      add_logit_tables_xml(L2327.Supplysector_paper, "Supplysector") %>%
      add_xml_data(L2327.FinalEnergyKeyword_paper, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2327.SubsectorLogit_paper, "SubsectorLogit") %>%
      add_xml_data(L2327.SubsectorShrwtFllt_paper, "SubsectorShrwtFllt") %>%
      add_xml_data(L2327.SubsectorInterp_paper, "SubsectorInterp") %>%
      add_xml_data(L2327.StubTech_paper, "StubTech") %>%
      add_xml_data(L2327.GlobalTechShrwt_paper, "GlobalTechShrwt") %>%
      add_xml_data(L2327.GlobalTechCoef_paper, "GlobalTechCoef") %>%
      add_xml_data(L2327.GlobalTechSCurve_paper, "GlobalTechSCurve") %>%
      add_xml_data(L2327.GlobalTechProfitShutdown_paper, "GlobalTechProfitShutdown") %>%
      add_node_equiv_xml("input") %>%
      add_xml_data(L2327.GlobalTechTrackCapital_paper, "GlobalTechTrackCapital") %>%
      add_xml_data(L2327.GlobalTechCost_paper, "GlobalTechCost") %>%
      add_xml_data(L2327.GlobalTechCapture_paper, "GlobalTechCapture") %>%
      add_xml_data(L2327.StubTechProd_paper, "StubTechProd") %>%
      add_xml_data(L2327.StubTechCalInput_paper_heat, "StubTechCalInput") %>%
      add_xml_data(L2327.StubTechCoef_paper, "StubTechCoef") %>%
      add_xml_data(L2327.PerCapitaBased_paper, "PerCapitaBased") %>%
      add_xml_data(L2327.BaseService_paper, "BaseService") %>%
      add_xml_data(L2327.PriceElasticity_paper, "PriceElasticity") %>%
      add_xml_data(L2327.GlobalTechSecOut_paper, "GlobalTechSecOut") %>%
      add_precursors("L2327.Supplysector_paper", "L2327.FinalEnergyKeyword_paper", "L2327.SubsectorLogit_paper",
                     "L2327.SubsectorShrwtFllt_paper", "L2327.SubsectorInterp_paper", "L2327.StubTech_paper",
                     "L2327.GlobalTechSCurve_paper", "L2327.GlobalTechProfitShutdown_paper", "L2327.GlobalTechShrwt_paper",
                     "L2327.GlobalTechCoef_paper", "L2327.GlobalTechCost_paper", "L2327.GlobalTechCapture_paper",
                     "L2327.StubTechProd_paper", "L2327.StubTechCalInput_paper_heat", "L2327.StubTechCoef_paper",
                     "L2327.PerCapitaBased_paper", "L2327.BaseService_paper", "L2327.PriceElasticity_paper",
                     "L2327.GlobalTechSecOut_paper", "L2327.GlobalTechTrackCapital_paper") ->
      paper.xml

    return_data(paper.xml)
  } else {
    stop("Unknown command")
  }
}
