# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_hydrogen_xml
#'
#' Construct XML data structure for \code{hydrogen.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{hydrogen.xml}. The corresponding file in the
#' original data system was \code{batch_hydrogen.xml.R} (energy XML).
module_energy_hydrogen_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L225.Supplysector_h2",
             "L225.SectorUseTrialMarket_h2",
             "L225.SubsectorLogit_h2",
             "L225.SubsectorShrwtFllt_h2",
             "L225.StubTech_h2",
             "L225.GlobalTechCoef_h2_ref",
             "L225.GlobalTechCoef_h2_adv",
             "L225.GlobalTechCoef_h2_lotech",
             "L225.StubTechCoef_h2_hybrid_ref",
             "L225.StubTechCoef_h2_hybrid_adv",
             "L225.StubTechCoef_h2_hybrid_lotech",
             "L225.GlobalTechCost_h2_ref",
             "L225.GlobalTechCost_h2_adv",
             "L225.GlobalTechCost_h2_lotech",
             "L225.GlobalTechTrackCapital_h2_ref",
             "L225.GlobalTechTrackCapital_h2_adv",
             "L225.GlobalTechTrackCapital_h2_lotech",
             "L225.GlobalTechShrwt_h2",
             "L225.PrimaryRenewKeyword_h2",
             "L225.AvgFossilEffKeyword_h2",
             "L225.GlobalTechCapture_h2",
             "L225.GlobalTechInputPMult_h2",
             "L225.GlobalTechProfitShutdown_h2",
             "L225.GlobalTechSCurve_h2",
             "L225.StubTechCost_h2_hybrid_ref",
             "L225.StubTechCost_h2_hybrid_adv",
             "L225.StubTechCost_h2_hybrid_lotech",
             "L225.OutputEmissCoeff_h2"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "hydrogen.xml",
             XML = "hydrogen_adv.xml",
             XML = "hydrogen_lotech.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L225.Supplysector_h2 <- get_data(all_data, "L225.Supplysector_h2")
    L225.SectorUseTrialMarket_h2 <- get_data(all_data, "L225.SectorUseTrialMarket_h2")
    L225.SubsectorLogit_h2 <- get_data(all_data, "L225.SubsectorLogit_h2")
    L225.SubsectorShrwtFllt_h2 <- get_data(all_data, "L225.SubsectorShrwtFllt_h2")
    L225.StubTech_h2 <- get_data(all_data, "L225.StubTech_h2")
    L225.GlobalTechCoef_h2_ref <- get_data(all_data, "L225.GlobalTechCoef_h2_ref")
    L225.GlobalTechCoef_h2_adv <- get_data(all_data, "L225.GlobalTechCoef_h2_adv")
    L225.GlobalTechCoef_h2_lotech <- get_data(all_data, "L225.GlobalTechCoef_h2_lotech")
    L225.StubTechCoef_h2_hybrid_ref <- get_data(all_data, "L225.StubTechCoef_h2_hybrid_ref")
    L225.StubTechCoef_h2_hybrid_adv <- get_data(all_data, "L225.StubTechCoef_h2_hybrid_adv")
    L225.StubTechCoef_h2_hybrid_lotech <- get_data(all_data, "L225.StubTechCoef_h2_hybrid_lotech")
    L225.GlobalTechCost_h2_ref <- get_data(all_data, "L225.GlobalTechCost_h2_ref")
    L225.GlobalTechCost_h2_adv <- get_data(all_data, "L225.GlobalTechCost_h2_adv")
    L225.GlobalTechCost_h2_lotech <- get_data(all_data, "L225.GlobalTechCost_h2_lotech")
    L225.GlobalTechTrackCapital_h2_ref <- get_data(all_data, "L225.GlobalTechTrackCapital_h2_ref")
    L225.GlobalTechTrackCapital_h2_adv <- get_data(all_data, "L225.GlobalTechTrackCapital_h2_adv")
    L225.GlobalTechTrackCapital_h2_lotech <- get_data(all_data, "L225.GlobalTechTrackCapital_h2_lotech")
    L225.GlobalTechShrwt_h2 <- get_data(all_data, "L225.GlobalTechShrwt_h2")
    L225.PrimaryRenewKeyword_h2 <- get_data(all_data, "L225.PrimaryRenewKeyword_h2")
    L225.AvgFossilEffKeyword_h2 <- get_data(all_data, "L225.AvgFossilEffKeyword_h2")
    L225.GlobalTechCapture_h2 <- get_data(all_data, "L225.GlobalTechCapture_h2")
    L225.GlobalTechInputPMult_h2 <- get_data(all_data, "L225.GlobalTechInputPMult_h2")
    L225.GlobalTechProfitShutdown_h2 <- get_data(all_data, "L225.GlobalTechProfitShutdown_h2")
    L225.GlobalTechSCurve_h2 <- get_data(all_data, "L225.GlobalTechSCurve_h2")
    L225.StubTechCost_h2_hybrid_ref <- get_data(all_data, "L225.StubTechCost_h2_hybrid_ref")
    L225.StubTechCost_h2_hybrid_adv <- get_data(all_data, "L225.StubTechCost_h2_hybrid_adv")
    L225.StubTechCost_h2_hybrid_lotech <- get_data(all_data, "L225.StubTechCost_h2_hybrid_lotech")
    L225.OutputEmissCoeff_h2 <- get_data(all_data, "L225.OutputEmissCoeff_h2")
    # ===================================================

    # Produce outputs
    create_xml("hydrogen.xml") %>%
      add_logit_tables_xml(L225.Supplysector_h2, "Supplysector") %>%
      add_xml_data(L225.SectorUseTrialMarket_h2, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L225.SubsectorLogit_h2, "SubsectorLogit") %>%
      add_xml_data(L225.SubsectorShrwtFllt_h2, "SubsectorShrwtFllt") %>%
      add_xml_data(L225.StubTech_h2, "StubTech") %>%
      add_xml_data(L225.StubTechCost_h2_hybrid_ref, "StubTechCost") %>%
      add_xml_data(L225.StubTechCoef_h2_hybrid_ref, "StubTechCoef") %>%
      add_xml_data(L225.GlobalTechCoef_h2_ref, "GlobalTechCoef") %>%
      # set non-energy inputs to be read as tracking
      add_node_equiv_xml("input") %>%
      add_xml_data(L225.GlobalTechTrackCapital_h2_ref, "GlobalTechTrackCapital") %>%
      add_xml_data(L225.GlobalTechCost_h2_ref, "GlobalTechCost") %>%
      add_xml_data(L225.GlobalTechShrwt_h2, "GlobalTechShrwt") %>%
      add_xml_data(L225.PrimaryRenewKeyword_h2, "PrimaryRenewKeyword") %>%
      add_xml_data(L225.AvgFossilEffKeyword_h2, "AvgFossilEffKeyword") %>%
      add_xml_data(L225.GlobalTechCapture_h2, "GlobalTechCapture") %>%
      add_xml_data(L225.GlobalTechInputPMult_h2, "GlobalTechInputPMult") %>%
      add_xml_data(L225.GlobalTechSCurve_h2, "GlobalTechSCurve") %>%
      add_xml_data(L225.GlobalTechProfitShutdown_h2, "GlobalTechProfitShutdown") %>%
      add_xml_data(L225.OutputEmissCoeff_h2, "OutputEmissCoeff") %>%
      add_precursors("L225.Supplysector_h2",
                     "L225.SectorUseTrialMarket_h2",
                     "L225.SubsectorLogit_h2",
                     "L225.SubsectorShrwtFllt_h2",
                     "L225.StubTech_h2",
                     "L225.GlobalTechCoef_h2_ref",
                     "L225.StubTechCoef_h2_hybrid_ref",
                     "L225.GlobalTechCost_h2_ref",
                     "L225.GlobalTechTrackCapital_h2_ref",
                     "L225.GlobalTechShrwt_h2",
                     "L225.PrimaryRenewKeyword_h2",
                     "L225.AvgFossilEffKeyword_h2",
                     "L225.GlobalTechCapture_h2",
                     "L225.GlobalTechInputPMult_h2",
                     "L225.GlobalTechProfitShutdown_h2",
                     "L225.GlobalTechSCurve_h2",
                     "L225.StubTechCost_h2_hybrid_ref",
                     "L225.OutputEmissCoeff_h2") ->
      hydrogen.xml

    create_xml("hydrogen_adv.xml") %>%
      add_xml_data(L225.StubTechCoef_h2_hybrid_adv, "StubTechCoef") %>%
      add_xml_data(L225.GlobalTechTrackCapital_h2_adv, "GlobalTechTrackCapital") %>%
      add_xml_data(L225.StubTechCost_h2_hybrid_adv, "StubTechCost") %>%
      add_xml_data(L225.GlobalTechCost_h2_adv, "GlobalTechCost") %>%
      add_xml_data(L225.GlobalTechCoef_h2_adv, "GlobalTechCoef") %>%
      add_precursors("L225.StubTechCoef_h2_hybrid_adv",
                     "L225.GlobalTechTrackCapital_h2_adv",
                     "L225.StubTechCost_h2_hybrid_adv",
                     "L225.GlobalTechCost_h2_adv",
                     "L225.GlobalTechCoef_h2_adv") ->
      hydrogen_adv.xml

    create_xml("hydrogen_lotech.xml") %>%
      add_xml_data(L225.StubTechCoef_h2_hybrid_lotech, "StubTechCoef") %>%
      add_xml_data(L225.GlobalTechTrackCapital_h2_lotech, "GlobalTechTrackCapital") %>%
      add_xml_data(L225.StubTechCost_h2_hybrid_lotech, "StubTechCost") %>%
      add_xml_data(L225.GlobalTechCost_h2_lotech, "GlobalTechCost") %>%
      add_xml_data(L225.GlobalTechCoef_h2_lotech, "GlobalTechCoef") %>%
      add_precursors("L225.StubTechCoef_h2_hybrid_lotech",
                     "L225.GlobalTechTrackCapital_h2_lotech",
                     "L225.StubTechCost_h2_hybrid_lotech",
                     "L225.GlobalTechCost_h2_lotech",
                     "L225.GlobalTechCoef_h2_lotech") ->
      hydrogen_lotech.xml

     return_data(hydrogen.xml,
                hydrogen_adv.xml,
                hydrogen_lotech.xml)
  } else {
    stop("Unknown command")
  }
}
