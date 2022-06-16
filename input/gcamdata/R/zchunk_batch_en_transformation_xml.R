# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_en_transformation_xml
#'
#' Construct XML data structure for \code{en_transformation.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_transformation.xml}. The corresponding file in the
#' original data system was \code{batch_en_transformation.xml.R} (energy XML).
module_energy_batch_en_transformation_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L222.Supplysector_en",
             "L222.SectorUseTrialMarket_en",
             "L222.SubsectorLogit_en",
             "L222.SubsectorShrwtFllt_en",
             "L222.SubsectorInterp_en",
             "L222.StubTech_en",
             "L222.GlobalTechInterp_en",
             "L222.GlobalTechCoef_en",
             "L222.GlobalTechCost_en",
             "L222.GlobalTechShrwt_en",
             "L222.GlobalTechCapture_en",
             "L222.GlobalTechSCurve_en",
             "L222.GlobalTechProfitShutdown_en",
             "L222.GlobalTechKeyword_en",
             "L222.StubTechProd_gasproc",
             "L222.StubTechProd_refining",
             "L222.StubTechCoef_refining"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_transformation.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L222.Supplysector_en <- get_data(all_data, "L222.Supplysector_en")
    L222.SectorUseTrialMarket_en <- get_data(all_data, "L222.SectorUseTrialMarket_en")
    L222.SubsectorLogit_en <- get_data(all_data, "L222.SubsectorLogit_en")
    L222.SubsectorShrwtFllt_en <- get_data(all_data, "L222.SubsectorShrwtFllt_en")
    L222.SubsectorInterp_en <- get_data(all_data, "L222.SubsectorInterp_en")
    L222.StubTech_en <- get_data(all_data, "L222.StubTech_en")
    L222.GlobalTechInterp_en <- get_data(all_data, "L222.GlobalTechInterp_en")
    L222.GlobalTechCoef_en <- get_data(all_data, "L222.GlobalTechCoef_en")
    L222.GlobalTechCost_en <- get_data(all_data, "L222.GlobalTechCost_en")
    L222.GlobalTechShrwt_en <- get_data(all_data, "L222.GlobalTechShrwt_en")
    L222.GlobalTechCapture_en <- get_data(all_data, "L222.GlobalTechCapture_en")
    L222.GlobalTechSCurve_en <- get_data(all_data, "L222.GlobalTechSCurve_en")
    L222.GlobalTechProfitShutdown_en <- get_data(all_data, "L222.GlobalTechProfitShutdown_en")
    L222.GlobalTechKeyword_en <- get_data(all_data, "L222.GlobalTechKeyword_en")
    L222.StubTechProd_gasproc <- get_data(all_data, "L222.StubTechProd_gasproc")
    L222.StubTechProd_refining <- get_data(all_data, "L222.StubTechProd_refining")
    L222.StubTechCoef_refining <- get_data(all_data, "L222.StubTechCoef_refining")

    year.share.weight <- share.weight <- NULL # silence package checks
    # ===================================================
    # Rename the tibble columns to match the header information.
    L222.StubTechProd_gasproc <- rename(L222.StubTechProd_gasproc, share.weight.year = year.share.weight, tech.share.weight = share.weight)
    L222.StubTechProd_refining <- rename(L222.StubTechProd_refining, share.weight.year = year.share.weight, tech.share.weight = share.weight)

    # Produce outputs
    create_xml("en_transformation.xml") %>%
      add_logit_tables_xml(L222.Supplysector_en, "Supplysector") %>%
      add_xml_data(L222.SectorUseTrialMarket_en, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L222.SubsectorLogit_en, "SubsectorLogit") %>%
      add_xml_data(L222.SubsectorShrwtFllt_en, "SubsectorShrwtFllt") %>%
      add_xml_data(L222.SubsectorInterp_en, "SubsectorInterp") %>%
      add_xml_data(L222.StubTech_en, "StubTech") %>%
      add_xml_data(L222.GlobalTechInterp_en, "GlobalTechInterp") %>%
      add_xml_data(L222.GlobalTechCoef_en, "GlobalTechCoef") %>%
      add_xml_data(L222.GlobalTechCost_en, "GlobalTechCost") %>%
      add_xml_data(L222.GlobalTechShrwt_en, "GlobalTechShrwt") %>%
      add_xml_data(L222.GlobalTechCapture_en, "GlobalTechCapture") %>%
      add_xml_data(L222.GlobalTechSCurve_en, "GlobalTechSCurve") %>%
      add_xml_data(L222.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown") %>%
      add_xml_data(L222.GlobalTechKeyword_en, "PrimaryConsKeyword") %>%
      add_xml_data(L222.StubTechProd_gasproc, "StubTechProd") %>%
      add_xml_data(L222.StubTechProd_refining, "StubTechProd") %>%
      add_xml_data(L222.StubTechCoef_refining, "StubTechCoef") %>%
      add_precursors("L222.Supplysector_en",
                     "L222.SectorUseTrialMarket_en",
                     "L222.SubsectorLogit_en",
                     "L222.SubsectorShrwtFllt_en",
                     "L222.SubsectorInterp_en",
                     "L222.StubTech_en",
                     "L222.GlobalTechInterp_en",
                     "L222.GlobalTechCoef_en",
                     "L222.GlobalTechCost_en",
                     "L222.GlobalTechShrwt_en",
                     "L222.GlobalTechCapture_en",
                     "L222.GlobalTechSCurve_en",
                     "L222.GlobalTechProfitShutdown_en",
                     "L222.GlobalTechKeyword_en",
                     "L222.StubTechProd_gasproc",
                     "L222.StubTechProd_refining",
                     "L222.StubTechCoef_refining") ->
      en_transformation.xml

    return_data(en_transformation.xml)
  } else {
    stop("Unknown command")
  }
}
