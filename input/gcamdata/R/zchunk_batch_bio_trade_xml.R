# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_batch_bio_trade_xml
#'
#' Construct XML data structure for \code{bio_trade.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{bio_trade.xml}. The corresponding file in the
#' original data system was \code{batch_bio_trade.xml.R} (aglu XML).
module_aglu_batch_bio_trade_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L243.DeleteInput_RegBio",
             "L243.TechCoef_RegBio",
             "L243.Supplysector_Bio",
             "L243.SectorUseTrialMarket_Bio",
             "L243.SubsectorLogit_Bio",
             "L243.SubsectorShrwt_TotBio",
             "L243.SubsectorShrwtFllt_TradedBio",
             "L243.GlobalTechCoef_TotBio",
             "L243.GlobalTechShrwt_TotBio",
             "L243.StubTech_TotBio",
             "L243.StubTechShrwt_TotBio",
             "L243.StubTechCoef_ImportedBio",
             "L243.StubTechCoef_DomesticBio",
             "L243.TechCoef_TradedBio",
             "L243.TechShrwt_TradedBio"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "bio_trade.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L243.DeleteInput_RegBio <- get_data(all_data, "L243.DeleteInput_RegBio")
    L243.TechCoef_RegBio <- get_data(all_data, "L243.TechCoef_RegBio")
    L243.Supplysector_Bio <- get_data(all_data, "L243.Supplysector_Bio")
    L243.SectorUseTrialMarket_Bio <- get_data(all_data, "L243.SectorUseTrialMarket_Bio")
    L243.SubsectorLogit_Bio <- get_data(all_data, "L243.SubsectorLogit_Bio")
    L243.SubsectorShrwt_TotBio <- get_data(all_data, "L243.SubsectorShrwt_TotBio")
    L243.SubsectorShrwtFllt_TradedBio <- get_data(all_data, "L243.SubsectorShrwtFllt_TradedBio")
    L243.GlobalTechCoef_TotBio <- get_data(all_data, "L243.GlobalTechCoef_TotBio")
    L243.GlobalTechShrwt_TotBio <- get_data(all_data, "L243.GlobalTechShrwt_TotBio")
    L243.StubTech_TotBio <- get_data(all_data, "L243.StubTech_TotBio")
    L243.StubTechShrwt_TotBio <- get_data(all_data, "L243.StubTechShrwt_TotBio")
    L243.StubTechCoef_ImportedBio <- get_data(all_data, "L243.StubTechCoef_ImportedBio")
    L243.StubTechCoef_DomesticBio <- get_data(all_data, "L243.StubTechCoef_DomesticBio")
    L243.TechCoef_TradedBio <- get_data(all_data, "L243.TechCoef_TradedBio")
    L243.TechShrwt_TradedBio <- get_data(all_data, "L243.TechShrwt_TradedBio")

    market <- NULL # Silence package check

    # ===================================================
    # Rename tibble columns to match header info.
    L243.StubTechCoef_ImportedBio <- rename(L243.StubTechCoef_ImportedBio, market.name = market)
    L243.StubTechCoef_DomesticBio <- rename(L243.StubTechCoef_DomesticBio, market.name = market)

    # Produce outputs
    create_xml("bio_trade.xml") %>%
      add_xml_data(L243.DeleteInput_RegBio, "DeleteInput") %>%
      add_xml_data(L243.TechCoef_RegBio, "TechCoef") %>%
      add_logit_tables_xml(L243.Supplysector_Bio, "Supplysector") %>%
      add_xml_data(L243.SectorUseTrialMarket_Bio, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L243.SubsectorLogit_Bio, "SubsectorLogit") %>%
      add_xml_data(L243.SubsectorShrwt_TotBio, "SubsectorShrwt") %>%
      add_xml_data(L243.SubsectorShrwtFllt_TradedBio, "SubsectorShrwtFllt") %>%
      add_xml_data(L243.GlobalTechCoef_TotBio, "GlobalTechCoef") %>%
      add_xml_data(L243.GlobalTechShrwt_TotBio, "GlobalTechShrwt") %>%
      add_xml_data(L243.StubTech_TotBio, "StubTech") %>%
      add_xml_data(L243.StubTechShrwt_TotBio, "StubTechShrwt") %>%
      add_xml_data(L243.StubTechCoef_ImportedBio, "StubTechCoef") %>%
      add_xml_data(L243.StubTechCoef_DomesticBio, "StubTechCoef") %>%
      add_xml_data(L243.TechCoef_TradedBio, "TechCoef") %>%
      add_xml_data(L243.TechShrwt_TradedBio, "TechShrwt") %>%
      add_precursors("L243.DeleteInput_RegBio",
                     "L243.TechCoef_RegBio",
                     "L243.Supplysector_Bio",
                     "L243.SectorUseTrialMarket_Bio",
                     "L243.SubsectorLogit_Bio",
                     "L243.SubsectorShrwt_TotBio",
                     "L243.SubsectorShrwtFllt_TradedBio",
                     "L243.GlobalTechCoef_TotBio",
                     "L243.GlobalTechShrwt_TotBio",
                     "L243.StubTech_TotBio",
                     "L243.StubTechShrwt_TotBio",
                     "L243.StubTechCoef_ImportedBio",
                     "L243.StubTechCoef_DomesticBio",
                     "L243.TechCoef_TradedBio",
                     "L243.TechShrwt_TradedBio") ->
      bio_trade.xml

    return_data(bio_trade.xml)
  } else {
    stop("Unknown command")
  }
}
