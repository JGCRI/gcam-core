# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_en_supply_xml
#'
#' Construct XML data structure for \code{en_supply.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_supply.xml}. The corresponding file in the
#' original data system was \code{batch_en_supply_xml.R} (energy XML).
module_energy_batch_en_supply_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L221.Supplysector_en",
             "L221.SectorUseTrialMarket_en",
             "L221.SubsectorLogit_en",
             "L221.SubsectorShrwt_en",
             "L221.SubsectorShrwtFllt_en",
             "L221.SubsectorInterp_en",
             "L221.SubsectorInterpTo_en",
             "L221.StubTech_en",
             "L221.GlobalTechCoef_en",
             "L221.GlobalTechCost_en",
             "L221.GlobalTechShrwt_en",
             "L221.PrimaryConsKeyword_en",
             "L221.StubTechFractSecOut_en",
             "L221.StubTechFractProd_en",
             "L221.StubTechFractCalPrice_en",
             "L221.Rsrc_en",
             "L221.RsrcPrice_en",
             "L221.TechCoef_en_Traded",
             "L221.TechCost_en_Traded",
             "L221.TechShrwt_en_Traded",
             "L221.StubTechCoef_unoil",
             "L221.Production_unoil",
             "L221.StubTechProd_oil_unoil",
             "L221.StubTechProd_oil_crude",
             "L221.StubTechCalInput_bioOil",
             "L221.StubTechInterp_bioOil",
             "L221.StubTechShrwt_bioOil"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_supply.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L221.Supplysector_en <- get_data(all_data, "L221.Supplysector_en")
    L221.SectorUseTrialMarket_en <- get_data(all_data, "L221.SectorUseTrialMarket_en")
    L221.SubsectorLogit_en <- get_data(all_data, "L221.SubsectorLogit_en")
    L221.SubsectorShrwt_en <- get_data(all_data, "L221.SubsectorShrwt_en")
    L221.SubsectorShrwtFllt_en <- get_data(all_data, "L221.SubsectorShrwtFllt_en")
    L221.SubsectorInterp_en <- get_data(all_data, "L221.SubsectorInterp_en")
    L221.SubsectorInterpTo_en <- get_data(all_data, "L221.SubsectorInterpTo_en")
    L221.StubTech_en <- get_data(all_data, "L221.StubTech_en")
    L221.GlobalTechCoef_en <- get_data(all_data, "L221.GlobalTechCoef_en")
    L221.GlobalTechCost_en <- get_data(all_data, "L221.GlobalTechCost_en")
    L221.GlobalTechShrwt_en <- get_data(all_data, "L221.GlobalTechShrwt_en")
    L221.PrimaryConsKeyword_en <- get_data(all_data, "L221.PrimaryConsKeyword_en")
    L221.StubTechFractSecOut_en <- get_data(all_data, "L221.StubTechFractSecOut_en")
    L221.StubTechFractProd_en <- get_data(all_data, "L221.StubTechFractProd_en")
    L221.StubTechFractCalPrice_en <- get_data(all_data, "L221.StubTechFractCalPrice_en")
    L221.Rsrc_en <- get_data(all_data, "L221.Rsrc_en")
    L221.RsrcPrice_en <- get_data(all_data, "L221.RsrcPrice_en")
    L221.TechCoef_en_Traded <- get_data(all_data, "L221.TechCoef_en_Traded")
    L221.TechCost_en_Traded <- get_data(all_data, "L221.TechCost_en_Traded")
    L221.TechShrwt_en_Traded <- get_data(all_data, "L221.TechShrwt_en_Traded")
    L221.StubTechCoef_unoil <- get_data(all_data, "L221.StubTechCoef_unoil")
    L221.Production_unoil <- get_data(all_data, "L221.Production_unoil")
    L221.StubTechProd_oil_unoil <- get_data(all_data, "L221.StubTechProd_oil_unoil")
    L221.StubTechProd_oil_crude <- get_data(all_data, "L221.StubTechProd_oil_crude")
    L221.StubTechCalInput_bioOil <- get_data(all_data, "L221.StubTechCalInput_bioOil")
    L221.StubTechInterp_bioOil <- get_data(all_data, "L221.StubTechInterp_bioOil")
    L221.StubTechShrwt_bioOil <- get_data(all_data, "L221.StubTechShrwt_bioOil")

    # ===================================================

    # Produce outputs
    create_xml("en_supply.xml") %>%
      add_logit_tables_xml(L221.Supplysector_en, "Supplysector") %>%
      add_xml_data(L221.SectorUseTrialMarket_en, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L221.SubsectorLogit_en, "SubsectorLogit") ->
        en_supply.xml

      # Some data inputs may not actually contain data. If so, do not add_xml_data.
      if(!is.null(L221.SubsectorShrwt_en)) {
        en_supply.xml %>%
          add_xml_data(L221.SubsectorShrwt_en, "SubsectorShrwt") ->
          en_supply.xml
      }

      if(!is.null(L221.SubsectorShrwtFllt_en)) {
        en_supply.xml %>%
          add_xml_data(L221.SubsectorShrwtFllt_en, "SubsectorShrwtFllt") ->
          en_supply.xml
      }

      if(!is.null(L221.SubsectorInterp_en)) {
        en_supply.xml %>%
          add_xml_data(L221.SubsectorInterp_en, "SubsectorInterp") ->
          en_supply.xml
      }

      if(!is.null(L221.SubsectorInterpTo_en)) {
        en_supply.xml %>%
          add_xml_data(L221.SubsectorInterpTo_en, "SubsectorInterpTo") ->
          en_supply.xml
      }

    en_supply.xml %>%
      add_xml_data(L221.StubTech_en, "StubTech") %>%
      add_xml_data(L221.GlobalTechCoef_en, "GlobalTechCoef") %>%
      add_xml_data(L221.GlobalTechCost_en, "GlobalTechCost") %>%
      add_xml_data(L221.GlobalTechShrwt_en, "GlobalTechShrwt") %>%
      add_xml_data(L221.PrimaryConsKeyword_en, "PrimaryConsKeyword") %>%
      add_xml_data(L221.StubTechFractSecOut_en, "StubTechFractSecOut") %>%
      add_xml_data(L221.StubTechFractProd_en, "StubTechFractProd") %>%
      add_xml_data(L221.StubTechFractCalPrice_en, "StubTechFractCalPrice") %>%
      add_xml_data(L221.Rsrc_en, "Rsrc") %>%
      add_xml_data(L221.RsrcPrice_en, "RsrcPrice") %>%
      add_xml_data(L221.TechCoef_en_Traded, "TechCoef") %>%
      add_xml_data(L221.TechCost_en_Traded, "TechCost") %>%
      add_xml_data(L221.TechShrwt_en_Traded, "TechShrwt") %>%
      add_xml_data(L221.StubTechCoef_unoil, "StubTechCoef") %>%
      add_xml_data(L221.Production_unoil, "Production", column_order_lookup = NULL) %>%
      add_xml_data(L221.StubTechProd_oil_unoil, "StubTechProd", column_order_lookup = NULL) %>%
      add_xml_data(L221.StubTechProd_oil_crude, "StubTechProd", column_order_lookup = NULL) %>%
      add_xml_data(L221.StubTechCalInput_bioOil, "StubTechCalInput") %>%
      add_xml_data(L221.StubTechInterp_bioOil, "StubTechInterp") %>%
      add_xml_data(L221.StubTechShrwt_bioOil, "StubTechShrwt") %>%
      add_precursors("L221.Supplysector_en", "L221.SectorUseTrialMarket_en", "L221.SubsectorLogit_en",
                     "L221.SubsectorShrwt_en", "L221.SubsectorShrwtFllt_en", "L221.SubsectorInterp_en",
                     "L221.SubsectorInterpTo_en", "L221.StubTech_en", "L221.GlobalTechCoef_en",
                     "L221.GlobalTechCost_en", "L221.GlobalTechShrwt_en", "L221.PrimaryConsKeyword_en",
                     "L221.StubTechFractSecOut_en", "L221.StubTechFractProd_en", "L221.StubTechFractCalPrice_en",
                     "L221.Rsrc_en", "L221.RsrcPrice_en", "L221.TechCoef_en_Traded", "L221.TechCost_en_Traded",
                     "L221.TechShrwt_en_Traded", "L221.StubTechCoef_unoil", "L221.Production_unoil",
                     "L221.StubTechProd_oil_unoil", "L221.StubTechProd_oil_crude", "L221.StubTechCalInput_bioOil",
                     "L221.StubTechInterp_bioOil", "L221.StubTechShrwt_bioOil") ->
      en_supply.xml

    return_data(en_supply.xml)
  } else {
    stop("Unknown command")
  }
}
