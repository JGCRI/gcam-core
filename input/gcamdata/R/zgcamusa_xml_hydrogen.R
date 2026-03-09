# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_hydrogen_xml
#'
#' Construct XML data structure for \code{hydrogen_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{hydrogen_USA.xml}. The corresponding file in the
#' original data system was \code{batch_hydrogen_USA_xml.R} (gcamusa XML).
module_gcamusa_hydrogen_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L225.DeleteSupplysector_h2_USA",
             "L225.Supplysector_h2_USA",
             "L225.SectorUseTrialMarket_h2_USA",
             "L225.SubsectorLogit_h2_USA",
             "L225.SubsectorShrwtFllt_h2_USA",
             "L225.StubTech_h2_USA",
             "L225.StubTechMarket_h2_USA",
             "L225.DeleteStubTechMinicamEnergyInput_H2_USA",
             "L225.StubTechCost_h2_USA_ref",
             "L225.StubTechCost_h2_USA_adv",
             "L225.StubTechCost_h2_USA_lotech",
             "L225.StubTechCoef_h2_USA_ref",
             "L225.StubTechCoef_h2_USA_adv",
             "L225.StubTechCoef_h2_USA_lotech",
             "L225.InterestRate_PADD",
             "L225.Pop_PADD",
             "L225.GDP_PADD",
             "L225.Supplysector_h2_PADD",
             "L225.SubsectorShrwtFllt_h2_PADD",
             "L225.SubsectorShrwt_h2_PADD",
             "L225.SubsectorLogit_h2_PADD",
             "L225.TechShrwt_h2_PADD",
             "L225.TechCoef_h2_PADD"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "hydrogen_USA.xml",
             XML = "hydrogen_electrolysis_USA_adv.xml",
             XML = "hydrogen_electrolysis_USA_lotech.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L225.DeleteStubTechMinicamEnergyInput_H2_USA <- get_data(all_data, "L225.DeleteStubTechMinicamEnergyInput_H2_USA")
    L225.DeleteSupplysector_h2_USA <- get_data(all_data, "L225.DeleteSupplysector_h2_USA")
    L225.Supplysector_h2_USA <- get_data(all_data, "L225.Supplysector_h2_USA")
    L225.SectorUseTrialMarket_h2_USA <- get_data(all_data, "L225.SectorUseTrialMarket_h2_USA")
    L225.SubsectorLogit_h2_USA <- get_data(all_data, "L225.SubsectorLogit_h2_USA")
    L225.SubsectorShrwtFllt_h2_USA <- get_data(all_data, "L225.SubsectorShrwtFllt_h2_USA")
    L225.StubTech_h2_USA <- get_data(all_data, "L225.StubTech_h2_USA")
    L225.StubTechMarket_h2_USA <- get_data(all_data, "L225.StubTechMarket_h2_USA")
    L225.StubTechCost_h2_USA_ref <- get_data(all_data, "L225.StubTechCost_h2_USA_ref")
    L225.StubTechCost_h2_USA_adv <- get_data(all_data, "L225.StubTechCost_h2_USA_adv")
    L225.StubTechCost_h2_USA_lotech <- get_data(all_data, "L225.StubTechCost_h2_USA_lotech")
    L225.StubTechCoef_h2_USA_ref <- get_data(all_data, "L225.StubTechCoef_h2_USA_ref")
    L225.StubTechCoef_h2_USA_adv <- get_data(all_data, "L225.StubTechCoef_h2_USA_adv")
    L225.StubTechCoef_h2_USA_lotech <- get_data(all_data, "L225.StubTechCoef_h2_USA_lotech")

    L225.InterestRate_PADD <- get_data(all_data, "L225.InterestRate_PADD")
    L225.Pop_PADD <- get_data(all_data, "L225.Pop_PADD")
    L225.GDP_PADD <- get_data(all_data, "L225.GDP_PADD")
    L225.Supplysector_h2_PADD <- get_data(all_data, "L225.Supplysector_h2_PADD")
    L225.SubsectorShrwtFllt_h2_PADD <- get_data(all_data,  "L225.SubsectorShrwtFllt_h2_PADD")
    L225.SubsectorShrwt_h2_PADD <- get_data(all_data, "L225.SubsectorShrwt_h2_PADD")
    L225.SubsectorLogit_h2_PADD <- get_data(all_data, "L225.SubsectorLogit_h2_PADD")
    L225.TechShrwt_h2_PADD <- get_data(all_data, "L225.TechShrwt_h2_PADD")
    L225.TechCoef_h2_PADD <- get_data(all_data, "L225.TechCoef_h2_PADD")

    # ===================================================

    # Produce outputs
    create_xml("hydrogen_USA.xml") %>%
      add_xml_data(L225.DeleteSupplysector_h2_USA, "DeleteSupplysector") %>%
      add_logit_tables_xml(L225.Supplysector_h2_USA, "Supplysector") %>%
      add_xml_data(L225.SectorUseTrialMarket_h2_USA, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L225.SubsectorLogit_h2_USA, "SubsectorLogit") %>%
      add_xml_data(L225.SubsectorShrwtFllt_h2_USA, "SubsectorShrwtFllt") %>%
      add_xml_data(L225.StubTech_h2_USA, "StubTech") %>%
      add_xml_data(L225.StubTechCost_h2_USA_ref, "StubTechCost") %>%
      add_xml_data(L225.StubTechCoef_h2_USA_ref, "StubTechCoef") %>%
      add_xml_data(L225.StubTechMarket_h2_USA, "StubTechMarket") %>%
      add_xml_data(L225.DeleteStubTechMinicamEnergyInput_H2_USA,"DeleteStubTechMinicamEnergyInput") %>%
      add_xml_data(L225.InterestRate_PADD, "InterestRate") %>%
      add_xml_data(L225.Pop_PADD, "Pop") %>%
      add_xml_data(L225.GDP_PADD, "GDP") %>%
      add_logit_tables_xml(L225.Supplysector_h2_PADD, "Supplysector") %>%
      add_logit_tables_xml(L225.SubsectorLogit_h2_PADD, "SubsectorLogit") %>%
      add_xml_data(L225.SubsectorShrwtFllt_h2_PADD, "SubsectorShrwtFllt") %>%
      add_xml_data(L225.SubsectorShrwt_h2_PADD, "SubsectorShrwt") %>%
      add_xml_data(L225.TechShrwt_h2_PADD, "TechShrwt") %>%
      add_xml_data(L225.TechCoef_h2_PADD, "TechCoef") %>%
      add_precursors("L225.DeleteSupplysector_h2_USA",
                     "L225.Supplysector_h2_USA",
                     "L225.SectorUseTrialMarket_h2_USA",
                     "L225.SubsectorLogit_h2_USA",
                     "L225.SubsectorShrwtFllt_h2_USA",
                     "L225.StubTech_h2_USA",
                     "L225.StubTechMarket_h2_USA",
                     "L225.DeleteStubTechMinicamEnergyInput_H2_USA",
                     "L225.StubTechCost_h2_USA_ref",
                     "L225.StubTechCoef_h2_USA_ref",
                     "L225.InterestRate_PADD",
                     "L225.Pop_PADD",
                     "L225.GDP_PADD",
                     "L225.Supplysector_h2_PADD",
                     "L225.SubsectorShrwtFllt_h2_PADD",
                     "L225.SubsectorShrwt_h2_PADD",
                     "L225.SubsectorLogit_h2_PADD",
                     "L225.TechShrwt_h2_PADD",
                     "L225.TechCoef_h2_PADD") ->
      hydrogen_USA.xml

    create_xml("hydrogen_electrolysis_USA_adv.xml") %>%
      add_xml_data(L225.StubTechCost_h2_USA_adv, "StubTechCost") %>%
      add_xml_data(L225.StubTechCoef_h2_USA_adv, "StubTechCoef") %>%
      add_precursors("L225.StubTechCost_h2_USA_adv",
                     "L225.StubTechCoef_h2_USA_adv") ->
      hydrogen_electrolysis_USA_adv.xml

    create_xml("hydrogen_electrolysis_USA_lotech.xml") %>%
      add_xml_data(L225.StubTechCost_h2_USA_lotech, "StubTechCost") %>%
      add_xml_data(L225.StubTechCoef_h2_USA_lotech, "StubTechCoef") %>%
      add_precursors("L225.StubTechCost_h2_USA_lotech",
                     "L225.StubTechCoef_h2_USA_lotech") ->
      hydrogen_electrolysis_USA_lotech.xml

    return_data(hydrogen_USA.xml,
                hydrogen_electrolysis_USA_adv.xml,
                hydrogen_electrolysis_USA_lotech.xml)
  } else {
    stop("Unknown command")
  }
}
