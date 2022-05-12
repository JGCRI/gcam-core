# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_other_industry_xml
#'
#' Construct XML data structure for \code{other_industry.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{industry.xml}. The corresponding file in the
#' original data system was \code{batch_industry_xml.R} (energy XML).
module_energy_batch_other_industry_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L232.SubsectorLogit_ind",
             "L232.FinalEnergyKeyword_ind",
             # "L232.SubsectorShrwt_ind",
             "L232.SubsectorInterp_ind",
             # "L232.SubsectorInterpTo_ind",
             "L232.StubTech_ind",
             "L232.GlobalTechShrwt_ind",
             "L232.StubTechInterp_ind",
             "L232.GlobalTechEff_ind",
             "L232.GlobalTechCoef_ind",
             "L232.GlobalTechCost_ind",
             "L232.GlobalTechSecOut_ind",
             "L232.GlobalTechCSeq_ind",
             "L232.StubTechCalInput_indenergy",
             "L232.GlobalTechSCurve_en",
             "L232.GlobalTechProfitShutdown_en",
             "L232.StubTechCalInput_indfeed",
             "L232.StubTechProd_industry",
             "L232.StubTechCoef_industry",
             "L232.FuelPrefElast_indenergy",
             "L232.PerCapitaBased_ind",
             "L232.PriceElasticity_ind",
             "L232.BaseService_ind",
             "L232.SubsectorShrwtFllt_ind",
             "L232.Supplysector_ind"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "other_industry.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L232.SubsectorLogit_ind <- get_data(all_data, "L232.SubsectorLogit_ind")
    L232.FinalEnergyKeyword_ind <- get_data(all_data, "L232.FinalEnergyKeyword_ind")
    # L232.SubsectorShrwt_ind <- get_data(all_data, "L232.SubsectorShrwt_ind")
    L232.SubsectorInterp_ind <- get_data(all_data, "L232.SubsectorInterp_ind")
    # L232.SubsectorInterpTo_ind <- get_data(all_data, "L232.SubsectorInterpTo_ind")
    L232.StubTech_ind <- get_data(all_data, "L232.StubTech_ind")
    L232.GlobalTechShrwt_ind <- get_data(all_data, "L232.GlobalTechShrwt_ind")
    L232.StubTechInterp_ind <- get_data(all_data, "L232.StubTechInterp_ind")
    L232.GlobalTechEff_ind <- get_data(all_data, "L232.GlobalTechEff_ind")
    L232.GlobalTechCoef_ind <- get_data(all_data, "L232.GlobalTechCoef_ind")
    L232.GlobalTechCost_ind <- get_data(all_data, "L232.GlobalTechCost_ind")
    L232.GlobalTechSecOut_ind <- get_data(all_data, "L232.GlobalTechSecOut_ind")
    L232.GlobalTechCSeq_ind <- get_data(all_data, "L232.GlobalTechCSeq_ind")
    L232.StubTechCalInput_indenergy <- get_data(all_data, "L232.StubTechCalInput_indenergy")
    L232.StubTechCalInput_indfeed <- get_data(all_data, "L232.StubTechCalInput_indfeed")
    L232.StubTechProd_industry <- get_data(all_data, "L232.StubTechProd_industry")
    L232.GlobalTechSCurve_en <- get_data(all_data, "L232.GlobalTechSCurve_en")
    L232.GlobalTechProfitShutdown_en <- get_data(all_data, "L232.GlobalTechProfitShutdown_en")
    L232.StubTechCoef_industry <- get_data(all_data, "L232.StubTechCoef_industry")
    L232.FuelPrefElast_indenergy <- get_data(all_data, "L232.FuelPrefElast_indenergy")
    L232.PerCapitaBased_ind <- get_data(all_data, "L232.PerCapitaBased_ind")
    L232.PriceElasticity_ind <- get_data(all_data, "L232.PriceElasticity_ind")
    L232.BaseService_ind <- get_data(all_data, "L232.BaseService_ind")
    L232.SubsectorShrwtFllt_ind <- get_data(all_data, "L232.SubsectorShrwtFllt_ind")
    L232.Supplysector_ind <- get_data(all_data, "L232.Supplysector_ind")

    # ===================================================

    # Produce outputs
    create_xml("other_industry.xml") %>%
      add_logit_tables_xml(L232.Supplysector_ind, "Supplysector") %>%
      add_logit_tables_xml(L232.SubsectorLogit_ind, "SubsectorLogit") %>%
      add_xml_data(L232.SubsectorShrwtFllt_ind, "SubsectorShrwtFllt") %>%
      add_xml_data(L232.FinalEnergyKeyword_ind, "FinalEnergyKeyword") %>%
      # add_xml_data(L232.SubsectorShrwt_ind, "SubsectorShrwt") %>%
      add_xml_data(L232.SubsectorInterp_ind, "SubsectorInterp") %>%
      # add_xml_data(L232.SubsectorInterpTo_ind, "SubsectorInterpTo") %>%
      add_xml_data(L232.StubTech_ind, "StubTech") %>%
      add_xml_data(L232.GlobalTechShrwt_ind, "GlobalTechShrwt") %>%
      add_xml_data(L232.GlobalTechSCurve_en, "GlobalTechSCurve") %>%
      add_xml_data(L232.GlobalTechProfitShutdown_en, "GlobalTechProfitShutdown") %>%
      add_xml_data(L232.StubTechInterp_ind, "StubTechInterp") %>%
      add_xml_data(L232.GlobalTechEff_ind, "GlobalTechEff") %>%
      add_xml_data(L232.GlobalTechCoef_ind, "GlobalTechCoef") %>%
      add_xml_data(L232.GlobalTechCost_ind, "GlobalTechCost") %>%
      add_xml_data(L232.GlobalTechSecOut_ind, "GlobalTechSecOut") %>%
      add_xml_data(L232.GlobalTechCSeq_ind, "GlobalTechCSeq") %>%
      add_xml_data(L232.StubTechCalInput_indenergy, "StubTechCalInput") %>%
      add_xml_data(L232.StubTechCalInput_indfeed, "StubTechCalInput") %>%
      add_xml_data(L232.StubTechProd_industry, "StubTechProd") %>%
      add_xml_data(L232.StubTechCoef_industry, "StubTechCoef") %>%
      add_xml_data(L232.FuelPrefElast_indenergy, "FuelPrefElast") %>%
      add_xml_data(L232.PerCapitaBased_ind, "PerCapitaBased") %>%
      add_xml_data(L232.PriceElasticity_ind, "PriceElasticity") %>%
      add_xml_data(L232.BaseService_ind, "BaseService") %>%
      add_precursors("L232.SubsectorLogit_ind", "L232.FinalEnergyKeyword_ind", #"L232.SubsectorShrwt_ind",
                     "L232.SubsectorInterp_ind", #"L232.SubsectorInterpTo_ind",
                     "L232.StubTech_ind",
                     "L232.GlobalTechShrwt_ind", "L232.StubTechInterp_ind", "L232.GlobalTechEff_ind",
                     "L232.GlobalTechCoef_ind", "L232.GlobalTechCost_ind", "L232.GlobalTechSecOut_ind",
                     "L232.GlobalTechCSeq_ind", "L232.StubTechCalInput_indenergy", "L232.StubTechCalInput_indfeed",
                     "L232.GlobalTechSCurve_en", "L232.GlobalTechProfitShutdown_en",
                     "L232.StubTechProd_industry", "L232.StubTechCoef_industry", "L232.FuelPrefElast_indenergy",
                     "L232.PerCapitaBased_ind", "L232.PriceElasticity_ind", "L232.BaseService_ind",
                     "L232.SubsectorShrwtFllt_ind", "L232.Supplysector_ind") ->
      other_industry.xml

    return_data(other_industry.xml)
  } else {
    stop("Unknown command")
  }
}
