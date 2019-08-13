#' module_gcamindia_batch_trn_freight_xml
#'
#' Construct XML data structure for \code{comm_india.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{trn_freight_india.xml}. The corresponding file in the
#' original data system was \code{batch_trn_freight_india_xml.R} (gcamindia XML).
module_gcamindia_batch_trn_freight_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L252.india_state_Supplysector_trn_freight",
             "L252.india_state_FinalEnergyKeyword_trn_freight",
             "L252.india_state_SubsectorLogit_trn_freight",
             "L252.india_state_SubsectorShrwtFllt_trn_freight",
             "L252.india_state_SubsectorInterp_trn_freight",
             "L252.india_state_StubTech_trn_freight",
             "L252.india_state_PerCapitaBased_trn_freight",
             "L252.india_state_PriceElasticity_trn_freight",
             "L252.india_state_IncomeElasticity_trn_freight",
             "L252.india_state_StubTechMarket_trn_freight",
             "L252.india_state_FuelPrefElast_trn_freight",
             "L252.india_state_StubTechCalInput_trn_freight",
             "L252.india_state_GlobalTechShrwt_trn_freight",
             "L252.india_state_GlobalTechEff_trn_freight",
             "L252.india_state_GlobalTechCost_trn_freight",
             "L252.india_state_BaseService_trn_freight"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "trn_freight_india.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L252.india_state_Supplysector_trn_freight <- get_data(all_data, "L252.india_state_Supplysector_trn_freight")
    L252.india_state_FinalEnergyKeyword_trn_freight <- get_data(all_data, "L252.india_state_FinalEnergyKeyword_trn_freight")
    L252.india_state_SubsectorLogit_trn_freight <- get_data(all_data, "L252.india_state_SubsectorLogit_trn_freight")
    L252.india_state_SubsectorShrwtFllt_trn_freight <- get_data(all_data, "L252.india_state_SubsectorShrwtFllt_trn_freight")
    L252.india_state_SubsectorInterp_trn_freight <- get_data(all_data, "L252.india_state_SubsectorInterp_trn_freight")
    L252.india_state_StubTech_trn_freight <- get_data(all_data, "L252.india_state_StubTech_trn_freight")
    L252.india_state_PerCapitaBased_trn_freight <- get_data(all_data, "L252.india_state_PerCapitaBased_trn_freight")
    L252.india_state_PriceElasticity_trn_freight <- get_data(all_data, "L252.india_state_PriceElasticity_trn_freight")
    L252.india_state_IncomeElasticity_trn_freight <- get_data(all_data, "L252.india_state_IncomeElasticity_trn_freight")
    L252.india_state_StubTechCalInput_trn_freight <- get_data(all_data, "L252.india_state_StubTechCalInput_trn_freight")
    L252.india_state_StubTechMarket_trn_freight <- get_data(all_data, "L252.india_state_StubTechMarket_trn_freight")
    L252.india_state_FuelPrefElast_trn_freight <- get_data(all_data, "L252.india_state_FuelPrefElast_trn_freight")
    L252.india_state_GlobalTechShrwt_trn_freight <- get_data(all_data, "L252.india_state_GlobalTechShrwt_trn_freight")
    L252.india_state_GlobalTechEff_trn_freight <- get_data(all_data, "L252.india_state_GlobalTechEff_trn_freight")
    L252.india_state_GlobalTechCost_trn_freight <- get_data(all_data, "L252.india_state_GlobalTechCost_trn_freight")
    L252.india_state_BaseService_trn_freight <- get_data(all_data, "L252.india_state_BaseService_trn_freight")

    # ===================================================

    # Produce outputs
    create_xml("trn_freight_india.xml") %>%
      add_logit_tables_xml(L252.india_state_Supplysector_trn_freight, "Supplysector") %>%
      add_xml_data(L252.india_state_FinalEnergyKeyword_trn_freight, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L252.india_state_SubsectorLogit_trn_freight, "SubsectorLogit") %>%
      add_xml_data(L252.india_state_SubsectorShrwtFllt_trn_freight, "SubsectorShrwtFllt") %>%
      add_xml_data(L252.india_state_SubsectorInterp_trn_freight, "SubsectorInterp") %>%
      add_xml_data(L252.india_state_StubTech_trn_freight, "StubTech") %>%
      add_xml_data(L252.india_state_PerCapitaBased_trn_freight, "PerCapitaBased") %>%
      add_xml_data(L252.india_state_PriceElasticity_trn_freight, "PriceElasticity") %>%
      add_xml_data(L252.india_state_IncomeElasticity_trn_freight, "IncomeElasticity") %>%
      add_xml_data(L252.india_state_StubTechCalInput_trn_freight, "StubTechCalInput") %>%
      add_xml_data(L252.india_state_StubTechMarket_trn_freight, "StubTechMarket") %>%
      add_xml_data(L252.india_state_FuelPrefElast_trn_freight, "FuelPrefElast") %>%
      add_xml_data(L252.india_state_GlobalTechShrwt_trn_freight, "GlobalTechShrwt") %>%
      add_xml_data(L252.india_state_GlobalTechEff_trn_freight, "GlobalTechEff") %>%
      add_xml_data(L252.india_state_GlobalTechCost_trn_freight, "GlobalTechCost") %>%
      add_xml_data(L252.india_state_BaseService_trn_freight, "BaseService") %>%
      add_precursors("L252.india_state_Supplysector_trn_freight",
                     "L252.india_state_FinalEnergyKeyword_trn_freight",
                     "L252.india_state_SubsectorLogit_trn_freight",
                     "L252.india_state_SubsectorShrwtFllt_trn_freight",
                     "L252.india_state_SubsectorInterp_trn_freight",
                     "L252.india_state_StubTech_trn_freight",
                     "L252.india_state_PerCapitaBased_trn_freight",
                     "L252.india_state_PriceElasticity_trn_freight",
                     "L252.india_state_IncomeElasticity_trn_freight",
                     "L252.india_state_StubTechCalInput_trn_freight",
                     "L252.india_state_StubTechMarket_trn_freight",
                     "L252.india_state_FuelPrefElast_trn_freight",
                     "L252.india_state_GlobalTechShrwt_trn_freight",
                     "L252.india_state_GlobalTechEff_trn_freight",
                     "L252.india_state_GlobalTechCost_trn_freight",
                     "L252.india_state_BaseService_trn_freight") ->
      trn_freight_india.xml

    return_data(trn_freight_india.xml)
  } else {
    stop("Unknown command")
  }
}
