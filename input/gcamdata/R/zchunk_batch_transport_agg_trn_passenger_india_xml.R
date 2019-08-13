#' module_gcamindia_batch_trn_passenger_xml
#'
#' Construct XML data structure for \code{comm_india.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{trn_passenger_india.xml}. The corresponding file in the
#' original data system was \code{batch_trn_passenger_india_xml.R} (gcamindia XML).
module_gcamindia_batch_trn_passenger_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L252.india_state_DeleteSupplysector_trn",
             "L252.india_state_DeleteFinalDemand_trn",
             "L252.india_state_Supplysector_trn_passenger",
             "L252.india_state_FinalEnergyKeyword_trn_passenger",
             "L252.india_state_SubsectorLogit_trn_passenger",
             "L252.india_state_SubsectorShrwtFllt_trn_passenger",
             "L252.india_state_SubsectorInterp_trn_passenger",
             "L252.india_state_StubTech_trn_passenger",
             "L252.india_state_PerCapitaBased_trn_passenger",
             "L252.india_state_PriceElasticity_trn_passenger",
             "L252.india_state_IncomeElasticity_trn_passenger",
             "L252.india_state_StubTechMarket_trn_passenger",
             "L252.india_state_FuelPrefElast_trn_passenger",
             "L252.india_state_StubTechCalInput_trn_passenger",
             "L252.india_state_GlobalTechShrwt_trn_passenger",
             "L252.india_state_GlobalTechEff_trn_passenger",
             "L252.india_state_GlobalTechCost_trn_passenger",
             "L252.india_state_BaseService_trn_passenger"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "trn_passenger_india.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs

    L252.india_state_DeleteSupplysector_trn <- get_data(all_data, "L252.india_state_DeleteSupplysector_trn")
    L252.india_state_DeleteFinalDemand_trn <- get_data(all_data, "L252.india_state_DeleteFinalDemand_trn")
    L252.india_state_Supplysector_trn_passenger <- get_data(all_data, "L252.india_state_Supplysector_trn_passenger")
    L252.india_state_FinalEnergyKeyword_trn_passenger <- get_data(all_data, "L252.india_state_FinalEnergyKeyword_trn_passenger")
    L252.india_state_SubsectorLogit_trn_passenger <- get_data(all_data, "L252.india_state_SubsectorLogit_trn_passenger")
    L252.india_state_SubsectorShrwtFllt_trn_passenger <- get_data(all_data, "L252.india_state_SubsectorShrwtFllt_trn_passenger")
    L252.india_state_SubsectorInterp_trn_passenger <- get_data(all_data, "L252.india_state_SubsectorInterp_trn_passenger")
    L252.india_state_StubTech_trn_passenger <- get_data(all_data, "L252.india_state_StubTech_trn_passenger")
    L252.india_state_PerCapitaBased_trn_passenger <- get_data(all_data, "L252.india_state_PerCapitaBased_trn_passenger")
    L252.india_state_PriceElasticity_trn_passenger <- get_data(all_data, "L252.india_state_PriceElasticity_trn_passenger")
    L252.india_state_IncomeElasticity_trn_passenger <- get_data(all_data, "L252.india_state_IncomeElasticity_trn_passenger")
    L252.india_state_StubTechCalInput_trn_passenger <- get_data(all_data, "L252.india_state_StubTechCalInput_trn_passenger")
    L252.india_state_StubTechMarket_trn_passenger <- get_data(all_data, "L252.india_state_StubTechMarket_trn_passenger")
    L252.india_state_FuelPrefElast_trn_passenger <- get_data(all_data, "L252.india_state_FuelPrefElast_trn_passenger")
    L252.india_state_GlobalTechShrwt_trn_passenger <- get_data(all_data, "L252.india_state_GlobalTechShrwt_trn_passenger")
    L252.india_state_GlobalTechEff_trn_passenger <- get_data(all_data, "L252.india_state_GlobalTechEff_trn_passenger")
    L252.india_state_GlobalTechCost_trn_passenger <- get_data(all_data, "L252.india_state_GlobalTechCost_trn_passenger")
    L252.india_state_BaseService_trn_passenger <- get_data(all_data, "L252.india_state_BaseService_trn_passenger")

    # ===================================================

    # Produce outputs
    create_xml("trn_passenger_india.xml") %>%
      add_xml_data(L252.india_state_DeleteSupplysector_trn, "DeleteSupplysector") %>%
      add_xml_data(L252.india_state_DeleteFinalDemand_trn, "DeleteFinalDemand") %>%
      add_logit_tables_xml(L252.india_state_Supplysector_trn_passenger, "Supplysector") %>%
      add_xml_data(L252.india_state_FinalEnergyKeyword_trn_passenger, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L252.india_state_SubsectorLogit_trn_passenger, "SubsectorLogit") %>%
      add_xml_data(L252.india_state_SubsectorShrwtFllt_trn_passenger, "SubsectorShrwtFllt") %>%
      add_xml_data(L252.india_state_SubsectorInterp_trn_passenger, "SubsectorInterp") %>%
      add_xml_data(L252.india_state_StubTech_trn_passenger, "StubTech") %>%
      add_xml_data(L252.india_state_PerCapitaBased_trn_passenger, "PerCapitaBased") %>%
      add_xml_data(L252.india_state_PriceElasticity_trn_passenger, "PriceElasticity") %>%
      add_xml_data(L252.india_state_IncomeElasticity_trn_passenger, "IncomeElasticity") %>%
      add_xml_data(L252.india_state_StubTechCalInput_trn_passenger, "StubTechCalInput") %>%
      add_xml_data(L252.india_state_StubTechMarket_trn_passenger, "StubTechMarket") %>%
      add_xml_data(L252.india_state_FuelPrefElast_trn_passenger, "FuelPrefElast") %>%
      add_xml_data(L252.india_state_GlobalTechShrwt_trn_passenger, "GlobalTechShrwt") %>%
      add_xml_data(L252.india_state_GlobalTechEff_trn_passenger, "GlobalTechEff") %>%
      add_xml_data(L252.india_state_GlobalTechCost_trn_passenger, "GlobalTechCost") %>%
      add_xml_data(L252.india_state_BaseService_trn_passenger, "BaseService") %>%
      add_precursors("L252.india_state_DeleteSupplysector_trn",
                     "L252.india_state_DeleteFinalDemand_trn",
                     "L252.india_state_Supplysector_trn_passenger",
                     "L252.india_state_FinalEnergyKeyword_trn_passenger",
                     "L252.india_state_SubsectorLogit_trn_passenger",
                     "L252.india_state_SubsectorShrwtFllt_trn_passenger",
                     "L252.india_state_SubsectorInterp_trn_passenger",
                     "L252.india_state_StubTech_trn_passenger",
                     "L252.india_state_PerCapitaBased_trn_passenger",
                     "L252.india_state_PriceElasticity_trn_passenger",
                     "L252.india_state_IncomeElasticity_trn_passenger",
                     "L252.india_state_StubTechCalInput_trn_passenger",
                     "L252.india_state_StubTechMarket_trn_passenger",
                     "L252.india_state_FuelPrefElast_trn_passenger",
                     "L252.india_state_GlobalTechShrwt_trn_passenger",
                     "L252.india_state_GlobalTechEff_trn_passenger",
                     "L252.india_state_GlobalTechCost_trn_passenger",
                     "L252.india_state_BaseService_trn_passenger") ->
      trn_passenger_india.xml

    return_data(trn_passenger_india.xml)
  } else {
    stop("Unknown command")
  }
}
