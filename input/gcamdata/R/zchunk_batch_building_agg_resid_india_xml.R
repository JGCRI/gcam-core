#' module_gcamindia_batch_resid_xml
#'
#' Construct XML data structure for \code{resid_india.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{resid_india.xml}. The corresponding file in the
#' original data system was \code{batch_resid_india_xml.R} (gcamindia XML).
module_gcamindia_batch_resid_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L242.india_state_DeleteSupplysector_bld",
             "L242.india_state_DeleteFinalDemand_bld",
             "L242.india_state_Supplysector_resid",
             "L242.india_state_FinalEnergyKeyword_resid",
             "L242.india_state_SubsectorLogit_resid",
             "L242.india_state_SubsectorShrwtFllt_resid",
             "L242.india_state_SubsectorInterp_resid",
             "L242.india_state_StubTech_resid",
             "L242.india_state_GlobalTechInterp_resid",
             "L242.india_state_PerCapitaBased_resid",
             "L242.india_state_PriceElasticity_resid",
             "L242.india_state_IncomeElasticity_resid",
             "L242.india_state_StubTechMarket_resid",
             "L242.india_state_FuelPrefElast_resid",
             "L242.india_state_StubTechCalInput_resid",
             "L242.india_state_GlobalTechShrwt_resid",
             "L242.india_state_GlobalTechEff_resid",
             "L242.india_state_GlobalTechCost_resid",
             "L242.india_state_BaseService_resid"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "resid_india.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L242.india_state_DeleteSupplysector_bld <- get_data(all_data, "L242.india_state_DeleteSupplysector_bld")
    L242.india_state_DeleteFinalDemand_bld <- get_data(all_data, "L242.india_state_DeleteFinalDemand_bld")
    L242.india_state_Supplysector_resid <- get_data(all_data, "L242.india_state_Supplysector_resid")
    L242.india_state_FinalEnergyKeyword_resid <- get_data(all_data, "L242.india_state_FinalEnergyKeyword_resid")
    L242.india_state_SubsectorLogit_resid <- get_data(all_data, "L242.india_state_SubsectorLogit_resid")
    L242.india_state_SubsectorShrwtFllt_resid <- get_data(all_data, "L242.india_state_SubsectorShrwtFllt_resid")
    L242.india_state_SubsectorInterp_resid <- get_data(all_data, "L242.india_state_SubsectorInterp_resid")
    L242.india_state_StubTech_resid <- get_data(all_data, "L242.india_state_StubTech_resid")
    L242.india_state_GlobalTechInterp_resid <- get_data(all_data, "L242.india_state_GlobalTechInterp_resid")
    L242.india_state_PerCapitaBased_resid <- get_data(all_data, "L242.india_state_PerCapitaBased_resid")
    L242.india_state_PriceElasticity_resid <- get_data(all_data, "L242.india_state_PriceElasticity_resid")
    L242.india_state_IncomeElasticity_resid <- get_data(all_data, "L242.india_state_IncomeElasticity_resid")
    L242.india_state_StubTechCalInput_resid <- get_data(all_data, "L242.india_state_StubTechCalInput_resid")
    L242.india_state_StubTechMarket_resid <- get_data(all_data, "L242.india_state_StubTechMarket_resid")
    L242.india_state_FuelPrefElast_resid <- get_data(all_data, "L242.india_state_FuelPrefElast_resid")
    L242.india_state_GlobalTechShrwt_resid <- get_data(all_data, "L242.india_state_GlobalTechShrwt_resid")
    L242.india_state_GlobalTechEff_resid <- get_data(all_data, "L242.india_state_GlobalTechEff_resid")
    L242.india_state_GlobalTechCost_resid <- get_data(all_data, "L242.india_state_GlobalTechCost_resid")
    L242.india_state_BaseService_resid <- get_data(all_data, "L242.india_state_BaseService_resid")

    # ===================================================

    # Produce outputs
    create_xml("resid_india.xml") %>%
      add_xml_data(L242.india_state_DeleteSupplysector_bld, "DeleteSupplysector") %>%
      add_xml_data(L242.india_state_DeleteFinalDemand_bld, "DeleteFinalDemand") %>%
      add_logit_tables_xml(L242.india_state_Supplysector_resid, "Supplysector") %>%
      add_xml_data(L242.india_state_FinalEnergyKeyword_resid, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L242.india_state_SubsectorLogit_resid, "SubsectorLogit") %>%
      add_xml_data(L242.india_state_SubsectorShrwtFllt_resid, "SubsectorShrwtFllt") %>%
      add_xml_data(L242.india_state_SubsectorInterp_resid, "SubsectorInterp") %>%
      add_xml_data(L242.india_state_StubTech_resid, "StubTech") %>%
      add_xml_data(L242.india_state_GlobalTechInterp_resid, "GlobalTechInterp") %>%
      add_xml_data(L242.india_state_PerCapitaBased_resid, "PerCapitaBased") %>%
      add_xml_data(L242.india_state_PriceElasticity_resid, "PriceElasticity") %>%
      add_xml_data(L242.india_state_IncomeElasticity_resid, "IncomeElasticity") %>%
      add_xml_data(L242.india_state_StubTechCalInput_resid, "StubTechCalInput") %>%
      add_xml_data(L242.india_state_StubTechMarket_resid, "StubTechMarket") %>%
      add_xml_data(L242.india_state_FuelPrefElast_resid, "FuelPrefElast") %>%
      add_xml_data(L242.india_state_GlobalTechShrwt_resid, "GlobalTechShrwt") %>%
      add_xml_data(L242.india_state_GlobalTechEff_resid, "GlobalTechEff") %>%
      add_xml_data(L242.india_state_GlobalTechCost_resid, "GlobalTechCost") %>%
      add_xml_data(L242.india_state_BaseService_resid, "BaseService") %>%
      add_precursors("L242.india_state_DeleteSupplysector_bld",
                     "L242.india_state_DeleteFinalDemand_bld",
                     "L242.india_state_Supplysector_resid",
                     "L242.india_state_FinalEnergyKeyword_resid",
                     "L242.india_state_SubsectorLogit_resid",
                     "L242.india_state_SubsectorShrwtFllt_resid",
                     "L242.india_state_SubsectorInterp_resid",
                     "L242.india_state_StubTech_resid",
                     "L242.india_state_GlobalTechInterp_resid",
                     "L242.india_state_PerCapitaBased_resid",
                     "L242.india_state_PriceElasticity_resid",
                     "L242.india_state_IncomeElasticity_resid",
                     "L242.india_state_StubTechCalInput_resid",
                     "L242.india_state_StubTechMarket_resid",
                     "L242.india_state_FuelPrefElast_resid",
                     "L242.india_state_GlobalTechShrwt_resid",
                     "L242.india_state_GlobalTechEff_resid",
                     "L242.india_state_GlobalTechCost_resid",
                     "L242.india_state_BaseService_resid") ->
      resid_india.xml

    return_data(resid_india.xml)
  } else {
    stop("Unknown command")
  }
}
