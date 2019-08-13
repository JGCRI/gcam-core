#' module_gcamindia_batch_industry_xml
#'
#' Construct XML data structure for \code{industry_india.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{industry_india.xml}. The corresponding file in the
#' original data system was \code{batch_industry_india_xml.R} (gcamindia XML).
module_gcamindia_batch_industry_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L232.india_state_DeleteSupplysector_ind",
             "L232.india_state_DeleteFinalDemand_ind",
             "L2321.india_state_DeleteSupplysector_cement",
             "L2321.india_state_DeleteFinalDemand_cement",
             "L232.india_state_Supplysector_ind",
             "L232.india_state_FinalEnergyKeyword_ind",
             "L232.india_state_SubsectorLogit_ind",
             "L232.india_state_SubsectorShrwtFllt_ind",
             "L232.india_state_SubsectorInterp_ind",
             "L232.india_state_StubTech_ind",
             "L232.india_state_StubTechInterp_ind",
             "L232.india_state_PerCapitaBased_ind",
             "L232.india_state_PriceElasticity_ind",
             "L232.india_state_IncomeElasticity_ind_gcam3",
             "L232.india_state_StubTechCalInput_indenergy",
             "L232.india_state_StubTechCalInput_indfeed",
             "L232.india_state_StubTechProd_industry",
             "L232.india_state_StubTechCoef_industry",
             "L232.india_state_StubTechMarket_ind",
             "L232.india_state_BaseService_ind",
             "L232.india_state_StubTechSecMarket_ind"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "industry_india.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L232.india_state_DeleteSupplysector_ind <- get_data(all_data, "L232.india_state_DeleteSupplysector_ind")
    L232.india_state_DeleteFinalDemand_ind <- get_data(all_data, "L232.india_state_DeleteFinalDemand_ind")
    L2321.india_state_DeleteSupplysector_cement <- get_data(all_data, "L2321.india_state_DeleteSupplysector_cement")
    L2321.india_state_DeleteFinalDemand_cement <- get_data(all_data, "L2321.india_state_DeleteFinalDemand_cement")
    L232.india_state_Supplysector_ind <- get_data(all_data, "L232.india_state_Supplysector_ind")
    L232.india_state_FinalEnergyKeyword_ind <- get_data(all_data, "L232.india_state_FinalEnergyKeyword_ind")
    L232.india_state_SubsectorLogit_ind <- get_data(all_data, "L232.india_state_SubsectorLogit_ind")
    L232.india_state_SubsectorShrwtFllt_ind <- get_data(all_data, "L232.india_state_SubsectorShrwtFllt_ind")
    L232.india_state_SubsectorInterp_ind <- get_data(all_data, "L232.india_state_SubsectorInterp_ind")
    L232.india_state_StubTech_ind <- get_data(all_data, "L232.india_state_StubTech_ind")
    L232.india_state_StubTechInterp_ind <- get_data(all_data, "L232.india_state_StubTechInterp_ind")
    L232.india_state_PerCapitaBased_ind <- get_data(all_data, "L232.india_state_PerCapitaBased_ind")
    L232.india_state_PriceElasticity_ind <- get_data(all_data, "L232.india_state_PriceElasticity_ind")
    L232.india_state_IncomeElasticity_ind_gcam3 <- get_data(all_data, "L232.india_state_IncomeElasticity_ind_gcam3")
    L232.india_state_StubTechCalInput_indenergy <- get_data(all_data, "L232.india_state_StubTechCalInput_indenergy")
    L232.india_state_StubTechCalInput_indfeed <- get_data(all_data, "L232.india_state_StubTechCalInput_indfeed")
    L232.india_state_StubTechProd_industry <- get_data(all_data, "L232.india_state_StubTechProd_industry")
    L232.india_state_StubTechCoef_industry <- get_data(all_data, "L232.india_state_StubTechCoef_industry")
    L232.india_state_StubTechMarket_ind <- get_data(all_data, "L232.india_state_StubTechMarket_ind")
    L232.india_state_StubTechSecMarket_ind <- get_data(all_data, "L232.india_state_StubTechSecMarket_ind")
    L232.india_state_BaseService_ind <- get_data(all_data, "L232.india_state_BaseService_ind")

    # ===================================================

    # Produce outputs
    create_xml("industry_india.xml") %>%
      add_xml_data(L232.india_state_DeleteSupplysector_ind, "DeleteSupplysector") %>%
      add_xml_data(L232.india_state_DeleteFinalDemand_ind, "DeleteFinalDemand") %>%
      add_xml_data(L2321.india_state_DeleteSupplysector_cement, "DeleteSupplysector") %>%
      add_xml_data(L2321.india_state_DeleteFinalDemand_cement, "DeleteFinalDemand") %>%
      add_logit_tables_xml(L232.india_state_Supplysector_ind, "Supplysector") %>%
      add_xml_data(L232.india_state_FinalEnergyKeyword_ind, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L232.india_state_SubsectorLogit_ind, "SubsectorLogit") %>%
      add_xml_data(L232.india_state_SubsectorShrwtFllt_ind, "SubsectorShrwtFllt") %>%
      add_xml_data(L232.india_state_SubsectorInterp_ind, "SubsectorInterp") %>%
      add_xml_data(L232.india_state_StubTech_ind, "StubTech") %>%
      add_xml_data(L232.india_state_StubTechInterp_ind, "StubTechInterp") %>%
      add_xml_data(L232.india_state_PerCapitaBased_ind, "PerCapitaBased") %>%
      add_xml_data(L232.india_state_PriceElasticity_ind, "PriceElasticity") %>%
      add_xml_data(L232.india_state_IncomeElasticity_ind_gcam3, "IncomeElasticity") %>%
      add_xml_data(L232.india_state_StubTechCalInput_indenergy, "StubTechCalInput") %>%
      add_xml_data(L232.india_state_StubTechCalInput_indfeed, "StubTechCalInput") %>%
      add_xml_data(L232.india_state_StubTechProd_industry, "StubTechProd") %>%
      add_xml_data(L232.india_state_StubTechCoef_industry, "StubTechCoef") %>%
      add_xml_data(L232.india_state_StubTechMarket_ind, "StubTechMarket") %>%
      add_xml_data(L232.india_state_StubTechSecMarket_ind, "StubTechSecMarket") %>%
      add_xml_data(L232.india_state_BaseService_ind, "BaseService") %>%
      add_precursors("L232.india_state_DeleteSupplysector_ind",
                     "L232.india_state_DeleteFinalDemand_ind",
                     "L2321.india_state_DeleteSupplysector_cement",
                     "L2321.india_state_DeleteFinalDemand_cement",
                     "L232.india_state_Supplysector_ind",
                     "L232.india_state_FinalEnergyKeyword_ind",
                     "L232.india_state_SubsectorLogit_ind",
                     "L232.india_state_SubsectorShrwtFllt_ind",
                     "L232.india_state_SubsectorInterp_ind",
                     "L232.india_state_StubTech_ind",
                     "L232.india_state_StubTechInterp_ind",
                     "L232.india_state_PerCapitaBased_ind",
                     "L232.india_state_PriceElasticity_ind",
                     "L232.india_state_IncomeElasticity_ind_gcam3",
                     "L232.india_state_StubTechCalInput_indenergy",
                     "L232.india_state_StubTechCalInput_indfeed",
                     "L232.india_state_StubTechProd_industry",
                     "L232.india_state_StubTechCoef_industry",
                     "L232.india_state_StubTechMarket_ind",
                     "L232.india_state_BaseService_ind",
                     "L232.india_state_StubTechSecMarket_ind") ->
      industry_india.xml

    return_data(industry_india.xml)
  } else {
    stop("Unknown command")
  }
}
