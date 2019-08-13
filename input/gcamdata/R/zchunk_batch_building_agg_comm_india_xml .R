#' module_gcamindia_batch_comm_xml
#'
#' Construct XML data structure for \code{comm_india.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{comm_india.xml}. The corresponding file in the
#' original data system was \code{batch_comm_india_xml.R} (gcamindia XML).
module_gcamindia_batch_comm_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L242.india_state_Supplysector_comm",
             "L242.india_state_FinalEnergyKeyword_comm",
             "L242.india_state_SubsectorLogit_comm",
             "L242.india_state_SubsectorShrwtFllt_comm",
             "L242.india_state_SubsectorInterp_comm",
             "L242.india_state_StubTech_comm",
             "L242.india_state_GlobalTechInterp_comm",
             "L242.india_state_PerCapitaBased_comm",
             "L242.india_state_PriceElasticity_comm",
             "L242.india_state_IncomeElasticity_comm",
             "L242.india_state_StubTechMarket_comm",
             "L242.india_state_FuelPrefElast_comm",
             "L242.india_state_StubTechCalInput_comm",
             "L242.india_state_GlobalTechShrwt_comm",
             "L242.india_state_GlobalTechEff_comm",
             "L242.india_state_GlobalTechCost_comm",
             "L242.india_state_BaseService_comm"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "comm_india.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L242.india_state_Supplysector_comm <- get_data(all_data, "L242.india_state_Supplysector_comm")
    L242.india_state_FinalEnergyKeyword_comm <- get_data(all_data, "L242.india_state_FinalEnergyKeyword_comm")
    L242.india_state_SubsectorLogit_comm <- get_data(all_data, "L242.india_state_SubsectorLogit_comm")
    L242.india_state_SubsectorShrwtFllt_comm <- get_data(all_data, "L242.india_state_SubsectorShrwtFllt_comm")
    L242.india_state_SubsectorInterp_comm <- get_data(all_data, "L242.india_state_SubsectorInterp_comm")
    L242.india_state_StubTech_comm <- get_data(all_data, "L242.india_state_StubTech_comm")
    L242.india_state_GlobalTechInterp_comm <- get_data(all_data, "L242.india_state_GlobalTechInterp_comm")
    L242.india_state_PerCapitaBased_comm <- get_data(all_data, "L242.india_state_PerCapitaBased_comm")
    L242.india_state_PriceElasticity_comm <- get_data(all_data, "L242.india_state_PriceElasticity_comm")
    L242.india_state_IncomeElasticity_comm <- get_data(all_data, "L242.india_state_IncomeElasticity_comm")
    L242.india_state_StubTechCalInput_comm <- get_data(all_data, "L242.india_state_StubTechCalInput_comm")
    L242.india_state_StubTechMarket_comm <- get_data(all_data, "L242.india_state_StubTechMarket_comm")
    L242.india_state_FuelPrefElast_comm <- get_data(all_data, "L242.india_state_FuelPrefElast_comm")
    L242.india_state_GlobalTechShrwt_comm <- get_data(all_data, "L242.india_state_GlobalTechShrwt_comm")
    L242.india_state_GlobalTechEff_comm <- get_data(all_data, "L242.india_state_GlobalTechEff_comm")
    L242.india_state_GlobalTechCost_comm <- get_data(all_data, "L242.india_state_GlobalTechCost_comm")
    L242.india_state_BaseService_comm <- get_data(all_data, "L242.india_state_BaseService_comm")

    # ===================================================

    # Produce outputs
    create_xml("comm_india.xml") %>%
      add_logit_tables_xml(L242.india_state_Supplysector_comm, "Supplysector") %>%
      add_xml_data(L242.india_state_FinalEnergyKeyword_comm, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L242.india_state_SubsectorLogit_comm, "SubsectorLogit") %>%
      add_xml_data(L242.india_state_SubsectorShrwtFllt_comm, "SubsectorShrwtFllt") %>%
      add_xml_data(L242.india_state_SubsectorInterp_comm, "SubsectorInterp") %>%
      add_xml_data(L242.india_state_StubTech_comm, "StubTech") %>%
      add_xml_data(L242.india_state_GlobalTechInterp_comm, "GlobalTechInterp") %>%
      add_xml_data(L242.india_state_PerCapitaBased_comm, "PerCapitaBased") %>%
      add_xml_data(L242.india_state_PriceElasticity_comm, "PriceElasticity") %>%
      add_xml_data(L242.india_state_IncomeElasticity_comm, "IncomeElasticity") %>%
      add_xml_data(L242.india_state_StubTechCalInput_comm, "StubTechCalInput") %>%
      add_xml_data(L242.india_state_StubTechMarket_comm, "StubTechMarket") %>%
      add_xml_data(L242.india_state_FuelPrefElast_comm, "FuelPrefElast") %>%
      add_xml_data(L242.india_state_GlobalTechShrwt_comm, "GlobalTechShrwt") %>%
      add_xml_data(L242.india_state_GlobalTechEff_comm, "GlobalTechEff") %>%
      add_xml_data(L242.india_state_GlobalTechCost_comm, "GlobalTechCost") %>%
      add_xml_data(L242.india_state_BaseService_comm, "BaseService") %>%
      add_precursors("L242.india_state_Supplysector_comm",
                     "L242.india_state_FinalEnergyKeyword_comm",
                     "L242.india_state_SubsectorLogit_comm",
                     "L242.india_state_SubsectorShrwtFllt_comm",
                     "L242.india_state_SubsectorInterp_comm",
                     "L242.india_state_StubTech_comm",
                     "L242.india_state_GlobalTechInterp_comm",
                     "L242.india_state_PerCapitaBased_comm",
                     "L242.india_state_PriceElasticity_comm",
                     "L242.india_state_IncomeElasticity_comm",
                     "L242.india_state_StubTechCalInput_comm",
                     "L242.india_state_StubTechMarket_comm",
                     "L242.india_state_FuelPrefElast_comm",
                     "L242.india_state_GlobalTechShrwt_comm",
                     "L242.india_state_GlobalTechEff_comm",
                     "L242.india_state_GlobalTechCost_comm",
                     "L242.india_state_BaseService_comm") ->
      comm_india.xml

    return_data(comm_india.xml)
  } else {
    stop("Unknown command")
  }
}
