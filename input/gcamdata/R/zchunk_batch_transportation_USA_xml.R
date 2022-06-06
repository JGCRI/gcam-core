# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_transportation_USA_xml
#'
#' Construct XML data structure for \code{transportation_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_USA.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_USA_xml.R} (gcamusa XML).
module_gcamusa_batch_transportation_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.DeleteSupplysector_USAtrn",
             "L254.DeleteFinalDemand_USAtrn",
             "L254.Supplysector_trn_USA",
             "L254.FinalEnergyKeyword_trn_USA",
             "L254.tranSubsectorLogit_USA",
             "L254.tranSubsectorShrwtFllt_USA",
             "L254.tranSubsectorInterp_USA",
             "L254.tranSubsectorSpeed_USA",
             "L254.tranSubsectorSpeed_passthru_USA",
             "L254.tranSubsectorSpeed_noVOTT_USA",
             "L254.tranSubsectorSpeed_nonmotor_USA",
             "L254.tranSubsectorVOTT_USA",
             "L254.tranSubsectorFuelPref_USA",
             "L254.StubTranTech_USA",
             "L254.StubTranTech_passthru_USA",
             "L254.StubTranTech_nonmotor_USA",
             "L254.StubTranTechLoadFactor_USA",
             "L254.StubTranTechCost_USA",
             "L254.StubTranTechCoef_USA",
             "L254.PerCapitaBased_trn_USA",
             "L254.PriceElasticity_trn_USA",
             "L254.IncomeElasticity_trn_USA",
             "L254.StubTranTechCalInput_USA",
             "L254.StubTranTechProd_nonmotor_USA",
             "L254.StubTranTechCalInput_passthru_USA",
             "L254.BaseService_trn_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    #xml_files<- c("transportation_USA_CORE.xml","transportation_USA_highEV.xml")
    xml_files<- c("transportation_USA_CORE.xml")
    names(xml_files) <- rep("XML", length(xml_files))
    return(xml_files)
  } else if(command == driver.MAKE) {

    # Silence package check notes
    sce <- year <- . <- NULL

    all_data <- list(...)[[1]]

    ret_data <- c()
    curr_env <- environment()
    for (i in c("CORE")){

    xml_name <- paste0("transportation_USA_", i, ".xml")

    # Load required inputs
    L254.DeleteSupplysector_USAtrn <- get_data(all_data, "L254.DeleteSupplysector_USAtrn") %>% mutate(sce=paste0("CORE")) %>% filter(sce==i)

    L254.DeleteFinalDemand_USAtrn <- get_data(all_data, "L254.DeleteFinalDemand_USAtrn") %>% filter(sce==i)
    L254.Supplysector_trn_USA <- get_data(all_data, "L254.Supplysector_trn_USA") %>% filter(sce==i)

    L254.FinalEnergyKeyword_trn_USA <- get_data(all_data, "L254.FinalEnergyKeyword_trn_USA")%>% filter(sce==i)
    L254.tranSubsectorLogit_USA <- get_data(all_data, "L254.tranSubsectorLogit_USA") %>% filter(sce==i)
    L254.tranSubsectorShrwtFllt_USA <- get_data(all_data, "L254.tranSubsectorShrwtFllt_USA")%>% filter(sce==i)
    L254.tranSubsectorInterp_USA <- get_data(all_data, "L254.tranSubsectorInterp_USA")%>% filter(sce==i)
    L254.tranSubsectorSpeed_USA <- get_data(all_data, "L254.tranSubsectorSpeed_USA") %>% filter(sce==i)
    L254.tranSubsectorSpeed_passthru_USA <- get_data(all_data, "L254.tranSubsectorSpeed_passthru_USA")%>% filter(sce==i)
    L254.tranSubsectorSpeed_noVOTT_USA <- get_data(all_data, "L254.tranSubsectorSpeed_noVOTT_USA")%>% filter(sce==i)
    L254.tranSubsectorSpeed_nonmotor_USA <- get_data(all_data, "L254.tranSubsectorSpeed_nonmotor_USA")%>% filter(sce==i)
    L254.tranSubsectorVOTT_USA <- get_data(all_data, "L254.tranSubsectorVOTT_USA") %>% filter(sce==i)
    L254.StubTranTech_USA <- get_data(all_data, "L254.StubTranTech_USA") %>% filter(sce==i)
    L254.StubTranTech_passthru_USA <- get_data(all_data, "L254.StubTranTech_passthru_USA") %>% filter(sce==i)
    L254.StubTranTech_nonmotor_USA <- get_data(all_data, "L254.StubTranTech_nonmotor_USA") %>% filter(sce==i)
    L254.StubTranTechLoadFactor_USA <- get_data(all_data, "L254.StubTranTechLoadFactor_USA")%>% filter(sce==i)
    L254.StubTranTechCost_USA <- get_data(all_data, "L254.StubTranTechCost_USA")%>% filter(sce==i)
    L254.StubTranTechCoef_USA <- get_data(all_data, "L254.StubTranTechCoef_USA")%>% filter(sce==i)
    L254.PerCapitaBased_trn_USA <- get_data(all_data, "L254.PerCapitaBased_trn_USA") %>% filter(sce==i)
    L254.PriceElasticity_trn_USA <- get_data(all_data, "L254.PriceElasticity_trn_USA") %>% filter(sce==i)
    L254.IncomeElasticity_trn_USA <- get_data(all_data, "L254.IncomeElasticity_trn_USA")%>% filter(sce==i)
    L254.StubTranTechCalInput_USA <- get_data(all_data, "L254.StubTranTechCalInput_USA") %>% filter(sce==i)
    L254.StubTranTechProd_nonmotor_USA <- get_data(all_data, "L254.StubTranTechProd_nonmotor_USA")
    L254.StubTranTechCalInput_passthru_USA <- get_data(all_data, "L254.StubTranTechCalInput_passthru_USA")%>% mutate(sce=paste0("CORE")) %>% filter(sce==i)
    L254.BaseService_trn_USA <- get_data(all_data, "L254.BaseService_trn_USA") %>% mutate(sce=paste0("CORE")) %>% filter(sce==i)

    if (i != "CORE"){
      L254.tranSubsectorSpeed_USA <- L254.tranSubsectorSpeed_USA %>% filter(year>MODEL_FINAL_BASE_YEAR)
      L254.StubTranTechLoadFactor_USA <- L254.StubTranTechLoadFactor_USA %>% filter(year>MODEL_FINAL_BASE_YEAR)
      L254.StubTranTechCost_USA <- L254.StubTranTechCost_USA %>% filter(year > MODEL_FINAL_BASE_YEAR)
      L254.StubTranTechCoef_USA <- L254.StubTranTechCoef_USA %>%  filter(year > MODEL_FINAL_BASE_YEAR)
      L254.StubTranTechCalInput_USA <- L254.StubTranTechCalInput_USA %>%  filter(year > MODEL_FINAL_BASE_YEAR)

    }
    # ===================================================

    # Produce outputs
    create_xml(xml_name) %>%
      add_xml_data(L254.DeleteSupplysector_USAtrn, "DeleteSupplysector") %>%
      add_xml_data(L254.DeleteFinalDemand_USAtrn, "DeleteFinalDemand") %>%
      add_logit_tables_xml(L254.Supplysector_trn_USA, "Supplysector") %>%
      add_xml_data(L254.FinalEnergyKeyword_trn_USA, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L254.tranSubsectorLogit_USA, "tranSubsectorLogit", "tranSubsector") %>%
      add_xml_data(L254.tranSubsectorShrwtFllt_USA, "tranSubsectorShrwtFllt") %>%
      add_xml_data(L254.tranSubsectorInterp_USA, "tranSubsectorInterp") %>%
      add_xml_data(L254.tranSubsectorSpeed_USA, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_passthru_USA, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_noVOTT_USA, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_nonmotor_USA, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorVOTT_USA, "tranSubsectorVOTT") %>%
      #add_xml_data(L254.tranSubsectorFuelPref_USA, "tranSubsectorFuelPref") %>%
      add_xml_data(L254.StubTranTech_USA, "StubTranTech") %>%
      add_xml_data(L254.StubTranTech_passthru_USA, "StubTranTech") %>%
      add_xml_data(L254.StubTranTech_nonmotor_USA, "StubTranTech") %>%
      add_xml_data(L254.StubTranTechLoadFactor_USA, "StubTranTechLoadFactor") %>%
      add_xml_data(L254.StubTranTechCost_USA, "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_USA, "StubTranTechCoef") %>%
      add_xml_data(L254.PerCapitaBased_trn_USA, "PerCapitaBased") %>%
      add_xml_data(L254.PriceElasticity_trn_USA, "PriceElasticity") %>%
      add_xml_data(L254.IncomeElasticity_trn_USA, "IncomeElasticity") %>%
      add_xml_data(L254.StubTranTechCalInput_USA, "StubTranTechCalInput") %>%
      add_xml_data(L254.StubTranTechProd_nonmotor_USA, "StubTranTechProd") %>%
      add_xml_data(L254.StubTranTechCalInput_passthru_USA, "StubTranTechCalInput") %>%
      add_xml_data(L254.BaseService_trn_USA, "BaseService") %>%
      add_precursors("L254.DeleteSupplysector_USAtrn",
                     "L254.DeleteFinalDemand_USAtrn",
                     "L254.Supplysector_trn_USA",
                     "L254.FinalEnergyKeyword_trn_USA",
                     "L254.tranSubsectorLogit_USA",
                     "L254.tranSubsectorShrwtFllt_USA",
                     "L254.tranSubsectorInterp_USA",
                     "L254.tranSubsectorSpeed_USA",
                     "L254.tranSubsectorSpeed_passthru_USA",
                     "L254.tranSubsectorSpeed_noVOTT_USA",
                     "L254.tranSubsectorSpeed_nonmotor_USA",
                     "L254.tranSubsectorVOTT_USA",
                     "L254.tranSubsectorFuelPref_USA",
                     "L254.StubTranTech_USA",
                     "L254.StubTranTech_passthru_USA",
                     "L254.StubTranTech_nonmotor_USA",
                     "L254.StubTranTechLoadFactor_USA",
                     "L254.StubTranTechCost_USA",
                     "L254.StubTranTechCoef_USA",
                     "L254.PerCapitaBased_trn_USA",
                     "L254.PriceElasticity_trn_USA",
                     "L254.IncomeElasticity_trn_USA",
                     "L254.StubTranTechCalInput_USA",
                     "L254.StubTranTechProd_nonmotor_USA",
                     "L254.StubTranTechCalInput_passthru_USA",
                     "L254.BaseService_trn_USA") %>%
                      assign(xml_name, ., envir = curr_env)
    ret_data <- c(ret_data, xml_name)

    }
    ret_data %>%
      paste(collapse = ", ") %>%
      paste0("return_data(", ., ")") %>%
      parse(text = .) %>%
      eval()
  } else {
    stop("Unknown command")
  }
}
