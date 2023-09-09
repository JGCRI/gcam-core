# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_gas_trade_xml
#'
#' Construct XML data structure for \code{gas_trade.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{gas_trade.xml}. The corresponding file in the
#' original data system was \code{batch_en_supply_xml.R} (energy XML).
module_energy_gas_trade_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2392.Delete_Supplysector_tra_NG",
             "L2392.Delete_Supplysector_reg_NG",
             "L2392.PrimaryConsKeyword_en_NG",
             "L2392.CarbonCoef_NG",
             "L2392.Supplysector_tra_NG",
             "L2392.SectorUseTrialMarket_tra_NG",
             "L2392.SubsectorAll_tra_NG",
             "L2392.TechShrwt_tra_NG",
             "L2392.TechCost_tra_NG",
             "L2392.TechCoef_tra_NG",
             "L2392.TechLifetime_tra_NG",
             "L2392.TechSCurve_tra_NG",
             "L2392.ProfitShutdown_tra_NG",
             "L2392.Supplysector_reg_NG",
             "L2392.NestingSubsectorAll_reg_NG",
             "L2392.SubsectorAll_reg_NG",
             "L2392.TechShrwt_reg_NG",
             "L2392.TechCost_reg_NG",
             "L2392.TechCoef_reg_NG",
             "L2392.TechLifetime_reg_NG",
             "L2392.TechSCurve_reg_NG",
             "L2392.ProfitShutdown_reg_NG",
             "L2392.TechInterp_reg_NG",
             "L2392.Production_tra_NG",
             "L2392.Production_reg_imp_NG",
             "L2392.Production_reg_dom_NG",
             "L281.TechAccountOutput_entrade",
             "L281.TechAccountInput_NG_entrade"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "gas_trade.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    L2392.Delete_Supplysector_reg_NG <- get_data(all_data, "L2392.Delete_Supplysector_reg_NG")
    L2392.Delete_Supplysector_tra_NG <- get_data(all_data, "L2392.Delete_Supplysector_tra_NG")
    L2392.PrimaryConsKeyword_en_NG <- get_data(all_data, "L2392.PrimaryConsKeyword_en_NG")
    L2392.Supplysector_tra_NG <- get_data(all_data, "L2392.Supplysector_tra_NG")
    L2392.SectorUseTrialMarket_tra_NG <- get_data(all_data, "L2392.SectorUseTrialMarket_tra_NG")
    L2392.SubsectorAll_tra_NG <- get_data(all_data, "L2392.SubsectorAll_tra_NG")
    L2392.TechShrwt_tra_NG <- get_data(all_data, "L2392.TechShrwt_tra_NG")
    L2392.TechCost_tra_NG <- get_data(all_data, "L2392.TechCost_tra_NG")
    L2392.TechCoef_tra_NG <- get_data(all_data, "L2392.TechCoef_tra_NG")
    L2392.TechLifetime_tra_NG <- get_data(all_data, "L2392.TechLifetime_tra_NG")
    L2392.TechSCurve_tra_NG <- get_data(all_data, "L2392.TechSCurve_tra_NG")
    L2392.ProfitShutdown_tra_NG <- get_data(all_data, "L2392.ProfitShutdown_tra_NG")
    L2392.Supplysector_reg_NG <- get_data(all_data, "L2392.Supplysector_reg_NG")
    L2392.NestingSubsectorAll_reg_NG <- get_data(all_data, "L2392.NestingSubsectorAll_reg_NG")

    L2392.SubsectorLogit_reg_NG <- get_data(all_data, "L2392.SubsectorAll_reg_NG") %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]], "subsector0") %>%
      mutate(logit.type = "relative-cost-logit")
    L2392.SubsectorShrwt_reg_NG <- get_data(all_data, "L2392.SubsectorAll_reg_NG") %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], "subsector0")
    L2392.SubsectorInterp_reg_NG <- get_data(all_data, "L2392.SubsectorAll_reg_NG") %>%
      select(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], "subsector0")

    L2392.TechShrwt_reg_NG <- get_data(all_data, "L2392.TechShrwt_reg_NG")
    L2392.TechCoef_reg_NG <- get_data(all_data, "L2392.TechCoef_reg_NG")
    L2392.TechCost_reg_NG <- get_data(all_data, "L2392.TechCost_reg_NG")
    L2392.TechLifetime_reg_NG <- get_data(all_data, "L2392.TechLifetime_reg_NG")
    L2392.TechSCurve_reg_NG <- get_data(all_data, "L2392.TechSCurve_reg_NG")
    L2392.ProfitShutdown_reg_NG <- get_data(all_data, "L2392.ProfitShutdown_reg_NG")
    L2392.TechInterp_reg_NG <- get_data(all_data, "L2392.TechInterp_reg_NG")
    L2392.Production_tra_NG <- get_data(all_data, "L2392.Production_tra_NG")
    L2392.Production_reg_imp_NG <- get_data(all_data, "L2392.Production_reg_imp_NG")
    L2392.Production_reg_dom_NG <- get_data(all_data, "L2392.Production_reg_dom_NG")
    L2392.CarbonCoef_NG <- get_data(all_data, "L2392.CarbonCoef_NG")
    L281.TechAccountInput_NG_entrade <- get_data(all_data, "L281.TechAccountInput_NG_entrade")

    # filter for only gas trade
    L281.TechAccountOutput_entrade <- get_data(all_data, "L281.TechAccountOutput_entrade") %>%
        filter(supplysector %in% unique(L2392.TechCoef_tra_NG$supplysector))

    # ===================================================

    # Produce outputs

    create_xml("gas_trade.xml") %>%
      add_xml_data(L2392.Delete_Supplysector_reg_NG,"DeleteSupplysector") %>%
      add_xml_data(L2392.Delete_Supplysector_tra_NG, "DeleteSupplysector") %>%
      add_xml_data_generate_levels(L2392.PrimaryConsKeyword_en_NG,
                                   "PrimaryConsKeywordff","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data(L2392.CarbonCoef_NG, "CarbonCoef") %>%
      add_logit_tables_xml(L2392.Supplysector_tra_NG, "Supplysector") %>%
      add_xml_data(L2392.SectorUseTrialMarket_tra_NG, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L2392.SubsectorAll_tra_NG, "SubsectorAllTo", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L2392.TechShrwt_tra_NG, "TechShrwt") %>%
      add_xml_data(L2392.TechCost_tra_NG, "TechCost") %>%
      add_xml_data(L2392.TechLifetime_tra_NG, "TechLifetime") %>%
      add_xml_data(L2392.TechSCurve_tra_NG, "TechSCurve") %>%
      add_xml_data(L2392.ProfitShutdown_tra_NG, "TechProfitShutdown") %>%
      add_xml_data(L2392.TechCoef_tra_NG, "TechCoef") %>%
      add_xml_data(L281.TechAccountOutput_entrade, "TechAccountOutput") %>%
      add_xml_data(L2392.Production_tra_NG, "Production") %>%
      add_logit_tables_xml(L2392.Supplysector_reg_NG, "Supplysector") %>%
      add_logit_tables_xml(L2392.NestingSubsectorAll_reg_NG, "SubsectorAllTo", base_logit_header = "SubsectorLogit") %>%
      add_logit_tables_xml_generate_levels(L2392.SubsectorLogit_reg_NG,
                                           "SubsectorLogit","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.SubsectorShrwt_reg_NG,
                                   "SubsectorShrwtFllt", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.SubsectorInterp_reg_NG,
                                   "SubsectorInterpTo", "subsector","nesting-subsector",1,FALSE) %>%

      add_xml_data_generate_levels(L2392.TechShrwt_reg_NG,
                                   "TechShrwt", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("input") %>%
      add_xml_data_generate_levels(L281.TechAccountInput_NG_entrade,
                                   "TechAccountInput", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.TechCoef_reg_NG,
                                   "TechCoef", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.TechCost_reg_NG,
                                   "TechCost", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.TechLifetime_reg_NG,
                                   "TechLifetime", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.TechSCurve_reg_NG,
                                   "TechSCurve", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.ProfitShutdown_reg_NG,
                                   "TechProfitShutdown", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.TechInterp_reg_NG,
                                   "TechInterpTo", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.Production_reg_imp_NG,
                                   "Production","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2392.Production_reg_dom_NG,
                                   "Production","subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%


      add_precursors("L2392.Delete_Supplysector_tra_NG",
                     "L2392.Delete_Supplysector_reg_NG",
                     "L2392.PrimaryConsKeyword_en_NG",
                     "L2392.CarbonCoef_NG",
                     "L2392.Supplysector_tra_NG",
                     "L2392.SectorUseTrialMarket_tra_NG",
                     "L2392.SubsectorAll_tra_NG",
                     "L2392.TechShrwt_tra_NG",
                     "L2392.TechCost_tra_NG",
                     "L2392.TechLifetime_tra_NG",
                     "L2392.TechSCurve_tra_NG",
                     "L2392.ProfitShutdown_tra_NG",
                     "L2392.TechCoef_tra_NG",
                     "L2392.Supplysector_reg_NG",
                     "L2392.NestingSubsectorAll_reg_NG",
                     "L2392.SubsectorAll_reg_NG",
                     "L2392.TechShrwt_reg_NG",
                     "L2392.TechCoef_reg_NG",
                     "L2392.TechCost_reg_NG",
                     "L2392.TechLifetime_reg_NG",
                     "L2392.TechSCurve_reg_NG",
                     "L2392.ProfitShutdown_reg_NG",
                     "L2392.TechInterp_reg_NG",
                     "L2392.Production_tra_NG",
                     "L2392.Production_reg_imp_NG",
                     "L2392.Production_reg_dom_NG",
                     "L281.TechAccountOutput_entrade",
                     "L281.TechAccountInput_NG_entrade") ->
      gas_trade.xml

    return_data(gas_trade.xml)
  } else {
    stop("Unknown command")
  }
}
