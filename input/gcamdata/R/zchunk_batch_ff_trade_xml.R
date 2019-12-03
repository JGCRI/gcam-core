#' module_energy_batch_ff_trade_xml
#'
#' Construct XML data structure for \code{ag_trade.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ff_trade.xml}.
module_energy_batch_ff_trade_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L239.PrimaryConsKeyword_en",
             "L239.Supplysector_tra",
             "L239.SectorUseTrialMarket_tra",
             "L239.SubsectorAll_tra",
             "L239.TechShrwt_tra",
             "L239.TechCost_tra",
             "L239.TechCoef_tra",
             "L239.Production_tra",
             "L239.Supplysector_reg",
             "L239.SubsectorAll_reg",
             "L239.TechShrwt_reg",
             "L239.TechCoef_reg",
             "L239.Production_reg_imp",
             "L239.Production_reg_dom",
             "L239.Consumption_intraregional"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ff_trade.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L239.PrimaryConsKeyword_en <- get_data(all_data, "L239.PrimaryConsKeyword_en")
    L239.Supplysector_tra <- get_data(all_data, "L239.Supplysector_tra")
    L239.SectorUseTrialMarket_tra <- get_data(all_data, "L239.SectorUseTrialMarket_tra")
    L239.SubsectorAll_tra <- get_data(all_data, "L239.SubsectorAll_tra")
    L239.TechShrwt_tra <- get_data(all_data, "L239.TechShrwt_tra")
    L239.TechCost_tra <- get_data(all_data, "L239.TechCost_tra")
    L239.TechCoef_tra <- get_data(all_data, "L239.TechCoef_tra")
    L239.Production_tra <- get_data(all_data, "L239.Production_tra")
    L239.Supplysector_reg <- get_data(all_data, "L239.Supplysector_reg")
    L239.SubsectorAll_reg <- get_data(all_data, "L239.SubsectorAll_reg")
    L239.TechShrwt_reg <- get_data(all_data, "L239.TechShrwt_reg")
    L239.TechCoef_reg <- get_data(all_data, "L239.TechCoef_reg")
    L239.Production_reg_imp <- get_data(all_data, "L239.Production_reg_imp")
    L239.Production_reg_dom <- get_data(all_data, "L239.Production_reg_dom")
    L239.Consumption_intraregional <- get_data(all_data, "L239.Consumption_intraregional")

    # ===================================================

    # Produce outputs
    create_xml("ff_trade.xml") %>%
      add_logit_tables_xml(L239.Supplysector_tra, "Supplysector") %>%
      add_xml_data(L239.SectorUseTrialMarket_tra, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L239.SubsectorAll_tra, "SubsectorAll", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L239.TechShrwt_tra, "TechShrwt") %>%
      add_xml_data(L239.TechCost_tra, "TechCost") %>%
      add_xml_data(L239.TechCoef_tra, "TechCoef") %>%
      add_xml_data(L239.Production_tra, "Production") %>%
      add_logit_tables_xml(L239.Supplysector_reg, "Supplysector") %>%
      add_logit_tables_xml(L239.SubsectorAll_reg, "SubsectorAll", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L239.TechShrwt_reg, "TechShrwt") %>%
      add_xml_data(L239.TechCoef_reg, "TechCoef") %>%
      add_xml_data(L239.Production_reg_imp, "Production") %>%
      add_xml_data(L239.Production_reg_dom, "Production") %>%
      add_xml_data(L239.Consumption_intraregional, "Production") %>%
      add_xml_data(L239.PrimaryConsKeyword_en, "PrimaryConsKeyword") %>%
      add_precursors("L239.PrimaryConsKeyword_en",
                     "L239.Supplysector_tra",
                     "L239.SectorUseTrialMarket_tra",
                     "L239.SubsectorAll_tra",
                     "L239.TechShrwt_tra",
                     "L239.TechCost_tra",
                     "L239.TechCoef_tra",
                     "L239.Production_tra",
                     "L239.Supplysector_reg",
                     "L239.SubsectorAll_reg",
                     "L239.TechShrwt_reg",
                     "L239.TechCoef_reg",
                     "L239.Production_reg_imp",
                     "L239.Production_reg_dom",
                     "L239.Consumption_intraregional") ->
      ff_trade.xml

    return_data(ff_trade.xml)
  } else {
    stop("Unknown command")
  }
}
