# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_iron_steel_trade_xml
#'
#' Construct XML data structure for \code{iron_steel_trade.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{iron_steel_trade.xml}.
module_energy_iron_steel_trade_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L238.Supplysector_tra",
             "L238.SectorUseTrialMarket_tra",
             "L238.SubsectorAll_tra",
             "L238.TechShrwt_tra",
             "L238.TechCost_tra",
             "L238.TechCoef_tra",
             "L238.Production_tra",
             "L238.Supplysector_reg",
             "L238.SubsectorAll_reg",
             "L238.TechShrwt_reg",
             "L238.TechCoef_reg",
             "L238.Production_reg_imp",
             "L238.Production_reg_dom"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "iron_steel_trade.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L238.Supplysector_tra <- get_data(all_data, "L238.Supplysector_tra")
    L238.SectorUseTrialMarket_tra <- get_data(all_data, "L238.SectorUseTrialMarket_tra")
    L238.SubsectorAll_tra <- get_data(all_data, "L238.SubsectorAll_tra")
    L238.TechShrwt_tra <- get_data(all_data, "L238.TechShrwt_tra")
    L238.TechCost_tra <- get_data(all_data, "L238.TechCost_tra")
    L238.TechCoef_tra <- get_data(all_data, "L238.TechCoef_tra")
    L238.Production_tra <- get_data(all_data, "L238.Production_tra")
    L238.Supplysector_reg <- get_data(all_data, "L238.Supplysector_reg")
    L238.SubsectorAll_reg <- get_data(all_data, "L238.SubsectorAll_reg")
    L238.TechShrwt_reg <- get_data(all_data, "L238.TechShrwt_reg")
    L238.TechCoef_reg <- get_data(all_data, "L238.TechCoef_reg")
    L238.Production_reg_imp <- get_data(all_data, "L238.Production_reg_imp")
    L238.Production_reg_dom <- get_data(all_data, "L238.Production_reg_dom")

    # ===================================================

    # Produce outputs
    create_xml("iron_steel_trade.xml") %>%
      add_logit_tables_xml(L238.Supplysector_tra, "Supplysector") %>%
      add_xml_data(L238.SectorUseTrialMarket_tra, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L238.SubsectorAll_tra, "SubsectorAllTo", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L238.TechShrwt_tra, "TechShrwt") %>%
      add_xml_data(L238.TechCost_tra, "TechCost") %>%
      add_xml_data(L238.TechCoef_tra, "TechCoef") %>%
      add_xml_data(L238.Production_tra, "Production") %>%
      add_logit_tables_xml(L238.Supplysector_reg, "Supplysector") %>%
      add_logit_tables_xml(L238.SubsectorAll_reg, "SubsectorAllTo", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L238.TechShrwt_reg, "TechShrwt") %>%
      add_xml_data(L238.TechCoef_reg, "TechCoef") %>%
      add_xml_data(L238.Production_reg_imp, "Production") %>%
      add_xml_data(L238.Production_reg_dom, "Production") %>%
      add_precursors("L238.Supplysector_tra",
                     "L238.SectorUseTrialMarket_tra",
                     "L238.SubsectorAll_tra",
                     "L238.TechShrwt_tra",
                     "L238.TechCost_tra",
                     "L238.TechCoef_tra",
                     "L238.Production_tra",
                     "L238.Supplysector_reg",
                     "L238.SubsectorAll_reg",
                     "L238.TechShrwt_reg",
                     "L238.TechCoef_reg",
                     "L238.Production_reg_imp",
                     "L238.Production_reg_dom") ->
      iron_steel_trade.xml

    return_data(iron_steel_trade.xml)
  } else {
    stop("Unknown command")
  }
}
