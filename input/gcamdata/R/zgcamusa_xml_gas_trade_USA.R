# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_gas_trade_xml
#'
#' Construct XML data structure for \code{gas_trade_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{gas_trade_USA.xml}. The corresponding file in the
#' original data system was \code{batch_en_supply_xml.R} (energy XML).
module_gcamusa_gas_trade_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2392.TechCoef_tra_NG_USA",
             "L2392.TechCoef_reg_NG_USA",
             "L2392.TechCoef_tra_NG_USA_delete",
             "L2392.TechCoef_reg_NG_USA_delete"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "gas_trade_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    L2392.TechCoef_tra_NG_USA <- get_data(all_data, "L2392.TechCoef_tra_NG_USA")
    L2392.TechCoef_tra_NG_USA_delete <- get_data(all_data, "L2392.TechCoef_tra_NG_USA_delete")
    L2392.TechCoef_reg_NG_USA <- get_data(all_data, "L2392.TechCoef_reg_NG_USA")
    L2392.TechCoef_reg_NG_USA_delete <- get_data(all_data, "L2392.TechCoef_reg_NG_USA_delete")

    # ===================================================
    # Produce outputs
    create_xml("gas_trade_USA.xml") %>%
      add_xml_data(L2392.TechCoef_tra_NG_USA_delete,"DeleteInput") %>%
      add_xml_data_generate_levels(L2392.TechCoef_reg_NG_USA_delete,
                                   "DeleteInput", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data(L2392.TechCoef_tra_NG_USA, "TechCoef") %>%
      add_xml_data_generate_levels(L2392.TechCoef_reg_NG_USA,
                                   "TechCoef", "subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("subsector") %>%
      add_precursors("L2392.TechCoef_tra_NG_USA_delete",
                     "L2392.TechCoef_reg_NG_USA_delete",
                     "L2392.TechCoef_tra_NG_USA",
                     "L2392.TechCoef_reg_NG_USA") ->
      gas_trade_USA.xml

    return_data(gas_trade_USA.xml)
  } else {
    stop("Unknown command")
  }
}
