# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_en_supply_xml
#'
#' Construct XML data structure for \code{en_supply_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_supply_USA.xml}. The corresponding file in the
#' original data system was \code{batch_en_supply_xml.R} (energy XML).
module_gcamusa_en_supply_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L239.TechCoef_tra_USA",
             "L239.TechCoef_reg_USA",
             "L239.DeleteInput_reg_tra_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_supply_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L239.TechCoef_tra_USA <- get_data(all_data, "L239.TechCoef_tra_USA")
    L239.TechCoef_reg_USA <- get_data(all_data, "L239.TechCoef_reg_USA")
    L239.DeleteInput_reg_tra_USA <- get_data(all_data, "L239.DeleteInput_reg_tra_USA")
    # ===================================================
    # Produce outputs
    create_xml("en_supply_USA.xml") %>%
      add_xml_data(L239.DeleteInput_reg_tra_USA, "DeleteInput") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L239.TechCoef_tra_USA, "TechCoef") %>%
      add_xml_data(L239.TechCoef_reg_USA, "TechCoef") %>%
      add_precursors("L239.TechCoef_tra_USA",
                     "L239.TechCoef_reg_USA",
                     "L239.DeleteInput_reg_tra_USA") ->
      en_supply_USA.xml


    return_data(en_supply_USA.xml)
  } else {
    stop("Unknown command")
  }
}
