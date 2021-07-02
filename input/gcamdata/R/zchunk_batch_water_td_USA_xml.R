# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_water_td_USA_xml
#'
#' Construct XML data structure for \code{water_mapping.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_td_USA.xml}.
module_gcamusa_batch_water_td_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L203.Supplysector_USA",
             "L203.SubsectorLogit_USA",
             "L203.SubsectorShrwt_USA",
             "L203.TechShrwt_USA",
             "L203.TechCoef_USA",
             "L203.TechPmult_USA",
             "L203.DeleteSupplysector_USA",
             "L203.DeleteResTechInput",
             "L203.DeleteSubsector_USA",
             "L203.TechDesalCoef_USA",
             "L203.TechDesalShrwt_USA",
             "L203.TechDesalCost_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_td_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L203.Supplysector_USA <- get_data(all_data, "L203.Supplysector_USA")
    L203.SubsectorLogit_USA <- get_data(all_data, "L203.SubsectorLogit_USA")
    L203.SubsectorShrwt_USA <- get_data(all_data, "L203.SubsectorShrwt_USA")
    L203.TechShrwt_USA <- get_data(all_data, "L203.TechShrwt_USA")
    L203.TechCoef_USA <- get_data(all_data, "L203.TechCoef_USA")
    L203.TechPmult_USA <- get_data(all_data,"L203.TechPmult_USA")
    L203.DeleteSupplysector_USA <- get_data(all_data, "L203.DeleteSupplysector_USA")
    L203.DeleteResTechInput <- get_data(all_data, "L203.DeleteResTechInput")
    L203.DeleteSubsector_USA <- get_data(all_data, "L203.DeleteSubsector_USA")
    L203.TechDesalCoef_USA  <- get_data(all_data, "L203.TechDesalCoef_USA")
    L203.TechDesalShrwt_USA <- get_data(all_data, "L203.TechDesalShrwt_USA")
    L203.TechDesalCost_USA <- get_data(all_data, "L203.TechDesalCost_USA")

    # ===================================================

    # Produce outputs
    create_xml("water_td_USA.xml") %>%
      add_logit_tables_xml(L203.Supplysector_USA, "Supplysector") %>%
      add_logit_tables_xml(L203.SubsectorLogit_USA, "SubsectorLogit") %>%
      add_xml_data(L203.DeleteSupplysector_USA, "DeleteSupplysector") %>%
      add_xml_data(L203.DeleteResTechInput, "DeleteResTechInput") %>%
      add_xml_data(L203.SubsectorShrwt_USA, "SubsectorShrwt") %>%
      add_xml_data(L203.DeleteSubsector_USA, "DeleteSubsector") %>%
      add_xml_data(L203.TechShrwt_USA, "TechShrwt") %>%
      add_xml_data(L203.TechCoef_USA, "TechCoef") %>%
      add_xml_data(L203.TechPmult_USA, "TechPmult") %>%
      add_xml_data(L203.TechDesalCoef_USA, "TechCoef") %>%
      add_xml_data(L203.TechDesalShrwt_USA, "TechShrwt") %>%
      add_xml_data(L203.TechDesalCost_USA, "TechCost") %>%
      add_precursors("L203.Supplysector_USA", "L203.SubsectorLogit_USA", "L203.DeleteSupplysector_USA", "L203.DeleteResTechInput", "L203.DeleteSubsector_USA", "L203.SubsectorShrwt_USA",
                     "L203.TechShrwt_USA", "L203.TechCoef_USA","L203.TechPmult_USA","L203.TechDesalCoef_USA", "L203.TechDesalShrwt_USA", "L203.TechDesalCost_USA") ->
      water_td_USA.xml

    return_data(water_td_USA.xml)
  } else {
    stop("Unknown command")
  }
}
