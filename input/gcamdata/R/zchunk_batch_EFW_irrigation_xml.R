# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_batch_EFW_irrigation_xml
#'
#' Construct XML data structure for \code{EFW_irrigation.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{EFW_irrigation.xml}.
module_water_batch_EFW_irrigation_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L272.Supplysector_irr",
             "L272.FinalEnergyKeyword_irr",
             "L272.SubsectorLogit_irr",
             "L272.SubsectorShrwtFllt_irr",
             "L272.StubTech_irr",
             "L272.GlobalTechCoef_irr",
             "L272.GlobalTechShrwt_irr",
             "L272.StubTechCoef_irr"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "EFW_irrigation.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L272.Supplysector_irr <- get_data(all_data, "L272.Supplysector_irr")
    L272.FinalEnergyKeyword_irr <- get_data(all_data, "L272.FinalEnergyKeyword_irr")
    L272.SubsectorLogit_irr <- get_data(all_data, "L272.SubsectorLogit_irr")
    L272.SubsectorShrwtFllt_irr <- get_data(all_data, "L272.SubsectorShrwtFllt_irr")
    L272.StubTech_irr <- get_data(all_data, "L272.StubTech_irr")
    L272.GlobalTechCoef_irr <- get_data(all_data, "L272.GlobalTechCoef_irr")
    L272.GlobalTechShrwt_irr <- get_data(all_data, "L272.GlobalTechShrwt_irr")
    L272.StubTechCoef_irr <- get_data(all_data, "L272.StubTechCoef_irr")

    # Produce outputs
    create_xml("EFW_irrigation.xml") %>%
      add_logit_tables_xml(L272.Supplysector_irr, "Supplysector") %>%
      add_logit_tables_xml(L272.SubsectorLogit_irr, "SubsectorLogit") %>%
      add_xml_data(L272.FinalEnergyKeyword_irr, "FinalEnergyKeyword") %>%
      add_xml_data(L272.SubsectorShrwtFllt_irr, "SubsectorShrwtFllt") %>%
      add_xml_data(L272.StubTech_irr, "StubTech") %>%
      add_xml_data(L272.GlobalTechCoef_irr, "GlobalTechCoef") %>%
      add_xml_data(L272.GlobalTechShrwt_irr, "GlobalTechShrwt") %>%
      add_xml_data(L272.StubTechCoef_irr, "StubTechCoef") %>%
      add_precursors("L272.Supplysector_irr",
                     "L272.SubsectorLogit_irr",
                     "L272.FinalEnergyKeyword_irr",
                     "L272.SubsectorShrwtFllt_irr",
                     "L272.StubTech_irr",
                     "L272.GlobalTechCoef_irr",
                     "L272.GlobalTechShrwt_irr",
                     "L272.StubTechCoef_irr") ->
      EFW_irrigation.xml

    return_data(EFW_irrigation.xml)
  } else {
    stop("Unknown command")
  }
}
