# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_batch_desalination_xml
#'
#' Construct XML data structure for \code{desalination.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{desalination.xml}.
module_water_batch_desalination_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L271.Supplysector_desal",
             "L271.FinalEnergyKeyword_desal",
             "L271.SubsectorLogit_desal",
             "L271.SubsectorShrwtFllt_desal",
             "L271.SubsectorInterp_desal",
             "L271.SubsectorInterpTo_desal",
             "L271.StubTech_desal",
             "L271.GlobalTechCoef_desal",
             "L271.GlobalTechShrwt_desal",
             "L271.GlobalTechCost_desal",
             "L271.StubTechProd_desal"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "desalination.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L271.Supplysector_desal <- get_data(all_data, "L271.Supplysector_desal")
    L271.FinalEnergyKeyword_desal <- get_data(all_data, "L271.FinalEnergyKeyword_desal")
    L271.SubsectorLogit_desal <- get_data(all_data, "L271.SubsectorLogit_desal")
    L271.SubsectorShrwtFllt_desal <- get_data(all_data, "L271.SubsectorShrwtFllt_desal")
    L271.SubsectorInterp_desal <- get_data(all_data, "L271.SubsectorInterp_desal")
    L271.SubsectorInterpTo_desal <- get_data(all_data, "L271.SubsectorInterpTo_desal")
    L271.StubTech_desal <- get_data(all_data, "L271.StubTech_desal")
    L271.GlobalTechCoef_desal <- get_data(all_data, "L271.GlobalTechCoef_desal")
    L271.GlobalTechShrwt_desal <- get_data(all_data, "L271.GlobalTechShrwt_desal")
    L271.GlobalTechCost_desal <- get_data(all_data, "L271.GlobalTechCost_desal")
    L271.StubTechProd_desal <- get_data(all_data, "L271.StubTechProd_desal")

    # Produce outputs
    create_xml("desalination.xml") %>%
      add_logit_tables_xml(L271.Supplysector_desal, "Supplysector") %>%
      add_logit_tables_xml(L271.SubsectorLogit_desal, "SubsectorLogit") %>%
      add_xml_data(L271.FinalEnergyKeyword_desal, "FinalEnergyKeyword") %>%
      add_xml_data(L271.SubsectorShrwtFllt_desal, "SubsectorShrwtFllt") %>%
      add_xml_data(L271.SubsectorInterp_desal, "SubsectorInterp") %>%
      add_xml_data(L271.SubsectorInterpTo_desal, "SubsectorInterpTo") %>%
      add_xml_data(L271.StubTech_desal, "StubTech") %>%
      add_xml_data(L271.GlobalTechCoef_desal, "GlobalTechCoef") %>%
      add_xml_data(L271.GlobalTechShrwt_desal, "GlobalTechShrwt") %>%
      add_xml_data(L271.GlobalTechCost_desal, "GlobalTechCost") %>%
      add_xml_data(L271.StubTechProd_desal, "StubTechProd") %>%
      add_precursors("L271.Supplysector_desal",
                     "L271.SubsectorLogit_desal",
                     "L271.FinalEnergyKeyword_desal",
                     "L271.SubsectorShrwtFllt_desal",
                     "L271.SubsectorInterp_desal",
                     "L271.SubsectorInterpTo_desal",
                     "L271.StubTech_desal",
                     "L271.GlobalTechCoef_desal",
                     "L271.GlobalTechShrwt_desal",
                     "L271.GlobalTechCost_desal",
                     "L271.StubTechProd_desal") ->
      desalination.xml

    return_data(desalination.xml)
  } else {
    stop("Unknown command")
  }
}
