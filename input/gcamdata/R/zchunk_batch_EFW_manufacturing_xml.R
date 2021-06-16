# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_batch_EFW_manufacturing_xml
#'
#' Construct XML data structure for \code{EFW_manufacturing.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{EFW_manufacturing.xml}.
module_water_batch_EFW_manufacturing_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L273.Supplysector_ind",
             "L273.FinalEnergyKeyword_ind",
             "L273.SubsectorLogit_ind",
             "L273.SubsectorShrwtFllt_ind",
             "L273.StubTech_ind",
             "L273.GlobalTechCoef_ind",
             "L273.GlobalTechShrwt_ind",
             "L273.StubTechCoef_ind"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "EFW_manufacturing.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L273.Supplysector_ind <- get_data(all_data, "L273.Supplysector_ind")
    L273.FinalEnergyKeyword_ind <- get_data(all_data, "L273.FinalEnergyKeyword_ind")
    L273.SubsectorLogit_ind <- get_data(all_data, "L273.SubsectorLogit_ind")
    L273.SubsectorShrwtFllt_ind <- get_data(all_data, "L273.SubsectorShrwtFllt_ind")
    L273.StubTech_ind <- get_data(all_data, "L273.StubTech_ind")
    L273.GlobalTechCoef_ind <- get_data(all_data, "L273.GlobalTechCoef_ind")
    L273.GlobalTechShrwt_ind <- get_data(all_data, "L273.GlobalTechShrwt_ind")
    L273.StubTechCoef_ind <- get_data(all_data, "L273.StubTechCoef_ind")

    # Produce outputs
    create_xml("EFW_manufacturing.xml") %>%
      add_logit_tables_xml(L273.Supplysector_ind, "Supplysector") %>%
      add_logit_tables_xml(L273.SubsectorLogit_ind, "SubsectorLogit") %>%
      add_xml_data(L273.FinalEnergyKeyword_ind, "FinalEnergyKeyword") %>%
      add_xml_data(L273.SubsectorShrwtFllt_ind, "SubsectorShrwtFllt") %>%
      add_xml_data(L273.StubTech_ind, "StubTech") %>%
      add_xml_data(L273.GlobalTechCoef_ind, "GlobalTechCoef") %>%
      add_xml_data(L273.GlobalTechShrwt_ind, "GlobalTechShrwt") %>%
      add_xml_data(L273.StubTechCoef_ind, "StubTechCoef") %>%
      add_precursors("L273.Supplysector_ind",
                     "L273.SubsectorLogit_ind",
                     "L273.FinalEnergyKeyword_ind",
                     "L273.SubsectorShrwtFllt_ind",
                     "L273.StubTech_ind",
                     "L273.GlobalTechCoef_ind",
                     "L273.GlobalTechShrwt_ind",
                     "L273.StubTechCoef_ind") ->
      EFW_manufacturing.xml

    return_data(EFW_manufacturing.xml)
  } else {
    stop("Unknown command")
  }
}
