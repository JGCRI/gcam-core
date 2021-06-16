# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_batch_EFW_municipal_xml
#'
#' Construct XML data structure for \code{EFW_municipal.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{EFW_municipal.xml}.
module_water_batch_EFW_municipal_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L274.Supplysector_muni",
             "L274.FinalEnergyKeyword_muni",
             "L274.SubsectorLogit_muni",
             "L274.SubsectorShrwtFllt_muni",
             "L274.StubTech_muni",
             "L274.GlobalTechCoef_muni",
             "L274.GlobalTechShrwt_muni",
             "L274.StubTechCoef_muni"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "EFW_municipal.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L274.Supplysector_muni <- get_data(all_data, "L274.Supplysector_muni")
    L274.FinalEnergyKeyword_muni <- get_data(all_data, "L274.FinalEnergyKeyword_muni")
    L274.SubsectorLogit_muni <- get_data(all_data, "L274.SubsectorLogit_muni")
    L274.SubsectorShrwtFllt_muni <- get_data(all_data, "L274.SubsectorShrwtFllt_muni")
    L274.StubTech_muni <- get_data(all_data, "L274.StubTech_muni")
    L274.GlobalTechCoef_muni <- get_data(all_data, "L274.GlobalTechCoef_muni")
    L274.GlobalTechShrwt_muni <- get_data(all_data, "L274.GlobalTechShrwt_muni")
    L274.StubTechCoef_muni <- get_data(all_data, "L274.StubTechCoef_muni")

    # Produce outputs
    create_xml("EFW_municipal.xml") %>%
      add_logit_tables_xml(L274.Supplysector_muni, "Supplysector") %>%
      add_logit_tables_xml(L274.SubsectorLogit_muni, "SubsectorLogit") %>%
      add_xml_data(L274.FinalEnergyKeyword_muni, "FinalEnergyKeyword") %>%
      add_xml_data(L274.SubsectorShrwtFllt_muni, "SubsectorShrwtFllt") %>%
      add_xml_data(L274.StubTech_muni, "StubTech") %>%
      add_xml_data(L274.GlobalTechCoef_muni, "GlobalTechCoef") %>%
      add_xml_data(L274.GlobalTechShrwt_muni, "GlobalTechShrwt") %>%
      add_xml_data(L274.StubTechCoef_muni, "StubTechCoef") %>%
      add_precursors("L274.Supplysector_muni",
                     "L274.SubsectorLogit_muni",
                     "L274.FinalEnergyKeyword_muni",
                     "L274.SubsectorShrwtFllt_muni",
                     "L274.StubTech_muni",
                     "L274.GlobalTechCoef_muni",
                     "L274.GlobalTechShrwt_muni",
                     "L274.StubTechCoef_muni") ->
      EFW_municipal.xml

    return_data(EFW_municipal.xml)
  } else {
    stop("Unknown command")
  }
}
