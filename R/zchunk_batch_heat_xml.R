# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_heat_xml
#'
#' Construct XML data structure for \code{heat.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{heat.xml}. The corresponding file in the
#' original data system was \code{batch_heat.xml.R} (energy XML).
module_energy_batch_heat_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L224.Supplysector_heat",
             "L224.SubsectorLogit_heat",
             "L224.SubsectorShrwtFllt_heat",
             "L224.SubsectorInterp_heat",
             "L224.StubTech_heat",
             "L224.GlobalTechCoef_heat",
             "L224.GlobalTechCost_heat",
             "L224.GlobalTechShrwt_heat",
             "L224.StubTechCalInput_heat",
             "L224.StubTechSecOut_elec",
             "L224.StubTechCost_elec"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "heat.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L224.Supplysector_heat <- get_data(all_data, "L224.Supplysector_heat")
    L224.SubsectorLogit_heat <- get_data(all_data, "L224.SubsectorLogit_heat")
    L224.SubsectorShrwtFllt_heat <- get_data(all_data, "L224.SubsectorShrwtFllt_heat")
    L224.SubsectorInterp_heat <- get_data(all_data, "L224.SubsectorInterp_heat")
    L224.StubTech_heat <- get_data(all_data, "L224.StubTech_heat")
    L224.GlobalTechCoef_heat <- get_data(all_data, "L224.GlobalTechCoef_heat")
    L224.GlobalTechCost_heat <- get_data(all_data, "L224.GlobalTechCost_heat")
    L224.GlobalTechShrwt_heat <- get_data(all_data, "L224.GlobalTechShrwt_heat")
    L224.StubTechCalInput_heat <- get_data(all_data, "L224.StubTechCalInput_heat")
    L224.StubTechSecOut_elec <- get_data(all_data, "L224.StubTechSecOut_elec")
    L224.StubTechCost_elec <- get_data(all_data, "L224.StubTechCost_elec")

    share.weight <- year.share.weight <- NULL # Silence package checks 

    # ===================================================
    # Rename the tibble columns to match the header info.
    L224.StubTechCalInput_heat <- rename(L224.StubTechCalInput_heat, tech.share.weight = share.weight, share.weight.year = year.share.weight)


    # Produce outputs
    create_xml("heat.xml") %>%
      add_logit_tables_xml(L224.Supplysector_heat, "Supplysector") %>%
      add_logit_tables_xml(L224.SubsectorLogit_heat, "SubsectorLogit") %>%
      add_xml_data(L224.SubsectorShrwtFllt_heat, "SubsectorShrwtFllt") %>%
      add_xml_data(L224.SubsectorInterp_heat, "SubsectorInterp") %>%
      add_xml_data(L224.StubTech_heat, "StubTech") %>%
      add_xml_data(L224.GlobalTechCoef_heat, "GlobalTechCoef") %>%
      add_xml_data(L224.GlobalTechCost_heat, "GlobalTechCost") %>%
      add_xml_data(L224.GlobalTechShrwt_heat, "GlobalTechShrwt") %>%
      add_xml_data(L224.StubTechCalInput_heat, "StubTechCalInput") %>%
      add_xml_data(L224.StubTechSecOut_elec, "StubTechSecOut") %>%
      add_xml_data(L224.StubTechCost_elec, "StubTechCost") %>%
      add_precursors("L224.Supplysector_heat",
                     "L224.SubsectorLogit_heat",
                     "L224.SubsectorShrwtFllt_heat",
                     "L224.SubsectorInterp_heat",
                     "L224.StubTech_heat",
                     "L224.GlobalTechCoef_heat",
                     "L224.GlobalTechCost_heat",
                     "L224.GlobalTechShrwt_heat",
                     "L224.StubTechCalInput_heat",
                     "L224.StubTechSecOut_elec",
                     "L224.StubTechCost_elec") ->
      heat.xml

    return_data(heat.xml)
  } else {
    stop("Unknown command")
  }
}
