# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_hydrogen_USA_xml
#'
#' Construct XML data structure for \code{hydrogen_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{hydrogen_USA.xml}. The corresponding file in the
#' original data system was \code{batch_hydrogen_USA_xml.R} (gcamusa XML).
module_gcamusa_batch_hydrogen_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L225.DeleteSupplysector_h2_USA",
             "L225.Supplysector_h2_USA",
             "L225.SectorUseTrialMarket_h2_USA",
             "L225.SubsectorLogit_h2_USA",
             "L225.SubsectorShrwtFllt_h2_USA",
             "L225.StubTech_h2_USA",
             "L225.StubTechMarket_h2_USA",
             "L225.DeleteStubTechMinicamEnergyInput_H2_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "hydrogen_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L225.DeleteStubTechMinicamEnergyInput_H2_USA <- get_data(all_data, "L225.DeleteStubTechMinicamEnergyInput_H2_USA")
    L225.DeleteSupplysector_h2_USA <- get_data(all_data, "L225.DeleteSupplysector_h2_USA")
    L225.Supplysector_h2_USA <- get_data(all_data, "L225.Supplysector_h2_USA")
    L225.SectorUseTrialMarket_h2_USA <- get_data(all_data, "L225.SectorUseTrialMarket_h2_USA")
    L225.SubsectorLogit_h2_USA <- get_data(all_data, "L225.SubsectorLogit_h2_USA")
    L225.SubsectorShrwtFllt_h2_USA <- get_data(all_data, "L225.SubsectorShrwtFllt_h2_USA")
    L225.StubTech_h2_USA <- get_data(all_data, "L225.StubTech_h2_USA")
    L225.StubTechMarket_h2_USA <- get_data(all_data, "L225.StubTechMarket_h2_USA")

    # ===================================================

    # Produce outputs
    create_xml("hydrogen_USA.xml") %>%
      add_xml_data(L225.DeleteSupplysector_h2_USA, "DeleteSupplysector") %>%
      add_logit_tables_xml(L225.Supplysector_h2_USA, "Supplysector") %>%
      add_xml_data(L225.SectorUseTrialMarket_h2_USA, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L225.SubsectorLogit_h2_USA, "SubsectorLogit") %>%
      add_xml_data(L225.SubsectorShrwtFllt_h2_USA, "SubsectorShrwtFllt") %>%
      add_xml_data(L225.StubTech_h2_USA, "StubTech") %>%
      add_xml_data(L225.StubTechMarket_h2_USA, "StubTechMarket") %>%
      add_xml_data(L225.DeleteStubTechMinicamEnergyInput_H2_USA,"DeleteStubTechMinicamEnergyInput") %>%
      add_precursors("L225.DeleteSupplysector_h2_USA",
                     "L225.Supplysector_h2_USA",
                     "L225.SectorUseTrialMarket_h2_USA",
                     "L225.SubsectorLogit_h2_USA",
                     "L225.SubsectorShrwtFllt_h2_USA",
                     "L225.StubTech_h2_USA",
                     "L225.StubTechMarket_h2_USA",
                     "L225.DeleteStubTechMinicamEnergyInput_H2_USA") ->
      hydrogen_USA.xml

    return_data(hydrogen_USA.xml)
  } else {
    stop("Unknown command")
  }
}
