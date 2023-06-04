# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_solar_reeds_xml
#'
#' Construct XML data structure for \code{solar_reeds_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{solar_reeds_USA.xml}.
#' The corresponding file in the original data system was \code{batch_solar_USA_reeds.xml} (gcamusa XML batch).
module_gcamusa_solar_reeds_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2238.DeleteStubTechMinicamEnergyInput_PV_reeds_USA",
             "L2238.RenewRsrc_PV_reeds_USA",
             "L2238.GrdRenewRsrcCurves_PV_reeds_USA",
             "L2238.GrdRenewRsrcMax_PV_reeds_USA",
             "L2238.StubTechEffFlag_PV_reeds_USA",
             "L2238.StubTechCapFactor_PV_reeds_USA",
             "L2238.RenewRsrcTechChange_PV_reeds_USA",
             "L2238.StubTechCost_PV_reeds_USA",
             "L2238.ResTechShrwt_PV_reeds_USA",
             "L2239.DeleteUnlimitRsrc_reeds_USA",
             "L2239.DeleteStubTechMinicamEnergyInput_CSP_reeds_USA",
             "L2239.RenewRsrc_CSP_reeds_USA",
             "L2239.GrdRenewRsrcCurves_CSP_reeds_USA",
             "L2239.GrdRenewRsrcMax_CSP_reeds_USA",
             "L2239.StubTechEffFlag_CSP_reeds_USA",
             "L2239.StubTechCapFactor_CSP_reeds_USA",
             "L2239.RenewRsrcTechChange_CSP_reeds_USA",
             "L2239.StubTechCost_CSP_reeds_USA",
             "L2239.ResTechShrwt_CSP_reeds_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "solar_reeds_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    technology <- stub.technology <- NULL  # silence package check notes

    # Load required inputs
    L2238.DeleteStubTechMinicamEnergyInput_PV_reeds_USA <- get_data(all_data, "L2238.DeleteStubTechMinicamEnergyInput_PV_reeds_USA")
    L2238.RenewRsrc_PV_reeds_USA <- get_data(all_data, "L2238.RenewRsrc_PV_reeds_USA")
    L2238.GrdRenewRsrcCurves_PV_reeds_USA <- get_data(all_data, "L2238.GrdRenewRsrcCurves_PV_reeds_USA")
    L2238.GrdRenewRsrcMax_PV_reeds_USA <- get_data(all_data, "L2238.GrdRenewRsrcMax_PV_reeds_USA")
    L2238.StubTechEffFlag_PV_reeds_USA <- get_data(all_data, "L2238.StubTechEffFlag_PV_reeds_USA")
    L2238.StubTechCapFactor_PV_reeds_USA <- get_data(all_data, "L2238.StubTechCapFactor_PV_reeds_USA")
    L2238.RenewRsrcTechChange_PV_reeds_USA <- get_data(all_data, "L2238.RenewRsrcTechChange_PV_reeds_USA")
    L2238.StubTechCost_PV_reeds_USA <- get_data(all_data, "L2238.StubTechCost_PV_reeds_USA")
    L2238.ResTechShrwt_PV_reeds_USA <- get_data(all_data, "L2238.ResTechShrwt_PV_reeds_USA")
    L2239.DeleteUnlimitRsrc_reeds_USA <- get_data(all_data, "L2239.DeleteUnlimitRsrc_reeds_USA")
    L2239.DeleteStubTechMinicamEnergyInput_CSP_reeds_USA <- get_data(all_data, "L2239.DeleteStubTechMinicamEnergyInput_CSP_reeds_USA")
    L2239.RenewRsrc_CSP_reeds_USA <- get_data(all_data, "L2239.RenewRsrc_CSP_reeds_USA")
    L2239.GrdRenewRsrcCurves_CSP_reeds_USA <- get_data(all_data, "L2239.GrdRenewRsrcCurves_CSP_reeds_USA")
    L2239.GrdRenewRsrcMax_CSP_reeds_USA <- get_data(all_data, "L2239.GrdRenewRsrcMax_CSP_reeds_USA")
    L2239.StubTechEffFlag_CSP_reeds_USA <- get_data(all_data, "L2239.StubTechEffFlag_CSP_reeds_USA")
    L2239.StubTechCapFactor_CSP_reeds_USA <- get_data(all_data, "L2239.StubTechCapFactor_CSP_reeds_USA")
    L2239.RenewRsrcTechChange_CSP_reeds_USA <- get_data(all_data, "L2239.RenewRsrcTechChange_CSP_reeds_USA")
    L2239.StubTechCost_CSP_reeds_USA <- get_data(all_data, "L2239.StubTechCost_CSP_reeds_USA")
    L2239.ResTechShrwt_CSP_reeds_USA <- get_data(all_data, "L2239.ResTechShrwt_CSP_reeds_USA")

    # ===================================================
    # Produce outputs

    create_xml("solar_reeds_USA.xml") %>%
      add_xml_data_generate_levels(L2238.DeleteStubTechMinicamEnergyInput_PV_reeds_USA%>% rename(stub.technology = technology), "DeleteStubTechMinicamEnergyInput","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data(L2238.RenewRsrc_PV_reeds_USA, "RenewRsrc") %>%
      add_xml_data(L2238.GrdRenewRsrcCurves_PV_reeds_USA, "GrdRenewRsrcCurves") %>%
      add_xml_data(L2238.GrdRenewRsrcMax_PV_reeds_USA, "GrdRenewRsrcMax") %>%
      add_xml_data_generate_levels(L2238.StubTechEffFlag_PV_reeds_USA %>%
                                     rename(stub.technology = technology),
                                   "StubTechEffFlag","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2238.StubTechCapFactor_PV_reeds_USA %>%
                                     rename(stub.technology = technology),
                                   "StubTechCapFactor","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data(L2238.RenewRsrcTechChange_PV_reeds_USA, "RenewRsrcTechChange") %>%
      add_xml_data_generate_levels(L2238.StubTechCost_PV_reeds_USA %>%
                                     rename(stub.technology = technology),
                                   "StubTechCost","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data(L2239.DeleteUnlimitRsrc_reeds_USA, "DeleteUnlimitRsrc") %>%
      add_xml_data_generate_levels(L2239.DeleteStubTechMinicamEnergyInput_CSP_reeds_USA %>%
                                     rename(stub.technology = technology),
                                   "DeleteStubTechMinicamEnergyInput","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data(L2239.RenewRsrc_CSP_reeds_USA, "RenewRsrc") %>%
      add_xml_data(L2239.GrdRenewRsrcCurves_CSP_reeds_USA, "GrdRenewRsrcCurves") %>%
      add_xml_data(L2239.GrdRenewRsrcMax_CSP_reeds_USA, "GrdRenewRsrcMax") %>%
      add_xml_data_generate_levels(L2239.StubTechEffFlag_CSP_reeds_USA %>%
                                     rename(stub.technology = technology),
                                   "StubTechEffFlag","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L2239.StubTechCapFactor_CSP_reeds_USA %>%
                                     rename(stub.technology = technology),
                                   "StubTechCapFactor","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data(L2239.RenewRsrcTechChange_CSP_reeds_USA, "RenewRsrcTechChange") %>%
      add_xml_data_generate_levels(L2239.StubTechCost_CSP_reeds_USA %>%
                                     rename(stub.technology = technology),
                                   "StubTechCost","subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_xml_data(L2238.ResTechShrwt_PV_reeds_USA, "ResTechShrwt") %>%
      add_xml_data(L2239.ResTechShrwt_CSP_reeds_USA, "ResTechShrwt") %>%
      add_precursors("L2238.DeleteStubTechMinicamEnergyInput_PV_reeds_USA",
                     "L2238.RenewRsrc_PV_reeds_USA",
                     "L2238.GrdRenewRsrcCurves_PV_reeds_USA",
                     "L2238.GrdRenewRsrcMax_PV_reeds_USA",
                     "L2238.StubTechEffFlag_PV_reeds_USA",
                     "L2238.StubTechCapFactor_PV_reeds_USA",
                     "L2238.RenewRsrcTechChange_PV_reeds_USA",
                     "L2238.StubTechCost_PV_reeds_USA",
                     "L2238.ResTechShrwt_PV_reeds_USA",
                     "L2239.DeleteUnlimitRsrc_reeds_USA",
                     "L2239.DeleteStubTechMinicamEnergyInput_CSP_reeds_USA",
                     "L2239.RenewRsrc_CSP_reeds_USA",
                     "L2239.GrdRenewRsrcCurves_CSP_reeds_USA",
                     "L2239.GrdRenewRsrcMax_CSP_reeds_USA",
                     "L2239.StubTechEffFlag_CSP_reeds_USA",
                     "L2239.StubTechCapFactor_CSP_reeds_USA",
                     "L2239.RenewRsrcTechChange_CSP_reeds_USA",
                     "L2239.StubTechCost_CSP_reeds_USA",
                     "L2239.ResTechShrwt_CSP_reeds_USA") ->
      solar_reeds_USA.xml

    return_data(solar_reeds_USA.xml)
  } else {
    stop("Unknown command")
  }
}
