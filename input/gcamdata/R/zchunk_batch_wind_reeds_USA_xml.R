#' module_gcamusa_batch_wind_reeds_USA_xml
#'
#' Construct XML data structure for \code{wind_reeds_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{wind_reeds_USA.xml}.
#' The corresponding file in the original data system was \code{batch_wind_USA_reeds.xml} (gcamusa XML batch).
module_gcamusa_batch_wind_reeds_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2237.SmthRenewRsrcCurves_wind_reeds_USA",
             "L2237.StubTechCapFactor_wind_reeds_USA",
             "L2237.SmthRenewRsrcTechChange_wind_reeds_USA",
             "L2237.StubTechCost_wind_reeds_USA",
             "L2237.ResTechShrwt_wind_reeds_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "wind_reeds_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2237.SmthRenewRsrcCurves_wind_reeds_USA <- get_data(all_data, "L2237.SmthRenewRsrcCurves_wind_reeds_USA")
    L2237.StubTechCapFactor_wind_reeds_USA <- get_data(all_data, "L2237.StubTechCapFactor_wind_reeds_USA")
    L2237.SmthRenewRsrcTechChange_wind_reeds_USA <- get_data(all_data, "L2237.SmthRenewRsrcTechChange_wind_reeds_USA")
    L2237.StubTechCost_wind_reeds_USA <- get_data(all_data, "L2237.StubTechCost_wind_reeds_USA")
    L2237.ResTechShrwt_wind_reeds_USA <- get_data(all_data, "L2237.ResTechShrwt_wind_reeds_USA")

    # ===================================================
    # Produce outputs

    create_xml("wind_reeds_USA.xml") %>%
      add_xml_data(L2237.SmthRenewRsrcCurves_wind_reeds_USA, "SmthRenewRsrcCurves") %>%
      add_xml_data_generate_levels(L2237.StubTechCapFactor_wind_reeds_USA%>% rename(stub.technology = technology), "StubTechCapFactor","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data(L2237.SmthRenewRsrcTechChange_wind_reeds_USA, "SmthRenewRsrcTechChange") %>%
      add_xml_data_generate_levels(L2237.StubTechCost_wind_reeds_USA%>% rename(stub.technology = technology), "StubTechCost","subsector","nesting-subsector",1,FALSE) %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_xml_data(L2237.ResTechShrwt_wind_reeds_USA, "ResTechShrwt") %>%
      add_precursors("L2237.SmthRenewRsrcCurves_wind_reeds_USA",
                     "L2237.StubTechCapFactor_wind_reeds_USA",
                     "L2237.SmthRenewRsrcTechChange_wind_reeds_USA",
                     "L2237.StubTechCost_wind_reeds_USA",
                     "L2237.ResTechShrwt_wind_reeds_USA") ->
      wind_reeds_USA.xml

    return_data(wind_reeds_USA.xml)
  } else {
    stop("Unknown command")
  }
}
