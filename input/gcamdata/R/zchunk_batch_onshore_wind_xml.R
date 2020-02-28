# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_onshore_wind_xml
#'
#' Construct XML data structure for \code{onshore_wind.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{onshore_wind.xml}.
module_energy_batch_onshore_wind_xml<- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2231.SmthRenewRsrcCurves_onshore_wind",
             "L2231.StubTechCapFactor_onshore_wind",
             "L2231.SmthRenewRsrcTechChange_onshore_wind",
             "L2231.StubTechCost_onshore_wind"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "onshore_wind.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2231.SmthRenewRsrcCurves_onshore_wind <- get_data(all_data, "L2231.SmthRenewRsrcCurves_onshore_wind")
    L2231.StubTechCapFactor_onshore_wind <- get_data(all_data, "L2231.StubTechCapFactor_onshore_wind")
    L2231.SmthRenewRsrcTechChange_onshore_wind <- get_data(all_data, "L2231.SmthRenewRsrcTechChange_onshore_wind")
    L2231.StubTechCost_onshore_wind <- get_data(all_data, "L2231.StubTechCost_onshore_wind")

    # ===================================================

    # Produce outputs
    create_xml("onshore_wind.xml") %>%
      add_xml_data(L2231.SmthRenewRsrcCurves_onshore_wind, "SmthRenewRsrcCurves") %>%
      add_xml_data(L2231.StubTechCapFactor_onshore_wind, "StubTechCapFactor") %>%
      add_xml_data(L2231.SmthRenewRsrcTechChange_onshore_wind, "SmthRenewRsrcTechChange") %>%
      add_xml_data(L2231.StubTechCost_onshore_wind, "StubTechCost") %>%
      add_precursors("L2231.SmthRenewRsrcCurves_onshore_wind",
                     "L2231.StubTechCapFactor_onshore_wind",
                     "L2231.SmthRenewRsrcTechChange_onshore_wind",
                     "L2231.StubTechCost_onshore_wind") ->
      onshore_wind.xml

    return_data(onshore_wind.xml)
  } else {
    stop("Unknown command")
  }
}
