# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_batch_coal_vintage_USA_xml
#'
#' Construct XML data structure for \code{coal_vintage_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{coal_vintage_USA.xml}. The corresponding file in the
#' original data system was \code{batch_coal_vintage_USA.xml} (gcamusa xml-batch).
module_gcamusa_batch_coal_vintage_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2246.StubTechProd_coal_vintage_USA",
             "L2246.StubTechEff_coal_vintage_USA",
             "L2246.StubTechLifetime_coal_vintage_USA",
             "L2246.StubTechSCurve_coal_vintage_USA",
             "L2246.StubTechProfitShutdown_coal_vintage_USA",
             "L2246.StubTechMarket_coal_vintage_USA",
             "L2246.GlobalTechShrwt_coal_vintage_USA",
             "L2246.GlobalTechEff_coal_vintage_USA",
             "L2246.GlobalTechCapFac_coal_vintage_USA",
             "L2246.GlobalTechCapital_coal_vintage_USA",
             "L2246.GlobalTechOMfixed_coal_vintage_USA",
             "L2246.GlobalTechOMvar_coal_vintage_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "coal_vintage_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2246.StubTechProd_coal_vintage_USA <- get_data(all_data, "L2246.StubTechProd_coal_vintage_USA")
    L2246.StubTechEff_coal_vintage_USA <- get_data(all_data, "L2246.StubTechEff_coal_vintage_USA")
    L2246.StubTechLifetime_coal_vintage_USA <- get_data(all_data, "L2246.StubTechLifetime_coal_vintage_USA")
    L2246.StubTechSCurve_coal_vintage_USA <- get_data(all_data, "L2246.StubTechSCurve_coal_vintage_USA")
    L2246.StubTechProfitShutdown_coal_vintage_USA <- get_data(all_data, "L2246.StubTechProfitShutdown_coal_vintage_USA")
    L2246.StubTechMarket_coal_vintage_USA <- get_data(all_data, "L2246.StubTechMarket_coal_vintage_USA")
    L2246.GlobalTechShrwt_coal_vintage_USA <- get_data(all_data, "L2246.GlobalTechShrwt_coal_vintage_USA")
    L2246.GlobalTechEff_coal_vintage_USA <- get_data(all_data, "L2246.GlobalTechEff_coal_vintage_USA")
    L2246.GlobalTechCapFac_coal_vintage_USA <- get_data(all_data, "L2246.GlobalTechCapFac_coal_vintage_USA")
    L2246.GlobalTechCapital_coal_vintage_USA <- get_data(all_data, "L2246.GlobalTechCapital_coal_vintage_USA")
    L2246.GlobalTechOMfixed_coal_vintage_USA <- get_data(all_data, "L2246.GlobalTechOMfixed_coal_vintage_USA")
    L2246.GlobalTechOMvar_coal_vintage_USA <- get_data(all_data, "L2246.GlobalTechOMvar_coal_vintage_USA")

    # Produce outputs
    create_xml("coal_vintage_USA.xml") %>%
      add_xml_data(L2246.StubTechProd_coal_vintage_USA, "StubTechProd") %>%
      add_xml_data(L2246.StubTechEff_coal_vintage_USA, "StubTechEff") %>%
      add_xml_data(L2246.StubTechLifetime_coal_vintage_USA, "StubTechLifetime") %>%
      add_xml_data(L2246.StubTechSCurve_coal_vintage_USA, "StubTechSCurve") %>%
      add_xml_data(L2246.StubTechProfitShutdown_coal_vintage_USA, "StubTechProfitShutdown") %>%
      add_xml_data(L2246.StubTechMarket_coal_vintage_USA, "StubTechMarket") %>%
      add_xml_data(L2246.GlobalTechShrwt_coal_vintage_USA, "GlobalTechShrwt") %>%
      add_xml_data(L2246.GlobalTechEff_coal_vintage_USA, "GlobalTechEff") %>%
      add_xml_data(L2246.GlobalTechCapFac_coal_vintage_USA, "GlobalTechCapFac") %>%
      add_xml_data(L2246.GlobalTechCapital_coal_vintage_USA, "GlobalTechCapital") %>%
      add_xml_data(L2246.GlobalTechOMfixed_coal_vintage_USA, "GlobalTechOMfixed") %>%
      add_xml_data(L2246.GlobalTechOMvar_coal_vintage_USA, "GlobalTechOMvar") %>%
      add_precursors("L2246.StubTechProd_coal_vintage_USA",
                     "L2246.StubTechEff_coal_vintage_USA",
                     "L2246.StubTechLifetime_coal_vintage_USA",
                     "L2246.StubTechSCurve_coal_vintage_USA",
                     "L2246.StubTechProfitShutdown_coal_vintage_USA",
                     "L2246.StubTechMarket_coal_vintage_USA",
                     "L2246.GlobalTechShrwt_coal_vintage_USA",
                     "L2246.GlobalTechEff_coal_vintage_USA",
                     "L2246.GlobalTechCapFac_coal_vintage_USA",
                     "L2246.GlobalTechCapital_coal_vintage_USA",
                     "L2246.GlobalTechOMfixed_coal_vintage_USA",
                     "L2246.GlobalTechOMvar_coal_vintage_USA") ->
      coal_vintage_USA.xml

    return_data(coal_vintage_USA.xml)
  } else {
    stop("Unknown command")
  }
}
