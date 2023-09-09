# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_coal_retire_xml
#'
#' Construct XML data structure for \code{coal_retire_vintage_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{coal_retire_vintage_USA.xml}.
module_gcamusa_coal_retire_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2241.StubTechProd_elec_coalret_USA",
             "L2241.StubTechEff_elec_coalret_USA",
             "L2241.StubTechSCurve_elec_coalret_USA",
             "L2241.StubTechMarket_elec_coalret_USA",
             "L2241.GlobalTechShrwt_elec_coalret_USA",
             "L2241.GlobalTechCapFac_elec_coalret_USA",
             "L2241.GlobalTechCapital_elec_coalret_USA",
             "L2241.GlobalTechOMfixed_elec_coalret_USA",
             "L2241.GlobalTechOMvar_elec_coalret_USA",
             "L2241.GlobalTechEff_elec_coalret_USA",
             "L2241.GlobalTechProfitShutdown_elec_coalret_USA",
             # coal vintage
             "L2241.StubTechProd_coal_vintage_USA",
             "L2241.StubTechEff_coal_vintage_USA",
             "L2241.StubTechSCurve_coal_vintage_USA",
             "L2241.StubTechProfitShutdown_coal_vintage_USA",
             "L2241.StubTechMarket_coal_vintage_USA",
             "L2241.GlobalTechShrwt_coal_vintage_USA",
             "L2241.GlobalTechEff_coal_vintage_USA",
             "L2241.GlobalTechCapFac_coal_vintage_USA",
             "L2241.GlobalTechCapital_coal_vintage_USA",
             "L2241.GlobalTechOMfixed_coal_vintage_USA",
             "L2241.GlobalTechOMvar_coal_vintage_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "coal_retire_vintage_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # silence package check notes
    tech.share.weight <- share.weight <- sector.name <- supplysector <- subsector.name <- subsector <- NULL

    # Load required inputs
    L2241.StubTechProd_elec_coalret_USA <- get_data(all_data, "L2241.StubTechProd_elec_coalret_USA")
    L2241.StubTechEff_elec_coalret_USA <- get_data(all_data, "L2241.StubTechEff_elec_coalret_USA")
    L2241.StubTechSCurve_elec_coalret_USA <- get_data(all_data, "L2241.StubTechSCurve_elec_coalret_USA")
    L2241.StubTechMarket_elec_coalret_USA <- get_data(all_data, "L2241.StubTechMarket_elec_coalret_USA")
    L2241.GlobalTechShrwt_elec_coalret_USA <- get_data(all_data, "L2241.GlobalTechShrwt_elec_coalret_USA")
    L2241.GlobalTechCapFac_elec_coalret_USA <- get_data(all_data, "L2241.GlobalTechCapFac_elec_coalret_USA") %>%
      rename(sector.name = supplysector, subsector.name = subsector)
    L2241.GlobalTechCapital_elec_coalret_USA <- get_data(all_data, "L2241.GlobalTechCapital_elec_coalret_USA") %>%
      rename(sector.name = supplysector, subsector.name = subsector)
    L2241.GlobalTechOMfixed_elec_coalret_USA <- get_data(all_data, "L2241.GlobalTechOMfixed_elec_coalret_USA") %>%
      rename(sector.name = supplysector, subsector.name = subsector)
    L2241.GlobalTechOMvar_elec_coalret_USA <- get_data(all_data, "L2241.GlobalTechOMvar_elec_coalret_USA") %>%
      rename(sector.name = supplysector, subsector.name = subsector)
    L2241.GlobalTechEff_elec_coalret_USA <- get_data(all_data, "L2241.GlobalTechEff_elec_coalret_USA") %>%
      rename(sector.name = supplysector, subsector.name = subsector)
    L2241.GlobalTechProfitShutdown_elec_coalret_USA <- get_data(all_data, "L2241.GlobalTechProfitShutdown_elec_coalret_USA") %>%
      rename(sector.name = supplysector, subsector.name = subsector)
    # coal vintage
    L2241.StubTechProd_coal_vintage_USA <- get_data(all_data, "L2241.StubTechProd_coal_vintage_USA")
    L2241.StubTechEff_coal_vintage_USA <- get_data(all_data, "L2241.StubTechEff_coal_vintage_USA")
    L2241.StubTechSCurve_coal_vintage_USA <- get_data(all_data, "L2241.StubTechSCurve_coal_vintage_USA")
    L2241.StubTechProfitShutdown_coal_vintage_USA <- get_data(all_data, "L2241.StubTechProfitShutdown_coal_vintage_USA")
    L2241.StubTechMarket_coal_vintage_USA <- get_data(all_data, "L2241.StubTechMarket_coal_vintage_USA")
    L2241.GlobalTechShrwt_coal_vintage_USA <- get_data(all_data, "L2241.GlobalTechShrwt_coal_vintage_USA")
    L2241.GlobalTechEff_coal_vintage_USA <- get_data(all_data, "L2241.GlobalTechEff_coal_vintage_USA")
    L2241.GlobalTechCapFac_coal_vintage_USA <- get_data(all_data, "L2241.GlobalTechCapFac_coal_vintage_USA")
    L2241.GlobalTechCapital_coal_vintage_USA <- get_data(all_data, "L2241.GlobalTechCapital_coal_vintage_USA")
    L2241.GlobalTechOMfixed_coal_vintage_USA <- get_data(all_data, "L2241.GlobalTechOMfixed_coal_vintage_USA")
    L2241.GlobalTechOMvar_coal_vintage_USA <- get_data(all_data, "L2241.GlobalTechOMvar_coal_vintage_USA")

    # Produce outputs
    create_xml("coal_retire_vintage_USA.xml") %>%
      add_xml_data(L2241.StubTechProd_elec_coalret_USA, "StubTechProd") %>%
      add_xml_data(L2241.StubTechEff_elec_coalret_USA, "StubTechEff") %>%
      add_xml_data(L2241.StubTechSCurve_elec_coalret_USA, "StubTechSCurve") %>%
      add_xml_data(L2241.StubTechMarket_elec_coalret_USA, "StubTechMarket") %>%
      add_xml_data(L2241.GlobalTechShrwt_elec_coalret_USA, "GlobalTechShrwt") %>%
      add_xml_data(L2241.GlobalTechCapFac_elec_coalret_USA, "GlobalTechCapFac") %>%
      add_xml_data(L2241.GlobalTechCapital_elec_coalret_USA, "GlobalTechCapital") %>%
      add_xml_data(L2241.GlobalTechOMfixed_elec_coalret_USA, "GlobalTechOMfixed") %>%
      add_xml_data(L2241.GlobalTechOMvar_elec_coalret_USA, "GlobalTechOMvar") %>%
      add_xml_data(L2241.GlobalTechEff_elec_coalret_USA, "GlobalTechEff") %>%
      add_xml_data(L2241.GlobalTechProfitShutdown_elec_coalret_USA, "GlobalTechProfitShutdown") %>%
      # coal vintage
      add_xml_data(L2241.StubTechProd_coal_vintage_USA, "StubTechProd") %>%
      add_xml_data(L2241.StubTechEff_coal_vintage_USA, "StubTechEff") %>%
      add_xml_data(L2241.StubTechSCurve_coal_vintage_USA, "StubTechSCurve") %>%
      add_xml_data(L2241.StubTechProfitShutdown_coal_vintage_USA, "StubTechProfitShutdown") %>%
      add_xml_data(L2241.StubTechMarket_coal_vintage_USA, "StubTechMarket") %>%
      add_xml_data(L2241.GlobalTechShrwt_coal_vintage_USA, "GlobalTechShrwt") %>%
      add_xml_data(L2241.GlobalTechEff_coal_vintage_USA, "GlobalTechEff") %>%
      add_xml_data(L2241.GlobalTechCapFac_coal_vintage_USA, "GlobalTechCapFac") %>%
      add_xml_data(L2241.GlobalTechCapital_coal_vintage_USA, "GlobalTechCapital") %>%
      add_xml_data(L2241.GlobalTechOMfixed_coal_vintage_USA, "GlobalTechOMfixed") %>%
      add_xml_data(L2241.GlobalTechOMvar_coal_vintage_USA, "GlobalTechOMvar") %>%
      add_precursors("L2241.StubTechProd_elec_coalret_USA",
                     "L2241.StubTechEff_elec_coalret_USA",
                     "L2241.StubTechSCurve_elec_coalret_USA",
                     "L2241.StubTechMarket_elec_coalret_USA",
                     "L2241.GlobalTechShrwt_elec_coalret_USA",
                     "L2241.GlobalTechCapFac_elec_coalret_USA",
                     "L2241.GlobalTechCapital_elec_coalret_USA",
                     "L2241.GlobalTechOMfixed_elec_coalret_USA",
                     "L2241.GlobalTechOMvar_elec_coalret_USA",
                     "L2241.GlobalTechEff_elec_coalret_USA",
                     "L2241.GlobalTechProfitShutdown_elec_coalret_USA",
                     # coal vintage
                     "L2241.StubTechProd_coal_vintage_USA",
                     "L2241.StubTechEff_coal_vintage_USA",
                     "L2241.StubTechSCurve_coal_vintage_USA",
                     "L2241.StubTechProfitShutdown_coal_vintage_USA",
                     "L2241.StubTechMarket_coal_vintage_USA",
                     "L2241.GlobalTechShrwt_coal_vintage_USA",
                     "L2241.GlobalTechEff_coal_vintage_USA",
                     "L2241.GlobalTechCapFac_coal_vintage_USA",
                     "L2241.GlobalTechCapital_coal_vintage_USA",
                     "L2241.GlobalTechOMfixed_coal_vintage_USA",
                     "L2241.GlobalTechOMvar_coal_vintage_USA") ->
      coal_retire_vintage_USA.xml

    return_data(coal_retire_vintage_USA.xml)
  } else {
    stop("Unknown command")
  }
}
