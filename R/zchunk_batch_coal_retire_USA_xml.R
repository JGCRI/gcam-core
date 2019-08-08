#' module_gcamusa_batch_coal_retire_USA_xml
#'
#' Construct XML data structure for \code{coal_retire_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{coal_retire_USA.xml}. The corresponding file in the
#' original data system was \code{batch_coal_retire_USA.xml} (gcamusa xml-batch).
module_gcamusa_batch_coal_retire_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2240.coal_conv_pul_delete_USA",
             "L2240.StubTechProd_elecS_coal_USA",
             "L2240.StubTechProd_elec_coalret_USA",
             "L2240.StubTechEff_elec_coalret_USA",
             "L2240.StubTechSCurve_elec_coalret_USA",
             "L2240.StubTechMarket_elec_coalret_USA",
             "L2240.GlobalTechShrwt_elec_coalret_USA",
             "L2240.GlobalTechCapFac_elec_coalret_USA",
             "L2240.GlobalTechCapital_elec_coalret_USA",
             "L2240.GlobalTechOMfixed_elec_coalret_USA",
             "L2240.GlobalTechOMvar_elec_coalret_USA",
             "L2240.GlobalTechEff_elec_coalret_USA",
             "L2240.GlobalTechProfitShutdown_elec_coalret_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "coal_retire_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # silence package check notes
    tech.share.weight <- share.weight <- sector.name <- supplysector <- subsector.name <- subsector <- NULL

    # Load required inputs
    L2240.coal_conv_pul_delete_USA <- get_data(all_data, "L2240.coal_conv_pul_delete_USA")
    L2240.StubTechProd_elecS_coal_USA <- get_data(all_data, "L2240.StubTechProd_elecS_coal_USA")
    L2240.StubTechProd_elec_coalret_USA <- get_data(all_data, "L2240.StubTechProd_elec_coalret_USA")
    L2240.StubTechEff_elec_coalret_USA <- get_data(all_data, "L2240.StubTechEff_elec_coalret_USA")
    L2240.StubTechSCurve_elec_coalret_USA <- get_data(all_data, "L2240.StubTechSCurve_elec_coalret_USA")
    L2240.StubTechMarket_elec_coalret_USA <- get_data(all_data, "L2240.StubTechMarket_elec_coalret_USA")
    L2240.GlobalTechShrwt_elec_coalret_USA <- get_data(all_data, "L2240.GlobalTechShrwt_elec_coalret_USA")
    L2240.GlobalTechCapFac_elec_coalret_USA <- get_data(all_data, "L2240.GlobalTechCapFac_elec_coalret_USA") %>%
      rename(sector.name = supplysector, subsector.name = subsector)
    L2240.GlobalTechCapital_elec_coalret_USA <- get_data(all_data, "L2240.GlobalTechCapital_elec_coalret_USA") %>%
      rename(sector.name = supplysector, subsector.name = subsector)
    L2240.GlobalTechOMfixed_elec_coalret_USA <- get_data(all_data, "L2240.GlobalTechOMfixed_elec_coalret_USA") %>%
      rename(sector.name = supplysector, subsector.name = subsector)
    L2240.GlobalTechOMvar_elec_coalret_USA <- get_data(all_data, "L2240.GlobalTechOMvar_elec_coalret_USA") %>%
      rename(sector.name = supplysector, subsector.name = subsector)
    L2240.GlobalTechEff_elec_coalret_USA <- get_data(all_data, "L2240.GlobalTechEff_elec_coalret_USA") %>%
      rename(sector.name = supplysector, subsector.name = subsector)
    L2240.GlobalTechProfitShutdown_elec_coalret_USA <- get_data(all_data, "L2240.GlobalTechProfitShutdown_elec_coalret_USA") %>%
      rename(sector.name = supplysector, subsector.name = subsector)

    # Produce outputs
    create_xml("coal_retire_USA.xml") %>%
      add_xml_data(L2240.coal_conv_pul_delete_USA, "DeleteStubTech") %>%
      add_xml_data(L2240.StubTechProd_elecS_coal_USA, "StubTechProd") %>%
      add_xml_data(L2240.StubTechProd_elec_coalret_USA, "StubTechProd") %>%
      add_xml_data(L2240.StubTechEff_elec_coalret_USA, "StubTechEff") %>%
      add_xml_data(L2240.StubTechSCurve_elec_coalret_USA, "StubTechSCurve") %>%
      add_xml_data(L2240.StubTechMarket_elec_coalret_USA, "StubTechMarket") %>%
      add_xml_data(L2240.GlobalTechShrwt_elec_coalret_USA, "GlobalTechShrwt") %>%
      add_xml_data(L2240.GlobalTechCapFac_elec_coalret_USA, "GlobalTechCapFac") %>%
      add_xml_data(L2240.GlobalTechCapital_elec_coalret_USA, "GlobalTechCapital") %>%
      add_xml_data(L2240.GlobalTechOMfixed_elec_coalret_USA, "GlobalTechOMfixed") %>%
      add_xml_data(L2240.GlobalTechOMvar_elec_coalret_USA, "GlobalTechOMvar") %>%
      add_xml_data(L2240.GlobalTechEff_elec_coalret_USA, "GlobalTechEff") %>%
      add_xml_data(L2240.GlobalTechProfitShutdown_elec_coalret_USA, "GlobalTechProfitShutdown") %>%
      add_precursors("L2240.coal_conv_pul_delete_USA",
                     "L2240.StubTechProd_elecS_coal_USA",
                     "L2240.StubTechProd_elec_coalret_USA",
                     "L2240.StubTechEff_elec_coalret_USA",
                     "L2240.StubTechSCurve_elec_coalret_USA",
                     "L2240.StubTechMarket_elec_coalret_USA",
                     "L2240.GlobalTechShrwt_elec_coalret_USA",
                     "L2240.GlobalTechCapFac_elec_coalret_USA",
                     "L2240.GlobalTechCapital_elec_coalret_USA",
                     "L2240.GlobalTechOMfixed_elec_coalret_USA",
                     "L2240.GlobalTechOMvar_elec_coalret_USA",
                     "L2240.GlobalTechEff_elec_coalret_USA",
                     "L2240.GlobalTechProfitShutdown_elec_coalret_USA") ->
      coal_retire_USA.xml

    return_data(coal_retire_USA.xml)
  } else {
    stop("Unknown command")
  }
}
