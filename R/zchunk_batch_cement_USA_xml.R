#' module_gcamusa_batch_cement_USA_xml
#'
#' Construct XML data structure for \code{cement_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{cement_USA.xml}. The corresponding file in the
#' original data system was \code{batch_cement_USA_xml.R} (gcamusa XML).
module_gcamusa_batch_cement_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L210.DeleteUnlimitRsrc_USAlimestone",
             "L210.UnlimitRsrc_limestone_USA",
             "L210.UnlimitRsrcPrice_limestone_USA",
             "L2321.DeleteSupplysector_USAcement",
             "L2321.Supplysector_cement_USA",
             "L2321.FinalEnergyKeyword_cement_USA",
             "L2321.SubsectorLogit_cement_USA",
             "L2321.SubsectorShrwtFllt_cement_USA",
             "L2321.SubsectorInterp_cement_USA",
             "L2321.StubTech_cement_USA",
             "L2321.PerCapitaBased_cement_USA",
             "L2321.PriceElasticity_cement_USA",
             "L2321.IncomeElasticity_cement_gcam3_USA",
             "L2321.DeleteFinalDemand_USAcement",
             "L2321.StubTechProd_cement_USA",
             "L2321.StubTechCoef_cement_USA",
             "L2321.StubTechCalInput_cement_heat_USA",
             "L2321.StubTechMarket_cement_USA",
             "L2321.BaseService_cement_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "cement_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L210.DeleteUnlimitRsrc_USAlimestone <- get_data(all_data, "L210.DeleteUnlimitRsrc_USAlimestone")
    L210.UnlimitRsrc_limestone_USA <- get_data(all_data, "L210.UnlimitRsrc_limestone_USA")
    L210.UnlimitRsrcPrice_limestone_USA <- get_data(all_data, "L210.UnlimitRsrcPrice_limestone_USA")
    L2321.DeleteSupplysector_USAcement <- get_data(all_data, "L2321.DeleteSupplysector_USAcement")
    L2321.FinalEnergyKeyword_cement_USA <- get_data(all_data, "L2321.FinalEnergyKeyword_cement_USA")
    L2321.SubsectorLogit_cement_USA <- get_data(all_data, "L2321.SubsectorLogit_cement_USA")
    L2321.SubsectorShrwtFllt_cement_USA <- get_data(all_data, "L2321.SubsectorShrwtFllt_cement_USA")
    L2321.SubsectorInterp_cement_USA <- get_data(all_data, "L2321.SubsectorInterp_cement_USA")
    L2321.StubTech_cement_USA <- get_data(all_data, "L2321.StubTech_cement_USA")
    L2321.PerCapitaBased_cement_USA <- get_data(all_data, "L2321.PerCapitaBased_cement_USA")
    L2321.PriceElasticity_cement_USA <- get_data(all_data, "L2321.PriceElasticity_cement_USA")
    L2321.IncomeElasticity_cement_gcam3_USA <- get_data(all_data, "L2321.IncomeElasticity_cement_gcam3_USA")
    L2321.DeleteFinalDemand_USAcement <- get_data(all_data, "L2321.DeleteFinalDemand_USAcement")
    L2321.Supplysector_cement_USA <- get_data(all_data, "L2321.Supplysector_cement_USA")
    L2321.StubTechProd_cement_USA <- get_data(all_data, "L2321.StubTechProd_cement_USA")
    L2321.StubTechCoef_cement_USA <- get_data(all_data, "L2321.StubTechCoef_cement_USA")
    L2321.StubTechCalInput_cement_heat_USA <- get_data(all_data, "L2321.StubTechCalInput_cement_heat_USA")
    L2321.StubTechMarket_cement_USA <- get_data(all_data, "L2321.StubTechMarket_cement_USA")
    L2321.BaseService_cement_USA <- get_data(all_data, "L2321.BaseService_cement_USA")

    # ===================================================

    # Produce outputs
    create_xml("cement_USA.xml") %>%
      add_xml_data(L210.DeleteUnlimitRsrc_USAlimestone,"DeleteUnlimitRsrc") %>%
      add_xml_data(L210.UnlimitRsrc_limestone_USA,"UnlimitRsrc") %>%
      add_xml_data(L210.UnlimitRsrcPrice_limestone_USA,"UnlimitRsrcPrice") %>%
      add_xml_data(L2321.DeleteSupplysector_USAcement,"DeleteSupplysector") %>%
      add_xml_data(L2321.DeleteFinalDemand_USAcement,"DeleteFinalDemand") %>%
      add_logit_tables_xml(L2321.Supplysector_cement_USA, "Supplysector") %>%
      add_xml_data(L2321.FinalEnergyKeyword_cement_USA, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2321.SubsectorLogit_cement_USA, "SubsectorLogit") %>%
      add_xml_data(L2321.SubsectorShrwtFllt_cement_USA, "SubsectorShrwtFllt") %>%
      add_xml_data(L2321.SubsectorInterp_cement_USA, "SubsectorInterp") %>%
      add_xml_data(L2321.StubTech_cement_USA, "StubTech") %>%
      add_xml_daata(L2321.PerCapitaBased_cement_USA, "PerCapitaBased") %>%
      add_xml_data(L2321.PriceElasticity_cement_USA, "PriceElasticity") %>%
      add_xml_data(L2321.IncomeElasticity_cement_gcam3_USA, "IncomeElasticity") %>%
      add_xml_data(L2321.StubTechProd_cement_USA,"StubTechProd") %>%
      add_xml_data(L2321.StubTechCoef_cement_USA,"StubTechCoef") %>%
      add_xml_data(L2321.StubTechCalInput_cement_heat_USA,"StubTechCalInput") %>%
      add_xml_data(L2321.StubTechMarket_cement_USA,"StubTechMarket") %>%
      add_xml_data(L2321.BaseService_cement_USA,"BaseService") %>%
      add_precursors("L210.DeleteUnlimitRsrc_USAlimestone",
                     "L210.UnlimitRsrc_limestone_USA",
                     "L210.UnlimitRsrcPrice_limestone_USA",
                     "L2321.DeleteSupplysector_USAcement",
                     "L2321.Supplysector_cement_USA",
                     "L2321.FinalEnergyKeyword_cement_USA",
                     "L2321.SubsectorLogit_cement_USA",
                     "L2321.SubsectorShrwtFllt_cement_USA",
                     "L2321.SubsectorInterp_cement_USA",
                     "L2321.StubTech_cement_USA",
                     "L2321.PerCapitaBased_cement_USA",
                     "L2321.PriceElasticity_cement_USA",
                     "L2321.IncomeElasticity_cement_gcam3_USA",
                     "L2321.DeleteFinalDemand_USAcement",
                     "L2321.StubTechProd_cement_USA",
                     "L2321.StubTechCoef_cement_USA",
                     "L2321.StubTechCalInput_cement_heat_USA",
                     "L2321.StubTechMarket_cement_USA",
                     "L2321.BaseService_cement_USA") ->
      cement_USA.xml

    return_data(cement_USA.xml)
  } else {
    stop("Unknown command")
  }
}
