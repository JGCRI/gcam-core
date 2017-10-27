#' module_gcamusa_batch_industry_USA_xml
#'
#' Construct XML data structure for \code{industry_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{industry_USA.xml}. The corresponding file in the
#' original data system was \code{batch_industry_USA_xml.R} (gcamusa XML).
module_gcamusa_batch_industry_USA_xml_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L232.DeleteSupplysector_USAind",
             "L232.DeleteFinalDemand_USAind",
             "L232.StubTechCalInput_indenergy_USA",
             "L232.StubTechCalInput_indfeed_USA",
             "L232.StubTechProd_industry_USA",
             "L232.StubTechCoef_industry_USA",
             "L232.StubTechMarket_ind_USA",
             "L232.StubTechSecMarket_ind_USA",
             "L232.BaseService_ind_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "industry_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L232.DeleteSupplysector_USAind <- get_data(all_data, "L232.DeleteSupplysector_USAind")
    L232.DeleteFinalDemand_USAind <- get_data(all_data, "L232.DeleteFinalDemand_USAind")
    object <- get_data(all_data, "object")
    L232.StubTechCalInput_indenergy_USA <- get_data(all_data, "L232.StubTechCalInput_indenergy_USA")
    L232.StubTechCalInput_indfeed_USA <- get_data(all_data, "L232.StubTechCalInput_indfeed_USA")
    L232.StubTechProd_industry_USA <- get_data(all_data, "L232.StubTechProd_industry_USA")
    L232.StubTechCoef_industry_USA <- get_data(all_data, "L232.StubTechCoef_industry_USA")
    L232.StubTechMarket_ind_USA <- get_data(all_data, "L232.StubTechMarket_ind_USA")
    L232.StubTechSecMarket_ind_USA <- get_data(all_data, "L232.StubTechSecMarket_ind_USA")
    L232.BaseService_ind_USA <- get_data(all_data, "L232.BaseService_ind_USA")

    # ===================================================

    # Produce outputs
    create_xml("industry_USA.xml") %>%
      add_xml_data(L232.DeleteSupplysector_USAind,"DeleteSupplysector") %>%
      add_xml_data(L232.DeleteFinalDemand_USAind,"DeleteFinalDemand") %>%
      add_xml_data(object,"IDstring") %>%
      add_xml_data(L232.StubTechCalInput_indenergy_USA,"StubTechCalInput") %>%
      add_xml_data(L232.StubTechCalInput_indfeed_USA,"StubTechCalInput") %>%
      add_xml_data(L232.StubTechProd_industry_USA,"StubTechProd") %>%
      add_xml_data(L232.StubTechCoef_industry_USA,"StubTechCoef") %>%
      add_xml_data(L232.StubTechMarket_ind_USA,"StubTechMarket") %>%
      add_xml_data(L232.StubTechSecMarket_ind_USA,"StubTechSecMarket") %>%
      add_xml_data(L232.BaseService_ind_USA,"BaseService") %>%
      add_precursors("L232.DeleteSupplysector_USAind", "L232.DeleteFinalDemand_USAind", "object", "L232.StubTechCalInput_indenergy_USA", "L232.StubTechCalInput_indfeed_USA", "L232.StubTechProd_industry_USA", "L232.StubTechCoef_industry_USA", "L232.StubTechMarket_ind_USA", "L232.StubTechSecMarket_ind_USA", "L232.BaseService_ind_USA") ->
      industry_USA.xml

    return_data(industry_USA.xml)
  } else {
    stop("Unknown command")
  }
}
