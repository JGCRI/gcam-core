#' module_gcamusa_batch_transportation_USA_xml
#'
#' Construct XML data structure for \code{transportation_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_USA.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_USA_xml.R} (gcamusa XML).
module_gcamusa_batch_transportation_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.DeleteSupplysector_USAtrn",
             "L254.DeleteFinalDemand_USAtrn",
             "L254.StubTranTechCalInput_USA",
             "L254.StubTranTechProd_nonmotor_USA",
             "L254.StubTranTechCalInput_passthru_USA",
             "L254.BaseService_trn_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "transportation_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L254.DeleteSupplysector_USAtrn <- get_data(all_data, "L254.DeleteSupplysector_USAtrn")
    L254.DeleteFinalDemand_USAtrn <- get_data(all_data, "L254.DeleteFinalDemand_USAtrn")
    L254.StubTranTechCalInput_USA <- get_data(all_data, "L254.StubTranTechCalInput_USA")
    L254.StubTranTechProd_nonmotor_USA <- get_data(all_data, "L254.StubTranTechProd_nonmotor_USA")
    L254.StubTranTechCalInput_passthru_USA <- get_data(all_data, "L254.StubTranTechCalInput_passthru_USA")
    L254.BaseService_trn_USA <- get_data(all_data, "L254.BaseService_trn_USA")

    # ===================================================

    # Produce outputs
    create_xml("transportation_USA.xml") %>%
      add_xml_data(L254.DeleteSupplysector_USAtrn,"DeleteSupplysector") %>%
      add_xml_data(L254.DeleteFinalDemand_USAtrn,"DeleteFinalDemand") %>%
      add_xml_data(L254.StubTranTechCalInput_USA,"StubTranTechCalInput") %>%
      add_xml_data(L254.StubTranTechProd_nonmotor_USA,"StubTranTechProd") %>%
      add_xml_data(L254.StubTranTechCalInput_passthru_USA,"StubTranTechCalInput") %>%
      add_xml_data(L254.BaseService_trn_USA,"BaseService") %>%
      add_precursors("L254.DeleteSupplysector_USAtrn", "L254.DeleteFinalDemand_USAtrn", "L254.StubTranTechCalInput_USA", "L254.StubTranTechProd_nonmotor_USA", "L254.StubTranTechCalInput_passthru_USA", "L254.BaseService_trn_USA") ->
      transportation_USA.xml

    return_data(transportation_USA.xml)
  } else {
    stop("Unknown command")
  }
}
