#' module_gcamusa_batch_electd_USA_xml
#'
#' Construct XML data structure for \code{electd_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electd_USA.xml}. The corresponding file in the
#' original data system was \code{batch_electd_USA_xml.R} (gcamusa XML).
module_gcamusa_batch_electd_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L226.DeleteSupplysector_USAelec",
             "L226.Supplysector_electd_USA",
             "L226.SubsectorLogit_electd_USA",
             "L226.SubsectorShrwtFllt_electd_USA",
             "L226.SubsectorInterp_electd_USA",
             "L226.Supplysector_electd_USA",
             "L226.TechShrwt_electd_USA",
             "L226.TechCost_electd_USA",
             "L226.TechCoef_electd_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electd_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L226.DeleteSupplysector_USAelec <- get_data(all_data, "L226.DeleteSupplysector_USAelec")
    L226.Supplysector_electd_USA<- get_data(all_data, "L226.Supplysector_electd_USA")
    L226.SubsectorLogit_electd_USA <- get_data(all_data, "L226.SubsectorLogit_electd_USA")
    L226.SubsectorShrwtFllt_electd_USA <- get_data(all_data, "L226.SubsectorShrwtFllt_electd_USA")
    L226.SubsectorInterp_electd_USA <- get_data(all_data, "L226.SubsectorInterp_electd_USA")
    L226.Supplysector_electd_USA <- get_data(all_data, "L226.Supplysector_electd_USA")
    L226.TechShrwt_electd_USA <- get_data(all_data, "L226.TechShrwt_electd_USA")
    L226.TechCost_electd_USA <- get_data(all_data, "L226.TechCost_electd_USA")
    L226.TechCoef_electd_USA <- get_data(all_data, "L226.TechCoef_electd_USA")

    # ===================================================

    # Produce outputs
    create_xml("electd_USA.xml") %>%
      add_xml_data(L226.DeleteSupplysector_USAelec, "DeleteSupplysector") %>%
      add_logit_tables_xml(L226.Supplysector_electd_USA, "Supplysector") %>%
      add_logit_tables_xml(L226.SubsectorLogit_electd_USA, "SubsectorLogit") %>%
      add_xml_data(L226.SubsectorShrwtFllt_electd_USA, "SubsectorShrwtFllt") %>%
      add_xml_data(L226.SubsectorInterp_electd_USA, "SubsectorInterp") %>%
      add_xml_data(L226.TechShrwt_electd_USA, "TechShrwt") %>%
      add_xml_data(L226.TechCost_electd_USA, "TechCost") %>%
      add_xml_data(L226.TechCoef_electd_USA, "TechCoef") %>%
      add_precursors("L226.DeleteSupplysector_USAelec",
                     "L226.Supplysector_electd_USA",
                     "L226.SubsectorLogit_electd_USA",
                     "L226.SubsectorShrwtFllt_electd_USA",
                     "L226.SubsectorInterp_electd_USA",
                     "L226.Supplysector_electd_USA",
                     "L226.TechShrwt_electd_USA",
                     "L226.TechCost_electd_USA",
                     "L226.TechCoef_electd_USA") ->
      electd_USA.xml

    return_data(electd_USA.xml)
  } else {
    stop("Unknown command")
  }
}
