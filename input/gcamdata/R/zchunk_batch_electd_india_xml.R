#' module_gcamindia_batch_electd_xml
#'
#' Construct XML data structure for \code{electd_india.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electd_india.xml}. The corresponding file in the
#' original data system was \code{batch_electd_india_xml.R} (gcamindia XML).
module_gcamindia_batch_electd_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L226.india_state_DeleteSupplysector_elec",
             "L226.india_state_Supplysector_electd",
             "L226.india_state_SubsectorLogit_electd",
             "L226.india_state_SubsectorShrwtFllt_electd",
             "L226.india_state_SubsectorInterp_electd",
             "L226.india_state_Supplysector_electd",
             "L226.india_state_TechShrwt_electd",
             "L226.india_state_TechCost_electd",
             "L226.india_state_TechCoef_electd"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electd_india.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L226.india_state_DeleteSupplysector_elec <- get_data(all_data, "L226.india_state_DeleteSupplysector_elec")
    L226.india_state_Supplysector_electd<- get_data(all_data, "L226.india_state_Supplysector_electd")
    L226.india_state_SubsectorLogit_electd <- get_data(all_data, "L226.india_state_SubsectorLogit_electd")
    L226.india_state_SubsectorShrwtFllt_electd <- get_data(all_data, "L226.india_state_SubsectorShrwtFllt_electd")
    L226.india_state_SubsectorInterp_electd <- get_data(all_data, "L226.india_state_SubsectorInterp_electd")
    L226.india_state_Supplysector_electd <- get_data(all_data, "L226.india_state_Supplysector_electd")
    L226.india_state_TechShrwt_electd <- get_data(all_data, "L226.india_state_TechShrwt_electd")
    L226.india_state_TechCost_electd <- get_data(all_data, "L226.india_state_TechCost_electd")
    L226.india_state_TechCoef_electd <- get_data(all_data, "L226.india_state_TechCoef_electd")

    # ===================================================

    # Produce outputs
    create_xml("electd_india.xml") %>%
      add_xml_data(L226.india_state_DeleteSupplysector_elec, "DeleteSupplysector") %>%
      add_logit_tables_xml(L226.india_state_Supplysector_electd, "Supplysector") %>%
      add_logit_tables_xml(L226.india_state_SubsectorLogit_electd, "SubsectorLogit") %>%
      add_xml_data(L226.india_state_SubsectorShrwtFllt_electd, "SubsectorShrwtFllt") %>%
      add_xml_data(L226.india_state_SubsectorInterp_electd, "SubsectorInterp") %>%
      add_xml_data(L226.india_state_TechShrwt_electd, "TechShrwt") %>%
      add_xml_data(L226.india_state_TechCost_electd, "TechCost") %>%
      add_xml_data(L226.india_state_TechCoef_electd, "TechCoef") %>%
      add_precursors("L226.india_state_DeleteSupplysector_elec",
                     "L226.india_state_Supplysector_electd",
                     "L226.india_state_SubsectorLogit_electd",
                     "L226.india_state_SubsectorShrwtFllt_electd",
                     "L226.india_state_SubsectorInterp_electd",
                     "L226.india_state_Supplysector_electd",
                     "L226.india_state_TechShrwt_electd",
                     "L226.india_state_TechCost_electd",
                     "L226.india_state_TechCoef_electd") ->
      electd_india.xml

    return_data(electd_india.xml)
  } else {
    stop("Unknown command")
  }
}
