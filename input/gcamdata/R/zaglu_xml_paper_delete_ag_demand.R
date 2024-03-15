# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_paper_delete_ag_demand_xml
#'
#' Construct XML data structure for \code{paper_delete_ag_demand.xml} and \code{paper_delete_ag_demand_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{paper_delete_ag_demand.xml}. The corresponding file in the
#' original data system was \code{batch_paper_delete_ag_demand_xml.R} (energy XML).
module_aglu_paper_delete_ag_demand_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2327.DeleteSupplysector_PaperAgDemand",
             "L2327.DeleteFinalDemand_PaperAgDemand",
             "L2327.DeleteSupplysector_PaperAgDemand_USA",
             "L2327.DeleteFinalDemand_PaperAgDemand_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "paper_delete_ag_demand.xml",
             XML = "paper_delete_ag_demand_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2327.DeleteSupplysector_PaperAgDemand <- get_data(all_data, "L2327.DeleteSupplysector_PaperAgDemand", strip_attributes = TRUE)
    L2327.DeleteFinalDemand_PaperAgDemand <- get_data(all_data, "L2327.DeleteFinalDemand_PaperAgDemand", strip_attributes = TRUE)
    L2327.DeleteSupplysector_PaperAgDemand_USA <- get_data(all_data, "L2327.DeleteSupplysector_PaperAgDemand_USA", strip_attributes = TRUE)
    L2327.DeleteFinalDemand_PaperAgDemand_USA <- get_data(all_data, "L2327.DeleteFinalDemand_PaperAgDemand_USA", strip_attributes = TRUE)

    # ===================================================

    # Produce outputs
    create_xml("paper_delete_ag_demand.xml") %>%
      add_xml_data(L2327.DeleteSupplysector_PaperAgDemand, "DeleteSupplysector") %>%
      add_xml_data(L2327.DeleteFinalDemand_PaperAgDemand, "DeleteFinalDemand") %>%
      add_precursors("L2327.DeleteSupplysector_PaperAgDemand",
                     "L2327.DeleteFinalDemand_PaperAgDemand") ->
      paper_delete_ag_demand.xml

    create_xml("paper_delete_ag_demand_USA.xml") %>%
      add_xml_data(L2327.DeleteSupplysector_PaperAgDemand_USA, "DeleteSupplysector") %>%
      add_xml_data(L2327.DeleteFinalDemand_PaperAgDemand_USA, "DeleteFinalDemand") %>%
      add_precursors("L2327.DeleteSupplysector_PaperAgDemand_USA",
                     "L2327.DeleteFinalDemand_PaperAgDemand_USA") ->
      paper_delete_ag_demand_USA.xml


    return_data(paper_delete_ag_demand.xml, paper_delete_ag_demand_USA.xml)
  } else {
    stop("Unknown command")
  }
}
