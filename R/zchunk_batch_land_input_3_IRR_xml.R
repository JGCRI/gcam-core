#' module_aglu_batch_land_input_3_IRR_xml
#'
#' Construct XML data structure for \code{land_input_3_IRR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{land_input_3_IRR.xml}. The corresponding file in the
#' original data system was \code{batch_land_input_3_IRR.xml.R} (aglu XML).
module_aglu_batch_land_input_3_IRR_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2231.LN3_Logit",
              "L2231.LN3_HistUnmgdAllocation",
              "L2231.LN3_UnmgdAllocation",
              "L2231.LN3_NoEmissCarbon",
              "L2231.LN3_NodeCarbon",
              "L2231.LN3_HistMgdAllocation_noncrop",
              "L2231.LN3_MgdAllocation_noncrop",
              "L2231.LN3_UnmgdCarbon",
              "L2231.LN3_MgdCarbon_noncrop"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "land_input_3_IRR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2231.LN3_Logit <- get_data(all_data, "L2231.LN3_Logit")
    L2231.LN3_HistUnmgdAllocation <- get_data(all_data, "L2231.LN3_HistUnmgdAllocation")
    L2231.LN3_UnmgdAllocation <- get_data(all_data, "L2231.LN3_UnmgdAllocation")
    L2231.LN3_NoEmissCarbon <- get_data(all_data, "L2231.LN3_NoEmissCarbon")
    L2231.LN3_NodeCarbon <- get_data(all_data, "L2231.LN3_NodeCarbon")
    L2231.LN3_HistMgdAllocation_noncrop <- get_data(all_data, "L2231.LN3_HistMgdAllocation_noncrop")
    L2231.LN3_MgdAllocation_noncrop <- get_data(all_data, "L2231.LN3_MgdAllocation_noncrop")
    L2231.LN3_UnmgdCarbon <- get_data(all_data, "L2231.LN3_UnmgdCarbon")
    L2231.LN3_MgdCarbon_noncrop <- get_data(all_data, "L2231.LN3_MgdCarbon_noncrop")

    # ===================================================

    # Produce outputs
    create_xml("land_input_3_IRR.xml") %>%
      add_logit_tables_xml(L2231.LN3_Logit, "LN3_Logit") %>%
      add_xml_data(L2231.LN3_HistUnmgdAllocation, "LN3_HistUnmgdAllocation") %>%
      add_xml_data(L2231.LN3_UnmgdAllocation, "LN3_UnmgdAllocation") %>%
      add_node_equiv_xml("LandLeaf") %>%
      add_node_equiv_xml("carbon-calc") %>%
      add_xml_data(L2231.LN3_NoEmissCarbon, "LN3_NoEmissCarbon") %>%
      add_xml_data(L2231.LN3_NodeCarbon, "LN3_NodeCarbon") %>%
      add_xml_data(L2231.LN3_HistMgdAllocation_noncrop, "LN3_HistMgdAllocation") %>%
      add_xml_data(L2231.LN3_MgdAllocation_noncrop, "LN3_MgdAllocation") %>%
      add_xml_data(L2231.LN3_UnmgdCarbon, "LN3_UnmgdCarbon") %>%
      add_xml_data(L2231.LN3_MgdCarbon_noncrop, "LN3_MgdCarbon") %>%
      add_rename_landnode_xml() %>%
      add_precursors("L2231.LN3_Logit",
                     "L2231.LN3_HistUnmgdAllocation",
                     "L2231.LN3_UnmgdAllocation",
                     "L2231.LN3_NoEmissCarbon",
                     "L2231.LN3_NodeCarbon",
                     "L2231.LN3_HistMgdAllocation_noncrop",
                     "L2231.LN3_MgdAllocation_noncrop",
                     "L2231.LN3_UnmgdCarbon",
                     "L2231.LN3_MgdCarbon_noncrop") ->
      land_input_3_IRR.xml

    return_data(land_input_3_IRR.xml)
  } else {
    stop("Unknown command")
  }
}
