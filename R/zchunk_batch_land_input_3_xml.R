#' module_aglu_batch_land_input_3_xml
#'
#' Construct XML data structure for \code{land_input_3.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{land_input_3.xml}. The corresponding file in the
#' original data system was \code{batch_land_input_3.xml.R} (aglu XML).
module_aglu_batch_land_input_3_xml_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L223.LN3_LeafGhostShare",
              "L223.LN3_LeafIsGhostShareRel",
              "L223.LN3_HistUnmgdAllocation",
              "L223.LN3_UnmgdAllocation",
              "L223.NodeEquiv",
              "L223.LN3_NoEmissCarbon",
              "L223.LN3_NodeCarbon",
              "L223.LN3_HistMgdAllocation_noncrop",
              "L223.LN3_MgdAllocation_noncrop",
              "L223.LN3_HistMgdAllocation_crop",
              "L223.LN3_MgdAllocation_crop",
              "L223.LN3_HistMgdAllocation_bio",
              "L223.LN3_MgdAllocation_bio",
              "L223.LN3_UnmgdCarbon",
              "L223.LN3_MgdCarbon_noncrop",
              "L223.LN3_MgdCarbon_crop",
              "L223.LN3_MgdCarbon_bio"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "land_input_3.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L223.LN3_LeafGhostShare <- get_data(all_data, "L223.LN3_LeafGhostShare")
    L223.LN3_LeafIsGhostShareRel <- get_data(all_data, "L223.LN3_LeafIsGhostShareRel")
    L223.LN3_HistUnmgdAllocation <- get_data(all_data, "L223.LN3_HistUnmgdAllocation")
    L223.LN3_UnmgdAllocation <- get_data(all_data, "L223.LN3_UnmgdAllocation")
    L223.NodeEquiv <- get_data(all_data, "L223.NodeEquiv")
    L223.LN3_NoEmissCarbon <- get_data(all_data, "L223.LN3_NoEmissCarbon")
    L223.LN3_NodeCarbon <- get_data(all_data, "L223.LN3_NodeCarbon")
    L223.LN3_HistMgdAllocation_noncrop <- get_data(all_data, "L223.LN3_HistMgdAllocation_noncrop")
    L223.LN3_MgdAllocation_noncrop <- get_data(all_data, "L223.LN3_MgdAllocation_noncrop")
    L223.LN3_HistMgdAllocation_crop <- get_data(all_data, "L223.LN3_HistMgdAllocation_crop")
    L223.LN3_MgdAllocation_crop <- get_data(all_data, "L223.LN3_MgdAllocation_crop")
    L223.LN3_HistMgdAllocation_bio <- get_data(all_data, "L223.LN3_HistMgdAllocation_bio")
    L223.LN3_MgdAllocation_bio <- get_data(all_data, "L223.LN3_MgdAllocation_bio")
    L223.LN3_UnmgdCarbon <- get_data(all_data, "L223.LN3_UnmgdCarbon")
    L223.LN3_MgdCarbon_noncrop <- get_data(all_data, "L223.LN3_MgdCarbon_noncrop")
    L223.LN3_MgdCarbon_crop <- get_data(all_data, "L223.LN3_MgdCarbon_crop")
    L223.LN3_MgdCarbon_bio <- get_data(all_data, "L223.LN3_MgdCarbon_bio")

    # ===================================================

    # Produce outputs
    create_xml("land_input_3.xml") %>%
      add_xml_data(L223.LN3_LeafGhostShare,"LN3_LeafGhostShare") %>%
      add_xml_data(L223.LN3_LeafIsGhostShareRel,"LN3_LeafIsGhostShareRel") %>%
      add_xml_data(L223.LN3_HistUnmgdAllocation,"LN3_HistUnmgdAllocation") %>%
      add_xml_data(L223.LN3_UnmgdAllocation,"LN3_UnmgdAllocation") %>%
      add_xml_data(L223.NodeEquiv,"EQUIV_TABLE") %>%
      add_xml_data(L223.LN3_NoEmissCarbon,"LN3_NoEmissCarbon") %>%
      add_xml_data(L223.LN3_NodeCarbon,"LN3_NodeCarbon") %>%
      add_xml_data(L223.LN3_HistMgdAllocation_noncrop,"LN3_HistMgdAllocation") %>%
      add_xml_data(L223.LN3_MgdAllocation_noncrop,"LN3_MgdAllocation") %>%
      add_xml_data(L223.LN3_HistMgdAllocation_crop,"LN3_HistMgdAllocation") %>%
      add_xml_data(L223.LN3_MgdAllocation_crop,"LN3_MgdAllocation") %>%
      add_xml_data(L223.LN3_HistMgdAllocation_bio,"LN3_HistMgdAllocation") %>%
      add_xml_data(L223.LN3_MgdAllocation_bio,"LN3_MgdAllocation") %>%
      add_xml_data(L223.LN3_UnmgdCarbon,"LN3_UnmgdCarbon") %>%
      add_xml_data(L223.LN3_MgdCarbon_noncrop,"LN3_MgdCarbon") %>%
      add_xml_data(L223.LN3_MgdCarbon_crop,"LN3_MgdCarbon") %>%
      add_xml_data(L223.LN3_MgdCarbon_bio,"LN3_MgdCarbon") %>%
      add_precursors("L223.LN3_LeafGhostShare", "L223.LN3_LeafIsGhostShareRel", "L223.LN3_HistUnmgdAllocation", "L223.LN3_UnmgdAllocation", "L223.NodeEquiv", "L223.LN3_NoEmissCarbon", "L223.LN3_NodeCarbon", "L223.LN3_HistMgdAllocation_noncrop", "L223.LN3_MgdAllocation_noncrop", "L223.LN3_HistMgdAllocation_crop", "L223.LN3_MgdAllocation_crop", "L223.LN3_HistMgdAllocation_bio", "L223.LN3_MgdAllocation_bio", "L223.LN3_UnmgdCarbon", "L223.LN3_MgdCarbon_noncrop", "L223.LN3_MgdCarbon_crop", "L223.LN3_MgdCarbon_bio") ->
      land_input_3.xml

    return_data(land_input_3.xml)
  } else {
    stop("Unknown command")
  }
}
