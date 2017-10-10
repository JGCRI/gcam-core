#' module_aglu_batch_land_input_4_IRR.xml
#'
#' Construct XML data structure for \code{land_input_4_IRR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{land_input_4_IRR.xml}. The corresponding file in the
#' original data system was \code{batch_land_input_4_IRR.xml.R} (aglu XML).
module_aglu_batch_land_input_4_IRR.xml_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2241.LN4_HistMgdAllocation_crop",
              "L2241.LN4_MgdAllocation_crop",
              "L2241.LN4_HistMgdAllocation_bio",
              "L2241.LN4_MgdAllocation_bio",
              "L2241.LN4_MgdCarbon_crop",
              "L2241.LN4_MgdCarbon_bio",
              "L2241.LN4_LeafGhostShare",
              "L2241.LN4_NodeGhostShare",
              "L2241.LN4_NodeIsGhostShareRel"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "land_input_4_IRR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2241.LN4_HistMgdAllocation_crop <- get_data(all_data, "L2241.LN4_HistMgdAllocation_crop")
    L2241.LN4_MgdAllocation_crop <- get_data(all_data, "L2241.LN4_MgdAllocation_crop")
    L2241.LN4_HistMgdAllocation_bio <- get_data(all_data, "L2241.LN4_HistMgdAllocation_bio")
    L2241.LN4_MgdAllocation_bio <- get_data(all_data, "L2241.LN4_MgdAllocation_bio")
    L2241.LN4_MgdCarbon_crop <- get_data(all_data, "L2241.LN4_MgdCarbon_crop")
    L2241.LN4_MgdCarbon_bio <- get_data(all_data, "L2241.LN4_MgdCarbon_bio")
    L2241.LN4_LeafGhostShare <- get_data(all_data, "L2241.LN4_LeafGhostShare")
    L2241.LN4_NodeGhostShare <- get_data(all_data, "L2241.LN4_NodeGhostShare")
    L2241.LN4_NodeIsGhostShareRel <- get_data(all_data, "L2241.LN4_NodeIsGhostShareRel")

    # ===================================================

    # Produce outputs
    create_xml("land_input_4_IRR.xml") %>%
      add_xml_data(L2241.LN4_HistMgdAllocation_crop,"LN4_HistMgdAllocation") %>%
      add_xml_data(L2241.LN4_MgdAllocation_crop,"LN4_MgdAllocation") %>%
      add_xml_data(L2241.LN4_HistMgdAllocation_bio,"LN4_HistMgdAllocation") %>%
      add_xml_data(L2241.LN4_MgdAllocation_bio,"LN4_MgdAllocation") %>%
      add_xml_data(L2241.LN4_MgdCarbon_crop,"LN4_MgdCarbon") %>%
      add_xml_data(L2241.LN4_MgdCarbon_bio,"LN4_MgdCarbon") %>%
      add_xml_data(L2241.LN4_LeafGhostShare,"LN4_LeafGhostShare") %>%
      add_xml_data(L2241.LN4_NodeGhostShare,"LN4_NodeGhostShare") %>%
      add_xml_data(L2241.LN4_NodeIsGhostShareRel,"LN4_NodeIsGhostShareRel") %>%
      add_precursors("L2241.LN4_HistMgdAllocation_crop", "L2241.LN4_MgdAllocation_crop", "L2241.LN4_HistMgdAllocation_bio", "L2241.LN4_MgdAllocation_bio", "L2241.LN4_MgdCarbon_crop", "L2241.LN4_MgdCarbon_bio", "L2241.LN4_LeafGhostShare", "L2241.LN4_NodeGhostShare", "L2241.LN4_NodeIsGhostShareRel") ->
      land_input_4_IRR.xml

    return_data(land_input_4_IRR.xml)
  } else {
    stop("Unknown command")
  }
}
