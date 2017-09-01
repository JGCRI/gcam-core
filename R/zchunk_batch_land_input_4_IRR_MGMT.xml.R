#' module_aglu_batch_land_input_4_IRR_MGMT.xml
#'
#' Construct XML data structure for \code{land_input_4_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{land_input_4_IRR_MGMT.xml}. The corresponding file in the
#' original data system was \code{batch_land_input_4_IRR_MGMT.xml.R} (aglu XML).
module_aglu_batch_land_input_4_IRR_MGMT.xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2242.LN4_Logit",
             "L2242.LN4_NodeGhostShare",
             "L2242.LN4_NodeIsGhostShareRel"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "land_input_4_IRR_MGMT.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2242.LN4_Logit <- get_data(all_data, "L2242.LN4_Logit")
    L2242.LN4_NodeGhostShare <- get_data(all_data, "L2242.LN4_NodeGhostShare")
    L2242.LN4_NodeIsGhostShareRel <- get_data(all_data, "L2242.LN4_NodeIsGhostShareRel")

    # ===================================================

    # Produce outputs
    create_xml("land_input_4_IRR_MGMT.xml") %>%
      add_logit_tables_xml(L2242.LN4_Logit,"LN4_Logit") %>%
      add_xml_data(L2242.LN4_NodeGhostShare,"LN4_NodeGhostShare") %>%
      add_xml_data(L2242.LN4_NodeIsGhostShareRel,"LN4_NodeIsGhostShareRel") %>%
      add_rename_landnode_xml() %>%
      add_precursors("L2242.LN4_Logit", "L2242.LN4_NodeGhostShare", "L2242.LN4_NodeIsGhostShareRel") ->
      land_input_4_IRR_MGMT.xml

    return_data(land_input_4_IRR_MGMT.xml)
  } else {
    stop("Unknown command")
  }
}
