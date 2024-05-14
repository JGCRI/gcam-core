# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_land_input_4_IRR_MGMT_xml
#'
#' Construct XML data structure for \code{land_input_4_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{land_input_4_IRR_MGMT.xml}. The corresponding file in the
#' original data system was \code{batch_land_input_4_IRR_MGMT.xml.R} (aglu XML).
module_aglu_land_input_4_IRR_MGMT_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L2242.LN4_Logit",
      "L2242.LN4_NodeGhostShare",
      "L2242.LN4_NodeIsGhostShareRel")

  MODULE_OUTPUTS <-
    c(XML = "land_input_4_IRR_MGMT.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # ===================================================


    # Produce outputs
    create_xml("land_input_4_IRR_MGMT.xml") %>%
      add_logit_tables_xml(L2242.LN4_Logit, "LN4_Logit") %>%
      add_xml_data(L2242.LN4_NodeGhostShare, "LN4_NodeGhostShare") %>%
      add_xml_data(L2242.LN4_NodeIsGhostShareRel, "LN4_NodeIsGhostShareRel") %>%
      add_rename_landnode_xml() %>%
      add_precursors("L2242.LN4_Logit",
                     "L2242.LN4_NodeGhostShare",
                     "L2242.LN4_NodeIsGhostShareRel") ->
      land_input_4_IRR_MGMT.xml


    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
