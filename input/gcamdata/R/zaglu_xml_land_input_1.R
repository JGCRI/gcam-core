# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_land_input_1_xml
#'
#' Construct XML data structure for \code{land_input_1.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{land_input_1.xml}. The corresponding file in the
#' original data system was \code{batch_land_input_1.xml.R} (aglu XML).
module_aglu_land_input_1_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L221.LN0_Logit",
      "L221.LN0_Land",
      "L221.LN0_SoilTimeScale",
      "L221.LN1_ValueLogit",
      "L221.LN1_HistUnmgdAllocation",
      "L221.LN1_UnmgdAllocation",
      "L221.LN1_UnmgdCarbon")

  MODULE_OUTPUTS <-
    c(XML = "land_input_1.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # ===================================================

    # Produce outputs ----
    create_xml("land_input_1.xml") %>%
      add_logit_tables_xml(L221.LN0_Logit, "LN0_Logit") %>%
      add_xml_data(L221.LN0_Land, "LN0_Land") %>%
      add_xml_data(L221.LN0_SoilTimeScale, "LN0_SoilTimeScale") %>%
      add_logit_tables_xml(L221.LN1_ValueLogit, "LN1_ValueLogit", "LN1_Logit") %>%
      add_xml_data(L221.LN1_HistUnmgdAllocation, "LN1_HistUnmgdAllocation") %>%
      add_xml_data(L221.LN1_UnmgdAllocation, "LN1_UnmgdAllocation") %>%
      add_xml_data(L221.LN1_UnmgdCarbon, "LN1_UnmgdCarbon") %>%
      add_rename_landnode_xml() %>%
      add_precursors("L221.LN0_Logit", "L221.LN0_Land", "L221.LN0_SoilTimeScale", "L221.LN1_ValueLogit", "L221.LN1_HistUnmgdAllocation", "L221.LN1_UnmgdAllocation", "L221.LN1_UnmgdCarbon") ->
      land_input_1.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
