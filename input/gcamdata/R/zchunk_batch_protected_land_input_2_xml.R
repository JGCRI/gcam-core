# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_batch_protected_land_input_2_xml
#'
#' Construct XML data structure for \code{protected_land_input_2.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{protected_land_input_2.xml}. The corresponding file in the
#' original data system was \code{batch_protected_land_input_2.xml.R} (aglu XML).
module_aglu_batch_protected_land_input_2_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L222.LN2_HistUnmgdAllocation_noprot",
              "L222.LN2_UnmgdAllocation_noprot",
              "L222.LN2_UnmgdCarbon",
              "L222.LN1_HistUnmgdAllocation_prot",
              "L222.LN1_UnmgdAllocation_prot",
              "L222.LN1_UnmgdCarbon_prot",
              "L222.LN1_Logit_prot"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "protected_land_input_2.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L222.LN2_HistUnmgdAllocation_noprot <- get_data(all_data, "L222.LN2_HistUnmgdAllocation_noprot")
    L222.LN2_UnmgdAllocation_noprot <- get_data(all_data, "L222.LN2_UnmgdAllocation_noprot")
    L222.LN2_UnmgdCarbon <- get_data(all_data, "L222.LN2_UnmgdCarbon")
    L222.LN1_HistUnmgdAllocation_prot <- get_data(all_data, "L222.LN1_HistUnmgdAllocation_prot")
    L222.LN1_UnmgdAllocation_prot <- get_data(all_data, "L222.LN1_UnmgdAllocation_prot")
    L222.LN1_UnmgdCarbon_prot <- get_data(all_data, "L222.LN1_UnmgdCarbon_prot")
    L222.LN1_Logit_prot <- get_data(all_data, "L222.LN1_Logit_prot")

    # ===================================================

    # Produce outputs
    create_xml("protected_land_input_2.xml") %>%
      add_xml_data(L222.LN2_HistUnmgdAllocation_noprot, "LN2_HistUnmgdAllocation") %>%
      add_xml_data(L222.LN2_UnmgdAllocation_noprot, "LN2_UnmgdAllocation") %>%
      add_xml_data(L222.LN2_UnmgdCarbon, "LN2_UnmgdCarbon") %>%
      add_logit_tables_xml(L222.LN1_Logit_prot, "LN1_ValueLogit", "LN1_Logit") %>%
      add_xml_data(L222.LN1_HistUnmgdAllocation_prot, "LN1_HistUnmgdAllocation") %>%
      add_xml_data(L222.LN1_UnmgdAllocation_prot, "LN1_UnmgdAllocation") %>%
      add_xml_data(L222.LN1_UnmgdCarbon_prot, "LN1_UnmgdCarbon") %>%
      add_rename_landnode_xml() %>%
      add_precursors("L222.LN2_HistUnmgdAllocation_noprot", "L222.LN2_UnmgdAllocation_noprot", "L222.LN2_UnmgdCarbon", "L222.LN1_HistUnmgdAllocation_prot", "L222.LN1_UnmgdAllocation_prot", "L222.LN1_UnmgdCarbon_prot", "L222.LN1_Logit_prot") ->
      protected_land_input_2.xml

    return_data(protected_land_input_2.xml)
  } else {
    stop("Unknown command")
  }
}
