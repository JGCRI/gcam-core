# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_batch_protected_land_input_3_xml
#'
#' Construct XML data structure for \code{protected_land_input_3.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{protected_land_input_3.xml}. The corresponding file in the
#' original data system was \code{batch_protected_land_input_3.xml.R} (aglu XML).
module_aglu_batch_protected_land_input_3_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2231.LN1_Logit_prot",
              "L2231.LN3_HistUnmgdAllocation_noprot",
              "L2231.LN3_UnmgdAllocation_noprot",
              "L2231.LN1_HistUnmgdAllocation_prot",
              "L2231.LN1_UnmgdAllocation_prot",
              "L2231.LN1_UnmgdCarbon_prot"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "protected_land_input_3.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      L2231.LN1_Logit_prot <- get_data(all_data, "L2231.LN1_Logit_prot")
      L2231.LN3_HistUnmgdAllocation_noprot <- get_data(all_data, "L2231.LN3_HistUnmgdAllocation_noprot")
      L2231.LN3_UnmgdAllocation_noprot <- get_data(all_data, "L2231.LN3_UnmgdAllocation_noprot")
      L2231.LN1_HistUnmgdAllocation_prot <- get_data(all_data, "L2231.LN1_HistUnmgdAllocation_prot")
      L2231.LN1_UnmgdAllocation_prot <- get_data(all_data, "L2231.LN1_UnmgdAllocation_prot")
      L2231.LN1_UnmgdCarbon_prot <- get_data(all_data, "L2231.LN1_UnmgdCarbon_prot")

    # ===================================================

    # Produce outputs
    create_xml("protected_land_input_3.xml") %>%
      add_logit_tables_xml(L2231.LN1_Logit_prot, "LN1_ValueLogit", "LN1_Logit") %>%
      add_xml_data(L2231.LN3_HistUnmgdAllocation_noprot, "LN3_HistUnmgdAllocation") %>%
      add_xml_data(L2231.LN3_UnmgdAllocation_noprot, "LN3_UnmgdAllocation") %>%
      add_xml_data(L2231.LN1_HistUnmgdAllocation_prot, "LN1_HistUnmgdAllocation") %>%
      add_xml_data(L2231.LN1_UnmgdAllocation_prot, "LN1_UnmgdAllocation") %>%
      add_xml_data(L2231.LN1_UnmgdCarbon_prot, "LN1_UnmgdCarbon") %>%
      add_rename_landnode_xml() %>%
      add_precursors("L2231.LN1_Logit_prot",
                     "L2231.LN3_HistUnmgdAllocation_noprot",
                     "L2231.LN3_UnmgdAllocation_noprot",
                     "L2231.LN1_HistUnmgdAllocation_prot",
                     "L2231.LN1_UnmgdAllocation_prot",
                     "L2231.LN1_UnmgdCarbon_prot") ->
      protected_land_input_3.xml

    return_data(protected_land_input_3.xml)
  } else {
    stop("Unknown command")
  }
}



