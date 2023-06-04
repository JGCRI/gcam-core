# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_all_protected_unmgd_emissions_xml
#'
#' Construct XML data structure for \code{all_protected_unmgd_emissions.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{all_protected_unmgd_emissions.xml}. The corresponding file in the
#' original data system was \code{batch_all_protected_unmgd_emissions.xml} (emissions XML).
module_emissions_all_protected_unmgd_emissions_xml <- function(command, ...) {

  MODULE_INPUTS <- c("L212.ItemName",
                   "L212.ItemName_prot",
                   "L212.GRASSEmissions_prot",
                   "L212.GRASSEmissions_noprot",
                   "L212.FORESTEmissions_FF_prot",
                   "L212.FORESTEmissions_FF_noprot",
                   "L212.FORESTEmissions_D_prot",
                   "L212.FORESTEmissions_D_noprot",
                   "L212.FORESTEmissionsFactors_future",
                   "L212.FORESTEmissionsFactors_future_prot")

  MODULE_OUTPUTS <-
    c(XML = "all_protected_unmgd_emissions.xml")

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
    create_xml("all_protected_unmgd_emissions.xml") %>%
      add_xml_data(L212.ItemName_prot, "ItemName") %>%
      add_xml_data(L212.ItemName, "ItemName") %>%
      add_xml_data(L212.GRASSEmissions_prot, "InputEmissionsUnmgd") %>%
      add_xml_data(L212.GRASSEmissions_noprot, "InputEmissionsUnmgd") %>%
      add_xml_data(L212.FORESTEmissions_FF_prot, "InputEmissionsUnmgd") %>%
      add_xml_data(L212.FORESTEmissions_FF_noprot, "InputEmissionsUnmgd") %>%
      add_xml_data(L212.FORESTEmissions_D_prot, "OutputEmissionsUnmgd") %>%
      add_xml_data(L212.FORESTEmissions_D_noprot, "OutputEmissionsUnmgd") %>%
      add_xml_data(L212.FORESTEmissionsFactors_future, "OutputEmFactUnmgd") %>%
      add_xml_data(L212.FORESTEmissionsFactors_future_prot, "OutputEmFactUnmgd") ->
      all_protected_unmgd_emissions.xml
    # need to call add_precursors indirectly to ensure input_names gets "unlisted"
    all_protected_unmgd_emissions.xml <- do.call("add_precursors", c(list(all_protected_unmgd_emissions.xml), MODULE_INPUTS))

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
