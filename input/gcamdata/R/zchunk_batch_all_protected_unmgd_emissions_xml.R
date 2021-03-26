# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_batch_all_protected_unmgd_emissions_xml
#'
#' Construct XML data structure for \code{all_protected_unmgd_emissions.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{all_protected_unmgd_emissions.xml}. The corresponding file in the
#' original data system was \code{batch_all_protected_unmgd_emissions.xml} (emissions XML).
module_emissions_batch_all_protected_unmgd_emissions_xml <- function(command, ...) {
  input_names <- c("L212.ItemName",
                   "L212.ItemName_prot",
                   "L212.GRASSEmissions_prot",
                   "L212.GRASSEmissions_noprot",
                   "L212.FORESTEmissions_FF_prot",
                   "L212.FORESTEmissions_FF_noprot",
                   "L212.FORESTEmissions_D_prot",
                   "L212.FORESTEmissions_D_noprot",
                   "L212.FORESTEmissionsFactors_future",
                   "L212.FORESTEmissionsFactors_future_prot")

  if(command == driver.DECLARE_INPUTS) {
    return(input_names)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "all_protected_unmgd_emissions.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L212.ItemName <- get_data(all_data, "L212.ItemName")
    L212.ItemName_prot <- get_data(all_data, "L212.ItemName_prot")
    L212.GRASSEmissions_prot <- get_data(all_data, "L212.GRASSEmissions_prot")
    L212.GRASSEmissions_noprot <- get_data(all_data, "L212.GRASSEmissions_noprot")
    L212.FORESTEmissions_FF_prot <- get_data(all_data, "L212.FORESTEmissions_FF_prot")
    L212.FORESTEmissions_FF_noprot <- get_data(all_data, "L212.FORESTEmissions_FF_noprot")
    L212.FORESTEmissions_D_prot <- get_data(all_data, "L212.FORESTEmissions_D_prot")
    L212.FORESTEmissions_D_noprot <- get_data(all_data, "L212.FORESTEmissions_D_noprot")
    L212.FORESTEmissionsFactors_future <- get_data(all_data, "L212.FORESTEmissionsFactors_future")
    L212.FORESTEmissionsFactors_future_prot <- get_data(all_data, "L212.FORESTEmissionsFactors_future_prot")

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
    all_protected_unmgd_emissions.xml <- do.call("add_precursors", c(list(all_protected_unmgd_emissions.xml), input_names))

    return_data(all_protected_unmgd_emissions.xml)
  } else {
    stop("Unknown command")
  }
}
