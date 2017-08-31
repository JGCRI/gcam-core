#' module_emissions_batch_all_unmgd_emissions.xml
#'
#' Construct XML data structure for \code{all_unmgd_emissions.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{all_unmgd_emissions.xml}. The corresponding file in the
#' original data system was \code{batch_all_unmgd_emissions.xml} (emissions XML).
module_emissions_batch_all_unmgd_emissions.xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L212.AgSupplySector",
             "L212.AgSupplySubsector",
             "L212.ItemName",
             "L212.GRASSEmissions",
             "L212.FORESTEmissions_FF",
             "L212.FORESTEmissions_D",
             "L212.GRASSEmissionsFactors_BCOC",
             "L212.FORESTEmissionsFactors_BCOC_FF",
             "L212.FORESTEmissionsFactors_BCOC_D",
             "L212.FORESTEmissionsFactors_future"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "all_unmgd_emissions.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L212.AgSupplySector <- get_data(all_data, "L212.AgSupplySector")
    L212.AgSupplySubsector <- get_data(all_data, "L212.AgSupplySubsector")
    L212.ItemName <- get_data(all_data, "L212.ItemName")
    L212.GRASSEmissions <- get_data(all_data, "L212.GRASSEmissions")
    L212.FORESTEmissions_FF <- get_data(all_data, "L212.FORESTEmissions_FF")
    L212.FORESTEmissions_D <- get_data(all_data, "L212.FORESTEmissions_D")
    L212.GRASSEmissionsFactors_BCOC <- get_data(all_data, "L212.GRASSEmissionsFactors_BCOC")
    L212.FORESTEmissionsFactors_BCOC_FF <- get_data(all_data, "L212.FORESTEmissionsFactors_BCOC_FF")
    L212.FORESTEmissionsFactors_BCOC_D <- get_data(all_data, "L212.FORESTEmissionsFactors_BCOC_D")
    L212.FORESTEmissionsFactors_future <- get_data(all_data, "L212.FORESTEmissionsFactors_future")

    # ===================================================

    # Produce outputs
    create_xml("all_unmgd_emissions.xml") %>%
      add_logit_tables_xml(L212.AgSupplySector, "AgSupplySector") %>%
      add_logit_tables_xml(L212.AgSupplySubsector, "AgSupplySubsector") %>%
      add_xml_data(L212.ItemName, "ItemName") %>%
      add_xml_data(L212.GRASSEmissions, "InputEmissionsUnmgd") %>%
      add_xml_data(L212.FORESTEmissions_FF, "InputEmissionsUnmgd") %>%
      add_xml_data(L212.FORESTEmissions_D, "OutputEmissionsUnmgd") %>%
      add_xml_data(L212.GRASSEmissionsFactors_BCOC, "InputEmFactUnmgd") %>%
      add_xml_data(L212.FORESTEmissionsFactors_BCOC_FF, "InputEmFactUnmgd") %>%
      add_xml_data(L212.FORESTEmissionsFactors_BCOC_D, "OutputEmFactUnmgd") %>%
      add_xml_data(L212.FORESTEmissionsFactors_future, "OutputEmFactUnmgd") %>%
      add_precursors("L212.AgSupplySector",
                     "L212.AgSupplySubsector",
                     "L212.ItemName",
                     "L212.GRASSEmissions",
                     "L212.FORESTEmissions_FF",
                     "L212.FORESTEmissions_D",
                     "L212.GRASSEmissionsFactors_BCOC",
                     "L212.FORESTEmissionsFactors_BCOC_FF",
                     "L212.FORESTEmissionsFactors_BCOC_D",
                     "L212.FORESTEmissionsFactors_future") ->
      all_unmgd_emissions.xml

    return_data(all_unmgd_emissions.xml)
  } else {
    stop("Unknown command")
  }
}
