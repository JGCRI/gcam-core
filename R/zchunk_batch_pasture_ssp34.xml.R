#' module_aglu_batch_pasture_ssp34.xml
#'
#' Construct XML data structure for \code{pasture_ssp34.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{pasture_ssp34.xml}. The corresponding file in the
#' original data system was \code{batch_pasture_ssp34.xml.R} (aglu XML).
module_aglu_batch_pasture_ssp34.xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L242.LN2_UnmgdAllocation_SSP34",
              "L242.LN2_HistMgdAllocation_SSP34",
              "L242.LN2_MgdAllocation_SSP34"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "pasture_ssp34.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L242.LN2_UnmgdAllocation_SSP34 <- get_data(all_data, "L242.LN2_UnmgdAllocation_SSP34")
    L242.LN2_HistMgdAllocation_SSP34 <- get_data(all_data, "L242.LN2_HistMgdAllocation_SSP34")
    L242.LN2_MgdAllocation_SSP34 <- get_data(all_data, "L242.LN2_MgdAllocation_SSP34")

    # ===================================================

    # Produce outputs
    create_xml("pasture_ssp34.xml") %>%
      add_xml_data(L242.LN2_UnmgdAllocation_SSP34,"LN2_UnmgdAllocation") %>%
      add_xml_data(L242.LN2_HistMgdAllocation_SSP34,"LN2_HistMgdAllocation") %>%
      add_xml_data(L242.LN2_MgdAllocation_SSP34,"LN2_MgdAllocation") %>%
      add_rename_landnode_xml() %>%
      add_precursors("L242.LN2_UnmgdAllocation_SSP34", "L242.LN2_HistMgdAllocation_SSP34", "L242.LN2_MgdAllocation_SSP34") ->
      pasture_ssp34.xml

    return_data(pasture_ssp34.xml)
  } else {
    stop("Unknown command")
  }
}
