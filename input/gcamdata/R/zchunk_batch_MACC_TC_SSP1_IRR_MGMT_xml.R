#' module_emissions_batch_MACC_TC_SSP1_IRR_MGMT_xml
#'
#' Construct XML data structure for \code{MACC_TC_SSP1_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{MACC_TC_SSP1_IRR_MGMT.xml}. The corresponding file in the
#' original data system was \code{batch_MACC_TC_SSP1_IRR_MGMT_xml.R} (emissions XML).
module_emissions_batch_MACC_TC_SSP1_IRR_MGMT_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2522.MAC_Ag_TC_SSP1",
              "L2522.MAC_An_TC_SSP1"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "MACC_TC_SSP1_IRR_MGMT.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2522.MAC_Ag_TC_SSP1 <- get_data(all_data, "L2522.MAC_Ag_TC_SSP1")
    L2522.MAC_An_TC_SSP1 <- get_data(all_data, "L2522.MAC_An_TC_SSP1")

    # ===================================================

    # Produce outputs
    create_xml("MACC_TC_SSP1_IRR_MGMT.xml") %>%
      add_xml_data(L2522.MAC_Ag_TC_SSP1, "AgMACTC") %>%
      add_xml_data(L2522.MAC_An_TC_SSP1, "MACTC") %>%
      add_precursors("L2522.MAC_Ag_TC_SSP1", "L2522.MAC_An_TC_SSP1") ->
      MACC_TC_SSP1_IRR_MGMT.xml

    return_data(MACC_TC_SSP1_IRR_MGMT.xml)
  } else {
    stop("Unknown command")
  }
}
