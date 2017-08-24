#' module_emissions_batch_MACC_TC_SSP_IRR.xml
#'
#' Construct XML data structure for all the \code{MACC_TC_SSP_IRR.xml} files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{MACC_TC_SSP1_IRR.xml}, \code{MACC_TC_SSP2_IRR.xml}, and \code{MACC_TC_SSP5_IRR.xml}.
module_emissions_batch_MACC_TC_SSP_IRR.xml <- function(command, ...) {

  SSP_NUMS <- c(1,2,5)

  if(command == driver.DECLARE_INPUTS) {
    return(c(paste0("L2521.MAC_Ag_TC_SSP", SSP_NUMS),
             paste0("L2521.MAC_An_TC_SSP", SSP_NUMS)))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "MACC_TC_SSP1_IRR.xml",
             XML = "MACC_TC_SSP2_IRR.xml",
             XML = "MACC_TC_SSP5_IRR.xml"))
  } else if(command == driver.MAKE) {

    # silence package checks
    MACC_TC_SSP1_IRR.xml <- MACC_TC_SSP2_IRR.xml <- MACC_TC_SSP5_IRR.xml <- NULL

    all_data <- list(...)[[1]]

    for(i in SSP_NUMS) {
      # Names of inputs and outputs
      ag_MAC <- paste0("L2521.MAC_Ag_TC_SSP", i)
      an_MAC <- paste0("L2521.MAC_An_TC_SSP", i)
      xmlfn <- paste0("MACC_TC_SSP", i, "_IRR.xml")

      # Load required inputs
      L2521.MAC_Ag_TC_SSP <- get_data(all_data, ag_MAC)
      L2521.MAC_An_TC_SSP <- get_data(all_data, an_MAC)

      # Produce outputs
      create_xml(xmlfn) %>%
        add_xml_data(L2521.MAC_Ag_TC_SSP, "AgMACTC") %>%
        add_xml_data(L2521.MAC_An_TC_SSP, "MACTC") %>%
        add_precursors(ag_MAC, an_MAC) ->
        xml_obj

      # Assign output to output name
      assign(xmlfn, xml_obj)
    }

    return_data(MACC_TC_SSP1_IRR.xml, MACC_TC_SSP2_IRR.xml,
                MACC_TC_SSP5_IRR.xml)
  } else {
    stop("Unknown command")
  }
}
