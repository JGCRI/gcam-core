# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_batch_MACC_TC_SSP_xml
#'
#' Construct XML data structure for all the \code{MACC_TC_SSP.xml} files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{MACC_TC_SSP1.xml}, \code{MACC_TC_SSP2.xml}, and \code{MACC_TC_SSP5.xml}.
module_emissions_batch_MACC_TC_SSP_xml <- function(command, ...) {

  SSP_NUMS <- c(1, 2, 5)

  if(command == driver.DECLARE_INPUTS) {
    return(c(paste0("L252.MAC_Ag_TC_SSP", SSP_NUMS),
             paste0("L252.MAC_An_TC_SSP", SSP_NUMS),
             paste0("L252.MAC_prc_TC_SSP", SSP_NUMS),
             paste0("L252.MAC_res_TC_SSP", SSP_NUMS)))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "MACC_TC_SSP1.xml",
             XML = "MACC_TC_SSP2.xml",
             XML = "MACC_TC_SSP5.xml"))
  } else if(command == driver.MAKE) {

    # silence package checks
    MACC_TC_SSP1.xml <- MACC_TC_SSP2.xml <- MACC_TC_SSP5.xml <- NULL

    all_data <- list(...)[[1]]

    for (i in SSP_NUMS) {
      # Names of inputs and outputs
      ag_MAC <- paste0("L252.MAC_Ag_TC_SSP", i)
      an_MAC <- paste0("L252.MAC_An_TC_SSP", i)
      prc_MAC <- paste0("L252.MAC_prc_TC_SSP", i)
      res_MAC <- paste0("L252.MAC_res_TC_SSP", i)
      xmlfn <- paste0("MACC_TC_SSP", i, ".xml")

      # Load required inputs
      L252.MAC_Ag_TC_SSP <- get_data(all_data, ag_MAC)
      L252.MAC_An_TC_SSP <- get_data(all_data, an_MAC)
      L252.MAC_prc_TC_SSP <- get_data(all_data, prc_MAC)
      L252.MAC_res_TC_SSP <- get_data(all_data, res_MAC)

      # Produce outputs
      create_xml(xmlfn) %>%
        add_xml_data(L252.MAC_Ag_TC_SSP, "AgMACTC") %>%
        add_xml_data(L252.MAC_An_TC_SSP, "MACTC") %>%
        add_xml_data(L252.MAC_prc_TC_SSP, "MACTC") %>%
        add_xml_data(L252.MAC_res_TC_SSP, "ResMACTC") %>%
        add_precursors(ag_MAC, an_MAC, prc_MAC, res_MAC) ->
        xml_obj

      # Assign output to output name
      assign(xmlfn, xml_obj)
    }

    return_data(MACC_TC_SSP1.xml, MACC_TC_SSP2.xml,
                MACC_TC_SSP5.xml)
  } else {
    stop("Unknown command")
  }
}
