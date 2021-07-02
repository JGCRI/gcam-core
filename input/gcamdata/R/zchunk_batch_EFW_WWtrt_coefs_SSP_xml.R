# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_batch_EFW_WWtrt_coefs_SSP_xml
#'
#' Construct XML data structure for all the \code{EFW_WWtrt_coefs_[g]SSP[1-5].xml} files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:\code{EFW_WWtrt_coefs_SSP1.xml},
#' \code{EFW_WWtrt_coefs_SSP2.xml}, \code{EFW_WWtrt_coefs_SSP3.xml},
#' \code{EFW_WWtrt_coefs_SSP4.xml}, and \code{EFW_WWtrt_coefs_SSP5.xml}.
module_water_batch_EFW_WWtrt_coefs_SSP_xml <- function(command, ...) {

  SSP_NUMS <- 1:5

  if(command == driver.DECLARE_INPUTS) {
    return(c("L270.TechCoef_WWtrt_SSP"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "EFW_WWtrt_coefs_SSP1.xml",
             XML = "EFW_WWtrt_coefs_SSP2.xml",
             XML = "EFW_WWtrt_coefs_SSP3.xml",
             XML = "EFW_WWtrt_coefs_SSP4.xml",
             XML = "EFW_WWtrt_coefs_SSP5.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    EFW_WWtrt_coefs_SSP1.xml <- EFW_WWtrt_coefs_SSP2.xml <- EFW_WWtrt_coefs_SSP3.xml <-
      EFW_WWtrt_coefs_SSP4.xml <- EFW_WWtrt_coefs_SSP5.xml <- scenario <- NULL  # silence package check notes

    # Load required inputs
    L270.TechCoef_WWtrt_SSP <- get_data(all_data, "L270.TechCoef_WWtrt_SSP")

      for(ssp_num in SSP_NUMS) {
        ssp <- paste0("SSP", ssp_num)
        xmlfn <- paste0("EFW_WWtrt_coefs_", ssp, ".xml")
        L270.TechCoef_WWtrt <- filter(L270.TechCoef_WWtrt_SSP, scenario == ssp) %>%
          select(-scenario)

        # Produce output
        create_xml(xmlfn) %>%
          add_xml_data(L270.TechCoef_WWtrt, "TechCoef") %>%
          add_precursors("L270.TechCoef_WWtrt_SSP") ->
          x

        # ...and assign into environment
        assign(xmlfn, x)
      }

    return_data(EFW_WWtrt_coefs_SSP1.xml,
                EFW_WWtrt_coefs_SSP2.xml, EFW_WWtrt_coefs_SSP3.xml,
                EFW_WWtrt_coefs_SSP4.xml, EFW_WWtrt_coefs_SSP5.xml)
  } else {
    stop("Unknown command")
  }
}
