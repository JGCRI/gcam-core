# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_SSP_xml
#'
#' Construct XML data structure for all the \code{socioeconomics_[g]SSP[1-5].xml} files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{socioeconomics_gSSP1.xml}, \code{socioeconomics_gSSP2.xml}, \code{socioeconomics_gSSP3.xml},
#' \code{socioeconomics_gSSP4.xml}, \code{socioeconomics_gSSP5.xml}, \code{socioeconomics_SSP1.xml},
#' \code{socioeconomics_SSP2.xml}, \code{socioeconomics_SSP3.xml},
#' \code{socioeconomics_SSP4.xml}, and \code{socioeconomics_SSP5.xml}.
module_socio_SSP_xml <- function(command, ...) {

  SSP_NUMS <- 1:5

  if(command == driver.DECLARE_INPUTS) {
    return(c(paste0("L201.Pop_gSSP", SSP_NUMS),
             paste0("L201.Pop_SSP", SSP_NUMS),
             "L201.GDP_Scen",
             paste0("L201.TotalFactorProductivity_gSSP", SSP_NUMS),
             paste0("L201.TotalFactorProductivity_SSP", SSP_NUMS),
             "L201.PPPConvert",
             "L201.GDP_GCAM3",
             "L201.TotalFactorProductivity_GCAM3",
             "L201.Pop_GCAM3"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "socioeconomics_gSSP1.xml",
             XML = "socioeconomics_gSSP2.xml",
             XML = "socioeconomics_gSSP3.xml",
             XML = "socioeconomics_gSSP4.xml",
             XML = "socioeconomics_gSSP5.xml",
             XML = "socioeconomics_SSP1.xml",
             XML = "socioeconomics_SSP2.xml",
             XML = "socioeconomics_SSP3.xml",
             XML = "socioeconomics_SSP4.xml",
             XML = "socioeconomics_SSP5.xml",
             XML = "socioeconomics_GCAM3.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    socioeconomics_gSSP1.xml <- socioeconomics_gSSP2.xml <- socioeconomics_gSSP3.xml <-
      socioeconomics_gSSP4.xml <- socioeconomics_gSSP5.xml <- socioeconomics_SSP1.xml <-
      socioeconomics_SSP2.xml <- socioeconomics_SSP3.xml <- socioeconomics_SSP4.xml <-
      socioeconomics_SSP5.xml <- socioeconomics_GCAM3.xml <- NULL  # silence package check notes

    # Load required inputs
    L201.GDP_Scen <- get_data(all_data, "L201.GDP_Scen")
    L201.PPPConvert <- get_data(all_data, "L201.PPPConvert")
    L201.GDP_GCAM3 <- get_data(all_data, "L201.GDP_GCAM3")
    L201.TotalFactorProductivity_GCAM3 <- get_data(all_data, "L201.TotalFactorProductivity_GCAM3")
    L201.Pop_GCAM3 <- get_data(all_data, "L201.Pop_GCAM3")

    for(g in c("g", "")) {
      for(ssp in SSP_NUMS) {
        gssp <- paste0(g, "SSP", ssp)
        xmlfn <- paste0("socioeconomics_", gssp, ".xml")
        popname <- paste0("L201.Pop_", gssp)
        L201.Pop_SSP <- get_data(all_data, popname)
        laborname <- paste0("L201.TotalFactorProductivity_", gssp)
        L201.TotalFactorProductivity_SSP <- get_data(all_data, laborname) %>%
          filter(!is.na(productivity))

        # Produce output
        create_xml(xmlfn) %>%
          add_xml_data(L201.Pop_SSP, "Pop") %>%
          add_xml_data(L201.GDP_Scen %>% filter(scenario == gssp) %>% select(-scenario), "GDP") %>%
          add_xml_data(L201.TotalFactorProductivity_SSP, "TotalFactorProductivity") %>%
          add_xml_data(L201.PPPConvert, "PPPConvert") %>%
          add_precursors(popname, "L201.GDP_Scen", laborname, "L201.PPPConvert") ->
          x

        # ...and assign into environment
        assign(xmlfn, x)
      }
    }

    # GCAM3 xml
    create_xml("socioeconomics_GCAM3.xml") %>%
      add_xml_data(L201.Pop_GCAM3, "Pop") %>%
      add_xml_data(L201.GDP_GCAM3, "GDP") %>%
      add_xml_data(L201.TotalFactorProductivity_GCAM3, "TotalFactorProductivity") %>%
      add_xml_data(L201.PPPConvert, "PPPConvert") %>%
      add_precursors("L201.Pop_GCAM3", "L201.GDP_GCAM3", "L201.TotalFactorProductivity_GCAM3", "L201.PPPConvert") ->
      socioeconomics_GCAM3.xml


    return_data(socioeconomics_gSSP1.xml, socioeconomics_gSSP2.xml,
                socioeconomics_gSSP3.xml, socioeconomics_gSSP4.xml,
                socioeconomics_gSSP5.xml, socioeconomics_SSP1.xml,
                socioeconomics_SSP2.xml, socioeconomics_SSP3.xml,
                socioeconomics_SSP4.xml, socioeconomics_SSP5.xml,
                socioeconomics_GCAM3.xml)
  } else {
    stop("Unknown command")
  }
}
