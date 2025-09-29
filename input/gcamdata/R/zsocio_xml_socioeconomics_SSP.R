# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_SSP_xml
#'
#' Construct XML data structure for all the \code{socioeconomics_SSP[1-5].xml} files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{socioeconomics_SSP1.xml},
#' \code{socioeconomics_SSP2.xml}, \code{socioeconomics_SSP3.xml},
#' \code{socioeconomics_SSP4.xml}, and \code{socioeconomics_SSP5.xml}.
module_socio_SSP_xml <- function(command, ...) {

  SSP_NUMS <- 1:5

  MODULE_INPUTS <-
    c("L201.GDP_Scen",
      "L201.Pop_Scen",
      "L201.TotalFactorProductivity_Scen",
      "L201.LaborForceShare_Scen",
      "L201.PPPConvert")

  MODULE_OUTPUTS <- setNames(
    c(paste0("socioeconomics_SSP", SSP_NUMS, ".xml"),
      "socioeconomics_CORE.xml"),
    rep("XML", 6)
  )

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    socioeconomics_SSP1.xml <-
      socioeconomics_SSP2.xml <- socioeconomics_SSP3.xml <- socioeconomics_SSP4.xml <-
      socioeconomics_SSP5.xml <- socioeconomics_CORE.xml <- NULL  # silence package check notes

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    for(ssp in SSP_NUMS) {
      ssp_name <- paste0("SSP", ssp)
      xmlfn <- paste0("socioeconomics_", ssp_name, ".xml")

      # Produce output
      create_xml(xmlfn) %>%
        add_xml_data(L201.Pop_Scen %>% filter(scenario == ssp_name) %>% select(-scenario), "Pop") %>%
        add_xml_data(L201.GDP_Scen %>% filter(scenario == ssp_name) %>% select(-scenario), "GDP") %>%
        add_xml_data(L201.TotalFactorProductivity_Scen %>%
                       filter(scenario == ssp_name) %>% select(-scenario), "TotalFactorProductivity") %>%
        add_xml_data(L201.LaborForceShare_Scen %>%
                       filter(scenario == ssp_name) %>% select(-scenario), "LaborForceShare") %>%
        add_xml_data(L201.PPPConvert, "PPPConvert") %>%
        add_precursors(MODULE_INPUTS) ->
        x

      # ...and assign into environment
      assign(xmlfn, x)

      # CORE is SSP2 however the TFP will be different
      if(ssp == 2) {
        xmlfn <- "socioeconomics_CORE.xml"
        create_xml(xmlfn) %>%
          add_xml_data(L201.Pop_Scen %>% filter(scenario == ssp_name) %>% select(-scenario), "Pop") %>%
          add_xml_data(L201.GDP_Scen %>% filter(scenario == ssp_name) %>% select(-scenario), "GDP") %>%
          # this is what is different for CORE
          add_xml_data(L201.TotalFactorProductivity_Scen %>%
                         filter(scenario == "CORE") %>% select(-scenario), "TotalFactorProductivity") %>%
          add_xml_data(L201.LaborForceShare_Scen %>%
                         filter(scenario == ssp_name) %>% select(-scenario), "LaborForceShare") %>%
          add_xml_data(L201.PPPConvert, "PPPConvert") %>%
          add_precursors(MODULE_INPUTS) ->
          x

        # ...and assign into environment
        assign(xmlfn, x)
      }
    }

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
