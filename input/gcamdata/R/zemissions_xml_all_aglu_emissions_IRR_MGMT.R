# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_all_aglu_emissions_IRR_MGMT_xml
#'
#' Construct XML data structure for \code{all_aglu_emissions_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{all_aglu_emissions_IRR_MGMT.xml}, \code{all_aglu_emissions_IRR_MGMT_MAC.xml}.
#' The corresponding file in the original data system was
#' \code{batch_all_aglu_emissions_IRR_MGMT.xml} (emissions XML).
module_emissions_all_aglu_emissions_IRR_MGMT_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L2112.AWBEmissions",
      "L2112.AGREmissions",
      "L211.AnEmissions",
      "L211.AnNH3Emissions",
      "L252.MAC_an",
      "L252.MAC_an_tc_average",
      "L2112.AGRBio",
      "L2112.AWB_BCOC_EmissCoeff",
      "L2112.nonghg_max_reduction",
      "L2112.nonghg_steepness",
      "L252.AgMAC",
      "L252.AgMAC_tc_average")

  MODULE_OUTPUTS <-
    c(XML = "all_aglu_emissions_IRR_MGMT.xml",
      XML = "all_aglu_emissions_IRR_MGMT_MAC.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    tech.change <- tech.change.year <- bio_N20_coef <- compVal <- bio_N2O_coef<- NULL # Silence package checks

    # ===================================================
    # Rename the tibble column names to match the header names.
    L2112.AGRBio <- rename(L2112.AGRBio, emiss.coef = bio_N2O_coef)

    # Produce outputs
    create_xml("all_aglu_emissions_IRR_MGMT.xml") %>%
      add_xml_data(L2112.AGRBio, "OutputEmissCoeffAg") %>%
      add_xml_data(L2112.AWB_BCOC_EmissCoeff, "OutputEmissCoeffAg") %>%
      add_xml_data(L2112.nonghg_max_reduction, "AgGDPCtrlMax") %>%
      add_xml_data(L2112.nonghg_steepness, "AgGDPCtrlSteep") %>%
      add_xml_data(L211.AnEmissions, "OutputEmissions") %>%
      add_xml_data(L211.AnNH3Emissions, "OutputEmissions") %>%
      add_xml_data(L2112.AWBEmissions, "OutputEmissionsAg") %>%
      add_xml_data(L2112.AGREmissions, "OutputEmissionsAg") %>%
      add_precursors("L2112.AWBEmissions",
                     "L2112.AGREmissions",
                     "L211.AnEmissions",
                     "L211.AnNH3Emissions",
                     "L2112.AGRBio",
                     "L2112.AWB_BCOC_EmissCoeff",
                     "L2112.nonghg_max_reduction",
                     "L2112.nonghg_steepness") ->
        all_aglu_emissions_IRR_MGMT.xml

    create_xml("all_aglu_emissions_IRR_MGMT_MAC.xml") %>%
      add_xml_data(L252.AgMAC, "AgMAC") %>%
      add_xml_data(L252.AgMAC_tc_average, "AgMACTC") %>%
      add_xml_data(L252.MAC_an, "MAC") %>%
      add_xml_data(L252.MAC_an_tc_average, "MACTC") %>%
      add_precursors("L252.MAC_an",
                     "L252.MAC_an_tc_average",
                     "L252.AgMAC",
                     "L252.AgMAC_tc_average") ->
      all_aglu_emissions_IRR_MGMT_MAC.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}

