# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_batch_all_aglu_emissions_IRR_MGMT_xml
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
module_emissions_batch_all_aglu_emissions_IRR_MGMT_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2112.AWBEmissions",
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
              "L252.AgMAC_tc_average"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "all_aglu_emissions_IRR_MGMT.xml",
             XML = "all_aglu_emissions_IRR_MGMT_MAC.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2112.AWBEmissions <- get_data(all_data, "L2112.AWBEmissions")
    L2112.AGREmissions <- get_data(all_data, "L2112.AGREmissions")
    L211.AnEmissions <- get_data(all_data, "L211.AnEmissions")
    L211.AnNH3Emissions <- get_data(all_data, "L211.AnNH3Emissions")
    L252.MAC_an <- get_data(all_data, "L252.MAC_an")
    L252.MAC_an_tc_average <- get_data(all_data, "L252.MAC_an_tc_average")
    L2112.AGRBio <- get_data(all_data, "L2112.AGRBio")
    L2112.AWB_BCOC_EmissCoeff <- get_data(all_data, "L2112.AWB_BCOC_EmissCoeff")
    L2112.nonghg_max_reduction <- get_data(all_data, "L2112.nonghg_max_reduction")
    L2112.nonghg_steepness <- get_data(all_data, "L2112.nonghg_steepness")
    L252.AgMAC <- get_data(all_data, "L252.AgMAC")
    L252.AgMAC_tc_average <- get_data(all_data, "L252.AgMAC_tc_average")

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

    return_data(all_aglu_emissions_IRR_MGMT.xml,
                all_aglu_emissions_IRR_MGMT_MAC.xml)
  } else {
    stop("Unknown command")
  }
}

