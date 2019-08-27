# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_batch_all_aglu_emissions_xml
#'
#' Construct XML data structure for \code{all_aglu_emissions.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{all_aglu_emissions.xml}. The corresponding file in the
#' original data system was \code{batch_all_aglu_emissions.xml} (emissions XML).
module_emissions_batch_all_aglu_emissions_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L211.AWBEmissions",
              "L211.AGREmissions",
              "L211.AnEmissions",
              "L211.AnNH3Emissions",
              "L211.AGRBio",
              "L211.AWB_BCOC_EmissCoeff",
              "L211.nonghg_max_reduction",
              "L211.nonghg_steepness",
              "L252.AgMAC",
              "L252.MAC_an"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "all_aglu_emissions.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    bio_N2O_coef <- NULL  # silence package check note

    # Load required inputs
    L211.AWBEmissions <- get_data(all_data, "L211.AWBEmissions")
    L211.AGREmissions <- get_data(all_data, "L211.AGREmissions")
    L211.AnEmissions <- get_data(all_data, "L211.AnEmissions")
    L211.AnNH3Emissions <- get_data(all_data, "L211.AnNH3Emissions")
    L211.AGRBio <- get_data(all_data, "L211.AGRBio") %>% rename (emiss.coef = bio_N2O_coef)
    L211.AWB_BCOC_EmissCoeff <- get_data(all_data, "L211.AWB_BCOC_EmissCoeff")
    L211.nonghg_max_reduction <- get_data(all_data, "L211.nonghg_max_reduction")
    L211.nonghg_steepness <- get_data(all_data, "L211.nonghg_steepness")
    L252.AgMAC <- get_data(all_data, "L252.AgMAC")
    L252.MAC_an <- get_data(all_data, "L252.MAC_an")
    # ===================================================

    # Produce outputs
    create_xml("all_aglu_emissions.xml") %>%
      add_xml_data(L211.AWBEmissions, "OutputEmissionsAg") %>%
      add_xml_data(L211.AGREmissions, "OutputEmissionsAg") %>%
      add_xml_data(L211.AnEmissions, "StbTechOutputEmissions") %>%
      add_xml_data(L211.AnNH3Emissions, "StbTechOutputEmissions") %>%
      add_xml_data(L211.AGRBio, "OutputEmissCoeffAg") %>%
      add_xml_data(L211.AWB_BCOC_EmissCoeff, "OutputEmissCoeffAg") %>%
      add_xml_data(L211.nonghg_max_reduction, "AgGDPCtrlMax") %>%
      add_xml_data(L211.nonghg_steepness, "AgGDPCtrlSteep") %>%
      add_xml_data(L252.AgMAC, "AgMAC") %>%
      add_xml_data(L252.MAC_an, "MAC") %>%
      add_precursors("L211.AWBEmissions", "L211.AGREmissions", "L211.AnEmissions", "L211.AnNH3Emissions",
                     "L211.AGRBio", "L211.AWB_BCOC_EmissCoeff", "L211.nonghg_max_reduction",
                     "L211.nonghg_steepness", "L252.AgMAC", "L252.MAC_an") ->
      all_aglu_emissions.xml

    return_data(all_aglu_emissions.xml)
  } else {
    stop("Unknown command")
  }
}
