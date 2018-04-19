#' module_emissions_batch_all_aglu_emissions_IRR_xml
#'
#' Construct XML data structure for \code{all_aglu_emissions_IRR.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{all_aglu_emissions_IRR.xml}. The corresponding file in the
#' original data system was \code{batch_all_aglu_emissions_IRR.xml} (emissions XML).
module_emissions_batch_all_aglu_emissions_IRR_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2521.AgMAC",
              "L252.MAC_an",
              "L2111.AWBEmissions",
              "L2111.AGREmissions",
              "L211.AnEmissions",
              "L211.AnNH3Emissions",
              "L2111.AGRBio",
              "L2111.AWB_BCOC_EmissCoeff",
              "L2111.nonghg_max_reduction",
              "L2111.nonghg_steepness"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "all_aglu_emissions_IRR.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    bio_N2O_coef <- NULL  # silence package check notes

    # Load required inputs
    L2521.AgMAC <- get_data(all_data, "L2521.AgMAC")
    L252.MAC_an <- get_data(all_data, "L252.MAC_an")
    L2111.AWBEmissions <- get_data(all_data, "L2111.AWBEmissions")
    L2111.AGREmissions <- get_data(all_data, "L2111.AGREmissions")
    L211.AnEmissions <- get_data(all_data, "L211.AnEmissions")
    L211.AnNH3Emissions <- get_data(all_data, "L211.AnNH3Emissions")
    L2111.AGRBio <- get_data(all_data, "L2111.AGRBio") %>% rename (emiss.coef = bio_N2O_coef)
    L2111.AWB_BCOC_EmissCoeff <- get_data(all_data, "L2111.AWB_BCOC_EmissCoeff")
    L2111.nonghg_max_reduction <- get_data(all_data, "L2111.nonghg_max_reduction")
    L2111.nonghg_steepness <- get_data(all_data, "L2111.nonghg_steepness")

    # ===================================================

    # Produce outputs
    create_xml("all_aglu_emissions_IRR.xml") %>%
      add_xml_data(L2521.AgMAC, "AgMAC") %>%
      add_xml_data(L252.MAC_an, "MAC") %>%
      add_xml_data(L2111.AWBEmissions, "OutputEmissionsAg") %>%
      add_xml_data(L2111.AGREmissions, "OutputEmissionsAg") %>%
      add_xml_data(L211.AnEmissions, "StbTechOutputEmissions") %>%
      add_xml_data(L211.AnNH3Emissions, "StbTechOutputEmissions") %>%
      add_xml_data(L2111.AGRBio, "OutputEmissCoeffAg") %>%
      add_xml_data(L2111.AWB_BCOC_EmissCoeff, "OutputEmissCoeffAg") %>%
      add_xml_data(L2111.nonghg_max_reduction, "AgGDPCtrlMax") %>%
      add_xml_data(L2111.nonghg_steepness, "AgGDPCtrlSteep") %>%
      add_precursors("L2521.AgMAC", "L252.MAC_an", "L2111.AWBEmissions", "L2111.AGREmissions", "L211.AnEmissions",
                     "L211.AnNH3Emissions", "L2111.AGRBio", "L2111.AWB_BCOC_EmissCoeff", "L2111.nonghg_max_reduction", "L2111.nonghg_steepness") ->
        all_aglu_emissions_IRR.xml

    return_data(all_aglu_emissions_IRR.xml)
  } else {
    stop("Unknown command")
  }
}
