#' module_gcamusa_batch_ghg_emissions_USA_xml
#'
#' Construct XML data structure for \code{ghg_emissions_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ghg_emissions_USA.xml}. The corresponding file in the
#' original data system was \code{batch_ghg_emissions_USA.xml} (gcamusa XML).
module_gcamusa_batch_ghg_emissions_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L273.en_ghg_tech_coeff_USA",
             "L273.en_ghg_emissions_USA",
             "L273.out_ghg_emissions_USA",
             "L273.MAC_higwp_USA",
             "L273.MAC_higwp_TC_USA",
             "L273.MAC_higwp_phaseInTime_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ghg_emissions_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    emiss.coeff <- output.emissions <- NULL

    # Load required inputs
    L273.en_ghg_tech_coeff_USA     <- get_data(all_data, "L273.en_ghg_tech_coeff_USA")
    L273.en_ghg_emissions_USA      <- get_data(all_data, "L273.en_ghg_emissions_USA")
    L273.out_ghg_emissions_USA     <- get_data(all_data, "L273.out_ghg_emissions_USA")
    L273.MAC_higwp_USA             <- get_data(all_data, "L273.MAC_higwp_USA")
    L273.MAC_higwp_TC_USA          <- get_data(all_data, "L273.MAC_higwp_TC_USA")
    L273.MAC_higwp_phaseInTime_USA <- get_data(all_data, "L273.MAC_higwp_phaseInTime_USA")

    # ===================================================
    # Rename to meet header requriements
    L273.en_ghg_tech_coeff_USA <- rename(L273.en_ghg_tech_coeff_USA, emiss.coef = emiss.coeff)
    L273.out_ghg_emissions_USA <- rename(L273.out_ghg_emissions_USA, input.emissions = output.emissions)

    # Produce outputs
    create_xml("ghg_emissions_USA.xml") %>%
      add_xml_data(L273.en_ghg_tech_coeff_USA, "InputEmissCoeff") %>%
      add_xml_data(L273.en_ghg_emissions_USA, "InputEmissions") %>%
      add_xml_data(L273.out_ghg_emissions_USA, "StbTechOutputEmissions") %>%
      add_xml_data(L273.MAC_higwp_USA, "MAC") %>%
      add_xml_data(L273.MAC_higwp_TC_USA, "MACTC") %>%
      add_xml_data(L273.MAC_higwp_phaseInTime_USA, "MACPhaseIn") %>%
      add_precursors("L273.en_ghg_tech_coeff_USA",
                     "L273.en_ghg_emissions_USA",
                     "L273.out_ghg_emissions_USA",
                     "L273.MAC_higwp_USA",
                     "L273.MAC_higwp_TC_USA",
                     "L273.MAC_higwp_phaseInTime_USA") ->
      ghg_emissions_USA.xml

    return_data(ghg_emissions_USA.xml)
  } else {
    stop("Unknown command")
  }
}
