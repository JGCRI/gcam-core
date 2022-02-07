#' module_gcamusa_batch_elecS_ghg_emissions_USA_xml
#'
#' Construct XML data structure for \code{elecS_ghg_emissions_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{elecS_ghg_emissions_USA.xml}. The corresponding file in the
#' original data system was \code{elecS_ghg_emissions_USA.xml} (gcamusa XML).
module_gcamusa_batch_elecS_ghg_emissions_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2236.elecS_ghg_tech_coeff_USA",
             "L2236.elecS_ghg_emissions_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "elecS_ghg_emissions_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2236.elecS_ghg_tech_coeff_USA <- get_data(all_data, 'L2236.elecS_ghg_tech_coeff_USA')
    L2236.elecS_ghg_emissions_USA  <- get_data(all_data, 'L2236.elecS_ghg_emissions_USA')

    # Silence package checks
    emiss.coeff <- NULL

    # ===================================================
    # Rename columns to meet the LEVEL2_DATA_NAMES header requirements
    L2236.elecS_ghg_tech_coeff_USA <- L2236.elecS_ghg_tech_coeff_USA %>%
      select(-technology) %>%
      rename(stub.technology = subsector, subsector = subsector0)

    L2236.elecS_ghg_emissions_USA <- L2236.elecS_ghg_emissions_USA %>%
      select(-stub.technology) %>%
      rename(stub.technology = subsector, subsector = subsector0)

    # Produce outputs
    create_xml("elecS_ghg_emissions_USA.xml") %>%
      add_xml_data(L2236.elecS_ghg_tech_coeff_USA, "OutputEmissCoeff") %>%
      add_xml_data(L2236.elecS_ghg_emissions_USA, "InputEmissions") %>%
      add_precursors("L2236.elecS_ghg_tech_coeff_USA",
                     "L2236.elecS_ghg_emissions_USA") ->
      elecS_ghg_emissions_USA.xml

    return_data(elecS_ghg_emissions_USA.xml)
  } else {
    stop("Unknown command")
  }
}
