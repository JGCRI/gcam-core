#' module_water_batch_water_elec_liquids_limits_xml
#'
#' Construct XML data structure for \code{water_elec_liquids_limits.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_elec_liquids_limits.xml}. The corresponding file in the
#' original data system was \code{batch_water_elec_liquids_limits_xml.R} (water XML).
module_water_batch_water_elec_liquids_limits_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L2233.DeleteCreditInput_elec",
              "L2233.CreditInput_elec"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_elec_liquids_limits.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2233.DeleteCreditInput_elec <- get_data(all_data, "L2233.DeleteCreditInput_elec")
    L2233.CreditInput_elec <- get_data(all_data, "L2233.CreditInput_elec")

    # ===================================================

    # Produce outputs
    create_xml("water_elec_liquids_limits.xml") %>%
      add_xml_data(L2233.DeleteCreditInput_elec, "DeleteGlobalTechInput") %>%
      add_xml_data(L2233.CreditInput_elec, "GlobalTechCoef") %>%
      add_precursors("L2233.DeleteCreditInput_elec", "L2233.CreditInput_elec") ->
      water_elec_liquids_limits.xml

    return_data(water_elec_liquids_limits.xml)
  } else {
    stop("Unknown command")
  }
}
