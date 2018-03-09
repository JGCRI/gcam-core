#' module_energy_batch_liquids_limits_xml
#'
#' Construct XML data structure for \code{liquids_limits.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{liquids_limits.xml}. The corresponding file in the
#' original data system was \code{L270.limits.R} (energy XML).
module_energy_batch_liquids_limits_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L270.CreditOutput",
              "L270.CreditInput_elec",
              "L270.CreditInput_feedstocks",
              "L270.CreditMkt"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "liquids_limits.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L270.CreditOutput <- get_data(all_data, "L270.CreditOutput")
    L270.CreditInput_elec <- get_data(all_data, "L270.CreditInput_elec")
    L270.CreditInput_feedstocks <- get_data(all_data, "L270.CreditInput_feedstocks")
    L270.CreditMkt <- get_data(all_data, "L270.CreditMkt")

    # ===================================================

    # Produce outputs
    create_xml("liquids_limits.xml") %>%
      add_xml_data(L270.CreditOutput,"GlobalTechRESSecOut") %>%
      add_xml_data(L270.CreditInput_elec,"GlobalTechCoef") %>%
      add_xml_data(L270.CreditInput_feedstocks,"GlobalTechCoef") %>%
      add_xml_data(L270.CreditMkt,"PortfolioStd") %>%
      add_precursors("L270.CreditOutput", "L270.CreditInput_elec", "L270.CreditInput_feedstocks", "L270.CreditMkt") ->
      liquids_limits.xml

    return_data(liquids_limits.xml)
  } else {
    stop("Unknown command")
  }
}
