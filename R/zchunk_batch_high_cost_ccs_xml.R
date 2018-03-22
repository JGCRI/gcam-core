#' module_energy_batch_high_cost_ccs_xml
#'
#' Construct XML data structure for \code{high_cost_ccs.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{high_cost_ccs.xml}. The corresponding file in the
#' original data system was \code{batch_high_cost_ccs.xml.R} (energy XML).
module_energy_batch_high_cost_ccs_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L261.GlobalTechCost_C_High"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "high_cost_ccs.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L261.GlobalTechCost_C_High <- get_data(all_data, "L261.GlobalTechCost_C_High")

    # ===================================================

    # Produce outputs
    create_xml("high_cost_ccs.xml") %>%
      add_xml_data(L261.GlobalTechCost_C_High,"GlobalTechCost") %>%
      add_precursors("L261.GlobalTechCost_C_High") ->
      high_cost_ccs.xml

    return_data(high_cost_ccs.xml)
  } else {
    stop("Unknown command")
  }
}
