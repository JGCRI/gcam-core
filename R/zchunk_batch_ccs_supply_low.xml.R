#' module_energy_batch_ccs_supply_low.xml
#'
#' Construct XML data structure for \code{ccs_supply_low.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ccs_supply_low.xml}. The corresponding file in the
#' original data system was \code{batch_ccs_supply_low.xml.R} (energy XML).
module_energy_batch_ccs_supply_low.xml_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L261.DepRsrcCurves_C_low"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ccs_supply_low.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L261.DepRsrcCurves_C_low <- get_data(all_data, "L261.DepRsrcCurves_C_low")

    # ===================================================

    # Produce outputs
    create_xml("ccs_supply_low.xml") %>%
      add_xml_data(L261.DepRsrcCurves_C_low,"DepRsrcCurves") %>%
      add_precursors("L261.DepRsrcCurves_C_low") ->
      ccs_supply_low.xml

    return_data(ccs_supply_low.xml)
  } else {
    stop("Unknown command")
  }
}
