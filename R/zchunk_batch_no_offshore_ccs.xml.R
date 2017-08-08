#' module_energy_batch_no_offshore_ccs.xml
#'
#' Construct XML data structure for \code{no_offshore_ccs.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{no_offshore_ccs.xml}. The corresponding file in the
#' original data system was \code{batch_no_offshore_ccs.xml.R} (energy XML).
module_energy_batch_no_offshore_ccs.xml_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L261.GlobalTechShrwt_C_nooffshore"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "no_offshore_ccs.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L261.GlobalTechShrwt_C_nooffshore <- get_data(all_data, "L261.GlobalTechShrwt_C_nooffshore")

    # ===================================================

    # Produce outputs
    create_xml("no_offshore_ccs.xml") %>%
      add_xml_data(L261.GlobalTechShrwt_C_nooffshore,"GlobalTechShrwt") %>%
      add_precursors("L261.GlobalTechShrwt_C_nooffshore") ->
      no_offshore_ccs.xml

    return_data(no_offshore_ccs.xml)
  } else {
    stop("Unknown command")
  }
}
