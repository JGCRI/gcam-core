#' module_energy_batch_elec_bio_low_xml
#'
#' Construct XML data structure for \code{elec_bio_low.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{elec_bio_low.xml}. The corresponding file in the
#' original data system was \code{batch_elec_bio_low_xml.R} (energy XML).
module_energy_batch_elec_bio_low_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L223.GlobalTechCapital_bio_low"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "elec_bio_low.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L223.GlobalTechCapital_bio_low <- get_data(all_data, "L223.GlobalTechCapital_bio_low")

    # ===================================================

    # Produce outputs
    create_xml("elec_bio_low.xml") %>%
      add_xml_data(L223.GlobalTechCapital_bio_low, "GlobalTechCapital") %>%
      add_precursors("L223.GlobalTechCapital_bio_low") ->
      elec_bio_low.xml

    return_data(elec_bio_low.xml)
  } else {
    stop("Unknown command")
  }
}
