# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_batch_interest_rate_xml
#'
#' Construct XML data structure for \code{interest_rate.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{interest_rate.xml}. The corresponding file in the
#' original data system was \code{batch_interest_rate.xml.R} (socio XML).
module_socio_batch_interest_rate_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.InterestRate"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "interest_rate.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L201.InterestRate <- get_data(all_data, "L201.InterestRate")

    # ===================================================

    # Produce outputs
    create_xml("interest_rate.xml") %>%
      add_xml_data(L201.InterestRate, "InterestRate") %>%
      add_precursors("L201.InterestRate") ->
      interest_rate.xml

    return_data(interest_rate.xml)
  } else {
    stop("Unknown command")
  }
}
