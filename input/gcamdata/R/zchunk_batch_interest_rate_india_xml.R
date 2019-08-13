#' module_gcamindia_batch_interest_rate_xml
#'
#' Construct XML data structure for \code{interest_rate_india.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{interest_rate_india.xml}. The corresponding file in the
#' original data system was \code{batch_interest_rate_india.xml} (gcamindia XML).
module_gcamindia_batch_interest_rate_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.InterestRate_GCAMINDIA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "interest_rate_india.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L201.InterestRate_GCAMINDIA <- get_data(all_data, "L201.InterestRate_GCAMINDIA")

    # ===================================================

    # Produce outputs
    create_xml("interest_rate_india.xml") %>%
      add_xml_data(L201.InterestRate_GCAMINDIA, "InterestRate") %>%
      add_precursors("L201.InterestRate_GCAMINDIA") ->
      interest_rate_india.xml

    return_data(interest_rate_india.xml)
  } else {
    stop("Unknown command")
  }
}
