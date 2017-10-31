#' module_gcamusa_batch_interest_rate_USA_xml
#'
#' Construct XML data structure for \code{interest_rate_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{interest_rate_USA.xml}. The corresponding file in the
#' original data system was \code{batch_interest_rate_USA.xml} (gcamusa XML).
module_gcamusa_batch_interest_rate_USA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.InterestRate"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "interest_rate_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L201.InterestRate <- get_data(all_data, "L201.InterestRate")

    # ===================================================

    # Produce outputs
    create_xml("interest_rate_USA.xml") %>%
      add_xml_data(L201.InterestRate,"InterestRate") %>%
      add_precursors("L201.InterestRate") ->
      interest_rate_USA.xml

    return_data(interest_rate_USA.xml)
  } else {
    stop("Unknown command")
  }
}
