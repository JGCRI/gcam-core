#' module_gcamusa_batch_HDDCDD_A2_GFDL_USA.xml
#'
#' Construct XML data structure for \code{HDDCDD_A2_GFDL_USA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{HDDCDD_A2_GFDL_USA.xml}. The corresponding file in the
#' original data system was \code{batch_HDDCDD_A2_GFDL_USA.xml} (gcamusa XML).
module_gcamusa_batch_HDDCDD_A2_GFDL_USA.xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L244.HDDCDD_A2_GFDL"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "HDDCDD_A2_GFDL_USA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L244.HDDCDD_A2_GFDL <- get_data(all_data, "L244.HDDCDD_A2_GFDL")

    # Produce outputs
    create_xml("HDDCDD_A2_GFDL_USA.xml") %>%
      add_xml_data(L244.HDDCDD_A2_GFDL, "HDDCDD") %>%
      add_precursors("L244.HDDCDD_A2_GFDL") ->
      HDDCDD_A2_GFDL_USA.xml

    return_data(HDDCDD_A2_GFDL_USA.xml)
  } else {
    stop("Unknown command")
  }
}
