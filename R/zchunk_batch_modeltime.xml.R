#' module_modeltime_batch_modeltime.xml
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{modeltime.xml}. The corresponding file in the
#' original data system was \code{batch_modeltime.xml.R} (modeltime XML).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_modeltime_batch_modeltime.xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L200.ModelTime",
              "L200.ModelTimeInterYears"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "modeltime.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L200.ModelTime <- get_data(all_data, "L200.ModelTime")
    L200.ModelTimeInterYears <- get_data(all_data, "L200.ModelTimeInterYears")

    # Produce outputs
    create_xml("modeltime.xml") %>%
      add_xml_data(L200.ModelTime, "ModelTime") %>%
      add_xml_data(L200.ModelTimeInterYears, "ModelTimeInterYears") %>%
      add_precursors("L200.ModelTime", "L200.ModelTimeInterYears")
      run_xml_conversion() ->
      modeltime.xml

    return_data(modeltime.xml)
  } else {
    stop("Unknown command")
  }
}
