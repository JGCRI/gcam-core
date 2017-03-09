# sample-chunk.R


#' module_sample_sample
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}.
#' @author Author name
#' @export
module_sample_sample <- function(command, ...) {
  if(command == driver.DECLARE_OUTPUTS) {
    return(c("first_output",
             "second_output"))
  } else if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",  # input from a file
             "L200.ModelTime"))  # input produced by another chunk
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # printlog( "Historical GDP and per-capita GDP by state" )

    # Load data
    input1 <- get_data(all_data, "common/iso_GCAM_regID")
    input2 <- get_data(all_data, "L200.ModelTime")

    # Process...

    # Produce outputs, add appropriate flags and comments
    tibble() %>%
      add_title("First output") %>%
      add_units("None") %>%
      add_precursors("L200.ModelTime") %>%
      add_flags(FLAG_LONG_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST, FLAG_NO_OUTPUT) %>%
      add_comments("Sample chunk output") ->
      first_output

    tibble() %>%
      add_title("Second output") %>%
      add_units("None") %>%
      add_precursors("L200.ModelTime") %>%
      add_flags(FLAG_LONG_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST, FLAG_NO_OUTPUT) %>%
      add_comments("Sample chunk output") ->
      second_output

    return_data(first_output, second_output)
  } else {
    stop("Unknown command")
  }
}
