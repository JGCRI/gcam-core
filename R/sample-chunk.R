# sample-chunk.R


#' module_sample_sample
#'
#' Briefly describe what this chunk does.
#'
#' @details Give a detailed description of what this chunk does.
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
    return(c(FILE = "sample/sample_file_input",
             "L200.ModelTime"))
  } else if(command == driver.MAKE) {
    sample_sample_makedata(...)
  } else {
    stop("Unknown command")
  }
}


#' gcam-sample_sample_makedata
#'
#' @param all_data A named list, holding necessary input data
#' @return A named list with all output data.
#' @importFrom tibble tibble
#' @importFrom assertthat assert_that
#' @importFrom tidyr gather spread
#' @import dplyr
#' @author Author name
#' @export
sample_sample_makedata <- function(all_data) {

  # printlog( "Historical GDP and per-capita GDP by state" )

  input1 <- get_data(all_data, "sample/sample_file_input")
  input2 <- get_data(all_data, "L200.ModelTime")

  input1 %>%
    add_dsflags(FLAG_LONG_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST, FLAG_NO_OUTPUT) %>%
    add_dscomments("Sample chunk output") ->
    first_output

  input2 %>%
    add_dsflags(FLAG_LONG_FORM, FLAG_NO_XYEAR, FLAG_NO_TEST, FLAG_NO_OUTPUT) %>%
    add_dscomments("Sample chunk output") ->
    second_output

  return_data(first_output, second_output)
}
