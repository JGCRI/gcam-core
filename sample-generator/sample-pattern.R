#' CHUNK_NAME
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: DOCOUT_PATTERN. This corresponding file in the
#' original data system was \code{ORIGINALFILE_PATTERN} (MODULE_PATTERN LEVEL_PATTERN).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author Author name(s)
#' @export
CHUNK_NAME <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(INPUTS_PATTERN)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(OUTPUTS_PATTERN)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    LOAD_PATTERN

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    tibble() %>%
      add_dsflags(FLAG_NO_TEST, FLAG_LONG_FORM, FLAG_NO_XYEAR) ->
    MAKEOUT_PATTERN

    return_data(RETURNOUT_PATTERN)
  } else {
    stop("Unknown command")
  }
}
