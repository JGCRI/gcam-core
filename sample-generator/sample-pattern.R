#' CHUNK_NAME
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: DOCOUT_PATTERN
#' @details Describe in detail what this chunk does.
#' @author Author name
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

    # Below is the code of the original data system file
    # This should all be deleted eventually
    COMMENTED_CODE_PATTERN

    # Produce outputs, adding appropriate flags and comments
    # Temporary code below
    MAKEOUT_PATTERN

    return_data(RETURNOUT_PATTERN)
  } else {
    stop("Unknown command")
  }
}
