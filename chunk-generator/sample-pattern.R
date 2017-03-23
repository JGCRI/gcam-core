#' CHUNK_NAME
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: DOCOUT_PATTERN. The corresponding file in the
#' original data system was \code{ORIGINALFILE_PATTERN} (MODULE_PATTERN LEVEL_PATTERN).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
CHUNK_NAME_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(INPUTS_PATTERN)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(OUTPUTS_PATTERN)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    LOAD_PATTERN

    # ===================================================
    # TRANSLATED PROCESSING CODE GOES HERE...
    #
    # If you find a mistake/thing to update in the old code and
    # fixing it will change the output data, causing the tests to fail,
    # (i) open an issue on GitHub, (ii) consult with colleagues, and
    # then (iii) code a fix:
    #
    # if(OLD_DATA_SYSTEM_BEHAVIOR) {
    #   ... code that replicates old, incorrect behavior
    # } else {
    #   ... new code with a fix
    # }
    #
   WARNING_PATTERN
    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    MAKEOUT_PATTERN

    return_data(RETURNOUT_PATTERN)
  } else {
    stop("Unknown command")
  }
}

