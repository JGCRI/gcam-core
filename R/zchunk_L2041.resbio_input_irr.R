#' module_aglu_L2041.resbio_input_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2041.AgResBio_For}, \code{L2041.GlobalResBio_Mill}, \code{L2041.AgResBio_ag_irr}, \code{L2041.AgResBioCurve_For}, \code{L2041.StubResBioCurve_Mill}, \code{L2041.AgResBioCurve_ag_irr}. The corresponding file in the
#' original data system was \code{L2041.resbio_input_irr.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2041.resbio_input_irr_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L204.AgResBio_For",
              "L204.GlobalResBio_Mill",
              "L204.AgResBio_ag",
              "L204.AgResBioCurve_For",
              "L204.StubResBioCurve_Mill",
              "L204.AgResBioCurve_ag"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2041.AgResBio_For",
             "L2041.GlobalResBio_Mill",
             "L2041.AgResBio_ag_irr",
             "L2041.AgResBioCurve_For",
             "L2041.StubResBioCurve_Mill",
             "L2041.AgResBioCurve_ag_irr"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L204.AgResBio_For <- get_data(all_data, "L204.AgResBio_For")
    L204.GlobalResBio_Mill <- get_data(all_data, "L204.GlobalResBio_Mill")
    L204.AgResBio_ag <- get_data(all_data, "L204.AgResBio_ag")
    L204.AgResBioCurve_For <- get_data(all_data, "L204.AgResBioCurve_For")
    L204.StubResBioCurve_Mill <- get_data(all_data, "L204.StubResBioCurve_Mill")
    L204.AgResBioCurve_ag <- get_data(all_data, "L204.AgResBioCurve_ag")

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
    #
    # NOTE: This code uses repeat_and_add_vector
    # This function can be removed; see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2041.AgResBio_For") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2041.AgResBio_For
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2041.GlobalResBio_Mill") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2041.GlobalResBio_Mill
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2041.AgResBio_ag_irr") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2041.AgResBio_ag_irr
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2041.AgResBioCurve_For") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2041.AgResBioCurve_For
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2041.StubResBioCurve_Mill") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2041.StubResBioCurve_Mill
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2041.AgResBioCurve_ag_irr") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2041.AgResBioCurve_ag_irr

    return_data(L2041.AgResBio_For, L2041.GlobalResBio_Mill, L2041.AgResBio_ag_irr, L2041.AgResBioCurve_For, L2041.StubResBioCurve_Mill, L2041.AgResBioCurve_ag_irr)
  } else {
    stop("Unknown command")
  }
}
