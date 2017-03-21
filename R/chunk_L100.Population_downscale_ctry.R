#' module_socioeconomics_L100.Population_downscale_ctry
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.Pop_thous_ctry_Yh}, \code{L100.Pop_thous_SSP_ctry_Yfut}. The corresponding file in the
#' original data system was \code{L100.Population_downscale_ctry.R} (socioeconomics level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_socioeconomics_L100.Population_downscale_ctry_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "socioeconomics/socioeconomics_ctry",
             "Maddison_population",
             FILE = "socioeconomics/SSP_database_v9",
             FILE = "socioeconomics/UN_popTot"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.Pop_thous_ctry_Yh",
             "L100.Pop_thous_SSP_ctry_Yfut"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    socioeconomics_ctry <- get_data(all_data, "socioeconomics/socioeconomics_ctry")
    Maddison_population <- get_data(all_data, "Maddison_population")
    SSP_database_v9 <- get_data(all_data, "socioeconomics/SSP_database_v9")
    UN_popTot <- get_data(all_data, "socioeconomics/UN_popTot")

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
      add_legacy_name("L100.Pop_thous_ctry_Yh") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L100.Pop_thous_ctry_Yh

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L100.Pop_thous_SSP_ctry_Yfut") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L100.Pop_thous_SSP_ctry_Yfut

    return_data(L100.Pop_thous_ctry_Yh, L100.Pop_thous_SSP_ctry_Yfut)
  } else {
    stop("Unknown command")
  }
}



