#' module_socioeconomics_L101.Population
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L101.Pop_thous_R_Yh}, \code{L101.Pop_thous_Scen_R_Yfut}, \code{L101.Pop_thous_GCAM3_R_Y}, \code{L101.Pop_thous_GCAM3_ctry_Y}. The corresponding file in the
#' original data system was \code{L101.Population.R} (socioeconomics level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL April 2017
#' @export
module_socioeconomics_L101.Population <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "socioeconomics/GCAM3_population",
             "L100.Pop_thous_ctry_Yh",
             "L100.Pop_thous_SSP_ctry_Yfut"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.Pop_thous_R_Yh",
             "L101.Pop_thous_Scen_R_Yfut",
             "L101.Pop_thous_GCAM3_R_Y",
             "L101.Pop_thous_GCAM3_ctry_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM3_population <- get_data(all_data, "socioeconomics/GCAM3_population")
    L100.Pop_thous_ctry_Yh <- get_data(all_data, "L100.Pop_thous_ctry_Yh")
    L100.Pop_thous_SSP_ctry_Yfut <- get_data(all_data, "L100.Pop_thous_SSP_ctry_Yfut")

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
    # NOTE: there are `merge` calls in this code. Be careful!
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # NOTE: there are 'match' calls in this code. You probably want to use left_join_error_no_match
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # ===================================================

    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L101.Pop_thous_R_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "socioeconomics/GCAM3_population",
                     "L100.Pop_thous_ctry_Yh", "L100.Pop_thous_SSP_ctry_Yfut") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.Pop_thous_R_Yh
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L101.Pop_thous_Scen_R_Yfut") %>%
      add_precursors("common/iso_GCAM_regID", "socioeconomics/GCAM3_population",
                     "L100.Pop_thous_ctry_Yh", "L100.Pop_thous_SSP_ctry_Yfut") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.Pop_thous_Scen_R_Yfut
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L101.Pop_thous_GCAM3_R_Y") %>%
      add_precursors("common/iso_GCAM_regID", "socioeconomics/GCAM3_population",
                     "L100.Pop_thous_ctry_Yh", "L100.Pop_thous_SSP_ctry_Yfut") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.Pop_thous_GCAM3_R_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L101.Pop_thous_GCAM3_ctry_Y") %>%
      add_precursors("common/iso_GCAM_regID", "socioeconomics/GCAM3_population",
                     "L100.Pop_thous_ctry_Yh", "L100.Pop_thous_SSP_ctry_Yfut") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.Pop_thous_GCAM3_ctry_Y

    return_data(L101.Pop_thous_R_Yh, L101.Pop_thous_Scen_R_Yfut, L101.Pop_thous_GCAM3_R_Y, L101.Pop_thous_GCAM3_ctry_Y)
  } else {
    stop("Unknown command")
  }
}
