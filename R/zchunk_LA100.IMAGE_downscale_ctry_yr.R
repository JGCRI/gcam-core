#' module_aglu_LA100.IMAGE_downscale_ctry_yr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y}, \code{L100.IMAGE_an_FeedIO_ctry_C_Sys_Y}, \code{L100.IMAGE_an_Prodmixfrac_ctry_C_Y}. The corresponding file in the
#' original data system was \code{LA100.IMAGE_downscale_ctry_yr.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LA100.IMAGE_downscale_ctry_yr_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "aglu/AGLU_ctry",
             FILE = "aglu/IMAGE_an_Feedfrac_Rimg_C_Sys_Fd_Y",
             FILE = "aglu/IMAGE_an_FeedIO_Rimg_C_Sys_Y",
             FILE = "aglu/IMAGE_an_Prodmixfrac_Rimg_C_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y",
             "L100.IMAGE_an_FeedIO_ctry_C_Sys_Y",
             "L100.IMAGE_an_Prodmixfrac_ctry_C_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    IMAGE_an_Feedfrac_Rimg_C_Sys_Fd_Y <- get_data(all_data, "aglu/IMAGE_an_Feedfrac_Rimg_C_Sys_Fd_Y")
    IMAGE_an_FeedIO_Rimg_C_Sys_Y <- get_data(all_data, "aglu/IMAGE_an_FeedIO_Rimg_C_Sys_Y")
    IMAGE_an_Prodmixfrac_Rimg_C_Y <- get_data(all_data, "aglu/IMAGE_an_Prodmixfrac_Rimg_C_Y")

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
      add_legacy_name("L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L100.IMAGE_an_FeedIO_ctry_C_Sys_Y") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L100.IMAGE_an_FeedIO_ctry_C_Sys_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L100.IMAGE_an_Prodmixfrac_ctry_C_Y") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L100.IMAGE_an_Prodmixfrac_ctry_C_Y

    return_data(L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y, L100.IMAGE_an_FeedIO_ctry_C_Sys_Y, L100.IMAGE_an_Prodmixfrac_ctry_C_Y)
  } else {
    stop("Unknown command")
  }
}
