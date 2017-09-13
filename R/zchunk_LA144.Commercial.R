#' module_gcam.usa_LA144.Commercial
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L144.flsp_bm2_state_comm}, \code{L144.in_EJ_state_comm_F_U_Y}. The corresponding file in the
#' original data system was \code{LA144.Commercial.R} (gcam-usa level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_LA144.Commercial_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/Census_pop_hist",
             FILE = "gcam-usa/CBECS_variables",
             FILE = "gcam-usa/EIA_AEO_Tab5",
             FILE = "gcam-usa/EIA_distheat",
             FILE = "gcam-usa/PNNL_Commext_elec",
             FILE = "gcam-usa/CBECS_1979_1983",
             FILE = "gcam-usa/CBECS_1986",
             FILE = "gcam-usa/CBECS_1989",
             FILE = "gcam-usa/CBECS_1992",
             FILE = "gcam-usa/CBECS_1995",
             FILE = "gcam-usa/CBECS_1999",
             FILE = "gcam-usa/CBECS_2003",
             "L142.in_EJ_state_bld_F",
             "L143.share_state_Pop_CDD_sR9",
             "L143.share_state_Pop_HDD_sR9"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L144.flsp_bm2_state_comm",
             "L144.in_EJ_state_comm_F_U_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    Census_pop_hist <- get_data(all_data, "gcam-usa/Census_pop_hist")
    CBECS_variables <- get_data(all_data, "gcam-usa/CBECS_variables")
    EIA_AEO_Tab5 <- get_data(all_data, "gcam-usa/EIA_AEO_Tab5")
    EIA_distheat <- get_data(all_data, "gcam-usa/EIA_distheat")
    PNNL_Commext_elec <- get_data(all_data, "gcam-usa/PNNL_Commext_elec")
    CBECS_1979_1983 <- get_data(all_data, "gcam-usa/CBECS_1979_1983")
    CBECS_1986 <- get_data(all_data, "gcam-usa/CBECS_1986")
    CBECS_1989 <- get_data(all_data, "gcam-usa/CBECS_1989")
    CBECS_1992 <- get_data(all_data, "gcam-usa/CBECS_1992")
    CBECS_1995 <- get_data(all_data, "gcam-usa/CBECS_1995")
    CBECS_1999 <- get_data(all_data, "gcam-usa/CBECS_1999")
    CBECS_2003 <- get_data(all_data, "gcam-usa/CBECS_2003")
    L142.in_EJ_state_bld_F <- get_data(all_data, "L142.in_EJ_state_bld_F")
    L143.share_state_Pop_CDD_sR9 <- get_data(all_data, "L143.share_state_Pop_CDD_sR9")
    L143.share_state_Pop_HDD_sR9 <- get_data(all_data, "L143.share_state_Pop_HDD_sR9")

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
    # NOTE: there are 'match' calls in this code. You probably want to use left_join_error_no_match
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # NOTE: This code uses vecpaste
    # This function can be removed; see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
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
      add_legacy_name("L144.flsp_bm2_state_comm") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L144.flsp_bm2_state_comm

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L144.in_EJ_state_comm_F_U_Y") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L144.in_EJ_state_comm_F_U_Y

    return_data(L144.flsp_bm2_state_comm, L144.in_EJ_state_comm_F_U_Y)
  } else {
    stop("Unknown command")
  }
}
