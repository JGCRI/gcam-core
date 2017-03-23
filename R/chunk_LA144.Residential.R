#' module_gcam.usa_LA144.Residential
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L144.flsp_bm2_state_res}, \code{L144.in_EJ_state_res_F_U_Y}. The corresponding file in the
#' original data system was \code{LA144.Residential.R} (gcam-usa level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_LA144.Residential_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
FILE = "gcam-usa/RECS_variables",
FILE = "gcam-usa/EIA_AEO_fuels",
FILE = "gcam-usa/EIA_AEO_services",
FILE = "gcam-usa/Census_pop_hist",
FILE = "gcam-usa/EIA_AEO_Tab4",
FILE = "gcam-usa/RECS_1979",
FILE = "gcam-usa/RECS_1984",
FILE = "gcam-usa/RECS_1990",
FILE = "gcam-usa/RECS_1993",
FILE = "gcam-usa/RECS_1997",
FILE = "gcam-usa/RECS_2001",
FILE = "gcam-usa/RECS_2005",
FILE = "gcam-usa/RECS_2009",
 "L142.in_EJ_state_bld_F",
 "L143.share_state_Pop_CDD_sR13",
 "L143.share_state_Pop_HDD_sR13"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L144.flsp_bm2_state_res",
"L144.in_EJ_state_res_F_U_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
  RECS_variables <- get_data(all_data, "gcam-usa/RECS_variables")
  EIA_AEO_fuels <- get_data(all_data, "gcam-usa/EIA_AEO_fuels")
  EIA_AEO_services <- get_data(all_data, "gcam-usa/EIA_AEO_services")
  Census_pop_hist <- get_data(all_data, "gcam-usa/Census_pop_hist")
  EIA_AEO_Tab4 <- get_data(all_data, "gcam-usa/EIA_AEO_Tab4")
  RECS_1979 <- get_data(all_data, "gcam-usa/RECS_1979")
  RECS_1984 <- get_data(all_data, "gcam-usa/RECS_1984")
  RECS_1990 <- get_data(all_data, "gcam-usa/RECS_1990")
  RECS_1993 <- get_data(all_data, "gcam-usa/RECS_1993")
  RECS_1997 <- get_data(all_data, "gcam-usa/RECS_1997")
  RECS_2001 <- get_data(all_data, "gcam-usa/RECS_2001")
  RECS_2005 <- get_data(all_data, "gcam-usa/RECS_2005")
  RECS_2009 <- get_data(all_data, "gcam-usa/RECS_2009")
  L142.in_EJ_state_bld_F <- get_data(all_data, "L142.in_EJ_state_bld_F")
  L143.share_state_Pop_CDD_sR13 <- get_data(all_data, "L143.share_state_Pop_CDD_sR13")
  L143.share_state_Pop_HDD_sR13 <- get_data(all_data, "L143.share_state_Pop_HDD_sR13")

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
 add_legacy_name("L144.flsp_bm2_state_res") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L144.flsp_bm2_state_res
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L144.in_EJ_state_res_F_U_Y") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L144.in_EJ_state_res_F_U_Y

    return_data(L144.flsp_bm2_state_res, L144.in_EJ_state_res_F_U_Y)
  } else {
    stop("Unknown command")
  }
}



