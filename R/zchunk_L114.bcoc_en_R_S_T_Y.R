#' module_emissions_L114.bcoc_en_R_S_T_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L114.bcoc_tgej_R_en_S_F_2000}. The corresponding file in the
#' original data system was \code{L114.bcoc_en_R_S_T_Y.R} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L114.bcoc_en_R_S_T_Y_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EPA_ghg_tech",
             FILE = "emissions/GCAM_sector_tech",
             "L101.in_EJ_R_en_Si_F_Yh",
             FILE = "temp-data-inject/L104.bcoc_tgej_USA_en_T_1990",
             FILE = "emissions/RCP_BC_2000",
             FILE = "emissions/RCP_OC_2000"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L114.bcoc_tgej_R_en_S_F_2000"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EPA_ghg_tech <- get_data(all_data, "emissions/EPA_ghg_tech")
    GCAM_sector_tech <- get_data(all_data, "emissions/GCAM_sector_tech")
    L101.in_EJ_R_en_Si_F_Yh <- get_data(all_data, "L101.in_EJ_R_en_Si_F_Yh")
    L104.bcoc_tgej_USA_en_T_1990 <- get_data(all_data, "temp-data-inject/L104.bcoc_tgej_USA_en_T_1990")
    RCP_BC_2000 <- get_data(all_data, "emissions/RCP_BC_2000")
    RCP_OC_2000 <- get_data(all_data, "emissions/RCP_OC_2000")

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
      add_legacy_name("L114.bcoc_tgej_R_en_S_F_2000") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L114.bcoc_tgej_R_en_S_F_2000

    return_data(L114.bcoc_tgej_R_en_S_F_2000)
  } else {
    stop("Unknown command")
  }
}
