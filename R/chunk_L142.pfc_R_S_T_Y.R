#' module_emissions_L142.pfc_R_S_T_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L142.pfc_R_S_T_Yh}. The corresponding file in the
#' original data system was \code{L142.pfc_R_S_T_Y.R} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L142.pfc_R_S_T_Y_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
FILE = "emissions/gcam_fgas_tech",
FILE = "emissions/other_f_gases",
 "L144.in_EJ_R_bld_serv_F_Yh",
FILE = "common/iso_GCAM_regID",
FILE = "emissions/EDGAR_sector_fgas",
FILE = "emissions/EDGAR_nation",
FILE = "emissions/A41.GWP",
FILE = "emissions/EDGAR_SF6",
FILE = "emissions/EDGAR_C2F6",
FILE = "emissions/EDGAR_CF4"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L142.pfc_R_S_T_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  gcam_fgas_tech <- get_data(all_data, "emissions/gcam_fgas_tech")
  other_f_gases <- get_data(all_data, "emissions/other_f_gases")
  L144.in_EJ_R_bld_serv_F_Yh <- get_data(all_data, "L144.in_EJ_R_bld_serv_F_Yh")
  iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
  EDGAR_sector_fgas <- get_data(all_data, "emissions/EDGAR_sector_fgas")
  EDGAR_nation <- get_data(all_data, "emissions/EDGAR_nation")
  A41.GWP <- get_data(all_data, "emissions/A41.GWP")
  EDGAR_SF6 <- get_data(all_data, "emissions/EDGAR_SF6")
  EDGAR_C2F6 <- get_data(all_data, "emissions/EDGAR_C2F6")
  EDGAR_CF4 <- get_data(all_data, "emissions/EDGAR_CF4")

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
 add_legacy_name("L142.pfc_R_S_T_Yh") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L142.pfc_R_S_T_Yh

    return_data(L142.pfc_R_S_T_Yh)
  } else {
    stop("Unknown command")
  }
}



