#' module_emissions_L121.nonco2_awb_R_S_T_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L121.AWBshare_R_C_Y_GLU}, \code{L121.nonco2_tg_R_awb_C_Y_GLU}. The corresponding file in the
#' original data system was \code{L121.nonco2_awb_R_S_T_Y.R} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_emissions_L121.nonco2_awb_R_S_T_Y_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
FILE = "emissions/EDGAR_nation",
FILE = "emissions/EDGAR_sector",
 "L103.ag_Prod_Mt_R_C_Y_GLU",
 "L111.ag_resbio_R_C",
FILE = "emissions/EDGAR_SO2",
FILE = "emissions/EDGAR_CO",
FILE = "emissions/EDGAR_NOx",
FILE = "emissions/EDGAR_NMVOC",
FILE = "emissions/EDGAR_CH4",
FILE = "emissions/EDGAR_N2O",
FILE = "emissions/EDGAR_NH3"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L121.AWBshare_R_C_Y_GLU",
"L121.nonco2_tg_R_awb_C_Y_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
  EDGAR_nation <- get_data(all_data, "emissions/EDGAR_nation")
  EDGAR_sector <- get_data(all_data, "emissions/EDGAR_sector")
  L103.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y_GLU")
  L111.ag_resbio_R_C <- get_data(all_data, "L111.ag_resbio_R_C")
  EDGAR_SO2 <- get_data(all_data, "emissions/EDGAR_SO2")
  EDGAR_CO <- get_data(all_data, "emissions/EDGAR_CO")
  EDGAR_NOx <- get_data(all_data, "emissions/EDGAR_NOx")
  EDGAR_NMVOC <- get_data(all_data, "emissions/EDGAR_NMVOC")
  EDGAR_CH4 <- get_data(all_data, "emissions/EDGAR_CH4")
  EDGAR_N2O <- get_data(all_data, "emissions/EDGAR_N2O")
  EDGAR_NH3 <- get_data(all_data, "emissions/EDGAR_NH3")

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
 add_legacy_name("L121.AWBshare_R_C_Y_GLU") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L121.AWBshare_R_C_Y_GLU
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L121.nonco2_tg_R_awb_C_Y_GLU") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L121.nonco2_tg_R_awb_C_Y_GLU

    return_data(L121.AWBshare_R_C_Y_GLU, L121.nonco2_tg_R_awb_C_Y_GLU)
  } else {
    stop("Unknown command")
  }
}



