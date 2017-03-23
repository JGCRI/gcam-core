#' module_aglu_LA105.an_FAO_R_C_Y
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L105.an_Food_Mt_R_C_Y}, \code{L105.an_Food_Pcal_R_C_Y}, \code{L105.an_kcalg_R_C_Y}, \code{L105.an_Prod_Mt_R_C_Y}, \code{L105.an_Prod_Mt_ctry_C_Y}, \code{L105.an_StockShares_R_BufGoat_2005}. The corresponding file in the
#' original data system was \code{LA105.an_FAO_R_C_Y.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LA105.an_FAO_R_C_Y_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
FILE = "common/GCAM_region_names",
FILE = "aglu/AGLU_ctry",
FILE = "aglu/FAO_an_items_cal_SUA",
FILE = "aglu/FAO_an_items_PRODSTAT",
 "L100.FAO_an_Food_t",
 "L100.FAO_an_Prod_t",
 "L100.FAO_an_Stocks"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L105.an_Food_Mt_R_C_Y",
"L105.an_Food_Pcal_R_C_Y",
"L105.an_kcalg_R_C_Y",
"L105.an_Prod_Mt_R_C_Y",
"L105.an_Prod_Mt_ctry_C_Y",
"L105.an_StockShares_R_BufGoat_2005"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
  GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
  AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
  FAO_an_items_cal_SUA <- get_data(all_data, "aglu/FAO_an_items_cal_SUA")
  FAO_an_items_PRODSTAT <- get_data(all_data, "aglu/FAO_an_items_PRODSTAT")
  L100.FAO_an_Food_t <- get_data(all_data, "L100.FAO_an_Food_t")
  L100.FAO_an_Prod_t <- get_data(all_data, "L100.FAO_an_Prod_t")
  L100.FAO_an_Stocks <- get_data(all_data, "L100.FAO_an_Stocks")

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
 add_legacy_name("L105.an_Food_Mt_R_C_Y") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L105.an_Food_Mt_R_C_Y
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L105.an_Food_Pcal_R_C_Y") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L105.an_Food_Pcal_R_C_Y
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L105.an_kcalg_R_C_Y") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L105.an_kcalg_R_C_Y
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L105.an_Prod_Mt_R_C_Y") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L105.an_Prod_Mt_R_C_Y
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L105.an_Prod_Mt_ctry_C_Y") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L105.an_Prod_Mt_ctry_C_Y
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L105.an_StockShares_R_BufGoat_2005") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L105.an_StockShares_R_BufGoat_2005

    return_data(L105.an_Food_Mt_R_C_Y, L105.an_Food_Pcal_R_C_Y, L105.an_kcalg_R_C_Y, L105.an_Prod_Mt_R_C_Y, L105.an_Prod_Mt_ctry_C_Y, L105.an_StockShares_R_BufGoat_2005)
  } else {
    stop("Unknown command")
  }
}



