#' module_aglu_LB132.ag_an_For_Prices_USA_C_2005
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L132.ag_an_For_Prices}. The corresponding file in the
#' original data system was \code{LB132.ag_an_For_Prices_USA_C_2005.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB132.ag_an_For_Prices_USA_C_2005_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "aglu/FAO_ag_items_PRODSTAT",
             FILE = "aglu/FAO_an_items_PRODSTAT",
             FILE = "aglu/FAO_USA_ag_an_P_USDt_PRICESTAT",
             FILE = "aglu/FAO_USA_For_Exp_t_USD_FORESTAT",
             FILE = "aglu/USDA_Alfalfa_prices_USDt",
             "L100.FAO_ag_Prod_t",
             FILE = "aglu/FAO_USA_an_Prod_t_PRODSTAT"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return("L132.ag_an_For_Prices")
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO_ag_items_PRODSTAT")
    FAO_an_items_PRODSTAT <- get_data(all_data, "aglu/FAO_an_items_PRODSTAT")
    FAO_USA_ag_an_P_USDt_PRICESTAT <- get_data(all_data, "aglu/FAO_USA_ag_an_P_USDt_PRICESTAT")
    FAO_USA_For_Exp_t_USD_FORESTAT <- get_data(all_data, "aglu/FAO_USA_For_Exp_t_USD_FORESTAT")
    USDA_Alfalfa_prices_USDt <- get_data(all_data, "aglu/USDA_Alfalfa_prices_USDt")
    L100.FAO_ag_Prod_t <- get_data(all_data, "L100.FAO_ag_Prod_t")
    FAO_USA_an_Prod_t_PRODSTAT <- get_data(all_data, "aglu/FAO_USA_an_Prod_t_PRODSTAT")

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
      add_legacy_name("L132.ag_an_For_Prices") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L132.ag_an_For_Prices

    return_data(L132.ag_an_For_Prices)
  } else {
    stop("Unknown command")
  }
}
