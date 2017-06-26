#' module_aglu_LB164.ag_Costs_USA_C_2005_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L164.ag_Cost_75USDkg_C}. The corresponding file in the
#' original data system was \code{LB164.ag_Costs_USA_C_2005_irr.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB164.ag_Costs_USA_C_2005_irr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/USDA_crops",
             FILE = "aglu/USDA_item_cost",
             FILE = "aglu/USDA_cost_data",
             "L100.LDS_ag_HA_ha",
             "L133.ag_Cost_75USDkg_C",
             "L161.ag_irrHA_frac_R_C_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L164.ag_Cost_75USDkg_C"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    USDA_crops <- get_data(all_data, "aglu/USDA_crops")
    USDA_item_cost <- get_data(all_data, "aglu/USDA_item_cost")
    USDA_cost_data <- get_data(all_data, "aglu/USDA_cost_data")
    L100.LDS_ag_HA_ha <- get_data(all_data, "L100.LDS_ag_HA_ha")
    L133.ag_Cost_75USDkg_C <- get_data(all_data, "L133.ag_Cost_75USDkg_C")
    L161.ag_irrHA_frac_R_C_GLU <- get_data(all_data, "L161.ag_irrHA_frac_R_C_GLU")

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
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L164.ag_Cost_75USDkg_C") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/USDA_crops",
                     "aglu/USDA_item_cost",
                     "aglu/USDA_cost_data",
                     "L100.LDS_ag_HA_ha",
                     "L133.ag_Cost_75USDkg_C",
                     "L161.ag_irrHA_frac_R_C_GLU") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L164.ag_Cost_75USDkg_C

    return_data(L164.ag_Cost_75USDkg_C)
  } else {
    stop("Unknown command")
  }
}
