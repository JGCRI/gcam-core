#' module_aglu_LB142.ag_Fert_IO_R_C_Y_GLU
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L142.ag_Fert_Prod_MtN_ctry_Y}, \code{L142.ag_Fert_NetExp_MtN_R_Y}, \code{L142.ag_Fert_IO_R_C_Y_GLU}. The corresponding file in the
#' original data system was \code{LB142.ag_Fert_IO_R_C_Y_GLU.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB142.ag_Fert_IO_R_C_Y_GLU_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/AGLU_ctry",
             FILE = "aglu/FAO_ag_items_PRODSTAT",
             "L100.LDS_ag_prod_t",
             "L100.FAO_ag_Prod_t",
             "L100.FAO_Fert_Cons_tN",
             "L100.FAO_Fert_Prod_tN",
             "L141.ag_Fert_Cons_MtN_ctry_crop"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L142.ag_Fert_Prod_MtN_ctry_Y",
             "L142.ag_Fert_NetExp_MtN_R_Y",
             "L142.ag_Fert_IO_R_C_Y_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO_ag_items_PRODSTAT")
    L100.LDS_ag_prod_t <- get_data(all_data, "L100.LDS_ag_prod_t")
    L100.FAO_ag_Prod_t <- get_data(all_data, "L100.FAO_ag_Prod_t")
    L100.FAO_Fert_Cons_tN <- get_data(all_data, "L100.FAO_Fert_Cons_tN")
    L100.FAO_Fert_Prod_tN <- get_data(all_data, "L100.FAO_Fert_Prod_tN")
    L141.ag_Fert_Cons_MtN_ctry_crop <- get_data(all_data, "L141.ag_Fert_Cons_MtN_ctry_crop")

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
    # NOTE: This code uses translate_to_full_table
    # This function can be removed; see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
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
      add_legacy_name("L142.ag_Fert_Prod_MtN_ctry_Y") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L142.ag_Fert_Prod_MtN_ctry_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L142.ag_Fert_NetExp_MtN_R_Y") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L142.ag_Fert_NetExp_MtN_R_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L142.ag_Fert_IO_R_C_Y_GLU") %>%
      add_precursors("precursor1", "precursor2", "etc") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L142.ag_Fert_IO_R_C_Y_GLU

    return_data(L142.ag_Fert_Prod_MtN_ctry_Y, L142.ag_Fert_NetExp_MtN_R_Y, L142.ag_Fert_IO_R_C_Y_GLU)
  } else {
    stop("Unknown command")
  }
}
