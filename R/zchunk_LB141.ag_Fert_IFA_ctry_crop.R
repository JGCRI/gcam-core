#' module_aglu_LB141.ag_Fert_IFA_ctry_crop
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L141.ag_Fert_Cons_MtN_ctry_crop}. The corresponding file in the
#' original data system was \code{LB141.ag_Fert_IFA_ctry_crop.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB141.ag_Fert_IFA_ctry_crop <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "aglu/FAO_ag_items_PRODSTAT",
             FILE = "common/iso_GCAM_regID",
             FILE = "aglu/AGLU_ctry",
             "L100.LDS_ag_HA_ha",
             "L101.ag_HA_bm2_R_C_Y",
             "L102.ag_HA_bm2_R_C_GLU",
             FILE = "aglu/IFA2002_Fert_ktN",
             FILE = "aglu/IFA_Fert_ktN"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L141.ag_Fert_Cons_MtN_ctry_crop"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO_ag_items_PRODSTAT")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    L100.LDS_ag_HA_ha <- get_data(all_data, "L100.LDS_ag_HA_ha")
    L101.ag_HA_bm2_R_C_Y <- get_data(all_data, "L101.ag_HA_bm2_R_C_Y")
    L102.ag_HA_bm2_R_C_GLU <- get_data(all_data, "L102.ag_HA_bm2_R_C_GLU")
    IFA2002_Fert_ktN <- get_data(all_data, "aglu/IFA2002_Fert_ktN")
    IFA_Fert_ktN <- get_data(all_data, "aglu/IFA_Fert_ktN")

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
    # NOTE: there are `merge` calls in this code. Be careful!
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
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
      add_legacy_name("L141.ag_Fert_Cons_MtN_ctry_crop") %>%
      add_precursors("aglu/FAO_ag_items_PRODSTAT",
                     "common/iso_GCAM_regID",
                     "aglu/AGLU_ctry",
                     "L100.LDS_ag_HA_ha",
                     "L101.ag_HA_bm2_R_C_Y",
                     "L102.ag_HA_bm2_R_C_GLU",
                     "aglu/IFA2002_Fert_ktN",
                     "aglu/IFA_Fert_ktN") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L141.ag_Fert_Cons_MtN_ctry_crop

    return_data(L141.ag_Fert_Cons_MtN_ctry_crop)
  } else {
    stop("Unknown command")
  }
}
