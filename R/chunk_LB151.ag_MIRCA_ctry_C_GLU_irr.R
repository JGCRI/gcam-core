#' module_aglu_LB151.ag_MIRCA_ctry_C_GLU_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L151.ag_irrHA_ha_ctry_crop}, \code{L151.ag_rfdHA_ha_ctry_crop}, \code{L151.ag_irrProd_t_ctry_crop}, \code{L151.ag_rfdProd_t_ctry_crop}. The corresponding file in the
#' original data system was \code{LB151.ag_MIRCA_ctry_C_GLU_irr.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB151.ag_MIRCA_ctry_C_GLU_irr_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
FILE = "aglu/AGLU_ctry",
FILE = "aglu/FAO_ag_items_PRODSTAT",
FILE = "aglu/FAO_ag_CROSIT",
 "L100.LDS_ag_HA_ha",
 "L100.LDS_ag_prod_t",
 "L100.MIRCA_irrHA_ha",
 "L100.MIRCA_rfdHA_ha"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L151.ag_irrHA_ha_ctry_crop",
"L151.ag_rfdHA_ha_ctry_crop",
"L151.ag_irrProd_t_ctry_crop",
"L151.ag_rfdProd_t_ctry_crop"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
  AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
  FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO_ag_items_PRODSTAT")
  FAO_ag_CROSIT <- get_data(all_data, "aglu/FAO_ag_CROSIT")
  L100.LDS_ag_HA_ha <- get_data(all_data, "L100.LDS_ag_HA_ha")
  L100.LDS_ag_prod_t <- get_data(all_data, "L100.LDS_ag_prod_t")
  L100.MIRCA_irrHA_ha <- get_data(all_data, "L100.MIRCA_irrHA_ha")
  L100.MIRCA_rfdHA_ha <- get_data(all_data, "L100.MIRCA_rfdHA_ha")

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
 add_legacy_name("L151.ag_irrHA_ha_ctry_crop") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L151.ag_irrHA_ha_ctry_crop
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L151.ag_rfdHA_ha_ctry_crop") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L151.ag_rfdHA_ha_ctry_crop
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L151.ag_irrProd_t_ctry_crop") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L151.ag_irrProd_t_ctry_crop
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L151.ag_rfdProd_t_ctry_crop") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L151.ag_rfdProd_t_ctry_crop

    return_data(L151.ag_irrHA_ha_ctry_crop, L151.ag_rfdHA_ha_ctry_crop, L151.ag_irrProd_t_ctry_crop, L151.ag_rfdProd_t_ctry_crop)
  } else {
    stop("Unknown command")
  }
}



