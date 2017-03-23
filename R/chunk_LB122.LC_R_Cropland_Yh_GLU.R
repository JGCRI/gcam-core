#' module_aglu_LB122.LC_R_Cropland_Yh_GLU
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L122.ag_HA_to_CropLand_R_Y_GLU}, \code{L122.ag_EcYield_kgm2_R_C_Y_GLU}, \code{L122.LC_bm2_R_OtherArableLand_Yh_GLU}, \code{L122.LC_bm2_R_ExtraCropLand_Yh_GLU}, \code{L122.LC_bm2_R_HarvCropLand_C_Yh_GLU}, \code{L122.LC_bm2_R_HarvCropLand_Yh_GLU}. The corresponding file in the
#' original data system was \code{LB122.LC_R_Cropland_Yh_GLU.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB122.LC_R_Cropland_Yh_GLU_DISABLED <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
 "L100.FAO_fallowland_kha",
 "L100.FAO_CL_kha",
 "L100.FAO_harv_CL_kha",
 "L103.ag_HA_bm2_R_C_Y_GLU",
 "L103.ag_Prod_Mt_R_C_Y_GLU",
 "L120.LC_bm2_R_LT_Yh_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L122.ag_HA_to_CropLand_R_Y_GLU",
"L122.ag_EcYield_kgm2_R_C_Y_GLU",
"L122.LC_bm2_R_OtherArableLand_Yh_GLU",
"L122.LC_bm2_R_ExtraCropLand_Yh_GLU",
"L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
"L122.LC_bm2_R_HarvCropLand_Yh_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
      iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
  L100.FAO_fallowland_kha <- get_data(all_data, "L100.FAO_fallowland_kha")
  L100.FAO_CL_kha <- get_data(all_data, "L100.FAO_CL_kha")
  L100.FAO_harv_CL_kha <- get_data(all_data, "L100.FAO_harv_CL_kha")
  L103.ag_HA_bm2_R_C_Y_GLU <- get_data(all_data, "L103.ag_HA_bm2_R_C_Y_GLU")
  L103.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L103.ag_Prod_Mt_R_C_Y_GLU")
  L120.LC_bm2_R_LT_Yh_GLU <- get_data(all_data, "L120.LC_bm2_R_LT_Yh_GLU")

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
 add_legacy_name("L122.ag_HA_to_CropLand_R_Y_GLU") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L122.ag_HA_to_CropLand_R_Y_GLU
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L122.ag_EcYield_kgm2_R_C_Y_GLU") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L122.ag_EcYield_kgm2_R_C_Y_GLU
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L122.LC_bm2_R_OtherArableLand_Yh_GLU") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L122.LC_bm2_R_OtherArableLand_Yh_GLU
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L122.LC_bm2_R_ExtraCropLand_Yh_GLU") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L122.LC_bm2_R_ExtraCropLand_Yh_GLU
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L122.LC_bm2_R_HarvCropLand_C_Yh_GLU") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L122.LC_bm2_R_HarvCropLand_C_Yh_GLU
tibble() %>%
   add_title("descriptive title of data") %>%
 add_units("units") %>%
 add_comments("comments describing how data generated") %>%
 add_comments("can be multiple lines") %>%
 add_legacy_name("L122.LC_bm2_R_HarvCropLand_Yh_GLU") %>%
 add_precursors("precursor1", "precursor2", "etc") %>%
 # typical flags, but there are others--see `constants.R` 
 add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
   L122.LC_bm2_R_HarvCropLand_Yh_GLU

    return_data(L122.ag_HA_to_CropLand_R_Y_GLU, L122.ag_EcYield_kgm2_R_C_Y_GLU, L122.LC_bm2_R_OtherArableLand_Yh_GLU, L122.LC_bm2_R_ExtraCropLand_Yh_GLU, L122.LC_bm2_R_HarvCropLand_C_Yh_GLU, L122.LC_bm2_R_HarvCropLand_Yh_GLU)
  } else {
    stop("Unknown command")
  }
}



