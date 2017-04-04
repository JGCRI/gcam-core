#' module_aglu_LB120.LC_GIS_R_LTgis_Yh_GLU
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L120.LC_bm2_R_LT_Yh_GLU}, \code{L120.LC_bm2_R_UrbanLand_Yh_GLU}, \code{L120.LC_bm2_R_Tundra_Yh_GLU}, \code{L120.LC_bm2_R_RckIceDsrt_Yh_GLU}, \code{L120.LC_bm2_ctry_LTsage_GLU}, \code{L120.LC_bm2_ctry_LTpast_GLU}. The corresponding file in the
#' original data system was \code{LB120.LC_GIS_R_LTgis_Yh_GLU.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL April 2017
#' @export
module_aglu_LB120.LC_GIS_R_LTgis_Yh_GLU <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/LDS/LDS_land_types",
             FILE = "aglu/SAGE_LT",
             "L100.Land_type_area_ha"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L120.LC_bm2_R_LT_Yh_GLU",
             "L120.LC_bm2_R_UrbanLand_Yh_GLU",
             "L120.LC_bm2_R_Tundra_Yh_GLU",
             "L120.LC_bm2_R_RckIceDsrt_Yh_GLU",
             "L120.LC_bm2_ctry_LTsage_GLU",
             "L120.LC_bm2_ctry_LTpast_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    LDS_land_types <- get_data(all_data, "aglu/LDS/LDS_land_types")
    SAGE_LT <- get_data(all_data, "aglu/SAGE_LT")
    L100.Land_type_area_ha <- get_data(all_data, "L100.Land_type_area_ha")


    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L120.LC_bm2_R_LT_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT") %>%
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L120.LC_bm2_R_LT_Yh_GLU
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L120.LC_bm2_R_UrbanLand_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT") %>%
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L120.LC_bm2_R_UrbanLand_Yh_GLU
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L120.LC_bm2_R_Tundra_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT") %>%
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L120.LC_bm2_R_Tundra_Yh_GLU
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L120.LC_bm2_R_RckIceDsrt_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT") %>%
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L120.LC_bm2_R_RckIceDsrt_Yh_GLU
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L120.LC_bm2_ctry_LTsage_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT") %>%
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L120.LC_bm2_ctry_LTsage_GLU
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L120.LC_bm2_ctry_LTpast_GLU") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/LDS/LDS_land_types", "aglu/SAGE_LT") %>%
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L120.LC_bm2_ctry_LTpast_GLU

    return_data(L120.LC_bm2_R_LT_Yh_GLU, L120.LC_bm2_R_UrbanLand_Yh_GLU, L120.LC_bm2_R_Tundra_Yh_GLU, L120.LC_bm2_R_RckIceDsrt_Yh_GLU, L120.LC_bm2_ctry_LTsage_GLU, L120.LC_bm2_ctry_LTpast_GLU)
  } else {
    stop("Unknown command")
  }
}



