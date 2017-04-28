#' module_aglu_LB165.ag_water_R_C_Y_GLU_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L165.BlueIrr_m3kg_R_C_GLU}, \code{L165.TotIrr_m3kg_R_C_GLU}, \code{L165.GreenRfd_m3kg_R_C_GLU}, \code{L165.ag_IrrEff_R}. The corresponding file in the
#' original data system was \code{LB165.ag_water_R_C_Y_GLU_irr.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL April 2017
#' @export
module_aglu_LB165.ag_water_R_C_Y_GLU_irr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/FAO_ag_items_PRODSTAT",
             "L100.Water_footprint_m3",
             "L100.LDS_ag_prod_t",
             FILE = "aglu/Mekonnen_Hoekstra_Rep47_A2",
             FILE = "aglu/Rohwer_2007_IrrigationEff",
             "L151.ag_irrProd_t_ctry_crop",
             "L151.ag_rfdProd_t_ctry_crop",
             "L151.ag_irrHA_ha_ctry_crop"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L165.BlueIrr_m3kg_R_C_GLU",
             "L165.TotIrr_m3kg_R_C_GLU",
             "L165.GreenRfd_m3kg_R_C_GLU",
             "L165.ag_IrrEff_R"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO_ag_items_PRODSTAT")
    L100.Water_footprint_m3 <- get_data(all_data, "L100.Water_footprint_m3")
    Mekonnen_Hoekstra_Rep47_A2 <- get_data(all_data, "aglu/Mekonnen_Hoekstra_Rep47_A2")
    Rohwer_2007_IrrigationEff <- get_data(all_data, "aglu/Rohwer_2007_IrrigationEff")
    L100.LDS_ag_prod_t <- get_data(all_data, "L100.LDS_ag_prod_t")
    L151.ag_irrProd_t_ctry_crop <- get_data(all_data, "L151.ag_irrProd_t_ctry_crop")
    L151.ag_rfdProd_t_ctry_crop <- get_data(all_data, "L151.ag_rfdProd_t_ctry_crop")
    L151.ag_irrHA_ha_ctry_crop <- get_data(all_data, "L151.ag_irrHA_ha_ctry_crop")

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
      add_legacy_name("L165.BlueIrr_m3kg_R_C_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "L100.Water_footprint_m3",
                     "L100.LDS_ag_prod_t",
                     "aglu/Mekonnen_Hoekstra_Rep47_A2",
                     "aglu/Rohwer_2007_IrrigationEff",
                     "L151.ag_irrProd_t_ctry_crop",
                     "L151.ag_rfdProd_t_ctry_crop",
                     "L151.ag_irrHA_ha_ctry_crop") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L165.BlueIrr_m3kg_R_C_GLU

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L165.TotIrr_m3kg_R_C_GLU") %>%
      same_precursors_as(L165.BlueIrr_m3kg_R_C_GLU) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L165.TotIrr_m3kg_R_C_GLU

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L165.GreenRfd_m3kg_R_C_GLU") %>%
      same_precursors_as(L165.BlueIrr_m3kg_R_C_GLU) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L165.GreenRfd_m3kg_R_C_GLU

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L165.ag_IrrEff_R") %>%
      same_precursors_as(L165.BlueIrr_m3kg_R_C_GLU) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L165.ag_IrrEff_R

    return_data(L165.BlueIrr_m3kg_R_C_GLU, L165.TotIrr_m3kg_R_C_GLU, L165.GreenRfd_m3kg_R_C_GLU, L165.ag_IrrEff_R)
  } else {
    stop("Unknown command")
  }
}
