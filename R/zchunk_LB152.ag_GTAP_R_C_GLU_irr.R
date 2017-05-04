#' module_aglu_LB152.ag_GTAP_R_C_GLU_irr
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L152.ag_irrHA_bm2_R_C_GLU}, \code{L152.ag_rfdHA_bm2_R_C_GLU}, \code{L152.ag_irrProd_Mt_R_C_GLU}, \code{L152.ag_rfdProd_Mt_R_C_GLU}. The corresponding file in the
#' original data system was \code{LB152.ag_GTAP_R_C_GLU_irr.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB152.ag_GTAP_R_C_GLU_irr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/FAO_ag_items_PRODSTAT",
             "L151.ag_irrHA_ha_ctry_crop",
             "L151.ag_rfdHA_ha_ctry_crop",
             "L151.ag_irrProd_t_ctry_crop",
             "L151.ag_rfdProd_t_ctry_crop"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L152.ag_irrHA_bm2_R_C_GLU",
             "L152.ag_rfdHA_bm2_R_C_GLU",
             "L152.ag_irrProd_Mt_R_C_GLU",
             "L152.ag_rfdProd_Mt_R_C_GLU"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO_ag_items_PRODSTAT")
    L151.ag_irrHA_ha_ctry_crop <- get_data(all_data, "L151.ag_irrHA_ha_ctry_crop")
    L151.ag_rfdHA_ha_ctry_crop <- get_data(all_data, "L151.ag_rfdHA_ha_ctry_crop")
    L151.ag_irrProd_t_ctry_crop <- get_data(all_data, "L151.ag_irrProd_t_ctry_crop")
    L151.ag_rfdProd_t_ctry_crop <- get_data(all_data, "L151.ag_rfdProd_t_ctry_crop")

    # Add region and crop lookup vectors to GTAP tables
    # Collaps ag commodity data into GCAM regions and commodities, and convert to appropriate units (bm2 and Mt)
    L151.ag_irrHA_ha_ctry_crop %>%
      left_join_keep_first_only(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%                      # Match country iso with GCAM region
      left_join_keep_first_only(select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity), by = "GTAP_crop") %>%   # Match GTAP crop with GCAM commodity
      na.omit %>%                                                                                                 # Drop "NatRubber" not belong to any GCAM commodity
      mutate(irrHA = irrHA * CONV_HA_BM2) %>%                                                                     # Convert the harvest area unit from hectare to billion m2
      group_by(GCAM_region_ID, GCAM_commodity, GLU) %>%                                                           # Aggregate to GCAM region and commodity by each GLU
      summarise(irrHA = sum(irrHA)) %>%
      ungroup() ->
      L152.ag_irrHA_bm2_R_C_GLU

    L151.ag_rfdHA_ha_ctry_crop %>%
      left_join_keep_first_only(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%                      # Match country iso with GCAM region
      left_join_keep_first_only(select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity), by = "GTAP_crop") %>%   # Match GTAP crop with GCAM commodity
      na.omit %>%                                                                                                 # Drop "NatRubber" not belong to any GCAM commodity
      mutate(rfdHA = rfdHA * CONV_HA_BM2) %>%                                                                     # Convert the harvest area unit from hectare to billion m2
      group_by(GCAM_region_ID, GCAM_commodity, GLU) %>%                                                           # Aggregate to GCAM region and commodity by each GLU
      summarise(rfdHA = sum(rfdHA)) %>%
      ungroup() ->
      L152.ag_rfdHA_bm2_R_C_GLU

    L151.ag_irrProd_t_ctry_crop %>%
      left_join_keep_first_only(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%                      # Match country iso with GCAM region
      left_join_keep_first_only(select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity), by = "GTAP_crop") %>%   # Match GTAP crop with GCAM commodity
      na.omit %>%                                                                                                 # Drop "NatRubber" not belong to any GCAM commodity
      mutate(irrProd = irrProd * CONV_TON_MEGATON) %>%                                                            # Convert the production unit from ton to megaton
      group_by(GCAM_region_ID, GCAM_commodity, GLU) %>%                                                           # Aggregate to GCAM region and commodity by each GLU
      summarise(irrProd = sum(irrProd)) %>%
      ungroup() ->
      L152.ag_irrProd_Mt_R_C_GLU

    L151.ag_rfdProd_t_ctry_crop %>%
      left_join_keep_first_only(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%                      # Match country iso with GCAM region
      left_join_keep_first_only(select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity), by = "GTAP_crop") %>%   # Match GTAP crop with GCAM commodity
      na.omit %>%                                                                                                 # Drop "NatRubber" not belong to any GCAM commodity
      mutate(rfdProd = rfdProd * CONV_TON_MEGATON) %>%                                                            # Convert the production unit from ton to megaton
      group_by(GCAM_region_ID, GCAM_commodity, GLU) %>%                                                           # Aggregate to GCAM region and commodity by each GLU
      summarise(rfdProd = sum(rfdProd)) %>%
      ungroup() ->
      L152.ag_rfdProd_Mt_R_C_GLU

    # Produce outputs
    L152.ag_irrHA_bm2_R_C_GLU %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L152.ag_irrHA_bm2_R_C_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "L151.ag_irrHA_ha_ctry_crop") ->
      L152.ag_irrHA_bm2_R_C_GLU

    L152.ag_rfdHA_bm2_R_C_GLU %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L152.ag_rfdHA_bm2_R_C_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "L151.ag_rfdHA_ha_ctry_crop") ->
      L152.ag_rfdHA_bm2_R_C_GLU

    L152.ag_irrProd_Mt_R_C_GLU %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L152.ag_irrProd_Mt_R_C_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "L151.ag_irrProd_t_ctry_crop") ->
      L152.ag_irrProd_Mt_R_C_GLU

    L152.ag_rfdProd_Mt_R_C_GLU %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L152.ag_rfdProd_Mt_R_C_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO_ag_items_PRODSTAT",
                     "L151.ag_rfdProd_t_ctry_crop") ->
      L152.ag_rfdProd_Mt_R_C_GLU

    return_data(L152.ag_irrHA_bm2_R_C_GLU, L152.ag_rfdHA_bm2_R_C_GLU, L152.ag_irrProd_Mt_R_C_GLU, L152.ag_rfdProd_Mt_R_C_GLU)
  } else {
    stop("Unknown command")
  }
}
