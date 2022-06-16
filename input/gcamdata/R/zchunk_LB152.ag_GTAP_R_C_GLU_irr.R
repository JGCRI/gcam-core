# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LB152.ag_GTAP_R_C_GLU_irr
#'
#' Aggregate the irrigated/rainfed harvest area and production data by GCAM region / commodity / GLU.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L152.ag_irrHA_bm2_R_C_GLU}, \code{L152.ag_rfdHA_bm2_R_C_GLU}, \code{L152.ag_irrProd_Mt_R_C_GLU}, \code{L152.ag_rfdProd_Mt_R_C_GLU}. The corresponding file in the
#' original data system was \code{LB152.ag_GTAP_R_C_GLU_irr.R} (aglu level1).
#' @details This chunk aggregates the irrigated/rainfed harvest area and production data from country and GTAP crop to GCAM region and commodity by each GLU.
#' @importFrom assertthat assert_that
#' @importFrom dplyr group_by left_join mutate select summarise
#' @author RC May 2017
module_aglu_LB152.ag_GTAP_R_C_GLU_irr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
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

    iso <- GCAM_region_ID <- GTAP_crop <- GCAM_commodity <- irrHA <- GLU <-
        rfdHA <- irrProd <- rfdProd <- GCAM_subsector <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_items_PRODSTAT")
    L151.ag_irrHA_ha_ctry_crop <- get_data(all_data, "L151.ag_irrHA_ha_ctry_crop")
    L151.ag_rfdHA_ha_ctry_crop <- get_data(all_data, "L151.ag_rfdHA_ha_ctry_crop")
    L151.ag_irrProd_t_ctry_crop <- get_data(all_data, "L151.ag_irrProd_t_ctry_crop")
    L151.ag_rfdProd_t_ctry_crop <- get_data(all_data, "L151.ag_rfdProd_t_ctry_crop")

    # For each of the four GTAP tables - irrigated harvest area, rainfed harvest area, irrigated production, and rainfed production:
    # First add GCAM region and commodity lookup vectors to iso and GTAP crop.
    # Second, convert to appropriate units (bm2 and Mt).
    # Third, aggregate Ag commodity data into GCAM regions and commodities by GLU.

    # Irrigated harvest area
    L151.ag_irrHA_ha_ctry_crop %>%
      left_join_keep_first_only(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%                      # Match country iso with GCAM region
      left_join_keep_first_only(select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity, GCAM_subsector), by = "GTAP_crop") %>%   # Match GTAP crop with GCAM commodity
      na.omit %>%                                                                                                 # Drop "NatRubber" not belong to any GCAM commodity
      mutate(irrHA = irrHA * CONV_HA_BM2) %>%                                                                     # Convert the harvest area unit from hectare to billion m2
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%                                                           # Aggregate to GCAM region and commodity by each GLU
      summarise(irrHA = sum(irrHA)) %>%
      ungroup() ->
      L152.ag_irrHA_bm2_R_C_GLU

    # Rainfed harvest area
    L151.ag_rfdHA_ha_ctry_crop %>%
      left_join_keep_first_only(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%                      # Match country iso with GCAM region
      left_join_keep_first_only(select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity, GCAM_subsector), by = "GTAP_crop") %>%   # Match GTAP crop with GCAM commodity
      na.omit %>%                                                                                                 # Drop "NatRubber" not belong to any GCAM commodity
      mutate(rfdHA = rfdHA * CONV_HA_BM2) %>%                                                                     # Convert the harvest area unit from hectare to billion m2
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%                                                           # Aggregate to GCAM region and commodity by each GLU
      summarise(rfdHA = sum(rfdHA)) %>%
      ungroup() ->
      L152.ag_rfdHA_bm2_R_C_GLU

    # Irrigated production
    L151.ag_irrProd_t_ctry_crop %>%
      left_join_keep_first_only(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%                      # Match country iso with GCAM region
      left_join_keep_first_only(select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity, GCAM_subsector), by = "GTAP_crop") %>%   # Match GTAP crop with GCAM commodity
      na.omit %>%                                                                                                 # Drop "NatRubber" not belong to any GCAM commodity
      mutate(irrProd = irrProd * CONV_TON_MEGATON) %>%                                                            # Convert the production unit from ton to megaton
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%                                                           # Aggregate to GCAM region and commodity by each GLU
      summarise(irrProd = sum(irrProd)) %>%
      ungroup() ->
      L152.ag_irrProd_Mt_R_C_GLU

    # Rainfed production
    L151.ag_rfdProd_t_ctry_crop %>%
      left_join_keep_first_only(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%                      # Match country iso with GCAM region
      left_join_keep_first_only(select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity, GCAM_subsector), by = "GTAP_crop") %>%   # Match GTAP crop with GCAM commodity
      na.omit %>%                                                                                                 # Drop "NatRubber" not belong to any GCAM commodity
      mutate(rfdProd = rfdProd * CONV_TON_MEGATON) %>%                                                            # Convert the production unit from ton to megaton
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%                                                           # Aggregate to GCAM region and commodity by each GLU
      summarise(rfdProd = sum(rfdProd)) %>%
      ungroup() ->
      L152.ag_rfdProd_Mt_R_C_GLU

    # Produce outputs
    L152.ag_irrHA_bm2_R_C_GLU %>%
      add_title("Irrigated harvested area by GCAM region / commodity / GLU") %>%
      add_units("bm2") %>%
      add_comments("Irrigated harvested area data are aggregated from country and GTAP crop to GCAM region and commodity by each GLU") %>%
      add_legacy_name("L152.ag_irrHA_bm2_R_C_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "L151.ag_irrHA_ha_ctry_crop") ->
      L152.ag_irrHA_bm2_R_C_GLU

    L152.ag_rfdHA_bm2_R_C_GLU %>%
      add_title("Rainfed harvested area by GCAM region / commodity / GLU") %>%
      add_units("bm2") %>%
      add_comments("Rainfed harvested area data are aggregated from country and GTAP crop to GCAM region and commodity by each GLU") %>%
      add_legacy_name("L152.ag_rfdHA_bm2_R_C_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "L151.ag_rfdHA_ha_ctry_crop") ->
      L152.ag_rfdHA_bm2_R_C_GLU

    L152.ag_irrProd_Mt_R_C_GLU %>%
      add_title("Irrigated crop production by GCAM region / commodity / GLU") %>%
      add_units("Mt") %>%
      add_comments("Irrigated crop production data are aggregated from country and GTAP crop to GCAM region and commodity by each GLU") %>%
      add_legacy_name("L152.ag_irrProd_Mt_R_C_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "L151.ag_irrProd_t_ctry_crop") ->
      L152.ag_irrProd_Mt_R_C_GLU

    L152.ag_rfdProd_Mt_R_C_GLU %>%
      add_title("Rainfed crop production by GCAM region / commodity / GLU") %>%
      add_units("Mt") %>%
      add_comments("Rainfed crop production data are aggregated from country and GTAP crop to GCAM region and commodity by each GLUd") %>%
      add_legacy_name("L152.ag_rfdProd_Mt_R_C_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "L151.ag_rfdProd_t_ctry_crop") ->
      L152.ag_rfdProd_Mt_R_C_GLU

    return_data(L152.ag_irrHA_bm2_R_C_GLU, L152.ag_rfdHA_bm2_R_C_GLU, L152.ag_irrProd_Mt_R_C_GLU, L152.ag_rfdProd_Mt_R_C_GLU)
  } else {
    stop("Unknown command")
  }
}
