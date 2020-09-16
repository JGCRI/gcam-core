# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L103.water_basin_mapping
#'
#' Calculate percentage shares to map water demands by region / sector to basin.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L103.water_mapping_R_GLU_B_W_Ws_share},
#' \code{L103.water_mapping_R_B_W_Ws_share}. There was no corresponding file in the
#' original data system.
#' @details  Water demands by sector and region to basins.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather
#' @author ST Oct 2018
module_water_L103.water_basin_mapping <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/basin_to_country_mapping",
             FILE = "water/nonirrigation_consumption",
             FILE = "water/nonirrigation_withdrawal",
             FILE = "common/iso_GCAM_regID",
             "L125.LC_bm2_R_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L103.water_mapping_R_GLU_B_W_Ws_share",
             "L103.water_mapping_R_B_W_Ws_share"))
  } else if(command == driver.MAKE) {

    region <- GCAM_region_ID <- GLU <- water_type <- GCAM_ID_1 <- ISO_3DIGIT <-
      iso <- water_sector <- value <- GCAM_basin_ID <- demand <- demand_total <-
      share <- NULL                      # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    nonirr_cons <- get_data(all_data, "water/nonirrigation_consumption")
    nonirr_wthd <- get_data(all_data, "water/nonirrigation_withdrawal")
    iso_GCAM_mapping <- get_data(all_data, "common/iso_GCAM_regID", strip_attributes = TRUE)
    L125.LC_bm2_R_GLU <- get_data(all_data, "L125.LC_bm2_R_GLU", strip_attributes = TRUE)

    # expand all GLUs for each water type
    expand.grid(GLU = basin_to_country_mapping$GCAM_basin_ID,
                water_type = water.MAPPED_WATER_TYPES) ->
      GLU_water_types

    # irrigation mappings
    L125.LC_bm2_R_GLU %>% select(GCAM_region_ID, GLU) %>%
      mutate(GLU = as.integer(substr(GLU, 4, nchar(GLU)))) %>%
      left_join(GLU_water_types, by = "GLU") %>%
      mutate(water_type = as.character(water_type),
             water_sector = water.IRRIGATION,
             share = water.IRR_SHARE) ->
      L103.water_mapping_R_GLU_B_W_Ws_share

    # non-irrigation mappings
    bind_rows(
      nonirr_cons %>%
        select(GCAM_ID_1, ISO_3DIGIT, one_of(water.NONIRRIGATION_SECTORS)) %>%
        mutate(water_type = "water consumption"),
      nonirr_wthd %>%
        select(GCAM_ID_1, ISO_3DIGIT, one_of(water.NONIRRIGATION_SECTORS)) %>%
        mutate(water_type = "water withdrawals")
    ) %>%
      rename(GCAM_basin_ID = GCAM_ID_1,
             iso = ISO_3DIGIT) %>%
      mutate(iso = tolower(iso)) %>%
      gather(water_sector, value, -GCAM_basin_ID, -iso, -water_type) %>%
      left_join(select(iso_GCAM_mapping, iso, GCAM_region_ID),
                by = "iso") %>%
      filter(!is.na(GCAM_region_ID)) %>%
      group_by(GCAM_region_ID, GCAM_basin_ID, water_type, water_sector) %>%
      summarise(demand = sum(value)) %>% ungroup() %>%
      group_by(GCAM_region_ID, water_type, water_sector) %>%
      mutate(demand_total = sum(demand),
             share = demand / demand_total) %>% ungroup() %>%
      filter(share > 0) %>%
      select(-demand, -demand_total) ->
      L103.water_mapping_R_B_W_Ws_share

    # ===================================================

    # Produce outputs

    L103.water_mapping_R_GLU_B_W_Ws_share %>%
      add_title("Water mapping by region / GLU / water type / water sector to basin") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L103.water_mapping_R_GLU_B_W_Ws_share") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L125.LC_bm2_R_GLU") ->
      L103.water_mapping_R_GLU_B_W_Ws_share


    L103.water_mapping_R_B_W_Ws_share %>%
      add_title("Water mapping by region / water type / water sector to basin") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L103.water_mapping_R_B_W_Ws_share") %>%
      add_precursors("water/basin_to_country_mapping",
                     "common/iso_GCAM_regID",
                     "water/nonirrigation_consumption",
                     "water/nonirrigation_withdrawal") ->
      L103.water_mapping_R_B_W_Ws_share


    return_data(L103.water_mapping_R_GLU_B_W_Ws_share,
                L103.water_mapping_R_B_W_Ws_share)


  } else {
    stop("Unknown command")
  }
}
