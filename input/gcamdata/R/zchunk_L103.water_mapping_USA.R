# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L103.water_mapping_USA
#'
#' Calculate percentage shares to map water demands from USA regional level to state and basin.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L103.water_mapping_R_GLU_B_W_Ws_share}, \code{L103.water_mapping_R_LS_W_Ws_share},
#' \code{L103.water_mapping_R_B_W_Ws_share},  \code{L103.water_mapping_R_PRI_W_Ws_share}
#' There was no corresponding file in the original data system.
#' @details  Water demands by USA region / sector to basin and state.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author NTG Oct 2019
module_gcamusa_L103.water_mapping_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/irrigation_shares_0p5degree",
             FILE = "gcam-usa/nonirrigation_shares_0p5degree",
             FILE = "water/basin_to_country_mapping",
             FILE = "gcam-usa/USGS_mining_water_shares",
             FILE = "gcam-usa/USGS_livestock_water_withdrawals"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L103.water_mapping_USA_R_LS_W_Ws_share",
             "L103.water_mapping_USA_R_PRI_W_Ws_share",
             "L103.water_mapping_USA_R_GLU_W_Ws_share",
             "L103.water_mapping_USA_R_B_W_Ws_share"))
  } else if(command == driver.MAKE) {

    region <- state  <- year <- water_type <- water_sector <- demand <- demand_total <- Value <- state.to.country.share <-
      fresh.share <- saline.share <- value <- GCAM_basin_ID <- basin_name <- state_abbr <- volume <- Basin_name <-
      GLU_name <- share <- NULL        # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    irrigation_shares <- get_data(all_data, "gcam-usa/irrigation_shares_0p5degree")
    nonirrigation_shares <- get_data(all_data, "gcam-usa/nonirrigation_shares_0p5degree")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    USGS_mining_water_shares <- get_data(all_data, "gcam-usa/USGS_mining_water_shares")
    USGS_livestock_water_withdrawals <- get_data(all_data, "gcam-usa/USGS_livestock_water_withdrawals") %>%
      gather_years()

    # ===================================================
    # Data Processing

    # Livestock mappings are precalculated as total withdrawals in historical periods at the state level from USGS data.
    # Copy shares for 1990 to 1975, and then from 2015 through end of century
    USGS_livestock_water_withdrawals %>%
      select(-water_type) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      complete(nesting(state,value), year = c(max(MODEL_BASE_YEARS), MODEL_FUTURE_YEARS)) %>%
      bind_rows(
        USGS_livestock_water_withdrawals %>%
          select(-water_type) %>%
          filter(year == min(year)) %>%
          complete(nesting(state, value), year = first(MODEL_BASE_YEARS)),
        USGS_livestock_water_withdrawals %>%
          filter(year < max(MODEL_BASE_YEARS)) %>%
          select(-water_type)
      ) %>%
      group_by(year) %>%
      mutate(value = value / sum(value)) %>%
      ungroup() %>%
      arrange(state, year) %>%
      repeat_add_columns(tibble(water_type = water.MAPPED_WATER_TYPES)) %>%
      unique() ->
      L103.water_mapping_USA_R_LS_W_Ws_share

    # Mining shares are precalculated as total withdrawals in historical periods at the state level from USGS data.
    # Copy shares for 1990 to 1975, and then from 2015 through end of century
    USGS_mining_water_shares %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      complete(nesting(state, state.to.country.share, fresh.share, saline.share), year = c(max(MODEL_BASE_YEARS), MODEL_FUTURE_YEARS)) %>%
      bind_rows(
        USGS_mining_water_shares %>%
          filter(year == min(year)) %>%
          complete(nesting(state, state.to.country.share, fresh.share, saline.share), year = first(MODEL_BASE_YEARS)),
        USGS_mining_water_shares %>%
          filter(year < max(MODEL_BASE_YEARS))
      ) %>%
      arrange(state, year) %>%
      repeat_add_columns(tibble(water_type = water.MAPPED_WATER_TYPES)) %>%
      unique() ->
      L103.water_mapping_USA_R_PRI_W_Ws_share

    # Calculate shares from basin level Irrigation to state level using Huang et al. (2018) grid-level data.
    # Input file is generated from R package created by Chris Vernon with modifications by NTG
    irrigation_shares %>%
      filter(year == gcamusa.FINAL_MAPPING_YEAR) %>%
      complete(nesting(GCAM_basin_ID, basin_name, state_abbr, volume), year = MODEL_FUTURE_YEARS) %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      bind_rows(
        irrigation_shares,
        # Copy shares from 2010 to 2015
        irrigation_shares %>% filter(year==gcamusa.FINAL_MAPPING_YEAR) %>% mutate(year=max(MODEL_BASE_YEARS))
      ) %>%
      left_join_error_no_match(basin_to_country_mapping, by = "GCAM_basin_ID") %>%
      select(GLU_name, basin_name, state_abbr, volume, year) %>%
      rename(region = state_abbr) %>%
      group_by(basin_name, year) %>%
      mutate(demand_total = sum(volume),
             share = volume / demand_total) %>%
      ungroup() %>%
      filter(share > 0) %>%
      select(-volume, -demand_total) %>%
      arrange(region, year) %>%
      repeat_add_columns(tibble(water_type = water.MAPPED_WATER_TYPES)) ->
      L103.water_mapping_USA_R_GLU_W_Ws_share

    # Calculate shares from state level nonirrigation sectors to basin level using Huang et al. (2018) grid-level data.
    # Input file is generated from R package created by Chris Vernon with modifications by NTG
    nonirrigation_shares %>%
      gather(water_sector, value, -GCAM_basin_ID, -basin_name, -state_abbr, -year) %>%
      filter(year == gcamusa.FINAL_MAPPING_YEAR) %>%
      complete(nesting(GCAM_basin_ID, basin_name, state_abbr, water_sector, value), year = MODEL_FUTURE_YEARS) %>%
      bind_rows(
        nonirrigation_shares %>%
          gather(water_sector, value, -GCAM_basin_ID, -basin_name, -state_abbr, -year)
      ) %>%
      left_join_error_no_match(basin_to_country_mapping, by = "GCAM_basin_ID") %>%
      rename(region = state_abbr) %>%
      group_by(basin_name, GCAM_basin_ID, region, water_sector, year) %>%
      summarise(demand = sum(value)) %>%
      ungroup() %>%
      group_by(region, year, water_sector) %>%
      mutate(demand_total = sum(demand),
             share = demand / demand_total) %>%
      ungroup() %>%
      filter(share > 0) %>%
      select(-demand, -demand_total) %>%
      arrange(region, year) %>%
      repeat_add_columns(tibble(water_type = water.MAPPED_WATER_TYPES)) ->
      L103.water_mapping_USA_R_B_W_Ws_share

    # ===================================================
    # Produce outputs

    L103.water_mapping_USA_R_LS_W_Ws_share %>%
      add_title("Water mapping for livestock by state / water type") %>%
      add_units("NA") %>%
      add_comments("Water mapping for livestock by state / water type") %>%
      add_legacy_name("L103.water_mapping_R_B_W_Ws_share") %>%
      add_precursors("gcam-usa/USGS_livestock_water_withdrawals") ->
      L103.water_mapping_USA_R_LS_W_Ws_share

    L103.water_mapping_USA_R_PRI_W_Ws_share %>%
      add_title("Water mapping for primary energy mining by state / water type") %>%
      add_units("NA") %>%
      add_comments("Water mapping for primary energy mining by state / water type ") %>%
      add_legacy_name("L103.water_mapping_R_B_W_Ws_share") %>%
      add_precursors("gcam-usa/USGS_mining_water_shares") ->
      L103.water_mapping_USA_R_PRI_W_Ws_share

    L103.water_mapping_USA_R_GLU_W_Ws_share %>%
      add_title("Water mapping for irrigation sectors by state/ basin / water type") %>%
      add_units("NA") %>%
      add_comments("Water mapping for irrigation sectors by state/ basin / water type") %>%
      add_legacy_name("L103.water_mapping_R_B_W_Ws_share") %>%
      add_precursors("gcam-usa/irrigation_shares_0p5degree",
                     "water/basin_to_country_mapping") ->
      L103.water_mapping_USA_R_GLU_W_Ws_share

    L103.water_mapping_USA_R_B_W_Ws_share %>%
      add_title("Water mapping for nonirrigation sectors by state/ basin / water type") %>%
      add_units("NA") %>%
      add_comments("Water mapping for nonirrigation sectors by state/ basin / water type") %>%
      add_legacy_name("L103.water_mapping_R_B_W_Ws_share") %>%
      add_precursors("gcam-usa/nonirrigation_shares_0p5degree",
                     "water/basin_to_country_mapping") ->
      L103.water_mapping_USA_R_B_W_Ws_share

    return_data(L103.water_mapping_USA_R_LS_W_Ws_share,
                L103.water_mapping_USA_R_PRI_W_Ws_share,
                L103.water_mapping_USA_R_GLU_W_Ws_share,
                L103.water_mapping_USA_R_B_W_Ws_share)

  } else {
    stop("Unknown command")
  }
}
