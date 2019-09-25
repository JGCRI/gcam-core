#' module_gcamusa_L103.water.mapping_USA
#'
#' Calculate percentage shares to map water demands from USA regional level to state and basin.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L103.water_mapping_R_GLU_B_W_Ws_share},
#' \code{L103.water_mapping_R_B_W_Ws_share}. There was no corresponding file in the
#' original data system.
#' @details  Water demands by USA region / sector to basin and state.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author NTG Aug 2019
module_gcamusa_L103.water.mapping_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/USDA_an_Stocks",
             FILE = "gcam-usa/USDA_an_items_Stocks",
             FILE = "gcam-usa/states_subregions",
             FILE = "water/LivestockWaterFootprint_MH2010",
             FILE = "gcam-usa/USGS_mining_water_shares"))
#             FILE = "gcam-usa/withdrawal_values",
#             FILE = "gcam-usa/consumption_values"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L103.water_mapping_USA_R_LS_W_Ws_share",
             "L103.water_mapping_USA_R_PRI_W_Ws_share"))
#             "L103.water_mapping_USA_R_B_W_Ws_share"))
#             "L103.water_mapping_USA_R_GLU_B_W_Ws_share"))
  } else if(command == driver.MAKE) {

    region <- Animal <- GCAM_commodity <- State <- `Data Item` <- Year <-
      Coefficient <- water_type <- water_sector <- demand <- demand_total <- Value <- NULL        # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    USDA_an_Stocks <- get_data(all_data, "gcam-usa/USDA_an_Stocks")
    USDA_an_items_Stocks <- get_data(all_data, "gcam-usa/USDA_an_items_Stocks")
    GCAM_state_names <- get_data(all_data, "gcam-usa/states_subregions")
    LivestockWaterFootprint_MH2010 <- get_data(all_data, "water/LivestockWaterFootprint_MH2010")
    USGS_mining_water_shares <- get_data(all_data, "gcam-usa/USGS_mining_water_shares")
#    withdrawal_values <- get_data(all_data, "withdrawal_values")
#    consumption_values <- get_data(all_data, "consumption_values")


    ##Add additional shareweight calcualtions either before or after the livestock calcs below

    # Livestock mappings
    ## Add Primary Energy here as it will be mapped from the USA region level to the state level
    # We create shares of livestock water demands in each state in order to map the water demands (C and W)
    # from the USA to each individual state. USDA inventories of each animal (heads) are multiplied by
    # service water use coefficients (liters/day/head) from Mekonnen and Hoekstra (2010) and summed over all animals.
    # Inventory values are averaged in the time period 1997-2002 to complement the 2000 water footprint
    # values from Mekonnen and Hoekstra (2010).
    USDA_an_Stocks %>%
      rename("state_name" = "State","year"="Year","value"="Value") %>%
      left_join_error_no_match((GCAM_state_names %>%
                                  select(state_name, state) %>%
                                  mutate(state_name = toupper(state_name))), by = c("state_name")) %>%
      # Remove Rabbits from dataset for now as they are not mapped to a Mekonnen and Hoeskstra WF type and create NAs if kept
      dplyr::filter(Commodity!=gcamusa.LIVESTOCK_NO_EXIST) %>%
      left_join_error_no_match(USDA_an_items_Stocks, by=c("Data Item")) %>%
      select(state, year, GCAM_commodity, Animal, value) %>%
      repeat_add_columns(tibble(water_type=water.MAPPED_WATER_TYPES)) %>%
      mutate(value = replace_na(value,0)) %>%
      left_join_error_no_match(LivestockWaterFootprint_MH2010, by=c("Animal")) %>%
      mutate(demand = value * Coefficient) %>%
      select(-Animal) %>%
      group_by(state,year,water_type) %>%
      summarise(demand = sum(demand)) %>% ungroup %>%
      dplyr::filter(year==gcamusa.LIVESTOCK_FIRST_YEAR|year==gcamusa.LIVESTOCK_FINAL_YEAR) %>%
      group_by(water_type,state) %>%
      summarise(demand = mean(demand)) %>% ungroup %>%
      group_by(water_type) %>%
      mutate(demand_total = sum(demand)) %>% ungroup %>%
      group_by(state,water_type) %>%
      mutate(share = demand/ demand_total) %>% ungroup %>%
      filter(share > 0) %>%
      select(-demand, -demand_total) ->
      L103.water_mapping_USA_R_LS_W_Ws_share

    USGS_mining_water_shares %>%
      repeat_add_columns(tibble(water_type=water.MAPPED_WATER_TYPES)) ->
      L103.water_mapping_USA_R_PRI_W_Ws_share


    # Calculate shares from state to basin level for Domestic, Livestock, Primary Energy, Manufacturing
    # and Electricity using withdrawal and consumption data from Huang et al. (2018)
#    bind_rows(
#      withdrawal_values %>% select(-Irrigation) %>%
#        dplyr::filter(year==gcamusa.WATER_MAPPING_YEAR,
#                      region_32_code==gcamusa.USA_REGION_NUMBER) %>%
#        mutate(water_type = "water withdrawals"),
#      consumption_values %>% select(-Irrigation) %>%
#        dplyr::filter(year==gcamusa.WATER_MAPPING_YEAR,
#                      region_32_code==gcamusa.USA_REGION_NUMBER) %>%
#        mutate(water_type = "water consumption")
#    ) %>%
#      select(-region_32_code) %>%
#      rename(basin_id = basin) %>%
#      gather(water_sector, value, -basin_id, -state, -water_type) %>%
#      group_by(state, basin_id, water_type, water_sector) %>%
#      summarise(demand = sum(value)) %>% ungroup %>%
#      group_by(state, water_type, water_sector) %>%
#      mutate(demand_total = sum(demand),
#             share = demand/demand_total) %>% ungroup() %>%
#      filter(share >0) %>%
#      select(-demand, -demand_total) ->
#      L103.water_mapping_USA_R_B_W_Ws_share

    # Calculate shares from basin level Irrigation to state level using Huang et al. (2018) grid-level data
#    bind_rows(
#      withdrawal_values %>% select(year, basin, region_32_code, Irrigation) %>%
#        dplyr::filter(year==gcamusa.WATER_MAPPING_YEAR,
#                      region_32_code==gcamusa.USA_REGION_NUMBER) %>%
#        mutate(water_type = "water withdrawals"),
#      consumption_values %>% select(year, basin, region_32_code, Irrigation) %>%
#        dplyr::filter(year==gcamusa.WATER_MAPPING_YEAR,
#                      region_32_code==gcamusa.USA_REGION_NUMBER) %>%
#        mutate(water_type = "water consumption")
#    ) %>%
#      select(-region_32_code) %>%
#      rename(basin_id = basin,
#             value = Irrigation) %>%
#      mutate(water_sector = water.IRRIGATION) %>%
#      group_by(state, basin_id, water_type) %>%
#      summarise(demand = sum(value)) %>% ungroup() %>%
#      group_by(basin_id, water_type) %>%
#      mutate(demand_total = sum(demand),
#             share = demand/demand_total) %>% ungroup() %>%
#      filter(share > 0) %>%
#      rename(GLU = basin_id) %>%
#      select(-demand, -demand_total) ->
#      L103.water_mapping_USA_R_GLU_B_W_Ws_share

    # ===================================================

    # Produce outputs


    L103.water_mapping_USA_R_LS_W_Ws_share %>%
      add_title("Water mapping for livestock by state / water type ") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L103.water_mapping_R_B_W_Ws_share") %>%
      add_precursors("gcam-usa/USDA_an_Stocks",
                     "gcam-usa/USDA_an_items_Stocks",
                     "gcam-usa/states_subregions",
                     "water/LivestockWaterFootprint_MH2010") ->
      L103.water_mapping_USA_R_LS_W_Ws_share

    L103.water_mapping_USA_R_PRI_W_Ws_share %>%
      add_title("Water mapping for primary energy mining by state / water type ") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L103.water_mapping_R_B_W_Ws_share") %>%
      add_precursors("gcam-usa/USGS_mining_water_shares") ->
      L103.water_mapping_USA_R_PRI_W_Ws_share

#    L103.water_mapping_USA_R_B_W_Ws_share %>%
#      add_title("Water mapping for nonirrigation sectors by state/ basin / water type") %>%
#      add_units("NA") %>%
#      add_comments("") %>%
#      add_legacy_name("L103.water_mapping_R_B_W_Ws_share") %>%
#      add_precursors("gcam-usa/withdrawal_values",
#                     "gcam-usa/consumption_values") ->
#      L103.water_mapping_USA_R_B_W_Ws_share

#    L103.water_mapping_USA_R_GLU_B_W_Ws_share %>%
#      add_title("Water mapping for the irrigation sectors by state/ basin / water type") %>%
#      add_units("NA") %>%
#      add_comments("") %>%
#      add_legacy_name("L103.water_mapping_R_GLU_B_W_Ws_share") %>%
#      add_precursors("gcam-usa/withdrawal_values",
#                     "gcam-usa/consumption_values") ->
#      L103.water_mapping_USA_R_GLU_B_W_Ws_share


    return_data(L103.water_mapping_USA_R_LS_W_Ws_share, L103.water_mapping_USA_R_PRI_W_Ws_share)# L103.water_mapping_USA_R_B_W_Ws_share, L103.water_mapping_USA_R_GLU_B_W_Ws_share )


  } else {
    stop("Unknown command")
  }
}
