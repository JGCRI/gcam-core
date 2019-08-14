#' module_gcamusa_L103.water.basin_mapping
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
#' @details  Water demands to by region / sector to basin.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author ST Oct 2018, NTG Aug 2019
module_gcamusa_L103.water.basin_mapping <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/basin_ID",
             FILE = "common/iso_GCAM_regID",
             FILE = "gcam-usa/USDA_an_Stocks",
             FILE = "gcam-usa/USDA_an_items_Stocks",
             FILE = "gcam-usa/states_subregions",
             FILE = "water/LivestockWaterFootprint_MH2010",
             "L125.LC_bm2_R_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L103.water_mapping_USA_R_LS_W_Ws_share"))
  } else if(command == driver.MAKE) {

    region <- Animal <- GCAM_commodity <- State <- `Data Item` <- Year <-
      Coefficient <- water_type <- demand <- demand_total <- Value<- NULL                      # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    basin_ids <- get_data(all_data, "water/basin_ID")
    iso_GCAM_mapping <- get_data(all_data, "common/iso_GCAM_regID")
    USDA_an_Stocks <- get_data(all_data, "gcam-usa/USDA_an_Stocks")
    USDA_an_items_Stocks <- get_data(all_data, "gcam-usa/USDA_an_items_Stocks")
    GCAM_state_names <- get_data(all_data, "gcam-usa/states_subregions")
    LivestockWaterFootprint_MH2010 <- get_data(all_data, "water/LivestockWaterFootprint_MH2010")
    L125.LC_bm2_R_GLU <- get_data(all_data, "L125.LC_bm2_R_GLU")

    # livestock mappings
    USDA_an_Stocks %>%
      rename("state_name" = "State","year"="Year","value"="Value") %>%
      left_join((GCAM_state_names %>%
                   select(state_name, state) %>%
                   mutate(state_name = toupper(state_name))), by = c("state_name")) %>%
      left_join(USDA_an_items_Stocks, by=c("Data Item")) %>%
      select(state, year, GCAM_commodity, Animal, value) %>%
      repeat_add_columns(tibble(water_type=water.MAPPED_WATER_TYPES)) %>%
      mutate(value = replace_na(value,0)) %>%
      left_join(LivestockWaterFootprint_MH2010, by=c("Animal")) %>%
      mutate(demand = value * Coefficient) %>%
      select(-Animal) %>%
      group_by(state,year,water_type) %>%
      summarise(demand = sum(demand)) %>% ungroup %>%
      dplyr::filter(year==1997|year==2002) %>%
      group_by(water_type,state) %>%
      summarise(demand = mean(demand)) %>% ungroup %>%
      group_by(water_type) %>%
      mutate(demand_total = sum(demand)) %>% ungroup %>%
      group_by(state,water_type) %>%
      mutate(share = demand/ demand_total) %>% ungroup %>%
      filter(share > 0) %>%
      select(-demand, -demand_total) ->
      L103.water_mapping_USA_R_LS_W_Ws_share

    # ===================================================

    # Produce outputs


    L103.water_mapping_USA_R_LS_W_Ws_share %>%
      add_title("Water mapping for livestock by state / water type ") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L103.water_mapping_R_B_W_Ws_share") %>%
      add_precursors("water/basin_ID",
                     "common/iso_GCAM_regID",
                     "gcam-usa/USDA_an_Stocks",
                     "gcam-usa/USDA_an_items_Stocks",
                     "gcam-usa/states_subregions",
                     "water/LivestockWaterFootprint_MH2010",
                     "L125.LC_bm2_R_GLU") ->
      L103.water_mapping_USA_R_LS_W_Ws_share


    return_data(L103.water_mapping_USA_R_LS_W_Ws_share)


  } else {
    stop("Unknown command")
  }
}
