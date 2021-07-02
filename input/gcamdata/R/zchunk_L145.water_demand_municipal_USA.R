# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L145.water_demand_municipal
#'
#' Generate US state municipal water withdrawals, municipal water base delivery cost, and municipal water use efficiency.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L145.municipal_water_state_W_Yh_km3}, \code{L145.municipal_water_cost_state_75USD_m3},
#' \code{L145.municipal_water_eff_state_Yh}.
#' @details Generate municipal water withdrawals, municipal water base delivery cost, and municipal water use efficiency
#' by US state and historical year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RC March 2019
module_gcamusa_L145.water_demand_municipal <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/USGS_municipal_water_withdrawal_USA",
             FILE = "gcam-usa/municipal_water_use_efficiency_USA",
             "L145.municipal_water_cost_R_75USD_m3",
             "L100.Pop_thous_state"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L145.municipal_water_state_W_Yh_km3",
             "L145.municipal_water_cost_state_75USD_m3",
             "L145.municipal_water_eff_state_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    GCAM_region_ID <- year <- value <- water_pc <- water_km3 <- water_type <-
      sector <- state <- input.cost <- NULL  # silence package check notes

    # Load required inputs
    USGS_municipal_water_withdrawal_USA <- get_data(all_data, "gcam-usa/USGS_municipal_water_withdrawal_USA") %>%
      gather_years()
    municipal_water_use_efficiency_USA <- get_data(all_data, "gcam-usa/municipal_water_use_efficiency_USA") %>%
      gather_years()
    L145.municipal_water_cost_R_75USD_m3 <- get_data(all_data, "L145.municipal_water_cost_R_75USD_m3", strip_attributes = TRUE)
    L100.Pop_thous_state <- get_data(all_data, "L100.Pop_thous_state")

    # Complete municipal water demand data, using linear interpolation of per-capita demands in years between known data points,
    # and constant per-capita demands in years outside of the known data points.
    USGS_municipal_water_withdrawal_USA %>%
      rename(water_km3 = value) %>%
      complete(nesting(state, water_type), year = HISTORICAL_YEARS) %>%
      left_join_error_no_match(L100.Pop_thous_state, by = c("state", "year")) %>%
      # Calculate per capita water demand
      mutate(water_pc = water_km3 / value) %>%
      group_by(state) %>%
      mutate(water_pc = approx_fun(year, water_pc, rule = 2),
             water_km3 = replace(water_km3, is.na(water_km3), water_pc[is.na(water_km3)] * value[is.na(water_km3)]),
             sector = gcamusa.MUNICIPAL_SECTOR) %>%
      ungroup %>%
      select(state, sector, water_type, year, value = water_km3) ->
      L145.municipal_water_state_W_Yh_km3

    # Use the US national cost from GCAM-core for all states
    L145.municipal_water_cost_R_75USD_m3 %>%
      # Filter USA national data
      filter(GCAM_region_ID == gcam.USA_CODE) %>%
      # Assign the same cost value to all US states
      repeat_add_columns(tibble(state = gcamusa.STATES)) %>%
      select(state, input.cost) ->
      L145.municipal_water_cost_state_75USD_m3

    # Interpolate water efficiency to all historical years
    municipal_water_use_efficiency_USA %>%
      group_by(state) %>%
      complete(year = HISTORICAL_YEARS) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup ->
      L145.municipal_water_eff_state_Yh

    # Produce outputs
    L145.municipal_water_state_W_Yh_km3 %>%
      add_title("Municipal water withdrawals by US state for all historical years ") %>%
      add_units("km3") %>%
      add_comments("FAO_municipal_water_AQUASTAT by country interpolated linearly on a per-capita basis") %>%
      add_legacy_name("L145.municipal_water_state_W_Yh_km3") %>%
      add_precursors("gcam-usa/USGS_municipal_water_withdrawal_USA",
                     "L100.Pop_thous_state") ->
      L145.municipal_water_state_W_Yh_km3

    L145.municipal_water_eff_state_Yh %>%
      add_title("Municipal water use efficiency by US state for all years") %>%
      add_units("ratio") %>%
      add_comments("Efficiency data are interpolated to all historical years") %>%
      add_legacy_name("L145.municipal_water_eff_state_Yh") %>%
      add_precursors("gcam-usa/municipal_water_use_efficiency_USA") ->
      L145.municipal_water_eff_state_Yh

    L145.municipal_water_cost_state_75USD_m3 %>%
      add_title("Municipal water base deleivery cost by US state") %>%
      add_units("1975$/m3") %>%
      add_comments("Use the same USA national average cost data from IBNET for all states") %>%
      add_legacy_name("L145.municipal_water_cost_state_75USD_m3") %>%
      add_precursors("L145.municipal_water_cost_R_75USD_m3") ->
      L145.municipal_water_cost_state_75USD_m3

    return_data(L145.municipal_water_state_W_Yh_km3, L145.municipal_water_eff_state_Yh, L145.municipal_water_cost_state_75USD_m3)

  } else {
    stop("Unknown command")
  }
}
