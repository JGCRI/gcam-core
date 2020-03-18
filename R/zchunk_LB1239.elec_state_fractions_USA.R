# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LB1239.elec_state_fractions_USA
#'
#' Map electricity generation by fuel | grid region | horizontal segment to generation by fuel | state | segment.
#' The fraction of generation by fuel by horizontal segment is assumed to be equal for all states within a grid region.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1239.state_elec_supply_USA}.
#'
#' The corresponding file in the original data system was \code{LB1239.elec_state_fractions.R} (gcam-usa level1).
#' @details Calculates the fraction of electricity generation by fuel, by horizontal load segment, by state.
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter left_join mutate select semi_join
#' @author MTB August 2018
module_gcamusa_LB1239.elec_state_fractions_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             "L123.out_EJ_state_elec_F",
             "L1236.grid_elec_supply_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1239.state_elec_supply_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    grid_region <- segment <- fuel <- year <- tot_generation <- fraction <- generation <-
      state <- sector <- value <- NULL # silence package check notes

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    L123.out_EJ_state_elec_F <- get_data(all_data, "L123.out_EJ_state_elec_F")
    L1236.grid_elec_supply_USA <- get_data(all_data, "L1236.grid_elec_supply_USA")

    # ===================================================
    # Data Processing

    # Create table of electricity generation by load segment | fuel | state
    # L123.out_EJ_state_elec_F contains electricity generation by fuel & state
    L123.out_EJ_state_elec_F %>%
      # The PV / CSP distinction does not matter for allocating electricity
      # generation by fuel across load segments, so assign both to "solar"
      mutate(fuel = sub("solar CSP", "solar", fuel),
             fuel = sub("solar PV", "solar", fuel)) %>%
      group_by(state, sector, fuel, year) %>%
      summarise(tot_generation = sum(value)) %>%
      ungroup() %>%
      # filter out years which are not present in the electricity load segments calibration data table
      semi_join(L1236.grid_elec_supply_USA, by = c("year")) %>%
      left_join_error_no_match(states_subregions %>%
                  select(state, grid_region),
                by = "state") %>%
      # map fuel shares by horizontal load segment and grid to the states
      # joining L1236.grid_elec_supply_USA is intended to duplicate rows,
      # creating four rows for every state | fuel | year (one row per load segment)
      # left_join_error_no_match throws error when number of rows changes, so left_join is used
      left_join(L1236.grid_elec_supply_USA %>%
                  select(grid_region, segment, fuel, year, fraction),
                by = c("grid_region", "fuel", "year")) %>%
      mutate(generation = tot_generation * fraction) %>%
      select(state, grid_region, segment, fuel, year, tot_generation, fraction, generation) -> L1239.state_elec_supply

    # ===================================================

    # Produce outputs

    L1239.state_elec_supply %>%
      add_title("Electricity supply by fuel by horizontal load segment in each state.") %>%
      add_units("EJ; unitless (fraction)") %>%
      add_comments("Electricity generation by fuel & state (from L123.out_EJ_state_elec_F) allocated across horizontal load segments.") %>%
      add_comments("This allocation is based on the fraction of fuel in the horizontal load segments by grid region (from L1236.grid_elec_supply_USA).") %>%
      add_legacy_name("L1239.state_elec_supply") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L123.out_EJ_state_elec_F",
                     "L1236.grid_elec_supply_USA") ->
      L1239.state_elec_supply_USA

    return_data(L1239.state_elec_supply_USA)

  } else {
    stop("Unknown command")
  }
}
