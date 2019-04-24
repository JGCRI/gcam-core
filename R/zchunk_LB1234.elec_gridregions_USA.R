#' module_gcamusa_LB1234.elec_gridregions_USA
#'
#' Calculate electricity fuel consumption and electricity generation by grid region.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1234.in_EJ_grid_elec_F}, \code{L1234.out_EJ_grid_elec_F}.
#' The corresponding file in the original data system was \code{LB1234.elec_gridregions.R} (gcam-usa level1).
#' @details By grid region, calculates electricity fuel consumption and electricity generation.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author MTB August 2018
module_gcamusa_LB1234.elec_gridregions_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             "L123.in_EJ_state_elec_F",
             "L123.out_EJ_state_elec_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1234.in_EJ_grid_elec_F",
             "L1234.out_EJ_grid_elec_F"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    grid_region <- sector <- fuel <- year <- fuel.input <- generation <- state <-
      value <- NULL  # silence package check notes

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    L123.in_EJ_state_elec_F <- get_data(all_data, "L123.in_EJ_state_elec_F")
    L123.out_EJ_state_elec_F <- get_data(all_data, "L123.out_EJ_state_elec_F")

    # ===================================================
    # Data Processing

    # Take in state-level data on electric power sector fuel consumption, aggregate to grid regions
    L123.in_EJ_state_elec_F %>%
      left_join_error_no_match(states_subregions %>%
                                 select(state, grid_region),
                               by ="state") %>%
      group_by(grid_region, sector, fuel, year) %>%
      summarise(fuel.input = sum(value)) %>%
      ungroup() -> L1234.in_EJ_grid_elec_F

    # Take in state-level data on electricity generation by fuel, aggregate to grid regions
    L123.out_EJ_state_elec_F %>%
      left_join_error_no_match(states_subregions %>%
                                 select(state, grid_region),
                               by ="state") %>%
      group_by(grid_region, sector, fuel, year) %>%
      summarise(generation = sum(value)) %>%
      ungroup() -> L1234.out_EJ_grid_elec_F

    # ===================================================
    # Produce outputs

    L1234.in_EJ_grid_elec_F %>%
      add_title("Fuel input into electricity by fuel and grid region") %>%
      add_units("EJ") %>%
      add_comments("Fuel input values from L123.in_EJ_state_elec_F aggregated to grid region level") %>%
      add_legacy_name("L1234_in_EJ_grid_elec_F") %>%
      add_precursors("L123.in_EJ_state_elec_F",
                     "gcam-usa/states_subregions") ->
      L1234.in_EJ_grid_elec_F

    L1234.out_EJ_grid_elec_F %>%
      add_title("Electricity generation by fuel and grid region") %>%
      add_units("EJ") %>%
      add_comments("Electricity generation (output) values from L123.out_EJ_state_elec_F aggregated to grid region level") %>%
      add_legacy_name("L1234_out_EJ_grid_elec_F") %>%
      add_precursors("L123.out_EJ_state_elec_F",
                     "gcam-usa/states_subregions") ->
      L1234.out_EJ_grid_elec_F

    return_data(L1234.in_EJ_grid_elec_F, L1234.out_EJ_grid_elec_F)
  } else {
    stop("Unknown command")
  }
}
