#' module_gcam.usa_LB1239.elec_state_fractions_USA
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
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author MTB August 2018
module_gcam.usa_LB1239.elec_state_fractions_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             "L123.out_EJ_state_elec_F",
             "L1238.grid_elec_supply_USA"))
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
    L1238.grid_elec_supply_USA <- get_data(all_data, "L1238.grid_elec_supply_USA")

    # ===================================================
    # Data Processing

    # Initialize varables
    L1239.grid_elec_supply <- L1238.grid_elec_supply_USA

    #Initialize state electricity table
    L123.out_EJ_state_elec_F %>%
      filter(year %in% BASE_YEARS) %>%
      mutate(fuel = sub("solar CSP", "solar", fuel)) %>%
      mutate(fuel = sub("solar PV", "solar", fuel)) %>%
      group_by(state, sector, fuel, year) %>%
      summarise(tot_generation = sum(value)) %>%
      ungroup() -> L1239.out_EJ_state_elec_F

    L1239.out_EJ_state_elec_F %>%
      left_join_error_no_match(states_subregions %>%
                  select(state, grid_region),
                by = "state") %>%
      # L1239.grid_elec_supply join is intended to duplicate rows
      # left_join_error_no_match throws error, so left_join is used
      left_join(L1239.grid_elec_supply %>%
                  select(-tot_generation, -generation),
                by = c("grid_region", "fuel", "year")) %>%
      mutate(generation = tot_generation * fraction) %>%
      select(state, grid_region, segment, fuel, year, tot_generation, fraction, generation) -> L1239.state_elec_supply

    # ===================================================

    # Produce outputs

    L1239.state_elec_supply %>%
      add_title("Electricity supply by fuel by horizontal load segment in each state.") %>%
      add_units("EJ; unitless (fraction)") %>%
      add_comments("Based on calculated fraction of fuel in the horizontal load segments by grid region.") %>%
      add_legacy_name("L1239.state_elec_supply") %>%
      add_precursors("gcam-usa/states_subregions",
                     "L123.out_EJ_state_elec_F",
                     "L1238.grid_elec_supply_USA") ->
      L1239.state_elec_supply_USA

    return_data(L1239.state_elec_supply_USA)

  } else {
    stop("Unknown command")
  }
}
