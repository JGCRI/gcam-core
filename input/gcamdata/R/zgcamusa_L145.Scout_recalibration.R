# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L145.Scout_recalibration
#'
#' Recalibrates GCAM-USA residential and commercial energy consumption according to Scout model data.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L145.in_EJ_state_comm_F_U_Y}, \code{L145.in_EJ_state_res_F_U_Y}.
#' @details Re-assigns energy consumption by state / sector / fuel / service / technology according to service / technology shares from Scout.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select lead lag
#' @importFrom tidyr gather spread
#' @author GPK 2022
module_gcamusa_L145.Scout_recalibration <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/Scout_bld_calibration",
             "L144.in_EJ_state_comm_F_U_Y",
             "L144.in_EJ_state_res_F_U_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L145.in_EJ_state_bld_F_U_tech_fby"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    year <- value <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    Scout_bld_calibration <- get_data(all_data, "gcam-usa/Scout_bld_calibration")
    L144.in_EJ_state_comm_F_U_Y <- get_data(all_data, "L144.in_EJ_state_comm_F_U_Y")
    L144.in_EJ_state_res_F_U_Y <- get_data(all_data, "L144.in_EJ_state_res_F_U_Y")

    # ===================================================

    # initial processing of scout calibration data for joining with similar data from L144
    # use semi-join to drop any sector/service/fuel combinations that aren't in GCAM, and re-compute shares
    L145.Scout_bld_calibration <- Scout_bld_calibration %>%
      mutate(service = paste(sector, service)) %>%
      semi_join(bind_rows(L144.in_EJ_state_comm_F_U_Y,
                          L144.in_EJ_state_res_F_U_Y),
                by = c("sector", "service", "fuel")) %>%
      group_by(state, sector, fuel, year) %>%
      mutate(share = share / sum(share)) %>%
      ungroup() %>%
      rename(state_name = state) %>%
      left_join_error_no_match(select(states_subregions, state, state_name),
                               by = "state_name") %>%
      select(-state_name,-energy)

    # GPK 8/29/2023 revision: the v3 Scout data don't disaggregate commercial unspecified
    # electricity into building and non-building categories.
    # Also resid other needs to be downscaled to resid televisions and other
    L145.share_state_S_elec_Uother <- bind_rows(L144.in_EJ_state_comm_F_U_Y,
                                                L144.in_EJ_state_res_F_U_Y) %>%
      filter(fuel == "electricity",
             service %in% c("comm other", "comm non-building", "resid other", "resid televisions"),
             year == max(year)) %>%
      group_by(state, sector, fuel, year) %>%
      mutate(other_share = value / sum(value)) %>%
      ungroup() %>%
      select(state, sector, fuel, service, other_share)

    L145.Scout_bld_downscale_other <- L145.Scout_bld_calibration %>%
      filter(fuel == "electricity",
             service %in% c("comm other", "resid other")) %>%
      select(-service) %>%
      left_join(L145.share_state_S_elec_Uother,
                by = c("state", "sector", "fuel")) %>%
      mutate(share = share * other_share) %>%
      select(-other_share)

    # Remove the initially reported "other" data, bind the downscaled other
    L145.Scout_bld_calibration <- anti_join(L145.Scout_bld_calibration, L145.Scout_bld_downscale_other,
                                            by = c("state", "sector", "service", "fuel", "technology", "year")) %>%
      bind_rows(L145.Scout_bld_downscale_other)

    # Only re-scale the year, service, and fuel categories that are in Scout. All others just keep the GCAM values.
    L145.in_EJ_state_bld_F_U_tech_fby <- bind_rows(L144.in_EJ_state_comm_F_U_Y,
                                                   L144.in_EJ_state_res_F_U_Y) %>%
      semi_join(L145.Scout_bld_calibration, by = c("state", "service", "fuel", "year")) %>%
      group_by(state, sector, fuel, year) %>%
      summarise(total = sum(value)) %>%
      ungroup() %>%
      inner_join(L145.Scout_bld_calibration, by = c("state", "sector", "fuel", "year")) %>%
      mutate(value = total * share) %>%
      select(state, sector, fuel, service, technology, year, value)

    # ===================================================

    # Produce outputs
    L145.in_EJ_state_bld_F_U_tech_fby %>%
      add_title("Residential and commercial energy consumption by state/fuel/end use") %>%
      add_units("EJ/yr") %>%
      add_comments("Re-computed from RECS- and CBECS-based processing using Scout data") %>%
      add_precursors("gcam-usa/Scout_bld_calibration",
                     "gcam-usa/states_subregions",
                     "L144.in_EJ_state_comm_F_U_Y",
                     "L144.in_EJ_state_res_F_U_Y") ->
      L145.in_EJ_state_bld_F_U_tech_fby

    return_data(L145.in_EJ_state_bld_F_U_tech_fby)
  } else {
    stop("Unknown command")
  }
}
