#' module_gcam.usa_LB1231.Elec_tech
#'
#' Downscaling electricity by state/fuel to state/fuel/technology
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1231.in_EJ_state_elec_F_tech}, \code{L1231.out_EJ_state_elec_F_tech}. The corresponding file in the
#' original data system was \code{LB1231.Elec_tech.R} (gcam-usa level1).
#' @details Downscaling electricity by state/fuel to state/fuel/technology
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RLH August 2017
module_gcam.usa_LB1231.Elec_tech<- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L123.out_EJ_R_elec_F_Yh",
             "L1231.in_EJ_R_elec_F_tech_Yh",
             "L1231.out_EJ_R_elec_F_tech_Yh",
             FILE = "temp-data-inject/L123.in_EJ_state_elec_F",
             FILE = "temp-data-inject/L123.out_EJ_state_elec_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1231.in_EJ_state_elec_F_tech",
             "L1231.out_EJ_state_elec_F_tech"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    year <- value <- GCAM_region_ID <- value.x <- value.y <- state <- sector.x <-
      fuel <- technology <- NULL

    # Load required inputs
    L123.out_EJ_R_elec_F_Yh <- get_data(all_data, "L123.out_EJ_R_elec_F_Yh")
    L1231.in_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.in_EJ_R_elec_F_tech_Yh")
    L1231.out_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.out_EJ_R_elec_F_tech_Yh")
    L123.in_EJ_state_elec_F <- get_data(all_data, "temp-data-inject/L123.in_EJ_state_elec_F") %>%
      # temp-data-inject code
      gather(year, value, starts_with("X")) %>%
      mutate(year = as.integer(substr(year, 2, 5)))
    L123.out_EJ_state_elec_F <- get_data(all_data, "temp-data-inject/L123.out_EJ_state_elec_F") %>%
      # temp-data-inject code
      gather(year, value, starts_with("X")) %>%
      mutate(year = as.integer(substr(year, 2, 5)))

    # ==================================================
    # Downscaling of electricity by fuel to fuel and technology
    # Computing nation-level shares of technology within fuel
    L1231.share_elec_F_tech <- L1231.out_EJ_R_elec_F_tech_Yh %>%
      filter(GCAM_region_ID == gcam.USA_CODE) %>%
      left_join_error_no_match(L123.out_EJ_R_elec_F_Yh, by = c("GCAM_region_ID", "fuel", "year")) %>%
      # Value is equal to technology total / fuel total
      mutate(value = value.x / value.y) %>%
      replace_na(list(value = 0)) %>%
      # Repeat for all states
      repeat_add_columns(distinct(L123.out_EJ_state_elec_F,state)) %>%
      select(state, sector = sector.x, fuel, technology, year, value)

    # Multiply the tech shares by the input and output by state and fuel
    L1231.in_EJ_state_elec_F_tech <- L1231.share_elec_F_tech %>%
      # only the fuels that use "inputs" (oil, gas, coal, biomass)
      filter(fuel %in% L1231.in_EJ_R_elec_F_tech_Yh$fuel) %>%
      left_join_error_no_match(L123.in_EJ_state_elec_F, by = c("state", "sector", "fuel", "year")) %>%
      # State/Technology output = technology share * state/fuel output
      mutate(value = value.x * value.y)

    L1231.out_EJ_state_elec_F_tech <- L1231.share_elec_F_tech %>%
      left_join_error_no_match(L123.out_EJ_state_elec_F, by = c("state", "sector", "fuel", "year")) %>%
      # State/Technology output = technology share * state/fuel output
      mutate(value = value.x * value.y)

    # ===================================================

    # Produce outputs

    L1231.in_EJ_state_elec_F_tech %>%
      add_title("Electricity sector energy consumption by state / fuel / technology") %>%
      add_units("EJ") %>%
      add_comments("National level technology shares multiplied by state level fuel shares") %>%
      add_legacy_name("L1231.in_EJ_state_elec_F_tech") %>%
      add_precursors("L123.out_EJ_R_elec_F_Yh",
                     "L1231.out_EJ_R_elec_F_tech_Yh",
                     "L1231.in_EJ_R_elec_F_tech_Yh",
                     "temp-data-inject/L123.in_EJ_state_elec_F",
                     "temp-data-inject/L123.out_EJ_state_elec_F") %>%
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1231.in_EJ_state_elec_F_tech

    L1231.out_EJ_state_elec_F_tech %>%
      add_title("Electricity generation by state / fuel / technology") %>%
      add_units("EJ") %>%
      add_comments("National level technology shares multiplied by state level fuel shares") %>%
      add_legacy_name("L1231.out_EJ_state_elec_F_tech") %>%
      add_precursors("L123.out_EJ_R_elec_F_Yh",
                     "L1231.out_EJ_R_elec_F_tech_Yh",
                     "temp-data-inject/L123.out_EJ_state_elec_F") %>%
      add_flags(FLAG_NO_TEST, FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1231.out_EJ_state_elec_F_tech

    return_data(L1231.in_EJ_state_elec_F_tech, L1231.out_EJ_state_elec_F_tech)
  } else {
    stop("Unknown command")
  }
}
