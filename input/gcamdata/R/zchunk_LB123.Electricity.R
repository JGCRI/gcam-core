#' module_gcamusa_LB123.Electricity
#'
#' Calculate electricity fuel consumption, electricity generation, and inputs and outputs of net ownuse
#' (the electricity used by production/transformation facilities) by state.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L123.in_EJ_state_elec_F}, \code{L123.out_EJ_state_elec_F}, \code{L123.in_EJ_state_ownuse_elec}, \code{L123.out_EJ_state_ownuse_elec}.
#' The corresponding file in the original data system was \code{LB123.Electricity.R} (gcam-usa level1).
#' @details By state, calculates electricity fuel consumption, electricity generation, and inputs and outputs of net ownuse.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RLH August 2017
module_gcamusa_LB123.Electricity <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/NREL_us_re_technical_potential",
             "L123.in_EJ_R_elec_F_Yh",
             "L123.out_EJ_R_elec_F_Yh",
             FILE = "gcam-usa/EIA_elect_td_ownuse_prices",
             "L126.in_EJ_R_elecownuse_F_Yh",
             "L126.out_EJ_R_elecownuse_F_Yh",
             "L101.inEIA_EJ_state_S_F",
             "L132.out_EJ_state_indchp_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L123.in_EJ_state_elec_F",
             "L123.out_EJ_state_elec_F",
             "L123.in_EJ_state_ownuse_elec",
             "L123.out_EJ_state_ownuse_elec"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    State <- state <- state_name <- GCAM_region_ID <- year <- value <- sector <-
      fuel <- CSP_GWh <- value.x <- value.y <- net_EJ_USA <- DirectUse_MWh <- NULL

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    NREL_us_re_technical_potential <- get_data(all_data, "gcam-usa/NREL_us_re_technical_potential") %>%
      # Remove TOTAL and add in state abbreviations
      filter(State != "TOTAL") %>%
      left_join_error_no_match(states_subregions %>%
                                 select(state, state_name),
                               by = c("State" = "state_name"))
    L123.in_EJ_R_elec_F_Yh <- get_data(all_data, "L123.in_EJ_R_elec_F_Yh") %>%
      filter(GCAM_region_ID == gcam.USA_CODE)
    L123.out_EJ_R_elec_F_Yh <- get_data(all_data, "L123.out_EJ_R_elec_F_Yh") %>%
      filter(GCAM_region_ID == gcam.USA_CODE)
    EIA_elect_td_ownuse_prices <- get_data(all_data, "gcam-usa/EIA_elect_td_ownuse_prices")
    L126.in_EJ_R_elecownuse_F_Yh <- get_data(all_data, "L126.in_EJ_R_elecownuse_F_Yh") %>%
      filter(GCAM_region_ID == gcam.USA_CODE)
    L126.out_EJ_R_elecownuse_F_Yh <- get_data(all_data, "L126.out_EJ_R_elecownuse_F_Yh") %>%
      filter(GCAM_region_ID == gcam.USA_CODE)
    L101.inEIA_EJ_state_S_F <- get_data(all_data, "L101.inEIA_EJ_state_S_F")
    L132.out_EJ_state_indchp_F <- get_data(all_data, "L132.out_EJ_state_indchp_F")

    # ===================================================
    # SEDS (EIA) indicates electricity generation technologies either in terms of fuel inputs or fuel outputs (not both)
    # ELECTRICITY_INPUT: coal, gas, oil, biomass
    # ELECTRICITY_OUTPUT: nuclear and renewables
    L123.pct_state_elec_F <- L101.inEIA_EJ_state_S_F %>%
      filter(sector %in% c("electricity_input", "electricity_output")) %>%
      # Compute each state's percentage, by fuel
      group_by(sector, fuel, year) %>%
      mutate(value = value / sum(value)) %>%
      ungroup() %>%
      # This is just PV solar, we will add in CSP next
      mutate(fuel = replace(fuel, fuel == "solar", "solar PV")) %>%
      replace_na(list(value = 0))

    # NOTE: SEDS does not disaggregate PV and CSP. Using solar shares for PV, and NREL data for CSP
    # Many states have zero CSP potential, and allocating production to these states will cause errors later on
    state_CSP_shares <- NREL_us_re_technical_potential %>%
      select(state, CSP_GWh) %>%
      # value = state share of total CSP potential
      transmute(state, value = CSP_GWh / sum(CSP_GWh))

    # Create L123.pct_state_elec_F values for CSP
    L123.pct_state_elec_CSP <- L123.pct_state_elec_F %>%
      select(-value) %>%
      filter(fuel == "solar PV") %>%
      mutate(fuel = "solar CSP") %>%
      left_join_error_no_match(state_CSP_shares, by = "state")

    L123.pct_state_elec_F <- bind_rows(L123.pct_state_elec_F, L123.pct_state_elec_CSP)

    # Electricity generation inputs by fuel and state
    # Allocating total energy input values to states using shares
    L123.in_EJ_state_elec_F <- L123.pct_state_elec_F %>%
      # L123.in_EJ_R_elec_F_Yh only has certain fuels
      filter(fuel %in% unique(L123.in_EJ_R_elec_F_Yh$fuel)) %>%
      left_join_error_no_match(L123.in_EJ_R_elec_F_Yh %>%
                                 select(fuel, year, value),
                               by = c("fuel", "year")) %>%
      # Multiplying state share by total value
      mutate(value = value.x * value.y,
             sector = "electricity generation") %>%
      select(-value.x, -value.y)

    # Electricity generation outputs by fuel and state
    # Allocating total electricity generation values to states using shares
    L123.out_EJ_state_elec_F <- L123.pct_state_elec_F %>%
      left_join_error_no_match(L123.out_EJ_R_elec_F_Yh %>%
                                 select(fuel, year, value),
                               by = c("fuel", "year")) %>%
      # Multiplying state share by total value
      mutate(value = value.x * value.y,
             sector = "electricity generation") %>%
      select(-value.x, -value.y)

    # ELECTRICITY - OWNUSE
    # NOTE: Electricity net own use energy is apportioned to states on the basis of EIA's direct use by state
    # First calculate the national own use quantity
    L123.net_EJ_USA_ownuse <- L126.in_EJ_R_elecownuse_F_Yh %>%
      left_join_error_no_match(L126.out_EJ_R_elecownuse_F_Yh, by = c("sector", "fuel", "year")) %>%
      # Net value = input value - output value
      mutate(net_EJ_USA = value.x - value.y) %>%
      select(sector, year, net_EJ_USA)

    # Then build table with each state's share of the national ownuse. Note that this is assumed invariant over time.
    L123.net_pct_state_USA_ownuse_elec <- tidyr::crossing(state = gcamusa.STATES,
                                                 sector = "electricity ownuse",
                                                 fuel = "electricity",
                                                 year = HISTORICAL_YEARS) %>%
      # Add in ownuse by state
      left_join_error_no_match(EIA_elect_td_ownuse_prices %>%
                                 select(State, DirectUse_MWh), by = c("state" = "State")) %>%
      group_by(sector, fuel, year) %>%
      # Compute state share of total
      mutate(value = DirectUse_MWh / sum(DirectUse_MWh)) %>%
      ungroup()

    # Net own use = national total multiplied by each state's share
    L123.net_EJ_state_ownuse_elec <- L123.net_pct_state_USA_ownuse_elec %>%
      left_join_error_no_match(L123.net_EJ_USA_ownuse, by = c("sector", "year")) %>%
      # Multiply state share by USA total
      mutate(value = value * net_EJ_USA)

    # The input of the electricity_net_ownuse sector is equal to sum of all generation (industrial CHP + electric sector)
    L123.in_EJ_state_ownuse_elec <- bind_rows(L123.out_EJ_state_elec_F, L132.out_EJ_state_indchp_F) %>%
      group_by(state, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(sector = "electricity ownuse",
             fuel = "electricity") %>%
      select(state, sector, fuel, year, value)

    # Output of electricity_net_ownuse sector is equal to input minus ownuse "net" energy
    L123.out_EJ_state_ownuse_elec <- L123.in_EJ_state_ownuse_elec %>%
      left_join_error_no_match(L123.net_EJ_state_ownuse_elec, by = c("state", "sector", "fuel", "year")) %>%
      # Input value - net value
      mutate(value = value.x - value.y) %>%
      select(state, sector, fuel, year, value)
    # ===================================================

    # Produce outputs
    L123.in_EJ_state_elec_F %>%
      add_title("Electricity sector energy consumption by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("State fuel shares created from L101.inEIA_EJ_state_S_F multiplied by USA totals from L123.in_EJ_R_elec_F_Yh") %>%
      add_legacy_name("L123.in_EJ_state_elec_F") %>%
      add_precursors("L101.inEIA_EJ_state_S_F", "gcam-usa/NREL_us_re_technical_potential",
                     "gcam-usa/states_subregions", "L123.in_EJ_R_elec_F_Yh") ->
      L123.in_EJ_state_elec_F

    L123.out_EJ_state_elec_F %>%
      add_title("Electricity generation by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("State fuel shares created from L101.inEIA_EJ_state_S_F multiplied by USA totals from L123.out_EJ_R_elec_F_Yh") %>%
      add_legacy_name("L123.out_EJ_state_elec_F") %>%
      add_precursors("L101.inEIA_EJ_state_S_F", "gcam-usa/NREL_us_re_technical_potential",
                     "gcam-usa/states_subregions", "L123.out_EJ_R_elec_F_Yh") ->
      L123.out_EJ_state_elec_F

    L123.in_EJ_state_ownuse_elec %>%
      add_title("Input to electricity net ownuse by state") %>%
      add_units("EJ") %>%
      add_comments("Sum of all generation from L123.out_EJ_state_elec_F and L132.out_EJ_state_indchp_F") %>%
      add_legacy_name("L123.in_EJ_state_ownuse_elec") %>%
      add_precursors("L101.inEIA_EJ_state_S_F", "gcam-usa/NREL_us_re_technical_potential",
                     "gcam-usa/states_subregions", "L123.out_EJ_R_elec_F_Yh", "L132.out_EJ_state_indchp_F") ->
      L123.in_EJ_state_ownuse_elec

    L123.out_EJ_state_ownuse_elec %>%
      add_title("Output of electricity net ownuse by state") %>%
      add_units("EJ") %>%
      add_comments("Input values from L123.in_EJ_state_ownuse_elec subtracted by net values") %>%
      add_comments("Net values created with states shares from EIA_elect_td_ownuse_prices and USA total net from L126 files") %>%
      add_legacy_name("L123.out_EJ_state_ownuse_elec") %>%
      add_precursors("L101.inEIA_EJ_state_S_F", "gcam-usa/NREL_us_re_technical_potential",
                     "gcam-usa/states_subregions", "L123.out_EJ_R_elec_F_Yh", "L132.out_EJ_state_indchp_F",
                     "L126.in_EJ_R_elecownuse_F_Yh", "L126.out_EJ_R_elecownuse_F_Yh", "gcam-usa/EIA_elect_td_ownuse_prices")  ->
      L123.out_EJ_state_ownuse_elec

    return_data(L123.in_EJ_state_elec_F, L123.out_EJ_state_elec_F, L123.in_EJ_state_ownuse_elec, L123.out_EJ_state_ownuse_elec)
  } else {
    stop("Unknown command")
  }
}
