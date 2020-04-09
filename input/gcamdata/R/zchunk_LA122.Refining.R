# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LA122.Refining
#'
#' Downscales crude oil, corn ethanol, and biodiesel refining inputs and outputs to state-level.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L122.in_EJ_state_refining_F}, \code{L122.out_EJ_state_refining_F}. The corresponding file in the
#' original data system was \code{LA122.Refining.R} (gcam-usa level1).
#' @details Downscales crude oil, corn ethanol, and biodiesel refining inputs and outputs to state-level.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter group_by left_join mutate select transmute
#' @importFrom tidyr replace_na
#' @author RLH September 2017
module_gcamusa_LA122.refining <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L122.in_EJ_R_refining_F_Yh",
             "L122.out_EJ_R_refining_F_Yh",
             "L101.inEIA_EJ_state_S_F",
             FILE = "gcam-usa/EIA_biodiesel_Mgal.yr"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L122.in_EJ_state_refining_F",
             "L122.out_EJ_state_refining_F"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    GCAM_region_ID <- sector <- fuel <- year <- value <- value.x <- value.y <- state <-
      fuel.x <- Mgal.yr <- pct <- NULL

    # Load required inputs
    L122.in_EJ_R_refining_F_Yh <- get_data(all_data, "L122.in_EJ_R_refining_F_Yh") %>%
      filter(GCAM_region_ID == gcam.USA_CODE)
    L122.out_EJ_R_refining_F_Yh <- get_data(all_data, "L122.out_EJ_R_refining_F_Yh") %>%
      filter(GCAM_region_ID == gcam.USA_CODE)
    L101.inEIA_EJ_state_S_F <- get_data(all_data, "L101.inEIA_EJ_state_S_F")
    EIA_biodiesel_Mgal.yr <- get_data(all_data, "gcam-usa/EIA_biodiesel_Mgal.yr")

    # ===================================================
    # CRUDE OIL REFINING
    # NOTE: using SEDS crude oil input to industry as basis for allocation of crude oil refining to states
    # Crude oil consumption by industry is the energy used at refineries (input - output)

    # Calculate the percentages of oil consumption in each state
    L122.pct_state_cor <- L101.inEIA_EJ_state_S_F %>%
      filter(sector == "industry",
             fuel == "crude oil") %>%
      mutate(sector = "oil refining") %>%
      group_by(year) %>%
      # State percentage of total in each year
      mutate(value = value / sum(value)) %>%
      ungroup()

    # Crude oil refining output by state
    # Apportion the national total to the states
    L122.out_EJ_state_cor <- L122.pct_state_cor %>%
      left_join_error_no_match(L122.out_EJ_R_refining_F_Yh, by = c("sector", "year")) %>%
      # State output value = state proportion * national output value
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel = fuel.x, year, value)

    # Inputs to crude oil refining - same method of portional allocations, but with multiple fuels
    # Oil refining input fuels
    oil_input_fuels <- L122.in_EJ_R_refining_F_Yh %>%
      filter(sector == "oil refining") %>%
      select(fuel) %>%
      distinct()

    # Repeat state proportions for all fuels in oil refining sector
    L122.pct_state_cor_repF <- L122.pct_state_cor %>%
      select(-fuel) %>%
      repeat_add_columns(oil_input_fuels)

    # Calculate state oil input values
    L122.in_EJ_state_cor_F <- L122.pct_state_cor_repF %>%
      left_join_error_no_match(L122.in_EJ_R_refining_F_Yh, by = c("sector", "fuel", "year")) %>%
      # State input value = state proportion * national input value
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel, year, value)

    # BIOMASS LIQUIDS
    # NOTE: using SEDS biofuel transformation-related losses to disaggregate ethanol production to states

    # Calculate the percentages of corn ethanol consumption in each state
    L122.pct_state_btle <- L101.inEIA_EJ_state_S_F %>%
      filter(sector == "corn ethanol") %>%
      group_by(year) %>%
      mutate(value = value / sum(value)) %>%
      ungroup() %>%
      replace_na(list(value = 0))

    # Corn ethanol output by state
    L122.out_EJ_state_btle <- L122.pct_state_btle %>%
      left_join_error_no_match(L122.out_EJ_R_refining_F_Yh, by = c("sector", "year")) %>%
      # State output value = state proportion * national output value
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel = fuel.x, year, value)

    # Corn ethanol inputs by state and fuel: Repeat percentage-wise table by number of fuel inputs
    # Corn ethanol input fuels
    corneth_input_fuels <- L122.in_EJ_R_refining_F_Yh %>%
      filter(sector == "corn ethanol") %>%
      select(fuel) %>%
      distinct()

    # Repeat state proportions for all fuels used in corn ethanol sector
    L122.pct_state_btle_repF <- L122.pct_state_btle %>%
      select(-fuel) %>%
      repeat_add_columns(corneth_input_fuels)

    # Corn ethanol inputs by state
    L122.in_EJ_state_btle_F <- L122.pct_state_btle_repF %>%
      left_join_error_no_match(L122.in_EJ_R_refining_F_Yh, by = c("sector", "fuel", "year")) %>%
      # State input value = state proportion * national input value
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel, year, value)

    # Biodiesel output by state
    # NOTE: SEDS does not cover biodiesel; using a separate EIA database for disaggregating this to states

    # Build table of percentages by historical year
    EIA_biodiesel_Mgal.yr <- EIA_biodiesel_Mgal.yr %>%
      transmute(pct = Mgal.yr / sum(Mgal.yr), state)

    # Joining EIA_biodiesel_Mgal.yr to all states and years
    L122.pct_state_btlbd <- tidyr::crossing(state = gcamusa.STATES,
                                            year = HISTORICAL_YEARS) %>%
      mutate(sector = "biodiesel",
             fuel = "biomass oil") %>%
      # Using left_join because not all states in EIA_biodiesel_Mgal.yr
      left_join(EIA_biodiesel_Mgal.yr, by = "state") %>%
      replace_na(list(pct = 0))

    # Apportion to the states
    L122.out_EJ_state_btlbd <- L122.pct_state_btlbd %>%
      left_join_error_no_match(L122.out_EJ_R_refining_F_Yh, by = c("sector", "year")) %>%
      # State output value = state proportion * national output value
      mutate(value = pct * value) %>%
      select(state, sector, fuel = fuel.x, year, value)

    # Biodiesel inputs by state and fuel
    # Biodiesel input fuels
    biodiesel_input_fuels <- L122.in_EJ_R_refining_F_Yh %>%
      filter(sector == "biodiesel") %>%
      select(fuel) %>%
      distinct()

    # Repeat state proportions for all fuels used in biodiesel sector
    L122.pct_state_btlbd_repF <- L122.pct_state_btlbd %>%
      select(-fuel) %>%
      repeat_add_columns(biodiesel_input_fuels)

    # Biodiesel inputs by state
    L122.in_EJ_state_btlbd_F <- L122.pct_state_btlbd_repF %>%
      left_join_error_no_match(L122.in_EJ_R_refining_F_Yh, by = c("sector", "fuel", "year")) %>%
      # State input value = state proportion * national input value
      mutate(value = pct * value) %>%
      select(state, sector, fuel, year, value)

    # Bind the tables of inputs and outputs of all refineries by state in the base years
    L122.in_EJ_state_refining_F <- bind_rows(L122.in_EJ_state_cor_F, L122.in_EJ_state_btle_F, L122.in_EJ_state_btlbd_F)

    L122.out_EJ_state_refining_F <- bind_rows( L122.out_EJ_state_cor, L122.out_EJ_state_btle, L122.out_EJ_state_btlbd)
    # ===================================================
    # Produce outputs
    L122.in_EJ_state_refining_F %>%
      add_title("Refinery energy inputs by state, sector, and fuel") %>%
      add_units("EJ") %>%
      add_comments("Crude oil, corn ethanol, and biodiesel input values apportioned to states") %>%
      add_legacy_name("L122.in_EJ_state_refining_F") %>%
      add_precursors("L101.inEIA_EJ_state_S_F",
                     "L122.in_EJ_R_refining_F_Yh",
                     "gcam-usa/EIA_biodiesel_Mgal.yr") ->
      L122.in_EJ_state_refining_F

    L122.out_EJ_state_refining_F %>%
      add_title("Refinery output by state and sector") %>%
      add_units("EJ") %>%
      add_comments("Crude oil, corn ethanol, and biodiesel output values apportioned to states") %>%
      add_legacy_name("L122.out_EJ_state_refining_F") %>%
      add_precursors("L101.inEIA_EJ_state_S_F",
                     "L122.out_EJ_R_refining_F_Yh",
                     "gcam-usa/EIA_biodiesel_Mgal.yr") ->
      L122.out_EJ_state_refining_F

    return_data(L122.in_EJ_state_refining_F, L122.out_EJ_state_refining_F)
  } else {
    stop("Unknown command")
  }
}
