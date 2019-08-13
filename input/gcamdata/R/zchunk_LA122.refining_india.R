#' module_gcamindia_LA122.refining
#'
#' Downscales crude oil refining inputs and outputs to state-level.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L122.india_state_in_EJ_refining_F}, \code{L122.india_state_out_EJ_refining_F}. The corresponding file in the
#' original data system was \code{LA122.Refining.R} (gcam-usa level1).
#' @details Downscales crude oil refining inputs and outputs to state-level.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author PNK July 2019


module_gcamindia_LA122.refining <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L122.in_EJ_R_refining_F_Yh",
             "L122.out_EJ_R_refining_F_Yh",
             "L101.india_state_EB_EJ_state_S_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L122.india_state_in_EJ_refining_F",
             "L122.india_state_out_EJ_refining_F"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    GCAM_region_ID <- sector <- fuel <- year <- value <- value.x <- value.y <- state <-
      fuel.x <- Mgal.yr <- pct <- NULL

    # Load required inputs
    L122.in_EJ_R_refining_F_Yh        <- get_data(all_data, "L122.in_EJ_R_refining_F_Yh") %>%
      filter(GCAM_region_ID == gcam.INDIA_CODE)

    L122.out_EJ_R_refining_F_Yh       <- get_data(all_data, "L122.out_EJ_R_refining_F_Yh") %>%
      filter(GCAM_region_ID == gcam.INDIA_CODE)

    L101.india_state_EB_EJ_state_S_F        <- get_data(all_data, "L101.india_state_EB_EJ_state_S_F")

        # ===================================================
    # CRUDE OIL REFINING
    # NOTE: using crude oil input to industry as basis for allocation of crude oil refining to states
    # Crude oil consumption by industry is the energy used at refineries (input - output)

    # Step 1: Calculate the percentages of oil consumption in each state
    L122.india_state_pct_cor <- L101.india_state_EB_EJ_state_S_F %>%
      filter(sector == "industry",
             fuel == "crude oil") %>%
      mutate(sector = "oil refining") %>%
      group_by(year) %>%
      # Percentage share of each state in a year for crude oil consumption by oil refining sector
      mutate(value = value / sum(value)) %>%
      ungroup()

    # Crude oil refining output by state
    # Apportion the national total to the states
    L122.india_state_out_EJ_cor <- L122.india_state_pct_cor %>%
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
    L122.india_state_pct_cor_repF <- L122.india_state_pct_cor %>%
      select(-fuel) %>%
      repeat_add_columns(oil_input_fuels)

    # Calculate state oil input values
    L122.india_state_in_EJ_cor_F <- L122.india_state_pct_cor_repF %>%
      left_join_error_no_match(L122.in_EJ_R_refining_F_Yh, by = c("sector", "fuel", "year")) %>%
      # State input value = state proportion * national input value
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel, year, value)


    # Bind the tables of inputs and outputs of all refineries by state in the base years
    L122.india_state_in_EJ_refining_F <- bind_rows(L122.india_state_in_EJ_cor_F)

    L122.india_state_out_EJ_refining_F <- bind_rows( L122.india_state_out_EJ_cor)
    # ===================================================
    # Produce outputs
    L122.india_state_in_EJ_refining_F %>%
      add_title("Refinery energy inputs by state, sector, and fuel") %>%
      add_units("EJ") %>%
      add_comments("Crude oil input values apportioned to states") %>%
      add_legacy_name("L122.india_state_in_EJ_refining_F") %>%
      add_precursors("L101.india_state_EB_EJ_state_S_F",
                     "L122.in_EJ_R_refining_F_Yh") ->
      L122.india_state_in_EJ_refining_F

    L122.india_state_out_EJ_refining_F %>%
      add_title("Refinery output by state and sector") %>%
      add_units("EJ") %>%
      add_comments("Crude oil output values apportioned to states") %>%
      add_legacy_name("L122.india_state_out_EJ_refining_F") %>%
      add_precursors("L101.india_state_EB_EJ_state_S_F",
                     "L122.out_EJ_R_refining_F_Yh") ->
      L122.india_state_out_EJ_refining_F

    return_data(L122.india_state_in_EJ_refining_F, L122.india_state_out_EJ_refining_F)
  } else {
    stop("Unknown command")
  }
}
