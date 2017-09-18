#' module_gcam.usa_LA115.RooftopPV
#'
#' Prepares resource curves for rooftop PV (commercial and residential combined).
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L115.rsrc_state_rooftopPV}. The corresponding file in the
#' original data system was \code{LA115.RooftopPV.R} (gcam-usa level1).
#' @details Prepares resource curves for rooftop PV (commercial and residential combined).
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author ST September 2017
module_gcam.usa_LA115.RooftopPV <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/NREL_Com_PV_supply_curve",
             FILE = "gcam-usa/NREL_Res_PV_supply_curve"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L115.rsrc_state_rooftopPV"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    NREL_Com_PV_supply_curve <- get_data(all_data, "gcam-usa/NREL_Com_PV_supply_curve")
    NREL_Res_PV_supply_curve <- get_data(all_data, "gcam-usa/NREL_Res_PV_supply_curve")

    # ===================================================

    # Compute PV fixed charge rate
    PV_FCR <- ( PV.DISCOUNT_RATE * ( 1 + PV.DISCOUNT_RATE ) ^ PV.LIFETIME ) /
      ( ( PV.DISCOUNT_RATE + 1 ) ^ PV.LIFETIME - 1 )

    # Get maximum residential capacity factor
    filter(NREL_Res_PV_supply_curve, Relative_Cost == min(Relative_Cost))$MWh /
      filter(NREL_Res_PV_supply_curve, Relative_Cost == min(Relative_Cost))$MW /
      HOURS_PER_YEAR -> max_resid_capacity_factor

    # Get maximum commerical capacity factor
    filter(NREL_Com_PV_supply_curve, Rel_Cost == min(Rel_Cost))$GWH * CONV_BIL_MIL /
      filter(NREL_Com_PV_supply_curve, Rel_Cost == min(Rel_Cost))$MW /
      HOURS_PER_YEAR -> max_comm_capacity_factor

    # Get minimum levelized electricity cost (LEC) for residential PV in 2005USD / KW
    PV_resid_min_LEC_2005 <- (PV.RESID_INSTALLED_COST / PV.DERATING_FACTOR) * PV_FCR /
      (max_resid_capacity_factor * HOURS_PER_YEAR) +
      PV.RESID_OM / (max_resid_capacity_factor * HOURS_PER_YEAR)
    # ... and in 1975USD / GJ
    PV_resid_min_LEC_1975 <- PV_resid_min_LEC_2005 * gdp_deflator(1975, 2005) / CONV_KWH_GJ

    # Get minimum levelized electricity cost (LEC) for commercial PV in 2005USD / KW
    PV_comm_min_LEC_2005 <- (PV.COMM_INSTALLED_COST / PV.DERATING_FACTOR ) * PV_FCR /
      (max_comm_capacity_factor * HOURS_PER_YEAR) +
      PV.COMM_OM / (max_comm_capacity_factor * HOURS_PER_YEAR)
    # ... and in 1975USD / GJ
    PV_comm_min_LEC_1975 <- PV_comm_min_LEC_2005 * gdp_deflator(1975, 2005) / CONV_KWH_GJ

    # COMPUTE SUPPLY CURVES

    # Get residential supply curve
    NREL_Res_PV_supply_curve %>%
      mutate(p = Relative_Cost * PV_resid_min_LEC_1975,
             generation = MWh / CONV_BIL_MIL) %>%  # << convert to GWh (like commercial)
      rename(state_name = State) %>%
      select(state_name, p, generation) ->
      L115.pv_sc_res

    # Get commercial supply curve
    NREL_Com_PV_supply_curve %>%
      mutate(p = Rel_Cost * PV_comm_min_LEC_1975,
             generation = GWH) %>%
      rename(state_name = State) %>%
      select(state_name, p, generation) ->
      L115.pv_sc_com

    # Combine supply curves, add state abbrv., and rebase p
    bind_rows(L115.pv_sc_res, L115.pv_sc_com) %>%
      left_join_error_no_match(select(states_subregions, state_name, state),
                               by = "state_name") %>%
      arrange(state_name) %>%
      select(-state_name, state, p, generation) %>%
      mutate(p = p - min(p)) %>%
      arrange(p) ->
      # ^^ note: ordering of generation for matching p values will affect supply curve parameters (fitted below).
      L115.pv_sc

    unique(select(L115.pv_sc, state)) %>% mutate(p = 0, generation = 0) %>%
      # ^^ adds a 0,0 row to all states for supply curve fitting)
      bind_rows(L115.pv_sc) %>%
      arrange(state, p) %>%
      # ^^ sort for cumulative column calcs to follow
      group_by(state) %>% mutate(cumul = cumsum(generation),
                                 percent_cumul = cumul / sum(generation)) ->
      # ^^ computes cumulative sum (abs. and percent) for each state separately
      L115.pv_sc_State

    # Get mid-price for each state
    L115.pv_sc_State %>%
      filter(percent_cumul > 0.5) %>%
      # mid-price taken as the first price after the 50th percentile
      summarise(mid_p = first(p)) -> L115.pv_midPrice

    # Create smoothing functions
    smooth_res_curve_approx <- function( b, midp, p ) {
      p_pow_b <- p ^ b
      f_p <- p_pow_b / ( midp ^ b + p_pow_b )
    }
    smooth_res_curve_approx_error <- function( b, midp, supply_curve ) {
      f_p <- smooth_res_curve_approx( b, midp, supply_curve$p )
      error <- f_p - supply_curve$percent_cumul
      crossprod( error, error )
    }
    # ^^ computes the smooth renewable resource function at price points...
    # ... from the actual supply curve, then returns the error between...
    # ... computed and actual values.
    get_error_min_b <- function(stateAb) {
      midPrice <- filter(L115.pv_midPrice, state == stateAb)$mid_p
      supplyCurve <- filter(ungroup(L115.pv_sc_State), state == stateAb)
      optimize(f = smooth_res_curve_approx_error, interval = c(1.0, 15.0),
               midPrice, supplyCurve) %>% unlist
    }
    # Get error_min_b for all states using apply (replaces "for loop" structure of legacy code)
    sapply(unique(L115.pv_sc$state), get_error_min_b) %>% t %>% as.data.frame %>%
      tibble::rownames_to_column(var = "state") %>% as_tibble %>%
      select(state, minimum) %>% rename(b_exp = minimum) ->
      L115.pv_error_min_b

    # Join up mid_price up with error value to get table of parameters with all states
    L115.pv_midPrice %>%
      left_join_error_no_match(L115.pv_error_min_b, by = "state") ->
      L115.rsrc_state_rooftopPV

    L115.pv_sc %>%
      group_by(state) %>% summarise(generation = sum(generation)) %>%
      mutate(generation = generation * CONV_GWH_EJ) %>%
      left_join_error_no_match(L115.rsrc_state_rooftopPV, by = "state") %>%
      ## output attributes...
      add_title("Resource curves for rooftop PV") %>%
      add_units("1975$/GJ (mid-price) and EJ/yr (maxsubresource)") %>%
      add_comments("Note: Fitted supply curve parameters are sensitive to ordering generation values with equal price") %>%
      add_legacy_name("L115.rsrc_state_rooftopPV") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/NREL_Com_PV_supply_curve",
                     "gcam-usa/NREL_Res_PV_supply_curve") %>%
      add_flags(FLAG_NO_TEST) ->
      L115.rsrc_state_rooftopPV

    return_data(L115.rsrc_state_rooftopPV)
  } else {
    stop("Unknown command")
  }
}
