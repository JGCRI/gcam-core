#' module_gcam.usa_LA142.Building
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L142.in_EJ_state_bld_F}. The corresponding file in the
#' original data system was \code{LA142.Building.R} (gcam-usa level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_gcam.usa_LA142.Building <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L142.in_EJ_R_bld_F_Yh",
              "L101.inEIA_EJ_state_S_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L142.in_EJ_state_bld_F"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L142.in_EJ_R_bld_F_Yh <- get_data(all_data, "L142.in_EJ_R_bld_F_Yh")
    L101.inEIA_EJ_state_S_F <- get_data(all_data, "L101.inEIA_EJ_state_S_F")

    # ===================================================
    # All comments are currently from the old data system will need to change -----

    #Subset residential and commercial from the SEDS table, and only the fuels that are part of the GCAM buildings sector. Sort by fuel.

    # Calculating each state and sector's (res/comm) shares of USA building energy use, by fuel

    # NOTE: Using SEDS rather than IEA for nation-level disaggregation between residential and commercial
    L101.inEIA_EJ_state_S_F %>%
      filter(sector %in% c("comm", "resid") , fuel %in% L142.in_EJ_R_bld_F_Yh$fuel) ->
      L142.in_EJ_state_bld_F_unscaled

    #Aggregate by fuel to calculate each state and sector's portional allocation
    L142.in_EJ_state_bld_F_unscaled %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      group_by(year, fuel) %>%
      summarise(value = sum(value)) ->
      L142.in_EJ_USA_bld_F_unscaled # rename to aggregated??

    #Calculate the portional allocation of total US buildings energy use (by each fuel) to each state and sector
    L142.in_pct_state_bld_F <- L142.in_EJ_state_bld_F_unscaled

    L142.in_EJ_state_bld_F_unscaled %>%
      rename(state_val = value) %>%
      # COMMENT ON WHY USE LEFT JOIN
      left_join(L142.in_EJ_USA_bld_F_unscaled %>% rename(usa_val = value), by = c("fuel", "year")) %>%
      mutate(value = state_val / usa_val) %>%
      select(-state_val, - usa_val) ->
      L142.in_pct_state_bld_F


    #Apportion nation-level energy by fuel to states and sectors
    #First need to aggregate the buildings sectors in the whole-usa data

    L142.in_EJ_R_bld_F_Yh %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value)) %>%
      filter(year %in% HISTORICAL_YEARS) -> # may be can removed by the filter years... may be redundante un less extra years
      L142.in_EJ_R_bldtot_F_Yh

    USA_regID = 1

    L142.in_EJ_R_bldtot_F_Yh %>%
      filter(GCAM_region_ID == USA_regID) %>%
      ungroup %>%
      select(-GCAM_region_ID) ->
      L142.in_EJ_R_bldtot_F_Yh_USA


    L142.in_pct_state_bld_F %>%
      left_join_error_no_match(L142.in_EJ_R_bldtot_F_Yh_USA, by = c("year", "fuel")) %>%
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel, year, value) ->
      L142.in_EJ_state_bld_F

    # ===================================================

    L142.in_EJ_state_bld_F %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L142.in_EJ_state_bld_F") %>%
      add_precursors("L142.in_EJ_R_bld_F_Yh", "L101.inEIA_EJ_state_S_F") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L142.in_EJ_state_bld_F

    return_data(L142.in_EJ_state_bld_F)
  } else {
    stop("Unknown command")
  }
}
