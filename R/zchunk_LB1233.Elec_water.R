#' module_gcam.usa_LB1233.Elec_water
#'
#' Compute water withdrawals/consumption by state, fuel, technology, and cooling system type.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1233.out_EJ_state_elec_F_tech_cool}, \code{L1233.share_sR_elec_F_tech_cool}, \code{L1233.wdraw_km3_state_elec}, \code{L1233.wcons_km3_state_elec}. The corresponding file in the
#' original data system was \code{LB1233.Elec_water.R} (gcam-usa level1).
#' @details Compute water withdrawals/consumption by state, fuel, technology, and cooling system type.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author ST October 2017
module_gcam.usa_LB1233.Elec_water <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/UCS_tech_names",
             FILE = "gcam-usa/UCS_water_types",
             FILE = "gcam-usa/Macknick_elec_water_m3MWh",
             FILE = "gcam-usa/UCS_Database",
             "L1231.out_EJ_state_elec_F_tech"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1233.out_EJ_state_elec_F_tech_cool",
             "L1233.share_sR_elec_F_tech_cool",
             "L1233.wdraw_km3_state_elec",
             "L1233.wcons_km3_state_elec"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    State <- `Reported Water Source (Type)` <- `Estimated Generation (MWh)` <-
      `Technology Code` <- cooling_system <- water_type <- technology <-
      state <- sector <- fuel <- out_MWh <- out_MWh_ <- `First Year of Operation` <-
      grid_region <- share <- sum_share_tech <- share_nat <- value <-
      year <- water_withdrawals <- water_consumption <- NULL

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    UCS_tech_names <- get_data(all_data, "gcam-usa/UCS_tech_names")
    UCS_water_types <- get_data(all_data, "gcam-usa/UCS_water_types")
    Macknick_elec_water_m3MWh <- get_data(all_data, "gcam-usa/Macknick_elec_water_m3MWh")
    UCS_Database <- get_data(all_data, "gcam-usa/UCS_Database")
    L1231.out_EJ_state_elec_F_tech <- get_data(all_data, "L1231.out_EJ_state_elec_F_tech")

    # ===================================================

    # adjust UCS database by resetting Michigan and Wisconsin power plant seawater use to surface water...
    UCS_Database %>%
      mutate(`Reported Water Source (Type)` = if_else(State %in% c("MI", "WI") &
                                                        `Reported Water Source (Type)` == "Ocean",
                                                      "Surface Water",
                                                      `Reported Water Source (Type)`)) %>%
      # make some further minor adjustments and introduce tech and water type names...
      rename(state = State,
             out_MWh = `Estimated Generation (MWh)`,
             Technology.Code = `Technology Code`) %>%
      right_join(UCS_tech_names, by = "Technology.Code") %>%
      right_join(UCS_water_types, by = "Reported Water Source (Type)") %>%
      # set water type to "none" for cases with no cooling system...
      mutate(water_type = if_else(cooling_system == "none", "none", water_type),
             # ...and change PV and hydro to fresh
             water_type = if_else(technology %in% c("hydro", "PV"), "fresh", water_type)) ->
      USC_db_adj

    # aggregate and compute shares for states
    USC_db_adj %>% group_by(state, sector, fuel, technology) %>%
      summarise(out_MWh_ = sum(out_MWh)) %>% ungroup ->
      L1233.out_MWh_state_elec_F_tech
    USC_db_adj %>% group_by(state, sector, fuel, technology, cooling_system, water_type) %>%
      summarise(out_MWh = sum(out_MWh)) %>% ungroup %>%
      left_join_error_no_match(L1233.out_MWh_state_elec_F_tech,
                               by = c("state", "sector", "fuel", "technology")) %>%
      mutate(share = out_MWh / out_MWh_) %>% select(-out_MWh_) ->
      L1233.out_MWh_state_elec_F_tech_cool

    # aggregate and compute shares for FERC subregions, filtering for power plants built in the 2000s only
    USC_db_adj %>% filter(`First Year of Operation` >= 2000) %>%
      left_join_error_no_match(select(states_subregions, state, grid_region), by = "state") ->
      USC_db_adj_2000s

    USC_db_adj_2000s %>% group_by(grid_region, sector, fuel, technology) %>%
      summarise(out_MWh_ = sum(out_MWh)) %>% ungroup ->
      L1233.out_MWh_sR_elec_F_tech
    USC_db_adj_2000s %>% group_by(grid_region, sector, fuel, technology, cooling_system, water_type) %>%
      summarise(out_MWh = sum(out_MWh)) %>% ungroup %>%
      left_join_error_no_match(L1233.out_MWh_sR_elec_F_tech,
                               by = c("grid_region", "sector", "fuel", "technology")) %>%
      mutate(share = out_MWh / out_MWh_) %>% select(-out_MWh_) ->
      L1233.out_MWh_sR_elec_F_tech_cool

    # calculate national averages, to be used as default values where data are missing
    L1233.out_MWh_state_elec_F_tech %>% group_by(sector, fuel, technology) %>%
      summarise(out_MWh_ = sum(out_MWh_)) %>% ungroup ->
      L1233.out_MWh_USA_elec_F_tech
    L1233.out_MWh_state_elec_F_tech_cool %>% group_by(sector, fuel, technology, cooling_system, water_type) %>%
      summarise(out_MWh = sum(out_MWh)) %>% ungroup %>%
      left_join_error_no_match(L1233.out_MWh_USA_elec_F_tech,
                               by = c("sector", "fuel", "technology")) %>%
      mutate(share_nat = out_MWh / out_MWh_) %>% select(-out_MWh_, -out_MWh) ->
      L1233.out_MWh_USA_elec_F_tech_cool

    # get all possible combinations of power plants, cooling system types, and water types in all states
    L1233.out_MWh_state_elec_F_tech_cool %>%
      select(-out_MWh, -share, -state) %>% unique -> L1233.elec_tech_cool
    L1233.elec_tech_cool %>%
      repeat_add_columns(tibble(state = gcamusa.STATES)) %>%
      left_join_keep_first_only(L1233.out_MWh_state_elec_F_tech_cool,
                                by = c("state", "sector", "fuel", "technology", "cooling_system", "water_type")) %>%
      # ^^ lots of NAs induced in the above join... most should be zeros; where technologies have all zeros, replace with national averages
      replace_na(list(share = 0)) %>% select(-out_MWh) %>%
      left_join_error_no_match(L1233.out_MWh_USA_elec_F_tech_cool,
                               by = c("sector", "fuel", "technology", "cooling_system", "water_type")) %>%
      # ^^ brings in national average data
      group_by(state, sector, fuel, technology) %>%
      mutate(sum_share_tech = sum(share)) %>% ungroup %>%
      mutate(share = if_else(sum_share_tech == 0, share_nat, share)) %>%
      select(-share_nat, -sum_share_tech) %>%
      # multiply through by historical output
      left_join(L1231.out_EJ_state_elec_F_tech, by = c("sector", "fuel", "technology", "state")) %>%
      # ^^ non-restricted join used to allow for expanded rows for all historical years
      mutate(value = value * share) %>% select(-share) ->
      L1233.out_EJ_state_elec_F_tech_cool


    # Get shares by FERC subregion for new investment in future periods
    L1233.elec_tech_cool %>%
      repeat_add_columns(tibble(grid_region = unique(states_subregions$grid_region))) %>%
      left_join_keep_first_only(L1233.out_MWh_sR_elec_F_tech_cool,
                                by = c("grid_region", "sector", "fuel", "technology", "cooling_system", "water_type")) %>%
      # ^^ lots of NAs induced in the above join... most should be zeros; where technologies have all zeros, replace with national averages
      replace_na(list(share = 0)) %>%
      left_join_error_no_match(L1233.out_MWh_USA_elec_F_tech_cool,
                               by = c("sector", "fuel", "technology", "cooling_system", "water_type")) %>%
      # ^^ brings in national average data
      group_by(grid_region, sector, fuel, technology) %>%
      mutate(sum_share_tech = sum(share)) %>% ungroup %>%
      mutate(share = if_else(sum_share_tech == 0, share_nat, share)) %>%
      select(-share_nat, -sum_share_tech, -out_MWh) ->
      L1233.share_sR_elec_F_tech_cool

    # Calculate the withdrawals and consumption of water by power sector for post-2000
    L1233.out_EJ_state_elec_F_tech_cool %>%
      filter(year >= 2000) %>%
      left_join_error_no_match(Macknick_elec_water_m3MWh,
                               by = c("sector", "fuel", "technology", "cooling_system", "water_type")) ->
      L1233.out_wdraw_cons

    L1233.out_wdraw_cons %>%
      mutate(value = value * water_withdrawals / CONV_MWH_GJ) %>%
      # ^^ conversion to GJ (as opposed to EJ) means result is in km3 (not m3)
      filter(water_type != "none") %>%
      group_by(state, sector, water_type, year) %>% summarise(value = sum(value)) %>%
      ungroup -> L1233.wdraw_km3_state_elec  # << withdrawal

    L1233.out_wdraw_cons %>%
      mutate(value = value * water_consumption / CONV_MWH_GJ) %>%
      # ^^ conversion to GJ (as opposed to EJ) means result is in km3 (not m3)
      filter(water_type != "none") %>%
      group_by(state, sector, water_type, year) %>% summarise(value = sum(value)) %>%
      ungroup -> L1233.wcons_km3_state_elec  # << consumption

    # ===================================================

    L1233.out_EJ_state_elec_F_tech_cool %>%
      add_title("Electricity output by state / fuel / technology / cooling system / water type") %>%
      add_units("EJ") %>%
      add_comments("UCS data aggregated to state level otuput") %>%
      add_legacy_name("L1233.out_EJ_state_elec_F_tech_cool") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/UCS_tech_names",
                     "gcam-usa/UCS_water_types",
                     "gcam-usa/UCS_Database",
                     "L1231.out_EJ_state_elec_F_tech") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1233.out_EJ_state_elec_F_tech_cool

    L1233.share_sR_elec_F_tech_cool %>%
      add_title("Electricity generation shares by grid region / fuel / technology / cooling system / water type") %>%
      add_units("Unitless") %>%
      add_comments("Calculated at FERC grid region") %>%
      add_legacy_name("L1233.share_sR_elec_F_tech_cool") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/UCS_tech_names",
                     "gcam-usa/UCS_water_types",
                     "gcam-usa/UCS_Database") ->
      L1233.share_sR_elec_F_tech_cool

    L1233.wdraw_km3_state_elec %>%
      add_title("Water withdrawals for electricity generation by state / water type") %>%
      add_units("km^3 (bm^3)") %>%
      add_comments("Outputs multiplied by Macknick withdrawal coefficients") %>%
      add_legacy_name("L1233.wdraw_km3_state_elec") %>%
      add_precursors("L1233.out_EJ_state_elec_F_tech_cool",
                     "gcam-usa/states_subregions",
                     "gcam-usa/UCS_tech_names",
                     "gcam-usa/UCS_water_types",
                     "gcam-usa/UCS_Database",
                     "gcam-usa/Macknick_elec_water_m3MWh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1233.wdraw_km3_state_elec

    L1233.wcons_km3_state_elec %>%
      add_title("Water consumption for electricity generation by state / water type") %>%
      add_units("km^3 (bm^3)") %>%
      add_comments("Outputs multiplied by Macknick consumption coefficients") %>%
      add_legacy_name("L1233.wcons_km3_state_elec") %>%
      add_precursors("L1233.out_EJ_state_elec_F_tech_cool",
                     "gcam-usa/states_subregions",
                     "gcam-usa/UCS_tech_names",
                     "gcam-usa/UCS_water_types",
                     "gcam-usa/UCS_Database",
                     "gcam-usa/Macknick_elec_water_m3MWh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1233.wcons_km3_state_elec

    return_data(L1233.out_EJ_state_elec_F_tech_cool, L1233.share_sR_elec_F_tech_cool, L1233.wdraw_km3_state_elec, L1233.wcons_km3_state_elec)
  } else {
    stop("Unknown command")
  }
}
