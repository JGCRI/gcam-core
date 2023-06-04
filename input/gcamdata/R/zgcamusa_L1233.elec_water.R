# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L1233.elec_water
#'
#' Compute water withdrawals/consumption by state, fuel, technology, and cooling system type.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1233.out_EJ_state_elec_F_tech_cool}, \code{L1233.share_sR_elec_F_tech_cool}. The corresponding file in the
#' original data system was \code{LB1233.Elec_water.R} (gcam-usa level1).
#' @details Compute water withdrawals/consumption by state, fuel, technology, and cooling system type.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author ST October 2017, NTG May 2020
module_gcamusa_L1233.elec_water <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/UCS_tech_names",
             FILE = "gcam-usa/UCS_water_types",
             FILE = "gcam-usa/A23.elecS_tech_mapping_cool",
             FILE = "gcam-usa/UCS_Database",
             FILE = "gcam-usa/usa_seawater_states_basins",
             "L1231.out_EJ_state_elec_F_tech"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1233.out_EJ_state_elec_F_tech_cool",
             "L1233.share_sR_elec_F_tech_cool"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    State <- `Reported Water Source (Type)` <- reported_water_source <- `Estimated Generation (MWh)` <-
      `Technology Code` <- cooling_system <- water_type <- technology <- state <-
      state <- sector <- fuel <- out_MWh <- out_MWh_ <- `First Year of Operation` <-
      grid_region <- share <- sum_share_tech <- share_nat <- value <-
      year <- water_withdrawals <- water_consumption <- out_MWh.sea <- `Generation Technology` <-
      out_MWh_sea <- share_nat.sea <- Electric.sector <- Electric.sector.technology <- NULL

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    UCS_tech_names <- get_data(all_data, "gcam-usa/UCS_tech_names")
    UCS_water_types <- get_data(all_data, "gcam-usa/UCS_water_types")
    A23.elecS_tech_mapping_cool <- get_data(all_data, "gcam-usa/A23.elecS_tech_mapping_cool")
    UCS_Database <- get_data(all_data, "gcam-usa/UCS_Database")
    usa_seawater_states_basins <- get_data(all_data, "gcam-usa/usa_seawater_states_basins")
    L1231.out_EJ_state_elec_F_tech <- get_data(all_data, "L1231.out_EJ_state_elec_F_tech")

    # ===================================================

    `Generation Technology` <- NULL # silence package check notes

    # Define unique states and basins that have access to seawater that will
    # allow for seawate cooling

    seawater_states_basins <- unique(usa_seawater_states_basins$seawater_region)

    # inital processing of UCS database
    UCS_Database %>%
      rename(state = State,
             out_MWh = `Estimated Generation (MWh)`,
             Technology.Code = `Technology Code`,
             reported_water_source = `Reported Water Source (Type)`) %>%
      # filter out entries with no technology detail
      filter(!is.na(`Generation Technology`)) %>%
      # reset Michigan and Wisconsin power plant seawater use to surface water
      mutate(reported_water_source = if_else(!(state %in% seawater_states_basins) &
                                               reported_water_source == gcamusa.UCS_WATER_TYPE_OCEAN,
                                             gcamusa.UCS_WATER_TYPE_SURFACE,
                                             reported_water_source)) %>%
      # introduce tech and water type names
      left_join_error_no_match(UCS_tech_names, by = "Technology.Code") %>%
      left_join_error_no_match(UCS_water_types %>%
                                 rename(reported_water_source = `Reported Water Source (Type)`),
                               by = "reported_water_source") %>%
      # set water type to "none" for techs with no cooling system...
      mutate(water_type = if_else(cooling_system == gcamusa.ELEC_COOLING_SYSTEM_NONE, gcamusa.ELEC_COOLING_SYSTEM_NONE, water_type),
             # ...and change hydro and PV to fresh
             water_type = if_else(technology %in% gcamusa.ELEC_TECHS_NO_COOLING_FRESH, gcamusa.WATER_TYPE_FRESH, water_type),
             cooling_system = if_else(water_type == gcamusa.WATER_TYPE_SEAWATER, gcamusa.WATER_TYPE_SEAWATER, cooling_system)) %>%
      # Filter out all non hydro / PV / wind technologies which do not use cooling systems and binary recirculation which we do not model
      filter((!grepl(gcamusa.ELEC_COOLING_SYSTEM_NONE, cooling_system) | technology %in% gcamusa.ELEC_TECHS_NO_COOLING),
             !grepl(gcamusa.ELEC_COOLING_SYSTEM_BINARY, cooling_system)) ->
      USC_db_adj

    # aggregate and compute shares for states
    USC_db_adj %>%
      group_by(state, sector, fuel, technology) %>%
      summarise(out_MWh_ = sum(out_MWh)) %>%
      ungroup() ->
      L1233.out_MWh_state_elec_F_tech

    USC_db_adj %>%
      group_by(state, sector, fuel, technology, cooling_system, water_type) %>%
      summarise(out_MWh = sum(out_MWh)) %>%
      ungroup() %>%
      left_join_error_no_match(L1233.out_MWh_state_elec_F_tech,
                               by = c("state", "sector", "fuel", "technology")) %>%
      mutate(share = out_MWh / out_MWh_) %>%
      select(-out_MWh_) ->
      L1233.out_MWh_state_elec_F_tech_cool

    # aggregate and compute shares for Federeal Energy Regulatory Commission (FERC) subregions; ...
    # ... filtering for power plants built this century for contempory output estimates
    USC_db_adj %>%
      filter(`First Year of Operation` >= gcamusa.UCS_WATER_FIRST_YEAR) %>%
      left_join_error_no_match(select(states_subregions, state, grid_region), by = "state") ->
      USC_db_adj_2000s

    USC_db_adj_2000s %>%
      group_by(grid_region, sector, fuel, technology) %>%
      summarise(out_MWh_ = sum(out_MWh)) %>%
      ungroup() ->
      L1233.out_MWh_sR_elec_F_tech  # all technologies

    USC_db_adj_2000s %>%
      group_by(grid_region, sector, fuel, technology, cooling_system, water_type) %>%
      summarise(out_MWh = sum(out_MWh)) %>%
      ungroup() %>%
      left_join_error_no_match(L1233.out_MWh_sR_elec_F_tech,
                               by = c("grid_region", "sector", "fuel", "technology")) %>%
      mutate(share = out_MWh / out_MWh_) %>%
      select(-out_MWh_) ->
      L1233.out_MWh_sR_elec_F_tech_cool  # cooling technologies

    # calculate national averages, to be used as default values where data are missing
    # we separate states with and without seawater availability to calculate separate shares for these states.
    # therefore we have a national avg for non seawater states and for seawater states
    L1233.out_MWh_state_elec_F_tech %>%
      filter(state %in% seawater_states_basins) %>%
      group_by(sector, fuel, technology) %>%
      summarise(out_MWh_sea = sum(out_MWh_)) %>%
      ungroup() ->
      L1233.out_MWh_USA_elec_F_tech_sea

    L1233.out_MWh_state_elec_F_tech %>%
      filter(!(state %in% seawater_states_basins)) %>%
      group_by(sector, fuel, technology) %>%
      summarise(out_MWh_ = sum(out_MWh_)) %>%
      ungroup() ->
      L1233.out_MWh_USA_elec_F_tech

    L1233.out_MWh_USA_elec_F_tech_sea %>%
      left_join_error_no_match(L1233.out_MWh_USA_elec_F_tech,
                               by = c("sector", "fuel", "technology")) %>%
      replace_na(list(out_MWh_sea = 0, out_MWh_=0)) ->
      L1233.out_MWh_USA_elec_F_tech

    L1233.out_MWh_state_elec_F_tech_cool %>%
      filter(state %in% seawater_states_basins) %>%
      group_by(sector, fuel, technology, cooling_system, water_type) %>%
      summarise(out_MWh.sea = sum(out_MWh)) %>%
      ungroup() -> L1233.out_MWh_state_elec_F_tech_cool_sea

    L1233.out_MWh_state_elec_F_tech_cool %>%
      filter(!(state %in% seawater_states_basins)) %>%
      group_by(sector, fuel, technology, cooling_system, water_type) %>%
      summarise(out_MWh = sum(out_MWh)) %>%
      ungroup() -> L1233.out_MWh_state_elec_F_tech_cool_nosea

    L1233.out_MWh_state_elec_F_tech_cool_sea %>%
      # sea and nosea tables have different tech / cooling system / water type combinations
      # all of which need to be reflected in final table.  Thus, full_join() is used, and
      # NA values are replaced with zeros below
      full_join(L1233.out_MWh_state_elec_F_tech_cool_nosea,
                by = c("sector", "fuel", "technology", "cooling_system", "water_type")) %>%
      replace_na(list(out_MWh = 0, out_MWh.sea=0)) %>%
      left_join_error_no_match(L1233.out_MWh_USA_elec_F_tech,
                               by = c("sector", "fuel", "technology")) %>%
      mutate(share_nat.sea = out_MWh.sea / out_MWh_sea, share_nat =  out_MWh / out_MWh_) %>%
      select(-out_MWh_, -out_MWh, -out_MWh_sea, -out_MWh.sea) ->
      L1233.out_MWh_USA_elec_F_tech_cool

    # get all possible combinations of power plants, cooling system types, and water types in all states
    L1233.out_MWh_state_elec_F_tech_cool %>%
      select(-out_MWh, -share, -state) %>%
      unique ->
      L1233.elec_tech_cool

    L1233.elec_tech_cool %>%
      repeat_add_columns(tibble(state = gcamusa.STATES)) %>%
      left_join_keep_first_only(L1233.out_MWh_state_elec_F_tech_cool,
                                by = c("state", "sector", "fuel", "technology", "cooling_system", "water_type")) %>%
      # ^^ lots of NAs induced in the above join... most should be zeros;
      # where technologies have all zeros, replace with national averages
      replace_na(list(share = 0)) %>%
      select(-out_MWh) %>%
      left_join_error_no_match(L1233.out_MWh_USA_elec_F_tech_cool,
                               by = c("sector", "fuel", "technology", "cooling_system", "water_type")) %>%
      # ^^ brings in national average data
      group_by(state, sector, fuel, technology) %>%
      mutate(sum_share_tech = sum(share)) %>%
      ungroup() %>%
      mutate(share = if_else(sum_share_tech == 0 & (state %in% seawater_states_basins), share_nat.sea,
                             if_else(sum_share_tech == 0 & !(state %in% seawater_states_basins), share_nat, share))) %>%
      # we want to make sure that states that do not have seawater available (i.e. inland states) do not get the national average added
      select(-share_nat, -share_nat.sea, -sum_share_tech) %>%
      # multiply through by historical output
      left_join(L1231.out_EJ_state_elec_F_tech,
                by = c("sector", "fuel", "technology", "state")) %>%
      # ^^ non-restricted join used to allow for expanded rows for all historical years
      mutate(value = value * share) ->
      L1233.out_EJ_state_elec_F_tech_cool

    # Add GCAM-USA supplysector names and correct subsector vintages
    L1233.out_EJ_state_elec_F_tech_cool %>%
      # left_join here as numerous new rows are created due to vintages addition
      left_join(A23.elecS_tech_mapping_cool %>%
                  select(technology, Electric.sector, Electric.sector.technology, water_type),
                by = c("technology", "water_type")) %>%
      filter(year %in% MODEL_YEARS) %>%
      unique() ->
      L1233.out_EJ_state_elec_F_tech_cool

    # Get shares by FERC subregion for new investment in future periods
    L1233.elec_tech_cool %>%
      repeat_add_columns(tibble(grid_region = unique(states_subregions$grid_region))) %>%
      left_join_keep_first_only(L1233.out_MWh_sR_elec_F_tech_cool,
                                by = c("grid_region", "sector", "fuel", "technology", "cooling_system", "water_type")) %>%
      # ^^ lots of NAs induced in the above join... most should be zeros;
      # where technologies have all zeros, replace with national averages
      replace_na(list(share = 0)) %>%
      left_join_error_no_match(L1233.out_MWh_USA_elec_F_tech_cool,
                               by = c("sector", "fuel", "technology", "cooling_system", "water_type")) %>%
      # ^^ brings in national average data
      group_by(grid_region, sector, fuel, technology) %>%
      mutate(sum_share_tech = sum(share)) %>%
      ungroup() %>%
      mutate(share = if_else(sum_share_tech == 0, share_nat, share)) %>%
      select(-share_nat, -sum_share_tech, -out_MWh) %>%
      # join is intended to duplicate rows to introduce load segments
      # LJENM produces error, so left_join() is used
      left_join(A23.elecS_tech_mapping_cool %>%
                  select(technology, Electric.sector, Electric.sector.technology),
                by = c("technology")) %>%
      unique() %>%
      select(-sector, -technology) %>%
      rename(sector = Electric.sector,
             technology = Electric.sector.technology) ->
      L1233.share_sR_elec_F_tech_cool

    # Rename sector and subsectors based on GCAM-USA set-up
    L1233.out_EJ_state_elec_F_tech_cool %>%
      select(-sector) %>%
      rename(sector = Electric.sector,
             subsector = Electric.sector.technology) ->
      L1233.out_EJ_state_elec_F_tech_cool

    # ===================================================

    L1233.out_EJ_state_elec_F_tech_cool %>%
      add_title("Electricity output by state / fuel / technology / cooling system / water type") %>%
      add_units("EJ") %>%
      add_comments("Electricity output by state / fuel / technology / cooling system / water type data source: UCS") %>%
      add_legacy_name("L1233.out_EJ_state_elec_F_tech_cool") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/UCS_tech_names",
                     "gcam-usa/UCS_water_types",
                     "gcam-usa/UCS_Database",
                     "gcam-usa/usa_seawater_states_basins",
                     "L1231.out_EJ_state_elec_F_tech",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L1233.out_EJ_state_elec_F_tech_cool

    L1233.share_sR_elec_F_tech_cool %>%
      add_title("Electricity generation shares by grid region / fuel / technology / cooling system / water type") %>%
      add_units("Unitless") %>%
      add_comments("Electricity generation shares by grid region / fuel / technology / cooling system / water type") %>%
      add_legacy_name("L1233.share_sR_elec_F_tech_cool") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/UCS_tech_names",
                     "gcam-usa/UCS_water_types",
                     "gcam-usa/UCS_Database",
                     "gcam-usa/usa_seawater_states_basins",
                     "gcam-usa/A23.elecS_tech_mapping_cool") ->
      L1233.share_sR_elec_F_tech_cool


    return_data(L1233.out_EJ_state_elec_F_tech_cool, L1233.share_sR_elec_F_tech_cool)
  } else {
    stop("Unknown command")
  }
}
