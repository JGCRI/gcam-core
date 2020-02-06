# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LB1236.elec_load_segments_solver_USA
#'
#' Calculate the fraction of electricity generation by fuel by horizontal load segment such that the total supply
#' of electricity in each grid region matches total demand of electricity in that grid region.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1236.grid_elec_supply_USA}.
#'
#' The corresponding file in the original data system was \code{LB1236.elec_load_segments_solver_2010.R} (gcam-usa level1).
#' @details Calculates the fraction of electricity generation by fuel, by horizontal load segment, by grid region, in 2010.
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter mutate pull select
#' @author MTB August 2018
module_gcamusa_LB1236.elec_load_segments_solver_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/elecS_horizontal_to_vertical_map",
             "L1234.out_EJ_grid_elec_F",
             "L1235.grid_elec_supply_USA",
             "L1235.elecS_demand_fraction_USA",
             "L1235.elecS_horizontal_vertical_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1236.grid_elec_supply_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    grid_region <- segment <- fuel <- year <- segment_year <- generation <- fraction <- demand <- demand_fraction <-
      vertical_segment <- vertical_segment_demand <- horizontal_segment <- horizontal_segment_demand <-
      off.peak.electricity <- off.peak.electricity.demand <- off.peak.electricity.time <- off.peak.electricity.tot <-
      intermediate.electricity <- intermediate.electricity.demand <- intermediate.electricity.time <- intermediate.electricity.tot <-
      subpeak.electricity <- subpeak.electricity.demand <- subpeak.electricity.time <- subpeak.electricity.tot <-
      peak.electricity <- peak.electricity.demand <- peak.electricity.time <- peak.electricity.tot <-
      sector <- tot_generation <- grid_total <- grid_share_fuel <- tot_demand <- check <- pct_check <-
      supplysector <- subsector <- technology <- minicam.energy.input <- coefficient <-
      generation.x <- generation.x.x <- generation.y <- generation.y.y <-
      base_intermediate <- base_subpeak <- base_peak <- int_peak <- int_subpeak <- subpeak_peak <-
      off.peak.electricity.x <- off.peak.electricity.x.x <- off.peak.electricity.y <- off.peak.electricity.y.y <-
      intermediate.electricity.x <- intermediate.electricity.x.x <-  intermediate.electricity.y <- intermediate.electricity.y.y <-
      subpeak.electricity.x <- subpeak.electricity.x.x <- subpeak.electricity.y <- subpeak.electricity.y.y <-
      vertical_segment_demand.x <- vertical_segment_demand.x.x <- vertical_segment_demand.y <- vertical_segment_demand.y.y <-
      peak.electricity.x <- peak.electricity.x.x <- peak.electricity.y <- peak.electricity.y.y <-
      segment.x <- segment.x.x <- segment.y <- segment.y.y <- . <- uniroot <- fraction.y <- non_segment_frac <-
      non_int_frac <- NULL # silence package check notes

    # Load required inputs
    elecS_horizontal_to_vertical_map <- get_data(all_data, "gcam-usa/elecS_horizontal_to_vertical_map")
    L1234.out_EJ_grid_elec_F <- get_data(all_data, "L1234.out_EJ_grid_elec_F")
    L1235.grid_elec_supply_USA <- get_data(all_data, "L1235.grid_elec_supply_USA")
    L1235.elecS_demand_fraction_USA <- get_data(all_data, "L1235.elecS_demand_fraction_USA")
    L1235.elecS_horizontal_vertical_USA <- get_data(all_data, "L1235.elecS_horizontal_vertical_USA")

    # ===================================================
    # Data Processing

    # Initialize Variables
    L1236.elecS_demand_fraction <- L1235.elecS_demand_fraction_USA
    L1236.elecS_horizontal_vertical <- L1235.elecS_horizontal_vertical_USA

    # Filter for years for which electricity load segments will be calibrated
    L1236.grid_elec_supply <- L1235.grid_elec_supply_USA %>%
      filter(year %in% gcamusa.LOAD_SEG_CAL_YEARS)

    # Summarize generation by year / grid region / fuel; remove distinction between solar PV & CSP
    L1234.out_EJ_grid_elec_F %>%
      filter(year %in% gcamusa.LOAD_SEG_CAL_YEARS) %>%
      mutate(fuel = sub("solar CSP", "solar", fuel),
             fuel = sub("solar PV", "solar", fuel)) %>%
      group_by(grid_region, sector, year, fuel) %>%
      summarise(tot_generation = sum(generation)) %>%
      ungroup() -> L1236.out_EJ_grid_elec_F

    # Join in total generation data by year / grid region / fuel from L1236.out_EJ_grid_elec_F
    L1236.grid_elec_supply %>%
      left_join_error_no_match(L1236.out_EJ_grid_elec_F,
                               by = c("grid_region", "year", "fuel")) %>%
      select(grid_region, segment, fuel, year, tot_generation, fraction, generation ) -> L1236.grid_elec_supply

    # Create a table to hold data for years for which electricity load segments will not be calibrated
    # Even though the electricity load segment shares for these years will not be calibrated,
    # we need to carry this historical data forward.
    # Process is similar to the above for calibrated years:
    # Remove distinction between solar PV & CSP;
    # Summarize generation by year / grid region / fuel;
    # Filter for years for which electricity load segments will not be calibrated
    L1235.grid_elec_supply_USA %>%
      filter(!(year %in% gcamusa.LOAD_SEG_CAL_YEARS)) %>%
      left_join_error_no_match(L1234.out_EJ_grid_elec_F %>%
                                 mutate(fuel = sub("solar CSP", "solar", fuel),
                                        fuel = sub("solar PV", "solar", fuel)) %>%
                                 group_by(grid_region, sector, year, fuel) %>%
                                 summarise(tot_generation = sum(generation)) %>%
                                 ungroup(),
                               by = c("grid_region", "year", "fuel")) %>%
      select(grid_region, segment, fuel, year, tot_generation, fraction, generation ) -> L1236.grid_elec_supply_non_cal

    # List of horizontal and vertical electricity segments
    L1236.segment_list <- unique(elecS_horizontal_to_vertical_map$horizontal_segment)
    L1236.vertical_segment_list <- unique(elecS_horizontal_to_vertical_map$vertical_segment)

    L1236.gridregion_list <- unique(L1236.grid_elec_supply$grid_region)

    # Function for replacing the existing fraction of a fuel consumed in a given load segment
    # (by grid region & year) with a new value
    replace_fraction <- function(data, elec_fuel, load_segment, new_fraction) {
      data %>%
        mutate(fraction = replace(fraction, grid_region == L1236.region &
                                    fuel == elec_fuel &
                                    segment == load_segment &
                                    year == segment_year,
                                  new_fraction))
    }

    # Function for calculating the fraction of a fuel consumed by other load segments in a given grid region & year
    calc_non_segment_frac <- function(data, elec_fuel, load_segment) {
      data %>%
        filter(grid_region == L1236.region &
                 fuel == elec_fuel &
                 segment != load_segment &
                 year == segment_year) %>%
        summarise(non_segment_frac = sum(fraction)) %>%
        pull(non_segment_frac)
    }

    # Function to check that electricity demands and supplies match by load segment and grid region
    # Function is subsequently solved by uniroot() - One Dimensional Root (Zero) Finding -
    # see https://www.rdocumentation.org/packages/stats/versions/3.5.3/topics/uniroots
    check_elec_segments <- function(gen_fraction, L1236.region, L1236.segment, L1236.fuel = "gas") {

      # Set fraction as specified
      L1236.grid_elec_supply %>%
        replace_fraction(L1236.fuel, L1236.segment, gen_fraction) -> L1236.grid_elec_supply

      # If fuel == gas or oil, adjust fraction of fuel consumed in peak load segment to make sure that sum of fractions is 1
      if (L1236.fuel == "gas" | L1236.fuel == "refined liquids") {

        L1236.grid_elec_supply %>%
          calc_non_segment_frac(L1236.fuel, gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

        L1236.grid_elec_supply %>%
          replace_fraction(L1236.fuel, gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

      }

      # If fuel == coal, adjust fraction of fuel consumed in intermediate load segment to make sure sum of fractions is 1
      if (L1236.fuel == "coal" | L1236.fuel == "hydro") {

        L1236.grid_elec_supply %>%
          calc_non_segment_frac(L1236.fuel, gcamusa.ELEC_SEGMENT_INT) -> L1236.non_int

        L1236.grid_elec_supply %>%
          replace_fraction(L1236.fuel, gcamusa.ELEC_SEGMENT_INT, 1 - L1236.non_int) -> L1236.grid_elec_supply

      }

      L1236.grid_elec_supply %>%
        mutate(generation = tot_generation * fraction) -> L1236.grid_elec_supply

      # Calculate electricity supply by horizontal segment in each grid region by aggregating all technologies
      L1236.grid_elec_supply %>%
        group_by(grid_region, segment, year) %>%
        summarise(generation = sum(generation)) %>%
        ungroup() -> L1236.grid_check

      # Calculate electricity demand for each horizontal segment in each grid region
      L1236.grid_elec_supply %>%
        group_by(grid_region, year) %>%
        summarise(tot_demand = sum(generation)) %>%
        ungroup() -> L1236.grid_elec_demand

      L1236.grid_check %>%
        left_join_error_no_match(L1236.grid_elec_demand,
                                 by = c("grid_region","year")) %>%
        left_join_error_no_match(elecS_horizontal_to_vertical_map,
                                 by = c("segment" = "horizontal_segment")) %>%
        left_join_error_no_match (L1236.elecS_demand_fraction ,
                                  by = c("grid_region", "vertical_segment")) %>%
        mutate(vertical_segment_demand = tot_demand * demand_fraction) -> L1236.grid_elec_demand

      L1236.grid_check %>%
        left_join_error_no_match(L1236.grid_elec_demand,
                                 by = c("grid_region", "segment", "year")) %>%
        select(grid_region, segment, year, generation.x, vertical_segment_demand) %>%
        rename(generation = generation.x) -> L1236.grid_check

      # Prepare tables to check that supplies and demands balance for each load segment.  For each horizontal (supply-side) load segment:
      # (1) Filter for the relevant load segment.
      # (2) Join L1236.elecS_horizontal_vertical.  This table outlines how generation in the horizontal (supply-side) load segments -
      # base load generation, intermeidate generation, subpeak generation, peak generation - are shared across the four vertical
      # (demand-side) load segments - off.peak.electricity, intermediate.electricity, subpeak.electricity, peak.electricity.
      # (3) Calculate the size of generation in the horizontal load segment across all of the relevant vertical segments.
      # For example, base load generation provides all of off.peak.electricity demand plus a portion of intermediate.electricity,
      # subpeak.electricity, and peak.electricity demands.  Intermediate generation serves the remaining portion of
      # intermediate.electricity as well as some subpeak.electricity and peak.electricity demands.  Peak generation serves only the
      # portion of peak.electricity demands not met by generation from the other horizontal load segments.

      L1236.grid_check %>%
        filter(segment == gcamusa.ELEC_SEGMENT_BASE ) %>%
        left_join_error_no_match(L1236.elecS_horizontal_vertical,
                                 by = c("grid_region", "segment" = "horizontal_segment")) %>%
        # Calculate total demand for base load generation. This is equal to the demand for off.peak.electricity divided by the
        # share of base load generation that serves off.peak.electricity (to account for the fact that base load generation
        # also serves a portion of intermediate.electricity, subpeak.electricity, and peak.electricity demands).
        mutate(horizontal_segment_demand = vertical_segment_demand / off.peak.electricity,
               # The below three calculations are not relevant for base load generation but will be used in calculations
               # for the other three horizontal load segments below.
               # Calculate amount of base load generation that serves the vertical intermediate.electricity segment
               base_intermediate = horizontal_segment_demand * intermediate.electricity,
               # Calculate amount of base load generation that serves the vertical subpeak.electricity segment
               base_subpeak = horizontal_segment_demand * subpeak.electricity,
               # Calculate amount of base load generation that serves the vertical peak.electricity segment
               base_peak = horizontal_segment_demand * peak.electricity) -> L1236.grid_check_base

      L1236.grid_check %>%
        filter(segment == gcamusa.ELEC_SEGMENT_INT ) %>%
        left_join_error_no_match(L1236.elecS_horizontal_vertical,
                                 by = c("grid_region", "segment" = "horizontal_segment")) %>%
        left_join_error_no_match(L1236.grid_check_base %>%
                                   select(grid_region, year, base_intermediate),
                                 by = c("grid_region", "year")) %>%
        # Calculate total demand for intermediate generation (horizontal segment).  This is equal to the demand
        # for intermediate.electricity (vertical segment) minus the amount of intermediate.electricity served by
        # base load generation, divided by the share of intermediate generation that serves intermediate.electricity
        # (to account for the fact that intermediate generation also serves a portion of subpeak.electricity and peak.electricity demands).
        mutate(horizontal_segment_demand = (vertical_segment_demand - base_intermediate) /
                 intermediate.electricity ,
               # Calculate amount of intermediate generation that serves the vertical subpeak.electricity segment
               int_subpeak = horizontal_segment_demand * subpeak.electricity,
               # Calculate amount of intermediate generation that serves the vertical peak.electricity segment
               int_peak = horizontal_segment_demand * peak.electricity) -> L1236.grid_check_int

      L1236.grid_check %>%
        filter(segment == gcamusa.ELEC_SEGMENT_SUBPEAK ) %>%
        left_join_error_no_match(L1236.elecS_horizontal_vertical,
                                 by = c("grid_region", "segment" = "horizontal_segment")) %>%
        left_join_error_no_match(L1236.grid_check_base %>%
                                   select(grid_region, year, base_subpeak),
                                 by = c("grid_region", "year")) %>%
        left_join_error_no_match(L1236.grid_check_int %>%
                                   select(grid_region, year, int_subpeak),
                                 by = c("grid_region", "year")) %>%
        # Calculate total demand for subpeak generation (horizontal segment).  This is equal to the demand
        # for subpeak.electricity (vertical segment) minus the amount of  subpeak.electricity served by
        # base load generation and intermediate generation, divided by the share of subpeak generation that serves subpeak.electricity
        # (to account for the fact that subpeak generation also serves a portion of peak.electricity demands).
        mutate(horizontal_segment_demand = (vertical_segment_demand - base_subpeak - int_subpeak) /
                 subpeak.electricity,
               # Calculate amount of subpeak generation that serves the vertical peak.electricity segment
               subpeak_peak = horizontal_segment_demand * peak.electricity) -> L1236.grid_check_subpeak

      L1236.grid_check %>%
        filter(segment == gcamusa.ELEC_SEGMENT_PEAK ) %>%
        left_join_error_no_match(L1236.elecS_horizontal_vertical,
                                 by = c("grid_region", "segment" = "horizontal_segment")) %>%
        left_join_error_no_match(L1236.grid_check_base %>%
                                   select(grid_region, year, base_peak),
                                 by = c("grid_region", "year")) %>%
        left_join_error_no_match(L1236.grid_check_int %>%
                                   select(grid_region, year, int_peak),
                                 by = c("grid_region", "year")) %>%
        left_join_error_no_match(L1236.grid_check_subpeak %>%
                                   select(grid_region, year, subpeak_peak),
                                 by = c("grid_region", "year")) %>%
        # Calculate total demand for peak generation (horizontal segment).  This is equal to the demand
        # for peak.electricity (vertical segment) minus the amount of peak.electricity served by
        # base load generation, intermediate generation, and subpeak generation.
        mutate(horizontal_segment_demand = (vertical_segment_demand - base_peak - int_peak - subpeak_peak) /
                 peak.electricity) ->  L1236.grid_check_peak

      # Filter for the information needed going forward.  We needed to carry some additional information
      # previously to build each of the tables below.
      L1236.grid_check_base %>%
        select(grid_region, segment, year, generation,
               vertical_segment_demand, horizontal_segment_demand) -> L1236.grid_check_base

      L1236.grid_check_int %>%
        select(grid_region, segment, year, generation,
               vertical_segment_demand, horizontal_segment_demand) -> L1236.grid_check_int

      L1236.grid_check_subpeak %>%
        select(grid_region, segment, year, generation,
               vertical_segment_demand, horizontal_segment_demand) -> L1236.grid_check_subpeak

      L1236.grid_check_peak %>%
        select(grid_region, segment, year, generation,
               vertical_segment_demand, horizontal_segment_demand) -> L1236.grid_check_peak

      L1236.grid_check_base %>%
        bind_rows(L1236.grid_check_int, L1236.grid_check_subpeak, L1236.grid_check_peak) -> L1236.grid_check

      # Check that supply meets demand for each load segment, i.e. that generation from a given horizontal
      # electricity load segment matches demand for this generation across the four vertical load segments
      L1236.grid_check %>%
        mutate(check = horizontal_segment_demand - generation,
               pct_check = check / generation) -> L1236.grid_check

      L1236.grid_check %>%
        filter(grid_region == L1236.region & segment == L1236.segment & year == segment_year) %>%
        select(check) %>%
        pull(check) -> check

      check
    }


    # Calculate total electricity generation by year / grid region
    L1236.out_EJ_grid_elec_F %>%
      group_by(grid_region, sector, year) %>%
      summarise(grid_total = sum(tot_generation)) %>%
      ungroup() -> L1236.grid_total

    # Calculate the share of generation from a given fuel across load segment by year / grid region
    L1236.out_EJ_grid_elec_F %>%
      left_join_error_no_match(L1236.grid_total, by = c("grid_region", "sector", "year")) %>%
      mutate(grid_share_fuel = tot_generation / grid_total) -> L1236.out_EJ_grid_elec_F


    # For each grid region and year, calculate the fraction of electricity generation by fuel by horizontal load segment such that
    # electricity supplies and demands balance.
    for (r in seq_along(L1236.gridregion_list)){
      for(y in seq_along(gcamusa.LOAD_SEG_CAL_YEARS)){
        L1236.region <- L1236.gridregion_list[r]
        segment_year <- gcamusa.LOAD_SEG_CAL_YEARS[y]

        # Calculate fractions of electricity generation by fuel for particular fuels
        L1236.out_EJ_grid_elec_F %>%
          filter(grid_region == L1236.region & fuel == "gas" & year == segment_year) %>%
          select(grid_share_fuel) %>%
          pull(grid_share_fuel) -> L1236.gas_frac

        L1236.out_EJ_grid_elec_F %>%
          filter(grid_region == L1236.region & fuel == "refined liquids" & year == segment_year) %>%
          select(grid_share_fuel) %>%
          pull(grid_share_fuel) -> L1236.oil_frac

        L1236.out_EJ_grid_elec_F %>%
          filter(grid_region == L1236.region & fuel == "coal" & year == segment_year) %>%
          select(grid_share_fuel) %>%
          pull(grid_share_fuel) -> L1236.coal_frac

        L1236.out_EJ_grid_elec_F %>%
          filter(grid_region == L1236.region & fuel == "hydro" & year == segment_year) %>%
          select(grid_share_fuel) %>%
          pull(grid_share_fuel) -> L1236.hydro_frac

        if (segment_year %in% c(2005, 1990)) {
          # For years 2005 & 1990, we use solved fractions from the most recent year as a starting point
          # for calculating the fuel fractions for the current year.
          # Map fractions from prevoius gcamusa.LOAD_SEG_CAL_YEARS to current segment_year.
          L1236.grid_elec_supply %>%
            filter(year == segment_year,
                   grid_region == L1236.region) %>%
            left_join_error_no_match(L1236.grid_elec_supply %>%
                        filter(year > segment_year) %>%
                        # NOTE:  can't combine these filters because doing so filters out all entries
                        filter(year == min(year)) %>%
                        select(-year, -tot_generation, -generation),
                      by = c("grid_region", "segment", "fuel")) %>%
            mutate(fraction = fraction.y,
                   generation = tot_generation * fraction) %>%
            select(grid_region, segment, fuel, year, tot_generation, fraction, generation) %>%
            bind_rows(L1236.grid_elec_supply %>%
                        filter(year != segment_year | grid_region != L1236.region)) -> L1236.grid_elec_supply

        }

        if (segment_year %in% c(2010, 2005) & L1236.oil_frac > 0.5) {
          # For oil heavy regions such as Hawaii grid, solve for oil fractions.
          # This will allow for some oil in baseload and intermediate segments.
          # Solve for oil fractions
          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_BASE, "refined liquids")

          L1236.grid_elec_supply %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_BASE, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("refined liquids", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_INT, "refined liquids")

          L1236.grid_elec_supply %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_INT, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("refined liquids", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, "refined liquids")

          L1236.grid_elec_supply %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("refined liquids", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

        } else if (segment_year == 1990 & L1236.hydro_frac > 0.5) {
          # For hydro-heavy regions such as Northwest grid, allocate some hydro to the intermediate segment.
          # Such regions have an excess of base load technologies, so assign some coal to the subpeak and peak segments.
          # This is in no way perfect and could be revisited / improved.
          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_BASE, "hydro")

          L1236.grid_elec_supply %>%
            replace_fraction("hydro", gcamusa.ELEC_SEGMENT_BASE, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("hydro", gcamusa.ELEC_SEGMENT_INT) -> L1236.non_int

          L1236.grid_elec_supply %>%
            replace_fraction("hydro", gcamusa.ELEC_SEGMENT_INT, 1 - L1236.non_int) -> L1236.grid_elec_supply

          # Solve for some coal in subpeak and peak segments since there are not enough sources to supply those segments.
          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, "coal")

          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_SUBPEAK, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("coal", gcamusa.ELEC_SEGMENT_INT) -> L1236.non_int

          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_INT, 1 - L1236.non_int) -> L1236.grid_elec_supply

          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_PEAK, "coal")

          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_PEAK, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("coal", gcamusa.ELEC_SEGMENT_INT) -> L1236.non_int

          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_INT, 1 - L1236.non_int) -> L1236.grid_elec_supply

        } else if (segment_year == 1990 & L1236.oil_frac > 0.2) {
          # For oil heavy regions such as Hawaii grid, solve for oil fractions.
          # This will allow for some oil in baseload and intermediate segments.
          # Solve for oil fractions after removing gas from baseload.
          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_BASE, 0) %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, 0.9) %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.1) %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 0) -> L1236.grid_elec_supply

          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_BASE, "refined liquids")

          L1236.grid_elec_supply %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_BASE, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("refined liquids", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_INT, "refined liquids")

          L1236.grid_elec_supply %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_INT, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("refined liquids", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, "refined liquids")

          L1236.grid_elec_supply %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("refined liquids", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

        } else if (segment_year == 2010 & L1236.gas_frac > 0.2) {
          # If a grid region has 20% or more gas, first assign refined liquids to the baseload segment, then solve for gas fractions.
          L1236.grid_elec_supply %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_BASE, 1) %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_INT, 0) %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, 0) %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 0) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            mutate(generation = tot_generation * fraction) -> L1236.grid_elec_supply

          # Solve for gas fractions
          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_BASE)

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_BASE, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("gas", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_INT)

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("gas", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK)

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("gas", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

        } else if (segment_year == 2005 & L1236.gas_frac > 0.2 & L1236.coal_frac < 0.5) {
          # If a grid region has 20% or more gas and less than 50% coal, solve for gas fractions.
          # To reduce solution error, increase coal in base load and assign some oil to base load.
          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_BASE, 0.8) %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_INT, 0.2) %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_BASE, 0.1) %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_INT, 0.8) %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.1) %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 0) -> L1236.grid_elec_supply

          # Solve for gas fractions
          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_BASE)

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_BASE, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("gas", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_INT)

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("gas", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK)

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("gas", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

        } else if (segment_year == 1990 & L1236.gas_frac > 0.2 & L1236.coal_frac < 0.5) {
          # If a grid region has 20% or more gas and less than 50% coal, solve for gas fractions.
          # To reduce solution error, increase coal in base load and assign some oil to base load.
          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_BASE, 0.95) %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_INT, 0.05) %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_BASE, 0.1) %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_INT, 0.8) %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.1) %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 0) -> L1236.grid_elec_supply

          # Solve for gas fractions
          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_BASE)

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_BASE, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("gas", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_INT)

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("gas", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK)

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("gas", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

        } else if (segment_year == 2010 & L1236.coal_frac > L1236.gas_frac & L1236.coal_frac < 0.55) {

          # For regions with moderate levels of coal such as Southeast grid, Northwest grid, Mid-Atlantic grid, and Central Southwest grid,
          # allocate some coal to intermediate.  Solve for coal in baseload and assign the remaining to the intermediate segment.
          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_BASE, "coal")

          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_BASE, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("coal", gcamusa.ELEC_SEGMENT_INT) -> L1236.non_int

          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_INT, 1 - L1236.non_int) -> L1236.grid_elec_supply

          # Solve for gas fractions in intermediate, subpeak and peak
          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_INT, "gas")

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("gas", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, "gas")

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("gas", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

        } else if (segment_year == 2005 & L1236.coal_frac > L1236.gas_frac & L1236.coal_frac < 0.55) {
          # For regions with moderate levels of coal such as Southeast grid, Northwest grid, Mid-Atlantic grid, and Central Southwest grid,
          # allocate some gas and refined liquids to the intermediate segment.
          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_BASE, 0) %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, 0.6) %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.3) %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 0.1) %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_BASE, 0) %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_INT, 0.8) %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.2) %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 0) -> L1236.grid_elec_supply

          # Solve for coal in baseload and assign the remaining to intermediate
          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_BASE, "coal")

          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_BASE, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            filter(grid_region == L1236.region & fuel == "coal" & segment != gcamusa.ELEC_SEGMENT_INT & year == segment_year) %>%
            summarise(non_int_frac = sum(fraction)) %>%
            pull(non_int_frac) -> L1236.non_int

          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_INT, 1 - L1236.non_int) -> L1236.grid_elec_supply

          # Solve for gas fractions in intermediate, subpeak and peak
          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_INT, "gas")

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("gas", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, "gas")

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("gas", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

        } else if (segment_year == 1990 & L1236.coal_frac > L1236.gas_frac & L1236.coal_frac < 0.52) {
          # For regions with moderate levels of coal such as Southeast grid, Northwest grid, and Central Southwest grid,
          # move refined liquids to subpeak. Then solve for coal in base load and allocate remaining coal to intermediate.
          # Assign some gas and refined liquids into the intermediate and subpeak segments.
          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_BASE, 0)  %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, 0.6)  %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.25)  %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 0.15) %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_BASE, 0.2)  %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_INT, 0.8)  %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, 0)  %>%
            replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 0) -> L1236.grid_elec_supply

          # Solve for coal fractions
          # Solve for coal in baseload and assign the remaining to intermediate
          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_BASE, "coal")

          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_BASE, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("coal", gcamusa.ELEC_SEGMENT_INT) -> L1236.non_int

          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_INT, 1 - L1236.non_int) -> L1236.grid_elec_supply

          # Solve for gas fractions in intermediate, subpeak and peak
          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_INT, "gas")

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("gas", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, "gas")

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("gas", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

        } else if (segment_year %in% c(2010, 2005)) {
          # In regions with high levels of coal such as Central Northeast, Central East, Central Northwest and Sothwest grids,
          # allocate some wind into intermediate, and subpeak segments,and (for 2005) assign some refined liquids into the subpeak.
          # Note that we also read in different load curves for these regions with lower peak demand.
          L1236.grid_elec_supply %>%
            replace_fraction("wind", gcamusa.ELEC_SEGMENT_BASE, 0.6) %>%
            replace_fraction("wind", gcamusa.ELEC_SEGMENT_INT, 0.25) %>%
            replace_fraction("wind", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.15) -> L1236.grid_elec_supply

          if (segment_year == 2005) {
            # Assigning all refined liquids into subpeak
            L1236.grid_elec_supply  %>%
              replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, 1) %>%
              replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 0) -> L1236.grid_elec_supply
          }

          # Solve for coal in baseload and assign the remaining to intermediate
          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_BASE, "coal")

          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_BASE, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("coal", gcamusa.ELEC_SEGMENT_INT) -> L1236.non_int

          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_INT, 1 - L1236.non_int) -> L1236.grid_elec_supply

          # Solve for gas fractions in intermediate, subpeak and peak
          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_INT, "gas")

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("gas", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, "gas")

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("gas", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

        } else if (segment_year == 1990)  {
          # In regions with high levels of coal such as Central Northeast, Central East, Central Northwest and Sothwest grids,
          # allocate some wind into intermediate, and subpeak segments first. Second, remove gas from the baseload and assign
          # some refined liquids into the subpeak. It was found that even then, there are not enough sources to supply the
          # subpeak. So we solve for some coal in the subpeak. That is potentially problematic and might need to be verified
          # against real-world data.
          # Note that we also read in different load curves for these regions with lower peak demand.
          L1236.grid_elec_supply %>%
            replace_fraction("wind", gcamusa.ELEC_SEGMENT_BASE, 0.6) %>%
            replace_fraction("wind", gcamusa.ELEC_SEGMENT_INT, 0.25) %>%
            replace_fraction("wind", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.15) -> L1236.grid_elec_supply

          # Solve for coal in baseload and assign the remaining to intermediate
          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_BASE, "coal")

          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_BASE, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("coal", gcamusa.ELEC_SEGMENT_INT) -> L1236.non_int

          # First, solve for coal in baseload and assign the remaining to intermediate
          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_BASE, "coal")

          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_BASE, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("coal", gcamusa.ELEC_SEGMENT_INT) -> L1236.non_int

          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_INT, 1 - L1236.non_int) -> L1236.grid_elec_supply

          # Then, solve for some coal in subpeak and peak since there are not enough sources to supply that segment.
          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, "coal")

          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_SUBPEAK, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("coal", gcamusa.ELEC_SEGMENT_INT) -> L1236.non_int

          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_INT, 1 - L1236.non_int) -> L1236.grid_elec_supply

          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_PEAK, "coal")

          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_PEAK, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("coal", gcamusa.ELEC_SEGMENT_INT) -> L1236.non_int

          L1236.grid_elec_supply %>%
            replace_fraction("coal", gcamusa.ELEC_SEGMENT_INT, 1 - L1236.non_int) -> L1236.grid_elec_supply

          # Solve for gas fractions in intermediate, subpeak and peak
          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_INT, "gas")

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("gas", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

          L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, "gas")

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, L1236.solved_fraction$root) -> L1236.grid_elec_supply

          L1236.grid_elec_supply %>%
            calc_non_segment_frac("gas", gcamusa.ELEC_SEGMENT_PEAK) -> L1236.non_peak

          L1236.grid_elec_supply %>%
            replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply
        }
      }
    }

    # Re-join data for non calibrated years
    # Ensure that generation = total generation * calibrated load segment fuel fraction
    L1236.grid_elec_supply %>%
      bind_rows(L1236.grid_elec_supply_non_cal) %>%
      mutate(generation = tot_generation * fraction) -> L1236.grid_elec_supply

    # ===================================================

    # Produce outputs

    L1236.grid_elec_supply %>%
      add_title("Electricity supply by fuel by horizontal load segment in each grid region.") %>%
      add_units("EJ; unitless (fraction)") %>%
      add_comments("Electricity supply by fuel by horizontal load segment in each grid region.") %>%
      add_comments("Based on calculated fraction of fuel in the horizontal load segments.") %>%
      add_legacy_name("L1236.grid_elec_supply") %>%
      add_precursors("L1234.out_EJ_grid_elec_F",
                     "L1235.grid_elec_supply_USA",
                     "L1235.elecS_demand_fraction_USA",
                     "L1235.elecS_horizontal_vertical_USA",
                     "gcam-usa/elecS_horizontal_to_vertical_map") ->
      L1236.grid_elec_supply_USA

    return_data(L1236.grid_elec_supply_USA)

  } else {
    stop("Unknown command")
  }
}
