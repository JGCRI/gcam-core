#' module_gcam.usa_LB1238.elec_load_segments_solver_1990_USA
#'
#' Calculate the fraction of electricity generation by fuel by horizontal load segment in 1990 such that
#' the total supply of electricity in each grid region matches total demand of electricity in that grid.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1238.grid_elec_supply_USA}, \code{L1238.grid_elec_demand_USA}, \code{L1238.grid_check_USA}.
#'
#' The corresponding file in the original data system was \code{LB1238.elec_load_segments_solver_1990.R} (gcam-usa level1).
#' @details Calculates the fraction of electricity generation by fuel, by horizontal load segment, by grid region, in 1990.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author MTB August 2018
module_gcam.usa_LB1238.elec_load_segments_solver_1990_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/elecS_horizontal_to_vertical_map",
             "L1234.out_EJ_grid_elec_F",
             "L1235.elecS_demand_fraction_USA",
             "L1235.elecS_horizontal_vertical_USA",
             "L1237.grid_elec_supply_USA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1238.grid_elec_supply_USA",
             "L1238.grid_elec_demand_USA",
             "L1238.grid_check_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    grid_region <- segment <- fuel <- year <- script_year <- next_year<- generation <- fraction <- demand <-
      demand_fraction <- vertical_segment <- vertical_segment_demand <- horizontal_segment <- horizontal_segment_demand <-
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
      segment.x <- segment.x.x <- segment.y <- segment.y.y <- fraction.x <- fraction.y <- tot_generation.x <-
      year.x <- . <- uniroot <- NULL # silence package check notes

    # Load required inputs
    elecS_horizontal_to_vertical_map <- get_data(all_data, "gcam-usa/elecS_horizontal_to_vertical_map")
    L1234.out_EJ_grid_elec_F <- get_data(all_data, "L1234.out_EJ_grid_elec_F")
    L1235.elecS_demand_fraction_USA <- get_data(all_data, "L1235.elecS_demand_fraction_USA")
    L1235.elecS_horizontal_vertical_USA <- get_data(all_data, "L1235.elecS_horizontal_vertical_USA")
    L1237.grid_elec_supply_USA <- get_data(all_data, "L1237.grid_elec_supply_USA")

    # ===================================================
    # Data Processing

    #Initialize Variables
    script_year = MODEL_BASE_YEARS[2]
    next_year = MODEL_BASE_YEARS[3]
    L1238.elecS_demand_fraction <- L1235.elecS_demand_fraction_USA
    L1238.elecS_horizontal_vertical <- L1235.elecS_horizontal_vertical_USA

    L1238.grid_elec_supply <- L1237.grid_elec_supply_USA

    L1238.grid_elec_supply %>%
      filter(year == script_year) -> L1238.grid_elec_supply_script_year

    L1238.grid_elec_supply %>%
      filter(year == next_year) -> L1238.grid_elec_supply_next_year

    L1238.grid_elec_supply %>%
      filter(year != script_year) -> L1238.grid_elec_supply_non_script_year

    L1238.grid_elec_supply_script_year %>%
      left_join_error_no_match(L1238.grid_elec_supply_next_year, by = c("grid_region", "segment", "fuel")) %>%
      mutate(fraction.x = fraction.y) %>%
      mutate(generation.x = tot_generation.x * fraction.x) %>%
      select(grid_region, segment, fuel, year = year.x, tot_generation = tot_generation.x,
             fraction = fraction.x, generation = generation.x) %>%
      bind_rows(L1238.grid_elec_supply_non_script_year) -> L1238.grid_elec_supply

    L1234.out_EJ_grid_elec_F %>%
      mutate(fuel = sub("solar CSP", "solar", fuel)) %>%
      mutate(fuel = sub("solar PV", "solar", fuel)) %>%
      group_by(grid_region, sector, year, fuel) %>%
      summarise_at("generation", sum) %>%
      ungroup() %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(tot_generation = generation) -> L1238.out_EJ_grid_elec_F

    L1238.segment_list <- unique(elecS_horizontal_to_vertical_map$horizontal_segment)
    L1238.vertical_segment_list <- unique(elecS_horizontal_to_vertical_map$vertical_segment)
    L1238.gridregion_list <- unique(L1238.grid_elec_supply$grid_region)

    #Function to check demands and supplys by segment and L1238.region
    check_elec_segments <- function (gen_fraction, L1238.region, L1238.segment, L1238.fuel = "gas") {
      #set fraction as specified

      L1238.grid_elec_supply %>%
        mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == L1238.fuel
                                  & segment == L1238.segment & year == script_year, gen_fraction)) -> L1238.grid_elec_supply

      # If fuel = gas or oil, adjust peak fraction to make sure that sum of fractions is 1

      if (L1238.fuel == "gas" | L1238.fuel == "refined liquids") {

        L1238.grid_elec_supply %>%
          filter(grid_region == L1238.region & fuel == L1238.fuel & segment != L1238.segment_list[4] & year == script_year) %>%
          summarise(nonpeak_frac = sum(fraction)) %>%
          .[['nonpeak_frac']] -> L1238.nonpeak

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == L1238.fuel
                                    & segment == L1238.segment_list[4] & year == script_year,
                                    1 - L1238.nonpeak)) -> L1238.grid_elec_supply
      }

      # If fuel = coal or hydro, adjust intermediate fraction to make sure sum of fractions is 1

      if (L1238.fuel == "coal" | L1238.fuel == "hydro") {

        L1238.grid_elec_supply %>%
          filter(grid_region == L1238.region & fuel == L1238.fuel & segment != L1238.segment_list[2] & year == script_year) %>%
          summarise(non_int_frac = sum(fraction)) %>%
          .[['non_int_frac']] -> L1238.non_int

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == L1238.fuel
                                    & segment == L1238.segment_list[2] & year == script_year,
                                    1 - L1238.non_int)) -> L1238.grid_elec_supply
      }

      L1238.grid_elec_supply %>%
        mutate(generation = tot_generation * fraction) -> L1238.grid_elec_supply

      #Calculate supply by horizontal segment in each grid region by aggregating all technologies

      L1238.grid_elec_supply %>%
        group_by(grid_region, segment, year) %>%
        summarise_at("generation", sum) %>%
        ungroup() -> L1238.grid_check

      #Calculate demand for each horizontal segment by vertical segments in each grid region

      L1238.grid_elec_supply %>%
        group_by(grid_region, year) %>%
        summarise_at("generation", sum) %>%
        ungroup() %>%
        rename (tot_demand = generation) -> L1238.grid_elec_demand

      L1238.grid_check %>%
        left_join_error_no_match(L1238.grid_elec_demand, by = c("grid_region","year")) %>%
        left_join_error_no_match(elecS_horizontal_to_vertical_map, by = c("segment" = "horizontal_segment")) %>%
        left_join_error_no_match (L1238.elecS_demand_fraction , by = c("grid_region", "vertical_segment")) %>%
        mutate(vertical_segment_demand = tot_demand * demand_fraction) -> L1238.grid_elec_demand

      L1238.grid_check %>%
        left_join_error_no_match(L1238.grid_elec_demand, by = c("grid_region", "segment", "year")) %>%
        select(grid_region, segment, year, generation.x, vertical_segment_demand) %>%
        rename(generation = generation.x) -> L1238.grid_check

      L1238.grid_check %>%
        filter(segment == L1238.segment_list[1] ) -> L1238.grid_check_base

      L1238.grid_check %>%
        filter(segment == L1238.segment_list[2] ) -> L1238.grid_check_int

      L1238.grid_check %>%
        filter(segment == L1238.segment_list[3] ) -> L1238.grid_check_subpeak

      L1238.grid_check %>%
        filter(segment == L1238.segment_list[4] ) -> L1238.grid_check_peak

      L1238.grid_check_base %>%
        left_join_error_no_match(L1238.elecS_horizontal_vertical,
                                 by = c("grid_region", "segment" = "horizontal_segment")) %>%
        mutate(horizontal_segment_demand = vertical_segment_demand / off.peak.electricity) %>%
        mutate(base_intermediate = horizontal_segment_demand * intermediate.electricity) %>%
        mutate(base_subpeak = horizontal_segment_demand * subpeak.electricity) %>%
        mutate(base_peak = horizontal_segment_demand * peak.electricity) -> L1238.grid_check_base

      L1238.grid_check_int %>%
        left_join_error_no_match(L1238.elecS_horizontal_vertical,
                                 by = c("grid_region", "segment" = "horizontal_segment")) %>%
        left_join_error_no_match(L1238.grid_check_base, by = c("grid_region", "year")) %>%
        mutate(horizontal_segment_demand = (vertical_segment_demand.x - base_intermediate) / intermediate.electricity.x ) %>%
        mutate(int_subpeak = horizontal_segment_demand * subpeak.electricity.x) %>%
        mutate(int_peak = horizontal_segment_demand * peak.electricity.x)-> L1238.grid_check_int

      L1238.grid_check_subpeak %>%
        left_join_error_no_match(L1238.elecS_horizontal_vertical,
                                 by = c("grid_region", "segment" = "horizontal_segment")) %>%
        left_join_error_no_match(L1238.grid_check_int, by = c("grid_region", "year")) %>%
        mutate(horizontal_segment_demand = (vertical_segment_demand - base_subpeak - int_subpeak) / subpeak.electricity) %>%
        mutate(subpeak_peak = horizontal_segment_demand * peak.electricity)-> L1238.grid_check_subpeak

      L1238.grid_check_peak %>%
        left_join_error_no_match(L1238.elecS_horizontal_vertical,
                                 by = c("grid_region", "segment" = "horizontal_segment")) %>%
        left_join_error_no_match(L1238.grid_check_subpeak, by = c("grid_region", "year")) %>%
        mutate(horizontal_segment_demand = (vertical_segment_demand.x.x - base_peak - int_peak - subpeak_peak) /
                 peak.electricity.x.x) ->  L1238.grid_check_peak

      L1238.grid_check_base %>%
        select(grid_region, segment, year, generation,
               vertical_segment_demand, horizontal_segment_demand) -> L1238.grid_check_base

      L1238.grid_check_int %>%
        select(grid_region, segment = segment.x, year, generation = generation.x,
               vertical_segment_demand = vertical_segment_demand.x, horizontal_segment_demand) -> L1238.grid_check_int

      L1238.grid_check_subpeak %>%
        select(grid_region, segment, year, generation,
               vertical_segment_demand, horizontal_segment_demand) -> L1238.grid_check_subpeak

      L1238.grid_check_peak %>%
        select(grid_region, segment = segment.x.x, year, generation = generation.x.x,
               vertical_segment_demand = vertical_segment_demand.x.x, horizontal_segment_demand) -> L1238.grid_check_peak

      L1238.grid_check_base %>%
        bind_rows(L1238.grid_check_int) %>%
        bind_rows(L1238.grid_check_subpeak) %>%
        bind_rows(L1238.grid_check_peak) -> L1238.grid_check

      #Check that supply meets demand
      L1238.grid_check %>%
        mutate(check = horizontal_segment_demand - generation) %>%
        mutate(pct_check = check/generation) -> L1238.grid_check

      L1238.grid_check %>%
        filter(grid_region == L1238.region & segment == L1238.segment & year == script_year) %>%
        select(check) %>%
        .[['check']] -> check

      check
    }

    L1238.out_EJ_grid_elec_F %>%
      group_by(grid_region, sector, year) %>%
      summarise_at("tot_generation",sum) %>%
      ungroup() %>%
      rename(grid_total = tot_generation) -> L1238.grid_total

    L1238.out_EJ_grid_elec_F %>%
      left_join_error_no_match(L1238.grid_total, by = c("grid_region", "sector", "year")) %>%
      mutate(grid_share_fuel = tot_generation / grid_total) -> L1238.out_EJ_grid_elec_F

    #Loop through each gridregion
    for (j in 1:length(L1238.gridregion_list)){
      L1238.region <- L1238.gridregion_list[j]

      L1238.out_EJ_grid_elec_F %>%
        filter(grid_region == L1238.region & fuel == "gas" & year == script_year) %>%
        select(grid_share_fuel) %>%
        .[['grid_share_fuel']] -> L1238.gas_frac

      L1238.out_EJ_grid_elec_F %>%
        filter(grid_region == L1238.region & fuel == "refined liquids" & year == script_year) %>%
        select(grid_share_fuel) %>%
        .[['grid_share_fuel']] -> L1238.oil_frac

      L1238.out_EJ_grid_elec_F %>%
        filter(grid_region == L1238.region & fuel == "coal" & year == script_year) %>%
        select(grid_share_fuel) %>%
        .[['grid_share_fuel']] -> L1238.coal_frac

      L1238.out_EJ_grid_elec_F %>%
        filter(grid_region == L1238.region & fuel == "hydro" & year == script_year) %>%
        select(grid_share_fuel) %>%
        .[['grid_share_fuel']] -> L1238.hydro_frac

      if (L1238.hydro_frac > 0.5) {

        # For hydro-heavy regions such as Northwest grid, allocate some hydro in the intermediate.
        # Such regions have too much base load technologies.
        # Hence, assign some coal to the subpeak and peak.
        # This is no way perfect and needs to be addressed later.

        L1238.solved_fraction <- uniroot(check_elec_segments, c(0,1), L1238.region, L1238.segment_list[1], "hydro")

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "hydro"
                                    & segment == L1238.segment_list[1] & year == script_year,
                                    L1238.solved_fraction$root)) -> L1238.grid_elec_supply
        L1238.grid_elec_supply %>%
          filter(grid_region == L1238.region & fuel == "hydro" & segment != L1238.segment_list[2] & year == script_year) %>%
          summarise(non_int_frac = sum(fraction)) %>%
          .[['non_int_frac']] -> L1238.non_int

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "hydro"
                                    & segment == L1238.segment_list[2] & year == script_year,
                                    1 - L1238.non_int)) -> L1238.grid_elec_supply

        # Then, solve for some coal in subpeak and peak since there are not enough sources to supply that segment.
        L1238.solved_fraction <- uniroot(check_elec_segments, c(0,1), L1238.region, L1238.segment_list[3], "coal")

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "coal"
                                    & segment == L1238.segment_list[3] & year == script_year,
                                    L1238.solved_fraction$root)) -> L1238.grid_elec_supply
        L1238.grid_elec_supply %>%
          filter(grid_region == L1238.region & fuel == "coal" & segment != L1238.segment_list[2] & year == script_year) %>%
          summarise(non_int_frac = sum(fraction)) %>%
          .[['non_int_frac']] -> L1238.non_int

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "coal"
                                    & segment == L1238.segment_list[2] & year == script_year,
                                    1 - L1238.non_int)) -> L1238.grid_elec_supply

        L1238.solved_fraction <- uniroot(check_elec_segments, c(0,1), L1238.region, L1238.segment_list[4], "coal")

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "coal"
                                    & segment == L1238.segment_list[4] & year == script_year,
                                    L1238.solved_fraction$root)) -> L1238.grid_elec_supply
        L1238.grid_elec_supply %>%
          filter(grid_region == L1238.region & fuel == "coal" & segment != L1238.segment_list[2] & year == script_year) %>%
          summarise(non_int_frac = sum(fraction)) %>%
          .[['non_int_frac']] -> L1238.non_int

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "coal"
                                    & segment == L1238.segment_list[2] & year == script_year,
                                    1 - L1238.non_int)) -> L1238.grid_elec_supply

      } else if (L1238.oil_frac > 0.2) {
        # For oil heavy regions such as Hawaii, solve for oil fractions.
        # This will allow for some oil in baseload and intermediate segments.

        # Solve for oil fractions after removing gas from baseload.

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[1] & year == script_year, 0)) %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[2] & year == script_year, 0.9)) %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[3] & year == script_year, 0.1)) %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[4] & year == script_year, 0)) -> L1238.grid_elec_supply

        L1238.solved_fraction <- uniroot(check_elec_segments, c(0,1), L1238.region, L1238.segment_list[1], "refined liquids")

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "refined liquids"
                                    & segment == L1238.segment_list[1] & year == script_year,
                                    L1238.solved_fraction$root)) -> L1238.grid_elec_supply

        L1238.grid_elec_supply %>%
          filter(grid_region == L1238.region & fuel == "refined liquids" & segment != L1238.segment_list[4]
                 & year == script_year) %>%
          summarise(nonpeak_frac = sum(fraction)) %>%
          .[['nonpeak_frac']] -> L1238.nonpeak

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "refined liquids"
                                    & segment == L1238.segment_list[4] & year == script_year,
                                    1 - L1238.nonpeak)) -> L1238.grid_elec_supply

        L1238.solved_fraction <- uniroot(check_elec_segments, c(0,1), L1238.region, L1238.segment_list[2], "refined liquids")

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "refined liquids"
                                    & segment == L1238.segment_list[2] & year == script_year,
                                    L1238.solved_fraction$root)) -> L1238.grid_elec_supply

        L1238.grid_elec_supply %>%
          filter(grid_region == L1238.region & fuel == "refined liquids" & segment != L1238.segment_list[4]
                 & year == script_year) %>%
          summarise(nonpeak_frac = sum(fraction)) %>%
          .[['nonpeak_frac']] -> L1238.nonpeak

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "refined liquids"
                                    & segment == L1238.segment_list[4] & year == script_year,
                                    1 - L1238.nonpeak)) -> L1238.grid_elec_supply

        L1238.solved_fraction <- uniroot(check_elec_segments, c(0,1), L1238.region, L1238.segment_list[3], "refined liquids")

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "refined liquids"
                                    & segment == L1238.segment_list[3] & year == script_year,
                                    L1238.solved_fraction$root)) -> L1238.grid_elec_supply

        L1238.grid_elec_supply %>%
          filter(grid_region == L1238.region & fuel == "refined liquids" & segment != L1238.segment_list[4]
                 & year == script_year) %>%
          summarise(nonpeak_frac = sum(fraction)) %>%
          .[['nonpeak_frac']] -> L1238.nonpeak

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "refined liquids"
                                    & segment == L1238.segment_list[4] & year == script_year,
                                    1 - L1238.nonpeak)) -> L1238.grid_elec_supply

      } else if (L1238.gas_frac > 0.2 & L1238.coal_frac < 0.5) {

        # If gridregion has 20% or more gas, solve for gas fractions.
        # To reduce solution error, let's increase coal in base load
        # Let's also assign some oil to base load.

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "coal"
                                    & segment == L1238.segment_list[1] & year == script_year, 0.95)) %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "coal"
                                    & segment == L1238.segment_list[2] & year == script_year, 0.05)) %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "refined liquids"
                                    & segment == L1238.segment_list[1] & year == script_year, 0.8)) %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "refined liquids"
                                    & segment == L1238.segment_list[2] & year == script_year, 0.1)) %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "refined liquids"
                                    & segment == L1238.segment_list[3] & year == script_year, 0.1)) %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "refined liquids"
                                    & segment == L1238.segment_list[4] & year == script_year, 0)) -> L1238.grid_elec_supply

        # Solve for gas fractions
        L1238.solved_fraction <- uniroot(check_elec_segments, c(0,1), L1238.region, L1238.segment_list[1])

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[1] & year == script_year,
                                    L1238.solved_fraction$root)) -> L1238.grid_elec_supply

        L1238.grid_elec_supply %>%
          filter(grid_region == L1238.region & fuel == "gas" & segment != L1238.segment_list[4]
                 & year == script_year) %>%
          summarise(nonpeak_frac = sum(fraction)) %>%
          .[['nonpeak_frac']] -> L1238.nonpeak

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[4] & year == script_year,
                                    1 - L1238.nonpeak)) -> L1238.grid_elec_supply

        L1238.solved_fraction <- uniroot(check_elec_segments, c(0,1), L1238.region, L1238.segment_list[2])

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[2] & year == script_year,
                                    L1238.solved_fraction$root)) -> L1238.grid_elec_supply
        L1238.grid_elec_supply %>%
          filter(grid_region == L1238.region & fuel == "gas" & segment != L1238.segment_list[4]
                 & year == script_year) %>%
          summarise(nonpeak_frac = sum(fraction)) %>%
          .[['nonpeak_frac']] -> L1238.nonpeak

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[4] & year == script_year,
                                    1 - L1238.nonpeak)) -> L1238.grid_elec_supply

        L1238.solved_fraction <- uniroot(check_elec_segments, c(0,1), L1238.region, L1238.segment_list[3])

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[3] & year == script_year,
                                    L1238.solved_fraction$root)) -> L1238.grid_elec_supply

        L1238.grid_elec_supply %>%
          filter(grid_region == L1238.region & fuel == "gas" & segment != L1238.segment_list[4]
                 & year == script_year) %>%
          summarise(nonpeak_frac = sum(fraction)) %>%
          .[['nonpeak_frac']] -> L1238.nonpeak

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[4] & year == script_year,
                                    1 - L1238.nonpeak)) -> L1238.grid_elec_supply

      } else if (L1238.coal_frac > L1238.gas_frac & L1238.coal_frac < 0.52) {
        # For regions with moderate levels of coal such as Southeast grid, Northwest grid, and
        # Central Southwest grid, first move refined liquids to subpeak. Then solve for coal in
        # base load and allocate remaining coal to intermediate.

        # Assigning some gas and refined liquids into intermediate and subpeak
        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[1] & year == script_year, 0)) %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[2] & year == script_year, 0.6)) %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[3] & year == script_year, 0.25)) %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[4] & year == script_year, 0.15)) %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "refined liquids"
                                    & segment == L1238.segment_list[1] & year == script_year, 0.2)) %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "refined liquids"
                                    & segment == L1238.segment_list[2] & year == script_year, 0.8)) %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "refined liquids"
                                    & segment == L1238.segment_list[3] & year == script_year, 0.0)) %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "refined liquids"
                                    & segment == L1238.segment_list[4] & year == script_year, 0)) -> L1238.grid_elec_supply

        # Solve for coal fractions

        # Solve for coal in baseload and assign the remaining to intermediate

        L1238.solved_fraction <- uniroot(check_elec_segments, c(0,1), L1238.region, L1238.segment_list[1], "coal")

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "coal"
                                    & segment == L1238.segment_list[1] & year == script_year,
                                    L1238.solved_fraction$root)) -> L1238.grid_elec_supply
        L1238.grid_elec_supply %>%
          filter(grid_region == L1238.region & fuel == "coal" & segment != L1238.segment_list[2] & year == script_year) %>%
          summarise(non_int_frac = sum(fraction)) %>%
          .[['non_int_frac']] -> L1238.non_int

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "coal"
                                    & segment == L1238.segment_list[2] & year == script_year,
                                    1 - L1238.non_int)) -> L1238.grid_elec_supply

        # Solve for gas fractions in intermediate, subpeak and peak

        L1238.solved_fraction <- uniroot(check_elec_segments, c(0,1), L1238.region, L1238.segment_list[2], "gas")

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[2] & year == script_year,
                                    L1238.solved_fraction$root)) -> L1238.grid_elec_supply
        L1238.grid_elec_supply %>%
          filter(grid_region == L1238.region & fuel == "gas" & segment != L1238.segment_list[4]
                 & year == script_year) %>%
          summarise(nonpeak_frac = sum(fraction)) %>%
          .[['nonpeak_frac']] -> L1238.nonpeak

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[4] & year == script_year,
                                    1 - L1238.nonpeak)) -> L1238.grid_elec_supply

        L1238.solved_fraction <- uniroot(check_elec_segments, c(0,1), L1238.region, L1238.segment_list[3], "gas")

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[3] & year == script_year,
                                    L1238.solved_fraction$root)) -> L1238.grid_elec_supply

        L1238.grid_elec_supply %>%
          filter(grid_region == L1238.region & fuel == "gas" & segment != L1238.segment_list[4]
                 & year == script_year) %>%
          summarise(nonpeak_frac = sum(fraction)) %>%
          .[['nonpeak_frac']] -> L1238.nonpeak

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[4] & year == script_year,
                                    1 - L1238.nonpeak)) -> L1238.grid_elec_supply

      } else {

        # In regions with high levels of coal such as Central Northeast, Central East, Central Northwest and Sothwest grids,
        # allocate some wind into intermediate, and subpeak segments first. Second, remove gas from the baseload and assign
        # some refined liquids into the subpeak. It was found that even then, there are not enough sources to supply the
        # subpeak. So we solve for some coal in the subpeak. That is potentially problematic and might need to be verified
        # against real-world data.
        # Then follow the same steps as above. Note that we also read in different load curves for these regions
        # with lower peak demand.

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "wind"
                                    & segment == L1238.segment_list[1] & year == script_year,
                                    0.6)) %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "wind"
                                    & segment == L1238.segment_list[2] & year == script_year,
                                    0.25))%>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "wind"
                                    & segment == L1238.segment_list[3] & year == script_year,
                                    0.15))-> L1238.grid_elec_supply

        # First, solve for coal in baseload and assign the remaining to intermediate
        L1238.solved_fraction <- uniroot(check_elec_segments, c(0,1), L1238.region, L1238.segment_list[1], "coal")

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "coal"
                                    & segment == L1238.segment_list[1] & year == script_year,
                                    L1238.solved_fraction$root)) -> L1238.grid_elec_supply
        L1238.grid_elec_supply %>%
          filter(grid_region == L1238.region & fuel == "coal" & segment != L1238.segment_list[2] & year == script_year) %>%
          summarise(non_int_frac = sum(fraction)) %>%
          .[['non_int_frac']] -> L1238.non_int

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "coal"
                                    & segment == L1238.segment_list[2] & year == script_year,
                                    1 - L1238.non_int)) -> L1238.grid_elec_supply

        # Then, solve for some coal in subpeak and peak since there are not enough sources to supply that segment.
        L1238.solved_fraction <- uniroot(check_elec_segments, c(0,1), L1238.region, L1238.segment_list[3], "coal")

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "coal"
                                    & segment == L1238.segment_list[3] & year == script_year,
                                    L1238.solved_fraction$root)) -> L1238.grid_elec_supply
        L1238.grid_elec_supply %>%
          filter(grid_region == L1238.region & fuel == "coal" & segment != L1238.segment_list[2] & year == script_year) %>%
          summarise(non_int_frac = sum(fraction)) %>%
          .[['non_int_frac']] -> L1238.non_int

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "coal"
                                    & segment == L1238.segment_list[2] & year == script_year,
                                    1 - L1238.non_int)) -> L1238.grid_elec_supply

        L1238.solved_fraction <- uniroot(check_elec_segments, c(0,1), L1238.region, L1238.segment_list[4], "coal")

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "coal"
                                    & segment == L1238.segment_list[4] & year == script_year,
                                    L1238.solved_fraction$root)) -> L1238.grid_elec_supply
        L1238.grid_elec_supply %>%
          filter(grid_region == L1238.region & fuel == "coal" & segment != L1238.segment_list[2] & year == script_year) %>%
          summarise(non_int_frac = sum(fraction)) %>%
          .[['non_int_frac']] -> L1238.non_int

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "coal"
                                    & segment == L1238.segment_list[2] & year == script_year,
                                    1 - L1238.non_int)) -> L1238.grid_elec_supply

        # Solve for gas fractions in intermediate, subpeak and peak just to make sure all segments are balanced
        L1238.solved_fraction <- uniroot(check_elec_segments, c(0,1), L1238.region, L1238.segment_list[2], "gas")

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[2] & year == script_year,
                                    L1238.solved_fraction$root)) -> L1238.grid_elec_supply

        L1238.grid_elec_supply %>%
          filter(grid_region == L1238.region & fuel == "gas" & segment != L1238.segment_list[4]
                 & year == script_year) %>%
          summarise(nonpeak_frac = sum(fraction)) %>%
          .[['nonpeak_frac']] -> L1238.nonpeak

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[4] & year == script_year,
                                    1 - L1238.nonpeak)) -> L1238.grid_elec_supply

        L1238.solved_fraction <- uniroot(check_elec_segments, c(0,1), L1238.region, L1238.segment_list[3], "gas")

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[3] & year == script_year,
                                    L1238.solved_fraction$root)) -> L1238.grid_elec_supply

        L1238.grid_elec_supply %>%
          filter(grid_region == L1238.region & fuel == "gas" & segment != L1238.segment_list[4]
                 & year == script_year) %>%
          summarise(nonpeak_frac = sum(fraction)) %>%
          .[['nonpeak_frac']] -> L1238.nonpeak

        L1238.grid_elec_supply %>%
          mutate(fraction = replace(fraction, grid_region == L1238.region & fuel == "gas"
                                    & segment == L1238.segment_list[4] & year == script_year,
                                    1 - L1238.nonpeak)) -> L1238.grid_elec_supply

      }
    }

    # Re-do check calculations to output

    L1238.grid_elec_supply %>%
      mutate(generation = tot_generation * fraction) -> L1238.grid_elec_supply

    #Calculate supply by horizontal segment in each grid region by aggregating all technologies

    L1238.grid_elec_supply %>%
      group_by(grid_region, segment, year) %>%
      summarise_at("generation", sum) %>%
      ungroup() -> L1238.grid_check

    #Calculate demand for each horizontal segment by vertical segments in each grid region

    L1238.grid_elec_supply %>%
      group_by(grid_region, year) %>%
      summarise_at("generation", sum) %>%
      ungroup() %>%
      rename (tot_demand = generation) -> L1238.grid_elec_demand

    L1238.grid_check %>%
      left_join_error_no_match(L1238.grid_elec_demand, by = c("grid_region","year")) %>%
      left_join_error_no_match(elecS_horizontal_to_vertical_map, by = c("segment" = "horizontal_segment")) %>%
      left_join_error_no_match(L1238.elecS_demand_fraction , by = c("grid_region", "vertical_segment")) %>%
      mutate(vertical_segment_demand = tot_demand * demand_fraction) -> L1238.grid_elec_demand

    L1238.grid_check %>%
      left_join_error_no_match(L1238.grid_elec_demand, by = c("grid_region", "segment", "year")) %>%
      select(grid_region, segment, year, generation.x, vertical_segment_demand) %>%
      rename(generation = generation.x) -> L1238.grid_check

    L1238.grid_check %>%
      filter(segment == L1238.segment_list[1] ) -> L1238.grid_check_base

    L1238.grid_check %>%
      filter(segment == L1238.segment_list[2] ) -> L1238.grid_check_int

    L1238.grid_check %>%
      filter(segment == L1238.segment_list[3] ) -> L1238.grid_check_subpeak

    L1238.grid_check %>%
      filter(segment == L1238.segment_list[4] ) -> L1238.grid_check_peak

    L1238.grid_check_base %>%
      left_join_error_no_match(L1238.elecS_horizontal_vertical, by = c("grid_region", "segment" = "horizontal_segment")) %>%
      mutate(horizontal_segment_demand = vertical_segment_demand / off.peak.electricity) %>%
      mutate(base_intermediate = horizontal_segment_demand * intermediate.electricity) %>%
      mutate(base_subpeak = horizontal_segment_demand * subpeak.electricity) %>%
      mutate(base_peak = horizontal_segment_demand * peak.electricity) -> L1238.grid_check_base

    L1238.grid_check_int %>%
      left_join_error_no_match(L1238.elecS_horizontal_vertical, by = c("grid_region", "segment" = "horizontal_segment")) %>%
      left_join_error_no_match(L1238.grid_check_base, by = c("grid_region", "year")) %>%
      mutate(horizontal_segment_demand = (vertical_segment_demand.x - base_intermediate) / intermediate.electricity.x ) %>%
      mutate(int_subpeak = horizontal_segment_demand * subpeak.electricity.x) %>%
      mutate(int_peak = horizontal_segment_demand * peak.electricity.x)-> L1238.grid_check_int

    L1238.grid_check_subpeak %>%
      left_join_error_no_match(L1238.elecS_horizontal_vertical, by = c("grid_region", "segment" = "horizontal_segment")) %>%
      left_join_error_no_match(L1238.grid_check_int, by = c("grid_region", "year")) %>%
      mutate(horizontal_segment_demand = (vertical_segment_demand - base_subpeak - int_subpeak) / subpeak.electricity) %>%
      mutate(subpeak_peak = horizontal_segment_demand * peak.electricity)-> L1238.grid_check_subpeak

    L1238.grid_check_peak %>%
      left_join_error_no_match(L1238.elecS_horizontal_vertical, by = c("grid_region", "segment" = "horizontal_segment")) %>%
      left_join_error_no_match(L1238.grid_check_subpeak, by = c("grid_region", "year")) %>%
      mutate(horizontal_segment_demand = (vertical_segment_demand.x.x - base_peak - int_peak - subpeak_peak) /
               peak.electricity.x.x) ->  L1238.grid_check_peak

    L1238.grid_check_base %>%
      select(grid_region, segment, year, generation,
             vertical_segment_demand, horizontal_segment_demand) -> L1238.grid_check_base

    L1238.grid_check_int %>%
      select(grid_region, segment = segment.x, year, generation = generation.x,
             vertical_segment_demand = vertical_segment_demand.x, horizontal_segment_demand) -> L1238.grid_check_int

    L1238.grid_check_subpeak %>%
      select(grid_region, segment, year, generation,
             vertical_segment_demand, horizontal_segment_demand) -> L1238.grid_check_subpeak

    L1238.grid_check_peak %>%
      select(grid_region, segment = segment.x.x, year, generation = generation.x.x,
             vertical_segment_demand = vertical_segment_demand.x.x, horizontal_segment_demand) -> L1238.grid_check_peak

    L1238.grid_check_base %>%
      bind_rows(L1238.grid_check_int) %>%
      bind_rows(L1238.grid_check_subpeak) %>%
      bind_rows(L1238.grid_check_peak) -> L1238.grid_check

    #Check that supply meets demand
    L1238.grid_check %>%
      mutate(check = horizontal_segment_demand - generation) %>%
      mutate(pct_check = check/generation) -> L1238.grid_check


    # ===================================================

    # Produce outputs

    L1238.grid_elec_supply %>%
      add_title("Electricity supply by fuel by horizontal load segment in each grid region.") %>%
      add_units("EJ; unitless (fraction)") %>%
      add_comments("Electricity supply by fuel by horizontal load segment in each grid region.") %>%
      add_comments("Based on calculated fraction of fuel in the horizontal load segments.") %>%
      add_legacy_name("L1238.grid_elec_supply") %>%
      add_precursors("gcam-usa/elecS_horizontal_to_vertical_map",
                     "L1234.out_EJ_grid_elec_F",
                     "L1237.grid_elec_supply_USA") ->
      L1238.grid_elec_supply_USA

    L1238.grid_elec_demand %>%
      add_title("Electricity demand by load segment in each grid region.") %>%
      add_units("EJ; unitless (demand_fraction)") %>%
      add_comments("Electricity demand by load segment in each grid region.") %>%
      add_legacy_name("L1238.grid_elec_demand") %>%
      add_precursors("gcam-usa/elecS_horizontal_to_vertical_map",
                     "L1234.out_EJ_grid_elec_F",
                     "L1235.elecS_demand_fraction_USA",
                     "L1237.grid_elec_supply_USA") ->
      L1238.grid_elec_demand_USA

    L1238.grid_check %>%
      add_title("Electricity supplies and demands by load segment in each grid region.") %>%
      add_units("EJ; unitless (pct_check)") %>%
      add_comments("Electricity supplies and demands by load segment in each grid region.") %>%
      add_legacy_name("L1238.grid_check") %>%
      add_precursors("gcam-usa/elecS_horizontal_to_vertical_map",
                     "L1234.out_EJ_grid_elec_F",
                     "L1235.elecS_horizontal_vertical_USA",
                     "L1237.grid_elec_supply_USA") ->
      L1238.grid_check_USA

    return_data(L1238.grid_elec_supply_USA, L1238.grid_elec_demand_USA, L1238.grid_check_USA)

  } else {
    stop("Unknown command")
  }
}
