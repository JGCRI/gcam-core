# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LB1235.elec_load_segments_USA
#'
#' Read in demand fraction and time fraction and compute load curve related parameters
#' Calculate an initial estimate of generation by fuel (EJ) in the horizontal segments
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1235.grid_elec_supply_USA}, \code{L1235.elecS_horizontal_vertical_USA},
#' \code{L1235.elecS_horizontal_vertical_GCAM_coeff_USA}, \code{L1235.elecS_demand_fraction_USA}.
#'
#' The corresponding file in the original data system was \code{LB1235.elec_load_segments.R} (gcam-usa level1).
#' @details Compute load curve related parameters and nitial estimate of generation by fuel in the horizontal segments.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select summarise_at summarise_if
#' @importFrom tidyr gather
#' @author MTB August 2018
module_gcamusa_LB1235.elec_load_segments_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/elecS_demand_fraction",
             FILE = "gcam-usa/elecS_time_fraction",
             FILE = "gcam-usa/elecS_fuel_fraction",
             FILE = "gcam-usa/elecS_horizontal_to_vertical_map",
             "L1234.out_EJ_grid_elec_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1235.grid_elec_supply_USA",
             "L1235.elecS_horizontal_vertical_USA",
             "L1235.elecS_horizontal_vertical_GCAM_coeff_USA",
             "L1235.elecS_demand_fraction_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    grid_region <- year <- sector <- segment <- horizontal_segment <- vertical_segment <-
      off.peak.electricity <- intermediate.electricity <- subpeak.electricity <- peak.electricity <-
      off.peak.electricity.time <- intermediate.electricity.time <- subpeak.electricity.time <- peak.electricity.time <-
      off.peak.electricity.demand <- intermediate.electricity.demand <- subpeak.electricity.demand <- peak.electricity.demand <-
      off.peak.electricity.tot <- intermediate.electricity.tot <- subpeak.electricity.tot <- peak.electricity.tot <-
      supplysector <- subsector <- technology <- minicam.energy.input <- coefficient <-
      fuel <- generation <- demand <- tot_demand <- fraction <- demand_fraction <- check <-
      off.peak.electricity.x <- off.peak.electricity.y <- intermediate.electricity.x <- intermediate.electricity.y <-
      subpeak.electricity.x <- subpeak.electricity.y <- peak.electricity.x <- peak.electricity.y <-
      intermediate.sum <- subpeak.sum <- NULL # silence package check notes

    # Load required inputs
    elecS_demand_fraction <- get_data(all_data, "gcam-usa/elecS_demand_fraction")
    elecS_time_fraction <- get_data(all_data, "gcam-usa/elecS_time_fraction")
    elecS_fuel_fraction <- get_data(all_data, "gcam-usa/elecS_fuel_fraction")
    elecS_horizontal_to_vertical_map <- get_data(all_data, "gcam-usa/elecS_horizontal_to_vertical_map")
    L1234.out_EJ_grid_elec_F <- get_data(all_data, "L1234.out_EJ_grid_elec_F")

    # ===================================================
    # Data Processing

    horizontal_segment_list <- unique(elecS_horizontal_to_vertical_map$horizontal_segment)
    vertical_segment_list <- unique(elecS_horizontal_to_vertical_map$vertical_segment)

    # Computing the fraction of vertical segment supplied by each horizontal segment.
    # These fractions will be used to calculate horizontal segment demands to check if supplies
    # match demands in calibration years.
    # Note that these are not the same as GCAM I/O coefficients which are calculated subsequently.

    elecS_time_fraction %>%
      mutate(horizontal_segment = horizontal_segment_list[1]) -> elecS_horizontal_vertical_base

    elecS_time_fraction %>%
      mutate(horizontal_segment = horizontal_segment_list[2],
             off.peak.electricity.time = 0,
             intermediate.sum = intermediate.electricity.time + subpeak.electricity.time + peak.electricity.time,
             intermediate.electricity.time = intermediate.electricity.time / intermediate.sum,
             subpeak.electricity.time = subpeak.electricity.time / intermediate.sum,
             peak.electricity.time = peak.electricity.time / intermediate.sum) %>%
      select(-intermediate.sum) -> elecS_horizontal_vertical_intermediate

    elecS_time_fraction %>%
      mutate(horizontal_segment = horizontal_segment_list[3],
             off.peak.electricity.time = 0, intermediate.electricity.time = 0,
             subpeak.sum = subpeak.electricity.time + peak.electricity.time,
             subpeak.electricity.time = subpeak.electricity.time / subpeak.sum,
             peak.electricity.time = peak.electricity.time / subpeak.sum) %>%
      select(-subpeak.sum) -> elecS_horizontal_vertical_subpeak

    elecS_time_fraction %>%
      mutate(horizontal_segment = horizontal_segment_list[4],
             off.peak.electricity.time = 0, intermediate.electricity.time = 0, subpeak.electricity.time = 0,
             peak.electricity.time = 1) -> elecS_horizontal_vertical_peak

    elecS_horizontal_vertical_base %>%
      bind_rows(elecS_horizontal_vertical_intermediate,
                elecS_horizontal_vertical_subpeak,
                elecS_horizontal_vertical_peak) %>%
      arrange(grid_region) %>%
      select(grid_region, horizontal_segment, off.peak.electricity = off.peak.electricity.time,
             intermediate.electricity = intermediate.electricity.time,
             subpeak.electricity = subpeak.electricity.time,
             peak.electricity = peak.electricity.time)-> L1235.elecS_horizontal_vertical_USA

    # Computing GCAM I/O coefficients

    # First compute % of load supplied by horizontal segments
    elecS_demand_fraction %>%
      left_join_error_no_match(elecS_time_fraction, by ="grid_region") %>%
      mutate(horizontal_segment = horizontal_segment_list[1],
             off.peak.electricity = off.peak.electricity.demand,
             intermediate.electricity = off.peak.electricity * intermediate.electricity.time / off.peak.electricity.time,
             subpeak.electricity = off.peak.electricity * subpeak.electricity.time / off.peak.electricity.time,
             peak.electricity = off.peak.electricity * peak.electricity.time / off.peak.electricity.time) ->
      elecS_horizontal_vertical_GCAM_coeff_base

    elecS_horizontal_vertical_GCAM_coeff_base %>%
      select(grid_region, off.peak.electricity, intermediate.electricity, subpeak.electricity, peak.electricity) ->
      elecS_horizontal_vertical_GCAM_coeff_base_elec # to be used subsequently in a left join

    elecS_horizontal_vertical_GCAM_coeff_base %>%
      mutate(horizontal_segment = horizontal_segment_list[2],
             off.peak.electricity = 0,
             intermediate.electricity = intermediate.electricity.demand - intermediate.electricity,
             subpeak.electricity = intermediate.electricity * subpeak.electricity.time / intermediate.electricity.time,
             peak.electricity = intermediate.electricity * peak.electricity.time / intermediate.electricity.time) ->
      elecS_horizontal_vertical_GCAM_coeff_intermediate

    elecS_horizontal_vertical_GCAM_coeff_intermediate %>%
      select(grid_region, off.peak.electricity, intermediate.electricity, subpeak.electricity, peak.electricity) ->
      elecS_horizontal_vertical_GCAM_coeff_int_elec # to be used subsequently in a left join

    elecS_horizontal_vertical_GCAM_coeff_intermediate %>%
      mutate(horizontal_segment = horizontal_segment_list[3],
             off.peak.electricity = 0,
             intermediate.electricity = 0) %>%
      left_join_error_no_match(elecS_horizontal_vertical_GCAM_coeff_base_elec, by = c("grid_region")) %>%
      mutate(subpeak.electricity.x = subpeak.electricity.demand - subpeak.electricity.x - subpeak.electricity.y,
             peak.electricity.x = subpeak.electricity.x * peak.electricity.time / subpeak.electricity.time) %>%
      select(-off.peak.electricity.y, -intermediate.electricity.y, -subpeak.electricity.y, -peak.electricity.y) %>%
      rename(off.peak.electricity = off.peak.electricity.x,
             intermediate.electricity = intermediate.electricity.x,
             subpeak.electricity = subpeak.electricity.x,
             peak.electricity= peak.electricity.x) -> elecS_horizontal_vertical_GCAM_coeff_subpeak

    elecS_horizontal_vertical_GCAM_coeff_subpeak %>%
      mutate(horizontal_segment = horizontal_segment_list[4],
             off.peak.electricity = 0,
             intermediate.electricity = 0,
             subpeak.electricity = 0) %>%
      left_join_error_no_match(elecS_horizontal_vertical_GCAM_coeff_int_elec, by = c("grid_region")) %>%
      left_join_error_no_match(elecS_horizontal_vertical_GCAM_coeff_base_elec, by = c("grid_region")) %>%
      mutate(peak.electricity.x = peak.electricity.demand - peak.electricity - peak.electricity.y - peak.electricity.x) %>%
      select(-off.peak.electricity.y, -intermediate.electricity.y, -subpeak.electricity.y, -peak.electricity.y,
             -off.peak.electricity, -intermediate.electricity, -subpeak.electricity, -peak.electricity) %>%
      rename(off.peak.electricity = off.peak.electricity.x,
             intermediate.electricity = intermediate.electricity.x,
             subpeak.electricity = subpeak.electricity.x,
             peak.electricity= peak.electricity.x) -> elecS_horizontal_vertical_GCAM_coeff_peak

    elecS_horizontal_vertical_GCAM_coeff_base %>%
      bind_rows(elecS_horizontal_vertical_GCAM_coeff_intermediate,
                elecS_horizontal_vertical_GCAM_coeff_subpeak,
                elecS_horizontal_vertical_GCAM_coeff_peak) %>%
      arrange(grid_region) %>%
      select(grid_region, horizontal_segment, off.peak.electricity, intermediate.electricity,
             subpeak.electricity, peak.electricity) -> L1235.elecS_horizontal_vertical_GCAM_coeff_USA

    # The final GCAM coefficients will be obtained by dividing the percentage of load supplied by
    # horizontal segments by total load in the vertical segments

    L1235.elecS_horizontal_vertical_GCAM_coeff_USA %>%
      group_by(grid_region) %>%
      summarise_if(is.numeric, sum) %>%
      ungroup() %>%
      rename(off.peak.electricity.tot = off.peak.electricity,
             intermediate.electricity.tot = intermediate.electricity,
             subpeak.electricity.tot = subpeak.electricity,
             peak.electricity.tot = peak.electricity) -> L1235.elecS_horizontal_vertical_GCAM_coeff_USA_tot

    L1235.elecS_horizontal_vertical_GCAM_coeff_USA %>%
      left_join_error_no_match(L1235.elecS_horizontal_vertical_GCAM_coeff_USA_tot, by = "grid_region") %>%
      mutate(off.peak.electricity = off.peak.electricity / off.peak.electricity.tot,
             intermediate.electricity = intermediate.electricity / intermediate.electricity.tot,
             subpeak.electricity = subpeak.electricity / subpeak.electricity.tot,
             peak.electricity = peak.electricity / peak.electricity.tot) %>%
      select(-off.peak.electricity.tot, -intermediate.electricity.tot,
             -subpeak.electricity.tot, -peak.electricity.tot) %>%
      # Gather the GCAM coefficients in long form
      gather(supplysector, coefficient, -grid_region, -horizontal_segment) %>%
      mutate(supplysector = gsub("off.peak.electricity", "off peak electricity", supplysector),
             supplysector = gsub("intermediate.electricity", "intermediate electricity", supplysector),
             supplysector = gsub("subpeak.electricity", "subpeak electricity", supplysector),
             supplysector = gsub("peak.electricity", "peak electricity", supplysector),
             subsector = supplysector, technology = supplysector) %>%
      filter(coefficient != 0) %>%
      select(grid_region, supplysector, subsector, technology,
             minicam.energy.input = horizontal_segment, coefficient) %>%
      arrange(grid_region) -> L1235.elecS_horizontal_vertical_GCAM_coeff_USA

    # Check for negative coefficients.
    stopifnot(all(L1235.elecS_horizontal_vertical_GCAM_coeff_USA$coefficient >= 0))

    # The rest of the script calculates an initial estimate of the amount (in EJ) of
    # generation by each fuel in the horizontal segments

    # Gathering some information in long-form first
    elecS_demand_fraction %>%
      gather(vertical_segment, demand_fraction, -grid_region) %>%
      mutate(vertical_segment = gsub("off.peak.electricity.demand", "off peak electricity", vertical_segment),
             vertical_segment = gsub("intermediate.electricity.demand", "intermediate electricity", vertical_segment),
             vertical_segment = gsub("subpeak.electricity.demand", "subpeak electricity", vertical_segment),
             vertical_segment = gsub("peak.electricity.demand", "peak electricity", vertical_segment)) ->
      L1235.elecS_demand_fraction_USA

    elecS_fuel_fraction %>%
      gather(year, fraction, -fuel, -segment) %>%
      mutate(year = gsub("fraction", "", year),
             year = as.integer(year)) -> elecS_fuel_fraction

    L1234.out_EJ_grid_elec_F %>%
      mutate(fuel = sub("solar CSP", "solar", fuel),
             fuel = sub("solar PV", "solar", fuel)) %>%
      group_by(grid_region, sector, fuel, year) %>%
      summarise_at("generation", sum) %>%
      ungroup() %>%
      mutate(year = as.integer(year)) %>%
      # this join is intended to duplicate rows; left_join_error_no_match throws an error,
      # so left_join is used instead
      left_join(elecS_fuel_fraction, by = c("fuel", "year")) %>%
      filter(!is.na(fraction)) %>%
      mutate(generation = generation * fraction) %>%
      select(grid_region, segment, fuel, year, generation, fraction) -> L1235.grid_elec_supply_USA


    # ===================================================

    # Produce outputs

    L1235.grid_elec_supply_USA %>%
      add_title("Electricity generation by fuel in the horizontal load segments") %>%
      add_units("EJ (generation); unitless (fraction)") %>%
      add_comments("Initial estimates of electricity generation by fuel in the horizontal load segments; by grid region") %>%
      add_comments("Based on initial estimates of fraction of fuel in the horizontal segments") %>%
      add_legacy_name("L1235.grid_elec_supply") %>%
      add_precursors("L1234.out_EJ_grid_elec_F",
                     "gcam-usa/elecS_fuel_fraction") ->
      L1235.grid_elec_supply_USA

    L1235.elecS_horizontal_vertical_USA %>%
      add_title("Fraction of electricity demand supplied by vertical load segment") %>%
      add_units("unitless (fraction)") %>%
      add_comments("This table specifies the fraction of supply from horizontal load segment available for vertical load segment") %>%
      add_comments("For example - 33.33% of base load generation is available for the vertical segment off peak electricity") %>%
      add_legacy_name("L1235.elecS_horizontal_vertical") %>%
      add_precursors("gcam-usa/elecS_time_fraction",
                     "gcam-usa/elecS_horizontal_to_vertical_map") ->
      L1235.elecS_horizontal_vertical_USA

    L1235.elecS_horizontal_vertical_GCAM_coeff_USA %>%
      add_title("GCAM I-O Coefficients from horizontal to vertical load segments") %>%
      add_units("unitless") %>%
      add_comments("GCAM I-O Coefficients from horizontal to vertical load segments") %>%
      add_legacy_name("L1235.elecS_horizontal_vertical_GCAM_coeff") %>%
      add_precursors("gcam-usa/elecS_demand_fraction",
                     "gcam-usa/elecS_time_fraction",
                     "gcam-usa/elecS_horizontal_to_vertical_map") ->
      L1235.elecS_horizontal_vertical_GCAM_coeff_USA

    L1235.elecS_demand_fraction_USA %>%
      add_title("Electricity vertical load segment demand fractions by grid region") %>%
      add_units("unitless") %>%
      add_comments("Fraction of grid-level electricity demand by vertical segment") %>%
      add_legacy_name("L1235.elecS_demand_fraction") %>%
      add_precursors("gcam-usa/elecS_demand_fraction") ->
      L1235.elecS_demand_fraction_USA

    return_data(L1235.grid_elec_supply_USA,
                L1235.elecS_horizontal_vertical_USA,
                L1235.elecS_horizontal_vertical_GCAM_coeff_USA,
                L1235.elecS_demand_fraction_USA)
  } else {
    stop("Unknown command")
  }
}
