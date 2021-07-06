# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA124.heat
#'
#' Process historical heat data into input, output from district heat, output from CHP, CHP ratio
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L124.in_EJ_R_heat_F_Yh}, \code{L124.out_EJ_R_heat_F_Yh}, \code{L124.out_EJ_R_heatfromelec_F_Yh}, \code{L124.heatoutratio_R_elec_F_tech_Yh}. The corresponding file in the
#' original data system was \code{LA124.heat.R} (energy level1).
#' @details This chunk takes in the energy balance, electricity output and mapping files on technology and fuel aggregations.
#' From this, it creates tables of heat input and output and uses the electricity gen. to determine heat from CHP.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange distinct filter if_else group_by left_join mutate select summarise
#' @importFrom tidyr complete nesting replace_na
#' @author JDH April 2017
module_energy_LA124.heat <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A_regions",
             FILE = "energy/A24.globaltech_coef",
             FILE = "energy/calibrated_techs",
             FILE = "energy/mappings/enduse_fuel_aggregation",
             "L1012.en_bal_EJ_R_Si_Fi_Yh",
             "L1231.out_EJ_R_elec_F_tech_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L124.in_EJ_R_heat_F_Yh",
             "L124.out_EJ_R_heat_F_Yh",
             "L124.out_EJ_R_heatfromelec_F_Yh",
             "L124.heatoutratio_R_elec_F_tech_Yh"))
  } else if(command == driver.MAKE) {

    ## silence package check
    year <- value <- fuel <- sector <- GCAM_region_ID <- has_district_heat <-
      heat <- supplysector <- subsector <- technology <- minicam.energy.input <- NULL
    IO_Coef <- value_heatfromelec <- dist_heat <- elec_heat <- sector.y <-
      norm_val <- mult_val <- heat_val <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    A_regions <- get_data(all_data, "energy/A_regions")
    A24.globaltech_coef <- get_data(all_data, "energy/A24.globaltech_coef")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    enduse_fuel_aggregation <- get_data(all_data, "energy/mappings/enduse_fuel_aggregation")
    L1012.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1012.en_bal_EJ_R_Si_Fi_Yh")
    L1231.out_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.out_EJ_R_elec_F_tech_Yh", strip_attributes = TRUE)

    # ===================================================
    # Create list of regions with district heat modeled
    A_regions %>%
      filter(has_district_heat == 1) %>%
      select(GCAM_region_ID) -> heat_regionIDs

    # Fuel inputs to district heat
    # Process fuel inputs in all regions; some will have the energy assigned to bld/ind, and others have a district heat sector
    L1012.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "in_heat") %>%
      mutate(sector = sub("in_", "", sector)) %>%
      left_join(enduse_fuel_aggregation, by = "fuel") %>%
      select(GCAM_region_ID, sector, year, value, heat) %>%
      rename(fuel = heat) %>%
      group_by(fuel, sector, GCAM_region_ID, year) %>% # removed -> temp
      summarise(value = sum(value)) %>%
      ungroup() -> L124.in_EJ_R_heat_F_Yh

    # Heat production from district heat sector
    A24.globaltech_coef %>%
      gather_years %>%
      # Adding empty historical years to fill in with interpolation
      complete(year = unique(c(HISTORICAL_YEARS, year)),
               nesting(supplysector, subsector, technology, minicam.energy.input)) %>%
      arrange(year) %>%
      group_by(technology, subsector, supplysector, minicam.energy.input) %>%
      # Interpolate to fill in missing globaltech_coef historical years
      mutate(value = approx_fun(year, value)) %>%
      ungroup() %>%
      left_join(distinct(calibrated_techs), by = c("supplysector", "subsector", "technology", "minicam.energy.input")) %>%
      select(supplysector, subsector, technology, minicam.energy.input, year, value, sector, fuel) -> L124.globaltech_coef

    # Heat output: fuel inputs to heat divided by exogenous input-output coefficients
    L124.in_EJ_R_heat_F_Yh %>%
      left_join_error_no_match(L124.globaltech_coef %>%
                                 rename(IO_Coef = value),
                               by = c("sector", "fuel", "year")) %>%
      mutate(value = value / IO_Coef) %>%
      select(-IO_Coef) %>%
      filter(GCAM_region_ID %in% heat_regionIDs$GCAM_region_ID)-> L124.out_EJ_R_heat_F_Yh

    # Secondary output of heat from main activity CHP plants
    L1012.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "out_electricity_heat" , GCAM_region_ID %in% heat_regionIDs$GCAM_region_ID) %>%
      mutate(sector = sub("out_", "", sector)) %>%
      left_join(enduse_fuel_aggregation, by = "fuel") %>%
      select(GCAM_region_ID,   sector, year, value, heat) %>%
      rename(fuel = heat) %>%
      group_by(fuel, sector, GCAM_region_ID, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(fuel != is.na(fuel)) -> L124.out_EJ_R_heatfromelec_F_Yh

    # Secondary output coefficients on heat produced by main activity CHP plants
    L1231.out_EJ_R_elec_F_tech_Yh %>%
      filter(GCAM_region_ID %in% heat_regionIDs$GCAM_region_ID,
             year %in% HISTORICAL_YEARS) %>%
      # Select only technologies that have heat output in calibrated techs mapping
      filter(technology %in% calibrated_techs$technology[calibrated_techs$secondary.output == "heat"]) %>%
      left_join_error_no_match(L124.out_EJ_R_heatfromelec_F_Yh %>%
                                 rename(value_heatfromelec = value) %>%
                                 rename(temp = sector), by = c("GCAM_region_ID", "fuel", "year")) %>%
      # Heat output divided by electricity output
      mutate(value = value_heatfromelec / value) %>%
      select(GCAM_region_ID, sector, fuel, technology, year, value) %>%
      # Reset missing and infinite values (applicable for CC in the base years) to 0
      mutate(value = if_else(is.na(value) | is.infinite(value), 0, value)) ->
      L124.heatoutratio_R_elec_F_tech_Yh


    # Drop all rows where value = 0 for all years
    # Create a table of all the years of all value = 0
    L124.heatoutratio_R_elec_F_tech_Yh %>%
      group_by(GCAM_region_ID, technology, sector, fuel) %>%
      summarise(sum = sum(value)) %>%
      ungroup() %>%
      filter(sum == 0) -> years_heatout_0

    # Filter out the years in years_heatout_0 from heatoutratio
    L124.heatoutratio_R_elec_F_tech_Yh %>%
      left_join(years_heatout_0,
                by = c("GCAM_region_ID", "sector", "fuel", "technology")) %>%
      # Using 1 for all rows where heatout is not 0 for all years
      replace_na(list(sum = 1)) %>%
      filter(sum != 0) %>%
      select(-sum) -> L124.heatoutratio_R_elec_F_tech_Yh

    # This is complicated. Some historical regions / years have 0 district heat output (all fuels). This is fine unless there is heat output from
    # IEA's MAINCHP flow, which we model in the electric sector with a secondary output of heat to the district heat sector. In GCAM this will
    # cause solution failure as the district heat supply will be 0. In order to avoid this outcome we need a nominal amount of heat produced
    # in the zero years. The code below sets an exogenous year as the one to use to extract fuel shares for each region. In all 0 region / years,
    # output is equal to the output in the fuel-share year times 1e-3 to avoid throwing off the energy balances.

    norm_year <- max(HISTORICAL_YEARS) # originally was hardcoded as 2010
    output_scalar <- 1e-3

    L124.in_EJ_R_heat_F_Yh %>%
      group_by(sector, GCAM_region_ID, year) %>%
      summarise(dist_heat = sum(value)) %>%
      ungroup() -> L124.mult_R_heat_Yh

    L124.out_EJ_R_heatfromelec_F_Yh %>%
      group_by(sector, GCAM_region_ID, year) %>%
      summarise(elec_heat = sum(value)) %>%
      ungroup() -> L124.mult_R_heatfromelec_Yh

    # If heat output is 0 and heat from CHP is nonzero, multiplier will be nonzero (1e-3)
    # All other cases, multiplier is 0
    L124.mult_R_heat_Yh %>%
      left_join(L124.mult_R_heatfromelec_Yh %>%
                  rename(sector.y=sector),
                by = c("year", "GCAM_region_ID")) %>%
      mutate(value = if_else(dist_heat == 0 & elec_heat != 0, output_scalar, 0)) %>%
      select(-sector.y, -elec_heat) %>%
      replace_na(list(value = 0)) -> L124.mult_R_heat_Yh

    L124.in_EJ_R_heat_F_Yh %>%
      filter(year == norm_year) %>%
      select(-year) %>%
      rename(norm_val = value) -> norm_in

    # If heat output is 0 and heat from CHP is nonzero, value for heat input will be equal to
    # the heat input plus the fuel share year input times 1e-3
    L124.in_EJ_R_heat_F_Yh %>%
      left_join_error_no_match(norm_in, by = c("fuel", "sector", "GCAM_region_ID")) %>%
      left_join_error_no_match(rename(L124.mult_R_heat_Yh, mult_val = value)
                               , by = c("sector", "GCAM_region_ID", "year")) %>%
      mutate(value = value + (norm_val * mult_val)) %>%
      select(fuel, sector, GCAM_region_ID, year, value) -> L124.in_EJ_R_heat_F_Yh

    L124.out_EJ_R_heat_F_Yh %>%
      filter(year == norm_year) %>%
      select(fuel, sector, GCAM_region_ID, value) %>%
      rename(norm_val = value) -> norm_out

    # If heat output is 0 and heat from CHP is nonzero, value for heat output will be equal to
    # the heat output plus the fuel share year output times 1e-3
    L124.out_EJ_R_heat_F_Yh %>%
      left_join_error_no_match(norm_out, by = c("fuel", "sector", "GCAM_region_ID")) %>%
      left_join_error_no_match(L124.mult_R_heat_Yh %>%
                                 rename(heat_val = value), by = c("sector", "GCAM_region_ID", "year")) %>%
      mutate(value = value + (norm_val * heat_val)) %>%
      select(fuel, sector, GCAM_region_ID, year, value) -> L124.out_EJ_R_heat_F_Yh

    # ===================================================
    # Produce outputs
    L124.in_EJ_R_heat_F_Yh %>%
      add_title("Inputs to heat by GCAM region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Input heat is extracted from energy balance, aggregated based on aggregate fuel types") %>%
      add_comments("To avoid processing failure, 0 years have base year (2010) * 1e-3 added") %>%
      add_legacy_name("L124.in_EJ_R_heat_F_Yh") %>%
      add_precursors("energy/A_regions", "L1012.en_bal_EJ_R_Si_Fi_Yh", "energy/mappings/enduse_fuel_aggregation") ->
      L124.in_EJ_R_heat_F_Yh

    L124.out_EJ_R_heat_F_Yh %>%
      add_title("Output of district heat sector by GCAM region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Output heat calculated based on input divided by a technology coefficient") %>%
      add_comments("To avoid processing failure, 0 years have base year (2010) * 1e-3 added") %>%
      add_legacy_name("L124.out_EJ_R_heat_F_Yh") %>%
      add_precursors("energy/A_regions", "energy/A24.globaltech_coef", "energy/calibrated_techs") ->
      L124.out_EJ_R_heat_F_Yh

    L124.out_EJ_R_heatfromelec_F_Yh %>%
      add_title("Heat output from electricity generation by GCAM region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Data on heat from CHP is read in, aggregated") %>%
      add_legacy_name("L124.out_EJ_R_heatfromelec_F_Yh") %>%
      add_precursors("energy/A_regions", "L1012.en_bal_EJ_R_Si_Fi_Yh", "energy/mappings/enduse_fuel_aggregation") ->
      L124.out_EJ_R_heatfromelec_F_Yh

    L124.heatoutratio_R_elec_F_tech_Yh %>%
      add_title("Heat output ratio from electricity generation by GCAM region / fuel / historical year") %>%
      add_units("GJ heat / GJ elec") %>%
      add_comments("Data on CHP electricity generation read in, heat from elec divided by electricity gives ratio") %>%
      add_legacy_name("L124.heatoutratio_R_elec_F_tech_Yh") %>%
      add_precursors("energy/A_regions", "L1231.out_EJ_R_elec_F_tech_Yh", "energy/calibrated_techs") ->
      L124.heatoutratio_R_elec_F_tech_Yh

    return_data(L124.in_EJ_R_heat_F_Yh, L124.out_EJ_R_heat_F_Yh, L124.out_EJ_R_heatfromelec_F_Yh, L124.heatoutratio_R_elec_F_tech_Yh)
  } else {
    stop("Unknown command")
  }
}
