# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L232.water_demand_manufacturing
#'
#' Computes manufacturing water withdrawal/consumption coefficients (m3/GJ output) by region and year
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L232.TechCoef}. The corresponding file in the
#' original data system was \code{L232.water_demand_manufacturing.R} (water level2).
#' @details Water widthdrawal and consumption coefficients for manufacturing for GCAM regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange distinct filter if_else group_by left_join mutate select
#' @importFrom tidyr complete nesting
#' @author GPK June 2018
module_water_L232.water_demand_manufacturing <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/water_td_sectors",
             FILE = "energy/A32.globaltech_coef",
             "L132.water_km3_R_ind_Yh",
             "L232.StubTechProd_industry",
             FILE = "water/paper_mfg_intensity",
             FILE = "water/mfg_water_ratios",
             "L1327.out_Mt_R_paper_Yh",
             "L2327.StubTechProd_paper"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L232.TechCoef",
             "L2327.TechCoef_paper"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    stub.technology <- subs.share.weight <- tech.share.weight <- share.weight.year <-
      water_sector <- water_type <- region <- supplysector <- subsector <- technology <-
      year <- minicam.energy.input <- coefficient <- market.name <- calOutputValue <-
      water_km3 <- energy_EJ <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    water_td_sectors <- get_data(all_data, "water/water_td_sectors")
    A32.globaltech_coef <- get_data(all_data, "energy/A32.globaltech_coef")
    L132.water_km3_R_ind_Yh <- get_data(all_data, "L132.water_km3_R_ind_Yh")

    paper_mfg_intensity <- get_data(all_data, "water/paper_mfg_intensity")
    mfg_water_ratios <- get_data(all_data, "water/mfg_water_ratios")
    L1327.out_Mt_R_paper_Yh <- get_data(all_data, "L1327.out_Mt_R_paper_Yh")
    L2327.StubTechProd_paper <- get_data(all_data, "L2327.StubTechProd_paper")

    # Extrapolate this one to all model years if necessary
    get_data(all_data, "L232.StubTechProd_industry") %>%
      complete(nesting(region, supplysector, subsector, stub.technology), year = MODEL_YEARS) %>%
      group_by(region, supplysector, subsector, stub.technology) %>%
      arrange(year) %>%
      mutate(calOutputValue = approx_fun(year, calOutputValue, rule = 2),
             subs.share.weight = approx_fun(year, subs.share.weight, rule = 2),
             tech.share.weight = approx_fun(year, tech.share.weight, rule = 2),
             share.weight.year = approx_fun(year, share.weight.year, rule = 2)) %>%
      ungroup ->
      L232.StubTechProd_industry


    # Addition for pulp and paper industry:
    # Compute pulp and paper sector water withdrawals, as paper production * water intensity
    # This will be subtracted from total industrial water use

    L1327.out_Mt_R_paper_Yh %>%
      select(GCAM_region_ID, sector, year, prod=value) %>%
      filter(year %in% unique(L132.water_km3_R_ind_Yh$year)) %>%
      left_join(paper_mfg_intensity %>% select(-country), by = "sector") %>%
      mutate(withdrawals = prod * intensity * CONV_MIL_BIL,
             # Calculate water consumption in paper industry - use constant ratio in mfg_water_ratios.csv
             consumption = withdrawals * mfg_water_ratios$`cons-to-with-ratio`) %>%
      select(GCAM_region_ID, sector, year, prod, `water withdrawals` = withdrawals, `water consumption` = consumption) %>%
      gather(c(`water withdrawals`, `water consumption`), key = "water_type", value = water_km3) %>%
      mutate(coefficient = water_km3 / prod) %>%
      select(-prod) ->
      L1327.water_km3_R_ind_Yh_paper

    # Subtract paper industry water use from total industry
    L132.water_km3_R_ind_Yh %>%
      rename(industry_water = water_km3) %>%
      left_join(L1327.water_km3_R_ind_Yh_paper %>% select(GCAM_region_ID, year, water_type, paper_water=water_km3),
                by = c("GCAM_region_ID", "year", "water_type")) %>%
      mutate(paper_water = replace_na(paper_water, 0),
             water_km3 = if_else(industry_water - paper_water < 0, 0, industry_water - paper_water)) %>%
      select(GCAM_region_ID, year, water_type, water_km3) ->
      L1327.water_km3_R_ind_Yh


    # For total industry:
    # First, compute historical coefficients, as total withdrawals/consumption divided by industrial sector output
    # (base-service)
    # After subtracting paper sector water consumption
    L232.water_km3_R_ind_Yh <-
      filter(L1327.water_km3_R_ind_Yh, year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(L232.StubTechProd_industry, by = c("region", "year")) %>%
      rename(energy_EJ = calOutputValue) %>%
      mutate(coefficient = round(water_km3 / energy_EJ, digits = energy.DIGITS_COEFFICIENT)) ->
      L232.water_km3_R_ind_Yh

    # Read in water coefs for all years
    L232.water_km3_R_ind_Yh %>%
      select(region, water_type, year, coefficient) %>%
      repeat_add_columns(distinct(A32.globaltech_coef, supplysector, subsector, technology)) %>%
      mutate(water_sector = "Manufacturing",
             minicam.energy.input = set_water_input_name(water_sector, water_type, water_td_sectors),
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES$TechCoef) %>%
      # Fill out the values in the final base year to all future years
      group_by(region, supplysector, subsector, technology, minicam.energy.input, market.name) %>%
      complete(year = MODEL_YEARS) %>%
      mutate(coefficient = if_else(year %in% MODEL_FUTURE_YEARS, coefficient[year == max(MODEL_BASE_YEARS)], coefficient)) %>%
      ungroup() %>%

      # add attributes for output
      add_title("Water withdrawal and consumption coefficients for manufacturing") %>%
      add_units("m3/GJ") %>%
      add_comments("Manufacturing water demand coefficients by region, water type, and year") %>%
      add_legacy_name("L232.TechCoef") %>%
      add_precursors("common/GCAM_region_names",
                     "water/water_td_sectors",
                     "energy/A32.globaltech_coef",
                     "L132.water_km3_R_ind_Yh",
                     "L232.StubTechProd_industry") ->
      L232.TechCoef


    # Create data frame of water coefficients for paper industry
    L1327.water_km3_R_ind_Yh_paper %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, year, water_type, coefficient) %>%
      repeat_add_columns(distinct(L2327.StubTechProd_paper, supplysector, subsector, technology = stub.technology)) %>%
      mutate(water_sector = "Manufacturing",
             minicam.energy.input = set_water_input_name(water_sector, water_type, water_td_sectors),
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES$TechCoef) %>%
      # Fill out the values in the final base year to all future years
      group_by(region, supplysector, subsector, technology, minicam.energy.input, market.name) %>%
      complete(year = MODEL_YEARS) %>%
      mutate(coefficient = if_else(year %in% MODEL_FUTURE_YEARS, coefficient[year == max(MODEL_BASE_YEARS)], coefficient)) %>%
      ungroup() %>%
      mutate(coefficient = replace_na(coefficient, 0)) ->
      L2327.TechCoef_paper



    # add attributes for output
    L2327.TechCoef_paper %>%
      add_title("Water withdrawal and consumption coefficients for paper industry") %>%
      add_units("m3/GJ") %>%
      add_comments("Paper manufacturing water demand coefficients by region, water type, and year") %>%
      add_legacy_name("L2327.TechCoef_paper") %>%
      add_precursors("common/GCAM_region_names",
                     "water/water_td_sectors",
                     "water/paper_mfg_intensity",
                     "water/mfg_water_ratios",
                     "L1327.out_Mt_R_paper_Yh",
                     "L2327.StubTechProd_paper") ->
      L2327.TechCoef_paper

    return_data(L232.TechCoef, L2327.TechCoef_paper)
  } else {
    stop("Unknown command")
  }
}
