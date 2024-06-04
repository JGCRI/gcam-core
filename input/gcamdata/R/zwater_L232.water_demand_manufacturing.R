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
#' @details Water withdrawal and consumption coefficients for manufacturing for GCAM regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange distinct filter if_else group_by left_join mutate select
#' @importFrom tidyr complete nesting
#' @author GPK June 2018
module_water_L232.water_demand_manufacturing <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "water/water_td_sectors",
      FILE = "energy/A32.globaltech_coef",
      "L132.water_km3_R_ind_Yh",
      "L232.StubTechProd_industry",
      FILE = "water/paper_mfg_intensity",
      FILE = "water/food_mfg_intensity",
      FILE = "water/mfg_water_ratios",
      "L1327.out_Mt_R_paper_Yh",
      "L2327.StubTechProd_paper",
      "L1328.out_Pcal_R_food_Yh",
      "L1328.in_EJ_R_food_F_Yh",
      "L2328.StubTechProd_food")

  MODULE_OUTPUTS <-
    c("L232.TechCoef",
      "L2327.TechCoef_paper",
      "L232.TechCoef_food")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    stub.technology <- subs.share.weight <- tech.share.weight <- share.weight.year <-
      water_sector <- water_type <- region <- supplysector <- subsector <- technology <-
      year <- minicam.energy.input <- coefficient <- market.name <- calOutputValue <-
      water_km3 <- energy_EJ <- GCAM_region_ID <- year <- sector <- value <- energy <-
      prod <- energy_coef <- energy_coef_comp <- intensity <- energy_coef_ration <-
      intensity_orig <- withdrawals <- consumption <- food_water <- industry_water <-
      food_water_recal <- water_type <- coefficient <- stub.technology <-
      water_sector <- NULL  # silence package check notes

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

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


    # Food processing water withdrawals
    # Assume that the regional water use intensity coefficients scale with the regional energy use intensity coefficients
    # i.e., for all regions without regional water use intensity coefficients, we will obtain intensity coefficients by scaling the
    # USA water use coefficient, where the scaling factor is set by the ratio of the other region's energy use coefficient
    # to the USA's energy use coefficient (using latest base year data)
    # First calculate the energy use coefficients
    L1328.out_Pcal_R_food_Yh %>%
      select(GCAM_region_ID, year, prod = value) %>%
      left_join_error_no_match(L1328.in_EJ_R_food_F_Yh %>%
                                 group_by(GCAM_region_ID, year, sector) %>%
                                 summarize(energy = sum(value)) %>%
                                 ungroup(),
                               by = c("GCAM_region_ID", "year")) %>%
      mutate(energy_coef = energy / prod) ->
      L1328.IO_inputEJPcal_R_food_Yh

    # Calculate ratio of coefficients in latest base year relative to USA's coefficient
    L1328.IO_inputEJPcal_R_food_Yh %>%
      left_join_error_no_match(L1328.IO_inputEJPcal_R_food_Yh %>%
                                 left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
                                 filter(region == water.FOOD_PROCESSING.REGION_BASE) %>%
                                 select(year, sector, energy_coef_comp = energy_coef),
                               by = c("year", "sector")) %>%
      mutate(energy_coef_ratio = energy_coef / energy_coef_comp) %>%
      filter(year == max(MODEL_BASE_YEARS)) ->
      food_energy_coef_ratio

    # Scale the water use coefficients
    # First expand USA values to all regions without data
    food_mfg_intensity %>%
      filter(region == water.FOOD_PROCESSING.REGION_BASE) %>%
      # expand USA values to all regions
      complete(nesting(sector, intensity), region = unique(GCAM_region_names$region)) %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      # multiply by the ratio of each region's food processing energy use coefficient to the USA's coefficient
      left_join_error_no_match(food_energy_coef_ratio %>%
                                 select(GCAM_region_ID, energy_coef_ratio),
                               by = c("GCAM_region_ID")) %>%
      mutate(intensity = intensity * energy_coef_ratio) %>%
      # replace values for any regions that did have data in the original input file with their actual values
      left_join(food_mfg_intensity %>%
                  rename(intensity_orig = intensity),
                by = c("sector", "region")) %>%
      mutate(intensity = if_else(!is.na(intensity_orig), intensity_orig, intensity)) %>%
      select(sector, GCAM_region_ID, region, intensity) ->
      food_mfg_intensity_expanded

    # Calculate food processing water withdrawals as intensity (km^3/Pcal) * production (Pcal)
    L1328.out_Pcal_R_food_Yh %>%
      select(GCAM_region_ID, year, prod = value) %>%
      filter(year %in% unique(L132.water_km3_R_ind_Yh$year)) %>%
      mutate(sector = "food processing") %>%
      left_join_error_no_match(food_mfg_intensity_expanded, by = c("sector", "GCAM_region_ID")) %>%
      mutate(withdrawals = prod * intensity,
            # Calculate water consumption in food industry - use constant ratio in mfg_water_ratios.csv
            consumption = withdrawals * mfg_water_ratios$`cons-to-with-ratio`) %>%
      select(GCAM_region_ID, sector, year, prod, `water withdrawals` = withdrawals, `water consumption` = consumption) %>%
      gather(c(`water withdrawals`, `water consumption`), key = "water_type", value = "water_km3") %>%
      mutate(coefficient = water_km3 / prod) ->
      L1328.water_km3_R_ind_Yh_food

    # Subtract food industry water use from total industry (with paper water use already subtracted)
    L1327.water_km3_R_ind_Yh %>%
      rename(industry_water = water_km3) %>%
      # Using left join since L1328.water_km3_R_ind_Yh_food starts from 1973 while the other starts from 1971
      # replaced na years with zero later
      left_join(L1328.water_km3_R_ind_Yh_food %>%
                  select(GCAM_region_ID, year, water_type, food_water = water_km3),
                by = c("GCAM_region_ID", "year", "water_type")) %>%
      mutate(food_water = replace_na(food_water, 0),
             # if calculated food water use is larger than total industry water use, use total industry water use as food value
             food_water_recal = if_else(food_water > industry_water, industry_water, food_water),
             water_km3 = industry_water - food_water_recal) ->
      L1328.water_km3_R_ind_Yh_temp

    L1328.water_km3_R_ind_Yh_temp %>%
      select(GCAM_region_ID, year, water_type, water_km3) ->
      L1328.water_km3_R_ind_Yh

    # Adjust food industry water use downwards for regions where it exceeds total industry water use,
    # so that food industry water use is just equal to the total industry water use in those cases
    L1328.water_km3_R_ind_Yh_food %>%
      left_join_error_no_match(L1328.water_km3_R_ind_Yh_temp %>%
                                 select(GCAM_region_ID, year, water_type, food_water_recal),
                               by = c("GCAM_region_ID", "year", "water_type")) %>%
      # use the recalculated value for food water use, and recalculate the coefficient
      mutate(water_km3 = food_water_recal,
             coefficient = round(water_km3 / prod, digits = energy.DIGITS_COEFFICIENT)) %>%
      select(-food_water_recal, -prod) ->
      L1328.water_km3_R_ind_Yh_food_recal

    # For total industry:
    # First, compute historical coefficients, as total withdrawals/consumption divided by industrial sector output
    # (base-service)
    # After subtracting paper sector water consumption and food sector water consumption
    L232.water_km3_R_ind_Yh <-
      filter(L1328.water_km3_R_ind_Yh, year %in% MODEL_BASE_YEARS) %>%
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
      add_comments("with food processing and paper industry water use subtracted") %>%
      add_legacy_name("L232.TechCoef") %>%
      add_precursors("common/GCAM_region_names",
                     "water/water_td_sectors",
                     "energy/A32.globaltech_coef",
                     "L132.water_km3_R_ind_Yh",
                     "L232.StubTechProd_industry",
                     "water/paper_mfg_intensity",
                     "water/mfg_water_ratios",
                     "L1327.out_Mt_R_paper_Yh",
                     "L2327.StubTechProd_paper",
                     "water/food_mfg_intensity",
                     "L1328.out_Pcal_R_food_Yh",
                     "L1328.in_EJ_R_food_F_Yh",
                     "L2328.StubTechProd_food") ->
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

    # Create a data frame of coefficients for food processing industry
    L1328.water_km3_R_ind_Yh_food_recal %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, water_type, year, coefficient) %>%
      repeat_add_columns(distinct(L2328.StubTechProd_food, supplysector, subsector, technology = stub.technology)) %>%
      mutate(water_sector = "Manufacturing",
             minicam.energy.input = set_water_input_name(water_sector, water_type, water_td_sectors),
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES$TechCoef) %>%
      # Fill out the values in the final base year to all future years
      group_by(region, supplysector, subsector, technology, minicam.energy.input, market.name) %>%
      complete(year = MODEL_YEARS) %>%
      mutate(coefficient = if_else(year %in% MODEL_FUTURE_YEARS, coefficient[year == max(MODEL_BASE_YEARS)], coefficient)) %>%
      ungroup() ->
      L232.TechCoef_food

    # add attributes for output
    L232.TechCoef_food %>%
      add_title("Water withdrawal and consumption coefficients for food processing industry") %>%
      add_units("km3/Pcal") %>%
      add_comments("Food processing industry water demand coefficients by region, water type, and year") %>%
      add_legacy_name("L232.TechCoef_food") %>%
      add_precursors("common/GCAM_region_names",
                     "water/water_td_sectors",
                     "L132.water_km3_R_ind_Yh",
                     "water/food_mfg_intensity",
                     "water/mfg_water_ratios",
                     "L1328.out_Pcal_R_food_Yh",
                     "L1328.in_EJ_R_food_F_Yh",
                     "L2328.StubTechProd_food") ->
      L232.TechCoef_food

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
