# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L145.water_demand_municipal
#'
#' Generate municipal water withdrawals, municipal water base delivery cost, and municipal water use efficiency.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L145.municipal_water_R_W_Yh_km3}, \code{L145.municipal_water_cost_R_75USD_m3}, \code{L145.municipal_water_eff_R_Yh}. The corresponding file in the
#' original data system was \code{L145.water_demand_municipal.R} (water level1).
#' @details Generate municipal water withdrawals, municipal water base delivery cost, and municipal water use efficiency.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter full_join if_else group_by left_join matches mutate select summarise
#' @importFrom tidyr complete  drop_na gather
#' @author YL May 2017
module_water_L145.water_demand_municipal <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "water/aquastat_ctry",
             FILE = "water/FAO_municipal_water_AQUASTAT",
             FILE = "water/IBNET_municipal_water_cost_USDm3",
             FILE = "water/municipal_water_use_efficiency",
             FILE = "water/mfg_water_mapping",
             "L100.Pop_thous_ctry_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L145.municipal_water_ctry_W_Yh_km3",
             "L145.municipal_water_R_W_Yh_km3",
             "L145.municipal_water_cost_R_75USD_m3",
             "L145.municipal_water_eff_ctry_Yh",
             "L145.municipal_water_eff_R_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    iso <- aquastat_ctry <- GCAM_region_ID <- Area <- Year <- Value <- cost <- consumption <-
      expenditure <- input.cost <- year <- value <- population <- value_pc <- value_pc_filled <-
      efficiency <- withdrawals <- deflator <- NULL  # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    aquastat_ctry <- get_data(all_data, "water/aquastat_ctry")
    FAO_municipal_water_AQUASTAT <- get_data(all_data, "water/FAO_municipal_water_AQUASTAT")
    IBNET_municipal_water_cost_USDm3 <- get_data(all_data, "water/IBNET_municipal_water_cost_USDm3")
    municipal_water_use_efficiency <- get_data(all_data, "water/municipal_water_use_efficiency")
    mfg_water_mapping <- get_data(all_data, "water/mfg_water_mapping")

    L100.Pop_thous_ctry_Yh <- get_data(all_data, "L100.Pop_thous_ctry_Yh")

    # The first sequence just cleans and completes the data. Because the years in Aquastat may be more recent than the
    # historical time years (e.g., our final data year is 2010 but the aquastat data goes to 2012). and because there's
    # no way to perform population-based interpolation without the more recent population data processed, the method
    # below instead re-sets the reporting year to the maximum historical year, where the reporting year exceeds the max
    # historical year. Note that this does not result in double-reporting as aquastat only reports one year within each
    # 5-year increment
    # Note re: the Sudans: South Sudan was granted its own iso code in 2012 but most of the gcamdata input datasets
    # precede this change, and as such the mappings do not recognize this as a separate country
    L145.municipal_water_ctry_W_Yh_km3 <-
      FAO_municipal_water_AQUASTAT %>%
      left_join_error_no_match(aquastat_ctry[ c("iso", "aquastat_ctry")], by = c(Area = "aquastat_ctry")) %>%
      mutate(Year = if_else(Year > max(HISTORICAL_YEARS), max(HISTORICAL_YEARS), Year)) %>%
      select(iso, Year, Value) %>%
      complete(iso = unique(iso), Year = HISTORICAL_YEARS) %>%
      # Aggregate the Sudans
      group_by(iso, Year) %>%
      summarise(Value = sum(Value)) %>%
      ungroup() %>%
      rename(year = Year, value = Value)

    # At this point the municipal water demand data are ready to be interpolated and extrapolated
    # The method uses linear interpolation of per-capita demands in years between known data points, and
    # constant per-capita demands in years outside of the known data points.
    L145.Pop_thous_ctry_Yh <- L100.Pop_thous_ctry_Yh %>%
      rename(population = value)

    L145.municipal_water_ctry_W_Yh_km3 <-
      left_join(L145.municipal_water_ctry_W_Yh_km3, L145.Pop_thous_ctry_Yh, by = c("iso", "year")) %>%
      mutate(value_pc = value / population) %>%
      group_by(iso) %>%
      # If there's only one observation (year) available for any country, approx(x, y, rule = 2) will wipe it out,
      # putting in missing values in all years. The sequence below achieves a simple rule = 2 extrapolation, but is
      # capable of extrapolation from a single observation
      mutate(value_pc_filled = approx_fun(year = year, value = value_pc, rule = 2),
             value_pc_filled = if_else(is.na(value_pc_filled), median(value_pc, na.rm = TRUE), as.numeric(value_pc_filled))) %>%
      ungroup() %>%
      mutate(value = round(population * value_pc_filled, digits = water.DIGITS_MUNI_WATER)) %>%
      select(iso, year, value)

    # Come up with GCAM regional average prices starting with the country level IBNET data.
    # Note that since the years are all over the place, we will convert all to 1975 dollar,
    # and use the average across years too.

    # Since IBNET_municipal_water_cost_USDm3 is in nominal years we will need a table of deflators
    # to normalize each to constant 1975$
    # The pipeline below uses group_by() to apply the gdp_deflator() function to each row
    tibble(year = unique(IBNET_municipal_water_cost_USDm3$year)) %>%
      mutate(deflator = gdp_deflator(1975, year)) ->
      conv_Price_DollarYear

    L145.municipal_water_cost_R_75USD_m3 <- IBNET_municipal_water_cost_USDm3 %>%
      left_join_error_no_match(aquastat_ctry[ c("aquastat_ctry", "iso")], by = c("country" = "aquastat_ctry")) %>%
      left_join_error_no_match(iso_GCAM_regID[c("iso", "GCAM_region_ID")], by = "iso") %>%
      left_join_error_no_match(conv_Price_DollarYear, by = "year") %>%
      # Convert nominal dollars to 1975$
      mutate(cost = cost * deflator,
             expenditure = cost * consumption) %>%
      group_by(GCAM_region_ID) %>%
      summarise(expenditure = sum(expenditure), consumption = sum(consumption)) %>%
      mutate(input.cost = round(expenditure / consumption, water.DIGITS_MUNI_WATER)) %>%
      select(GCAM_region_ID, input.cost)

    # The IBNET data is incomplete and so it is possible that we have entire GCAM regions in which none
    # of the contained countries had any reported data. Just fill these out with default (median) values
    default_muni_water_price <- median(L145.municipal_water_cost_R_75USD_m3$input.cost)
    L145.municipal_water_cost_R_75USD_m3 <- complete(L145.municipal_water_cost_R_75USD_m3,
                                                     GCAM_region_ID = sort(unique(iso_GCAM_regID$GCAM_region_ID))) %>%
      mutate(input.cost = if_else(is.na(input.cost), default_muni_water_price, input.cost))

    # Map water use efficiencies from the continent scale to GCAM regions
    # Note that the continent names were set to the same as Vassolo and Doll (mfg_water_mapping) to avoid the need for
    # an additional mapping file here
    L145.municipal_water_eff_ctry_Yh <-
      full_join(mfg_water_mapping, municipal_water_use_efficiency, by = "continent") %>%
      gather(year, efficiency, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) %>%
      select(iso, year, efficiency) %>%
      group_by(iso) %>%
      complete(year = sort(unique(c(year, HISTORICAL_YEARS)))) %>%
      mutate(efficiency = approx_fun(year = year, value = efficiency)) %>%
      ungroup() %>%
      filter(year %in% HISTORICAL_YEARS)

    # Calculate nation-level consumption as withdrawals times efficiency, aggregate flow volumes by GCAM region, and
    # calculate the average efficiency by GCAM region and year
    L145.municipal_water_ctry_ALL_Yh_km3 <- L145.municipal_water_eff_ctry_Yh %>%
      left_join(L145.municipal_water_ctry_W_Yh_km3, by = c("iso", "year")) %>%
      rename(withdrawals = value) %>%
      drop_na(withdrawals) %>%
      mutate(consumption = efficiency * withdrawals)

    L145.municipal_water_R_ALL_Yh_km3 <-
      left_join(L145.municipal_water_ctry_ALL_Yh_km3,
                iso_GCAM_regID[c("iso", "GCAM_region_ID")], by = "iso") %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(withdrawals = sum(withdrawals),
                consumption = sum(consumption)) %>%
      ungroup() %>%
      mutate(efficiency = consumption / withdrawals)

    #Compile data to be written out in the final format, selecting the appropriate columns
    L145.municipal_water_ctry_W_Yh_km3 <- select(L145.municipal_water_ctry_ALL_Yh_km3, iso, year, withdrawals)
    L145.municipal_water_R_W_Yh_km3 <- select(L145.municipal_water_R_ALL_Yh_km3, GCAM_region_ID, year, withdrawals, consumption)
    L145.municipal_water_eff_ctry_Yh <- select(L145.municipal_water_ctry_ALL_Yh_km3, iso, year, efficiency)
    L145.municipal_water_eff_R_Yh <- select(L145.municipal_water_R_ALL_Yh_km3, GCAM_region_ID, year, efficiency)

    # Produce outputs
    L145.municipal_water_ctry_W_Yh_km3 %>%
      add_title("Municipal water withdrawals by GCAM_region_ID for all historical years ") %>%
      add_units("km^3") %>%
      add_comments("FAO_municipal_water_AQUASTAT by country interpolated linearly on a per-capita basis") %>%
      add_legacy_name("L145.municipal_water_ctry_W_Yh_km3") %>%
      add_precursors("water/aquastat_ctry",
                     "water/FAO_municipal_water_AQUASTAT",
                     "L100.Pop_thous_ctry_Yh") ->
      L145.municipal_water_ctry_W_Yh_km3

    L145.municipal_water_R_W_Yh_km3 %>%
      add_title("Municipal water withdrawals by GCAM_region_ID for all historical years ") %>%
      add_units("km^3") %>%
      add_comments("FAO_municipal_water_AQUASTAT interpolated linearly on a per-capita basis aggregated by GCAM region") %>%
      add_legacy_name("L145.municipal_water_R_W_Yh_km3") %>%
      add_precursors("common/iso_GCAM_regID",
                     "water/aquastat_ctry",
                     "water/FAO_municipal_water_AQUASTAT",
                     "L100.Pop_thous_ctry_Yh") ->
      L145.municipal_water_R_W_Yh_km3

    L145.municipal_water_eff_ctry_Yh %>%
      add_title("Municipal water use efficiency by country for all years") %>%
      add_units("%") %>%
      add_comments("Interpolate and map exogenous water use efficiencies from the continent scale to countries") %>%
      add_legacy_name("L145.municipal_water_eff_ctry_Yh") %>%
      add_precursors("water/municipal_water_use_efficiency",
                     "water/mfg_water_mapping") ->
      L145.municipal_water_eff_ctry_Yh

    L145.municipal_water_eff_R_Yh %>%
      add_title("Municipal water use efficiency by GCAM_region_ID for all years") %>%
      add_units("%") %>%
      add_comments("1. Interpolate and map exogenous water use efficiencies from the continent scale to countries;
                   2. Multiply by withdrawal volumes to calculate consumption;
                   3. Divide consumption by withdrawals to calculate efficiencies, averaged by region") %>%
      add_legacy_name("L145.municipal_water_eff_R_Yh") %>%
      add_precursors("common/iso_GCAM_regID",
                     "water/aquastat_ctry",
                     "water/FAO_municipal_water_AQUASTAT",
                     "water/municipal_water_use_efficiency",
                     "water/mfg_water_mapping") ->
      L145.municipal_water_eff_R_Yh

    L145.municipal_water_cost_R_75USD_m3 %>%
      add_title("Municipal water base deleivery cost by GCAM_region_ID") %>%
      add_units("1975$/m^3") %>%
      add_comments("Generate GCAM regional average prices starting with the country-level IBNET data") %>%
      add_comments("1. Convert nominal dollars to 1975$;
                   2. Get country-level expenditure (cost * consumption);
                   3. Sum up country-level expenditure and cost to get region-level expenditure and cost;
                   4. Divide region-level expenditure by region-level consumption to get region-level cost") %>%
      add_legacy_name("L145.municipal_water_cost_R_75USD_m3") %>%
      add_precursors("common/iso_GCAM_regID",
                     "water/IBNET_municipal_water_cost_USDm3") ->
      L145.municipal_water_cost_R_75USD_m3

    return_data(L145.municipal_water_ctry_W_Yh_km3,
                L145.municipal_water_R_W_Yh_km3,
                L145.municipal_water_eff_ctry_Yh,
                L145.municipal_water_eff_R_Yh,
                L145.municipal_water_cost_R_75USD_m3)
  } else {
    stop("Unknown command")
  }
}
