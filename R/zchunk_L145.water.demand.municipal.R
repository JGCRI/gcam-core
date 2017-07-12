#' module_water_L145.water.demand.municipal
#'
#' Generate municipal water withdrawals, municipal water base delivery cost, and municipal water use efficiency.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L145.municipal_water_R_W_Yh_km3}, \code{L145.municipal_water_cost_R_75USD_m3}, \code{L145.municipal_water_eff_R_Y}. The corresponding file in the
#' original data system was \code{L145.water.demand.municipal.R} (water level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YL May 2017
module_water_L145.water.demand.municipal <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "common/GCAM_region_names",
             FILE = "aglu/AGLU_ctry",
             FILE = "water/FAO_municipal_water_AQUASTAT",
             FILE = "water/IBNET_municipal_water_cost_USDm3",
             FILE = "water/municipal_water_use_efficiency",
             FILE = "water/manufacturing_water_mapping"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L145.municipal_water_R_W_Yh_km3",
             "L145.municipal_water_cost_R_75USD_m3",
             "L145.municipal_water_eff_R_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    iso <- FAO_country <- GCAM_region_ID <- Area <- Year <- Value <- cost <- consumption <-
      expenditure <- input.cost <- year <- value <- NULL  # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    FAO_municipal_water_AQUASTAT <- get_data(all_data, "water/FAO_municipal_water_AQUASTAT")
    IBNET_municipal_water_cost_USDm3 <- get_data(all_data, "water/IBNET_municipal_water_cost_USDm3")
    municipal_water_use_efficiency <- get_data(all_data, "water/municipal_water_use_efficiency")
    manufacturing_water_mapping <- get_data(all_data, "water/manufacturing_water_mapping")

    # Aggregate FAO_municipal_water_AQUASTAT to GCAM regions and fill out the years for missing data.

    # Map FAO country to GCAM region ID
    AGLU_ctry %>%
      select(iso, FAO_country) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") ->
      FAO_ctry_GCAM_region_ID

    FAO_municipal_water_AQUASTAT %>%
      rename(FAO_country = Area) %>%
      inner_join(select(FAO_ctry_GCAM_region_ID, GCAM_region_ID, FAO_country), by = "FAO_country") ->
      ctry_municipal_W

    # Aggregate to regions and fill out missing years using rule=2
    ctry_municipal_W %>%
      group_by(GCAM_region_ID, Year) %>%
      summarise(Value = sum(Value)) %>%
      ungroup() %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               Year = unique(c(ctry_municipal_W$Year, HISTORICAL_YEARS)),
               fill = list(Value = NA)) %>%
      arrange(GCAM_region_ID, Year) %>%
      group_by(GCAM_region_ID) %>%
      mutate(Value = approx_fun(Year, Value, rule = 2)) %>%
      filter(Year %in% HISTORICAL_YEARS) %>%
      rename(value = Value, year = Year) %>%
      mutate(water_type = "water withdrawals") ->
      municipal_water_R_W_Yh_km3

    # Come up with GCAM regional average prices starting with the country level IBNET data.
    # Note since the years are all over the place we will just use the average across years too.
    # Come up with regional municipal prices from IBNET data
    IBNET_municipal_water_cost_USDm3 %>%
      inner_join(FAO_ctry_GCAM_region_ID, by = c("country" = "FAO_country")) %>%
      mutate(expenditure = cost * consumption)  %>%
      group_by(GCAM_region_ID) %>%
      summarise(expenditure = sum(expenditure), consumption = sum(consumption)) %>%
      mutate(input.cost = expenditure / consumption) %>%
      select(GCAM_region_ID, input.cost)->
      municipal_water_cost_R_75USD_m3

    # The IBNET data is incomplete and so it is very possible that we have entire GCAM regions in which none
    # of the contained countries had any reported data.
    stopifnot(nrow( municipal_water_cost_R_75USD_m3) == nrow(GCAM_region_names))

    # Map water use efficiencies from the continent scale to GCAM regions
    # Note names were changed to match manufacturing continent to avoid the need for an additional mapping

    # Map municipal water use efficiencies to GCAM regions
    municipal_water_use_efficiency %>%
      gather(year, value, matches(YEAR_PATTERN)) %>%
      mutate(year = as.integer(year)) ->
      municipal_water_use_efficiency_tmp

    municipal_water_use_efficiency_tmp %>%
      # We want all combinations of matches between these two tables so use an inner join
      inner_join(manufacturing_water_mapping, by = "continent") %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      select(GCAM_region_ID, year, value) %>%
      ungroup() %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               year = c(MODEL_YEARS, municipal_water_use_efficiency_tmp$year),
               fill = list(value = NA)) %>%
      # Fill out the coefficients for all years using rule=2
      arrange(GCAM_region_ID, year) %>%
      group_by(GCAM_region_ID) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      filter(year %in% MODEL_YEARS) %>%
      ungroup() %>%
      select(GCAM_region_ID, year, coefficient = value) ->
      municipal_water_eff_R_Y

    # Produce outputs
    municipal_water_R_W_Yh_km3 %>%
      add_title("Municipal water withdrawals by GCAM_region_ID for all historical years ") %>%
      add_units("km^3") %>%
      add_comments("Aggregate FAO_municipal_water_AQUASTAT to GCAM regions and fill out the years for missing data by rule=2") %>%
      add_legacy_name("L145.municipal_water_R_W_Yh_km3") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/AGLU_ctry",
                     "water/FAO_municipal_water_AQUASTAT") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_SUM_TEST) ->
      L145.municipal_water_R_W_Yh_km3

    municipal_water_cost_R_75USD_m3 %>%
      add_title("Municipal water base deleivery cost by GCAM_region_ID") %>%
      add_units("1975$/m^3") %>%
      add_comments("Generate GCAM regional average prices starting with the country-level IBNET data") %>%
      add_comments("1. Get country-level expenditure (cost * consumption);
                   2. Sum up country-level expenditure and cost to get region-level expenditure and cost;
                   3. Divide region-level expenditure by region-level consumption to get region-level cost") %>%
      add_legacy_name("L145.municipal_water_cost_R_75USD_m3") %>%
      add_precursors("common/iso_GCAM_regID",
                     "water/IBNET_municipal_water_cost_USDm3") ->
      L145.municipal_water_cost_R_75USD_m3

    municipal_water_eff_R_Y %>%
      add_title("Municipal water use efficiency by GCAM_region_ID for all years") %>%
      add_units("%") %>%
      add_comments("1. Map water use efficiencies from the continent scale to GCAM regions;
                   2. Fill out the coefficients for all years using rule=2") %>%
      add_legacy_name("L145.municipal_water_eff_R_Y") %>%
      add_precursors("common/GCAM_region_names",
                     "water/municipal_water_use_efficiency",
                     "water/manufacturing_water_mapping") ->
      L145.municipal_water_eff_R_Y

    return_data(L145.municipal_water_R_W_Yh_km3, L145.municipal_water_cost_R_75USD_m3, L145.municipal_water_eff_R_Y)
  } else {
    stop("Unknown command")
  }
}
