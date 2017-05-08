#' module_water_L1233.Elec_water
#'
#' Water consumption and withdrawals for electricity.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1233.out_EJ_R_elec_F_tech_Yh_cool}, \code{L1233.in_EJ_R_elec_F_tech_Yh_cool}, \code{L1233.wdraw_km3_R_elec}, \code{L1233.wcons_km3_R_elec}, \code{L1233.shrwt_R_elec_cool_Yf}. The corresponding file in the
#' original data system was \code{L1233.Elec_water.R} (water level1).
#' @details Categorizes electiricty generating technologies by water type, and computes water withdrawals and consumption.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author SWDT May 2017
module_water_L1233.Elec_water <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "energy/calibrated_techs",
             FILE = "energy/enduse_fuel_aggregation",
             FILE = "temp-data-inject/L101.en_bal_EJ_ctry_Si_Fi_Yh_full",
             FILE = "temp-data-inject/L1231.in_EJ_R_elec_F_tech_Yh",
             FILE = "temp-data-inject/L1231.out_EJ_R_elec_F_tech_Yh",
             FILE = "water/A23.CoolingSystemShares_RG3",
             FILE = "water/elec_tech_water_map",
             FILE = "water/Macknick_elec_water_m3MWh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1233.out_EJ_R_elec_F_tech_Yh_cool",
             "L1233.in_EJ_R_elec_F_tech_Yh_cool",
             "L1233.wdraw_km3_R_elec",
             "L1233.wcons_km3_R_elec",
             "L1233.shrwt_R_elec_cool_Yf"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    enduse_fuel_aggregation <- get_data(all_data, "energy/enduse_fuel_aggregation")
    A23.CoolingSystemShares_RG3 <- get_data(all_data, "water/A23.CoolingSystemShares_RG3")
    elec_tech_water_map <- get_data(all_data, "water/elec_tech_water_map")
    Macknick_elec_water_m3MWh <- get_data(all_data, "water/Macknick_elec_water_m3MWh")

    L101.en_bal_EJ_ctry_Si_Fi_Yh_full <-
      get_data(all_data, "temp-data-inject/L101.en_bal_EJ_ctry_Si_Fi_Yh_full") %>%
      gather(year, value, -iso, -GCAM_region_ID, -sector, -fuel) %>%
      mutate(year = as.integer(substr(year, 2, 5)))

    L1231.in_EJ_R_elec_F_tech_Yh <-
      get_data(all_data, "temp-data-inject/L1231.in_EJ_R_elec_F_tech_Yh") %>%
      gather(year, value, -GCAM_region_ID, -sector, -fuel, -technology) %>%
      mutate(year = as.integer(substr(year, 2, 5)))

    L1231.out_EJ_R_elec_F_tech_Yh <-
      get_data(all_data, "temp-data-inject/L1231.out_EJ_R_elec_F_tech_Yh") %>%
      gather(year, value, -GCAM_region_ID, -sector, -fuel, -technology) %>%
      mutate(year = as.integer(substr(year, 2, 5)))

    # ===================================================

    # STEP 1. DOWNSCALE WATER COOLING SYSTEM SHARES FROM 14 REGIONS to 201 COUNTRIES

    # RENAME INTERMEDIATE FUELS AND AGGREGATE BY UPDATED FUEL TYPE
    L101.en_bal_EJ_ctry_Si_Fi_Yh_full %>%
      filter(sector == "out_electricity generation") %>%
      left_join(select(enduse_fuel_aggregation, fuel, electricity), by = "fuel") %>%
      select(-fuel) %>%
      rename(fuel = electricity) -> L1233.out_EJ_ctry_elec_Fi_Yh_full

    L1233.out_EJ_ctry_elec_Fi_Yh_full %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      group_by(iso, year, fuel) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(is.na(fuel) == F) %>%
      mutate(sector = "electricity generation") -> L1233.out_EJ_ctry_elec_F_Yh

    # INTERPOLATE A23.CoolingSystemShares_RG3 FOR HISTORICAL YEARS AND HISTORICAL + FUTURE YEARS
    A23.CoolingSystemShares_RG3 %>%
      gather(year, value, -region_GCAM3, -plant_type, -cooling_system, -water_type) %>%
      mutate(year = as.integer(year)) -> A23.CoolingSystemShares_RG3_LF

    INTERPOLATED_YEARS <- HISTORICAL_YEARS[HISTORICAL_YEARS %in%
                                              unique(A23.CoolingSystemShares_RG3_LF$year) == F]
    EXTRAPOLATED_YEARS <- c(HISTORICAL_YEARS, FUTURE_YEARS)[c(HISTORICAL_YEARS, FUTURE_YEARS) %in%
                                                              unique(A23.CoolingSystemShares_RG3_LF$year) == F]

    setNames(data.frame(matrix(ncol = length(INTERPOLATED_YEARS),
                               nrow = nrow(A23.CoolingSystemShares_RG3))),
             as.character(INTERPOLATED_YEARS)) %>%
      as_tibble() %>%
      bind_cols(A23.CoolingSystemShares_RG3) %>%
      gather(year, value, -region_GCAM3, -plant_type, -cooling_system, -water_type) %>%
      mutate(year = as.integer(year)) %>%
      arrange(year) %>%
      group_by(region_GCAM3, plant_type, cooling_system, water_type) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup() -> A23.CoolingSystemShares_RG3_LF_interp

    setNames(data.frame(matrix(ncol = length(EXTRAPOLATED_YEARS),
                               nrow = nrow(A23.CoolingSystemShares_RG3))),
             as.character(EXTRAPOLATED_YEARS)) %>%
      as_tibble() %>%
      bind_cols(A23.CoolingSystemShares_RG3) %>%
      gather(year, value, -region_GCAM3, -plant_type, -cooling_system, -water_type) %>%
      mutate(year = as.integer(year)) %>%
      arrange(year) %>%
      group_by(region_GCAM3, plant_type, cooling_system, water_type) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup() -> A23.CoolingSystemShares_RG3_LF_future

    # WRITE OUT REGIONAL LEVEL COOLING INFORMATION TO ALL COUNTRIES...
    # ... AND MATCH GCAM region_GCAM3 INTO TIBBLE
    elec_tech_water_map %>%
      semi_join(calibrated_techs, by = c("sector", "fuel", "technology")) %>%
      select(sector, fuel, technology, cooling_system, water_type, plant_type) -> elec_tech_water_map_cal
    elec_tech_water_map_cal %>%
      repeat_add_columns(select(L1233.out_EJ_ctry_elec_F_Yh, iso) %>% unique()) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, region_GCAM3), by = "iso") %>%
      left_join(A23.CoolingSystemShares_RG3_LF_interp,
                by = c("cooling_system", "water_type", "plant_type", "region_GCAM3")) -> L1233.weights_ctry_elec_F_Yh_cool

    # ADD TECHNOLOGIES WITH NO COOLING FOR ALL HISTORICAL YEARS
    L1233.weights_ctry_elec_F_Yh_cool %>%
      filter(plant_type == "no cooling") %>%
      mutate(value = 1) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = HISTORICAL_YEARS)) %>%
      bind_rows(L1233.weights_ctry_elec_F_Yh_cool) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      rename(share = value) -> L1233.weights_ctry_elec_F_Yh_cool_withNoCool


    # STEP 2. MULTIPLY SHARES BY COUNTRY LEVEL GENERATION TO GET ELECTRICITY...
    # ... OUTPUT BY GENERATING AND COOLING TECHNOLOGIES
    L1233.weights_ctry_elec_F_Yh_cool_withNoCool %>%
      left_join(L1233.out_EJ_ctry_elec_F_Yh, by = c("iso", "year", "fuel", "sector")) %>%
      rename(gen = value) %>%
      mutate(output = share * gen) %>%
      select(-share, -gen) %>%
      mutate(output = if_else(is.na(output) == T, 0, output)) -> L1233.output_ctry_elec_F_Yh_cool

    # STEP 3: AGGREGATE COUNTRY-LEVEL DATA TO NEW GCAM REGIONS
    # (a) ...WITH COOLING TECHNOLOGIES
    L1233.output_ctry_elec_F_Yh_cool %>%
      left_join_error_no_match(select(iso_GCAM_regID,-country_name, -region_GCAM3), by = "iso") %>%
      group_by(GCAM_region_ID, sector, fuel, technology, cooling_system, water_type, year) %>%
      summarise(output = sum(output)) %>%
      ungroup() -> L1233.weights_R_elec_F_Yh_cool
    # (b) ...WITHOUT COOLING TECHNOLOGIES
    L1233.output_ctry_elec_F_Yh_cool %>%
      left_join_error_no_match(select(iso_GCAM_regID,-country_name, -region_GCAM3), by = "iso") %>%
      group_by(GCAM_region_ID, sector, fuel, technology, year) %>%
      summarise(output = sum(output)) %>%
      ungroup() -> L1233.weights_R_elec_F_Yh

    # STEP 4: MATCH REGIONAL-LEVEL SHARES OF COOLING TECHNOLOGY WITH GENERATION TECHNOLOGY
    elec_tech_water_map_cal %>%
      select(sector, fuel, technology, cooling_system, water_type, plant_type) %>%
      repeat_add_columns(unique(select(iso_GCAM_regID, GCAM_region_ID))) %>%
      select(GCAM_region_ID, sector, fuel, technology, cooling_system, water_type, plant_type) %>%
      left_join(L1233.weights_R_elec_F_Yh_cool,
                by = c("GCAM_region_ID", "sector", "fuel", "technology", "cooling_system", "water_type")) %>%
      rename(output_cool = output) %>%
      left_join(L1233.weights_R_elec_F_Yh,
                by = c("GCAM_region_ID", "sector", "fuel", "technology", "year")) %>%
      mutate(share = output_cool / output) %>% select(-output_cool, -output) %>%
      arrange(GCAM_region_ID, sector, fuel, technology, cooling_system, water_type, plant_type, year) %>%
      mutate(share = if_else(year == tail(HISTORICAL_YEARS, 1) & is.na(share), -1, share)) ->
      L1233.shares_R_elec_F_tech_Yh_cool

    ## REMOVE NA VALUES BY PULLING FROM YEAR AHEAD IN EACH GROUP
    for(i in (nrow(L1233.shares_R_elec_F_tech_Yh_cool) - 1) : 1){
      if (is.na(L1233.shares_R_elec_F_tech_Yh_cool$share[i])){
        L1233.shares_R_elec_F_tech_Yh_cool$share[i] <-
          L1233.shares_R_elec_F_tech_Yh_cool$share[i+1]
      }
    }

    # STEP 5: MULTIPLY SHARES BY TECHNOLOGY-LEVEL OUTPUT TO COMPUTE NEW CALIBRATION DATASET
    L1233.shares_R_elec_F_tech_Yh_cool %>%
      left_join(L1231.out_EJ_R_elec_F_tech_Yh,
                               by = c("GCAM_region_ID", "sector", "fuel", "technology", "year")) %>%
      mutate(value = share * value) %>% select(-share) -> L1233.out_EJ_R_elec_F_tech_Yh_cool


    # STEP 6: PARTITION FUEL INPUTS TO ELECTRICITY GENERATION...
    # ...  USING COOLING-WITHIN-GENERATION TECHNOLOGY SHARES
    # NOTE: ASSUMES UNIFORM EFFICIENCY ACROSS ALL COOLING SYSTEM TYPES
    L1233.shares_R_elec_F_tech_Yh_cool %>%
      filter(fuel %in% L1231.in_EJ_R_elec_F_tech_Yh$fuel) %>%
      filter(technology %in% L1231.in_EJ_R_elec_F_tech_Yh$technology) %>%
      left_join(L1231.in_EJ_R_elec_F_tech_Yh,
                               by = c("GCAM_region_ID", "sector", "fuel", "technology", "year")) %>%
      mutate(value = share * value) %>% select(-share) -> L1233.in_EJ_R_elec_F_tech_Yh_cool


    # STEP 7: MULTIPLY ELECTRICITY GENERATION BY WITHDRAWAL AND CONSUMPTION...
    # ... COEFFICIENTS, AND AGGREGATE BY REGION

    # WITHDRAWALS
    L1233.out_EJ_R_elec_F_tech_Yh_cool %>%
      left_join_error_no_match(Macknick_elec_water_m3MWh,
                               by = c("sector", "fuel", "technology", "cooling_system", "water_type")) %>%
      mutate(value = value * water_withdrawals / CONV_MWH_GJ) %>%
      select(-water_withdrawals, - water_consumption) -> L1233.wdraw_km3_R_elec_F_tech_Yh_cool
    L1233.wdraw_km3_R_elec_F_tech_Yh_cool %>%
      group_by(GCAM_region_ID, sector, water_type, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(water_type != "none") -> L1233.wdraw_km3_R_elec

    # CONSUMPTION
    L1233.out_EJ_R_elec_F_tech_Yh_cool %>%
      left_join_error_no_match(Macknick_elec_water_m3MWh,
                               by = c("sector", "fuel", "technology", "cooling_system", "water_type")) %>%
      mutate(value = value * water_consumption / CONV_MWH_GJ) %>%
      select(-water_withdrawals, - water_consumption) -> L1233.wcons_km3_R_elec_F_tech_Yh_cool
    L1233.wcons_km3_R_elec_F_tech_Yh_cool %>%
      group_by(GCAM_region_ID, sector, water_type, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      filter(water_type != "none") -> L1233.wcons_km3_R_elec

    # STEP 8: COMPUTE REGIONAL-LEVEL FUTURE SHARE WEIGHTS...
    # ... BASED ON GCAM 3.0, 14-REGION ASSUMPTIONS. NOTE: USES ...
    # ... REPRESENTATIVE COUNTRIES RATHER THAN WEIGHTED AVERAGES ...
    # ... (COUNTRY WITH MOST ELEC AS THE REPRESENTATIVE)
    L1233.out_EJ_ctry_elec_Fi_Yh_full %>%
      filter(year == tail(HISTORICAL_YEARS, 1)) %>%
      group_by(iso, GCAM_region_ID) %>%
      summarise(value = sum(value)) %>%
      arrange(GCAM_region_ID) %>% ungroup() -> L1233.out_EJ_ctry_elec_Yfby

    L1233.out_EJ_ctry_elec_Yfby %>%
      group_by(GCAM_region_ID) %>%
      filter(value == max(value)) %>% ungroup() %>%
      left_join_error_no_match(select(iso_GCAM_regID, -country_name),
                               by = c("iso", "GCAM_region_ID")) -> L1233.R_iso_RG3


    # STEP 9: FILL OUT TABLE OF ALL TECHNOLOGIES TO MATCH IN SHARES
    elec_tech_water_map %>%
      select(sector, fuel, technology, cooling_system, water_type, plant_type) %>%
      repeat_add_columns(select(iso_GCAM_regID, GCAM_region_ID) %>% unique()) %>%
      arrange(GCAM_region_ID) %>%
      left_join(select(L1233.R_iso_RG3, -value, -iso),
                by = "GCAM_region_ID") %>%
      mutate(plant_type = sub( "\\ \\(CCS\\)", "", plant_type)) %>%
      right_join(A23.CoolingSystemShares_RG3_LF_future,
                by = c("cooling_system", "water_type", "plant_type", "region_GCAM3")) %>%
      select(-region_GCAM3, -plant_type) ->
      L1233.shrwt_R_elec_cool_Yf


    ## OUTPUTS
    L1233.out_EJ_R_elec_F_tech_Yh_cool %>%
      add_title("Electricity output by region, fuel, technology, cooling system, and water type") %>%
      add_units("EJ") %>%
      add_comments("Computed by multiplying shares by technology-level output") %>%
      add_legacy_name("L1233.out_EJ_R_elec_F_tech_Yh_cool") %>%
      add_precursors("common/iso_GCAM_regID",
                     "energy/calibrated_techs",
                     "energy/enduse_fuel_aggregation",
                     "temp-data-inject/L101.en_bal_EJ_ctry_Si_Fi_Yh_full",
                     "temp-data-inject/L1231.in_EJ_R_elec_F_tech_Yh",
                     "temp-data-inject/L1231.out_EJ_R_elec_F_tech_Yh",
                     "water/A23.CoolingSystemShares_RG3",
                     "water/elec_tech_water_map",
                     "water/Macknick_elec_water_m3MWh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1233.out_EJ_R_elec_F_tech_Yh_cool
    L1233.in_EJ_R_elec_F_tech_Yh_cool %>%
      add_title("Fuel inputs to electricity generation by region, fuel, technology, cooling system, and water type") %>%
      add_units("EJ") %>%
      add_comments("Computed by multiplying shares by technology-level input") %>%
      add_legacy_name("L1233.in_EJ_R_elec_F_tech_Yh_cool") %>%
      add_precursors("common/iso_GCAM_regID",
                     "energy/calibrated_techs",
                     "energy/enduse_fuel_aggregation",
                     "temp-data-inject/L101.en_bal_EJ_ctry_Si_Fi_Yh_full",
                     "temp-data-inject/L1231.in_EJ_R_elec_F_tech_Yh",
                     "temp-data-inject/L1231.out_EJ_R_elec_F_tech_Yh",
                     "water/A23.CoolingSystemShares_RG3",
                     "water/elec_tech_water_map",
                     "water/Macknick_elec_water_m3MWh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1233.in_EJ_R_elec_F_tech_Yh_cool
    L1233.wdraw_km3_R_elec %>%
      add_title("Water withdrawals for electricity generation by region and water type") %>%
      add_units("km^3") %>%
      add_comments("Computed by multiplying generation by water withdrawal coefficients") %>%
      add_legacy_name("L1233.wdraw_km3_R_elec") %>%
      add_precursors("common/iso_GCAM_regID",
                     "energy/calibrated_techs",
                     "energy/enduse_fuel_aggregation",
                     "temp-data-inject/L101.en_bal_EJ_ctry_Si_Fi_Yh_full",
                     "temp-data-inject/L1231.in_EJ_R_elec_F_tech_Yh",
                     "temp-data-inject/L1231.out_EJ_R_elec_F_tech_Yh",
                     "water/A23.CoolingSystemShares_RG3",
                     "water/elec_tech_water_map",
                     "water/Macknick_elec_water_m3MWh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1233.wdraw_km3_R_elec
    L1233.wcons_km3_R_elec %>%
      add_title("Water consumption for electricity generation by region and water type") %>%
      add_units("km^3") %>%
      add_comments("Computed by multiplying generation by water consumption coefficients") %>%
      add_legacy_name("L1233.wcons_km3_R_elec") %>%
      add_precursors("common/iso_GCAM_regID",
                     "energy/calibrated_techs",
                     "energy/enduse_fuel_aggregation",
                     "temp-data-inject/L101.en_bal_EJ_ctry_Si_Fi_Yh_full",
                     "temp-data-inject/L1231.in_EJ_R_elec_F_tech_Yh",
                     "temp-data-inject/L1231.out_EJ_R_elec_F_tech_Yh",
                     "water/A23.CoolingSystemShares_RG3",
                     "water/elec_tech_water_map",
                     "water/Macknick_elec_water_m3MWh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1233.wcons_km3_R_elec
    L1233.shrwt_R_elec_cool_Yf %>%
      add_title("Future cooling system shareweights by region, electric sector, and technology ") %>%
      add_units("Unitless") %>%
      add_comments("Filled out using cooling shares table") %>%
      add_legacy_name("L1233.shrwt_R_elec_cool_Yf") %>%
      add_precursors("common/iso_GCAM_regID",
                     "energy/calibrated_techs",
                     "energy/enduse_fuel_aggregation",
                     "temp-data-inject/L101.en_bal_EJ_ctry_Si_Fi_Yh_full",
                     "temp-data-inject/L1231.in_EJ_R_elec_F_tech_Yh",
                     "temp-data-inject/L1231.out_EJ_R_elec_F_tech_Yh",
                     "water/A23.CoolingSystemShares_RG3",
                     "water/elec_tech_water_map",
                     "water/Macknick_elec_water_m3MWh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1233.shrwt_R_elec_cool_Yf

    return_data(L1233.out_EJ_R_elec_F_tech_Yh_cool, L1233.in_EJ_R_elec_F_tech_Yh_cool, L1233.wdraw_km3_R_elec, L1233.wcons_km3_R_elec, L1233.shrwt_R_elec_cool_Yf)
  } else {
    stop("Unknown command")
  }
}
