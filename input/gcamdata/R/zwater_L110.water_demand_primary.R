# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_water_L110.water_demand_primary
#'
#' Gets water use coefficients for primary energy production.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L110.water_demand_primary_R_S_W_m3_GJ}. The corresponding file in the
#' original data system was \code{L110.water_demand_primary.R} (water level1).
#' @details Gets water use coefficients for primary energy production using regional
#' fractions of saline and freshwater shares.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else left_join mutate select
#' @importFrom tibble as_tibble
#' @author SWDT April 2017
module_water_L110.water_demand_primary <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "common/iso_GCAM_regID",
             FILE = "water/A227.resource_water_coef_mapping",
             FILE = "water/resource_water_data",
             FILE = "water/resource_water_share",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A23.globaltech_eff",
             "L1012.en_bal_EJ_R_Si_Fi_Yh",
             "L103.water_mapping_R_B_W_Ws_share",
             "L121.in_EJ_R_TPES_unoil_Yh",
             "L121.in_EJ_R_TPES_crude_Yh",
             "L122.in_EJ_R_gasproc_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L110.water_demand_primary_R_S_W_m3_GJ",
             "L110.in_km3_water_primary",
             "L110.in_km3_water_primary_basin"))
  } else if(command == driver.MAKE) {

    fuel <- supplysector <- water.coefficient.m3.per.TJ <- region <- rsrc_region <-
      GCAM_region_ID <- region_GCAM3 <- fresh <- coefficient_WC <- cons_fr <-
      cal <- water_type <- coefficient <- sal <- sector <- year <- value <-
      subsector <- minicam.energy.input <- efficiency <- water_sector <- share <-
      GCAM_basin_ID <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)
    A227.resource_water_coef_mapping <- get_data(all_data, "water/A227.resource_water_coef_mapping")
    resource_water_data <- get_data(all_data, "water/resource_water_data")
    resource_water_share <- get_data(all_data, "water/resource_water_share")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A23.globaltech_eff <- get_data(all_data, "energy/A23.globaltech_eff")
    L1012.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1012.en_bal_EJ_R_Si_Fi_Yh", strip_attributes = TRUE)
    L103.water_mapping_R_B_W_Ws_share <- get_data(all_data, "L103.water_mapping_R_B_W_Ws_share")
    L121.in_EJ_R_TPES_unoil_Yh <- get_data(all_data, "L121.in_EJ_R_TPES_unoil_Yh")
    L121.in_EJ_R_TPES_crude_Yh <- get_data(all_data, "L121.in_EJ_R_TPES_crude_Yh")
    L122.in_EJ_R_gasproc_F_Yh <- get_data(all_data, "L122.in_EJ_R_gasproc_F_Yh")

    # ===================================================

    # Get water consumption (m^3/TJ) by fuel and supply sector
    A227.resource_water_coef_mapping %>%
      left_join_error_no_match(resource_water_data, by = c("fuel", "subsector", "technology")) %>%
      select(fuel, supplysector, water.coefficient.m3.per.TJ) %>%
      arrange(fuel) %>%
      select(-fuel) %>%
      unique() -> L110.global_water_cons_coef

    # Create tibble for all regions and supply sectors
    GCAM_region_names %>%
      repeat_add_columns(L110.global_water_cons_coef) %>%
      select(-region) %>%
      arrange(GCAM_region_ID) -> L110.water_coef_region_supplysector

    # Get water consumption and withdrawal ratios 32 GCAM regions...
    # ... all regions other than Middle East set to US values
    # Set the middle east region to the one containing Saudi Arabia
    MiddleEastRegID <- iso_GCAM_regID$GCAM_region_ID[iso_GCAM_regID$iso == "sau"]
    MiddleEastRegion <- GCAM_region_names$region[GCAM_region_names$GCAM_region_ID == MiddleEastRegID]
    GCAM_region_names %>%
      mutate(rsrc_region = if_else(region == MiddleEastRegion, "Middle East", "USA")) %>%
      left_join_error_no_match(resource_water_share, by = c(rsrc_region = "region_GCAM3")) %>%
      select(-rsrc_region, -region) -> L110.resource_water_share
    names(L110.resource_water_share) <- c("GCAM_region_ID", "sal", "fresh", "cons_fr", "cons_tot")

    # Bring consumption ratios and water usage coefficents into single table
    L110.resource_water_share %>%
      left_join(L110.water_coef_region_supplysector, by = "GCAM_region_ID") -> L110.water_ratios_coef

    # Calculate water withdrawal, seawater, and consumption, then bind to single tibble
    L110.water_ratios_coef %>%
      mutate(water_type = "water withdrawals",
             coefficient_WC = fresh * water.coefficient.m3.per.TJ * 1e-3,
             coefficient = coefficient_WC / cons_fr) %>%
      select(-coefficient_WC) -> L110.water_withdrawals
    L110.water_ratios_coef %>%
      mutate(water_type = "seawater",
             coefficient = sal * water.coefficient.m3.per.TJ * 1e-3) -> L110.seawater
    L110.water_ratios_coef %>%
      mutate(water_type = "water consumption",
             coefficient = fresh * water.coefficient.m3.per.TJ * 1e-3) %>%
      bind_rows(L110.water_withdrawals, L110.seawater) %>%
      select(GCAM_region_ID, supplysector, water_type, coefficient) ->
      L110.water_demand_primary

    # Multiply the coefficients by the energy flow volumes of these different sectors
    # Note that the energy flow volumes are determined from processing energy balance data;
    # data on each fuel comes from a different place
    L110.energy_flow_coal <- subset(L1012.en_bal_EJ_R_Si_Fi_Yh, sector == "TPES" & fuel == "coal") %>%
      mutate(supplysector = "regional coal") %>%
      select(GCAM_region_ID, supplysector, year, value)
    L110.energy_flow_oil <- L121.in_EJ_R_TPES_crude_Yh %>%
      mutate(supplysector = "regional oil") %>%
      select(GCAM_region_ID, supplysector, year, value)
    L110.energy_flow_unoilprod <- subset(L121.in_EJ_R_TPES_unoil_Yh, fuel == "unconventional oil") %>%
      mutate(supplysector = "regional oil") %>%
      select(GCAM_region_ID, supplysector, year, value)
    L110.energy_flow_gas <- subset(L122.in_EJ_R_gasproc_F_Yh, fuel == "gas") %>%
      mutate(supplysector = "regional natural gas") %>%
      select(GCAM_region_ID, supplysector, year, value)

    # Nuclear fuel flow volumes are estimated as the elec output divided by the exogenous efficiency
    # While the IEA energy balances do report "input" fuel, estimated in similar fashion to GCAM, the specific
    # conversion assumptions are slightly different, and in any case the IEA's number is not used in GCAM
    elec_nuc_efficiency <- filter(A23.globaltech_eff, subsector == "nuclear") %>%
      gather_years(value_col = "efficiency") %>%
      select(supplysector = minicam.energy.input, year, efficiency) %>%
      complete(nesting(supplysector), year = HISTORICAL_YEARS) %>%
      mutate(efficiency = approx_fun(year, efficiency)) %>%
      filter(year %in% HISTORICAL_YEARS)
    L110.energy_flow_nuc <- subset(L1012.en_bal_EJ_R_Si_Fi_Yh, fuel == "elec_nuclear" & sector == "out_electricity generation") %>%
      mutate(sector = "electricity generation", fuel = "nuclear") %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, supplysector = minicam.energy.input),
                               by = c("sector", "fuel")) %>%
      left_join_error_no_match(elec_nuc_efficiency, by = c("supplysector", "year")) %>%
      mutate(value = value / efficiency) %>%
      select(GCAM_region_ID, supplysector, year, value)

    L110.water_demand_primary_fresh <- subset(L110.water_demand_primary, water_type %in% water.MAPPED_WATER_TYPES)

    L110.in_km3_water_primary <- bind_rows(L110.energy_flow_coal, L110.energy_flow_oil, L110.energy_flow_unoilprod,
                                           L110.energy_flow_gas, L110.energy_flow_nuc) %>%
      left_join(L110.water_demand_primary_fresh, by = c("GCAM_region_ID", "supplysector")) %>%
      mutate(value = value * coefficient) %>%
      select(GCAM_region_ID, supplysector, water_type, year, value)

    # Downscale water demands for primary energy production to the basin
    L110.in_km3_water_primary_basin <- L110.in_km3_water_primary %>%
      left_join(filter(L103.water_mapping_R_B_W_Ws_share, water_sector == "Mining"),
                by = c("GCAM_region_ID", "water_type")) %>%
      mutate(value = value * share) %>%
      select(GCAM_region_ID, GCAM_basin_ID, supplysector, water_type, year, value)

    L110.water_demand_primary %>%
      add_title("Primary energy water coefficients by region ID / supply sector / water type") %>%
      add_units("m^3 / GJ") %>%
      add_comments("Primary energy water use coefficients calculated using US data for fractions of saline and freshwater share") %>%
      add_legacy_name("L110.water_demand_primary_R_S_W_m3_GJ") %>%
      add_precursors("common/GCAM_region_names",
                     "common/iso_GCAM_regID",
                     "water/A227.resource_water_coef_mapping",
                     "water/resource_water_data",
                     "water/resource_water_share") ->
      L110.water_demand_primary_R_S_W_m3_GJ

    L110.in_km3_water_primary %>%
      add_title("Primary energy water demands by GCAM_region_ID / fuel / water_type / year") %>%
      add_units("km^3") %>%
      add_comments("Estimated as the energy flow volumes multiplied by freshwater demand coefficients (withdrawals, consumption)") %>%
      same_precursors_as(L110.water_demand_primary_R_S_W_m3_GJ) %>%
      add_precursors("energy/calibrated_techs",
                     "energy/A23.globaltech_eff",
                     "L1012.en_bal_EJ_R_Si_Fi_Yh",
                     "L121.in_EJ_R_TPES_unoil_Yh",
                     "L121.in_EJ_R_TPES_crude_Yh",
                     "L122.in_EJ_R_gasproc_F_Yh") ->
      L110.in_km3_water_primary

    L110.in_km3_water_primary_basin %>%
      add_title("Primary energy water demands by GCAM_region_ID / basin / fuel / water_type / year") %>%
      add_units("km^3") %>%
      add_comments("Estimated as regional water flow volumes multiplied by basin-specific shares") %>%
      same_precursors_as(L110.in_km3_water_primary) %>%
      add_precursors("L103.water_mapping_R_B_W_Ws_share") ->
      L110.in_km3_water_primary_basin

    return_data(L110.water_demand_primary_R_S_W_m3_GJ, L110.in_km3_water_primary, L110.in_km3_water_primary_basin)

  } else {
    stop("Unknown command")
  }
}
