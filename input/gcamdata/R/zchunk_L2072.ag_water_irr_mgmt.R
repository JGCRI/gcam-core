# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L2072.ag_water_irr_mgmt
#'
#' Calculate agriculture water Input-Output coefficients by region / crop / year / GLU / technology.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L2071.AgCoef_IrrBphysWater_ag_mgmt},
#'   \code{L2072.AgCoef_IrrWaterWdraw_ag_mgmt}, \code{L2072.AgCoef_IrrWaterCons_ag_mgmt},
#'   \code{L2072.AgCoef_RfdBphysWater_ag_mgmt}, \code{L2072.AgCoef_BphysWater_bio_mgmt},
#'   \code{L2072.AgCoef_IrrWaterWdraw_bio_mgmt}, \code{L2072.AgCoef_IrrWaterCons_bio_mgmt},
#'   \code{L2072.AgNonEnergyCost_IrrWaterWdraw}. The corresponding file in the original data system was
#'   \code{L2072.ag_water_irr_mgmt.R} (aglu level2).
#' @details This chunk calculates the Input-Output coefficients of irrigation water withdrawals and consumption, and
#'   biophysical water consumption of irrigated and rainfed crops, for each primary and dedicated bioenergy crop by
#'   region / year / GLU / management level.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter full_join group_by if_else left_join mutate right_join select semi_join summarise
#' @importFrom tidyr complete nesting replace_na separate
#' @importFrom tibble tibble
#' @author RC July 2017
module_aglu_L2072.ag_water_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             "L2012.AgSupplySector",
             "L161.ag_irrProd_Mt_R_C_Y_GLU",
             "L161.ag_rfdProd_Mt_R_C_Y_GLU",
             "L165.BlueIrr_m3kg_R_C_GLU",
             "L165.TotIrr_m3kg_R_C_GLU",
             "L165.GreenRfd_m3kg_R_C_GLU",
             "L165.ag_IrrEff_R",
             "L172.Coef_GJm3_IrrEnergy_R",
             "L2052.AgCost_ag_irr_mgmt",
             "L2052.AgCost_bio_irr_mgmt",
             FILE = "water/water_td_sectors"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2072.AgCoef_IrrBphysWater_ag_mgmt",
             "L2072.AgCoef_IrrWaterWdraw_ag_mgmt",
             "L2072.AgCoef_IrrWaterCons_ag_mgmt",
             "L2072.AgCoef_RfdBphysWater_ag_mgmt",
             "L2072.AgCoef_BphysWater_bio_mgmt",
             "L2072.AgCoef_IrrWaterWdraw_bio_mgmt",
             "L2072.AgCoef_IrrWaterCons_bio_mgmt",
             "L2072.AgNonEnergyCost_IrrWaterWdraw"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    BlueIrr_m3kg <- BlueIrr_km3 <- TotIrr_m3kg <- TotIrr_km3 <- GreenRfd_m3kg <- water_sector <- water_type <-
      Prod_Mt <- AgSupplySector <- AgSupplySubsector <- AgProductionTechnology <- minicam.energy.input <-
      field.eff <- blue_fract <- conveyance.eff <- WaterPrice <- calPrice <- WaterCost <- nonLandVariableCost <-
      biomass <- type <- region <- GCAM_region_ID <- year <- value <- GCAM_commodity <- GLU <- GLU_name <-
      IRR_RFD <- MGMT <- coefficient <- Profit <- fuel <- elec_GJm3 <- input.cost <- minicam.non.energy.input <-
      . <- GCAM_subsector <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    L2012.AgSupplySector <- get_data(all_data, "L2012.AgSupplySector")
    L161.ag_irrProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_irrProd_Mt_R_C_Y_GLU")
    L161.ag_rfdProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_rfdProd_Mt_R_C_Y_GLU")
    L165.BlueIrr_m3kg_R_C_GLU <- get_data(all_data, "L165.BlueIrr_m3kg_R_C_GLU", strip_attributes = TRUE)
    L165.TotIrr_m3kg_R_C_GLU <- get_data(all_data, "L165.TotIrr_m3kg_R_C_GLU", strip_attributes = TRUE)
    L165.GreenRfd_m3kg_R_C_GLU <- get_data(all_data, "L165.GreenRfd_m3kg_R_C_GLU", strip_attributes = TRUE)
    L165.ag_IrrEff_R <- get_data(all_data, "L165.ag_IrrEff_R", strip_attributes = TRUE)
    L172.Coef_GJm3_IrrEnergy_R <- get_data(all_data, "L172.Coef_GJm3_IrrEnergy_R", strip_attributes = TRUE)
    L2052.AgCost_ag_irr_mgmt <- get_data(all_data, "L2052.AgCost_ag_irr_mgmt", strip_attributes = TRUE)
    L2052.AgCost_bio_irr_mgmt <- get_data(all_data, "L2052.AgCost_bio_irr_mgmt", strip_attributes = TRUE)
    water_td_sectors <- get_data(all_data, "water/water_td_sectors")

    # Primary crops
    # Compute irrigation water consumption IO coefficients (km3/Mt crop = m3/kg) by region / irrigated crop / year / GLU / management level
    L165.BlueIrr_m3kg_R_C_GLU %>%
      # Initial adjustment: drop any water coefs for region/crop/GLU combinations that aren't written out
      semi_join(L161.ag_irrProd_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      mutate(IRR_RFD = "IRR", water_type = "water consumption") %>%
      rename(value = BlueIrr_m3kg) ->
      L2072.Blue_IRR_IO_R_C_GLU

    # Compute biophysical water consumption IO coefficients (km3/Mt crop = m3/kg) by region / irrigated crop / year / GLU / management level
    L165.TotIrr_m3kg_R_C_GLU %>%
      # Initial adjustment: drop any water coefs for region/crop/GLU combinations that aren't written out
      semi_join(L161.ag_irrProd_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      mutate(IRR_RFD = "IRR", water_type = "biophysical water consumption") %>%
      rename(value = TotIrr_m3kg) ->
      L2072.Bio_IRR_IO_R_C_GLU

    # Compute biophysical water consumption IO coefficients (km3/Mt crop = m3/kg) by region / rainfed crop / year / GLU / management level
    L165.GreenRfd_m3kg_R_C_GLU %>%
      # Initial adjustment: drop any water coefs for region/crop/GLU combinations that aren't written out
      semi_join(L161.ag_rfdProd_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      mutate(IRR_RFD = "RFD", water_type = "biophysical water consumption") %>%
      rename(value = GreenRfd_m3kg) ->
      L2072.Bio_RFD_IO_R_C_GLU

    # Following are repeated processing steps for the three files, so combine them all together
    L2072.Blue_IRR_IO_R_C_GLU %>%
      bind_rows(L2072.Bio_IRR_IO_R_C_GLU, L2072.Bio_RFD_IO_R_C_GLU) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      # Copy to both high and low management levels
      repeat_add_columns(tibble(MGMT = c("hi", "lo"))) %>%
      # Add sector, subsector, technology names
      mutate(AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_subsector, GLU_name, sep = aglu.CROP_GLU_DELIMITER),
             AgProductionTechnology = paste(paste(AgSupplySubsector, IRR_RFD, sep = aglu.IRR_DELIMITER),
                                            MGMT, sep = aglu.MGMT_DELIMITER),
             coefficient = round(value, aglu.DIGITS_CALOUTPUT)) %>%
      filter(coefficient > 0) %>%
      # Assume coefs stay constant, copy to all model years
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      L2072.AgCoef_Water_ag_mgmt

    # Separate irrigation water consumption IO coefficients
    L2072.AgCoef_Water_ag_mgmt %>%
      filter(water_type == "water consumption") %>%
      # Standardize irrigation water input names
      mutate(water_sector = "Irrigation",
             minicam.energy.input = set_water_input_name(water_sector, water_type, water_td_sectors, GLU = GLU_name)) %>%
      select(LEVEL2_DATA_NAMES[["AgCoef"]]) ->
      L2072.AgCoef_IrrWaterCons_ag_mgmt

    # Separate biophysical water consumption IO coefficients
    L2072.AgCoef_Water_ag_mgmt %>%
      filter(water_type == "biophysical water consumption" & IRR_RFD == "IRR") %>%
      # Standardize irrigation water input names
      mutate(water_sector = "Irrigation",
             minicam.energy.input = set_water_input_name(water_sector, water_type, water_td_sectors, GLU = GLU_name)) %>%
      select(LEVEL2_DATA_NAMES[["AgCoef"]]) ->
      L2072.AgCoef_IrrBphysWater_ag_mgmt

    # Separate biophysical water consumption IO coefficients
    L2072.AgCoef_Water_ag_mgmt %>%
      filter(water_type == "biophysical water consumption" & IRR_RFD == "RFD") %>%
      # Standardize irrigation water input names
      mutate(water_sector = "Irrigation",
             minicam.energy.input = set_water_input_name(water_sector, water_type, water_td_sectors, GLU = GLU_name)) %>%
      select(LEVEL2_DATA_NAMES[["AgCoef"]]) ->
      L2072.AgCoef_RfdBphysWater_ag_mgmt

    # Dedicated bioenergy crops
    # Compute biophysical water IO coefficients (km3/EJ biomass) by region / dedicated bioenergy crop / year / GLU / management level
    L2052.AgCost_bio_irr_mgmt %>%
      select(LEVEL2_DATA_NAMES[["AgTechYr"]]) %>%
      mutate(minicam.energy.input = "biophysical water consumption",
             coefficient = aglu.BIO_GRASS_WATER_IO_KM3EJ,
             coefficient = replace(coefficient, grepl("biomassTree", AgProductionTechnology), aglu.BIO_TREE_WATER_IO_KM3EJ)) ->
      L2072.AgCoef_BphysWater_bio_mgmt

    # Compute irrigation water consumption IO coefficients (km3/EJ biomass) by region / dedicated bioenergy crop / year / GLU / management level
    # First compute % of blue water for irrigated bioenergy crops
    # Match in green and blue water for existing crops in 2005 -- note for this we are only using blue & green from irrigated crops
    L165.BlueIrr_m3kg_R_C_GLU %>%
      full_join(L165.TotIrr_m3kg_R_C_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      # Initial adjustment: drop any water coefs for region/crop/GLU combinations that aren't written out
      semi_join(L161.ag_irrProd_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      # Match in 2010 irrigated crop production
      left_join_error_no_match(filter(L161.ag_irrProd_Mt_R_C_Y_GLU, year == max(HISTORICAL_YEARS)),
                               by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      # Use crop prodcution for weighting
      mutate(BlueIrr_km3 = BlueIrr_m3kg * value, TotIrr_km3 = TotIrr_m3kg * value) %>%
      group_by(GCAM_region_ID, GLU) %>%
      summarise(Prod_Mt = sum(value), BlueIrr_km3 = sum(BlueIrr_km3), TotIrr_km3 = sum(TotIrr_km3)) %>%
      ungroup() %>%
      # Calculate production-weighted blue and green water of all irrigated crops by region / GLU
      mutate(BlueIrr_m3kg = BlueIrr_km3 / Prod_Mt, TotIrr_m3kg = TotIrr_km3  / Prod_Mt,
             # Calculate % of blue water use
             blue_fract = BlueIrr_m3kg / TotIrr_m3kg) %>%
      replace_na(list(blue_fract = 0)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) ->
      L2072.BlueFract_R_GLU

    # Use biophysical water consumption IO coefs to compute irrigation water consumption IO coefs for dedicated bioenergy crops
    L2072.AgCoef_BphysWater_bio_mgmt %>%
      mutate(water_type = "water consumption") %>%
      # Separate subsector variale for GLU names
      separate(AgSupplySubsector, c("biomass", "GLU_name"), sep = "_") %>%
      # Match in % of blue water by region / GLU, create NAs, use left_join instead
      left_join(select(L2072.BlueFract_R_GLU, region, GLU_name, blue_fract), by = c("region", "GLU_name")) %>%
      # Multiply biophysical water consumption IO coefs and blue water %
      mutate(coefficient = round(coefficient * blue_fract, aglu.DIGITS_CALOUTPUT)) %>%
      replace_na(list(coefficient = 0)) %>%
      # Irrigated water consumption only applies to the irrigated techs, which are assumed to end in the string "IRR"
      filter(grepl("IRR", AgProductionTechnology)) %>%
      mutate(AgSupplySubsector = paste(biomass, GLU_name, sep = "_")) %>%
      # Standardize irrigation water input names
      mutate(water_sector = "Irrigation",
             minicam.energy.input = set_water_input_name(water_sector, water_type, water_td_sectors, GLU = GLU_name)) %>%
      # Remove extra columns
      select(LEVEL2_DATA_NAMES[["AgCoef"]]) ->
      L2072.AgCoef_IrrWaterCons_bio_mgmt

    # Create tables for water withdrawals IO coefs (consumption divided by irrigation efficiency)
    L165.ag_IrrEff_R %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      right_join(bind_rows(L2072.AgCoef_IrrWaterCons_ag_mgmt, L2072.AgCoef_IrrWaterCons_bio_mgmt), by = "region") %>%
      # Calculate water withdrawals using consumption divided by irrigation efficiency
      mutate(coefficient = round(coefficient / field.eff, aglu.DIGITS_CALOUTPUT),
             # Replace the last character "C" with "W" for input name
             minicam.energy.input = gsub(".{1}$", "W", minicam.energy.input)) %>%
      # Remove extra columns
      select(LEVEL2_DATA_NAMES[["AgCoef"]]) ->
      L2072.AgCoef_IrrWaterWdraw_mgmt

    # Separate table of water withdrawals IO coefs into 2: one for bioenergy crops, another for non-bio crops
    L2072.AgCoef_IrrWaterWdraw_mgmt %>%
      filter(AgSupplySector != "biomass") ->
      L2072.AgCoef_IrrWaterWdraw_ag_mgmt

    L2072.AgCoef_IrrWaterWdraw_mgmt %>%
      filter(AgSupplySector == "biomass") ->
      L2072.AgCoef_IrrWaterWdraw_bio_mgmt

    # Ad hoc assignment of subsidies to observations with negative profit. In GCAM, the land profit rate is calculated
    # as price minus cost times yield, so if the cost exceeds the price, then the profit goes negative. By adding in the
    # water cost without modifying the non-land variable costs, we risk having costs that exceed commodity prices. This
    # calculation checks whether any of our costs exceed the prices, and uses this information to determine a "water
    # subsidy", applied as a negative cost to the price paid for irrigation water.

    # NOTE: The electricity price used for this purpose is approximate, not calculated from each region's
    # fuel mix and primary energy prices; as such, the resulting profit rates are not exact.
    L2072.Coef_GJm3_IrrEnergy_R <- rename(L172.Coef_GJm3_IrrEnergy_R, elec_GJm3 = coefficient) %>%
      filter(fuel == "electricity",
             year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, year, elec_GJm3) %>%
      complete(region = region, year = MODEL_YEARS) %>%
      group_by(region) %>%
      mutate(elec_GJm3 = if_else(year %in% MODEL_FUTURE_YEARS, elec_GJm3[year == max(MODEL_BASE_YEARS)], elec_GJm3)) %>%
      ungroup()

    L165.ag_IrrEff_R %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      right_join(L2072.AgCoef_IrrWaterWdraw_ag_mgmt, by = "region") %>%
      # Calculate water price and water cost, including abstraction-related energy costs
      left_join_error_no_match(L2072.Coef_GJm3_IrrEnergy_R,
                               by = c("region", "year")) %>%
      # Calculate water price and water cost
      mutate(WaterCost = coefficient * (water.DEFAULT_IRR_WATER_PRICE +
                                          (elec_GJm3 * efw.DEFAULT_IRR_ELEC_PRICE_75USDGJ * water.IRR_PRICE_SUBSIDY_MULT)) /
               conveyance.eff) %>%
      # Join in non-land variable costs
      left_join_error_no_match(L2052.AgCost_ag_irr_mgmt,
                               by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year")) %>%
      # Join in commodity price
      left_join(select(L2012.AgSupplySector, region, AgSupplySector, calPrice),
                by = c("region", "AgSupplySector")) %>%
      mutate(Profit = calPrice - WaterCost - nonLandVariableCost) ->
      L2072.AgNonEnergyCost_IrrWaterWdraw

    # Assume an exogenous floor on profit rates to prevent negative, zero, and very low profit rates
    # Set a floor on profit rates, equal to the minimum observed profit rate not considering water costs
    minProfitMargin <- min(with(L2072.AgNonEnergyCost_IrrWaterWdraw, calPrice - nonLandVariableCost))

    # Note: there is no need to write out zero subsidy values in region/supplysector/subsector/technology observations
    # where all years are 0. The following steps ensure that there aren't observations with subsidies in some years but
    # not others.
    L2072.AgNonEnergyCost_IrrWaterWdraw %>%
      mutate(minicam.non.energy.input = "water subsidy",
             # Round to the same number of digits as cal-output rather than cal-price; many are close to zero
             input.cost = if_else(Profit < minProfitMargin, round(Profit - minProfitMargin, aglu.DIGITS_CALOUTPUT), 0)) %>%
      filter(input.cost < 0) %>%
      complete(nesting(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, minicam.non.energy.input), year = MODEL_YEARS) %>%
      replace_na(list(input.cost = 0)) %>%
      select(LEVEL2_DATA_NAMES[["AgNonEnergyCost"]]) ->
      L2072.AgNonEnergyCost_IrrWaterWdraw

    # Produce outputs
    L2072.AgCoef_IrrWaterCons_ag_mgmt %>%
      add_title("Irrigation water consumption IO coefficients by region / irrigated crop / year / GLU / management level") %>%
      add_units("km3/Mt") %>%
      add_comments("The same IO coefficients are assigned to both high and low management and keep constant for all model years") %>%
      add_legacy_name("L2072.AgCoef_IrrWaterCons_ag_mgmt") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L165.BlueIrr_m3kg_R_C_GLU",
                     "L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "water/water_td_sectors") ->
      L2072.AgCoef_IrrWaterCons_ag_mgmt

    L2072.AgCoef_IrrWaterWdraw_ag_mgmt %>%
      add_title("Irrigation water withdrawals IO coefficients by region / irrigated crop / year / GLU / management level") %>%
      add_units("km3/Mt") %>%
      add_comments("Withdrawals coefs are calculated as consumption coefs divided by irrigation efficiency") %>%
      add_comments("Set a floor on profit and adjust the coefs to ensure the profit floor is met") %>%
      add_comments("The same IO coefficients are assigned to both high and low management and keep constant for all model years") %>%
      add_legacy_name("L2072.AgCoef_IrrWaterWdraw_ag_mgmt") %>%
      same_precursors_as("L2072.AgCoef_IrrWaterCons_ag_mgmt") %>%
      add_precursors("L165.ag_IrrEff_R") ->
      L2072.AgCoef_IrrWaterWdraw_ag_mgmt

    L2072.AgCoef_IrrBphysWater_ag_mgmt %>%
      add_title("Biophysical water consumption IO coefficients by region / irrigated crop / year / GLU / management level") %>%
      add_units("km3/Mt") %>%
      add_comments("The same IO coefficients are assigned to both high and low management and keep constant for all model years") %>%
      add_legacy_name("L2072.AgCoef_IrrBphysWater_ag_mgmt") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L165.TotIrr_m3kg_R_C_GLU",
                     "L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "water/water_td_sectors") ->
      L2072.AgCoef_IrrBphysWater_ag_mgmt

    L2072.AgCoef_RfdBphysWater_ag_mgmt %>%
      add_title("Biophysical water consumption IO coefficients by region / rainfed crop / year / GLU / management level") %>%
      add_units("km3/Mt") %>%
      add_comments("The same IO coefficients are assigned to both high and low management and keep constant for all model years") %>%
      add_legacy_name("L2072.AgCoef_RfdBphysWater_ag_mgmt") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L165.GreenRfd_m3kg_R_C_GLU",
                     "L161.ag_rfdProd_Mt_R_C_Y_GLU",
                     "water/water_td_sectors") ->
      L2072.AgCoef_RfdBphysWater_ag_mgmt

    L2072.AgCoef_BphysWater_bio_mgmt %>%
      add_title("Biophysical water IO coefficients by region / dedicated bioenergy crop / year / GLU / management level") %>%
      add_units("km3/EJ") %>%
      add_comments("The same IO coefficients are assigned to both high and low management and keep constant for all model years") %>%
      add_legacy_name("L2072.AgCoef_BphysWater_bio_mgmt") %>%
      add_precursors("L2052.AgCost_bio_irr_mgmt",
                     "water/water_td_sectors") ->
      L2072.AgCoef_BphysWater_bio_mgmt

    L2072.AgCoef_IrrWaterCons_bio_mgmt %>%
      add_title("Irrigation water consumption IO coefficients by region / dedicated bioenergy crop / year / GLU / management level") %>%
      add_units("km3/EJ") %>%
      add_comments("Multiply biophysical water coefs of bioenergy crops with the average % of blue water for primary crops by region / GLU") %>%
      add_comments("The same IO coefficients are assigned to both high and low management and keep constant for all model years") %>%
      add_legacy_name("L2072.AgCoef_IrrWaterCons_bio_mgmt") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L165.BlueIrr_m3kg_R_C_GLU",
                     "L165.TotIrr_m3kg_R_C_GLU",
                     "L161.ag_irrProd_Mt_R_C_Y_GLU",
                     "water/water_td_sectors") ->
      L2072.AgCoef_IrrWaterCons_bio_mgmt

    L2072.AgCoef_IrrWaterWdraw_bio_mgmt %>%
      add_title("Irrigation water withdrawals IO coefficients by region / dedicated bioenergy crop / year / GLU / management level") %>%
      add_units("km3/EJ") %>%
      add_comments("Withdrawals coefs are calculated as consumption coefs divided by irrigation efficiency") %>%
      add_comments("The same IO coefficients are assigned to both high and low management and keep constant for all model years") %>%
      add_legacy_name("L2072.AgCoef_IrrWaterWdraw_bio_mgmt") %>%
      same_precursors_as("L2072.AgCoef_IrrWaterCons_bio_mgmt") %>%
      add_precursors("L165.ag_IrrEff_R") ->
      L2072.AgCoef_IrrWaterWdraw_bio_mgmt

    L2072.AgNonEnergyCost_IrrWaterWdraw %>%
      add_title("Irrigation water subsidies by region / crop / year / GLU / management level") %>%
      add_units("1975$/kg") %>%
      add_comments("Water subsidies are calculated to keep profit rates of irrigated technologies above a minimum level") %>%
      add_comments("While implemented using <input-cost>, all values are negative so these reduce net costs") %>%
      same_precursors_as("L2072.AgCoef_IrrWaterWdraw_ag_mgmt") %>%
      add_precursors("L172.Coef_GJm3_IrrEnergy_R",
                     "L2012.AgSupplySector",
                     "L2052.AgCost_ag_irr_mgmt") ->
      L2072.AgNonEnergyCost_IrrWaterWdraw

    return_data(L2072.AgCoef_IrrBphysWater_ag_mgmt,
                L2072.AgCoef_IrrWaterWdraw_ag_mgmt,
                L2072.AgCoef_IrrWaterCons_ag_mgmt,
                L2072.AgCoef_RfdBphysWater_ag_mgmt,
                L2072.AgCoef_BphysWater_bio_mgmt,
                L2072.AgCoef_IrrWaterWdraw_bio_mgmt,
                L2072.AgCoef_IrrWaterCons_bio_mgmt,
                L2072.AgNonEnergyCost_IrrWaterWdraw)
  } else {
    stop("Unknown command")
  }
}
