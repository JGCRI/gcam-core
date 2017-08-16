#' module_aglu_L2072.ag_water_irr_mgmt
#'
#' Calculate agriculture water Input-Output coefficients by region / crop / year / GLU / technology.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2071.AgCoef_IrrBphysWater_ag_mgmt}, \code{L2072.AgCoef_IrrWaterWdraw_ag_mgmt}, \code{L2072.AgCoef_IrrWaterCons_ag_mgmt}, \code{L2072.AgCoef_RfdBphysWater_ag_mgmt}, \code{L2072.AgCoef_BphysWater_bio_mgmt}, \code{L2072.AgCoef_IrrWaterWdraw_bio_mgmt}, \code{L2072.AgCoef_IrrWaterCons_bio_mgmt}. The corresponding file in the
#' original data system was \code{L2072.ag_water_irr_mgmt.R} (aglu level2).
#' @details This chunk calculates the Input-Output coefficients of irrigation water withdrawals and consumption, and biophysical water consumption of irrigated and rainfed crops,
#' for each primary and dedicated bioenergy crop by region / year / GLU / management level.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RC July 2017
module_aglu_L2072.ag_water_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             "L132.ag_an_For_Prices",
             "L161.ag_irrProd_Mt_R_C_Y_GLU",
             "L161.ag_rfdProd_Mt_R_C_Y_GLU",
             "L165.BlueIrr_m3kg_R_C_GLU",
             "L165.TotIrr_m3kg_R_C_GLU",
             "L165.GreenRfd_m3kg_R_C_GLU",
             "L165.ag_IrrEff_R",
             "L2052.AgCost_ag_irr_mgmt",
             "L2052.AgCost_bio_irr_mgmt",
             FILE = "water/A03.sector"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2072.AgCoef_IrrBphysWater_ag_mgmt",
             "L2072.AgCoef_IrrWaterWdraw_ag_mgmt",
             "L2072.AgCoef_IrrWaterCons_ag_mgmt",
             "L2072.AgCoef_RfdBphysWater_ag_mgmt",
             "L2072.AgCoef_BphysWater_bio_mgmt",
             "L2072.AgCoef_IrrWaterWdraw_bio_mgmt",
             "L2072.AgCoef_IrrWaterCons_bio_mgmt"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    BlueIrr_m3kg <- BlueIrr_km3 <- TotIrr_m3kg <- TotIrr_km3 <- GreenRfd_m3kg <- water_sector <- water_type <-
      Prod_Mt <- AgSupplySector <- AgSupplySubsector <- AgProductionTechnology <- minicam.energy.input <-
      field.eff <- blue_fract <- conveyance.eff <- WaterPrice <- calPrice <- WaterCost <- nonLandVariableCost <-
      biomass <- type <- region <- GCAM_region_ID <- year <- value <- GCAM_commodity <- GLU <- GLU_name <-
      IRR_RFD <- MGMT <- coefficient <- . <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    L132.ag_an_For_Prices <- get_data(all_data, "L132.ag_an_For_Prices")
    L161.ag_irrProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_irrProd_Mt_R_C_Y_GLU")
    L161.ag_rfdProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_rfdProd_Mt_R_C_Y_GLU")
    L165.BlueIrr_m3kg_R_C_GLU <- get_data(all_data, "L165.BlueIrr_m3kg_R_C_GLU")
    L165.TotIrr_m3kg_R_C_GLU <- get_data(all_data, "L165.TotIrr_m3kg_R_C_GLU")
    L165.GreenRfd_m3kg_R_C_GLU <- get_data(all_data, "L165.GreenRfd_m3kg_R_C_GLU")
    L165.ag_IrrEff_R <- get_data(all_data, "L165.ag_IrrEff_R")
    L2052.AgCost_ag_irr_mgmt <- get_data(all_data, "L2052.AgCost_ag_irr_mgmt")
    L2052.AgCost_bio_irr_mgmt <- get_data(all_data, "L2052.AgCost_bio_irr_mgmt")
    A03.sector <- get_data(all_data, "water/A03.sector")

    # Primary crops
    # Compute irrigation water consumption IO coefficients (km3/Mt crop = m3/kg) by region / irrigated crop / year / GLU / management level
    L165.BlueIrr_m3kg_R_C_GLU %>%
      # Initial adjustment: drop any water coefs for region/crop/GLU combinations that aren't written out
      semi_join(L161.ag_irrProd_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      mutate(IRR_RFD = "IRR", water_type = "water consumption") %>%
      rename(value = BlueIrr_m3kg) ->
      L2072.Blue_IRR_IO_R_C_GLU

    # Compute biophysical water consumption IO coefficients (km3/Mt crop = m3/kg) by region / irrigated crop / year / GLU / management level
    L165.TotIrr_m3kg_R_C_GLU %>%
      # Initial adjustment: drop any water coefs for region/crop/GLU combinations that aren't written out
      semi_join(L161.ag_irrProd_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      mutate(IRR_RFD = "IRR", water_type = "biophysical water consumption") %>%
      rename(value = TotIrr_m3kg) ->
      L2072.Bio_IRR_IO_R_C_GLU

    # Compute biophysical water consumption IO coefficients (km3/Mt crop = m3/kg) by region / rainfed crop / year / GLU / management level
    L165.GreenRfd_m3kg_R_C_GLU %>%
      # Initial adjustment: drop any water coefs for region/crop/GLU combinations that aren't written out
      semi_join(L161.ag_rfdProd_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      mutate(IRR_RFD = "RFD", water_type = "biophysical water consumption") %>%
      rename(value = GreenRfd_m3kg) ->
      L2072.Bio_RFD_IO_R_C_GLU

    # Following are repeated processing steps for the three files, so combine them all togather
    L2072.Blue_IRR_IO_R_C_GLU %>%
      bind_rows(L2072.Bio_IRR_IO_R_C_GLU, L2072.Bio_RFD_IO_R_C_GLU) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      # Copy to both high and low management levels
      repeat_add_columns(tibble::tibble(MGMT = c("hi", "lo"))) %>%
      # Add sector, subsector, technology names
      mutate(AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_commodity, GLU_name, sep = "_"),
             AgProductionTechnology = paste(GCAM_commodity, GLU_name, IRR_RFD, MGMT, sep = "_"),
             coefficient = round(value, aglu.DIGITS_CALOUTPUT)) %>%
      filter(coefficient > 0) %>%
      # Assume coefs stay constant, copy to all model years
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) ->
      L2072.AgCoef_Water_ag_mgmt

    # Separate irrigation water consumption IO coefficients
    L2072.AgCoef_Water_ag_mgmt %>%
      filter(water_type == "water consumption") %>%
      # Standardize irrigation water input names
      mutate(water_sector = "Irrigation",
             minicam.energy.input = set_water_input_name(water_sector, water_type, A03.sector, GLU = GLU_name)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["AgCoef"]])) ->
      L2072.AgCoef_IrrWaterCons_ag_mgmt

    # Separate biophysical water consumption IO coefficients
    L2072.AgCoef_Water_ag_mgmt %>%
      filter(water_type == "biophysical water consumption" & IRR_RFD == "IRR") %>%
      # Standardize irrigation water input names
      mutate(water_sector = "Irrigation",
             minicam.energy.input = set_water_input_name(water_sector, water_type, A03.sector, GLU = GLU_name)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["AgCoef"]])) ->
      L2072.AgCoef_IrrBphysWater_ag_mgmt

    # Separate biophysical water consumption IO coefficients
    L2072.AgCoef_Water_ag_mgmt %>%
      filter(water_type == "biophysical water consumption" & IRR_RFD == "RFD") %>%
      # Standardize irrigation water input names
      mutate(water_sector = "Irrigation",
             minicam.energy.input = set_water_input_name(water_sector, water_type, A03.sector, GLU = GLU_name)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["AgCoef"]])) ->
      L2072.AgCoef_RfdBphysWater_ag_mgmt

    # Dedicated bioenergy crops
    # Compute biophysical water IO coefficients (km3/EJ biomass) by region / dedicated bioenergy crop / year / GLU / management level
    L2052.AgCost_bio_irr_mgmt %>%
      select(one_of(LEVEL2_DATA_NAMES[["AgTechYr"]])) %>%
      mutate(minicam.energy.input = "biophysical water consumption",
             coefficient = aglu.BIO_GRASS_WATER_IO_KM3EJ,
             coefficient = replace(coefficient, grepl("biomass_tree", AgProductionTechnology), aglu.BIO_TREE_WATER_IO_KM3EJ)) ->
      L2072.AgCoef_BphysWater_bio_mgmt

    # Compute irrigation water consumption IO coefficients (km3/EJ biomass) by region / dedicated bioenergy crop / year / GLU / management level
    # First compute % of blue water for irrigated bioenergy crops
    # Match in green and blue water for existing crops in 2005 -- note for this we are only using blue & green from irrigated crops
    L165.BlueIrr_m3kg_R_C_GLU %>%
      full_join(L165.TotIrr_m3kg_R_C_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      # Initial adjustment: drop any water coefs for region/crop/GLU combinations that aren't written out
      semi_join(L161.ag_irrProd_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      # Match in 2010 irrigated crop production
      left_join_error_no_match(filter(L161.ag_irrProd_Mt_R_C_Y_GLU, year == max(HISTORICAL_YEARS)),
                               by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      # Use crop prodcution for weighting
      mutate(BlueIrr_km3 = BlueIrr_m3kg * value, TotIrr_km3 = TotIrr_m3kg * value) %>%
      group_by(GCAM_region_ID, GLU) %>%
      summarise(Prod_Mt = sum(value), BlueIrr_km3 = sum(BlueIrr_km3), TotIrr_km3 = sum(TotIrr_km3)) %>%
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
      separate(AgSupplySubsector, c("biomass", "type", "GLU_name"), sep = "_") %>%
      # Match in % of blue water by region / GLU, create NAs, use left_join instead
      left_join(select(L2072.BlueFract_R_GLU, region, GLU_name, blue_fract), by = c("region", "GLU_name")) %>%
      # Multiply biophysical water consumption IO coefs and blue water %
      mutate(coefficient = round(coefficient * blue_fract, aglu.DIGITS_CALOUTPUT)) %>%
      replace_na(list(coefficient = 0)) %>%
      # Irrigated water consumption only applies to the irrigated techs, which are assumed to end in the string "IRR"
      filter(grepl("IRR", AgProductionTechnology)) %>%
      mutate(AgSupplySubsector = paste(biomass, type, GLU_name, sep = "_")) %>%
      # Standardize irrigation water input names
      mutate(water_sector = "Irrigation",
             minicam.energy.input = set_water_input_name(water_sector, water_type, A03.sector, GLU = GLU_name)) %>%
      # Remove extra columns
      select(one_of(LEVEL2_DATA_NAMES[["AgCoef"]])) ->
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
      select(one_of(LEVEL2_DATA_NAMES[["AgCoef"]])) ->
      L2072.AgCoef_IrrWaterWdraw_mgmt

    # Separate table of water withdrawals IO coefs for dedicated bioenergy crops
    L2072.AgCoef_IrrWaterWdraw_mgmt %>%
      filter(AgSupplySector == "biomass") ->
      L2072.AgCoef_IrrWaterWdraw_bio_mgmt

    # Ad hoc adjustment to water coefficients so that none of the region/GLU/crops have negative profit
    # In GCAM, the profit rate is calculated as price minus cost times yield, so if the cost exceeds the price, then the profit goes negative.
    # By adding in the water cost without modifying the non-land variable costs, we risk having costs that exceed commodity prices, which will
    # cause solution/calibration failure in base years, and zero share in future years. This calculation checks whether any of our costs exceed
    # the prices, and reduces water withdrawal coefficients where necessary.
    # Note that the default unlimited water price is divided by the conveyance efficiency in order to replicate the prices in the model
    L165.ag_IrrEff_R %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # Separate table of water withdrawals IO coefs for primary crops
      right_join(filter(L2072.AgCoef_IrrWaterWdraw_mgmt, AgSupplySector != "biomass"), by = "region") %>%
      # Calculate water price and water cost
      mutate(WaterPrice = DEFAULT_UNLIMITED_IRR_WATER_PRICE / conveyance.eff,
             WaterCost = coefficient * WaterPrice) %>%
      # Match in non-land variable costs
      left_join_error_no_match(L2052.AgCost_ag_irr_mgmt, by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year")) %>%
      # Match in commodity price
      left_join_error_no_match(L132.ag_an_For_Prices, by = c("AgSupplySector" = "GCAM_commodity")) ->
      L2072.AgCoef_IrrWaterWdraw_ag_mgmt

    # Assume an exogenous floor on profit rates to prevent negative, zero, and very low profit rates
    # For the model, low profit rates are fine as long as it's not the dominant crop in a nest where bioenergy is allowed
    # Define minimum profit
    minProfit <- min(L2072.AgCoef_IrrWaterWdraw_ag_mgmt["calPrice"] - L2072.AgCoef_IrrWaterWdraw_ag_mgmt["nonLandVariableCost"]) / 2
    L2072.AgCoef_IrrWaterWdraw_ag_mgmt %>%
      # Adjust profit floor
      mutate(coefficient = round(pmin(coefficient, (calPrice - nonLandVariableCost - minProfit) / WaterPrice), aglu.DIGITS_CALOUTPUT)) %>%
      # Remove the unnecessary columns
      select(one_of(LEVEL2_DATA_NAMES[["AgCoef"]])) ->
      L2072.AgCoef_IrrWaterWdraw_ag_mgmt

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
                     "water/A03.sector") ->
      L2072.AgCoef_IrrWaterCons_ag_mgmt

    L2072.AgCoef_IrrWaterWdraw_ag_mgmt %>%
      add_title("Irrigation water withdrawals IO coefficients by region / irrigated crop / year / GLU / management level") %>%
      add_units("km3/Mt") %>%
      add_comments("Withdrawals coefs are calculated as consumption coefs divided by irrigation efficiency") %>%
      add_comments("Set a floor on profit and adjust the coefs to ensure the profit floor is met") %>%
      add_comments("The same IO coefficients are assigned to both high and low management and keep constant for all model years") %>%
      add_legacy_name("L2072.AgCoef_IrrWaterWdraw_ag_mgmt") %>%
      same_precursors_as("L2072.AgCoef_IrrWaterCons_ag_mgmt") %>%
      add_precursors("L132.ag_an_For_Prices",
                     "L165.ag_IrrEff_R",
                     "L2052.AgCost_ag_irr_mgmt") ->
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
                     "water/A03.sector") ->
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
                     "water/A03.sector") ->
      L2072.AgCoef_RfdBphysWater_ag_mgmt

    L2072.AgCoef_BphysWater_bio_mgmt %>%
      add_title("Biophysical water IO coefficients by region / dedicated bioenergy crop / year / GLU / management level") %>%
      add_units("km3/EJ") %>%
      add_comments("The same IO coefficients are assigned to both high and low management and keep constant for all model years") %>%
      add_legacy_name("L2072.AgCoef_BphysWater_bio_mgmt") %>%
      add_precursors("L2052.AgCost_bio_irr_mgmt",
                     "water/A03.sector") ->
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
                     "water/A03.sector") ->
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

    return_data(L2072.AgCoef_IrrBphysWater_ag_mgmt, L2072.AgCoef_IrrWaterWdraw_ag_mgmt, L2072.AgCoef_IrrWaterCons_ag_mgmt, L2072.AgCoef_RfdBphysWater_ag_mgmt, L2072.AgCoef_BphysWater_bio_mgmt, L2072.AgCoef_IrrWaterWdraw_bio_mgmt, L2072.AgCoef_IrrWaterCons_bio_mgmt)
  } else {
    stop("Unknown command")
  }
}
