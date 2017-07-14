#' module_aglu_L2072.ag_water_irr_mgmt
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2071.AgCoef_IrrBphysWater_ag}, \code{L2072.AgCoef_IrrWaterWdraw_ag}, \code{L2072.AgCoef_IrrWaterCons_ag}, \code{L2072.AgCoef_RfdBphysWater_ag}, \code{L2072.AgCoef_BphysWater_bio}, \code{L2072.AgCoef_IrrWaterWdraw_bio}, \code{L2072.AgCoef_IrrWaterCons_bio}. The corresponding file in the
#' original data system was \code{L2072.ag_water_irr_mgmt.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_L2072.ag_water_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             "L132.ag_an_For_Prices",
             FILE = "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU",
             FILE = "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU",
             FILE = "temp-data-inject/L161.ag_irrYield_kgm2_R_C_Y_GLU",
             "L165.BlueIrr_m3kg_R_C_GLU",
             "L165.TotIrr_m3kg_R_C_GLU",
             "L165.GreenRfd_m3kg_R_C_GLU",
             "L165.ag_IrrEff_R",
             "L2052.AgCost_ag_irr_mgmt",
             "L2052.AgCost_bio_irr_mgmt",
             FILE = "water/A03.sector"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2072.AgCoef_IrrBphysWater_ag",
             "L2072.AgCoef_IrrWaterWdraw_ag",
             "L2072.AgCoef_IrrWaterCons_ag",
             "L2072.AgCoef_RfdBphysWater_ag",
             "L2072.AgCoef_BphysWater_bio",
             "L2072.AgCoef_IrrWaterWdraw_bio",
             "L2072.AgCoef_IrrWaterCons_bio"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    L132.ag_an_For_Prices <- get_data(all_data, "L132.ag_an_For_Prices")
    L161.ag_irrProd_Mt_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU") %>%
      gather(year, value, -GCAM_region_ID, -GCAM_commodity, -GLU) %>%  # temporary
      mutate(year = as.integer(substr(year, 2, 5)))
    L161.ag_rfdProd_Mt_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      gather(year, value, -GCAM_region_ID, -GCAM_commodity, -GLU) %>%  # temporary
      mutate(year = as.integer(substr(year, 2, 5)))
    L161.ag_irrYield_kgm2_R_C_Y_GLU <- get_data(all_data, "temp-data-inject/L161.ag_irrYield_kgm2_R_C_Y_GLU") %>%
      gather(year, value, -GCAM_region_ID, -GCAM_commodity, -GLU) %>%  # temporary
      mutate(year = as.integer(substr(year, 2, 5)))
    L165.BlueIrr_m3kg_R_C_GLU <- get_data(all_data, "L165.BlueIrr_m3kg_R_C_GLU")
    L165.TotIrr_m3kg_R_C_GLU <- get_data(all_data, "L165.TotIrr_m3kg_R_C_GLU")
    L165.GreenRfd_m3kg_R_C_GLU <- get_data(all_data, "L165.GreenRfd_m3kg_R_C_GLU")
    L165.ag_IrrEff_R <- get_data(all_data, "L165.ag_IrrEff_R")
    L2052.AgCost_ag_irr_mgmt <- get_data(all_data, "L2052.AgCost_ag_irr_mgmt")
    L2052.AgCost_bio_irr_mgmt <- get_data(all_data, "L2052.AgCost_bio_irr_mgmt")
    A03.sector <- get_data(all_data, "water/A03.sector")

    # Agricultural component
    #  Water consumption IO coefficients (km3 / Mt crop) for all regions, crops, GLUs, and years
    # Table L2071.AgCoef_IrrWaterCons_ag: Irrigation water consumption IO coefficients by region / crop / year / GLU
    L165.BlueIrr_m3kg_R_C_GLU %>%
      ungroup() %>%
      # Initial adjustment: drop any water coefs for region/crop/GLU combinations that aren't written out
      semi_join(L161.ag_irrProd_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      # Copy costs to high and low management levels
      repeat_add_columns(tibble::tibble(MGMT = c("hi", "lo"))) %>%
      # Add sector, subsector, technology names
      mutate(IRR_RFD = "IRR",
             AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_commodity, GLU_name, sep = "_"),
             AgProductionTechnology = paste(GCAM_commodity, GLU_name, IRR_RFD, MGMT, sep = "_"),
             water_type = "water consumption",
             coefficient = round(BlueIrr_m3kg, aglu.DIGITS_CALOUTPUT)) %>%
      filter(coefficient > 0) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      mutate(water_sector = "Irrigation",
             minicam.energy.input = set_water_input_name(water_sector, water_type, A03.sector, GLU = GLU_name)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["AgCoef"]])) ->
      L2072.AgCoef_IrrWaterCons_ag

    # Table L2071.AgCoef_IrrBphysWater_ag: Biophysical water consumption IO coefficients by region / irrigated crop / year / GLU
    L165.TotIrr_m3kg_R_C_GLU %>%
      ungroup() %>%
      # Initial adjustment: drop any water coefs for region/crop/GLU combinations that aren't written out
      semi_join(L161.ag_irrProd_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      # Copy costs to high and low management levels
      repeat_add_columns(tibble::tibble(MGMT = c("hi", "lo"))) %>%
      # Add sector, subsector, technology names
      mutate(IRR_RFD = "IRR",
             AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_commodity, GLU_name, sep = "_"),
             AgProductionTechnology = paste(GCAM_commodity, GLU_name, IRR_RFD, MGMT, sep = "_"),
             water_type = "biophysical water consumption",
             coefficient = round(TotIrr_m3kg, aglu.DIGITS_CALOUTPUT)) %>%
      filter(coefficient > 0) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      mutate(water_sector = "Irrigation",
             minicam.energy.input = set_water_input_name(water_sector, water_type, A03.sector, GLU = GLU_name)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["AgCoef"]])) ->
      L2072.AgCoef_IrrBphysWater_ag

    # Table L2071.AgCoef_RfdBphysWater_ag: Biophysical water consumption IO coefficients by region / rainfed crop / year / GLU"
    L165.GreenRfd_m3kg_R_C_GLU %>%
      ungroup() %>%
      # Initial adjustment: drop any water coefs for region/crop/GLU combinations that aren't written out
      semi_join(L161.ag_rfdProd_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      # Copy costs to high and low management levels
      repeat_add_columns(tibble::tibble(MGMT = c("hi", "lo"))) %>%
      # Add sector, subsector, technology names
      mutate(IRR_RFD = "RFD",
             AgSupplySector = GCAM_commodity,
             AgSupplySubsector = paste(GCAM_commodity, GLU_name, sep = "_"),
             AgProductionTechnology = paste(GCAM_commodity, GLU_name, IRR_RFD, MGMT, sep = "_"),
             water_type = "biophysical water consumption",
             coefficient = round(GreenRfd_m3kg, aglu.DIGITS_CALOUTPUT)) %>%
      filter(coefficient > 0) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      mutate(water_sector = "Irrigation",
             minicam.energy.input = set_water_input_name(water_sector, water_type, A03.sector, GLU = GLU_name)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["AgCoef"]])) ->
      L2071.AgCoef_RfdBphysWater_ag

    # Water IO coefficients for dedicated bioenergy crops (km3 / EJ biomass)
    # Table L2071.AgCoef_BphysWater_bio: Biophysical water input-output coefficients by region / dedicated bioenergy crop / year / GLU
    L2052.AgCost_bio_irr_mgmt %>%
      select(one_of(LEVEL2_DATA_NAMES[["AgTechYr"]])) %>%
      mutate(minicam.energy.input = "biophysical water consumption",
             coefficient = aglu.BIO_GRASS_WATER_IO_KM3EJ,
             coefficient = replace(coefficient, grepl("biomass_tree", AgProductionTechnology), aglu.BIO_TREE_WATER_IO_KM3EJ)) ->
      L2072.AgCoef_BphysWater_bio

    # Compute blue water for irrigated bioenergy
    # Match in green and blue water for existing crops in 2005 -- note for this we are only using blue & green from irrigated crops
    # Compute % of blue water for irrigated
    L165.BlueIrr_m3kg_R_C_GLU %>%
      full_join(L165.TotIrr_m3kg_R_C_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      # Initial adjustment: drop any water coefs for region/crop/GLU combinations that aren't written out
      semi_join(L161.ag_irrProd_Mt_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      left_join_error_no_match(filter(L161.ag_irrProd_Mt_R_C_Y_GLU, year == max(HISTORICAL_YEARS)),
                               by = c("GCAM_region_ID", "GCAM_commodity", "GLU")) %>%
      mutate(BlueIrr_km3 = BlueIrr_m3kg * value, TotIrr_km3 = TotIrr_m3kg * value) %>%
      group_by(GCAM_region_ID, GLU) %>%
      summarise(Prod_Mt = sum(value), BlueIrr_km3 = sum(BlueIrr_km3), TotIrr_km3 = sum(TotIrr_km3)) %>%
      mutate(BlueIrr_m3kg = BlueIrr_km3 / Prod_Mt, TotIrr_m3kg = TotIrr_km3  / Prod_Mt,
             blue_fract = BlueIrr_m3kg / TotIrr_m3kg) %>%
      replace_na(list(blue_fract = 0)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(basin_to_country_mapping[c("GLU_code", "GLU_name")], by = c("GLU" = "GLU_code")) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) ->
      L2071.BlueFract_R_GLU

    # Create table for model input
    L2072.AgCoef_BphysWater_bio %>%
      mutate(water_type = "water consumption") %>%
      separate(AgSupplySubsector, c("biomass", "type", "GLU_name"), sep = "_") %>%
      left_join(select(L2071.BlueFract_R_GLU, region, GLU_name, blue_fract), by = c("region", "GLU_name")) %>%
      mutate(coefficient = round(coefficient * blue_fract, aglu.DIGITS_CALOUTPUT)) %>%
      replace_na(list(coefficient = 0)) %>%
      # Irrigated water consumption only applies to the irrigated techs, which are assumed to end in the string "IRR"
      filter(grepl("IRR", AgProductionTechnology)) %>%
      mutate(AgSupplySubsector = paste(biomass, type, GLU_name, sep = "_")) %>%
      mutate(water_sector = "Irrigation",
             minicam.energy.input = set_water_input_name(water_sector, water_type, A03.sector, GLU = GLU_name)) %>%
      # Remove extra columns
      select(one_of(LEVEL2_DATA_NAMES[["AgCoef"]])) ->
      L2072.AgCoef_IrrWaterCons_bio

    # Create table for water withdrawals
    # Create table for water withdrawals (consumption divided by irrigation efficiency)
    L165.ag_IrrEff_R %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      full_join(bind_rows(L2072.AgCoef_IrrWaterCons_ag, L2072.AgCoef_IrrWaterCons_bio), by = "region") %>%
      mutate(coefficient = round(coefficient / field.eff, aglu.DIGITS_CALOUTPUT),
             minicam.energy.input = sub("C", "W", minicam.energy.input)) %>%
      # Remove extra columns
      select(one_of(LEVEL2_DATA_NAMES[["AgCoef"]])) ->
      L2072.AgCoef_IrrWaterWdraw

    L2072.AgCoef_IrrWaterWdraw %>%
      filter(AgSupplySector == "biomass") ->
      L2072.AgCoef_IrrWaterWdraw_bio

    # Ad hoc adjustment to water coefficients so that none of the region/GLU/crops have negative profit
    # In GCAM, the profit rate is calculated as price minus cost times yield, so if the cost exceeds the price, then the profit goes negative.
    # By adding in the water cost without modifying the non-land variable costs, we risk having costs that exceed commodity prices, which will
    # cause solution/calibration failure in base years, and zero share in future years. This calculation checks whether any of our costs exceed
    # the prices, and reduces water withdrawal coefficients where necessary.
    # Note that the default unlimited water price is divided by the conveyance efficiency in order to replicate the prices in the model
    L165.ag_IrrEff_R %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      right_join(filter(L2072.AgCoef_IrrWaterWdraw, AgSupplySector != "biomass"), by = "region") %>%
      mutate(WaterPrice = DEFAULT_UNLIMITED_IRR_WATER_PRICE / conveyance.eff,
             WaterCost = coefficient * WaterPrice) %>%
      left_join_error_no_match(L2052.AgCost_ag_irr_mgmt, by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year")) %>%
      left_join_error_no_match(L132.ag_an_For_Prices, by = c("AgSupplySector" = "GCAM_commodity")) %>%
      mutate(Profit = calPrice - WaterCost - nonLandVariableCost) ->
      L2072.AgCoef_IrrWaterWdraw_ag

    # Assuming an exogenous floor on profit rates to prevent negative, zero, and very low profit rates
    # For the model, low profit rates are fine as long as it's not the dominant crop in a nest where bioenergy is allowed
    minProfit <- min(L2072.AgCoef_IrrWaterWdraw_ag["calPrice"] - L2072.AgCoef_IrrWaterWdraw_ag["nonLandVariableCost"]) / 2
    L2072.AgCoef_IrrWaterWdraw_ag %>%
      mutate(coefficient = round(pmin(coefficient, (calPrice - nonLandVariableCost - minProfit) / WaterPrice), aglu.DIGITS_CALOUTPUT)) %>%
      # As a last step, remove the unnecessary columns
      select(one_of(LEVEL2_DATA_NAMES[["AgCoef"]])) ->
      L2072.AgCoef_IrrWaterWdraw_ag
    # # Print out the number of values being changed in each year
    # L2072.AgCoef_IrrWaterWdraw_ag %>% filter(year == 2010) -> tmp1
    # tmp1 %>% filter(Profit < minProfit) -> tmp2
    # printlog("Out of", nrow(tmp1), "observations,", nrow(tmp2), "had water coefficients reduced to keep positive profit rates")

    # Produce outputs
    L2072.AgCoef_IrrBphysWater_ag %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2071.AgCoef_IrrBphysWater_ag") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L165.TotIrr_m3kg_R_C_GLU",
                     "water/A03.sector") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2072.AgCoef_IrrBphysWater_ag

    L2072.AgCoef_IrrWaterWdraw_ag %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2072.AgCoef_IrrWaterWdraw_ag") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L132.ag_an_For_Prices",
                     "L165.ag_IrrEff_R") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2072.AgCoef_IrrWaterWdraw_ag

    L2072.AgCoef_IrrWaterCons_ag %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2072.AgCoef_IrrWaterCons_ag") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "temp-data-inject/L161.ag_irrProd_Mt_R_C_Y_GLU") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2072.AgCoef_IrrWaterCons_ag

    L2072.AgCoef_RfdBphysWater_ag %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2072.AgCoef_RfdBphysWater_ag") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L165.BlueIrr_m3kg_R_C_GLU",
                     "water/A03.sector") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2072.AgCoef_RfdBphysWater_ag

    L2072.AgCoef_BphysWater_bio %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2072.AgCoef_BphysWater_bio") %>%
      add_precursors("L2052.AgCost_bio_irr_mgmt",
                     "water/A03.sector") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2072.AgCoef_BphysWater_bio

    L2072.AgCoef_IrrWaterWdraw %>%
      filter(AgSupplySector == "biomass") %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2072.AgCoef_IrrWaterWdraw_bio") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L165.ag_IrrEff_R",
                     "L2052.AgCost_bio_irr_mgmt",
                     "water/A03.sector") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2072.AgCoef_IrrWaterWdraw_bio

    L2072.AgCoef_IrrWaterCons_bio %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L2072.AgCoef_IrrWaterCons_bio") %>%
      add_precursors("common/GCAM_region_names",
                     "water/basin_to_country_mapping",
                     "L165.ag_IrrEff_R") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L2072.AgCoef_IrrWaterCons_bio

    return_data(L2072.AgCoef_IrrBphysWater_ag, L2072.AgCoef_IrrWaterWdraw_ag, L2072.AgCoef_IrrWaterCons_ag, L2072.AgCoef_RfdBphysWater_ag, L2072.AgCoef_BphysWater_bio, L2072.AgCoef_IrrWaterWdraw_bio, L2072.AgCoef_IrrWaterCons_bio)
  } else {
    stop("Unknown command")
  }
}
