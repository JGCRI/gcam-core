# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LB165.ag_water_R_C_Y_GLU_irr
#'
#' Compute irrigation efficiency (by GCAM region) and blue, green, and total consumption coefficients (by region, commodity, and GLU).
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L165.BlueIrr_m3kg_R_C_GLU}, \code{L165.TotIrr_m3kg_R_C_GLU},
#' \code{L165.GreenRfd_m3kg_R_C_GLU}, \code{L165.ag_IrrEff_R}, \code{L165.IrrWithd_km3_R_Y}. The corresponding file in
#' the original data system was \code{LB165.ag_water_R_C_Y_GLU_irr.R} (aglu level1).
#' @details Use inventory estimates of either water demand coefficients by country and crop,
#' or aggregated gridded volumes of water use by country, GLU, and crop, to calculate average
#' water consumption coefficients by GCAM region, crop, GLU, and irrigation level.
#' Blue water is assigned to only irrigated production, but green water applies to both
#' irrigated and rainfed production, as the two management technologies are not disaggregated
#' in the inventory data. For rainfed crops, total biophysical = green, and for irrigated crops,
#' total biophysical = blue + green.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by left_join mutate right_join select summarise summarise_at vars
#' @importFrom tidyr gather replace_na spread
#' @author BBL April 2017
module_aglu_LB165.ag_water_R_C_Y_GLU_irr <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
             "L100.Water_footprint_m3",
             "L100.LDS_ag_prod_t",
             FILE = "aglu/Mekonnen_Hoekstra_Rep47_A2",
             FILE = "aglu/Rohwer_2007_IrrigationEff",
             "L151.ag_irrProd_t_ctry_crop",
             "L151.ag_rfdProd_t_ctry_crop",
             "L151.ag_irrHA_ha_ctry_crop",
             "L161.ag_irrProd_Mt_R_C_Y_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L165.BlueIrr_m3kg_R_C_GLU",
             "L165.TotIrr_m3kg_R_C_GLU",
             "L165.GreenRfd_m3kg_R_C_GLU",
             "L165.ag_IrrEff_R",
             "L165.IrrWithd_km3_R_Y",
             "L165.IrrWithd_km3_R_B_Y"))
  } else if(command == driver.MAKE) {

    ## Silence package check.
    GTAP_crop <- MH_crop <- iso <- coef_m3t <- FAO_crop <- water_type <-
      coef_m3kg <- value <- Prod_t <- Blue <- Green <- item <- value_other <-
      blue_m3kg <- green_m3kg <- irrProd <- blue_thousm3 <- irrProd_t <-
      BlueIrr_m3kg <- total_m3kg <- GreenIrr_m3kg <- green_thousm3 <-
      GreenIrr_thousm3 <- rfdProd <- rfdProd_t <- GreenRfd_thousm3 <-
      MH2014_proxy <- GLU <- year <- IrrWithd_km3 <- NULL
    GreenRfd_m3kg <- GCAM_region_ID <- GCAM_commodity <- BlueIrr_thousm3 <-
      TotIrr_m3kg <- application.eff <- management.eff <- irrHA <-
      field.eff <- conveyance.eff <- GCAM_subsector <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_items_PRODSTAT")
    L100.Water_footprint_m3 <- get_data(all_data, "L100.Water_footprint_m3")
    Mekonnen_Hoekstra_Rep47_A2 <- get_data(all_data, "aglu/Mekonnen_Hoekstra_Rep47_A2")
    Rohwer_2007_IrrigationEff <- get_data(all_data, "aglu/Rohwer_2007_IrrigationEff")
    L100.LDS_ag_prod_t <- get_data(all_data, "L100.LDS_ag_prod_t")
    L151.ag_irrProd_t_ctry_crop <- get_data(all_data, "L151.ag_irrProd_t_ctry_crop")
    L151.ag_rfdProd_t_ctry_crop <- get_data(all_data, "L151.ag_rfdProd_t_ctry_crop")
    L151.ag_irrHA_ha_ctry_crop <- get_data(all_data, "L151.ag_irrHA_ha_ctry_crop")
    L161.ag_irrProd_Mt_R_C_Y_GLU <- get_data(all_data, "L161.ag_irrProd_Mt_R_C_Y_GLU")


    # Perform computations

    # The method here is simple in principle: we are taking inventory
    # estimates of either water demand coefficients by country and crop, or aggregated
    # gridded volumes of water use by country, GLU, and crop, and using these estimates to
    # calculate average water consumption coefficients by GCAM region, crop, GLU, and
    # irrigation level.
    #
    # All data represent a statistical year around 2000, and the GTAP/Monfreda data were
    # used in constructing the gridded and national inventory data, so this is the appropriate
    # production data to use.
    #
    # The major methodological decisions relate to how to estimate the coefficients by GLU for
    # crops that are not available in the gridded inventory, how to extrapolate from the given
    # crops to the fodder crops, and how to assign priority for the crops included in both the
    # gridded and national inventories. These decisions are documented below.

    # The inventory data disaggregate green, blue, and gray water. Gray water indicates
    # downstream pollution; this is not considered in GCAM and has not been included in the
    # gridded data processing. Blue water is assigned to only irrigated production, but green
    # water applies to both irrigated and rainfed production, as the two management technologies
    # are not disaggregated in the inventory data. For rainfed crops, total biophysical = green,
    # and for irrigated crops, total biophysical = blue + green.

    # Initial data cleaning (original lines 55-57)
    L100.Water_footprint_m3 %>%
      rename(MH_crop = GTAP_crop) %>%
      left_join(select(FAO_ag_items_PRODSTAT, MH_crop, GTAP_crop), by = "MH_crop") ->
      L165.Water_footprint_m3

    # Re-cast nation-level data so that blue and green are represented as data columns (original lines 59-64)
    Mekonnen_Hoekstra_Rep47_A2 %>%
      gather(iso, coef_m3t, -FAO_crop_item, -item_code, -water_type) %>%
      mutate(coef_m3kg = coef_m3t / CONV_T_KG) %>%
      select(iso, FAO_crop_item, item_code, water_type, coef_m3kg) %>%
      # Set all missing values to zero -- this may be re-visited at another point
      # We may want to substitute default values from the MH2011 table

      # More info on this from @pkyle:
      # The source data table has 29,725 observations (country x crop), of which 24,063 are missing values,
      # and of these missing values, 321 did actually have crop production in Monfreda's inventory, 13 of which
      # had > 1 Mt of production. We are using this dataset to supplement a global gridded inventory with 18 major
      # crops. Filtering those out, there are still 4 observations with between 1 and 3 Mt of production, all in
      # Nigeria: CitrusFrtNES, CowPeasDry, Plantains, and FrtFrshNES.
      # If we want to replace these with defaults, the blue water coef (i.e., the important one) will still be zero,
      # because it's in Nigeria, where ~100% of crop production is rainfed (according to the inventories). So, if we
    # do implement a different method, we'll get a slightly higher estimate of biophysical water consumption for
    # MiscCrop in Africa_Western.
    replace_na(list(coef_m3kg = 0)) %>%
      spread(water_type, coef_m3kg) ->
      L165.Mekonnen_Hoekstra_Rep47_A2

    # Build inventory of water use by country, crop, and GLU for the 18 gridded crops (lines 66-74 in original)
    # We give precedence to the aggregated gridded data for the 18 crops that are mapped,
    # and fill in nation-level inventory data for the remainder
    L165.ag_Prod_t_ctry_crop_GLU <- rename(L100.LDS_ag_prod_t, Prod_t = value)

    L165.Water_footprint_m3 %>%
      filter(water_type %in% c("green", "blue")) %>%
      # TODO: this following join has NAs but shouldn't change # of rows (currently doesn't) - use left_join_restricted
      left_join(L165.ag_Prod_t_ctry_crop_GLU, by = c("iso", "GTAP_crop", "GLU")) %>%
      # M+H have some country/crop/GLU combinations that Monfreda doesn't
      # Set missing values to 0
      replace_na(list(Prod_t = 0L)) %>%
      # For the 18 M+H gridded crops, calculate the coefs by country, GLU, and crop (original lines 79-82)
      # NOTE: dropping all country / GLU / crops with zero production; these won't have any water anyway
      filter(Prod_t > 0) %>%
      mutate(coef_m3kg = value / (Prod_t * CONV_T_KG)) ->
      L165.ag_Water_ctry_MHcrop_GLU

    # Original lines 84-106
    # The assumed maximum values are crop specific and based on analysis of the 2011 inventory data,
    # but because of statistical discrepancies we need to clip extremely high values. To do so
    # we compute the maximum observed value across all national averages, add a small (2 m3/kg)
    # adjustment, and use that as a cap. We want to use something a bit higher than the maximum of
    # the national averages, because the sub-national regions' values will have a greater range.
    #
    # This sort of cap is necessary because many of the water quantities, particularly in
    # small regions, are clearly inconsistent with the underlying production data and produce
    # extremely high water demand coefficients. In the case of Barley in Iraq it is clearly an
    # error in the M+H gridded inventory, but for others it could be a simple mismatch in
    # estimates of crop production by region, and share of irrigated production by region.
    CTRY_GLU_MAX_ADDER <- 2
    L165.Mekonnen_Hoekstra_Rep47_A2 %>%
      group_by(FAO_crop_item, item_code) %>%
      summarise(Blue = max(Blue + CTRY_GLU_MAX_ADDER),
                Green = max(Green + CTRY_GLU_MAX_ADDER)) %>%
      ungroup() %>%
      left_join(select(FAO_ag_items_PRODSTAT, item_code, MH_crop), by = c("item_code")) ->
      L165.MaxWaterCoefs_m3kg

    # Apply the cap computed above: change L165.ag_Water_ctry_MHcrop_GLU$coef_m3kg to be the minimum
    # of itself and the corresponding (by crop and water_type) value in L165.MaxWaterCoefs_m3kg
    L165.MaxWaterCoefs_m3kg %>%
      gather(water_type, value_other, Blue, Green) %>%
      mutate(water_type = tolower(water_type)) %>%
      right_join(L165.ag_Water_ctry_MHcrop_GLU,
                 by = c("MH_crop", "water_type")) %>%
      mutate(coef_m3kg = pmin(coef_m3kg, value_other)) %>%
      select(-value_other, -FAO_crop_item, -item_code) ->
      L165.ag_Water_ctry_MHcrop_GLU

    # Work through calculation sequence to determine blue and green water coefficients for irrigated
    # and rainfed production for gridded crops (original lines 108-122). The sequence has 5 steps:
    #	(1) Calculate the total biophysical (green + blue) water quantities and coefficients for each country, GLU, and crop
    #	(2) Match in the irrigated production and calculate the blue water coef for irrigated as min(blue water quantity / irr prod, total biophysical coef)
    #	(3) Calculate the green water coef for irrigated crops as the total biophysical coef minus the blue water coef
    #	(4) Calculate the total volume of green water on rainfed crops as the total green water minus green water used by irrigated crops
    #	(5) Calculate the green water coef for rainfed crops as rainfed green water volume divided by rainfed production

    # Here we go:
    #	(1) Calculate the total biophysical (green + blue) water quantities and coefficients for each country, GLU, and crop
    L165.ag_Water_ctry_MHcrop_GLU %>%
      ungroup %>%
      select(-MH_crop, -value) %>%
      mutate(water_type = paste0(water_type, "_m3kg")) %>%
      spread(water_type, coef_m3kg, fill = 0.0) %>%
      mutate(total_m3kg = blue_m3kg + green_m3kg,
             blue_thousm3 = Prod_t * blue_m3kg,
             green_thousm3 = Prod_t * green_m3kg) ->
      L165.ag_Water_ctry_MHcrop_GLU

    #	(2) Match in the irrigated production and calculate the blue water coef (original lines 124-147)
    L165.ag_Water_ctry_MHcrop_GLU %>%
      left_join_error_no_match(L151.ag_irrProd_t_ctry_crop, by = c("iso", "GTAP_crop", "GLU")) %>%
      rename(irrProd_t = irrProd) %>%
      mutate(BlueIrr_m3kg = blue_thousm3 / irrProd_t,
             # Set the coefs to zero wherever irrigated production is zero (these couldn't have any water consumption in GCAM anyway)
             BlueIrr_m3kg = if_else(irrProd_t == 0, 0, BlueIrr_m3kg),
             # Cap blue water coefficients at the total biophysical quantity
             BlueIrr_m3kg = pmin(BlueIrr_m3kg, total_m3kg),
             #	(3) Calculate the green water coef for irrigated crops
             GreenIrr_m3kg = total_m3kg - BlueIrr_m3kg,
             #	(4) Calculate the total volume of green water on rainfed crops
             GreenIrr_thousm3 = GreenIrr_m3kg * irrProd_t,
             GreenRfd_thousm3 = green_thousm3 - GreenIrr_thousm3) %>%
      # join in rainfed crop production values to calculate the green water coefficient for rainfed crops
      left_join_error_no_match(L151.ag_rfdProd_t_ctry_crop, by = c("iso", "GTAP_crop", "GLU")) %>%
      rename(rfdProd_t = rfdProd) %>%
      #	(5) Calculate the green water coef for rainfed crops as rainfed green water volume divided by rainfed production
      mutate(GreenRfd_m3kg = if_else(rfdProd_t != 0, GreenRfd_thousm3 / rfdProd_t, 0)) ->
      L165.ag_Water_ctry_MHcrop_GLU

    # Original lines 149-155
    # This table needs to be expanded to the more complete list of crops in the MH2011 inventory
    # The expanded set only includes crops not already accounted in the gridded inventory
    # Increase the data set in countries and crops in the country-level inventory, and where production is non-zero
    GTAP_addl_crops <- FAO_ag_items_PRODSTAT$GTAP_crop[is.na(FAO_ag_items_PRODSTAT$MH_crop)]
    L165.ag_Prod_t_ctry_crop_GLU %>%
      filter(iso %in% L165.Mekonnen_Hoekstra_Rep47_A2$iso,
             GTAP_crop %in% GTAP_addl_crops) ->
      L165.ag_Water_ctry_MHcropX_GLU

    # Original lines 157-164
    # We don't want to multiply each GLU's total production by the nation-level blue and green coefs,
    # as this will do a poor job of allocating blue water to the GLUs with the most irrigated production.
    # The method followed here is similar to the method followed above for gridded crops, but here it is
    # performed without any GLU-level detail.
    # Drop the GLU from the first stage of calculations for the additional country-level crops
    L165.ag_Water_ctry_MHcropX_GLU %>%
      group_by(iso, GTAP_crop) %>%
      summarise(Prod_t = sum(Prod_t)) %>%
      left_join_keep_first_only(select(FAO_ag_items_PRODSTAT, GTAP_crop, item), by = "GTAP_crop") %>%
      rename(FAO_crop = item) ->
      L165.ag_Water_ctry_MHcropX

    # Original lines 166-179
    # A number of crops that are not in the MH country-level inventory--mostly the fodder crops--will
    # instead inherit their water consumption coefficients from a proxy crop, indicated in the
    # FAO_ag_items_PRODSTAT mapping file
    crops_to_substitute <- FAO_ag_items_PRODSTAT$item[!is.na( FAO_ag_items_PRODSTAT$MH2014_proxy)]
    tibble(item = crops_to_substitute) %>%
      left_join_keep_first_only(FAO_ag_items_PRODSTAT, by = "item") %>%
      select(item, MH2014_proxy) ->
      proxy_crops

    L165.ag_Water_ctry_MHcropX %>%
      filter(FAO_crop %in% crops_to_substitute) %>%
      left_join_error_no_match(proxy_crops, c("FAO_crop" = "item")) %>%
      select(-FAO_crop) %>%
      rename(FAO_crop = MH2014_proxy) %>%
      # combine with the non-substituted crops
      bind_rows(filter(L165.ag_Water_ctry_MHcropX, ! FAO_crop %in% crops_to_substitute)) %>%
      # match in the water contents
      left_join_keep_first_only(L165.Mekonnen_Hoekstra_Rep47_A2 %>% rename(FAO_crop = FAO_crop_item),
                                by = c("iso", "FAO_crop")) %>%
      rename(blue_m3kg = Blue, green_m3kg = Green) %>%
      # drop the crops that aren't assigned to anything, which will return missing values here
      na.omit() ->
      L165.ag_Water_ctry_MHcropX

    # Original lines 181-203
    # See below for note on fodder crops - this method over-states the water coefs of fodder crops,
    # and it is adjusted later. The next steps follow the 5-step method described above for
    # assigning blue and green water to irrigated and rainfed production
    L165.ag_Water_ctry_MHcropX %>%
      mutate(total_m3kg = blue_m3kg + green_m3kg,
             blue_thousm3 = Prod_t * blue_m3kg,
             green_thousm3 = Prod_t * green_m3kg) ->
      L165.ag_Water_ctry_MHcropX

    # Match in the irrigated production to calculate the blue water coef for irrigated crops
    L151.ag_irrProd_t_ctry_crop %>%
      group_by(iso, GTAP_crop) %>%
      summarise(irrProd = sum(irrProd)) ->
      L165.irrProd_t_ctry_crop

    L165.ag_Water_ctry_MHcropX %>%
      left_join_error_no_match(L165.irrProd_t_ctry_crop, by = c("iso", "GTAP_crop")) %>%
      rename(irrProd_t = irrProd) %>%
      mutate(BlueIrr_m3kg = if_else(irrProd_t != 0, blue_thousm3 / irrProd_t, 0),
             BlueIrr_m3kg = pmin(BlueIrr_m3kg, total_m3kg),
             # green water coefs of irrigated crops = total biophysical coef minus calculated blue water coef
             GreenIrr_m3kg = total_m3kg - BlueIrr_m3kg,
             # green water coefs of rainfed crops = total green water minus green water on irrigated
             GreenIrr_thousm3 = GreenIrr_m3kg * irrProd_t,
             GreenRfd_thousm3 = green_thousm3 - GreenIrr_thousm3) ->
      L165.ag_Water_ctry_MHcropX

    # Original lines 204-210
    L151.ag_rfdProd_t_ctry_crop %>%
      group_by(iso, GTAP_crop) %>%
      summarise(rfdProd_t = sum(rfdProd)) ->
      L165.rfdProd_t_ctry_crop

    L165.ag_Water_ctry_MHcropX %>%
      left_join_error_no_match(L165.rfdProd_t_ctry_crop, by = c("iso", "GTAP_crop")) %>%
      mutate(GreenRfd_m3kg = if_else(rfdProd_t != 0, GreenRfd_thousm3 / rfdProd_t, 0)) ->
      L165.ag_Water_ctry_MHcropX

    # The grid-based and nation-based tables of green and blue water coefs and volumes for irrigated
    # and rainfed crops can now be combined to provide estimates of green and blue water coefs for all
    # available countries, crops, and GLUs (original lines 212-229)
    # First we need to write out the MH2014 coefficients to all applicable GLUs
    L165.ag_Prod_t_ctry_crop_GLU %>%
      select(iso, GTAP_crop, GLU) %>%
      filter(iso %in% L165.Mekonnen_Hoekstra_Rep47_A2$iso,
             GTAP_crop %in% L165.ag_Water_ctry_MHcropX$GTAP_crop) %>%
      left_join_error_no_match(select(L151.ag_irrProd_t_ctry_crop, iso, GTAP_crop, GLU, irrProd),
                               by = c("iso", "GTAP_crop", "GLU")) %>%
      rename(irrProd_t = irrProd) %>%
      left_join_error_no_match(select(L151.ag_rfdProd_t_ctry_crop, iso, GTAP_crop, GLU, rfdProd),
                               by = c("iso", "GTAP_crop", "GLU")) %>%
      rename(rfdProd_t = rfdProd) %>%
      left_join_error_no_match(select(L165.ag_Water_ctry_MHcropX, iso, GTAP_crop, BlueIrr_m3kg, GreenIrr_m3kg, GreenRfd_m3kg),
                               by = c("iso", "GTAP_crop")) ->
      L165.ag_Water_ctry_MHcropX_GLU

    # Combine with the grid-based estimates (original lines 231-241)
    L165.ag_Water_ctry_MHcropX_GLU %>%
      bind_rows(L165.ag_Water_ctry_MHcrop_GLU) %>%
      select(iso, GTAP_crop, GLU, irrProd_t, rfdProd_t, BlueIrr_m3kg, GreenIrr_m3kg, GreenRfd_m3kg) %>%
      # calculate green and blue water quantities by all available countries, crops, GLUs, and irrigated/rainfed
      mutate(BlueIrr_thousm3 = BlueIrr_m3kg * irrProd_t,
             GreenIrr_thousm3 = GreenIrr_m3kg * irrProd_t,
             GreenRfd_thousm3 = GreenRfd_m3kg * rfdProd_t) %>%
      # match in GCAM regions and commodities for aggregation
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      left_join_keep_first_only(select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity, GCAM_subsector), by = "GTAP_crop") ->
      L165.ag_Water_ctry_crop_GLU

    # Original lines 243-265
    # NOTE: Fodder crops are not in the M+H inventories. They instead have been assigned a
    # corresponding crop that is in the M+H inventories and at this point are multiplied by
    # an exogenous fodder:crop adjustment factor. This factor is designed to reflect that
    # fodder crop masses include the whole plant, often harvested green (~70% water). As such,
    # their water coefficients will tend to be a good deal lower than the corresponding crop
    # to which they are assigned. The specific number is based on analysis of a virtual water
    # content dataset (Chapagain 2004) that included fodder crops.
    # The "corresponding" crops are assigned in the mapping file (FAO_ag_items_PRODSTAT),
    # based on species (e.g., maize) or family (e.g., alfalfa)
    FODDER_CROP_WATER_ADJUSTMENT <- 0.2
    FODDER_CROPS <- c("FodderGrass", "FodderHerb")
    L165.ag_Water_ctry_crop_GLU %>%
      filter(GCAM_commodity %in% FODDER_CROPS) %>%
      mutate(BlueIrr_thousm3 = BlueIrr_thousm3 * FODDER_CROP_WATER_ADJUSTMENT,
             GreenIrr_thousm3 = GreenIrr_thousm3 * FODDER_CROP_WATER_ADJUSTMENT,
             GreenRfd_thousm3 = GreenRfd_thousm3 * FODDER_CROP_WATER_ADJUSTMENT) %>%
      bind_rows(filter(L165.ag_Water_ctry_crop_GLU, ! GCAM_commodity %in% FODDER_CROPS)) %>%

      # at this point, the crops are ready for aggregation by GCAM region and commodity
      filter(!is.na(GCAM_commodity)) %>%   # to replicate `aggregate` behavior
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%
      summarise_at(vars(irrProd_t, rfdProd_t, BlueIrr_thousm3, GreenIrr_thousm3, GreenRfd_thousm3), sum) %>%
      ungroup %>%
      mutate(BlueIrr_m3kg = BlueIrr_thousm3 / irrProd_t,
             GreenIrr_m3kg = GreenIrr_thousm3 / irrProd_t,
             GreenRfd_m3kg = GreenRfd_thousm3 / rfdProd_t,
             TotIrr_m3kg = BlueIrr_m3kg + GreenIrr_m3kg) ->
      L165.ag_Water_R_C_GLU

    # Re-set missing values to zero
    L165.ag_Water_R_C_GLU[is.na(L165.ag_Water_R_C_GLU)] <- 0

    # Create the final tables to be written out (original lines 267-270)
    L165.BlueIrr_m3kg_R_C_GLU <- select(L165.ag_Water_R_C_GLU, GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, BlueIrr_m3kg)
    L165.TotIrr_m3kg_R_C_GLU <- select(L165.ag_Water_R_C_GLU, GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, TotIrr_m3kg)
    L165.GreenRfd_m3kg_R_C_GLU <- select(L165.ag_Water_R_C_GLU, GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, GreenRfd_m3kg)

    # Original lines 272-290
    # Irrigation Efficiency: Compile country-level data to GCAM regions, weighted by total irrigated harvested area
    # NOTE: Using application and management efficiencies to define whole-system efficiencies (not conveyance)
    Rohwer_2007_IrrigationEff <- mutate(Rohwer_2007_IrrigationEff, field.eff = application.eff * management.eff)

    L151.ag_irrHA_ha_ctry_crop %>%
      group_by(iso) %>%
      summarise(irrHA = sum(irrHA)) %>%
      left_join(select(Rohwer_2007_IrrigationEff, iso, field.eff, conveyance.eff), by = "iso") %>%
      filter(irrHA > 0) %>%
      na.omit %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      # weighted (irrHA) mean by iso
      group_by(GCAM_region_ID) %>%
      summarise(field.eff = weighted.mean(field.eff, irrHA),
                conveyance.eff = weighted.mean(conveyance.eff, irrHA)) ->
      L165.ag_IrrEff_R

    # Final step: Computing water withdrawals volumes by region for energy-for-water estimation
    # Note that this was not in the original data system
    # water withdrawal volumes are calculated as irrigated crop production times blue water coefficients, divided by field efficiencies
    # The ignore_columns argument is used due to mismatches in coverage between MIRCA and Mekonnen+Hoekstra; these are
    # minor region/glu/crop observations
    L165.IrrWithd_km3_R_C_Y_GLU <- L161.ag_irrProd_Mt_R_C_Y_GLU %>%
      left_join_error_no_match(L165.BlueIrr_m3kg_R_C_GLU,
                               by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU"),
                               ignore_columns = "BlueIrr_m3kg") %>%
      left_join_error_no_match(select(L165.ag_IrrEff_R, GCAM_region_ID, field.eff),
                               by = "GCAM_region_ID") %>%
      mutate(BlueIrr_m3kg = if_else(is.na(BlueIrr_m3kg), 0, BlueIrr_m3kg),
             IrrWithd_km3 = value * BlueIrr_m3kg / field.eff) %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, IrrWithd_km3)

    # aggregate by GCAM region and year
    L165.IrrWithd_km3_R_Y <- group_by(L165.IrrWithd_km3_R_C_Y_GLU, GCAM_region_ID, year) %>%
      summarise(IrrWithd_km3 = sum(IrrWithd_km3)) %>%
      ungroup()

    # aggregate by GCAM region, basin, and year
    L165.IrrWithd_km3_R_B_Y <- group_by(L165.IrrWithd_km3_R_C_Y_GLU, GCAM_region_ID, GLU, year) %>%
      summarise(IrrWithd_km3 = sum(IrrWithd_km3)) %>%
      ungroup()

    # Produce outputs
    L165.BlueIrr_m3kg_R_C_GLU %>%
      add_title("Blue water consumption coefficients for irrigated crops by GCAM region / commodity / GLU") %>%
      add_units("m3/kg") %>%
      add_comments("Calculated from the total biophysical (green + blue) water quantities and coefficients;") %>%
      add_comments("the blue water coef for irrigated; the green water coef for irrigated crops; ") %>%
      add_comments("the total volume of green water on rainfed crops; and the green water coef for rainfed crops") %>%
      add_legacy_name("L165.BlueIrr_m3kg_R_C_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "L100.Water_footprint_m3",
                     "L100.LDS_ag_prod_t",
                     "aglu/Mekonnen_Hoekstra_Rep47_A2",
                     "aglu/Rohwer_2007_IrrigationEff",
                     "L151.ag_irrProd_t_ctry_crop",
                     "L151.ag_rfdProd_t_ctry_crop",
                     "L151.ag_irrHA_ha_ctry_crop") ->
      L165.BlueIrr_m3kg_R_C_GLU

    L165.TotIrr_m3kg_R_C_GLU %>%
      add_title("Total biophysical water consumption coefficients for irrigated crops by GCAM region / commodity / GLU") %>%
      add_units("m3/kg") %>%
      add_comments("Calculated from the total biophysical (green + blue) water quantities and coefficients;") %>%
      add_comments("the blue water coef for irrigated; the green water coef for irrigated crops; ") %>%
      add_comments("the total volume of green water on rainfed crops; and the green water coef for rainfed crops") %>%
      add_legacy_name("L165.TotIrr_m3kg_R_C_GLU") %>%
      same_precursors_as(L165.BlueIrr_m3kg_R_C_GLU) ->
      L165.TotIrr_m3kg_R_C_GLU

    L165.GreenRfd_m3kg_R_C_GLU %>%
      add_title("Green water consumption coefficients for rainfed crops by GCAM region / commodity / GLU") %>%
      add_units("m3/kg") %>%
      add_comments("Calculated from the total biophysical (green + blue) water quantities and coefficients;") %>%
      add_comments("the blue water coef for irrigated; the green water coef for irrigated crops; ") %>%
      add_comments("the total volume of green water on rainfed crops; and the green water coef for rainfed crops") %>%
      add_legacy_name("L165.GreenRfd_m3kg_R_C_GLU") %>%
      same_precursors_as(L165.BlueIrr_m3kg_R_C_GLU) ->
      L165.GreenRfd_m3kg_R_C_GLU

    L165.ag_IrrEff_R %>%
      add_title("Irrigation efficiency by GCAM region") %>%
      add_units("Unitless efficiency") %>%
      add_comments("Calculated from the total biophysical (green + blue) water quantities and coefficients;") %>%
      add_comments("the blue water coef for irrigated; the green water coef for irrigated crops; ") %>%
      add_comments("the total volume of green water on rainfed crops; and the green water coef for rainfed crops") %>%
      add_legacy_name("L165.ag_IrrEff_R") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/Rohwer_2007_IrrigationEff",
                     "L151.ag_irrHA_ha_ctry_crop") ->
      L165.ag_IrrEff_R

    L165.IrrWithd_km3_R_Y %>%
      add_title("Irrigation water withdrawals by GCAM region") %>%
      add_units("km^3") %>%
      add_comments("All irrigation water provided to agricultural sector by region (crops and land use regions aggregated)") %>%
      same_precursors_as(L165.BlueIrr_m3kg_R_C_GLU) %>%
      same_precursors_as(L165.ag_IrrEff_R) %>%
      add_precursors("L161.ag_irrProd_Mt_R_C_Y_GLU")->
      L165.IrrWithd_km3_R_Y

    L165.IrrWithd_km3_R_B_Y %>%
      add_title("Irrigation water withdrawals by GCAM region and GLU") %>%
      add_units("km^3") %>%
      add_comments("All irrigation water provided to agricultural sector by land use region (GCAM region and GLU)") %>%
      same_precursors_as(L165.IrrWithd_km3_R_Y) ->
      L165.IrrWithd_km3_R_B_Y

      return_data(L165.BlueIrr_m3kg_R_C_GLU,
                  L165.TotIrr_m3kg_R_C_GLU,
                  L165.GreenRfd_m3kg_R_C_GLU,
                  L165.ag_IrrEff_R,
                  L165.IrrWithd_km3_R_Y,
                  L165.IrrWithd_km3_R_B_Y)
  } else {
    stop("Unknown command")
  }
}
