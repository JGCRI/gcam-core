# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LA105.an_FAO_R_C_Y
#'
#' Aggregates FAO animal products consumption and production data to GCAM region / commodity.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L105.an_Food_Mt_R_C_Y}, \code{L105.an_Food_Pcal_R_C_Y}, \code{L105.an_kcalg_R_C_Y}, \code{L105.an_Prod_Mt_R_C_Y}, \code{L105.an_Prod_Mt_ctry_C_Y}. The corresponding file in the
#' original data system was \code{LA105.an_FAO_R_C_Y.R} (aglu level1).
#' @details This chunk aggregates FAO animal products food consumption and production data up to GCAM commodities and GCAM regions,
#' and calculates the average animal products caloric content by GCAM region / commodity / year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else group_by left_join mutate select summarize
#' @importFrom tidyr complete
#' @author RC June 2017
module_aglu_LA105.an_FAO_R_C_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/FAO/FAO_an_items_cal_SUA",
             "L100.FAO_an_Food_t",
             "L100.FAO_an_Prod_t"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L105.an_Food_Mt_R_C_Y",
             "L105.an_Food_Pcal_R_C_Y",
             "L105.an_kcalg_R_C_Y",
             "L105.an_Prod_Mt_R_C_Y",
             "L105.an_Prod_Mt_ctry_C_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    Country <- D_driver <- Deforest <- DeforestEmiss <- FF_driver <- ForestFire <-
      ForestFireEmiss <- GCAM_commodity <- GCAM_region_ID <- GLU <- GTAP_crop <- HA <-
      HA_kha_irrigated <- HA_kha_rainfed <- Irr_Rfd <- Land_Type <- Mcal_t <- Mt <- Mult <-
      Non.CO2 <- Pcal <- PctForestFire <- Prod_kt_irrigated <- Prod_kt_rainfed <- Prod_mod <-
      YieldRate <- YieldRatio <- YieldRatio_lag <- Yield_kgHa_irrigated <-
      Yield_kgHa_rainfed <- country_ID <- crop_ID <- defaultRate <- em_factor <- irrHA <- iso <-
      lagyear <- lcf <- pop2 <- rfdHA <- sav <- setNames <- technology <- timestep <- value <- year <-
      yield_kgHa <- NULL   # silence package check notes

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    FAO_an_items_cal_SUA <- get_data(all_data, "aglu/FAO/FAO_an_items_cal_SUA")
    L100.FAO_an_Food_t <- get_data(all_data, "L100.FAO_an_Food_t")
    L100.FAO_an_Prod_t <- get_data(all_data, "L100.FAO_an_Prod_t")

    # Process FAO animal products food consumption data: map in GCAM region and commodities, convert units, aggregate to region and commodity
    L100.FAO_an_Food_t %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%                               # Map in GCAM regions
      left_join(FAO_an_items_cal_SUA, by = "item") %>%                                       # Map in GCAM commodities, creates NAs
      filter(!is.na(GCAM_commodity)) %>%                                                      # Remove commodities not included in GCAM
      mutate(Mt = value * CONV_TON_MEGATON, Pcal = value * Mcal_t * CONV_MCAL_PCAL) %>%      # Convert from tons to Mt and Pcal
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%                                      # Group by region, commodity, year
      summarize(Mt = sum(Mt), Pcal = sum(Pcal)) %>%                                           # Aggregate food consumption in Mt and Pcal
      ungroup() %>%                                                                           # Ungroup before complete
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),                        # Fill in missing region/commodity combinations with 0
               GCAM_commodity, year, fill = list(Mt = 0, Pcal = 0)) %>%
      # Calculate average caloric content of consumed commodities (Pcal/Mt = 10e12 kcal / 10e12 g = kcal/g) for each region and animal type, set NA values to 1
      mutate(value = if_else(Mt == 0, 1, Pcal / Mt))  ->
      L105.an_Food_R_C_Y

    # Build table of food consumption in Mt
    L105.an_Food_R_C_Y %>%
      select(GCAM_region_ID, GCAM_commodity, year, value = Mt) ->
      L105.an_Food_Mt_R_C_Y
    # Build table of food consumption in Pcal
    L105.an_Food_R_C_Y %>%
      select(GCAM_region_ID, GCAM_commodity, year, value = Pcal) ->
      L105.an_Food_Pcal_R_C_Y
    # Build table of caloric content in kcal/g
    L105.an_Food_R_C_Y %>%
      select(GCAM_region_ID, GCAM_commodity, year, value) ->
      L105.an_kcalg_R_C_Y

    # Process FAO animal products production data: map in GCAM region and commodities, convert units
    L100.FAO_an_Prod_t %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%                               # Map in GCAM regions
      left_join(FAO_an_items_cal_SUA, by = "item") %>%                                       # Map in GCAM commodities, creates NAs
      filter(!is.na(GCAM_commodity)) %>%                                                      # Remove commodities not included in GCAM
      mutate(value = value * CONV_TON_MEGATON)  ->                                           # Convert from tons to Mt
      L105.FAO_an_Prod_Mt
    # Build country table, aggregate to iso and GCAM commodity
    L105.FAO_an_Prod_Mt %>%
      group_by(iso, GCAM_commodity, year) %>%                                                 # Group by iso, commodity, year
      summarize(value = sum(value)) %>%                                                       # Aggregate food consumption in Mt
      ungroup() %>%                                                                           # Ungroup before complete
      complete(iso = unique(iso_GCAM_regID$iso),                                              # Fill in missing country/commodity combinations with 0
               GCAM_commodity, year, fill = list(value = 0)) ->
      L105.an_Prod_Mt_ctry_C_Y
    # Build region table, aggregate to GCAM region and commodity
    L105.FAO_an_Prod_Mt %>%
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%                                      # Group by region, commodity, year
      summarize(value = sum(value)) %>%                                                       # Aggregate food consumption in Mt
      ungroup() %>%                                                                           # Ungroup before complete
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),                        # Fill in missing region/commodity combinations with 0
               GCAM_commodity, year, fill = list(value = 0)) ->
      L105.an_Prod_Mt_R_C_Y

    # Produce outputs
    L105.an_Food_Mt_R_C_Y %>%
      add_title("Animal consumption by GCAM region / commodity / year") %>%
      add_units("Mt") %>%
      add_comments("Aggregate FAO country and item data by GCAM region, commodity, and year") %>%
      add_comments("Convert data from ton to Mt") %>%
      add_legacy_name("L105.an_Food_Mt_R_C_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/FAO_an_items_cal_SUA",
                     "L100.FAO_an_Food_t") ->
      L105.an_Food_Mt_R_C_Y

    L105.an_Food_Pcal_R_C_Y %>%
      add_title("Animal consumption by GCAM region / commodity / year") %>%
      add_units("Pcal") %>%
      add_comments("Aggregate FAO country and item data by GCAM region, commodity, and year") %>%
      add_comments("Convert data from ton to Pcal") %>%
      add_legacy_name("L105.an_Food_Pcal_R_C_Y") %>%
      same_precursors_as(L105.an_Food_Mt_R_C_Y) ->
      L105.an_Food_Pcal_R_C_Y

    L105.an_kcalg_R_C_Y %>%
      add_title("Average caloric content of animal products by GCAM region / commodity / year") %>%
      add_units("kcal/g") %>%
      add_comments("Combine animal consumption in Mt (L105.an_Food_Mt_R_C_Y) and in Pcal (L105.an_Food_Pcal_R_C_Y)") %>%
      add_comments("Calculate the average caloric content as Pcal devided by Mt") %>%
      add_legacy_name("L105.an_kcalg_R_C_Y") %>%
      same_precursors_as(L105.an_Food_Mt_R_C_Y) ->
      L105.an_kcalg_R_C_Y

    L105.an_Prod_Mt_R_C_Y %>%
      add_title("Animal production by GCAM region / commodity / year") %>%
      add_units("Mt") %>%
      add_comments("Aggregate FAO country and item data by GCAM region commodity, and year") %>%
      add_comments("Convert data from ton to Mt") %>%
      add_legacy_name("L105.an_Prod_Mt_R_C_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/FAO/FAO_an_items_cal_SUA",
                     "L100.FAO_an_Prod_t") ->
      L105.an_Prod_Mt_R_C_Y

    L105.an_Prod_Mt_ctry_C_Y %>%
      add_title("Animal production by country / commodity / year") %>%
      add_units("Mt") %>%
      add_comments("Aggregate FAO country and item data by GCAM commodity, and year") %>%
      add_comments("Convert data from ton to Mt") %>%
      add_legacy_name("L105.an_Prod_Mt_ctry_C_Y") %>%
      same_precursors_as(L105.an_Prod_Mt_R_C_Y) ->
      L105.an_Prod_Mt_ctry_C_Y

    return_data(L105.an_Food_Mt_R_C_Y, L105.an_Food_Pcal_R_C_Y, L105.an_kcalg_R_C_Y, L105.an_Prod_Mt_R_C_Y, L105.an_Prod_Mt_ctry_C_Y)
  } else {
    stop("Unknown command")
  }
}
