# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA1324.Off_road
#'
#' Sets up input, output, and IO coefficients for Off_road and subtracts input energy from industry energy use
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1324.in_EJ_R_Off_road_F_Y}, \code{L1324.in_EJ_R_indenergy_F_Yh}, \code{L1323.in_EJ_R_indfeed_F_Yh}. The corresponding file in the
#' @details This chunk generates input, Energy inputs are then subtracted from industrial energy use, feedstock and any resulting negative values
#' are dealt with by moving their accounting to the Off_road sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by left_join mutate select semi_join summarise summarise_all
#' @importFrom tidyr gather spread
#' @author Yang Liu Sep 2019
module_energy_LA1324.Off_road <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L1011.en_bal_EJ_R_Si_Fi_Yh",
             FILE = "energy/A_regions",
             FILE = "energy/mappings/enduse_fuel_aggregation",
             "L1323.in_EJ_R_indenergy_F_Yh",
             "L1322.in_EJ_R_indfeed_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1324.in_EJ_R_Off_road_F_Y",
             "L1324.in_EJ_R_indenergy_F_Yh",
             "L1323.in_EJ_R_indfeed_F_Yh"))
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    raw <- subsector <- minicam.energy.input <- Country <- sector <-
    share <- value <- Off_road <- year <- value.y <- value.x <- fuel <- industry <-
      GCAM_region_ID <- has_district_heat <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    A_regions <- get_data(all_data, "energy/A_regions")
    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1011.en_bal_EJ_R_Si_Fi_Yh", strip_attributes = TRUE)
	  enduse_fuel_aggregation <- get_data(all_data, "energy/mappings/enduse_fuel_aggregation")
    L1323.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1323.in_EJ_R_indenergy_F_Yh", strip_attributes = TRUE)
    L1322.in_EJ_R_indfeed_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indfeed_F_Yh", strip_attributes = TRUE)

    # ===================================================
    # 2. Perform computations

    # Set constants used for this chunk
    # ---------------------------------
    # Determine historical years not available in data set (additional years) to copy values from final available year (final_CO2_year)

    # ===================================================


    # Construction energy input and construction feedstocks
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(grepl("construction", sector)) ->
      L1324.in_EJ_R_Off_road_F_Y

     # Agriculture energy input
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(grepl("agriculture", sector)) %>%
      bind_rows(L1324.in_EJ_R_Off_road_F_Y) ->
      L1324.in_EJ_R_Off_road_F_Y

    # Mining energy input
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(grepl("mining", sector)) %>%
      bind_rows(L1324.in_EJ_R_Off_road_F_Y) ->
      L1324.in_EJ_R_Off_road_F_Y

    #Mapping the fuel used in off-road sector
    L1324.in_EJ_R_Off_road_F_Y %>%
      left_join(select(enduse_fuel_aggregation, fuel, industry), by = "fuel") %>%
      select(-fuel, fuel = industry) %>%
      na.omit() %>%
      group_by(GCAM_region_ID, year, sector, fuel) %>%
      summarise(value = sum(value)) %>%
      ungroup()->
      L1324.in_EJ_R_Off_road_F_Y

    ## Heat: drop heat in regions where heat is not modeled as a final fuel
    A_regions %>%
      filter(has_district_heat == 0) %>%
      select(GCAM_region_ID) %>%
      unique %>%
      mutate(fuel = "heat") ->
      region_heat

    L1324.in_EJ_R_Off_road_F_Y  %>%
      anti_join(region_heat, by = c("GCAM_region_ID", "fuel")) %>% # then drop the regions selected in region_heat
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup ->
      L1324.in_EJ_R_Off_road_F_Y


	  #Calculate the remaining industrial energy use and feedstock
    L1323.in_EJ_R_indenergy_F_Yh %>%
      complete(GCAM_region_ID, nesting(sector, fuel, year), fill = list(value = 0)) %>%
      rename(raw = value) %>%
      left_join(L1324.in_EJ_R_Off_road_F_Y %>%
                  filter(!grepl("feedstock", sector)) %>%
                  group_by(GCAM_region_ID, year, fuel) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel")) %>%
      ungroup() %>%
      mutate(value = replace_na(value, 0),
             value = raw - value ) ->
      L1324.in_EJ_R_indenergy_F_Yh_tmp

    L1322.in_EJ_R_indfeed_F_Yh %>%
      rename(raw = value) %>%
      left_join(L1324.in_EJ_R_Off_road_F_Y %>%
                  filter(grepl("feedstock", sector)) %>%
                  group_by(GCAM_region_ID, year, fuel) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel")) %>%
      ungroup() %>%
      mutate(value = replace_na(value, 0),
             value = raw - value ) ->
      L1323.in_EJ_R_indfeed_F_Yh

    L1324.in_EJ_R_indenergy_F_Yh_tmp %>%
      filter(value < 0) %>%
      mutate(value = NULL) ->
      indeergy_tmp

    L1323.in_EJ_R_indfeed_F_Yh %>%
      filter(value < 0) %>%
      mutate(value = NULL) ->
      indfeed_tmp

    #Adjust negative energy use
    L1324.in_EJ_R_Off_road_F_Y %>%
      filter(!grepl("feedstock", sector)) %>%
      left_join(indeergy_tmp %>% select(-sector) ,by = c("GCAM_region_ID", "fuel", "year"))  %>%
      mutate(raw = replace_na(raw, -1), value = if_else(raw < 0 , value, 0)) %>%
      select(GCAM_region_ID, fuel, year, sector, value) ->
      L1324.in_EJ_R_Off_road_F_Y_recal

    L1324.in_EJ_R_Off_road_F_Y %>%
      filter(grepl("feedstock", sector)) %>%
      left_join(indfeed_tmp %>% select(-sector), by = c("GCAM_region_ID", "fuel", "year"))  %>%
      mutate(raw = replace_na(raw,-1), value = if_else(raw < 0 , value, 0)) %>%
      select(GCAM_region_ID, fuel, year, sector, value) ->
      L1323.in_EJ_R_indfeed_F_Yh_recal

    #Recalculate
    L1323.in_EJ_R_indenergy_F_Yh %>%
      rename(raw = value) %>%
      left_join(L1324.in_EJ_R_Off_road_F_Y_recal %>%
                  group_by(GCAM_region_ID, year, fuel) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel")) %>%
      ungroup() %>%
      mutate(value = replace_na(value, 0),
             value = raw - value , raw = NULL) ->
      L1324.in_EJ_R_indenergy_F_Yh

    L1322.in_EJ_R_indfeed_F_Yh %>%
      rename(raw = value) %>%
      left_join(L1323.in_EJ_R_indfeed_F_Yh_recal %>%
                  group_by(GCAM_region_ID, year, fuel) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel")) %>%
      ungroup() %>%
      mutate(value = replace_na(value, 0),
             value = raw - value , raw = NULL) ->
      L1323.in_EJ_R_indfeed_F_Yh

    L1324.in_EJ_R_Off_road_F_Y_recal %>%
      bind_rows(L1323.in_EJ_R_indfeed_F_Yh_recal) %>%
      filter(fuel %in% c("electricity", "gas", "refined liquids", "biomass", "Hydrogen", "coal", "heat" )) ->
      L1324.in_EJ_R_Off_road_F_Y

    # ===================================================
    # Produce outputs
    L1324.in_EJ_R_Off_road_F_Y %>%
      add_title("Historical input energy use for the Off_road sector") %>%
      add_units("EJ") %>%
      add_comments("Obtained from IEA") %>%
      add_legacy_name("L1324.in_EJ_R_Off_road_F_Y") %>%
      add_precursors("L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "energy/A_regions",
                     "energy/mappings/enduse_fuel_aggregation") ->
      L1324.in_EJ_R_Off_road_F_Y

    L1324.in_EJ_R_indenergy_F_Yh %>%
      add_title("Adjusted historical input energy balances for industrial energy use") %>%
      add_units("Exajoules") %>%
      add_comments("Subtracted Off_road energy use from industrial energy use values in L1323.in_EJ_R_indenergy_F_Yh") %>%
      add_comments("To determine adjusted input energy for industrial energy use") %>%
      add_legacy_name("L1324.in_EJ_R_indenergy_F_Yh") %>%
      add_precursors("L1323.in_EJ_R_indenergy_F_Yh", "L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "energy/mappings/enduse_fuel_aggregation") ->
      L1324.in_EJ_R_indenergy_F_Yh

    L1323.in_EJ_R_indfeed_F_Yh %>%
      add_title("Adjusted historical input energy balances for industrial energy use") %>%
      add_units("Exajoules") %>%
      add_comments("Subtracted Off_road feedstock use from industrial feedstock use values in L1322.in_EJ_R_indfeed_F_Yh") %>%
      add_comments("To determine adjusted input feedstock for industrial feed use") %>%
      add_legacy_name("L1323.in_EJ_R_indfeed_F_Yh") %>%
      add_precursors("L1322.in_EJ_R_indfeed_F_Yh", "L1011.en_bal_EJ_R_Si_Fi_Yh", "energy/mappings/enduse_fuel_aggregation") ->
      L1323.in_EJ_R_indfeed_F_Yh

    return_data(L1324.in_EJ_R_Off_road_F_Y, L1324.in_EJ_R_indenergy_F_Yh, L1323.in_EJ_R_indfeed_F_Yh)

  } else {
    stop("Unknown command")
  }
}

