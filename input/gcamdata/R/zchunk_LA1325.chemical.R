# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA1325.chemical
#'
#' Sets up input, output, and IO coefficients for chemical and subtracts input energy from industry energy use
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1325.in_EJ_R_chemical_F_Y}, \code{L1325.in_EJ_R_indenergy_F_Yh}, \code{L1324.in_EJ_R_indfeed_F_Yh}. The corresponding file in the
#' @details This chunk generates input, Energy inputs are then subtracted from industrial energy use, feedstock and any resulting negative values
#' are dealt with by moving their accounting to the chemical sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by left_join mutate select semi_join summarise summarise_all
#' @importFrom tidyr gather spread
#' @author Yang Liu Sep 2019
module_energy_LA1325.chemical <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L1011.en_bal_EJ_R_Si_Fi_Yh",
			       FILE = "energy/A_regions",
             FILE = "energy/mappings/enduse_fuel_aggregation",
			       "L1321.in_EJ_R_indenergy_F_Yh",
			       "L132.in_EJ_R_indfeed_F_Yh",
			       "L1322.in_EJ_R_indenergy_F_Yh",
			       "L1322.in_EJ_R_indfeed_F_Yh",
             "L1324.in_EJ_R_indenergy_F_Yh",
			       "L1323.in_EJ_R_indfeed_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1325.in_EJ_R_chemical_F_Y",
             "L1325.in_EJ_R_indenergy_F_Yh",
             "L1324.in_EJ_R_indfeed_F_Yh"))
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    raw <- subsector <- minicam.energy.input <- Country <- sector <-
      share <- value <- chemical <- year <- value.y <- value.x <- GCAM_region_ID <-
      `chemical energy use` <- `chemical feedstocks` <- fuel <- industry <- has_district_heat <-
      value_ori <- value_new <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    A_regions <- get_data(all_data, "energy/A_regions")
    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1011.en_bal_EJ_R_Si_Fi_Yh", strip_attributes = TRUE)
    enduse_fuel_aggregation <- get_data(all_data, "energy/mappings/enduse_fuel_aggregation")
    L1324.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1324.in_EJ_R_indenergy_F_Yh", strip_attributes = TRUE)
    L1323.in_EJ_R_indfeed_F_Yh <- get_data(all_data, "L1323.in_EJ_R_indfeed_F_Yh", strip_attributes = TRUE)

    L1321.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1321.in_EJ_R_indenergy_F_Yh")
    L132.in_EJ_R_indfeed_F_Yh <- get_data(all_data, "L132.in_EJ_R_indfeed_F_Yh")
    L1322.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indenergy_F_Yh")
    L1322.in_EJ_R_indfeed_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indfeed_F_Yh")
    # ===================================================
    # 2. Perform computations

    # Set constants used for this chunk
    # ---------------------------------
    # Determine historical years not available in data set (additional years) to copy values from final available year (final_CO2_year)

    # ===================================================

    # Construction energy and feedstock input
    L1011.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(grepl("chemical", sector)) ->
      L1325.in_EJ_R_chemical_F_Y

    # fix regions that only have feedstock use
    L1325.in_EJ_R_chemical_F_Y %>%
      group_by(GCAM_region_ID, year, sector) %>%
      summarise(value = sum(value)) %>%
      spread(sector, value) %>%
      filter(`chemical energy use` == 0 & `chemical feedstocks` > 0) %>%
      ungroup() %>%
      select(GCAM_REGION_ID, year) ->
      regions_feedstock_only

    L1325.in_EJ_R_chemical_F_Y %>%
      anti_join(regions_feedstock_only, by = c("GCAM_region_ID", "year")) ->
      L1325.in_EJ_R_chemical_F_Y

    #Mapping the fuel used in chemical sector
    L1325.in_EJ_R_chemical_F_Y %>%
      left_join(select(enduse_fuel_aggregation, fuel, industry), by = "fuel") %>%
      select(-fuel, fuel = industry) %>%
      na.omit() %>%
      group_by(GCAM_region_ID, year, sector, fuel) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      L1325.in_EJ_R_chemical_F_Y

    ## Heat: drop heat in regions where heat is not modeled as a final fuel
    A_regions %>%
      filter(has_district_heat == 0) %>%
      select(GCAM_region_ID) %>%
      unique %>%
      mutate(fuel = "heat") ->
      region_heat

    L1325.in_EJ_R_chemical_F_Y  %>%
      anti_join(region_heat, by = c("GCAM_region_ID", "fuel")) %>% # then drop the regions selected in region_heat
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup ->
      L1325.in_EJ_R_chemical_F_Y_raw

    #Minus fertilizer energy and feedstock
    L1325.in_EJ_R_chemical_F_Y_raw %>%
      filter(grepl("energy", sector), value > 0) %>%
      left_join_error_no_match(L1321.in_EJ_R_indenergy_F_Yh%>%
                                 group_by(GCAM_region_ID, year, fuel) %>%
                                 summarise(value_ori = sum(value)),
                                           by = c("GCAM_region_ID",  "fuel", "year")) %>%
      left_join_error_no_match(L1322.in_EJ_R_indenergy_F_Yh%>%
                                 group_by(GCAM_region_ID, year, fuel) %>%
                                 summarise(value_new = sum(value)),
                               by = c("GCAM_region_ID",  "fuel", "year")) %>%
      mutate(value = value - value_ori + value_new,
             value =if_else(value > 0 , value, 0)) %>%
      select(-value_ori, -value_new) ->
      L1325.in_EJ_R_chemical_F_Y_CHEMICAL

    L1325.in_EJ_R_chemical_F_Y_raw %>%
      filter(grepl("feedstock", sector)) %>%
      left_join_error_no_match(L132.in_EJ_R_indfeed_F_Yh%>%
                                 group_by(GCAM_region_ID, year, fuel) %>%
                                 summarise(value_ori = sum(value)),
                               by = c("GCAM_region_ID",  "fuel", "year")) %>%
      left_join_error_no_match(L1322.in_EJ_R_indfeed_F_Yh%>%
                                 group_by(GCAM_region_ID, year, fuel) %>%
                                 summarise(value_new = sum(value)),
                               by = c("GCAM_region_ID",  "fuel", "year")) %>%
      mutate(value = value - value_ori + value_new,
             value =if_else(value > 0 , value, 0)) %>%
      select(-value_ori, -value_new) ->
      L1325.in_EJ_R_chemical_F_Y_NECHEM

    L1325.in_EJ_R_chemical_F_Y_CHEMICAL %>%
      bind_rows(L1325.in_EJ_R_chemical_F_Y_NECHEM) ->
      L1325.in_EJ_R_chemical_F_Y

	  #Calculate the remaining industrial energy use and feedstock
    L1324.in_EJ_R_indenergy_F_Yh %>%
      complete(GCAM_region_ID, nesting(sector, fuel, year), fill = list(value = 0)) %>%
      rename(raw = value) %>%
      left_join(L1325.in_EJ_R_chemical_F_Y %>%
                  filter(grepl("energy", sector)) %>%
                  group_by(GCAM_region_ID, year, fuel) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel")) %>%
      ungroup() %>%
      mutate(value = replace_na(value, 0),
             value = raw - value ) ->
      L1325.in_EJ_R_indenergy_F_Yh_tmp

    L1323.in_EJ_R_indfeed_F_Yh %>%
      rename(raw = value) %>%
      left_join(L1325.in_EJ_R_chemical_F_Y %>%
                  filter(grepl("feedstock", sector)) %>%
                  group_by(GCAM_region_ID, year, fuel) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel")) %>%
      ungroup() %>%
      mutate(value = replace_na(value, 0),
             value = raw - value ) ->
      L1324.in_EJ_R_indfeed_F_Yh

    L1325.in_EJ_R_indenergy_F_Yh_tmp %>%
      filter(value < 0) %>%
      mutate(value = NULL) ->
      indenergy_tmp

    L1324.in_EJ_R_indfeed_F_Yh %>%
      filter(value < 0) %>%
      mutate(value = NULL) ->
      indfeed_tmp

    #Adjust negative energy use
    L1325.in_EJ_R_chemical_F_Y %>%
      filter(grepl("energy", sector)) %>%
      left_join(indenergy_tmp %>% select(-sector),by = c("GCAM_region_ID", "fuel", "year"))  %>%
      mutate(raw =replace_na(raw, -1) ,value = if_else(raw < 0 , value, 0)) %>%
      select(GCAM_region_ID, fuel, year, sector, value) ->
      L1325.in_EJ_R_chemical_F_Y_recal

    L1325.in_EJ_R_chemical_F_Y %>%
      filter(grepl("feedstock", sector)) %>%
      left_join(indfeed_tmp %>% select(-sector),by = c("GCAM_region_ID", "fuel", "year"))  %>%
      mutate(raw = replace_na(raw, -1) ,value = if_else(raw < 0 , value, 0)) %>%
      select(GCAM_region_ID, fuel, year, sector, value) ->
      L1325.in_EJ_R_indfeed_F_Yh_recal

    #Recalculate
    L1324.in_EJ_R_indenergy_F_Yh %>%
      rename(raw = value) %>%
      left_join(L1325.in_EJ_R_chemical_F_Y_recal %>%
                  group_by(GCAM_region_ID, year, fuel) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel")) %>%
      ungroup() %>%
      mutate(value = replace_na(value, 0),
             value = raw - value , raw = NULL) ->
      L1325.in_EJ_R_indenergy_F_Yh

    L1323.in_EJ_R_indfeed_F_Yh %>%
      rename(raw = value) %>%
      left_join(L1325.in_EJ_R_indfeed_F_Yh_recal %>%
                  group_by(GCAM_region_ID, year, fuel) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel")) %>%
      ungroup() %>%
      mutate(value = replace_na(value, 0),
             value = raw - value , raw = NULL) ->
      L1324.in_EJ_R_indfeed_F_Yh

    L1325.in_EJ_R_chemical_F_Y_recal %>%
      bind_rows(L1325.in_EJ_R_indfeed_F_Yh_recal) ->
      L1325.in_EJ_R_chemical_F_Y

    # ===================================================
    # Produce outputs
    L1325.in_EJ_R_chemical_F_Y %>%
      add_title("Historical input energy use for the chemical sector") %>%
      add_units("EJ") %>%
      add_comments("Obtained from IEA") %>%
      add_legacy_name("L1325.in_EJ_R_chemical_F_Y") %>%
      add_precursors("energy/A_regions",
					           "L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "energy/mappings/enduse_fuel_aggregation") ->
      L1325.in_EJ_R_chemical_F_Y

    L1325.in_EJ_R_indenergy_F_Yh %>%
      add_title("Adjusted historical input energy balances for industrial energy use") %>%
      add_units("Exajoules") %>%
      add_comments("Subtracted chemical energy use from industrial energy use values in L1324.in_EJ_R_indenergy_F_Yh") %>%
      add_comments("To determine adjusted input energy for industrial energy use") %>%
      add_legacy_name("L1325.in_EJ_R_indenergy_F_Yh") %>%
      add_precursors("L1324.in_EJ_R_indenergy_F_Yh","L1321.in_EJ_R_indenergy_F_Yh",
                     "L1322.in_EJ_R_indenergy_F_Yh","L1011.en_bal_EJ_R_Si_Fi_Yh","energy/mappings/enduse_fuel_aggregation") ->
      L1325.in_EJ_R_indenergy_F_Yh

    L1324.in_EJ_R_indfeed_F_Yh %>%
      add_title("Adjusted historical input feedstock balances for industrial energy use") %>%
      add_units("Exajoules") %>%
      add_comments("Subtracted chemical feedstock use from industrial feedstock use values in L1324.in_EJ_R_indfeed_F_Yh") %>%
      add_comments("To determine adjusted input feedstock for industrial feed use") %>%
      add_legacy_name("L1324.in_EJ_R_indfeed_F_Yh") %>%
      add_precursors("L1323.in_EJ_R_indfeed_F_Yh","L1322.in_EJ_R_indfeed_F_Yh", "L132.in_EJ_R_indfeed_F_Yh","L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "energy/mappings/enduse_fuel_aggregation") ->
      L1324.in_EJ_R_indfeed_F_Yh

    return_data(L1325.in_EJ_R_chemical_F_Y, L1325.in_EJ_R_indenergy_F_Yh, L1324.in_EJ_R_indfeed_F_Yh)

  } else {
    stop("Unknown command")
  }
}

