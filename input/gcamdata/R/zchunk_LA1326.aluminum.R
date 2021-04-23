# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA1326.aluminum
#'
#' Sets up input, output, and IO coefficients for aluminum and subtracts input energy from industry energy use
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1326.in_EJ_R_aluminum_Yh}, \code{L1326.IO_GJkg_R_aluminum_F_Yh},\code{L1326.out_Mt_R_aluminum_Yh}, \code{L1326.in_EJ_R_indenergy_F_Yh}. The corresponding file in the
#' @details This chunk generates input, Energy inputs are then subtracted from industrial energy use, feedstock and any resulting negative values
#' are dealt with by moving their accounting to the aluminum sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by left_join mutate select semi_join summarise summarise_all
#' @importFrom tidyr gather spread
#' @author Yang Liu Sep 2019
module_energy_LA1326.aluminum <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/en_aluminum",
             FILE = "energy/A_regions",
             FILE = "energy/mappings/enduse_fuel_aggregation",
             FILE = "energy/aluminum_prod",
             "L1325.in_EJ_R_indenergy_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1326.in_EJ_R_aluminum_Yh",
             "L1326.IO_GJkg_R_aluminum_F_Yh",
             "L1326.in_EJ_R_indenergy_F_Yh",
             "L1326.out_Mt_R_aluminum_Yh"))
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    raw <- subsector <- minicam.energy.input <- Country <- sector <-
    share <- value <- aluminum <- year <- value.y <- value.x <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    A_regions <- get_data(all_data, "energy/A_regions")
    en_aluminum <- get_data(all_data, "energy/en_aluminum")
    aluminum_prod <- get_data(all_data, "energy/aluminum_prod", strip_attributes = TRUE)
	  enduse_fuel_aggregation <- get_data(all_data, "energy/mappings/enduse_fuel_aggregation")
    L1325.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1325.in_EJ_R_indenergy_F_Yh", strip_attributes = TRUE)

    # ===================================================
    # 2. Perform computations

    # Set constants used for this chunk
    # ---------------------------------
    # Determine historical years not available in data set (additional years) to copy values from final available year (final_CO2_year)

    # ===================================================

    # Aluminum energy input
    en_aluminum %>%
      mutate(sector = "Aluminum", value = value, unit = NULL) ->
      L1326.in_EJ_R_aluminum_Yh

    #Aluminum production
    aluminum_prod %>%
      mutate(sector = flow, value = value, unit_prod = NULL, flow = NULL) ->
      L1326.out_Mt_R_aluminum_Yh


    #Mapping the fuel used in aluminum sector
    L1326.in_EJ_R_aluminum_Yh %>%
      left_join(select(enduse_fuel_aggregation, fuel, industry), by = "fuel") %>%
      select(-fuel, fuel = industry) %>%
      na.omit() %>%
      group_by(region, GCAM_region_ID, year, sector, fuel) %>%
      summarise(value = sum(value)) %>%
      ungroup()->
      L1326.in_EJ_R_aluminum_Yh


    L1326.in_EJ_R_aluminum_Yh  %>%
      group_by(region, GCAM_region_ID, year, sector, fuel) %>%
      summarise(value = sum(value)) %>%
      ungroup ->
      L1326.in_EJ_R_aluminum_Yh

	  #Calculate the remaining industrial energy use and feedstock
    L1325.in_EJ_R_indenergy_F_Yh %>%
      complete(GCAM_region_ID,nesting(sector, fuel, year), fill = list(value = 0)) %>%
      rename(raw = value) %>%
      left_join(L1326.in_EJ_R_aluminum_Yh %>%
                  group_by(GCAM_region_ID, year, fuel) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel")) %>%
      ungroup() %>%
      mutate(value = replace_na(value, 0)) %>%
      mutate(value = raw - value , raw = NULL) ->
      L1326.in_EJ_R_indenergy_F_Yh_tmp


    L1326.in_EJ_R_indenergy_F_Yh_tmp %>%
      filter(value < 0) %>%
      mutate(value = if_else(value > 0 , value, 0)) ->
      indeergy_tmp

    #Adjust negative energy use
    L1326.in_EJ_R_aluminum_Yh %>%
      filter(sector == "Aluminum") %>%
      left_join(indeergy_tmp %>% select(-sector), by = c("GCAM_region_ID", "fuel", "year"))  %>%
      mutate(value.y = replace_na(value.y, -1) ,value = if_else(value.y < 0 , value.x, 0)) %>%
      select(region, GCAM_region_ID, fuel, year, sector, value) ->
      L1326.in_EJ_R_aluminum_Yh_recal


    #Recalculate
    L1325.in_EJ_R_indenergy_F_Yh %>%
      rename(raw = value) %>%
      left_join(L1326.in_EJ_R_aluminum_Yh_recal %>%
                  group_by(GCAM_region_ID, year, fuel) %>%
                  summarise(value = sum(value)), by = c("GCAM_region_ID", "year", "fuel")) %>%
      ungroup() %>%
      mutate(value = replace_na(value, 0)) %>%
      mutate(value = raw - value, raw = NULL) ->
      L1326.in_EJ_R_indenergy_F_Yh


    L1326.in_EJ_R_aluminum_Yh_recal %>%
      filter(fuel %in% c("electricity", "gas", "refined liquids", "biomass", "Hydrogen", "coal", "heat" )) %>%
      mutate(sector = if_else(fuel == "electricity", "Aluminum", "Alumina")) ->
      L1326.in_EJ_R_aluminum_Yh

    #Calculate coefficients
    L1326.in_EJ_R_aluminum_Yh %>%
      rename(input = value) %>%
      left_join(L1326.out_Mt_R_aluminum_Yh %>% rename(output = value), by = c("region", "GCAM_region_ID", "year", "sector")) %>%
      mutate(value = if_else(output > 0, input / output, 0)) ->
      L1326.IO_GJkg_R_aluminum_F_Yh_tmp #It's not the final coefficients.Need scale


    L1326.IO_GJkg_R_aluminum_F_Yh_tmp %>%
      filter(fuel != "electricity") %>%
      left_join(L1326.IO_GJkg_R_aluminum_F_Yh_tmp %>% filter(fuel != "electricity", value > 0) %>%
                  group_by(region, sector, year) %>% summarise(sum = sum(output)), by = c("region", "year", "sector")) %>%
      mutate(value = sum / output * value, sum = NULL) %>%
      bind_rows(L1326.IO_GJkg_R_aluminum_F_Yh_tmp %>% filter(fuel == "electricity")) %>%
      mutate(value = replace_na(value, 0), input = NULL, output =NULL) ->
      L1326.IO_GJkg_R_aluminum_F_Yh


    # =======================================================
    # Produce outputs
    L1326.in_EJ_R_aluminum_Yh %>%
      add_title("Historical input energy use for the aluminum sector") %>%
      add_units("EJ") %>%
      add_comments("Obtained from IEA") %>%
      add_legacy_name("L1326.in_EJ_R_aluminum_Yh") %>%
      add_precursors("energy/A_regions",
                     "energy/en_aluminum",
                     "energy/aluminum_prod",
                     "energy/mappings/enduse_fuel_aggregation") ->
      L1326.in_EJ_R_aluminum_Yh

    L1326.out_Mt_R_aluminum_Yh %>%
      add_title("Historical aluminum production by region, fuel, and year") %>%
      add_units("Mt") %>%
      add_comments("Outputs are collecting from World aluminum association and then aggregating to GCAM regions") %>%
      add_legacy_name("L1326.out_Mt_R_aluminum_Yh") %>%
      add_precursors( "energy/A_regions",
                      "energy/en_aluminum",
                      "energy/aluminum_prod",
                      "energy/mappings/enduse_fuel_aggregation") ->
      L1326.out_Mt_R_aluminum_Yh


    L1326.IO_GJkg_R_aluminum_F_Yh%>%
      add_title("Input-output coefficients for aluminum production") %>%
      add_units("EJ/Mt") %>%
      add_comments("Calculated by input/output") %>%
      add_legacy_name("L1326.IO_GJkg_R_aluminum_F_Yh") %>%
      add_precursors("energy/A_regions",
                     "energy/en_aluminum",
                     "energy/aluminum_prod",
                     "energy/mappings/enduse_fuel_aggregation") ->
      L1326.IO_GJkg_R_aluminum_F_Yh

    L1326.in_EJ_R_indenergy_F_Yh %>%
      add_title("Adjusted historical input energy balances for industrial energy use") %>%
      add_units("Exajoules") %>%
      add_comments("Subtracted aluminum energy use from industrial energy use values in L1325.in_EJ_R_indenergy_F_Yh") %>%
      add_comments("To determine adjusted input energy for industrial energy use") %>%
      add_legacy_name("L1326.in_EJ_R_indenergy_F_Yh") %>%
      add_precursors("L1325.in_EJ_R_indenergy_F_Yh", "energy/en_aluminum","energy/aluminum_prod","energy/mappings/enduse_fuel_aggregation") ->
      L1326.in_EJ_R_indenergy_F_Yh


    return_data(L1326.in_EJ_R_aluminum_Yh, L1326.IO_GJkg_R_aluminum_F_Yh ,L1326.out_Mt_R_aluminum_Yh,L1326.in_EJ_R_indenergy_F_Yh)

  } else {
    stop("Unknown command")
  }
}

