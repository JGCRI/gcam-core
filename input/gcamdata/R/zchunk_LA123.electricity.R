# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA123.electricity
#'
#' This script creates electricity generation and inputs by fuel, region and historical year. Estimates are adjusted by efficiency factors.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L123.out_EJ_R_elec_F_Yh}, \code{L123.in_EJ_R_elec_F_Yh}, \code{L123.eff_R_elec_F_Yh}, \code{L123.out_EJ_R_indchp_F_Yh}, \code{L123.in_EJ_R_indchp_F_Yh}. The corresponding file in the
#' original data system was \code{LA123.electricity.R} (energy level1).
#' @details This script creates electricity generation and inputs by fuel, region and historical year. Estimates are adjusted by efficiency factors.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter full_join if_else group_by left_join mutate select semi_join summarise summarise_all
#' @author FF April 2017
module_energy_LA123.electricity <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/mappings/enduse_fuel_aggregation",
             FILE = "energy/A23.chp_elecratio",
             "L1012.en_bal_EJ_R_Si_Fi_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L123.out_EJ_R_elec_F_Yh",
             "L123.in_EJ_R_elec_F_Yh",
             "L123.eff_R_elec_F_Yh",
             "L123.out_EJ_R_indchp_F_Yh",
             "L123.in_EJ_R_indchp_F_Yh"))
  } else if(command == driver.MAKE) {

    year <- value <- GCAM_region_ID <- fuel <- sector <- electricity <-
        outputs <- input <- outputs_new <- industry <- elec_ratio <-
        outputs_ratio <- NULL           # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    enduse_fuel_aggregation <- get_data(all_data, "energy/mappings/enduse_fuel_aggregation")
    A23.chp_elecratio <- get_data(all_data, "energy/A23.chp_elecratio")
    L1012.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1012.en_bal_EJ_R_Si_Fi_Yh")


    # ===================================================

    # Creates end use fuel for electricity to be used to create L123.in_EJ_R_elec_F_Yh
    enduse_fuel_aggregation %>%
      select(fuel,electricity) %>%
      filter(!is.na(electricity)) ->
      enduse_fuel_aggregation_electricity

    # Creates L123.in_EJ_R_elec_F_Yh based on L1012.en_bal_EJ_R_Si_Fi_Yh and enduse_fuel_aggregation_electricity
    # Calculates the inpunts by fuel (based on electricity input fuels), region ID and sector (electricity)
    L1012.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "in_electricity generation") %>%
      mutate(sector = "electricity generation") %>%
      left_join(enduse_fuel_aggregation_electricity, by = "fuel") %>%
      mutate(fuel = electricity, input = value) %>%
      select(-electricity, -value) %>%
      filter(fuel %in% energy.ELECTRICITY_INPUT_FUELS) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise_all(sum) %>%
      ungroup() ->
      L123.in_EJ_R_elec_F_Yh

    # Creates L123.out_EJ_R_elec_F_Yh based on L1012.en_bal_EJ_R_Si_Fi_Yh and enduse_fuel_aggregation_electricity
    # Calculates the electricity outputs by fuel, region ID and sector (electricity generation)
    L1012.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "out_electricity generation") %>%
      mutate(sector = "electricity generation") %>%
      left_join(enduse_fuel_aggregation_electricity, by = "fuel") %>%
      mutate(fuel = electricity, outputs = value) %>%
      select(-electricity, -value) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise_all(sum) %>%
      ungroup() %>%
      filter(!is.na(fuel)) ->
      L123.out_EJ_R_elec_F_Yh

    # Long form of L123.in_EJ_R_elec_F_Yh (previously created) to be used to calculate efficiencies
    # based on electricity inputs and outputs by fuel and year.

    # Calculates electricity generation efficiencies (L123.eff_R_elec_F_Yh) by region, fuel and year
    L123.out_EJ_R_elec_F_Yh %>%
      semi_join(L123.in_EJ_R_elec_F_Yh, by = c("GCAM_region_ID", "fuel", "year")) %>%
      left_join(L123.in_EJ_R_elec_F_Yh, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(value = outputs / input) %>%
      select(-outputs, -input) ->
      L123.eff_R_elec_F_Yh

    # Taking care of NA, 0, and INF values generatted in previous step (efficiency calculations) and updating
    # the efficiency output L123.eff_R_elec_F_Yh
    L123.eff_R_elec_F_Yh %>%
      mutate(value = if_else(!is.na(value), value, energy.DEFAULT_ELECTRIC_EFFICIENCY),
             value = if_else(value == 0, energy.DEFAULT_ELECTRIC_EFFICIENCY, value),
             value = if_else(is.infinite(value), energy.DEFAULT_ELECTRIC_EFFICIENCY, value)) ->
      L123.eff_R_elec_F_Yh

    # Tibble (Output_efficiency_based) created to adjust electricity outputs (L123.out_EJ_R_elec_F_Yh)
    # based on above modified efficiency calculations

    L123.in_EJ_R_elec_F_Yh %>%
      left_join(L123.eff_R_elec_F_Yh, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(outputs_new = input * value) %>%
      select(-input, -value) ->
      Output_efficiency_based

    # Adjusting electricity outputs based on new efficiencies
    L123.out_EJ_R_elec_F_Yh %>%
      left_join(Output_efficiency_based, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(outputs = if_else(!is.na(outputs_new), outputs_new, outputs)) %>%
      select(-outputs_new) ->
      L123.out_EJ_R_elec_F_Yh

    #2: CHP calculations

    # filter end use fuel in the industry sector and remove any NA generated.
    enduse_fuel_aggregation %>%
      select(fuel, industry) %>%
      filter(!is.na(industry)) ->
      enduse_fuel_aggregation_industry

    # Create Industry CHP estimates by fuel, region and industry sector.
    L1012.en_bal_EJ_R_Si_Fi_Yh %>%
      filter(sector == "out_chp_elec") %>%
      mutate(sector = "chp_elec") %>%
      left_join(enduse_fuel_aggregation_industry, by = "fuel") %>%
      mutate(fuel = industry) %>%
      select(-industry) %>%
      mutate(fuel = replace(fuel, fuel == "heat", NA),
             fuel = replace(fuel, fuel == "electricity", NA)) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise_all(sum) %>%
      ungroup() %>%
      filter(!is.na(fuel)) ->
      L123.out_EJ_R_indchp_F_Yh

    # Estimates inputs for CHP in the industry sector based on outputs and the electricity ration by fuel (output/elec_ratio)
    L123.out_EJ_R_indchp_F_Yh %>%
      full_join(filter(A23.chp_elecratio,fuel != "hydrogen"), by = "fuel") %>%
      mutate(outputs_ratio = value / elec_ratio) %>%
      select(-value, -elec_ratio) ->
      L123.in_EJ_R_indchp_F_Yh

    # Rename necessary outputs to value for long test flag
    L123.out_EJ_R_elec_F_Yh %>%
      mutate(value = outputs) %>%
      select(-outputs) ->
      L123.out_EJ_R_elec_F_Yh

    L123.in_EJ_R_elec_F_Yh %>%
      mutate(value = input) %>%
      select(-input) ->
      L123.in_EJ_R_elec_F_Yh

    L123.in_EJ_R_indchp_F_Yh %>%
      mutate(value = outputs_ratio) %>%
      select(-outputs_ratio) ->
      L123.in_EJ_R_indchp_F_Yh

    # Save results
    L123.out_EJ_R_elec_F_Yh %>%
      add_title("Outputs of electricity sector by GCAM region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Written by LA123.electricity.R") %>%
      add_legacy_name("L123.out_EJ_R_elec_F_Yh") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh",
                     "energy/mappings/enduse_fuel_aggregation", "energy/A23.chp_elecratio") ->
      L123.out_EJ_R_elec_F_Yh

    L123.in_EJ_R_elec_F_Yh %>%
      add_title("Inputs to electricity sector by GCAM region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Written by LA123.electricity.R") %>%
      add_legacy_name("L123.in_EJ_R_elec_F_Yh") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh",
                     "energy/mappings/enduse_fuel_aggregation", "energy/A23.chp_elecratio") ->
      L123.in_EJ_R_elec_F_Yh

    L123.eff_R_elec_F_Yh %>%
      add_title("Electric sector efficiencies by GCAM region / fuel / historical year") %>%
      add_units("Unitless") %>%
      add_comments("Written by LA123.electricity.R") %>%
      add_legacy_name("L123.eff_R_elec_F_Yh") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh",
                     "energy/mappings/enduse_fuel_aggregation", "energy/A23.chp_elecratio") ->
      L123.eff_R_elec_F_Yh

    L123.out_EJ_R_indchp_F_Yh %>%
      add_title("Industrial CHP electricity generation by GCAM region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Written by LA123.electricity.R") %>%
      add_legacy_name("L123.out_EJ_R_indchp_F_Yh") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh",
                     "energy/mappings/enduse_fuel_aggregation", "energy/A23.chp_elecratio") ->
      L123.out_EJ_R_indchp_F_Yh

    L123.in_EJ_R_indchp_F_Yh %>%
      add_title("Inputs to industrial CHP by GCAM region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Written by LA123.electricity.R") %>%
      add_legacy_name("L123.in_EJ_R_indchp_F_Yh") %>%
      add_precursors("L1012.en_bal_EJ_R_Si_Fi_Yh",
                     "energy/mappings/enduse_fuel_aggregation", "energy/A23.chp_elecratio") ->
      L123.in_EJ_R_indchp_F_Yh

    return_data(L123.out_EJ_R_elec_F_Yh, L123.in_EJ_R_elec_F_Yh, L123.eff_R_elec_F_Yh, L123.out_EJ_R_indchp_F_Yh, L123.in_EJ_R_indchp_F_Yh)
  } else {
    stop("Unknown command")
  }
}
