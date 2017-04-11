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
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author FF, April 2017
#' @export
module_energy_LA123.electricity <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/enduse_fuel_aggregation",
             FILE = "energy/A23.chp_elecratio",
             FILE = "energy/L1011.en_bal_EJ_R_Si_Fi_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L123.out_EJ_R_elec_F_Yh",
             "L123.in_EJ_R_elec_F_Yh",
             "L123.eff_R_elec_F_Yh",
             "L123.out_EJ_R_indchp_F_Yh",
             "L123.in_EJ_R_indchp_F_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]



    # Load required inputs
    enduse_fuel_aggregation <- get_data(all_data, "energy/enduse_fuel_aggregation")
    A23.chp_elecratio <- get_data(all_data, "energy/A23.chp_elecratio")
    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "energy/L1011.en_bal_EJ_R_Si_Fi_Yh")

    # ===================================================
    # TRANSLATED PROCESSING CODE GOES HERE...

    #Creates end use fuel for electricity to be used to create L123.in_EJ_R_elec_F_Yh
    enduse_fuel_aggregation%>%
      select(fuel,electricity)%>%
      filter(!is.na(electricity))->enduse_fuel_aggregation_electricity

    # Creates L123.in_EJ_R_elec_F_Yh based on L1011.en_bal_EJ_R_Si_Fi_Yh and enduse_fuel_aggregation_electricity
    # Calculates the inpunts by fuel (based on electricity input fuels), region ID and sector (electricity)
    L1011.en_bal_EJ_R_Si_Fi_Yh%>%
      filter(sector == "in_electricity generation")%>%
      mutate(sector = "electricity generation")%>%
      left_join(enduse_fuel_aggregation_electricity,by="fuel")%>%
      mutate(fuel=electricity)%>%
      select(-electricity)%>%
      filter(fuel %in% ELECTRICITY_INPUT_FUELS)%>%
      group_by(GCAM_region_ID, sector,fuel)%>%
      summarise_each(funs(sum))->L123.in_EJ_R_elec_F_Yh

    # Creates L123.out_EJ_R_elec_F_Yh based on L1011.en_bal_EJ_R_Si_Fi_Yh and enduse_fuel_aggregation_electricity
    # Calculates the electricity outputs by fuel, region ID and sector (electricity generation)
    L1011.en_bal_EJ_R_Si_Fi_Yh%>%
      filter(sector == "out_electricity generation")%>%
      mutate(sector = "electricity generation")%>%
      left_join(enduse_fuel_aggregation_electricity,by="fuel")%>%
      mutate(fuel=electricity)%>%
      select(-electricity)%>%
      group_by(GCAM_region_ID, sector,fuel)%>%
      summarise_each(funs(sum))%>%
      filter(!is.na(fuel))->L123.out_EJ_R_elec_F_Yh


    # Long form of L123.in_EJ_R_elec_F_Yh (previously created) to be used to calculate efficiencies
    # based on electricity inputs and outputs by fuel and year.
    L123.in_EJ_R_elec_F_Yh%>%
      gather(X_HISTORICAL_YEARS,input,-GCAM_region_ID,-sector,-fuel)->input_fuels_by_year_region

    # Calculates electricity generation efficiencies (L123.eff_R_elec_F_Yh) by fuel and year
    L123.out_EJ_R_elec_F_Yh%>%
      semi_join(L123.in_EJ_R_elec_F_Yh,by=c("GCAM_region_ID", "fuel"))%>%
      gather(X_HISTORICAL_YEARS,outputs,-GCAM_region_ID,-sector,-fuel)%>%
      left_join(input_fuels_by_year_region, by=c("GCAM_region_ID","sector","fuel","X_HISTORICAL_YEARS"))%>%
      mutate(eff=outputs/input)%>%
      select(-outputs,-input)%>%
      spread(X_HISTORICAL_YEARS,eff)->L123.eff_R_elec_F_Yh

    # Taking care of NA, 0, and INF values generatted in previous step (efficiency calculations) and updating
    # the efficiency output L123.eff_R_elec_F_Yh
    L123.eff_R_elec_F_Yh%>%
      mutate_at(funs(replace(., is.na(.), DEFAULT_ELECTRIC_EFFICIENCY)), .cols = X_HISTORICAL_YEARS)%>%
      mutate_at(funs(replace(., .==0, DEFAULT_ELECTRIC_EFFICIENCY)), .cols = X_HISTORICAL_YEARS)%>%
      mutate_at(funs(replace(., .==Inf, DEFAULT_ELECTRIC_EFFICIENCY)), .cols = X_HISTORICAL_YEARS)->L123.eff_R_elec_F_Yh

    # Tibble (Output_efficiency_based) created to adjust electricity outputs (L123.out_EJ_R_elec_F_Yh)
    # based on above modified efficiency calculations
    L123.in_EJ_R_elec_F_Yh%>%
      gather(X_HISTORICAL_YEARS,outputs,-GCAM_region_ID,-sector,-fuel)%>%
      left_join(gather(L123.eff_R_elec_F_Yh,X_HISTORICAL_YEARS,outputs,-GCAM_region_ID,-sector,-fuel),
                by=c("GCAM_region_ID","sector","fuel","X_HISTORICAL_YEARS"))%>%
      mutate(outputs_new=outputs.x*outputs.y)%>%
      select(-outputs.x,-outputs.y)->Output_efficiency_based

    # Adjusting electricity outputs based on new efficiencies
    L123.out_EJ_R_elec_F_Yh%>%
      gather(X_HISTORICAL_YEARS,outputs,-GCAM_region_ID,-sector,-fuel)%>%
      left_join(Output_efficiency_based,by=c("GCAM_region_ID","sector","fuel","X_HISTORICAL_YEARS"))%>%
      mutate(outputs =if_else(!is.na(outputs_new),outputs_new,outputs))%>%
      select(-outputs_new)%>%
      spread(X_HISTORICAL_YEARS,outputs)->L123.out_EJ_R_elec_F_Yh

    #2: CHP calculations

    # filter end use fuel in the industry sector and remove any NA generated.
    enduse_fuel_aggregation%>%
      select(fuel,industry)%>%
      filter(!is.na(industry))->enduse_fuel_aggregation_industry

    # Create Industry CHP estimates by fuel, region and industry sector.
    L1011.en_bal_EJ_R_Si_Fi_Yh%>%
      filter(sector == "out_chp_elec")%>%
      mutate(sector = "chp_elec")%>%
      left_join(enduse_fuel_aggregation_industry,by="fuel")%>%
      mutate(fuel=industry)%>%
      select(-industry)%>%
      mutate(fuel= replace(fuel,fuel== "heat",NA))%>%
      group_by(GCAM_region_ID, sector,fuel)%>%
      summarise_each(funs(sum))%>%
      filter(!is.na(fuel))->L123.out_EJ_R_indchp_F_Yh

    # Select electricity ratios by fuel to be used to estimate inputs for CHP by region and fuel.
    A23.chp_elecratio%>%
      select(elec_ratio,fuel)%>%
      full_join(L123.out_EJ_R_indchp_F_Yh, by = "fuel")%>%
      select(GCAM_region_ID,sector,fuel,elec_ratio)->elect_ratio_input_fuels


    # Estimates inputs for CHP in the industry sector based on outputs and the electricity ration by fuel (output/elec_ratio)
    L123.out_EJ_R_indchp_F_Yh%>%
      gather(X_HISTORICAL_YEARS,outputs,-GCAM_region_ID,-sector,-fuel)%>%
      left_join(elect_ratio_input_fuels, by=c("GCAM_region_ID","sector","fuel"))%>%
      mutate(outputs_ratio=outputs/elec_ratio)%>%
      select(-outputs,-elec_ratio)%>%
      spread(X_HISTORICAL_YEARS,outputs_ratio)->L123.in_EJ_R_indchp_F_Yh

    # NOTE: there are 'match' calls in this code. You probably want to use left_join_error_no_match
    # For more information, see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # NOTE: This code uses vecpaste
    # This function can be removed; see https://github.com/JGCRI/gcamdata/wiki/Name-That-Function
    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    L123.out_EJ_R_elec_F_Yh %>%
      add_title("Outputs of electricity sector by GCAM region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Written by LA123.electricity.R") %>%
      add_legacy_name("L123.out_EJ_R_elec_F_Yh") %>%
      add_precursors("energy/enduse_fuel_aggregation", "energy/A23.chp_elecratio") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST) ->
      L123.out_EJ_R_elec_F_Yh
    L123.in_EJ_R_elec_F_Yh %>%
      add_title("Inputs to electricity sector by GCAM region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Written by LA123.electricity.R") %>%
      add_legacy_name("L123.in_EJ_R_elec_F_Yh") %>%
      add_precursors("energy/enduse_fuel_aggregation", "energy/A23.chp_elecratio") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST) ->
      L123.in_EJ_R_elec_F_Yh
    L123.eff_R_elec_F_Yh %>%
      add_title("Electric sector efficiencies by GCAM region / fuel / historical year") %>%
      add_units("Unitless") %>%
      add_comments("Written by LA123.electricity.R") %>%
      add_legacy_name("L123.eff_R_elec_F_Yh") %>%
      add_precursors("energy/enduse_fuel_aggregation", "energy/A23.chp_elecratio") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST) ->
      L123.eff_R_elec_F_Yh
    L123.out_EJ_R_indchp_F_Yh %>%
      add_title("Industrial CHP electricity generation by GCAM region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("Written by LA123.electricity.R") %>%
      add_legacy_name("L123.out_EJ_R_indchp_F_Yh") %>%
      add_precursors("energy/enduse_fuel_aggregation", "energy/A23.chp_elecratio") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST) ->
      L123.out_EJ_R_indchp_F_Yh
    L123.in_EJ_R_indchp_F_Yh %>%
      add_title("Inputs to industrial CHP by GCAM region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("Written by LA123.electricity.R") %>%
      add_legacy_name("L123.in_EJ_R_indchp_F_Yh") %>%
      add_precursors("energy/enduse_fuel_aggregation", "energy/A23.chp_elecratio") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_NO_TEST) ->
      L123.in_EJ_R_indchp_F_Yh

    return_data(L123.out_EJ_R_elec_F_Yh, L123.in_EJ_R_elec_F_Yh, L123.eff_R_elec_F_Yh, L123.out_EJ_R_indchp_F_Yh, L123.in_EJ_R_indchp_F_Yh)
  } else {
    stop("Unknown command")
  }
}
