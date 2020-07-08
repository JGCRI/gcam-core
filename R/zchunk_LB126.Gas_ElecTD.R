# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcam.usa_LB126.Gas_ElecTD
#'
#' Calculates inputs and outputs of: gas processing by fuel and state, gas pipeline by state, and transmission and distribution of electricity by state.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L126.out_EJ_state_pipeline_gas}, \code{L126.in_EJ_state_pipeline_gas}, \code{L126.out_EJ_state_gasproc_F}, \code{L126.in_EJ_state_gasproc_F}, \code{L126.out_EJ_state_td_elec}, \code{L126.in_EJ_state_td_elec}. The corresponding file in the
#' original data system was \code{LB126.Gas_ElecTD.R} (gcam-usa level1).
#' @details Calculates inputs and outputs of: gas processing by fuel and state, gas pipeline by state, and transmission and distribution of electricity by state.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by left_join mutate select summarise transmute
#' @author RLH September 2017
module_gcamusa_LB126.Gas_ElecTD <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L122.in_EJ_R_gasproc_F_Yh",
              "L122.out_EJ_R_gasproc_F_Yh",
              "L126.in_EJ_R_gaspipe_F_Yh",
              "L126.out_EJ_R_gaspipe_F_Yh",
              "L126.IO_R_electd_F_Yh",
              "L101.EIA_use_all_Bbtu",
              "L101.inEIA_EJ_state_S_F",
              "L122.in_EJ_state_refining_F",
              "L123.out_EJ_state_elec_F",
              "L132.in_EJ_state_indchp_F",
              "L132.in_EJ_state_indfeed_F",
              "L132.in_EJ_state_indnochp_F",
              "L1321.in_EJ_state_cement_F_Y",
              "L1322.in_EJ_state_Fert_Yh",
              "L142.in_EJ_state_bld_F",
              "L154.in_EJ_state_trn_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L126.out_EJ_state_pipeline_gas",
             "L126.in_EJ_state_pipeline_gas",
             "L126.out_EJ_state_gasproc_F",
             "L126.in_EJ_state_gasproc_F",
             "L126.out_EJ_state_td_elec",
             "L126.in_EJ_state_td_elec"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    GCAM_region_ID <- year <- value <- value.x <- value.y <- sector <- fuel <- state <- EIA_sector <-
      EIA_fuel <- sector.x <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L122.in_EJ_R_gasproc_F_Yh <- get_data(all_data, "L122.in_EJ_R_gasproc_F_Yh") %>%
      filter(GCAM_region_ID == gcam.USA_CODE) %>%
      select(-GCAM_region_ID)
    L122.out_EJ_R_gasproc_F_Yh <- get_data(all_data, "L122.out_EJ_R_gasproc_F_Yh") %>%
      filter(GCAM_region_ID == gcam.USA_CODE) %>%
      select(-GCAM_region_ID)
    L126.in_EJ_R_gaspipe_F_Yh <- get_data(all_data, "L126.in_EJ_R_gaspipe_F_Yh") %>%
      filter(GCAM_region_ID == gcam.USA_CODE)
    L126.out_EJ_R_gaspipe_F_Yh <- get_data(all_data, "L126.out_EJ_R_gaspipe_F_Yh") %>%
      filter(GCAM_region_ID == gcam.USA_CODE)
    L126.IO_R_electd_F_Yh <- get_data(all_data, "L126.IO_R_electd_F_Yh") %>%
      filter(GCAM_region_ID == gcam.USA_CODE) %>%
      select(-GCAM_region_ID)
    L101.EIA_use_all_Bbtu <- get_data(all_data, "L101.EIA_use_all_Bbtu")
    L101.inEIA_EJ_state_S_F <- get_data(all_data, "L101.inEIA_EJ_state_S_F")
    L122.in_EJ_state_refining_F <- get_data(all_data, "L122.in_EJ_state_refining_F")
    L123.out_EJ_state_elec_F <- get_data(all_data, "L123.out_EJ_state_elec_F")
    L132.in_EJ_state_indchp_F <- get_data(all_data, "L132.in_EJ_state_indchp_F")
    L132.in_EJ_state_indfeed_F <- get_data(all_data, "L132.in_EJ_state_indfeed_F")
    L132.in_EJ_state_indnochp_F <- get_data(all_data, "L132.in_EJ_state_indnochp_F")
    L1321.in_EJ_state_cement_F_Y <- get_data(all_data, "L1321.in_EJ_state_cement_F_Y")
    L1322.in_EJ_state_Fert_Yh <- get_data(all_data, "L1322.in_EJ_state_Fert_Yh")
    L142.in_EJ_state_bld_F <- get_data(all_data, "L142.in_EJ_state_bld_F")
    L154.in_EJ_state_trn_F <- get_data(all_data, "L154.in_EJ_state_trn_F")

    # ===================================================
    # PIPELINE ENERGY USE (NET)

    # Deriving gas pipeline energy use (net) from national estimate and each state's share
    L126.net_EJ_USA_pipeline <- L126.in_EJ_R_gaspipe_F_Yh %>%
      left_join_error_no_match(L126.out_EJ_R_gaspipe_F_Yh,
                               by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      # Net energy use = input energy - output energy
      transmute(value = value.x - value.y, sector, fuel, year)

    # Calculate net pipeline energy use as total value * state shares of pipeline energy use
    L126.net_EJ_state_pipeline_gas <- L101.inEIA_EJ_state_S_F %>%
      filter(sector == "gas pipeline",
             fuel == "gas") %>%
      group_by(year) %>%
      # Pct share = state/year value divided by total USA value for that year
      mutate(value = value / sum(value)) %>%
      ungroup() %>%
      # Apportion to states
      left_join_error_no_match(L126.net_EJ_USA_pipeline, by = c("sector", "fuel", "year")) %>%
      # State value = state share * USA total
      transmute(value = value.x * value.y, state, sector, fuel, year)

    # PIPELINE OUTPUT AND INPUT

    # Pipeline "output" is equal to each state's total gas consumption by all sectors
    L126.in_EJ_state_S_F <- bind_rows(L122.in_EJ_state_refining_F, L123.out_EJ_state_elec_F,
                                      L132.in_EJ_state_indchp_F, L132.in_EJ_state_indfeed_F,
                                      L132.in_EJ_state_indnochp_F, L1321.in_EJ_state_cement_F_Y,
                                      L1322.in_EJ_state_Fert_Yh, L142.in_EJ_state_bld_F,
                                      L154.in_EJ_state_trn_F)

    # Final energy by fuel
    L126.in_EJ_state_F <- L126.in_EJ_state_S_F %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      group_by(state, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # Pipeline output
    L126.out_EJ_state_pipeline_gas <- L126.in_EJ_state_F %>%
      filter(fuel == "gas") %>%
      mutate(sector = "gas pipeline") %>%
      select(state, sector, fuel, year, value)

    # Gas pipeline input = output plus pipeline energy use
    L126.in_EJ_state_pipeline_gas <- L126.out_EJ_state_pipeline_gas %>%
      left_join_error_no_match(L126.net_EJ_state_pipeline_gas,
                               by = c("state", "sector", "fuel", "year")) %>%
      # Input energy = output energy + net energy
      mutate(value = value.x + value.y) %>%
      select(state, sector, fuel, year, value)

    # GAS PROCESSING

    # Coal gasification inputs and outputs
    # NOTE: Coal gasification (town gas) is disaggregated to states on the basis of industrial coal consumption
    L126.pct_state_gasproc_coal <- L101.inEIA_EJ_state_S_F %>%
      filter(sector == "industry",
             fuel == "coal") %>%
      mutate(sector = "gas processing") %>%
      group_by(year) %>%
      # State share = state value / total value
      mutate(value = value / sum(value))

    # Apportion nation-level data to states
    L126.in_EJ_state_gasproc_coal <- L126.pct_state_gasproc_coal %>%
      left_join_error_no_match(L122.in_EJ_R_gasproc_F_Yh, by = c("sector", "fuel", "year")) %>%
      # State input energy = state share * USA input energy
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel, year, value)

    L126.out_EJ_state_gasproc_coal <- L126.pct_state_gasproc_coal %>%
      left_join_error_no_match(L122.out_EJ_R_gasproc_F_Yh, by = c("sector", "fuel", "year")) %>%
      # State output energy = state share * USA output energy
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel, year, value)

    # Biomass gasification inputs and outputs
    # NOTE: Biomass gasification (biogas) is disaggregated to states on the basis of electric sector waste biomass consumption
    L126.in_pct_state_gasproc_bio <- L101.EIA_use_all_Bbtu %>%
      # Filter out electric sector waste biomass in historical years
      filter(EIA_sector == "EI",
             EIA_fuel == "WS",
             year %in% HISTORICAL_YEARS) %>%
      select(state, year, value) %>%
      mutate(fuel = "biomass") %>%
      group_by(year) %>%
      # State share in each year = state value / total value
      mutate(value = value / sum(value)) %>%
      ungroup()

    # Multiply national totals by state shares to get each state's scaled biomass gasification outputs and inputs
    L126.out_EJ_state_gasproc_bio <- L126.in_pct_state_gasproc_bio %>%
      left_join_error_no_match(L122.out_EJ_R_gasproc_F_Yh, by = c("fuel", "year")) %>%
      # State output energy = state share * USA output energy
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel, year, value)

    L126.in_EJ_state_gasproc_bio <- L126.in_pct_state_gasproc_bio %>%
      left_join_error_no_match(L122.in_EJ_R_gasproc_F_Yh %>%
                                 filter(fuel == "biomass"),
                               by = c("fuel", "year")) %>%
      # State output energy = state share * USA output energy
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel, year, value)

    # The remainder of each state's consumption of gas is assigned to the natural gas technology
    L126.out_EJ_state_gasproc_gas <- L126.in_EJ_state_pipeline_gas %>%
      left_join_error_no_match(L126.out_EJ_state_gasproc_coal, by = c("state", "year")) %>%
      left_join_error_no_match(L126.out_EJ_state_gasproc_bio, by = c("state", "year")) %>%
      # Keep state and year columns, set sector, fuel and value columns
      transmute(state,
                sector = "gas processing",
                fuel = "natural gas",
                year,
                # Natural gas value = total pipeline gas - coal value - biomass value
                value = value.x - value.y - value)

    L126.in_EJ_state_gasproc_gas <- L126.out_EJ_state_gasproc_gas

    # Bind the input and output tables of gas processing technologies
    L126.out_EJ_state_gasproc_F <- bind_rows(L126.out_EJ_state_gasproc_gas, L126.out_EJ_state_gasproc_bio, L126.out_EJ_state_gasproc_coal)
    L126.in_EJ_state_gasproc_F <- bind_rows(L126.in_EJ_state_gasproc_gas, L126.in_EJ_state_gasproc_bio, L126.in_EJ_state_gasproc_coal)

    # ELECTRICITY TRANSMISSION AND DISTRIBUTION
    # Compile each state's total elec consumption: refining, bld, ind, trn.
    L126.in_EJ_state_elec <- L126.in_EJ_state_F %>%
      filter(fuel == "electricity")

    # Deriving electricity T&D output as the sum of all tracked demands of electricity
    L126.out_EJ_state_td_elec <- L126.in_EJ_state_elec %>%
      mutate(sector = "elect_td") %>%
      select(state, sector, fuel, year, value)

    # Assigning all states the national average T&D coefficients from L126.IO_R_electd_F_Yh
    L126.in_EJ_state_td_elec <- L126.out_EJ_state_td_elec %>%
      left_join_error_no_match(L126.IO_R_electd_F_Yh, by = c("fuel", "year")) %>%
      # State input elec = state output elec * coefficient
      mutate(value = value.x * value.y) %>%
      select(state, sector = sector.x, fuel, year, value)

    # ===================================================

    # Produce outputs
    L126.out_EJ_state_pipeline_gas %>%
      add_title("Output of gas pipeline sector by state") %>%
      add_units("EJ") %>%
      add_comments("Sum of final energy input from gas in USA states") %>%
      add_legacy_name("L126.out_EJ_state_pipeline_gas") %>%
      add_precursors("L126.in_EJ_R_gaspipe_F_Yh",
                     "L126.out_EJ_R_gaspipe_F_Yh",
                     "L101.inEIA_EJ_state_S_F",
                     "L122.in_EJ_state_refining_F",
                     "L123.out_EJ_state_elec_F",
                     "L132.in_EJ_state_indchp_F",
                     "L132.in_EJ_state_indfeed_F",
                     "L132.in_EJ_state_indnochp_F",
                     "L1321.in_EJ_state_cement_F_Y",
                     "L1322.in_EJ_state_Fert_Yh",
                     "L142.in_EJ_state_bld_F",
                     "L154.in_EJ_state_trn_F") ->
      L126.out_EJ_state_pipeline_gas

    L126.in_EJ_state_pipeline_gas %>%
      add_title("Input to gas pipeline sector by state") %>%
      add_units("EJ") %>%
      add_comments("Values from L126.out_EJ_state_pipeline_gas plus net energy values") %>%
      add_legacy_name("L126.in_EJ_state_pipeline_gas") %>%
      same_precursors_as(L126.out_EJ_state_pipeline_gas) ->
      L126.in_EJ_state_pipeline_gas

    L126.out_EJ_state_gasproc_F %>%
      add_title("Output of gas processing sector by state and technology") %>%
      add_units("EJ") %>%
      add_comments("Coal and biomass gas calculated by apportioning national values to states") %>%
      add_comments("Natural gas calculated by subtracting coal and biomass gas from total pipeline input") %>%
      add_legacy_name("L126.out_EJ_state_gasproc_F") %>%
      add_precursors("L122.out_EJ_R_gasproc_F_Yh",
                     "L101.EIA_use_all_Bbtu",
                     "L126.in_EJ_R_gaspipe_F_Yh",
                     "L126.out_EJ_R_gaspipe_F_Yh",
                     "L101.inEIA_EJ_state_S_F",
                     "L122.in_EJ_state_refining_F",
                     "L123.out_EJ_state_elec_F",
                     "L132.in_EJ_state_indchp_F",
                     "L132.in_EJ_state_indfeed_F",
                     "L132.in_EJ_state_indnochp_F",
                     "L1321.in_EJ_state_cement_F_Y",
                     "L1322.in_EJ_state_Fert_Yh",
                     "L142.in_EJ_state_bld_F",
                     "L154.in_EJ_state_trn_F") ->
      L126.out_EJ_state_gasproc_F

    L126.in_EJ_state_gasproc_F %>%
      add_title("Inputs to gas processing sector by state and technology") %>%
      add_units("EJ") %>%
      add_comments("Coal and biomass gas calculated by apportioning national values to states") %>%
      add_comments("Natural gas calculated by subtracting coal and biomass gas from total pipeline input") %>%
      add_legacy_name("L126.in_EJ_state_gasproc_F") %>%
      add_precursors("L122.in_EJ_R_gasproc_F_Yh",
                     "L122.out_EJ_R_gasproc_F_Yh",
                     "L126.in_EJ_R_gaspipe_F_Yh",
                     "L126.out_EJ_R_gaspipe_F_Yh",
                     "L101.EIA_use_all_Bbtu",
                     "L101.inEIA_EJ_state_S_F",
                     "L122.in_EJ_state_refining_F",
                     "L123.out_EJ_state_elec_F",
                     "L132.in_EJ_state_indchp_F",
                     "L132.in_EJ_state_indfeed_F",
                     "L132.in_EJ_state_indnochp_F",
                     "L1321.in_EJ_state_cement_F_Y",
                     "L1322.in_EJ_state_Fert_Yh",
                     "L142.in_EJ_state_bld_F",
                     "L154.in_EJ_state_trn_F") ->
      L126.in_EJ_state_gasproc_F

    L126.out_EJ_state_td_elec %>%
      add_title("Output of electricity T&D sector by state") %>%
      add_units("EJ") %>%
      add_comments("Sum of all tracked demands of electricity") %>%
      add_legacy_name("L126.out_EJ_state_td_elec") %>%
      add_precursors("L122.in_EJ_state_refining_F",
                     "L123.out_EJ_state_elec_F",
                     "L132.in_EJ_state_indchp_F",
                     "L132.in_EJ_state_indfeed_F",
                     "L132.in_EJ_state_indnochp_F",
                     "L1321.in_EJ_state_cement_F_Y",
                     "L1322.in_EJ_state_Fert_Yh",
                     "L142.in_EJ_state_bld_F",
                     "L154.in_EJ_state_trn_F") ->
      L126.out_EJ_state_td_elec

    L126.in_EJ_state_td_elec %>%
      add_title("Input to electricity T&D sector by state") %>%
      add_units("EJ") %>%
      add_comments("Output electricity multiplied by T&D coefficient") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L126.in_EJ_state_td_elec") %>%
      add_precursors("L122.in_EJ_state_refining_F",
                     "L123.out_EJ_state_elec_F",
                     "L132.in_EJ_state_indchp_F",
                     "L132.in_EJ_state_indfeed_F",
                     "L132.in_EJ_state_indnochp_F",
                     "L1321.in_EJ_state_cement_F_Y",
                     "L1322.in_EJ_state_Fert_Yh",
                     "L142.in_EJ_state_bld_F",
                     "L154.in_EJ_state_trn_F",
                     "L126.IO_R_electd_F_Yh") ->
      L126.in_EJ_state_td_elec

    return_data(L126.out_EJ_state_pipeline_gas, L126.in_EJ_state_pipeline_gas, L126.out_EJ_state_gasproc_F, L126.in_EJ_state_gasproc_F, L126.out_EJ_state_td_elec, L126.in_EJ_state_td_elec)
  } else {
    stop("Unknown command")
  }
}
