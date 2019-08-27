# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcam.usa_LA132.Industry
#'
#' Provides industrial energy consumption and industrial feedstock consumption by region/fuel/historical year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L132.in_EJ_state_indnochp_F}, \code{L132.in_EJ_state_indchp_F}, \code{L132.out_EJ_state_indchp_F}, \code{L132.in_EJ_state_indfeed_F}. The corresponding file in the
#' original data system was \code{LA132.Industry.R} (gcam-usa level1).
#' @details Provides for each US state industrial energy consumption and industrial feedstock consumption by region/fuel/historical year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by mutate select summarise
#' @importFrom tidyr gather spread
#' @author ST September 2017
module_gcamusa_LA132.industry <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L101.inEIA_EJ_state_S_F",
             "L122.in_EJ_state_refining_F",
             "L123.in_EJ_R_indchp_F_Yh",
             "L123.out_EJ_R_indchp_F_Yh",
             "L1322.in_EJ_R_indenergy_F_Yh",
             "L1322.in_EJ_R_indfeed_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L132.in_EJ_state_indnochp_F",
             "L132.in_EJ_state_indchp_F",
             "L132.out_EJ_state_indchp_F",
             "L132.in_EJ_state_indfeed_F"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    fuel <- year <- state <- value <- sector <- refinery_comsumption <- GCAM_region_ID <-
      multiplier <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L101.inEIA_EJ_state_S_F <- get_data(all_data, "L101.inEIA_EJ_state_S_F")
    L122.in_EJ_state_refining_F <- get_data(all_data, "L122.in_EJ_state_refining_F")
    L123.in_EJ_R_indchp_F_Yh <- get_data(all_data, "L123.in_EJ_R_indchp_F_Yh")
    L123.out_EJ_R_indchp_F_Yh <- get_data(all_data, "L123.out_EJ_R_indchp_F_Yh")
    L1322.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indenergy_F_Yh")
    L1322.in_EJ_R_indfeed_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indfeed_F_Yh")

    # ===================================================

    # PART 1. Compute industrial energy use, removing energy used in refining

    # Aggregate all refining fuel consumption for electricity and gas by state
    L122.in_EJ_state_refining_F %>%
      filter(fuel %in% c("electricity", "gas")) %>%
      group_by(state, fuel, year) %>% summarise(refinery_comsumption = sum(value)) %>% ungroup ->
      L132.in_EJ_state_refining_elecgas

    # Adjust industrial fuel consumption by removing refinery consumption computed above
    L101.inEIA_EJ_state_S_F %>%
      filter(sector == "industry", fuel %in% c("electricity", "gas")) %>%
      left_join_error_no_match(L132.in_EJ_state_refining_elecgas, by = c("state", "fuel", "year")) %>%
      mutate(value = value - refinery_comsumption,
             value = if_else(value < 0, 0, value)) %>%
      select(-refinery_comsumption) ->
      L132.in_EJ_state_ind_elecgas_adj

    # Bind above with other fuels considered in GCAM's "industrial energy use" sector
    L101.inEIA_EJ_state_S_F %>%
      filter(sector == "industry",
             fuel %in% gcam.IND_ENERGY_USE,
             fuel != "gas") %>%
      bind_rows(L132.in_EJ_state_ind_elecgas_adj) ->
      L132.in_EJ_state_indenergy_F_unscaled

    # Compute fuel consumption by state and sector as proportion of USA total
    L132.in_EJ_state_indenergy_F_unscaled %>%
      group_by(sector, fuel, year) %>%
      mutate(value = value / sum(value)) %>% ungroup ->
      L132.in_pct_state_ind_F


    # PART 2. Apportion USA consumption and output to state level for non-cogeneration and cogeneration

    # Apportion national-level industrial energy consumption to states - NON-COGEN
    L132.in_pct_state_ind_F %>%
      mutate(sector = "industry_energy") %>% rename(multiplier = value) %>%
      # ^^ prepare for smooth join
      left_join_error_no_match(filter(L1322.in_EJ_R_indenergy_F_Yh, GCAM_region_ID == gcam.USA_CODE),
                               by = c("fuel", "year", "sector")) %>%
      mutate(value = value *  multiplier) %>%
      select(-multiplier, -GCAM_region_ID) ->
      L132.in_EJ_state_indnochp_F

    # Apportion national-level industrial energy consumption to states - COGEN
    L132.in_pct_state_ind_F %>%
      filter(fuel %in% gcam.IND_ENERGY_USE) %>%
      # ^^ remove fuels that are inputs to cogen systems, i.e., not electricity
      mutate(sector = "chp_elec") %>% rename(multiplier = value) %>%
      # ^^ prepare for smooth join
      left_join_error_no_match(filter(L123.in_EJ_R_indchp_F_Yh, GCAM_region_ID == gcam.USA_CODE),
                               by = c("fuel", "year", "sector")) %>%
      mutate(value = value * multiplier) %>%
      select(-multiplier, -GCAM_region_ID) ->
      L132.in_EJ_state_indchp_F

    # Apportion nation-level industrial cogen output to states
    L132.in_pct_state_ind_F %>%
      filter(fuel %in% gcam.IND_ENERGY_USE) %>%
      mutate(sector = "chp_elec") %>% rename(multiplier = value) %>%
      # ^^ prepare for smooth join
      left_join_error_no_match(filter(L123.out_EJ_R_indchp_F_Yh, GCAM_region_ID == gcam.USA_CODE),
                               by = c("fuel", "year", "sector")) %>%
      mutate(value = value * multiplier) %>%
      select(-multiplier, -GCAM_region_ID) ->
      L132.out_EJ_state_indchp_F


    # PART 3: Industrial feedstocks
    # Note: Gas and liquid fuels only; each is treated separately...
    # ... Liquid fuels are apportioned to states by petchem feed and asphalt;
    # ... gas fuels are apportioned by petchem feed only.

    # Compute petroleum feedstocks by state (as proportion of USA total)
    L101.inEIA_EJ_state_S_F %>%
      filter(sector == "industry",
             fuel %in% c("refined liquids (const feed)", "refined liquids (petchem feed)")) %>%
      group_by(state, sector, year) %>% summarise(value = sum(value)) %>% ungroup %>%
      mutate(sector = "industry_feedstocks",
             fuel = "refined liquids") %>%
      group_by(sector, fuel, year) %>%
      mutate(value = value / sum(value)) %>% ungroup ->
      # ^^ computes values as proportion of USA totals
      L132.in_EJ_state_indfeed_liq_unscaled

    # Compute natural gas feedstocks by state (as proportion of USA total)
    # Note: assumes petrochemical feedstocks as basis for disaggregating natural gas feedstocks to states
    L101.inEIA_EJ_state_S_F %>%
      filter(sector == "industry",
             fuel == "refined liquids (petchem feed)") %>%
      mutate(sector = "industry_feedstocks", fuel = "gas") %>%
      group_by(sector, fuel, year) %>%
      mutate(value = value / sum(value)) %>% ungroup ->
      # ^^ computes values as proportion of USA totals
      L132.pct_state_indfeed_gas

    # Replicate natural gas feedstock proportions to create coal table
    # Note: these proportions don't actually matter, because coal feedstocks are zero for all years in USA
    L132.pct_state_indfeed_gas %>%
      mutate(fuel = "coal") %>%
      bind_rows(L132.pct_state_indfeed_gas) %>%
      bind_rows(L132.in_EJ_state_indfeed_liq_unscaled) ->
      L132.pct_state_indfeed_F
    # ^^ proportions for petroleum, gas, and coal

    # Apportion feedstocks among states
    L132.pct_state_indfeed_F %>%
      rename(multiplier = value) %>%
      # ^^ prepare for smooth join
      left_join_error_no_match(filter(L1322.in_EJ_R_indfeed_F_Yh, GCAM_region_ID == gcam.USA_CODE),
                               by = c("sector", "fuel", "year")) %>%
      mutate(value = value * multiplier) %>%
      # ^^ get state portions
      select(-multiplier, -GCAM_region_ID) %>%
      arrange(state) ->
      L132.in_EJ_state_indfeed_F


    ## OUTPUTS
    L132.in_EJ_state_indnochp_F %>%
      add_title("Industrial sector non-cogen input energy by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("Computed by apportioning USA-level consumption among states") %>%
      add_legacy_name("L132.in_EJ_state_indnochp_F") %>%
      add_precursors("L1322.in_EJ_R_indenergy_F_Yh",
                     "L101.inEIA_EJ_state_S_F",
                     "L122.in_EJ_state_refining_F") ->
      L132.in_EJ_state_indnochp_F

    L132.in_EJ_state_indchp_F %>%
      add_title("Industrial sector cogeneration input energy by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("Computed by apportioning USA-level consumption among states") %>%
      add_legacy_name("L132.in_EJ_state_indchp_F") %>%
      add_precursors("L123.out_EJ_R_indchp_F_Yh",
                     "L101.inEIA_EJ_state_S_F",
                     "L122.in_EJ_state_refining_F") ->
      L132.in_EJ_state_indchp_F

    L132.out_EJ_state_indchp_F %>%
      add_title("Industrial sector electricity cogeneration by state") %>%
      add_units("EJ") %>%
      add_comments("Computed by apportioning USA-level CHP generation among states") %>%
      add_legacy_name("L132.out_EJ_state_indchp_F") %>%
      add_precursors("L123.in_EJ_R_indchp_F_Yh",
                     "L101.inEIA_EJ_state_S_F",
                     "L122.in_EJ_state_refining_F") ->
      L132.out_EJ_state_indchp_F

    L132.in_EJ_state_indfeed_F %>%
      add_title("Industrial feedstocks by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("Computed by apportioning USA-level feedstocks among states") %>%
      add_legacy_name("L132.in_EJ_state_indfeed_F") %>%
      add_precursors("L101.inEIA_EJ_state_S_F", "L1322.in_EJ_R_indfeed_F_Yh") ->
      L132.in_EJ_state_indfeed_F

    return_data(L132.in_EJ_state_indnochp_F, L132.in_EJ_state_indchp_F, L132.out_EJ_state_indchp_F, L132.in_EJ_state_indfeed_F)
  } else {
    stop("Unknown command")
  }
}
