# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LA132.industry_USA
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
#' @author ST September 2017. Modified November 2020 GPK
module_gcamusa_LA132.industry_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/mappings/enduse_fuel_aggregation",
             FILE = "energy/mappings/enduse_sector_aggregation",
             "L101.inEIA_EJ_state_S_F",
             "L122.in_EJ_state_refining_F",
             "L121.in_EJ_R_unoil_F_Yh",
             "L122.in_EJ_R_refining_F_Yh",
             "L122.in_EJ_R_gasproc_F_Yh",
             "L122.out_EJ_R_gasproc_F_Yh",
             "L123.in_EJ_R_indchp_F_Yh",
             "L123.out_EJ_R_indchp_F_Yh",
             "L124.in_EJ_R_heat_F_Yh",
             "L131.in_EJ_USA_Senduse_F_Yh_noEFW",
             "L1321.in_EJ_R_cement_F_Y",
             "L1321.in_EJ_R_indenergy_F_Yh",
             "L131.share_R_Senduse_heat_Yh",
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
      multiplier <- value.input <- value.output <- value.before <- value.after <-
      deduction <- heat_allsectors <- sector_share <- industry <- sector_agg <-
      value.initial <- value.heat <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    enduse_fuel_aggregation           <- get_data(all_data, "energy/mappings/enduse_fuel_aggregation")
    enduse_sector_aggregation         <- get_data(all_data, "energy/mappings/enduse_sector_aggregation")
    L101.inEIA_EJ_state_S_F           <- get_data(all_data, "L101.inEIA_EJ_state_S_F")
    L122.in_EJ_state_refining_F       <- get_data(all_data, "L122.in_EJ_state_refining_F")
    L121.in_EJ_R_unoil_F_Yh           <- get_data(all_data, "L121.in_EJ_R_unoil_F_Yh")
    L122.in_EJ_R_refining_F_Yh        <- get_data(all_data, "L122.in_EJ_R_refining_F_Yh")
    L122.in_EJ_R_gasproc_F_Yh         <- get_data(all_data, "L122.in_EJ_R_gasproc_F_Yh")
    L122.out_EJ_R_gasproc_F_Yh        <- get_data(all_data, "L122.out_EJ_R_gasproc_F_Yh")
    L123.in_EJ_R_indchp_F_Yh          <- get_data(all_data, "L123.in_EJ_R_indchp_F_Yh")
    L123.out_EJ_R_indchp_F_Yh         <- get_data(all_data, "L123.out_EJ_R_indchp_F_Yh")
    L124.in_EJ_R_heat_F_Yh            <- get_data(all_data, "L124.in_EJ_R_heat_F_Yh")
    L131.in_EJ_USA_Senduse_F_Yh_noEFW <- get_data(all_data, "L131.in_EJ_USA_Senduse_F_Yh_noEFW")
    L131.share_R_Senduse_heat_Yh      <- get_data(all_data, "L131.share_R_Senduse_heat_Yh")
    L1321.in_EJ_R_cement_F_Y          <- get_data(all_data, "L1321.in_EJ_R_cement_F_Y")
    L1321.in_EJ_R_indenergy_F_Yh      <- get_data(all_data, "L1321.in_EJ_R_indenergy_F_Yh")
    L1322.in_EJ_R_indenergy_F_Yh      <- get_data(all_data, "L1322.in_EJ_R_indenergy_F_Yh")
    L1322.in_EJ_R_indfeed_F_Yh        <- get_data(all_data, "L1322.in_EJ_R_indfeed_F_Yh")
    L122.in_EJ_state_refining_F       <- get_data(all_data, "L122.in_EJ_state_refining_F")
    L122.in_EJ_state_refining_F       <- get_data(all_data, "L122.in_EJ_state_refining_F")

    # ===================================================

    # Part 1. Compute industrial energy use by the USA region, to be apportioned to the states
    # This part replicates the calculations that take place in module_energy_LA132.industry, with subsequent deductions
    # for energy consumption for cement and fertilizer manufacturing. It is re-performed here because GCAM-USA does not
    # have energy-for-water, so the energy quantities in L1322.in_EJ_R_indenergy_F_Yh are too low, and due to scaling
    # factors applied to electricity demand, we can't simply add in the EFW-related energy.

    # Part 1a - deductions
    # See module_energy_LA132.industry for explanations of each flow that contributes to these deductions; for the most
    # part, these reflect activities whose energy is estimated bottom-up, from output multiplied by exogenous energy
    # input-output coefficients, and whose energy is otherwise reported with industrial energy use.
    L132.deduction_unoil <- filter(L121.in_EJ_R_unoil_F_Yh, GCAM_region_ID == gcamusa.USA_REGION_NUMBER & fuel == "gas") %>%
      rename(deduction = value)

    L132.deduction_gasproc <- filter(L122.in_EJ_R_gasproc_F_Yh, GCAM_region_ID == gcamusa.USA_REGION_NUMBER & fuel == "gas") %>%
      left_join_error_no_match(L122.out_EJ_R_gasproc_F_Yh, by = c("GCAM_region_ID", "sector", "fuel", "year"),
                               suffix = c(".input", ".output")) %>%
      mutate(deduction = value.input - value.output)

    L132.deduction_refining <- filter(L122.in_EJ_R_refining_F_Yh,
                                      GCAM_region_ID == gcamusa.USA_REGION_NUMBER,
                                      fuel %in% c("gas", "coal"),
                                      !grepl("oil refining", sector)) %>%
      rename(deduction = value)

    L132.deduction_indchp <- filter(L123.in_EJ_R_indchp_F_Yh, GCAM_region_ID == gcamusa.USA_REGION_NUMBER) %>%
      rename(deduction = value)

    L132.deduction_cement <- filter(L1321.in_EJ_R_cement_F_Y, GCAM_region_ID == gcamusa.USA_REGION_NUMBER) %>%
      rename(deduction = value)

    L132.deduction_Fert <- filter(L1321.in_EJ_R_indenergy_F_Yh, GCAM_region_ID == gcamusa.USA_REGION_NUMBER) %>%
      left_join_error_no_match(L1322.in_EJ_R_indenergy_F_Yh, by = c("GCAM_region_ID", "sector", "fuel", "year"),
                               suffix = c(".before", ".after")) %>%
      mutate(deduction = value.before - value.after)

    L132.deduction_ind <- bind_rows(L132.deduction_unoil, L132.deduction_gasproc, L132.deduction_refining, L132.deduction_indchp,
                                    L132.deduction_cement, L132.deduction_Fert) %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(deduction = sum(deduction)) %>%
      ungroup()

    # Part 1b - add in the fuel inputs to heat
    L132.ind_heat_shares <- filter(L131.share_R_Senduse_heat_Yh, grepl("industry", sector)) %>%
      select(GCAM_region_ID, sector, year, sector_share = value)

    L124.in_EJ_R_heat_F_Yh %>%
      filter(GCAM_region_ID == gcamusa.USA_REGION_NUMBER) %>%
      select(GCAM_region_ID, fuel, year, heat_allsectors = value) %>%
      left_join(L132.ind_heat_shares,
                by = c("GCAM_region_ID", "year")) %>%
      mutate(value = heat_allsectors * sector_share) %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      L132.in_EJ_R_indheat_F_Yh

    # Part 1c - Compile the energy use by the industrial sector that is available for apportioning
    # L131 estimate + fuel inputs to heat - deductions for flows estimated bottom up
    L132.in_EJ_USA_indenergy_F_Yh <- L131.in_EJ_USA_Senduse_F_Yh_noEFW %>%
      filter(GCAM_region_ID == gcamusa.USA_REGION_NUMBER,
             grepl("industry", sector),
             !grepl("feedstocks", sector),
             fuel != "heat") %>%
      left_join_error_no_match(select(enduse_fuel_aggregation, fuel, industry), by = "fuel") %>%
      left_join_error_no_match(enduse_sector_aggregation, by = "sector") %>%
      select(GCAM_region_ID, sector = sector_agg, fuel = industry, year, value) %>%
      mutate(sector = sub("in_", "", sector)) %>%
      group_by(GCAM_region_ID, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      left_join(L132.in_EJ_R_indheat_F_Yh, by = c("GCAM_region_ID", "fuel", "year"),
                suffix = c(".initial", ".heat")) %>%
      left_join(L132.deduction_ind, by = c("GCAM_region_ID", "fuel", "year")) %>%
      replace_na(list(value.heat = 0, deduction = 0)) %>%
      mutate(value = value.initial + value.heat - deduction) %>%
      select(GCAM_region_ID, sector, fuel, year, value)

    # PART 2. Compute industrial energy use by state, removing energy used in refining

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


    # PART 3. Apportion USA consumption and output to state level for non-cogeneration and cogeneration

    # Apportion national-level industrial energy consumption to states - NON-COGEN
    L132.in_pct_state_ind_F %>%
      mutate(sector = "industry_energy") %>% rename(multiplier = value) %>%
      # ^^ prepare for smooth join
      left_join_error_no_match(filter(L132.in_EJ_USA_indenergy_F_Yh, GCAM_region_ID == gcam.USA_CODE),
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
      add_precursors("energy/mappings/enduse_fuel_aggregation", "energy/mappings/enduse_sector_aggregation",
                     "L121.in_EJ_R_unoil_F_Yh", "L122.in_EJ_R_refining_F_Yh", "L122.in_EJ_R_gasproc_F_Yh",
                     "L122.out_EJ_R_gasproc_F_Yh", "L124.in_EJ_R_heat_F_Yh", "L131.in_EJ_USA_Senduse_F_Yh_noEFW",
                     "L1321.in_EJ_R_cement_F_Y", "L1321.in_EJ_R_indenergy_F_Yh", "L131.share_R_Senduse_heat_Yh",
                     "L1322.in_EJ_R_indenergy_F_Yh", "L101.inEIA_EJ_state_S_F", "L122.in_EJ_state_refining_F") ->
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
