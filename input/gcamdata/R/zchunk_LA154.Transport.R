# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LA154.Transport
#'
#' Downscale transportation energy consumption and nonmotor data to the state level, generating three ouput tables.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L154.in_EJ_state_trn_m_sz_tech_F}, \code{L154.out_mpkm_state_trn_nonmotor_Yh}, \code{L154.in_EJ_state_trn_F}. The corresponding file in the
#' original data system was \code{LA154.Transport.R} (gcam-usa level1).
#' @details Transportation energy data was downscaled in proportion to EIA state-level transportation energy data
#' @details Transportation nonmotor data was downscaled in proportion to state population
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter group_by left_join mutate select summarise
#' @importFrom tidyr complete nesting replace_na
#' @author AJS June 2017
module_gcamusa_LA154.Transport <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/trnUCD_EIA_mapping",
             FILE="gcam-usa/trnUCD_EIA_mapping_revised",
             "L154.in_EJ_R_trn_m_sz_tech_F_Yh",
             "L154.out_mpkm_R_trn_nonmotor_Yh",
             "L100.Pop_thous_state",
             "L101.EIA_use_all_Bbtu",
             "L131.in_EJ_USA_Senduse_F_Yh_noEFW",
             "L131.in_EJ_R_Senduse_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L154.in_EJ_state_trn_m_sz_tech_F",
             "L154.out_mpkm_state_trn_nonmotor_Yh",
             "L154.in_EJ_state_trn_F"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    #kbn 2019-11-10- Extending transportation changes made in the CORE to introduce revised size classes to GCAM-USA. If the
    #user has chosen the new modes and size classes, use the revised mapping file that will map the UCD data to the new modes and size classes.
    if (toString(energy.TRAN_UCD_MODE)=='rev.mode'){
      trnUCD_EIA_mapping <- get_data(all_data, "gcam-usa/trnUCD_EIA_mapping_revised")
    }
    else{trnUCD_EIA_mapping <- get_data(all_data, "gcam-usa/trnUCD_EIA_mapping")}

    L154.in_EJ_R_trn_m_sz_tech_F_Yh <- get_data(all_data, "L154.in_EJ_R_trn_m_sz_tech_F_Yh")
    L154.out_mpkm_R_trn_nonmotor_Yh <- get_data(all_data, "L154.out_mpkm_R_trn_nonmotor_Yh", strip_attributes = TRUE)
    L100.Pop_thous_state <- get_data(all_data, "L100.Pop_thous_state")
    L101.EIA_use_all_Bbtu <- get_data(all_data, "L101.EIA_use_all_Bbtu")
    L131.in_EJ_R_Senduse_F_Yh <- get_data(all_data, "L131.in_EJ_R_Senduse_F_Yh")
    L131.in_EJ_USA_Senduse_F_Yh_noEFW <- get_data(all_data, "L131.in_EJ_USA_Senduse_F_Yh_noEFW")

    # ===================================================

      # Silence package notes
      GCAM_region_ID <- UCD_sector <- mode <- size.class <- UCD_technology <- UCD_fuel <- fuel <- EIA_fuel <-
        year <- value <- EIA_sector <- . <- fuel_sector <- state <- sector <- value_state <- value_national <-
        value_share <- value_mode <- value.noEFW <- value.withEFW <- scaler <- NULL

      # Calculate the state-wise percentages for each of EIA's sector/fuel combinations that is relevant for disaggregating
      # nation-level transportation energy to the states

      # This starting table is transportation energy consumption by GCAM region (and other variables)
      # We will first subset this data for only the USA and values that are > 0 in the historical periods
        # Nonzero data are filtered out because it adds no useful information. By doing so, the number of rows of the tibble
        # will be reduced from 1840 to 800, and this is before expanding the table to apportion to states.
      L154.in_EJ_R_trn_m_sz_tech_F_Yh %>%
        filter(year %in% HISTORICAL_YEARS, GCAM_region_ID == gcam.USA_CODE) %>% # Filter for the USA and for historical years only
        filter(value != 0) %>% # Here any rows with value of 0 will be lost, even if other years of the same group are nonzero
        # We will next reintroduce those rows using "complete" and assign those values to be 0
        complete(nesting(GCAM_region_ID, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, fuel), year = HISTORICAL_YEARS, fill = list(value = 0)) %>%
        # Fuel and mode will be mapped to EIA fuel and sector
        left_join_error_no_match(trnUCD_EIA_mapping, by = c("fuel", "mode")) ->
        Transportation_energy_consumption

      # EFW-related modification for GCAM-USA: Because energy-for-water is not deducted from the "unscalable"
      # electricity demands prior to computing end-use-sector electricity scalers (in LA131), the electricity scalers
      # end up being slightly different. This is addressed explicitly in the buildings and industry sectors of GCAM-USA.
      # Here we compute a separate scaler to resolve the difference in electricity consumption by the transportation sector
      # following these two different approaches
      L154.trn_elec_scaler <- left_join_error_no_match(L131.in_EJ_USA_Senduse_F_Yh_noEFW,
                                                       L131.in_EJ_R_Senduse_F_Yh,
                                                       by = c("GCAM_region_ID", "sector", "fuel", "year"),
                                                       suffix = c(".noEFW", ".withEFW")) %>%
        filter(fuel == "electricity",
               grepl("trn", sector)) %>%
        group_by(GCAM_region_ID, fuel, year) %>%
        summarise(value.noEFW = sum(value.noEFW),
                  value.withEFW = sum(value.withEFW)) %>%
        ungroup() %>%
        mutate(scaler = value.noEFW / value.withEFW) %>%
        select(fuel, year, scaler)

      Transportation_energy_consumption <- left_join(Transportation_energy_consumption,
                                                     L154.trn_elec_scaler,
                                                     by = c("fuel", "year")) %>%
        mutate(value = if_else(is.na(scaler), value, value * scaler)) %>%
        select(-scaler)

      # From the full state database, state shares will be calculated based on relevant EIA sector and fuel combinations
      # These shares will later be multipled by the transportation energy consumption data above
      # We will create a list first, concatenating EIA-fuel and -sector, so as to selectively remove those pairs from the dataset
      list_fuel_sector <- unique(paste(trnUCD_EIA_mapping$EIA_fuel, trnUCD_EIA_mapping$EIA_sector))

      # Here is the state-level data for which to calculate state shares
      # We will first filter for only relevant EIA-fuel and -sector pairs
      L101.EIA_use_all_Bbtu %>%
        filter(year %in% HISTORICAL_YEARS) %>% # Ensure within historical period
        mutate(fuel_sector = paste(EIA_fuel, EIA_sector)) %>% # Create concatenated list in base dataframe to match the syntax of our list above
        filter(fuel_sector %in% list_fuel_sector) %>% # Filtering for just EIA-fuel/sector pairs
        select(state, EIA_fuel, EIA_sector, sector, fuel, year, value_state = value) ->
        EIA_transportation_state

      # To calculate the state share, we need to calculate the national amount
      EIA_transportation_state %>%
        group_by(EIA_fuel, EIA_sector, sector, fuel, year) %>% # Dropping state
        summarise(value_national = sum(value_state)) %>%
        ungroup() ->
        EIA_transportation_national

      # Now the state shares can be calculated by dividing the state data by the national
      EIA_transportation_state %>%
        left_join_error_no_match(EIA_transportation_national, by = c("EIA_fuel", "EIA_sector", "sector", "fuel", "year")) %>%
        mutate(value_share = value_state / value_national) %>% # Calculating state's share
        # NAs were introduced where national values were 0. Replace NAs with zeros.
        replace_na(list(value_share = 0)) %>%
        select(state, EIA_fuel, EIA_sector, year, value_share) ->
        EIA_transportation_state_share

      # The full USA tran UCD database can now be apportioned to the states
      # A list of states is created for when expanding the transportation table to include states
      list_states <- unique(L100.Pop_thous_state$state)

      # Creating the first of the three output tables
      Transportation_energy_consumption %>%
        repeat_add_columns(tibble::tibble(state = list_states)) %>%
        left_join_error_no_match(EIA_transportation_state_share, by = c("state", "EIA_fuel", "EIA_sector", "year")) %>%
        mutate(value = value * value_share) %>% # Allocating across the states
        select(state, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, fuel, year, value) ->
        L154.in_EJ_state_trn_m_sz_tech_F

      # As a final step, aggregate by fuel and name the sector
      # This creates the second of three output tables
      L154.in_EJ_state_trn_m_sz_tech_F %>%
        group_by(state, fuel, year) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        mutate(sector = "transportation") %>% # Adding a column named "sector" with "transportation" as the entries
        select(state, sector, fuel, year, value) ->
        L154.in_EJ_state_trn_F

      # Apportion non-motorized energy consumption to states on the basis of population
      # First we will create the state shares based on population
      L100.Pop_thous_state %>%
        group_by(year) %>%
        summarise(value_national = sum(value)) ->
        Pop_national

      L100.Pop_thous_state %>%
        left_join_error_no_match(Pop_national, by = "year") %>%
        mutate(value_share = value / value_national) %>% # Creating state share based on population
        select(state, year, value_share) ->
        Pop_state_share

      # Now we can use these shares to allocate the national data across the states
      L154.out_mpkm_R_trn_nonmotor_Yh %>%
        rename(value_mode = value) %>%
        filter(GCAM_region_ID == gcam.USA_CODE) %>%
        # Number of rows will change by adding states, so left_join_error_no_match cannot be used
        left_join(Pop_state_share, by = "year") %>%
        mutate(value = value_mode * value_share) %>% # Apportioning across the modes using the share data
        filter(year %in% HISTORICAL_YEARS) %>% # Ensuring within historical period
        select(state, mode, year, value) %>%
        mutate(year = as.integer(year)) ->
        L154.out_mpkm_state_trn_nonmotor_Yh

    # ===================================================

    L154.in_EJ_state_trn_m_sz_tech_F %>%
      add_title("Transportation energy consumption by state") %>%
      add_units("EJ") %>%
      add_comments("Transportation energy consumption data was downscaled to the state level using EIA state energy data") %>%
      add_legacy_name("L154.in_EJ_state_trn_m_sz_tech_F") %>%
      add_precursors("gcam-usa/trnUCD_EIA_mapping_revised",
                     "L154.in_EJ_R_trn_m_sz_tech_F_Yh",
                     "gcam-usa/trnUCD_EIA_mapping",
                     "L101.EIA_use_all_Bbtu",
                     "L131.in_EJ_USA_Senduse_F_Yh_noEFW",
                     "L131.in_EJ_R_Senduse_F_Yh") ->
      L154.in_EJ_state_trn_m_sz_tech_F

    L154.out_mpkm_state_trn_nonmotor_Yh %>%
      add_title("Transportation non-motorized travel by mode and state") %>%
      add_units("million person-km") %>%
      add_comments("National data was allocated across the states in proportion to population") %>%
      add_legacy_name("L154.out_mpkm_state_trn_nonmotor_Yh") %>%
      add_precursors("L154.out_mpkm_R_trn_nonmotor_Yh", "L100.Pop_thous_state")  ->
      L154.out_mpkm_state_trn_nonmotor_Yh

    L154.in_EJ_state_trn_F %>%
      add_title("Transportation energy consumption by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("Transportation energy consumption was aggregated by fuel, and the sector was named transportation") %>%
      add_legacy_name("L154.in_EJ_state_trn_F") %>%
      same_precursors_as(L154.in_EJ_state_trn_m_sz_tech_F) ->
      L154.in_EJ_state_trn_F

    return_data(L154.in_EJ_state_trn_m_sz_tech_F, L154.out_mpkm_state_trn_nonmotor_Yh, L154.in_EJ_state_trn_F)
  } else {
    stop("Unknown command")
  }
}
