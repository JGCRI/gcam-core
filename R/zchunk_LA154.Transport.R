#' module_gcam.usa_LA154.Transport
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L154.in_EJ_state_trn_m_sz_tech_F}, \code{L154.out_mpkm_state_trn_nonmotor_Yh}, \code{L154.in_EJ_state_trn_F}. The corresponding file in the
#' original data system was \code{LA154.Transport.R} (gcam-usa level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AS May 2017
#' @export
module_gcam.usa_LA154.Transport <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/trnUCD_EIA_mapping",
             FILE = "temp-data-inject/L154.in_EJ_R_trn_m_sz_tech_F_Yh",
             FILE = "temp-data-inject/L154.out_mpkm_R_trn_nonmotor_Yh",
             "L100.Pop_thous_state",
             "L101.inEIA_EJ_state_S_F",
             "L101.EIA_use_all_Bbtu"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L154.in_EJ_state_trn_m_sz_tech_F",
             "L154.out_mpkm_state_trn_nonmotor_Yh",
             "L154.in_EJ_state_trn_F"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    trnUCD_EIA_mapping <- get_data(all_data, "gcam-usa/trnUCD_EIA_mapping")
    L154.in_EJ_R_trn_m_sz_tech_F_Yh <- get_data(all_data, "temp-data-inject/L154.in_EJ_R_trn_m_sz_tech_F_Yh")
    L154.out_mpkm_R_trn_nonmotor_Yh <- get_data(all_data, "temp-data-inject/L154.out_mpkm_R_trn_nonmotor_Yh")
    L100.Pop_thous_state <- get_data(all_data, "L100.Pop_thous_state")
    L101.inEIA_EJ_state_S_F <- get_data(all_data, "L101.inEIA_EJ_state_S_F")
    L101.EIA_use_all_Bbtu <- get_data(all_data, "L101.EIA_use_all_Bbtu")

    # ===================================================

    EIA_fuel <- NULL

    # Calculate the state-wise percentages for each of EIA's sector/fuel combinations that is relevant for disaggregating
    # nation-level transportation energy to the states

    # Creates lists
    trnUCD_EIA_mapping %>%
      mutate(sector_fuel = paste(EIA_fuel, EIA_sector, sep = '_')) ->
      trnUCD_EIA_mapping_concatenate

    list_sector_fuel <- unique(trnUCD_EIA_mapping_concatenate$sector_fuel) # May need to use tru list instead

    list_states <- unique(L100.Pop_thous_state$state)

    # Subset only the usa, and only values that are > 0 in the historical periods
    L154.in_EJ_R_trn_m_sz_tech_F_Yh %>%
      gather(year, value, -GCAM_region_ID, -UCD_sector, -mode, -size.class, -UCD_technology, -UCD_fuel, -fuel) %>%
      mutate(year = as.integer(substr(year, 2, 5))) %>%
      filter(GCAM_region_ID == gcam.USA_CODE) %>%
      #mutate(sector_fuel = paste(EIA_fuel, EIA_sector, sep = '_')) %>%
      #filter(sector_fuel %in% list_sector_fuel) %>%
      group_by(GCAM_region_ID, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, fuel, year) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      filter(value != 0) %>% # %>% also took out two lines in dataset I want. May want to do this last?
      spread(year, value) %>%
      gather(year, value, -GCAM_region_ID, -UCD_sector, -mode, -size.class, -UCD_technology, -UCD_fuel, -fuel) %>%
      mutate(year = as.integer(year)) %>%
      replace_na(list(value = 0)) %>%
      left_join_error_no_match(trnUCD_EIA_mapping, by = c("fuel", "mode")) -> # May want to filter in tru lists
      a

    # Next, extract the relevant EIA sector & fuel combinations from the full state database
    L101.EIA_use_all_Bbtu %>%
      filter(year %in% HISTORICAL_YEARS) %>% # Ensure within historical period
      mutate(sector_fuel = paste(EIA_fuel, EIA_sector, sep = '_')) %>%
      filter(sector_fuel %in% list_sector_fuel) %>%
      # Aggregate states to compute each state's share (change note)
      group_by(state, EIA_fuel, EIA_sector, sector, fuel, year) %>% #would be nice to get rid of some of these
      summarise(value_state = sum(value)) %>%
      ungroup() ->
      b

    # Calculate state's share
    b %>%
      group_by(EIA_fuel, EIA_sector, sector, fuel, year) %>% #would be nice to get rid of some of these
      summarise(value_national = sum(value_state)) %>%
      ungroup() ->
      c

    b %>%
      left_join_error_no_match(c, by = c("EIA_fuel", "EIA_sector", "sector", "fuel", "year")) %>%
      mutate(value_share = value_state / value_national) %>%
      select(-value_state, -value_national) %>% #NG AC is still there...need to exclude
      replace_na(list(value_share = 0)) %>%
      select(-sector, -fuel) -> # Replace NAs with zeros
      d

    # Match these percentages into a table of all transportation technologies that will be written out
    # Now, the full USA tran UCD database can be apportioned to the states
    # Creating a final output table
    a %>%
      repeat_add_columns(tibble::tibble(state = list_states)) %>%
      #why
      left_join_error_no_match(d, by = c("state", "EIA_fuel", "EIA_sector", "year")) %>%
      mutate(value = value * value_share) %>% #need to rename value
      select(state, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, fuel, year, value) ->
      L154.in_EJ_state_trn_m_sz_tech_F

    # As a final step, aggregate by fuel and name the sector
    L154.in_EJ_state_trn_m_sz_tech_F %>%
      group_by(state, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate("sector" = "transportation") %>%
      select(state, sector, fuel, year, value) ->
      L154.in_EJ_state_trn_F

    # Apportion non-motorized energy consumption to states on the basis of population
    L154.out_mpkm_R_trn_nonmotor_Yh %>%
      gather(year, value_mode, -GCAM_region_ID, -mode) %>%
      mutate(year = as.integer(substr(year, 2, 5))) %>%
      filter(GCAM_region_ID == gcam.USA_CODE) %>%
      select(mode, year, value_mode) ->
      i

    L100.Pop_thous_state %>%
      group_by(year) %>%
      summarise(value_national = sum(value)) ->
      g

    L100.Pop_thous_state %>%
      left_join_error_no_match(g, by = "year") %>%
      #comment
      left_join(i, by = "year") %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      mutate(value_share = value / value_national) %>%
      mutate(value = value_mode * value_share) %>%
      select(state, mode, year, value) ->
      L154.out_mpkm_state_trn_nonmotor_Yh

    # ===================================================

    L154.in_EJ_state_trn_m_sz_tech_F %>%
      add_title("placeholder") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.in_EJ_state_trn_m_sz_tech_F") %>%
      add_precursors("temp-data-inject/L154.in_EJ_R_trn_m_sz_tech_F_Yh", "gcam-usa/trnUCD_EIA_mapping", "temp-data-inject/L154.out_mpkm_R_trn_nonmotor_Yh",
                     "L100.Pop_thous_state", "L101.inEIA_EJ_state_S_F", "L101.EIA_use_all_Bbtu") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.in_EJ_state_trn_m_sz_tech_F
    L154.out_mpkm_state_trn_nonmotor_Yh %>%
      add_title("placeholder") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.out_mpkm_state_trn_nonmotor_Yh") %>%
      add_precursors("temp-data-inject/L154.in_EJ_R_trn_m_sz_tech_F_Yh", "gcam-usa/trnUCD_EIA_mapping", "temp-data-inject/L154.out_mpkm_R_trn_nonmotor_Yh",
                     "L100.Pop_thous_state", "L101.inEIA_EJ_state_S_F", "L101.EIA_use_all_Bbtu") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.out_mpkm_state_trn_nonmotor_Yh
    L154.in_EJ_state_trn_F %>%
      add_title("placeholder") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L154.in_EJ_state_trn_F") %>%
      add_precursors("temp-data-inject/L154.in_EJ_R_trn_m_sz_tech_F_Yh", "gcam-usa/trnUCD_EIA_mapping", "temp-data-inject/L154.out_mpkm_R_trn_nonmotor_Yh",
                     "L100.Pop_thous_state", "L101.inEIA_EJ_state_S_F", "L101.EIA_use_all_Bbtu") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L154.in_EJ_state_trn_F



    return_data(L154.in_EJ_state_trn_m_sz_tech_F, L154.out_mpkm_state_trn_nonmotor_Yh, L154.in_EJ_state_trn_F)
  } else {
    stop("Unknown command")
  }
}
