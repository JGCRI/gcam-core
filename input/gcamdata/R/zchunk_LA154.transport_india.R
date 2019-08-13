#' module_gcamindia_LA154.transport
#'
#' Provides passenger and freight transportation sector energy consumption by region/fuel/historical year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L154.india_state_in_EJ_trn_freight_F}, \code{L154.india_state_in_EJ_trn_passenger_F}.The corresponding file in the
#' original data system was \code{LA154.transportation.R} (gcam-india level1).
#' @details Provides for each US state industrial energy consumption and industrial feedstock consumption by region/fuel/historical year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author PNK Aug 2019


module_gcamindia_LA154.transport <- function(command, ...) {

  if(command == driver.DECLARE_INPUTS) {
    return(c("L101.india_state_EB_EJ_state_S_F",
             "L154.in_EJ_R_trn_m_sz_tech_F_Yh"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L154.india_state_in_EJ_trn_freight_F",
             "L154.india_state_in_EJ_trn_passenger_F"))

  } else if(command == driver.MAKE) {

    # Silence package checks
    fuel <- year <- state <- value <- sector <- GCAM_region_ID <- multiplier <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L101.india_state_EB_EJ_state_S_F <- get_data(all_data, "L101.india_state_EB_EJ_state_S_F")
    L154.in_EJ_R_trn_m_sz_tech_F_Yh <- get_data(all_data, "L154.in_EJ_R_trn_m_sz_tech_F_Yh")

    # ===================================================


    #Part 1: Freight sector

    # Computing Trn_freight energy consumption share from Energy Balance
    L154.india_state_in_EJ_freightsh <- L101.india_state_EB_EJ_state_S_F %>%
      filter(sector == "trn_freight") %>%
      group_by(sector,fuel,year) %>%
      mutate(value = value /sum(value)) %>%
      ungroup %>%
      rename (multiplier = value , UCD_sector = sector) %>%
      mutate (UCD_sector = "Freight")

    #Preparing historical trn file for left join

    L154.india_state_in_EJ_freight_Yh <- L154.in_EJ_R_trn_m_sz_tech_F_Yh %>%
      filter (GCAM_region_ID == gcam.INDIA_CODE,
              UCD_sector == "Freight") %>%
      group_by (UCD_sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup

    #Calibrating from shares to trn_freight cons

    L154.india_state_in_EJ_trn_freight_F <- L154.india_state_in_EJ_freightsh %>%
      left_join_error_no_match(L154.india_state_in_EJ_freight_Yh, by = c("fuel", "year", "UCD_sector")) %>%
      mutate (value = value * multiplier) %>%
      select (-multiplier) %>%
      mutate (UCD_sector = "trn_freight")

    L154.india_state_in_EJ_trn_freight_F[is.na(L154.india_state_in_EJ_trn_freight_F)] <- 0

    #Part 1: Pass sector

    # Computing trn_pass energy consumption share from Energy Balance
    L154.india_state_in_EJ_passsh <- L101.india_state_EB_EJ_state_S_F %>%
      filter(sector == "trn_passenger") %>%
      group_by(sector,fuel,year) %>%
      mutate(value = value /sum(value)) %>%
      ungroup %>%
      rename (multiplier = value , UCD_sector = sector) %>%
      mutate(UCD_sector = "Passenger")

    #Preparing historical trn file for left join

    L154.india_state_in_EJ_pass_Yh <- L154.in_EJ_R_trn_m_sz_tech_F_Yh %>%
      filter (GCAM_region_ID == gcam.INDIA_CODE,
              UCD_sector == "Passenger") %>%
      group_by (UCD_sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup

    #Calibrating from shares to trn_freight cons

    L154.india_state_in_EJ_trn_passenger_F <- L154.india_state_in_EJ_passsh %>%
      left_join_error_no_match(L154.india_state_in_EJ_pass_Yh, by = c("fuel", "year", "UCD_sector")) %>%
      mutate(value = value * multiplier) %>%
      select (-multiplier) %>%
      mutate (UCD_sector = "trn_passenger")

    L154.india_state_in_EJ_trn_passenger_F[is.na(L154.india_state_in_EJ_trn_passenger_F)] <- 0



    ## OUTPUTS
    L154.india_state_in_EJ_trn_freight_F %>%
      add_title("Trn sector input energy by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("Computed by apportioning india-level consumption among states") %>%
      add_legacy_name("L154.india_state_in_EJ_trn_freight_F") %>%
      add_precursors("L101.india_state_EB_EJ_state_S_F",
                     "L154.in_EJ_R_trn_m_sz_tech_F_Yh") ->
      L154.india_state_in_EJ_trn_freight_F

    L154.india_state_in_EJ_trn_passenger_F %>%
      add_title("Trn sector input energy by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("Computed by apportioning india-level consumption among states") %>%
      add_legacy_name("L154.india_state_in_EJ_trn_passenger_F") %>%
      add_precursors("L101.india_state_EB_EJ_state_S_F",
                     "L154.in_EJ_R_trn_m_sz_tech_F_Yh") ->
      L154.india_state_in_EJ_trn_passenger_F


    return_data(L154.india_state_in_EJ_trn_freight_F,L154.india_state_in_EJ_trn_passenger_F)
  } else {
    stop("Unknown command")
  }
}
