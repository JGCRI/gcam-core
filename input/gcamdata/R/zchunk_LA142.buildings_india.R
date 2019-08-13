#' module_gcamindia_LA142.buildings
#'
#' Provides residential and commercial building energy consumption by region/fuel/historical year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L142.india_state_in_EJ_comm_F}, \code{L142.india_state_in_EJ_resid_F}. The corresponding file in the
#' original data system was \code{LA142.Buildings.R} (gcam-india level1).
#' @details Provides for each US state industrial energy consumption and industrial feedstock consumption by region/fuel/historical year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author PNK August 2019


module_gcamindia_LA142.buildings <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L101.india_state_EB_EJ_state_S_F",
             "L142.in_EJ_R_bld_F_Yh"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L142.india_state_in_EJ_comm_F",
             "L142.india_state_in_EJ_resid_F"))

  } else if(command == driver.MAKE) {

    # Silence package checks
    fuel <- year <- state <- value <- sector <- GCAM_region_ID <- multiplier <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L101.india_state_EB_EJ_state_S_F <- get_data(all_data, "L101.india_state_EB_EJ_state_S_F")
    L142.in_EJ_R_bld_F_Yh <- get_data(all_data, "L142.in_EJ_R_bld_F_Yh")

    # ===================================================

    # Computing comm energy consumption share from Energy Balance
    L142.india_state_in_EJ_comm_F <- L101.india_state_EB_EJ_state_S_F %>%
      filter(sector == "comm") %>%
      group_by(sector,fuel,year) %>%
      mutate(value = value /sum(value)) %>%
      ungroup %>%
      mutate (sector = "bld_comm") %>%
      rename (multiplier = value) %>%
      left_join_error_no_match(filter(L142.in_EJ_R_bld_F_Yh, GCAM_region_ID == gcam.INDIA_CODE),
                               by = c("fuel", "year", "sector")) %>%
      mutate (value = value * multiplier) %>%
      select (-multiplier, -GCAM_region_ID) %>%
      mutate (sector = "comm")

    # Computing resid energy consumption share from Energy Balance
    L142.india_state_in_EJ_resid_F <- L101.india_state_EB_EJ_state_S_F %>%
      filter(sector == "resid") %>%
      group_by(sector,fuel,year) %>%
      mutate(value = value /sum(value)) %>%
      ungroup %>%
      mutate (sector = "bld_resid") %>%
      rename (multiplier = value) %>%
      left_join_error_no_match(filter(L142.in_EJ_R_bld_F_Yh, GCAM_region_ID == gcam.INDIA_CODE),
                               by = c("fuel", "year", "sector")) %>%
      mutate (value = value * multiplier) %>%
      select (-multiplier, -GCAM_region_ID) %>%
      mutate (sector = "resid")


    ## OUTPUTS
    L142.india_state_in_EJ_comm_F %>%
      add_title("Comm sector input energy by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("Computed by apportioning india-level consumption among states") %>%
      add_legacy_name("L142.india_state_in_EJ_comm_F") %>%
      add_precursors("L101.india_state_EB_EJ_state_S_F",
                     "L142.in_EJ_R_bld_F_Yh") ->
      L142.india_state_in_EJ_comm_F

    L142.india_state_in_EJ_resid_F %>%
      add_title("Resid sector input energy by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("Computed by apportioning india-level consumption among states") %>%
      add_legacy_name("L142.india_state_in_EJ_resid_F") %>%
      add_precursors("L101.india_state_EB_EJ_state_S_F",
                     "L142.in_EJ_R_bld_F_Yh") ->
      L142.india_state_in_EJ_resid_F



    return_data(L142.india_state_in_EJ_comm_F, L142.india_state_in_EJ_resid_F)
  } else {
    stop("Unknown command")
  }
}
