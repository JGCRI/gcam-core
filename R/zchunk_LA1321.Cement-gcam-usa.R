#' module_gcam.usa_LA1321.Cement
#'
#' This chunk allocates across the states national cement production, input-output cofficients, and energy inputs to cement production
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1321.out_Mt_state_cement_Yh}, \code{L1321.IO_GJkg_state_cement_F_Yh}, \code{L1321.in_EJ_state_cement_F_Y}. The corresponding file in the
#' original data system was \code{LA1321.Cement.R} (gcam-usa level1).
#' @details The tables for cement production, i.e., out, and energy inputs, i.e., in, were calculated by applying state shares to national data.
#' @details The state shares were determined by the states' relative values of cement shipments.
#' @details The input-out coefficients were downscaled to the states in proportation to the national data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AS May 2017
module_gcam.usa_LA1321.Cement <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/Census_ind_VoS_state",
             FILE = "temp-data-inject/L1321.out_Mt_R_cement_Yh",
             FILE = "temp-data-inject/L1321.IO_GJkg_R_cement_F_Yh",
             FILE = "temp-data-inject/L1321.in_EJ_R_cement_F_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1321.out_Mt_state_cement_Yh",
             "L1321.IO_GJkg_state_cement_F_Yh",
             "L1321.in_EJ_state_cement_F_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    . <- NULL   # silence package check notes

    # Load required inputs
    Census_ind_VoS_state <- get_data(all_data, "gcam-usa/Census_ind_VoS_state")

    get_data(all_data, "temp-data-inject/L1321.out_Mt_R_cement_Yh") %>%
      gather(year, value, -GCAM_region_ID, -sector) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L1321.out_Mt_R_cement_Yh

    get_data(all_data, "temp-data-inject/L1321.IO_GJkg_R_cement_F_Yh") %>%
      gather(year, value, -GCAM_region_ID, -sector, -fuel) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L1321.IO_GJkg_R_cement_F_Yh

    get_data(all_data, "temp-data-inject/L1321.in_EJ_R_cement_F_Y") %>%
      gather(year, value, -GCAM_region_ID, -sector, -fuel) %>%
      mutate(year = as.integer(substr(year, 2, 5))) ->
      L1321.in_EJ_R_cement_F_Y

    # ===================================================

    variable <- value_state <- value_share <- value_national <- NAICS_code <- state <-
      VoS_thousUSD <- sector <- fuel <- year <- value <- GCAM_region_ID <- NULL   # silence package check notes

    # Assigning national cement production to states on the basis of value of shipments (VoS) of
    # the North American Industry Classification System (NAICS) code 3273 by state
    # The NAICS code 3273 includes cement and concrete product manufacturing. Includes cement, ready-mix concrete,
    # concrete pipe, brick, and block, and other concrete products
    Census_ind_VoS_state %>%
      filter(NAICS_code == 3273) %>% # This is the code for cement and concrete product manufacturing
      select(state, VoS_thousUSD) %>% # Selecting value of shipments
      gather(variable, value_state, -state) %>% # Converting to long form
      select(-variable) ->
      Cement_VoS_state

    # Here the relative state shares will be calculated. First the states will be summed to get the national total.
    # The national total is a single value
    Cement_VoS_state %>%
      select(-state) %>%
      summarise(value_national = sum(value_state)) %>%
      .[["value_national"]] ->
      Cement_VoS_national

    # The national total will be added as a separate column, so that the state share can be calculated
    Cement_VoS_state %>%
      # The national total is a single value, so left_join cannot be used
      repeat_add_columns(tibble::tibble(value_national = Cement_VoS_national)) %>%
      mutate(value_share = value_state / value_national) %>% # Calculating the state share
      select(state, value_share) ->
      Cement_share_state

    # Creating a list of states
    # This will be helpful when expanding the national table into state-level
    # Note that not every state has cement manufacturing
    state_list <- unique(Cement_share_state$state)

    # This section is calculating state-level data by multiplying the state share by the USA component in the global data
    # This will generate an output table: Cement production by state / historical year
    L1321.out_Mt_R_cement_Yh %>%
      filter(GCAM_region_ID == gcam.USA_CODE) %>% # Filtering for the USA component
      repeat_add_columns(tibble::tibble(state = state_list)) %>% # Expanding the table to the state-level
      left_join_error_no_match(Cement_share_state, by = "state") %>% # Adding the state share we calculated above
      mutate(value = value * value_share) %>% # Multiplying the national amount with the state share
      select(state, sector, year, value) ->
      L1321.out_Mt_state_cement_Yh

    # This section is downscaling the national input/output (IO) coefficients to the state level
    # Assuming all states have the same IO coefficients for heat, electricity, and limestone
    # This will generate an output table: Input-output coefficients of cement production by state / fuel / historical year
    L1321.IO_GJkg_R_cement_F_Yh %>%
      filter(GCAM_region_ID == gcam.USA_CODE) %>%
      repeat_add_columns(tibble::tibble(state = state_list)) %>%
      select(state, sector, fuel, year, value) ->
      L1321.IO_GJkg_state_cement_F_Yh

    # Calculating energy inputs to cement production by state
    # Again, this section is downscaling the national data to the state level, using the state share calculated above
    # Note that this assumes the same fuel blend in all states
    # This will generate an output table: Energy inputs to cement production by state / fuel / historical year
    L1321.in_EJ_R_cement_F_Y %>%
      filter(GCAM_region_ID == gcam.USA_CODE) %>%
      repeat_add_columns(tibble::tibble(state = state_list)) %>%
      left_join_error_no_match(Cement_share_state, by = "state") %>%
      mutate(value = value_share * value) %>%
      select(state, sector, fuel, year, value) ->
      L1321.in_EJ_state_cement_F_Y

    # ===================================================

    L1321.out_Mt_state_cement_Yh %>%
      add_title("Cement production by state / historical year ") %>%
      add_units("Mt") %>%
      add_comments("downscaling national data using state shares") %>%
      add_comments("these state shares were calculated to be proportional to the their values of cement shipments") %>%
      add_legacy_name("L1321.out_Mt_state_cement_Yh") %>%
      add_precursors("gcam-usa/Census_ind_VoS_state", "temp-data-inject/L1321.out_Mt_R_cement_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1321.out_Mt_state_cement_Yh

    L1321.IO_GJkg_state_cement_F_Yh %>%
      add_title("Input-output coefficients of cement production by state / fuel / historical year") %>%
      add_units("GJ/kg and kg/kg") %>%
      add_comments("downscaling national data assuming the same IO coefficients for each respective fuel") %>%
      add_legacy_name("L1321.IO_GJkg_state_cement_F_Yh") %>%
      add_precursors("temp-data-inject/L1321.IO_GJkg_R_cement_F_Yh") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1321.IO_GJkg_state_cement_F_Yh

    L1321.in_EJ_state_cement_F_Y %>%
      add_title("Energy inputs to cement production by state / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("downscaling national data using state shares") %>%
      add_comments("these state shares were calculated to be proportional to the their values of cement shipments") %>%
      add_legacy_name("L1321.in_EJ_state_cement_F_Y") %>%
      add_precursors("gcam-usa/Census_ind_VoS_state", "temp-data-inject/L1321.in_EJ_R_cement_F_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1321.in_EJ_state_cement_F_Y

    return_data(L1321.out_Mt_state_cement_Yh, L1321.IO_GJkg_state_cement_F_Yh, L1321.in_EJ_state_cement_F_Y)
  } else {
    stop("Unknown command")
  }
}
