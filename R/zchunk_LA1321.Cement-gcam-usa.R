#' module_gcam.usa_LA1321.Cement
#'
#' This chunk does allocates nation energy inputs to cement production, cement production, and input-output cofficients across the states
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1321.out_Mt_state_cement_Yh}, \code{L1321.IO_GJkg_state_cement_F_Yh}, \code{L1321.in_EJ_state_cement_F_Y}. The corresponding file in the
#' original data system was \code{LA1321.Cement.R} (gcam-usa level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AS May 2017
#' @export
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

    # Assigning national cement production to states on the basis of value of shipments of NAICS 3273 by state
    # Note: The NAICS code 3273 includes cement and concrete product manufacturing. Includes cement, ready-mix concrete,
    # concrete pipe, brick, and block, and other concrete products
    Census_ind_VoS_state %>%
      filter(NAICS_code == 3273) %>% # This is the code for cement and concrete product
      select(state, VoS_thousUSD) %>%
      gather(variable, value, -state) %>% # Converting to long form
      repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) ->
      x

    x %>%
      group_by(year) %>%
      summarise(value_sum = sum(value)) %>%
      ungroup() ->
      Census_ind_VoS_state_sum

    x %>%
      left_join_error_no_match(Census_ind_VoS_state_sum, by = "year") %>%
      mutate(value_share = value / value_sum) %>%
      select(state, year, value_share) ->
      #mutate(sector = "cement") -> #might be able to leave off
      #looks like the share doesn't change by year??
      L1321.VoS_share_state_cement #can connect with next section (might be using it below)

    L1321.VoS_share_state_cement %>%
      left_join_error_no_match(
        filter(L1321.out_Mt_R_cement_Yh, GCAM_region_ID == gcam.USA_CODE), #assumes its in long form
        by = "year") %>%
      mutate(value = value * value_share) %>%
      select(state, sector, year, value) ->
      L1321.out_Mt_state_cement_Yh

    # Assuming all states have the same IO coefficients for heat, electricity, and limestone
    L1321.IO_GJkg_R_cement_F_Yh %>%
      filter(GCAM_region_ID == USA_regID) %>%
      repeat_add_columns(tibble::tibble(state = L1321.out_Mt_state_cement_Yh$state, year = HISTORICAL_YEARS)) ->
      L1321.IO_GJkg_state_cement_F_Yh # not sure if this will duplicate heat, electricity, and limestone values

    # Calculating inputs to cement production by state
    # NOTE: assuming the same fuel blend in all states
    L1321.VoS_share_state_cement %>%
      repeat_add_columns(tibble::tibble(state = L1321.out_Mt_state_cement_Yh$state, year = HISTORICAL_YEARS)) %>%
      left_join_error_no_match(
        filter(L1321.in_EJ_R_cement_F_Y, GCAM_region_ID == USA_regID),
        by = c("sector", "fuel", "year")) %>%
      mutate(value = value_share * value) -> #not sure if value labels are correct
      L1321.in_EJ_state_cement_F_Y

    # ===================================================

    L1321.out_Mt_state_cement_Yh %>%
      #add_title("Cement production by state / historical year ") %>%
      add_units("Mt") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1321.out_Mt_state_cement_Yh") %>%
      add_precursors("gcam-usa/Census_ind_VoS_state", "temp-data-inject/L1321.out_Mt_R_cement_Yh",
                     "temp-data-inject/L1321.IO_GJkg_R_cement_F_Yh", "temp-data-inject/L1321.in_EJ_R_cement_F_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1321.out_Mt_state_cement_Yh

    L1321.IO_GJkg_state_cement_F_Yh %>%
      #add_title(" Input-output coefficients of cement production by state / input / historical year") %>%
      add_units("GJ/kg") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1321.IO_GJkg_state_cement_F_Yh") %>%
      add_precursors("gcam-usa/Census_ind_VoS_state", "temp-data-inject/L1321.out_Mt_R_cement_Yh",
                     "temp-data-inject/L1321.IO_GJkg_R_cement_F_Yh", "temp-data-inject/L1321.in_EJ_R_cement_F_Y") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1321.IO_GJkg_state_cement_F_Yh

    L1321.in_EJ_state_cement_F_Y %>%
      #add_title("Energy inputs to cement production by GCAM region / fuel / historical year") %>%
      add_units("EJ") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L1321.in_EJ_state_cement_F_Y") %>%
      add_precursors("gcam-usa/Census_ind_VoS_state", "temp-data-inject/L1321.out_Mt_R_cement_Yh",
                     "temp-data-inject/L1321.IO_GJkg_R_cement_F_Yh", "temp-data-inject/L1321.in_EJ_R_cement_F_Y" ) %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L1321.in_EJ_state_cement_F_Y

    return_data(L1321.out_Mt_state_cement_Yh, L1321.IO_GJkg_state_cement_F_Yh, L1321.in_EJ_state_cement_F_Y)
  } else {
    stop("Unknown command")
  }
}
