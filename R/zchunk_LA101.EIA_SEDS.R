#' module_gcam.usa_LA101.EIA_SEDS
#'
#' This chunk produces two ouput tables from the EIA state energy database:
#'    L101.inEIA_EJ_state_S_F: Energy data by GCAM sector and fuel, state, and year; energy units in EJ, years from 1971-2010, includes only rows that have a defined sector and fuel
#'    L101.EIA_use_all_Bbtu: Energy data by EIA sector and fuel code, GCAM sector and fuel, MSN, state, and year; energy units in Billion BTU, years from 1960-2011, includes all original data
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L101.EIA_use_all_Bbtu}, \code{L101.inEIA_EJ_state_S_F}. The corresponding file in the
#' original data system was \code{LA101.EIA_SEDS.R} (gcam-usa level1).
#' @details See above
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AS April 2017
#'
module_gcam.usa_LA101.EIA_SEDS <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/EIA_SEDS_fuels",
             FILE = "gcam-usa/EIA_SEDS_sectors",
             FILE = "gcam-usa/EIA_use_all_Bbtu",
             FILE = "gcam-usa/A_fuel_conv"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.EIA_use_all_Bbtu",
             "L101.inEIA_EJ_state_S_F"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    EIA_SEDS_fuels <- get_data(all_data, "gcam-usa/EIA_SEDS_fuels")
    EIA_SEDS_sectors <- get_data(all_data, "gcam-usa/EIA_SEDS_sectors")
    EIA_use_all_Bbtu <- get_data(all_data, "gcam-usa/EIA_use_all_Bbtu")
    A_fuel_conv <- get_data(all_data, "gcam-usa/A_fuel_conv")

    # ===================================================

    # Define vector to be used for filtering
    GCAM_year <- 1971:2010

    # Prep for output tables - add columns for GCAM sector and fuel names, using the substrings of the MSN code, and filter out U.S.
    EIA_use_all_Bbtu %>%
      gather(year, value, 4:55) %>%
      mutate(year = as.integer(year)) %>%
      mutate(EIA_fuel = substr(MSN, 1, 2 )) %>%
      mutate(EIA_sector = substr(MSN, 3, 4)) %>%
      left_join(EIA_SEDS_fuels, by="EIA_fuel") %>%
      left_join(EIA_SEDS_sectors, by="EIA_sector") %>%
      filter(State != "US") %>%
      mutate(state = State, fuel = GCAM_fuel, sector = GCAM_sector) ->
      x

    # Create 1 of the 2 ouput tables: narrow years from 1971-2010, convert billion BTU to EJ (fuel specific), remove rows that have no defined sector or fuel name
    x %>%
      select(state, sector, fuel, year, value) %>%
      filter(year %in% GCAM_year) %>%
      filter(fuel != "NA", sector != "NA") %>%
      left_join(A_fuel_conv, by="fuel") %>%
      mutate(value = value*conv_Bbtu_EJ) %>%
      group_by(state, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      arrange(fuel, sector) ->
      L101.inEIA_EJ_state_S_F

    # Create other output table: leave units as billion BTU, replace NAs in 1971-1979 with values from one year more recent
    x %>%
      select(Data_Status, state, MSN, year, value, EIA_fuel, EIA_sector, sector, fuel, -State, -description.x, -description.y) %>%
      spread(year, value) %>%
      mutate(`1979` =if_else(!is.na(`1979`),`1979`,`1980`)) %>%
      mutate(`1978` =if_else(!is.na(`1978`),`1978`,`1979`)) %>%
      mutate(`1977` =if_else(!is.na(`1977`),`1977`,`1978`)) %>%
      mutate(`1976` =if_else(!is.na(`1976`),`1976`,`1977`)) %>%
      mutate(`1975` =if_else(!is.na(`1975`),`1975`,`1976`)) %>%
      mutate(`1974` =if_else(!is.na(`1974`),`1974`,`1975`)) %>%
      mutate(`1973` =if_else(!is.na(`1973`),`1973`,`1974`)) %>%
      mutate(`1972` =if_else(!is.na(`1972`),`1972`,`1973`)) %>%
      mutate(`1971` =if_else(!is.na(`1971`),`1971`,`1972`)) %>%
      gather(year, value, 8:59) %>%
      select(Data_Status, state, MSN, year, value, EIA_fuel, EIA_sector, sector, fuel) ->
      L101.EIA_use_all_Bbtu

    # ===================================================

    L101.EIA_use_all_Bbtu %>%
      add_title("State Energy Data in Bbtu by Year, GCAM-Sector, and GCAM-Fuel") %>%
      add_units("Billion BTU") %>%
      add_comments("GCAM sector and fuel names were added, NAs for years 1971-1979 were replaced with most recent year's data available") %>%
      add_legacy_name("L101.EIA_use_all_Bbtu") %>%
      add_precursors("gcam-usa/EIA_use_all_Bbtu", "gcam-usa/EIA_SEDS_fuels", "gcam-usa/EIA_SEDS_sectors") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.EIA_use_all_Bbtu
    L101.inEIA_EJ_state_S_F %>%
      add_title("State Energy Data in EJ by Year, GCAM-Sector, and GCAM-Fuel") %>%
      add_units("EJ") %>%
      add_comments("GCAM sector and fuel names were added, units converted to EJ, data with no GCAM fuel or sector name removed") %>%
      add_legacy_name("L101.inEIA_EJ_state_S_F") %>%
      add_precursors("gcam-usa/EIA_use_all_Bbtu", "gcam-usa/EIA_SEDS_fuels", "gcam-usa/EIA_SEDS_sectors", "gcam-usa/A_fuel_conv") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.inEIA_EJ_state_S_F

    return_data(L101.EIA_use_all_Bbtu, L101.inEIA_EJ_state_S_F)
  } else {
    stop("Unknown command")
  }
}
