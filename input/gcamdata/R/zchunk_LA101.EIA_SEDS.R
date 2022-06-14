# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LA101.EIA_SEDS
#'
#' Produce two ouput tables from the EIA state energy database:
#' \itemize{
#'  \item{L101.inEIA_EJ_state_S_F: Energy data by GCAM sector and fuel, state, and year; energy units in EJ, years from 1971-2010, includes only rows that have a defined sector and fuel}
#'  \item{L101.EIA_use_all_Bbtu: Energy data by EIA sector and fuel code, GCAM sector and fuel, MSN, state, and year; energy units in Billion BTU, years from 1960-2011, includes all original data}
#' }
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L101.EIA_use_all_Bbtu}, \code{L101.inEIA_EJ_state_S_F}, \code{L101.inEIA_EJ_state_S_F_all_years}. The corresponding file in the
#' original data system was \code{LA101.EIA_SEDS.R} (gcam-usa level1).
#' @details See above
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by left_join mutate select summarise
#' @importFrom tidyr fill
#' @author AS April 2017
module_gcamusa_LA101.EIA_SEDS <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/EIA_SEDS_fuels",
             FILE = "gcam-usa/EIA_SEDS_sectors",
             FILE = "gcam-usa/EIA_use_all_Bbtu",
             FILE = "gcam-usa/A_fuel_conv"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.EIA_use_all_Bbtu",
             "L101.inEIA_EJ_state_S_F",
             "L101.inEIA_EJ_state_S_F_all_years"))
  } else if(command == driver.MAKE) {

    year <- value <- Data_Status <- State <- MSN <- GCAM_fuel <- GCAM_sector <-
      state <- sector <- fuel <- conv_Bbtu_EJ <- EIA_fuel <- EIA_sector <-
      description <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    EIA_SEDS_fuels <- get_data(all_data, "gcam-usa/EIA_SEDS_fuels") %>%
      select(-description) %>%
      filter(!is.na(GCAM_fuel))
    EIA_SEDS_sectors <- get_data(all_data, "gcam-usa/EIA_SEDS_sectors") %>%
      select(-description) %>%
      filter(!is.na(GCAM_sector))
    EIA_use_all_Bbtu <- get_data(all_data, "gcam-usa/EIA_use_all_Bbtu")
    A_fuel_conv <- get_data(all_data, "gcam-usa/A_fuel_conv")

    # ===================================================

    # Prep for output tables - add columns for GCAM sector and fuel names, using the substrings of the Mnemonic Series Name (MSN) code, and filter out U.S.
    EIA_use_all_Bbtu %>%
      gather_years %>%
      mutate(EIA_fuel = substr(MSN, 1, 2),  # First and second digits of MSN is energy code
             EIA_sector = substr(MSN, 3, 4)) %>% # Third and fourth digits of MSN is sector code
      # use semi-join to filter out irrelevant fuel & sector codes
      semi_join(EIA_SEDS_fuels, by = "EIA_fuel") %>%
      semi_join(EIA_SEDS_sectors, by = "EIA_sector") %>%
      left_join_error_no_match(EIA_SEDS_fuels, by = "EIA_fuel") %>%
      left_join_error_no_match(EIA_SEDS_sectors, by = "EIA_sector") %>%
      filter(State != "US") %>%
      mutate(state = State, fuel = GCAM_fuel, sector = GCAM_sector) ->
      Bbtu_with_GCAM_names

    # Create 1 of the 3 output tables: narrow years from 1971-2010, convert billion BTU to EJ (fuel specific), remove rows that have no defined sector or fuel name
    Bbtu_with_GCAM_names %>%
      select(state, sector, fuel, year, value) %>%
      filter(year %in% HISTORICAL_YEARS, !is.na(fuel), !is.na(sector)) %>%
      left_join(A_fuel_conv, by = "fuel") %>%
      mutate(value = value * conv_Bbtu_EJ) %>%
      group_by(state, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      arrange(fuel, sector) %>%
      ungroup() ->
      L101.inEIA_EJ_state_S_F

    # Create 2 of the 3 output tables: keep all years, convert billion BTU to EJ (fuel specific), remove rows that have no defined sector or fuel name
    Bbtu_with_GCAM_names %>%
      select(state, sector, fuel, year, value) %>%
      filter(!is.na(fuel), !is.na(sector)) %>%
      left_join(A_fuel_conv, by = "fuel") %>%
      mutate(value = value * conv_Bbtu_EJ) %>%
      group_by(state, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      arrange(fuel, sector) %>%
      ungroup() ->
      L101.inEIA_EJ_state_S_F_all_years

    # Create 3 of the 3 output tables: leave units as billion BTU, getting rid of missing values: prior to 1980, lots are missing. These data are only used for state-wise allocations
    Bbtu_with_GCAM_names %>%
      select(Data_Status, state, MSN, year, value, EIA_fuel, EIA_sector, sector, fuel, -State) %>%
      arrange(Data_Status, state, MSN, EIA_fuel, EIA_sector, sector, fuel, -year) -> # Year needs to be in descending order to use fill function
      Bbtu_with_GCAM_names_intermediate

    # To create this third output table, I need to split the dataframe and recombine
    Bbtu_with_GCAM_names_intermediate %>%
      filter(year %in% gcamusa.SEDS_DATA_YEARS) %>% # Custom year range (1971:2017), want to keep NAs in 1960-1970
      fill(value) %>% # Replace NAs in 1971-1979 with values from one year more recent
      bind_rows(filter(Bbtu_with_GCAM_names_intermediate, year %in% 1960:1970)) %>% # Reattaching 1960-1970 rows
      arrange(Data_Status, state, MSN, EIA_fuel, EIA_sector, sector, fuel, -year) ->
      L101.EIA_use_all_Bbtu

    # ===================================================

    L101.EIA_use_all_Bbtu %>%
      add_title("State Energy Data in Bbtu by Year, GCAM-Sector, and GCAM-Fuel") %>%
      add_units("Billion BTU") %>%
      add_comments("GCAM sector and fuel names were added, NAs for years 1971-1979 were replaced with most recent year's data available") %>%
      add_legacy_name("L101.EIA_use_all_Bbtu") %>%
      add_precursors("gcam-usa/EIA_use_all_Bbtu", "gcam-usa/EIA_SEDS_fuels",
                     "gcam-usa/EIA_SEDS_sectors") ->
      L101.EIA_use_all_Bbtu

    L101.inEIA_EJ_state_S_F %>%
      add_title("State Energy Data in EJ by Year, GCAM-Sector, and GCAM-Fuel") %>%
      add_units("EJ") %>%
      add_comments("GCAM sector and fuel names were added, units converted to EJ, data with no GCAM fuel or sector name removed") %>%
      add_legacy_name("L101.inEIA_EJ_state_S_F") %>%
      add_precursors("gcam-usa/EIA_use_all_Bbtu", "gcam-usa/EIA_SEDS_fuels",
                     "gcam-usa/EIA_SEDS_sectors", "gcam-usa/A_fuel_conv") ->
      L101.inEIA_EJ_state_S_F

    L101.inEIA_EJ_state_S_F_all_years %>%
      add_title("State Energy Data in EJ by Year, GCAM-Sector, and GCAM-Fuel, for all available EIA years") %>%
      add_units("EJ") %>%
      add_comments("GCAM sector and fuel names were added, units converted to EJ, data with no GCAM fuel or sector name removed") %>%
      add_precursors("gcam-usa/EIA_use_all_Bbtu", "gcam-usa/EIA_SEDS_fuels",
                     "gcam-usa/EIA_SEDS_sectors", "gcam-usa/A_fuel_conv") ->
      L101.inEIA_EJ_state_S_F_all_years

    return_data(L101.EIA_use_all_Bbtu, L101.inEIA_EJ_state_S_F, L101.inEIA_EJ_state_S_F_all_years)
  } else {
    stop("Unknown command")
  }
}
