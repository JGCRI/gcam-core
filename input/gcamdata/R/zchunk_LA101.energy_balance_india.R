#' module_gcamindia_LA101.energy_balance
#'
#' Produce two ouput tables from GHG Platform state energy database:
#' \itemize{
#'  \item{L101.india_state_EB_EJ_state_S_F: Energy data by GCAM sector and fuel, state, and year; energy units in EJ, years from 2005 - 2010, includes only rows that have a defined sector and fuel}
#'  \item{L101.india_state_A10.EB_SecFuel_mtoe: Energy data by sector and fuel, GCAM sector and fuel, state, and year; energy units in mtoe, years from 1960-2011, includes all original data}
#' }
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L101.india_state_A10.EB_SecFuel_mtoe}, \code{L101.india_state_EB_EJ_state_S_F}.
#' @details See above
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread fill
#' @author PNK Jul 2019


module_gcamindia_LA101.energy_balance <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/A10.EB_FuelMap",
             FILE = "gcam-india/A10.EB_SectorMap",
             FILE = "gcam-india/A10.EB_SecFuel_mtoe",
             FILE = "gcam-india/A10.fuel_conv"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.india_state_A10.EB_SecFuel_mtoe",
             "L101.india_state_EB_EJ_state_S_F"))
  } else if(command == driver.MAKE) {

    year <- value <- State <- GHG_Platform_sectors <- GHG_Platform_fuels <- GCAM_fuel <- GCAM_sector <-
      state <- sector <- fuel <- conv_mtoe_EJ <- description.x <- description.y <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    A10.EB_FuelMap                   <- get_data(all_data, "gcam-india/A10.EB_FuelMap")
    A10.EB_SectorMap                 <- get_data(all_data, "gcam-india/A10.EB_SectorMap")
    A10.EB_SecFuel_mtoe              <- get_data(all_data, "gcam-india/A10.EB_SecFuel_mtoe")
    A10.fuel_conv                    <- get_data(all_data, "gcam-india/A10.fuel_conv")

    # ===================================================

    # Preparing intermediate tables - rearranging the table with sector and fuel name
    A10.EB_SecFuel_mtoe %>%
      gather_years %>%
      left_join(A10.EB_FuelMap, by = "GHG_Platform_fuels") %>%
      left_join(A10.EB_SectorMap, by = "GHG_Platform_sectors") %>%
      mutate(state = State, fuel = GCAM_fuel, sector = GCAM_sector) ->
      mtoe_with_GCAM_names

    # convert mtoe to EJ (fuel specific)
    L101.india_state_EB_EJ_state_S_F <- mtoe_with_GCAM_names %>%
      select(state, sector, fuel, year, value) %>%
      filter(year %in% HISTORICAL_YEARS, !is.na(fuel), !is.na(sector)) %>%  #This step removes rows that have no defined sector or fuel name
      left_join(A10.fuel_conv, by = "fuel") %>%
      mutate(value = value * conv_mtoe_EJ) %>%
      group_by(state, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      arrange(fuel, sector) %>%
      ungroup()

    L101.india_state_EB_EJ_state_S_F[is.na(L101.india_state_EB_EJ_state_S_F)]<-0

    # Create other output table: leave units as mtoe, getting rid of missing values: prior to 1980, lots are missing. These data are only used for state-wise allocations
    mtoe_with_GCAM_names %>%
      select(state, year, value, GHG_Platform_fuels, GHG_Platform_sectors, sector, fuel, -State, -description.x, -description.y) %>%
      arrange(state, GHG_Platform_fuels, GHG_Platform_sectors, sector, fuel, -year) -> # Year needs to be in descending order to use fill function
      mtoe_with_GCAM_names_intermediate

    # To create this second output table, I need to split the dataframe and recombine
    mtoe_with_GCAM_names_intermediate %>%
      filter(year %in% 1971:2011) %>% # Custom year range, want to keep NAs in 1960-1970
      fill(value) %>% # Replace NAs in 1971-1979 with values from one year more recent
      # Reattaching 1960-1970 rows
      bind_rows(filter(mtoe_with_GCAM_names_intermediate, year %in% 1960:1970)) %>%
      arrange(state, GHG_Platform_fuels, GHG_Platform_sectors, sector, fuel, -year) ->
      L101.india_state_A10.EB_SecFuel_mtoe

    L101.india_state_A10.EB_SecFuel_mtoe[is.na(L101.india_state_A10.EB_SecFuel_mtoe)]<-0


    # ===================================================

    L101.india_state_A10.EB_SecFuel_mtoe %>%
      add_title("State Energy Data in mtoe by Year, GCAM_sector, and GCAM_fuel") %>%
      add_units("mtoe") %>%
      add_comments("GCAM sector and fuel names were added, NAs for years 1971-1979 were replaced with most recent year's data available") %>%
      add_legacy_name("L101.india_state_A10.EB_SecFuel_mtoe") %>%
      add_precursors("gcam-india/A10.EB_SecFuel_mtoe",
                     "gcam-india/A10.EB_FuelMap",
                     "gcam-india/A10.EB_SectorMap") ->
      L101.india_state_A10.EB_SecFuel_mtoe

    L101.india_state_EB_EJ_state_S_F %>%
      add_title("State Energy Data in EJ by Year, GCAM_sector, and GCAM_fuel") %>%
      add_units("EJ") %>%
      add_comments("GCAM sector and fuel names were added, units converted to EJ, data with no GCAM fuel or sector name removed") %>%
      add_legacy_name("L101.india_state_EB_EJ_state_S_F") %>%
      add_precursors("gcam-india/A10.EB_SecFuel_mtoe",
                     "gcam-india/A10.EB_FuelMap",
                     "gcam-india/A10.EB_SectorMap",
                     "gcam-india/A10.fuel_conv") ->
      L101.india_state_EB_EJ_state_S_F

    return_data(L101.india_state_A10.EB_SecFuel_mtoe, L101.india_state_EB_EJ_state_S_F)
  } else {
    stop("Unknown command")
  }
}
