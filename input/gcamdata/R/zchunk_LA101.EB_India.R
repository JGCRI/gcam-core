#' module_gcam.india_LA101.EB_ASI_MOSPI
#'
#' Produce two ouput tables from the GHG Platform (ASI-MOSPI combined data) state energy database:
#' \itemize{
#'  \item{L101.in_EJ_state_S_F_India: Energy data by GCAM sector and fuel, state, and year; energy units in EJ, years from 1971-2010, includes only rows that have a defined sector and fuel}
#'  \item{L101.use_all_Mtoe_India: Energy data by ASI-MOSPI sector and fuel, GCAM sector and fuel, state, and year; energy units in Million tonnes of oil equivalent (Mtoe), years from 1960-2011, includes all original data}
#' }
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L101.in_EJ_state_S_F_India}, \code{L101.use_all_Mtoe_India}.
#' @details See above
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread fill
#' @author Malyan_Ankur_CEEW
module_gcam.india_LA101.EB_ASI_MOSPI <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/EN_Fuelmapping_ASI_MOSPI_GCAM",
             FILE = "gcam-india/EN_Sectormapping_ASI_MOSPI_GCAM",
             FILE = "gcam-india/EN_use_all_MTOe_ASI_MOSPI"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.use_all_Mtoe_India",
             "L101.in_EJ_state_S_F_India"))
  } else if(command == driver.MAKE) {

    year <- value <- GCAM_fuel <- GCAM_sector <-
      state <- sector <- fuel  <- ASI_MOSPI_fuel <- ASI_MOSPI_sector <-
      description.x <- description.y <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # ===================================================
    # Load required inputs
    EN_Fuelmapping_ASI_MOSPI_GCAM <- get_data(all_data, "gcam-india/EN_Fuelmapping_ASI_MOSPI_GCAM")
    EN_Sectormapping_ASI_MOSPI_GCAM <- get_data(all_data, "gcam-india/EN_Sectormapping_ASI_MOSPI_GCAM")
    EN_use_all_MTOe_ASI_MOSPI <- get_data(all_data, "gcam-india/EN_use_all_MTOe_ASI_MOSPI")

    # ===================================================
    # Conversions and Calculations

    # Preparation for the output tables.Adding GCAM fuel and GCAM sector columns.
    EN_use_all_MTOe_ASI_MOSPI %>%
      gather_years %>%
      left_join(EN_Fuelmapping_ASI_MOSPI_GCAM, by = "ASI_MOSPI_fuel") %>%
      left_join(EN_Sectormapping_ASI_MOSPI_GCAM, by = "ASI_MOSPI_sector") %>%
      mutate(state = state, fuel = GCAM_fuel, sector = GCAM_sector) ->
      Mtoe_with_GCAM_names


    # Creating L101.in_EJ_state_S_F_India: Select year from 1971 to 2010, convert Million tonnes of oil eq. (Mtoe) to EJ (fuel specific), remove rows that have no defined sector or fuel name
    Mtoe_with_GCAM_names %>%
      select(state, sector, fuel, year, value) %>%
      filter(year %in% HISTORICAL_YEARS, !is.na(fuel), !is.na(sector)) %>%
      mutate(value = as.numeric(value)) %>%
      mutate(value = value * CONV_MTOE_EJ) %>%
      group_by(state, sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      arrange(fuel, sector) %>%
      ungroup() ->
      L101.in_EJ_state_S_F_India


    # Create other output table: leave units as Mtoe, getting rid of missing values: prior to 1980, lots are missing. These data are only used for state-wise allocations
    Mtoe_with_GCAM_names %>%
      select(state, year, value, ASI_MOSPI_fuel, ASI_MOSPI_sector, sector, fuel, -description.x, -description.y) %>%
      arrange(state, ASI_MOSPI_fuel, ASI_MOSPI_sector, sector, fuel, -year) -> # Year needs to be in descending order to use fill function
      Mtoe_with_GCAM_names_intermediate

    # To create L101.use_all_Mtoe_India, It is required to split the dataframe and recombine
    Mtoe_with_GCAM_names_intermediate %>%
      # Custom year range, want to keep NAs in 1960-1970
      filter(year %in% 1971:2011) %>%
      # Replace NAs in 1971-1979 with values from one year more recent
      fill(value) %>%
      # Reattaching 1960-1970 rows
      bind_rows(filter(Mtoe_with_GCAM_names_intermediate, year %in% 1960:1970)) %>%
      arrange(state, ASI_MOSPI_fuel, ASI_MOSPI_sector, sector, fuel, -year) ->
      L101.use_all_Mtoe_India

    # ===================================================
    # Producing Outputs

    L101.use_all_Mtoe_India %>%
      add_title("State Energy Data in Mtoe by Year, GCAM-Sector, and GCAM-Fuel") %>%
      add_units("Million tonnes of Oil equivalent (Mtoe)") %>%
      add_comments("GCAM sector and fuel names were added, NAs for years 1971-1979 were replaced with most recent year's data available") %>%
      add_legacy_name("L101.use_all_Mtoe_India") %>%
      add_precursors("gcam-india/EN_Fuelmapping_ASI_MOSPI_GCAM", "gcam-india/EN_Sectormapping_ASI_MOSPI_GCAM",
                     "gcam-india/EN_use_all_MTOe_ASI_MOSPI") ->
      L101.use_all_Mtoe_India

    L101.in_EJ_state_S_F_India %>%
      add_title("State-wise Energy Data in EJ by Year from 1971 to 2010, GCAM-Sector, and GCAM-Fuel") %>%
      add_units("EJ") %>%
      add_comments("GCAM sector and fuel names were added, units converted to EJ") %>%
      add_legacy_name("L101.in_EJ_state_S_F_India") %>%
      add_precursors("gcam-india/EN_Fuelmapping_ASI_MOSPI_GCAM", "gcam-india/EN_Sectormapping_ASI_MOSPI_GCAM",
                     "gcam-india/EN_use_all_MTOe_ASI_MOSPI") ->
      L101.in_EJ_state_S_F_India

    return_data(L101.use_all_Mtoe_India, L101.in_EJ_state_S_F_India)
  } else {
    stop("Unknown command")
  }
}

