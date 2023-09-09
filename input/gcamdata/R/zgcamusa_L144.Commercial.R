# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L144.Commercial
#'
#' Calculates commercial floorspace by state and energy consumption by state/fuel/end use
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L144.flsp_bm2_state_comm}, \code{L144.in_EJ_state_comm_F_U_Y}. The corresponding file in the
#' original data system was \code{LA144.Commercial.R} (gcam-usa level1).
#' @details Calculates commercial floorspace by state and energy consumption by state/fuel/end use
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join mutate select summarise transmute
#' @importFrom tidyr gather replace_na
#' @author RLH September 2017
module_gcamusa_L144.Commercial <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/Census_pop",
             FILE = "gcam-usa/CBECS_variables",
             FILE = "gcam-usa/EIA_AEO_fuels",
             FILE = "gcam-usa/EIA_AEO_services",
             FILE = "gcam-usa/EIA_AEO_Tab5",
             FILE = "gcam-usa/EIA_distheat",
             FILE = "gcam-usa/PNNL_Commext_elec",
             FILE = "gcam-usa/CBECS_1979_1983",
             FILE = "gcam-usa/CBECS_1986",
             FILE = "gcam-usa/CBECS_1989",
             FILE = "gcam-usa/CBECS_1992",
             FILE = "gcam-usa/CBECS_1995",
             FILE = "gcam-usa/CBECS_1999",
             FILE = "gcam-usa/CBECS_2003",
             FILE = "gcam-usa/CBECS_2012",
             "L142.in_EJ_state_bld_F",
             "L143.share_state_Pop_CDD_sR9",
             "L143.share_state_Pop_HDD_sR9"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L144.flsp_bm2_state_comm",
             "L144.in_EJ_state_comm_F_U_Y"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    subregion4 <- subregion9 <- REGION <- DIVISION <- state <- year <- value <- setNames <-
      SQFT1 <- SQFT2 <- SQFT <- ADJWT <- subregion4 <- . <- pcflsp_m2 <- pcflsp_m2.x <- pcflsp_m2.y <-
      conv_4_9 <- variable <- scaler <- sector <- value <- fuel <- share <- efficiency <- service <-
      value.x <- value.y <- Year <- unit <- value_EJ <- pre <- post <- state_EJ <- AEO_target <-
      initial <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions") %>%
      select(subregion4, subregion9, REGION, DIVISION, state) %>%
      distinct()
    Census_pop <- get_data(all_data, "gcam-usa/Census_pop") %>%
      gather_years
    CBECS_variables <- get_data(all_data, "gcam-usa/CBECS_variables")
    EIA_AEO_Tab5 <- get_data(all_data, "gcam-usa/EIA_AEO_Tab5") %>%
      gather_years
    EIA_distheat <- get_data(all_data, "gcam-usa/EIA_distheat")
    PNNL_Commext_elec <- get_data(all_data, "gcam-usa/PNNL_Commext_elec")
    CBECS_1979_1983 <- get_data(all_data, "gcam-usa/CBECS_1979_1983")
    CBECS_1986 <- get_data(all_data, "gcam-usa/CBECS_1986")
    CBECS_1989 <- get_data(all_data, "gcam-usa/CBECS_1989")
    CBECS_1992 <- get_data(all_data, "gcam-usa/CBECS_1992")
    CBECS_1995 <- get_data(all_data, "gcam-usa/CBECS_1995")
    CBECS_1999 <- get_data(all_data, "gcam-usa/CBECS_1999")
    CBECS_2003 <- get_data(all_data, "gcam-usa/CBECS_2003")
    CBECS_2012 <- get_data(all_data, "gcam-usa/CBECS_2012")
    EIA_AEO_services <- get_data(all_data, "gcam-usa/EIA_AEO_services")
    EIA_AEO_fuels <- get_data(all_data, "gcam-usa/EIA_AEO_fuels")
    L142.in_EJ_state_bld_F <- get_data(all_data, "L142.in_EJ_state_bld_F")
    L143.share_state_Pop_CDD_sR9 <- get_data(all_data, "L143.share_state_Pop_CDD_sR9")
    L143.share_state_Pop_HDD_sR9 <- get_data(all_data, "L143.share_state_Pop_HDD_sR9")

    # ===================================================
    # a) PREPARATION AND CLEANING OF CBECS DATA (Commercial Buildings Energy Consumption Survey)
    # The 1979 and 1983 only have floorspace by census region
    # We can't bind_rows because all CBECS have different columns
    L144.CBECS_all <- list(CBECS_1986, CBECS_1989, CBECS_1992, CBECS_1995, CBECS_1999, CBECS_2003, CBECS_2012)
    names(L144.CBECS_all) <- paste0("CBECS", c(1986, 1989, 1992, 1995, 1999, 2003, 2012))

    # In order to be able to work with these data across years, the "edition" number needs to be removed from all
    # variable names. E.g., re-naming square footage from "SQFT3" in 1986 and "SQFT4" in 1989 to "SQFT" in all.
    L144.CBECS_all <- lapply(L144.CBECS_all, function(df) {
      stats::setNames(df, sub("[0-9]{1}$", "", names(df)))
    })

    # Add in the census region (subregion4) and census division (subregion9)
    # Census regions (subregion4) are used for 1979-1986 floorspace, as the first editions didn't have census divisions (subregion9)
    states_subregions_sub9 <- states_subregions %>%
      select(subregion4, subregion9, REGION, DIVISION) %>%
      distinct()

    states_subregions_sub4 <- states_subregions %>%
      select(subregion4, REGION) %>%
      distinct()

    L144.CBECS_all <- lapply(L144.CBECS_all, function(df) {
        left_join_error_no_match(df, states_subregions_sub9,
                                 by = c("REGION", "CENDIV" = "DIVISION"))
    })

    # Convert all missing value strings to 0 in all databases
    L144.CBECS_all[["CBECS1992"]][is.na(L144.CBECS_all[["CBECS1992"]])] <- 0
    # In CBECS1995, missing values are indicated with 1e14
    L144.CBECS_all[["CBECS1995"]][L144.CBECS_all[["CBECS1995"]] == 1e14] <- 0
    L144.CBECS_all[["CBECS2003"]][is.na(L144.CBECS_all[["CBECS2003"]])] <- 0
    L144.CBECS_all[["CBECS2012"]][is.na(L144.CBECS_all[["CBECS2012"]])] <- 0

    # Add subregions to census population for aggregating
    L144.Census_pop <- Census_pop %>%
      left_join_error_no_match(states_subregions, by = "state") %>%
      filter(year %in% HISTORICAL_YEARS)

    # Aggregate population to subregion4
    L144.pop_sR4 <- L144.Census_pop %>%
      group_by(subregion4, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # Aggregate population to subregion9
    L144.pop_sR9 <- L144.Census_pop %>%
      group_by(subregion9, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # b) FLOORSPACE BY STATE AND YEAR
    # Estimating total floorspace by census region (subregion4) and division (subregion9)
    L144.CBECS_1979_1983 <- CBECS_1979_1983 %>%
      left_join_error_no_match(states_subregions_sub4, by = "REGION") %>%
      # Using left_join because there will be two values for the two years for each subregion
      left_join(L144.pop_sR4 %>%
                  filter(year %in% c(1979, 1983)), by = "subregion4") %>%
      # SQFT1 represents 1979 square footage, SQFT2 represents 1983 square footage
      mutate(pcflsp_m2 = if_else(year == 1979, SQFT1 / value * CONV_MILFT2_M2, SQFT2 / value * CONV_MILFT2_M2))

    # Add in a year column to CBECS data so that we can bind rows later
    #Note that the 6, 9 below will get the correct year names from the columns.
    for (i in seq_along(L144.CBECS_all)) {
      df <- L144.CBECS_all[[i]]
      data_year <- substr(names(L144.CBECS_all[i]), 6, 9)
      df$year <- as.integer(data_year)
      L144.CBECS_all[[i]] <- df
    }

    # Aggregate CBECS floorspace data by year and subregion
    L144.flsp_bm2_sR4 <- L144.CBECS_all %>%
      # For each tibble, select sqft, weights for summing, year, and subregions
      # in CBECS 2013: ADJWT is called "Final full sample building weight"
      lapply(function(df) {
        df %>%
          select(SQFT, ADJWT, subregion4, subregion9, year)
      }) %>%
      # Bind all CBECS tibbles
      do.call(bind_rows, .)

    # For 1986, calculate per capita floorspace by subregion4, because it didn't have subregion9 divisions
    L144.flsp_bm2_sR4_CBECS1986 <- L144.flsp_bm2_sR4 %>%
      filter(year == 1986) %>%
      group_by(subregion4, year) %>%
      # Calculate square footage
      summarise(SQFT = sum(SQFT * ADJWT * CONV_FT2_M2)) %>%
      ungroup() %>%
      left_join_error_no_match(L144.pop_sR4, by = c("year", "subregion4")) %>%
      # Calculate square footage per capita
      mutate(pcflsp_m2 = SQFT / value)

    # Calculate per capita floorspace by subregion9
    L144.flsp_bm2_sR9 <- L144.flsp_bm2_sR4 %>%
      group_by(subregion9, year) %>%
      # Calculate square footage
      summarise(SQFT = sum(SQFT * ADJWT * CONV_FT2_M2)) %>%
      ungroup() %>%
      left_join_error_no_match(L144.pop_sR9, by = c("year", "subregion9")) %>%
      # Calculate square footage per capita
      mutate(pcflsp_m2 = SQFT / value) %>%
      select(subregion9, year, pcflsp_m2)

    # Downscale 1983 and 1979 floorspace to subregion9, using the ratios of per-capita floorspace in 1986
    L144.flsp_conv_4_9 <- L144.flsp_bm2_sR9 %>%
      filter(year == 1986) %>%
      # Add in subregion9 values
      left_join_error_no_match(states_subregions_sub9, by = "subregion9") %>%
      # Add in subregion4 values
      left_join_error_no_match(L144.flsp_bm2_sR4_CBECS1986, by = c("subregion4", "year")) %>%
      mutate(conv_4_9 = pcflsp_m2.x / pcflsp_m2.y) %>%
      select(subregion4, subregion9, conv_4_9)

    # Multiplying the per-capita floorspace ratios from subregion4 to subregion9, to expand from 4 to 9
    L144.flsp_bm2_sR9_CBECS1979_1983 <- L144.flsp_conv_4_9 %>%
      # Using left_join to expand to 2 years per observation
      left_join(L144.CBECS_1979_1983, by = "subregion4") %>%
      mutate(pcflsp_m2 = pcflsp_m2 * conv_4_9) %>%
      select(subregion9, year, pcflsp_m2)

    # Combine all subregion9 floorspace
    L144.pcflsp_m2_sR9_CBECS <- bind_rows(L144.flsp_bm2_sR9, L144.flsp_bm2_sR9_CBECS1979_1983)

    # Interpolate floorspace values to all historical years
    L144.pcflsp_m2_sR9_comm <- L144.pcflsp_m2_sR9_CBECS %>%
      select(subregion9) %>%
      distinct() %>%
      repeat_add_columns(tibble(year = HISTORICAL_YEARS)) %>%
      # Using left_join because not all years included
      left_join(L144.pcflsp_m2_sR9_CBECS, by = c("subregion9", "year")) %>%
      group_by(subregion9) %>%
      mutate(pcflsp_m2 = approx_fun(year, pcflsp_m2, rule = 2)) %>%
      ungroup()

    # Expand to states: multiply per-capita floorspace in each subregion9 times the population of each state
    L144.flsp_bm2_state_comm <- L144.Census_pop %>%
      left_join_error_no_match(L144.pcflsp_m2_sR9_comm, by = c("subregion9", "year")) %>%
      transmute(state, year, subregion9,
                # Floorspace = population * per-capita floorspace
                value = value * pcflsp_m2 / CONV_BM2_M2)

    # NOTE: we are scaling aggregated CBECS floorspace to match AEO base year estimates from 1999 to 2010
    # The main reason for this step is that the most recent CBECS edition is a decade old (2003)
    AEO_Tab5_yearcols <- unique(EIA_AEO_Tab5$year)

    # Convert AEO values to billion square meters
    AEO_USA_flsp_bm2 <- EIA_AEO_Tab5 %>%
      filter(variable == "Floorspace",
             # To fix timeshift, year can't be greater than historical years
             year <= max(HISTORICAL_YEARS)) %>%
      mutate(value = value * CONV_FT2_M2,
             unit = "Billion square meters")

    # Sum floorspace by year to get national value
    L144.flsp_bm2_state_comm_sum <- L144.flsp_bm2_state_comm %>%
      group_by(year) %>%
      summarise(sum = sum(value)) %>%
      ungroup()

    # Calculate scaler to convert from CBECS tp AEO totals
    AEO_USA_flsp_bm2_scalers <- AEO_USA_flsp_bm2 %>%
      left_join_error_no_match(L144.flsp_bm2_state_comm_sum, by = "year") %>%
      mutate(scaler = value / sum) %>%
      select(year, scaler)

    L144.flsp_bm2_state_comm <- L144.flsp_bm2_state_comm %>%
      # Using left_join because we are only scaling for certain years
      left_join(AEO_USA_flsp_bm2_scalers, by = "year") %>%
      # If there isn't a scaler right now, use 1 so value isn't changed
      replace_na(list(scaler = 1)) %>%
      mutate(sector = "comm",
             value = value * scaler) %>%
      select(state, subregion9, sector, year, value)

    # 2c: ENERGY CONSUMPTION BY STATE, SERVICE, AND YEAR
    # Aggregating energy consumption by sampling weights
    L144.in_EJ_sR9_comm <- L144.CBECS_all %>%
      lapply(function(df) {
        # We are only keeping certain energy-related columns
        cols_to_keep <- which(names(df) %in% CBECS_variables$variable)
        if(length(cols_to_keep) > 0) {
          df %>%
            select(cols_to_keep, ADJWT, subregion9, year) %>%
            # All cols_to_keep have BTU in name
            gather(variable, value, dplyr::contains("BTU")) %>%
            group_by(subregion9, year, variable) %>%
            # Sum by census division, multiplying by sampling weights
            summarise(value = sum(value * ADJWT * CONV_KBTU_EJ)) %>%
            ungroup()
        } else {
          # If no cols_to_keep, return an empty tibble so that we can bind rows
          tibble()
        }
      }) %>%
      # Bind all CBECS tibbles
      do.call(bind_rows, .)

    # Match in GCAM fuel and service
    L144.in_EJ_sR9_CBECS_F_U_Y <- L144.in_EJ_sR9_comm %>%
      left_join_error_no_match(CBECS_variables, by = "variable")

    # District services are backed out to their fuel inputs here
    # NOTE: in GCAM-USA, district services consumed by buildings are indicated by the fuel inputs to the district service plants
    L144.in_EJ_sR9_CBECS_Fdist_U_Y <- L144.in_EJ_sR9_CBECS_F_U_Y %>%
      filter(fuel == "district services") %>%
      select(-fuel) %>%
      # Convert from total district service value to value for each fuel
      repeat_add_columns(tibble(fuel = unique(EIA_distheat$fuel))) %>%
      left_join_error_no_match(EIA_distheat, by = c("fuel", "service")) %>%
      mutate(value = value * share / efficiency) %>%
      select(-share, -efficiency)

    # Replace old district services with new district service values
    L144.in_EJ_sR9_CBECS_F_U_Y <- L144.in_EJ_sR9_CBECS_F_U_Y %>%
      filter(fuel != "district services") %>%
      bind_rows(L144.in_EJ_sR9_CBECS_Fdist_U_Y)

    # Aggregate by fuel and service
    L144.in_EJ_sR9_comm_F_U_Y <- L144.in_EJ_sR9_CBECS_F_U_Y %>%
      group_by(subregion9, fuel, service, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # Interpolate fuel and service aggregation to all historical years
    L144.in_EJ_sR9_comm_F_U_Y <- L144.in_EJ_sR9_comm_F_U_Y %>%
      select(subregion9, fuel, service) %>%
      distinct() %>%
      repeat_add_columns(tibble(year = HISTORICAL_YEARS)) %>%
      left_join(L144.in_EJ_sR9_comm_F_U_Y, by = c("subregion9", "fuel", "service", "year")) %>%
      group_by(subregion9, fuel, service) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup()

    # At this point, we have a table of energy by subregion9, fuel, and end use that needs to be
    # apportioned to states, and scaled
    # The reason for apportioning to states first is that the heating and cooling energy will be modified by pop-weighted HDD and CDD
    # prior to calculating energy shares
    # Downscaling heating and cooling energy to states according to person-HDD and -CDD
    CBECS_heating_fuels <- c("electricity", "gas", "refined liquids")
    CBECS_cooling_fuels <- c("electricity", "gas")

    # Expand L144.in_EJ_sR9_comm_F_U_Y heating to all states by multiplying by HDD share within subregion9
    L144.in_EJ_state_comm_F_heating_Y <- states_subregions %>%
      select(state, subregion9) %>%
      # Add all fuels and years to each state
      repeat_add_columns(tidyr::crossing(fuel = CBECS_heating_fuels, year = HISTORICAL_YEARS)) %>%
      mutate(service = "comm heating") %>%
      left_join_error_no_match(L144.in_EJ_sR9_comm_F_U_Y, by = c("subregion9", "fuel", "service", "year")) %>%
      left_join_error_no_match(L143.share_state_Pop_HDD_sR9, by = c("state", "subregion9", "year")) %>%
      mutate(value = value.x * value.y) %>%
      select(state, subregion9, fuel, service, year, value)

    # Expand L144.in_EJ_sR9_comm_F_U_Y cooling to all states by multiplying by CDD share within subregion9
    L144.in_EJ_state_comm_F_cooling_Y <- states_subregions %>%
      select(state, subregion9) %>%
      repeat_add_columns(tidyr::crossing(fuel = CBECS_cooling_fuels, year = HISTORICAL_YEARS)) %>%
      mutate(service = "comm cooling") %>%
      left_join_error_no_match(L144.in_EJ_sR9_comm_F_U_Y, by = c("subregion9", "fuel", "service", "year")) %>%
      left_join_error_no_match(L143.share_state_Pop_CDD_sR9, by = c("state", "subregion9", "year")) %>%
      mutate(value = value.x * value.y) %>%
      select(state, subregion9, fuel, service, year, value)

    # Downscaling all remaining services to states according to floorspace
    # Calculate the share of each state within its census division's (subregion9's) floorspace
    L144.flsp_state_share_sR9 <- L144.flsp_bm2_state_comm %>%
      group_by(subregion9, year) %>%
      mutate(share = value / sum(value)) %>%
      ungroup() %>%
      select(-value)

    # Remove heating and cooling
    L144.in_EJ_sR9_comm_F_Uoth_Y <- L144.in_EJ_sR9_comm_F_U_Y %>%
      filter(!(service %in% c("comm heating", "comm cooling")))

    # Convert to state data by multiplying by floorspace share
    L144.in_EJ_state_comm_F_Uoth_Y <- L144.in_EJ_sR9_comm_F_Uoth_Y %>%
      select(fuel, service) %>%
      distinct() %>%
      repeat_add_columns(tidyr::crossing(state = gcamusa.STATES, year = HISTORICAL_YEARS)) %>%
      # We now have all combos of fuel, service, state and historical year; add in subregion9
      left_join_error_no_match(states_subregions, by = "state") %>%
      # Add in floorspace data
      left_join_error_no_match(L144.flsp_state_share_sR9, by = c("state", "year", "subregion9")) %>%
      # Add in energy data
      left_join_error_no_match(L144.in_EJ_sR9_comm_F_Uoth_Y,
                               by = c("subregion9", "fuel", "service", "year")) %>%
      mutate(value = value * share) %>%
      select(state, subregion9, fuel, service, year, value)

    # Assembling unscaled energy consumption by state, fuel, and service
    L144.in_EJ_state_comm_F_U_Y_unscaled <- bind_rows(L144.in_EJ_state_comm_F_heating_Y,
                                                      L144.in_EJ_state_comm_F_cooling_Y,
                                                      L144.in_EJ_state_comm_F_Uoth_Y)

    # Adjusting commercial building energy consumption by fuel and energy service to better match AEO 2015.
    # The problem is likely that CBECS is >10 years out of date (last survey was 2003), which results in
    # too much lighting service and not enough office service.
    # Note that the EIA "target" estimates are in QBtu HHV, whereas GCAM uses EJ LHV, but this doesn't matter
    # because it's all going to be scaled later anyway. These unscaled values are only used to compute
    # percentage-wise allocation of fuels to specific services.
    EIA_AEO_Tab5 %>%
      filter(year == max(HISTORICAL_YEARS),
             variable != "Floorspace") %>%
      select(EIA_fuel = fuel,
             EIA_service = service,
             AEO_target = value) %>%
      mutate(EIA_sector = "Commercial") %>%
      left_join_error_no_match(EIA_AEO_fuels, by = c("EIA_fuel")) %>%
      left_join_error_no_match(EIA_AEO_services, by = c("EIA_service", "EIA_sector")) %>%
      group_by(fuel, service) %>%
      summarise(AEO_target = sum(AEO_target)) %>%
      ungroup() -> L144.EIA_AEO_target

    L144.in_EJ_state_comm_F_U_Y_unscaled %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      group_by(fuel, service) %>%
      summarise(initial = sum(value)) %>%
      ungroup() %>%
      left_join_error_no_match(L144.EIA_AEO_target, by = c("fuel", "service")) %>%
      # For "comm other" electricity, most of it is non-building, and will be taken into account below.
      # For now, just set "comm other" electricity scaler to 1.
      mutate(scaler = if_else(fuel == "electricity" & service == "comm other",
                              1, AEO_target / initial )) -> L144.scaler_USA_comm_F_U_2010

    # Multiply state-level un-scaled energy use by these scalers prior to calculating end-use proportions
    L144.in_EJ_state_comm_F_U_Y_unscaled %>%
      left_join_error_no_match(L144.scaler_USA_comm_F_U_2010 %>%
                                 select(fuel, service, scaler),
                               by = c("fuel", "service")) %>%
      mutate(value = if_else(year == max(HISTORICAL_YEARS), value * scaler, value)) %>%
      select(-scaler) -> L144.in_EJ_state_comm_F_U_Y_unscaled


    # Calculating shares of energy consumption by each service, within each state and fuel
    L144.pct_state_comm_F_U_Y <- L144.in_EJ_state_comm_F_U_Y_unscaled %>%
      group_by(state, fuel, year) %>%
      mutate(value = value / sum(value)) %>%
      ungroup() %>%
      mutate(sector = "comm")

    # At this point we can disaggregate the state-level energy consumption by sector and fuel to the specific end uses
    # Non-building electricity use by state is estimated separately, and deducted from state-wide commercial electricity consumption
    # National non-building electricity use is disaggregated to states according to population shares
    L144.in_TWh_USA_commext_elec <- PNNL_Commext_elec %>%
      gather(variable, value, -Year, -unit) %>%
      rename(year = Year) %>%
      mutate(service = "comm non-building",
             value_EJ = value * CONV_TWH_EJ)

    # Aggregate national non-building electricity use by year
    L144.in_EJ_USA_commext_elec <- L144.in_TWh_USA_commext_elec %>%
      group_by(year, service) %>%
      summarise(value_EJ = sum(value_EJ)) %>%
      ungroup()

    # This dataset needs to be expanded to all historical years. Use population ratios
    first_year_commext <- min(PNNL_Commext_elec$Year)
    last_year_commext <- max(PNNL_Commext_elec$Year)

    commext_Census_pop_hist <- Census_pop %>%
      group_by(year) %>%
      summarise(sum = sum(value)) %>%
      ungroup() %>%
      # For each year, add ratio to min PNNL_Commext_elec year
      transmute(year,
                pre = sum / sum[year == first_year_commext])

    # One more adjustment - set 2010 comm exterior other ("comm non-building") electricity use equal to
    # AEO_target minus unscaled aggregated 2010 value for "comm other". Interpolate back.
    # This separates out "comm non-building" from "comm other" to provide a better estimate of
    # "comm non-building" in 2010 (which is outside the time scope of PNNL_Commext_elec) than the
    # previous method of scaling by population growth.
    comm_ext_2010 <- L144.scaler_USA_comm_F_U_2010 %>%
      filter(fuel == "electricity" & service == "comm other") %>%
      mutate(AEO_target = AEO_target - initial) %>%
      distinct(AEO_target)
    comm_ext_2010 <- unique(comm_ext_2010$AEO_target)

    # Expand to all historical years
    L144.in_EJ_USA_commext_elec <- tibble(year = HISTORICAL_YEARS,
                                                    service = "comm non-building") %>%
      # Using left_join b/c not all historical years in L144.in_EJ_USA_commext_elec
      left_join(L144.in_EJ_USA_commext_elec, by = c("year", "service")) %>%
      # Add in population ratios
      left_join_error_no_match(commext_Census_pop_hist, by = "year") %>%
      # Interpolate to all historical years, using adjusted 2010 value described above
      mutate(value_EJ = if_else(year==max(HISTORICAL_YEARS), comm_ext_2010, value_EJ),
        # If year is above the max PNNL_Commext_elec year, interpolate to adjusted 2010 value
        value_EJ = if_else(year > last_year_commext, approx_fun(year, value_EJ), value_EJ),
        value_EJ = approx_fun(year, value_EJ, rule = 2),
        # If year is below the min PNNL_Commext_elec year, multiply by population ratio to min year
        value_EJ = if_else(year < first_year_commext, value_EJ * pre, value_EJ)) %>%
      select(service, year, value_EJ)

    # Population ratio of each state by year
    L144.pct_state_commext_elec_Y <- Census_pop %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      group_by(year) %>%
      mutate(value = value / sum(value)) %>%
      ungroup() %>%
      mutate(sector = "comm",
             fuel = "electricity",
             service = "comm non-building")

    # Downscale national non-building electricity use to state by population ratio
     L144.in_EJ_state_commext_F_U_Y <- L144.pct_state_commext_elec_Y %>%
       left_join_error_no_match(L144.in_EJ_USA_commext_elec, by = c("service", "year")) %>%
       # State value = state proportion * USA value
       mutate(state_EJ = value * value_EJ) %>%
       select(state, sector, fuel, service, year, value = state_EJ)

     # Commercial non-building electricity is not scaled; it is deducted from the top-down estimate of commercial electricity use
     # Calculate total commercial building electricity use by subtracting non-building use
     L144.in_EJ_state_commint_elec <- L142.in_EJ_state_bld_F %>%
       filter(sector == "comm",
              fuel == "electricity") %>%
       left_join_error_no_match(L144.in_EJ_state_commext_F_U_Y, by = c("state", "year", "sector", "fuel")) %>%
       mutate(value = value.x - value.y) %>%
       select(-service, -value.x, - value.y)

     # Bind this back to the initial table of commercial energy use by state and fuel
     L144.in_EJ_state_commint_F <- L142.in_EJ_state_bld_F %>%
       filter(sector == "comm", fuel != "electricity") %>%
       bind_rows(L144.in_EJ_state_commint_elec)


     # This energy can now be apportioned to the end-use services
     L144.in_EJ_state_commint_F_U_Y <- L144.pct_state_comm_F_U_Y %>%
       left_join_error_no_match(L144.in_EJ_state_commint_F, by = c("state", "sector", "fuel", "year")) %>%
       # Value for state/sector/service/fuel = value for state/sector/fuel * share of service in state/sector/fuel
       mutate(value = value.x * value.y) %>%
       select(state, sector, fuel, service, year, value)

     # Bind the building (interior) and non-building (exterior) energy use tables
     L144.in_EJ_state_comm_F_U_Y <- bind_rows(L144.in_EJ_state_commint_F_U_Y, L144.in_EJ_state_commext_F_U_Y)

     # This table needs to have coal and biomass added; just assign these to heating
     L144.in_EJ_state_comm_FnoCBECS <- L142.in_EJ_state_bld_F %>%
       filter(sector == "comm",
              !(fuel %in% L144.in_EJ_state_comm_F_U_Y$fuel)) %>%
       mutate(service = "comm heating")

    L144.in_EJ_state_comm_F_U_Y <- L144.in_EJ_state_comm_F_U_Y %>%
      bind_rows(L144.in_EJ_state_comm_FnoCBECS)


    # ===================================================

    # Produce outputs
    L144.flsp_bm2_state_comm %>%
      add_title("Commercial floorspace by state") %>%
      add_units("billion m2") %>%
      add_comments("CBECS data used to calculate per-capita census division value") %>%
      add_comments("Floorspace by state calculated by multiplying state population by per-capita census division floorspace") %>%
      add_comments("Most recent values scaled by ratio of national value from CBECS data to AEO estimates") %>%
      add_legacy_name("L144.flsp_bm2_state_comm") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/Census_pop",
                     "gcam-usa/EIA_AEO_Tab5",
                     "gcam-usa/CBECS_1979_1983",
                     "gcam-usa/CBECS_1986",
                     "gcam-usa/CBECS_1989",
                     "gcam-usa/CBECS_1992",
                     "gcam-usa/CBECS_1995",
                     "gcam-usa/CBECS_1999",
                     "gcam-usa/CBECS_2003",
                     "gcam-usa/CBECS_2012") ->
      L144.flsp_bm2_state_comm

    L144.in_EJ_state_comm_F_U_Y %>%
      add_title("Commercial energy consumption by state/fuel/end use") %>%
      add_units("EJ/yr") %>%
      add_comments("For commercial building use, scales data from L142.in_EJ_state_bld_F using CBECS data") %>%
      add_comments("For commercial non-building use, expands PNNL_Commext_elec to all states and historical years ") %>%
      add_legacy_name("L144.in_EJ_state_comm_F_U_Y") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/Census_pop",
                     "gcam-usa/CBECS_variables",
                     "gcam-usa/EIA_AEO_fuels",
                     "gcam-usa/EIA_AEO_services",
                     "gcam-usa/EIA_AEO_Tab5",
                     "gcam-usa/EIA_distheat",
                     "gcam-usa/PNNL_Commext_elec",
                     "gcam-usa/CBECS_1979_1983",
                     "gcam-usa/CBECS_1986",
                     "gcam-usa/CBECS_1989",
                     "gcam-usa/CBECS_1992",
                     "gcam-usa/CBECS_1995",
                     "gcam-usa/CBECS_1999",
                     "gcam-usa/CBECS_2003",
                     "gcam-usa/CBECS_2012",
                     "L142.in_EJ_state_bld_F",
                     "L143.share_state_Pop_CDD_sR9",
                     "L143.share_state_Pop_HDD_sR9") ->
      L144.in_EJ_state_comm_F_U_Y

    return_data(L144.flsp_bm2_state_comm, L144.in_EJ_state_comm_F_U_Y)
  } else {
    stop("Unknown command")
  }
}
