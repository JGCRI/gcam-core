# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_LA144.Residential
#'
#' Calculate residential floorspace by state and residential energy consumption by state/fuel/end use.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L144.flsp_bm2_state_res}, \code{L144.in_EJ_state_res_F_U_Y}. The corresponding file in the
#' original data system was \code{LA144.Residential.R} (gcam-usa level1).
#' @details Calculate residential floorspace by state and residential energy consumption by state/fuel/end use.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select lead lag
#' @importFrom tidyr gather spread
#' @author RLH September 2017; PK and NK, January 2020,   updated for RECS 2015
module_gcamusa_LA144.Residential <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/RECS_variables",
             FILE = "gcam-usa/EIA_AEO_fuels",
             FILE = "gcam-usa/EIA_AEO_services",
             FILE = "gcam-usa/Census_pop",
             FILE = "gcam-usa/AEO_2015_flsp",
             FILE = "gcam-usa/EIA_AEO_Tab4",
             FILE = "gcam-usa/RECS_1979",
             FILE = "gcam-usa/RECS_1984",
             FILE = "gcam-usa/RECS_1990",
             FILE = "gcam-usa/RECS_1993",
             FILE = "gcam-usa/RECS_1997",
             FILE = "gcam-usa/RECS_2001",
             FILE = "gcam-usa/RECS_2005",
             FILE = "gcam-usa/RECS_2009",
             FILE = "gcam-usa/RECS_2015",
             "L142.in_EJ_state_bld_F",
             "L143.share_state_Pop_CDD_sR13",
             "L143.share_state_Pop_HDD_sR13"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L144.flsp_bm2_state_res",
             "L144.in_EJ_state_res_F_U_Y"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    year <- value <- subregion9 <- DIVISION2009 <- DIVISION <- subregion13 <- LRGSTATE <- subregion13 <- REPORTABLE_DOMAIN <-
      state <- subregion9 <- year <- variable <- HOUSEHOLDS <- NWEIGHT <- . <- value.x <- value.y <- variable <- pcflsp_m2 <-
      pcflsp_m2.x <- pcflsp_m2.y <- conv_9_13 <- sector <- fuel <- service <- DIVISION <- val_1993 <- conv <- val_1990 <-
      Fuel <- Service <- tv_1995 <- fuel_sum <- share <- service.x <- Sector <- RECS_flspc_2010 <- scaler <- EIA_sector <-
      val_2009 <- RECS_flspc <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    RECS_variables <- get_data(all_data, "gcam-usa/RECS_variables")
    EIA_AEO_fuels <- get_data(all_data, "gcam-usa/EIA_AEO_fuels")
    EIA_AEO_services <- get_data(all_data, "gcam-usa/EIA_AEO_services")
    Census_pop <- get_data(all_data, "gcam-usa/Census_pop") %>%
      gather_years
    AEO_2015_flsp <- get_data(all_data, "gcam-usa/AEO_2015_flsp")
    EIA_AEO_Tab4 <- get_data(all_data, "gcam-usa/EIA_AEO_Tab4") %>%
      gather_years
    RECS_1979 <- get_data(all_data, "gcam-usa/RECS_1979")
    RECS_1984 <- get_data(all_data, "gcam-usa/RECS_1984")
    RECS_1990 <- get_data(all_data, "gcam-usa/RECS_1990")
    RECS_1993 <- get_data(all_data, "gcam-usa/RECS_1993")
    RECS_1997 <- get_data(all_data, "gcam-usa/RECS_1997")
    RECS_2001 <- get_data(all_data, "gcam-usa/RECS_2001")
    RECS_2005 <- get_data(all_data, "gcam-usa/RECS_2005")
    RECS_2009 <- get_data(all_data, "gcam-usa/RECS_2009")
    RECS_2015 <- get_data(all_data, "gcam-usa/RECS_2015")
    L142.in_EJ_state_bld_F <- get_data(all_data, "L142.in_EJ_state_bld_F", strip_attributes = TRUE)
    L143.share_state_Pop_CDD_sR13 <- get_data(all_data, "L143.share_state_Pop_CDD_sR13")
    L143.share_state_Pop_HDD_sR13 <- get_data(all_data, "L143.share_state_Pop_HDD_sR13")

    # ===================================================
    # a) PREPARATION AND CLEANING OF RECS DATABASES
    # All RECS data has different columns, so we create a list rather than bind_rows
    L144.RECS_all <- list(RECS_1979, RECS_1984, RECS_1990, RECS_1993, RECS_1997, RECS_2001, RECS_2005, RECS_2009, RECS_2015)
    names(L144.RECS_all) <- paste0("RECS", c(1979, 1984, 1990, 1993, 1997, 2001, 2005, 2009, 2015))

    # Add year column to each tibble in list
    for(i in seq_along(L144.RECS_all)) {
      L144.RECS_all[[i]]$year <- substr(names(L144.RECS_all[i]), 5, 8) %>% as.integer()
    }

    # Add a vector specifying the census division (subregion9) in 2015. DIVISION2009 is from the mapping file "states_subregions.csv" and is the same for 2015
    L144.RECS_all <- L144.RECS_all %>%
     lapply(function(df) {
       if(unique(df$year) %in% gcamusa.RECS_YEARS) {

         left_join_error_no_match(df,
                                  states_subregions %>% select(subregion9, DIVISION2009) %>% distinct,
                                  by = c("DIVISION" = "DIVISION2009"))
       } else {
         left_join_error_no_match(df,
                                  states_subregions %>% select(subregion9, DIVISION) %>% distinct,
                                  by = "DIVISION")
       }
     })


    # Add a vector specifying the census division plus four large states (subregion13)
    L144.RECS_all <- L144.RECS_all %>%
      lapply(function(df) {
        if("LRGSTATE" %in% names(df)) {
          left_join_error_no_match(df,
                                   states_subregions %>% select(subregion13, LRGSTATE, subregion9) %>% distinct,
                                   by = c("LRGSTATE", "subregion9"))
        } else {
          # Return normal tibble if large states not specified
          df
        }
      })

    # The 2009 RECS uses 27 "reportable domains", which consist of 16 single states and 11 small clusters of states.
    # while this would enhance the geographic specificity of the RECS data, the small sample sizes cause obviously incorrect
    # data on aggregation (e.g., states with < 300 ft2 of residential floorspace per capita).
    # For this reason, we use subregion13, consistent with the 1993-2005 RECS
    L144.RECS_all$RECS2009 <- L144.RECS_all$RECS2009 %>%
      left_join_error_no_match(states_subregions %>%
                                 select(subregion13, REPORTABLE_DOMAIN) %>%
                                 distinct(), by = "REPORTABLE_DOMAIN")
    # Convert all missing value strings to 0 in all databases
    L144.RECS_all[["RECS1990"]][L144.RECS_all[["RECS1990"]] == 9999999] <- 0
    L144.RECS_all[["RECS2005"]][L144.RECS_all[["RECS2005"]] == 9999999] <- 0
    L144.RECS_all[["RECS2015"]][L144.RECS_all[["RECS2015"]] == 9999999] <- 0
    L144.RECS_all[["RECS2005"]][is.na(L144.RECS_all[["RECS2005"]]) ] <- 0
    L144.RECS_all[["RECS2015"]][is.na(L144.RECS_all[["RECS2015"]]) ] <- 0

    # Aggregate population to the subregion9 and subregion13 levels for calculation of per-capita values
    L144.Census_pop <- Census_pop %>%
      left_join_error_no_match(states_subregions, by = "state") %>%
      select(state, year, value, subregion9, subregion13)

    L144.pop_sR13 <- L144.Census_pop %>%
      group_by(subregion13, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    L144.pop_sR9 <- L144.Census_pop %>%
      group_by(subregion9, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # b) FLOORSPACE BY STATE AND YEAR
    # Estimating total floorspace by census division (subregion9) and subregion13 if available, in each RECS year where floorspace is available
    # The variable names differ by edition, but in no case does a variable name mean one thing in one edition and another thing in a later edition
    # HOUSEHOLDS is only in the 1984 edition. 1984 also has a different unit on the weight
    flsp_vars <- c("UNHEATED", "HOMEAREA", "SQFTREG", "TOTSQFT")
    # Per-capita floorspace by subregion9
    L144.flsp_bm2_sR9 <- L144.RECS_all %>%
      lapply(function(df) {
        # For RECS1984
        if("HOUSEHOLDS" %in% names(df)) {
          flsp_var <- names(df)[which(names(df) %in% flsp_vars)]
          df %>%
            select(tidyselect::all_of(c("year", "subregion9", "HOUSEHOLDS", flsp_var))) %>%
            gather("variable", "value", tidyselect::all_of(flsp_var)) %>%
            group_by(year, subregion9, variable) %>%
            summarise(value = sum(value * HOUSEHOLDS * CONV_MILFT2_M2)) %>%
            ungroup()
        } else {
          # For all other years that have weight category to multiply by
          if("NWEIGHT" %in% names(df)) {
            flsp_var <- names(df)[which(names(df) %in% flsp_vars)]
            df %>%
              select(tidyselect::all_of(c("year", "subregion9", "NWEIGHT", flsp_var))) %>%
              gather("variable", "value", tidyselect::all_of(flsp_var)) %>%
              group_by(year, subregion9, variable) %>%
              summarise(value = sum(value * NWEIGHT * CONV_FT2_M2)) %>%
              ungroup()
          } else {
            # Return empty tibble for binding rows
            tibble()
          }
        }
      }) %>%
      do.call(bind_rows, .) %>%
      left_join_error_no_match(L144.pop_sR9, by = c("year", "subregion9")) %>%
      mutate(pcflsp_m2 = value.x / value.y) %>%
      select(year, subregion9, variable, pcflsp_m2)

    # Per-capita floorspace by subregion13
    L144.flsp_bm2_sR13 <- L144.RECS_all %>%
      lapply(function(df) {
        if("subregion13" %in% names(df)) {
          flsp_var <- names(df)[which(names(df) %in% flsp_vars)]
          df %>%
            select(tidyselect::all_of(c("year", "subregion13", "NWEIGHT", flsp_var))) %>%
            gather("variable", "value", tidyselect::all_of(flsp_var)) %>%
            group_by(year, subregion13, variable) %>%
            summarise(value = sum(value * NWEIGHT * CONV_FT2_M2)) %>%
            ungroup()
        } else {
          # Return empty tibble for binding rows
          tibble()
        }
      }) %>%
      do.call(bind_rows, .) %>%
      left_join_error_no_match(L144.pop_sR13, by = c("year", "subregion13")) %>%
      mutate(pcflsp_m2 = value.x / value.y) %>%
      select(year, subregion13, pcflsp_m2)

    # Downscale 1990 and 1984 floorspace to subregion13, using the ratios of per-capita floorspace
    # GPK - also do this from the 2009 to the 2015 years b/c 2015 doesn't have LRGSTATE
    L144.flsp_conv_9_13_EARLY <- L144.flsp_bm2_sR13 %>%
      filter(year == 1993) %>%
      left_join_error_no_match(states_subregions %>%
                                 select(subregion9, subregion13) %>%
                                 distinct(), by = "subregion13") %>%
      left_join_error_no_match(L144.flsp_bm2_sR9, by = c("year", "subregion9")) %>%
      mutate(conv_9_13 = pcflsp_m2.x / pcflsp_m2.y) %>%
      select(subregion13, subregion9, conv_9_13)

    L144.flsp_conv_9_13_RECENT <- L144.flsp_bm2_sR13 %>%
      filter(year == 2009) %>%
      left_join_error_no_match(states_subregions %>%
                                 select(subregion9, subregion13) %>%
                                 distinct(), by = "subregion13") %>%
      left_join_error_no_match(L144.flsp_bm2_sR9, by = c("year", "subregion9")) %>%
      mutate(conv_9_13 = pcflsp_m2.x / pcflsp_m2.y) %>%
      select(subregion13, subregion9, conv_9_13)


    # Find years that don't have subregion13
    conv_years <- dplyr::setdiff(unique(L144.flsp_bm2_sR9$year), unique(L144.flsp_bm2_sR13$year))
    allRECS_year <- union(unique(L144.flsp_bm2_sR9$year), unique(L144.flsp_bm2_sR13$year))
    EARLY_years <- conv_years[conv_years < 1993]
    RECENT_years <- conv_years[conv_years > 2009]

    # Multiplying the per-capita floorspace ratios from subregion9 to subregion13, to expand from 9 to 13
    L144.flsp_bm2_sR13_EARLY <- L144.flsp_conv_9_13_EARLY %>%
      # Using left_join b/c not all years have subregion13 value yet
      left_join(L144.flsp_bm2_sR9 %>% filter(year %in% EARLY_years), by = "subregion9") %>%
      mutate(pcflsp_m2 = pcflsp_m2 * conv_9_13) %>%
      select(subregion13, year, pcflsp_m2)
    L144.flsp_bm2_sR13_RECENT <- L144.flsp_conv_9_13_RECENT %>%
      # Using left_join b/c not all years have subregion13 value yet
      left_join(L144.flsp_bm2_sR9 %>% filter(year %in% RECENT_years), by = "subregion9") %>%
      mutate(pcflsp_m2 = pcflsp_m2 * conv_9_13) %>%
      select(subregion13, year, pcflsp_m2)

    # Bind the early, the years with LRGSTATE, and the recent data frames
    L144.flsp_bm2_sR13 <- bind_rows(L144.flsp_bm2_sR13_EARLY,
                                    L144.flsp_bm2_sR13,
                                    L144.flsp_bm2_sR13_RECENT)


    # Interpolate to all historical years
    L144.pcflsp_m2_sR13_RECS <- tidyr::crossing(subregion13 = unique(L144.flsp_bm2_sR13$subregion13),
                                                year = HISTORICAL_YEARS) %>%
      # Use left_join b/c not all years in L144.flsp_bm2_sR13
      left_join(L144.flsp_bm2_sR13, by = c("subregion13", "year")) %>%
      group_by(subregion13) %>%
      mutate(pcflsp_m2 = approx_fun(year, pcflsp_m2, rule = 2)) %>%
      ungroup()

    # Expand to states: multiply per-capita floorspace in each subregion13 times the population of each state
    L144.flsp_bm2_state_res <- L144.Census_pop %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      mutate(sector = "resid") %>%
      left_join_error_no_match(L144.pcflsp_m2_sR13_RECS, by = c("year", "subregion13")) %>%
      mutate(value = value * pcflsp_m2 / CONV_BM2_M2) %>%
      select(state, subregion13, sector, year, value)

    # Final step - adjustment for AEO 2015 harmonization. NEMS residential floorspace is a lot lower than RECS.
    L144.flsp_bm2_state_res    %>%
      group_by(sector, year) %>%
      summarise(RECS_flspc = sum(value)) %>%
      ungroup() %>%
      select(-sector) %>%
      # use left_join due to lack of AEO data prior to 1993
      left_join(AEO_2015_flsp %>%
                gather_years() %>%
                filter(Sector == "Residential") %>%
                select(-Sector),
              by = c("year")) %>%
      mutate(scaler = value / RECS_flspc) %>%
      # extrapolate the scaler to years before 1993
      mutate(scaler =if_else(is.na(scaler),approx_fun(year, scaler, rule = 2),scaler)) %>%
      select(year, scaler) -> L144.flsp_scaler

   L144.flsp_bm2_state_res %>%
      left_join_error_no_match(L144.flsp_scaler, by = c("year")) %>%
      mutate(value = if_else(year %in% HISTORICAL_YEARS, value * scaler, value)) %>%
      select(-scaler) -> L144.flsp_bm2_state_res

    # c) ENERGY CONSUMPTION BY STATE, SERVICE, AND YEAR
    # Aggregating energy consumption by sampling weights
    # First, do 1990, 1993 and 2015 by subregion9
    L144.in_EJ_sR9 <- L144.RECS_all %>%
      lapply(function(df) {
        if(unique(df$year) %in% c(1990, 1993, 2009, 2015)) {
          df %>%
            gather(variable, value, -NWEIGHT, -year, -subregion9) %>%
            filter(variable %in% unique(RECS_variables$variable)) %>%
            mutate(NWEIGHT = as.numeric(NWEIGHT),
                   value = as.numeric(value)) %>%
            group_by(subregion9, year, variable) %>%
            summarise(value = sum(value * NWEIGHT * CONV_KBTU_EJ, na.rm = TRUE)) %>%
            ungroup()
        } else {
          tibble()
        }
      }) %>%
      do.call(bind_rows, .)

    # Then, aggregate from 1993 to 2009 in at the subregion13 level
    L144.in_EJ_sR13 <- L144.RECS_all %>%
      lapply(function(df) {
        if(unique(df$year) %in% c(1993:2009)) {
          df %>%
            gather(variable, value, -NWEIGHT, -year, -subregion13) %>%
            filter(variable %in% unique(RECS_variables$variable)) %>%
            mutate(NWEIGHT = as.numeric(NWEIGHT),
                   value = as.numeric(value)) %>%
            group_by(subregion13, year, variable) %>%
            summarise(value = sum(value * NWEIGHT * CONV_KBTU_EJ, na.rm = TRUE)) %>%
            ungroup()
        } else {
          tibble()
        }
      }) %>%
      do.call(bind_rows, .)

    # Match in GCAM fuel and service, and aggregate to fuel and service (this will get rid of the different liquid fuels)
    L144.in_EJ_sR9_res_F_U_Y <- L144.in_EJ_sR9 %>%
      left_join_error_no_match(RECS_variables, by = "variable") %>%
      group_by(subregion9, fuel, service, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    L144.in_EJ_sR13_res_F_U_Y <- L144.in_EJ_sR13 %>%
      left_join_error_no_match(RECS_variables, by = "variable") %>%
      group_by(subregion13, fuel, service, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # Add 1979 to the 9-region table, and use the 9-subregion data to scale the 13-subregion data back to 1990 and 1979
    # We sum by service and fuel first
    L144.RECS_1979 <- RECS_1979 %>%
      left_join_error_no_match(states_subregions %>%
                                 select(subregion9, DIVISION) %>%
                                 distinct(), by = "DIVISION") %>%
      gather(variable, value, -DIVISION, -subregion9) %>%
      mutate(value = value * CONV_TBTU_EJ,
             variable = sub("TBTU", "BTU", variable),
             year = 1979) %>%
      # Add in GCAM fuel and service
      left_join_error_no_match(RECS_variables, by = "variable") %>%
      group_by(subregion9, fuel, service, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # Calculate 1990 conversion factor using 1990/1993 ratio
    L144.in_EJ_sR9_res_F_U_Y <- L144.in_EJ_sR9_res_F_U_Y %>%
      bind_rows(L144.RECS_1979) %>%
      group_by(subregion9, fuel, service) %>%
      mutate(conv = if_else(year < 2009,
                            value / lead(value, n = 1L, order_by = year),
                            value / lag(value, n = 1L, order_by = year))) %>%
      ungroup() %>%
      replace_na(list(conv = 1))

    # Find years for converting
    conv_years <- dplyr::setdiff(unique(L144.in_EJ_sR9_res_F_U_Y$year), unique(L144.in_EJ_sR13_res_F_U_Y$year))
    allyears <- union(unique(L144.in_EJ_sR9_res_F_U_Y$year), unique(L144.in_EJ_sR13_res_F_U_Y$year))

    # It would be possible to do this without hardcoding years, but not sure if worth time and complicated code given that RECS years are set
    # Converting 1990 to subregion13 using 1993 values and then use those new 1990 values to convert 1979 to subregion13
    L144.in_EJ_sR13_res_F_U_Y <- L144.in_EJ_sR13_res_F_U_Y %>%
      left_join_error_no_match(states_subregions %>%
                                 select(subregion13, subregion9) %>%
                                 distinct, by = "subregion13") %>%
      # Add all years to subregion13, fuel, service, subregion9
      select(-year, -value) %>%
      distinct() %>%
      repeat_add_columns(tibble(year = sort(allyears))) %>%
      # Using left_join b/c 1979 and 1990 doesn't currently have subregion13 values
      left_join(L144.in_EJ_sR13_res_F_U_Y, by = c("fuel", "service", "subregion13", "year")) %>%
      # Add in subregion9 value for 1990 and 1993
      left_join(L144.in_EJ_sR9_res_F_U_Y, by = c("fuel", "service", "subregion9", "year")) %>%
      group_by(subregion13, fuel, service, subregion9) %>%
      mutate(val_1993 = value.x[year == 1993],
             val_2009 = value.x[year == 2009]) %>%
      ungroup() %>%
      # If year is 1990, replace it with 1993 value * conversion factor
      mutate(value.x = if_else(year == 1990, val_1993 * conv, value.x),
             value.x = if_else(year == 2015, val_2009 * conv, value.x)) %>%
      # Next go from 1990 back to 1979. Need to re-group to get the val_1990 (which wasn't available above)
      group_by(subregion13, fuel, service, subregion9) %>%
      mutate(val_1990 = value.x[year == 1990]) %>%
      ungroup() %>%
      # If year is 1979, replace it with 1990 value * conversion factor
      mutate(value.x = if_else(year == 1979, val_1990 * conv, value.x)) %>%
      select(subregion13, fuel, service, year, value = value.x)

    # Interpolate and extrapolate all missing years
    L144.in_EJ_sR13_res_F_U_Y <- L144.in_EJ_sR13_res_F_U_Y %>%
      select(-year, -value) %>%
      distinct() %>%
      repeat_add_columns(tibble(year = HISTORICAL_YEARS)) %>%
      left_join(L144.in_EJ_sR13_res_F_U_Y, by = c("fuel", "service", "subregion13", "year")) %>%
      group_by(subregion13, fuel, service) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup()

    # Next, disaggregate "appliances and other" to all relevant services that are being modeled
    # Using EIA AEO Table 4 from 1996-2013 editions to disaggregate residential appliances and other to more specific end uses
    # NOTE: The national averages of appliances and other energy are assumed constant in all states
    # NOTE: The national estimate for lighting energy increased substantially from 1999 to 2000 because a different method was used
    # NOTE: We double the lighting energy in the prior years, deducting the balance from Other Uses
    EIA_AEO_years <- seq(1993, 2015)
    lgt_adj_years <- seq(1993, 1999)
    L144.EIA_AEO_Tab4_lighting_fix <- EIA_AEO_Tab4 %>%
      filter(Fuel == "Electricity", Service %in% c("Other Uses", "Lighting")) %>%
      group_by(year) %>%
      # Subtract lighting from other uses, then double lighting value
      mutate(value = if_else(Service == "Other Uses" & year %in% lgt_adj_years, value - value[Service == "Lighting"], value),
             value = if_else(Service == "Lighting" & year %in% lgt_adj_years, value * 2, value)) %>%
      ungroup()

    # Now remove old lighting and Other Uses values and replace with new ones
    L144.EIA_AEO_Tab4 <- EIA_AEO_Tab4 %>%
      filter(!(Fuel == "Electricity" & Service %in% c("Other Uses", "Lighting"))) %>%
      bind_rows(L144.EIA_AEO_Tab4_lighting_fix)

    # NOTE: Television energy was not estimated prior to 1995. Copying the 1995 output to prior years
    L144.EIA_AEO_Tab4_tv_fix <-  L144.EIA_AEO_Tab4 %>%
      filter(Fuel == "Electricity", Service %in% c("Other Uses", "Color Televisions")) %>%
      # Subtract 1995 TV value from Other Uses years prior to 1995 & assign 1995 TV values to TV prior to 1995
      mutate(tv_1995 = value[Service == "Color Televisions" & year == 1995],
             value = if_else(Service == "Other Uses" & year < 1995, value - tv_1995, value),
             value = if_else(Service == "Color Televisions" & year < 1995, tv_1995, value)) %>%
      select(-tv_1995)

    # Now remove old TV and Other Uses values and replace with new ones
    L144.EIA_AEO_Tab4 <- L144.EIA_AEO_Tab4 %>%
      filter(!(Fuel == "Electricity" & Service %in% c("Other Uses", "Color Televisions"))) %>%
      bind_rows(L144.EIA_AEO_Tab4_tv_fix)

    # Match in GCAM services and fuels
    L144.EIA_AEO_Tab4 <- L144.EIA_AEO_Tab4 %>%
      left_join_error_no_match(EIA_AEO_fuels, by = c("Fuel" = "EIA_fuel")) %>%
      left_join_error_no_match(EIA_AEO_services %>%
                                 filter(EIA_sector == "Residential") %>%
                                 select(-EIA_sector),
                               by = c("Service" = "EIA_service"))

    # Compute shares of "appliances and other" energy
    # Select services to keep
    appl_other_services <- EIA_AEO_services %>%
      filter(EIA_sector == "Residential") %>%
      select(service) %>%
      filter(!(service %in% unique(RECS_variables$service)))

    # Aggregate appliances and other by service and fuel
    L144.EIA_AEO_appl_other_F <- L144.EIA_AEO_Tab4 %>%
      filter(service %in% unique(appl_other_services$service)) %>%
      group_by(service, fuel, year) %>%
      summarise(value = sum(value)) %>%
      group_by(fuel, year) %>%
      # Add in sum by fuel
      mutate(fuel_sum = sum(value)) %>%
      ungroup()

    # Compute shares of service in fuel. These will be used in all states
    L144.shares_appl_other_F <- L144.EIA_AEO_appl_other_F %>%
      mutate(share = value / fuel_sum) %>%
      select(service, fuel, year, share)

    # Extrapolate to all historical years
    L144.shares_appl_other_F <- L144.shares_appl_other_F %>%
      select(service, fuel) %>%
      distinct() %>%
      repeat_add_columns(tibble(year = HISTORICAL_YEARS)) %>%
      left_join(L144.shares_appl_other_F, by = c("service", "fuel", "year")) %>%
      group_by(service, fuel) %>%
      mutate(share = approx_fun(year, share, rule = 2)) %>%
      ungroup()

    # At this point, we have a table of energy by subregion13, fuel, and end use that needs to be (a) apportioned to states, and (b) scaled
    # The reason for apportioning to states first is that the heating and cooling energy will be modified by pop-weighted HDD and CDD
    # prior to calculating energy shares

    # Downscaling heating and cooling energy to states according to person-HDD and -CDD
    L144.in_EJ_state_res_F_heating_Y <- states_subregions %>%
      select(state, subregion13) %>%
      repeat_add_columns(tidyr::crossing(fuel = c("electricity", "gas", "refined liquids"), year = HISTORICAL_YEARS)) %>%
      mutate(service = "resid heating") %>%
      # Add resid heating energy data
      left_join_error_no_match(L144.in_EJ_sR13_res_F_U_Y, by = c("subregion13", "fuel", "service", "year")) %>%
      # Add in HDD proportions
      left_join_error_no_match(L143.share_state_Pop_HDD_sR13, by = c("state", "subregion13", "year")) %>%
      # Assign state value as subregion13 value * HDD proportion
      mutate(value = value.x * value.y) %>%
      select(state, subregion13, fuel, service, year, value)

    L144.in_EJ_state_res_F_cooling_Y <- states_subregions %>%
      select(state, subregion13) %>%
      repeat_add_columns(tibble(fuel = "electricity", year = HISTORICAL_YEARS)) %>%
      mutate(service = "resid cooling") %>%
      # Add resid cooling energy data
      left_join_error_no_match(L144.in_EJ_sR13_res_F_U_Y, by = c("subregion13", "fuel", "service", "year")) %>%
      # Add in CDD proportions
      left_join_error_no_match(L143.share_state_Pop_CDD_sR13, by = c("state", "subregion13", "year")) %>%
      # Assign state value as subregion13 value * CDD proportion
      mutate(value = value.x * value.y) %>%
      select(state, subregion13, fuel, service, year, value)

    # Downscaling water heating energy to states according to population
    # First calculate state share of subregion13 population
    L144.state_pop_share_sR13 <- L144.Census_pop %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(L144.pop_sR13, by = c("subregion13", "year")) %>%
      mutate(value = value.x / value.y) %>%
      select(state, subregion13, year, value)

    L144.in_EJ_state_res_F_hotwater_Y <- states_subregions %>%
      select(state, subregion13) %>%
      repeat_add_columns(tidyr::crossing(fuel = c("electricity", "gas", "refined liquids"), year = HISTORICAL_YEARS)) %>%
      mutate(service = "resid hot water") %>%
      # Add resid hot water energy data
      left_join_error_no_match(L144.in_EJ_sR13_res_F_U_Y, by = c("subregion13", "fuel", "service", "year")) %>%
      left_join_error_no_match(L144.state_pop_share_sR13, by = c("state", "subregion13", "year")) %>%
      # Assign state value as subregion13 value * state proportion
      mutate(value = value.x * value.y) %>%
      select(state, subregion13, fuel, service, year, value)

    # Downscaling appliances and other energy to states according to floorspace
    L144.in_EJ_sR13_res_F_apploth_Y <- L144.in_EJ_sR13_res_F_U_Y %>%
      filter(service == "resid appliances and other")

    # Floorspace state share of subregion13
    L144.flsp_state_share_sR13 <- L144.flsp_bm2_state_res %>%
      group_by(subregion13, year) %>%
      mutate(value = value / sum(value)) %>%
      ungroup() %>%
      select(state, subregion13, year, value)

    # Calculate state appliance other by multiplying shares by subregion13 energy totals
    L144.in_EJ_state_res_F_apploth_Y <- L144.shares_appl_other_F %>%
      repeat_add_columns(tibble(state = gcamusa.STATES)) %>%
      left_join_error_no_match(states_subregions %>% select(state, subregion13), by = "state") %>%
      left_join_error_no_match(L144.in_EJ_sR13_res_F_apploth_Y, by = c("subregion13", "fuel", "year")) %>%
      left_join_error_no_match(L144.flsp_state_share_sR13, by = c("state", "subregion13", "year")) %>%
      # State value = share of service in fuel * resid appliances and other energy by fuel/subregion13 * state share
      mutate(value = share * value.x * value.y) %>%
      select(state, subregion13, fuel, service = service.x, year, value)

    # Assembling unscaled energy consumption by state, fuel, and service
    L144.in_EJ_state_res_F_U_Y_unscaled <- bind_rows(L144.in_EJ_state_res_F_heating_Y, L144.in_EJ_state_res_F_cooling_Y,
                                                     L144.in_EJ_state_res_F_hotwater_Y, L144.in_EJ_state_res_F_apploth_Y)

    # Calculating shares of energy consumption by each service, within each state and fuel
    L144.in_EJ_state_res_F_Y_unscaled <- L144.in_EJ_state_res_F_U_Y_unscaled %>%
      group_by(state, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # Calculating scaler from RECS data and multiply by L142.in_EJ_state_bld_F data to get final estimates
    L144.in_EJ_state_res_F_U_Y <- L144.in_EJ_state_res_F_U_Y_unscaled %>%
      left_join_error_no_match(L144.in_EJ_state_res_F_Y_unscaled, by = c("state", "fuel", "year")) %>%
      mutate(value = value.x / value.y,
             sector = "resid") %>%
      select(-value.x, -value.y) %>%
      left_join_error_no_match(L142.in_EJ_state_bld_F, by = c("state", "sector", "fuel", "year")) %>%
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel, service, year, value)

    # This table needs to have coal and biomass added; just assign these to heating
    L144.in_EJ_state_bld_Fnorecs <- L142.in_EJ_state_bld_F %>%
      filter(sector == "resid",
             !(fuel %in% unique(L144.in_EJ_state_res_F_U_Y$fuel))) %>%
      mutate(service = "resid heating")

    L144.in_EJ_state_res_F_U_Y <- bind_rows(L144.in_EJ_state_bld_Fnorecs, L144.in_EJ_state_res_F_U_Y)

    # ===================================================

    # Produce outputs
    L144.flsp_bm2_state_res %>%
      add_title("Residential floorspace by state") %>%
      add_units("billion m2") %>%
      add_comments("RECS data interpolated and downscaled to state based on population ratios") %>%
      add_legacy_name("L144.flsp_bm2_state_res") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/Census_pop",
                     "gcam-usa/AEO_2015_flsp",
                     "gcam-usa/RECS_1979",
                     "gcam-usa/RECS_1984",
                     "gcam-usa/RECS_1990",
                     "gcam-usa/RECS_1993",
                     "gcam-usa/RECS_1997",
                     "gcam-usa/RECS_2001",
                     "gcam-usa/RECS_2005",
                     "gcam-usa/RECS_2009",
                     "gcam-usa/RECS_2015") ->
      L144.flsp_bm2_state_res

    L144.in_EJ_state_res_F_U_Y %>%
      add_title("Residential energy consumption by state/fuel/end use") %>%
      add_units("EJ/yr") %>%
      add_comments("Downscaled L142.in_EJ_state_bld_F to states using RECS data") %>%
      add_legacy_name("L144.in_EJ_state_res_F_U_Y") %>%
      add_precursors("gcam-usa/states_subregions",
                     "gcam-usa/RECS_variables",
                     "gcam-usa/EIA_AEO_fuels",
                     "gcam-usa/EIA_AEO_services",
                     "gcam-usa/Census_pop",
                     "gcam-usa/EIA_AEO_Tab4",
                     "gcam-usa/RECS_1979",
                     "gcam-usa/RECS_1984",
                     "gcam-usa/RECS_1990",
                     "gcam-usa/RECS_1993",
                     "gcam-usa/RECS_1997",
                     "gcam-usa/RECS_2001",
                     "gcam-usa/RECS_2005",
                     "gcam-usa/RECS_2009",
                     "gcam-usa/RECS_2015", #2019/12/06 nk: add 2015
                     "L142.in_EJ_state_bld_F",
                     "L143.share_state_Pop_CDD_sR13",
                     "L143.share_state_Pop_HDD_sR13") ->
      L144.in_EJ_state_res_F_U_Y

    return_data(L144.flsp_bm2_state_res, L144.in_EJ_state_res_F_U_Y)
  } else {
    stop("Unknown command")
  }
}
