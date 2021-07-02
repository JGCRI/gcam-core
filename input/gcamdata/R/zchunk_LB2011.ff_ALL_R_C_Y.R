# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LB2011.ff_ALL_R_C_Y
#'
#' Calculate fossil fuel energy balances, by region / commodity / year and harmonize GCAM's data to Comtrade's.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2011.ff_ALL_EJ_R_C_Y} \code{L2011.ff_GrossTrade_EJ_R_C_Y}.
#' @details This chunk combines fossil fuel production and consumption to calculate energy balances by
#' GCAM region, commodity and year. After calculating GCAM's (assumed) net trade
#' Comtrade's gross trade is adjusted to match.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author JEH Nov 2019
module_energy_LB2011.ff_ALL_R_C_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             "L1011.en_bal_EJ_R_Si_Fi_Yh",
             "L121.in_EJ_R_TPES_crude_Yh",
             "L121.in_EJ_R_TPES_unoil_Yh",
             "L111.Prod_EJ_R_F_Yh",
             "L1011.ff_GrossTrade_EJ_R_C_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2011.ff_ALL_EJ_R_C_Y",
             "L2011.ff_GrossTrade_EJ_R_C_Y"))
  } else if(command == driver.MAKE) {

    year <- value <- GCAM_region_ID <- GCAM_commodity <- region <-  fuel <- year <-
      consumption <- calOutputValue <- . <- sector <- subsector <- production <-
      cal.production <- net_trade <- GrossExp_EJ <- GrossImp_EJ <- technology <-
      GCAM_net_trade <- GrossExp_EJ_old <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1011.en_bal_EJ_R_Si_Fi_Yh")
    L121.in_EJ_R_TPES_crude_Yh <- get_data(all_data, "L121.in_EJ_R_TPES_crude_Yh")
    L121.in_EJ_R_TPES_unoil_Yh <- get_data(all_data, "L121.in_EJ_R_TPES_unoil_Yh")
    L111.Prod_EJ_R_F_Yh <- get_data(all_data, "L111.Prod_EJ_R_F_Yh")
    L1011.ff_GrossTrade_EJ_R_C_Y <- get_data(all_data, "L1011.ff_GrossTrade_EJ_R_C_Y")

    #There is no single file in GCAM that calculates net trade of fossil fuels. To build regional
    # markets for fossil fuels (gas, oil, coal) we need to maintain GCAM's calibrations, so we
    # calculate GCAM's implied net-trade to maintain.

    #This treats natural gas and LNG as one fuel type, but that should be changed in the future.
    # Total production is taken from L111.Prod_EJ_R_F_Yh and total consumption is calculated from
    # L1011.en_bal_EJ_R_Si_Fi_Yh and L121.in_EJ_R_TPES_crude_Yh/unoil.

    #Part 1: Calculate toal consumption of fuels by region
    bind_rows(L1011.en_bal_EJ_R_Si_Fi_Yh,
              L121.in_EJ_R_TPES_crude_Yh,
              L121.in_EJ_R_TPES_unoil_Yh) %>%
      filter(sector == "TPES",
             year %in% HISTORICAL_YEARS,
             fuel %in% c("gas", "coal", "crude oil", "unconventional oil")) %>%
      #Unconventional oil is crude oil
      mutate(fuel = if_else(fuel == "gas", "natural gas", if_else(fuel == "unconventional oil","crude oil",fuel))) %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      select(GCAM_region_ID, fuel, year, consumption = value) ->
      ff_consumption

    #Part 2: Gather total production of fossil fuels
    L111.Prod_EJ_R_F_Yh %>%
      select(GCAM_region_ID, fuel, year, production = value, technology) %>%
      mutate(fuel= if_else(technology=="unconventional oil","crude oil",fuel)) %>%
      complete(fuel = unique(ff_consumption$fuel),
               GCAM_region_ID = unique(GCAM_region_names$GCAM_region_ID),
               year = ff_consumption$year,
               fill = list(production = 0)) %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      mutate(production= sum(production)) %>%
      ungroup() %>%
      select(GCAM_region_ID, fuel, year, production) %>%
      distinct()->
      ff_production

    #Part 3: Calculate net-trade by subtracting consumption from production by region and year
    ff_production %>%
      left_join_error_no_match(ff_consumption, by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(net_trade = production - consumption) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, fuel, year, production, consumption, net_trade) ->
      L2011.ff_ALL_EJ_R_C_Y

    #Part 4: Adjust Comtrade's trade to match GCAM's calibrated data

    L1011.ff_GrossTrade_EJ_R_C_Y %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      complete(GCAM_Commodity = unique(L2011.ff_ALL_EJ_R_C_Y$fuel),
               nesting(GCAM_region_ID, year)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(L2011.ff_ALL_EJ_R_C_Y %>% select(region, fuel, year, GCAM_net_trade = net_trade),
                by = c("region", "GCAM_Commodity" = "fuel", "year")) %>%
      mutate(net_trade = if_else(is.na(net_trade), GCAM_net_trade, net_trade),
             GrossExp_EJ = if_else(is.na(GrossExp_EJ), if_else(GCAM_net_trade>0, GCAM_net_trade, 0), GrossExp_EJ ),
             GrossImp_EJ = if_else(is.na(GrossImp_EJ), if_else(GCAM_net_trade<=0, -1*GCAM_net_trade, 0), GrossImp_EJ )) %>%
      #We will maintain GCAM's calibration values and harmonize net_trade by scaling Comtrade's imports and exports by the ratio between GCAM's net trade and comtrade's
      mutate(GrossExp_EJ = if_else(!is.na(GrossExp_EJ * GCAM_net_trade/net_trade) & !is.infinite(GrossExp_EJ * GCAM_net_trade/net_trade), GrossExp_EJ * GCAM_net_trade/net_trade, GrossExp_EJ),
             GrossImp_EJ = if_else(!is.na(GrossImp_EJ * GCAM_net_trade/net_trade) & !is.infinite(GrossImp_EJ * GCAM_net_trade/net_trade), GrossImp_EJ * GCAM_net_trade/net_trade, GrossImp_EJ),
             net_trade = GrossExp_EJ - GrossImp_EJ) %>%
      #There are a few rows where Comtrade says a region is an importer and GCAM says they're an exporter
      #This discrepency makes the gross imports and exports negative which cannot happen.
      #We turn the values positive and flip imports and exports to maintain GCAM's values
      mutate(GrossExp_EJ_old = GrossExp_EJ,
             GrossExp_EJ = if_else(GrossExp_EJ<0, -1*GrossImp_EJ, GrossExp_EJ),
             GrossImp_EJ = if_else(GrossImp_EJ<0, -1*GrossExp_EJ_old, GrossImp_EJ),
             net_trade = GrossExp_EJ - GrossImp_EJ) %>%
      select(names(L1011.ff_GrossTrade_EJ_R_C_Y)) ->
      L2011.ff_GrossTrade_EJ_R_C_Final_Cal_Year

    # This structure does not allow regions to trade more product than they produce, so decrease
    # Exports and Imports for any region where GrossExp is greater than production
    L2011.ff_GrossTrade_EJ_R_C_Final_Cal_Year %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(L2011.ff_ALL_EJ_R_C_Y %>% select(region, fuel, year, production),
                               by = c("region", "GCAM_Commodity" = "fuel", "year")) %>%
      mutate(GrossImp_EJ = if_else(GrossExp_EJ>production, GrossImp_EJ - (GrossExp_EJ-production), if_else(GrossExp_EJ==production, GrossImp_EJ - (GrossExp_EJ-0.95*production),GrossImp_EJ)),
             GrossExp_EJ = if_else(GrossExp_EJ>production, production, if_else(GrossExp_EJ==production, 0.95*production,GrossExp_EJ))) %>%
      distinct() %>%
      mutate(GrossImp_EJ = if_else(GrossExp_EJ==production, GrossImp_EJ - (GrossExp_EJ-0.95*production),GrossImp_EJ),
             GrossExp_EJ = if_else(GrossExp_EJ==production, 0.95*production,GrossExp_EJ)) %>%
      select(names(L1011.ff_GrossTrade_EJ_R_C_Y)) ->
      L2011.ff_GrossTrade_EJ_R_C_Final_Cal_Year_adj

    #Only the final calibration period's calibration matters, so for earlier periods simply assume that
    # each region is solely an importer or an exporter.
    L2011.ff_ALL_EJ_R_C_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      filter(! year %in% L2011.ff_GrossTrade_EJ_R_C_Final_Cal_Year_adj$year) %>%
      mutate(GrossExp_EJ = if_else(net_trade<=0, 0, net_trade),
             GrossImp_EJ = if_else(net_trade<0, -1*net_trade, 0),
             GCAM_Commodity = fuel) %>%
      select(names(L2011.ff_GrossTrade_EJ_R_C_Final_Cal_Year_adj)) %>%
      bind_rows(L2011.ff_GrossTrade_EJ_R_C_Final_Cal_Year_adj)->
      L2011.ff_GrossTrade_EJ_R_C_Y

    #Produce outputs
    L2011.ff_ALL_EJ_R_C_Y %>%
      add_title("L2011.ff_ALL_EJ_R_C_Y") %>%
      add_units("EJ") %>%
      add_comments("Calculate fossil fuel net trade by GCAM region, commodity and year") %>%
      add_precursors("common/GCAM_region_names",
                     "L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "L121.in_EJ_R_TPES_crude_Yh",
                     "L121.in_EJ_R_TPES_unoil_Yh",
                     "L111.Prod_EJ_R_F_Yh") ->
      L2011.ff_ALL_EJ_R_C_Y

    L2011.ff_GrossTrade_EJ_R_C_Y %>%
      add_title("L2011.ff_GrossTrade_EJ_R_C_Y") %>%
      add_units("EJ") %>%
      add_comments("Adjust Comtrade fossil fuel net trade to match GCAM's calibrated values by GCAM region, commodity and year") %>%
      add_precursors("common/GCAM_region_names",
                     "L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "L121.in_EJ_R_TPES_crude_Yh",
                     "L121.in_EJ_R_TPES_unoil_Yh",
                     "L111.Prod_EJ_R_F_Yh",
                     "L1011.ff_GrossTrade_EJ_R_C_Y") ->
      L2011.ff_GrossTrade_EJ_R_C_Y

    return_data(L2011.ff_ALL_EJ_R_C_Y, L2011.ff_GrossTrade_EJ_R_C_Y)
  } else {
    stop("Unknown command")
  }
}
