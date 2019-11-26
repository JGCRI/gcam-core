#' module_energy_LB2011.ff_ALL_R_C_Y
#'
#' Calculate fossil fuel energy balances, by region / commodity / year.
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
      cal.production <- net_trade <- GrossExp_EJ <- GrossImp_EJ <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1011.en_bal_EJ_R_Si_Fi_Yh")
    L121.in_EJ_R_TPES_crude_Yh <- get_data(all_data, "L121.in_EJ_R_TPES_crude_Yh")
    L121.in_EJ_R_TPES_unoil_Yh <- get_data(all_data, "L121.in_EJ_R_TPES_unoil_Yh")
    L111.Prod_EJ_R_F_Yh <- get_data(all_data, "L111.Prod_EJ_R_F_Yh")
    L1011.ff_GrossTrade_EJ_R_C_Y <- get_data(all_data, "L1011.ff_GrossTrade_EJ_R_C_Y")

    #There is no single file in GCAM that calculates net trade of fossil fuels. To build regional
    # markets for fossil fuels (gas, oil, coal) we need to maintain GCAM's calibrations, so we need
    # to calculate a net-trade to maintain.

    #This treats crude oil and unconventional oil as one fuel type
    # and natural gas and LNG as one fuel type, but that may need to be changed.
    # Total production is taken from L111.Prod_EJ_R_F_Yh and total consumption is calculated from
    # L1011.en_bal_EJ_R_Si_Fi_Yh and L121.in_EJ_R_TPES_crude_Yh/unoil.

    #Part 1: Calculate toal consumption of fuels by region
    bind_rows(L1011.en_bal_EJ_R_Si_Fi_Yh,
              L121.in_EJ_R_TPES_crude_Yh,
              L121.in_EJ_R_TPES_unoil_Yh) %>%
      filter(sector == "TPES",
             year %in% HISTORICAL_YEARS,
             fuel %in% c("gas", "coal", "crude oil", "unconventional oil")) %>%
      mutate(fuel = if_else(fuel == "gas", "natural gas", fuel)) %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      select(GCAM_region_ID, fuel, year, consumption = value) ->
      ff_consumption

    #Part 2: Gather total production of fossil fuels
    L111.Prod_EJ_R_F_Yh %>%
      select(GCAM_region_ID, fuel, year, production = value) ->
      ff_production

    #Part 3: Calculate net-trade by subtracting consumption from production by region and year
    ff_production %>%
      left_join_error_no_match(ff_consumption, by = c("GCAM_region_ID", "fuel", "year")) %>%
      mutate(net_trade = production - consumption) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, fuel, year, production, consumption, net_trade) ->
      L2011.ff_ALL_EJ_R_C_Y

    #Part 4: Adjust Comtrade's trade to match GCAM's calibrated data

    #NOTE: There are some LARGE discrepancies between GCAM's data and Comtrade's
    # enough so that I want to confirm how we're converting Comtrade's weight to energy and check
    # that there are no conversion factors we're missing
    L1011.ff_GrossTrade_EJ_R_C_Y %>%
      complete(GCAM_Commodity = unique(L2011.ff_ALL_EJ_R_C_Y$fuel),
               nesting(GCAM_region_ID, year)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(L2011.ff_ALL_EJ_R_C_Y %>% select(region, fuel, year, GCAM_net_trade = net_trade),
                by = c("region", "GCAM_Commodity" = "fuel", "year")) %>%
      #We are creating a traded structure for all fossil fuels even if we don't have data for them (just unconventional oil as of Nov 25th 2019)
      mutate(GCAM_net_trade = if_else(is.na(GCAM_net_trade), 0, GCAM_net_trade),
             net_trade = if_else(is.na(net_trade), GCAM_net_trade, net_trade),
             GrossExp_EJ = if_else(is.na(GrossExp_EJ), if_else(GCAM_net_trade<=0, 0, GCAM_net_trade), GrossExp_EJ ),
             GrossImp_EJ = if_else(is.na(GrossImp_EJ), if_else(GCAM_net_trade>=0, 0, GCAM_net_trade), GrossImp_EJ )) %>%
      #scale both imports and exports by the ratio between GCAM's net trade and comtrade's
      mutate(GrossExp_EJ = if_else(year == max(MODEL_BASE_YEARS), if_else(!is.na(GrossExp_EJ * GCAM_net_trade/net_trade), GrossExp_EJ * GCAM_net_trade/net_trade, GrossExp_EJ), if_else(GCAM_net_trade<=0, 0, GCAM_net_trade) ),
             GrossImp_EJ = if_else(year == max(MODEL_BASE_YEARS), if_else(!is.na(GrossImp_EJ * GCAM_net_trade/net_trade), GrossImp_EJ * GCAM_net_trade/net_trade, GrossImp_EJ), if_else(GCAM_net_trade>=0, 0, GCAM_net_trade) ),
             net_trade = GrossExp_EJ - GrossImp_EJ) %>%
      select(names(L1011.ff_GrossTrade_EJ_R_C_Y)) ->
      L2011.ff_GrossTrade_EJ_R_C_Y

    L2011.ff_ALL_EJ_R_C_Y %>%
      add_title("L2011.ff_ALL_EJ_R_C_Y") %>%
      add_units("EJ") %>%
      add_comments("Calculate fossil fuel net trade by GCAM region, commodity and year") %>%
      add_precursors("common/GCAM_region_names",
                     "L1011.en_bal_EJ_R_Si_Fi_Yh",
                     "L221.Production_unoil",
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
                     "L221.Production_unoil",
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
