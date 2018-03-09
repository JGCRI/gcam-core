#' module_aglu_LB132.ag_an_For_Prices_USA_C_2005
#'
#' Calculate the calibration prices for all GCAM AGLU commodities.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L132.ag_an_For_Prices}. The corresponding file in the
#' original data system was \code{LB132.ag_an_For_Prices_USA_C_2005.R} (aglu level1).
#' @details This chunk calculates average prices over calibration years by GCAM commodity.
#' Averages across years are unweighted; averages over FAO item are weighted by production.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RC April 2017
module_aglu_LB132.ag_an_For_Prices_USA_C_2005 <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
             FILE = "aglu/FAO/FAO_an_items_PRODSTAT",
             FILE = "aglu/FAO/FAO_USA_ag_an_P_USDt_PRICESTAT",
             FILE = "aglu/FAO/FAO_USA_For_Exp_t_USD_FORESTAT",
             FILE = "aglu/USDA_Alfalfa_prices_USDt",
             # Use level0 production data instead of level1 with the 5-yr rolling average
             FILE = "aglu/FAO/FAO_ag_Prod_t_PRODSTAT",
             FILE = "aglu/FAO/FAO_USA_an_Prod_t_PRODSTAT"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return("L132.ag_an_For_Prices")
  } else if(command == driver.MAKE) {

    country.codes <- item.codes <- element <- element.codes <- year <- price <-
      countries <- item <- Cotton_lint <- Cottonseed <- Cattle_meat <-
      value <- GCAM_commodity <- V_USD <- Price_USDt <- calPrice <- avg <-
      FodderHerb <- FodderGrass <- ExpV_USD <- Exp_m3 <- Price_USDm3 <-
      `country codes` <- `item codes` <- `element codes` <- deflator <- . <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_items_PRODSTAT")
    FAO_an_items_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_an_items_PRODSTAT")
    FAO_USA_ag_an_P_USDt_PRICESTAT <- get_data(all_data, "aglu/FAO/FAO_USA_ag_an_P_USDt_PRICESTAT")
    FAO_USA_For_Exp_t_USD_FORESTAT <- get_data(all_data, "aglu/FAO/FAO_USA_For_Exp_t_USD_FORESTAT")
    USDA_Alfalfa_prices_USDt <- get_data(all_data, "aglu/USDA_Alfalfa_prices_USDt")
    FAO_ag_Prod_t_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_Prod_t_PRODSTAT")
    FAO_USA_an_Prod_t_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_USA_an_Prod_t_PRODSTAT")

    # Since FAO_USA_ag_an_P_USDt_PRICESTAT is in nominal years we will need a table of deflators
    # to nomalize each to constant 1975$
    tibble(year = aglu.MODEL_PRICE_YEARS) %>%
      group_by(year) %>%
      summarize(deflator = gdp_deflator(1975, year)) ->
      conv_Price_DollarYear
    # hard coded changes to gdp deflator for some reason:
    conv_Price_DollarYear[conv_Price_DollarYear$year == 2010, "deflator"] <- round( gdp_deflator(1975, 1990) / gdp_deflator(2010, 1990), digits = 4 )
    conv_Price_DollarYear[conv_Price_DollarYear$year == 2011, "deflator"] <- 0.3036 # from BEA (2015), value in other years are slightly higher than those in the data system

    # Converting cotton back to primary equivalent (seed cotton)
    # Seed cotton has no price in PRICESTAT. Need to derive its price from cotton lint and cottonseed
    FAO_USA_ag_an_P_USDt_PRICESTAT %>%
      select(-country.codes, -item.codes, -element, -element.codes) %>%
      gather_years(value_col = "price") %>%
      filter(item == "Seed cotton" | item == "Cotton lint" | item == "Cottonseed" | item == "Game meat" | item == "Cattle meat") %>%
      # Modify item names to one word so that they can be used as column names when spreading item and doing calculations
      mutate(item = sub(" ", "_", item)) %>%
      spread(item, price) %>%
      mutate(Seed_cotton = Cotton_lint * aglu.WEIGHT_COTTON_LINT + Cottonseed * (1 - aglu.WEIGHT_COTTON_LINT)) %>%
      # Assigning a price for game meat so that OtherMeat is assigned a price
      mutate(Game_meat = Cattle_meat) %>%
      gather(item, price, -countries, -year) %>%
      # Change item names back to original for mapping GCAM commodities
      mutate(item = sub("_", " ", item)) ->
      extra_price
    # Put these calculated prices back to the dataset
    FAO_USA_ag_an_P_USDt_PRICESTAT %>%
      select(-country.codes, -item.codes, -element, -element.codes) %>%
      gather_years(value_col = "price") %>%
      filter(!(item == "Seed cotton" | item == "Cotton lint" | item == "Cottonseed" | item == "Game meat" | item == "Cattle meat")) %>%
      bind_rows(extra_price) %>%
      # Calculate a single unweighted average price over price years for each FAO agricultural item
      filter(year %in% aglu.MODEL_PRICE_YEARS) ->
      Adj_price

    # Computing average prices and production quantities of all commodities
    # Subset only the relevant country/item combinations from the ag prodstat database
    # NOTE: This mostly excludes fodder crops and "not elsewhere specified" crops. It also excludes several crops that are
    # minor enough to not distort price estimates, including: Brans, Rubber, Sweeteners, Other, Tobacco, and etc.

    # Build tables with production and price, and calculate production-weighted average price of each GCAM commodity.
    # Part 1: Primary agricultural goods and animal products
    # Primary agricultural goods
    FAO_ag_Prod_t_PRODSTAT %>%
      select(-`country codes`, -`item codes`, -element, -`element codes`) %>%
      gather_years(value_col = "prod") %>%
      filter(year %in% aglu.MODEL_PRICE_YEARS) %>%
      # Match production and price for each FAO item to calculate revenue, avoid any missing value by inner_join
      inner_join(Adj_price, by = c("countries", "item", "year")) %>%
      # Calculate revenue by commodity as production times price
      mutate(V_USD = price * prod) %>%
      filter(!is.na(V_USD)) %>%
      left_join_error_no_match(select(FAO_ag_items_PRODSTAT, item, GCAM_commodity), by = "item") %>%
      # Remove any fodder crops, calculated separately below in Part 3
      filter(!(GCAM_commodity %in% c("FodderHerb", "FodderGrass"))) %>%
      # 9/21/2016 GPK - removing Sorghum because it is used as a fodder crop in the USA, and its prices are relatively low.
      # The net effect of excluding it here is to raise the price of the OtherGrain commodity, and therefore the profit rate,
      # which otherwise is the lowest of all crops. Any land use regions where this is dominant become bioenergy.
      filter(item != "Sorghum") %>%
      # Aggregate revenue and production by GCAM commodity
      group_by(GCAM_commodity, year) %>%
      summarize_at(vars(V_USD, prod), sum, na.rm = TRUE) %>%
      ungroup() %>%
      # Calculate production weighted average price for each GCAM commodity
      mutate(Price_USDt = V_USD / prod) %>%
      select(GCAM_commodity, year, Price_USDt) %>%
      # Convert nominal dollar year to constant 1975$
      left_join_error_no_match(conv_Price_DollarYear, by = "year") %>%
      mutate(Price_USDt  = Price_USDt * deflator) %>%
      select(-deflator) %>%
      # Aggregate by GCAM crop names and compute average prices
      group_by(GCAM_commodity) %>%
      summarize(Price_USDt = mean(Price_USDt)) %>%
      ungroup() ->
      Price_ag

    # Animal products
    FAO_USA_an_Prod_t_PRODSTAT %>%
      select(-country.codes, -item.codes, -element, -element.codes) %>%
      gather_years(value_col = "prod") %>%
      # Calculate a single unweighted average production value over price years for each FAO animal product
      filter(year %in% aglu.MODEL_PRICE_YEARS) %>%
      # Match production and price for each FAO item to calculate revenue, avoid any missing value by inner_join
      inner_join(Adj_price, by = c("countries", "item", "year")) %>%
      # Calculate revenue by commodity as production times price
      mutate(V_USD = price * prod) %>%
      filter(!is.na(V_USD)) %>%
      left_join(FAO_an_items_PRODSTAT, by = "item") %>%
      filter(!is.na(GCAM_commodity)) %>%
      # Aggregate revenue and production by GCAM commodity
      group_by(GCAM_commodity, year) %>%
      summarize_at(vars(V_USD, prod), sum, na.rm = TRUE) %>%
      ungroup() %>%
      # Calculate production weighted average price for each GCAM commodity
      mutate(Price_USDt = V_USD / prod) %>%
      # Convert nominal dollar year to constant 1975$
      left_join_error_no_match(conv_Price_DollarYear, by = "year") %>%
      mutate(Price_USDt  = Price_USDt * deflator) %>%
      select(-deflator) %>%
      # Aggregate by GCAM commodity and compute average prices
      group_by(GCAM_commodity) %>%
      summarize(Price_USDt = mean(Price_USDt)) %>%
      ungroup() %>%
      # Convert to model units
      mutate(calPrice = round(Price_USDt / CONV_T_KG, digits = aglu.DIGITS_CALPRICE)) %>%
      select(-Price_USDt) ->
      Price_an


    # Part 2: Fodder crops and pasture, and will be combined with outputs from Part 1
    # Calculate average FodderHerb prices from alfalfa prices
    USDA_Alfalfa_prices_USDt %>%
      select(year, avg) %>%
      filter(year %in% aglu.MODEL_PRICE_YEARS) %>%
      # Convert nominal dollar year to constant 1975$
      left_join_error_no_match(conv_Price_DollarYear, by = "year") %>%
      mutate(avg  = avg * deflator) %>%
      select(-deflator) %>%
      # Calculate a single unweighted average price of Alfalfa over the price years
      summarise_at(vars(avg), mean, na.rm = TRUE) %>%
      rename(FodderHerb = avg) %>%
      # Note: Setting FodderGrass price as a ratio to FodderHerb
      mutate(FodderGrass = FodderHerb * aglu.PRICERATIO_GRASS_ALFALFA) %>%
      # NOTE: Setting Pasture price equal to FodderGrass price
      mutate(Pasture = FodderGrass) %>%
      gather(GCAM_commodity, Price_USDt) %>%
      # Combine crop commodities to get all primary agricultural commodities in a single tibble
      bind_rows(Price_ag) %>%
      # Convert to model units
      mutate(calPrice = round(Price_USDt / CONV_T_KG, digits = aglu.DIGITS_CALPRICE)) %>%
      select(-Price_USDt) %>%
      # Combine animal products
      bind_rows(Price_an) %>%
      mutate(unit = "1975$/kg") ->
      Price_ag_an

    # Part 3: Forest products, and will be combined with outputs from Part 1 and Part 2
    # Because roundwood producer prices are not reported in FAOSTAT, we use FAO forest export value and export quantities to estimate price
    FAO_USA_For_Exp_t_USD_FORESTAT %>%
      select(-countries, -country.codes, -item, -item.codes, -element.codes) %>%
      gather_years %>%
      filter(year %in% aglu.MODEL_PRICE_YEARS) %>%
      # Modify element names to one word so that they can be used as column names when spreading element and doing calculations
      mutate(element = if_else(element == "Export Quantity (m3)", "Exp_m3", "ExpV_USD")) %>%
      mutate(GCAM_commodity = "Forest") %>%
      spread(element, value) %>%
      # Calculate forest price as export value (in thous USD) divided by export quantity
      mutate(Price_USDm3 = ExpV_USD * 1000 / Exp_m3) %>%
      # Convert nominal dollar year to constant 1975$
      left_join_error_no_match(conv_Price_DollarYear, by = "year") %>%
      mutate(Price_USDm3  = Price_USDm3 * deflator) %>%
      select(-deflator) %>%
      # Calculate a single unweighted average export value and a single unweighted average export quantity over price years
      group_by(GCAM_commodity) %>%
      summarize(Price_USDm3=mean(Price_USDm3)) %>%
      ungroup() %>%
      # Convert to model units
      mutate(calPrice = round(Price_USDm3, digits = aglu.DIGITS_CALPRICE)) %>%
      select(GCAM_commodity, calPrice) %>%
      mutate(unit = "1975$/m3") %>%
      # Part 4: merging everything into a single table
      bind_rows(Price_ag_an) ->
      Price_ag_an_For

    # Check that all commodities being modeled have prices assigned
    FAO_ag_items_PRODSTAT %>%
      select(GCAM_commodity) %>%
      distinct() %>%
      bind_rows(distinct(select(FAO_an_items_PRODSTAT, GCAM_commodity))) %>%
      na.omit() %>%
      pull(GCAM_commodity) -> commodities
    Price_ag_an_For %>%
      pull(GCAM_commodity) -> prices

    # Check missing commodities
    if(any(!(commodities %in% prices))) {
      missing_commodities <- commodities[!(commodities %in% prices)]
      stop("Missing commodity prices for ", missing_commodities)
    }

    # Produce outputs
    Price_ag_an_For %>%
      add_title("Prices for all GCAM AGLU commodities") %>%
      add_units("1975$/kg and 1975$/m3") %>%
      add_comments("Calculate average prices over calibration years by GCAM commodity.") %>%
      add_comments("Averages across years are unweighted; averages over FAO item are weighted by production.") %>%
      add_legacy_name("L132.ag_an_For_Prices") %>%
      add_precursors("aglu/FAO/FAO_ag_items_PRODSTAT",
                     "aglu/FAO/FAO_an_items_PRODSTAT",
                     "aglu/FAO/FAO_USA_ag_an_P_USDt_PRICESTAT",
                     "aglu/FAO/FAO_USA_For_Exp_t_USD_FORESTAT",
                     "aglu/USDA_Alfalfa_prices_USDt",
                     "aglu/FAO/FAO_ag_Prod_t_PRODSTAT",
                     "aglu/FAO/FAO_USA_an_Prod_t_PRODSTAT") ->
      L132.ag_an_For_Prices
    return_data(L132.ag_an_For_Prices)
  } else {
    stop("Unknown command")
  }
}
