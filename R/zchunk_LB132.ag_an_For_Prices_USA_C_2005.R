#' module_aglu_LB132.ag_an_For_Prices_USA_C_2005
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L132.ag_an_For_Prices}. The corresponding file in the
#' original data system was \code{LB132.ag_an_For_Prices_USA_C_2005.R} (aglu level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YourInitials CurrentMonthName 2017
#' @export
module_aglu_LB132.ag_an_For_Prices_USA_C_2005 <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "aglu/FAO_ag_items_PRODSTAT",
             FILE = "aglu/FAO_an_items_PRODSTAT",
             FILE = "aglu/FAO_USA_ag_an_P_USDt_PRICESTAT",
             FILE = "aglu/FAO_USA_For_Exp_t_USD_FORESTAT",
             FILE = "aglu/USDA_Alfalfa_prices_USDt",
             "L100.FAO_ag_Prod_t",
             FILE = "aglu/FAO_USA_an_Prod_t_PRODSTAT"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return("L132.ag_an_For_Prices")
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO_ag_items_PRODSTAT")
    FAO_an_items_PRODSTAT <- get_data(all_data, "aglu/FAO_an_items_PRODSTAT")
    FAO_USA_ag_an_P_USDt_PRICESTAT <- get_data(all_data, "aglu/FAO_USA_ag_an_P_USDt_PRICESTAT")
    FAO_USA_For_Exp_t_USD_FORESTAT <- get_data(all_data, "aglu/FAO_USA_For_Exp_t_USD_FORESTAT")
    USDA_Alfalfa_prices_USDt <- get_data(all_data, "aglu/USDA_Alfalfa_prices_USDt")
    L100.FAO_ag_Prod_t <- get_data(all_data, "L100.FAO_ag_Prod_t")
    FAO_USA_an_Prod_t_PRODSTAT <- get_data(all_data, "aglu/FAO_USA_an_Prod_t_PRODSTAT")

    # Converting cotton back to primary equivalent (seed cotton)
    # Seed cotton has no price in PRICESTAT. Need to derive its price from cotton lint and cottonseed
    # Assigning a price for game meat so that OtherMeat is assigned a price
    FAO_USA_ag_an_P_USDt_PRICESTAT %>%
      select(-country.codes, -item.codes, -element, -element.codes) %>%
      gather(year, price, -countries, -item) %>%
      filter(item == "Seed cotton" | item == "Cotton lint" | item == "Cottonseed" | item == "Game meat" | item == "Cattle meat") %>%
      mutate(item = sub(" ", "_", item)) %>%
      spread(item, price) %>%
      mutate(Seed_cotton = Cotton_lint * CONV_COTTON_LINT + Cottonseed * (1 - CONV_COTTON_LINT)) %>%
      mutate(Game_meat = Cattle_meat) %>%
      gather(item, price, -countries, -year) %>%
      mutate(item = sub("_", " ", item)) ->
      extra_price
    # Put these calculated prices back to the dataset
    FAO_USA_ag_an_P_USDt_PRICESTAT %>%
      select(-country.codes, -item.codes, -element, -element.codes) %>%
      gather(year, price, -countries, -item) %>%
      filter(!(item == "Seed cotton" | item == "Cotton lint" | item == "Cottonseed" | item == "Game meat" | item == "Cattle meat")) %>%
      rbind(extra_price) %>%
      # Calculate unweighted averages for each FAO commodity over price years
      filter(year %in% MODEL_PRICE_YEARS) %>%
      group_by(countries, item) %>%
      summarise_at(vars(price), mean, na.rm = T) %>%
      ungroup() %>%
      filter(!is.na(price)) ->
      Avg_price

    # Computing average prices and production quantities of all commodities
    # Subset only the relevant country/item combinations from the ag prodstat database
    # NOTE: This mostly excludes fodder crops and "not elsewhere specified" crops. It also excludes about seven crops that are
    # minor enough to not distort price estimates, but their omission may cause problems if a large number of commodities is used

    # Build tables with production and price, and calculate revenue
    # Part 1: Primary agricultural goods and animal products
    # Primary agricultural goods
    L100.FAO_ag_Prod_t %>%
      filter(year %in% MODEL_PRICE_YEARS) %>%
      rename(prod = value) %>%
      group_by(countries, item) %>%
      summarise_at(vars(prod), mean, na.rm = T) %>%
      ungroup() %>%
      filter(!is.na(prod)) %>%
      # Calculating revenue by commodity as production times price
      inner_join(Avg_price, by = c("countries", "item")) %>%
      mutate(V_USD = price * prod) %>%
      left_join_error_no_match(FAO_ag_items_PRODSTAT[c("item", "GCAM_commodity")], by = "item") %>%
      # Remove any fodder crops
      filter(!(GCAM_commodity %in% c("FodderHerb", "FodderGrass"))) %>%
      # 9/21/2016 GPK - removing Sorghum because it is used as a fodder crop in the USA, and its prices are relatively low.
      # The net effect of excluding it here is to raise the price of the OtherGrain commodity, and therefore the profit rate,
      # which otherwise is the lowest of all crops. Any land use regions where this is dominant become bioenergy.
      filter(item != "Sorghum") %>%
      group_by(GCAM_commodity) %>%
      summarize_at(vars(V_USD, prod), sum, na.rm = T) %>%
      ungroup() %>%
      mutate(Price_USDt = V_USD / prod) %>%
      select(GCAM_commodity, Price_USDt) ->
      Price_ag

    # Animal products
    FAO_USA_an_Prod_t_PRODSTAT %>%
      select(-country.codes, -item.codes, -element, -element.codes) %>%
      gather(year, prod, -countries, -item) %>%
      filter(year %in% MODEL_PRICE_YEARS) %>%
      group_by(countries, item) %>%
      summarise_at(vars(prod), mean, na.rm = T) %>%
      ungroup() %>%
      filter(!is.na(prod)) %>%
      # Calculating revenue by commodity as production times price
      inner_join(Avg_price, by = c("countries", "item")) %>%
      mutate(V_USD = price * prod) %>%
      left_join(FAO_an_items_PRODSTAT, by = "item") %>%
      filter(!is.na(GCAM_commodity)) %>%
      group_by(GCAM_commodity) %>%
      summarize_at(vars(V_USD, prod), sum, na.rm = T) %>%
      ungroup() %>%
      mutate(Price_USDt = V_USD / prod) %>%
      # Convert to model units
      mutate(calPrice = round(Price_USDt * CONV_T_METRIC_SHORT * CONV_2004_1975_USD / CONV_T_KG, digits = DIGITS_CALPRICE)) %>%
      select(GCAM_commodity, calPrice) ->
      Price_an

    # Part 2: Fodder crops and pasture
    # Calculate average FodderHerb prices from alfalfa prices
    USDA_Alfalfa_prices_USDt %>%
      select(year, avg) %>%
      filter(year %in% MODEL_PRICE_YEARS) %>%
      summarise_at(vars(avg), mean) %>%
      rename(FodderHerb = avg) %>%
      mutate(FodderGrass = FodderHerb * PRICERATIO_GRASS_ALFALFA) %>%
      # NOTE: Setting Pasture price equal to FodderGrass price
      mutate(Pasture = FodderGrass) %>%
      gather(GCAM_commodity, Price_USDt) %>%
      # Combine crop commodities to get all primary agricultural commodities in a single tibble
      rbind(Price_ag) %>%
      # Convert to model units
      mutate(calPrice = round(Price_USDt * CONV_2004_1975_USD / CONV_T_KG, digits = DIGITS_CALPRICE)) %>%
      select(-Price_USDt) %>%
      # Combine animal products
      rbind(Price_an) %>%
      mutate(unit = "1975$/kg") ->
      Price_ag_an

    # Part 3: Forest products
    FAO_USA_For_Exp_t_USD_FORESTAT %>%
      select(-countries, -country.codes, -item, -item.codes, -element.codes) %>%
      gather(year, value, -element) %>%
      filter(year %in% MODEL_PRICE_YEARS) %>%
      group_by(element) %>%
      summarise_at(vars(value), mean) %>%
      ungroup() %>%
      mutate(element = if_else(element == "Export Quantity (m3)", "Exp_m3", "ExpV_USD")) %>%
      mutate(GCAM_commodity = "Forest") %>%
      spread(element, value) %>%
      mutate(Price_USDm3 = ExpV_USD * 1000 / Exp_m3) %>%
      mutate(calPrice = round(Price_USDm3 * CONV_2004_1975_USD, digits = DIGITS_CALPRICE)) %>%
      select(GCAM_commodity, calPrice) %>%
      mutate(unit = "1975$/m3") %>%
      # Part 4: merging everything into a single table
      rbind(Price_ag_an) %>%

    # if(OLD_DATA_SYSTEM_BEHAVIOR) {
    #   ... code that replicates old, incorrect behavior
    # } else {
    #   ... new code with a fix
    # }

    # Produce outputs
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L132.ag_an_For_Prices") %>%
      add_precursors("aglu/FAO_ag_items_PRODSTAT",
                     "aglu/FAO_an_items_PRODSTAT",
                     "aglu/FAO_USA_ag_an_P_USDt_PRICESTAT",
                     "aglu/FAO_USA_For_Exp_t_USD_FORESTAT",
                     "aglu/USDA_Alfalfa_prices_USDt",
                     "L100.FAO_ag_Prod_t",
                     "aglu/FAO_USA_an_Prod_t_PRODSTAT") %>%
      add_flags(FLAG_NO_XYEAR) ->
      L132.ag_an_For_Prices
    return_data(L132.ag_an_For_Prices)
  } else {
    stop("Unknown command")
  }
}
