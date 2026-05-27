# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L100.regional_ag_an_for_prices
#'
#' @description Calculate region-specific calibration prices for all GCAM AGLU commodities
#' (agricultural, animal, and forest products) in 1975 USD/kg or 1975 USD/m3, averaged over
#' MODEL_PRICE_YEARS with adjustments for known FAO data quality issues.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#'
#' @return Depends on \code{command}:
#'   - "DECLARE_INPUTS": A vector of required input files and data
#'   - "DECLARE_OUTPUTS": A vector of output data object names
#'   - "MAKE": All generated outputs:
#'     - \code{L1321.ag_prP_R_C_75USDkg}: Agricultural commodity prices by region
#'     - \code{L1321.an_prP_R_C_75USDkg}: Animal commodity prices by region
#'     - \code{L1321.expP_R_F_75USDm3}: Forest commodity export prices by region
#'     - \code{L1321.For_Cost}: Forest product conversion costs by region and commodity
#'
#' @details This module processes three main price categories:
#'
#' **1. Agricultural and Animal Producer Prices:**
#' - Extracts FAO PRODSTAT price data and aggregates to GCAM commodities and regions
#' - Averages across MODEL_PRICE_YEARS (unweighted); weighted average across FAO items by production
#' - Deflates prices to nominal DEFLATOR_BASE_YEAR, then converts to real REAL_PRICE_BASE_YEAR
#' - Fills missing prices by region using price ratios relative to USA or Indonesia
#' - Applies regional adjustments (e.g., India Dairy, Brazil Livestock) to correct FAO data quality issues
#' - For storage commodities with high import dependence, prices weighted by domestic availability
#'
#' **2. Forest Export Prices:**
#' - Extracts FAO trade data for forest products (sawnwood, woodpulp, etc.)
#' - Calculates weighted average export prices by region and commodity
#' - Uses same deflation methodology as agricultural/animal prices
#'
#' **3. Forest Product Conversion Costs:**
#' - Derives cost of producing secondary forest products (sawnwood, woodpulp) from primary inputs
#' - Cost = output price - (input cost × conversion ratio), where input is pulpwood
#' - Uses IO coefficients from L110.IO_Coefs_pulp
#'
#' **Price Adjustments:**
#' Corrections for known FAO data quality issues are configured in FAO_price_adjustments.csv
#' and applied to specific region-commodity combinations to improve macro value-added tracing.
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else inner_join left_join mutate rename select summarise
#' @importFrom tidyr  complete drop_na gather nesting spread replace_na
#' @importFrom tibble tibble
#' @author GPK/RC/STW February 2019; XZ 2022/2024; code improvements and CSV configuration 2026
module_aglu_L100.regional_ag_an_for_prices <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      FILE = "common/GCAM_region_names",
      FILE = "aglu/AGLU_ctry",
      # region-specific deflators
      FILE = "common/FAO_GDP_Deflators",
      # US alfalfa price for interpolating fodder prices
      FILE = "aglu/USDA/USDA_Alfalfa_prices_USDt",
      # price items mapping
      FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
      FILE = "aglu/FAO/FAO_an_items_PRODSTAT",
      # price data (nominal)
      FILE = "aglu/FAO/GCAMFAOSTAT_ProdPrice",
      FILE = "aglu/FAO/GCAMFAOSTAT_ForExpPrice",
      "L110.IO_Coefs_pulp",
      FILE="aglu/A_forest_mapping",
      # Price adjustments for known FAO data quality issues
      FILE = "aglu/FAO/FAO_price_adjustments",
      # Supply utilization for crops
      "L109.ag_ALL_Mt_R_C_Y")

  MODULE_OUTPUTS <-
    c("L1321.ag_prP_R_C_75USDkg",
      "L1321.an_prP_R_C_75USDkg",
      "L1321.expP_R_F_75USDm3",
      "L1321.For_Cost")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- deflator <-
      countries <- currentUSD_per_baseyearUSD <- item <- pp_commod <- Cottonseed <-
      `Cotton lint` <- item.codes <- production <- prod_commod <- item.code <-
      GCAM_commodity <- GCAM_region_ID <- revenue <- avg_prP_C <- prP <- prPmult <-
      production_wt_prPmult <- prPmult_R <- countries <- `item codes` <- calPrice <-
      `element codes` <- element <- ExpV_kUSD <- Exp_m3 <- avg_expP_F <-
      Price_USDm3 <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # Note that all AgLU prices are derived here
    # Alfalfa price (Fodder) is unweighted average over aglu.MODEL_PRICE_YEARS
    # Producer prices are used if available. Export price is used for forest
    # Regional prices are deflated to aglu.DEFLATOR_BASE_YEAR which should be nominal price of MODEL_FINAL_BASE_YEAR
    # Nominal prices are then converted to gcam.REAL_PRICE_BASE_YEAR using US deflators
    # All other prices are weighted average over aglu.MODEL_PRICE_YEARS
    # If changing price year, only aglu.MODEL_PRICE_YEARS needs to be changed
    # since only aglu.MODEL_PRICE_YEARS is used for prices and historical prices are not used in GCAM


    # 1. Process region-specific GDP deflator ----
    #    used for deflating regional ag an for prices

    L100.FAO_GDP_Deflators <-
      FAO_GDP_Deflators %>% gather_years() %>%
      group_by(area, area_code) %>%
      mutate(currentUSD_per_baseyearUSD = (value / value[year == aglu.DEFLATOR_BASE_YEAR])) %>%
      ungroup() %>%
      select(area, area_code, year, currentUSD_per_baseyearUSD)

    # 2. Ag and An prices ----

    ## 2.1. Get price item mapping ----
    # Price items (excluded): non-data items for 3 unimportant crops and fish species
    FAO_ag_an_price_items <-
      FAO_ag_items_PRODSTAT %>%
      select(item, item_code, price_item, GCAM_commodity) %>%
      bind_rows(FAO_an_items_PRODSTAT %>%
                  select(item, item_code, price_item, GCAM_commodity)) %>%
      filter(price_item == T) %>% select(-price_item)

    ## 2.2. Process Ag An producer prices and mapping to price items and regions ----
    L100.FAO_ag_an_ProducerPrice_0 <-
      GCAMFAOSTAT_ProdPrice %>%
      gather_years() %>%
      # keep aglu.MODEL_PRICE_YEARS only here, price will be weighted average across the years
      filter(year %in% aglu.MODEL_PRICE_YEARS) %>% filter(!is.na(value)) %>%
      # Spread element and rename to add units in headers for Q and V
      select(-unit) %>% spread(element, value) %>%
      rename(Prod_Value_USD = Prod_Value, Prod_Q_t = Production) %>%
      # Join price items mapping and price data
      left_join(FAO_ag_an_price_items, by = c("item_code", "item")) %>%
      # filter NA includes no production area (e.g., Sudan former after 2011) or items
      # and non GCAM_commodity item (e.g., beewax)
      filter(!is.na(Prod_Q_t), !is.na(GCAM_commodity))

    ## 2.3. Join iso and GCAM_region_ID; Taiwan has no price info, adjusted later ----
    L100.FAO_ag_an_ProducerPrice_1 <-
      L100.FAO_ag_an_ProducerPrice_0 %>%
      left_join_error_no_match(AGLU_ctry %>% select(area = FAO_country, iso), by = "area") %>%
      left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso")

    ## 2.4. Join region-specific GDP deflator ----
    # Data has no NA in aglu.MODEL_PRICE_YEARS
    # left_join_error_no_match should be replaced by inner_join if NA is an issue

    L100.FAO_ag_an_ProducerPrice <-
      L100.FAO_ag_an_ProducerPrice_1 %>%
      left_join_error_no_match(L100.FAO_GDP_Deflators,
                               by = c("area_code", "area", "year"))

    ## clean a bit ----
    rm(GCAMFAOSTAT_ProdPrice,
       L100.FAO_ag_an_ProducerPrice_0, L100.FAO_ag_an_ProducerPrice_1)

    ## 2.5. Aggregation to GCAM regions, items, and price years ----
    L100.ag_an_ProducerPrice_R_C_Y_nofodder <-
      L100.FAO_ag_an_ProducerPrice %>%
      ##* Price is a weighted averge across aglu.MODEL_PRICE_YEARS ----
      #  so not including year in group-by
      group_by(GCAM_region_ID, GCAM_commodity) %>%
      # Weighted average real prices in base year
      summarise(value = sum(Prod_Value_USD /currentUSD_per_baseyearUSD) / sum(Prod_Q_t),
                Prod_Q_t = sum(Prod_Q_t)) %>%
      # Convert unit to $ per kg in a gcam.REAL_PRICE_BASE_YEAR
      # gdp_deflator here are used to convert each reported year- and country- values to a common unit of measure
      # This step filters years, sets iso codes to countries, and converts all deflators from an index-100 with a base
      # year to a multiplier with an exogenous base year. The deflator base year is the year in which relative
      # regional nominal prices are preserved in the constant dollar (i.e., 1975$ in this code) prices. For example, with
      # deflator base year set to base year, prices are in base year Constant USD but expressed in terms of 1975 USD.
      mutate(value = value * gdp_deflator(gcam.REAL_PRICE_BASE_YEAR, aglu.DEFLATOR_BASE_YEAR) / CONV_T_KG) %>%
      ungroup() %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID")

    ## Generate Ag_price_index_relative_to_USA and Indonesia  ----
    # for fill in missing and interpolate regional prices for FodderHerb using P_index_toUSA
    # P_index_toIndonesia is a safeguard for tropical crops the US does not have, e.g., palm
    # assert USA and Indonesia exist

    assertthat::assert_that(intersect(c("USA", "Indonesia"),unique(GCAM_region_names$region)) %>%
                              length == 2, msg = "USA or Indonesia not in the regions")

    Ag_price_index_relative_to_USA_Indonesia <-
      L100.ag_an_ProducerPrice_R_C_Y_nofodder %>%
      # Using Corn here only (to reduce regional variation) for missing values
      filter(GCAM_commodity == "Corn") %>%
      group_by(GCAM_region_ID, region) %>%
      summarise(value = weighted.mean(value, w = Prod_Q_t)) %>%
      ungroup() %>%
      mutate(P_index_toUSA = value / value[region == "USA"],
             P_index_toIndonesia = value / value[region == "Indonesia"]) %>%
      select(-value)

    ## 2.6. Fodder and pasture prices from USA alfalfa prices ----

    L100.Fodder_USA_Prices_R_C_Y <-
      USDA_Alfalfa_prices_USDt %>%
      select(year = Year, value = Value) %>%
      filter(year %in% aglu.MODEL_PRICE_YEARS) %>%
      # Convert nominal dollar year to constant 1975$
      mutate(value = gdp_deflator(gcam.REAL_PRICE_BASE_YEAR, year) * value) %>%
      # Calculate an unweighted average price of Alfalfa over the price years
      summarise_at(vars(value), mean, na.rm = TRUE) %>%
      # convert unit
      mutate(value = value / CONV_T_KG) %>%
      # Note: Setting FodderGrass price as a ratio to FodderHerb
      mutate(FodderHerb = value * aglu.PRICERATIO_FODDERHERB_ALFALFA,
             FodderGrass = FodderHerb * aglu.PRICERATIO_FODDERGRASS_FODDERHERB,
             # NOTE: Setting Pasture price equal to FodderGrass price
             Pasture = FodderGrass * aglu.PRICERATIO_PASTURE_FODDERGRASS) %>%
      select(-value) %>%
      gather(GCAM_commodity, value) %>%
      mutate(GCAM_region_ID = 1, region = "USA")

    ## 2.7. Bind US fodder price into Ag An price, complete and fill ----
    L100.FAO_ag_an_ProducerPrice_R_C_Y_0 <-
      bind_rows(L100.ag_an_ProducerPrice_R_C_Y_nofodder %>% select(-Prod_Q_t),
                L100.Fodder_USA_Prices_R_C_Y ) %>%
      complete(nesting(GCAM_region_ID, region), GCAM_commodity)

    ## Interpolate regional price based on US or Indonesia prices ----
    L100.FAO_ag_an_ProducerPrice_R_C_Y_1_interpolated <-
      L100.FAO_ag_an_ProducerPrice_R_C_Y_0 %>%
      filter(region %in% c("USA", "Indonesia")) %>% select(GCAM_commodity, region, value) %>%
      spread(region, value) %>%
      cross_join(Ag_price_index_relative_to_USA_Indonesia) %>%
      mutate(RegP_interpolated = USA * P_index_toUSA,
             RegP_interpolated = if_else(is.na(RegP_interpolated), Indonesia * P_index_toIndonesia,
                                         RegP_interpolated)) %>%
      select(GCAM_region_ID, GCAM_commodity, RegP_interpolated)

    ## 2.8. Fill in missing where needed ----
    L100.FAO_ag_an_ProducerPrice_R_C_Y <-
      L100.FAO_ag_an_ProducerPrice_R_C_Y_0 %>%
      left_join(L100.FAO_ag_an_ProducerPrice_R_C_Y_1_interpolated,
                by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      group_by(GCAM_commodity) %>%
      # For aglu.IWM_TRADED_COMM ("FodderHerb", "OtherMeat_Fish"), use US price for all regions
      mutate(value = if_else(GCAM_commodity %in% aglu.IWM_TRADED_COMM, value[region == "USA"], value)) %>%
      ungroup() %>%
      # For all others, no trade or regional trade, use RegP_interpolated if missing
      mutate(value = if_else(!GCAM_commodity %in% aglu.IWM_TRADED_COMM & is.na(value),
                             RegP_interpolated, value)) %>%
      transmute(GCAM_region_ID, region, GCAM_commodity,
                value = round(value, digits = aglu.DIGITS_CALPRICE),
                unit = "1975$/kg")

    ## Use China price for Taiwan when whole region of Taiwan is missing ----
    # If China and Taiwan are in the regions
    if (intersect(c("China", "Taiwan"),unique(GCAM_region_names$region)) %>% length == 2 &
        intersect(c("Taiwan"),unique(L100.FAO_ag_an_ProducerPrice_R_C_Y$region)) %>% length == 0 &
        intersect(c("China"),unique(L100.FAO_ag_an_ProducerPrice_R_C_Y$region)) %>% length == 1 ) {

      L100.FAO_ag_an_ProducerPrice_R_C_Y <-
        L100.FAO_ag_an_ProducerPrice_R_C_Y %>%
        bind_rows(
          L100.FAO_ag_an_ProducerPrice_R_C_Y %>%
            filter(region == "China") %>%
            mutate(region = "Taiwan",
                   GCAM_region_ID = c(GCAM_region_names %>%
                                        filter(region == "Taiwan") %>%
                                        pull(GCAM_region_ID)))
        )
    }

    ## 2.9. Update producer prices when domestic production is zero ----
    # This is important for pricing opening stocks
    # We want to use imported price (world price) as opposed  to the value interpolated above using relative regional index

    # Calculate import prices for crops
    # [Note that this price adjustments have not been done for livestock
    #  if incorporating meat storage, that likely will be needed]

    L1321.ag_tradedP_C_75USDkg <-
      L109.ag_ALL_Mt_R_C_Y %>%
      select(GCAM_region_ID, GCAM_commodity, year, GrossExp_Mt, GrossImp_Mt) %>%
      filter(year == max(MODEL_BASE_YEARS),
             GCAM_commodity%in% aglu.TRADED_CROPS) %>%
      inner_join(L100.FAO_ag_an_ProducerPrice_R_C_Y, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      mutate(Exp_wtd_price = GrossExp_Mt * value) %>%
      group_by(GCAM_commodity) %>%
      summarize(GrossExp_Mt = sum(GrossExp_Mt),
                Exp_wtd_price = sum(Exp_wtd_price)) %>%
      ungroup() %>%
      mutate(tradedP = Exp_wtd_price / GrossExp_Mt) %>%
      select(GCAM_commodity, tradedP)

    L100.FAO_ag_an_ProducerPrice_R_C_Y %>%
      # Join traded prices only for storage commodities; NA for non-storage commodities expected
      left_join(L1321.ag_tradedP_C_75USDkg, by = "GCAM_commodity") %>%
      # Join SUA table to calculate a few metrics
      # NA expected from above
      left_join(
        L109.ag_ALL_Mt_R_C_Y %>%
          filter(year == max(MODEL_BASE_YEARS), GCAM_commodity%in% aglu.TRADED_CROPS) %>%
          transmute(GCAM_commodity, GCAM_region_ID, Prod_Mt, GrossImp_Mt,
                    ArmingtonDomestic = `Opening stocks` + Prod_Mt - GrossExp_Mt,
                    # setting metrics to a large value e.g., 10 when GrossImp_Mt == 0 or ArmingtonDomestic == 0
                    ProdImpRatio = if_else(GrossImp_Mt == 0, 10, Prod_Mt / GrossImp_Mt),
                    ProdDomRatio = if_else(ArmingtonDomestic == 0, 10, Prod_Mt / ArmingtonDomestic)),
        by = c("GCAM_region_ID", "GCAM_commodity")
      ) %>%
      # Adjust prices for storage-dependent regions
      # When a region both imports significantly (ProdImpRatio < 0.5) AND
      # relies heavily on storage (ProdDomRatio < 0.5), we price storage
      # using international (traded) prices rather than domestic production prices.
      # This prevents artificially inflating domestic prices when storage dominates supply.
      mutate(
        # Calculate storage-adjusted price: weighted average of production and storage costs
        Price_StorageAdjusted = (value * Prod_Mt + tradedP * (ArmingtonDomestic - Prod_Mt)) / ArmingtonDomestic,
        # Safeguard: use original price if adjustment is invalid
        Price_StorageAdjusted = if_else(ArmingtonDomestic == 0 |Price_StorageAdjusted < 0, value, Price_StorageAdjusted),
        # Apply adjustment only to high storage-import dependent regions
        value = if_else(ProdDomRatio < 0.5 & ProdImpRatio < 0.5 & !is.na(Prod_Mt), Price_StorageAdjusted, value)
      ) %>%
      select(GCAM_region_ID, region, GCAM_commodity, value, unit) ->
      L100.FAO_ag_an_ProducerPrice_R_C_Y

    # Finally replace zero with world values
    L100.FAO_ag_an_ProducerPrice_R_C_Y %>%
      left_join(L1321.ag_tradedP_C_75USDkg, by = "GCAM_commodity") %>%
      mutate(value = if_else(value == 0, tradedP, value)) %>% select(-tradedP) ->
      L100.FAO_ag_an_ProducerPrice_R_C_Y

    assertthat::assert_that(
      L100.FAO_ag_an_ProducerPrice_R_C_Y %>% filter(is.na(value)|value == 0) %>% nrow == 0
    )

    ## clean more ----
    rm(L100.FAO_ag_an_ProducerPrice_R_C_Y_0,
       L100.FAO_ag_an_ProducerPrice_R_C_Y_1_interpolated,
       Ag_price_index_relative_to_USA_Indonesia,
       L100.Fodder_USA_Prices_R_C_Y,
       L100.ag_an_ProducerPrice_R_C_Y_nofodder)

    ## Apply price adjustments from configuration file ----
    # These adjustments correct for known FAO data quality issues in specific regions/commodities
    # They are documented in FAO_price_adjustments.csv with justification and notes
    # We need to re-evaluate these when FAO data is updated

    L100.FAO_ag_an_ProducerPrice_R_C_Y <-
      L100.FAO_ag_an_ProducerPrice_R_C_Y %>%
      left_join(FAO_price_adjustments %>%
                  select(region, GCAM_commodity, adjustment_multiplier),
                by = c("region", "GCAM_commodity")) %>%
      mutate(value = value * if_else(is.na(adjustment_multiplier), 1, adjustment_multiplier)) %>%
      select(-adjustment_multiplier)

    # Quick assertation for India after the above adjustment
    # to ensure Dairy prices is still reasonably small < 0.2 1975$/kg
    assertthat::assert_that(
      L100.FAO_ag_an_ProducerPrice_R_C_Y %>% filter(region == "India" & GCAM_commodity == "Dairy") %>%
        pull(value) < 0.2
    )


    # 3. Forest export prices by country, analysis year, and crop ----
    ## 3.1. Export value and quantity ----
    L100.FAO_for_ExpPrice_0 <-
      GCAMFAOSTAT_ForExpPrice %>% filter(item %in% A_forest_mapping$item) %>%
      left_join_error_no_match(A_forest_mapping, by = c("item")) %>%
      gather_years() %>%
      filter(year %in% aglu.MODEL_PRICE_YEARS) %>%
      select(-element_code, -unit) %>%
      mutate(element = if_else(element == "Export Quantity", "Exp_m3", element),
             element = if_else(element == "Export Value", "ExpV_kUSD", element)) %>%
      spread(element, value) %>%
      group_by(area,year,GCAM_commodity) %>%
      summarize(Exp_m3= sum(Exp_m3, na.rm = T),
             ExpV_kUSD=sum(ExpV_kUSD, na.rm = T)) %>%
      distinct() %>%
      # data was preprocessed so removing unneeded NA comfortably
      na.omit()

    ## 3.2. Join iso and and GCAM_region_ID ----
    L100.FAO_for_ExpPrice_1 <-
      L100.FAO_for_ExpPrice_0 %>%
      left_join(AGLU_ctry %>% select(area = FAO_country, iso), by = "area") %>%
      filter(!is.na(iso)) %>%
      left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso")

    ## 3.3. Join region-specific GDP deflator ----
    L100.FAO_for_ExpPrice <-
      L100.FAO_for_ExpPrice_1 %>%
      inner_join(L100.FAO_GDP_Deflators %>%
                 select(area, year, currentUSD_per_baseyearUSD),
               by = c("area", "year")) %>%
      filter(!is.na(currentUSD_per_baseyearUSD))

    # Remove intermediate objects and shared inputs that are no longer needed
    # (L100.FAO_GDP_Deflators was used in both ag/an section 2.4 and forest section 3.3)
    rm(GCAMFAOSTAT_ForExpPrice,
       L100.FAO_for_ExpPrice_0, L100.FAO_for_ExpPrice_1,
       L100.FAO_GDP_Deflators)

    ## 3.4. Aggregation to GCAM regions, items, and price years ----
    L100.FAO_for_ExpPrice_R_C_Y <-
      L100.FAO_for_ExpPrice %>%
      ##* Price is a weighted average across aglu.MODEL_PRICE_YEARS ----
      #  so not including year in group-by
      group_by(GCAM_region_ID, GCAM_commodity) %>%
      # Weighted average real prices in base year
      summarise(value = sum(ExpV_kUSD /currentUSD_per_baseyearUSD) / sum(Exp_m3)) %>%
      ungroup() %>%
      # convert to gcam.REAL_PRICE_BASE_YEAR and unit to $/m3
      mutate(value = value * 1000 * gdp_deflator(gcam.REAL_PRICE_BASE_YEAR, aglu.DEFLATOR_BASE_YEAR),
             unit = "1975$/m3") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")

    # Produce outputs ----
    #*********************************

    L1321.expP_R_F_75USDm3 <- L100.FAO_for_ExpPrice_R_C_Y

    # Extract pulpwood price (used as input cost to produce sawnwood and other forest products)
    L100.pulpwood_price_R <-
      L1321.expP_R_F_75USDm3 %>%
      filter(!GCAM_commodity %in% aglu.FOREST_COMMODITIES) %>%
      rename(Price_USDm3 = value) %>%
      select(GCAM_region_ID, Price_USDm3)

    # Extract IO coefficients (conversion ratios for forest products)
    L100.forest_IO_coefs_R <-
      L110.IO_Coefs_pulp %>%
      filter(year %in% c(MODEL_FINAL_BASE_YEAR)) %>%
      group_by(GCAM_region_ID) %>%
      summarise(IO = mean(IO), .groups = "drop")

    # Calculate forest product costs: output price minus (input price * conversion ratio)
    L1321.expP_R_F_75USDm3 %>%
      filter(GCAM_commodity %in% aglu.FOREST_COMMODITIES) %>%
      left_join_error_no_match(L100.pulpwood_price_R, by = "GCAM_region_ID") %>%
      left_join(L100.forest_IO_coefs_R, by = "GCAM_region_ID") %>%
      mutate(
        # Use regional IO coefficient if available, otherwise use default
        IO = if_else(is.na(IO), aglu.FOREST_SAWTIMBER_CONVERSION, IO),
        # Calculate cost: output price - (input cost * conversion factor)
        ForCost = if_else(
          GCAM_commodity == "sawnwood",
          value - (Price_USDm3 * IO),
          value - (Price_USDm3 * aglu.FOREST_PULP_CONVERSION)
        )
      ) %>%
      select(-Price_USDm3, -IO) %>%
      # Remove invalid (zero or negative) costs
      filter(ForCost > 0) %>%
      # Average across any duplicates (e.g., from multiple regions with same commodity)
      group_by(GCAM_region_ID, GCAM_commodity) %>%
      summarise(ForCost = mean(ForCost), .groups = "drop") %>%
      # Handle any infinite values from division by zero
      mutate(ForCost = if_else(is.infinite(ForCost), 0, ForCost)) ->
      L1321.For_Cost


    L1321.an_prP_R_C_75USDkg <-
      L100.FAO_ag_an_ProducerPrice_R_C_Y %>%
      filter(GCAM_commodity %in% c(FAO_an_items_PRODSTAT %>% filter(price_item == T) %>%
                                     distinct(GCAM_commodity) %>% pull))

    L1321.ag_prP_R_C_75USDkg <-
      L100.FAO_ag_an_ProducerPrice_R_C_Y %>%
      filter(!GCAM_commodity %in% c(L1321.an_prP_R_C_75USDkg %>% distinct(GCAM_commodity) %>% pull))


    # Produce outputs
    L1321.ag_prP_R_C_75USDkg %>%
      add_title("Regional agricultural commodity prices for all traded primary GCAM AGLU commodities") %>%
      add_units("1975$/kg") %>%
      add_comments("Region-specific calibration prices by GCAM commodity and region") %>%
      add_precursors("common/iso_GCAM_regID",
                     "common/GCAM_region_names",
                     "aglu/AGLU_ctry",
                     "aglu/FAO/GCAMFAOSTAT_ProdPrice",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "aglu/FAO/FAO_an_items_PRODSTAT",
                     "common/FAO_GDP_Deflators",
                     "aglu/USDA/USDA_Alfalfa_prices_USDt",
                     "L109.ag_ALL_Mt_R_C_Y") ->
      L1321.ag_prP_R_C_75USDkg

    L1321.an_prP_R_C_75USDkg %>%
      add_title("Regional animal commodity prices") %>%
      add_units("1975$/kg") %>%
      add_comments("Region-specific prices by GCAM commodity and region") %>%
      same_precursors_as(L1321.ag_prP_R_C_75USDkg) %>%
      add_precursors("common/iso_GCAM_regID",
                     "common/GCAM_region_names",
                     "aglu/FAO/GCAMFAOSTAT_ProdPrice",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "aglu/FAO/FAO_an_items_PRODSTAT",
                     "common/FAO_GDP_Deflators",
                     "L109.ag_ALL_Mt_R_C_Y") ->
      L1321.an_prP_R_C_75USDkg

    L1321.expP_R_F_75USDm3 %>%
      filter(GCAM_commodity %in% aglu.FOREST_SUPPLY_SECTOR) %>%
      add_title("Regional prices for GCAM forest commodities") %>%
      add_units("1975$/m3") %>%
      add_comments("Region-specific calibration prices by GCAM commodity and region") %>%
      add_precursors("common/iso_GCAM_regID",
                     "common/GCAM_region_names",
                     "aglu/AGLU_ctry",
                     "aglu/FAO/GCAMFAOSTAT_ForExpPrice",
                     "common/FAO_GDP_Deflators") ->
      L1321.expP_R_F_75USDm3

    L1321.For_Cost %>%
      select(GCAM_region_ID,year,GCAM_commodity,ForCost) %>%
      add_title("Regional cost for GCAM forest secondary commoditties") %>%
      add_units("1975$/unit") %>%
      add_comments("Region-specific costs by GCAM commodity and region") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/AGLU_ctry",
                     "aglu/FAO/GCAMFAOSTAT_ForExpPrice",
                     "common/FAO_GDP_Deflators",
                     "aglu/A_forest_mapping",
                     "L110.IO_Coefs_pulp") ->
      L1321.For_Cost

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
