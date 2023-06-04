# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L100.regional_ag_an_for_prices
#'
#' Calculate the calibration prices for all GCAM AGLU commodities.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L1321.ag_prP_R_C_75USDkg},
#'   \code{L1321.an_prP_R_C_75USDkg}, \code{L1321.expP_R_F_75USDm3}
#' @details This chunk calculates average prices over calibration years by GCAM commodity and region. Averages across
#'   years, when applicable, are unweighted; averages over FAO item are weighted by production.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else inner_join left_join mutate rename select
#' @importFrom tidyr  complete drop_na gather nesting spread replace_na
#' @importFrom tibble tibble
#' @author GPK/RC/STW February 2019; XZ 2022
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
      # price data
      FILE = "aglu/FAO/GCAMDATA_FAOSTAT_ProducerPrice_170Regs_185PrimaryItems_2010to2020",
      FILE = "aglu/FAO/GCAMDATA_FAOSTAT_ForExportPrice_214Regs_Roundwood_1973to2020")

  MODULE_OUTPUTS <-
    c("L1321.ag_prP_R_C_75USDkg",
      "L1321.an_prP_R_C_75USDkg",
      "L1321.expP_R_F_75USDm3")

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
    # non-price items: (non data for 3 unimportant crops and fish items)
    FAO_ag_an_price_items <-
      FAO_ag_items_PRODSTAT %>%
      select(item, item_code, price_item, GCAM_commodity) %>%
      bind_rows(FAO_an_items_PRODSTAT %>%
                  select(item, item_code, price_item, GCAM_commodity)) %>%
      filter(price_item == T) %>% select(-price_item)

    ## 2.2. Process Ag An producer prices and mapping to price items and regions ----
    L100.FAO_ag_an_ProducerPrice_0 <-
      GCAMDATA_FAOSTAT_ProducerPrice_170Regs_185PrimaryItems_2010to2020 %>%
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

    ## 2.3. Join iso and and GCAM_region_ID; Taiwan has no price info, adjusted later ----
    L100.FAO_ag_an_ProducerPrice_1 <-
      L100.FAO_ag_an_ProducerPrice_0 %>%
      left_join_error_no_match(AGLU_ctry %>% select(area = FAO_country, iso), by = "area") %>%
      left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso")

    ## 2.4. Join region-specific GDP deflator ----
    # Data has no NA in aglu.MODEL_PRICE_YEARS
    # left_join_error_no_match shoud be replaced by inner_join if NA is an issue

    L100.FAO_ag_an_ProducerPrice <-
      L100.FAO_ag_an_ProducerPrice_1 %>%
      left_join_error_no_match(L100.FAO_GDP_Deflators,
                               by = c("area_code", "area", "year"))

    ## clean a bit ----
    rm(GCAMDATA_FAOSTAT_ProducerPrice_170Regs_185PrimaryItems_2010to2020,
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
      # year of 2015 to a multiplier with an exogenous base year. The deflator base year is the year in which relative
      # regional nominal prices are preserved in the constant dollar (i.e., 1975$ in this code) prices. For example, with
      # deflator base year set to 2015, prices are in 2015 Constant USD but expressed in terms of 1975 USD.
      mutate(value = value * gdp_deflator(gcam.REAL_PRICE_BASE_YEAR, aglu.DEFLATOR_BASE_YEAR) / CONV_T_KG) %>%
      ungroup() %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID")

    ## Generate Ag_price_index_relative_to_USA and Indonesia  ----
    # for fill in missing and interpolate regional prices for FodderHerb using P_index_toUSA
    # P_index_toIndonesia is a safeguard for tropical crops the US does no have, e.g., palm
    # assert USA and Indonesia exist

    assertthat::assert_that(intersect(c("USA", "Indonesia"),unique(GCAM_region_names$region)) %>%
                              length == 2, msg = "USA or Indonesia not in the regions")

    Ag_price_index_relative_to_USA_Indonesia <-
      L100.ag_an_ProducerPrice_R_C_Y_nofodder %>%
      group_by(GCAM_region_ID, region) %>%
      summarise(value = weighted.mean(value, w = Prod_Q_t)) %>%
      ungroup() %>%
      mutate(P_index_toUSA = value / value[region == "USA"],
             P_index_toIndonesia = value / value[region == "Indonesia"]) %>%
      select(-value)

    ## 2.6. Fodder and pasture prices from USA alfalfa prices ----
    # Moved from origional module_aglu_LB132.ag_an_For_Prices_USA_C_2005 which was removed
    # need to revisit assumptions

    L100.Fodder_USA_Prices_R_C_Y <-
      USDA_Alfalfa_prices_USDt %>%
      select(year = Year, value = Value) %>%
      filter(year %in% aglu.MODEL_PRICE_YEARS) %>%
      # Convert nominal dollar year to constant 1975$
      mutate(value = gdp_deflator(gcam.REAL_PRICE_BASE_YEAR, year) * value) %>%
      # Calculate a unweighted average price of Alfalfa over the price years
      summarise_at(vars(value), mean, na.rm = TRUE) %>%
      # convert unit
      mutate(value = value / CONV_T_KG) %>%
      rename(FodderHerb = value) %>%
      # Note: Setting FodderGrass price as a ratio to FodderHerb
      mutate(FodderGrass = FodderHerb * aglu.PRICERATIO_GRASS_ALFALFA,
             # NOTE: Setting Pasture price equal to FodderGrass price
             Pasture = FodderGrass * aglu.PRICERATIO_PASTURE_HAY) %>%
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
      full_join(Ag_price_index_relative_to_USA_Indonesia, by = character()) %>%
      mutate(RegP_interpolated = USA * P_index_toUSA,
             RegP_interpolated = if_else(is.na(RegP_interpolated), Indonesia * P_index_toIndonesia,
                                         RegP_interpolated)) %>%
      select(GCAM_region_ID, GCAM_commodity, RegP_interpolated)

    ## Fill in missing where needed ----
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




    ## clean more ----
    rm(L100.FAO_ag_an_ProducerPrice_R_C_Y_0,
       L100.FAO_ag_an_ProducerPrice_R_C_Y_1_interpolated,
       Ag_price_index_relative_to_USA_Indonesia,
       L100.Fodder_USA_Prices_R_C_Y,
       L100.ag_an_ProducerPrice_R_C_Y_nofodder)



    # 3. Forest export prices by country, analysis year, and crop ----
    ## 3.1. Export value and quantity ----
    L100.FAO_for_ExpPrice_0 <-
      GCAMDATA_FAOSTAT_ForExportPrice_214Regs_Roundwood_1973to2020 %>%
      gather_years() %>%
      filter(year %in% aglu.MODEL_PRICE_YEARS) %>%
      select(-element_code, -unit) %>%
      mutate(element = if_else(element == "Export Quantity", "Exp_m3", element),
             element = if_else(element == "Export Value", "ExpV_kUSD", element),
             GCAM_commodity = "Forest") %>%
      spread(element, value) %>%
      # data was preprocessed so removing unneeded NA comfortably
      filter(!is.na(Exp_m3), !is.na(ExpV_kUSD))

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
                 select(area_code, year, currentUSD_per_baseyearUSD),
               by = c("area_code", "year")) %>%
      filter(!is.na(currentUSD_per_baseyearUSD))

    rm(GCAMDATA_FAOSTAT_ForExportPrice_214Regs_Roundwood_1973to2020,
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
                     "aglu/FAO/GCAMDATA_FAOSTAT_ProducerPrice_170Regs_185PrimaryItems_2010to2020",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "aglu/FAO/FAO_an_items_PRODSTAT",
                     "common/FAO_GDP_Deflators",
                     "aglu/USDA/USDA_Alfalfa_prices_USDt") ->
      L1321.ag_prP_R_C_75USDkg

    L1321.an_prP_R_C_75USDkg %>%
      add_title("Regional animal commodity prices") %>%
      add_units("1975$/kg") %>%
      add_comments("Region-specific prices by GCAM commodity and region") %>%
      same_precursors_as(L1321.ag_prP_R_C_75USDkg) %>%
      add_precursors("common/iso_GCAM_regID",
                     "common/GCAM_region_names",
                     "aglu/FAO/GCAMDATA_FAOSTAT_ProducerPrice_170Regs_185PrimaryItems_2010to2020",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "aglu/FAO/FAO_an_items_PRODSTAT",
                     "common/FAO_GDP_Deflators") ->
      L1321.an_prP_R_C_75USDkg

    L1321.expP_R_F_75USDm3 %>%
      add_title("Regional prices for GCAM forest commodities") %>%
      add_units("1975$/m3") %>%
      add_comments("Region-specific calibration prices by GCAM commodity and region") %>%
      add_precursors("common/iso_GCAM_regID",
                     "common/GCAM_region_names",
                     "aglu/AGLU_ctry",
                     "aglu/FAO/GCAMDATA_FAOSTAT_ForExportPrice_214Regs_Roundwood_1973to2020",
                     "common/FAO_GDP_Deflators") ->
      L1321.expP_R_F_75USDm3

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
