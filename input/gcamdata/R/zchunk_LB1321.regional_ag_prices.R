# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LB1321.regional_ag_prices
#'
#' Calculate the calibration prices for all GCAM AGLU commodities.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L1321.prP_R_C_75USDkg}, \code{L1321.expP_R_F_75USDm3}.
#' @details This chunk calculates average prices over calibration years by GCAM commodity and region. Averages across
#'   years are unweighted; averages over FAO item are weighted by production.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else inner_join left_join mutate rename select
#' @importFrom tidyr  complete drop_na gather nesting spread replace_na
#' @importFrom tibble tibble
#' @author GPK/RC/STW February 2019
module_aglu_LB1321.regional_ag_prices <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/AGLU_ctry",
             FILE = "aglu/FAO/FAO_ag_items_TRADE",
             FILE = "aglu/FAO/FAO_ag_an_ProducerPrice",
             FILE = "aglu/FAO/FAO_ag_Prod_t_PRODSTAT",
             FILE = "common/FAO_GDP_Deflators",
             FILE = "aglu/FAO/FAO_an_Prod_t_PRODSTAT",
             FILE = "aglu/FAO/FAO_For_Exp_m3_USD_FORESTAT",
             "L132.ag_an_For_Prices"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1321.ag_prP_R_C_75USDkg",
             "L1321.an_prP_R_C_75USDkg",
             "L1321.expP_R_F_75USDm3"))
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- deflator <-
      countries <- currentUSD_per_baseyearUSD <- item <- pp_commod <- Cottonseed <-
      `Cotton lint` <- item.codes <- production <- prod_commod <- item.code <-
      GCAM_commodity <- GCAM_region_ID <- revenue <- avg_prP_C <- prP <- prPmult <-
      production_wt_prPmult <- prPmult_R <- countries <- `item codes` <- calPrice <-
      `element codes` <- element <- ExpV_kUSD <- Exp_m3 <- avg_expP_F <-
      Price_USDm3 <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    FAO_ag_items_TRADE <- get_data(all_data, "aglu/FAO/FAO_ag_items_TRADE")
    FAO_ag_an_ProducerPrice <- get_data(all_data, "aglu/FAO/FAO_ag_an_ProducerPrice")
    FAO_ag_Prod_t_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_Prod_t_PRODSTAT")
    FAO_GDP_Deflators <- get_data(all_data, "common/FAO_GDP_Deflators")
    FAO_an_Prod_t_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_an_Prod_t_PRODSTAT")
    L132.ag_an_For_Prices <- get_data(all_data,"L132.ag_an_For_Prices")
    # kbn 2019/09/23 added AGLU_Ctry_Unique since using AGLU_Ctry was causing extra rows to be added.
    AGLU_Ctry_Unique<-distinct(AGLU_ctry,FAO_country,.keep_all = TRUE)
    # xz 2021/4/14 added regional forest export prices
    FAO_For_Exp_m3_USD_FORESTAT <- get_data(all_data, "aglu/FAO/FAO_For_Exp_m3_USD_FORESTAT")


    # 1. Producer prices
    # 1.1 GDP deflators (to 2005) by country and analysis year

    # GDP deflators are used to convert each reported year- and country- values to a common unit of measure
    # This step filters years, sets iso codes to countries, and converts all deflators from an index-100 with a base
    # year of 2015 to a multiplier with an exogenous base year. The deflator base year is the year in which relative
    # regional nominal prices are preserved in the constant dollar (i.e., 1975$ in this code) prices. For example, with
    # deflator base year set to 2015, prices are in 2015 Constant USD but expressed in terms of 1975 USD.

    # Sudan (former) is re-set to Sudan for building the full time series
    # South Sudan is dropped as only a few data years are available and it isn't in the price data
    L1321.GDPdefl_ctry <- FAO_GDP_Deflators %>%
      gather_years() %>%
      select(countries, year, value) %>%
      mutate(countries= if_else(countries== "Sudan (former)", "Sudan", countries)) %>%
      filter(countries!= "South Sudan") %>%
      group_by(countries) %>%
      mutate(currentUSD_per_baseyearUSD = (value / value[year == aglu.DEFLATOR_BASE_YEAR])) %>%
      ungroup() %>%
      filter(year %in% aglu.TRADE_CAL_YEARS) %>%
      #kbn 2019/09/23 added AGLU_Ctry_Unique since using AGLU_Ctry was causing extra rows to be added.
      left_join_error_no_match(select(AGLU_Ctry_Unique, FAO_country, iso),
                               by = c(countries= "FAO_country")) %>%
      select(iso, countries, year, currentUSD_per_baseyearUSD)

    # 1.2. Producer prices by country, analysis year, and crop

    # filter analysis years, re-name items for merging with other datasets w/different item lists
    # inner_join producer prices and deflators in order to drop any countries and years w/o available price or deflator data
    # calculate the revised prices as the reported prices divided by deflators
    # Here "Sudan (former)" has data in most years, but "Sudan" is all missing values
    L1321.prP_ctry_item_75USDkg <- FAO_ag_an_ProducerPrice %>%
      filter(!countries %in% c("Sudan")) %>%
      mutate(countries = if_else(countries == "Sudan (former)", "Sudan", countries)) %>%
      gather_years() %>%
      filter(year %in% aglu.TRADE_CAL_YEARS) %>%
      inner_join(L1321.GDPdefl_ctry,
                 by = c("countries", "year")) %>%
      mutate(value = as.numeric(value),
             value = ((value / currentUSD_per_baseyearUSD) * gdp_deflator(1975, aglu.DEFLATOR_BASE_YEAR) / CONV_T_KG)) %>%
      select(iso, pp_commod = item, year, value) %>%
      drop_na(value)

    # Fill out data for missing years
    # Complete only the years (i.e. for each country and commodity, write out all possible years)
    # replace_na() the missing values generated in the prior step with averages from available years
    L1321.prP_ctry_item_75USDkg <- L1321.prP_ctry_item_75USDkg %>%
      complete(nesting(iso, pp_commod), year) %>%
      group_by(iso, pp_commod) %>%
      mutate(value = replace_na(value, mean(value, na.rm=TRUE))) %>%
      ungroup()

    # 1.2.1 Revise cotton prices to cover for gaps in the data

    # The "items" of cotton lint and cottonseed vary in price significantly (~7x), and where countries only report one
    # or the other, the producer price of "seed cotton" (i.e., the total, averaged) is biased accordingly. This is
    # particularly problematic for countries that only have cottonseed prices reported, as very low prices combined
    # with nonLandVariableCosts of the total product return negative profit in calibration years.
    # Because cottonseed is a by-product (from USDA stats, over 90% of the revenue of producing cotton is from the
    # lint), and also because its prices depend on the presence of large industrial consumers, the cotton
    # seed prices are not a good proxy for estimating the cotton lint prices in countries where the former are available
    # and the latter are not. Instead, countries without cotton lint prices are dropped at this stage (to be filled in
    # subsequent steps). Cotton seed prices where missing are assigned the cotton lint price divided by USA lint:seed
    # price ratio.

    P_Ratio_Cttn_Lint_Seed <-
      sum(L1321.prP_ctry_item_75USDkg$value[
        L1321.prP_ctry_item_75USDkg$pp_commod == "Cotton lint" & L1321.prP_ctry_item_75USDkg$iso == "usa"]) /
      sum(L1321.prP_ctry_item_75USDkg$value[
        L1321.prP_ctry_item_75USDkg$pp_commod == "Cottonseed" & L1321.prP_ctry_item_75USDkg$iso == "usa"])
    L1321.prP_ctry_Cttn_75USDkg <- filter(L1321.prP_ctry_item_75USDkg,
                                          pp_commod %in% c("Cotton lint", "Cottonseed")) %>%
      spread(key = pp_commod, value = value) %>%
      mutate(Cottonseed = if_else(is.na(Cottonseed), `Cotton lint` / P_Ratio_Cttn_Lint_Seed, Cottonseed)) %>%
      drop_na() %>%
      gather(key = pp_commod, value = value, -iso, -year)
    L1321.prP_ctry_item_75USDkg <- bind_rows(subset(L1321.prP_ctry_item_75USDkg,
                                                    !pp_commod %in% c("Cotton lint", "Cottonseed")),
                                             L1321.prP_ctry_Cttn_75USDkg)

    # 1.3. Country- and FAO commodity-level production weights

    # Average producer prices, aggregated from countries and FAO items by regions and commodities using production as
    # the weighting factor. The pipeline below is mostly just filtering and cleaning data, and preparing a table to be
    # joined in to the table of producer prices from above.
    L1321.ag_prod_kt_ctry_item <- FAO_ag_Prod_t_PRODSTAT %>%
      gather_years() %>%
      rename(production = value) %>%
      filter(year %in% aglu.TRADE_CAL_YEARS) %>%
      left_join_keep_first_only(select(AGLU_ctry, FAO_country, iso),
                                by = c(countries = "FAO_country")) %>%
      select(iso, item, item.code = item.codes, year, production)

    # 1.3.1 Revising cotton production weights to cover for data gaps

    # The cotton commodity in the PRODSTAT database (Seed cotton) does not match the two in the price database
    # (Cotton lint and Cottonseed). The former is broken into the latter using default ratios.

    L1321.prod_kt_ctry_CttnLnt <- filter(L1321.ag_prod_kt_ctry_item, item == "Seed cotton") %>%
      mutate(item = "Cotton lint",
             production = production * aglu.WEIGHT_COTTON_LINT)
    L1321.prod_kt_ctry_CttnSd <- filter(L1321.ag_prod_kt_ctry_item, item == "Seed cotton") %>%
      mutate(item = "Cottonseed",
             production = production * (1 - aglu.WEIGHT_COTTON_LINT))
    L1321.ag_prod_kt_ctry_item <- bind_rows(filter(L1321.ag_prod_kt_ctry_item, item != "Seed cotton"),
                                         L1321.prod_kt_ctry_CttnLnt,
                                         L1321.prod_kt_ctry_CttnSd)

    L1321.an_prod_kt_ctry_item <- FAO_an_Prod_t_PRODSTAT %>%
      gather_years() %>%
      rename(production = value) %>%
      filter(year %in% aglu.TRADE_CAL_YEARS) %>%
      left_join_keep_first_only(select(AGLU_ctry, FAO_country, iso),
                                by = c(countries = "FAO_country")) %>%
      select(iso, item, item.code = `item codes`, year, production)

    # 1.4. Join production weights into the producer price data
    # Subset only the items that map to GCAM commodities that are modeled as traded,
    # and only the ones that FAO includes in the producer prices database.
    FAO_ag_items_TRADE_pp <- filter(FAO_ag_items_TRADE, !is.na(pp_commod)) %>%
      select(pp_commod, prod_commod, item.code, GCAM_commodity)

    L1321.ag_prP_ctry_item_75USDkg <- L1321.prP_ctry_item_75USDkg %>%
      left_join(FAO_ag_items_TRADE_pp, by = "pp_commod") %>%
      drop_na(GCAM_commodity) %>%
      left_join(L1321.ag_prod_kt_ctry_item,
                by = c("iso", "item.code", "prod_commod" = "item", "year")) %>%
      # some of the country/crop combinations have no production weights. Drop em.
      drop_na(production)

    L1321.an_prP_ctry_item_75USDkg <- L1321.prP_ctry_item_75USDkg %>%
      left_join(FAO_ag_items_TRADE_pp, by = "pp_commod") %>%
      drop_na(GCAM_commodity) %>%
      left_join(L1321.an_prod_kt_ctry_item,
                by = c("iso", "item.code", "prod_commod" = "item", "year")) %>%
      # some of the country/crop combinations have no production weights. Drop em.
      drop_na(production)

    # 1.5. Calculate producer prices by GCAM region and GCAM commodity

    # Weighted average producer prices are calculated as total revenue (price times quantity, aggregated) divided by
    # production weight volumes. Note that at this stage, years are included in the average prices. Averages between
    # years are calculated later on an un-weighted basis in order to remove any bias from inter-annual fluctuation in
    # yield (i.e., more productive years will have higher production weights and lower prices)
    L1321.prP_R_C_Y_75USDkg <- L1321.ag_prP_ctry_item_75USDkg %>%
      bind_rows(L1321.an_prP_ctry_item_75USDkg) %>%
      mutate(revenue = value * production) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID),
                               by = "iso") %>%
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%
      summarise(revenue = sum(revenue),
                production = sum(production)) %>%
      ungroup() %>%
      mutate(value = revenue / production)

    # Default prices by GCAM commodity and regional price multipliers, used for filling missing values

    # Missing GCAM region and commodity observations are replaced with the average producer price of the given crop
    # times the regional average producer price multiplier.

    # 1.5.1 Calculate default global average producer prices by crop (weighted by production)
    L1321.prP_C_75USDkg <- L1321.prP_R_C_Y_75USDkg %>%
      group_by(GCAM_commodity) %>%
      summarise(revenue = sum(revenue),
                production = sum(production)) %>%
      ungroup() %>%
      mutate(avg_prP_C = revenue / production) %>%
      select(GCAM_commodity, avg_prP_C)

    # 1.5.2 Calculate default regional price multipliers

    # Regional default price multipliers are calculated as the sum of production-weighted price multipliers for each
    # crop (production_wt_prPmult), divided by the sum of production.
    # production_wt_prPmult = ratio between observed regional price and global average price of the given commodity

    L1321.prPmult_R <- L1321.prP_R_C_Y_75USDkg %>%
      rename(prP = value) %>%
      left_join_error_no_match( L1321.prP_C_75USDkg, by = "GCAM_commodity") %>%
      mutate(prPmult = prP / avg_prP_C,
             production_wt_prPmult = prPmult * production) %>%
      group_by(GCAM_region_ID) %>%
      summarise(production_wt_prPmult = sum(production_wt_prPmult),
                production = sum(production)) %>%
      mutate(prPmult_R = production_wt_prPmult / production) %>%
      select(GCAM_region_ID, prPmult_R)

    # 1.5.3. Using defaults, fill out missing values for all necessary regions and commodities and years

    # complete() the table to fill out missing values for all years
    # Need this annual data to calculate regional pasture prices
    # To determine the producer price by region and commodity, average across the years (unweighted) later
    # (note that individual years are kept here, so that this dataset can be used to estimate grass prices below)

    L1321.prP_R_C_Y_75USDkg <- L1321.prP_R_C_Y_75USDkg %>%
      complete(GCAM_region_ID, GCAM_commodity, year) %>%
      left_join_error_no_match(L1321.prP_C_75USDkg,
                               by = "GCAM_commodity") %>%
      left_join_error_no_match(L1321.prPmult_R,
                               by = "GCAM_region_ID") %>%
      mutate(value = if_else(is.na(value), avg_prP_C * prPmult_R, value)) %>%
      select(GCAM_region_ID, GCAM_commodity, year, value)

    # Take the unweighted average among years considered
    L1321.prP_R_C_75USDkg <- L1321.prP_R_C_Y_75USDkg %>%
      group_by(GCAM_region_ID, GCAM_commodity) %>%
      summarise(value = mean(value)) %>%
      ungroup %>%
      arrange(GCAM_region_ID, GCAM_commodity)

    L1321.ag_prP_R_C_75USDkg <- L1321.prP_R_C_75USDkg %>%
      filter(GCAM_commodity %in% aglu.TRADED_CROPS)

    L1321.an_prP_R_C_75USDkg <- L1321.prP_R_C_75USDkg %>%
      filter(GCAM_commodity %in% unique(L1321.an_prP_ctry_item_75USDkg$GCAM_commodity))

    # Addendum - to improve feed prices and meat price calibration, compute and include regionally adjusted foddergrass and pasture prices
    L1321.ag_prP_R_Grass_75USDkg <- L1321.prPmult_R %>%
      repeat_add_columns(tibble(GCAM_commodity = c("Pasture", "FodderGrass"))) %>%
      left_join_error_no_match(L132.ag_an_For_Prices, by = "GCAM_commodity") %>%
      mutate(value = calPrice * prPmult_R) %>%
      select(GCAM_region_ID, GCAM_commodity, value)

    L1321.ag_prP_R_C_75USDkg <- bind_rows(L1321.ag_prP_R_C_75USDkg, L1321.ag_prP_R_Grass_75USDkg)

    # Forest export price by country, analysis year, and crop
    L1321.expP_ctry_item_75kUSDm3 <- FAO_For_Exp_m3_USD_FORESTAT %>%
      select(-`element codes`) %>%
      filter(countries != "Sudan") %>%
      mutate(countries = if_else(countries == "Sudan (former)", "Sudan", countries)) %>%
      gather_years() %>%
      filter(year %in% aglu.TRADE_CAL_YEARS) %>%
      mutate(element = if_else(element == "Export Quantity (m3)", "Exp_m3", "ExpV_kUSD"),
             GCAM_commodity = "Forest") %>%
      spread(element, value) %>%
      inner_join(L1321.GDPdefl_ctry,
                 by = c("countries", "year")) %>%
      mutate(ExpV_kUSD = as.numeric(ExpV_kUSD),
             ExpV_kUSD = ((ExpV_kUSD / currentUSD_per_baseyearUSD) * gdp_deflator(1975, aglu.DEFLATOR_BASE_YEAR))) %>%
      select(iso, GCAM_commodity, year, ExpV_kUSD, Exp_m3) %>%
      drop_na(ExpV_kUSD, Exp_m3)

    # Fill out data for missing years
    # Complete only the years (i.e. for each country and commodity, write out all possible years)
    # replace_na() the missing values generated in the prior step with averages from available years
    L1321.expP_R_F_Y_75USDm3 <- L1321.expP_ctry_item_75kUSDm3 %>%
      complete(nesting(iso, GCAM_commodity), year) %>%
      group_by(iso, GCAM_commodity) %>%
      mutate(ExpV_kUSD = replace_na(ExpV_kUSD, mean(ExpV_kUSD, na.rm=TRUE)),
             Exp_m3 = replace_na(Exp_m3, mean(Exp_m3, na.rm=TRUE))) %>%
      ungroup()%>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID),
                               by = "iso") %>%
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%
      summarise(ExpV_kUSD = sum(ExpV_kUSD),
                Exp_m3 = sum(Exp_m3)) %>%
      ungroup() %>%
      # Calculate forest price as export value (in thous USD) divided by export quantity
      mutate(Price_USDm3 = ExpV_kUSD * 1000 / Exp_m3)

    # Calculate default global average export prices (weighted by volume)
    L1321.expP_F_75USDm3 <- L1321.expP_R_F_Y_75USDm3 %>%
      group_by(GCAM_commodity) %>%
      summarise(ExpV_kUSD = sum(ExpV_kUSD),
                Exp_m3 = sum(Exp_m3)) %>%
      ungroup() %>%
      mutate(avg_expP_F =  ExpV_kUSD * 1000 / Exp_m3) %>%
      select(GCAM_commodity, avg_expP_F)

    # Using global values fill out missing values for all necessary regions and commodities
    L1321.expP_R_F_Y_75USDm3 <- L1321.expP_R_F_Y_75USDm3 %>%
      filter(GCAM_commodity %in% aglu.TRADED_FORESTS) %>%
      complete(GCAM_region_ID, GCAM_commodity, year) %>%
      left_join_error_no_match(L1321.expP_F_75USDm3,
                               by = "GCAM_commodity") %>%
      mutate(Price_USDm3 = if_else(is.na(Price_USDm3), avg_expP_F , Price_USDm3)) %>%
      select(GCAM_region_ID, GCAM_commodity, year, Price_USDm3)

    # Final step - filter only traded crops and take the mean among years considered
    L1321.expP_R_F_75USDm3 <- L1321.expP_R_F_Y_75USDm3 %>%
      filter(GCAM_commodity %in% aglu.TRADED_FORESTS) %>%
      group_by(GCAM_region_ID, GCAM_commodity) %>%
      summarise(value = mean(Price_USDm3)) %>%
      ungroup %>%
      arrange(GCAM_region_ID, GCAM_commodity)

    # Produce outputs
    L1321.ag_prP_R_C_75USDkg %>%
      add_title("Regional agricultural commodity prices for all traded primary GCAM AGLU commodities") %>%
      add_units("1975$/kg") %>%
      add_comments("Region-specific calibration prices by GCAM commodity and region") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/AGLU_ctry",
                     "aglu/FAO/FAO_ag_items_TRADE",
                     "aglu/FAO/FAO_ag_an_ProducerPrice",
                     "aglu/FAO/FAO_ag_Prod_t_PRODSTAT",
                     "common/FAO_GDP_Deflators",
                     "L132.ag_an_For_Prices") ->
      L1321.ag_prP_R_C_75USDkg

    L1321.an_prP_R_C_75USDkg %>%
      add_title("Regional animal commodity prices") %>%
      add_units("1975$/kg") %>%
      add_comments("Region-specific prices by GCAM commodity and region") %>%
      same_precursors_as(L1321.ag_prP_R_C_75USDkg) %>%
      add_precursors("aglu/FAO/FAO_an_Prod_t_PRODSTAT") ->
      L1321.an_prP_R_C_75USDkg

    L1321.expP_R_F_75USDm3 %>%
      add_title("Regional prices for GCAM forest commodities") %>%
      add_units("1975$/M3") %>%
      add_comments("Region-specific calibration prices by GCAM commodity and region") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/AGLU_ctry",
                     "aglu/FAO/FAO_ag_items_TRADE",
                     "aglu/FAO/FAO_For_Exp_m3_USD_FORESTAT",
                     "common/FAO_GDP_Deflators") ->
      L1321.expP_R_F_75USDm3

    return_data(L1321.ag_prP_R_C_75USDkg,
                L1321.an_prP_R_C_75USDkg,
                L1321.expP_R_F_75USDm3)
  } else {
    stop("Unknown command")
  }
}
