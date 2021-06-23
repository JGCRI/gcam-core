# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_data_FAO_BilateralTrade
#'
#' Read the FAO_BilateralTrade trade from file and reduce / summarize as soon as possible to
#' keep its memory footprint small.  NOTE: we do not market the FAO_BilateralTrade as
#' INPUT in order to have more control of when it gets loaded and minimize the amount of
#' time it is loaded in memory.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{FAO_BilateralTrade}.
#' @importFrom dplyr filter if_else mutate select
#' @author PLP
module_data_FAO_BilateralTrade <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    # Note: we do not declare any inputs and will handle loading the data directly
    # to have more control.
    return(NULL)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("FAO_BilateralTrade"))
  } else if(command == driver.MAKE) {

    Element <- Reporter.Countries <- Partner.Countries <-
      Item.Code <- Item <- item.code <- NULL # silence package check.

    # The bilateral trade dataset is massive (286m data points). The order of the steps below is intended to minimize
    # processing time (i.e., the dataset is filtered before performing operations)

    # Select columns, filter to the import and export quantity variables, and gather_years dropping NAs
    # as the "matrix" is very sparse
    fn <- "aglu/FAO/FAO_BilateralTrade"
    fqfn <- find_csv_file(fn, FALSE, quiet = TRUE)
    load_csv_files(fn, FALSE, quiet = TRUE)[[1]] %>%
      select(Reporter.Countries, Partner.Countries, Item.Code, Item, Element, as.character(aglu.TRADE_CAL_YEARS)) %>%
      filter(Element %in% c( "Import Quantity", "Export Quantity")) %>%
      gather_years(na.rm = TRUE) %>%
      parse_csv_header(fqfn, find_header(fqfn)) %>%
      add_flags(FLAG_NO_TEST) ->
      FAO_BilateralTrade

    return_data(FAO_BilateralTrade)
  } else {
    stop("Unknown command")
  }
}

#' module_aglu_LB1091.ag_GrossTrade
#'
#' Calculate primary agricultural good and animal product mass balances, by region / commodity / year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1091.GrossTrade_Mt_R_C_Y} (aglu level1).
#' @details This chunk processes the bi-lateral trade flow data matrix from FAOSTAT, in order to differentiate trade of
#'   agricultural commodities between GCAM regions.
#' @importFrom dplyr anti_join bind_rows distinct filter group_by inner_join left_join mutate rename select ungroup
#' @importFrom tidyr complete drop_na replace_na spread
#' @author GPK/RC/STW February 2019
module_aglu_LB1091.ag_GrossTrade <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/AGLU_ctry",
             FILE = "aglu/FAO/FAO_ag_items_TRADE",
             "FAO_BilateralTrade",
             "L109.ag_ALL_Mt_R_C_Y",
             "L109.an_ALL_Mt_R_C_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1091.GrossTrade_Mt_R_C_Y"))
  } else if(command == driver.MAKE) {

    year <- value <- Element <- Reporter.Countries <- Partner.Countries <-
      Item.Code <- Item <- item.code <- bitrade_commod <- GCAM_commodity <-
      FAO_country <- iso <- iso.partner <- iso.reporter <- GCAM_region_ID <-
      GCAMreg.partner <- var <- export <- import <- NetExp_Mt <- net_trade <-
      GrossExp_Mt <- Prod_Mt <- GrossImp_Mt <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    FAO_ag_items_TRADE <- get_data(all_data, "aglu/FAO/FAO_ag_items_TRADE")
    FAO_BilateralTrade <- get_data(all_data, "FAO_BilateralTrade")
    L109.ag_ALL_Mt_R_C_Y <- get_data(all_data, "L109.ag_ALL_Mt_R_C_Y", strip_attributes = TRUE)
    L109.an_ALL_Mt_R_C_Y <- get_data(all_data, "L109.an_ALL_Mt_R_C_Y")

    # 0: Bind crops and livestock for prod and netexp
    L109.ag_an_ALL_Mt_R_C_Y <- L109.ag_ALL_Mt_R_C_Y %>%
      select(GCAM_region_ID, GCAM_commodity, year, Prod_Mt, NetExp_Mt) %>%
      bind_rows(L109.an_ALL_Mt_R_C_Y %>%
                  select(GCAM_region_ID, GCAM_commodity, year, Prod_Mt, NetExp_Mt))
    # 1: Filter and prepare the bi-lateral trade flow volume data by country and FAO commodity

    # Filter to traded GCAM commodities
    # left_join because many of the trade commodities don't map to GCAM commodities (e.g., silk worm cocoons)
    L1091.BiTrade_t_ctry_item <- left_join(FAO_BilateralTrade, select(FAO_ag_items_TRADE, item.code, bitrade_commod, GCAM_commodity),
                by = c(Item.Code = "item.code")) %>%
      filter(!is.na(GCAM_commodity),
             GCAM_commodity %in% c(aglu.TRADED_CROPS, aglu.TRADED_MEATS)) %>%
      # Join the reporter and partner countries. 10/17/2017 this does produce some missing iso codes for partner
      # countries but they're all tiny. Many  (e.g., "Unspecified Area") do not have 3-digit iso codes anyway. They are
      # dropped by drop_na().
      # Note also - this uses left_join_keep_first_only for countries like the USSR with multiple associated present-day
      # iso codes. We wouldn't want to repeat the trade data by each post-dissolution country, and since none of these
      # actually exist during the time frame for which gross trade is being assessed, there's no benefit to downscaling.
      left_join_keep_first_only(select(AGLU_ctry, FAO_country, iso),
                               by = c( Reporter.Countries = "FAO_country")) %>%
      rename(iso.reporter = iso) %>%
      left_join_keep_first_only(select(AGLU_ctry, FAO_country, iso),
                                by = c( Partner.Countries = "FAO_country")) %>%
      rename(iso.partner = iso) %>%
      drop_na(iso.partner, iso.reporter)

    #2. Re-balancing bilateral trade data
    # The bilateral trade data are not symmetrical - some countries are partner countries but not reporter countries
    # (e.g. Vietnam does not report trading with others, but appears as a partner countries for a number of others). In
    # those missing cases, we use what's already available in the bilateral trade data, and simply flip reporter/partner
    # countries, as well as export/import to get a symmetrical dataset.

    # First, we find those missing cases by comparing the export and import lists
    # The export list - reporter countries, partner countries, commodities, and year
    exp.list <- L1091.BiTrade_t_ctry_item %>%
      filter(Element == "Export Quantity") %>%
      select(Reporter.Countries, Partner.Countries, bitrade_commod, year) %>%
      distinct()
    # The import list - reporter countries, partner countries, commodities, and year
    imp.list <- L1091.BiTrade_t_ctry_item %>%
      filter(Element == "Import Quantity") %>%
      select(Reporter.Countries, Partner.Countries, bitrade_commod, year) %>%
      distinct()

    # The two lists should have the same number of observations -
    # Partner countries in the export list should be reporter countries in the import list, and vice versa

    # Find the partner countries that are missing to report import
    L1091.BiTrade_t_ctry_item_missing_exp <- exp.list %>%
      anti_join(imp.list, by = c("bitrade_commod", "year",
                                 "Reporter.Countries" = "Partner.Countries",
                                 "Partner.Countries" = "Reporter.Countries")) %>%
      mutate(Element = "Export Quantity")
    # Find the partner countries that are missing to report export
    L1091.BiTrade_t_ctry_item_missing_imp <- imp.list %>%
      anti_join(exp.list, by = c("bitrade_commod", "year",
                                 "Reporter.Countries" = "Partner.Countries",
                                 "Partner.Countries" = "Reporter.Countries")) %>%
      mutate(Element = "Import Quantity")

    # Filter those asymmetric observations in the original data, and flip reporter/partner, export/import
    L1091.BiTrade_t_ctry_item_full <- L1091.BiTrade_t_ctry_item_missing_exp %>%
      bind_rows(L1091.BiTrade_t_ctry_item_missing_imp) %>%
      inner_join(L1091.BiTrade_t_ctry_item,
                 by = c("Reporter.Countries", "Partner.Countries", "Element", "bitrade_commod", "year")) %>%
      mutate(Element = if_else(Element == "Import Quantity", "Export Quantity", "Import Quantity")) %>%
      select(Reporter.Countries = Partner.Countries, Partner.Countries = Reporter.Countries,
             iso.reporter = iso.partner, iso.partner = iso.reporter,
             Item.Code, Item, Element, bitrade_commod, GCAM_commodity, year, value) %>%
      bind_rows(L1091.BiTrade_t_ctry_item)

    # 3. Deriving extra-regional trade flows by GCAM region and traded commodity, filtering out within-region trade

    # Method: filter only the crops considered, join in the reporting region and partner region (from reporting and partner
    # country), filter out where reporting and partner region are the same, convert units, aggregate by GCAM region and
    # commodity, and calculate the net trade, and take an unweighted average of the years.
    # Explanation: In multi-country GCAM regions, within-region trade is excluded.
    iso_mapping_partner <- select(iso_GCAM_regID, iso, GCAM_region_ID) %>%
      rename(iso.partner = iso,
             GCAMreg.partner = GCAM_region_ID)
    L1091.XregTrade_Mt_R_C <- L1091.BiTrade_t_ctry_item_full %>%
      filter(GCAM_commodity %in% c(aglu.TRADED_CROPS, aglu.TRADED_MEATS)) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID),
                               by = c("iso.reporter" = "iso")) %>%
      left_join_error_no_match(iso_mapping_partner,
                               by = "iso.partner") %>%
      filter(GCAM_region_ID != GCAMreg.partner) %>%
      mutate(value = value * CONV_T_MT,
             var = tolower(sub(" Quantity", "", Element))) %>%
      group_by(GCAM_region_ID, GCAM_commodity, var, year) %>%
      summarise(value = sum(value)) %>%
      group_by(GCAM_region_ID, GCAM_commodity, var) %>%
      summarise(value = mean(value)) %>%
      ungroup() %>%
      complete(GCAM_region_ID = unique(GCAM_region_ID),
               GCAM_commodity = unique(GCAM_commodity),
               var = unique(var)) %>%
      replace_na(list(value = 0)) %>%
      spread(var, value) %>%
      mutate(net_trade = export - import)

    # 4. Deriving scaled gross trade flows by GCAM region and traded commodity

    # The steps below conserve the total NetExp_Mt that was calculated for GCAM's mass balances of agricultural
    # production, trade, food, feed, biofuels, and other.
    # If ScaledNetTrade > 0 (net exporter), then ScaledExports = Exports + (ScaledNetTrade - NetTrade) and ScaledImports = Imports
    # If ScaledNetTrade <= 0 (net importer), then ScaledImports = Imports + (NetTrade - ScaledNetTrade) and ScaledExports = Exports
    L1091.ag_ALL_Mt_R_C_fhy <- filter(L109.ag_an_ALL_Mt_R_C_Y, year == aglu.TRADE_FINAL_BASE_YEAR)
    L1091.GrossTrade_Mt_R_C_fhy <- L1091.XregTrade_Mt_R_C %>%
      left_join_error_no_match(L1091.ag_ALL_Mt_R_C_fhy,
                               by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      mutate(GrossExp_Mt = if_else(NetExp_Mt > 0, export + NetExp_Mt - net_trade, export),
             GrossImp_Mt = if_else(NetExp_Mt <= 0, import + net_trade - NetExp_Mt, import))

    # If exports exceed production, revise the gross imports and exports. Because we can't have negative calibration
    # quantities in GCAM, we can't represent in gross a nation that serves as a pass-through for a commodity flow.
    # The method below assumes that if a region exports more than it produces, all domestic production is consumed,
    # gross imports are set equal to net imports, and exports are set to 0 (i.e. defaults to net trade approach)
    L1091.GrossTrade_Mt_R_C_fhy <- L1091.GrossTrade_Mt_R_C_fhy %>%
      mutate(GrossExp_Mt = if_else(GrossExp_Mt > Prod_Mt, 0, GrossExp_Mt),
             GrossImp_Mt = if_else(GrossExp_Mt == 0, -1 * NetExp_Mt, GrossImp_Mt),
             # After the adjustment above, Australia_NZ OilCrop GrossImp becomes negative, because NetExp is positive
             # For the same potential cases, re-adjust GrossImp = 0, and GrossExp = NetExp
             GrossExp_Mt = replace(GrossExp_Mt, GrossImp_Mt < 0, NetExp_Mt[GrossImp_Mt < 0]),
             GrossImp_Mt = replace(GrossImp_Mt, GrossImp_Mt < 0, 0)) %>%
      select(GCAM_region_ID, GCAM_commodity, year, GrossExp_Mt, GrossImp_Mt)

    # The gross trade file needs to have data for all calibration years, so just use the net exports to determine this
    L1091.GrossTrade_Mt_R_C_Y <- filter(L109.ag_an_ALL_Mt_R_C_Y, year != aglu.TRADE_FINAL_BASE_YEAR) %>%
      select(GCAM_region_ID, GCAM_commodity, year, NetExp_Mt) %>%
      mutate(GrossExp_Mt = if_else(NetExp_Mt > 0, NetExp_Mt, 0),
             GrossImp_Mt = if_else(NetExp_Mt <= 0, -1 * NetExp_Mt, 0)) %>%
      select(-NetExp_Mt) %>%
      bind_rows(L1091.GrossTrade_Mt_R_C_fhy)

    # Produce outputs
    L1091.GrossTrade_Mt_R_C_Y %>%
      add_title("Gross trade of primary agricultural products, by region / commodity / year.") %>%
      add_units("Mt") %>%
      add_comments("Determined from bi-lateral trade flows; only includes trade between countries in different GCAM regions") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/AGLU_ctry",
                     "aglu/FAO/FAO_ag_items_TRADE",
                     "FAO_BilateralTrade",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y") ->
      L1091.GrossTrade_Mt_R_C_Y

    return_data(L1091.GrossTrade_Mt_R_C_Y)
  } else {
    stop("Unknown command")
  }
}
