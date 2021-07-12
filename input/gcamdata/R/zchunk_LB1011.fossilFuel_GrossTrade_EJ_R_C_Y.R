# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LB1011.ff_GrossTrade
#'
#' Calculate primary fossil fuel (coal, natural gas, crude oil) product balances, by region / commodity / year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1011.ff_GrossTrade_EJ_R_C_Y} (energy level1)
#' @details This chunk processes the bi-lateral trade flow data matrix from UN Comtrade, in order to identify trade of
#'   fossil fuels between GCAM regions.
#' @importFrom dplyr anti_join bind_rows distinct filter group_by inner_join left_join mutate rename select ungroup
#' @importFrom tidyr complete drop_na replace_na spread
#' @author GPK/JEH April 2019
module_energy_LB1011.ff_GrossTrade <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "common/GCAM_region_names",
             FILE = "emissions/A_PrimaryFuelCCoef",
             FILE = "energy/fuel_carbon_content",
             FILE = "energy/mappings/comtrade_countrycode_ISO",
             FILE = "energy/mappings/comtrade_commodity_code",
             FILE = "energy/mappings/comtrade_trade_flow",
             FILE = "energy/comtrade_ff_trade"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1011.ff_GrossTrade_EJ_R_C_Y"))
  } else if(command == driver.MAKE) {

    year <- value <- Element <- Reporter.Countries <- Partner.Countries <-
      Item.Code <- Item <- item.code <- bitrade_commod <- GCAM_Commodity <-
      FAO_country <- iso <- iso.partner <- iso.reporter <- GCAM_region_ID <-
      GCAMreg.partner <- var <- gross_exports <- gross_imports <- net_exports <-
      NetExp_EJ <- net_trade <- GrossExp_EJ <- GrossImp_EJ <- Year <- Reporter_Code <- Partner_Code <-
      Trade_Flow_Code <- Commodity_Code <- `Netweight_(kg)` <- Country_Code <- ISO3_digit_Alpha <- ISO3 <-
      Trade <- reporter_GCAM_region_ID <- partner_GCAM_region_ID <- PrimaryFuelCO2Coef.name <- PrimaryFuelCO2Coef <-
      Ccontent <- export <- import <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_PrimaryFuelCCoef <- get_data(all_data, "emissions/A_PrimaryFuelCCoef")
    fuel_carbon_content <- get_data(all_data, "energy/fuel_carbon_content")
    comtrade_ISO <- get_data(all_data, "energy/mappings/comtrade_countrycode_ISO")
    #April 29th 2019: Note that as Taiwan is not recognized by the UN there is no TWN ISO specified in the comtrade_ISO mappings
    #According to COMTRADE code 490 is (in practice) only Taiwan, but when/if better data is available for Taiwan we may want to update
    comtrade_commodity_GCAM <- get_data(all_data, "energy/mappings/comtrade_commodity_code")
    comtrade_trade_flow <- get_data(all_data, "energy/mappings/comtrade_trade_flow")
    comtrade_ff_trade <- get_data(all_data, "energy/comtrade_ff_trade")



    # 1: Filter and prepare the bi-lateral trade flow volume data by country and comtrade commodity

    # Select columns, filter to the import and export quantity variables, and filter to traded GCAM commodities
    # Note this data set was particularly selected for fossil fuel trade.
    # Additional data and mappings will need to be gathered at comtrade's website (https://comtrade.un.org/db/dqBasicQuery.aspx) if other commodities are added

    L1011.comtrade_ff_BiTrade_y_ctry_item <- select(comtrade_ff_trade, Year, Reporter_Code, Partner_Code, Trade_Flow_Code, Commodity_Code, `Netweight_(kg)`) %>%
      left_join_error_no_match(comtrade_trade_flow, by = c("Trade_Flow_Code" = "Trade_Flow_Code")) %>%
      # Join the reporter and partner countries.
      # Note also - this uses left_join_keep_first_only for countries like the USSR with multiple associated present-day
      # iso codes. We wouldn't want to repeat the trade data by each post-dissolution country, and since none of these
      # actually exist during the time frame for which gross trade is being assessed, there's no benefit to downscaling.
      left_join_keep_first_only(comtrade_ISO%>%select(Country_Code, ISO3=ISO3_digit_Alpha), by=c("Reporter_Code" = "Country_Code")) %>%
      rename(iso.reporter = ISO3) %>%
      left_join_keep_first_only(comtrade_ISO%>%select(Country_Code, ISO3=ISO3_digit_Alpha), by=c("Partner_Code" = "Country_Code")) %>%
      rename(iso.partner = ISO3, value = `Netweight_(kg)`, year = Year, Element = Trade) %>%
      mutate(iso.reporter = tolower(iso.reporter), iso.partner = tolower(iso.partner)) %>%
      select(year, iso.reporter, iso.partner, Element, Commodity_Code, value) %>%
      drop_na(iso.partner) %>%
      gather_years() %>%
      drop_na(value) %>%
      #We only want to look at regions that we can map to a GCAM region
      #In practice this filters out any NA iso or unmatching iso codes, which are generally small
      # as of Nov 15th 2019 this includes Antarctica, British Idian Ocean Territory,
      # Curacao, Sint Maarten, and French Southern Territory
      filter(iso.reporter %in% iso_GCAM_regID$iso, iso.partner %in% iso_GCAM_regID$iso)


    #2. Re-balancing bilateral trade data
    # The bilateral trade data may not be symmetrical - some countries could be partner countries but not reporter countries
    # (e.g. A country does not report trading with others, but appears as a partner countries for a number of others). In
    # those missing cases, we can use what's already available in the bilateral trade data, and simply flip reporter/partner
    # countries, as well as export/import to get a symmetrical dataset.
    # Note any pair of non-reporting partner/reporter countries will necessarily be unaccounted for

    # First, we find those missing cases by comparing the export and import lists
    # The export list - reporter countries, partner countries, commodities, and year
    exp.list <- L1011.comtrade_ff_BiTrade_y_ctry_item %>%
      filter(Element == "Export Quantity") %>%
      select(iso.reporter, iso.partner, Commodity_Code, year) %>%
      distinct()

    # The import list - reporter countries, partner countries, commodities, and year
    imp.list <- L1011.comtrade_ff_BiTrade_y_ctry_item %>%
      filter(Element == "Import Quantity") %>%
      select(iso.reporter, iso.partner, Commodity_Code, year) %>%
      distinct()

    # The two lists should have the same number of observations -
    # Partner countries in the export list should be reporter countries in the import list, and vice versa

    # Find the partner countries that are missing to report import
    L1011.comtrade_ff_BiTrade_y_ctry_item_missing_exp <- exp.list %>%
      anti_join(imp.list, by = c("Commodity_Code", "year",
                                 "iso.reporter" = "iso.partner",
                                 "iso.partner" = "iso.reporter")) %>%
      mutate(Element = "Export Quantity")
    # Find the partner countries that are missing to report export
    L1011.comtrade_ff_BiTrade_y_ctry_item_missing_imp <- imp.list %>%
      anti_join(exp.list, by = c("Commodity_Code", "year",
                                 "iso.reporter" = "iso.partner",
                                 "iso.partner" = "iso.reporter")) %>%
      mutate(Element = "Import Quantity")

    # Filter those asymetric obersvations in the original data, and flip reporter/partner, export/import
    L1011.comtrade_ff_BiTrade_y_ctry_item_full <- L1011.comtrade_ff_BiTrade_y_ctry_item_missing_exp %>%
      bind_rows(L1011.comtrade_ff_BiTrade_y_ctry_item_missing_imp) %>%
      inner_join(L1011.comtrade_ff_BiTrade_y_ctry_item,
                 by = c("iso.reporter", "iso.partner", "Element", "Commodity_Code", "year")) %>%
      mutate(Element = if_else(Element == "Import Quantity", "Export Quantity", "Import Quantity")) %>%
      select(iso.reporter = iso.partner, iso.partner = iso.reporter,
             Commodity_Code, Element, year, value) %>%
      bind_rows(L1011.comtrade_ff_BiTrade_y_ctry_item)

    # 3. Deriving extra-regional trade flows by GCAM region and traded commodity, filtering out within-region trade

    # Method: join in the reporting region and partner region (from reporting and partner
    # country), filter out where reporting and partner region are the same, convert units, aggregate by GCAM region and
    # commodity, and calculate the net trade, and take an unweighted average of the years.
    # Explanation: In multi-country GCAM regions, within-region trade is excluded.

    L1011.comtrade_ff_BiTrade_y_ctry_item_full %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, reporter_GCAM_region_ID = GCAM_region_ID),
                               by = c("iso.reporter" = "iso")) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, partner_GCAM_region_ID = GCAM_region_ID),
                               by = c("iso.partner" = "iso")) %>%
      filter(reporter_GCAM_region_ID != partner_GCAM_region_ID) %>%
      mutate(var = tolower(sub(" Quantity", "", Element)),
             GCAM_region_ID = reporter_GCAM_region_ID) %>%
      left_join(select(comtrade_commodity_GCAM, Commodity_Code, GCAM_Commodity), by = c("Commodity_Code")) %>%
      group_by(GCAM_region_ID, GCAM_Commodity, var, year) %>%
      summarise(value = sum(value)) %>%
      mutate(year = round(year/5)*5) %>% #Round year to nearest 5 (gcam period)
      group_by(GCAM_region_ID, GCAM_Commodity, var, year) %>%
      summarise(value = mean(value)) %>%
      ungroup() %>%
      complete(GCAM_region_ID = unique(GCAM_region_ID),
               GCAM_Commodity = unique(GCAM_Commodity),
               year = unique(year),
               var = unique(var)) %>%
      replace_na(list(value = 0)) %>%
      left_join(A_PrimaryFuelCCoef %>% select(PrimaryFuelCO2Coef.name, PrimaryFuelCO2Coef),
                by = c("GCAM_Commodity"="PrimaryFuelCO2Coef.name")) %>%
      left_join(fuel_carbon_content, by = "GCAM_Commodity") %>%
      mutate(value = value * Ccontent/(PrimaryFuelCO2Coef)*CONV_GJ_EJ) %>%
      spread(var, value) %>%
      mutate(net_trade = export - import) %>%
      select(GCAM_region_ID, GCAM_Commodity, year, GrossExp_EJ = export, GrossImp_EJ = import, net_trade) ->
      L1011.XregTrade_EJ_R_C


    #Reconcile comtrade bilateral trade data with fossil fuel trade data, so that net balance is zero.
    # NOTE: give precedence to imports (rather than exports) of each commodity. This is arbitrary but of little consequence, and generally reduces amount of trade.
    L1011.XregTrade_EJ_R_C %>%
      group_by(GCAM_Commodity, year) %>%
      mutate(GrossExp_EJ = GrossExp_EJ * sum(GrossImp_EJ)/sum(GrossExp_EJ),
             net_trade = GrossExp_EJ - GrossImp_EJ) %>%
      ungroup() ->
      L1011.ff_GrossTrade_EJ_R_C_Y

    # Produce outputs
    L1011.ff_GrossTrade_EJ_R_C_Y %>%
      add_title("L1011.ff_GrossTrade_EJ_R_C_Y") %>%
      add_units("EJ") %>%
      add_comments("Determined from bi-lateral trade flows; only includes trade between countries in different GCAM regions") %>%
      add_precursors("common/iso_GCAM_regID",
                     "common/GCAM_region_names",
                     "emissions/A_PrimaryFuelCCoef",
                     "energy/fuel_carbon_content",
                     "energy/mappings/comtrade_countrycode_ISO",
                     "energy/mappings/comtrade_commodity_code",
                     "energy/mappings/comtrade_trade_flow",
                     "energy/comtrade_ff_trade") ->
      L1011.ff_GrossTrade_EJ_R_C_Y

    return_data(L1011.ff_GrossTrade_EJ_R_C_Y)
  } else {
    stop("Unknown command")
  }
}
