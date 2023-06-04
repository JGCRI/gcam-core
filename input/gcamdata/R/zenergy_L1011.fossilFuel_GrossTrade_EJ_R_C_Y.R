# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L1011.ff_GrossTrade
#'
#' Calculate primary fossil fuel (coal, natural gas, crude oil) product balances, by region / commodity / year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1011.ff_GrossTrade_EJ_R_C_Y}, \code{L1011.ff_GrossTrade_EJ_R_Y_LNG},
#' \code{L1011.ff_GrossTrade_EJ_R_Y_NG_pipe}, \code{L1011.ff_BilatTrade_EJ_R_Y_NG_pipe} (energy level1).
#' @details This chunk processes the bi-lateral trade flow data matrix from UN Comtrade, in order to identify trade of
#'   fossil fuels between GCAM regions.
#' @importFrom dplyr anti_join bind_rows distinct filter group_by inner_join left_join mutate rename select ungroup
#' @importFrom tidyr complete drop_na replace_na spread
#' @author GPK/JEH April 2019
module_energy_L1011.ff_GrossTrade <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "common/GCAM_region_names",
             FILE = "emissions/A_PrimaryFuelCCoef",
             FILE = "energy/fuel_carbon_content",
             FILE = "energy/mappings/comtrade_countrycode_ISO",
             FILE = "energy/mappings/comtrade_commodity_code",
             FILE = "energy/mappings/comtrade_trade_flow",
             FILE = "energy/comtrade_ff_trade",
             FILE = "energy/GCAM_region_pipeline_bloc_import",
             FILE = "energy/GCAM_region_pipeline_bloc_export"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1011.ff_GrossTrade_EJ_R_C_Y",
             "L1011.ff_GrossTrade_EJ_R_Y_LNG",
             "L1011.ff_GrossTrade_EJ_R_Y_NG_pipe",
             "L1011.ff_BilatTrade_EJ_R_Y_NG_pipe"))
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
    # April 29th 2019: Note that as Taiwan is not recognized by the UN there is no TWN ISO specified in the comtrade_ISO mappings
    # According to COMTRADE code 490 is (in practice) only Taiwan, but when/if better data is available for Taiwan we may want to update
    comtrade_commodity_GCAM <- get_data(all_data, "energy/mappings/comtrade_commodity_code")
    comtrade_trade_flow <- get_data(all_data, "energy/mappings/comtrade_trade_flow")
    comtrade_ff_trade <- get_data(all_data, "energy/comtrade_ff_trade")
    GCAM_region_pipeline_bloc_import <- get_data(all_data, "energy/GCAM_region_pipeline_bloc_import")
    GCAM_region_pipeline_bloc_export <- get_data(all_data, "energy/GCAM_region_pipeline_bloc_export")


    # ----------------------------------------

    # 1: Filter and prepare the bi-lateral trade flow volume data by country and comtrade commodity
    # Select columns, filter to the import and export quantity variables, and filter to traded GCAM commodities
    # Note this data set was particularly selected for fossil fuel trade.
    # Additional data and mappings will need to be gathered at comtrade's website
    # (https://comtrade.un.org/db/dqBasicQuery.aspx) if other commodities are added

    comtrade_ff_trade %>%
      select(Year, Reporter_Code, Partner_Code, Trade_Flow_Code, Commodity_Code, value = `Netweight_(kg)`) %>%
      left_join_error_no_match(comtrade_trade_flow, by = c("Trade_Flow_Code" = "Trade_Flow_Code")) %>%
      # Join the reporter and partner countries.
      # Note also - this uses left_join_keep_first_only for countries like the USSR with multiple associated present-day
      # iso codes. We wouldn't want to repeat the trade data by each post-dissolution country, and since none of these
      # actually exist during the time frame for which gross trade is being assessed, there's no benefit to downscaling.
      left_join_keep_first_only(comtrade_ISO %>%
                                  select(Country_Code, ISO3 = ISO3_digit_Alpha),
                                by = c("Reporter_Code" = "Country_Code")) %>%
      rename(iso.reporter = ISO3) %>%
      left_join_keep_first_only(comtrade_ISO %>%
                                  select(Country_Code, ISO3 = ISO3_digit_Alpha),
                                by = c("Partner_Code" = "Country_Code")) %>%
      rename(iso.partner = ISO3,
             year = Year,
             Element = Trade) %>%
      mutate(iso.reporter = tolower(iso.reporter),
             iso.partner = tolower(iso.partner)) %>%
      select(year, iso.reporter, iso.partner, Element, Commodity_Code, value) %>%
      drop_na(value) %>%
      # We only want to look at regions that we can map to a GCAM region
      # In practice this filters out any NA iso or unmatching iso codes, which are generally small.
      # As of Nov 15th 2019, this includes Antarctica, British Indian Ocean Territory,
      # Curacao, Saint Maarten, and French Southern Territory.
      filter(iso.reporter %in% iso_GCAM_regID$iso,
             iso.partner %in% iso_GCAM_regID$iso) ->
      L1011.comtrade_ff_BiTrade_y_ctry_item


    # 2. Complete, clean, and re-balance the bilateral trade data
    # The bilateral trade data may not be symmetrical - some countries could be listed as partner countries but not reporter
    # countries (e.g. A country does not report trading with others, but appears as a partner countries for a number of others).
    # In those missing cases, we can use what's already available in the bilateral trade data, and simply flip reporter/partner
    # countries, as well as export/import to get a symmetrical dataset.
    # We'll also use the bilateral reporting to help filter out unreasonable values by taking the lower of the two reported values
    # if two countries report the same commodity trade flow for the same years.
    # For example, Mexico natural gas trade values for 2014 onward appear to be erroneous (unreasonably high).
    # Note that any pair of non-reporting partner/reporter countries will necessarily be unaccounted for.

    # Start by producing a complete set of trade patterns for all years
    L1011.comtrade_ff_BiTrade_y_ctry_item %>%
      complete(nesting(iso.reporter, iso.partner, Element, Commodity_Code),
               year = unique(L1011.comtrade_ff_BiTrade_y_ctry_item$year))  %>%
      group_by(iso.reporter, iso.partner, Element, Commodity_Code) %>%
      # Fill in missing values through interpolation between values and copying edge values forwards / backwards
      # TODO: the one place this extrapolation function is most questionable is for LNG,
      # which only picked up in the last several years.
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      # Map COMTRADE commodity to GCAM fuel commodities
      # Note that we also track trade modes for natural gas (pipeline vs. LNG) here
      left_join_error_no_match(comtrade_commodity_GCAM %>%
                                 select(Commodity_Code, GCAM_Commodity, GCAM_Commodity_traded),
                               by = c("Commodity_Code")) %>%
      left_join_error_no_match(A_PrimaryFuelCCoef %>%
                                 select(PrimaryFuelCO2Coef.name, PrimaryFuelCO2Coef),
                               by = c("GCAM_Commodity" = "PrimaryFuelCO2Coef.name")) %>%
      left_join_error_no_match(fuel_carbon_content, by = "GCAM_Commodity") %>%
      mutate(value = value * Ccontent / (PrimaryFuelCO2Coef) * CONV_GJ_EJ,
             Unit = "EJ") %>%
      select(-PrimaryFuelCO2Coef, -Ccontent) ->
      L1011.comtrade_ff_BiTrade_y_ctry_item_FULL

    # Create a separate list of export flows
    exp.list <- L1011.comtrade_ff_BiTrade_y_ctry_item_FULL %>%
      filter(Element == "Export Quantity")

    # Create a separate list of import flows
    imp.list <- L1011.comtrade_ff_BiTrade_y_ctry_item_FULL %>%
      filter(Element == "Import Quantity")

    # Join export and import lists. Using full join produces a full set of commodity flows, even if only one region reported.
    # Joining the import flows data by mapping "iso.reporter" to "iso.partner" (and vise versa) means that all flows in this
    # new data table are export flows (we've reversed the import flows).
    # This join will produce two value columns - value.x from the original export data, value.y from the original import data.
    exp.list %>%
      full_join(imp.list %>%
                  select(-Element),
                by = c("iso.reporter" = "iso.partner", "iso.partner" = "iso.reporter",
                       "Commodity_Code", "year", "GCAM_Commodity", "GCAM_Commodity_traded", "Unit")) %>%
      # If the export table value is NA, use the import table value
      mutate(value = if_else(is.na(value.x), value.y, value.x)) %>%
      # NAs in the value column mean that both the export table value and import table value are NA
      # Drop these entries
      drop_na(value) %>%
      # We'll default to the smaller of the two trade numbers to be conservative and avoid having
      # unreasonably large values skew the trade results.
      # Replace NAs with arbitrarily high number (otherwise NAs will be introduced to value column)
      replace_na(list(value.x = CONV_BIL_THOUS,
                      value.y = CONV_BIL_THOUS)) %>%
      mutate(value = if_else(value.y < value.x, value.y, value.x)) %>%
      select(-value.x, -value.y) %>%
      # Join in gcam region info
      left_join_error_no_match(select(iso_GCAM_regID, iso, reporter_GCAM_region_ID = GCAM_region_ID),
                               by = c("iso.reporter" = "iso")) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, partner_GCAM_region_ID = GCAM_region_ID),
                               by = c("iso.partner" = "iso")) %>%
      # Filter out intra-regional trade, i.e. trade between countries within a GCAM region, where
      # reporting and partner GCAM region are the same.
      filter(reporter_GCAM_region_ID != partner_GCAM_region_ID) -> L1011.comtrade_ff_BiTrade_y_ctry_item_FULL


    # 3. Calculate trade flows by GCAM region and traded commodity
    L1011.comtrade_ff_BiTrade_y_ctry_item_FULL %>%
      select(GCAM_region_ID = reporter_GCAM_region_ID, year, GCAM_Commodity, GCAM_Commodity_traded, value) %>%
      mutate(var = "exports") %>%
      bind_rows(L1011.comtrade_ff_BiTrade_y_ctry_item_FULL %>%
                  select(GCAM_region_ID = partner_GCAM_region_ID, year, GCAM_Commodity, GCAM_Commodity_traded, value) %>%
                  mutate(var = "imports")) %>%
      group_by(GCAM_region_ID,  year, GCAM_Commodity, var) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      spread(var, value) %>%
      replace_na(list(exports = 0,
                      imports = 0)) %>%
      mutate(net_trade = exports - imports) %>%
      select(GCAM_region_ID, GCAM_Commodity, year, GrossExp_EJ = exports, GrossImp_EJ = imports, net_trade) ->
      L1011.ff_GrossTrade_EJ_R_C_Y


    # 4. Add additional detail for natural gas trade.  We want
    #   - pipeline vs. LNG
    #   - explicit bilateral trade for pipelines

    # 4.a. Start with LNG, which is simplest
    L1011.comtrade_ff_BiTrade_y_ctry_item_FULL %>%
      filter(GCAM_Commodity_traded == "LNG") %>%
      select(GCAM_region_ID = reporter_GCAM_region_ID, year, GCAM_Commodity, GCAM_Commodity_traded, value) %>%
      mutate(var = "exports") %>%
      bind_rows(L1011.comtrade_ff_BiTrade_y_ctry_item_FULL %>%
                  filter(GCAM_Commodity_traded == "LNG") %>%
                  select(GCAM_region_ID = partner_GCAM_region_ID, year, GCAM_Commodity, GCAM_Commodity_traded, value) %>%
                  mutate(var = "imports")) %>%
      group_by(GCAM_region_ID, year, GCAM_Commodity, GCAM_Commodity_traded, var) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      spread(var, value) %>%
      replace_na(list(exports = 0,
                      imports = 0)) %>%
      mutate(net_trade = exports - imports) %>%
      select(GCAM_region_ID, GCAM_Commodity, GCAM_Commodity_traded, year,
             GrossExp_EJ = exports, GrossImp_EJ = imports, net_trade) ->
      L1011.ff_GrossTrade_EJ_R_Y_LNG


    # 4.b. Natural gas pipeline trade
    L1011.comtrade_ff_BiTrade_y_ctry_item_FULL %>%
      filter(GCAM_Commodity_traded == "gas pipeline") %>%
      group_by(GCAM_region_ID_exporter = reporter_GCAM_region_ID, GCAM_region_ID_importer = partner_GCAM_region_ID,
               year, GCAM_Commodity, GCAM_Commodity_traded) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      # Join in GCAM region names and pipeline network names for exporting regions
      left_join_error_no_match(GCAM_region_names %>%
                                 rename(GCAM_region_ID_exporter = GCAM_region_ID,
                                        origin.region = region),
                               by = c("GCAM_region_ID_exporter")) %>%
      left_join_error_no_match(GCAM_region_pipeline_bloc_export, by = c("origin.region")) %>%
      # Join in GCAM region names for importing regions
      left_join_error_no_match(GCAM_region_names %>%
                                 rename(GCAM_region_ID_importer = GCAM_region_ID,
                                        destination.region = region),
                               by = c("GCAM_region_ID_importer")) %>%
      # Use semi_join (filtering join) to remove any gas pipeline trade combinations that aren't allowed (e.g. USA to Africa)
      # NOTE: This removes nearly 50% of entries, but not more than 2% of trade in energy terms
      semi_join(GCAM_region_pipeline_bloc_import, by = c("destination.region" = "region", "pipeline.market")) %>%
      select(origin.region, pipeline.market, destination.region,
             GCAM_Commodity, GCAM_Commodity_traded, year, value) -> L1011.ff_BilatTrade_EJ_R_Y_NG_pipe

    # Gas exports by GCAM region and pipeline network (by default only one per region)
    L1011.ff_BilatTrade_EJ_R_Y_NG_pipe %>%
      group_by(origin.region, pipeline.market, GCAM_Commodity, GCAM_Commodity_traded, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() -> L1011.ff_GrossExport_EJ_R_Y_NG_pipe

    # Gas imports by GCAM region and pipeline network (regions can import from multiple pipeline networks)
    L1011.ff_BilatTrade_EJ_R_Y_NG_pipe %>%
      group_by(destination.region, pipeline.market, GCAM_Commodity, GCAM_Commodity_traded, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() -> L1011.ff_GrossImport_EJ_R_Y_NG_pipe

    # Gas imports by GCAM region and pipeline network (regions can import from multiple pipeline networks)
    L1011.ff_GrossImport_EJ_R_Y_NG_pipe %>%
      group_by(region = destination.region, GCAM_Commodity, GCAM_Commodity_traded, year) %>%
      summarise(GrossImp_EJ = sum(value)) %>%
      ungroup() %>%
      # not all regions have export or import data, which creates NAs when the tables are joined
      # LJENM throws an error, so left_join is used, and NAs addressed below
      full_join(L1011.ff_GrossExport_EJ_R_Y_NG_pipe %>%
                  select(region = origin.region, GCAM_Commodity, GCAM_Commodity_traded, year, GrossExp_EJ = value),
                by = c("region", "GCAM_Commodity", "GCAM_Commodity_traded", "year")) %>%
      replace_na(list(GrossExp_EJ = 0,
                      GrossImp_EJ = 0)) %>%
      mutate(net_trade = GrossExp_EJ - GrossImp_EJ) -> L1011.ff_GrossTrade_EJ_R_Y_NG_pipe


    # ----------------------------------------

    # Produce outputs
    L1011.ff_GrossTrade_EJ_R_C_Y %>%
      add_title("L1011.ff_GrossTrade_EJ_R_C_Y") %>%
      add_units("EJ") %>%
      add_comments("Determined from COMTRADE bi-lateral trade flows") %>%
      add_comments("only includes trade between countries in different GCAM regions") %>%
      add_precursors("common/iso_GCAM_regID",
                     "common/GCAM_region_names",
                     "emissions/A_PrimaryFuelCCoef",
                     "energy/fuel_carbon_content",
                     "energy/mappings/comtrade_countrycode_ISO",
                     "energy/mappings/comtrade_commodity_code",
                     "energy/mappings/comtrade_trade_flow",
                     "energy/comtrade_ff_trade") ->
      L1011.ff_GrossTrade_EJ_R_C_Y

    L1011.ff_GrossTrade_EJ_R_Y_LNG %>%
      add_title("Liquified Natural Gas (LNG) imports and exports by region and year") %>%
      add_units("EJ") %>%
      add_comments("Determined from COMTRADE bi-lateral trade flows") %>%
      add_comments("only includes trade between countries in different GCAM regions") %>%
      add_precursors("common/iso_GCAM_regID",
                     "common/GCAM_region_names",
                     "emissions/A_PrimaryFuelCCoef",
                     "energy/fuel_carbon_content",
                     "energy/mappings/comtrade_countrycode_ISO",
                     "energy/mappings/comtrade_commodity_code",
                     "energy/mappings/comtrade_trade_flow",
                     "energy/comtrade_ff_trade") ->
      L1011.ff_GrossTrade_EJ_R_Y_LNG

    L1011.ff_GrossTrade_EJ_R_Y_NG_pipe %>%
      add_title("Natural gas pipeline imports and exports by region and year") %>%
      add_units("EJ") %>%
      add_comments("Determined from COMTRADE bi-lateral trade flows") %>%
      add_comments("only includes trade between countries in different GCAM regions") %>%
      add_precursors("common/iso_GCAM_regID",
                     "common/GCAM_region_names",
                     "emissions/A_PrimaryFuelCCoef",
                     "energy/fuel_carbon_content",
                     "energy/mappings/comtrade_countrycode_ISO",
                     "energy/mappings/comtrade_commodity_code",
                     "energy/mappings/comtrade_trade_flow",
                     "energy/comtrade_ff_trade",
                     "energy/GCAM_region_pipeline_bloc_import",
                     "energy/GCAM_region_pipeline_bloc_export") ->
      L1011.ff_GrossTrade_EJ_R_Y_NG_pipe

    L1011.ff_BilatTrade_EJ_R_Y_NG_pipe %>%
      add_title("Natural gas pipeline imports and exports by region (with explicit bilateral trade detail) and year") %>%
      add_units("EJ") %>%
      add_comments("Determined from COMTRADE bi-lateral trade flows") %>%
      add_comments("only includes trade between countries in different GCAM regions") %>%
      add_precursors("common/iso_GCAM_regID",
                     "common/GCAM_region_names",
                     "emissions/A_PrimaryFuelCCoef",
                     "energy/fuel_carbon_content",
                     "energy/mappings/comtrade_countrycode_ISO",
                     "energy/mappings/comtrade_commodity_code",
                     "energy/mappings/comtrade_trade_flow",
                     "energy/comtrade_ff_trade",
                     "energy/GCAM_region_pipeline_bloc_import",
                     "energy/GCAM_region_pipeline_bloc_export") ->
      L1011.ff_BilatTrade_EJ_R_Y_NG_pipe

    return_data(L1011.ff_GrossTrade_EJ_R_C_Y,
                L1011.ff_GrossTrade_EJ_R_Y_LNG,
                L1011.ff_GrossTrade_EJ_R_Y_NG_pipe,
                L1011.ff_BilatTrade_EJ_R_Y_NG_pipe)
  } else {
    stop("Unknown command")
  }
}
