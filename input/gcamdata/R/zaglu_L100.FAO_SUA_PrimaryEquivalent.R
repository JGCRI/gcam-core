# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L100.FAO_SUA_PrimaryEquivalent
#'
#' Generate supply utilization balance in primary equivalent
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{GCAM_AgLU_SUA_APE_1973_2019},
#'   \code{FAO_AgProd_Kt_All},\code{FAO_AgArea_Kha_All},\code{FAO_Food_Macronutrient_All_2010_2019},
#'   \code{FAO_Food_MacronutrientRate_2010_2019_MaxValue}
#' @details This chunk compiles balanced supply utilization data in primary equivalent in GCAM region and commodities.
#' A method to generate primary equivalent is created for the new FAOSTAT supply utilization data (2010 to 2019).
#' New SUA balance is connected to the old one (before 2010). Production and harvested area data with FAO region and item
#' for primary production are provided. For FAO food items, macronutrient values are calculated at SUA item level.
#' Data processing was consistent across scales. Note that GCAM regions and commodities in aggregation mapping can
#' be changed in corresponding mappings. The output data is not averaged over time.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else inner_join left_join mutate rename select n group_by_at
#' @importFrom tidyr complete drop_na gather nesting spread replace_na
#' @author XZ 2022
module_aglu_L100.FAO_SUA_PrimaryEquivalent <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/AGLU_ctry",
      FILE = "common/iso_GCAM_regID",
      FILE = "common/GCAM_region_names",
      FILE = "aglu/FAO/GCAMDATA_FAOSTAT_SUA_195Regs_530Items_2010to2019",
      FILE = "aglu/FAO/GCAMDATA_FAOSTAT_BiTrade_194Regs_400Items_2010to2020",
      FILE = "aglu/FAO/Mapping_SUA_PrimaryEquivalent",
      FILE = "aglu/FAO/SUA_item_code_map",
      FILE = "aglu/FAO/GCAMDATA_FAOSTAT_ProdArea_195Regs_271Prod160AreaItems_1973to2020",
      FILE = "aglu/FAO/GCAMDATA_FAOSTAT_FBSH_CB_173Regs_118Items_1973to2009",
      FILE = "aglu/FAO/Mapping_item_FBS_GCAM",
      FILE = "aglu/FAO/GCAMDATA_FAOSTAT_ProdArea_96Regs_16FodderItems_1973to2020",
      FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
      FILE = "aglu/FAO/FAO_an_items_PRODSTAT",
      FILE = "aglu/FAO/GCAMDATA_FAOSTAT_MacroNutrientRate_179Regs_426Items_2010to2019Mean"
    )

  MODULE_OUTPUTS <-
    c("GCAM_AgLU_SUA_APE_1973_2019",
      "FAO_AgProd_Kt_All",
      "FAO_AgArea_Kha_All",
      "FAO_Food_Macronutrient_All_2010_2019",
      "FAO_Food_MacronutrientRate_2010_2019_MaxValue")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- Year <- Value <- FAO_country <- iso <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    All_Bal_element <- levels(GCAMDATA_FAOSTAT_SUA_195Regs_530Items_2010to2019$element)
    All_Bal_element <- factor(All_Bal_element, levels = All_Bal_element)

    # Section1: [2010-2019] Region aggregation of supply-utilization-accounting data ----

    # Note: the volume of data in this processing is quite large.  Therefore we took
    # extra care to be cognizant of processing speed and memory usage through section 1 and 2.
    # In particular we rely on ID codes and factors are much as possible to speed up joins.
    # In addition, we have filtered zero rows from the raw data to signfinficantly reduce
    # the overall volume.  Unfortunately, this change makes the processing riddled with
    # trap doors where we need to be extra careful to complete / refill zeros or risk loosing
    # rows of legitimate data.

    # create a complete area / iso / GCAM region mapping
    GCAMDATA_FAOSTAT_ProdArea_195Regs_271Prod160AreaItems_1973to2020 %>%
      select(area_code, area) %>%
      distinct() %>%
      left_join_error_no_match(AGLU_ctry %>% select(area = FAO_country, iso), by="area") %>%
      left_join(iso_GCAM_regID %>%select(iso, GCAM_region_ID), by = "iso") %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") ->
      Area_Region_Map

    # Aggregate to GCAM regions
    SUA_Reg_Agg <- function(GCAMDATA_FAOSTAT_SUA_195Regs_530Items_2010to2019, GCAMDATA_FAOSTAT_BiTrade_194Regs_400Items_2010to2020) {
      GCAMDATA_FAOSTAT_SUA_195Regs_530Items_2010to2019 %>%
      left_join_error_no_match(Area_Region_Map %>% select(area_code, GCAM_region_ID), by="area_code") %>%
      group_by(GCAM_region_ID, item_code, element, year) %>%
      summarize(value = sum(value)) %>%
      ungroup() ->
      DF_SUA_Agg

    # Calculate intra regional trade
    GCAMDATA_FAOSTAT_BiTrade_194Regs_400Items_2010to2020 %>%
      left_join_error_no_match(Area_Region_Map %>% select(area_code, GCAM_region_ID), by="area_code") %>%
      left_join_error_no_match(Area_Region_Map %>% select(source_code = area_code, source_GCAM_region_ID = GCAM_region_ID), by="source_code") %>%
      filter(GCAM_region_ID == source_GCAM_region_ID) %>%
      group_by(GCAM_region_ID, item_code, year) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      mutate(value = -value / 1000.0) ->
      DF_INTRA_REG_TRADE

    # #' Adjust gross trade in SUA data to ensure regional export is smaller than production for an SUA item
    bind_rows(DF_INTRA_REG_TRADE %>% mutate(element = All_Bal_element[All_Bal_element == "Export"]),
              DF_INTRA_REG_TRADE %>% mutate(element = All_Bal_element[All_Bal_element == "Import"]),
              DF_SUA_Agg) %>%
      group_by(GCAM_region_ID, item_code, element, year) %>%
      summarize(value = sum(value)) %>%
      ungroup() ->
      DF_SUA_Agg_TradeAdj

    # need to remove gross trade when export > production
    # to maintain triangle the inequality rule
    bind_rows(DF_INTRA_REG_TRADE %>% mutate(element = All_Bal_element[All_Bal_element == "Export"]),
              DF_INTRA_REG_TRADE %>% mutate(element = All_Bal_element[All_Bal_element == "Import"])) %>%
      rename(TCL = value) %>%
      # SUA has fewer items and years (2020) than the bilateral data set and in addition
      # there are some small discrepencies zero import/export in SUA vs tiny amounts of trade
      # in the bilateral.  Doing a left_join here will drop these descrepencies which is
      # what we would like to do in this case
      left_join(DF_SUA_Agg, ., by=c("GCAM_region_ID", "item_code", "year", "element")) %>%
      mutate(value = if_else(is.na(TCL), value, value + TCL)) %>%
      select(-TCL) %>%
      filter(value != 0.0) ->
      DF_SUA_Agg_TradeAdj

    DF_SUA_Agg_TradeAdj %>%
      filter(element %in% c("Production", "Import", "Export")) %>%
      spread(element, value, fill=0.0) %>%
      mutate(value = pmax(Production - Export, -Import)) %>%
      filter(value < 0) %>%
      select(-Production, -Import, -Export) ->
      GrossTradeRM

    bind_rows(GrossTradeRM %>% mutate(element = All_Bal_element[All_Bal_element == "Export"]),
              GrossTradeRM %>% mutate(element = All_Bal_element[All_Bal_element == "Import"]),
              DF_SUA_Agg_TradeAdj) %>%
      group_by(GCAM_region_ID, item_code, element, year) %>%
      summarize(value = sum(value)) %>%
      ungroup() ->
      DF_SUA_Agg_TradeAdj_TriagAdj

    return(DF_SUA_Agg_TradeAdj_TriagAdj)
    }

    # 1.2. Execution: regional aggregation ----
    # Get SUA data ready
    FAO_SUA_Kt_2010to2019_R <- SUA_Reg_Agg(GCAMDATA_FAOSTAT_SUA_195Regs_530Items_2010to2019,
                                         GCAMDATA_FAOSTAT_BiTrade_194Regs_400Items_2010to2020)

    Min_SUA_Year <- min(FAO_SUA_Kt_2010to2019_R$year)
    FAO_SUA_Kt_2010to2019 <- GCAMDATA_FAOSTAT_SUA_195Regs_530Items_2010to2019
    ## Clean up
    rm(GCAMDATA_FAOSTAT_SUA_195Regs_530Items_2010to2019)
    rm(GCAMDATA_FAOSTAT_BiTrade_194Regs_400Items_2010to2020)
    ## Done Section1 ----
    #****************************----

    # Section2: [2010-2019] Primary equivalent aggregation to GCAM commodities ----

    Mapping_SUA_PrimaryEquivalent %>%
      left_join_error_no_match(SUA_item_code_map %>% rename(sink_item_code = item_code), by=c("sink_item" = "item")) %>%
      left_join_error_no_match(SUA_item_code_map %>% rename(source_item_code = item_code), by=c("source_item" = "item")) %>%
      mutate(APE_comm = as.factor(APE_comm)) ->
      Mapping_SUA_PrimaryEquivalent_ID

    #Mapping_SUA_PrimaryEquivalent_ID[Mapping_SUA_PrimaryEquivalent_ID$sink_item_code == 235, "source_primary"] = FALSE

    Mapping_SUA_PrimaryEquivalent_ID %>%
      select(item_code = sink_item_code, output_specific_extraction_rate) %>%
      filter(!is.na(output_specific_extraction_rate)) ->
      OUTPUT_SPECIFIC_EXTRACTION_RATE

    # 2.1 Helper functions for SUA primary equivalent aggregation ----



    #' Get extraction rate
    #' @description Gross extraction rate is calculated for domestic, traded, and lagged values.
    #' By gross, it means sink items are aggregated.
    #' The function is used in Proc_primarize.
    #' @param DF_CURR_NEST Input supply-utilization accounting data frame with one tier of processing
    #' @param DF_ALL Input supply-utilization accounting data frame with ALL the data
    #' @return A data frame including regional, traded, and world extraction rates of a processing

    Get_GROSS_EXTRACTION_RATE <- function(DF_CURR_NEST, DF_ALL) {
      curr_sink_items = unique(DF_CURR_NEST$item_code)
      Mapping_SUA_PrimaryEquivalent_ID %>%
        filter(sink_item_code %in% curr_sink_items) ->
        Curr_Sink_Mapping
      curr_source_items = unique(Curr_Sink_Mapping$source_item_code)
      Mapping_SUA_PrimaryEquivalent_ID %>%
        filter(source_item_code %in% curr_source_items) ->
        Curr_Source_Mapping
      Curr_Source_Mapping %>%
        group_by(APE_comm) %>%
        mutate(minimium_extraction_rate = if_else(Q25asMin, extraction_rate_Q25, 0)) %>%
        select(APE_comm, minimium_extraction_rate) %>%
        distinct() ->
        MIN_EXTRACTION_RATE

      DF_ALL %>%
        #Prepare data to calculate regional, traded, and world average extraction rates
        tidyr::unnest(c(data)) %>%
        filter(element == "Processed", item_code %in% curr_source_items) %>%
        select(-nest_level) %>%
        bind_rows(DF_CURR_NEST %>% filter(element == "Production" | element == "Export")) %>%
        dplyr::group_by_at(vars(-item_code, -value)) %>%
        summarize(value=sum(value)) %>%
        ungroup() %>%
        complete(GCAM_region_ID = GCAM_region_names$GCAM_region_ID, nesting(element, year, APE_comm), fill=list(value=0)) %>%
        spread(element, value, fill = 0.0) %>%
        left_join_error_no_match(MIN_EXTRACTION_RATE, by=c("APE_comm")) %>%
        group_by(APE_comm, year) %>%
        mutate(extraction_rate_world = sum(Production) / sum(Processed),
               # in case sum(Processed) or sum(Production) == 0
               extraction_rate_world = if_else(extraction_rate_world != 0 & is.finite(extraction_rate_world),
                                               extraction_rate_world, 1),
               extraction_rate = Production / Processed,
               # Regional extraction rate = prod of an aggregated processed item  / Processed use of an aggregated primary item
               # Use world average to fill in NA or zero
               extraction_rate = if_else(is.na(extraction_rate) | extraction_rate == 0, extraction_rate_world, extraction_rate),
               # Using minimum extraction rate here
               extraction_rate = pmax(extraction_rate, minimium_extraction_rate),
               extraction_rate_trade = sum(Export) / sum(Export / extraction_rate),
               extraction_rate_trade = if_else(is.na(extraction_rate_trade), extraction_rate, extraction_rate_trade),
               # both processed and production > 0
               positive_prod = Production > 0 & Processed > 0) %>%
        ungroup() %>%
        group_by(APE_comm, GCAM_region_ID) %>%
        # Calculate lagged extraction_rate but replace NA with current rate (first period)
        mutate(extraction_rate_lag = lag(extraction_rate, default=extraction_rate[1])) %>%
        ungroup() %>%
        select(APE_comm, GCAM_region_ID, year, bal_import = extraction_rate_trade, bal_domestic_lag = extraction_rate_lag, bal_domestic_current = extraction_rate) %>%
        gather(bal_source, extraction_rate, bal_import, bal_domestic_lag, bal_domestic_current, factor_key = TRUE)
    }


    #' Separate the SUA balance into domestic and imported balanced for sink_item
    #' @description The function is used in Proc_primarize
    #' @param DF_CURR_NEST Input supply-utilization accounting data frame with one tier of processing
    #' @return SUA DF

    Get_ARMINGTON_BALANCE <- function(DF_CURR_NEST) {
      Import_Demand_Item <- factor(c("Food", "Feed", "Processed", "Other uses", "Seed", "Loss"), levels=All_Bal_element)

      DF_CURR_NEST %>%
        # Calculate imported consumption share
        # The assumption is that a portion of Import_Demand_Items was imported
        # so they need to be scaled by an international extraction rate
        # Note that stock variation is not included in import consumption to maintain stock balance
        # so additional adjustment may be needed
        filter(element == "Import" | element %in% Import_Demand_Item) %>%
        mutate(is_import = element == "Import") %>%
        spread(is_import, value, fill=0.0) %>%
        group_by(APE_comm, GCAM_region_ID, year, item_code) %>%
        summarize(import = sum(`TRUE`),
                  import_demand = sum(`FALSE`)) %>%
        ungroup() %>%
        mutate(Import_Demand_Share = import / import_demand,
               # replace NA and inf
               Import_Demand_Share = if_else(is.finite(Import_Demand_Share), Import_Demand_Share, 0),
               # The share should be small than 1 though outlier regions may import for storage
               Import_Demand_Share = pmin(Import_Demand_Share, 1),
               residual = import - import_demand * Import_Demand_Share) %>%
        ungroup() %>%
        select(APE_comm, GCAM_region_ID, item_code, year, Import_Demand_Share, residual) %>%
        left_join(DF_CURR_NEST, ., by=c("APE_comm", "GCAM_region_ID", "item_code", "year")) %>%
        # when Import_Demand_Item consumption < Import they are used to share out Import consumptions
        # otherwise, Residuals is used for adjustments
        mutate(bal_import = case_when(element == "Import" ~ value,
                                      element %in% Import_Demand_Item ~ value * Import_Demand_Share,
                                      element == "Residuals" ~ residual,
                                      TRUE ~ 0),
               # Calculate domestic balance
               bal_domestic = value - bal_import) %>%
        select(-value, -Import_Demand_Share, -residual) %>%
        gather(bal_source, value, bal_import, bal_domestic, factor_key = TRUE) ->
        TradeBal_Data

      Regional_supply_elements <- factor(c("Opening stocks", "Production", "Import"), levels=All_Bal_element)
      Regional_demand_elements <- factor(c("Export", "Feed", "Food", "Loss", "Processed", "Seed", "Other uses", "Closing stocks"), levels=All_Bal_element)
      TradeBal_Data %>%
        mutate(is_supply = element %in% Regional_supply_elements,
               is_demand = element %in% Regional_demand_elements) %>%
        filter(is_supply | is_demand) %>%
        group_by(APE_comm, GCAM_region_ID, year, item_code, bal_source) %>%
        # Clean the bal items
        summarize(`Regional supply` = sum(value[is_supply]),
                  `Regional demand` = sum(value[is_demand])) %>%
        ungroup() %>%
        mutate(`Residuals` = `Regional supply` - `Regional demand`) %>%
        gather(element, value, `Regional supply`, `Regional demand`, `Residuals`) %>%
        mutate(element = factor(element, levels=All_Bal_element)) %>%
        bind_rows(TradeBal_Data %>% filter(!element %in% c("Regional supply", "Regional demand", "Residuals")), .)
    }


    #' Separate the domestic SUA balance into current and lagged balanced for sink_item
    #' @description The function is used in Proc_primarize
    #' @param DF_CURR_NEST_TradeAdj Output from Get_ARMINGTON_BALANCE. Input supply-utilization accounting data frame with one tier of processing and
    #' @param .SINK_ITEM Sink items or processed items in the processing
    #' @return SUA DF

    Get_STOCK_BALANCE <- function(DF_CURR_NEST_TradeAdj) {
      Opening_Stock_Item <- factor(c("Food", "Feed", "Processed", "Other uses", "Seed", "Loss"), levels=All_Bal_element)

      get_bal_source_data <- function(data, bal_source_key) {
        data[data$bal_source == bal_source_key, "data", drop = TRUE][[1]]
      }

      DF_CURR_NEST_TradeAdj %>%
        tidyr::nest(data = -bal_source) ->
        StockCalcNested

      StockCalcNested %>%
        get_bal_source_data("bal_domestic") %>%
        filter(element == "Opening stocks" | element %in% Opening_Stock_Item) %>%
        mutate(is_opening = element == "Opening stocks") %>%
        spread(is_opening, value, fill=0.0) %>%
        group_by(APE_comm, GCAM_region_ID, year, item_code) %>%
        summarize(Ostock = sum(`TRUE`),
                  Ostock_demand = sum(`FALSE`)) %>%
        ungroup() %>%
        mutate(Ostock_Demand_Share = Ostock / Ostock_demand,
               # The share should be small than 1
               # Other elements will be adjusted if not
               Ostock_Demand_Share = if_else(is.finite(Ostock_Demand_Share), Ostock_Demand_Share, 0),
               Ostock_Demand_Share = pmin(Ostock_Demand_Share, 1),
               residual = Ostock - Ostock_demand * Ostock_Demand_Share) %>%
        ungroup() %>%
        select(APE_comm, GCAM_region_ID, item_code, year, Ostock_Demand_Share, residual) %>%
        left_join(StockCalcNested %>% get_bal_source_data("bal_domestic"), ., by=c("APE_comm", "GCAM_region_ID", "item_code", "year")) %>%
        mutate(bal_domestic_lag = case_when(element == "Opening stocks" ~ value,
                                            element %in% Opening_Stock_Item ~ value * Ostock_Demand_Share,
                                            element == "Residuals" ~ residual,
                                            TRUE ~ 0),
               # Calculate domestic balance
               bal_domestic_current = value - bal_domestic_lag) %>%
        select(-value, -Ostock_Demand_Share, -residual) %>%
        gather(bal_source, value, bal_domestic_lag, bal_domestic_current, factor_key = TRUE) ->
        StockBal_Data

      Regional_supply_elements <- factor(c("Opening stocks", "Production", "Import"), levels=All_Bal_element)
      Regional_demand_elements <- factor(c("Export", "Feed", "Food", "Loss", "Processed", "Seed", "Other uses", "Closing stocks"), levels=All_Bal_element)
      Bal_types = c("bal_import", "bal_domestic_lag", "bal_domestic_current")
      Bal_types = factor(Bal_types, levels=Bal_types)
      StockBal_Data %>%
        complete(year = unique(StockBal_Data$year), nesting(GCAM_region_ID, item_code, element, APE_comm, bal_source), fill=list(value=0)) %>%
        mutate(is_supply = element %in% Regional_supply_elements,
               is_demand = element %in% Regional_demand_elements) %>%
        group_by(APE_comm, GCAM_region_ID, year, item_code, bal_source) %>%
        # Clean the bal items
        summarize(`Regional supply` = sum(value[is_supply]),
                  `Regional demand` = sum(value[is_demand]),
                  # using max to guard against missing Closing stocks row
                  `Stock Variation` = max(value[element == "Closing stocks"], 0) - max(value[element == "Opening stocks"], 0)) %>%
        ungroup() %>%
        mutate(`Residuals` = `Regional supply` - `Regional demand`) %>%
        gather(element, value, `Regional supply`, `Regional demand`, `Stock Variation`, `Residuals`) %>%
        mutate(element = factor(element, levels=All_Bal_element)) %>%
        bind_rows(StockBal_Data %>% filter(!element %in% c("Regional supply", "Regional demand", "Stock Variation", "Residuals")), .) %>%
        tidyr::nest(data = - bal_source) %>%
        mutate(bal_source = factor(bal_source, levels=Bal_types)) %>%
        bind_rows(StockCalcNested %>% filter(bal_source == "bal_import") %>% mutate(bal_source = factor(bal_source, levels=Bal_types))) %>%
        tidyr::unnest(c("data"))
    }


    #' Primary equivalent aggregation
    #' @param DF_ALL Input supply-utilization accounting data frame with all levels of data nested which need to be primarized
    #' @return A supply-utilization accounting data frame with all levels processed and aggregated to GCAM_commodity

    Proc_primarize <- function(DF_ALL){
      MaxNest = max(DF_ALL$nest_level)
      MinNest = 1
      for(curr_nest in MaxNest:MinNest) {
        # get the current tier to process
        DF_ALL %>%
          filter(nest_level == curr_nest) %>%
          pull(data) %>%
          first() ->
          DF_CURR_NEST

        # Sink items or processed items in the processing
        curr_sink_items = unique(DF_CURR_NEST$item_code)
        Mapping_SUA_PrimaryEquivalent_ID %>%
          filter(sink_item_code %in% curr_sink_items) ->
          Curr_Sink_Mapping
        # Source items or primary items in the processing
        curr_source_items = unique(Curr_Sink_Mapping$source_item_code)
        Mapping_SUA_PrimaryEquivalent_ID %>%
          filter(source_item_code %in% curr_source_items) ->
          Curr_Source_Mapping

        # OUTPUT_SPECIFIC_EXTRACTION_RATE A data frame with item and output_specific_extraction_rate.
        #' # In some cases, prescale sink item SUA using output_specific_extraction_rate can improve the processing.
        #' # e.g., when coproduction shares are not fixed.
        if(nrow(OUTPUT_SPECIFIC_EXTRACTION_RATE %>% filter(item_code %in% curr_sink_items)) > 0) {
          ## a. Pre-scale sink item data when .OUTPUT_SPECIFIC_EXTRACTION_RATE is available ----
          DF_CURR_NEST %>%
            left_join(OUTPUT_SPECIFIC_EXTRACTION_RATE, by=c("item_code")) %>%
            replace_na(list(output_specific_extraction_rate = 1)) %>%
            mutate(value = value / output_specific_extraction_rate) %>%
            select(-output_specific_extraction_rate) ->
            DF_CURR_NEST
        }

        ## b. For the sink items of the tier, separate balance into domestic and imported ----
        # Note that the method here relies on Get_GROSS_EXTRACTION_RATE and Get_ARMINGTON_BALANCE
        DF_CURR_NEST %>%
          Get_ARMINGTON_BALANCE() %>%
          Get_STOCK_BALANCE() %>%
          # Get extraction rate for domestic and traded
          # Note that extraction rates are mean values across time
          # Note that regional extraction rate could be inf
          # It is likely due to data inconsistency, e.g., zero processed in source but positive sink
          # No adjustments were made since 1/inf become zero in the scaling process, preserving primary balance
          left_join(Get_GROSS_EXTRACTION_RATE(DF_CURR_NEST, DF_ALL), by=c("APE_comm", "GCAM_region_ID", "year", "bal_source")) %>%
          # Scale sink items to get source item equivalent
          mutate(value = value / extraction_rate) %>%
          select(-extraction_rate) ->
          .df1

        ## c. Aggregate sink_items are aggregated into "sink_item" ----
        # And production & processed are adjusted for primary aggregation
        # Bind source items as well
        DF_ALL %>%
          filter(nest_level <= curr_nest) %>%
          tidyr::unnest(c("data")) %>%
          filter(element == "Processed", item_code %in% curr_source_items) %>%
          select(-element) ->
          .df2
        .df2 %>%
          complete(GCAM_region_ID = GCAM_region_names$GCAM_region_ID, nesting(APE_comm, item_code, nest_level, year), fill=list(value=0)) %>%
          complete(.df2 %>% distinct(APE_comm, item_code, nest_level), nesting(GCAM_region_ID, year), fill=list(value=0)) %>%
          group_by(APE_comm, item_code) %>%
          mutate(value = sum(value)) %>%
          ungroup() %>%
          group_by(APE_comm, GCAM_region_ID, year) %>%
          mutate(share = value/ sum(value),
                 share = if_else(is.finite(share), share, dplyr::n()/sum(dplyr::n()))) %>%
          ungroup() %>%
          select(-value, source_item_code = item_code) ->
          source_share


        ## d. Merge sink SUA into source items SUA  ----
        # Note that with multiple source items, sinks are aggregated into sources based on average processed shares across sources
        # Prepare data to calculate world average source share
        .df1 %>%
          left_join(source_share, by=c("APE_comm", "GCAM_region_ID", "year")) %>%
          filter(!is.na(share)) %>%
          mutate(value = value * share,
                 item_code = source_item_code) %>%
          select(-share) %>%
          group_by(nest_level, APE_comm, GCAM_region_ID, year, item_code, element) %>%
          summarize(value = sum(value)) %>%
          ungroup() %>%
          complete(element=All_Bal_element[All_Bal_element %in% c("Prodution", "Processed")], nesting(nest_level, APE_comm, GCAM_region_ID, year, item_code), fill=list(value=0)) %>%
          group_by(nest_level, APE_comm, GCAM_region_ID, year, item_code) %>%
          mutate(value = if_else(element == "Production" | element == "Processed", value - value[element == "Production"], value)) %>%
          ungroup() ->
          .df3

        # Bind source item and aggregated across source & sink items based on primary equivalent
        # Note we will bind and aggregate by nest, ultimately it seems unlikely nesting and provided
        # any performance boost, but certainly didn't hurt either.
        .df3 %>%
          tidyr::nest(data = -nest_level) ->
          df3_nested

        for(nest_i in df3_nested$nest_level) {
          bind_rows(
            DF_ALL[DF_ALL$nest_level == nest_i, "data", drop=TRUE][[1]],
            df3_nested[df3_nested$nest_level == nest_i, "data", drop=TRUE][[1]]) %>%
            group_by(APE_comm, GCAM_region_ID, year, item_code, element) %>%
            summarize(value = sum(value)) %>%
            ungroup() ->
            AGG
          DF_ALL %>%
            filter(nest_level != nest_i) %>%
            bind_rows(tibble(nest_level = nest_i, data = list(AGG))) ->
            DF_ALL
        }
        # drop the processed tier as the data has now been aggregated and thus
        # no longer needed
        DF_ALL %<>% filter(nest_level != curr_nest)
      }

      # Combine the remaining items by APE_comm
      DF_ALL %>%
        tidyr::unnest(c("data")) %>%
        group_by(GCAM_region_ID, APE_comm, element, year) %>%
        summarize(value = sum(value)) %>%
        ungroup() %>%
        spread(element, value, fill = 0.0) %>%
        # Do a final balance cleaning
        mutate(`Regional supply` = `Opening stocks` + Production + `Import`,
               `Regional demand` = `Export` + Feed + Food + Loss + Processed + Seed + `Other uses` +`Closing stocks`,
               Residuals = `Regional supply` -  `Regional demand`) %>%
        gather(element, value, -GCAM_region_ID, -APE_comm, -year) ->
        APE_AGG

      # Aggregate by GCAM_commodity
      # At this point we ditch the ID codes and factors as we return the data and
      # make it available for the rest of the orginal processing
      APE_AGG %>%
        left_join_error_no_match(Mapping_SUA_PrimaryEquivalent %>% select(GCAM_commodity, APE_comm) %>% distinct(),
                                 by = c("APE_comm")) %>%
        group_by(GCAM_region_ID, GCAM_commodity, element, year) %>%
        summarize(value = sum(value)) %>%
        ungroup() %>%
        left_join_error_no_match(GCAM_region_names, by=c("GCAM_region_ID")) %>%
        mutate(element = as.character(element)) %>%
        select(region, year, GCAM_commodity, element, value) ->
        GCAM_APE_after2010

      return(GCAM_APE_after2010)
    }

    # 2.2. Execution: process data into APE ----


    ## Loop through all GCAM_commodity with available data ----

    FAO_SUA_Kt_2010to2019_R %>%
      left_join(Mapping_SUA_PrimaryEquivalent_ID %>% select(APE_comm, item_code = sink_item_code, nest_level) %>% distinct(), by=c("item_code" = "item_code")) %>%
      left_join(Mapping_SUA_PrimaryEquivalent_ID %>% select(APE_comm_source = APE_comm, item_code = source_item_code) %>% distinct(), by=c("item_code")) %>%
      # find SUA items which are truly not mapped to anything and filter them out
      mutate(APE_comm = if_else(is.na(APE_comm), APE_comm_source, APE_comm)) %>%
      select(-APE_comm_source) %>%
      filter(!is.na(APE_comm)) %>%
      # the remaining rows with NA nest_level are primary, we need to keep them
      # around for processing even though they don't need to be aggregated themselves
      # so we will give them a nest level of -1
      mutate(nest_level = if_else(is.na(nest_level), -1L, nest_level)) %>%
      # we will literally nest by nest level to avoid constant subseting
      # although we end up needed to unnest at times as well so ultimately,
      # it likely makes little difference in performance
      tidyr::nest(data = -nest_level)  %>%
      # we are now ready to recursively primarize APE commodities then aggregate
      # to GCAM commodities
      Proc_primarize() ->
      GCAM_APE_after2010

    rm(FAO_SUA_Kt_2010to2019_R)

    ## Done Section2 ----
    #****************************----

    # Section3 [1970-2009] Food balance sheet (original) aggregation to GCAM regions and commodities ----

    # 3.1. Helper functions ----

    #' Balance gross trade
    #' @description Scale gross export and import in all regions to make them equal at the world level.
    #' @param .DF An input dataframe with an element col including Import and Export
    #' @param .MIN_TRADE_PROD_RATIO Trade will be removed if world total export or import over production is smaller than .MIN_TRADE_PROD_RATIO (1% default value)
    #' @param .Reg_VAR Region variable name; default is ("area_code")
    #' @param .GROUP_VAR Group variable; default is ("item_code", "year")
    #' @return The same dataframe with balanced world export and import.

    GROSS_TRADE_ADJUST <- function(.DF,
                                   .MIN_TRADE_PROD_RATIO = 0.01,
                                   .Reg_VAR = 'area_code',
                                   .GROUP_VAR = c("item_code", "year")){

      # assert .DF structure
      assertthat::assert_that(all(c("element", .GROUP_VAR) %in% names(.DF)))
      assertthat::assert_that(dplyr::is.grouped_df(.DF) == F)
      assertthat::assert_that(all(c("Import", "Export", "Production") %in%
                                    c(.DF %>% distinct(element) %>% pull)))

      .DF %>%
        # Join ExportScaler and ImportScaler
        left_join(
          .DF %>%
            spread(element, value) %>%
            dplyr::group_by_at(vars(dplyr::all_of(.GROUP_VAR))) %>%
            # filter out items with zero world trade or production
            # and replace na to zero later for scaler
            replace_na(list(Export = 0, Import = 0, Production = 0)) %>%
            filter(sum(Export) != 0, sum(Import) != 0, sum(Production) != 0) %>%
            # world trade should be later than .MIN_TRADE_PROD_RATIO to have meaningful data
            # depending on item group, .MIN_TRADE_PROD_RATIO can be set differently
            filter(sum(Export) / sum(Production) > .MIN_TRADE_PROD_RATIO) %>%
            filter(sum(Import) / sum(Production) > .MIN_TRADE_PROD_RATIO) %>%
            # finally,
            # use average gross trade value to calculate trade scaler
            # the trade scalers will be applied to all regions
            mutate(ExportScaler = (sum(Export) + sum(Import))/ 2 / sum(Export),
                   ImportScaler = (sum(Export) + sum(Import))/ 2 / sum(Import)) %>%
            select(dplyr::all_of(c(.Reg_VAR, .GROUP_VAR)), ExportScaler, ImportScaler) %>%
            ungroup(),
          by = c(dplyr::all_of(c(.Reg_VAR, .GROUP_VAR)))) %>%
        replace_na(list(ExportScaler = 0, ImportScaler = 0)) %>%
        # If world export, import, or prod is 0, trade will be zero
        mutate(value = case_when(
          element %in% c("Export") ~ value * ExportScaler,
          element %in% c("Import") ~ value * ImportScaler,
          TRUE ~ value)) %>%
        select(-ExportScaler, -ImportScaler)

    }

    # 3.2. Execution ----
    ## a. FBSH_CB aggregate to GCAM commodity and region----

    GCAMDATA_FAOSTAT_FBSH_CB_173Regs_118Items_1973to2009 %>%
      gather_years() %>%
      filter(year < Min_SUA_Year) %>%
      filter(!is.na(value)) ->
      FBSH_CB

    Mapping_item_FBS_GCAM %>%
      select(item_code, GCAM_commodity)%>%
      filter(!is.na(GCAM_commodity)) %>%
      left_join(FBSH_CB %>%
                  # complete element
                  complete(nesting(area, area_code, item_code, item, year), element,
                           fill = list(value = 0)),
                by = "item_code") %>%
      dplyr::group_by_at(vars(-value, -item, -item_code)) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      gcamdata::left_join_error_no_match(AGLU_ctry %>% select(area = FAO_country, iso), by = "area") %>%
      gcamdata::left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
      gcamdata::left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      dplyr::group_by_at(vars(area = region, year, GCAM_commodity, element)) %>%
      summarise(value = sum(value), .groups = "drop") ->
      FBSH_CB_GCAM

    ## b. Get primary production in GCAM region and sector ----

    GCAMDATA_FAOSTAT_ProdArea_195Regs_271Prod160AreaItems_1973to2020 %>%
      gather_years() %>%
      filter(year < Min_SUA_Year) %>%
      filter(!is.na(value), element == "Production") %>%
      inner_join(
        Mapping_SUA_PrimaryEquivalent %>% filter(source_primary == T) %>%
          distinct(GCAM_commodity, item = source_item), by = "item") %>%
      gcamdata::left_join_error_no_match(AGLU_ctry %>% select(area = FAO_country, iso), by = "area") %>%
      gcamdata::left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
      gcamdata::left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      dplyr::group_by_at(vars(area = region, year, GCAM_commodity, element)) %>%
      summarise(value = sum(value), .groups = "drop") ->
      QCL_PROD_GCAM

    ## c. QCL_PROD_GCAM and FBSH_CB_GCAM: merge, scale, and connect----
    # Connect SUA (FBSH_CB) to primary production (QCL_PROD_GCAM)
    # Primary production could be different due to aggregation or inconsistency

    QCL_PROD_GCAM %>%
      # Complete elements in QCL_PROD
      # also GCAM_commodity because no pork production in Pakistan
      complete(area, year, GCAM_commodity,
               element = unique(FBSH_CB_GCAM$element), fill = list(value = 0)) %>%
      left_join_error_no_match(
        FBSH_CB_GCAM %>%
          # complete element to add zero productions
          complete(area, year, GCAM_commodity, element, fill = list(value = 0)) %>%
          rename(FBSH_CB = value),
        by = c("area", "year", "GCAM_commodity", "element")
      ) %>%
      # mapping
      group_by(area, GCAM_commodity, year) %>%
      # When production in FBSH_CB > primary production (QCL_PROD_GCAM), adjust processed
      # to ensure production <= primary production when processed is enough; (will scale later)
      # calculate Prod_diff which is the diff in production b/t the two data sets
      mutate(Prod_diff = FBSH_CB[element == "Production"] - value[element == "Production"] ) %>%
      # adjust processed when prod_diff > 0 by canceling off production and processed
      mutate(Prod_diff = if_else(Prod_diff > 0 & element %in% c("Production", "Processed"), Prod_diff, 0),
             Processed = if_else(Prod_diff > 0 & element %in% c("Production", "Processed"),
                                 FBSH_CB[element == "Processed"], 0),
             FBSH_CB = FBSH_CB - pmin(Prod_diff, Processed),
             # When production in FBSH_CB = 0 set to production in QCL so scaling will be consistent
             FBSH_CB = if_else(FBSH_CB[element == "Production"] == 0 & element == "Production", value, FBSH_CB),
             # After the above adjustments, re-scale SUA to match production in value
             value = FBSH_CB * value[element == "Production"]/FBSH_CB[element == "Production"],
             # fix NA
             value = if_else(!is.finite(value), FBSH_CB, value)) %>%
      ungroup() %>%
      select(area, GCAM_commodity, element, year, value) %>%
      # adjust gross trade
      GROSS_TRADE_ADJUST(.MIN_TRADE_PROD_RATIO = 0.001,
                         .Reg_VAR = "area",
                         .GROUP_VAR = c("GCAM_commodity", "year")) %>%
      spread(element, value) %>%
      # Note that stock variation here was  = opening - ending
      # reversed here so the variation is a demand
      mutate(`Stock Variation` = - `Stock Variation`,
             `Regional supply` = Production + `Import`,
             `Regional demand` = `Export` + Feed + Food + Loss + Processed + Seed + `Other uses` + `Stock Variation`,
             Residuals = `Regional supply` -  `Regional demand`) %>%
      tidyr::gather(element, value, -area, -GCAM_commodity, -year) %>%
      ungroup() %>%
      rename(region = area) ->
      GCAM_APE_before2010

    rm(FBSH_CB, FBSH_CB_GCAM)

    ## Done Section3 ----
    #****************************----

    # Section4 [1970-2019] GCAM_APE SUA ----

    # 4.1. Helper functions ----
    Check_Balance_SUA <- function(.DF){

      assertthat::assert_that(all(c("element") %in% names(.DF)))
      assertthat::assert_that(all(c("Import", "Export", "Production",
                                    "Food", "Feed", "Other uses") %in%
                                    c(.DF %>% distinct(element) %>% pull)))
      # 0. Check NA
      if (.DF %>% filter(is.na(value)) %>% nrow() > 0) {
        warning("NA values in SUA Balance")
      }


      # 1. Positive value except stock variation and residues
      if (isFALSE(.DF %>% filter(!element %in% c("Stock Variation", "Other uses")) %>%
                 summarise(min = min(value, na.rm = T)) %>% pull(min) >= -0.001)) {
        warning("Negative values in key elements (not including stock variation and other uses)")
      }

      # 2. Trade balance in all year and items
      if (isFALSE(.DF %>% filter(element %in% c("Import", "Export")) %>%
                 group_by(year, GCAM_commodity, element) %>%
                 summarise(value = sum(value), .groups = "drop") %>%
                 spread(element, value) %>% filter(abs(Import - Export) > 0.0001) %>% nrow() == 0)) {
        warning("Gross trade imbalance")
      }

      # 3. SUA balance check
      if (isFALSE(.DF %>%
                 spread(element, value) %>%
                 mutate(`Regional supply` = Production + `Import`,
                        `Regional demand` = `Export` + Feed + Food  + `Other uses`,
                        bal = abs(`Regional supply` -  `Regional demand`)) %>%
                 filter(bal > 0.0001) %>% nrow() == 0)) {
        warning("Regional supply != Regional demand + Residuals")
      }

      # 4. Balanced in all dimensions
      assertthat::assert_that(.DF %>% nrow() ==
                                .DF %>% distinct(year) %>% nrow *
                                .DF %>% distinct(GCAM_commodity) %>% nrow *
                                .DF %>% distinct(element) %>% nrow *
                                .DF %>% distinct(region) %>% nrow)

    }

    # 4.2. Connect and bind data from two periods ----

    GCAM_AgLU_SUA_APE_1973_2019 <-
      GCAM_APE_before2010 %>%
      bind_rows(GCAM_APE_after2010) %>%
      mutate(unit = "1000 tonnes") %>%
      # clean and aggregate elements not using
      filter(!element %in% c("Regional demand", "Regional supply",
                             "Opening stocks", "Closing stocks")) %>%
      mutate(element = replace(element,
                               element %in% c("Stock Variation", "Processed",
                                              "Seed", "Residuals", "Loss"),
                               "Other uses")) %>%
      dplyr::group_by_at(dplyr::vars(-value)) %>%
      summarise(value = sum(value), .groups = "drop")

    ## Check balance
    GCAM_AgLU_SUA_APE_1973_2019 %>% Check_Balance_SUA
    rm(GCAM_APE_before2010, GCAM_APE_after2010)


    ## Done Section4 ----
    #****************************----

    # Section5 [1970-2019] Connect production and area data ----

    # This section gets crop and livestock production before aggregation (FAO region and items)
    # For both before 2010 and after 2010
    # They are also aggregated to GCAM region and commodities to assert consistency
    # The processing includes all crops (including fodder crops) and livestock items

    # 5.1. Get all mapping straight ----

    Primary_Item_CROP <-
      FAO_ag_items_PRODSTAT %>%
        select(item, GCAM_commodity, GCAM_subsector) %>%
        filter(!is.na(item), !is.na(GCAM_commodity)) %>%
        # Fodder grass has a duplicate as it mapped to different GTAP crops
        distinct %>%
        mutate(CropMeat = if_else(GCAM_commodity %in% c("FodderGrass", "FodderHerb"),
                                  "Crop_Fodder", "Crop_NonFodder"))
    assertthat::assert_that(
      all(Primary_Item_CROP %>% filter(CropMeat == "Crop_NonFodder") %>%  pull(item) %in%
        c(Mapping_SUA_PrimaryEquivalent %>% filter(source_primary == T) %>%
          distinct(item = source_item) %>% pull)),
      msg = "Inconsistent mapping of primary crops between FAO_ag_items_PRODSTAT and Mapping_SUA_PrimaryEquivalent" )

    Primary_Item_MEAT <-
      Mapping_SUA_PrimaryEquivalent %>%
      # animal meat Eq since they are included as primary production after 2010
      filter(source_primary == T | grepl("MeatEq", APE_comm)) %>%
      distinct(GCAM_commodity, item = source_item) %>%
      filter(GCAM_commodity %in%
               c(FAO_an_items_PRODSTAT %>%
                   filter(!is.na(GCAM_commodity)) %>%
                   distinct(GCAM_commodity) %>% pull))%>%
      mutate(CropMeat = "Meat")

    # 5.2. Get primary production for all ----
    # Connecting, mapping, arrange, and assertion

    ## Bind production and area data for both fodder and nonfodder ----
    FAO_AgProd_Kt_Area_Kha <-
      GCAMDATA_FAOSTAT_ProdArea_96Regs_16FodderItems_1973to2020%>%
      mutate(item_set = "QCL_COMM_CROP_PRIMARY_FODDER") %>%
      bind_rows(GCAMDATA_FAOSTAT_ProdArea_195Regs_271Prod160AreaItems_1973to2020) %>%
      gather_years() %>%
      filter(!is.na(value))

    # Assert yield no inf.
    assertthat::assert_that(
      # only safeguard here as data was cleaned and area and prod are matched
      FAO_AgProd_Kt_Area_Kha %>%
        # filter only primary crop items (all crops with area)
        filter(item_set %in% c("QCL_COMM_CROP_PRIMARY",
                               "QCL_COMM_CROP_PRIMARY_FODDER")) %>%
        select(-unit) %>% spread(element, value) %>%
        filter(is.infinite(Production / `Area harvested`|
                             is.infinite(`Area harvested`/Production)) ) %>%
        nrow == 0,
      msg = "Check region/item for prod > 0 & area = 0 or prod = 0 & area > 0" )

    # Assert primary production in two sources area consistent
    assertthat::assert_that(
      FAO_SUA_Kt_2010to2019 %>% filter(element == "Production") %>%
        inner_join(FAO_AgProd_Kt_Area_Kha %>%
                     filter(item_set != "QCL_COMM_OTHERPROC") %>%
                     filter(element == "Production") %>% rename(value1 = value),
                   by = c("area_code", "item_code", "element", "year")) %>%
        mutate(diff = abs(value1 - value)) %>%
        filter(diff > 0.0001) %>% nrow() == 0,
      msg = "Primary production in SUA (FAO_SUA_Kt_2010to2019) and
      QCL (FAO_AgProd_Kt_Area_Kha) are inconsistent "
    )

    ## a. All production ----
    # Meat production is more than (QCL)FAO_AgProd_Kt_Area_Kha after 2010
    # Production in FAO_AgProd_Kt_Area_Kha before 2010 was used
    FAO_AgProd_Kt_Area_Kha %>%
      filter(element == "Production") %>%
      filter(year < Min_SUA_Year) %>%
      select(c(names(FAO_SUA_Kt_2010to2019), "item")) %>%
      bind_rows(
        # For after 2010
        # Note that not all meat items came from QCL_PROD (unlike primary crops)
        # E.g., meat Eq, offals (livers chicken), etc. were from derivation or SCL
        # But all items should exist in Bal_new_all
        # And Bal_new_all is identical to QCL_PROD for primary productions
        FAO_SUA_Kt_2010to2019 %>%
          filter(element == "Production") %>%
          # ensure we at least have a complete series across time otherwise it may
          # throw off moving avg calculations
          complete(area_code = Area_Region_Map$area_code, year = pull(., year) %>% unique(), nesting(item_code, element), fill=list(value=0)) %>%
          left_join_error_no_match(SUA_item_code_map, by = c("item_code"))) %>%
      bind_rows(
        # bind fodder crops for after 2010
        FAO_AgProd_Kt_Area_Kha %>%
          filter(item_set == "QCL_COMM_CROP_PRIMARY_FODDER",
                 element == "Production") %>%
          filter(year >= Min_SUA_Year) %>%
          select(c(names(FAO_SUA_Kt_2010to2019), "item"))
      ) %>%
      # Inner join works as filter here
      # Keep subsector info for crops
      inner_join(Primary_Item_CROP %>%
                   bind_rows(Primary_Item_MEAT %>%
                               mutate(GCAM_subsector = GCAM_commodity)),
                 by = "item") %>%
      # add in iso and gcam regions ID
      left_join_error_no_match(Area_Region_Map, by = "area_code") ->
      FAO_AgProd_Kt_All

    FAO_AgProd_Kt_All %>%
      dplyr::group_by_at(vars(region, year, GCAM_commodity, element, CropMeat)) %>%
      summarise(value = sum(value), .groups = "drop") ->
      QCL_PROD_GCAM

    FAO_AgProd_Kt_All %>%
      select(-region) ->
      FAO_AgProd_Kt_All

    assertthat::assert_that(
      GCAM_AgLU_SUA_APE_1973_2019 %>%
        filter(element == "Production") %>%
        left_join_error_no_match(
          QCL_PROD_GCAM %>% filter(CropMeat != "Crop_Fodder") %>%
            select(-CropMeat) %>%
            rename(value1 = value) %>%
            complete(nesting(region, year, element), GCAM_commodity, fill = list(value1 = 0)),
          by = c("region", "GCAM_commodity", "year", "element")) %>%
        mutate(diff = abs(value1 - value)) %>%
        filter(diff > 0.0001) %>% nrow() == 0,
        msg = "Primary production from two sources
              (GCAM_AgLU_SUA_APE_1973_2019 and FAO_AgProd_Kt_Area_Kha) are inconsistent." )

    ## b. All area harvested ----

    assertthat::assert_that(
      all(Primary_Item_CROP %>% pull(item) %in%
            c(FAO_AgProd_Kt_Area_Kha %>%
                filter(item_set %in% c("QCL_COMM_CROP_PRIMARY",
                                       "QCL_COMM_CROP_PRIMARY_FODDER")) %>%
                pull(item)) ),
      msg =  "Not all required primary crop items included in FAO_AgProd_Kt_Area_Kha" )

    FAO_AgProd_Kt_Area_Kha %>%
      filter(element == "Area harvested") %>%
      select(c(names(FAO_SUA_Kt_2010to2019), "item")) %>%
      # Keep subsector info for crops
      inner_join(Primary_Item_CROP, by = "item") %>%
      # add in iso and gcam regions ID
      left_join_error_no_match(Area_Region_Map %>% select(-region), by = "area_code") ->
      FAO_AgArea_Kha_All

    #****************************----
    #Section6 Connect food items and macronutrient rates ----

    # 6.1 Separate FAO food items into GCAM food items and NEC for macronutrient ----
    # GCAM included most of the food items
    # All food item with available macronutrient info from FAOSTAT are included

    # a. Get all GCAM SUA items from the mapping by binding both source and sink items
    # about 486 items (out of 530) used in GCAM

    Mapping_SUA_PrimaryEquivalent %>%
      select(GCAM_commodity, item = source_item) %>%
      bind_rows(Mapping_SUA_PrimaryEquivalent %>%
                  select(GCAM_commodity, item = sink_item)) %>%
      distinct() %>% arrange(GCAM_commodity) ->
      SUA_Items_GCAM

    assertthat::assert_that(
      SUA_Items_GCAM %>% distinct(item) %>% nrow() == SUA_Items_GCAM %>% nrow(),
      msg = "Check duplicates in Mapping_SUA_PrimaryEquivalent SUA items"
    )

    # highly processed products or other products are not included in GCAM
    # (e.g., wine, infant food, or other nonfood items etc.)

    SUA_item_code_map %>%
      filter(!item %in% unique(SUA_Items_GCAM$item)) -> SUA_Items_NonGCAM

    # b. There are 426 FAO food items, all included in FAO_SUA_Kt_2010to2019 (530 items)
    # SUA_Items_Food includes both GCAM and NonGCAM(NEC)
    SUA_item_code_map %>%
      filter(item_code %in% unique(GCAMDATA_FAOSTAT_MacroNutrientRate_179Regs_426Items_2010to2019Mean$item_code)) %>%
      left_join(SUA_Items_GCAM, by = "item") %>%
      # For NA GCAM_commodity: not elsewhere classified (NEC)
      # So we would know % of food calories not included in GCAM commodities
      mutate(GCAM_commodity = if_else(is.na(GCAM_commodity), "NEC", GCAM_commodity)) ->
      SUA_Items_Food


    # 6.2 Get macronutrient values ----

    ### a. Get world average macronutrient ----
    # For filling in missing values

    GCAMDATA_FAOSTAT_MacroNutrientRate_179Regs_426Items_2010to2019Mean %>%
      tidyr::gather(macronutrient, macronutrient_value, calperg:proteinperc) %>%
      group_by(item, item_code, macronutrient) %>%
      summarise(macronutrient_value_World = mean(macronutrient_value), .groups = "drop") %>%
      ungroup() ->
      SUA_food_macronutrient_rate_World


    ### b. Calculate SUA food Calories consumption by joining macronutrient rates and SUA food ----

    FAO_SUA_Kt_2010to2019 %>%
      filter(element == "Food", item_code %in% SUA_Items_Food$item_code) %>%
      # ensure we at least have a complete series across time otherwise it may
      # throw off moving avg calculations
      complete(area_code = Area_Region_Map$area_code, year = pull(., year) %>% unique(), nesting(item_code, element), fill=list(value=0)) %>%
      rename(Food_Kt = value) %>%
      select(-element) %>%
      left_join_error_no_match(SUA_Items_Food, by = c("item_code")) %>%
      repeat_add_columns(
        tibble(macronutrient = c("calperg", "fatperc", "proteinperc"))) %>%
      left_join(
        GCAMDATA_FAOSTAT_MacroNutrientRate_179Regs_426Items_2010to2019Mean %>%
          tidyr::gather(macronutrient, macronutrient_value, calperg:proteinperc),
        by = c("area_code", "item_code", "item", "macronutrient")
      ) %>%
      left_join_error_no_match(SUA_food_macronutrient_rate_World,
                               by = c("item_code", "item", "macronutrient")) %>%
      mutate(macronutrient_value = if_else(is.na(macronutrient_value),
                                           macronutrient_value_World,
                                           macronutrient_value),
             # calculate total Cal, protein and fat in food
             # value was in 1000 ton or 10^ 9 g
             value = macronutrient_value * Food_Kt,
             value = if_else(macronutrient %in% c("fatperc", "proteinperc"),
                             value / 100 /1000, value)) %>% # unit from perc to Mt
      select(-macronutrient_value, -macronutrient_value_World, -Food_Kt) %>%
      # rename element with units
      mutate(macronutrient = case_when(
        macronutrient == "calperg" ~ "MKcal",
        macronutrient == "fatperc" ~ "MtFat",
        macronutrient == "proteinperc" ~ "MtProtein" )) %>%
      left_join_error_no_match(Area_Region_Map %>% select(-region), by = "area_code") ->
      FAO_Food_Macronutrient_All_2010_2019

    ### c. Get the max values of macronutrient conversion rate (per GCAM_commodity) ----
    # This will be used later as an upper bound to improve the data
    GCAMDATA_FAOSTAT_MacroNutrientRate_179Regs_426Items_2010to2019Mean %>%
      tidyr::gather(macronutrient, macronutrient_value, calperg:proteinperc) %>%
      left_join_error_no_match(SUA_Items_Food,
                               by = c("item_code", "item")) %>%
      group_by(GCAM_commodity, macronutrient) %>%
      summarise(max_macronutrient_value = max(macronutrient_value), .groups = "drop") ->
      FAO_Food_MacronutrientRate_2010_2019_MaxValue


    #****************************----
    # Produce outputs ----
    #*******************************

    GCAM_AgLU_SUA_APE_1973_2019 %>%
      add_title("GCAM_AgLU_SUA_APE_1973_2019") %>%
      add_units("kton") %>%
      add_comments("Supply utilization balance for GCAM commodities and regions in primary equivalent") %>%
      add_precursors("aglu/AGLU_ctry",
                     "common/iso_GCAM_regID",
                     "common/GCAM_region_names",
                     "aglu/FAO/GCAMDATA_FAOSTAT_SUA_195Regs_530Items_2010to2019",
                     "aglu/FAO/GCAMDATA_FAOSTAT_BiTrade_194Regs_400Items_2010to2020",
                     "aglu/FAO/Mapping_SUA_PrimaryEquivalent",
                     "aglu/FAO/SUA_item_code_map",
                     "aglu/FAO/GCAMDATA_FAOSTAT_ProdArea_195Regs_271Prod160AreaItems_1973to2020",
                     "aglu/FAO/GCAMDATA_FAOSTAT_FBSH_CB_173Regs_118Items_1973to2009",
                     "aglu/FAO/Mapping_item_FBS_GCAM") ->
      GCAM_AgLU_SUA_APE_1973_2019

    FAO_AgProd_Kt_All %>%
      add_title("FAO_AgProd_Kt_All") %>%
      add_units("1000 tonnes") %>%
      add_comments("Supply utilization balance for GCAM commodities and regions in primary equivalent") %>%
      add_precursors("aglu/AGLU_ctry",
                     "common/iso_GCAM_regID",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "aglu/FAO/FAO_an_items_PRODSTAT",
                     "aglu/FAO/Mapping_SUA_PrimaryEquivalent",
                     "aglu/FAO/GCAMDATA_FAOSTAT_ProdArea_96Regs_16FodderItems_1973to2020",
                     "aglu/FAO/GCAMDATA_FAOSTAT_ProdArea_195Regs_271Prod160AreaItems_1973to2020") ->
      FAO_AgProd_Kt_All

    FAO_AgArea_Kha_All %>%
      add_title("FAO_AgArea_Kha_All") %>%
      add_units("1000 ha") %>%
      add_comments("Harvested area") %>%
      add_precursors("aglu/AGLU_ctry",
                     "common/iso_GCAM_regID",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "aglu/FAO/FAO_an_items_PRODSTAT",
                     "aglu/FAO/Mapping_SUA_PrimaryEquivalent",
                     "aglu/FAO/GCAMDATA_FAOSTAT_ProdArea_96Regs_16FodderItems_1973to2020",
                     "aglu/FAO/GCAMDATA_FAOSTAT_ProdArea_195Regs_271Prod160AreaItems_1973to2020") ->
      FAO_AgArea_Kha_All

    FAO_Food_Macronutrient_All_2010_2019 %>%
      add_title("GCAM_AgLU_SUA_APE_1973_2019") %>%
      add_units("MKcal, MtFat, MtProtein") %>%
      add_comments("Macronutrient consumption values connected to food consumption in GCAM_AgLU_SUA_APE_1973_2019") %>%
      add_precursors("aglu/AGLU_ctry",
                     "common/iso_GCAM_regID",
                     "aglu/FAO/GCAMDATA_FAOSTAT_SUA_195Regs_530Items_2010to2019",
                     "aglu/FAO/GCAMDATA_FAOSTAT_MacroNutrientRate_179Regs_426Items_2010to2019Mean",
                     "aglu/FAO/Mapping_SUA_PrimaryEquivalent") ->
      FAO_Food_Macronutrient_All_2010_2019

    FAO_Food_MacronutrientRate_2010_2019_MaxValue %>%
      add_title("FAO_Food_MacronutrientRate_2010_2019_MaxValue") %>%
      add_units("cal per g, fat perc. , protein perc.") %>%
      add_comments("The max value of macronutrient conversion rate across region, year, and SUA items (per GCAM_commodity") %>%
      add_precursors("aglu/AGLU_ctry",
                     "common/iso_GCAM_regID",
                     "aglu/FAO/GCAMDATA_FAOSTAT_SUA_195Regs_530Items_2010to2019",
                     "aglu/FAO/GCAMDATA_FAOSTAT_MacroNutrientRate_179Regs_426Items_2010to2019Mean",
                     "aglu/FAO/Mapping_SUA_PrimaryEquivalent") ->
      FAO_Food_MacronutrientRate_2010_2019_MaxValue

    return_data(MODULE_OUTPUTS)


  } else {
    stop("Unknown command")
  }
}
