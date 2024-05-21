# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L109.ag_an_ALL_R_C_Y
#'
#' Calculate primary agricultural good and animal product mass balances, by region / commodity / year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L109.ag_ALL_Mt_R_C_Y}, \code{L109.an_ALL_Mt_R_C_Y}. The corresponding file in the
#' original data system was \code{LB109.ag_an_ALL_R_C_Y.R} (aglu level1).
#' @details This chunk combines all flow tables of GCAM agricultural commodities, calculates mass balances by
#' GCAM region, commodity and year, and adjusts global and regional net exports to remove negative other uses.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by left_join mutate pull select summarise
#' @importFrom tidyr gather spread
#' @author RC April 2017 XZ 2022
module_aglu_L109.ag_an_ALL_R_C_Y <- function(command, ...) {

  MODULE_INPUTS <-
    c("L101.ag_Food_Mt_R_C_Y",
      "L101.ag_Prod_Mt_R_C_Y",
      "L101.an_Food_Mt_R_C_Y",
      "L101.an_Prod_Mt_R_C_Y",
      "L108.ag_Feed_Mt_R_C_Y",
      "L108.ag_NetExp_Mt_R_FodderHerb_Y",
      "L122.in_Mt_R_C_Yh",
      "L101.GrossTrade_Mt_R_C_Y",
      "L101.ag_Storage_Mt_R_C_Y")

  MODULE_OUTPUTS <-
    c("L109.ag_ALL_Mt_R_C_Y",
      "L109.an_ALL_Mt_R_C_Y")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    year <- value <- GCAM_region_ID <- GCAM_commodity <- . <- flow <- Feed_Mt <-
      Prod_Mt <- NetExp_Mt <- Supply_Mt <- Food_Mt <- Biofuels_Mt <-
      OtherUses_Mt <- NegOtherUses_Mt <- OtherUses_Mt_adj <-
      GlobalOtherUses_Mt <- NetExp_Mt_adj <- NetExpAdjFrac <-
      GlobalNetExpAdj <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # Balance elements
    # c("Prod_Mt", "GrossImp_Mt", "Supply_Mt", "Food_Mt", "Feed_Mt", "Biofuels_Mt",
    #   "GrossExp_Mt", "NetExp_Mt", "OtherUses_Mt", "Closing stocks", "Opening stocks", "InterAnnualStorageLoss")

    # Storage is separated (2023.9)
    # L101.ag_Storage_Mt_R_C_Y only include commodities for storage modeling
    # commodities not included there will have zero storage; see upstream adjustments

    # Note that when other use is negative, net trade is adjusted
    # There will be concerns on primary vs. secondary trade and trade within or across aggregated regions.

    # It is important to note that in our modeling, the InterAnnualStorageLoss is linked to Closing stocks
    # The assumption is that the storage loss was in the total loss which is now separated and aggregated into Closing stocks
    # So Closing stocks in t - InterAnnualStorageLoss in t = Opening stocks in t+1
    # We will adjust Closing stocks to include InterAnnualStorageLoss

    # List of commodities in production table ----
    L101.ag_Prod_Mt_R_C_Y %>%
      pull(GCAM_commodity) %>%
      unique() -> Primary_commodities
    # List of any commodities (e.g. pasture, residue, scavenging) in feed but not in production table
    L108.ag_Feed_Mt_R_C_Y %>%
      filter(!(GCAM_commodity %in% Primary_commodities)) %>%
      pull(GCAM_commodity) %>%
      unique() -> Feed_commodities

    L101.an_Prod_Mt_R_C_Y %>%
      pull(GCAM_commodity) %>%
      unique() -> Meat_commodities

    L101.ag_Storage_Mt_R_C_Y %>%
      distinct(GCAM_commodity) %>% pull ->
      Storage_commodities



    # Part 1: Primary agricultural goods ----

    ## Combine all flow tables ----

  L101.GrossTrade_Mt_R_C_Y %>%
    filter(GCAM_commodity %in% Primary_commodities) %>%
    mutate(NetExp_Mt = GrossExp_Mt - GrossImp_Mt) %>%
    gather(flow, value, -GCAM_region_ID, -GCAM_commodity, -year) %>%
    # Name the flows in each table, and combine all tables
      bind_rows(L108.ag_NetExp_Mt_R_FodderHerb_Y %>% mutate(flow = "NetExp_Mt")) %>%
      bind_rows(mutate(L101.ag_Prod_Mt_R_C_Y, flow = "Prod_Mt")) %>%
      bind_rows(mutate(L101.ag_Food_Mt_R_C_Y, flow = "Food_Mt")) %>%
      bind_rows(mutate(L108.ag_Feed_Mt_R_C_Y, flow = "Feed_Mt")) %>%
      bind_rows(mutate(L122.in_Mt_R_C_Yh, flow = "Biofuels_Mt")) %>%
      bind_rows(L101.ag_Storage_Mt_R_C_Y %>% rename(flow = element) %>%
                  filter(GCAM_commodity %in% Primary_commodities)) %>%
      # in case L101.ag_Storage_Mt_R_C_Y is empty
      bind_rows(tibble(`Opening stocks` = numeric(),
                       `Closing stocks` = numeric(),
                       `InterAnnualStockLoss` = numeric())) %>%
      # Get all combinations of each GCAM_commodity and flow, by spreading to wide format
      spread(flow, value) %>%
      # adjust for feedherb
      mutate(GrossExp_Mt = if_else(is.na(GrossExp_Mt) & NetExp_Mt > 0, NetExp_Mt, GrossExp_Mt),
             GrossExp_Mt = if_else(is.na(GrossExp_Mt) & NetExp_Mt <= 0, 0, GrossExp_Mt),
             GrossImp_Mt = GrossExp_Mt - NetExp_Mt) %>%
      filter(year %in% aglu.AGLU_HISTORICAL_YEARS) %>%
      # Set missing values in the complete combinations to zero
      dplyr::mutate_if(is.numeric, list(~ replace(., is.na(.), 0))) %>%
      # For any feed commodities (e.g. pasture, residue, scavenging) that are not reported in production or trade table,
      # assume all production are domestic, and set production = feed
      mutate(Prod_Mt = if_else(GCAM_commodity %in% Feed_commodities, Feed_Mt, Prod_Mt),
             # Calculate the domestic supply (used in zaglu_L202)
             Supply_Mt = `Opening stocks` + Prod_Mt - NetExp_Mt,
             ## Calculate other uses ----
             OtherUses_Mt = Supply_Mt - Food_Mt - Feed_Mt - Biofuels_Mt - `Closing stocks` - InterAnnualStockLoss,
             `Closing stocks` = `Closing stocks` +  InterAnnualStockLoss)  ->
    L109.ag_ALL_Mt_R_C_Y

  ## Adjust negative crop feed use using other use ----
  # The negative feed use, if exist, came from connecting feed crops to feedcake or ddgd (bioenergy) in LA108
  if(any(L109.ag_ALL_Mt_R_C_Y$Feed_Mt < 0)){
    L109.ag_ALL_Mt_R_C_Y %>% filter(Feed_Mt <0) %>%
      mutate(OtherUses_Mt = OtherUses_Mt + Feed_Mt,
             Feed_Mt = 0) %>%
      bind_rows(L109.ag_ALL_Mt_R_C_Y %>% filter(Feed_Mt >= 0) ) ->
      L109.ag_ALL_Mt_R_C_Y }

  ## Adjust negative other uses using trade or food ----
  # Assign negative other net uses to imports, and adjust global trade to maintain balances
  # Changes in global net exports are apportioned among regions with positive other uses, according to regional shares

  if(any(L109.ag_ALL_Mt_R_C_Y$OtherUses_Mt < 0)){
  # Filter commodities that may be imbalanced
  L109.ag_ALL_Mt_R_C_Y %>%
    filter(GCAM_commodity %in% Primary_commodities) %>%
    filter(GCAM_commodity != "FodderHerb") %>%
    mutate(negOther = if_else(OtherUses_Mt < 0, "Neg", "Pos") ) ->
    L109.ag_ALL_Mt_R_C_Y_1

    L109.ag_ALL_Mt_R_C_Y_1 %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      group_by(GCAM_commodity, year, negOther) %>%
      summarise(value = sum(OtherUses_Mt), .groups = "drop") %>%
      spread(negOther, value) %>%
      filter(!is.na(Neg), Neg + Pos < 0) %>% nrow() == 0 ->
      AG_OTHERUSE_WARNING

    if (AG_OTHERUSE_WARNING == F) {
      warning("Negative other use in model base years.
      Other use in ag crop commodities may require a case-by-case adjustment (in food) since total global other use is negative.
      Please check food consumption adjustments.") }

  ### Ship negative otheruse to other regions with positives ----
  # positive regions will be scaled down simply
  L109.ag_ALL_Mt_R_C_Y_1 %>%
    group_by(GCAM_commodity, year, negOther) %>%
    # world processed by neg or pos
    summarise(OtherUses_Mt = sum(OtherUses_Mt, na.rm = T)) %>%
    ungroup() %>%
    spread(negOther, OtherUses_Mt) %>%
    mutate(World_pos_scaler = -Neg/Pos) %>%
    select(-Neg, -Pos)->
    Pos_OtherUse_scaler

  # NA scalers mean no negative other uses in all region for the item
  L109.ag_ALL_Mt_R_C_Y_1 %>%
    left_join(Pos_OtherUse_scaler, by = c("year", "GCAM_commodity")) %>%
    filter(is.na(World_pos_scaler)) %>%
    select(-negOther, -World_pos_scaler) %>%
    # all positive other use bind ones with negative values
    bind_rows(
      L109.ag_ALL_Mt_R_C_Y_1 %>%
        left_join(Pos_OtherUse_scaler, by = c("year", "GCAM_commodity")) %>%
        filter(!is.na(World_pos_scaler)) %>%
        # Adjust negative OtherUse first by moving to import
        # set neg OtherUse to zero
        mutate(GrossImp_Mt = if_else(negOther == "Neg", GrossImp_Mt - OtherUses_Mt, GrossImp_Mt),
               OtherUses_Mt = if_else(negOther == "Neg", 0, OtherUses_Mt),
               # Adjust positive OtherUse by scaling and moving to export
               # reduce positive scaler
               GrossExp_Mt = if_else(negOther == "Pos", GrossExp_Mt + OtherUses_Mt * World_pos_scaler, GrossExp_Mt),
               OtherUses_Mt = if_else(negOther == "Pos", OtherUses_Mt - OtherUses_Mt * World_pos_scaler, OtherUses_Mt)) %>%
        select(-negOther, -World_pos_scaler) %>%
        mutate(NetExp_Mt = GrossExp_Mt - GrossImp_Mt,
               Supply_Mt = `Opening stocks` + Prod_Mt - NetExp_Mt)
    ) ->
    L109.ag_ALL_Mt_R_C_Y_2

  # Bind commodities that were balanced
  L109.ag_ALL_Mt_R_C_Y_2 %>%
  bind_rows(
    L109.ag_ALL_Mt_R_C_Y %>%
    filter(GCAM_commodity %in% c("FodderHerb", Feed_commodities))
    ) %>%
    gather(element, value, -GCAM_region_ID, -GCAM_commodity, -year) %>%
    mutate(value = round(value, aglu.DIGITS_CALOUTPUT))->
    L109.ag_ALL_Mt_R_C_Y_3

  L109.ag_ALL_Mt_R_C_Y_3 %>%
    spread(element, value) ->
    L109.ag_ALL_Mt_R_C_Y

  ## Check again in model base year and adjust food consumption if needed ----
  if(any(filter(L109.ag_ALL_Mt_R_C_Y, year %in% MODEL_BASE_YEARS)$OtherUses_Mt < 0)){

    L109.ag_ALL_Mt_R_C_Y %>%
      filter(year %in% MODEL_BASE_YEARS, OtherUses_Mt <0) %>%
      mutate(OtherUse_Food_Share = - OtherUses_Mt / Food_Mt) ->
      CheckOtherUsevsFood

    L109.ag_ALL_Mt_R_C_Y %>%
      mutate(Food_Mt = if_else(year %in% MODEL_BASE_YEARS & OtherUses_Mt <0,
                               Food_Mt + OtherUses_Mt, Food_Mt)) %>%
      mutate(OtherUses_Mt = if_else(year %in% MODEL_BASE_YEARS & OtherUses_Mt <0,
                                    0, OtherUses_Mt)) ->
      L109.ag_ALL_Mt_R_C_Y
  }

  rm(L109.ag_ALL_Mt_R_C_Y_1,
     L109.ag_ALL_Mt_R_C_Y_2,
     L109.ag_ALL_Mt_R_C_Y_3)
  }

    # After these adjustments, crops may still have negative other uses if the bottom-up biofuel estimates exceed the
    # domestic supply minus known (and fixed) food and feed quantities. This needs to be addressed on a case-by-case
    # basis, but failure to address the issue here will result in negative calibration values being read to GCAM, and
    # model solution failure.
    if(any(filter(L109.ag_ALL_Mt_R_C_Y, year %in% MODEL_BASE_YEARS)$OtherUses_Mt < 0)){
      stop("Still negative other uses in model base year, possibly due to biofuel crop requirements exceeding available domestic supply")
    }


    ## Address Zero current consumption issue for Storage_commodities ----


    # For South Korea sugar crop, Argentina palm had data problem
    # Adding dummy other use as 1% of stock in historical MODEL_BASE_YEARS, when no current consumption
    # Stocks is adjusted

    L109.ag_ALL_Mt_R_C_Y %>%
      mutate(CurrentConsumption = Biofuels_Mt + Feed_Mt + Food_Mt + OtherUses_Mt) ->
      L109.ag_ALL_Mt_R_C_Y_4

    # Adjustment is very simple since we do not have balance constraint of stock carryover
    # in historical years due to losses and gaps

    L109.ag_ALL_Mt_R_C_Y_4 %>%
      filter(year %in% MODEL_BASE_YEARS[MODEL_BASE_YEARS != max(MODEL_BASE_YEARS)],
             `Closing stocks` > 0 & CurrentConsumption == 0,
             GCAM_commodity %in% Storage_commodities) %>%
      mutate(OtherUses_Mt = 0.01 * `Closing stocks`,
             `Closing stocks` = `Closing stocks` - OtherUses_Mt,
             CurrentConsumption = Biofuels_Mt + Feed_Mt + Food_Mt + OtherUses_Mt) ->
      L109.ag_ALL_Mt_R_C_Y_5

    # Bind rows to get full table
    L109.ag_ALL_Mt_R_C_Y_4 %>%
      filter(!(year %in% MODEL_BASE_YEARS[MODEL_BASE_YEARS != max(MODEL_BASE_YEARS)] &
             `Closing stocks` > 0 & CurrentConsumption == 0 &
               GCAM_commodity %in% Storage_commodities)) %>%
      bind_rows(
        L109.ag_ALL_Mt_R_C_Y_5) ->
      L109.ag_ALL_Mt_R_C_Y

    ## Address Zero closing storage issue (potential) for Storage_commodities ----

    # adding dummy tiny storage as 1% current consumption when storage is zero in base years
    Adj_Storage_Share <- 0.01

    L109.ag_ALL_Mt_R_C_Y %>%
      filter(year %in% MODEL_BASE_YEARS, `Closing stocks` == 0,
             GCAM_commodity %in% Storage_commodities) %>%
      mutate(`Closing stocks` = `Closing stocks` + Adj_Storage_Share * CurrentConsumption,
             `Opening stocks` = `Opening stocks` + Adj_Storage_Share * CurrentConsumption) ->
      L109.ag_ALL_Mt_R_C_Y_6

    # Bind rows to get full table
    L109.ag_ALL_Mt_R_C_Y %>%
      filter(!(year %in% MODEL_BASE_YEARS & `Closing stocks` == 0 &
                 GCAM_commodity %in% Storage_commodities)) %>%
      bind_rows(L109.ag_ALL_Mt_R_C_Y_6) %>%
      select(-CurrentConsumption ) ->
      L109.ag_ALL_Mt_R_C_Y


    ## Addressing zero OtherUse (1975) ----

    # 1975 model warnings due to zero Feed + OtherUse
    # There are 7 places with zero Feed + OtherUse in min(MODEL_BASE_YEARS)
    ## We add tiny value in OtherUse by moving closing storage to there

    L109.ag_ALL_Mt_R_C_Y %>%
      filter(year %in% min(MODEL_BASE_YEARS) & `Closing stocks` > 0 & OtherUses_Mt == 0 & Feed_Mt == 0) %>%
      mutate(StockMove = pmin(0.01, `Closing stocks` * 0.1), # lower one in 1% storage or 0.01 Mt
             # Maintain loss rates
             LossMove = InterAnnualStockLoss / `Closing stocks` * StockMove,
             OtherUses_Mt = OtherUses_Mt + StockMove + LossMove,
             `Closing stocks` = `Closing stocks` - StockMove,
             InterAnnualStockLoss = InterAnnualStockLoss - LossMove) %>%
      select(-LossMove, -StockMove) ->
      L109.ag_ALL_Mt_R_C_Y_7

    # Bind others back
    L109.ag_ALL_Mt_R_C_Y %>%
      anti_join(L109.ag_ALL_Mt_R_C_Y_7,
                by = c("GCAM_commodity", "year", "GCAM_region_ID")) %>%
      bind_rows(L109.ag_ALL_Mt_R_C_Y_7) %>%
      arrange(year, GCAM_commodity, GCAM_region_ID) ->
      L109.ag_ALL_Mt_R_C_Y


    # Part 2: Animal commodities ----

    ## Combine all flow tables ----

    # Name the flows in each table
    L101.GrossTrade_Mt_R_C_Y %>%
      filter(GCAM_commodity %in% Meat_commodities) %>%
      mutate(NetExp_Mt = GrossExp_Mt - GrossImp_Mt) %>%
      gather(flow, value, -GCAM_region_ID, -GCAM_commodity, -year) %>%
      bind_rows(mutate(L101.an_Prod_Mt_R_C_Y, flow = "Prod_Mt")) %>%
      bind_rows(mutate(L101.an_Food_Mt_R_C_Y, flow = "Food_Mt")) %>%
      bind_rows(L101.ag_Storage_Mt_R_C_Y %>% rename(flow = element) %>%
                  filter(GCAM_commodity %in% Meat_commodities)) %>%
      spread(flow, value) %>%
      bind_rows(tibble(`Opening stocks` = numeric(),
                       `Closing stocks` = numeric(),
                       `InterAnnualStockLoss` = numeric())) %>%
      filter(year %in% aglu.AGLU_HISTORICAL_YEARS) %>%
      # Set missing values in the complete combinations to zero
      dplyr::mutate_if(is.numeric, list(~ replace(., is.na(.), 0))) %>%
      mutate(# Calculate the domestic supply
             Supply_Mt = `Opening stocks` + Prod_Mt - NetExp_Mt,
             ## Calculate other uses ----
             OtherUses_Mt = Supply_Mt - Food_Mt - `Closing stocks` - InterAnnualStockLoss,
             `Closing stocks` = `Closing stocks` +  InterAnnualStockLoss)  ->
      L109.an_ALL_Mt_R_C_Y


    if(any(L109.an_ALL_Mt_R_C_Y$OtherUses_Mt < 0)){
      # Filter commodities that may be imbalanced
      L109.an_ALL_Mt_R_C_Y %>%
        mutate(negOther = if_else(OtherUses_Mt < 0, "Neg", "Pos") ) ->
        L109.an_ALL_Mt_R_C_Y_1

      L109.an_ALL_Mt_R_C_Y_1 %>%
        filter(year %in% MODEL_BASE_YEARS) %>%
        group_by(GCAM_commodity, year, negOther) %>%
        summarise(value = sum(OtherUses_Mt), .groups = "drop") %>%
        spread(negOther, value) %>%
        filter(!is.na(Neg), Neg + Pos < 0) %>% nrow() == 0 ->
        An_OTHERUSE_WARNING

      # comment this out since adjustments are added later
      # if (An_OTHERUSE_WARNING == F) {
      #  warning("Negative other use in model base years.Other use in meat commodities may require a case-by-case adjustment (in food) since total global other use is negative. Please check food consumption adjustments.")
      #   }

      # Ship negative otheruse to other regions with positives
      # positive regions will be scaled down simplely
      L109.an_ALL_Mt_R_C_Y_1 %>%
        group_by(GCAM_commodity, year, negOther) %>%
        # world processed by neg or pos
        summarise(OtherUses_Mt = sum(OtherUses_Mt, na.rm = T)) %>%
        ungroup() %>%
        spread(negOther, OtherUses_Mt) %>%
        mutate(World_pos_scaler = -Neg/Pos) %>%
        select(-Neg, -Pos)->
        Pos_OtherUse_scaler

      # NA scalers mean no negative other uses in all region for the item
      L109.an_ALL_Mt_R_C_Y_1 %>%
        left_join(Pos_OtherUse_scaler, by = c("year", "GCAM_commodity")) %>%
        filter(is.na(World_pos_scaler)) %>%
        select(-negOther, -World_pos_scaler) %>%
        # all positive other use bind ones with negative values
        bind_rows(
          L109.an_ALL_Mt_R_C_Y_1 %>%
            left_join(Pos_OtherUse_scaler, by = c("year", "GCAM_commodity")) %>%
            filter(!is.na(World_pos_scaler)) %>%
            # Adjust negative OtherUse first by moving to import
            # set neg OtherUse to zero
            mutate(GrossImp_Mt = if_else(negOther == "Neg", GrossImp_Mt - OtherUses_Mt, GrossImp_Mt),
                   OtherUses_Mt = if_else(negOther == "Neg", 0, OtherUses_Mt),
                   # Adjust positive OtherUse by scaling and moving to export
                   # reduce positive scaler
                   GrossExp_Mt = if_else(negOther == "Pos", GrossExp_Mt + OtherUses_Mt * World_pos_scaler, GrossExp_Mt),
                   OtherUses_Mt = if_else(negOther == "Pos", OtherUses_Mt - OtherUses_Mt * World_pos_scaler, OtherUses_Mt)) %>%
            select(-negOther, -World_pos_scaler) %>%
            mutate(NetExp_Mt = GrossExp_Mt - GrossImp_Mt,
                   Supply_Mt = Prod_Mt - NetExp_Mt)
        ) ->
        L109.an_ALL_Mt_R_C_Y_2

      # round to aglu.DIGITS_CALOUTPUT
      L109.an_ALL_Mt_R_C_Y_2 %>%
        gather(element, value, -GCAM_region_ID, -GCAM_commodity, -year) %>%
        mutate(value = round(value, aglu.DIGITS_CALOUTPUT))->
        L109.an_ALL_Mt_R_C_Y_3

      L109.an_ALL_Mt_R_C_Y <- L109.an_ALL_Mt_R_C_Y_3 %>% spread(element, value)

      # Check again in model base year and adjust food consumption if needed.
      if(any(filter(L109.an_ALL_Mt_R_C_Y, year %in% MODEL_BASE_YEARS)$OtherUses_Mt < 0)){

        L109.an_ALL_Mt_R_C_Y %>%
          filter(year %in% MODEL_BASE_YEARS, OtherUses_Mt <0) %>%
          mutate(OtherUse_Food_Share = - OtherUses_Mt / Food_Mt) ->
          CheckOtherUsevsFood
        # Note that only 2015 SheepGoat negative OtherUse was moved to food, which was ~3%

        L109.an_ALL_Mt_R_C_Y %>%
          mutate(Food_Mt = if_else(year %in% MODEL_BASE_YEARS & OtherUses_Mt <0,
                                   Food_Mt + OtherUses_Mt, Food_Mt)) %>%
          mutate(OtherUses_Mt = if_else(year %in% MODEL_BASE_YEARS & OtherUses_Mt <0,
                                        0, OtherUses_Mt)) ->
          L109.an_ALL_Mt_R_C_Y
      }

      rm(L109.an_ALL_Mt_R_C_Y_1,
         L109.an_ALL_Mt_R_C_Y_2,
         L109.an_ALL_Mt_R_C_Y_3)
    }

    if(any(filter(L109.an_ALL_Mt_R_C_Y, year %in% MODEL_BASE_YEARS)$OtherUses_Mt < 0)){
      stop("Still negative other uses in meat commodities.")
    }


    # These adjustments are added for animal products
    # even though storage focuses on crops for now in modeling
    ## Address Zero current consumption issue for Storage_commodities ----

    # For South Korea sugar crop, Argentina palm had data problem
    # Adding dummy other use as 1% of stock in historical MODEL_BASE_YEARS, when no current consumption
    # Stocks is adjusted

    L109.an_ALL_Mt_R_C_Y %>%
      mutate(CurrentConsumption = Food_Mt + OtherUses_Mt) ->
      L109.an_ALL_Mt_R_C_Y_4

    # Adjustment is very simple since we do not have balance constraint of stock carryover
    # in historical years due to losses and gaps
    # Pakistain Pork will have issue

    L109.an_ALL_Mt_R_C_Y_4 %>%
      filter(year %in% MODEL_BASE_YEARS[MODEL_BASE_YEARS != max(MODEL_BASE_YEARS)],
             `Closing stocks` > 0 & CurrentConsumption == 0,
             GCAM_commodity %in% Storage_commodities) %>%
      mutate(OtherUses_Mt = 0.01 * `Closing stocks`,
             `Closing stocks` = `Closing stocks` - OtherUses_Mt,
             CurrentConsumption = Food_Mt + OtherUses_Mt) ->
      L109.an_ALL_Mt_R_C_Y_5

    # Bind rows to get full table
    L109.an_ALL_Mt_R_C_Y_4 %>%
      filter(!(year %in% MODEL_BASE_YEARS[MODEL_BASE_YEARS != max(MODEL_BASE_YEARS)] &
               `Closing stocks` > 0 & CurrentConsumption == 0 &
                 GCAM_commodity %in% Storage_commodities)) %>%
      bind_rows(
        L109.an_ALL_Mt_R_C_Y_5) ->
      L109.an_ALL_Mt_R_C_Y

    ## Address Zero closing storage issue (potential) for Storage_commodities ----

    # adding dummy tiny storage
    L109.an_ALL_Mt_R_C_Y %>%
      filter(year %in% MODEL_BASE_YEARS, `Closing stocks` == 0,
             GCAM_commodity %in% Storage_commodities) %>%
      mutate(`Closing stocks` = `Closing stocks` + 0.01 * CurrentConsumption,
             `Opening stocks` = `Opening stocks` + 0.01 * CurrentConsumption) ->
      L109.an_ALL_Mt_R_C_Y_6

    # Bind rows to get full table
    L109.an_ALL_Mt_R_C_Y %>%
      filter(!(year %in% MODEL_BASE_YEARS & `Closing stocks` == 0 &
                 GCAM_commodity %in% Storage_commodities)) %>%
      bind_rows(L109.an_ALL_Mt_R_C_Y_6) %>%
      select(-CurrentConsumption ) ->
      L109.an_ALL_Mt_R_C_Y



    # Part 3 Adjust self-trade to ensure export < production ----
    # this was an assumption in GCAM cpp
    # the assumption could be strong e.g., US does not product OilPalm but could export OilPalm product
    L109.ag_ALL_Mt_R_C_Y %>%
      # reduce import and export both by the same (GrossExp_Mt - Prod_Mt)
      mutate(GrossImp_Mt = if_else(GrossExp_Mt > Prod_Mt,
                                   GrossImp_Mt - (GrossExp_Mt - Prod_Mt),GrossImp_Mt),
             GrossExp_Mt = if_else(GrossExp_Mt > Prod_Mt,
                                   Prod_Mt, GrossExp_Mt)) ->
      L109.ag_ALL_Mt_R_C_Y

    L109.an_ALL_Mt_R_C_Y %>%
      # reduce import and export both by the same (GrossExp_Mt - Prod_Mt)
      mutate(GrossImp_Mt = if_else(GrossExp_Mt > Prod_Mt,
                                   GrossImp_Mt - (GrossExp_Mt - Prod_Mt),GrossImp_Mt),
             GrossExp_Mt = if_else(GrossExp_Mt > Prod_Mt,
                                   Prod_Mt, GrossExp_Mt)) ->
      L109.an_ALL_Mt_R_C_Y

    # Final thought:
    # including storage improves the other use data
    # in the case of negative values adj. with storage smaller adj. is needed
    # but cannot be avoided due to other reasons (residuals)
    # And note that trade could have been adjusted to ship negative other use around
    # so even initial trade data could be slightly different!

    # Produce outputs
    L109.ag_ALL_Mt_R_C_Y %>%
      add_title("Primary agricultural good mass balances, by region / commodity / year.") %>%
      add_units("Mt") %>%
      add_comments("Calculate primary agricultural good mass balances by GCAM region, commodity and year") %>%
      add_comments("Adjusts global and regional net exports to remove net negative other uses") %>%
      add_legacy_name("L109.ag_ALL_Mt_R_C_Y") %>%
      add_precursors("L101.ag_Food_Mt_R_C_Y",
                     "L101.ag_Prod_Mt_R_C_Y",
                     "L108.ag_Feed_Mt_R_C_Y",
                     "L108.ag_NetExp_Mt_R_FodderHerb_Y",
                     "L122.in_Mt_R_C_Yh",
                     "L101.GrossTrade_Mt_R_C_Y",
                     "L101.ag_Storage_Mt_R_C_Y"
                     ) ->
      L109.ag_ALL_Mt_R_C_Y

    L109.an_ALL_Mt_R_C_Y %>%
      add_title("Animal product mass balances, by region / commodity / year.") %>%
      add_units("Mt") %>%
      add_comments("Calculate animal product mass balances by GCAM region, commodity and year") %>%
      add_comments("Adjusts global and regional net exports to remove net negative other uses") %>%
      add_legacy_name("L109.an_ALL_Mt_R_C_Y") %>%
      add_precursors("L101.an_Food_Mt_R_C_Y",
                     "L101.an_Prod_Mt_R_C_Y",
                     "L101.GrossTrade_Mt_R_C_Y",
                     "L101.ag_Storage_Mt_R_C_Y") ->
      L109.an_ALL_Mt_R_C_Y

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
