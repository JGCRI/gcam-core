#' module_aglu_LB133.ag_Costs_USA_C_2005
#'
#' This module computes costs for each GCAM commodity (used by each region-GLU) using USDA cost
#' information for USA when available. For commodities without USDA cost data, the average profit
#' among USDA commodities and LDS harvested area and production information are used to compute
#' cost.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L133.ag_Cost_75USDkg_C}. The corresponding file in the
#' original data system was \code{LB133.ag_Costs_USA_C_2005.R} (aglu level1).
#' @details USDA cost information is scaled to the level of GCAM commodity using LDS harvested area and
#' production data to compute weighting factors for aggregation and convert cost from USD/square meter
#' to the more useful USD/kg. Cost information is converted to the GCAM base of 1975 USD before aggregation.
#' For commodities without USDA cost information, the average profit among USDA commodities is combined
#' with LDS harvested area and production data to calculate cost.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author ACS May 2017
module_aglu_LB133.ag_Costs_USA_C_2005 <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "aglu/USDA_crops",
             FILE = "aglu/USDA_item_cost",
             FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
             FILE = "aglu/USDA_cost_data",
             "L100.LDS_ag_HA_ha",
             "L100.LDS_ag_prod_t",
             "L132.ag_an_For_Prices"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L133.ag_Cost_75USDkg_C"))
  } else if(command == driver.MAKE) {

    year <- value <- Crop <- Item <- Unit <- cost_type <- GCAM_commodity <-
        GTAP_crop <- value1 <- cost_75USDm2 <- iso <- HA_bm2 <-
        Expenditures_bil75USD <- Prod_Mt <- Cost_75USDm2 <- Yield_kgm2 <-
        unit <- Cost_75USDkg <- calPrice <- Revenue_bil75USD <- HA_ha <-
        Prod_t <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    USDA_crops <- get_data(all_data, "aglu/USDA_crops")
    USDA_item_cost <- get_data(all_data, "aglu/USDA_item_cost")
    FAO_ag_items_PRODSTAT <- get_data(all_data, "aglu/FAO/FAO_ag_items_PRODSTAT")
    USDA_cost_data <- get_data(all_data, "aglu/USDA_cost_data")
    L100.LDS_ag_HA_ha <- get_data(all_data, "L100.LDS_ag_HA_ha")
    L100.LDS_ag_prod_t <- get_data(all_data, "L100.LDS_ag_prod_t")
    L132.ag_an_For_Prices <- get_data(all_data, "L132.ag_an_For_Prices")

    # convert USDA_cost_data to long form:
    USDA_cost_data %>%
      gather(year, value, -Crop, -Item, -Unit) %>%
      # force year to integer
      mutate(year = as.integer(year)) ->
      USDA_cost_data

    # 2. Perform computations

    # Lines 35-67 in original file
    # Get the cost in 1975 dollars for each GCAM commodity - Item combination covered in the USDA_data_cost table.
    # First, Take USDA item cost data from input table USDA_cost_data, and add GCAM commodity and GTAP crop mapping info
    # from the USDA crop mapping input table, USDA_crops.
    # Then add cost type (variable or na) from the USDA_item_cost input table.
    # Next, select only variable price data, only in MODEL_COST_YEARS = 2001:2005 by default.
    # Finally, convert each cost from the given nominal year dollars to 1975 dollars, average across MODEL_COST_YEARS,
    # and convert from dollars/acre to dollars/m2.
    #
    # Take USDA item cost data:
    USDA_cost_data %>%
      # join in the GCAM and GTAP mapping information:
      left_join_error_no_match(USDA_crops, by = c("Crop" = "USDA_crop")) %>%
      # then join cost_type information:
      left_join_error_no_match(USDA_item_cost, by = c("Item")) %>%
      # select only variable cost data and only MODEL_COST_YEARS
      filter(cost_type == "variable", year %in% MODEL_COST_YEARS) %>%
      # select just identifying information of interest:
      select(GCAM_commodity, GTAP_crop, Item, year, value) %>%
      # Convert costs from the given nominal dollars to 1975 dollars:
      # (have to group by years and store in a dummy value1 column to get the conversion correct)
      group_by(GCAM_commodity, GTAP_crop, Item, year, value) %>%
      mutate(value1 = value * gdp_deflator(1975, base_year = year)) %>%
      # ungroup, drop value and rename value1 to value to get the correct table of 1975 dollars/acre:
      ungroup() %>% select(-value) %>% rename(value = value1) %>%
      # Calculate the average across years for each commodity-item combination, and convert to dollars/m2:
      #   (In particular, the average is computed across non-NA years. So if only 3 of 5 years are non-NA,
      #   the average is over those 3 numbers, not over 5 with 0's filled in for the NA's.)
      group_by(GCAM_commodity, GTAP_crop, Item) %>%
      mutate(cost_75USDm2 = mean(value, na.rm = TRUE) * CONV_M2_ACR) %>%
      # if all years in MODEL_COST_YEARS have NA values, the above calculation will give NaN for the
      # mean value. Overwrite this to 0:
      # old comment: (indicates a variable cost not disaggregated in the target years)
      replace_na(list(cost_75USDm2 = 0)) ->
      # store in a table of costs in 1975 dollars per square meter, by commodity and year:
      L133.ag_Cost_75USDm2_Cusda_Yusda


    # Lines 69-72 in original file
    # Process cost data from the level of USDA commodity and years to the level of USDA commodity =
    # GCAM_commodity - GTAP_crop combination.
    # This is an intermediate calculation used for multiple other calculations.
    #
    # To calculate this intermediate step:
    # Aggregate variable cost components by summing cost_75USDm2 in L133.ag_Cost_75USDm2_Cusda_Yusda
    # to the level of GCAM commodity and GTAP Crop.
    #
    # Take the table L133.ag_Cost_75USDm2_Cusda_Yusda:
    L133.ag_Cost_75USDm2_Cusda_Yusda %>%
      # drop information related to year and keep only unique resulting rows:
      select(-year, -value) %>% unique() %>%
      # aggregate average cost to the level of GCAM_commodity and GTAP_crop:
      group_by(GCAM_commodity, GTAP_crop) %>%
      summarise(cost_75USDm2 = sum(cost_75USDm2)) ->
      # store in a table of cost in 1975 dollars/m2 by commodity:
      L133.ag_Cost_75USDm2_Cusda


    # Lines 69-95 in original file.
    # Cost in 1975 USD/kg is calculated for each GCAM_commodity.
    #
    # 1. LDS Harvested area and Production data are aggregated to different levels.
    #
    # 2. Aggregated LDS harvested area information is joined to the USDA commodity = GCAM_commodity-GTAP_crop combination
    # cost data, L133.ag_Cost_75USDm2_Cudsa.
    # This harvested area information is then used to compute:
    # aggregated expenditures at the GCAM_commodity level,
    # aggregated harvested area at the GCAM_commodity level,
    # and aggregated Cost = aggregated expenditures / aggregated harvested area at the GCAM_commodity level.
    #
    # 3. Aggregated LDS agricultural production information is joined to the  commodity cost data calculated in 2.
    # This production information is then used to compute Yield = kg/m^2 for each commodity.
    # This yield information is then used to convert Cost/m^2 to Cost/kg.

    # 1.LDS Harvested area and Production data are aggregated to different levels.
    # Prepare LDS harvested area data, input L100.LDS_ag_HA_ha, by subsetting to USA only data and aggregating
    # to the level of GTAP_crop:
    L100.LDS_ag_HA_ha %>%
      # only consider USA data and the GTAP crops present in above USDA data:
      filter(iso == "usa", GTAP_crop %in% L133.ag_Cost_75USDm2_Cusda$GTAP_crop) %>%
      # aggregate to the level of GTAP crop:
      group_by(GTAP_crop) %>%
      summarise(value = sum(value)) ->
      # store in a table of USA harvested area data:
      L133.LDS_ag_HA_ha_USA
    #
    # Prepare LDS ag production data, input L100.LDS_ag_prod_t, by subsetting to USA only data and aggregating
    # to the level of GCAM_commodity:
    L100.LDS_ag_prod_t %>%
      # only consider USA data and the GTAP crops present in above USDA data:
      filter(iso == "usa", GTAP_crop %in% L133.ag_Cost_75USDm2_Cusda$GTAP_crop) %>%
      # add GCAM_commodity information
      left_join_error_no_match(USDA_crops, by = c("GTAP_crop")) %>%
      # convert data from t to Mt:
      mutate(value = value * CONV_TON_MEGATON) %>%
      # aggregate to the level of GCAM_commodity:
      group_by(GCAM_commodity) %>%
      summarise(value = sum(value)) ->
      # store in a table of LDS production for the USA by commodity in Mt:
      L133.ag_Prod_Mt_USA_C

    # 2. and 3. Calculate Cost in USD/kg for each Commodity:
    #
    # 2.
    # Use the LDS data to calculate harvested area and expenditure  = cost * harvested area for each GCAM_commodity-GTAP_crop
    # combination. Then expenditure and harvested area are aggregated over GTAP_crops to get aggregate expenditure and
    # aggregate harvested area for each GCAM_commodity.
    # Finally, aggregated cost is calculated for each GCAM_commodity by aggregate cost = aggregate expenditure/ aggregate HA.
    #
    # 3.
    # Aggregate LDS agricultural production information is joined to the commodity cost data, L133.ag_Cost_75USDm2_C.
    # This production information is then used to compute Yield = kg/m^2 for each commodity.
    # This yield information is then used to convert Cost/m^2 to Cost/kg.
    #
    # Take the USDA commodity cost data:
    L133.ag_Cost_75USDm2_Cusda %>%
      #
      # 2.
      #
      # add harvested area in billion square meters for each GTAP crop by joining the aggregate LDS HA data,
      # L133.LDS_ag_HA_ha_USA, and converting the value from hectares to billion square meters:
      left_join_error_no_match(L133.LDS_ag_HA_ha_USA, by = c("GTAP_crop")) %>%
      rename(HA_bm2 = value) %>% mutate(HA_bm2 = HA_bm2 * CONV_HA_BM2) %>%
      # Calculate the total expenditure in billion 1975 dollars for each USDA commodity = GCAM_commodity-GTAP_crop combo
      # by Expenditures in bil75USD = Cost in 1975 dollars/square meter   * Harvested area in billion square meters
      #    Expenditures_bil75USD    = cost_75USDm2                        * HA_bm2:
      mutate(Expenditures_bil75USD = cost_75USDm2 * HA_bm2) %>%
      # aggregate over GTAP_crops to get Expenditures and harvested area at the level of GCAM_commodity:
      group_by(GCAM_commodity) %>%
      summarise(HA_bm2 = sum(HA_bm2), Expenditures_bil75USD = sum(Expenditures_bil75USD)) %>%
      # Calculate Cost for each GCAM commodity accounting for this aggregation;
      # aggregated cost = aggregated expenditures/aggregated harvested area:
      mutate(Cost_75USDm2 = Expenditures_bil75USD / HA_bm2) %>%
      #
      # 3.
      #
      # join the production data by Commodity:
      left_join_error_no_match(L133.ag_Prod_Mt_USA_C, by = c("GCAM_commodity")) %>%
      rename(Prod_Mt = value) %>%
      # use the Aggregated Production and Aggregated Harvested Area information to compute
      # aggregate yield = production / harvested area for each GCAM_commdity in kg/m^2:
      mutate(Yield_kgm2 = Prod_Mt / HA_bm2) %>%
      # Calculate Cost in USD/kg by using yield to convert from Cost in USD/m2;
      # Cost_75USDkg = Cost_75USDm2 / Yield_kgm2
      # USD/kg       = (USD/m2)     / (kg/m2):
      mutate(Cost_75USDkg = Cost_75USDm2 / Yield_kgm2) ->
      # store in the table of costs in USD/m2 by GCAM commodity:
      L133.ag_Cost_75USDm2_C


    # Lines 98-106 in original file
    # Agricultural Prices in table L132.ag_an_For_Prices are joined to the Commodity Cost table, L133.ag_Cost_75USDm2_C,
    # and used to ensure that Costs in 1975USD/kg don't lead to profits below a minimum profit margin, MIN_PROFIT_MARGIN.
    # Finally, revenue in Billion 1975 USD is calculated as Production * Price.
    L133.ag_Cost_75USDm2_C %>%
      # Join Agricultural Prices table to get a calPrice column:
      left_join_error_no_match(select(L132.ag_an_For_Prices, -unit), by = c("GCAM_commodity")) %>%
      # Keep the minimum of Cost_75USDkg and calPrice*(1-MIN_PROFIT_MARGIN) to insure that the minimum profit margin is met:
      mutate(Cost_75USDkg = if_else(Cost_75USDkg < calPrice * (1 - MIN_PROFIT_MARGIN),
                                    Cost_75USDkg,
                                    calPrice * (1 - MIN_PROFIT_MARGIN))) %>%
      # Calculate Revenue = Prod_Mt * calPrice:
      mutate(Revenue_bil75USD = Prod_Mt * calPrice) ->
      # store:
      L133.ag_Cost_75USDm2_C


    # Line 107 in original file
    # calculate the Average Profit in 1975USD/m2. This is a scaler quantity, the average profit across all
    # commodities:  (total revenue - total expenditures) / (total Harvested Area)
    L133.ag_Cost_75USDm2_C %>%
      select(-GCAM_commodity) %>%
      summarise(AvgProfit_75USDm2 = (sum(Revenue_bil75USD) - sum(Expenditures_bil75USD)) / sum(HA_bm2)) ->
      L133.AvgProfit_75USDm2


    # Lines 109-126 in original file
    # Finish calculating Cost = Price - (Profit / Yield) for each GCAM_commodity in 1975 USD/kg. There are
    # commodities not yet considered, such as PalmFruit, Fodders, etc. The data for these in the US is missing
    # from at least one input source used to calculate Costs so far.

    # get the LDS information for USA crops not covered so far:
    # Take the LDS harvested area information by region, GLU, and GTAP crop:
    L100.LDS_ag_HA_ha %>%
      # select the USA crops and Indonesia Palmfruit:
      filter(iso == "usa" | iso == "idn" & GTAP_crop == "OilPalmFruit") %>%
      # rename value to HA_ha:
      rename(HA_ha = value) %>%
      # join the corresponding production information:
      left_join_error_no_match((L100.LDS_ag_prod_t), by = c("iso", "GLU", "GTAP_crop")) %>%
      rename(Prod_t = value) %>%
      # use the FAO PRODSTAT data to join GCAM_commodity information:
      left_join_error_no_match(select(FAO_ag_items_PRODSTAT, GTAP_crop, GCAM_commodity), by = c("GTAP_crop")) %>%
      # filter to just the commodities we DON'T have costs for above
      filter(!(GCAM_commodity %in% L133.ag_Cost_75USDm2_C$GCAM_commodity)) ->
      L133.LDS_usa_oth

    # Use the LDS 'other' crop table, L133.LDS_usa_oth, to calculate missing costs.
    # 1. Aggregate to the level of GCAM_commodity
    # 2. Use aggregated Production and Harvested Area to Calculate Yield for each commodity
    # 3. Join in agricultural price information for these commodities
    # 4. use the USA average profit as the profit for each of these commodities, and calculate
    #    Cost = Price - (Profit / Yield)
    L133.LDS_usa_oth %>%
      # 1. Aggregate Production and Harvested Area to commodity level:
      group_by(GCAM_commodity) %>%
      summarise(HA_ha = sum(HA_ha), Prod_t = sum(Prod_t)) %>%
      # 2. calculate Yield in kg/square meter for each commodity:
      mutate(Yield_kgm2 = Prod_t / HA_ha * CONV_THA_KGM2) %>%
      # 3. join in price information for these commodities:
      left_join_error_no_match(select(L132.ag_an_For_Prices, -unit), by = c("GCAM_commodity")) %>%
      # 4. Use the average profit to calculate cost for these missing commodities
      #    Cost_75USDkg = calPrice - (AvgProfit) / Yield:
      mutate(Cost_75USDkg = calPrice - L133.AvgProfit_75USDm2$AvgProfit_75USDm2 / Yield_kgm2) %>%
      # only save Commodity and Cost Info
      select(GCAM_commodity, Cost_75USDkg) ->
      # store in a table of Costs for other commodities:
      L133.ag_Cost_75USDkg_Cothr


    # Lines 125 - 127
    # Join the two tables of cost information to get Cost in 1975 USD/kg for each GCAM Commodity
    L133.ag_Cost_75USDm2_C %>%
      select(GCAM_commodity, Cost_75USDkg) %>%
      bind_rows(L133.ag_Cost_75USDkg_Cothr) ->
      L133.ag_Cost_75USDkg_C


    # Produce outputs
    L133.ag_Cost_75USDkg_C %>%
      add_title("Costs of GCAM commodities") %>%
      add_units("Units = 1975$/kg (75USDkg)") %>%
      add_comments("GCAM commodity costs are determined by USDA cost data for USA, when available.") %>%
      add_comments("Commodities without USDA cost data have costs calculated using the average profit") %>%
      add_comments("among USDA commodities and LDS harvested area and production data.") %>%
      add_legacy_name("L133.ag_Cost_75USDkg_C") %>%
      add_precursors("aglu/USDA_crops",
                     "aglu/USDA_item_cost",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "aglu/USDA_cost_data",
                     "L100.LDS_ag_HA_ha",
                     "L100.LDS_ag_prod_t",
                     "L132.ag_an_For_Prices") ->
      L133.ag_Cost_75USDkg_C

    return_data(L133.ag_Cost_75USDkg_C)
  } else {
    stop("Unknown command")
  }
}
