# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L241.trade_input
#'
#' Build datasets for ssp4 agricultural trade.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L241.StubAgTradeCoeff_food}, \code{L241.StubAgTradeCoeff_nonfood}, \code{L241.StubAgTradeCoeff_feed}, \code{L241.AgProdTech_RES_output}, \code{L241.RES_Market}. The corresponding file in the
#' original data system was \code{L241.trade_input.R} (aglu level2).
#' @details Build datasets for ssp4 agricultural trade: food and nonfood trade coefficients, feed trade
#' coefficients, restricted agricultural trade, and trade regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else left_join mutate select
#' @importFrom tidyr gather spread
#' @author BBL June 2017
module_aglu_L241.trade_input <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "aglu/A_demand_technology",
             FILE = "aglu/A_an_input_technology",
             "L101.ag_kcalg_R_C_Y",
             "L2012.AgProduction_ag_irr_mgmt",
             "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L241.StubAgTradeCoeff_food",
             "L241.StubAgTradeCoeff_nonfood",
             "L241.StubAgTradeCoeff_feed",
             "L241.AgProdTech_RES_output",
             "L241.RES_Market"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    scenario <- year <- GCAM_region_ID <- region <- value <- GCAM_commodity <-
      technology <- supplysector <- res.secondary.output <- market <-
      output.ratio <- NULL # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_demand_technology <- get_data(all_data, "aglu/A_demand_technology")
    A_an_input_technology <- get_data(all_data, "aglu/A_an_input_technology")
    L101.ag_kcalg_R_C_Y <- get_data(all_data, "L101.ag_kcalg_R_C_Y")
    L2012.AgProduction_ag_irr_mgmt <- get_data(all_data, "L2012.AgProduction_ag_irr_mgmt")
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")

    # Build tables (lines 38-43 in old file)
    # First, determine which regions are in which groupings
    L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(scenario == "SSP4", year == 2010) %>%
      select(GCAM_region_ID, value) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(value = value * gdp_deflator(2010, base_year = 1990)) ->
      L241.pcgdp_2010

    L241.high_reg <- L241.pcgdp_2010$region[L241.pcgdp_2010$value > aglu.HIGH_GROWTH_PCGDP]
    L241.low_reg <- L241.pcgdp_2010$region[L241.pcgdp_2010$value < aglu.LOW_GROWTH_PCGDP]

    # Create table of regions, technologies, and all base years (47-51)
    # Easiest if the model base years are subsetted from a full table as a last step in the construction of each of these tables
    A_demand_technology %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Tech"]], "minicam.energy.input", "market.name"), GCAM_region_names) %>%
      mutate(stub.technology = technology) ->
      A_demand_technology_R
    A_an_input_technology %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Tech"]], "minicam.energy.input", "market.name"), GCAM_region_names) %>%
      mutate(stub.technology = technology) ->
      A_an_input_technology_R

    # Add lookup vectors to level1 output tables (55-59)
    # Add region names to Level1 data tables
    aglu_demand_calyears <- HISTORICAL_YEARS[HISTORICAL_YEARS %in% MODEL_YEARS]
    aglu_demand_futureyears <- MODEL_YEARS[!MODEL_YEARS %in% aglu_demand_calyears]
    L101.ag_kcalg_R_C_Y %>%
      filter(year %in% aglu_demand_calyears) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(year == max(aglu_demand_calyears)) %>%
      select(region, GCAM_commodity, value) ->
      L241.ag_kcalg_R_C_Yf.mlt

    # Fraction of food that should be produced locally
    LOCAL_FOOD_FRACT <- 0.8

    # Essentially, what we are doing is requiring that some fraction (80%, see LOCAL_FOOD_FRACT above)
    # of all food is produced domestically. We use the RES policy code in GCAM to do this. The two pipelines
    # below set up the demand portion of that, requiring the demand sectors to demand ag_trade credits in
    # addition to crops/livestocks. The demand is equal to 0.8 * the amount of crop/livestock demanded to
    # ensure that the 80% threshold is met.

    # Coefficient for ag_trade of food crops (incl secondary products) (61-69)
    A_demand_technology_R %>%
      filter(supplysector == "FoodDemand_Crops") %>%
      left_join_error_no_match(L241.ag_kcalg_R_C_Yf.mlt, by = c("region" = "region", "technology" = "GCAM_commodity")) %>%
      mutate(coefficient = LOCAL_FOOD_FRACT / round(value, aglu.DIGITS_CALOUTPUT),
             minicam.energy.input = "ag_trade") %>%
      select(-value) %>%
      repeat_add_columns(tibble(year = aglu_demand_futureyears)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef_NM"]]) ->
      L241.StubAgTradeCoeff_food

    # Coefficient for ag_trade of non-food crops (incl secondary products) (71-76)
    # Sets up the supply of ag_trade credits. It says that each supply sector generates
    # 1 credit for each unit of crop/livestock produced.
    A_demand_technology_R %>%
      filter(supplysector == "NonFoodDemand_Crops") %>%
      mutate(coefficient = LOCAL_FOOD_FRACT,
             minicam.energy.input = "ag_trade") %>%
      repeat_add_columns(tibble(year = aglu_demand_futureyears)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef_NM"]]) ->
      L241.StubAgTradeCoeff_nonfood

    # Coefficient for ag_trade of feed crops (incl secondary products) (78-83)
    A_an_input_technology_R %>%
      filter(supplysector == "FeedCrops") %>%
      mutate(coefficient = LOCAL_FOOD_FRACT,
             minicam.energy.input = "ag_trade") %>%
      repeat_add_columns(tibble(year = aglu_demand_futureyears)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef_NM"]]) ->
      L241.StubAgTradeCoeff_feed

    # Output coefficient for RES ag_trade of food crops (incl secondary products) (85-90)
    L2012.AgProduction_ag_irr_mgmt %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = aglu_demand_futureyears)) %>%
      mutate(res.secondary.output = "ag_trade",
             output.ratio = 1) %>%
      select(LEVEL2_DATA_NAMES[["AgRES"]]) ->
      L241.AgProdTech_RES_output

    # Next section sets up the trade regions. For low income regions, we are assuming trade is restricted,
    # so they have to meet the 80% threshold internally. Medium- to High-Income regions have more open trade,
    # so those thresholds can be met anywhere in the region. That is, we assume free trade in medium to high-
    # income regions, and restricted trade in low income regions. Note: these assumptions are derived from
    # the SSP4 storyline (see O'Neill et al., 2017).

    # Market for policy portfolio (92-100)
    GCAM_region_names %>%
      mutate(policy.portfolio.standard = "ag_trade",
             market = "Med_to_High_Income",
             market = if_else(region %in% L241.low_reg, region, market),
             policyType = "RES") %>%
      repeat_add_columns(tibble(year = aglu_demand_futureyears)) %>%
      mutate(constraint = 1) %>%
      select(-GCAM_region_ID) ->
      L241.RES_Market

    # Remove any regions for which agriculture and land use are not modeled
    L241.StubAgTradeCoeff_food    <- filter(L241.StubAgTradeCoeff_food, !region %in% aglu.NO_AGLU_REGIONS)
    L241.StubAgTradeCoeff_nonfood <- filter(L241.StubAgTradeCoeff_nonfood, !region %in% aglu.NO_AGLU_REGIONS)
    L241.StubAgTradeCoeff_feed    <- filter(L241.StubAgTradeCoeff_feed, !region %in% aglu.NO_AGLU_REGIONS)
    L241.AgProdTech_RES_output    <- filter(L241.AgProdTech_RES_output, !region %in% aglu.NO_AGLU_REGIONS)
    L241.RES_Market               <- filter(L241.RES_Market, !region %in% aglu.NO_AGLU_REGIONS)

    # Produce outputs
    L241.StubAgTradeCoeff_food %>%
      add_title("Coefficient for ag_trade of food crops") %>%
      add_units("None") %>%
      add_comments("Each supply sector generates 1 credit for each unit of crop/livestock produced.") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_legacy_name("L241.StubAgTradeCoeff_food") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L101.ag_kcalg_R_C_Y") ->
      L241.StubAgTradeCoeff_food

    L241.StubAgTradeCoeff_nonfood %>%
      add_title("Coefficient for ag_trade of nonfood crops") %>%
      add_units("None") %>%
      add_comments("Each supply sector generates 1 credit for each unit of crop/livestock produced.") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_legacy_name("L241.StubAgTradeCoeff_nonfood") %>%
      same_precursors_as(L241.StubAgTradeCoeff_food) ->
      L241.StubAgTradeCoeff_nonfood

    L241.StubAgTradeCoeff_feed %>%
      add_title("Coefficient for ag_trade of feed crops") %>%
      add_units("None") %>%
      add_comments("Each supply sector generates 1 credit for each unit of crop/livestock produced.") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_legacy_name("L241.StubAgTradeCoeff_feed") %>%
      add_precursors("aglu/A_an_input_technology") ->
      L241.StubAgTradeCoeff_feed

    L241.AgProdTech_RES_output %>%
      add_title("Coefficient for RES ag_trade of food crops (incl secondary products)") %>%
      add_units("None") %>%
      add_comments("Each supply sector generates 1 credit for each unit of crop/livestock produced.") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_legacy_name("L241.AgProdTech_RES_output") %>%
      add_precursors("L2012.AgProduction_ag_irr_mgmt") ->
      L241.AgProdTech_RES_output

    L241.RES_Market %>%
      add_title("Market for policy portfolio") %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_legacy_name("L241.RES_Market") %>%
      add_precursors("common/GCAM_region_names",
                     "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L241.RES_Market

    return_data(L241.StubAgTradeCoeff_food, L241.StubAgTradeCoeff_nonfood,
                L241.StubAgTradeCoeff_feed, L241.AgProdTech_RES_output, L241.RES_Market)
  } else {
    stop("Unknown command")
  }
}
