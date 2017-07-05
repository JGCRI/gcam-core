#' module_aglu_L203.demand_input
#'
#' Builds agriculture demand inputs for the core and each SSP scenario.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L203.SectorLogitTables[[ curr_table ]]$data}, \code{L203.Supplysector_demand}, \code{L203.SubsectorLogitTables[[ curr_table ]]$data}, \code{L203.SubsectorAll_demand}, \code{L203.StubTech_demand}, \code{L203.GlobalTechCoef_demand}, \code{L203.GlobalTechShrwt_demand}, \code{L203.StubTechProd_food_crop}, \code{L203.StubTechProd_food_meat}, \code{L203.StubTechProd_nonfood_crop}, \code{L203.StubTechProd_nonfood_meat}, \code{L203.StubTechProd_For}, \code{L203.StubTechFixOut_exp}, \code{L203.StubCalorieContent_crop}, \code{L203.StubCalorieContent_meat}, \code{L203.PerCapitaBased}, \code{L203.BaseService}, \code{L203.IncomeElasticity}, \code{L203.PriceElasticity}, \code{L203.FuelPrefElast_ssp1}, \code{L203.IncomeElasticity_SSP1}, \code{L203.IncomeElasticity_SSP2}, \code{L203.IncomeElasticity_SSP3}, \code{L203.IncomeElasticity_SSP4}, \code{L203.IncomeElasticity_SSP5}. The corresponding file in the
#' original data system was \code{L203.demand_input.R} (aglu level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author RC July 2017
#' @export
module_aglu_L203.demand_input <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "aglu/A_demand_supplysector",
             FILE = "aglu/A_demand_subsector",
             FILE = "aglu/A_demand_technology",
             FILE = "aglu/A_fuelprefElasticity_ssp1",
             "L101.ag_Food_Pcal_R_C_Y",
             "L101.ag_kcalg_R_C_Y",
             "L105.an_Food_Pcal_R_C_Y",
             "L105.an_kcalg_R_C_Y",
             "L109.ag_ALL_Mt_R_C_Y",
             "L109.an_ALL_Mt_R_C_Y",
             "L110.For_ALL_bm3_R_Y",
             "L134.pcFood_kcald_R_Dmnd_Y",
             "L134.pcFood_kcald_R_Dmnd_Y_ssp1",
             "L134.pcFood_kcald_R_Dmnd_Y_ssp2",
             "L134.pcFood_kcald_R_Dmnd_Y_ssp3",
             "L134.pcFood_kcald_R_Dmnd_Y_ssp4",
             "L134.pcFood_kcald_R_Dmnd_Y_ssp5",
             "L101.Pop_thous_R_Yh",
             "L102.pcgdp_thous90USD_Scen_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L203.Supplysector_demand",
             "L203.SubsectorAll_demand",
             "L203.StubTech_demand",
             "L203.GlobalTechCoef_demand",
             "L203.GlobalTechShrwt_demand",
             "L203.StubTechProd_food_crop",
             "L203.StubTechProd_food_meat",
             "L203.StubTechProd_nonfood_crop",
             "L203.StubTechProd_nonfood_meat",
             "L203.StubTechProd_For",
             "L203.StubTechFixOut_exp",
             "L203.StubCalorieContent_crop",
             "L203.StubCalorieContent_meat",
             "L203.PerCapitaBased",
             "L203.BaseService",
             "L203.IncomeElasticity",
             "L203.PriceElasticity",
             "L203.FuelPrefElast_ssp1",
             "L203.IncomeElasticity_SSP1",
             "L203.IncomeElasticity_SSP2",
             "L203.IncomeElasticity_SSP3",
             "L203.IncomeElasticity_SSP4",
             "L203.IncomeElasticity_SSP5"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    GCAM_commodity <- GCAM_region_ID <- element <- item <- value <- year <- aglu_demand_calyears <- aglu_demand_futureyears <-
      Prod_colnames <- names_FuelPrefElasticity <- NULL   # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_demand_supplysector <- get_data(all_data, "aglu/A_demand_supplysector")
    A_demand_subsector <- get_data(all_data, "aglu/A_demand_subsector")
    A_demand_technology <- get_data(all_data, "aglu/A_demand_technology")
    A_fuelprefElasticity_ssp1 <- get_data(all_data, "aglu/A_fuelprefElasticity_ssp1")
    L101.ag_Food_Pcal_R_C_Y <- get_data(all_data, "L101.ag_Food_Pcal_R_C_Y")
    L101.ag_kcalg_R_C_Y <- get_data(all_data, "L101.ag_kcalg_R_C_Y")
    L105.an_Food_Pcal_R_C_Y <- get_data(all_data, "L105.an_Food_Pcal_R_C_Y")
    L105.an_kcalg_R_C_Y <- get_data(all_data, "L105.an_kcalg_R_C_Y")
    L109.ag_ALL_Mt_R_C_Y <- get_data(all_data, "L109.ag_ALL_Mt_R_C_Y")
    L109.an_ALL_Mt_R_C_Y <- get_data(all_data, "L109.an_ALL_Mt_R_C_Y")
    L110.For_ALL_bm3_R_Y <- get_data(all_data, "L110.For_ALL_bm3_R_Y")
    L134.pcFood_kcald_R_Dmnd_Y <- get_data(all_data, "L134.pcFood_kcald_R_Dmnd_Y")
    L134.pcFood_kcald_R_Dmnd_Y_ssp1 <- get_data(all_data, "L134.pcFood_kcald_R_Dmnd_Y_ssp1")
    L134.pcFood_kcald_R_Dmnd_Y_ssp2 <- get_data(all_data, "L134.pcFood_kcald_R_Dmnd_Y_ssp2")
    L134.pcFood_kcald_R_Dmnd_Y_ssp3 <- get_data(all_data, "L134.pcFood_kcald_R_Dmnd_Y_ssp3")
    L134.pcFood_kcald_R_Dmnd_Y_ssp4 <- get_data(all_data, "L134.pcFood_kcald_R_Dmnd_Y_ssp4")
    L134.pcFood_kcald_R_Dmnd_Y_ssp5 <- get_data(all_data, "L134.pcFood_kcald_R_Dmnd_Y_ssp5")
    L101.Pop_thous_R_Yh <- get_data(all_data, "L101.Pop_thous_R_Yh")
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")

    # Build tables
    # NOTE: This is somewhat complicated. Unlike demands in the energy system, the aglu demands are treated as exogenous
    # in all historical periods, regardless of which model base years are used. Processing data for all historical years
    # that are also model years (i.e. not just the model base years)
    aglu_demand_calyears <- HISTORICAL_YEARS[HISTORICAL_YEARS %in% MODEL_YEARS]
    aglu_demand_futureyears <- c(max(aglu_demand_calyears), MODEL_YEARS[!MODEL_YEARS %in% aglu_demand_calyears])

    # Build L203.Supplysector_demand: generic info for demand sectors by region
    A_demand_supplysector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"), GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) -> # Remove any regions for which agriculture and land use are not modeled
    L203.Supplysector_demand

    # Build L203.SubsectorAll_demand: generic info for demand subsectors by region
    A_demand_subsector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorAll"]], "logit.type"), GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) -> # Remove any regions for which agriculture and land use are not modeled
      L203.SubsectorAll_demand

    # Build L203.StubTech_demand: identification of stub technologies for demands by region
    A_demand_technology %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names = GCAM_region_names) %>%
      rename(stub.technology = technology) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) -> # Remove any regions for which agriculture and land use are not modeled
      L203.StubTech_demand

    # Build L203.GlobalTechCoef_demand: input names of demand technologies
    A_demand_technology %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(one_of(LEVEL2_DATA_NAMES[["GlobalTechCoef"]])) ->
      L203.GlobalTechCoef_demand

    # Build L203.GlobalTechShrwt_demand: shareweights of demand technologies
    L203.GlobalTechCoef_demand %>%
      select(one_of((LEVEL2_DATA_NAMES[["GlobalTechYr"]]))) %>%
      mutate(share.weight = 1) ->
      L203.GlobalTechShrwt_demand

    # Calibrated food and nonfood demands of crops and meat
    # Create table of regions, technologies and all base years
    # NOTE: Easiest if the model base years are subsetted from a full table as a last step in the construction of each of these tables
    A_demand_technology %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Tech"]], "minicam.energy.input", "market.name"), GCAM_region_names = GCAM_region_names) %>%
      mutate(market.name = region, stub.technology = technology) ->
      A_demand_technology_R
    # Add all base years
    A_demand_technology_R %>%
      repeat_add_columns(tibble(year = aglu_demand_calyears)) ->
      A_demand_technology_R_Yh
    # Add all model years
    A_demand_technology_R %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      A_demand_technology_R_Y

    # Build L203.StubTechProd_food_crop and L203.StubTechProd_food_meat: crop and meat food demand by technology and region
    L101.ag_Food_Pcal_R_C_Y %>%
      # Combine crop and meat food demand in Pcal
      bind_rows(L105.an_Food_Pcal_R_C_Y) %>%
      filter(year %in% aglu_demand_calyears) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.ag_an_Food_Pcal_R_C_Y

    A_demand_technology_R_Yh %>%
      # Select food demand
      filter(supplysector %in% c("FoodDemand_Crops", "FoodDemand_Meat")) %>%
      # Map in food demand by region / commodity / year
      left_join_error_no_match(L203.ag_an_Food_Pcal_R_C_Y, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(calOutputValue = round(value, aglu.DIGITS_CALOUTPUT),
             share.weight.year = year,
             # Subsector and technology shareweights (subsector requires the year as well)
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTechProd"]])) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%           # Remove any regions for which agriculture and land use are not modeled
      filter(year %in% BASE_YEARS) ->                         # ALSO SUBSET THE CALIBRATION TABLES TO ONLY THE MODEL BASE YEARS
      L203.StubTechProd_food

    # Build L203.StubTechProd_nonfood_crop and L203.StubTechProd_nonfood_meat: crop and meat nonfood demand by technology and region
    L109.ag_ALL_Mt_R_C_Y %>%
      # Combine the balance tables of crop and meat in Mt
      bind_rows(L109.an_ALL_Mt_R_C_Y) %>%
      filter(year %in% aglu_demand_calyears) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(GCAM_region_ID, region, GCAM_commodity, year, OtherUses_Mt, NetExp_Mt) -> # only nonfood and net exports data will be used
      L203.ag_an_ALL_Mt_R_C_Y

    A_demand_technology_R_Yh %>%
      # Select nonfood demand
      filter(supplysector %in% c("NonFoodDemand_Crops", "NonFoodDemand_Meat")) %>%
      # Map in nonfood demand by region / commodity / year
      left_join_error_no_match(L203.ag_an_ALL_Mt_R_C_Y, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(calOutputValue = round(OtherUses_Mt, aglu.DIGITS_CALOUTPUT),
             share.weight.year = year,
             # Subsector and technology shareweights (subsector requires the year as well)
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTechProd"]])) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%           # Remove any regions for which agriculture and land use are not modeled
      filter(year %in% BASE_YEARS) ->                         # ALSO SUBSET THE CALIBRATION TABLES TO ONLY THE MODEL BASE YEARS
      L203.StubTechProd_nonfood

    # Build L203.StubTechFixOut_exp: animal exports for net exporting regions in all periods
    A_demand_technology_R_Y %>%
      filter(supplysector == "Exports_Meat") %>%
      # Create NAs for future years, use left_join instead
      left_join(L203.ag_an_ALL_Mt_R_C_Y, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      # Replace negative net exports with zero
      mutate(fixedOutput = pmax(0, round(NetExp_Mt, aglu.DIGITS_CALOUTPUT))) %>%
      # For each region / commodity
      group_by(region, subsector) %>%
      # Set value for future years at the final base year value
      mutate(fixedOutput = replace(fixedOutput, year > max(BASE_YEARS), fixedOutput[year == max(BASE_YEARS)]),
             share.weight.year = year,
             # Subsector and technology shareweights (subsector requires the year as well)
             subs.share.weight = 0, tech.share.weight = 0) %>%
      ungroup() %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTechFixOut"]])) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->           # Remove any regions for which agriculture and land use are not modeled
      L203.StubTechFixOut_exp

    # Build L203.StubTechProd_For: Forest product demand by technology and region
    L110.For_ALL_bm3_R_Y %>%
      unique() %>%
      filter(year %in% aglu_demand_calyears) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      ungroup() %>%
      select(GCAM_region_ID, region, year, Cons_bm3) -> # Select forest demand
      L203.For_ALL_bm3_R_Y

    A_demand_technology_R_Yh %>%
      filter(supplysector == "NonFoodDemand_Forest") %>%
      # Map in forest product demand in bm3
      left_join_error_no_match(L203.For_ALL_bm3_R_Y, by = c("region", "year")) %>%
      mutate(calOutputValue = round(Cons_bm3, aglu.DIGITS_CALOUTPUT),
             share.weight.year = year,
             # Subsector and technology shareweights (subsector requires the year as well)
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTechProd"]])) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%           # Remove any regions for which agriculture and land use are not modeled
      filter(year %in% BASE_YEARS) ->                         # ALSO SUBSET THE CALIBRATION TABLES TO ONLY THE MODEL BASE YEARS
      L203.StubTechProd_For

    # Build L203.StubCalorieContent_crop and L203.StubCalorieContent_meat:
    # caloric content of food crops (incl secondary products) and meat commodities
    L101.ag_kcalg_R_C_Y %>%
      # Combine the weigted average caloric content of crop and meat products
      bind_rows(L105.an_kcalg_R_C_Y) %>%
      filter(year %in% aglu_demand_calyears) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.ag_an_kcalg_R_C_Y

    A_demand_technology_R_Y %>%
      filter(supplysector %in% c("FoodDemand_Crops", "FoodDemand_Meat")) %>%
      # Create NAs for future years, use left_join instead
      left_join(L203.ag_an_kcalg_R_C_Y, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(efficiency = round(value, aglu.DIGITS_CALOUTPUT)) %>%
      # For each region / commodity,
      group_by(region, subsector) %>%
      # Set value for future years at the final base year value
      mutate(efficiency = replace(efficiency, year > max(BASE_YEARS), efficiency[year == max(BASE_YEARS)])) %>%
      ungroup() %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTechCalorieContent"]])) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->          # Remove any regions for which agriculture and land use are not modeled
      L203.StubCalorieContent

    # FINAL DEMANDS
    # Build L203.PerCapitaBased: per-capita final demand attributes that do not vary by time period
    A_demand_supplysector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PerCapitaBased"]], GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->          # Remove any regions for which agriculture and land use are not modeled
      L203.PerCapitaBased

    # Build L203.BaseService: base service of final demands
    Prod_colnames <- c("region", "supplysector", "year", "calOutputValue")
    L203.StubTechFixOut_exp %>%
      filter(year %in% BASE_YEARS) %>%
      rename(calOutputValue = fixedOutput) %>%
      select(one_of(Prod_colnames)) %>%
      # Combine all food and nonfood demand
      bind_rows(L203.StubTechProd_food[Prod_colnames], L203.StubTechProd_nonfood[Prod_colnames], L203.StubTechProd_For[Prod_colnames]) %>%
      group_by(region, supplysector, year) %>%
      # Sum the total of all commodities by region and supply sector
      summarise(calOutputValue = sum(calOutputValue)) %>%
      rename(energy.final.demand = supplysector, base.service = calOutputValue) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%           # Remove any regions for which agriculture and land use are not modeled
      filter(year %in% BASE_YEARS) ->                         # ALSO SUBSET THE CALIBRATION TABLES TO ONLY THE MODEL BASE YEARS
      L203.BaseService

    # L203.IncomeElasticity: Income elasticities
    # NOTE: food demands are taken as given in historical validation runs
    # Note: these steps contain processing normally considered level 1, but are done here because unlike other quantities computed in level 1,
    # the income elasticities depend on the timestep length chosen, and as such are dependent on the modeltime, which is not a level 1 attribute

    # Step 1: Build historical estimates of changes in per-capita food demands by region and demand type
    L101.Pop_thous_R_Yh %>%
      filter(year %in% aglu_demand_calyears) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.Pop_thous_R_Yh   # Population by region in calibration years

    L203.BaseService %>%
      # Filter total final food demand in Pcal
      filter(energy.final.demand %in% c("FoodDemand_Crops", "FoodDemand_Meat")) %>%
      # Map in total population in thousands
      left_join_error_no_match(L203.Pop_thous_R_Yh, by = c("region", "year")) %>%
      # Calculate daily per-capita food demand (Mcal / thous = kcal)
      mutate(value = base.service * (1 / CONV_MCAL_PCAL) * CONV_DAYS_YEAR / value) %>%
      select(-base.service) %>%
      group_by(region, GCAM_region_ID, energy.final.demand) %>%
      # Calculate per capita food demand changes (ratios)
      mutate(ratio = value / lag(value)) %>%
      replace_na(list(ratio = 1)) ->
      L203.pcFoodRatio_R_Yh

    # Step 2: Calculate future changes (ratios) in caloric demands by region and demand type
    L134.pcFood_kcald_R_Dmnd_Y %>%
      # Combine per capita food caloric demand of core and all SSP scenarios
      mutate(scenario = "core") %>%
      bind_rows(mutate(L134.pcFood_kcald_R_Dmnd_Y_ssp1, scenario = "SSP1"),
                mutate(L134.pcFood_kcald_R_Dmnd_Y_ssp2, scenario = "SSP2"),
                mutate(L134.pcFood_kcald_R_Dmnd_Y_ssp3, scenario = "SSP3"),
                mutate(L134.pcFood_kcald_R_Dmnd_Y_ssp4, scenario = "SSP4"),
                mutate(L134.pcFood_kcald_R_Dmnd_Y_ssp5, scenario = "SSP5")) %>%
      filter(year %in% aglu_demand_futureyears) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # Create category crop vs. meat
      mutate(energy.final.demand = "FoodDemand_Crops",
             energy.final.demand = replace(energy.final.demand, GCAM_demand == "meat", "FoodDemand_Meat")) %>%
      # For each region / category / scenario,
      group_by(GCAM_region_ID, region, energy.final.demand, scenario) %>%
      # Calculate per capita food demand changes (ratios)
      mutate(ratio = value / lag(value)) %>%
      replace_na(list(ratio = 1)) %>%
      select(-value) ->
      L203.pcFoodRatio_R_Dmnd_Yfut

    # Step 4: Calculate the per-capita GDP trajectories over the same time period
    # NOTE: only computing elasticities based on the specified GDP scenario
    L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # For each region / scenario,
      group_by(GCAM_region_ID, region, scenario) %>%
      # Calculate per capita GDP changes (ratios)
      mutate(gdp.ratio = value / lag(value)) %>%
      replace_na(list(gdp.ratio = 1)) %>%
      select(-value) %>%
      spread(scenario, gdp.ratio) %>%
      # Set the core scenario's per capita GDP changes the same as the SSP2
      mutate(core = SSP2) %>%
      gather(scenario, gdp.ratio, -region, -GCAM_region_ID, -year) ->
      L203.pcgdpRatio_R_Y

    # Step 5: Solve for the income elasticities in each time period
    L203.pcFoodRatio_R_Dmnd_Yfut %>%
      # Creates NAs, use left_join instead
      left_join(L203.pcgdpRatio_R_Y, by = c("GCAM_region_ID", "region", "year", "scenario")) %>%
      mutate(income.elasticity = round(log(ratio) / log(gdp.ratio), aglu.DIGITS_INCELAS)) %>%
      replace_na(list(income.elasticity = 0)) %>%
      # Step 6: Converte to appropriate formats
      filter(year %in% FUTURE_YEARS) %>%
      select(scenario, region, energy.final.demand, year, income.elasticity) %>%
      # Adjust income elasticity values between [0,1] for all the SSP scenarios
      mutate(income.elasticity = replace(income.elasticity, scenario != "core" & income.elasticity > 1, 1),
             income.elasticity = replace(income.elasticity, scenario != "core" & income.elasticity < 0, 0)) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->           # Remove any regions for which agriculture and land use are not modeled
      L203.IncomeElasticity_allScen

    # Build L203.PriceElasticity: Price elasticities
    L203.PriceElasticity <- write_to_all_regions(A_demand_supplysector, c(LEVEL2_DATA_NAMES[["EnergyFinalDemand"]], "price.elasticity"),
                                                 GCAM_region_names = GCAM_region_names ) %>%
      repeat_add_columns(tibble(year = FUTURE_YEARS)) %>% # Price elasticities are only read for future periods
      # Set the USA meat food price elasticity to a region-specific value
      mutate(price.elasticity = replace(price.elasticity, region == "USA" & energy.final.demand == "FoodDemand_Meat", aglu.FOOD_MEAT_P_ELAS_USA)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["PriceElasticity"]])) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->           # Remove any regions for which agriculture and land use are not modeled
      L203.PriceElasticity

    # Fuel preference elasticity
    # Build L203.FuelPrefElast_ssp1: Fuel preference elasticities for meat in SSP1
    names_FuelPrefElasticity <- c("region", "supplysector", "subsector", "year.fillout", "fuelprefElasticity")
    A_fuelprefElasticity_ssp1 %>%
      mutate(year.fillout = min(BASE_YEARS)) %>%
      write_to_all_regions(names_FuelPrefElasticity, GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->           # Remove any regions for which agriculture and land use are not modeled
      L203.FuelPrefElast_ssp1

    L203.Supplysector_demand %>%
      add_title("Generic information for agriculture demand sectors") %>%
      add_units("Unitless") %>%
      add_comments("Specify generic info for demand sectors") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.Supplysector_demand") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector") ->
      L203.Supplysector_demand

    L203.SubsectorAll_demand %>%
      add_title("Generic information for agriculture demand subsectors") %>%
      add_units("Unitless") %>%
      add_comments("Specify generic info for demand subsectors") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.SubsectorAll_demand") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_subsector") ->
      L203.SubsectorAll_demand

    L203.StubTech_demand %>%
      add_title("Identification for stub technologies for agriculture demands") %>%
      add_units("Unitless") %>%
      add_comments("Specify identification of stub technologies for demands") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.StubTech_demand") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology") ->
      L203.StubTech_demand

    L203.GlobalTechCoef_demand %>%
      add_title("Input names of agriculture demand technologies") %>%
      add_units("Unitless") %>%
      add_comments("Specify input names of demand technologies") %>%
      add_legacy_name("L203.GlobalTechCoef_demand") %>%
      add_precursors("aglu/A_demand_technology") ->
      L203.GlobalTechCoef_demand

    L203.GlobalTechShrwt_demand %>%
      add_title("Shareweights of agriculture demand technologies") %>%
      add_units("Unitless") %>%
      add_comments("Specify shareweights of agriculture demand technologies") %>%
      add_legacy_name("L203.GlobalTechShrwt_demand") %>%
      same_precursors_as(L203.GlobalTechCoef_demand) ->
      L203.GlobalTechShrwt_demand

    L203.StubTechProd_food %>%
      filter(supplysector == "FoodDemand_Crops") %>%
      add_title("Crop food demand by technology and region") %>%
      add_units("Pcal") %>%
      add_comments("Map in crop food demand in calibration years by region / commodity") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.StubTechProd_food_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L101.ag_Food_Pcal_R_C_Y") ->
      L203.StubTechProd_food_crop

    L203.StubTechProd_food %>%
      filter(supplysector == "FoodDemand_Meat") %>%
      add_title("Meat food demand by technology and region") %>%
      add_units("Pcal") %>%
      add_comments("Map in meat food demand in calibration years by region / commodity") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.StubTechProd_food_meat") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L105.an_Food_Pcal_R_C_Y") ->
      L203.StubTechProd_food_meat

    L203.StubTechProd_nonfood %>%
      filter(supplysector == "NonFoodDemand_Crops") %>%
      add_title("Crop non-food demand by technology and region") %>%
      add_units("Mt") %>%
      add_comments("Map in crop non-food demand in calibration years by region / commodity") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.StubTechProd_nonfood_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L109.ag_ALL_Mt_R_C_Y") ->
      L203.StubTechProd_nonfood_crop

    L203.StubTechProd_nonfood %>%
      filter(supplysector == "NonFoodDemand_Meat") %>%
      add_title("Meat non-food demand by technology and region") %>%
      add_units("Mt") %>%
      add_comments("Map in meat non-food demand in calibration years by region / commodity") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.StubTechProd_nonfood_meat") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L109.an_ALL_Mt_R_C_Y") ->
      L203.StubTechProd_nonfood_meat

    L203.StubTechProd_For %>%
      add_title("Forest product demand by technology and region") %>%
      add_units("bm3") %>%
      add_comments("Map in forest demand in calibration years by region / commodity") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.StubTechProd_For") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L110.For_ALL_bm3_R_Y") ->
      L203.StubTechProd_For

    L203.StubTechFixOut_exp %>%
      add_title("Animal exports for net exporting regions in all periods") %>%
      add_units("Mt") %>%
      add_comments("Map in animal products exports in calibartion years by region / commodity") %>%
      add_comments("Net importing countries are set to zero") %>%
      add_comments("Future years are set to the final base year value") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.StubTechFixOut_exp") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L109.an_ALL_Mt_R_C_Y") ->
      L203.StubTechFixOut_exp

    L203.StubCalorieContent %>%
      filter(supplysector == "FoodDemand_Crops") %>%
      add_title("Caloric content of food crops") %>%
      add_units("kcal/g") %>%
      add_comments("Map in weighted average of caloric content in calibration years by region / commodity") %>%
      add_comments("Future years are set to the final base year value") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.StubCalorieContent_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L101.ag_kcalg_R_C_Y") ->
      L203.StubCalorieContent_crop

    L203.StubCalorieContent %>%
      filter(supplysector == "FoodDemand_Meat") %>%
      add_title("Caloric content of meat commodities") %>%
      add_units("kcal/g") %>%
      add_comments("Map in weighted average of caloric content in calibration years by region / commodity") %>%
      add_comments("Future years are set to the final base year value") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.StubCalorieContent_meat") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L105.an_kcalg_R_C_Y") ->
      L203.StubCalorieContent_meat

    L203.PerCapitaBased %>%
      add_title("Per-capita final agriculture demand attributes") %>%
      add_units("Unitless") %>%
      add_comments("Attributes do not vary by time period") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.PerCapitaBased") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector") ->
      L203.PerCapitaBased

    L203.BaseService %>%
      add_title("Base service of final demands") %>%
      add_units("Pcal/Mt/bm3") %>%
      add_comments("Calculate the total final demands by supply sector in each region") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.BaseService") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_technology",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y") ->
      L203.BaseService

    L203.IncomeElasticity_allScen %>%
      filter(scenario == "core") %>%
      select(-scenario) %>%
      add_title("Core scenario future income elasticity of crop and meat food demand by region") %>%
      add_units("Unitless") %>%
      add_comments("Calculate income elasticity based on core scenario food demand and SSP2 GDP growth trajectory") %>%
      add_comments("Values are between zero and 1") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.IncomeElasticity") %>%
      add_precursors("common/GCAM_region_names",
                     "L134.pcFood_kcald_R_Dmnd_Y",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L203.IncomeElasticity

    L203.PriceElasticity %>%
      add_title("Price elasticities of crop and meat demand by supply sector") %>%
      add_units("Unitless") %>%
      add_comments("Set the USA meat food price elasticity to a region-specific value") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.PriceElasticity") %>%
      add_precursors("aglu/A_demand_supplysector") ->
      L203.PriceElasticity

    L203.FuelPrefElast_ssp1 %>%
      add_title("Fuel preference elasticities for meat in SSP1") %>%
      add_units("Unitless") %>%
      add_comments("Specify the minimum base year value") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.FuelPrefElast_ssp1") %>%
      add_precursors("aglu/A_fuelprefElasticity_ssp1") ->
      L203.FuelPrefElast_ssp1

    L203.IncomeElasticity_allScen %>%
      filter(scenario == "SSP1") %>%
      select(-scenario) %>%
      add_title("SSP1 future income elasticity of crop and meat food demand by region") %>%
      add_units("Unitless") %>%
      add_comments("Calculate income elasticity based on SSP1 food demand and GDP growth trajectory") %>%
      add_comments("Values are between zero and 1") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.IncomeElasticity_SSP1") %>%
      add_precursors("common/GCAM_region_names",
                     "L134.pcFood_kcald_R_Dmnd_Y_ssp1",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L203.IncomeElasticity_SSP1

    L203.IncomeElasticity_allScen %>%
      filter(scenario == "SSP2") %>%
      select(-scenario) %>%
      add_title("SSP2 future income elasticity of crop and meat food demand by region") %>%
      add_units("Unitless") %>%
      add_comments("Calculate income elasticity based on SSP2 food demand and GDP growth trajectory") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.IncomeElasticity_SSP2") %>%
      add_precursors("common/GCAM_region_names",
                     "L134.pcFood_kcald_R_Dmnd_Y_ssp2",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L203.IncomeElasticity_SSP2

    L203.IncomeElasticity_allScen %>%
      filter(scenario == "SSP3") %>%
      select(-scenario) %>%
      add_title("SSP3 future income elasticity of crop and meat food demand by region") %>%
      add_units("Unitless") %>%
      add_comments("Calculate income elasticity based on SSP3 food demand and GDP growth trajectory") %>%
      add_comments("Values are between zero and 1") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.IncomeElasticity_SSP3") %>%
      add_precursors("common/GCAM_region_names",
                     "L134.pcFood_kcald_R_Dmnd_Y_ssp3",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L203.IncomeElasticity_SSP3

    L203.IncomeElasticity_allScen %>%
      filter(scenario == "SSP4") %>%
      select(-scenario) %>%
      add_title("SSP4 future income elasticity of crop and meat food demand by region") %>%
      add_units("Unitless") %>%
      add_comments("Calculate income elasticity based on SSP4 food demand and GDP growth trajectory") %>%
      add_comments("Values are between zero and 1") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.IncomeElasticity_SSP4") %>%
      add_precursors("common/GCAM_region_names",
                     "L134.pcFood_kcald_R_Dmnd_Y_ssp4",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L203.IncomeElasticity_SSP4

    L203.IncomeElasticity_allScen %>%
      filter(scenario == "SSP5") %>%
      select(-scenario) %>%
      add_title("SSP5 future income elasticity of crop and meat food demand by region") %>%
      add_units("Unitless") %>%
      add_comments("Calculate income elasticity based on SSP5 food demand and GDP growth trajectory") %>%
      add_comments("Values are between zero and 1") %>%
      add_comments("Remove any regions for which agriculture and land use are not modeled") %>%
      add_legacy_name("L203.IncomeElasticity_SSP5") %>%
      add_precursors("common/GCAM_region_names",
                     "L134.pcFood_kcald_R_Dmnd_Y_ssp5",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L203.IncomeElasticity_SSP5

    return_data(L203.Supplysector_demand, L203.SubsectorAll_demand, L203.StubTech_demand, L203.GlobalTechCoef_demand, L203.GlobalTechShrwt_demand, L203.StubTechProd_food_crop, L203.StubTechProd_food_meat, L203.StubTechProd_nonfood_crop, L203.StubTechProd_nonfood_meat, L203.StubTechProd_For, L203.StubTechFixOut_exp, L203.StubCalorieContent_crop, L203.StubCalorieContent_meat, L203.PerCapitaBased, L203.BaseService, L203.IncomeElasticity, L203.PriceElasticity, L203.FuelPrefElast_ssp1, L203.IncomeElasticity_SSP1, L203.IncomeElasticity_SSP2, L203.IncomeElasticity_SSP3, L203.IncomeElasticity_SSP4, L203.IncomeElasticity_SSP5)
  } else {
    stop("Unknown command")
  }
}
