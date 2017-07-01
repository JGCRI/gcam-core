#' module_aglu_L203.demand_input
#'
#' Briefly describe what this chunk does.
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
#' @author YourInitials CurrentMonthName 2017
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

    GCAM_commodity <- GCAM_region_ID<- element <- item <- value <- year <- aglu_demand_calyears <- aglu_demand_futureyears <-
      names_Supplysector <- names_Subsector <- names_SubsectorAll <-
      names_Tech  <- names_StubTechProd <- names_StubTechFixOut <- names_StubTechCalorieContent <- names_EnergyFinalDemand <-
      names_PerCapitaBased <- Prod_colnames <- names_PriceElasticity <- NULL   # silence package check notes

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

    # Define column names
    # Supplysectors
    names_Supplysector <- LEVEL2_DATA_NAMES[["Supplysector"]]
    # Subsectors
    names_Subsector <- LEVEL2_DATA_NAMES[["Subsector"]]
    names_SubsectorAll <- LEVEL2_DATA_NAMES[["SubsectorAll"]]
    # Technologies
    names_Tech <- LEVEL2_DATA_NAMES[["Tech"]]
    names_StubTechProd <- LEVEL2_DATA_NAMES[["StubTechProd"]]
    names_StubTechFixOut <- LEVEL2_DATA_NAMES[["StubTechFixOut"]]
    names_StubTechCalorieContent <- LEVEL2_DATA_NAMES[["StubTechCalorieContent"]]
    # Final demands
    names_EnergyFinalDemand <- LEVEL2_DATA_NAMES[["EnergyFinalDemand"]]
    names_PerCapitaBased <- LEVEL2_DATA_NAMES[["PerCapitaBased"]]
    Prod_colnames <- c("region", "supplysector", "year", "calOutputValue")
    names_PriceElasticity <- LEVEL2_DATA_NAMES[["PriceElasticity"]]

    # L203.Supplysector_demand: generic info for demand sectors
    A_demand_supplysector %>%
      write_to_all_regions(c(names_Supplysector, "logit.type"), GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->
    L203.Supplysector_demand

    # L203.SubsectorAll_demand: generic info for demand subsectors
    A_demand_subsector %>%
      write_to_all_regions(c(names_SubsectorAll, "logit.type"), GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->
      L203.SubsectorAll_demand

    # L203.StubTech_demand: identification of stub technologies for demands
    A_demand_technology %>%
      write_to_all_regions(names_Tech, GCAM_region_names = GCAM_region_names) %>%
      rename(stub.technology = technology) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->
      L203.StubTech_demand

    # L203.GlobalTechCoef_demand: input names of demand technologies
    A_demand_technology %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      select(sector.name, subsector.name, technology, year, minicam.energy.input, coefficient) ->
      L203.GlobalTechCoef_demand

    # L203.GlobalTechShrwt_demand: shareweights of demand technologies
    L203.GlobalTechCoef_demand %>%
      select(sector.name, subsector.name, technology, year) %>%
      mutate(share.weight = 1) ->
      L203.GlobalTechShrwt_demand

    # Calibrated food and nonfood demands of crops and meat
    # Create table of regions, technologies, and all base years
    # NOTE: Easiest if the model base years are subsetted from a full table as a last step in the construction of each of these tables
    A_demand_technology %>%
      write_to_all_regions(c(names_Tech, "minicam.energy.input", "market.name"), GCAM_region_names = GCAM_region_names) %>%
      mutate(market.name = region, stub.technology = technology) ->
      A_demand_technology_R

    A_demand_technology_R %>%
      repeat_add_columns(tibble(year = aglu_demand_calyears)) ->
      A_demand_technology_R_Yh

    A_demand_technology_R %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      A_demand_technology_R_Y

    # L203.StubTechProd_food_crop: crop food demand by technology and region
    # L203.StubTechProd_food_meat: meat food demand by technology and region
    L101.ag_Food_Pcal_R_C_Y %>%
      bind_rows(L105.an_Food_Pcal_R_C_Y) %>%
      filter(year %in% aglu_demand_calyears) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.ag_an_Food_Pcal_R_C_Y

    A_demand_technology_R_Yh %>%
      filter(supplysector %in% c("FoodDemand_Crops", "FoodDemand_Meat")) %>%
      left_join_error_no_match(L203.ag_an_Food_Pcal_R_C_Y, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(calOutputValue = round(value, aglu.DIGITS_CALOUTPUT),
             share.weight.year = year,
             # Subsector and technology shareweights (subsector requires the year as well)
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(one_of(names_StubTechProd)) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS & year %in% BASE_YEARS) ->     ## ALSO SUBSET THE CALIBRATION TABLES TO ONLY THE MODEL BASE YEARS
      L203.StubTechProd_food

    # L203.StubTechProd_nonfood_crop: crop nonfood demand by technology and region
    # L203.StubTechProd_nonfood_meat: meat nonfood demand by technology and region
    L109.ag_ALL_Mt_R_C_Y %>%
      bind_rows(L109.an_ALL_Mt_R_C_Y) %>%
      filter(year %in% aglu_demand_calyears) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(GCAM_region_ID, region, GCAM_commodity, year, OtherUses_Mt, NetExp_Mt) ->
      L203.ag_an_ALL_Mt_R_C_Y

    A_demand_technology_R_Yh %>%
      filter(supplysector %in% c("NonFoodDemand_Crops", "NonFoodDemand_Meat")) %>%
      left_join_error_no_match(L203.ag_an_ALL_Mt_R_C_Y, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(calOutputValue = round(OtherUses_Mt, aglu.DIGITS_CALOUTPUT),
             share.weight.year = year,
             # Subsector and technology shareweights (subsector requires the year as well)
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(one_of(names_StubTechProd)) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS & year %in% BASE_YEARS) ->     ## ALSO SUBSET THE CALIBRATION TABLES TO ONLY THE MODEL BASE YEARS
      L203.StubTechProd_nonfood

    # L203.StubTechFixOut_exp: animal exports for net exporting regions in all periods
    A_demand_technology_R_Y %>%
      filter(supplysector == "Exports_Meat") %>%
      # create NAs for future years
      left_join(L203.ag_an_ALL_Mt_R_C_Y, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(fixedOutput = pmax(0, round(NetExp_Mt, aglu.DIGITS_CALOUTPUT))) %>%
      group_by(region, subsector) %>%
      mutate(fixedOutput = replace(fixedOutput, year > max(BASE_YEARS), fixedOutput[year == max(BASE_YEARS)]),
             share.weight.year = year,
             # Subsector and technology shareweights (subsector requires the year as well)
             subs.share.weight = 0, tech.share.weight = 0) %>%
      ungroup() %>%
      select(one_of(names_StubTechFixOut)) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->
      L203.StubTechFixOut_exp

    # L203.StubTechProd_For: Forest product demand by technology and region
    L110.For_ALL_bm3_R_Y %>%
      unique() %>%
      filter(year %in% aglu_demand_calyears) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      ungroup() %>%
      select(GCAM_region_ID, region, year, Cons_bm3) ->
      L203.For_ALL_bm3_R_Y

    A_demand_technology_R_Yh %>%
      filter(supplysector == "NonFoodDemand_Forest") %>%
      left_join_error_no_match(L203.For_ALL_bm3_R_Y, by = c("region", "year")) %>%
      mutate(calOutputValue = round(Cons_bm3, aglu.DIGITS_CALOUTPUT),
             share.weight.year = year,
             # Subsector and technology shareweights (subsector requires the year as well)
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(one_of(names_StubTechProd)) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS & year %in% BASE_YEARS) ->     ## ALSO SUBSET THE CALIBRATION TABLES TO ONLY THE MODEL BASE YEARS
      L203.StubTechProd_For

    # L203.StubCalorieContent_crop: caloric content of food crops (incl secondary products
    # L203.StubCalorieContent_meat: caloric content of meat commodities
    L101.ag_kcalg_R_C_Y %>%
      bind_rows(L105.an_kcalg_R_C_Y) %>%
      filter(year %in% aglu_demand_calyears) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.ag_an_kcalg_R_C_Y

    A_demand_technology_R_Y %>%
      filter(supplysector %in% c("FoodDemand_Crops", "FoodDemand_Meat")) %>%
      left_join(L203.ag_an_kcalg_R_C_Y, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(efficiency = round(value, aglu.DIGITS_CALOUTPUT)) %>%
      group_by(region, subsector) %>%
      mutate(efficiency = replace(efficiency, year > max(BASE_YEARS), efficiency[year == max(BASE_YEARS)])) %>%
      ungroup() %>%
      select(one_of(names_StubTechCalorieContent)) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->
      L203.StubCalorieContent

    # FINAL DEMANDS
    # L203.PerCapitaBased: final demand attributes that do not vary by time period
    A_demand_supplysector %>%
      write_to_all_regions(names_PerCapitaBased, GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->
      L203.PerCapitaBased

    # L203.BaseService: base service of final demands
    L203.StubTechFixOut_exp %>%
      filter(year %in% BASE_YEARS) %>%
      rename(calOutputValue = fixedOutput) %>%
      select(one_of(Prod_colnames)) %>%
      bind_rows(L203.StubTechProd_food[Prod_colnames], L203.StubTechProd_nonfood[Prod_colnames], L203.StubTechProd_For[Prod_colnames]) %>%
      group_by(region, supplysector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      rename(energy.final.demand = supplysector, base.service = calOutputValue) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS & year %in% BASE_YEARS) ->     ## ALSO SUBSET THE CALIBRATION TABLES TO ONLY THE MODEL BASE YEARS
      L203.BaseService

    # L203.IncomeElasticity: Income elasticities
    # NOTE: food demands are taken as given in historical validation runs
    # Note: these steps contain processing normally considered level 1, but are done here because unlike other quantities computed in level 1,
    # the income elasticities depend on the timestep length chosen, and as such are dependent on the modeltime, which is not a level 1 attribute

    # Step 1: Building historical estimates of changes in per-capita food demands by region and demand type
    L101.Pop_thous_R_Yh %>%
      filter(year %in% aglu_demand_calyears) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.Pop_thous_R_Yh

    L203.BaseService %>%
      filter(energy.final.demand %in% c("FoodDemand_Crops", "FoodDemand_Meat")) %>%
      left_join_error_no_match(L203.Pop_thous_R_Yh, by = c("region", "year")) %>%
      mutate(value = base.service * (1 / CONV_MCAL_PCAL) * CONV_DAYS_YEAR / value) %>%
      select(-base.service) %>%
      group_by(region, GCAM_region_ID, energy.final.demand) %>%
      mutate(ratio = value / lag(value)) %>%
      replace_na(list(ratio = 1)) ->
      L203.pcFoodRatio_R_Yh

    # Step 2: Calculating future changes (ratios) in caloric demands by region and demand type
    L134.pcFood_kcald_R_Dmnd_Y %>%
      mutate(scenario = "core") %>%
      bind_rows(mutate(L134.pcFood_kcald_R_Dmnd_Y_ssp1, scenario = "SSP1"),
                mutate(L134.pcFood_kcald_R_Dmnd_Y_ssp2, scenario = "SSP2"),
                mutate(L134.pcFood_kcald_R_Dmnd_Y_ssp3, scenario = "SSP3"),
                mutate(L134.pcFood_kcald_R_Dmnd_Y_ssp4, scenario = "SSP4"),
                mutate(L134.pcFood_kcald_R_Dmnd_Y_ssp5, scenario = "SSP5")) %>%
      filter(year %in% aglu_demand_futureyears) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.pcFood_kcald_R_Dmnd_Y

    L203.pcFood_kcald_R_Dmnd_Y %>%
      mutate(energy.final.demand = "FoodDemand_Crops",
             energy.final.demand = replace(energy.final.demand, GCAM_demand == "meat", "FoodDemand_Meat")) %>%
      group_by(GCAM_region_ID, region, energy.final.demand, scenario) %>%
      mutate(ratio = value / lag(value)) %>%
      replace_na(list(ratio = 1)) %>%
      select(-value) ->
      L203.pcFoodRatio_R_Dmnd_Yfut

    # Step 4: Calculating the per-capita GDP trajectories over the same time period
    # NOTE: only computing elasticities based on the specified GDP scenario
    L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      group_by(GCAM_region_ID, region, scenario) %>%
      mutate(gdp.ratio = value / lag(value)) %>%
      replace_na(list(gdp.ratio = 1)) %>%
      select(-value) %>%
      spread(scenario, gdp.ratio) %>%
      mutate(core = SSP2) %>%
      gather(scenario, gdp.ratio, -region, -GCAM_region_ID, -year) ->
      L203.pcgdpRatio_R_Y

    # Step 5: Solving for the income elasticities in each time period
    L203.pcFoodRatio_R_Dmnd_Yfut %>%
      left_join(L203.pcgdpRatio_R_Y, by = c("GCAM_region_ID", "region", "year", "scenario")) %>%
      mutate(income.elasticity = round(log(ratio) / log(gdp.ratio), aglu.DIGITS_INCELAS)) %>%
      replace_na(list(income.elasticity = 0)) %>%
      # Step 6: Converting to appropriate formats
      filter(year %in% FUTURE_YEARS) %>%
      select(scenario, region, energy.final.demand, year, income.elasticity) %>%
      mutate(income.elasticity = replace(income.elasticity, scenario != "core" & income.elasticity > 1, 1),
             income.elasticity = replace(income.elasticity, scenario != "core" & income.elasticity < 0, 0)) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->
      L203.IncomeElasticity_allScen

    # L203.PriceElasticity: Price elasticities
    L203.PriceElasticity <- write_to_all_regions(A_demand_supplysector, c(names_EnergyFinalDemand, "price.elasticity"),
                                                 GCAM_region_names = GCAM_region_names ) %>%
      repeat_add_columns(tibble(year = FUTURE_YEARS)) %>% # Price elasticities are only read for future periods
      #Set the USA price elasticity to a region-specific value
      mutate(price.elasticity = replace(price.elasticity, region == "USA" & energy.final.demand == "FoodDemand_Meat", aglu.FOOD_MEAT_P_ELAS_USA)) %>%
      select(one_of(names_PriceElasticity)) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->
      L203.PriceElasticity

    # Fuel preference elasticity
    # L203.FuelPrefElast_ssp1: Fuel preference elasticities for meat in SSP1
    names_FuelPrefElasticity <- c("region", "supplysector", "subsector", "year.fillout", "fuelprefElasticity")
    A_fuelprefElasticity_ssp1 %>%
      mutate(year.fillout = min(BASE_YEARS)) %>%
      write_to_all_regions(names_FuelPrefElasticity, GCAM_region_names = GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) ->
      L203.FuelPrefElast_ssp1

    L203.Supplysector_demand %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.Supplysector_demand") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector") ->
      L203.Supplysector_demand

    L203.SubsectorAll_demand %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.SubsectorAll_demand") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_subsector") ->
      L203.SubsectorAll_demand

    L203.StubTech_demand %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubTech_demand") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology") ->
      L203.StubTech_demand

    L203.GlobalTechCoef_demand %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.GlobalTechCoef_demand") %>%
      add_precursors("aglu/A_demand_technology") ->
      L203.GlobalTechCoef_demand

    L203.GlobalTechShrwt_demand %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.GlobalTechShrwt_demand") %>%
      same_precursors_as(L203.GlobalTechCoef_demand) ->
      L203.GlobalTechShrwt_demand

    L203.StubTechProd_food %>%
      filter(supplysector == "FoodDemand_Crops") %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubTechProd_food_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L101.ag_Food_Pcal_R_C_Y") ->
      L203.StubTechProd_food_crop

    L203.StubTechProd_food %>%
      filter(supplysector == "FoodDemand_Meat") %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubTechProd_food_meat") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L105.an_Food_Pcal_R_C_Y") ->
      L203.StubTechProd_food_meat

    L203.StubTechProd_nonfood %>%
      filter(supplysector == "NonFoodDemand_Crops") %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubTechProd_nonfood_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L109.ag_ALL_Mt_R_C_Y") ->
      L203.StubTechProd_nonfood_crop

    L203.StubTechProd_nonfood %>%
      filter(supplysector == "NonFoodDemand_Meat") %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubTechProd_nonfood_meat") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L109.an_ALL_Mt_R_C_Y") ->
      L203.StubTechProd_nonfood_meat

    L203.StubTechProd_For %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubTechProd_For") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L110.For_ALL_bm3_R_Y") ->
      L203.StubTechProd_For

    L203.StubTechFixOut_exp %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubTechFixOut_exp") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L109.an_ALL_Mt_R_C_Y") ->
      L203.StubTechFixOut_exp

    L203.StubCalorieContent %>%
      filter(supplysector == "FoodDemand_Crops") %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubCalorieContent_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L101.ag_kcalg_R_C_Y") ->
      L203.StubCalorieContent_crop

    L203.StubCalorieContent %>%
      filter(supplysector == "FoodDemand_Meat") %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubCalorieContent_meat") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L105.an_kcalg_R_C_Y") ->
      L203.StubCalorieContent_meat

    L203.PerCapitaBased %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.PerCapitaBased") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector") ->
      L203.PerCapitaBased

    L203.BaseService %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.BaseService") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_technology",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L101.ag_kcalg_R_C_Y",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L105.an_kcalg_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y") ->
      L203.BaseService

    L203.IncomeElasticity_allScen %>%
      filter(scenario == "core") %>%
      select(-scenario) %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.IncomeElasticity") %>%
      add_precursors("common/GCAM_region_names",
                     "L134.pcFood_kcald_R_Dmnd_Y",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L203.IncomeElasticity

    L203.PriceElasticity %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.PriceElasticity") %>%
      add_precursors("aglu/A_demand_supplysector") ->
      L203.PriceElasticity

    L203.FuelPrefElast_ssp1 %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.FuelPrefElast_ssp1") %>%
      add_precursors("aglu/A_fuelprefElasticity_ssp1") ->
      L203.FuelPrefElast_ssp1

    L203.IncomeElasticity_allScen %>%
      filter(scenario == "SSP1") %>%
      select(-scenario) %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.IncomeElasticity_SSP1") %>%
      add_precursors("common/GCAM_region_names",
                     "L134.pcFood_kcald_R_Dmnd_Y_ssp1",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L203.IncomeElasticity_SSP1

    L203.IncomeElasticity_allScen %>%
      filter(scenario == "SSP2") %>%
      select(-scenario) %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.IncomeElasticity_SSP2") %>%
      add_precursors("common/GCAM_region_names",
                     "L134.pcFood_kcald_R_Dmnd_Y_ssp2",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L203.IncomeElasticity_SSP2

    L203.IncomeElasticity_allScen %>%
      filter(scenario == "SSP3") %>%
      select(-scenario) %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.IncomeElasticity_SSP3") %>%
      add_precursors("common/GCAM_region_names",
                     "L134.pcFood_kcald_R_Dmnd_Y_ssp3",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L203.IncomeElasticity_SSP3

    L203.IncomeElasticity_allScen %>%
      filter(scenario == "SSP4") %>%
      select(-scenario) %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.IncomeElasticity_SSP4") %>%
      add_precursors("common/GCAM_region_names",
                     "L134.pcFood_kcald_R_Dmnd_Y_ssp4",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") ->
      L203.IncomeElasticity_SSP4

    L203.IncomeElasticity_allScen %>%
      filter(scenario == "SSP5") %>%
      select(-scenario) %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
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
