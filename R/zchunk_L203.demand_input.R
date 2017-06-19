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
    return(c("L203.SectorLogitTables",
             "L203.Supplysector_demand",
             "L203.SubsectorLogitTables",
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

    GCAM_commodity <- GCAM_region_ID <- aglu_demand_calyears <- aglu_demand_futureyears <-
      element <- item <- value <- year <- NULL   # silence package check notes

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

    # 2. Build tables
    # NOTE: This is somewhat complicated. Unlike demands in the energy system, the aglu demands are treated as exogenous
    # in all historical periods, regardless of which model base years are used. Processing data for all historical years
    # that are also model years (i.e. not just the model base years)
    aglu_demand_calyears <- HISTORICAL_YEARS[HISTORICAL_YEARS %in% MODEL_YEARS]
    aglu_demand_futureyears <- c(max(aglu_demand_calyears), MODEL_YEARS[!MODEL_YEARS %in% aglu_demand_calyears])


    L134.pcFood_kcald_R_Dmnd_Y %>%
      filter(year %in% aglu_demand_futureyears) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.pcFood_kcald_R_Dmnd_Y.melt

    L134.pcFood_kcald_R_Dmnd_Y_ssp1 %>%
      filter(year %in% aglu_demand_futureyears) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.pcFood_kcald_R_Dmnd_Y_ssp1.melt

    L134.pcFood_kcald_R_Dmnd_Y_ssp2 %>%
      filter(year %in% aglu_demand_futureyears) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.pcFood_kcald_R_Dmnd_Y_ssp2.melt

    L134.pcFood_kcald_R_Dmnd_Y_ssp3 %>%
      filter(year %in% aglu_demand_futureyears) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.pcFood_kcald_R_Dmnd_Y_ssp3.melt

    L134.pcFood_kcald_R_Dmnd_Y_ssp4 %>%
      filter(year %in% aglu_demand_futureyears) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.pcFood_kcald_R_Dmnd_Y_ssp4.melt

    L134.pcFood_kcald_R_Dmnd_Y_ssp5 %>%
      filter(year %in% aglu_demand_futureyears) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.pcFood_kcald_R_Dmnd_Y_ssp5.melt

    L101.Pop_thous_R_Yh %>%
      filter(year %in% aglu_demand_calyears) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.Pop_thous_R_Yh

    L102.pcgdp_thous90USD_Scen_R_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.pcgdp_thous90USD_Scen_R_Y.melt

    # # L203.Supplysector_demand: generic info for demand sectors
    # L203.SectorLogitTables <- get_logit_fn_tables( A_demand_supplysector, names_SupplysectorLogitType,
    #                                                base.header="Supplysector_", include.equiv.table=T, write.all.regions=T )
    # L203.Supplysector_demand <- write_to_all_regions( A_demand_supplysector, names_Supplysector )
    #
    # # L203.SubsectorAll_demand: generic info for demand subsectors
    # L203.SubsectorLogitTables <- get_logit_fn_tables( A_demand_subsector, names_SubsectorLogitType,
    #                                                   base.header="SubsectorLogit_", include.equiv.table=F, write.all.regions=T )
    # L203.SubsectorAll_demand <- write_to_all_regions( A_demand_subsector, names_SubsectorAll )
    #
    # # L203.StubTech_demand: identification of stub technologies for demands
    # L203.StubTech_demand <- write_to_all_regions( A_demand_technology, names_Tech )
    # names( L203.StubTech_demand ) <- gsub( "technology", "stub.technology", names( L203.StubTech_demand ) )

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
      select(supplysector, subsector, technology, minicam.energy.input) %>%
      repeat_add_columns(select(GCAM_region_names, region)) %>%
      mutate(market.name = region, stub.technology = technology) ->
    A_demand_technology_R

    A_demand_technology_R %>%
      repeat_add_columns(tibble(year = aglu_demand_calyears)) ->
      A_demand_technology_R_Yh

    A_demand_technology_R %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      A_demand_technology_R_Y

    L101.ag_Food_Pcal_R_C_Y %>%
      bind_rows(L105.an_Food_Pcal_R_C_Y) %>%
      filter(year %in% aglu_demand_calyears) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.ag_an_Food_Pcal_R_C_Y.melt

    # L203.StubTechProd_food_crop: crop food demand by technology and region
    # L203.StubTechProd_food_meat: meat food demand by technology and region
    A_demand_technology_R_Yh %>%
      filter(supplysector %in% c("FoodDemand_Crops", "FoodDemand_Meat")) %>%
      left_join_error_no_match(L203.ag_an_Food_Pcal_R_C_Y.melt, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(calOutputValue = round(value, aglu.DIGITS_CALOUTPUT),
             share.weight.year = year,
             # Subsector and technology shareweights (subsector requires the year as well)
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(region, supplysector, subsector, stub.technology, year, calOutputValue, share.weight.year, subs.share.weight, tech.share.weight) ->
    L203.StubTechProd_food

    L109.ag_ALL_Mt_R_C_Y %>%
      bind_rows(L109.an_ALL_Mt_R_C_Y) %>%
      filter(year %in% aglu_demand_calyears) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(GCAM_region_ID, region, GCAM_commodity, year, OtherUses_Mt, NetExp_Mt) ->
      L203.ag_an_ALL_Mt_R_C_Y

    # L203.StubTechProd_nonfood_crop: crop nonfood demand by technology and region
    # L203.StubTechProd_nonfood_meat: meat nonfood demand by technology and region
    A_demand_technology_R_Yh %>%
      filter(supplysector %in% c("NonFoodDemand_Crops", "NonFoodDemand_Meat")) %>%
      left_join_error_no_match(L203.ag_an_ALL_Mt_R_C_Y, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(calOutputValue = round(OtherUses_Mt, aglu.DIGITS_CALOUTPUT),
             share.weight.year = year,
             # Subsector and technology shareweights (subsector requires the year as well)
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(region, supplysector, subsector, stub.technology, year, calOutputValue, share.weight.year, subs.share.weight, tech.share.weight) ->
      L203.StubTechProd_nonfood

    # L203.StubTechFixOut_exp: animal exports for net exporting regions in all periods
    A_demand_technology_R_Y %>%
      filter(supplysector == "Exports_Meat") %>%
      # create NAs for future years
      left_join(L203.ag_an_ALL_Mt_R_C_Y, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(fixedOutput = pmax(0, round(NetExp_Mt, aglu.DIGITS_CALOUTPUT)),
             fixedOutput = replace(fixedOutput, year > max(BASE_YEARS), fixedOutput[year == max(BASE_YEARS)]),
             share.weight.year = year,
             # Subsector and technology shareweights (subsector requires the year as well)
             subs.share.weight = 0, tech.share.weight = 0) %>%
      select(region, supplysector, subsector, stub.technology, year, fixedOutput, share.weight.year, subs.share.weight, tech.share.weight) ->
      L203.StubTechFixOut_exp

    L110.For_ALL_bm3_R_Y %>%
      unique() %>%
      filter(year %in% aglu_demand_calyears) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      ungroup() %>%
      select(GCAM_region_ID, region, year, Cons_bm3) ->
      L203.For_ALL_bm3_R_Y

    # L203.StubTechProd_For: Forest product demand by technology and region
    A_demand_technology_R_Yh %>%
      filter(supplysector == "NonFoodDemand_Forest") %>%
      left_join_error_no_match(L203.For_ALL_bm3_R_Y, by = c("region", "year")) %>%
      mutate(calOutputValue = round(Cons_bm3, aglu.DIGITS_CALOUTPUT),
             share.weight.year = year,
             # Subsector and technology shareweights (subsector requires the year as well)
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(region, supplysector, subsector, stub.technology, year, calOutputValue, share.weight.year, subs.share.weight, tech.share.weight) ->
      L203.StubTechProd_For

    L101.ag_kcalg_R_C_Y %>%
      bind_rows(L105.an_kcalg_R_C_Y) %>%
      filter(year %in% aglu_demand_calyears) %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") ->
      L203.ag_an_kcalg_R_C_Y.melt

    # L203.StubCalorieContent_crop: caloric content of food crops (incl secondary products
    # L203.StubCalorieContent_meat: caloric content of meat commodities
    A_demand_technology_R_Y %>%
      filter(supplysector %in% c("FoodDemand_Crops", "FoodDemand_Meat")) %>%
      left_join(L203.ag_an_kcalg_R_C_Y.melt, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(efficiency = round(value, aglu.DIGITS_CALOUTPUT),
             efficiency = replace(efficiency, year > max(BASE_YEARS), efficiency[year == max(BASE_YEARS)])) %>%
      select(region, supplysector, subsector, stub.technology, year, minicam.energy.input, efficiency, market.name) ->
      L203.StubCalorieContent

    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.SectorLogitTables[[ curr_table ]]$data") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_subsector",
                     "aglu/A_demand_technology",
                     "aglu/A_fuelprefElasticity_ssp1",
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
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.SectorLogitTables

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.Supplysector_demand") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_subsector",
                     "aglu/A_demand_technology",
                     "aglu/A_fuelprefElasticity_ssp1",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L101.ag_kcalg_R_C_Y",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L105.an_kcalg_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.Supplysector_demand

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.SubsectorLogitTables[[ curr_table ]]$data") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_subsector",
                     "aglu/A_demand_technology",
                     "aglu/A_fuelprefElasticity_ssp1",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L101.ag_kcalg_R_C_Y",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L105.an_kcalg_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.SubsectorLogitTables

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.SubsectorAll_demand") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_subsector",
                     "aglu/A_demand_technology",
                     "aglu/A_fuelprefElasticity_ssp1",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L101.ag_kcalg_R_C_Y",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L105.an_kcalg_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.SubsectorAll_demand

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubTech_demand") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_subsector",
                     "aglu/A_demand_technology",
                     "aglu/A_fuelprefElasticity_ssp1",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L101.ag_kcalg_R_C_Y",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L105.an_kcalg_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.StubTech_demand

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.GlobalTechCoef_demand") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_subsector",
                     "aglu/A_demand_technology",
                     "aglu/A_fuelprefElasticity_ssp1",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L101.ag_kcalg_R_C_Y",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L105.an_kcalg_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L134.pcFood_kcald_R_Dmnd_Y",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.GlobalTechCoef_demand

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.GlobalTechShrwt_demand") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_subsector",
                     "aglu/A_demand_technology",
                     "aglu/A_fuelprefElasticity_ssp1",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L101.ag_kcalg_R_C_Y",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L105.an_kcalg_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L134.pcFood_kcald_R_Dmnd_Y",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
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
                     "L101.ag_Food_Pcal_R_C_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
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
                     "L105.an_Food_Pcal_R_C_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
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
                     "L109.ag_ALL_Mt_R_C_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
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
                     "L109.an_ALL_Mt_R_C_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.StubTechProd_nonfood_meat

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubTechProd_For") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L110.For_ALL_bm3_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.StubTechProd_For

    L203.StubTechFixOut_exp %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubTechFixOut_exp") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_technology",
                     "L109.an_ALL_Mt_R_C_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.StubTechFixOut_exp

    L203.StubCalorieContent %>%
      filter(supplysector == "FoodDemand_Crops") %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubCalorieContent_crop") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_subsector",
                     "aglu/A_demand_technology",
                     "aglu/A_fuelprefElasticity_ssp1",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L101.ag_kcalg_R_C_Y",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L105.an_kcalg_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.StubCalorieContent_crop

    L203.StubCalorieContent %>%
      filter(supplysector == "FoodDemand_Meat") %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.StubCalorieContent_meat") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_subsector",
                     "aglu/A_demand_technology",
                     "aglu/A_fuelprefElasticity_ssp1",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L101.ag_kcalg_R_C_Y",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L105.an_kcalg_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.StubCalorieContent_meat

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.PerCapitaBased") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_subsector",
                     "aglu/A_demand_technology",
                     "aglu/A_fuelprefElasticity_ssp1",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L101.ag_kcalg_R_C_Y",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L105.an_kcalg_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.PerCapitaBased

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.BaseService") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_subsector",
                     "aglu/A_demand_technology",
                     "aglu/A_fuelprefElasticity_ssp1",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L101.ag_kcalg_R_C_Y",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L105.an_kcalg_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.BaseService

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.IncomeElasticity") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_subsector",
                     "aglu/A_demand_technology",
                     "aglu/A_fuelprefElasticity_ssp1",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L101.ag_kcalg_R_C_Y",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L105.an_kcalg_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L134.pcFood_kcald_R_Dmnd_Y",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.IncomeElasticity

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.PriceElasticity") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_subsector",
                     "aglu/A_demand_technology",
                     "aglu/A_fuelprefElasticity_ssp1",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L101.ag_kcalg_R_C_Y",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L105.an_kcalg_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L134.pcFood_kcald_R_Dmnd_Y",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.PriceElasticity

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.FuelPrefElast_ssp1") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_subsector",
                     "aglu/A_demand_technology",
                     "aglu/A_fuelprefElasticity_ssp1",
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
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.FuelPrefElast_ssp1

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.IncomeElasticity_SSP1") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_subsector",
                     "aglu/A_demand_technology",
                     "aglu/A_fuelprefElasticity_ssp1",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L101.ag_kcalg_R_C_Y",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L105.an_kcalg_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L134.pcFood_kcald_R_Dmnd_Y",
                     "L134.pcFood_kcald_R_Dmnd_Y_ssp1",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.IncomeElasticity_SSP1

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.IncomeElasticity_SSP2") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_subsector",
                     "aglu/A_demand_technology",
                     "aglu/A_fuelprefElasticity_ssp1",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L101.ag_kcalg_R_C_Y",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L105.an_kcalg_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L134.pcFood_kcald_R_Dmnd_Y",
                     "L134.pcFood_kcald_R_Dmnd_Y_ssp2",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.IncomeElasticity_SSP2

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.IncomeElasticity_SSP3") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_subsector",
                     "aglu/A_demand_technology",
                     "aglu/A_fuelprefElasticity_ssp1",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L101.ag_kcalg_R_C_Y",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L105.an_kcalg_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L134.pcFood_kcald_R_Dmnd_Y",
                     "L134.pcFood_kcald_R_Dmnd_Y_ssp3",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.IncomeElasticity_SSP3

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.IncomeElasticity_SSP4") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_subsector",
                     "aglu/A_demand_technology",
                     "aglu/A_fuelprefElasticity_ssp1",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L101.ag_kcalg_R_C_Y",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L105.an_kcalg_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L134.pcFood_kcald_R_Dmnd_Y_ssp4",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.IncomeElasticity_SSP4

    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.IncomeElasticity_SSP5") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_demand_supplysector",
                     "aglu/A_demand_subsector",
                     "aglu/A_demand_technology",
                     "aglu/A_fuelprefElasticity_ssp1",
                     "L101.ag_Food_Pcal_R_C_Y",
                     "L101.ag_kcalg_R_C_Y",
                     "L105.an_Food_Pcal_R_C_Y",
                     "L105.an_kcalg_R_C_Y",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L134.pcFood_kcald_R_Dmnd_Y_ssp5",
                     "L101.Pop_thous_R_Yh",
                     "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L203.IncomeElasticity_SSP5

    # THIS DOESN'T SEEM TO BE FULL, CORRECT RETURN DATA - looks like L203 has a bunch of funkiness in it
    # Take a look at original code and talk to Ben as necessary!
    return_data(L203.SectorLogitTables, L203.Supplysector_demand, L203.SubsectorLogitTables, L203.SubsectorAll_demand, L203.StubTech_demand, L203.GlobalTechCoef_demand, L203.GlobalTechShrwt_demand, L203.StubTechProd_food_crop, L203.StubTechProd_food_meat, L203.StubTechProd_nonfood_crop, L203.StubTechProd_nonfood_meat, L203.StubTechProd_For, L203.StubTechFixOut_exp, L203.StubCalorieContent_crop, L203.StubCalorieContent_meat, L203.PerCapitaBased, L203.BaseService, L203.IncomeElasticity, L203.PriceElasticity, L203.FuelPrefElast_ssp1, L203.IncomeElasticity_SSP1, L203.IncomeElasticity_SSP2, L203.IncomeElasticity_SSP3, L203.IncomeElasticity_SSP4, L203.IncomeElasticity_SSP5)
  } else {
    stop("Unknown command")
  }
}
