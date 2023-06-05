# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2392.gas_trade
#'
#' Model input for regionally and globally traded gas commodities. Traded gas includes LNG and pipeline gas.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs:\code{L2392.Delete_Supplysector_tra_NG},
#'   \code{L2392.Delete_Supplysector_reg_NG}, \code{L2392.PrimaryConsKeyword_en_NG}, \code{L2392.CarbonCoef_NG},
#'   \code{L2392.Supplysector_tra_NG},\code{L2392.SectorUseTrialMarket_tra_NG}, \code{L2392.SubsectorAll_tra_NG},
#'   \code{L2392.TechShrwt_tra_NG}, \code{L2392.TechCost_tra_NG},\code{L2392.TechLifetime_tra_NG},\code{L2392.TechSCurve_tra_NG},
#'   \code{L2392.TechCoef_tra_NG}, \code{L2392.ProfitShutdown_tra_NG},
#'   \code{L2392.Supplysector_reg_NG}, \code{2392.NestingSubsectorAll_reg_NG}, \code{L2392.SubsectorAll_reg_NG},
#'   \code{L2392.TechShrwt_reg_NG},\code{L2392.TechCoef_reg_NG},\code{L2392.TechCost_reg_NG}, \code{L2392.TechLifetime_reg_NG},
#'   \code{L2392.TechSCurve_reg_NG},\code{L2392.ProfitShutdown_reg_NG}, \code{L2392.TechInterp_reg_NG},
#'   \code{L2392.Production_tra_NG}, \code{L2392.Production_reg_imp_NG}, \code{L2392.Production_reg_dom_NG}
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else left_join mutate rename select
#' @importFrom tibble tibble
#' @importFrom stringr str_detect str_extract
#' @author BY August 2021
#'
module_energy_L2392.gas_trade <- function(command, ...) {
if(command == driver.DECLARE_INPUTS) {
  return(c(FILE = "common/GCAM_region_names",
           FILE = "energy/GCAM_region_pipeline_bloc_export", #using this for export pipeline markets (a region can only export to one pipeline)
           FILE = "energy/GCAM_region_pipeline_bloc_import", #using this for import pipeline markets (a region can import from multiple pipelines)
           FILE = "energy/A_ff_RegionalNestingSubsector_NG",
           FILE = "energy/A_ff_RegionalSubsector_NG",
           FILE = "energy/A_ff_RegionalTechnology_NG",
           FILE = "energy/A_ff_TradedSector_NG",
           FILE = "energy/A_ff_TradedSubsector_NG",
           FILE = "energy/A_ff_TradedTechnology_NG",
           FILE = "energy/A_ff_RegionalTechnologyCost_NG",
           FILE = "energy/A_ff_TradedTechnologyCost_NG",
           "L202.CarbonCoef",
           "L239.Supplysector_tra",
           "L239.Supplysector_reg",
           "L2391.NG_export_calOutput_LNG",
           "L2391.NG_export_calOutput_pipeline",
           "L2391.NG_import_calOutput_LNG",
           "L2391.NG_import_calOutput_pipeline",
           "L2391.NG_import_calOutput_statdiff",
           "L2391.NG_export_calOutput_statdiff",
           "L239.Production_reg_dom"))
} else if(command == driver.DECLARE_OUTPUTS) {
  return(c("L2392.PrimaryConsKeyword_en_NG",
           "L2392.CarbonCoef_NG",
           "L2392.Supplysector_tra_NG",
           "L2392.SectorUseTrialMarket_tra_NG",
           "L2392.SubsectorAll_tra_NG",
           "L2392.TechShrwt_tra_NG",
           "L2392.TechCost_tra_NG",
           "L2392.TechLifetime_tra_NG",
           "L2392.TechSCurve_tra_NG",
           "L2392.ProfitShutdown_tra_NG",
           "L2392.TechCoef_tra_NG",
           "L2392.Supplysector_reg_NG",
           "L2392.NestingSubsectorAll_reg_NG",
           "L2392.SubsectorAll_reg_NG",
           "L2392.TechShrwt_reg_NG",
           "L2392.TechCoef_reg_NG",
           "L2392.TechCost_reg_NG",
           "L2392.TechLifetime_reg_NG",
           "L2392.TechSCurve_reg_NG",
           "L2392.ProfitShutdown_reg_NG",
           "L2392.TechInterp_reg_NG",
           "L2392.Delete_Supplysector_tra_NG",
           "L2392.Delete_Supplysector_reg_NG",
           "L2392.Production_tra_NG",
           "L2392.Production_reg_imp_NG",
           "L2392.Production_reg_dom_NG"))
} else if(command == driver.MAKE) {

  all_data <- list(...)[[1]]

  # Load required inputs
  GCAM_region_names <- get_data(all_data, "common/GCAM_region_names",strip_attributes = TRUE)
  GCAM_region_pipeline_bloc_export <- get_data(all_data, "energy/GCAM_region_pipeline_bloc_export", strip_attributes = TRUE)
  GCAM_region_pipeline_bloc_import <- get_data(all_data, "energy/GCAM_region_pipeline_bloc_import", strip_attributes = TRUE)
  A_ff_RegionalNestingSubsector_NG <- get_data(all_data, "energy/A_ff_RegionalNestingSubsector_NG",strip_attributes = TRUE)
  A_ff_RegionalSubsector_NG <- get_data(all_data, "energy/A_ff_RegionalSubsector_NG",strip_attributes = TRUE)
  A_ff_RegionalTechnology_NG <- get_data(all_data, "energy/A_ff_RegionalTechnology_NG",strip_attributes = TRUE)
  A_ff_TradedSector_NG <- get_data(all_data, "energy/A_ff_TradedSector_NG",strip_attributes = TRUE)
  A_ff_TradedSubsector_NG <- get_data(all_data, "energy/A_ff_TradedSubsector_NG",strip_attributes = TRUE)
  A_ff_TradedTechnology_NG <- get_data(all_data, "energy/A_ff_TradedTechnology_NG", strip_attributes = TRUE)
  A_ff_RegionalTechnologyCost_NG <- get_data(all_data, "energy/A_ff_RegionalTechnologyCost_NG", strip_attributes = TRUE)
  A_ff_TradedTechnologyCost_NG <- get_data(all_data, "energy/A_ff_TradedTechnologyCost_NG", strip_attributes = TRUE)
  L202.CarbonCoef <- get_data(all_data, "L202.CarbonCoef",strip_attributes = TRUE)
  L239.Supplysector_tra <- get_data(all_data, "L239.Supplysector_tra", strip_attributes = TRUE)
  L239.Supplysector_reg <- get_data(all_data, "L239.Supplysector_reg", strip_attributes = TRUE)
  L2391.NG_export_calOutput_LNG <- get_data(all_data, "L2391.NG_export_calOutput_LNG", strip_attributes = TRUE)
  L2391.NG_export_calOutput_pipeline <- get_data(all_data, "L2391.NG_export_calOutput_pipeline", strip_attributes = TRUE)
  L2391.NG_import_calOutput_LNG <- get_data(all_data, "L2391.NG_import_calOutput_LNG", strip_attributes = TRUE)
  L2391.NG_import_calOutput_pipeline <- get_data(all_data, "L2391.NG_import_calOutput_pipeline", strip_attributes = TRUE)
  L2391.NG_import_calOutput_statdiff <- get_data(all_data, "L2391.NG_import_calOutput_statdiff", strip_attributes = TRUE)
  L2391.NG_export_calOutput_statdiff <- get_data(all_data, "L2391.NG_export_calOutput_statdiff", strip_attributes = TRUE)
  L239.Production_reg_dom <- get_data(all_data, "L239.Production_reg_dom", strip_attributes = TRUE)

  # Delete natural gas from regional and traded supplysectors
  L2392.Delete_Supplysector_tra_NG <- L239.Supplysector_tra %>%
    filter(supplysector == "traded natural gas")

  L2392.Delete_Supplysector_reg_NG <- L239.Supplysector_reg %>%
    filter(supplysector == "regional natural gas")


  # Keywords of global technologies for NG (which has nesting subsectors)
  # process domestic natural gas and LNG which are mapped to all regions
  A_ff_RegionalTechnology_NG %>%
    filter(!is.na(primary.consumption), subsector %in% c("domestic natural gas", "imported LNG")) %>%
    repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
    select(supplysector, subsector0, subsector, technology, primary.consumption, year) %>%
    filter(year %in% MODEL_YEARS) %>%
    repeat_add_columns(GCAM_region_names) %>%
    select(-GCAM_region_ID)-> L2392.PrimaryConsKeyword_en_domestic_LNG


  # process pipeline gas; each region is mapped to a specific set of pipelines only
  # determined by the mapping in GCAM_region_pipeline_bloc_import
  # some regions (e.g. Central Asia) may import from multiple pipelines (Afr_MidE and RUS)
  A_ff_RegionalTechnology_NG %>%
    filter(!is.na(primary.consumption), subsector %in% c("imported pipeline gas")) %>%
    repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
    select(supplysector, subsector0, subsector, technology, primary.consumption, year) %>%
    filter(year %in% MODEL_YEARS) %>%
    repeat_add_columns(GCAM_region_names) %>%
    full_join(GCAM_region_pipeline_bloc_import, by = c("region")) %>%
    # keep only the rows for which technology matches the region-specific pipeline
    filter(stringr::str_detect(technology, pipeline.market)) %>%
    select(supplysector, subsector0, subsector, technology, primary.consumption, year, region)-> L2392.PrimaryConsKeyword_en_pipeline

  #bind all natural gas rows back together
  L2392.PrimaryConsKeyword_en_NG <- bind_rows(L2392.PrimaryConsKeyword_en_domestic_LNG,
                                              L2392.PrimaryConsKeyword_en_pipeline)

  # Fuel carbon coefficients for new sectors
  # Traded sectors first.  These are only set up in the USA.
  # NOTE: not including regional NG as this is already included in L202.CarbonCoef
  L202.CarbonCoef %>%
    filter(region == gcam.USA_REGION) %>%
    semi_join(A_ff_TradedTechnology_NG, by = c("PrimaryFuelCO2Coef.name" = "minicam.energy.input")) %>%
    #using left_join because there are now several NG technologies (in A_ff_TradedTechnology) rather than only one (from L202.CarbonCoef).
    left_join(A_ff_TradedTechnology_NG, by = c("PrimaryFuelCO2Coef.name" = "minicam.energy.input")) %>%
    select(region, PrimaryFuelCO2Coef.name = supplysector, PrimaryFuelCO2Coef, -PrimaryFuelCO2Coef.name) %>%
    # use anti_join to remove entries already in L202.CarbonCoef
    anti_join(L202.CarbonCoef, by = c("region", "PrimaryFuelCO2Coef.name", "PrimaryFuelCO2Coef")) -> L2392.CarbonCoef_NG

  # 1. TRADED SECTOR / SUBSECTOR / TECHNOLOGY")
  # L2392.Supplysector_tra: generic supplysector info for traded natural gas commodities
  # By convention, traded commodity information is contained within the USA region (could be within any)
  A_ff_TradedSector_NG$region <- gcam.USA_REGION

  # L2392.Supplysector_tra_NG: generic supplysector info for traded natural gas commodities
  L2392.Supplysector_tra_NG <- mutate(A_ff_TradedSector_NG, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
    select(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"))

  # L2392.SectorUseTrialMarket_tra_NG: Create solved markets for the traded sectors
  L2392.SectorUseTrialMarket_tra_NG <- select(A_ff_TradedSector_NG, region, supplysector) %>%
    mutate(use.trial.market = 1)

  # L2392.SubsectorAll_tra_NG: generic subsector info for traded natural gas commodities
  # Traded commodities have the region set to USA and the subsector gets the region name pre-pended
  # process LNG which is mapped to all regions
  L2392.SubsectorAll_tra_LNG <- A_ff_TradedSubsector_NG %>%
    filter(stringr::str_detect(subsector, "traded LNG")) %>%
    write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorAllTo"]], "logit.type"),
                                                GCAM_region_names,
                                                has_traded = TRUE)

  # process pipeline gas; each region is mapped to a specific pipeline only
  L2392.SubsectorAll_tra_pipeline <- A_ff_TradedSubsector_NG %>%
    filter(stringr::str_detect(subsector, "pipeline gas")) %>%
    write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorAllTo"]], "logit.type"),
                         GCAM_region_names,
                         has_traded = TRUE) %>%
    mutate(subsector_region = stringr::str_extract(subsector, "^.*(?= traded)")) %>%
    left_join_error_no_match(GCAM_region_pipeline_bloc_export, by = c("subsector_region" = "origin.region")) %>%
    # keep only the rows for which the technology matches the region-specific pipeline
    filter(stringr::str_detect(subsector, pipeline.market)) %>%
    select(-subsector_region, -pipeline.market)

  # process statistical differences which exist only in the base year
  # shareweight is set to 0 in first model year and filled out (carried forward)
  L2392.SubsectorAll_tra_stat_diff_exports <- A_ff_TradedSubsector_NG %>%
    filter(supplysector == "gas trade statistical differences") %>%
    write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorAllTo"]], "logit.type"),
                         GCAM_region_names,
                         has_traded = TRUE) %>%
    mutate(subsector_region = stringr::str_extract(subsector, "^.*(?= statistical differences)")) %>%
    right_join(L2391.NG_export_calOutput_statdiff, by = c("region", "supplysector" = "sector", "subsector" = "technology",
                                                          "subsector_region" = "market.name")) %>%
    select(c(LEVEL2_DATA_NAMES[["SubsectorAllTo"]], "logit.type")) %>%
    distinct() %>%
    mutate(year.fillout = min(MODEL_FUTURE_YEARS))

  L2392.SubsectorAll_tra_stat_diff_imports <- A_ff_TradedSubsector_NG %>%
    filter(subsector == "statistical differences",
           supplysector != "gas trade statistical differences") %>%
    write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorAllTo"]], "logit.type"),
                         filter(GCAM_region_names, region == gcam.USA_REGION),
                         has_traded = FALSE) %>%
    mutate(year.fillout = min(MODEL_FUTURE_YEARS))


  # bind all natural gas tables together
  L2392.SubsectorAll_tra_NG <- bind_rows(L2392.SubsectorAll_tra_LNG,
                                         L2392.SubsectorAll_tra_pipeline,
                                         L2392.SubsectorAll_tra_stat_diff_exports,
                                         L2392.SubsectorAll_tra_stat_diff_imports)

  # Base technology-level table for several tables to be written out")
  # process LNG which is mapped to all regions
  A_ff_TradedTechnology_R_Y_LNG <- repeat_add_columns(A_ff_TradedTechnology_NG,
                                                  tibble(year = MODEL_YEARS)) %>%
    filter(subsector == "traded LNG") %>%
    repeat_add_columns(GCAM_region_names) %>%
    mutate(subsector = paste(region, subsector, sep = " "),
           technology = subsector,
           market.name = region,
           region = gcam.USA_REGION)

  # process pipeline gas, each region is mapped to a specific pipeline only
  A_ff_TradedTechnology_R_Y_pipeline <- repeat_add_columns(A_ff_TradedTechnology_NG,
                                                        tibble(year = MODEL_YEARS)) %>%
      filter(stringr::str_detect(supplysector, "pipeline gas")) %>%
      repeat_add_columns(GCAM_region_names) %>%
      left_join_error_no_match(GCAM_region_pipeline_bloc_export, by = c("region" = "origin.region")) %>%
      # keep only the rows for which the technology matches the region-specific pipeline
      # this will include statistical differences (imports) which match the region-specific pipeline
      filter(stringr::str_detect(technology, pipeline.market)) %>%
      select(-pipeline.market) %>%
      mutate(subsector = paste(region, subsector, sep = " "),
             technology = subsector,
             market.name = region,
             region = gcam.USA_REGION)

  # process pipeline statistical differences (imports) each region is mapped to a specific pipeline only
  A_ff_TradedTechnology_R_Y_pipeline_stat_diff_imports <- repeat_add_columns(A_ff_TradedTechnology_NG,
                                                           tibble(year = MODEL_YEARS)) %>%
    filter(stringr::str_detect(supplysector, "pipeline gas"),
           technology == "statistical differences") %>%
    repeat_add_columns(GCAM_region_names) %>%
    left_join_error_no_match(GCAM_region_pipeline_bloc_export, by = c("region" = "origin.region")) %>%
    # keep only the rows for which the supplysector matches the pipeline bloc
    filter(stringr::str_detect(supplysector, pipeline.market)) %>%
    mutate(technology = subsector,
           market.name = gcam.USA_REGION,
           region = gcam.USA_REGION,
           GCAM_region_ID = gcam.USA_CODE,
           #set shareweight to zero for future years
           share.weight = if_else(year > MODEL_FINAL_BASE_YEAR, 0, 1)) %>%
    distinct()

  # process LNG statistical differences (imports)
  A_ff_TradedTechnology_R_Y_LNG_stat_diff_imports <- repeat_add_columns(A_ff_TradedTechnology_NG,
                                                                             tibble(year = MODEL_YEARS)) %>%
    filter(supplysector == "traded LNG",
           technology == "statistical differences") %>%
    mutate(technology = subsector,
           market.name = gcam.USA_REGION,
           region = gcam.USA_REGION,
           GCAM_region_ID = gcam.USA_CODE,
           #set shareweight to zero for future years
           share.weight = if_else(year > MODEL_FINAL_BASE_YEAR, 0, 1)) %>%
    distinct()

  #Get the pipeline markets that are included in the statistical differences (imports)
  #Use this to filter the full A_ff_TradedTechnology_R_Y_stat_diff_imports table to only the pipelines that exist
  NG_import_calOutput_statdiff_pipelines <- L2391.NG_import_calOutput_statdiff %>%
      select(-calOutputValue, -year) %>%
      distinct()

  A_ff_TradedTechnology_R_Y_stat_diff_imports <- bind_rows(A_ff_TradedTechnology_R_Y_pipeline_stat_diff_imports,
                                                           A_ff_TradedTechnology_R_Y_LNG_stat_diff_imports) %>%
    right_join(NG_import_calOutput_statdiff_pipelines, by = c("technology", "minicam.energy.input", "market.name", "pipeline.market")) %>%
    select(colnames(A_ff_TradedTechnology_R_Y_pipeline_stat_diff_imports))

  # process statistical differences (exports) which exist only in the base year

  #Get the pipeline markets that are included in the statistical differences (imports)
  #Use this to filter the full A_ff_TradedTechnology_R_Y_stat_diff_imports table to only the regions that exist
  NG_export_calOutput_statdiff_regions <- L2391.NG_export_calOutput_statdiff %>%
    select(-calOutputValue, -year, -subsector) %>%
    distinct()

  A_ff_TradedTechnology_R_Y_stat_diff_exports <- repeat_add_columns(A_ff_TradedTechnology_NG,
                                                      tibble(year = MODEL_YEARS)) %>%
    filter(supplysector == "gas trade statistical differences") %>%
    repeat_add_columns(GCAM_region_names) %>%
    mutate(subsector = paste(region, subsector, sep = " "),
           technology = subsector,
           market.name = region,
           region = gcam.USA_REGION,
           #set shareweight to zero for future years
           share.weight = if_else(year > MODEL_FINAL_BASE_YEAR, 0, 1)) %>%
    right_join(NG_export_calOutput_statdiff_regions, by = c("region", "technology", "minicam.energy.input", "market.name")) %>%
    # select column names which should be the same as other A_ff_TradedTechnology tables
    select(colnames(A_ff_TradedTechnology_R_Y_LNG))

  # bind all natural gas tables together
  A_ff_TradedTechnology_R_Y_NG <- bind_rows(A_ff_TradedTechnology_R_Y_LNG,
                                            A_ff_TradedTechnology_R_Y_pipeline,
                                            select(A_ff_TradedTechnology_R_Y_pipeline_stat_diff_imports, -pipeline.market),
                                            A_ff_TradedTechnology_R_Y_LNG_stat_diff_imports,
                                            A_ff_TradedTechnology_R_Y_stat_diff_exports)

  # L2392.TechShrwt_tra_NG: Share-weights of traded technologies
  L2392.TechShrwt_tra_NG <- select(A_ff_TradedTechnology_R_Y_NG, LEVEL2_DATA_NAMES[["TechShrwt"]])

  # L2392.TechCost_tra_NG: Costs of traded technologies
  A_ff_TradedTechnologyCost_NG_long <- A_ff_TradedTechnologyCost_NG %>%
    gather_years()

  A_ff_TradedTechnologyCost_NG_interp <- A_ff_TradedTechnologyCost_NG_long %>%
    select(-year, -value) %>%
    distinct() %>%
    repeat_add_columns(tibble::as_tibble(MODEL_YEARS)) %>%
    rename(year = value) %>%
    # left join as this will create new rows - some techs have multiple costs
    left_join(A_ff_TradedTechnologyCost_NG_long, by = c("supplysector", "subsector", "technology", "cost.component", "year")) %>%
    group_by(supplysector, subsector, technology, cost.component) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(value = approx_fun(year, value, rule = 1)) %>%
    ungroup()

  L2392.TechCost_tra_NG <- A_ff_TradedTechnology_R_Y_NG %>%
    select(region, supplysector, subsector, technology) %>%
    distinct() %>%
    full_join(select(A_ff_TradedTechnologyCost_NG_interp, -subsector, -technology), by = "supplysector") %>%
    mutate(minicam.non.energy.input = paste(cost.component, " cost"),
           input.cost = value) %>%
    select(LEVEL2_DATA_NAMES[["TechCost"]])

  # L2392.TechCoef_tra_NG: Coefficient and market name of traded technologies
  L2392.TechCoef_tra_NG <- select(A_ff_TradedTechnology_R_Y_NG, LEVEL2_DATA_NAMES[["TechCoef"]])

  # L2392.TechLifetime_tra_NG: Lifetime of traded technologies
  L2392.TechLifetime_tra_NG <- A_ff_TradedTechnology_R_Y_NG %>%
    select(LEVEL2_DATA_NAMES[["TechLifetime"]]) %>%
    filter(year >= max(MODEL_BASE_YEARS),
           !(stringr::str_detect(technology, "statistical differences")))

  # L2392.TechSCurve_tra_NG: S-curve retirement function for base year traded technologies
  L2392.TechSCurve_tra_NG <- A_ff_TradedTechnology_R_Y_NG %>%
    select(LEVEL2_DATA_NAMES[["TechSCurve"]]) %>%
    filter(year == max(MODEL_BASE_YEARS),
           !(stringr::str_detect(technology, "statistical differences")))

  # L2392.ProfitShutdown_tra_NG: Profit shutdown decider for traded technologies
  L2392.ProfitShutdown_tra_NG <- select(A_ff_TradedTechnology_R_Y_NG, LEVEL2_DATA_NAMES[["TechProfitShutdown"]]) %>%
    filter(!(stringr::str_detect(technology, "statistical differences")))

  # L2392.Production_tra: Output (gross exports) of traded technologies
  # LNG
   L2392.Production_tra_LNG <- filter(A_ff_TradedTechnology_R_Y_LNG, year %in% MODEL_BASE_YEARS) %>%
    left_join_error_no_match(L2391.NG_export_calOutput_LNG,
              by = c(market.name = "region", minicam.energy.input = "GCAM_Commodity", "year")) %>%
     mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
            share.weight.year = year,
            subs.share.weight = if_else(calOutputValue > 0, 1, 0),
            tech.share.weight = subs.share.weight) %>%
     select(LEVEL2_DATA_NAMES[["Production"]])

   # pipeline
   L2392.Production_tra_pipeline <- filter(A_ff_TradedTechnology_R_Y_pipeline, year %in% MODEL_BASE_YEARS) %>%
     left_join_error_no_match(L2391.NG_export_calOutput_pipeline,
                              by = c(market.name = "region", minicam.energy.input = "GCAM_Commodity", "year")) %>%
     mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
            share.weight.year = year,
            subs.share.weight = if_else(calOutputValue > 0, 1, 0),
            tech.share.weight = subs.share.weight) %>%
     select(LEVEL2_DATA_NAMES[["Production"]])

   # statistical differences (exports)
   L2392.Production_tra_stat_diff_exports <- filter(A_ff_TradedTechnology_R_Y_stat_diff_exports, year %in% MODEL_BASE_YEARS) %>%
     #using left_join because there are several regions that do not have statistical difference exports and will thus be NA
     #removing NAs in the next step
     left_join(select(L2391.NG_export_calOutput_statdiff, -subsector),
                              by = c("region", supplysector = "sector", "technology",
                                     "market.name", "minicam.energy.input", "year")) %>%
     na.omit() %>%
     mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
            share.weight.year = year,
            subs.share.weight = if_else(calOutputValue > 0, 1, 0),
            tech.share.weight = subs.share.weight) %>%
     select(LEVEL2_DATA_NAMES[["Production"]])

   # statistical differences (imports)
   L2392.Production_tra_stat_diff_imports <- filter(A_ff_TradedTechnology_R_Y_stat_diff_imports, year %in% MODEL_BASE_YEARS) %>%
     left_join_error_no_match(L2391.NG_import_calOutput_statdiff,
               by = c("technology","market.name", "minicam.energy.input", "year", "pipeline.market")) %>%
     mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
            share.weight.year = year,
            subs.share.weight = if_else(calOutputValue > 0, 1, 0),
            tech.share.weight = subs.share.weight) %>%
     select(LEVEL2_DATA_NAMES[["Production"]])


   L2392.Production_tra_NG <- bind_rows(L2392.Production_tra_LNG,
                                     L2392.Production_tra_pipeline,
                                     L2392.Production_tra_stat_diff_exports,
                                     L2392.Production_tra_stat_diff_imports)


  # PART 2: DOMESTIC SUPPLY SECTOR / SUBSECTOR / TECHNOLOGY")
  # L2392.Supplysector_reg_NG: generic supplysector info for regional natural gas commodities
  L2392.Supplysector_reg_NG <- L239.Supplysector_reg %>%
    filter(supplysector == "regional natural gas")

  # L2392.SubsectorAll_reg_NG: generic subsector info for regional natural gas  commodities (competing domestic vs imported)
  L2392.NestingSubsectorAll_reg_NG <- write_to_all_regions(A_ff_RegionalNestingSubsector_NG,
                                                    c(LEVEL2_DATA_NAMES[["SubsectorAllTo"]], "logit.type"),
                                                    GCAM_region_names)

  # L2392.SubsectorAll_reg_NG: generic subsector info for regional natural gas  commodities (competing imported LNG vs. pipeline)
  L2392.SubsectorAll_reg_NG <- write_to_all_regions(A_ff_RegionalSubsector_NG,
                                                c(LEVEL2_DATA_NAMES[["SubsectorAllTo"]],"subsector0", "logit.type"),
                                                GCAM_region_names)

  # Base technology-level table for several tables to be written out")
  # process domestic natural gas and LNG which are mapped to all regions
  A_ff_RegionalTechnology_R_Y_domestic_LNG <- A_ff_RegionalTechnology_NG %>%
    filter(subsector %in% c("domestic natural gas", "imported LNG")) %>%
    repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
    repeat_add_columns(GCAM_region_names["region"]) %>%
    mutate(market.name = if_else(market.name == "regional", region, market.name)) %>%
    set_years()

  # process pipeline gas; each region is mapped to a specific set of pipelines only
  # determined by the mapping in GCAM_region_pipeline_bloc_import
  # some regions (e.g. Central Asia) may import from multiple pipelines (Afr_MidE and RUS)
  A_ff_RegionalTechnology_R_Y_pipeline <- A_ff_RegionalTechnology_NG %>%
    filter(subsector %in% c("imported pipeline gas")) %>%
    repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
    repeat_add_columns(GCAM_region_names["region"]) %>%
    mutate(market.name = if_else(market.name == "regional", region, market.name)) %>%
    full_join(GCAM_region_pipeline_bloc_import, by = c("region")) %>%
    # keep only the rows for which pipeline the technology matches the region-specific pipeline
    filter(stringr::str_detect(technology, pipeline.market)) %>%
    set_years()


  # bind all natural gas tables together
  A_ff_RegionalTechnology_R_Y_NG <- bind_rows(A_ff_RegionalTechnology_R_Y_domestic_LNG,
                                              (select(A_ff_RegionalTechnology_R_Y_pipeline, -pipeline.market)))

  # L2392.TechShrwt_reg_NG: Share-weights of regional technologies
  L2392.TechShrwt_reg_NG <- select(A_ff_RegionalTechnology_R_Y_NG, c(LEVEL2_DATA_NAMES[["TechShrwt"]], "subsector0"))

  # L2392.TechCoef_reg_NG: Coefficient and market name of regional technologies
  L2392.TechCoef_reg_NG <- select(A_ff_RegionalTechnology_R_Y_NG, c(LEVEL2_DATA_NAMES[["TechCoef"]],"subsector0"))

  # L2392.TechInterp_reg_NG : Interpolation rules for regional technologies
  L2392.TechInterp_reg_NG <- select(A_ff_RegionalTechnology_R_Y_NG, c(LEVEL2_DATA_NAMES[["TechInterpTo"]], "subsector0"))

  # L2392.TechCost_reg_NG: Costs of regional technologies
  A_ff_RegionalTechnologyCost_NG_long <- A_ff_RegionalTechnologyCost_NG %>%
    gather_years()

  A_ff_RegionalTechnologyCost_NG_interp <- A_ff_RegionalTechnologyCost_NG_long %>%
    select(-year, -value) %>%
    distinct() %>%
    repeat_add_columns(tibble::as_tibble(MODEL_YEARS)) %>%
    rename(year = value) %>%
    # left join as this may create new rows - some techs may have multiple costs
    left_join(A_ff_RegionalTechnologyCost_NG_long, by = c("supplysector", "subsector0", "subsector", "technology", "cost.component", "year")) %>%
    group_by(supplysector, subsector0, subsector, technology, cost.component) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(value = approx_fun(year, value, rule = 1)) %>%
    ungroup()

  L2392.TechCost_reg_NG <- A_ff_RegionalTechnology_R_Y_NG %>%
    select(region, supplysector, subsector0, subsector, technology) %>%
    distinct() %>%
    full_join(A_ff_RegionalTechnologyCost_NG_interp, by = c("supplysector", "subsector0", "subsector", "technology")) %>%
    # domestic natural gas has no costs, omit these rows
    na.omit() %>%
    mutate(minicam.non.energy.input = paste(cost.component, " cost"),
           input.cost = value) %>%
    select(LEVEL2_DATA_NAMES[["TechCost"]],"subsector0")

  # L2392.TechLifetime_reg_NG: Lifetime of regional technologies
  L2392.TechLifetime_reg_NG <- A_ff_RegionalTechnology_R_Y_NG %>%
    select(LEVEL2_DATA_NAMES[["TechLifetime"]], "subsector0") %>%
    filter(year >= max(MODEL_BASE_YEARS))

  # L2392.TechSCurve_reg_NG: S-curve retirement function for base year regional technologies
  L2392.TechSCurve_reg_NG <- A_ff_RegionalTechnology_R_Y_NG %>%
    select(LEVEL2_DATA_NAMES[["TechSCurve"]], "subsector0") %>%
    filter(year == max(MODEL_BASE_YEARS))

  # L2392.TechSCurve_reg_NG: profit shutdown decider for regional technologies
  L2392.ProfitShutdown_reg_NG <- select(A_ff_RegionalTechnology_R_Y_NG, LEVEL2_DATA_NAMES[["TechProfitShutdown"]], "subsector0")

  # L2392.Production_reg_imp: Output (flow) of gross imports
  #LNG
  L2392.Production_reg_imp_LNG <- A_ff_RegionalTechnology_R_Y_NG %>%
    filter(year %in% MODEL_BASE_YEARS,
           subsector == "imported LNG") %>%
    left_join_error_no_match(L2391.NG_import_calOutput_LNG,
                             by = c("region", "year")) %>%
    mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
           share.weight.year = year,
           subs.share.weight = if_else(calOutputValue > 0, 1, 0),
           tech.share.weight = subs.share.weight) %>%
    select(LEVEL2_DATA_NAMES[["Production"]], "subsector0")

  #pipeline
  L2392.Production_reg_imp_pipeline <- A_ff_RegionalTechnology_R_Y_pipeline %>%
    filter(year %in% MODEL_BASE_YEARS) %>%
    left_join_error_no_match(L2391.NG_import_calOutput_pipeline,
                             by = c("region", "year", "pipeline.market")) %>%
    mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
           share.weight.year = year,
           subs.share.weight = if_else(calOutputValue > 0, 1, 0),
           tech.share.weight = subs.share.weight) %>%
    select(LEVEL2_DATA_NAMES[["Production"]], "subsector0")

  L2392.Production_reg_imp_NG <- bind_rows(L2392.Production_reg_imp_LNG,
                                           L2392.Production_reg_imp_pipeline)

  L2392.Production_reg_dom_NG <- L239.Production_reg_dom %>%
    filter(supplysector == "regional natural gas") %>%
    mutate(subsector0 = subsector)

  # Produce outputs
  L2392.Delete_Supplysector_tra_NG %>%
    add_title("Delete traded natural gas supplysector") %>%
    add_units("none") %>%
    add_comments("When running with detailed natural gas trade disaggregation (pipeline and LNG), delete original NG supplysectors") %>%
    add_precursors("L239.Supplysector_tra") ->
    L2392.Delete_Supplysector_tra_NG

  L2392.Delete_Supplysector_reg_NG %>%
    add_title("Delete regional natural gas supplysectors") %>%
    add_units("none") %>%
    add_comments("When running with detailed natural gas trade disaggregation (pipeline and LNG), delete original NG supplysectors") %>%
    add_precursors("L239.Supplysector_reg") ->
    L2392.Delete_Supplysector_reg_NG

  L2392.PrimaryConsKeyword_en_NG %>%
    add_title("Keywords of global natural gas technologies") %>%
    add_units("unitless") %>%
    add_comments("Primary consumption from A_ff_RegionalTechnology_NG written to all model periods") %>%
    add_precursors("energy/A_ff_RegionalTechnology_NG",
                   "energy/GCAM_region_pipeline_bloc_import",
                   "common/GCAM_region_names") ->
    L2392.PrimaryConsKeyword_en_NG

  L2392.CarbonCoef_NG %>%
    add_title("Primary Energy CO2 Coefficient for natural gas trade sectors") %>%
    add_units("kgC/GJ") %>%
    add_comments("All coefficients mapped from relevant fuels in L202.CarbonCoef") %>%
    add_precursors("energy/A_ff_TradedTechnology_NG",
                   "energy/A_ff_RegionalTechnology_NG",
                   "L202.CarbonCoef") ->
    L2392.CarbonCoef_NG

  L2392.Supplysector_tra_NG %>%
    add_title("Supplysector info for traded natural gas commodities") %>%
    add_units("None") %>%
    add_comments("Supplysector info for traded natural gas commodities") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_TradedSector_NG") ->
    L2392.Supplysector_tra_NG

  L2392.SectorUseTrialMarket_tra_NG %>%
    add_title("Supplysector flag indicating to make trial markets") %>%
    add_units("None") %>%
    add_comments("This helps model solution when running with ff trade") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_TradedSector_NG") ->
    L2392.SectorUseTrialMarket_tra_NG

  L2392.SubsectorAll_tra_NG %>%
    add_title("Subsector info for traded natural gas commodities") %>%
    add_units("None") %>%
    add_comments("Subsector info for traded natural gas commodities") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_TradedSubsector_NG",
                   "energy/GCAM_region_pipeline_bloc_export") ->
    L2392.SubsectorAll_tra_NG

  L2392.TechShrwt_tra_NG %>%
    add_title("Technology share-weights for traded natural gas commodities") %>%
    add_units("None") %>%
    add_comments("Technology share-weights for traded natural gas commodities") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_TradedTechnology_NG",
                   "energy/GCAM_region_pipeline_bloc_export") ->
    L2392.TechShrwt_tra_NG

  L2392.TechCost_tra_NG %>%
    add_title("Technology costs for traded natural gas commodities") %>%
    add_units("1975$/GJ") %>%
    add_comments("Exogenous costs to reflect liquefaction and shipping for LNG, pipeline transport costs for pipeline gas") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_TradedTechnologyCost_NG",
                   "energy/A_ff_TradedTechnology_NG",
                   "energy/GCAM_region_pipeline_bloc_export") ->
    L2392.TechCost_tra_NG

  L2392.TechLifetime_tra_NG %>%
    add_title("Lifetimes for traded natural gas commodities") %>%
    add_units("NA") %>%
    add_comments("Lifetimes for traded NG commodities") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_TradedTechnology_NG",
                   "energy/GCAM_region_pipeline_bloc_export") ->
    L2392.TechLifetime_tra_NG

  L2392.TechSCurve_tra_NG %>%
    add_title("S-curve retirement function for traded natural gas commodities (base year vintage only)") %>%
    add_units("NA") %>%
    add_comments("S-curve for traded NG commodities") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_TradedTechnology_NG",
                   "energy/GCAM_region_pipeline_bloc_export") ->
    L2392.TechSCurve_tra_NG

  L2392.ProfitShutdown_tra_NG %>%
    add_title("Profit shutdown decider for traded natural gas commodities") %>%
    add_units("NA") %>%
    add_comments("Profit shutdown decider for traded NG commodities") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_TradedTechnology_NG",
                   "energy/GCAM_region_pipeline_bloc_export") ->
    L2392.ProfitShutdown_tra_NG

  L2392.TechCoef_tra_NG %>%
    add_title("Technology input-output coefficients for traded natural gas commodities") %>%
    add_units("Unitless IO") %>%
    add_comments("Pass-through; 1 unless some portion is assumed lost/spoiled in shipping") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_TradedTechnology_NG",
                   "energy/GCAM_region_pipeline_bloc_export") ->
    L2392.TechCoef_tra_NG

  L2392.Supplysector_reg_NG %>%
    add_title("Supplysector info for regional natural gas commodities") %>%
    add_units("None") %>%
    add_comments("Supplysector info for traded natural gas commodities, filtered from L239.Supplysector_reg") %>%
    add_precursors("L239.Supplysector_reg") ->
    L2392.Supplysector_reg_NG

  L2392.NestingSubsectorAll_reg_NG %>%
    add_title("Nesting Subsector info for regional natural gas commodities") %>%
    add_units("None") %>%
    add_comments("Subsector info for regional natural gas commodities") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_RegionalNestingSubsector_NG",
                   "energy/GCAM_region_pipeline_bloc_import") ->
    L2392.NestingSubsectorAll_reg_NG

  L2392.SubsectorAll_reg_NG %>%
    add_title("Subsector info for regional natural gas commodities") %>%
    add_units("None") %>%
    add_comments("Subsector info for regional natural gas commodities") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_RegionalSubsector_NG",
                   "energy/GCAM_region_pipeline_bloc_import") ->
    L2392.SubsectorAll_reg_NG

  L2392.TechShrwt_reg_NG %>%
    add_title("Technology share-weights for regional natural gas commodities") %>%
    add_units("None") %>%
    add_comments("Technology share-weights for regional natural gas commodities") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_RegionalTechnology_NG",
                   "energy/GCAM_region_pipeline_bloc_import") ->
    L2392.TechShrwt_reg_NG

  L2392.TechCoef_reg_NG %>%
    add_title("Technology input-output coefficients for regional NG commodities") %>%
    add_units("Unitless IO") %>%
    add_comments("Pass-through; 1 unless some portion is assumed lost/spoiled in shipping") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_RegionalTechnology_NG",
                   "energy/GCAM_region_pipeline_bloc_import") ->
    L2392.TechCoef_reg_NG

  L2392.TechCost_reg_NG %>%
    add_title("Technology costs for regional natural gas commodities") %>%
    add_units("1975$/GJ") %>%
    add_comments("Exogenous costs to reflect regasification for LNG, potential trade costs for pipeline gas") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_RegionalTechnologyCost_NG",
                   "energy/A_ff_RegionalTechnology_NG",
                   "energy/GCAM_region_pipeline_bloc_import") ->
    L2392.TechCost_reg_NG

  L2392.TechLifetime_reg_NG %>%
    add_title("Lifetimes for regional natural gas commodities") %>%
    add_units("NA") %>%
    add_comments("Lifetimes for regional NG commodities") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_TradedTechnology_NG",
                   "energy/GCAM_region_pipeline_bloc_import") ->
    L2392.TechLifetime_reg_NG

  L2392.TechSCurve_reg_NG %>%
    add_title("S-curve retirement function for regional natural gas commodities (base year vintage only)") %>%
    add_units("NA") %>%
    add_comments("S-curve for regional NG commodities") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_TradedTechnology_NG",
                   "energy/GCAM_region_pipeline_bloc_import") ->
    L2392.TechSCurve_reg_NG

  L2392.ProfitShutdown_reg_NG %>%
    add_title("Profit shutdown decider for regional natural gas commodities") %>%
    add_units("NA") %>%
    add_comments("Profit shutdown decider for regional NG commodities") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_TradedTechnology_NG",
                   "energy/GCAM_region_pipeline_bloc_import") ->
    L2392.ProfitShutdown_reg_NG

  L2392.TechInterp_reg_NG %>%
    add_title("Interpolation rules for regional NG commodities") %>%
    add_units("Unitless") %>%
    add_comments("Region-specific interp rules for pipelines") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_RegionalTechnology_NG",
                   "energy/GCAM_region_pipeline_bloc_import") ->
    L2392.TechInterp_reg_NG

  L2392.Production_tra_NG %>%
    add_title("Technology calibration for traded NG commodities") %>%
    add_units("EJ") %>%
    add_comments("Regional exports of commodities that are traded between GCAM regions") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_TradedTechnology_NG",
                   "energy/GCAM_region_pipeline_bloc_export",
                   "L2391.NG_export_calOutput_LNG",
                   "L2391.NG_export_calOutput_pipeline",
                   "L2391.NG_export_calOutput_statdiff",
                   "L2391.NG_import_calOutput_statdiff") ->
    L2392.Production_tra_NG

  L2392.Production_reg_imp_NG %>%
    add_title("Technology calibration for regional NG commodities: imports") %>%
    add_units("EJ") %>%
    add_comments("Consumption of commodities that are traded between GCAM regions") %>%
    add_precursors("common/GCAM_region_names",
                   "energy/A_ff_RegionalTechnology_NG",
                   "energy/GCAM_region_pipeline_bloc_import",
                   "L2391.NG_import_calOutput_LNG",
                   "L2391.NG_import_calOutput_pipeline") ->
    L2392.Production_reg_imp_NG

  L2392.Production_reg_dom_NG %>%
    add_title("Technology calibration for regional NG commodities: consumption of domestic production") %>%
    add_units("EJ") %>%
    add_comments("Filtered directly from L239.Production_reg_dom as domestic NG should not change") %>%
    add_precursors("L239.Production_reg_dom") ->
    L2392.Production_reg_dom_NG

  return_data(L2392.Delete_Supplysector_tra_NG,
              L2392.Delete_Supplysector_reg_NG,
              L2392.PrimaryConsKeyword_en_NG,
              L2392.CarbonCoef_NG,
              L2392.Supplysector_tra_NG,
              L2392.SectorUseTrialMarket_tra_NG,
              L2392.SubsectorAll_tra_NG,
              L2392.TechShrwt_tra_NG,
              L2392.TechCost_tra_NG,
              L2392.TechLifetime_tra_NG,
              L2392.TechSCurve_tra_NG,
              L2392.ProfitShutdown_tra_NG,
              L2392.TechCoef_tra_NG,
              L2392.Supplysector_reg_NG,
              L2392.NestingSubsectorAll_reg_NG,
              L2392.SubsectorAll_reg_NG,
              L2392.TechShrwt_reg_NG,
              L2392.TechCoef_reg_NG,
              L2392.TechCost_reg_NG,
              L2392.TechLifetime_reg_NG,
              L2392.TechSCurve_reg_NG,
              L2392.ProfitShutdown_reg_NG,
              L2392.TechInterp_reg_NG,
              L2392.Production_tra_NG,
              L2392.Production_reg_imp_NG,
              L2392.Production_reg_dom_NG)
  } else {
  stop("Unknown command")
  }
}

