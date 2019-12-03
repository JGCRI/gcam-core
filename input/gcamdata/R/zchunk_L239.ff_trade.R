#' module_energy_L239.ff_trade
#'
#' Model input for regional and (globally) traded fossil fuel commodities
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L239.PrimaryConsKeyword_en}, \code{L239.Supplysector_tra},
#'   \code{L239.SectorUseTrialMarket_tra}, \code{L239.SubsectorAll_tra}, \code{L239.TechShrwt_tra},
#'   \code{L239.TechCost_tra}, \code{L239.TechCoef_tra}, \code{L239.Production_tra}, \code{L239.Supplysector_reg},
#'   \code{L239.SubsectorAll_reg}, \code{L239.TechShrwt_reg}, \code{L239.TechCoef_reg}, \code{L239.Production_reg_imp},
#'   \code{L239.Production_reg_dom}, \code{L239.Consumption_intraregional}.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else left_join mutate rename select
#' @importFrom tibble tibble
#' @author GPK February 2019
module_energy_L239.ff_trade <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A21.globaltech_keyword_ff",
             FILE = "energy/A_ff_RegionalSector",
             FILE = "energy/A_ff_RegionalSubsector",
             FILE = "energy/A_ff_RegionalTechnology",
             FILE = "energy/A_ff_TradedSector",
             FILE = "energy/A_ff_TradedSubsector",
             FILE = "energy/A_ff_TradedTechnology",
             "L2011.ff_GrossTrade_EJ_R_C_Y",
             "L2011.ff_ALL_EJ_R_C_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L239.PrimaryConsKeyword_en",
             "L239.Supplysector_tra",
             "L239.SectorUseTrialMarket_tra",
             "L239.SubsectorAll_tra",
             "L239.TechShrwt_tra",
             "L239.TechCost_tra",
             "L239.TechCoef_tra",
             "L239.Production_tra",
             "L239.Supplysector_reg",
             "L239.SubsectorAll_reg",
             "L239.TechShrwt_reg",
             "L239.TechCoef_reg",
             "L239.Production_reg_imp",
             "L239.Production_reg_dom",
             "L239.Consumption_intraregional"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- region <- supplysector <- subsector <- GCAM_Commodity <- GrossExp_EJ <-
      calOutputValue <- subs.share.weight <- market.name <- minicam.energy.input <-
      GrossImp_EJ <- Prod_EJ <- NULL # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A21.globaltech_keyword_ff <- get_data(all_data, "energy/A21.globaltech_keyword_ff")
    A_ff_RegionalSector <- get_data(all_data, "energy/A_ff_RegionalSector")
    A_ff_RegionalSubsector <- get_data(all_data, "energy/A_ff_RegionalSubsector")
    A_ff_RegionalTechnology <- get_data(all_data, "energy/A_ff_RegionalTechnology")
    A_ff_TradedSector <- get_data(all_data, "energy/A_ff_TradedSector")
    A_ff_TradedSubsector <- get_data(all_data, "energy/A_ff_TradedSubsector")
    A_ff_TradedTechnology <- get_data(all_data, "energy/A_ff_TradedTechnology")
    L2011.ff_GrossTrade_EJ_R_C_Y <- get_data(all_data, "L2011.ff_GrossTrade_EJ_R_C_Y")
    L2011.ff_ALL_EJ_R_C_Y <- get_data(all_data, "L2011.ff_ALL_EJ_R_C_Y")

    # In the structure of the model unconventional oil is upgraded before it is shipped out. The passthrough sector
    # that upgrades uncon oil is called unconventional oil production, so to make sure we match we'll change
    # the input here to the name of the passthrough sector.
    L2011.ff_GrossTrade_EJ_R_C_Y %>%
      mutate(GCAM_Commodity = if_else(GCAM_Commodity == "unconventional oil", "unconventional oil production", GCAM_Commodity)) ->
      L2011.ff_GrossTrade_EJ_R_C_Y

    L2011.ff_ALL_EJ_R_C_Y %>%
      mutate(fuel = if_else(fuel == "unconventional oil", "unconventional oil production", fuel)) ->
      L2011.ff_ALL_EJ_R_C_Y

    # Keywords of global technologies
    A21.globaltech_keyword_ff %>%
      repeat_add_columns(tibble(year = c(HISTORICAL_YEARS, MODEL_FUTURE_YEARS))) %>%
      select(sector.name = supplysector, subsector.name = subsector, technology, primary.consumption, year) %>%
      filter(year %in% MODEL_YEARS) -> L239.PrimaryConsKeyword_en

    # 1. TRADED SECTOR / SUBSECTOR / TECHNOLOGY")
    # L239.Supplysector_tra: generic supplysector info for traded ff commodities
    # By convention, traded commodity information is contained within the USA region (could be within any)
    A_ff_TradedSector$region <- gcam.USA_REGION

    # L239.Supplysector_tra: generic supplysector info for traded ff commodities
    L239.Supplysector_tra <- mutate(A_ff_TradedSector, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"))

    # L239.SectorUseTrialMarket_tra: Create solved markets for the traded sectors
    L239.SectorUseTrialMarket_tra <- select(A_ff_TradedSector, region, supplysector) %>%
      mutate(use.trial.market = 1)

    # L239.SubsectorAll_tra: generic subsector info for traded ff commodities
    # Traded commodities have the region set to USA and the subsector gets the region name pre-pended
    L239.SubsectorAll_tra <- write_to_all_regions(A_ff_TradedSubsector,
                                                  c(LEVEL2_DATA_NAMES[["SubsectorAll"]], "logit.type"),
                                                  GCAM_region_names,
                                                  has_traded = TRUE)

    # Base technology-level table for several tables to be written out")
    A_ff_TradedTechnology_R_Y <- repeat_add_columns(A_ff_TradedTechnology,
                                                   tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names) %>%
      mutate(subsector = paste(region, subsector, sep = " "),
             technology = subsector,
             market.name = region,
             region = gcam.USA_REGION)

    # L239.TechShrwt_tra: Share-weights of traded technologies
    L239.TechShrwt_tra <- select(A_ff_TradedTechnology_R_Y, LEVEL2_DATA_NAMES[["TechShrwt"]])

    # L239.TechCost_tra: Costs of traded technologies
    L239.TechCost_tra <- A_ff_TradedTechnology_R_Y %>%
      mutate(minicam.non.energy.input = "trade costs") %>%
      select(LEVEL2_DATA_NAMES[["TechCost"]])

    # L239.TechCoef_tra: Coefficient and market name of traded technologies
    L239.TechCoef_tra <- select(A_ff_TradedTechnology_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]])

    # L239.Production_tra: Output (gross exports) of traded technologies
    L239.GrossExports_EJ_R_C_Y <- left_join_error_no_match(L2011.ff_GrossTrade_EJ_R_C_Y,
                                                           GCAM_region_names,
                                                           by = "GCAM_region_ID") %>%
      select(region, GCAM_Commodity, year, GrossExp_EJ)

    L239.Production_tra <- filter(A_ff_TradedTechnology_R_Y, year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L239.GrossExports_EJ_R_C_Y,
                               by = c(market.name = "region", minicam.energy.input = "GCAM_Commodity", "year")) %>%
      rename(calOutputValue = GrossExp_EJ) %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # PART 2: DOMESTIC SUPPLY SECTOR / SUBSECTOR / TECHNOLOGY")
    # L239.Supplysector_reg: generic supplysector info for regional ff commodities
    L239.Supplysector_reg <- mutate(A_ff_RegionalSector, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"),
                           GCAM_region_names)

    # L239.SubsectorAll_reg: generic subsector info for regional ff commodities (competing domestic prod vs intl imports)
    L239.SubsectorAll_reg <- write_to_all_regions(A_ff_RegionalSubsector,
                                                  c(LEVEL2_DATA_NAMES[["SubsectorAll"]], "logit.type"),
                                                  GCAM_region_names)

    # Base technology-level table for several tables to be written out")
    A_ff_RegionalTechnology_R_Y <- repeat_add_columns(A_ff_RegionalTechnology,
                                                     tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names["region"]) %>%
      mutate(market.name = if_else(market.name == "regional", region, market.name))

    # L239.TechShrwt_tra: Share-weights of traded technologies
    L239.TechShrwt_reg <- select(A_ff_RegionalTechnology_R_Y, LEVEL2_DATA_NAMES[["TechShrwt"]])

    # L239.TechCoef_reg: Coefficient and market name of traded technologies
    L239.TechCoef_reg <- select(A_ff_RegionalTechnology_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]])

    # L239.Production_reg_imp: Output (flow) of gross imports
    # Imports are equal to the gross imports calculated in L1091
    L239.GrossImports_EJ_R_C_Y <- left_join_error_no_match(L2011.ff_GrossTrade_EJ_R_C_Y,
                                                           GCAM_region_names,
                                                           by = "GCAM_region_ID") %>%
      left_join(select(A_ff_TradedTechnology, supplysector, minicam.energy.input),
                by = c(GCAM_Commodity = "minicam.energy.input")) %>%
      select(region, supplysector, year, GrossImp_EJ)
    L239.Production_reg_imp <- A_ff_RegionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "import", subsector)) %>%
      left_join_error_no_match(L239.GrossImports_EJ_R_C_Y,
                               by = c("region", minicam.energy.input = "supplysector", "year")) %>%
      rename(calOutputValue = GrossImp_EJ) %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # L239.Production_reg_dom: Output (flow) of domestic
    # Domestic "output" is equal to production (production in L2011.ff_ALL_EJ_R_C_Y) minus gross exports (calculated in L2011.ff_GrossTrade_EJ_R_C_Y)

    ### DOMESTIC TECHNOLOGY OUTPUT = AG PRODUCTION - GROSS EXPORTS
    L239.GrossExports_EJ_R_C_Y <- left_join_error_no_match(L2011.ff_GrossTrade_EJ_R_C_Y,
                                                           GCAM_region_names,
                                                           by = "GCAM_region_ID") %>%
      select(region, GCAM_Commodity, year, GrossExp_EJ)
    L239.Prod_EJ_R_C_Y <- select(L2011.ff_ALL_EJ_R_C_Y, region, GCAM_Commodity = fuel, year, Prod_EJ = production)
    L239.Production_reg_dom <- A_ff_RegionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "domestic", subsector)) %>%
      left_join_error_no_match(L239.GrossExports_EJ_R_C_Y,
                               by = c("region", minicam.energy.input = "GCAM_Commodity", "year")) %>%
      left_join_error_no_match(L239.Prod_EJ_R_C_Y,
                               by = c("region", minicam.energy.input = "GCAM_Commodity", "year")) %>%
      mutate(calOutputValue = Prod_EJ - GrossExp_EJ,
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # Regional oil competes regional crude oil and regional unconventional oil, but this is all done within a market
    # We'll calibrate any intraregional competition here.
    A_ff_RegionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             !grepl( "domestic", subsector),
             !grepl("imported", subsector))  %>%
      left_join(L2011.ff_ALL_EJ_R_C_Y %>% mutate(fuel = gsub(" production", "", fuel), fuel = paste0("regional ",fuel)),
                by = c("region", minicam.energy.input = "fuel", "year")) %>%
      mutate(calOutputValue = consumption,
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])->
      L239.Consumption_intraregional



    # Produce outputs
    L239.PrimaryConsKeyword_en %>%
      add_title("Keywords of global technologies") %>%
      add_units("unitless") %>%
      add_comments("A21.globaltech_keyword_ff written to all model periods") %>%
      add_legacy_name("L239.PrimaryConsKeyword_en") %>%
      add_precursors("energy/A21.globaltech_keyword_ff") ->
      L239.PrimaryConsKeyword_en

    L239.Supplysector_tra %>%
      add_title("Supplysector info for traded ff commodities") %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_ff_TradedSector") ->
      L239.Supplysector_tra

    L239.SectorUseTrialMarket_tra %>%
      add_title("Supplysector flag indicating to make trial markets") %>%
      add_units("None") %>%
      add_comments("This helps model solution when running with ff trade") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_ff_TradedSector") ->
      L239.SectorUseTrialMarket_tra

    L239.SubsectorAll_tra %>%
      add_title("Subsector info for traded ff commodities") %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_ff_TradedSubsector") ->
      L239.SubsectorAll_tra

    L239.TechShrwt_tra %>%
      add_title("Technology share-weights for traded ff commodities") %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_ff_TradedTechnology") ->
      L239.TechShrwt_tra

    L239.TechCost_tra %>%
      add_title("Technology costs for traded ff commodities") %>%
      add_units("1975$/GJ") %>%
      add_comments("Exogenous cost to reflect shipping + handling of traded commodities") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_ff_TradedTechnology") ->
      L239.TechCost_tra

    L239.TechCoef_tra %>%
      add_title("Technology input-output coefficients for traded ff commodities") %>%
      add_units("Unitless IO") %>%
      add_comments("Pass-through; 1 unless some portion is assumed lost/spoiled in shipping") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_ff_TradedTechnology") ->
      L239.TechCoef_tra

    L239.Production_tra %>%
      add_title("Technology calibration for traded ff commodities") %>%
      add_units("EJ") %>%
      add_comments("Regional exports of commodities that are traded between GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_ff_TradedTechnology",
                     "L2011.ff_GrossTrade_EJ_R_C_Y") ->
      L239.Production_tra

    L239.Supplysector_reg %>%
      add_title("Supplysector info for regional ff commodities") %>%
      add_units("None") %>%
      add_comments("These sectors are used for sharing between consumption of domestically produced crops versus imports") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_ff_RegionalSector") ->
      L239.Supplysector_reg

    L239.SubsectorAll_reg %>%
      add_title("Subsector info for traded ff commodities") %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_ff_RegionalSubsector") ->
      L239.SubsectorAll_reg

    L239.TechShrwt_reg %>%
      add_title("Technology share-weights for traded ff commodities") %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_ff_RegionalTechnology") ->
      L239.TechShrwt_reg

    L239.TechCoef_reg %>%
      add_title("Technology input-output coefficients for regional ff commodities") %>%
      add_units("Unitless IO") %>%
      add_comments("Pass-through; 1 unless some portion is assumed lost/spoiled in shipping") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_ff_RegionalTechnology") ->
      L239.TechCoef_reg

    L239.Production_reg_imp %>%
      add_title("Technology calibration for regional ff commodities: imports") %>%
      add_units("EJ") %>%
      add_comments("Consumption of commodities that are traded between GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_ff_RegionalTechnology",
                     "L2011.ff_GrossTrade_EJ_R_C_Y") ->
      L239.Production_reg_imp

    L239.Production_reg_dom %>%
      add_title("Technology calibration for regional ff commodities: consumption of domestic production") %>%
      add_units("EJ") %>%
      add_comments("Consumption of commodities produced within-region") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_ff_RegionalTechnology",
                     "L2011.ff_ALL_EJ_R_C_Y",
                     "L2011.ff_GrossTrade_EJ_R_C_Y") ->
      L239.Production_reg_dom

    L239.Consumption_intraregional %>%
      add_title("Technology calibration for intraregional ff commodities: competition between goods within a single market") %>%
      add_units("EJ") %>%
      add_comments("Consumption of commodities competed within-region") %>%
      add_precursors("energy/A_ff_RegionalTechnology",
                     "L2011.ff_ALL_EJ_R_C_Y") ->
      L239.Consumption_intraregional

    return_data(L239.PrimaryConsKeyword_en,
                L239.Supplysector_tra,
                L239.SectorUseTrialMarket_tra,
                L239.SubsectorAll_tra,
                L239.TechShrwt_tra,
                L239.TechCost_tra,
                L239.TechCoef_tra,
                L239.Production_tra,
                L239.Supplysector_reg,
                L239.SubsectorAll_reg,
                L239.TechShrwt_reg,
                L239.TechCoef_reg,
                L239.Production_reg_imp,
                L239.Production_reg_dom,
                L239.Consumption_intraregional)
  } else {
    stop("Unknown command")
  }
}
