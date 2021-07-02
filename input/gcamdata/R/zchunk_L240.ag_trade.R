# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L240.ag_trade
#'
#' Model input for regional and (globally) traded agricultural crops and livestock commodities
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L240.Supplysector_tra},
#'   \code{L240.SectorUseTrialMarket_tra}, \code{L240.SubsectorAll_tra}, \code{L240.TechShrwt_tra},
#'   \code{L240.TechCost_tra}, \code{L240.TechCoef_tra}, \code{L240.Production_tra}, \code{L240.Supplysector_reg},
#'   \code{L240.SubsectorAll_reg}, \code{L240.TechShrwt_reg}, \code{L240.TechCoef_reg}, \code{L240.Production_reg_imp},
#'   \code{L240.Production_reg_dom}.
#' @details Build datasets for ssp4 agricultural trade: food and nonfood trade coefficients, feed trade
#' coefficients, restricted agricultural trade, and trade regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else left_join mutate rename select
#' @importFrom tidyr replace_na
#' @importFrom tibble tibble
#' @author GPK February 2019  XZ March 2020
module_aglu_L240.ag_trade <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "aglu/A_agRegionalSector",
             FILE = "aglu/A_agRegionalSubsector",
             FILE = "aglu/A_agRegionalTechnology",
             FILE = "aglu/A_agTradedSector",
             FILE = "aglu/A_agTradedSubsector",
             FILE = "aglu/A_agTradedTechnology",
             FILE = "common/iso_GCAM_regID",
             "L109.ag_ALL_Mt_R_C_Y",
             "L109.an_ALL_Mt_R_C_Y",
             "L110.For_ALL_bm3_R_Y",
             "L100.FAO_For_Exp_m3",
             "L1091.GrossTrade_Mt_R_C_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L240.Supplysector_tra",
             "L240.SectorUseTrialMarket_tra",
             "L240.SubsectorAll_tra",
             "L240.TechShrwt_tra",
             "L240.TechCost_tra",
             "L240.TechCoef_tra",
             "L240.Production_tra",
             "L240.Supplysector_reg",
             "L240.SubsectorAll_reg",
             "L240.TechShrwt_reg",
             "L240.TechCoef_reg",
             "L240.Production_reg_imp",
             "L240.Production_reg_dom"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- region <- supplysector <- subsector <- GCAM_commodity <- GrossExp_Mt <-
      calOutputValue <- subs.share.weight <- market.name <- minicam.energy.input <-
      GrossImp_Mt <- Prod_Mt <- GCAM_region_ID <- NetExp_Mt <- Prod_bm3 <-
      NetExp_bm3 <- value <- flow <- GrossExp <- NULL # silence package check notes


    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_agRegionalSector <- get_data(all_data, "aglu/A_agRegionalSector", strip_attributes = TRUE)
    A_agRegionalSubsector <- get_data(all_data, "aglu/A_agRegionalSubsector", strip_attributes = TRUE)
    A_agRegionalTechnology <- get_data(all_data, "aglu/A_agRegionalTechnology", strip_attributes = TRUE)
    A_agTradedSector <- get_data(all_data, "aglu/A_agTradedSector", strip_attributes = TRUE)
    A_agTradedSubsector <- get_data(all_data, "aglu/A_agTradedSubsector", strip_attributes = TRUE)
    A_agTradedTechnology <- get_data(all_data, "aglu/A_agTradedTechnology", strip_attributes = TRUE)
    L109.ag_ALL_Mt_R_C_Y <- get_data(all_data, "L109.ag_ALL_Mt_R_C_Y")
    L109.an_ALL_Mt_R_C_Y <- get_data(all_data, "L109.an_ALL_Mt_R_C_Y")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    L110.For_ALL_bm3_R_Y <- get_data(all_data, "L110.For_ALL_bm3_R_Y")
    L100.FAO_For_Exp_m3 <- get_data(all_data, "L100.FAO_For_Exp_m3")
    L1091.GrossTrade_Mt_R_C_Y <- get_data(all_data, "L1091.GrossTrade_Mt_R_C_Y")

    # 0: Bind crops, livestock, and forest for prod and netexp
    L109.ag_an_for_ALL_Mt_R_C_Y <- L109.ag_ALL_Mt_R_C_Y %>%
      select(GCAM_region_ID, GCAM_commodity, year, Prod_Mt, NetExp_Mt) %>%
      bind_rows(L109.an_ALL_Mt_R_C_Y %>%
                  select(GCAM_region_ID, GCAM_commodity, year, Prod_Mt, NetExp_Mt)) %>%
      bind_rows(L110.For_ALL_bm3_R_Y %>%
                  filter(GCAM_commodity %in% aglu.TRADED_FORESTS) %>%
                  select(GCAM_region_ID, GCAM_commodity, year,
                         Prod_Mt = Prod_bm3, NetExp_Mt = NetExp_bm3)) #note that physical unit for forest data is bm3

    # Adding forest trade data in L1091.GrossTrade_Mt_R_C_Y. Note that bilateral trade data are not used for now.
    # FAO does not provide primary roundwood bilateral trade data. We use export data to back calculate gross trade.
    # replace_na here only affect Taiwan, which we did not have trade data.
    L1091.GrossTrade_Mt_R_C_Y <- L1091.GrossTrade_Mt_R_C_Y %>%
      bind_rows(L110.For_ALL_bm3_R_Y %>%
                  left_join(
                    L100.FAO_For_Exp_m3 %>%
                      mutate(GCAM_region_ID = left_join_error_no_match(L100.FAO_For_Exp_m3, iso_GCAM_regID, by = c("iso"))[['GCAM_region_ID']],
                             GCAM_commodity = "Forest",                   # add the forest commodity label
                             value = CONV_M3_BM3 * value,                 # convert the value units from m3 to bm3, had to add this constant to constants.R
                             flow = "GrossExp") %>%
                      select(GCAM_region_ID, GCAM_commodity, flow, year, value) %>%
                      group_by(GCAM_region_ID, GCAM_commodity, flow, year) %>%
                      summarise(value = sum(value)) %>%
                      ungroup() %>%
                      spread(flow, value),
                    by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%
                  replace_na(list(GrossExp = 0)) %>%
                  filter(GCAM_commodity %in% aglu.TRADED_FORESTS) %>%
                  mutate(GrossImp_Mt = if_else(GrossExp - NetExp_bm3 > 0, GrossExp - NetExp_bm3, 0),
                         GrossExp_Mt = if_else(GrossExp - NetExp_bm3 > 0, GrossExp, NetExp_bm3)) %>%
                  select(names(L1091.GrossTrade_Mt_R_C_Y)) )

    # 1. TRADED SECTOR / SUBSECTOR / TECHNOLOGY")
    # L240.Supplysector_tra: generic supplysector info for traded ag commodities
    # By convention, traded commodity information is contained within the USA region (could be within any)
    A_agTradedSector$region <- gcam.USA_REGION

    # L240.Supplysector_tra: generic supplysector info for traded ag commodities
    L240.Supplysector_tra <- mutate(A_agTradedSector, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"))

    # L240.SectorUseTrialMarket_tra: Create solved markets for the traded sectors
    L240.SectorUseTrialMarket_tra <- select(A_agTradedSector, region, supplysector) %>%
      mutate(use.trial.market = 1)

    # L240.SubsectorAll_tra: generic subsector info for traded ag commodities
    # Traded commodities have the region set to USA and the subsector gets the region name pre-pended
    L240.SubsectorAll_tra <- write_to_all_regions(A_agTradedSubsector,
                                                  c(LEVEL2_DATA_NAMES[["SubsectorAll"]], "logit.type"),
                                                  filter(GCAM_region_names, !region %in% aglu.NO_AGLU_REGIONS),
                                                  has_traded = TRUE)

    # Base technology-level table for several tables to be written out")
    A_agTradedTechnology_R_Y <- repeat_add_columns(A_agTradedTechnology,
                                                   tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(filter(GCAM_region_names, !region %in% aglu.NO_AGLU_REGIONS)) %>%
      mutate(subsector = paste(region, subsector, sep = " "),
             technology = subsector,
             market.name = region,
             region = gcam.USA_REGION)

    # L240.TechShrwt_tra: Share-weights of traded technologies
    L240.TechShrwt_tra <- select(A_agTradedTechnology_R_Y, LEVEL2_DATA_NAMES[["TechShrwt"]])

    # L240.TechCost_tra: Costs of traded technologies
    L240.TechCost_tra <- A_agTradedTechnology_R_Y %>%
      mutate(minicam.non.energy.input = "trade costs") %>%
      select(LEVEL2_DATA_NAMES[["TechCost"]])

    # L240.TechCoef_tra: Coefficient and market name of traded technologies
    L240.TechCoef_tra <- select(A_agTradedTechnology_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]])

    # L240.Production_tra: Output (gross exports) of traded technologies
    L240.GrossExports_Mt_R_C_Y <- left_join_error_no_match(L1091.GrossTrade_Mt_R_C_Y,
                                                           GCAM_region_names,
                                                           by = "GCAM_region_ID") %>%
      select(region, GCAM_commodity, year, GrossExp_Mt)

    L240.Production_tra <- filter(A_agTradedTechnology_R_Y, year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L240.GrossExports_Mt_R_C_Y,
                               by = c(market.name = "region", minicam.energy.input = "GCAM_commodity", "year")) %>%
      rename(calOutputValue = GrossExp_Mt) %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # PART 2: DOMESTIC SUPPLY SECTOR / SUBSECTOR / TECHNOLOGY")
    # L240.Supplysector_reg: generic supplysector info for regional ag commodities
    L240.Supplysector_reg <- mutate(A_agRegionalSector, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"),
                           filter(GCAM_region_names, !region %in% aglu.NO_AGLU_REGIONS))

    # L240.SubsectorAll_reg: generic subsector info for regional ag commodities (competing domestic prod vs intl imports)
    L240.SubsectorAll_reg <- write_to_all_regions(A_agRegionalSubsector,
                                                  c(LEVEL2_DATA_NAMES[["SubsectorAll"]], "logit.type"),
                                                  filter(GCAM_region_names, !region %in% aglu.NO_AGLU_REGIONS))

    # Base technology-level table for several tables to be written out")
    A_agRegionalTechnology_R_Y <- repeat_add_columns(A_agRegionalTechnology,
                                                     tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(filter(GCAM_region_names["region"], !region %in% aglu.NO_AGLU_REGIONS)) %>%
      mutate(market.name = if_else(market.name == "regional", region, market.name))

    # L240.TechShrwt_tra: Share-weights of traded technologies
    L240.TechShrwt_reg <- select(A_agRegionalTechnology_R_Y, LEVEL2_DATA_NAMES[["TechShrwt"]])

    # L240.TechCoef_reg: Coefficient and market name of traded technologies
    L240.TechCoef_reg <- select(A_agRegionalTechnology_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]])

    # L240.Production_reg_imp: Output (flow) of gross imports
    # Imports are equal to the gross imports calculated in L1091
    L240.GrossImports_Mt_R_C_Y <- left_join_error_no_match(L1091.GrossTrade_Mt_R_C_Y,
                                                           GCAM_region_names,
                                                           by = "GCAM_region_ID") %>%
      left_join(select(A_agTradedTechnology, supplysector, minicam.energy.input),
                by = c(GCAM_commodity = "minicam.energy.input")) %>%
      select(region, supplysector, year, GrossImp_Mt)
    L240.Production_reg_imp <- A_agRegionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "import", subsector)) %>%
      left_join_error_no_match(L240.GrossImports_Mt_R_C_Y,
                               by = c("region", minicam.energy.input = "supplysector", "year")) %>%
      rename(calOutputValue = GrossImp_Mt) %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # L240.Production_reg_dom: Output (flow) of domestic
    # Domestic "output" is equal to production (Prod_Mt in L109) minus gross exports (calculated in L1091)

    #### DOMESTIC TECHNOLOGY OUTPUT = AG PRODUCTION - GROSS EXPORTS
    L240.GrossExports_Mt_R_C_Y <- left_join_error_no_match(L1091.GrossTrade_Mt_R_C_Y,
                                                           GCAM_region_names,
                                                           by = "GCAM_region_ID") %>%
      select(region, GCAM_commodity, year, GrossExp_Mt)
    L240.Prod_Mt_R_C_Y <- left_join_error_no_match(L109.ag_an_for_ALL_Mt_R_C_Y,
                                                   GCAM_region_names,
                                                   by = "GCAM_region_ID") %>%
      select(region, GCAM_commodity, year, Prod_Mt)
    L240.Production_reg_dom <- A_agRegionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "domestic", subsector)) %>%
      left_join_error_no_match(L240.GrossExports_Mt_R_C_Y,
                               by = c("region", minicam.energy.input = "GCAM_commodity", "year")) %>%
      left_join_error_no_match(L240.Prod_Mt_R_C_Y,
                               by = c("region", minicam.energy.input = "GCAM_commodity", "year")) %>%
      mutate(calOutputValue = Prod_Mt - GrossExp_Mt,
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # Produce outputs
    L240.Supplysector_tra %>%
      add_title("Supplysector info for traded ag commodities") %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agTradedSector") ->
      L240.Supplysector_tra

    L240.SectorUseTrialMarket_tra %>%
      add_title("Supplysector flag indicating to make trial markets") %>%
      add_units("None") %>%
      add_comments("This helps model solution when running with ag trade") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agTradedSector") ->
      L240.SectorUseTrialMarket_tra

    L240.SubsectorAll_tra %>%
      add_title("Subsector info for traded ag commodities") %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agTradedSubsector") ->
      L240.SubsectorAll_tra

    L240.TechShrwt_tra %>%
      add_title("Technology share-weights for traded ag commodities") %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agTradedTechnology") ->
      L240.TechShrwt_tra

    L240.TechCost_tra %>%
      add_title("Technology costs for traded ag commodities") %>%
      add_units("1975$/kg") %>%
      add_comments("Exogenous cost to reflect shipping + handling of traded commodities") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agTradedTechnology") ->
      L240.TechCost_tra

    L240.TechCoef_tra %>%
      add_title("Technology input-output coefficients for traded ag commodities") %>%
      add_units("Unitless IO") %>%
      add_comments("Pass-through; 1 unless some portion is assumed lost/spoiled in shipping") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agTradedTechnology") ->
      L240.TechCoef_tra

    L240.Production_tra %>%
      add_title("Technology calibration for traded ag commodities") %>%
      add_units("Mt") %>%
      add_comments("Regional exports of commodities that are traded between GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agTradedTechnology",
                     "L110.For_ALL_bm3_R_Y",
                     "L100.FAO_For_Exp_m3",
                     "common/iso_GCAM_regID",
                     "L1091.GrossTrade_Mt_R_C_Y") ->
      L240.Production_tra

    L240.Supplysector_reg %>%
      add_title("Supplysector info for regional ag commodities") %>%
      add_units("None") %>%
      add_comments("These sectors are used for sharing between consumption of domestically produced crops versus imports") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agRegionalSector") ->
      L240.Supplysector_reg

    L240.SubsectorAll_reg %>%
      add_title("Subsector info for traded ag commodities") %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agRegionalSubsector") ->
      L240.SubsectorAll_reg

    L240.TechShrwt_reg %>%
      add_title("Technology share-weights for traded ag commodities") %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agRegionalTechnology") ->
      L240.TechShrwt_reg

    L240.TechCoef_reg %>%
      add_title("Technology input-output coefficients for regional ag commodities") %>%
      add_units("Unitless IO") %>%
      add_comments("Pass-through; 1 unless some portion is assumed lost/spoiled in shipping") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agRegionalTechnology") ->
      L240.TechCoef_reg

    L240.Production_reg_imp %>%
      add_title("Technology calibration for regional ag commodities: imports") %>%
      add_units("Mt") %>%
      add_comments("Consumption of commodities that are traded between GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agRegionalTechnology",
                     "L1091.GrossTrade_Mt_R_C_Y") ->
      L240.Production_reg_imp

    L240.Production_reg_dom %>%
      add_title("Technology calibration for regional ag commodities: consumption of domestic production") %>%
      add_units("Mt") %>%
      add_comments("Consumption of commodities produced within-region") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agRegionalTechnology",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L100.FAO_For_Exp_m3",
                     "common/iso_GCAM_regID",
                     "L1091.GrossTrade_Mt_R_C_Y") ->
      L240.Production_reg_dom

    return_data(L240.Supplysector_tra,
                L240.SectorUseTrialMarket_tra,
                L240.SubsectorAll_tra,
                L240.TechShrwt_tra,
                L240.TechCost_tra,
                L240.TechCoef_tra,
                L240.Production_tra,
                L240.Supplysector_reg,
                L240.SubsectorAll_reg,
                L240.TechShrwt_reg,
                L240.TechCoef_reg,
                L240.Production_reg_imp,
                L240.Production_reg_dom)
  } else {
    stop("Unknown command")
  }
}
