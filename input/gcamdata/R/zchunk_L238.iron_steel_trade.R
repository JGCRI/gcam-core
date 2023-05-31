# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L238.iron_steel_trade
#'
#' Model input for regional and (globally) traded iron and steel
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L238.Supplysector_tra},
#'   \code{L238.SectorUseTrialMarket_tra}, \code{L238.SubsectorAll_tra}, \code{L238.TechShrwt_tra},
#'   \code{L238.TechCost_tra}, \code{L238.TechCoef_tra}, \code{L238.Production_tra}, \code{L238.Supplysector_reg},
#'   \code{L238.SubsectorAll_reg}, \code{L238.TechShrwt_reg}, \code{L238.TechCoef_reg}, \code{L238.Production_reg_imp},
#'   \code{L238.Production_reg_dom}.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else left_join mutate rename select
#' @importFrom tidyr replace_na
#' @importFrom tibble tibble
#' @author Siddarth Durga July 2022
module_energy_L238.iron_steel_trade <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A_irnstl_RegionalSector",
             FILE = "energy/A_irnstl_RegionalSubsector",
             FILE = "energy/A_irnstl_RegionalTechnology",
             FILE = "energy/A_irnstl_TradedSector",
             FILE = "energy/A_irnstl_TradedSubsector",
             FILE = "energy/A_irnstl_TradedTechnology",
             "LB1092.Tradebalance_iron_steel_Mt_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L238.Supplysector_tra",
             "L238.SectorUseTrialMarket_tra",
             "L238.SubsectorAll_tra",
             "L238.TechShrwt_tra",
             "L238.TechCost_tra",
             "L238.TechCoef_tra",
             "L238.Production_tra",
             "L238.Supplysector_reg",
             "L238.SubsectorAll_reg",
             "L238.TechShrwt_reg",
             "L238.TechCoef_reg",
             "L238.Production_reg_imp",
             "L238.Production_reg_dom"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- region <- supplysector <- subsector <- GCAM_commodity <- GrossExp_Mt <-
      calOutputValue <- subs.share.weight <- market.name <- minicam.energy.input <-
      GrossImp_Mt <- Prod_Mt <- GCAM_region_ID <- GCAM_region <- NetExp_Mt <- Prod_bm3 <-
      NetExp_bm3 <- value <- metric <- flow <- GrossExp <- NULL # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_irnstl_RegionalSector <- get_data(all_data, "energy/A_irnstl_RegionalSector", strip_attributes = TRUE)
    A_irnstl_RegionalSubsector <- get_data(all_data, "energy/A_irnstl_RegionalSubsector", strip_attributes = TRUE)
    A_irnstl_RegionalTechnology <- get_data(all_data, "energy/A_irnstl_RegionalTechnology", strip_attributes = TRUE)
    A_irnstl_TradedSector <- get_data(all_data, "energy/A_irnstl_TradedSector", strip_attributes = TRUE)
    A_irnstl_TradedSubsector <- get_data(all_data, "energy/A_irnstl_TradedSubsector", strip_attributes = TRUE)
    A_irnstl_TradedTechnology <- get_data(all_data, "energy/A_irnstl_TradedTechnology", strip_attributes = TRUE)
    LB1092.Tradebalance_iron_steel_Mt_R_Y <- get_data(all_data, "LB1092.Tradebalance_iron_steel_Mt_R_Y")

    # 1. TRADED SECTOR / SUBSECTOR / TECHNOLOGY")
    # L238.Supplysector_tra: generic supplysector info for traded iron and steel
    # By convention, traded commodity information is contained within the USA region (could be within any)
    A_irnstl_TradedSector$region <- gcam.USA_REGION

    # L238.Supplysector_tra: generic supplysector info for traded iron and steel
    L238.Supplysector_tra <- mutate(A_irnstl_TradedSector, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"))

    # L238.SectorUseTrialMarket_tra: Create solved markets for the traded sectors
    L238.SectorUseTrialMarket_tra <- select(A_irnstl_TradedSector, region, supplysector) %>%
      mutate(use.trial.market = 1)

    # L238.SubsectorAll_tra: generic subsector info for traded iron and steel
    # Traded commodities have the region set to USA and the subsector gets the region name pre-pended
    L238.SubsectorAll_tra <- write_to_all_regions(A_irnstl_TradedSubsector,
                                                  c(LEVEL2_DATA_NAMES[["SubsectorAllTo"]], "logit.type"),
                                                  GCAM_region_names,
                                                  has_traded = TRUE)


    # Change traded iron and steel interpolation rule and to.value in countries listed in energy.IRON_STEEL.DOMESTIC_SW
    L238.SubsectorAll_tra$interpolation.function[which(L238.SubsectorAll_tra$subsector %in% energy.IRON_STEEL.TRADED_SW)] <- "s-curve"
    L238.SubsectorAll_tra$to.year[which(L238.SubsectorAll_tra$subsector %in% energy.IRON_STEEL.TRADED_SW)] <- 2300

    # Base technology-level table for several tables to be written out")
    A_irnstl_TradedTechnology_R_Y <- repeat_add_columns(A_irnstl_TradedTechnology,
                                                   tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names) %>%
      mutate(subsector = paste(region, subsector, sep = " "),
             technology = subsector,
             market.name = region,
             region = gcam.USA_REGION)

    # L238.TechShrwt_tra: Share-weights of traded technologies
    L238.TechShrwt_tra <- select(A_irnstl_TradedTechnology_R_Y, LEVEL2_DATA_NAMES[["TechShrwt"]])

    # L238.TechCost_tra: Costs of traded technologies
    L238.TechCost_tra <- A_irnstl_TradedTechnology_R_Y %>%
      mutate(minicam.non.energy.input = "trade costs") %>%
      select(LEVEL2_DATA_NAMES[["TechCost"]])

    # L238.TechCoef_tra: Coefficient and market name of traded technologies
    L238.TechCoef_tra <- select(A_irnstl_TradedTechnology_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]])



    # L238.Production_tra: Output (gross exports) of traded technologies
    L238.GrossExports_Mt_R_Y <- left_join_error_no_match(LB1092.Tradebalance_iron_steel_Mt_R_Y %>%
                                                             filter(metric=="exports_reval") %>%
                                                             rename(GrossExp_Mt=value,region=GCAM_region),
                                                           GCAM_region_names,
                                                           by = "region") %>%
      select(region, year, GrossExp_Mt)

    L238.Production_tra <- filter(A_irnstl_TradedTechnology_R_Y, year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L238.GrossExports_Mt_R_Y,
                               by = c(market.name = "region", "year")) %>%
      rename(calOutputValue = GrossExp_Mt) %>%
      mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # PART 2: DOMESTIC SUPPLY SECTOR / SUBSECTOR / TECHNOLOGY")
    # L238.Supplysector_reg: generic supplysector info for iron and steel
    L238.Supplysector_reg <- mutate(A_irnstl_RegionalSector, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"),
                           GCAM_region_names)

    # L238.SubsectorAll_reg: generic subsector info for regional iron and steel (competing domestic prod vs intl imports)
    L238.SubsectorAll_reg <- write_to_all_regions(A_irnstl_RegionalSubsector,
                                                  c(LEVEL2_DATA_NAMES[["SubsectorAllTo"]], "logit.type"),
                                                  GCAM_region_names)

    # Change iron and steel domestic supply interpolation rule and to.value in countries listed in energy.IRON_STEEL.DOMESTIC_SW
    L238.SubsectorAll_reg$to.value[which(L238.SubsectorAll_reg$region %in% energy.IRON_STEEL.DOMESTIC_SW & L238.SubsectorAll_reg$subsector %in% c("domestic iron and steel"))] <- 1
    L238.SubsectorAll_reg$interpolation.function[which(L238.SubsectorAll_reg$region %in% energy.IRON_STEEL.DOMESTIC_SW & L238.SubsectorAll_reg$subsector %in% c("domestic iron and steel"))] <- "s-curve"
    L238.SubsectorAll_reg$to.year[which(L238.SubsectorAll_reg$region %in% energy.IRON_STEEL.DOMESTIC_SW & L238.SubsectorAll_reg$subsector %in% c("domestic iron and steel"))] <- 2105

    # Base technology-level table for several tables to be written out")
    A_irnstl_RegionalTechnology_R_Y <- repeat_add_columns(A_irnstl_RegionalTechnology,
                                                     tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names["region"]) %>%
      mutate(market.name = if_else(market.name == "regional", region, market.name))

    # L238.TechShrwt_tra: Share-weights of traded technologies
    L238.TechShrwt_reg <- select(A_irnstl_RegionalTechnology_R_Y, LEVEL2_DATA_NAMES[["TechShrwt"]])

    # L238.TechCoef_reg: Coefficient and market name of traded technologies
    L238.TechCoef_reg <- select(A_irnstl_RegionalTechnology_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]])

    # L238.Production_reg_imp: Output (flow) of gross imports
    # Imports are equal to the gross imports calculated in LB1092
    L238.GrossImports_Mt_R_Y <- left_join_error_no_match(LB1092.Tradebalance_iron_steel_Mt_R_Y %>%
                                                           filter(metric=="imports_reval") %>%
                                                           mutate(minicam.energy.input="iron and steel")%>%
                                                           rename(GrossImp_Mt=value,region=GCAM_region),
                                                           GCAM_region_names,
                                                           by = "region")%>%
      left_join(select(A_irnstl_TradedTechnology, supplysector, minicam.energy.input),
                by = c("minicam.energy.input")) %>%
      select(region, supplysector, year, GrossImp_Mt)

    L238.Production_reg_imp <- A_irnstl_RegionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "import", subsector)) %>%
      left_join_error_no_match(L238.GrossImports_Mt_R_Y,
                               by = c("region", minicam.energy.input = "supplysector", "year")) %>%
      rename(calOutputValue = GrossImp_Mt) %>%
      mutate(calOutputValue = round(calOutputValue, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # L238.Production_reg_dom: Output (flow) of domestic

    #### DOMESTIC TECHNOLOGY OUTPUT = iron and steel PRODUCTION - GROSS EXPORTS
    L238.DomSup_Mt_R_Y <- left_join_error_no_match(LB1092.Tradebalance_iron_steel_Mt_R_Y %>%
                                                     filter(metric=="domestic_supply") %>%
                                                     mutate(minicam.energy.input="iron and steel")%>%
                                                     rename(DomSup_Mt=value,region=GCAM_region),
                                                   GCAM_region_names,
                                                   by = "region") %>%
      select(region, GCAM_commodity, year, DomSup_Mt)

    L238.Production_reg_dom <- A_irnstl_RegionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "domestic", subsector)) %>%
      left_join_error_no_match(L238.DomSup_Mt_R_Y,
                               by = c("region", minicam.energy.input, "year")) %>%
      mutate(calOutputValue = round(DomSup_Mt, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # Produce outputs
    L238.Supplysector_tra %>%
      add_title("Supplysector info for iron and steel") %>%
      add_units("None") %>%
      add_comments("Modeled for all GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_TradedSector") ->
      L238.Supplysector_tra

    L238.SectorUseTrialMarket_tra %>%
      add_title("Supplysector flag indicating to make trial markets") %>%
      add_units("None") %>%
      add_comments("This helps model solution when running with iron and steel trade") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_TradedSector") ->
      L238.SectorUseTrialMarket_tra

    L238.SubsectorAll_tra %>%
      add_title("Subsector info for traded iron and steel") %>%
      add_units("None") %>%
      add_comments("Modeled for all GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_TradedSubsector") ->
      L238.SubsectorAll_tra

    L238.TechShrwt_tra %>%
      add_title("Technology share-weights for traded iron and steel") %>%
      add_units("None") %>%
      add_comments("Modeled for all GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_TradedTechnology") ->
      L238.TechShrwt_tra

    L238.TechCost_tra %>%
      add_title("Technology costs for traded iron and steel") %>%
      add_units("1975$/kg") %>%
      add_comments("Exogenous cost to reflect shipping + handling of traded commodities") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_TradedTechnology") ->
      L238.TechCost_tra

    L238.TechCoef_tra %>%
      add_title("Technology input-output coefficients for traded iron and steel") %>%
      add_units("Unitless IO") %>%
      add_comments("Pass-through; 1 unless some portion is assumed lost/spoiled in shipping") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_TradedTechnology") -> L238.TechCoef_tra

    L238.Production_tra %>%
      add_title("Technology calibration for traded iron and steel") %>%
      add_units("Mt") %>%
      add_comments("Regional exports of iron and steel that are traded between GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "LB1092.Tradebalance_iron_steel_Mt_R_Y") -> L238.Production_tra

    L238.Supplysector_reg %>%
      add_title("Supplysector info for regional iron and steel") %>%
      add_units("None") %>%
      add_comments("These sectors are used for sharing between consumption of domestically produced iron and steel versus imports") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_RegionalSector") ->
      L238.Supplysector_reg

    L238.SubsectorAll_reg %>%
      add_title("Subsector info for traded iron and steel") %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_RegionalSubsector") ->
      L238.SubsectorAll_reg

    L238.TechShrwt_reg %>%
      add_title("Technology share-weights for traded iron and steel") %>%
      add_units("None") %>%
      add_comments("Modeled for all GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_RegionalTechnology") ->
      L238.TechShrwt_reg

    L238.TechCoef_reg %>%
      add_title("Technology input-output coefficients for regional iron and steel") %>%
      add_units("Unitless IO") %>%
      add_comments("Pass-through; 1 unless some portion is assumed lost/spoiled in shipping") %>%
      add_precursors("common/GCAM_region_names",
                     "energy/A_irnstl_RegionalTechnology") ->
      L238.TechCoef_reg

    L238.Production_reg_imp %>%
      add_title("Technology calibration for regional iron and steel commodities: imports") %>%
      add_units("Mt") %>%
      add_comments("Consumption of iron and steelthat are traded between GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "LB1092.Tradebalance_iron_steel_Mt_R_Y") ->
      L238.Production_reg_imp

    L238.Production_reg_dom %>%
      add_title("Technology calibration for regional iron and steel: consumption of domestic production") %>%
      add_units("Mt") %>%
      add_comments("Consumption of iron and steel produced within-region") %>%
      add_precursors("common/GCAM_region_names",
                     "LB1092.Tradebalance_iron_steel_Mt_R_Y") ->
      L238.Production_reg_dom

    return_data(L238.Supplysector_tra,
                L238.SectorUseTrialMarket_tra,
                L238.SubsectorAll_tra,
                L238.TechShrwt_tra,
                L238.TechCost_tra,
                L238.TechCoef_tra,
                L238.Production_tra,
                L238.Supplysector_reg,
                L238.SubsectorAll_reg,
                L238.TechShrwt_reg,
                L238.TechCoef_reg,
                L238.Production_reg_imp,
                L238.Production_reg_dom)
  } else {
    stop("Unknown command")
  }
}
