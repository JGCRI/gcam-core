# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L202.an_input
#'
#' Produce a wide range of animal-related resource tables: production, import, resource curves.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L202.RenewRsrc}, \code{L202.RenewRsrcPrice}, \code{L202.maxSubResource},
#' \code{L202.RenewRsrcCurves}, \code{L202.ResTechShrwt}, \code{L202.UnlimitedRenewRsrcCurves}, \code{L202.UnlimitedRenewRsrcPrice},
#' \code{L202.Supplysector_in}, \code{L202.SubsectorAll_in}, \code{L202.StubTech_in}, \code{L202.StubTechInterp_in},
#' \code{L202.GlobalTechCoef_in}, \code{L202.GlobalTechShrwt_in}, \code{L202.StubTechProd_in},
#' \code{L202.Supplysector_an}, \code{L202.SubsectorAll_an}, \code{L202.GlobalTechShrwt_an}, \code{L202.StubTechInterp_an}
#' \code{L202.StubTechProd_an}, \code{L202.StubTechCoef_an}, \code{L202.StubTechCost_an},
#'. The corresponding file in the
#' original data system was \code{L202.an_input.R} (aglu level2).
#' @details This chunk produces 22 animal-related resource tables: production, import, resource curves.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join bind_rows distinct filter if_else group_by left_join mutate select summarise
#' @importFrom tidyr complete replace_na
#' @author BBL August 2017
module_aglu_L202.an_input <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A_regions",
             FILE = "aglu/A_agRsrc",
             FILE = "aglu/A_agSubRsrc",
             FILE = "aglu/A_agRsrcCurves",
             FILE = "aglu/A_agUnlimitedRsrcCurves",
             FILE = "aglu/A_an_input_supplysector",
             FILE = "aglu/A_an_input_subsector",
             FILE = "aglu/A_an_input_technology",
             FILE = "aglu/A_an_input_globaltech_shrwt",
             FILE = "aglu/A_an_supplysector",
             FILE = "aglu/A_an_subsector",
             FILE = "aglu/A_an_technology",
             "L107.an_Prod_Mt_R_C_Sys_Fd_Y",
             "L107.an_FeedIO_R_C_Sys_Fd_Y",
             "L107.an_Feed_Mt_R_C_Sys_Fd_Y",
             "L108.ag_Feed_Mt_R_C_Y",
             "L109.ag_ALL_Mt_R_C_Y",
             "L109.an_ALL_Mt_R_C_Y",
             "L1091.GrossTrade_Mt_R_C_Y",
             "L132.ag_an_For_Prices",
             "L1321.ag_prP_R_C_75USDkg",
             "L1321.an_prP_R_C_75USDkg"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L202.RenewRsrc",
             "L202.RenewRsrcPrice",
             "L202.maxSubResource",
             "L202.RenewRsrcCurves",
             "L202.ResTechShrwt",
             "L202.UnlimitedRenewRsrcCurves",
             "L202.UnlimitedRenewRsrcPrice",
             "L202.Supplysector_in",
             "L202.SubsectorAll_in",
             "L202.SubsectorInterpTo_in",
             "L202.StubTech_in",
             "L202.StubTechInterp_in",
             "L202.GlobalTechCoef_in",
             "L202.GlobalTechShrwt_in",
             "L202.StubTechProd_in",
             "L202.Supplysector_an",
             "L202.SubsectorAll_an",
             "L202.GlobalTechShrwt_an",
             "L202.StubTechInterp_an",
             "L202.StubTechProd_an",
             "L202.StubTechCoef_an",
             "L202.StubTechCost_an",
             "L202.ag_consP_R_C_75USDkg"
             ))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- market <- region <- renewresource <- GCAM_commodity <- GCAM_region_ID <- Prod_Mt <- value <-
      sub.renewable.resource <- year <- price <- unlimited.resource <- technology <- supplysector <-
      subsector <- share.weight <- calOutputValue <- subs.share.weight <- coefficient <- maxSubResource <-
      stub.technology <- output_supplysector <- grade <- extractioncost <- calPrice <- unit <- share_Fd <-
      feed <- wtd_price <- Feed_Mt <- FeedPrice_USDkg <- FeedCost_bilUSD <- CommodityPrice_USDkg <-
      FeedCost_USDkg <- nonFeedCost <- NetExp_Mt <- share.weight.year <- fixedOutput <- ethanol <-
      biomassOil_tech <- biodiesel <- resource <- subresource <- default_price <- revenue <-
      weight <- SalesRevenue_bilUSD <- tradedP <- Exp_wtd_price <- ImpShare <- PrP <- GrossExp_Mt <-
      Supply_Mt <- GrossImp_Mt <- ChinaCommodityPrice_USDkg <- to.value <- DefaultCommodityPrice_USDkg <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_regions <- get_data(all_data, "energy/A_regions")
    A_agRsrc <- get_data(all_data, "aglu/A_agRsrc", strip_attributes = TRUE)
    A_agSubRsrc <- get_data(all_data, "aglu/A_agSubRsrc", strip_attributes = TRUE)
    A_agRsrcCurves <- get_data(all_data, "aglu/A_agRsrcCurves", strip_attributes = TRUE)
    A_agUnlimitedRsrcCurves <- get_data(all_data, "aglu/A_agUnlimitedRsrcCurves", strip_attributes = TRUE)
    A_an_input_supplysector <- get_data(all_data, "aglu/A_an_input_supplysector", strip_attributes = TRUE)
    A_an_input_subsector <- get_data(all_data, "aglu/A_an_input_subsector", strip_attributes = TRUE)
    A_an_input_technology <- get_data(all_data, "aglu/A_an_input_technology", strip_attributes = TRUE)
    A_an_input_globaltech_shrwt <- get_data(all_data, "aglu/A_an_input_globaltech_shrwt")
    A_an_supplysector <- get_data(all_data, "aglu/A_an_supplysector", strip_attributes = TRUE)
    A_an_subsector <- get_data(all_data, "aglu/A_an_subsector", strip_attributes = TRUE)
    A_an_technology <- get_data(all_data, "aglu/A_an_technology", strip_attributes = TRUE)
    L132.ag_an_For_Prices <- get_data(all_data, "L132.ag_an_For_Prices")
    L109.ag_ALL_Mt_R_C_Y <- get_data(all_data, "L109.ag_ALL_Mt_R_C_Y")
    L1091.GrossTrade_Mt_R_C_Y <- get_data(all_data, "L1091.GrossTrade_Mt_R_C_Y")
    L1321.ag_prP_R_C_75USDkg <- get_data(all_data, "L1321.ag_prP_R_C_75USDkg", strip_attributes = TRUE)
    L1321.an_prP_R_C_75USDkg <- get_data(all_data, "L1321.an_prP_R_C_75USDkg")

    # 2. Build tables
    # Base table for resources - add region names to Level1 data tables (lines 49-70 old file)

    # Following datasets are already 'long' so just skip the old interpolate_and_melt step
    # Helper function to get_data, join with GCAM region names, and filter to base years
    get_join_filter <- function(x) {   #
      get_data(all_data, x) %>%
        left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
        filter(year %in% MODEL_BASE_YEARS)
    }

    L202.an_Prod_Mt_R_C_Sys_Fd_Y.mlt <- get_join_filter("L107.an_Prod_Mt_R_C_Sys_Fd_Y")
    L202.an_FeedIO_R_C_Sys_Fd_Y.mlt <- get_join_filter("L107.an_FeedIO_R_C_Sys_Fd_Y")
    L202.an_Feed_Mt_R_C_Sys_Fd_Y.mlt <- get_join_filter("L107.an_Feed_Mt_R_C_Sys_Fd_Y")
    L202.ag_Feed_Mt_R_C_Y.mlt <- get_join_filter("L108.ag_Feed_Mt_R_C_Y")
    L202.an_ALL_Mt_R_C_Y <- get_join_filter("L109.an_ALL_Mt_R_C_Y")

    # L202.RenewRsrc: generic resource attributes
    # Here, and in general below, we extend data across all GCAM regions for a particular set of
    # level 2 output columns; here also substitute region data when market is "regional"
    A_agRsrc %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["RenewRsrc"]], GCAM_region_names) %>%
      mutate(market = if_else(market == "regional", region, market)) ->
      L202.RenewRsrc

    # L202.RenewRsrcPrice: resource prices
    L202.RenewRsrc %>%
      select(region, renewresource) %>%
      mutate(year = min(MODEL_BASE_YEARS), price = gcam.DEFAULT_PRICE) ->
      L202.RenewRsrcPrice

    # L202.maxSubResource: maximum amount of resource production allowed in any period (72-97)
    # Compute the maxsubresource as the maximum of all base periods, for each region and resource
    # Note that the supply curves can exceed this number, by setting "available" to a number > 1.
    # In this computation, we're using the sub.renewable.resource for name matching because the resource
    # for scavenging is assigned a different name from the corresponding commodity and supplysector
    # (to avoid having two markets with the same name)
    L202.ag_Feed_Mt_R_C_Y.mlt %>%
      filter(GCAM_commodity %in% A_agRsrcCurves$sub.renewable.resource) %>%
      group_by(region, GCAM_region_ID, GCAM_commodity) %>%
      summarise(maxSubResource = max(value)) %>%
      # bind the two tables together, re-name the columns to the appropriate headers, and add in a sub.renewable.resource category
      ungroup %>%
      mutate(sub.renewable.resource = GCAM_commodity,
             maxSubResource = round(maxSubResource, aglu.DIGITS_CALOUTPUT),
             year.fillout = min(MODEL_BASE_YEARS)) %>%
      left_join_keep_first_only(select(A_agRsrcCurves, sub.renewable.resource, renewresource), by = "sub.renewable.resource") %>%
      select(LEVEL2_DATA_NAMES[["maxSubResource"]]) ->
      L202.maxSubResource

    # L202.RenewRsrcCurves
    A_agRsrcCurves %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["RenewRsrcCurves"]], GCAM_region_names) ->
      L202.RenewRsrcCurves

    # L261.ResTechShrwt_C
    A_agSubRsrc %>%
      rename(resource = renewresource, subresource = sub.renewable.resource) %>%
      repeat_add_columns(GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subresource,
             share.weight = 1.0) %>%
      select(LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
    L202.ResTechShrwt

    # L202.UnlimitedRenewRsrcCurves
    A_agUnlimitedRsrcCurves %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["UnlimitRsrc"]], GCAM_region_names) ->
      L202.UnlimitedRenewRsrcCurves

    # L202.UnlimitedRenewRsrcPrice (105-112)
    L202.an_prP_R_C_75USDkg <- left_join_error_no_match(L1321.an_prP_R_C_75USDkg,
                                                         GCAM_region_names, by = "GCAM_region_ID") %>%
      select(-GCAM_region_ID)

    A_agUnlimitedRsrcCurves %>%
      select(unlimited.resource, year, price) %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["UnlimitRsrcPrice"]], GCAM_region_names) %>%
      # replace these default prices with the prices calculated in L1321
      left_join(L202.an_prP_R_C_75USDkg, by = c("region", unlimited.resource = "GCAM_commodity")) %>%
      mutate(price = if_else(!is.na(value), value, price)) %>%
      select(LEVEL2_DATA_NAMES[["UnlimitRsrcPrice"]]) ->
      L202.UnlimitedRenewRsrcPrice

    # L202.Supplysector_in: generic supplysector info for inputs to animal production (114-122)
    A_an_input_supplysector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L202.Supplysector_in

    # L202.SubsectorAll_in: generic subsector info for inputs to animal production technologies
    A_an_input_subsector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorAll"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L202.SubsectorAll_in

    # If any subsectors have a to-value provided, generate another table: L202.SubsectorInterpTo_in
    if(any(!is.na(A_an_input_subsector$to.value))) {
      A_an_input_subsector %>%
        filter(!is.na(to.value)) %>%
        write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], GCAM_region_names = GCAM_region_names) ->
        L202.SubsectorInterpTo_in
    }

    # L202.StubTech_in: identification of stub technologies for inputs to animal production (124-140)
    A_an_input_technology %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L202.StubTech_in

    # L202.StubTechInterp_in: generic technology info for inputs to animal production
    A_an_input_technology %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["TechInterp"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L202.StubTechInterp_in

    # L202.GlobalTechCoef_in: coefficients for inputs to animal production
    A_an_input_technology %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L202.GlobalTechCoef_in

    # L202.GlobalTechShrwt_in: Default shareweights for inputs to animal production
    A_an_input_globaltech_shrwt %>%
      gather_years(value_col = "share.weight") %>%
      filter(year %in% MODEL_YEARS) %>%
      complete(year = MODEL_YEARS, fill = list(supplysector = A_an_input_globaltech_shrwt$supplysector),
               subsector = A_an_input_globaltech_shrwt$subsector, technology = A_an_input_globaltech_shrwt$technology) %>%
      mutate(share.weight = approx_fun(year, share.weight, rule = 2),
             sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "share.weight") ->
      L202.GlobalTechShrwt_in

    # L202.StubTechProd_in: base year output of the inputs (feed types) to animal production (142-149)
    A_an_input_technology %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      mutate(stub.technology = technology) %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      # not every region/technology/year has a match, so need to use left_join
      left_join(L202.ag_Feed_Mt_R_C_Y.mlt, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(calOutputValue = round(value, aglu.DIGITS_CALOUTPUT)) %>%
      # subsector and technology shareweights (subsector requires the year as well)
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L202.StubTechProd_in

    # L202.Supplysector_an: generic animal production supplysector info (159-162)
    A_an_supplysector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L202.Supplysector_an

    # L202.SubsectorAll_an: generic animal production subsector info (164-167)
    A_an_subsector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorAll"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L202.SubsectorAll_an

    # L202.GlobalTechShrwt_an: global technology default share-weights
    A_an_technology %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(sector.name = supplysector,
             subsector.name = subsector,
             share.weight = 1) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]], GCAM_region_names) ->
      L202.GlobalTechShrwt_an

    # L202.StubTechInterp_an: shareweight interpolation for animal production technologies (173-175)
    A_an_technology %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["TechInterp"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L202.StubTechInterp_an

    # L202.StubTechProd_an: animal production by technology and region (177-199)
    A_an_technology %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      rename(stub.technology = technology) %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(L202.an_Prod_Mt_R_C_Sys_Fd_Y.mlt,
                               by = c("region", "supplysector" = "GCAM_commodity",
                                      "subsector" = "system", "stub.technology" = "feed",
                                      "year")) %>%
      mutate(calOutputValue = round(value, aglu.DIGITS_CALOUTPUT),
             # subsector and technology shareweights (subsector requires the year as well)
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L202.StubTechProd_an

    # In general, technologies need to be aggregated to compute subsector share-weights. If any technology
    # within a subsector has a value > 0, then the subsector share-weight should be 1.
    # Some subsectors have multiple technologies, so shareweights should be derived from aggregation
    L202.StubTechProd_an %>%
      group_by(region, supplysector, subsector, year) %>%
      summarise(subs.share.weight = sum(calOutputValue)) %>%
      ungroup %>%
      mutate(subs.share.weight = if_else(subs.share.weight > 0, 1, 0)) ->
      L202.an_subs_sw

    # Override the share weights in the production table
    L202.StubTechProd_an %>%
      select(-subs.share.weight) %>%
      left_join_error_no_match(L202.an_subs_sw, by = c("region", "supplysector", "subsector", "year")) ->
      L202.StubTechProd_an

    # L202.StubTechCoef_an: animal production input-output coefficients by technology and region (201-214)
    A_an_technology %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Tech"]], "minicam.energy.input", "market.name"), GCAM_region_names) %>%
      rename(stub.technology = technology) %>%
      repeat_add_columns(tibble(year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS))) %>%
      # not everything has a match so need to use left_join
      left_join(L202.an_FeedIO_R_C_Sys_Fd_Y.mlt,
                by = c("region", "supplysector" = "GCAM_commodity",
                       "subsector" = "system", "minicam.energy.input" = "feed",
                       "year")) %>%
      mutate(coefficient = round(value, aglu.DIGITS_CALOUTPUT)) %>%
      select(-value, -GCAM_region_ID) ->
      L202.StubTechCoef_an

    # For values beyond the coefficient time series, use the final available year
    final_coef_year <- max(L202.an_FeedIO_R_C_Sys_Fd_Y.mlt$year)
    final_coef_year_data <- filter(L202.StubTechCoef_an, year == final_coef_year) %>% select(-year)
    L202.StubTechCoef_an %>%
      filter(year > final_coef_year) %>%
      select(-coefficient) %>%
      left_join(final_coef_year_data, by = c("region", "supplysector", "subsector", "stub.technology", "minicam.energy.input", "market.name")) %>%
      bind_rows(filter(L202.StubTechCoef_an, ! year > final_coef_year)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L202.StubTechCoef_an

    # Supplemental calculation of non-input cost of animal production (216-261)
    # Calculate non-feed costs of animal production based on regional producer prices and feed costs
    # First, calculate the weighted average price across the different feed types (supplysectors)
    L202.StubTechProd_in %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(region, supplysector, subsector, stub.technology, calOutputValue) ->
      L202.ag_Feed_P_share_R_C

    # Use the producer prices of crops in each exporting region to calculate the global "traded" crop prices
    L202.ag_tradedP_C_75USDkg <- filter(L1091.GrossTrade_Mt_R_C_Y, year == max(MODEL_BASE_YEARS)) %>%
      inner_join(L1321.ag_prP_R_C_75USDkg, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      mutate(Exp_wtd_price = GrossExp_Mt * value) %>%
      group_by(GCAM_commodity) %>%
      summarise(GrossExp_Mt = sum(GrossExp_Mt),
                Exp_wtd_price = sum(Exp_wtd_price)) %>%
      ungroup() %>%
      mutate(tradedP = Exp_wtd_price / GrossExp_Mt) %>%
      select(GCAM_commodity, tradedP)

    # Calculate the share of domestic supply (i.e., total consumption) that is from imports
    L202.ag_ImpShare_Mt_R_C_Y <- filter(L109.ag_ALL_Mt_R_C_Y, year == max(MODEL_BASE_YEARS)) %>%
      select(GCAM_region_ID, GCAM_commodity, year, Supply_Mt) %>%
      left_join(L1091.GrossTrade_Mt_R_C_Y, by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%
      mutate(ImpShare = if_else(is.na(GrossImp_Mt) | Supply_Mt == 0, 0, GrossImp_Mt / Supply_Mt)) %>%
      select(GCAM_region_ID, GCAM_commodity, ImpShare)

    # Calculate the weighted average regional crop prices, as the global traded crop price times the
    # import share plus the local producer price times the domestic source share (1 - ImpShare)
    L202.ag_consP_R_C_75USDkg <- L1321.ag_prP_R_C_75USDkg %>%
      rename(PrP = value) %>%
      left_join_error_no_match(L202.ag_ImpShare_Mt_R_C_Y, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      left_join_error_no_match(L202.ag_tradedP_C_75USDkg, by = "GCAM_commodity", ignore_columns = "tradedP") %>%
      mutate(value = if_else(is.na(tradedP),
                             PrP,
                             PrP * (1 - ImpShare) + tradedP * ImpShare)) %>%
      select(GCAM_region_ID, GCAM_commodity, value)

    L202.prP_R_C_75USDkg <- bind_rows(L202.ag_consP_R_C_75USDkg, L1321.an_prP_R_C_75USDkg) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, GCAM_commodity, price = value)
    L202.rsrcP_R_C_75USDkg <- filter(A_agRsrcCurves, grade == "grade 2") %>%
      select(GCAM_commodity = sub.renewable.resource, calPrice = extractioncost)

    L202.ag_Feed_Prices <- L132.ag_an_For_Prices %>%
      bind_rows(L202.rsrcP_R_C_75USDkg) %>%
      write_to_all_regions(c("region", "GCAM_commodity", "calPrice"), GCAM_region_names) %>%
      rename(default_price = calPrice) %>%
      left_join(L202.prP_R_C_75USDkg, by = c("region", "GCAM_commodity")) %>%
      mutate(price = if_else(is.na(price), default_price, price)) %>%
      select(region, GCAM_commodity, price)

    L202.ag_Feed_P_share_R_C %>%
      # not all stub.technology values are present as commodities in the price data; DDGS and feedcakes return NA and are dropped
      left_join(L202.ag_Feed_Prices, by = c("region", "stub.technology" = "GCAM_commodity")) %>%
      drop_na(price) ->
      L202.ag_Feed_P_share_R_C

    #
    L202.ag_Feed_P_share_R_C %>%
      group_by(region, supplysector) %>%
      summarise(revenue = sum(price * calOutputValue),
                weight = sum(calOutputValue)) %>%
      ungroup() %>%
      mutate(price = revenue / weight) %>%
      select(region, supplysector, price) ->
      L202.ag_FeedCost_USDkg_R_F

    # gpk 8/5/2020 - compute default animal commodity prices in case of missing values
    # China prices, to be used in Taiwan
    L202.ChinaAnPrices <- L1321.an_prP_R_C_75USDkg %>%
      filter(GCAM_region_ID == GCAM_region_names$GCAM_region_ID[GCAM_region_names$region == "China"]) %>%
      select(GCAM_commodity, ChinaCommodityPrice_USDkg = value)

    # Median prices, to be used elsewhere
    L202.DefaultAnPrices <- L1321.an_prP_R_C_75USDkg %>%
      group_by(GCAM_commodity) %>%
      summarise(DefaultCommodityPrice_USDkg = median(value)) %>%
      ungroup()

    # Calculate the total cost of all inputs, for each animal commodity, first matching in the feed quantity and the price
    L202.an_Prod_Mt_R_C_Sys_Fd_Y.mlt %>%
      filter(year == max(MODEL_BASE_YEARS),
             !region %in% aglu.NO_AGLU_REGIONS) %>%
      rename(Prod_Mt = value) %>%
      left_join_error_no_match(select(L202.an_Feed_Mt_R_C_Sys_Fd_Y.mlt, GCAM_region_ID, GCAM_commodity, system, feed, year, Feed_Mt = value),
                               by = c("GCAM_region_ID", "GCAM_commodity", "system", "feed", "year")) %>%
      left_join_error_no_match(L202.ag_FeedCost_USDkg_R_F, by = c("region", "feed" = "supplysector")) %>%
      rename(FeedPrice_USDkg = price) %>%
      left_join(L1321.an_prP_R_C_75USDkg, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      # gpk 2020-08-05 We don't have animal commodity price data for Taiwan here. Set that to China's data, and for any
      # other regions that may have missing data, use the median values computed above as defaults
      left_join_error_no_match(L202.ChinaAnPrices, by = "GCAM_commodity") %>%
      left_join_error_no_match(L202.DefaultAnPrices, by = "GCAM_commodity") %>%
      mutate(CommodityPrice_USDkg = if_else(region == "Taiwan" & is.na(value), ChinaCommodityPrice_USDkg, value),
             CommodityPrice_USDkg = if_else(is.na(CommodityPrice_USDkg), DefaultCommodityPrice_USDkg, CommodityPrice_USDkg)) %>%
      # multiply prices by quantities to calculate feed expenditures and commodity sales revenues
      mutate(SalesRevenue_bilUSD = Prod_Mt * CommodityPrice_USDkg,
             FeedCost_bilUSD = Feed_Mt * FeedPrice_USDkg) %>%
      # group by region, meat commodity, and system (i.e., assuming the same non-feed cost for the 'mixed' and 'pastoral' systems
      # irrespective of the feed type, but the costs can differ by region, commodity, and system)
      group_by(region, GCAM_commodity, system) %>%
      summarise(Prod_Mt = sum(Prod_Mt),
                SalesRevenue_bilUSD = sum(SalesRevenue_bilUSD),
                FeedCost_bilUSD = sum(FeedCost_bilUSD)) %>%
      ungroup() %>%
      mutate(nonFeedCost = if_else(Prod_Mt == 0, 0, (SalesRevenue_bilUSD - FeedCost_bilUSD) / Prod_Mt)) ->
      L202.an_nonFeedCost_R_C

    # L202.StubTechCost_an: costs of animal production technologies (263-270)
    A_an_technology %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%
      mutate(stub.technology = technology,
             minicam.non.energy.input = "non-feed") %>%
      left_join_error_no_match(select(L202.an_nonFeedCost_R_C, region, GCAM_commodity, system, nonFeedCost),
                               by = c("region", supplysector = "GCAM_commodity", subsector = "system")) %>%
      mutate(input.cost = round(nonFeedCost, aglu.DIGITS_CALPRICE)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCost"]]) ->
      L202.StubTechCost_an




    # Remove any regions for which agriculture and land use are not modeled (308-320)
    # Also, remove DDGS and feedcake subsectors and technologies in regions where these commodities are not available
    # First need to figure out what the names of these subsectors are, and which regions to exclude
    A_regions %>%
      filter(ethanol != "corn ethanol", paste(biomassOil_tech, biodiesel) != "OilCrop biodiesel") %>%
      select(-region) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region) ->
      L202.no_ddgs_regions

    A_an_input_technology %>%
      filter(grepl("DDGS", subsector)) %>%
      repeat_add_columns(L202.no_ddgs_regions) %>%
      select(region, supplysector, subsector, technology) ->
      L202.no_ddgs_regions_subs

    L202.RenewRsrc <- filter(L202.RenewRsrc, !region %in% aglu.NO_AGLU_REGIONS)
    L202.RenewRsrcPrice <- filter(L202.RenewRsrcPrice, !region %in% aglu.NO_AGLU_REGIONS)
    L202.maxSubResource <- filter(L202.maxSubResource, !region %in% aglu.NO_AGLU_REGIONS)
    L202.RenewRsrcCurves <- filter(L202.RenewRsrcCurves, !region %in% aglu.NO_AGLU_REGIONS)
    L202.ResTechShrwt <- filter(L202.ResTechShrwt, !region %in% aglu.NO_AGLU_REGIONS)
    L202.UnlimitedRenewRsrcCurves <- filter(L202.UnlimitedRenewRsrcCurves, !region %in% aglu.NO_AGLU_REGIONS)
    L202.UnlimitedRenewRsrcPrice <- filter(L202.UnlimitedRenewRsrcPrice, !region %in% aglu.NO_AGLU_REGIONS)
    L202.Supplysector_in <- filter(L202.Supplysector_in, !region %in% aglu.NO_AGLU_REGIONS)

    L202.SubsectorAll_in %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%
      anti_join(L202.no_ddgs_regions_subs, by = c("region", "supplysector", "subsector")) ->
      L202.SubsectorAll_in
    if(exists("L202.SubsectorInterpTo_in")){
      L202.SubsectorInterpTo_in %>%
        filter(!region %in% aglu.NO_AGLU_REGIONS) %>%
        anti_join(L202.no_ddgs_regions_subs, by = c("region", "supplysector", "subsector")) ->
        L202.SubsectorInterpTo_in
    }
    L202.StubTech_in %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%
      anti_join(L202.no_ddgs_regions_subs, by = c("region", "supplysector", "subsector", "stub.technology" = "technology")) ->
      L202.StubTech_in
    L202.StubTechInterp_in %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%
      anti_join(L202.no_ddgs_regions_subs, by = c("region", "supplysector", "subsector", "stub.technology" = "technology")) ->
      L202.StubTechInterp_in
    L202.StubTechProd_in %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%
      anti_join(L202.no_ddgs_regions_subs, by = c("region", "supplysector", "subsector", "stub.technology" = "technology")) ->
      L202.StubTechProd_in

    L202.Supplysector_an <- filter(L202.Supplysector_an, !region %in% aglu.NO_AGLU_REGIONS)
    L202.SubsectorAll_an <- filter(L202.SubsectorAll_an, !region %in% aglu.NO_AGLU_REGIONS)
    L202.StubTechInterp_an <- filter(L202.StubTechInterp_an, !region %in% aglu.NO_AGLU_REGIONS)
    L202.StubTechProd_an <- filter(L202.StubTechProd_an, !region %in% aglu.NO_AGLU_REGIONS)
    L202.StubTechCoef_an <- filter(L202.StubTechCoef_an, !region %in% aglu.NO_AGLU_REGIONS)


    # Produce outputs
    L202.RenewRsrc %>%
      add_title("Generic resource attributes") %>%
      add_units("NA") %>%
      add_comments("A_agRsrc written to all regions") %>%
      add_legacy_name("L202.RenewRsrc") %>%
      add_precursors("aglu/A_agRsrc", "common/GCAM_region_names") ->
      L202.RenewRsrc

    L202.RenewRsrcPrice %>%
      add_title("Resource prices") %>%
      add_units("1975$/kg") %>%
      add_comments("A_agRsrc written to all regions with `year` set to first model base year and `price` = 1") %>%
      add_legacy_name("L202.RenewRsrcPrice") %>%
      add_precursors("aglu/A_agRsrc", "common/GCAM_region_names") ->
      L202.RenewRsrcPrice

    L202.maxSubResource %>%
      add_title("Maximum amount of resource production allowed in any period") %>%
      add_units("Mt/yr") %>%
      add_comments("Computed as the maximum of all base periods, for each region and resource") %>%
      add_legacy_name("L202.maxSubResource") %>%
      add_precursors("L109.an_ALL_Mt_R_C_Y", "L108.ag_Feed_Mt_R_C_Y", "aglu/A_agRsrcCurves", "common/GCAM_region_names") ->
      L202.maxSubResource

    L202.RenewRsrcCurves %>%
      add_title("Renewable resource curves") %>%
      add_units("available: Unitless fraction of maxSubResource; extractioncost: 1975$/kg") %>%
      add_comments("A_agRsrcCurves written to all regions") %>%
      add_legacy_name("L202.RenewRsrcCurves") %>%
      add_precursors("aglu/A_agRsrcCurves", "common/GCAM_region_names") ->
      L202.RenewRsrcCurves

    L202.ResTechShrwt %>%
      add_title("Technology share-weights for the renewable resources") %>%
      add_units("NA") %>%
      add_comments("Mostly just to provide a shell of a technology for the resource to use") %>%
      add_precursors("aglu/A_agSubRsrc", "common/GCAM_region_names") ->
      L202.ResTechShrwt

    L202.UnlimitedRenewRsrcCurves %>%
      add_title("Unlimited renewable resource curves") %>%
      add_units("Unitless") %>%
      add_comments("A_agUnlimitedRsrcCurves written to all regions") %>%
      add_legacy_name("L202.UnlimitedRenewRsrcCurves") %>%
      add_precursors("aglu/A_agUnlimitedRsrcCurves", "common/GCAM_region_names") ->
      L202.UnlimitedRenewRsrcCurves

    L202.UnlimitedRenewRsrcPrice %>%
      add_title("Unlimited renewable resource price") %>%
      add_units("1975$/kg") %>%
      add_comments("A_agUnlimitedRsrcCurves written to all regions") %>%
      add_legacy_name("L202.UnlimitedRenewRsrcPrice") %>%
      add_precursors("aglu/A_agUnlimitedRsrcCurves", "common/GCAM_region_names") ->
      L202.UnlimitedRenewRsrcPrice

    L202.Supplysector_in %>%
      add_title("Generic supplysector info for inputs to animal production") %>%
      add_units("NA") %>%
      add_comments("A_an_input_supplysector written to all regions") %>%
      add_legacy_name("L202.Supplysector_in") %>%
      add_precursors("aglu/A_an_input_supplysector", "common/GCAM_region_names") ->
      L202.Supplysector_in

    L202.SubsectorAll_in %>%
      add_title("Generic subsector info for inputs to animal production technologies") %>%
      add_units("NA") %>%
      add_comments("A_an_input_subsector written to all regions") %>%
      add_legacy_name("L202.SubsectorAll_in") %>%
      add_precursors("aglu/A_an_input_subsector", "energy/A_regions", "common/GCAM_region_names") ->
      L202.SubsectorAll_in

    if(exists("L202.SubsectorInterpTo_in")) {
      L202.SubsectorInterpTo_in %>%
        add_title("Subsector interpolation rules with to-value specified") %>%
        add_units("NA") %>%
        add_comments("From A_an_input_subsector, written to all regions if used") %>%
        same_precursors_as(L202.SubsectorAll_in) ->
        L202.SubsectorInterpTo_in
    } else {
      missing_data()  %>%
        add_comments("Empty data table") ->
        L202.SubsectorInterpTo_in
    }

    L202.StubTech_in %>%
      add_title("Identification of stub technologies for inputs to animal production") %>%
      add_units("NA") %>%
      add_comments("A_an_input_technology written to all regions") %>%
      add_legacy_name("L202.StubTech_in") %>%
      add_precursors("aglu/A_an_input_technology", "energy/A_regions", "common/GCAM_region_names") ->
      L202.StubTech_in

    L202.StubTechInterp_in %>%
      add_title("Generic technology info for inputs to animal production") %>%
      add_units("NA") %>%
      add_comments("A_an_input_technology written to all regions") %>%
      add_legacy_name("L202.StubTechInterp_in") %>%
      add_precursors("aglu/A_an_input_technology", "energy/A_regions", "common/GCAM_region_names") ->
      L202.StubTechInterp_in

    L202.GlobalTechCoef_in %>%
      add_title("Coefficients for inputs to animal production") %>%
      add_units("NA") %>%
      add_comments("A_an_input_technology across all base and future years") %>%
      add_comments("These technologies are pass-through, used for competing different primary sources for animal feed commodities.
                   No transformations are taking place, and the coefficients are 1 in all years.") %>%
      add_legacy_name("L202.GlobalTechCoef_in") %>%
      add_precursors("aglu/A_an_input_technology", "common/GCAM_region_names") ->
      L202.GlobalTechCoef_in

    L202.GlobalTechShrwt_in %>%
      add_title("Default shareweights for inputs to animal production") %>%
      add_units("NA") %>%
      add_comments("A_an_input_globaltech_shrwt interpolated to all model years") %>%
      add_legacy_name("L202.GlobalTechShrwt_in") %>%
      add_precursors("aglu/A_an_input_globaltech_shrwt", "common/GCAM_region_names") ->
      L202.GlobalTechShrwt_in

    L202.StubTechProd_in %>%
      add_title("Base year output of the inputs (feed types) to animal production") %>%
      add_units("Mt/yr") %>%
      add_comments("Calibrated primary sources of animal feed commodities, specific to each region and time period.") %>%
      add_legacy_name("L202.StubTechProd_in") %>%
      add_precursors("aglu/A_an_input_technology", "L107.an_FeedIO_R_C_Sys_Fd_Y",
                     "energy/A_regions", "common/GCAM_region_names") ->
      L202.StubTechProd_in

    L202.Supplysector_an %>%
      add_title("Generic animal production supplysector info") %>%
      add_units("NA") %>%
      add_comments("A_an_supplysector written to all regions") %>%
      add_legacy_name("L202.Supplysector_an") %>%
      add_precursors("aglu/A_an_supplysector", "common/GCAM_region_names") ->
      L202.Supplysector_an

    L202.SubsectorAll_an %>%
      add_title("Generic animal production subsector info") %>%
      add_units("NA") %>%
      add_comments("A_an_subsector written to all regions") %>%
      add_legacy_name("L202.SubsectorAll_an") %>%
      add_precursors("aglu/A_an_subsector", "common/GCAM_region_names") ->
      L202.SubsectorAll_an

    L202.GlobalTechShrwt_an %>%
      add_title("Default share-weights of global technologies for animal production") %>%
      add_units("NA") %>%
      add_comments("A_an_technology written to all regions") %>%
      add_precursors("aglu/A_an_technology", "common/GCAM_region_names") ->
      L202.GlobalTechShrwt_an

    L202.StubTechInterp_an %>%
      add_title("Shareweight interpolation for animal production technologies") %>%
      add_units("NA") %>%
      add_comments("A_an_technology written to all regions") %>%
      add_legacy_name("L202.StubTechInterp_an") %>%
      add_precursors("aglu/A_an_technology", "common/GCAM_region_names") ->
      L202.StubTechInterp_an

    L202.StubTechProd_an %>%
      add_title("Calibrated animal commodity production by technology") %>%
      add_units("Unitless") %>%
      add_comments("Animal commodity production by subsector (mixed vs pastoral system) and technology (modeled feed commodity).") %>%
      add_legacy_name("L202.StubTechProd_an") %>%
      add_precursors("aglu/A_an_technology", "L107.an_Prod_Mt_R_C_Sys_Fd_Y", "common/GCAM_region_names") ->
      L202.StubTechProd_an

    L202.StubTechCoef_an %>%
      add_title("Animal production input-output coefficients by technology and region") %>%
      add_units("kg of dry feed per kg of produced animal commodity") %>%
      add_comments("Animal production input-output coefficients written across model base years and regions") %>%
      add_legacy_name("L202.StubTechCoef_an") %>%
      add_precursors("aglu/A_an_technology", "L107.an_Prod_Mt_R_C_Sys_Fd_Y", "common/GCAM_region_names") ->
      L202.StubTechCoef_an

    L202.StubTechCost_an %>%
      add_title("Costs of animal production technologies") %>%
      add_units("1975$/kg") %>%
      add_comments("Animal feed cost, prices, and technology") %>%
      add_comments("This is the non-feed cost; i.e., all costs of producing animal commodities except for the feed.") %>%
      add_legacy_name("L202.StubTechCost_an") %>%
      same_precursors_as(L202.StubTechCoef_an) %>%
      add_precursors("L132.ag_an_For_Prices", "L1321.ag_prP_R_C_75USDkg", "L1321.an_prP_R_C_75USDkg",
                     "L107.an_Feed_Mt_R_C_Sys_Fd_Y", "L1091.GrossTrade_Mt_R_C_Y", "L109.ag_ALL_Mt_R_C_Y") ->
      L202.StubTechCost_an

    # Return also the consumer prices, to be made available elsewhere
    L202.ag_consP_R_C_75USDkg %>%
      add_title("Consumer costs of crops") %>%
      add_units("1975$/kg") %>%
      add_comments("Computed from weighted average of domestically sourced crops (which use producer prices) and imports") %>%
      add_comments("Imported crop prices are computed from weighted average of producer prices of exporting countries") %>%
      add_precursors("L1321.ag_prP_R_C_75USDkg", "L109.ag_ALL_Mt_R_C_Y", "L1091.GrossTrade_Mt_R_C_Y") ->
      L202.ag_consP_R_C_75USDkg


    return_data(L202.RenewRsrc, L202.RenewRsrcPrice, L202.maxSubResource, L202.RenewRsrcCurves, L202.ResTechShrwt,
                L202.UnlimitedRenewRsrcCurves, L202.UnlimitedRenewRsrcPrice, L202.Supplysector_in,
                L202.SubsectorAll_in, L202.SubsectorInterpTo_in, L202.StubTech_in, L202.StubTechInterp_in, L202.GlobalTechCoef_in,
                L202.GlobalTechShrwt_in, L202.StubTechProd_in, L202.Supplysector_an, L202.SubsectorAll_an,
                L202.GlobalTechShrwt_an, L202.StubTechInterp_an, L202.StubTechProd_an, L202.StubTechCoef_an,
                L202.StubTechCost_an, L202.ag_consP_R_C_75USDkg)
  } else {
    stop("Unknown command")
  }
}
