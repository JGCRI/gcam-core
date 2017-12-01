#' module_aglu_L202.an_input
#'
#' Produce a wide range of animal-related resource tables: production, import, resource curves.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L202.RenewRsrc}, \code{L202.RenewRsrcPrice}, \code{L202.maxSubResource},
#' \code{L202.RenewRsrcCurves}, \code{L202.UnlimitedRenewRsrcCurves}, \code{L202.UnlimitedRenewRsrcPrice},
#' \code{L202.Supplysector_in}, \code{L202.SubsectorAll_in}, \code{L202.StubTech_in}, \code{L202.StubTechInterp_in},
#' \code{L202.GlobalTechCoef_in}, \code{L202.GlobalTechShrwt_in}, \code{L202.StubTechProd_in},
#' \code{L202.Supplysector_an}, \code{L202.SubsectorAll_an}, \code{L202.StubTech_an}, \code{L202.StubTechInterp_an},
#' \code{L202.StubTechProd_an}, \code{L202.StubTechCoef_an}, \code{L202.GlobalTechCost_an},
#' \code{L202.GlobalRenewTech_imp_an}, \code{L202.StubTechFixOut_imp_an}. The corresponding file in the
#' original data system was \code{L202.an_input.R} (aglu level2).
#' @details This chunk produces 22 animal-related resource tables: production, import, resource curves.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL August 2017
module_aglu_L202.an_input <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A_regions",
             FILE = "aglu/A_agRsrc",
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
             "L109.an_ALL_Mt_R_C_Y",
             "L132.ag_an_For_Prices"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L202.RenewRsrc",
             "L202.RenewRsrcPrice",
             "L202.maxSubResource",
             "L202.RenewRsrcCurves",
             "L202.UnlimitedRenewRsrcCurves",
             "L202.UnlimitedRenewRsrcPrice",
             "L202.Supplysector_in",
             "L202.SubsectorAll_in",
             "L202.StubTech_in",
             "L202.StubTechInterp_in",
             "L202.GlobalTechCoef_in",
             "L202.GlobalTechShrwt_in",
             "L202.StubTechProd_in",
             "L202.Supplysector_an",
             "L202.SubsectorAll_an",
             "L202.StubTech_an",
             "L202.StubTechInterp_an",
             "L202.StubTechProd_an",
             "L202.StubTechCoef_an",
             "L202.GlobalTechCost_an",
             "L202.GlobalRenewTech_imp_an",
             "L202.StubTechFixOut_imp_an"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- market <- region <- renewresource <- GCAM_commodity <- GCAM_region_ID <- Prod_Mt <- value <-
      sub.renewable.resource <- year <- price <- unlimited.resource <- technology <- supplysector <-
      subsector <- share.weight <- calOutputValue <- subs.share.weight <- coefficient <- maxSubResource <-
      stub.technology <- output_supplysector <- grade <- extractioncost <- calPrice <- unit <- share_Fd <-
      feed <- wtd_price <- Feed_Mt <- FeedPrice_USDkg <- FeedCost_bilUSD <- CommodityPrice_USDkg <-
      FeedCost_USDkg <- nonFeedCost <- NetExp_Mt <- share.weight.year <- fixedOutput <- ethanol <-
      biomassOil_tech <- biodiesel <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_regions <- get_data(all_data, "energy/A_regions")
    A_agRsrc <- get_data(all_data, "aglu/A_agRsrc")
    A_agRsrcCurves <- get_data(all_data, "aglu/A_agRsrcCurves")
    A_agUnlimitedRsrcCurves <- get_data(all_data, "aglu/A_agUnlimitedRsrcCurves")
    A_an_input_supplysector <- get_data(all_data, "aglu/A_an_input_supplysector")
    A_an_input_subsector <- get_data(all_data, "aglu/A_an_input_subsector")
    A_an_input_technology <- get_data(all_data, "aglu/A_an_input_technology")
    A_an_input_globaltech_shrwt <- get_data(all_data, "aglu/A_an_input_globaltech_shrwt")
    A_an_supplysector <- get_data(all_data, "aglu/A_an_supplysector")
    A_an_subsector <- get_data(all_data, "aglu/A_an_subsector")
    A_an_technology <- get_data(all_data, "aglu/A_an_technology")
    L132.ag_an_For_Prices <- get_data(all_data, "L132.ag_an_For_Prices")

    # 2. Build tables
    # Base table for resources - add region names to Level1 data tables (lines 49-70 old file)

    # Following datasets are already 'long' so just skip the old interpolate_and_melt step
    # Helper function to get_data, join with GCAM region names, and filter to base years
    get_join_filter <- function(x) {   #
      get_data(all_data, x) %>%
        left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
        filter(year %in% BASE_YEARS)
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
      mutate(year = min(BASE_YEARS), price = 1) ->
      L202.RenewRsrcPrice

    # L202.maxSubResource: maximum amount of resource production allowed in any period (72-97)
    # Compute the maxsubresource as the maximum of all base periods, for each region and resource
    # Note that the supply curves can exceed this number, by setting "available" to a number > 1.
    # In this computation, we're using the sub.renewable.resource for name matching because the resource
    # for scavenging is assigned a different name from the corresponding commodity and supplysector
    # (to avoid having two markets with the same name)
    L202.an_ALL_Mt_R_C_Y %>%
      filter(GCAM_commodity %in% A_agRsrcCurves$sub.renewable.resource) %>%
      group_by(region, GCAM_region_ID, GCAM_commodity) %>%
      summarise(maxSubResource = max(Prod_Mt)) ->
      L202.maxSubResource_an

    L202.ag_Feed_Mt_R_C_Y.mlt %>%
      filter(GCAM_commodity %in% A_agRsrcCurves$sub.renewable.resource) %>%
      group_by(region, GCAM_region_ID, GCAM_commodity) %>%
      summarise(maxSubResource = max(value)) %>%
      # bind the two tables together, re-name the columns to the appropriate headers, and add in a sub.renewable.resource category
      bind_rows(L202.maxSubResource_an) %>%
      ungroup %>%
      mutate(sub.renewable.resource = GCAM_commodity,
             maxSubResource = round(maxSubResource, aglu.DIGITS_CALOUTPUT),
             year.fillout = min(BASE_YEARS)) %>%
      left_join_keep_first_only(select(A_agRsrcCurves, sub.renewable.resource, renewresource), by = "sub.renewable.resource") %>%
      select(LEVEL2_DATA_NAMES[["maxSubResource"]]) ->
      L202.maxSubResource

    # L202.RenewRsrcCurves
    A_agRsrcCurves %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["RenewRsrcCurves"]], GCAM_region_names) ->
      L202.RenewRsrcCurves

    # L202.UnlimitedRenewRsrcCurves
    A_agUnlimitedRsrcCurves %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["UnlimitRsrc"]], GCAM_region_names) ->
      L202.UnlimitedRenewRsrcCurves

    # L202.UnlimitedRenewRsrcPrice (105-112)
    A_agUnlimitedRsrcCurves %>%
      gather_years(value_col = "price") %>%
      select(unlimited.resource, year, price) %>%
      filter(year %in% BASE_YEARS) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["UnlimitRsrcPrice"]], GCAM_region_names) ->
      L202.UnlimitedRenewRsrcPrice

    # L202.Supplysector_in: generic supplysector info for inputs to animal production (114-122)
    A_an_input_supplysector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Supplysector"]], GCAM_region_names) ->
      L202.Supplysector_in

    # L202.SubsectorAll_in: generic subsector info for inputs to animal production technologies
    A_an_input_subsector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorAll"]], GCAM_region_names) ->
      L202.SubsectorAll_in

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
      repeat_add_columns(tibble(year = BASE_YEARS)) %>%
      # not every region/technology/year has a match, so need to use left_join
      left_join(L202.ag_Feed_Mt_R_C_Y.mlt, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(calOutputValue = round(value, aglu.DIGITS_CALOUTPUT)) %>%
      # the DDGS/feedcake rows at this point are all missing values, as they are not
      # available in the historical years; set them to zero
      replace_na(list(calOutputValue = 0)) %>%
      # subsector and technology shareweights (subsector requires the year as well)
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L202.StubTechProd_in

    # L202.Supplysector_an: generic animal production supplysector info (159-162)
    A_an_supplysector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]]), GCAM_region_names) ->
      L202.Supplysector_an

    # L202.SubsectorAll_an: generic animal production subsector info (164-167)
    A_an_subsector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorAll"]]), GCAM_region_names) ->
      L202.SubsectorAll_an

    # L202.StubTech_an: identification of stub technologies for animal production (169-171)
    A_an_technology %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L202.StubTech_an

    # L202.StubTechInterp_an: shareweight interpolation for animal production technologies (173-175)
    A_an_technology %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["TechInterp"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L202.StubTechInterp_an

    # L202.StubTechProd_an: animal production by technology and region (177-199)
    A_an_technology %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      rename(stub.technology = technology) %>%
      repeat_add_columns(tibble(year = BASE_YEARS)) %>%
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
      repeat_add_columns(tibble(year = c(BASE_YEARS, FUTURE_YEARS))) %>%
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
      left_join(final_coef_year_data, by = c("region", "supplysector", "subsector", "stub.technology", "minicam.energy.input", "market.name", "stub.technology")) %>%
      bind_rows(filter(L202.StubTechCoef_an, ! year > final_coef_year)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L202.StubTechCoef_an

    # Supplemental calculation of non-input cost of animal production (216-261)
    # Calculate non-feed costs of animal production based on US commodity prices and feed costs
    # First, calculate the weighted average price across the different feed types (supplysectors)
    Index_region <- GCAM_region_names$region[1]
    L202.StubTechProd_in %>%
      filter(region == Index_region, year == max(BASE_YEARS)) %>%
      select(region, supplysector, subsector, stub.technology, calOutputValue) ->
      L202.ag_Feed_P_share_R_C

    L202.ag_Feed_P_share_R_C %>%
      group_by(region, supplysector) %>%
      summarise(output_supplysector = sum(calOutputValue)) ->
      L202.ag_Feed_Mt_R_F

    L202.ag_Feed_P_share_R_C %>%
      left_join_error_no_match(L202.ag_Feed_Mt_R_F, by = c("region", "supplysector")) %>%
      mutate(share_Fd = calOutputValue / output_supplysector) %>%
      # not all stub.technology values are present as commodities in L132.ag_an_For_Prices and A_agRsrcCurves, so use left_join
      left_join(L132.ag_an_For_Prices, by = c("stub.technology" = "GCAM_commodity")) %>%
      left_join(filter(A_agRsrcCurves, grade == "grade 2") %>% select(sub.renewable.resource, extractioncost),
                by = c("stub.technology" = "sub.renewable.resource")) %>%
      mutate(price = if_else(stub.technology %in% A_agRsrcCurves$sub.renewable.resource, extractioncost, calPrice)) %>%
      select(-calPrice, -unit, -extractioncost) ->
      L202.ag_Feed_P_share_R_C

    # Remaining NA's are the ddgs/feedcakes. Output is zero so no need for a price;
    # still, go ahead and extract the name of this commodity for later use
    L202.ag_Feed_P_share_R_C %>%
      filter(is.na(price)) %>%
      select(supplysector, subsector, technology = stub.technology) %>%
      distinct ->
      L202.ddgs_names

    L202.ag_Feed_P_share_R_C %>%
      replace_na(list(price = 0)) %>%
      group_by(region, supplysector) %>%
      summarise(wtd_price = sum(price * share_Fd)) ->
      L202.ag_FeedCost_USDkg_R_F

    # Calculate the total cost of all inputs, for each animal commodity, first matching in the feed quantity and the price
    L202.an_Prod_Mt_R_C_Sys_Fd_Y.mlt %>%
      filter(year == max(BASE_YEARS), region == Index_region) %>%
      left_join_error_no_match(select(L202.an_Feed_Mt_R_C_Sys_Fd_Y.mlt, GCAM_region_ID, GCAM_commodity, system, feed, year, Feed_Mt = value),
                               by = c("GCAM_region_ID", "GCAM_commodity", "system", "feed", "year")) %>%
      left_join_error_no_match(L202.ag_FeedCost_USDkg_R_F, by = c("region", "feed" = "supplysector")) %>%
      rename(FeedPrice_USDkg = wtd_price) %>%
      # multiply price by quantity to calculate feed expenditure, and aggregat expenditure and production to get weighted avg cost
      mutate(FeedCost_bilUSD = Feed_Mt * FeedPrice_USDkg) %>%
      group_by(GCAM_region_ID, GCAM_commodity) %>%
      summarise(value = sum(value), FeedCost_bilUSD = sum(FeedCost_bilUSD)) %>%
      ungroup %>%
      mutate(FeedCost_USDkg = FeedCost_bilUSD / value) %>%
      left_join_error_no_match(select(L132.ag_an_For_Prices, GCAM_commodity, CommodityPrice_USDkg = calPrice), by = "GCAM_commodity") %>%
      mutate(nonFeedCost = pmax(aglu.MIN_AN_NONINPUT_COST, CommodityPrice_USDkg - FeedCost_USDkg)) ->
      L202.an_FeedCost_R_C

    # L202.GlobalTechCost_an: costs of animal production technologies (263-270)
    A_an_technology %>%
      repeat_add_columns(tibble(year = c(BASE_YEARS, FUTURE_YEARS))) %>%
      mutate(sector.name = supplysector, subsector.name = subsector) %>%
      mutate(minicam.non.energy.input = "non-energy") %>%
      left_join_error_no_match(select(L202.an_FeedCost_R_C, GCAM_commodity, nonFeedCost), by = c("supplysector" = "GCAM_commodity")) %>%
      mutate(input.cost = round(nonFeedCost, aglu.DIGITS_CALPRICE)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]]) ->
      L202.GlobalTechCost_an

    # L202.GlobalRenewTech_imp_an: generic technology info for animal imports (272-277)
    tibble::as_tibble(expand.grid(sector.name = A_an_supplysector$supplysector,
                                  subsector.name = "Imports",
                                  technology = "Imports",
                                  renewable.input = "renewable",
                                  year = c(BASE_YEARS, FUTURE_YEARS))) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["GlobalRenewTech"]], GCAM_region_names) ->
      L202.GlobalRenewTech_imp_an

    # L202.StubTechFixOut_imp_an: animal imports for net importing regions in all periods (279-291)
    tibble(supplysector = A_an_supplysector$supplysector,
           subsector = "Imports",
           stub.technology = "Imports") %>%
      # write to all regions before repeating by years so that future year values will copy correctly
      write_to_all_regions(LEVEL2_DATA_NAMES[["StubTech"]], GCAM_region_names) %>%
      repeat_add_columns(tibble(year = c(BASE_YEARS, FUTURE_YEARS))) %>%
      # not every region and supplysector is present in L202.an_ALL_Mt_R_C_Y so use left_join
      left_join(select(L202.an_ALL_Mt_R_C_Y, region, GCAM_commodity, year, NetExp_Mt),
                by = c("region", "supplysector" = "GCAM_commodity", "year")) %>%
      mutate(fixedOutput = pmax(0, round(-1 * NetExp_Mt, aglu.DIGITS_CALOUTPUT))) %>%
      mutate(share.weight.year = year, subs.share.weight = 0, tech.share.weight = 0) %>%
      select(LEVEL2_DATA_NAMES[["StubTechFixOut"]]) ->
      L202.StubTechFixOut_imp_an

    # For values beyond the final base year, copy the final base year forward (293-306)
    # NOTE: This is complicated. Currently the base-service read in to the model is not used in years after the final
    # calibration year. If actual historical values are used here in historical years after the final calibration year,
    # the model will not solve as the supply of the Exports_* markets will be set while the demand will be purely inelastic,
    # carried forward with no changes from the final calibration year. This could theoretically be overcome in most
    # instances by reading in time- and region-specific income elasticities that return the correct values for each
    # historical year after the final calibration year. This is not done, and is not recommended, as any regions that switch
    # between imports and exports will have elasiticies of +/- Inf; the historical data cannot be represented.
    final_an_exp_year <- max(BASE_YEARS)
    final_an_exp_year_data <- filter(L202.StubTechFixOut_imp_an, year == final_an_exp_year) %>% select(-year, -share.weight.year)
    L202.StubTechFixOut_imp_an %>%
      filter(year > final_an_exp_year) %>%
      select(-fixedOutput) %>%
      left_join_error_no_match(final_an_exp_year_data, by = c("region", "supplysector", "subsector", "stub.technology", "subs.share.weight", "tech.share.weight")) %>%
      bind_rows(filter(L202.StubTechFixOut_imp_an, ! year > final_an_exp_year)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechFixOut"]]) ->
      L202.StubTechFixOut_imp_an

    # Remove any regions for which agriculture and land use are not modeled (308-320)
    # Also, remove DDGS and feedcake subsectors and technologies in regions where these commodities are not available
    # First need to figure out what the names of these subsectors are, and which regions to exclude
    A_regions %>%
      filter(ethanol != "corn ethanol", paste(biomassOil_tech, biodiesel) != "OilCrop biodiesel") %>%
      select(-region) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region) ->
      L202.no_ddgs_regions

    L202.ddgs_names %>%
      repeat_add_columns(L202.no_ddgs_regions) %>%
      select(region, supplysector, subsector, technology) %>%
      distinct ->
      L202.no_ddgs_regions_subs

    L202.RenewRsrc <- filter(L202.RenewRsrc, !region %in% aglu.NO_AGLU_REGIONS)
    L202.RenewRsrcPrice <- filter(L202.RenewRsrcPrice, !region %in% aglu.NO_AGLU_REGIONS)
    L202.maxSubResource <- filter(L202.maxSubResource, !region %in% aglu.NO_AGLU_REGIONS)
    L202.RenewRsrcCurves <- filter(L202.RenewRsrcCurves, !region %in% aglu.NO_AGLU_REGIONS)
    L202.UnlimitedRenewRsrcCurves <- filter(L202.UnlimitedRenewRsrcCurves, !region %in% aglu.NO_AGLU_REGIONS)
    L202.UnlimitedRenewRsrcPrice <- filter(L202.UnlimitedRenewRsrcPrice, !region %in% aglu.NO_AGLU_REGIONS)
    L202.Supplysector_in <- filter(L202.Supplysector_in, !region %in% aglu.NO_AGLU_REGIONS)

    L202.SubsectorAll_in %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%
      anti_join(L202.no_ddgs_regions_subs, by = c("region", "supplysector", "subsector")) ->
      L202.SubsectorAll_in
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
    L202.StubTech_an <- filter(L202.StubTech_an, !region %in% aglu.NO_AGLU_REGIONS)
    L202.StubTechInterp_an <- filter(L202.StubTechInterp_an, !region %in% aglu.NO_AGLU_REGIONS)
    L202.StubTechProd_an <- filter(L202.StubTechProd_an, !region %in% aglu.NO_AGLU_REGIONS)
    L202.StubTechCoef_an <- filter(L202.StubTechCoef_an, !region %in% aglu.NO_AGLU_REGIONS)
    L202.StubTechFixOut_imp_an <- filter(L202.StubTechFixOut_imp_an, !region %in% aglu.NO_AGLU_REGIONS)

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

    L202.StubTech_an %>%
      add_title("Identification of stub technologies for animal production") %>%
      add_units("NA") %>%
      add_comments("A_an_technology written to all regions") %>%
      add_legacy_name("L202.StubTech_an") %>%
      add_precursors("aglu/A_an_technology", "common/GCAM_region_names") ->
      L202.StubTech_an

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

    L202.GlobalTechCost_an %>%
      add_title("Costs of animal production technologies") %>%
      add_units("1975$/kg") %>%
      add_comments("Animal feed cost, prices, and technology") %>%
      add_comments("This is the non-feed cost; i.e., all costs of producing animal commodities except for the feed.") %>%
      add_legacy_name("L202.GlobalTechCost_an") %>%
      same_precursors_as(L202.StubTechCoef_an) %>%
      add_precursors("L132.ag_an_For_Prices", "L107.an_Feed_Mt_R_C_Sys_Fd_Y") ->
      L202.GlobalTechCost_an

    L202.GlobalRenewTech_imp_an %>%
      add_title("Generic technology info for animal imports") %>%
      add_units("NA") %>%
      add_comments("A_an_supplysector written to all model years and regions") %>%
      add_legacy_name("L202.GlobalRenewTech_imp_an") %>%
      add_precursors("aglu/A_an_supplysector", "common/GCAM_region_names") ->
      L202.GlobalRenewTech_imp_an

    L202.StubTechFixOut_imp_an %>%
      add_title("Animal imports for net importing regions in all periods") %>%
      add_units("Mt") %>%
      add_comments("A_an_supplysector and animal product mass balances written to all model years and regions") %>%
      add_comments("Prescribed trajectory of animal commodity imports by region; the final base year's values are
                   copied forward to all future model time periods (imports and exports of these secondary
                   commodities are not represented from economic competition).") %>%
      add_legacy_name("L202.StubTechFixOut_imp_an") %>%
      add_precursors("aglu/A_an_supplysector", "L109.an_ALL_Mt_R_C_Y", "common/GCAM_region_names") ->
      L202.StubTechFixOut_imp_an

    return_data(L202.RenewRsrc, L202.RenewRsrcPrice, L202.maxSubResource, L202.RenewRsrcCurves,
                L202.UnlimitedRenewRsrcCurves, L202.UnlimitedRenewRsrcPrice, L202.Supplysector_in,
                L202.SubsectorAll_in, L202.StubTech_in, L202.StubTechInterp_in, L202.GlobalTechCoef_in,
                L202.GlobalTechShrwt_in, L202.StubTechProd_in, L202.Supplysector_an, L202.SubsectorAll_an,
                L202.StubTech_an, L202.StubTechInterp_an, L202.StubTechProd_an, L202.StubTechCoef_an,
                L202.GlobalTechCost_an, L202.GlobalRenewTech_imp_an, L202.StubTechFixOut_imp_an)
  } else {
    stop("Unknown command")
  }
}
