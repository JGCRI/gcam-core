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

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
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
      "L108.an_Feed_Mt_R_C_Sys_Fd_Y_adj",
      "L108.ag_Feed_Mt_R_C_Y",
      "L109.ag_ALL_Mt_R_C_Y",
      "L1321.ag_prP_R_C_75USDkg",
      "L1321.an_prP_R_C_75USDkg",
      "L233.TechCoef")

  MODULE_OUTPUTS <-
    c("L202.RenewRsrc",
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
      "L202.ag_consP_R_C_75USDkg")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
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

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # 1. Livestock feed IO coef in GCAM regions ----
    # Moved to here from LA107 (XZ)
    # so any adjustment in feed demand can be included.
    # Calculate the weighted average feed input-output coefficients by region, commodity, system, feed, and year

    L108.an_Feed_Mt_R_C_Sys_Fd_Y_adj %>%
      rename(feedVal = value) %>%
      # add in the corresponding animal production amount
      left_join_error_no_match(L107.an_Prod_Mt_R_C_Sys_Fd_Y %>% rename(prodVal = value),
                               by = c("GCAM_region_ID", "GCAM_commodity", "year", "system", "feed")) %>%
      # calculate the region, commodity, system, feed type, year IO coefficient as feed consumption/animal production
      mutate(value = feedVal / prodVal) %>%
      select(-feedVal, -prodVal) ->
      L107.an_FeedIO_R_C_Sys_Fd_Y


    ### Need to fill in NAs (when Prod = 0)  ----
    # - new method RLH 3/9/22
    # Step 1: Extrapolate for any commodity/system/feed combos that are just missing some years
    L107.an_FeedIO_R_C_Sys_Fd_Y %>%
      group_by(GCAM_region_ID, GCAM_commodity, system, feed) %>%
      # Filter if there is an NA, but not in all years
      filter(any(is.na(value)) & !all(is.na(value))) %>%
      # Replace with last/first available year
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() ->
      L107.an_FeedIO_extrapolate

    # add in extrapolated values
    L107.an_FeedIO_R_C_Sys_Fd_Y %>%
      anti_join(L107.an_FeedIO_extrapolate, by = c("GCAM_region_ID", "GCAM_commodity", "year", "system", "feed")) %>%
      bind_rows(L107.an_FeedIO_extrapolate) ->
      L107.an_FeedIO_R_C_Sys_Fd_Y


    # Step 3: For any commodity/system/feed combos that exist in other regions
    # Replace NAs with max from other regions in that year as a conservative estimate
    L107.an_FeedIO_R_C_Sys_Fd_Y %>%
      group_by(GCAM_commodity, year, system, feed) %>%
      # we get superfluous warnings about no non-missing arguments to max despite
      # explicitly checking for it
      # this is due to: https://stackoverflow.com/questions/16275149/does-ifelse-really-calculate-both-of-its-vectors-every-time-is-it-slow
      # so in this case we can safely just suppress the warning
      mutate(value = if_else(is.na(value) & !all(is.na(value)), suppressWarnings(max(value, na.rm = T)), value)) %>%
      ungroup ->
      L107.an_FeedIO_R_C_Sys_Fd_Y

    # Step 4: if all NA in all region/year/commodity/system/feed, then replace with maximum for all
    # region/year/commodity (ie replace Pork/Mixed/Pasture_FodderGrass with max Pork value)
    L107.an_FeedIO_R_C_Sys_Fd_Y %>%
      group_by(GCAM_commodity) %>%
      mutate(value = if_else(is.na(value), max(value, na.rm = T), value)) %>%
      ungroup ->
      L107.an_FeedIO_R_C_Sys_Fd_Y



    # 2. Build tables ----
    # Base table for resources - add region names to Level1 data tables (lines 49-70 old file)

    # Following datasets are already 'long' so just skip the old interpolate_and_melt step
    # Helper function to get_data, join with GCAM region names, and filter to base years
    get_join_filter <- function(x) {   #
      get_data(all_data, x) %>%
        left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
        filter(year %in% MODEL_BASE_YEARS)
    }

    L202.an_Prod_Mt_R_C_Sys_Fd_Y.mlt <- get_join_filter("L107.an_Prod_Mt_R_C_Sys_Fd_Y")
    L202.an_Feed_Mt_R_C_Sys_Fd_Y.mlt <- get_join_filter("L108.an_Feed_Mt_R_C_Sys_Fd_Y_adj")
    L202.ag_Feed_Mt_R_C_Y.mlt <- get_join_filter("L108.ag_Feed_Mt_R_C_Y")

    L202.an_FeedIO_R_C_Sys_Fd_Y.mlt <- L107.an_FeedIO_R_C_Sys_Fd_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(year %in% MODEL_BASE_YEARS)



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
    L202.an_prP_R_C_75USDkg <- L1321.an_prP_R_C_75USDkg

    A_agUnlimitedRsrcCurves %>%
      select(unlimited.resource, price) %>%
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
      left_join(L202.an_Prod_Mt_R_C_Sys_Fd_Y.mlt %>%
                  select(region, supplysector = GCAM_commodity, year,
                         subsector = system, stub.technology = feed, value),
                by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
      replace_na(list(value = 0)) %>%
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
    L202.ag_tradedP_C_75USDkg <- L109.ag_ALL_Mt_R_C_Y %>%
      select(GCAM_region_ID, GCAM_commodity, year, GrossExp_Mt, GrossImp_Mt) %>%
      filter(year == max(MODEL_BASE_YEARS),
             GCAM_commodity%in% aglu.TRADED_CROPS) %>%
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
      select(GCAM_region_ID, GCAM_commodity, year, Supply_Mt, GrossExp_Mt, GrossImp_Mt) %>%
      mutate(ImpShare = if_else(is.na(GrossImp_Mt) | Supply_Mt == 0, 0, GrossImp_Mt / Supply_Mt)) %>%
      select(GCAM_region_ID, GCAM_commodity, ImpShare)

    # Calculate the weighted average regional crop prices, as the global traded crop price times the
    # import share plus the local producer price times the domestic source share (1 - ImpShare)
    # For aglu.IWM_TRADED_COMM (e.g., FodderHerb), single world price won't be affected
    L202.ag_consP_R_C_75USDkg <- L1321.ag_prP_R_C_75USDkg %>%
      rename(PrP = value) %>%
      left_join_error_no_match(L202.ag_ImpShare_Mt_R_C_Y, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      left_join_error_no_match(L202.ag_tradedP_C_75USDkg, by = "GCAM_commodity", ignore_columns = "tradedP") %>%
      mutate(value = if_else(is.na(tradedP),
                             PrP,
                             PrP * (1 - ImpShare) + tradedP * ImpShare)) %>%
      select(GCAM_region_ID, GCAM_commodity, value)

    # Remove meat prices here since meat is not used as feed. And even if it does, regional prices should be used!
    # This will need to be updated if meat outputs are included in the feed.
    L202.prP_R_C_75USDkg <- L202.ag_consP_R_C_75USDkg %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, GCAM_commodity, price = value)
    L202.rsrcP_R_C_75USDkg <- filter(A_agRsrcCurves, grade == "grade 2") %>%
      select(GCAM_commodity = sub.renewable.resource, calPrice = extractioncost)


    # The new calculation is more consistent as the missing Taiwan prices were filled in earlier in LB1321
    # This is important as they are need for calculating world/trade prices and thus consumer prices
    # Also L132 is no longer needed here
    L202.ag_Feed_Prices <- L202.prP_R_C_75USDkg %>%
      bind_rows(L202.rsrcP_R_C_75USDkg%>% rename(price = calPrice) %>%
                  write_to_all_regions(c("region", "GCAM_commodity", "price"), GCAM_region_names))


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
      group_by(region) %>%
      # using regional mean prices to fill in NA (no output)
      mutate(price_RegMean = sum(revenue)/ sum(weight)) %>%
      ungroup() %>%
      mutate(price = if_else(weight == 0, price_RegMean, revenue / weight)) %>%
      select(region, supplysector, price) ->
      L202.ag_FeedCost_USDkg_R_F

    # gpk 8/5/2020 - compute default animal commodity prices in case of missing values

    # Median prices, to be used elsewhere
    L202.DefaultAnPrices <- L1321.an_prP_R_C_75USDkg %>%
      group_by(GCAM_commodity) %>%
      summarise(DefaultCommodityPrice_USDkg = median(value)) %>%
      ungroup()

    # water IO in L233.TechCoef from module_water_L233.water_demand_livestock
    # 1975$/m3 * m3/Mt = 1975$ per Mt
    L233.TechCoef %>%
      filter(minicam.energy.input == "water_td_an_W") %>%
      mutate(watercost_an = water.DEFAULT_BASEYEAR_WATER_PRICE * coefficient) %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(region, GCAM_commodity = supplysector, system = subsector, feed = technology, watercost_an)->
      L202.an_WaterCost

    # Calculate the total cost of all inputs, for each animal commodity, first matching in the feed quantity and the price
    L202.an_Prod_Mt_R_C_Sys_Fd_Y.mlt %>%
      filter(year == max(MODEL_BASE_YEARS),
             !region %in% aglu.NO_AGLU_REGIONS) %>%
      rename(Prod_Mt = value) %>%
      left_join_error_no_match(select(L202.an_Feed_Mt_R_C_Sys_Fd_Y.mlt, GCAM_region_ID, GCAM_commodity, system, feed, year, Feed_Mt = value),
                               by = c("GCAM_region_ID", "GCAM_commodity", "system", "feed", "year")) %>%
      left_join_error_no_match(L202.ag_FeedCost_USDkg_R_F, by = c("region", "feed" = "supplysector")) %>%
      rename(FeedPrice_USDkg = price) %>%
      left_join(L1321.an_prP_R_C_75USDkg, by = c("GCAM_region_ID", "GCAM_commodity", "region")) %>%
      # gpk 2020-08-05 for any regions that may have missing data, use the median values computed above as defaults
      left_join_error_no_match(L202.DefaultAnPrices, by = "GCAM_commodity") %>%
      mutate(CommodityPrice_USDkg = if_else(is.na(value), DefaultCommodityPrice_USDkg, value)) %>%
      left_join(L202.an_WaterCost, by = c("GCAM_commodity", "system", "feed", "region")) %>%
      replace_na(list(watercost_an = 0)) %>%
      # adjust prices using water cost first
      mutate(CommodityPrice_USDkg = CommodityPrice_USDkg - watercost_an) %>%
      select(-watercost_an) %>%
      # multiply prices by quantities to calculate feed expenditures and commodity sales revenues
      mutate(SalesRevenue_bilUSD = Prod_Mt * CommodityPrice_USDkg,
             FeedCost_bilUSD = Feed_Mt * FeedPrice_USDkg) ->
      L202.an_nonFeedCost_R_C_0

    # 4/22/2023 (XZ)
    # We do not have data of nonfeedcost at the system or tech level
    # Originally, we assume nonfeedcost if the same at the system level and it is
    # computed as the delta between price and feedcost (per output unit).
    # However, this could give us negative value at the tech level!
    # The negative nonfeedcost may lead to negative price and causing solution
    # issues (at least higher iterations).
    # Our price data is from FAO; cost data from mostly FAO for FeedCrops but
    # our supply assumptions for other sources. There is also quality difference
    # between crops being used for food or feed. Allowing negative nonfeedcost
    # implies this quality difference.
    # In adjustment here, we
    # 1. follow the original approach to first calculate nonfeed cost at system level
    # 2. set minimum value for nonfeedcost to zero for all tech except FeedCrops
    # 3. recalculate FeedCrops nonfeedcost to balance the value
    # 4. check and adjust nonfeedcost of FeedCrops tech to ensure the cost share is reasonable


    # Step 1
    # group by region, meat commodity, and system (i.e., assuming the same non-feed cost for the 'mixed' and 'pastoral' systems
    # irrespective of the feed type, but the costs can differ by region, commodity, and system)
    # nonFeedCost_FeedCropsOnly is also calculated here for later uses

    L202.an_nonFeedCost_R_C_0 %>%
      group_by(region, GCAM_commodity, system) %>%
      mutate(Prod_Mt_sys = sum(Prod_Mt),
                SalesRevenue_bilUSD_sys = sum(SalesRevenue_bilUSD),
                FeedCost_bilUSD_sys = sum(FeedCost_bilUSD),
                nonFeedCost_bilUSD_sys = SalesRevenue_bilUSD_sys - FeedCost_bilUSD_sys) %>%
      mutate(nonFeedCost = if_else(Prod_Mt == 0, 0, nonFeedCost_bilUSD_sys / Prod_Mt_sys),
             nonFeedCost_FeedCropsOnly = if_else(Prod_Mt == 0, 0, nonFeedCost_bilUSD_sys / Prod_Mt[feed == "FeedCrops"]) ) %>%
      ungroup() ->
      L202.an_nonFeedCost_R_C_1

    # Step 1
    # set minimum value for nonfeedcost to zero for all tech except FeedCrops
    # recalculate FeedCrops nonfeedcost to balance the value

    L202.an_nonFeedCost_R_C_1 %>%
      mutate(nonFeedCost = if_else(feed != "FeedCrops" & nonFeedCost <0, 0, nonFeedCost),
             nonFeedCost = if_else(feed == "FeedCrops" & nonFeedCost <0, nonFeedCost_FeedCropsOnly, nonFeedCost)) %>%
      select(-nonFeedCost_FeedCropsOnly) ->
      L202.an_nonFeedCost_R_C_2


    # Step 3 assert_that value balance so that prices won't be affected

    assertthat::assert_that(
      L202.an_nonFeedCost_R_C_2 %>%
        select(region, GCAM_commodity, system, feed, nonFeedCost, SalesRevenue_bilUSD_sys, FeedCost_bilUSD_sys, Prod_Mt) %>%
        mutate(nonFeedCost_bilUSD = nonFeedCost * Prod_Mt) %>%
        group_by(region, GCAM_commodity, system) %>%
        summarize(nonFeedCost_bilUSD = sum(nonFeedCost_bilUSD),
                  SalesRevenue_bilUSD = first(SalesRevenue_bilUSD_sys),
                  FeedCost_bilUSD = first(FeedCost_bilUSD_sys)) %>% ungroup() %>%
        mutate(diff = SalesRevenue_bilUSD - FeedCost_bilUSD - nonFeedCost_bilUSD) %>%
        filter(diff > 0.001) %>% nrow() == 0, msg = "Value imbalance after adjustments"
    )


    # Step 4 check and adjust nonfeedcost of FeedCrops tech to ensure the cost share is reasonable
    # set nonfeedcost >= -50% of feedcost (per unit of output)
    # about 30 techs are adjusted but the prodduction was small so won't affect output prices significantly
    L202.an_nonFeedCost_R_C_2 %>%
      mutate(feedcostperoutput = if_else(Prod_Mt == 0, 0, FeedCost_bilUSD / Prod_Mt),
             nonFeedCost_min = -feedcostperoutput * 0.5,
             nonFeedCost = pmax(nonFeedCost, nonFeedCost_min) ) ->
      L202.an_nonFeedCost_R_C_3


    A_an_technology %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%
      mutate(stub.technology = technology,
             minicam.non.energy.input = "non-feed") %>%
      left_join_error_no_match(L202.an_nonFeedCost_R_C_3 %>%
                  select(region, supplysector = GCAM_commodity, subsector = system,
                         stub.technology = feed, nonFeedCost),
                  by = c("supplysector", "subsector", "region", "stub.technology")) %>%
      mutate(input.cost = round(nonFeedCost, aglu.DIGITS_CALPRICE)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCost"]]) ->
      L202.StubTechCost_an


    # Remove any regions for which agriculture and land use are not modeled (308-320)
    # Also, remove DDGS and feedcake subsectors and technologies in regions where these commodities are not available
    # First need to figure out what the names of these subsectors are, and which regions to exclude
    A_regions %>%
      filter(ethanol != "corn ethanol",
             !paste(biomassOil_tech, biodiesel) %in% c("OilCrop biodiesel", "Soybean biodiesel" )) %>%
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
      add_precursors("L108.ag_Feed_Mt_R_C_Y", "aglu/A_agRsrcCurves", "common/GCAM_region_names") ->
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
      add_precursors("aglu/A_an_input_technology",
                     "L107.an_Prod_Mt_R_C_Sys_Fd_Y",
                     "L108.an_Feed_Mt_R_C_Sys_Fd_Y_adj",
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
      add_precursors("L1321.ag_prP_R_C_75USDkg", "L1321.an_prP_R_C_75USDkg", "L233.TechCoef",
                     "L108.an_Feed_Mt_R_C_Sys_Fd_Y_adj", "L109.ag_ALL_Mt_R_C_Y") ->
      L202.StubTechCost_an

    # Return also the consumer prices, to be made available elsewhere
    L202.ag_consP_R_C_75USDkg %>%
      add_title("Consumer costs of crops") %>%
      add_units("1975$/kg") %>%
      add_comments("Computed from weighted average of domestically sourced crops (which use producer prices) and imports") %>%
      add_comments("Imported crop prices are computed from weighted average of producer prices of exporting countries") %>%
      add_precursors("L1321.ag_prP_R_C_75USDkg", "L109.ag_ALL_Mt_R_C_Y") ->
      L202.ag_consP_R_C_75USDkg


    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
