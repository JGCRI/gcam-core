# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L211.resources_fossil_USA
#'
#' GCAM-USA fossil resource market information, prices, TechChange parameters, and supply curves.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L210.DeleteRenewRsrc_USArsrc}, \code{L210.DeleteUnlimitRsrc_USArsrc}, \code{L210.RenewRsrc_USA},
#' \code{L210.UnlimitRsrc_USA}, \code{L210.UnlimitRsrcPrice_USA}, \code{L210.SmthRenewRsrcTechChange_USA}, \code{L210.SmthRenewRsrcTechChange_offshore_wind_USA},
#' \code{L210.SmthRenewRsrcCurves_wind_USA}, \code{L210.SmthRenewRsrcCurves_offshore_wind_USA}, \code{L210.GrdRenewRsrcCurves_geo_USA}, \code{L210.GrdRenewRsrcMax_geo_USA},
#' \code{L210.SmthRenewRsrcCurvesGdpElast_roofPV_USA}, \code{L210.DeleteUnlimitRsrc_USAlimestone},
#' \code{L210.UnlimitRsrc_limestone_USA}, \code{L210.UnlimitRsrcPrice_limestone_USA}, \code{L210.ResTechShrwt_USA},
#' \code{L211.ResReserveTechInvestmentInput_USA}, \code{L211.ResTechCoef_USA}, \code{L211.DeleteRsrc_fos_USA}. The corresponding file in the
#' original data system was \code{L210.resources_USA.R} (gcam-usa level2).
#' @details GCAM-USA fossil resource market information, prices, TechChange parameters, and supply curves.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join filter if_else group_by lag mutate select summarise bind_rows
#' @author YO Jan 2023

module_gcamusa_L211.resources_fossil_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/A10.fossil_prod_sector",
             FILE = "gcam-usa/A10.fossil_subsector_interp",
             FILE = "gcam-usa/A10.fossil_subsector_logit",
             FILE = "gcam-usa/A10.sector_states",
             FILE = "emissions/A_PrimaryFuelCCoef",
             FILE = "energy/A10.TechChange",
             FILE = "gcam-usa/A10.subsector_interp_states",
             FILE = "gcam-usa/EIA_gas_market_wellhead_price_states",
             FILE = "gcam-usa/EIA_NG_prod_mapping_wellhead_price",
             FILE = "gcam-usa/EIA_oil_first_purchase_price_states",
             FILE = "gcam-usa/EIA_oil_first_purchase_price_states_mapping",
             FILE = "gcam-usa/EIA_coal_open_market_price_states",
             FILE = "gcam-usa/EIA_coal_open_market_price_states_mapping",
             FILE = "gcam-usa/A21.globalrsrctech_coef_USA",
             FILE = "gcam-usa/states_subregions",
             FILE = "gcam-usa/A10.ResSubresourceProdLifetime_USA",
             "L111.ResCurves_EJ_R_Ffos_USA",
             "L111.Prod_EJ_R_F_Yh_USA",
             "L210.RsrcPrice",
             "L210.SubresourcePriceAdder",
             "L210.ResReserveTechLifetime",
             "L210.ResReserveTechDeclinePhase",
             "L210.ResReserveTechProfitShutdown",
             "L210.ResTechShrwt",
             "L211.ReserveCalReserve"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L211.PrimaryCO2Coef_USA",
             "L211.Rsrc_F_USA",
             "L211.RsrcPrice_F_USA",
             "L211.RsrcCalProd_USA",
             "L211.RsrcTechChange_USA",
             "L211.RsrcCurves_fos_USA",
             "L211.ResSubresourceProdLifetime_USA",
             "L211.SubresourcePriceAdder_USA",
             "L211.ReserveCalReserve_USA",
             "L211.ResReserveTechLifetime_USA",
             "L211.ResReserveTechDeclinePhase_USA",
             "L211.ResReserveTechProfitShutdown_USA",
             "L211.ResReserveTechInvestmentInput_USA",
             "L211.ResReserveTechShrwt_fossil_USA",
             "L211.Sector_prod_USA",
             "L211.Subsector_prod_USA",
             "L211.SubsShrwtFlt_USA",
             "L211.SubsInterpRule_USA",
             "L211.TechShrwt_USA",
             "L211.TechCoef_USA",
             "L211.TechCal_USA",
             "L211.TFFSubsectorLogit",
             "L211.TFFTechProduction_USA",
             "L211.TFFTechCoef_USA",
             "L211.TFFTechShrwtInterp_USA",
             "L211.ResTechCoef_USA",
             "L211.DeleteRsrc_fos_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    Geothermal_Hydrothermal_GWh <- State <- available <- b_exp <- cost_modifier <- curve.exponent <- curve_exponent <-
      extractioncost <- generation <- geothermal <- grade <- grade_share <- maxResource <- maxSubResource <- mid.price <-
      mid_p <- mid_price <- object <- offtake <- offtake_share <- region <- renewresource <- smooth.renewable.subresource <-
      state <- unlimited.resource <- value <- year <- year.fillout <- . <-
      sub.renewable.resource <- subresource <- NULL

    # Load required inputs
    A10.fossil_prod_sector <- get_data(all_data, "gcam-usa/A10.fossil_prod_sector", strip_attributes = TRUE)
    A10.fossil_subsector_logit <- get_data(all_data, "gcam-usa/A10.fossil_subsector_logit", strip_attributes = TRUE)
    A10.fossil_subsector_interp <- get_data(all_data, "gcam-usa/A10.fossil_subsector_interp", strip_attributes = TRUE)
    A10.sector_states <- get_data(all_data, "gcam-usa/A10.sector_states", strip_attributes = TRUE)
    A_PrimaryFuelCCoef <- get_data(all_data, "emissions/A_PrimaryFuelCCoef", strip_attributes = TRUE)
    A10.TechChange <- get_data(all_data, "energy/A10.TechChange", strip_attributes = TRUE)
    A10.subsector_interp_states <- get_data(all_data, "gcam-usa/A10.subsector_interp_states", strip_attributes = TRUE)
    EIA_gas_market_wellhead_price_states <- get_data(all_data, "gcam-usa/EIA_gas_market_wellhead_price_states", strip_attributes = TRUE)
    EIA_NG_prod_mapping_wellhead_price <- get_data(all_data, "gcam-usa/EIA_NG_prod_mapping_wellhead_price", strip_attributes = TRUE)
    EIA_oil_first_purchase_price_states <- get_data(all_data, "gcam-usa/EIA_oil_first_purchase_price_states", strip_attributes = TRUE)
    EIA_oil_first_purchase_price_states_mapping <- get_data(all_data, "gcam-usa/EIA_oil_first_purchase_price_states_mapping", strip_attributes = TRUE)
    EIA_coal_open_market_price_states <- get_data(all_data, "gcam-usa/EIA_coal_open_market_price_states", strip_attributes = TRUE)
    EIA_coal_open_market_price_states_mapping <- get_data(all_data, "gcam-usa/EIA_coal_open_market_price_states_mapping", strip_attributes = TRUE)
    A21.globalrsrctech_coef_USA <- get_data(all_data, "gcam-usa/A21.globalrsrctech_coef_USA")
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions")
    L111.ResCurves_EJ_R_Ffos_USA <- get_data(all_data, "L111.ResCurves_EJ_R_Ffos_USA", strip_attributes = TRUE)
    L111.Prod_EJ_R_F_Yh_USA <- get_data(all_data, "L111.Prod_EJ_R_F_Yh_USA", strip_attributes = TRUE)
    L210.RsrcPrice <- get_data(all_data, "L210.RsrcPrice", strip_attributes = TRUE)
    A10.ResSubresourceProdLifetime_USA <- get_data(all_data, "gcam-usa/A10.ResSubresourceProdLifetime_USA", strip_attributes = TRUE)
    L210.SubresourcePriceAdder <- get_data(all_data, "L210.SubresourcePriceAdder", strip_attributes = TRUE)
    L210.ResReserveTechLifetime <- get_data(all_data, "L210.ResReserveTechLifetime", strip_attributes = TRUE)
    L210.ResReserveTechDeclinePhase <- get_data(all_data, "L210.ResReserveTechDeclinePhase", strip_attributes = TRUE)
    L210.ResReserveTechProfitShutdown <- get_data(all_data, "L210.ResReserveTechProfitShutdown", strip_attributes = TRUE)
    L210.ResTechShrwt <- get_data(all_data, "L210.ResTechShrwt", strip_attributes = TRUE)
    L211.ReserveCalReserve <- get_data(all_data, "L211.ReserveCalReserve", strip_attributes = TRUE)


    # ===================================================
    # Perform computations

    # Part 1: construct XML structures for state-level resources
    # -------------------------------------------------------------------------------------------

    # Structure for "natural gas production", "crude oil production", and "coal production" supply sectors
    A10.fossil_prod_sector %>%
      mutate(region = gcam.USA_REGION,
             logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["Supplysector"]],logit.type) -> L211.Supplysector_FFprod

    # L211.Rsrc_F_USA: fossil resource parameters
    L111.Prod_EJ_R_F_Yh_USA %>%
      select(region, resource) %>%
      distinct() %>%
      mutate(output.unit = "EJ",
             price.unit = "1975$/GJ",
             market = region) -> L211.Rsrc_F_USA


    # L211.RsrcPrice_F_USA
    # scale the current region USA price to states
    # scalars are based on the ratio between states' and US' historical wellhead production prices in EIA
    # for missing years, use historical average

    # 1) clean up region mapping
    # natural gas
    EIA_gas_market_wellhead_price_states_raw <- EIA_gas_market_wellhead_price_states %>%
      tidyr::gather(category, value, -Date) %>%
      rename(year = Date) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(EIA_NG_prod_mapping_wellhead_price, by = "category") %>%
      filter(state != "other") %>%
      select(-category) %>%
      mutate(resource = "natural gas")

    # crude oil
    EIA_oil_first_purchase_price_states_raw <- EIA_oil_first_purchase_price_states %>%
      tidyr::gather(category, value, -Date) %>%
      rename(year = Date) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(EIA_oil_first_purchase_price_states_mapping, by = "category") %>%
      filter(state != "other") %>%
      select(-category) %>%
      mutate(resource = "crude oil")

    # coal
    EIA_coal_open_market_price_raw <- EIA_coal_open_market_price_states %>%
      select(-units, -`source key`) %>%
      mutate(description = gsub("Open market : ", "", description)) %>%
      tidyr::gather(year, value, -description) %>%
      mutate(year = as.integer(year))

    # fill in missing values for states
    # for states without data (W = Data withheld to prevent disclosure), use regional prices instead
    # for regions without data use national values (this is for Alaska)
    EIA_coal_open_market_price_US <- EIA_coal_open_market_price_raw %>%
      filter(description == "United States") %>%
      select(year, value.us = value)

    # obtain regional values after filling in national values to Pacific NAs
    EIA_coal_open_market_price_region <- EIA_coal_open_market_price_raw %>%
      filter(description %in% unique(EIA_coal_open_market_price_states_mapping$region)) %>%
      left_join_error_no_match(EIA_coal_open_market_price_US, by = "year") %>%
      mutate(value.region = if_else(grepl("--|W", value), value.us, value)) %>%
      select(description, year, value.region)

    # obtain state prices after filling in regional values to NA states
    EIA_coal_open_market_price_states_raw <- EIA_coal_open_market_price_raw %>%
      filter(!description %in% unique(EIA_coal_open_market_price_states_mapping$region)) %>%
      # add state to region mapping
      left_join_error_no_match(EIA_coal_open_market_price_states_mapping, by = "description") %>%
      # map the corresponding regions
      left_join_error_no_match(EIA_coal_open_market_price_region, by = c("region" = "description", "year")) %>%
      # fill in missing values with regional prices
      mutate(value = if_else(grepl("--|W", value), value.region, value)) %>%
      select(year, value, state) %>%
      # add back US to calculate scalars in the next step
      bind_rows(EIA_coal_open_market_price_US %>% mutate(state = gcam.USA_REGION) %>% rename(value = value.us)) %>%
      # finally add the resource name
      mutate(resource = "coal", value = as.numeric(value))

    # combine
    EIA_fossil_production_price_raw <- bind_rows(EIA_gas_market_wellhead_price_states_raw,
                                                 EIA_oil_first_purchase_price_states_raw,
                                                 EIA_coal_open_market_price_states_raw)

    # 2) develop state-to-US scalars
    EIA_fossil_production_price_states_scalars <- EIA_fossil_production_price_raw %>%
      filter(state != gcam.USA_REGION) %>%
      left_join(EIA_fossil_production_price_raw %>%
                  filter(state == gcam.USA_REGION) %>%
                  select(resource, year, value_usa = value),
                by = c("year", "resource")) %>%
      mutate(scalars = value / value_usa) %>%
      group_by(state) %>%
      # becuase different states have different years available, the late available year is no later
      # than 2010, here just use the historical average scalar
      mutate(scalar_average = mean(scalars, na.rm = TRUE)) %>%
      ungroup() %>%
      # for states no value available for any historcal year, just use the USA value (scalar = 1)
      # this is only for the case of Nevada
      mutate(scalar_average = ifelse(is.na(scalar_average), 1, scalar_average)) %>%
      select(state, resource, scalar = scalar_average) %>%
      distinct()

    # scale state-level calibrated prices based on historical wellhead prices relative to USA
    L111.Prod_EJ_R_F_Yh_USA %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(region, resource, year) %>%
      distinct() %>%
      left_join_error_no_match(L210.RsrcPrice %>%
                                 filter(region == gcam.USA_REGION) %>%
                                 select(year, resource, price), by = c("resource", "year")) %>%
      # use left_join because Idaho has historical productions but no wellhead price info available
      left_join(EIA_fossil_production_price_states_scalars, by = c("region" = "state", "resource")) %>%
      # replace NA scalars as 1 (using USA value)
      replace_na(list("scalar" = 1)) %>%
      mutate(price = price * scalar ) %>%
      select(-scalar) -> L211.RsrcPrice_F_USA

    # L211.RsrcTechChange_USA: resource technological change
    L111.Prod_EJ_R_F_Yh_USA %>%
      select(region, resource, reserve.subresource) %>%
      distinct() %>%
      # YZ add lines so that we can left_join by both resource and subresource
      mutate(type = case_when(
        resource == "natural gas" ~ "natural gas",
        resource == "coal" ~ "coal",
        reserve.subresource == "onshore conventional oil" ~ "crude oil",
        grepl("offshore oil",reserve.subresource) ~ "crude oil",
        reserve.subresource == "onshore unconventional oil" ~ "crude oil",
        reserve.subresource == "onshore unconventional heavy oil" ~ "unconventional oil")
      ) %>%
      # use core GCAM tech.change assumptions
      # use left_join becuase we need to copy this to different years (so number of rows will change)
      left_join(A10.TechChange %>% tidyr::gather(year, techChange, -resource, -subresource),
                by = c("resource","type"="subresource"),
                relationship = "many-to-many") %>%
      mutate(year.fillout = as.integer(year)) %>%
      select(region, resource, reserve.subresource, year.fillout, techChange) ->
      L211.RsrcTechChange_USA

    # L211.RsrcCurves_fos_USA: resource grades and extraction costs
    # this one has been created in level 1 data
    L211.RsrcCurves_fos_USA <- L111.ResCurves_EJ_R_Ffos_USA

    # L211.DepresourceCal: subresource calproduction
    L111.Prod_EJ_R_F_Yh_USA %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(cal.production = value) %>%
      select(region, resource, reserve.subresource, year, cal.production) -> L211.RsrcCalProd_USA


    # Part 2: Create state regional fossil fuel sectors which aggregate resource types.
    # -------------------------------------------------------------------------------------------
    # 1) reserve.subresource lifetime - follow national assumption
    L211.RsrcCalProd_USA %>%
      select(region, resource, reserve.subresource) %>%
      distinct() %>%
      left_join_error_no_match(A10.ResSubresourceProdLifetime_USA,
                               by = c("resource","reserve.subresource")) -> L211.ResSubresourceProdLifetime_USA

    # 2) reserve.subresource price adder in 2100 - follow national assumption
    L211.RsrcCalProd_USA %>%
      select(region, resource, reserve.subresource) %>%
      distinct() %>%
      # create a mapping type to match core GCAM's fossil fuel category
      mutate(type = case_when(
        resource == "natural gas" ~ "natural gas",
        resource == "coal" ~ "coal",
        reserve.subresource %in% c("onshore conventional oil","offshore oil","onshore unconventional oil") ~ "crude oil",
        reserve.subresource == "onshore unconventional heavy oil" ~ "unconventional oil")
      ) %>%
      # YZ 6/18/2025 use left_join because there will be NAs for crude oil, because the price adder is no longer there for
      # crude oil subresources (crude oil and unconv oil) due to C++ update which is committed as a part of the BYU21.
      # The previous values used for price adders were effectively workarounds for a bug or undefined behavior in how adders
      # were calibrated in regions with zero historical production (a common case for unconventional oil). The C++ code
      # was updated to calibrate price adders more appropriately in these scenarios, ensuring unconventional oil is only
      # produced once conventional resources are exhausted (excluding future tech change and natural gas effects).
      left_join(L210.SubresourcePriceAdder %>%
                                 filter(region == gcam.USA_REGION) %>%
                                 select(subresource, year, price.adder),
                               by = c("type" = "subresource")) %>%
      na.omit() %>%
      select(-type) -> L211.SubresourcePriceAdder_USA

    # 3) historical calculated reserve
    L211.ReserveCalReserve %>%
      rename(cal.reserve = value) %>%
      select(region, resource, reserve.subresource, year, cal.reserve) ->
      L211.ReserveCalReserve_USA

    # we want to read zero cal reserve for speculative subresource so that GCAM will
    # back calculate price adders to ensure they do not get produced until non-speculative
    # resources are consumed (absent un-even tech change)
    L211.RsrcCalProd_USA %>%
      filter(grepl('(speculative|unconventional heavy)', reserve.subresource)) %>%
      rename(cal.reserve = cal.production) %>%
      bind_rows(L211.ReserveCalReserve_USA, .) ->
      L211.ReserveCalReserve_USA

    # some error checking to ensure we didn't accidentally strand some reserve by assumption
    L211.ReserveCalReserve_USA %>%
      anti_join(L211.ResSubresourceProdLifetime_USA, by=c("region", "resource", "reserve.subresource")) %>%
      filter(cal.reserve > 0) ->
      L211.ReserveCalReserve_USA.check
    assert_that(nrow(L211.ReserveCalReserve_USA.check) == 0)

    # drop rows for region + resource + reserve.subresource which we assume to never exist
    # note the error checking above to ensure we do not drop any which actually produced
    # historically
    L211.ReserveCalReserve_USA %>%
      inner_join(L211.ResSubresourceProdLifetime_USA, by=c("region", "resource", "reserve.subresource")) %>%
      select(-avg.prod.lifetime) %>%
      distinct()->
      L211.ReserveCalReserve_USA

    # 4) reserve.subresource technology lifetime - follow national assumption
    L211.RsrcCalProd_USA %>%
      select(region, resource, reserve.subresource) %>%
      distinct() %>%
      # create a mapping type to match core GCAM's fossil fuel category
      mutate(type = case_when(
        resource == "natural gas" ~ "natural gas",
        resource == "coal" ~ "coal",
        reserve.subresource %in% c("onshore conventional oil","offshore oil","onshore unconventional oil") ~ "crude oil",
        reserve.subresource == "onshore unconventional heavy oil" ~ "unconventional oil")
      ) %>%
      # use left_join because number of rows will be changes (copy to all years)
      left_join(L210.ResReserveTechLifetime %>%
                  filter(region == gcam.USA_REGION) %>%
                  select(reserve.subresource, year, lifetime),
                by = c("type" = "reserve.subresource"),
                relationship = "many-to-many") %>%
      mutate(resource.reserve.technology = reserve.subresource) %>%
      select(names(L210.ResReserveTechLifetime)) -> L211.ResReserveTechLifetime_USA

    # 5) reserve.resource.technology decline.phase.percent - follow national assumption
    L211.RsrcCalProd_USA %>%
      select(region, resource, reserve.subresource) %>%
      distinct() %>%
      # create a mapping type to match core GCAM's fossil fuel category
      mutate(type = case_when(
        resource == "natural gas" ~ "natural gas",
        resource == "coal" ~ "coal",
        reserve.subresource %in% c("onshore conventional oil","offshore oil","onshore unconventional oil") ~ "crude oil",
        reserve.subresource == "onshore unconventional heavy oil" ~ "unconventional oil")
      ) %>%
      left_join(L210.ResReserveTechDeclinePhase %>%
                  filter(region == gcam.USA_REGION) %>%
                  select(reserve.subresource, year, decline.phase.percent),
                by = c("type" = "reserve.subresource"),
                relationship = "many-to-many") %>%
      mutate(resource.reserve.technology = reserve.subresource) %>%
      select(names(L210.ResReserveTechDeclinePhase)) -> L211.ResReserveTechDeclinePhase_USA

    # 6) reserve.resource.technology profit.shutdown parameters - follow national assumption
    L211.RsrcCalProd_USA %>%
      select(region, resource, reserve.subresource) %>%
      distinct() %>%
      # create a mapping type to match core GCAM's fossil fuel category
      mutate(type = case_when(
        resource == "natural gas" ~ "natural gas",
        resource == "coal" ~ "coal",
        reserve.subresource %in% c("onshore conventional oil","offshore oil","onshore unconventional oil") ~ "crude oil",
        reserve.subresource == "onshore unconventional heavy oil" ~ "unconventional oil")
      ) %>%
      left_join(L210.ResReserveTechProfitShutdown %>%
                  filter(region == gcam.USA_REGION) %>%
                  select(reserve.subresource, year, median.shutdown.point, profit.shutdown.steepness),
                by = c("type" = "reserve.subresource"),
                relationship = "many-to-many") %>%
      mutate(resource.reserve.technology = reserve.subresource) %>%
      select(names(L210.ResReserveTechProfitShutdown)) -> L211.ResReserveTechProfitShutdown_USA

    # investment inputs for tracking capital
    L211.ResSubresourceProdLifetime_USA %>%
      mutate(resource.reserve.technology = reserve.subresource,
             capital.ratio = socioeconomics.RESOURCE_CAPITAL_RATIO,
             interest.rate = socioeconomics.DEFAULT_INTEREST_RATE,
             payback.years = round(avg.prod.lifetime / 2),
             invest.unit.conversion = 1,
             minicam.non.energy.input = "resource-investment",
             tracking.market = socioeconomics.EN_CAPITAL_MARKET_NAME) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["ResReserveTechInvestmentInput"]]) ->
      L211.ResReserveTechInvestmentInput_USA

    # 7) reserve.resource.technology share.weight - follow national assumption
    L211.RsrcCalProd_USA %>%
      select(region, resource, reserve.subresource) %>%
      distinct() %>%
      # create a mapping type to match core GCAM's fossil fuel category
      mutate(type = case_when(
        resource == "natural gas" ~ "natural gas",
        resource == "coal" ~ "coal",
        reserve.subresource %in% c("onshore conventional oil","offshore oil","onshore unconventional oil") ~ "crude oil",
        reserve.subresource == "onshore unconventional heavy oil" ~ "unconventional oil")
      ) %>%
      left_join(L210.ResTechShrwt %>%
                  filter(region == gcam.USA_REGION) %>%
                  select(subresource, year, share.weight),
                by = c("type" = "subresource"),
                relationship = "many-to-many") %>%
      mutate(resource.reserve.technology = reserve.subresource) %>%
      select(region, resource, reserve.subresource, resource.reserve.technology, year, share.weight) -> L211.ResReserveTechShrwt_fossil_USA

    # Part 3: Create state regional natural gas sectors which aggregate resource types.
    # -------------------------------------------------------------------------------------------

    # L211.Sector: Regional natural gas sector to aggregate gas types.
    L111.Prod_EJ_R_F_Yh_USA %>%
      distinct(region, resource) -> L211.Rsrc_F_regions

    # logit assumptions
    L211.Rsrc_F_regions %>%
      mutate(supplysector = paste0(resource, " production"),
             logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(A10.sector_states,
                               by = 'supplysector',ignore_columns = 'logit.type')-> L211.Sector

    L211.Supplysector_FFprod %>%
      bind_rows(L211.Sector) %>%
      select(LEVEL2_DATA_NAMES[["Supplysector"]],logit.type) -> L211.Sector_prod_USA

    # Add USA & state natural gas production sectors to CO2 Coefs, format for output
    L211.Sector_prod_USA %>%
      select(region, supplysector) %>%
      distinct() %>%
      mutate(PrimaryFuelCO2Coef.name = gsub(" production", "", supplysector)) %>%
      left_join_error_no_match(A_PrimaryFuelCCoef, by = "PrimaryFuelCO2Coef.name") %>%
      select(region, PrimaryFuelCO2Coef.name, PrimaryFuelCO2Coef) -> L211.PrimaryCO2Coef_USA

    # L211.Subsector_prod_USA: state natural gas subsector to aggregate gas types
    L111.Prod_EJ_R_F_Yh_USA %>%
      select(region, subsector = resource) %>%
      distinct(region, subsector) %>%
      mutate(supplysector = paste0(subsector, " production"),
             logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(A10.sector_states, by = 'supplysector', ignore_columns = 'logit.type') %>%
      select(region, supplysector, subsector, logit.year.fillout, logit.exponent, logit.type) -> L211.Subsector_prod_USA

    # L211.SubsInterpRule: state natural gas subsector interpolation rules
    # Shareweight defaults to zero, which will get reset by tech cal if appropriate
    L211.Subsector_prod_USA %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS),
             share.weight = 0) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) -> L211.SubsShrwtFlt_USA

    # Warning: we should partition this table in two if to.value is not NA
    L211.Subsector_prod_USA %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      left_join(A10.subsector_interp_states, by = c("supplysector", "subsector")) %>%
      set_years() %>%
      mutate(from.year = as.character(from.year),
             to.year = as.character(to.year)) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorInterp"]]) -> L211.SubsInterpRule_States

    # L211.TechInput: Pass through tech
    L211.Subsector_prod_USA %>%
      distinct(region, supplysector,subsector) %>%
      mutate(technology = subsector) %>%
      repeat_add_columns(tibble("year" = MODEL_YEARS)) -> L211.TechInput

    L211.TechInput %>%
      mutate(share.weight = if_else(year <= MODEL_FINAL_BASE_YEAR, 0, 1)) %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]]) -> L211.TechShrwt

    L211.TechInput %>%
      mutate(minicam.energy.input = technology,
             coefficient = 1,
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) -> L211.TechCoef

    # L211.TechCal_USA: natural gas resource calibration
    L111.Prod_EJ_R_F_Yh_USA %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      filter(value > 0) %>%
      group_by(region, resource, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(supplysector = paste0(resource, " production"),
             subsector = resource,
             stub.technology = resource,
             minicam.energy.input = resource,
             calibrated.value = round(value, 7),
             share.weight.year = year,
             subs.share.weight = 1,
             tech.share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) -> L211.TechCal_USA

    # Part 4: Add these resources to the traded natural gas sector
    # -------------------------------------------------------------------------------------------
    L111.Prod_EJ_R_F_Yh_USA %>%
      group_by(region, resource, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() -> L211.fossil_prod_state_Yh_EJ

    A10.fossil_subsector_interp %>%
      mutate(region = gcam.USA_REGION) -> L211.TFFSubsInterp_USA

    # "L211.TFFSubsInterp: The interpolation rule for the regions in the traded natural gas sector."
    L211.SubsInterpRule_States %>%
      bind_rows(L211.TFFSubsInterp_USA) %>%
      set_years()-> L211.SubsInterpRule_USA

    # L211.TFFSubsectorLogit: The subsector logits for the regions in the traded natural gas sector.
    A10.fossil_subsector_logit %>%
      mutate(region = gcam.USA_REGION,
             logit.year.fillout = min(MODEL_BASE_YEARS))-> L211.TFFSubsectorLogit

    # "L211.TFFTech: create the tech to simply pass through state production"
    L211.fossil_prod_state_Yh_EJ %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(market.name = region) %>%
      mutate(minicam.energy.input = paste0(resource, " production"),
             region = gcam.USA_REGION,
             supplysector = minicam.energy.input,
             subsector = ifelse(market.name == "AK", paste(market.name, supplysector),
                                paste("Lower48", supplysector)),
             technology = paste(market.name, supplysector)) -> L211.TFFTech

    L211.TFFTech %>%
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = if_else(value > 0, 1, 0 ),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) -> L211.TFFTechProduction

    # use production share as the share.weight for subsector and technology
    # subsector share will split AK vs. lower 48 markets
    # tech share will split state production within lower 48
    # YZ 2024.03.05 do not need to use production shares as the shareweights,
    # because GCAM automatically calibrates historical shareweights.
    L211.TFFTechProduction_subsector_share <- L211.TFFTechProduction %>%
      group_by(region, supplysector, subsector, year) %>%
      dplyr::summarise(subsector.sum = sum(calOutputValue)) %>%
      ungroup() %>%
      group_by(region, supplysector, year) %>%
      mutate(subsector.share = subsector.sum / sum(subsector.sum)) %>%
      ungroup() %>%
      replace_na(list(subsector.share = 0)) %>%
      select(region, supplysector, subsector, year, subsector.share)

    L211.TFFTechProduction_tech_share <- L211.TFFTechProduction %>%
      group_by(region, supplysector, subsector, year) %>%
      mutate(tech.share = calOutputValue / sum(calOutputValue)) %>%
      ungroup() %>%
      replace_na(list(tech.share = 0)) %>%
      select(region, supplysector, subsector, technology, year, tech.share)

    # update share-weight
    L211.TFFTechProduction_USA <- L211.TFFTechProduction %>%
      left_join_error_no_match(L211.TFFTechProduction_subsector_share,
                               by = c("region", "supplysector", "subsector", "year")) %>%
      left_join_error_no_match(L211.TFFTechProduction_tech_share,
                               by = c("region", "supplysector", "subsector", "technology", "year")) %>%
      mutate(subs.share.weight = if_else(subsector.share==0,0,1),
             tech.share.weight = if_else(tech.share==0,0,1)) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])


    L211.TFFTech %>%
      mutate(coefficient = 1) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) %>%
      select(-year) %>%
      distinct() %>%
      repeat_add_columns(tibble("year" = MODEL_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) -> L211.TFFTechCoef


    # fix share-weight as the calibrated base-year value
    L211.TFFTechProduction_USA %>%
      mutate(apply.to = 'share-weight',
             from.year = MODEL_FINAL_BASE_YEAR,
             to.year = max(MODEL_FUTURE_YEARS),
             interpolation.function = 'fixed') %>%
      select(LEVEL2_DATA_NAMES[["TechInterp"]]) %>%
      distinct()->L211.TFFTechShrwtInterp

    # Part 5: Add gas and unconventional oil upscaling inputs to onshore unconventional heavy oil
    # (this is a category of unconventional oil that matches the unconventional
    # oil in GCAM-32, which has no historical production in the USA. It represents
    # possible future oil sources such as oil shale/ kerogen which is similarly energy
    # intensive as Canadian oil sands)
    # -------------------------------------------------------------------------------------------

    # get vector of states that have unconventional heavy oil potential
    unconv_heavy_oil_states <- L111.ResCurves_EJ_R_Ffos_USA %>%
      filter(reserve.subresource == "onshore unconventional heavy oil") %>%
      pull(region) %>%
      unique()

    # apply exogenous coefficients across these states, matching the
    # wholesale gas input with each state's grid region
    A21.globalrsrctech_coef_USA %>%
      gather_years(value_col = "coefficient") %>%
      complete(nesting(resource, reserve.subresource, resource.reserve.technology, minicam.energy.input), year = c(year, HISTORICAL_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(resource, year) %>%
      group_by(resource, reserve.subresource, resource.reserve.technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 1)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      repeat_add_columns(tibble(region = unconv_heavy_oil_states)) %>%
      rename(coefficient = value) %>%
      left_join_error_no_match(states_subregions, by = c("region" = "state")) %>%
      mutate(market.name = if_else(minicam.energy.input %in% gcamusa.REGIONAL_FUEL_MARKETS,
                                   grid_region, region)) %>%
      select(LEVEL2_DATA_NAMES[["ResReserveTechCoefMkt"]]) ->
      L211.ResTechCoef_USA

    # Delete USA fossil subresource
    L211.DeleteRsrc_fos_USA <- tibble(region = gcam.USA_REGION, resource = unique(L211.Rsrc_F_USA$resource))

    # ===================================================
    # Produce outputs

    L211.PrimaryCO2Coef_USA %>%
      add_title("USA and state fossil resource production carbon Coefs") %>%
      add_units("kg C per GJ") %>%
      add_comments("USA and state fossil resource production carbon Coefs") %>%
      add_precursors("emissions/A_PrimaryFuelCCoef",
                     "L111.Prod_EJ_R_F_Yh_USA") ->
      L211.PrimaryCO2Coef_USA

    L211.Rsrc_F_USA %>%
      add_title("resource parameters") %>%
      add_units("output unit EJ price unit 1975$ per GJ") %>%
      add_comments("resource parameters") %>%
      same_precursors_as("L211.PrimaryCO2Coef") ->
      L211.Rsrc_F_USA

    L211.RsrcPrice_F_USA %>%
      add_title("add a resource price in base year") %>%
      add_units("1975$/GJ") %>%
      add_comments("scale USA price assumptions to each state based on EIA historical wellhead prices") %>%
      same_precursors_as("L211.PrimaryCO2Coef") %>%
      add_precursors("gcam-usa/EIA_gas_market_wellhead_price_states",
                     "gcam-usa/EIA_NG_prod_mapping_wellhead_price",
                     "gcam-usa/EIA_oil_first_purchase_price_states",
                     "gcam-usa/EIA_oil_first_purchase_price_states_mapping",
                     "gcam-usa/EIA_coal_open_market_price_states",
                     "gcam-usa/EIA_coal_open_market_price_states_mapping",
                     "L210.RsrcPrice") ->
      L211.RsrcPrice_F_USA

    L211.RsrcCalProd_USA %>%
      add_title("subresource calibrated production") %>%
      add_units("EJ") %>%
      add_comments("subresource calibrated production") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA") ->
      L211.RsrcCalProd_USA

    L211.RsrcTechChange_USA %>%
      add_title("subresource tech change") %>%
      add_units("unitless") %>%
      add_comments("subresource tech change") %>%
      same_precursors_as("L211.PrimaryCO2Coef") %>%
      add_precursors("energy/A10.TechChange") ->
      L211.RsrcTechChange_USA

    L211.RsrcCurves_fos_USA %>%
      add_title("resource curve") %>%
      add_units("unitless") %>%
      add_comments("resource curve") %>%
      add_precursors("L111.ResCurves_EJ_R_Ffos_USA") ->
      L211.RsrcCurves_fos_USA

    L211.ResSubresourceProdLifetime_USA %>%
      add_title("reserve subresource lifetime") %>%
      add_units("years") %>%
      add_comments("copy global assumptions") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA",
                     "gcam-usa/A10.ResSubresourceProdLifetime_USA") ->
      L211.ResSubresourceProdLifetime_USA

    L211.SubresourcePriceAdder_USA %>%
      add_title("reserve subresource price adder in 2100") %>%
      add_units("zero") %>%
      add_comments("copy global assumptions") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA",
                     "L210.SubresourcePriceAdder") ->
      L211.SubresourcePriceAdder_USA

    L211.ReserveCalReserve_USA %>%
      add_title("historical calculated reserve") %>%
      add_units("EJ") %>%
      add_comments("follow the same method in L210.resources") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA") ->
      L211.ReserveCalReserve_USA

    L211.ResReserveTechLifetime_USA %>%
      add_title("reserve subresource technology lifetime") %>%
      add_units("years") %>%
      add_comments("copy global assumptions") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA",
                     "L210.ResReserveTechLifetime") ->
      L211.ResReserveTechLifetime_USA

    L211.ResReserveTechDeclinePhase_USA %>%
      add_title("reserve resource technology decline phase percent") %>%
      add_units("years") %>%
      add_comments("copy global assumptions") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA",
                     "L210.ResReserveTechDeclinePhase") ->
      L211.ResReserveTechDeclinePhase_USA

    L211.ResReserveTechProfitShutdown_USA %>%
      add_title("reserve resource technology profit shutdown parameters") %>%
      add_units("years") %>%
      add_comments("copy global assumptions") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA",
                     "L210.ResReserveTechProfitShutdown") ->
      L211.ResReserveTechProfitShutdown_USA

    L211.ResReserveTechInvestmentInput_USA %>%
      add_title("Non-energy input to keep track of resource curve investment cost") %>%
      add_units("NA") %>%
      add_comments("A resource reserve tech needs an input to keep track of the resource") %>%
      add_comments("curves cost that was used when the tech was invested.  It will be") %>%
      add_comments("for calculating shutdown deciders but also, this input will track") %>%
      add_comments("capital demands so those parameters are also read in") %>%
      same_attributes_as(L211.ResSubresourceProdLifetime_USA) ->
      L211.ResReserveTechInvestmentInput_USA

    L211.ResReserveTechShrwt_fossil_USA %>%
      add_title("reserve resource technology shareweight") %>%
      add_units("years") %>%
      add_comments("copy global assumptions") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA",
                     "L210.ResTechShrwt") ->
      L211.ResReserveTechShrwt_fossil_USA

    L211.Sector_prod_USA %>%
      add_title("supplysector logit table") %>%
      add_units("unitless") %>%
      add_comments("supplysector logit table") %>%
      add_precursors("gcam-usa/A10.fossil_prod_sector",
                     "gcam-usa/A10.sector_states",
                     "L111.Prod_EJ_R_F_Yh_USA") ->
      L211.Sector_prod_USA

    L211.Subsector_prod_USA %>%
      add_title("subresource logit table") %>%
      add_units("unitless") %>%
      add_comments("subresource logit table") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA") ->
      L211.Subsector_prod_USA

    L211.SubsShrwtFlt_USA %>%
      add_title("subsector shareweight table") %>%
      add_units("unitless") %>%
      add_comments("subsector shareweight table") %>%
      same_precursors_as("L211.Subsector_prod_USA") ->
      L211.SubsShrwtFlt_USA

    L211.SubsInterpRule_USA %>%
      add_title("subsector interpolation rule") %>%
      add_units("unitless") %>%
      add_comments("subsector interpolation rule") %>%
      same_precursors_as("L211.Subsector_prod_USA") %>%
      add_precursors("gcam-usa/A10.subsector_interp_states","gcam-usa/A10.fossil_subsector_interp") ->
      L211.SubsInterpRule_USA

    L211.TechShrwt %>%
      add_title("global technology database technology shareweight") %>%
      add_units("unitless") %>%
      add_comments("global technology database technology shareweight") %>%
      same_precursors_as("L211.Subsector_prod_USA") ->
      L211.TechShrwt_USA

    L211.TechCoef %>%
      add_title("global technology database technology coefficient") %>%
      add_units("unitless") %>%
      add_comments("global technology database technology coefficient") %>%
      same_precursors_as("L211.Subsector_prod_USA") ->
      L211.TechCoef_USA

    L211.TechCal_USA %>%
      add_title("natural gas resource technology calibration") %>%
      add_units("EJ") %>%
      add_comments("natural gas resource technology calibration") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA") ->
      L211.TechCal_USA

    L211.TFFSubsectorLogit %>%
      add_title("create subsector logit for national pass-through natural gas production") %>%
      add_units("unitless") %>%
      add_comments("create subsector logit for national pass-through natural gas production") %>%
      same_precursors_as("gcam-usa/A10.fossil_subsector_logit") ->
      L211.TFFSubsectorLogit

    L211.TFFTechProduction_USA %>%
      add_title("create the tech to simply pass through state production to USA and calibration technology production") %>%
      add_units("unitless") %>%
      add_comments("create the tech to simply pass through state production to USA and calibration technology production") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA") ->
      L211.TFFTechProduction_USA

    L211.TFFTechCoef %>%
      add_title("create the tech to simply pass through state production to USA and set coefficient as 1") %>%
      add_units("unitless") %>%
      add_comments("create the tech to simply pass through state production to USA and set coefficient as 1") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA") ->
      L211.TFFTechCoef_USA

    L211.TFFTechShrwtInterp %>%
      add_title("create the tech share-weight interp rule for model future years") %>%
      add_units("unitless") %>%
      add_comments("create the tech share-weight interp rule for model future years") %>%
      add_precursors("L111.Prod_EJ_R_F_Yh_USA") ->
      L211.TFFTechShrwtInterp_USA

    L211.ResTechCoef_USA %>%
      add_title("Co-efficients of resource production inputs for unconv heavy oil") %>%
      add_units("NA") %>%
      add_comments("A21.globalrsrctech_coef_USA written to all states with unconv heavy oil production") %>%
      add_legacy_name("L211.ResTechCoef") %>%
      add_precursors("gcam-usa/A21.globalrsrctech_coef_USA") ->
      L211.ResTechCoef_USA

    L211.DeleteRsrc_fos_USA %>%
      add_title("Delete USA fossil resoures and associated subresources") %>%
      add_units("unitless") %>%
      add_comments("replaced with more detailed categories of subresources for GCAM-USA") ->
      L211.DeleteRsrc_fos_USA


    return_data(L211.PrimaryCO2Coef_USA,
                L211.Rsrc_F_USA,
                L211.RsrcPrice_F_USA,
                L211.RsrcCalProd_USA,
                L211.RsrcTechChange_USA,
                L211.RsrcCurves_fos_USA,
                L211.ResSubresourceProdLifetime_USA,
                L211.SubresourcePriceAdder_USA,
                L211.ReserveCalReserve_USA,
                L211.ResReserveTechLifetime_USA,
                L211.ResReserveTechDeclinePhase_USA,
                L211.ResReserveTechProfitShutdown_USA,
                L211.ResReserveTechInvestmentInput_USA,
                L211.ResReserveTechShrwt_fossil_USA,
                L211.Sector_prod_USA,
                L211.Subsector_prod_USA,
                L211.SubsShrwtFlt_USA,
                L211.SubsInterpRule_USA,
                L211.TechShrwt_USA,
                L211.TechCoef_USA,
                L211.TechCal_USA,
                L211.TFFSubsectorLogit,
                L211.TFFTechProduction_USA,
                L211.TFFTechCoef_USA,
                L211.TFFTechShrwtInterp_USA,
                L211.ResTechCoef_USA,
                L211.DeleteRsrc_fos_USA)

  } else {
    stop("Unknown command")
  }
}
