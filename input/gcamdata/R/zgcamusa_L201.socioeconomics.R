# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamusa_L201.socioeconomics
#'
#' Interest rate, population, and GDP for GCAM-USA.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.InterestRate_GCAMUSA}, \code{L201.Pop_GCAMUSA}, \code{L201.BaseGDP_GCAMUSA},
#' \code{L201.LaborForceFillout_GCAMUSA}, \code{L201.Pop_national_updated_USA},
#' \code{L201.BaseGDP_national_updated_USA}, \code{L201.LaborProductivity_national_updated_USA}.
#' The corresponding file in the original data system was \code{L201.socioeconomics_USA.R} (gcam-usa level2).
#' @details Interest rate, population, and GDP for GCAM-USA.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter lag mutate select rename
#' @author RLH October 2017
module_gcamusa_L201.socioeconomics <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L100.Pop_thous_state",
             "L100.GDP_mil90usd_state",
             FILE = "socioeconomics/gcam_macro_nested_CES_function",
             FILE = "gcam-usa/states_subregions",
             "L201.Labor_Rsrc_price"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.Pop_GCAMUSA",
             "L201.GDP_GCAMUSA",
             "L201.Pop_national_updated_USA",
             "L201.GDP_national_updated_USA",
             "L201.DeleteNestedCESMacro_USA",
             "L201.LaborMaterials_UnlimitRsrc_USA",
             "L201.LaborMaterials_UnlimitRsrcPrice_USA",
             "L201.DeleteRsrc_USA",
             "L201.BasePriceIgnoreRegion_USA",
             "L201.CapResource_USA",
             "L201.CapResourcePrice_USA",
             "L201.CapitalLink_USA"))
  } else if(command == driver.MAKE) {

    # silence package checks
    year <- value <- state <- totalPop <- baseGDP <- iso <- growth <- timestep <- region <-
      GDP <- pop <- laborproductivity <- fn.name <- resource <- price <- unlimited.resource <-
      ignore.region <- linked.ghg.policy <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L100.Pop_thous_state <- get_data(all_data, "L100.Pop_thous_state", strip_attributes = TRUE)
    L100.GDP_mil90usd_state <- get_data(all_data, "L100.GDP_mil90usd_state", strip_attributes = TRUE)
    gcam_macro_nested_CES_function <- get_data(all_data, "socioeconomics/gcam_macro_nested_CES_function", strip_attributes = TRUE)
    states_subregions <- get_data(all_data, "gcam-usa/states_subregions", strip_attributes = TRUE)
    L201.Labor_Rsrc_price <- get_data(all_data, "L201.Labor_Rsrc_price", strip_attributes = TRUE)

    # ===================================================
    # NOTE: Socioeconomics for grid regions are dealt with in module_gcamusa_L223.electricity

    # L201.Pop_GCAMUSA: Population by region from the GCAM 3.0 core scenario
    L201.Pop_GCAMUSA <- L100.Pop_thous_state %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(totalPop = value,
             region = state) %>%
      mutate(totalPop = round(totalPop, socioeconomics.POP_DIGITS))

    # L201.GDP_GCAMUSA: Base GDP for GCAM-USA scenario
    L201.GDP_GCAMUSA <- L100.GDP_mil90usd_state %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(GDP = value,
             region = state) %>%
      mutate(GDP = round(GDP, socioeconomics.GDP_DIGITS))

    # Add USA-region updates
    # Updated USA-region population
    L201.Pop_GCAMUSA %>%
      group_by(year) %>%
      summarise(totalPop = sum(totalPop)) %>%
      ungroup() %>%
      mutate(totalPop = round(totalPop, socioeconomics.POP_DIGITS),
             region = gcam.USA_REGION) %>%
      select(region, year, totalPop) -> L201.Pop_national_updated_USA

    # Updated USA-region base GDP
    L201.GDP_GCAMUSA %>%
      group_by(year) %>%
      summarise(GDP = sum(GDP)) %>%
      ungroup() %>%
      mutate(GDP = round(GDP, socioeconomics.GDP_DIGITS),
             region = gcam.USA_REGION) %>%
      select(region, year, GDP) -> L201.GDP_national_updated_USA


    # ===================================================

    # L201.DeleteNestedCESMacro_USA: delete nested CES macro function for the USA region
    # (GCAM-USA handles this at the state level)
    gcam_macro_nested_CES_function %>%
      distinct(fn.name) %>%
      mutate(region = gcam.USA_REGION) %>%
      select(region, fn.name) ->
      L201.DeleteNestedCESMacro_USA

    # L201.LaborMaterials_UnlimitRsrc_USA: unlimited resource definition for Labor_Materials
    # for the USA region, using same units as L201.Labor_Rsrc_price
    L201.Labor_Rsrc_price %>%
      filter(region == gcam.USA_REGION, resource == "Labor_Materials") %>%
      distinct(region, unlimited.resource = resource, output.unit, price.unit) %>%
      mutate(market = region) ->
      L201.LaborMaterials_UnlimitRsrc_USA

    # L201.LaborMaterials_UnlimitRsrcPrice_USA: price for Labor_Materials unlimited resource
    # for the USA region. Prices come from L201.Labor_Rsrc_price for MODEL_BASE_YEARS,
    # then held constant (rule=2) for all MODEL_YEARS.
    L201.Labor_Rsrc_price %>%
      filter(region == gcam.USA_REGION, resource == "Labor_Materials") %>%
      select(region, unlimited.resource = resource, year, price) %>%
      complete(nesting(region, unlimited.resource),
               year = MODEL_YEARS) %>%
      group_by(region, unlimited.resource) %>%
      mutate(price = approx_fun(year, price, rule = 2)) %>%
      ungroup() ->
      L201.LaborMaterials_UnlimitRsrcPrice_USA

    # L201.DeleteRsrc_USA: delete "capital" and "Labor_Materials" resources from the USA region
    tibble(region = gcam.USA_REGION,
           resource = c("capital", "Labor_Materials")) ->
      L201.DeleteRsrc_USA

    # L201.BasePriceIgnoreRegion_USA: ignore regions for base price calculation
    # (USA, all states, grid regions, and PADDs from states_subregions)
    states_subregions %>%
      select(state, grid_region, PADD) %>%
      tidyr::gather(col, ignore.region) %>%
      distinct(ignore.region) %>%
      select(ignore.region) %>%
      bind_rows(tibble(ignore.region = gcam.USA_REGION), .) ->
      L201.BasePriceIgnoreRegion_USA

    # L201.CapResource_USA: unlimited resource "capital" for USA region + all states
    # using same units as L281.CapResource
    tibble(region = c(gcam.USA_REGION, gcamusa.STATES)) %>%
      mutate(unlimited.resource = socioeconomics.CAPITAL_MARKET_NAME,
             output.unit = "billion 1975$ per timestep",
             price.unit = "rate",
             market = region) ->
      L201.CapResource_USA

    # L201.CapResourcePrice_USA: price for capital resource = DEFAULT_INTEREST_RATE for all MODEL_YEARS
    tibble(region = c(gcam.USA_REGION, gcamusa.STATES)) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(unlimited.resource = socioeconomics.CAPITAL_MARKET_NAME,
             price = socioeconomics.DEFAULT_INTEREST_RATE) %>%
      select(region, unlimited.resource, year, price) ->
      L201.CapResourcePrice_USA

    # L201.CapitalLink_USA: capital link for states (similar to L281.CapitalLink but for gcamusa.STATES)
    L201.CapitalLink_USA <- tibble(region = gcamusa.STATES) %>%
      mutate(price.adjust = 1,
             demand.adjust = 1,
             market = region,
             linked.policy = socioeconomics.CAPITAL_MARKET_NAME,
             output.unit = "billion 1975$ per timestep",
             price.unit = "rate") %>%
      repeat_add_columns(tibble(linked.ghg.policy = c(
        socioeconomics.EN_CAPITAL_MARKET_NAME,
        socioeconomics.EN_DURABLE_MARKET_NAME)))

    # ===================================================

    # Produce outputs
    L201.Pop_GCAMUSA %>%
      add_title("Population by state") %>%
      add_units("thousand persons") %>%
      add_comments("Data from L100.Pop_thous_state") %>%
      add_legacy_name("L201.Pop_GCAMUSA") %>%
      add_precursors("L100.Pop_thous_state") ->
      L201.Pop_GCAMUSA

    L201.GDP_GCAMUSA %>%
      add_title("GDP by state and year") %>%
      add_units("million 1990 USD") %>%
      add_comments("Data from L100.GDP_mil90usd_state") %>%
      add_comments("The base GDP which will used when run in fixed or calibrated GDP modes") %>%
      add_comments("When in open GDP mode the actual GDP in modeled years may differ.") %>%
      add_precursors("L100.GDP_mil90usd_state") ->
      L201.GDP_GCAMUSA

    L201.Pop_national_updated_USA %>%
      add_title("Updated population for USA region, consistent with sum-of-states") %>%
      add_units("thousand persons") %>%
      add_comments("Updates USA region population to match the 50 state + DC total") %>%
      add_legacy_name("L2011.Pop_updated_USA_national") %>%
      same_precursors_as("L201.Pop_GCAMUSA") ->
      L201.Pop_national_updated_USA

    L201.GDP_national_updated_USA %>%
      add_title("Updated base year GDP for USA region, consistent with sum-of-states") %>%
      add_units("million 1990 USD") %>%
      add_comments("Updates USA region base year GDP to match the 50 state + DC total") %>%
      same_precursors_as("L201.GDP_GCAMUSA") ->
      L201.GDP_national_updated_USA

    L201.DeleteNestedCESMacro_USA %>%
      add_title("Delete nested CES macro function for the USA region") %>%
      add_units("NA") %>%
      add_comments("GCAM-USA handles the nested CES macro at the state level") %>%
      add_precursors("socioeconomics/gcam_macro_nested_CES_function") ->
      L201.DeleteNestedCESMacro_USA

    L201.LaborMaterials_UnlimitRsrc_USA %>%
      add_title("Unlimited resource definition for Labor_Materials in the USA region") %>%
      add_units("NA") %>%
      add_comments("Labor_Materials unlimited resource for USA region") %>%
      add_precursors("L201.Labor_Rsrc_price") ->
      L201.LaborMaterials_UnlimitRsrc_USA

    L201.LaborMaterials_UnlimitRsrcPrice_USA %>%
      add_title("Price for Labor_Materials unlimited resource in the USA region") %>%
      add_units("1990$/pers") %>%
      add_comments("Prices from L201.Labor_Rsrc_price for base years, held constant for future years") %>%
      add_precursors("L201.Labor_Rsrc_price") ->
      L201.LaborMaterials_UnlimitRsrcPrice_USA

    L201.DeleteRsrc_USA %>%
      add_title("Delete capital and Labor_Materials resources from the USA region") %>%
      add_units("NA") %>%
      add_comments("These resources are handled at the state level in GCAM-USA") %>%
      add_precursors("L201.Labor_Rsrc_price") ->
      L201.DeleteRsrc_USA

    L201.BasePriceIgnoreRegion_USA %>%
      add_title("Regions to ignore in base price calculation for GCAM-USA") %>%
      add_units("NA") %>%
      add_comments("Distinct state, grid_region, and PADD values from states_subregions") %>%
      add_precursors("gcam-usa/states_subregions") ->
      L201.BasePriceIgnoreRegion_USA

    L201.CapResource_USA %>%
      add_title("Capital unlimited resource for USA region and all states") %>%
      add_units("NA") %>%
      add_comments("Capital resource definition for USA region and all 50 states + DC") %>%
      add_precursors("L201.Labor_Rsrc_price") ->
      L201.CapResource_USA

    L201.CapResourcePrice_USA %>%
      add_title("Capital resource price for USA region and all states") %>%
      add_units("rate") %>%
      add_comments("Price set to DEFAULT_INTEREST_RATE for all model years") %>%
      add_precursors("L201.Labor_Rsrc_price") ->
      L201.CapResourcePrice_USA

    L201.CapitalLink_USA %>%
      add_title("Capital link for GCAM-USA states") %>%
      add_units("NA") %>%
      add_comments("Links energy, ag, and consumer durable capital markets to the state capital market") %>%
      add_precursors("L201.Labor_Rsrc_price") ->
      L201.CapitalLink_USA


    return_data(L201.Pop_GCAMUSA,
                L201.GDP_GCAMUSA,
                L201.Pop_national_updated_USA,
                L201.GDP_national_updated_USA,
                L201.DeleteNestedCESMacro_USA,
                L201.LaborMaterials_UnlimitRsrc_USA,
                L201.LaborMaterials_UnlimitRsrcPrice_USA,
                L201.DeleteRsrc_USA,
                L201.BasePriceIgnoreRegion_USA,
                L201.CapResource_USA,
                L201.CapResourcePrice_USA,
                L201.CapitalLink_USA)
  } else {
    stop("Unknown command")
  }
}
