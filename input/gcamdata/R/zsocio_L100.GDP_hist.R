# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L100.GDP_hist
#'
#' preprocessing historical GDP data from gcamfaostat
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.gdp_mil90usd_ctry_Yh}.
#' @details preprocessing historical GDP data from gcamfaostat
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate select
#' @author XZ 2024
module_socio_L100.GDP_hist <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/AGLU_ctry",
      FILE = "socioeconomics/GDP/GCAMFAOSTAT_GDP",
      FILE = "socioeconomics/GDP/GDP_twn",
      FILE = "common/FAO_GDP_Deflators")

  MODULE_OUTPUTS <-
    c("L100.gdp_mil90usd_ctry_Yh")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    Country <- year <- value <- iso <- NULL # silence package checks.

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Module specific constants
    DEFLATOR_BASE_YEAR <- MODEL_FINAL_BASE_YEAR

    # Process region-specific GDP deflator ----
    GDP_Deflators_To_Base_Year <-
      FAO_GDP_Deflators %>%
      gather_years() %>%
      group_by(area_code) %>%
      mutate(currentUSD_per_baseyearUSD = (value / value[year == DEFLATOR_BASE_YEAR])) %>%
      ungroup() %>%
      select(area_code, year, currentUSD_per_baseyearUSD) %>%
      filter(year == socioeconomics.GCAMFAOSTAT_GDP_Dollar_Year) %>%
      select(-year)

    # Note that GCAMFAOSTAT_GDP has data in 2015$ so regional GDP deflators are not used
    # But values are brought back to 1990$ using USA GDP deflators
    # FAO_GDP_Deflators can be added to this chunk for BYU uses
    # see module_aglu_L100.regional_ag_an_for_prices for examples
    # Note that historical regional values have been fixed using FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION_ALL in gcamfaostat
    GCAMFAOSTAT_GDP %>%
      gather_years() %>%
      filter(element_code == 6184, # 2015$
             !is.na(value)) %>%
      left_join_error_no_match(AGLU_ctry %>% select(area_code = FAO_country_code, iso), by = "area_code") %>%
      left_join(GDP_Deflators_To_Base_Year,
                by = c("area_code")) %>%
      # Use USA GDP deflator for countries doesn't exist
      replace_na(list(currentUSD_per_baseyearUSD =
                        gdp_deflator(socioeconomics.GCAMFAOSTAT_GDP_Dollar_Year, DEFLATOR_BASE_YEAR))) %>%
      mutate(value = value / currentUSD_per_baseyearUSD) %>%
      select(iso, year, value) %>%
      # agg two Yeman
      group_by(iso, year) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      mutate(value = value * gdp_deflator(socioeconomics.GCAM_GDP_Dollar_Year, base_year = DEFLATOR_BASE_YEAR) ) ->
      L100.gdp_mil90usd_ctry_Yh_0

    # Taiwan from pwt but also extend to recent years using Taiwan statistics
    GDP_twn %>%
      select(iso, year, value = gdp_2015USD) %>%
      left_join_error_no_match(
        GDP_Deflators_To_Base_Year %>%
          inner_join(
            AGLU_ctry %>% filter(iso == "twn") %>% distinct(area_code = FAO_country_code, iso),
                     by = "area_code") %>% select(-area_code),
        by = "iso"
      ) %>%
      mutate(value = value / currentUSD_per_baseyearUSD *
               gdp_deflator(socioeconomics.GCAM_GDP_Dollar_Year, base_year = socioeconomics.GCAMFAOSTAT_GDP_Dollar_Year)) %>%
      select(-currentUSD_per_baseyearUSD) ->
      L100.gdp_mil90usd_ctry_Yh_0_TWN


    L100.gdp_mil90usd_ctry_Yh_0 %>%
      bind_rows(
        L100.gdp_mil90usd_ctry_Yh_0_TWN
      ) %>%
      add_title("Historical GDP by country (iso) since 1970 per FAOSTAT") %>%
      add_comments("Units 2021$ converted to constant 1990 USD using USA GDP deflators") %>%
      add_precursors(MODULE_INPUTS) %>%
      add_units("Million 1990 USD") %>%
      add_legacy_name("L100.gdp_mil90usd_ctry_Yh") ->
      L100.gdp_mil90usd_ctry_Yh


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
