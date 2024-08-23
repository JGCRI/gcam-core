# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L100.GDP_hist
#'
#' Prepare GDP database for later use: filter missing values and convert units to 1990 USD.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.gdp_mil90usd_ctry_Yh}. The corresponding file in the
#' original data system was \code{L100.GDP_hist.R} (socioeconomics level1).
#' @details At present the GDP database used requires no downscaling and all
#' major countries are included, so really no processing steps are needed.
#' All that happens in this file right now is filtering out \code{NA} values
#' and converting the units to GCAM's GDP unit (million 1990 USD).
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate select
#' @author BBL February 2017
module_socio_L100.GDP_hist <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/AGLU_ctry",
      FILE = "socioeconomics/GDP/GCAMFAOSTAT_GDP",
      FILE = "socioeconomics/GDP/GDP_twn")

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

    # Note that GCAMFAOSTAT_GDP has data in 2015$ so regional GDP deflators are not used
    # But values are brought back to 1990$ using USA GDP deflators
    # FAO_GDP_Deflators is added to this chunk for BYU uses
    # see module_aglu_L100.regional_ag_an_for_prices for examples
    # Note that historical regional values have been fixed using FAO_AREA_DISAGGREGATE_HIST_DISSOLUTION_ALL in gcamfaostat
    GCAMFAOSTAT_GDP %>%
      gather_years() %>%
      filter(element_code == 6184, # 2015$
             !is.na(value)) %>%
      # ToDo: GCAMFAOSTAT_GDP is more recent than other GCAMFAOSTAT files
      # FAOSTAT recent changed name of 2 countries
      # next GCAMFAOSTAT update (ready already) will use area_code to avoid dealing with country name changes
      mutate(area = if_else(grepl("Yemen ", area), "Yemen", area),
             area = if_else(area == "Netherlands (Kingdom of the)", "Netherlands", area),
             area = if_else(area == "Sint Maarten (Dutch part)", "Sint Maarten (Dutch Part)", area)) %>%
      left_join_error_no_match(AGLU_ctry %>% select(area = FAO_country, iso), by = "area") %>%
      select(iso, year, value) %>%
      # agg two Yeman
      group_by(iso, year) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      mutate(value = value * gdp_deflator(1990, base_year = 2015) ) ->
      L100.gdp_mil90usd_ctry_Yh_0

    # Taiwan from pwt but also extend to recent years using Taiwan statistics
    GDP_twn %>%
      select(iso, year, value = gdp_2015USD) %>%
      mutate(value = value * gdp_deflator(1990, base_year = 2015)) ->
      L100.gdp_mil90usd_ctry_Yh_0_TWN

    L100.gdp_mil90usd_ctry_Yh_0 %>%
      bind_rows(
        L100.gdp_mil90usd_ctry_Yh_0_TWN
      ) %>%
      add_title("Historical GDP by country (iso) since 1970 per FAOSTAT") %>%
      add_comments("Units 2015$ converted to constant 1990 USD using USA GDP deflators") %>%
      add_precursors("socioeconomics/GDP/GCAMFAOSTAT_GDP",
                     "aglu/AGLU_ctry",
                     "socioeconomics/GDP/GDP_twn") %>%
      add_units("Million 1990 USD") %>%
      add_legacy_name("L100.gdp_mil90usd_ctry_Yh") ->
      L100.gdp_mil90usd_ctry_Yh


    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}
