# module-socioeconomics.R


#' module_socioeconomics_L100.GDP_hist
#'
#' Construct the \code{socioeconomics} data structures.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}.
#' @author BBL
#' @export
module_socioeconomics_L100.GDP_hist <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "socioeconomics/USDA_GDP_MER"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return("L100.gdp_mil90usd_ctry_Yh")
  } else if(command == driver.MAKE) {
    socioeconomics_L100.GDP_hist_makedata(...)
  } else {
    stop("Unknown command")
  }
}


#' socioeconomics_L100.GDP_hist_makedata
#'
#' @param all_data A named list, holding all data system products so far
#' @return A named list with all \code{socioeconomics_L100.GDP_hist} data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
socioeconomics_L100.GDP_hist_makedata <- function(all_data) {

  #printlog( "Historical GDP downscaled to modern country" )
  usda_gdp_mer <- get_data(all_data, "socioeconomics/USDA_GDP_MER")
  assert_that(tibble::is.tibble(usda_gdp_mer))

  # At present the GDP database used requires no downscaling and all
  # major countries are included, so really no processing steps are needed.
  # All that happens in this file right now is subsetting the years that
  # will be required by later files, and converting the units to GCAM's
  # GDP unit (million 1990 USD)

  # Convert to long form, filter to historical years, convert units
  usda_gdp_mer %>%
    gather(year, value, -Country, -iso) %>%
    mutate(year = as.numeric(year)) %>%
    filter(year %in% HISTORICAL_YEARS, !is.na(value), !is.na(iso)) %>%
    mutate(value = value * CONV_BIL_MIL / CONV_1990_2005_USD) %>%
    select(-Country) %>%
    add_comments(c("Historical GDP downscaled to country (iso)",
                     "Unit = million 1990 US dollars")) %>%
    # flag that this dataset is in different form from original
    add_flags(FLAG_LONG_FORM, FLAG_NO_XYEAR) ->
    L100.gdp_mil90usd_ctry_Yh

  return_data(L100.gdp_mil90usd_ctry_Yh)
}


#' downscale_Maddison_country
#'
#'function to downscale the countries that separated into multiple modern countries (e.g. USSR).
#'
#' @param data data
#' @param downscale_country_name country_name
#' @param socioeconomics_ctry socioeconomics_ctry
#' @param available_year available_year
#' @return something
#' @export
downscale_Maddison_country <- function(data,
                                       downscale_country_name,
                                       socioeconomics_ctry,
                                       available_year) {

  return(data)


  # socioeconomics_ctry$iso[!is.na(socioeconomics_ctry$Downscale_from) & socioeconomics_ctry$Downscale_from ==
  #                           "Czechoslovakia"]
  socioeconomics_ctry  %>%
    filter(Downscale_from == downscale_country_name) ->
    iso_codes

  data %>%
    filter(year <= available_year, Country == downscale_country_name) %>%
    mutate(ratio = value / nth(value, which(year == available_year))) %>%
    select(year, ratio) %>%
    right_join(filter(data, iso %in% iso_codes$iso), by = "year") %>%
    group_by(iso) %>%
    mutate(value = nth(value, which(year == available_year)) * ratio) %>%
    spread(year, value)

  ctry_years <- years[years < available_year]
  data_ratio <- subset(data, Country == country_name)
  data_ratio[c(X_ctry_years, X_available_year)] <- data_ratio[c(X_ctry_years, X_available_year)]/data_ratio[[X_available_year]]
  data_ratio <- data_ratio[rep(1, times = length(iso_codes)), ]
  data_downscaled <- subset(data, iso %in% iso_codes)
  data_downscaled[X_ctry_years] <- data_downscaled[[X_available_year]] * data_ratio[X_ctry_years]
  data[data$iso %in% iso_codes, ] <- data_downscaled
  return(data)
}

