

#' module_socioeconomics_inputs
#'
#' Load the \code{socioeconomics} input data
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}
#' @author BBL
#' @export
module_socioeconomics_inputs <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(NULL)   # module_socioeconomics_inputs doesn't depend on anything
  } else if(command == driver.DECLARE_OUTPUTS) {
    return("socioeconomics.USDA_GDP_MER")
  } else if(command == driver.MAKE) {
    # Read an input file and return data
    return(list("socioeconomics.USDA_GDP_MER" = load_csv("USDA_GDP_MER.csv")))
  } else {
    stop("Unknown command")
  }
}


#' module_socioeconomics_L100.GDP_hist
#'
#' Construct the \code{socioeconomics} data structures
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}
#' @author BBL
#' @export
module_socioeconomics_L100.GDP_hist <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return("socioeconomics.USDA_GDP_MER")
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
#' @return A named list with all socioeconomics_L100.GDP_hist_makedata data
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
socioeconomics_L100.GDP_hist_makedata <- function(all_data) {

  #printlog( "Historical GDP downscaled to modern country" )
  usda_gdp_mer <- all_data[["socioeconomics.USDA_GDP_MER"]]
  assert_that(is.data.frame(usda_gdp_mer))

  # At present the GDP database used requires no downscaling and all
  # major countries are included, so really no processing steps are needed.
  # All that happens in this file right now is subsetting the years that
  # will be required by later files, and converting the units to GCAM's
  # GDP unit (million 1990 USD)

  # Convert to long form, filter to historical years, convert units
  usda_gdp_mer %>%
    gather(year, value, -Country, -iso) %>%
    filter(year %in% HISTORICAL_YEARS, !is.na(value)) %>%
    mutate(value = value * CONV_BIL_MIL / CONV_1990_2005_USD) %>%
    select(-Country) ->
    L100.gdp_mil90usd_ctry_Yh

  comment(L100.gdp_mil90usd_ctry_Yh) <- c("Historical GDP downscaled to country (iso)",
                                          "Unit = million 1990 US dollars")

  list("L100.gdp_mil90usd_ctry_Yh" = L100.gdp_mil90usd_ctry_Yh)
}
