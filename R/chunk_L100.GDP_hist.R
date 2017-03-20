#' module_socioeconomics_L100.GDP_hist
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.gdp_mil90usd_ctry_Yh}. The corresponding file in the
#' original data system was \code{L100.GDP_hist.R} (socioeconomics level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @import dplyr
#' @importFrom tidyr gather spread
#' @author BBL
module_socioeconomics_L100.GDP_hist <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "socioeconomics/USDA_GDP_MER"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.gdp_mil90usd_ctry_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
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
      add_title("Historical GDP downscaled to country (iso)") %>%
      add_comments("Filtered to historical years, units converted") %>%
      add_precursors("socioeconomics/USDA_GDP_MER") %>%
      add_units("Million 1990 USD") %>%
      # flag that this dataset is in different form from original
      add_legacy_name("L100.gdp_mil90usd_ctry_Yh") %>%
      add_flags(FLAG_LONG_FORM, FLAG_NO_XYEAR) ->
      L100.gdp_mil90usd_ctry_Yh

    return_data(L100.gdp_mil90usd_ctry_Yh)
  } else {
    stop("Unknown command")
  }
}



