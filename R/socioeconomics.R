# socioeconomics.R


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
    return("socioeconomics/USDA_GDP_MER")
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
    add_dscomments(c("Historical GDP downscaled to country (iso)",
                     "Unit = million 1990 US dollars")) %>%
    # flag that this dataset is in different form from original
    add_dsflags(FLAG_LONG_NO_X_FORM) ->
    L100.gdp_mil90usd_ctry_Yh

  return_data(L100.gdp_mil90usd_ctry_Yh)
}


#' module_socioeconomics_L100.GDP_hist
#'
#' Construct the \code{socioeconomics} data structures.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}.
#' @author BBL
#' @export
module_socioeconomics_L102.GDP <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("common/iso_GCAM_regID",
             "socioeconomics/SSP_database_v9",
             "socioeconomics/GCAM3_GDP",
             "L100.gdp_mil90usd_ctry_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return("L102.PPP_MER_R")
  } else if(command == driver.MAKE) {
    socioeconomics_L102.GDP_makedata(...)
  } else {
    stop("Unknown command")
  }
}


#' socioeconomics_L102.GDP_makedata
#'
#' @param all_data A named list, holding all data system products so far
#' @return A named list with all \code{socioeconomics_L102.GDP} data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
socioeconomics_L102.GDP_makedata <- function(all_data) {
  # printlog("Historical and future GDP by GCAM region")

  iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")

  # Note this file has empty columns that get guessed as character, not numeric
  SSP_database_v9 <- get_data(all_data, "socioeconomics/SSP_database_v9")

  #browser()

  GCAM3_GDP <- get_data(all_data, "socioeconomics/GCAM3_GDP")
  L100.gdp_mil90usd_ctry_Yh <- get_data(all_data, "L100.gdp_mil90usd_ctry_Yh")
  # L101.Pop_thous_GCAM3_R_Y <- readdata("SOCIO_LEVEL1_DATA", "L101.Pop_thous_GCAM3_R_Y")
  # L101.Pop_thous_GCAM3_ctry_Y <- readdata("SOCIO_LEVEL1_DATA", "L101.Pop_thous_GCAM3_ctry_Y")
  # L101.Pop_thous_R_Yh <- readdata("SOCIO_LEVEL1_DATA", "L101.Pop_thous_R_Yh")
  # L101.Pop_thous_SSP_R_Yfut <- readdata("SOCIO_LEVEL1_DATA", "L101.Pop_thous_SSP_R_Yfut")
  #
  # # ----------------------------------------------------------------------------- 2.
  # # Perform computations
  # printlog("Aggregating historical and future GDP by GCAM region")
  # L100.gdp_mil90usd_ctry_Yh[[R]] <- iso_GCAM_regID[[R]][match(L100.gdp_mil90usd_ctry_Yh$iso,
  #                                                             iso_GCAM_regID$iso)]
  # L102.gdp_mil90usd_R_Yh <- aggregate(L100.gdp_mil90usd_ctry_Yh[X_historical_years], by = as.list(L100.gdp_mil90usd_ctry_Yh[R]),
  #                                     sum)
  #
  # # Future GDP in the SSP scenarios
  # printlog("NOTE: this point in the processing method defines the maximum number of countries that may be regions in GCAM (184)")
  # L102.gdp_bilusd_ctry_Yfut <- subset(SSP_database_v9, MODEL == gdp_model & VARIABLE ==
  #                                       "GDP|PPP")
  # L102.gdp_bilusd_ctry_Yfut$iso <- tolower(L102.gdp_bilusd_ctry_Yfut$REGION)
  #
  # # Romania is called 'rou' in the SSP database. reset this
  # L102.gdp_bilusd_ctry_Yfut$iso[L102.gdp_bilusd_ctry_Yfut$iso == "rou"] <- "rom"
  #
  # # Map in the region and scenario
  # L102.gdp_bilusd_ctry_Yfut[[R]] <- iso_GCAM_regID[[R]][match(L102.gdp_bilusd_ctry_Yfut$iso,
  #                                                             iso_GCAM_regID$iso)]
  # L102.gdp_bilusd_ctry_Yfut[[Scen]] <- substr(L102.gdp_bilusd_ctry_Yfut$SCENARIO, 1, 4)
  #
  # # Specify which years in the SSP databases to use
  # X_SSP_years <- names(L102.gdp_bilusd_ctry_Yfut)[names(L102.gdp_bilusd_ctry_Yfut) %in%
  #                                                   c(X_historical_years, X_future_years)]
  #
  # # Subset only the years that aren't missing
  # X_SSP_years <- names(colSums(L102.gdp_bilusd_ctry_Yfut[X_SSP_years])[!is.na(colSums(L102.gdp_bilusd_ctry_Yfut[X_SSP_years]))])
  #
  # # Find any common years between the historical data and the 'future' data, for
  # # computation of ratios
  # X_common_years <- X_SSP_years[X_SSP_years %in% names(L102.gdp_mil90usd_R_Yh)]
  #
  # # Use the most recent year as the transition point between the two databases
  # printlog("NOTE: using historical data through final historical year, and ratios from final historical year in projections")
  # X_base_year <- X_common_years[length(X_common_years)]
  #
  # L102.gdp_bilusd_SSP_R_Yfut <- aggregate(L102.gdp_bilusd_ctry_Yfut[X_SSP_years], by = as.list(L102.gdp_bilusd_ctry_Yfut[Scen_R]),
  #                                         sum)
  #
  # # Calculate the GDP ratios from the first year in the projections. Use this ratio to
  # # project GDP from historical dataset in final historical period
  # L102.gdpRatio_SSP_R_Yfut <- L102.gdp_bilusd_SSP_R_Yfut[c(Scen_R, X_future_years)]
  # L102.gdpRatio_SSP_R_Yfut[X_future_years] <- L102.gdp_bilusd_SSP_R_Yfut[X_future_years]/L102.gdp_bilusd_SSP_R_Yfut[[X_base_year]]
  #
  # # Use these ratios to build the GDP trajectories by SSP
  # L102.gdp_mil90usd_SSP_R_Y <- repeat_and_add_vector(L102.gdp_mil90usd_R_Yh, Scen, unique(L102.gdpRatio_SSP_R_Yfut[[Scen]]))
  # L102.gdp_mil90usd_SSP_R_Y[X_future_years] <- L102.gdp_mil90usd_SSP_R_Y[[X_base_year]] *
  #   L102.gdpRatio_SSP_R_Yfut[match(vecpaste(L102.gdp_mil90usd_SSP_R_Y[Scen_R]), vecpaste(L102.gdpRatio_SSP_R_Yfut[Scen_R])),
  #                            X_future_years]
  # L102.gdp_mil90usd_SSP_R_Y <- L102.gdp_mil90usd_SSP_R_Y[c(Scen_R, X_historical_years, X_future_years)]
  #
  # printlog("Calculating per-capita GDP by GCAM region and historical year and SSP scenario")
  # # First, merge the population datasets (historical and future/scenario)
  # L102.Pop_thous_SSP_R_Y <- L101.Pop_thous_SSP_R_Yfut
  # L102.Pop_thous_SSP_R_Y[X_historical_years] <- L101.Pop_thous_R_Yh[match(L102.Pop_thous_SSP_R_Y[[R]],
  #                                                                         L101.Pop_thous_R_Yh[[R]]), X_historical_years]
  # L102.Pop_thous_SSP_R_Y <- L102.Pop_thous_SSP_R_Y[c(Scen_R, X_historical_years, X_future_years)]
  #
  # # Calculate per-capita GDP
  # L102.pcgdp_thous90USD_SSP_R_Y <- L102.gdp_mil90usd_SSP_R_Y
  # L102.pcgdp_thous90USD_SSP_R_Y[c(X_historical_years, X_future_years)] <- L102.gdp_mil90usd_SSP_R_Y[c(X_historical_years,
  #                                                                                                     X_future_years)]/L102.Pop_thous_SSP_R_Y[match(vecpaste(L102.pcgdp_thous90USD_SSP_R_Y[Scen_R]),
  #                                                                                                                                                   vecpaste(L102.Pop_thous_SSP_R_Y[Scen_R])), c(X_historical_years, X_future_years)]
  #
  # # GDP by GCAM region from GCAM 3.0 GDPs.
  # printlog("Downscaling GCAM 3.0 GDP by GCAM 3.0 region to countries, using SSP2 GDP scenario")
  # # GDP by GCAM 3.0 region - downscale to country according to actual shares in the
  # # historical periods, and SSPbase in the future periods
  # L102.gdp_mil90usd_ctry_Yh <- subset(L100.gdp_mil90usd_ctry_Yh, iso %in% L102.gdp_bilusd_ctry_Yfut$iso)
  # L102.gdp_mil90usd_ctry_Yh[X_future_years] <- L102.gdp_bilusd_ctry_Yfut[match(paste(L102.gdp_mil90usd_ctry_Yh$iso,
  #                                                                                    base_pop_scen), paste(L102.gdp_bilusd_ctry_Yfut$iso, L102.gdp_bilusd_ctry_Yfut[[Scen]])),
  #                                                                        X_future_years]
  # L102.gdp_mil90usd_ctry_Yh$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[match(L102.gdp_mil90usd_ctry_Yh$iso,
  #                                                                             iso_GCAM_regID$iso)]
  # L102.gdp_mil90usd_SSPbase_RG3_Y <- aggregate(L102.gdp_mil90usd_ctry_Yh[c(X_historical_years,
  #                                                                          X_future_years)], by = as.list(L102.gdp_mil90usd_ctry_Yh["region_GCAM3"]), sum)
  #
  # # Calculate shares of each country within its region over the historical time series
  # L102.gdpshares_ctryRG3_Y <- L102.gdp_mil90usd_ctry_Yh[c("iso", "region_GCAM3", X_historical_years,
  #                                                         X_future_years)]
  # L102.gdpshares_ctryRG3_Y[c(X_historical_years, X_future_years)] <- L102.gdp_mil90usd_ctry_Yh[c(X_historical_years,
  #                                                                                                X_future_years)]/L102.gdp_mil90usd_SSPbase_RG3_Y[match(L102.gdp_mil90usd_ctry_Yh$region_GCAM3,
  #                                                                                                                                                       L102.gdp_mil90usd_SSPbase_RG3_Y$region_GCAM3), c(X_historical_years, X_future_years)]
  #
  # # Interpolate the GCAM population data to all historical and future years
  # L102.gdp_mil90usd_GCAM3_RG3_Y <- gcam_interp(GCAM3_GDP, c(historical_years, future_years))[c("region_GCAM3",
  #                                                                                              X_historical_years, X_future_years)]
  #
  # if ("X2100" %in% X_future_years && "X2100" %!in% names(L102.gdp_mil90usd_GCAM3_RG3_Y)) {
  #   printlog("Extending GCAM 3.0 scenario to 2100 using SSPbase GDP ratios by GCAM 3.0 region")
  #   L102.gdp_mil90usd_GCAM3_RG3_Y$X2100 <- L102.gdp_mil90usd_GCAM3_RG3_Y$X2095 * L102.gdp_mil90usd_SSPbase_RG3_Y$X2100[match(L102.gdp_mil90usd_GCAM3_RG3_Y$region_GCAM3,
  #                                                                                                                            L102.gdp_mil90usd_SSPbase_RG3_Y$region_GCAM3)]/L102.gdp_mil90usd_SSPbase_RG3_Y$X2095[match(L102.gdp_mil90usd_GCAM3_RG3_Y$region_GCAM3,
  #                                                                                                                                                                                                                       L102.gdp_mil90usd_SSPbase_RG3_Y$region_GCAM3)]
  #   L102.gdp_mil90usd_GCAM3_RG3_Y <- gcam_interp(L102.gdp_mil90usd_GCAM3_RG3_Y, c(historical_years,
  #                                                                                 future_years))
  # }
  # GCAM_GDP_years <- as.numeric(substr(names(GCAM3_GDP), 2, 5)[grepl(XYEARPATTERN, names(GCAM3_GDP))])
  # X_GCAM_GDP_years <- year_to_xyear(GCAM_GDP_years)
  # X_first_GCAM_GDP_year <- year_to_xyear(min(GCAM_GDP_years))
  # X_first_historical_year <- X_historical_years[1]
  # if (min(historical_years) < min(GCAM_GDP_years)) {
  #   printlog("Extending GCAM 3.0 scenario to first historical year using historical GDP ratios by GCAM 3.0 region")
  #   L102.gdp_mil90usd_GCAM3_RG3_Y[[X_first_historical_year]] <- L102.gdp_mil90usd_GCAM3_RG3_Y[[X_first_GCAM_GDP_year]] *
  #     L102.gdp_mil90usd_SSPbase_RG3_Y[[X_first_historical_year]][match(L102.gdp_mil90usd_GCAM3_RG3_Y$region_GCAM3,
  #                                                                      L102.gdp_mil90usd_SSPbase_RG3_Y$region_GCAM3)]/L102.gdp_mil90usd_SSPbase_RG3_Y[[X_first_GCAM_GDP_year]][match(L102.gdp_mil90usd_GCAM3_RG3_Y$region_GCAM3,
  #                                                                                                                                                                                    L102.gdp_mil90usd_SSPbase_RG3_Y$region_GCAM3)]
  #   L102.gdp_mil90usd_GCAM3_RG3_Y <- gcam_interp(L102.gdp_mil90usd_GCAM3_RG3_Y, c(historical_years,
  #                                                                                 future_years))
  # }
  #
  # # Multiply these GDP numbers by the shares of each country within GCAM region
  # L102.gdp_mil90usd_GCAM3_ctry_Y <- L102.gdpshares_ctryRG3_Y
  # L102.gdp_mil90usd_GCAM3_ctry_Y[c(X_historical_years, X_future_years)] <- L102.gdpshares_ctryRG3_Y[c(X_historical_years,
  #                                                                                                     X_future_years)] * L102.gdp_mil90usd_GCAM3_RG3_Y[match(L102.gdpshares_ctryRG3_Y$region_GCAM3,
  #                                                                                                                                                            L102.gdp_mil90usd_GCAM3_RG3_Y$region_GCAM3), c(X_historical_years, X_future_years)]
  #
  # printlog("Aggregating by GCAM regions")
  # L102.gdp_mil90usd_GCAM3_ctry_Y[[R]] <- iso_GCAM_regID[[R]][match(L102.gdp_mil90usd_GCAM3_ctry_Y$iso,
  #                                                                  iso_GCAM_regID$iso)]
  # L102.gdp_mil90usd_GCAM3_R_Y <- aggregate(L102.gdp_mil90usd_GCAM3_ctry_Y[c(X_historical_years,
  #                                                                           X_future_years)], by = as.list(L102.gdp_mil90usd_GCAM3_ctry_Y[R]), sum)
  # L102.gdp_mil90usd_GCAM3_ctry_Y <- L102.gdp_mil90usd_GCAM3_ctry_Y[c("iso", X_historical_years,
  #                                                                    X_future_years)]
  #
  # # Calculate per-capita GDP
  # L102.pcgdp_thous90USD_GCAM3_R_Y <- L102.gdp_mil90usd_GCAM3_R_Y
  # L102.pcgdp_thous90USD_GCAM3_R_Y[c(X_historical_years, X_future_years)] <- L102.gdp_mil90usd_GCAM3_R_Y[c(X_historical_years,
  #                                                                                                         X_future_years)]/L101.Pop_thous_GCAM3_R_Y[match(L102.gdp_mil90usd_GCAM3_R_Y[[R]],
  #                                                                                                                                                         L101.Pop_thous_GCAM3_R_Y[[R]]), c(X_historical_years, X_future_years)]
  #
  # L102.pcgdp_thous90USD_GCAM3_ctry_Y <- L102.gdp_mil90usd_GCAM3_ctry_Y
  # L102.pcgdp_thous90USD_GCAM3_ctry_Y[c(X_historical_years, X_future_years)] <- L102.gdp_mil90usd_GCAM3_ctry_Y[c(X_historical_years,
  #                                                                                                               X_future_years)]/L101.Pop_thous_GCAM3_ctry_Y[match(L102.gdp_mil90usd_GCAM3_ctry_Y[["iso"]],
  #                                                                                                                                                                  L101.Pop_thous_GCAM3_ctry_Y[["iso"]]), c(X_historical_years, X_future_years)]
  #
  # # Write out the difference between PPP and MER GDP by region
  # PPP_MER_year <- 2010
  # X_PPP_MER_year <- year_to_xyear(PPP_MER_year)
  # L102.PPP_MER_R <- aggregate(L100.gdp_mil90usd_ctry_Yh[X_PPP_MER_year] * conv_1990_2005_USD *
  #                               conv_thous_mil, by = as.list(L100.gdp_mil90usd_ctry_Yh[R]), sum)
  # names(L102.PPP_MER_R)[names(L102.PPP_MER_R) == X_PPP_MER_year] <- "MER"
  # L102.PPP_MER_R$PPP <- L102.gdp_bilusd_SSP_R_Yfut[[X_PPP_MER_year]][match(L102.PPP_MER_R[[R]],
  #                                                                          L102.gdp_bilusd_SSP_R_Yfut[[R]])]
  # L102.PPP_MER_R$PPP_MER <- L102.PPP_MER_R$PPP/L102.PPP_MER_R$MER
  #
  # # ----------------------------------------------------------------------------- 3.
  # # Output
  # writedata(L102.gdp_mil90usd_SSP_R_Y, domain = "SOCIO_LEVEL1_DATA", fn = "L102.gdp_mil90usd_SSP_R_Y",
  #           comments = c("GDP by SSP scenario and GCAM region (including historical time series)",
  #                        "Unit = million 1990 USD"),
  #           readr = TRUE)
  # writedata(L102.pcgdp_thous90USD_SSP_R_Y, domain = "SOCIO_LEVEL1_DATA", fn = "L102.pcgdp_thous90USD_SSP_R_Y",
  #           comments = c("per-capita GDP by SSP scenario and GCAM region (including historical time series)",
  #                        "Unit = thousand 1990 USD / cap"),
  #           readr = TRUE)
  # writedata(L102.gdp_mil90usd_GCAM3_R_Y, domain = "SOCIO_LEVEL1_DATA", fn = "L102.gdp_mil90usd_GCAM3_R_Y",
  #           comments = c("Total GDP from GCAM 3.0 by GCAM region (including historical time series)",
  #                        "Unit = 1990 USD"),
  #           readr = TRUE)
  # writedata(L102.gdp_mil90usd_GCAM3_ctry_Y, domain = "SOCIO_LEVEL1_DATA", fn = "L102.gdp_mil90usd_GCAM3_ctry_Y",
  #           comments = c("Total GDP from GCAM 3.0 by country (including historical time series)",
  #                        "Unit = 1990 USD"),
  #           readr = TRUE)
  # writedata(L102.pcgdp_thous90USD_GCAM3_R_Y, domain = "SOCIO_LEVEL1_DATA", fn = "L102.pcgdp_thous90USD_GCAM3_R_Y",
  #           comments = c("Total GDP from GCAM 3.0 by GCAM region (including historical time series)",
  #                        "Unit = thous 1990 USD / cap"),
  #           readr = TRUE)
  # writedata(L102.pcgdp_thous90USD_GCAM3_ctry_Y, domain = "SOCIO_LEVEL1_DATA", fn = "L102.pcgdp_thous90USD_GCAM3_ctry_Y",
  #           comments = c("Per-capita GDP from GCAM 3.0 by country (including historical time series)",
  #                        "Unit = thous 1990 USD / cap"),
  #           readr = TRUE)
  # writedata(L102.PPP_MER_R, domain = "SOCIO_LEVEL1_DATA", fn = "L102.PPP_MER_R",
  #           comments = c("Conversion from World Bank based GDP MER to SSP based GDP PPP",
  #                        "Unitless"),
  #           readr = TRUE)
  L102.PPP_MER_R <- tibble(x=1)
  return_data(L102.PPP_MER_R)
}


#' module_socioeconomics_L100.GDP_hist
#'
#' Construct the \code{socioeconomics} data structures.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}.
#' @author BBL
#' @export
module_socioeconomics_L101.Population <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("common/iso_GCAM_regID",
             "socioeconomics/GCAM3_population",
             "socioeconomics/GCAM3_GDP",
             "L100.Pop_thous_ctry_Yh",
             "L100.Pop_thous_SSP_ctry_Yfut"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.Pop_thous_R_Yh",
             "L101.Pop_thous_SSP_R_Yfut",
             "L101.Pop_thous_GCAM3_R_Y",
             "L101.Pop_thous_GCAM3_ctry_Y"))
  } else if(command == driver.MAKE) {
    socioeconomics_L101.Population_makedata(...)
  } else {
    stop("Unknown command")
  }
}


#' socioeconomics_L101.Population_makedata
#'
#' @param all_data A named list, holding all data system products so far
#' @return A named list with all \code{socioeconomics_L102.GDP} data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
socioeconomics_L102.GDP_makedata <- function(all_data) {

  iso_GCAM_regID <- get_data(all_data, "iso_GCAM_regID")
  GCAM3_population <- get_data(all_data, "GCAM3_population")
  L100.Pop_thous_ctry_Yh <- get_data(all_data, "L100.Pop_thous_ctry_Yh")
  L100.Pop_thous_SSP_ctry_Yfut <- get_data(all_data, "L100.Pop_thous_SSP_ctry_Yfut")

  # # ----------------------------------------------------------------------------- 2.
  # # Perform computations Historical population by region
  # L100.Pop_thous_ctry_Yh[[R]] <- iso_GCAM_regID[[R]][match(L100.Pop_thous_ctry_Yh$iso, iso_GCAM_regID$iso)]
  # L101.Pop_thous_R_Yh <- aggregate(L100.Pop_thous_ctry_Yh[c(X_Maddison_historical_years,
  #                                                           X_UN_historical_years)], by = as.list(L100.Pop_thous_ctry_Yh[R]), sum)
  #
  # # Future population in the SSP scenarios
  # L100.Pop_thous_SSP_ctry_Yfut[[R]] <- iso_GCAM_regID[[R]][match(L100.Pop_thous_SSP_ctry_Yfut$iso,
  #                                                                iso_GCAM_regID$iso)]
  # L101.Pop_thous_SSP_R_Yfut <- aggregate(L100.Pop_thous_SSP_ctry_Yfut[c(X_future_years)],
  #                                        by = as.list(L100.Pop_thous_SSP_ctry_Yfut[Scen_R]), sum)
  #
  # printlog("Downscaling GCAM 3.0 population to country on the basis of UN historical data and base SSP in future years")
  # # Population by GCAM 3.0 region - downscale to country according to actual shares in
  # # the historical periods, and SSPbase in the future periods
  # L101.Pop_thous_ctry_Y <- L100.Pop_thous_ctry_Yh
  # L101.Pop_thous_ctry_Y[X_future_years] <- L100.Pop_thous_SSP_ctry_Yfut[match(paste(L101.Pop_thous_ctry_Y$iso,
  #                                                                                   base_pop_scen), paste(L100.Pop_thous_SSP_ctry_Yfut$iso, L100.Pop_thous_SSP_ctry_Yfut[[Scen]])),
  #                                                                       X_future_years]
  # L101.Pop_thous_ctry_Y$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[match(L101.Pop_thous_ctry_Y$iso,
  #                                                                         iso_GCAM_regID$iso)]
  # L101.Pop_thous_SSPbase_RG3_Y <- aggregate(L101.Pop_thous_ctry_Y[c(X_historical_years,
  #                                                                   X_future_years)], by = as.list(L101.Pop_thous_ctry_Y["region_GCAM3"]), sum)
  #
  # # Calculate shares of each country within its region over the historical time series
  # L101.Popshares_ctryRG3_Y <- L101.Pop_thous_ctry_Y[c("iso", "region_GCAM3", X_historical_years,
  #                                                     X_future_years)]
  # L101.Popshares_ctryRG3_Y[c(X_historical_years, X_future_years)] <- L101.Pop_thous_ctry_Y[c(X_historical_years,
  #                                                                                            X_future_years)]/L101.Pop_thous_SSPbase_RG3_Y[match(L101.Pop_thous_ctry_Y$region_GCAM3,
  #                                                                                                                                                L101.Pop_thous_SSPbase_RG3_Y$region_GCAM3), c(X_historical_years, X_future_years)]
  #
  # # Interpolate the GCAM population data to all historical and future years
  # L101.Pop_thous_GCAM3_RG3_Y <- gcam_interp(GCAM3_population, c(historical_years, future_years))[c("region_GCAM3",
  #                                                                                                  X_historical_years, X_future_years)]
  #
  # if ("X2100" %in% X_future_years && "X2100" %!in% names(L101.Pop_thous_GCAM3_RG3_Y)) {
  #   printlog("Extending GCAM 3.0 scenario to 2100 using SSPbase population ratios by GCAM 3.0 region")
  #   L101.Pop_thous_GCAM3_RG3_Y$X2100 <- L101.Pop_thous_GCAM3_RG3_Y$X2095 * L101.Pop_thous_SSPbase_RG3_Y$X2100[match(L101.Pop_thous_GCAM3_RG3_Y$region_GCAM3,
  #                                                                                                                   L101.Pop_thous_SSPbase_RG3_Y$region_GCAM3)]/L101.Pop_thous_SSPbase_RG3_Y$X2095[match(L101.Pop_thous_GCAM3_RG3_Y$region_GCAM3,
  #                                                                                                                                                                                                        L101.Pop_thous_SSPbase_RG3_Y$region_GCAM3)]
  # }
  #
  # # Multiply these population numbers by the shares of each country within GCAM region
  # L101.Pop_thous_GCAM3_ctry_Y <- L101.Popshares_ctryRG3_Y
  # L101.Pop_thous_GCAM3_ctry_Y[c(X_historical_years, X_future_years)] <- L101.Popshares_ctryRG3_Y[c(X_historical_years,
  #                                                                                                  X_future_years)] * L101.Pop_thous_GCAM3_RG3_Y[match(L101.Popshares_ctryRG3_Y$region_GCAM3,
  #                                                                                                                                                      L101.Pop_thous_GCAM3_RG3_Y$region_GCAM3), c(X_historical_years, X_future_years)]
  #
  # printlog("Aggregating by GCAM regions")
  # L101.Pop_thous_GCAM3_ctry_Y[[R]] <- iso_GCAM_regID[[R]][match(L101.Pop_thous_GCAM3_ctry_Y$iso,
  #                                                               iso_GCAM_regID$iso)]
  # L101.Pop_thous_GCAM3_R_Y <- aggregate(L101.Pop_thous_GCAM3_ctry_Y[c(X_historical_years,
  #                                                                     X_future_years)], by = as.list(L101.Pop_thous_GCAM3_ctry_Y[R]), sum)
  # L101.Pop_thous_GCAM3_ctry_Y <- L101.Pop_thous_GCAM3_ctry_Y[c("iso", X_historical_years,
  #                                                              X_future_years)]
  #
  # # ----------------------------------------------------------------------------- 3.
  # # Output Add comments to tables
  # comments.L101.Pop_thous_R_Yh <- c("Population by region over the historical time period",
  #                                   "Unit = million persons")
  # comments.L101.Pop_thous_SSP_R_Yfut <- c("Population by region and SSP in future periods",
  #                                         "Unit = million persons")
  # comments.L101.Pop_thous_GCAM3_R_Y <- c("GCAM 3.0 population by region in historical and future years",
  #                                        "Unit = thousand persons")
  # comments.L101.Pop_thous_GCAM3_ctry_Y <- c("GCAM 3.0 population by country in historical and future years",
  #                                           "Unit = thousand persons")


  L101.Pop_thous_R_Yh <-
    L101.Pop_thous_SSP_R_Yfut <-
    L101.Pop_thous_GCAM3_R_Y <-
    L101.Pop_thous_GCAM3_ctry_Y <- tibble(x=1)
  return_data(L101.Pop_thous_R_Yh, L101.Pop_thous_SSP_R_Yfut, L101.Pop_thous_GCAM3_R_Y, L101.Pop_thous_GCAM3_ctry_Y)
}
