#' module_socioeconomics_L101.Population
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L101.Pop_thous_R_Yh}, \code{L101.Pop_thous_Scen_R_Yfut}, \code{L101.Pop_thous_GCAM3_R_Y}, \code{L101.Pop_thous_GCAM3_ctry_Y}. The corresponding file in the
#' original data system was \code{L101.Population.R} (socioeconomics level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL April 2017
#' @export
module_socioeconomics_L101.Population <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "socioeconomics/GCAM3_population",
             FILE = "temp-data-inject/L100.Pop_thous_ctry_Yh",
             FILE = "temp-data-inject/L100.Pop_thous_SSP_ctry_Yfut"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.Pop_thous_R_Yh",
             "L101.Pop_thous_Scen_R_Yfut",
             "L101.Pop_thous_GCAM3_R_Y",
             "L101.Pop_thous_GCAM3_ctry_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    GCAM3_population <- get_data(all_data, "socioeconomics/GCAM3_population")
    get_data(all_data, "temp-data-inject/L100.Pop_thous_ctry_Yh") %>%
      # temporary
      gather(year, value, -iso) %>% mutate(year = as.integer(substr(year, 2, 5))) ->
      L100.Pop_thous_ctry_Yh
    get_data(all_data, "temp-data-inject/L100.Pop_thous_SSP_ctry_Yfut") %>%
      # temporary
      gather(year, value, -scenario, -iso) %>% mutate(year = as.integer(substr(year, 2, 5))) ->
      L100.Pop_thous_SSP_ctry_Yfut

    # Historical population by region
    L100.Pop_thous_ctry_Yh %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") ->
      L100.Pop_thous_ctry_Yh

    # Sum by GCAM region and year, for UN and Maddison historical years
    L100.Pop_thous_ctry_Yh %>%
      filter(year %in% c(socioeconomics.UN_HISTORICAL_YEARS, socioeconomics.MADDISON_HISTORICAL_YEARS)) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value)) ->
      L101.Pop_thous_R_Yh

    # Future population in the SSP scenarios
    L100.Pop_thous_SSP_ctry_Yfut %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") ->
      L100.Pop_thous_SSP_ctry_Yfut

    # Sum by scenario and GCAM region and year, for future years
    L100.Pop_thous_SSP_ctry_Yfut %>%
      filter(year %in% c(FUTURE_YEARS)) %>%
      group_by(scenario, GCAM_region_ID, year) %>%
      summarise(value = sum(value)) ->
      L101.Pop_thous_SSP_R_Yfut

    # Future population in the GCAM-SSP (paP) scenarios
    # For now use SSP population for both SSP and gSSP; revisit this after consulting GCAM-China team
    L101.Pop_thous_SSP_R_Yfut %>%
      ungroup %>%
      mutate(scenario = paste0("g", substr(scenario, 1, 4))) %>%
      bind_rows(L101.Pop_thous_SSP_R_Yfut) ->
      L101.Pop_thous_Scen_R_Yfut


    # Downscale GCAM 3.0 population to country on the basis of UN historical data and base SSP in future years
    # This is done according to actual shares in the historical periods, and SSPbase in the future periods
    L100.Pop_thous_SSP_ctry_Yfut %>%
      filter(scenario == socioeconomics.BASE_POP_SCEN) %>%
      select(scenario, iso, year, value) %>%
      spread(year, value) %>%
      left_join_error_no_match(spread(L100.Pop_thous_ctry_Yh, year, value), ., by = c("iso")) %>%
      select(-country_name, -scenario, -region_GCAM3) %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, region_GCAM3), by = "iso") %>%
      gather(year, value, -iso, -GCAM_region_ID, -region_GCAM3) ->
      L101.Pop_thous_ctry_Y

    L101.Pop_thous_ctry_Y %>%
      filter(year %in% c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      group_by(region_GCAM3, year) %>%
      summarise(value = sum(value)) ->
      L101.Pop_thous_SSPbase_RG3_Y

    # Calculate shares of each country within its region over the historical time series
    L101.Pop_thous_ctry_Y %>%
      select(iso, region_GCAM3, year, value) %>%
      filter(year %in% c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%


      L101.Popshares_ctryRG3_Y

    L101.Popshares_ctryRG3_Y[ c( X_historical_years, X_future_years ) ] <-
      L101.Pop_thous_ctry_Y[ c( X_historical_years, X_future_years ) ] / L101.Pop_thous_SSPbase_RG3_Y[
        match( L101.Pop_thous_ctry_Y$region_GCAM3, L101.Pop_thous_SSPbase_RG3_Y$region_GCAM3 ),
        c( X_historical_years, X_future_years ) ]



    # Produce outputs
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L101.Pop_thous_R_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "socioeconomics/GCAM3_population",
                     "temp-data-inject/L100.Pop_thous_ctry_Yh", "temp-data-inject/L100.Pop_thous_SSP_ctry_Yfut") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.Pop_thous_R_Yh
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L101.Pop_thous_Scen_R_Yfut") %>%
      add_precursors("common/iso_GCAM_regID", "socioeconomics/GCAM3_population",
                     "temp-data-inject/L100.Pop_thous_ctry_Yh", "temp-data-inject/L100.Pop_thous_SSP_ctry_Yfut") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.Pop_thous_Scen_R_Yfut
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L101.Pop_thous_GCAM3_R_Y") %>%
      add_precursors("common/iso_GCAM_regID", "socioeconomics/GCAM3_population",
                     "temp-data-inject/L100.Pop_thous_ctry_Yh", "temp-data-inject/L100.Pop_thous_SSP_ctry_Yfut") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.Pop_thous_GCAM3_R_Y
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L101.Pop_thous_GCAM3_ctry_Y") %>%
      add_precursors("common/iso_GCAM_regID", "socioeconomics/GCAM3_population",
                     "temp-data-inject/L100.Pop_thous_ctry_Yh", "temp-data-inject/L100.Pop_thous_SSP_ctry_Yfut") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.Pop_thous_GCAM3_ctry_Y

    return_data(L101.Pop_thous_R_Yh, L101.Pop_thous_Scen_R_Yfut, L101.Pop_thous_GCAM3_R_Y, L101.Pop_thous_GCAM3_ctry_Y)
  } else {
    stop("Unknown command")
  }
}
