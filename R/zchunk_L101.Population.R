#' module_socioeconomics_L101.Population
#'
#' Compute population for historical and future years, by region and SSP.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L101.Pop_thous_R_Yh}, \code{L101.Pop_thous_Scen_R_Yfut}, \code{L101.Pop_thous_GCAM3_R_Y}, \code{L101.Pop_thous_GCAM3_ctry_Y}. The corresponding file in the
#' original data system was \code{L101.Population.R} (socioeconomics level1).
#' @details Interpolates GCAM population data to all historical and future years, aggregating by
#' country and/or region and/or SPP as necessary.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL April 2017
#' @export
module_socioeconomics_L101.Population <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "socioeconomics/GCAM3_population",
             "L100.Pop_thous_ctry_Yh",
             "L100.Pop_thous_SSP_ctry_Yfut"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.Pop_thous_R_Yh",
             "L101.Pop_thous_Scen_R_Yfut",
             "L101.Pop_thous_GCAM3_R_Y",
             "L101.Pop_thous_GCAM3_ctry_Y"))
  } else if(command == driver.MAKE) {

    year <- value <- region_GCAM3 <- GCAM_region_ID <- iso <- scenario <- . <-
        country_name <- value.x <- value.y <- SSPbase2100 <- SSPbase2095 <-
        year.y <- year.x <- NULL        # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    get_data(all_data, "socioeconomics/GCAM3_population") %>%
      gather(year, value, -region_GCAM3) %>%
      mutate(year = as.integer(year)) ->
      GCAM3_population
    L100.Pop_thous_ctry_Yh <- get_data(all_data, "L100.Pop_thous_ctry_Yh")
    L100.Pop_thous_SSP_ctry_Yfut <- get_data(all_data, "L100.Pop_thous_SSP_ctry_Yfut")

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
      gather(year, value, -iso, -GCAM_region_ID, -region_GCAM3) %>%
      mutate(year = as.integer(year)) ->
      L101.Pop_thous_ctry_Y

    L101.Pop_thous_ctry_Y %>%
      filter(year %in% c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      group_by(region_GCAM3, year) %>%
      summarise(value = sum(value)) ->
      L101.Pop_thous_SSPbase_RG3_Y

    # Calculate shares of each country within its region over the historical time series
    L101.Pop_thous_ctry_Y %>%
      select(iso, region_GCAM3, year) %>%
      filter(year %in% c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      left_join_error_no_match(select(L101.Pop_thous_ctry_Y, iso, region_GCAM3, year, value), by = c("iso", "region_GCAM3", "year")) %>%
      left_join_error_no_match(L101.Pop_thous_SSPbase_RG3_Y, by = c("region_GCAM3", "year")) %>%
      mutate(value = value.x / value.y) %>%
      select(-value.x, -value.y) ->
      L101.Popshares_ctryRG3_Y

    # Interpolate the GCAM population data to all historical and future years
    GCAM3_population %>%
      complete(nesting(region_GCAM3), year = c(year, HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      arrange(region_GCAM3, year) %>%
      group_by(region_GCAM3) %>%
      mutate(value = approx_fun(year, value)) %>%
      filter(year %in% c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      ungroup() ->
      L101.Pop_thous_GCAM3_RG3_Y

    # If necessary, extend GCAM 3.0 scenario to 2100 using SSPbase population ratios by GCAM 3.0 region
    # TODO: see issue #234
    if(2100 %in% FUTURE_YEARS & !(2100 %in% L101.Pop_thous_GCAM3_RG3_Y$year)) {
      L101.Pop_thous_GCAM3_RG3_Y %>%
        filter(year == 2095) %>%
        left_join_error_no_match(L101.Pop_thous_SSPbase_RG3_Y, by = c("region_GCAM3", "year")) %>%
        rename(SSPbase2095 = value.y) %>%
        left_join_error_no_match(filter(L101.Pop_thous_SSPbase_RG3_Y, year == 2100), by = c("region_GCAM3")) %>%
        rename(SSPbase2100 = value) %>%
        # scale 2095 L101.Pop_thous_GCAM3_RG3_Y value by 2100/2095 L101.Pop_thous_SSPbase_RG3_Y ratio
        mutate(value = value.x * SSPbase2100 / SSPbase2095) %>%
        rename(year = year.y) %>%
        select(-SSPbase2095, -SSPbase2100, -value.x, -year.x) %>%
        bind_rows(L101.Pop_thous_GCAM3_RG3_Y, .) ->
        L101.Pop_thous_GCAM3_RG3_Y
    }

    # Multiply these population numbers by the shares of each country within GCAM region
    L101.Popshares_ctryRG3_Y %>%
      left_join_error_no_match(L101.Pop_thous_GCAM3_RG3_Y, by = c("region_GCAM3", "year")) %>%
      mutate(value = value.x * value.y, year = as.integer(year)) %>%
      select(-value.x, -value.y) ->
      L101.Pop_thous_GCAM3_ctry_Y

    # Aggregate by GCAM regions
    L101.Pop_thous_GCAM3_ctry_Y %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      L101.Pop_thous_GCAM3_R_Y

    # Produce outputs

    L101.Pop_thous_R_Yh %>%
      add_title("Population by region over the historical time period") %>%
      add_units("thousand persons") %>%
      add_comments("Population by region over the historical time period") %>%
      add_legacy_name("L101.Pop_thous_R_Yh") %>%
      add_precursors("common/iso_GCAM_regID", "socioeconomics/GCAM3_population",
                     "L100.Pop_thous_ctry_Yh", "L100.Pop_thous_SSP_ctry_Yfut") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.Pop_thous_R_Yh

    L101.Pop_thous_Scen_R_Yfut %>%
      add_title("Population by region and gSSP SSP in future periods") %>%
      add_units("thousand persons") %>%
      add_comments("Population by region and gSSP SSP in future periods") %>%
      add_legacy_name("L101.Pop_thous_Scen_R_Yfut") %>%
      add_precursors("common/iso_GCAM_regID", "socioeconomics/GCAM3_population",
                     "L100.Pop_thous_ctry_Yh", "L100.Pop_thous_SSP_ctry_Yfut") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR, FLAG_SUM_TEST) ->
      L101.Pop_thous_Scen_R_Yfut

    L101.Pop_thous_GCAM3_R_Y %>%
      add_title("GCAM 3.0 population by region in historical and future years") %>%
      add_units("thousand persons") %>%
      add_comments("GCAM population data interpolated to all historical and future years") %>%
      add_legacy_name("L101.Pop_thous_GCAM3_R_Y") %>%
      add_precursors("common/iso_GCAM_regID", "socioeconomics/GCAM3_population",
                     "L100.Pop_thous_ctry_Yh", "L100.Pop_thous_SSP_ctry_Yfut") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.Pop_thous_GCAM3_R_Y

    L101.Pop_thous_GCAM3_ctry_Y %>%
      select(iso, year, value) %>%
      filter(year %in% c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      add_title("GCAM 3.0 population by country in historical and future years") %>%
      add_units("thousand persons") %>%
      add_comments("GCAM population data interpolated to all historical and future years") %>%
      add_legacy_name("L101.Pop_thous_GCAM3_ctry_Y") %>%
      add_precursors("common/iso_GCAM_regID", "socioeconomics/GCAM3_population",
                     "L100.Pop_thous_ctry_Yh", "L100.Pop_thous_SSP_ctry_Yfut") %>%
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L101.Pop_thous_GCAM3_ctry_Y

    return_data(L101.Pop_thous_R_Yh, L101.Pop_thous_Scen_R_Yfut, L101.Pop_thous_GCAM3_R_Y, L101.Pop_thous_GCAM3_ctry_Y)
  } else {
    stop("Unknown command")
  }
}
