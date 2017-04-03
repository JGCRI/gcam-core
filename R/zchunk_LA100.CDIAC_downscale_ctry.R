#' module_energy_LA100.CDIAC_downscale_ctry
#'
#' Combine and downscale (back in time, for USSR and Yugoslavia) the CDIAC emissions and sequestration data.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.CDIAC_CO2_ctry_hist}. The corresponding file in the
#' original data system was \code{LA100.CDIAC_downscale_ctry.R} (energy level1).
#' @details Combine and downscale (back in time, for USSR and Yugoslavia) the CDIAC emissions and sequestration data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL April 2017
module_energy_LA100.CDIAC_downscale_ctry <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/CDIAC_CO2_by_nation",
             FILE = "energy/CDIAC_Cseq_by_nation",
             FILE = "energy/CDIAC_nation"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.CDIAC_CO2_ctry_hist"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    CDIAC_CO2_by_nation <- get_data(all_data, "energy/CDIAC_CO2_by_nation")
    CDIAC_Cseq_by_nation <- get_data(all_data, "energy/CDIAC_Cseq_by_nation")
    CDIAC_nation <- get_data(all_data, "energy/CDIAC_nation")

    # Merge the sequestration and emissions datasets
    CDIAC_nation %>%
      select(nation, UN_code) %>%
      distinct(UN_code, .keep_all = TRUE) %>%
      left_join_error_no_match(CDIAC_Cseq_by_nation, ., by = "UN_code") %>%
      mutate(liquids.sequestration = abs(liquids.sequestration)) %>%

      # Join with CDIAC_CO2_by_nation
      select(nation, year, liquids.sequestration) %>%
      right_join(CDIAC_CO2_by_nation, by = c("nation", "year")) %>%

      # Zero out NAs and subset to years being processed
      mutate(liquids.sequestration = if_else(is.na(liquids.sequestration), 0, liquids.sequestration)) %>%
      filter(year %in% energy.CDIAC_CO2_HISTORICAL_YEARS ) ->
      L100.CDIAC_CO2_ctry_hist

    # Generate time series of Former Soviet Union by country, using national shares in the first available year
    USSR <- "USSR"
    L100.CO2_ctry_noUSSR_hist <- filter(L100.CDIAC_CO2_ctry_hist, nation != USSR)
    L100.CO2_USSR_hist <- filter(L100.CDIAC_CO2_ctry_hist, nation == USSR)
    USSR_years <- unique(L100.CO2_USSR_hist$year)

    L100.CO2_USSR_hist %>%
      repeat_add_columns(tibble(iso = CDIAC_nation$iso[CDIAC_nation$nation == USSR])) %>%
      gather(category, value, -nation, -year, -iso) ->
      L100.CO2_USSR_hist_repCtry

    CDIAC_nation %>%
      select(nation, iso) %>%
      distinct(nation, .keep_all = TRUE) %>%
      right_join(L100.CO2_ctry_noUSSR_hist, by = "nation") %>%
      na.omit ->
      L100.CO2_ctry_noUSSR_hist

    # For each category, compute shares of countries in first post-USSR year
    L100.CO2_ctry_noUSSR_hist %>%
      filter(iso %in% L100.CO2_USSR_hist_repCtry$iso, year == max(USSR_years) + 1 ) %>%
      gather(category, value, -nation, -iso, -year) %>%
      group_by(category) %>%
      mutate(share = value / sum(value)) %>%
      select(iso, share, category) %>%
      # Use those share values to project USSR totals back in time
      right_join(L100.CO2_USSR_hist_repCtry, by = c("iso", "category")) %>%
      mutate(value = value * share) %>%
      select(-share) %>%
      spread(category, value) ->
      L100.CO2_FSU_hist

    # Repeat: generate time series of Yugoslavia by country, using national shares in the first available year
    YUGOSLAVIA <- "YUGOSLAVIA (FORMER SOCIALIST FEDERAL REPUBLIC)"
    L100.CO2_ctry_noUSSR_Yug_hist <- filter(L100.CO2_ctry_noUSSR_hist, nation != YUGOSLAVIA) %>% na.omit
    L100.CO2_Yug_hist <- filter(L100.CO2_ctry_noUSSR_hist, nation == YUGOSLAVIA)
    Yug_years <- unique(L100.CO2_Yug_hist$year)

    L100.CO2_Yug_hist %>%
      select(-iso) %>%
      repeat_add_columns(tibble(iso = CDIAC_nation$iso[CDIAC_nation$nation == YUGOSLAVIA])) %>%
      gather(category, value, -nation, -year, -iso) ->
      L100.CO2_Yug_hist_repCtry

    # For each category, compute shares of countries in first post-Yugoslavia year
    L100.CO2_ctry_noUSSR_Yug_hist %>%
      filter(iso %in% L100.CO2_Yug_hist_repCtry$iso, year == max(Yug_years) + 1) %>%
      gather(category, value, -nation, -iso, -year) %>%
      group_by(category) %>%
      mutate(share = value / sum(value)) %>%
      select(iso, share, category) %>%
      # Use those share values to project Yugoslavia totals back in time
      right_join(L100.CO2_Yug_hist_repCtry, by = c("iso", "category")) %>%
      mutate(value = value * share) %>%
      select(-share) %>%
      mutate(value = if_else(is.na(value), 0, value)) %>%
      spread(category, value) ->
      L100.CO2_FYug_hist

    # Finish

    L100.CO2_ctry_noUSSR_Yug_hist %>%
      bind_rows(L100.CO2_FSU_hist, L100.CO2_FYug_hist) %>%
      select(-nation) %>%
      arrange(iso, year) %>%

      # Produce output
      add_title("CO2 emissions by country / fuel type / historical year") %>%
      add_units("kt C") %>%
      add_comments("CDIAC sequestration and emissions datasets combined and limited to historical years") %>%
      add_comments("Post-breakup USSR and Yugoslavia numbers used to project back in time") %>%
      add_legacy_name("L100.CDIAC_CO2_ctry_hist") %>%
      add_precursors("energy/CDIAC_CO2_by_nation",
                     "energy/CDIAC_Cseq_by_nation",
                     "energy/CDIAC_nation") ->
      L100.CDIAC_CO2_ctry_hist

    return_data(L100.CDIAC_CO2_ctry_hist)
  } else {
    stop("Unknown command")
  }
}
