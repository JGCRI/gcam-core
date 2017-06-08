#' module_socioeconomics_L201.Pop_GDP_scenarios
#'
#' Briefly describe what this chunk does.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.InterestRate}, \code{L201.Pop_GCAM3}, \code{L201.BaseGDP_GCAM3}, \code{L201.LaborForceFillout}, \code{L201.LaborProductivity_GCAM3}, \code{L201.PPPConvert}, \code{object}, \code{L201.BaseGDP_Scen}, \code{L201.LaborForceFillout}, \code{object2}, \code{L201.PPPConvert}. The corresponding file in the
#' original data system was \code{L201.Pop_GDP_scenarios.R} (socioeconomics level2).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author HM&RH June 2017
#' @export
module_socioeconomics_L201.Pop_GDP_scenarios <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             "L101.Pop_thous_GCAM3_R_Y",
             "L101.Pop_thous_R_Yh",
             "L101.Pop_thous_Scen_R_Yfut",
             FILE = "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y",
             "L102.gdp_mil90usd_Scen_R_Y",
             "L102.pcgdp_thous90USD_Scen_R_Y",
             "L102.PPP_MER_R"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.InterestRate",
             "L201.Pop_GCAM3",
             "L201.BaseGDP_GCAM3",
             "L201.LaborForceFillout",
             "L201.LaborProductivity_GCAM3",
             "L201.PPPConvert",
             "object",
             "L201.BaseGDP_Scen",
             "object2"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    L101.Pop_thous_GCAM3_R_Y <- get_data(all_data, "L101.Pop_thous_GCAM3_R_Y")
    L101.Pop_thous_R_Yh <- get_data(all_data, "L101.Pop_thous_R_Yh")
    L101.Pop_thous_Scen_R_Yfut <- get_data(all_data, "L101.Pop_thous_Scen_R_Yfut")
    L102.gdp_mil90usd_GCAM3_R_Y <- get_data(all_data, "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y")
    L102.gdp_mil90usd_Scen_R_Y <- get_data(all_data, "L102.gdp_mil90usd_Scen_R_Y") %>%
      mutate(year = as.integer(year)) %>%
      ungroup()
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y") %>%
      ungroup
    L102.PPP_MER_R <- get_data(all_data, "L102.PPP_MER_R")

    # ===================================================
    # Set default interest rate for all regions
    L201.InterestRate <- GCAM_region_names %>%
      select(region) %>%
      mutate(interest.rate = socioeconomics.DEFAULT_INTEREST_RATE)

    # Stitch together history and future population
    # First, repeat hisotry for all scenarios
    L101.Pop_thous_Scen_R_Y <- L101.Pop_thous_R_Yh %>%
      repeat_add_columns(tibble(scenario = unique(L101.Pop_thous_Scen_R_Yfut$scenario))) %>%
      bind_rows(L101.Pop_thous_Scen_R_Yfut) %>% # add future
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(year %in% c(HISTORICAL_YEARS, FUTURE_YEARS)) # delete unused years

    # L201.BaseGDP_Scen: Base GDP for all Scen scenarios
    # Get base GDP in start year
    L201.BaseGDP_Scen <- L102.gdp_mil90usd_Scen_R_Y %>%
      filter(scenario == BASE_GDP_SCENARIO) %>% # use the standard scenario
      filter(year == min(BASE_YEARS)) %>% # find the first year
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(value = round(value, socioeconomics.GDP_DIGITS)) %>%
      select(region, value)

    # L201.LaborForceFillout: Labor force participation and productivity for all scenarios
    # NOTE: No model of labor force used; labor force participation set to a constant
    # Simply fill out default rate
    L201.LaborForceFillout <- GCAM_region_names %>%
      select(region) %>%
      mutate(laborforce = socioeconomics.DEFAULT_LABORFORCE,
             year.fillout = min(BASE_YEARS))

    # Labor productivity growth is calculated from the change in per-capita GDP ratio in each time period
    L201.pcgdpGrowth_Scen_R_Y <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(year %in% MODEL_YEARS) %>%
      group_by(scenario, GCAM_region_ID) %>%
      mutate(timesteps = year - lag(year, n = 1L, order_by = c(GCAM_region_ID))) %>% # calculate time step
      mutate(lag_pcgdp = lag(value, n = 1L, order_by = c(GCAM_region_ID))) %>% # last period pcgdp
      mutate(ratio_pcgdp = value / lag_pcgdp) %>% # this year, last year ratio
      filter(year != min(BASE_YEARS)) %>% # drop first period with NA ratio
      mutate(rate_pcgdp = round(ratio_pcgdp ^ (1 / timesteps) - 1, socioeconomics.LABOR_PRODUCTIVITY_DIGITS)) %>% # Annualize the ratios to return annual growth rates
      select(-value, -lag_pcgdp, - ratio_pcgdp)
    # ===================================================

    # Produce outputs
    # Temporary code below sends back empty data frames marked "don't test"
    # Note that all precursor names (in `add_precursor`) must be in this chunk's inputs
    # There's also a `same_precursors_as(x)` you can use
    # If no precursors (very rare) don't call `add_precursor` at all
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.InterestRate") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_GCAM3_R_Y", "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_Scen_R_Yfut", "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "L102.gdp_mil90usd_Scen_R_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L102.PPP_MER_R") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.InterestRate
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.Pop_GCAM3") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_GCAM3_R_Y", "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_Scen_R_Yfut", "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "L102.gdp_mil90usd_Scen_R_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L102.PPP_MER_R") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.Pop_GCAM3
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.BaseGDP_GCAM3") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_GCAM3_R_Y", "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_Scen_R_Yfut", "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "L102.gdp_mil90usd_Scen_R_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L102.PPP_MER_R") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.BaseGDP_GCAM3
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.LaborForceFillout") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_GCAM3_R_Y", "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_Scen_R_Yfut", "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "L102.gdp_mil90usd_Scen_R_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L102.PPP_MER_R") %>%      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.LaborForceFillout
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.LaborProductivity_GCAM3") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_GCAM3_R_Y", "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_Scen_R_Yfut", "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "L102.gdp_mil90usd_Scen_R_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L102.PPP_MER_R") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.LaborProductivity_GCAM3
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.PPPConvert") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_GCAM3_R_Y", "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_Scen_R_Yfut", "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "L102.gdp_mil90usd_Scen_R_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L102.PPP_MER_R") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.PPPConvert
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("object") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_GCAM3_R_Y", "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_Scen_R_Yfut", "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "L102.gdp_mil90usd_Scen_R_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L102.PPP_MER_R") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      object
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L201.BaseGDP_Scen") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_GCAM3_R_Y", "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_Scen_R_Yfut", "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "L102.gdp_mil90usd_Scen_R_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L102.PPP_MER_R") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      L201.BaseGDP_Scen
    tibble() %>%
      add_title("descriptive title of data") %>%
      add_units("units") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("object2") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_GCAM3_R_Y", "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_Scen_R_Yfut", "temp-data-inject/L102.gdp_mil90usd_GCAM3_R_Y", "L102.gdp_mil90usd_Scen_R_Y",
                     "L102.pcgdp_thous90USD_Scen_R_Y", "L102.PPP_MER_R") %>%
      # typical flags, but there are others--see `constants.R`
      add_flags(FLAG_LONG_YEAR_FORM, FLAG_NO_XYEAR) ->
      object2


    return_data(L201.InterestRate, L201.Pop_GCAM3, L201.BaseGDP_GCAM3, L201.LaborForceFillout, L201.LaborProductivity_GCAM3, L201.PPPConvert, object, L201.BaseGDP_Scen, object2)
  } else {
    stop("Unknown command")
  }
}
