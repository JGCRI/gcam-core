# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L201.Pop_GDP_scenarios
#'
#' Labor productivity and population by scenario and region.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.GDP_Scen}, \code{L201.Pop_Scen},
#' \code{L201.TotalFactorProductivity_Scen}, \code{L201.PPPConvert}
#' @details Produces default interest rate by region, historical and future population by region and SSP scenario,
#' and uses per-capita GDP to calculate labor productivity by region and scenario.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by lag mutate order_by select transmute
#' @author HM & RH June 2017
module_socio_L201.Pop_GDP_scenarios <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "socioeconomics/gcam_macro_TFP_open",
      "L101.Pop_thous_Scen_R_Y",
      "L102.gdp_mil90usd_Scen_R_Y",
      "L102.PPP_MER_R",
      "L103.LaborForceShare_Scen_R_Y")

  MODULE_OUTPUTS <-
    c("L201.GDP_Scen",
      "L201.Pop_Scen",
      "L201.TotalFactorProductivity_Scen",
      "L201.LaborForceShare_Scen",
      "L201.PPPConvert")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    . <- GCAM_region_ID <-
      L201.TotalFactorProductivity_Scen <-  L201.Pop_Scen <-  PPPConvert <- PPP_MER <- baseGDP <-
      constRatio <- curr_table <- lag_pcgdp <- rate_pcgdp <- ratio_pcgdp <- region <- scenario <-
      timesteps <- totalPop <- value <- year <- NULL  # silence package check notes

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # (1) L201.Pop_Scen: pop for all scenarios ----
    L201.Pop_Scen <-
      L101.Pop_thous_Scen_R_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(totalPop = round(value, socioeconomics.POP_DIGITS)) %>%
      filter(year %in% MODEL_YEARS) %>% # delete unused years
      select(scenario, region, year, totalPop)


    L201.Pop_Scen %>%
      add_title("Population by scenario and region") %>%
      add_units("Thousand persons") %>%
      add_comments("Population by scenario and region") %>%
      add_legacy_name("L201.Pop_Scen") %>%
      add_precursors("common/GCAM_region_names",  "L101.Pop_thous_Scen_R_Y") ->
      L201.Pop_Scen


    # (2) L201.GDP_Scen: GDP for all scenarios ----
    L201.GDP_Scen <-
      L102.gdp_mil90usd_Scen_R_Y %>%
      filter(year %in% MODEL_YEARS) %>% # only keep model years
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(GDP = round(value, socioeconomics.GDP_DIGITS)) %>%
      select(scenario, region, year, GDP)

    L201.GDP_Scen %>%
      add_title("GDP by scenario, region and year") %>%
      add_units("Million 1990USD") %>%
      add_comments("The base GDP which will used when run in fixed or calibrated GDP modes") %>%
      add_comments("When in open GDP mode the actual GDP in modeled years may differ.") %>%
      add_legacy_name("L201.GDP_Scen") %>%
      add_precursors("common/GCAM_region_names",  "L102.gdp_mil90usd_Scen_R_Y") ->
      L201.GDP_Scen


    # (3) L201.TotalFactorProductivity_CORE ----

    gcam_macro_TFP_open %>%
      select(scenario, region, year, productivity) %>%
      filter(!is.na(productivity)) ->
      L201.TotalFactorProductivity_Scen

    L201.TotalFactorProductivity_Scen %>%
      add_title("Total factor productivity by SSP and CORE scenarios") %>%
      add_units("Unitless") %>%
      add_comments("Values (if available) are calibrated from prior GCAM runs and therefore") %>%
      add_comments("could potentially no longer match.") %>%
      add_precursors("socioeconomics/gcam_macro_TFP_open") ->
      L201.TotalFactorProductivity_Scen

    # (4) L201.LaborForceShare_Scen ----

    L103.LaborForceShare_Scen_R_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      transmute(scenario, region, year, labor.force.share = employed.share) ->
      L201.LaborForceShare_Scen

    L201.LaborForceShare_Scen %>%
      add_title("National Accounts data of employment share by SSP and CORE scenarios") %>%
      add_units("share") %>%
      add_comments("share of employment over population derived based on PWT and SSP database") %>%
      add_precursors("common/GCAM_region_names", "L103.LaborForceShare_Scen_R_Y") ->
      L201.LaborForceShare_Scen


    # (5) L201.PPPConvert ----
    # Write out the PPP:MER conversion factors (purely used for reporting)
    L201.PPPConvert <- L102.PPP_MER_R %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      transmute(region,
                PPP.convert = round(PPP_MER, socioeconomics.LABOR_PRODUCTIVITY_DIGITS))

    L201.PPPConvert %>%
      add_title("Conversion factor from MER to PPP") %>%
      add_units("PPP/MER") %>%
      add_comments("Uses division of PPP by MER performed in L102.PPP_MER_R") %>%
      add_legacy_name("L201.PPPConvert") %>%
      add_precursors("common/GCAM_region_names", "L102.PPP_MER_R") ->
      L201.PPPConvert

    return_data(MODULE_OUTPUTS)

  } else {
    stop("Unknown command")
  }
}

