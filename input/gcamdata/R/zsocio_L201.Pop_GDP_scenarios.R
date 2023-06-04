# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_socio_L201.Pop_GDP_scenarios
#'
#' Labor productivity and population by scenario and region.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.InterestRate}, \code{L201.LaborForceFillout}, \code{L201.PPPConvert}, \code{L201.BaseGDP_Scen}, \code{L201.Pop_gSSP1}, \code{L201.Pop_gSSP2}, \code{L201.Pop_gSSP3}, \code{L201.Pop_gSSP4}, \code{L201.Pop_gSSP5}, \code{L201.Pop_SSP1}, \code{L201.Pop_SSP2}, \code{L201.Pop_SSP3}, \code{L201.Pop_SSP4}, \code{L201.Pop_SSP5}, \code{L201.TotalFactorProductivity_gSSP1}, \code{L201.TotalFactorProductivity_gSSP2}, \code{L201.TotalFactorProductivity_gSSP3}, \code{L201.TotalFactorProductivity_gSSP4}, \code{L201.TotalFactorProductivity_gSSP5}, \code{L201.TotalFactorProductivity_SSP1}, \code{L201.TotalFactorProductivity_SSP2}, \code{L201.TotalFactorProductivity_SSP3}, \code{L201.TotalFactorProductivity_SSP4}, and \code{L201.TotalFactorProductivity_SSP5}. The corresponding file in the
#' original data system was \code{L201.Pop_GDP_scenarios.R} (socioeconomics level2).
#' @details Produces default interest rate by region, historical and future population by region and SSP scenario,
#' and uses per-capita GDP to calculate labor productivity by region and scenario.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter group_by lag mutate order_by select transmute
#' @author HM & RH June 2017
module_socio_L201.Pop_GDP_scenarios <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "socioeconomics/gcam_macro_TFP_open",
             "L101.Pop_thous_R_Yh",
             "L101.Pop_thous_Scen_R_Yfut",
             "L102.gdp_mil90usd_Scen_R_Y",
             "L102.PPP_MER_R",
             "L101.Pop_thous_GCAM3_R_Y",
             "L102.gdp_mil90usd_GCAM3_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.GDP_Scen",
             "L201.PPPConvert",
             paste0("L201.Pop_gSSP", seq(1, 5)),
             paste0("L201.Pop_SSP", seq(1, 5)),
             paste0("L201.TotalFactorProductivity_gSSP", seq(1, 5)),
             paste0("L201.TotalFactorProductivity_SSP", seq(1, 5)),
             "L201.GDP_GCAM3",
             "L201.TotalFactorProductivity_GCAM3",
             "L201.Pop_GCAM3"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    . <- GCAM_region_ID <- L201.TotalFactorProductivity_SSP1 <- L201.TotalFactorProductivity_SSP2 <-
      L201.TotalFactorProductivity_SSP3 <- L201.TotalFactorProductivity_SSP4 <- L201.TotalFactorProductivity_SSP5 <-
      L201.TotalFactorProductivity_gSSP1 <- L201.TotalFactorProductivity_gSSP2 <- L201.TotalFactorProductivity_gSSP3 <-
      L201.TotalFactorProductivity_gSSP4 <- L201.TotalFactorProductivity_gSSP5 <- L201.Pop_SSP1 <- L201.Pop_SSP2 <-
      L201.Pop_SSP3 <- L201.Pop_SSP4 <- L201.Pop_SSP5 <- L201.Pop_gSSP1 <- L201.Pop_gSSP2 <-
      L201.Pop_gSSP3 <- L201.Pop_gSSP4 <- L201.Pop_gSSP5 <- PPPConvert <- PPP_MER <- baseGDP <-
      constRatio <- curr_table <- lag_pcgdp <- rate_pcgdp <- ratio_pcgdp <- region <- scenario <-
      timesteps <- totalPop <- value <- year <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)
    gcam_macro_TFP_open <- get_data(all_data, "socioeconomics/gcam_macro_TFP_open", strip_attributes = TRUE)
    L101.Pop_thous_R_Yh <- get_data(all_data, "L101.Pop_thous_R_Yh", strip_attributes = TRUE)
    L101.Pop_thous_Scen_R_Yfut <- get_data(all_data, "L101.Pop_thous_Scen_R_Yfut")
    L102.gdp_mil90usd_Scen_R_Y <- get_data(all_data, "L102.gdp_mil90usd_Scen_R_Y", strip_attributes = TRUE)
    L102.PPP_MER_R <- get_data(all_data, "L102.PPP_MER_R", strip_attributes = TRUE)
    L101.Pop_thous_GCAM3_R_Y <- get_data(all_data, "L101.Pop_thous_GCAM3_R_Y", strip_attributes = TRUE)
    L102.gdp_mil90usd_GCAM3_R_Y <- get_data(all_data, "L102.gdp_mil90usd_GCAM3_R_Y", strip_attributes = TRUE)

    gcam_macro_TFP_open %>%
      select(scenario, region, year, productivity) ->
      gcam_macro_TFP_open

    # ===================================================
    # Stitch together history and future population
    # First, repeat hisotry for all scenarios
    L101.Pop_thous_Scen_R_Y <- L101.Pop_thous_R_Yh %>%
      repeat_add_columns(tibble(scenario = unique(L101.Pop_thous_Scen_R_Yfut$scenario))) %>%
      bind_rows(L101.Pop_thous_Scen_R_Yfut) %>% # add future
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(totalPop = round(value, socioeconomics.POP_DIGITS)) %>%
      filter(year %in% MODEL_YEARS) # delete unused years

    # L201.GDP_Scen: GDP for all scenarios
    L201.GDP_Scen <- L102.gdp_mil90usd_Scen_R_Y %>%
      filter(year %in% MODEL_YEARS) %>% # only keep model years
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(GDP = round(value, socioeconomics.GDP_DIGITS)) %>%
      select(scenario, region, year, GDP)

    # Write out the PPP:MER conversion factors (purely used for reporting)
    L201.PPPConvert <- L102.PPP_MER_R %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(PPP.convert = round(PPP_MER, socioeconomics.LABOR_PRODUCTIVITY_DIGITS)) %>%
      select(region, PPP.convert)

    # Assign each tibble in list
    for(i in c(unique(L201.GDP_Scen$scenario), "GCAM3")) {
      gcam_macro_TFP_open %>%
        filter(scenario == i) %>%
        select(-scenario) %>%
        add_title(paste0("Total factor productivity: ", i)) %>%
        add_legacy_name(paste0("L201.TotalFactorProductivity_", i)) %>%
        add_units("Unitless") %>%
        add_comments("Values (if available) are calibrated from prior GCAM runs and therefore") %>%
        add_comments("could potentially no longer match.") %>%
        add_precursors("socioeconomics/gcam_macro_TFP_open") ->
        curr_factor_prod
      assign(paste0("L201.TotalFactorProductivity_", i), curr_factor_prod)
    }

    # Repeat for population outputs
    L101.Pop_thous_Scen_R_Y_split <- L101.Pop_thous_Scen_R_Y %>%
      ungroup() %>%
      split(.$scenario) %>%
      lapply(function(df) {
        select(df, region, year, totalPop) %>%
          add_units("Thousand persons)") %>%
          add_comments("Population by scenario and region") %>%
          add_precursors("common/GCAM_region_names", "L101.Pop_thous_R_Yh", "L101.Pop_thous_Scen_R_Yfut")
      })
    # Assign each tibble in list
    for(i in names(L101.Pop_thous_Scen_R_Y_split)) {
      assign(paste0("L201.Pop_", i), L101.Pop_thous_Scen_R_Y_split[[i]] %>%
               add_title(paste0("Population: ", i)) %>%
               add_legacy_name(paste0("L201.Pop_", i)))
    }

    # L201.Pop_GCAM3: Population by region from the GCAM 3.0 core scenario
    L201.Pop_GCAM3 <- L101.Pop_thous_GCAM3_R_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(value = round(value, socioeconomics.POP_DIGITS)) %>%
      select(region, year, totalPop = value)

    # L201.GDP_GCAM3: GDP for GCAM 3.0 core scenario
    L201.GDP_GCAM3 <- L102.gdp_mil90usd_GCAM3_R_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(GDP = round(value, socioeconomics.GDP_DIGITS)) %>%
      select(region, year, GDP)

    # ===================================================

    # Produce outputs
    L201.PPPConvert %>%
      add_title("Conversion factor from MER to PPP") %>%
      add_units("PPP/MER") %>%
      add_comments("Uses division of PPP by MER performed in L102.PPP_MER_R") %>%
      add_legacy_name("L201.PPPConvert") %>%
      add_precursors("common/GCAM_region_names", "L102.PPP_MER_R") ->
      L201.PPPConvert

    L201.GDP_Scen %>%
      add_title("GDP by scenario, region and year") %>%
      add_units("Million 1990USD") %>%
      add_comments("The base GDP which will used when run in fixed or calibrated GDP modes") %>%
      add_comments("When in open GDP mode the actual GDP in modeled years may differ.") %>%
      add_precursors("common/GCAM_region_names",  "L102.gdp_mil90usd_Scen_R_Y") ->
        L201.GDP_Scen

    L201.GDP_GCAM3 %>%
      add_title("GCAM3 Model GDP by Region and year") %>%
      add_units("Million 1990 USD") %>%
      add_comments("The base GDP which will used when run in fixed or calibrated GDP modes") %>%
      add_comments("When in open GDP mode the actual GDP in modeled years may differ.") %>%
      add_precursors("common/GCAM_region_names",
                     "L102.gdp_mil90usd_GCAM3_R_Y")  ->
      L201.GDP_GCAM3

    L201.Pop_GCAM3 %>%
      add_title("GCAM3 Population") %>%
      add_units("thousand persons") %>%
      add_comments("Filtered years and renamed columns in L101.Pop_thous_GCAM3_R_Y") %>%
      add_legacy_name("L201.Pop_GCAM3") %>%
      add_precursors("common/GCAM_region_names",  "L101.Pop_thous_GCAM3_R_Y") ->
      L201.Pop_GCAM3


    return_data(L201.PPPConvert, L201.GDP_Scen,
                L201.Pop_gSSP1, L201.Pop_gSSP2, L201.Pop_gSSP3, L201.Pop_gSSP4, L201.Pop_gSSP5,
                L201.Pop_SSP1, L201.Pop_SSP2, L201.Pop_SSP3, L201.Pop_SSP4, L201.Pop_SSP5,
                L201.TotalFactorProductivity_gSSP1, L201.TotalFactorProductivity_gSSP2, L201.TotalFactorProductivity_gSSP3, L201.TotalFactorProductivity_gSSP4, L201.TotalFactorProductivity_gSSP5,
                L201.TotalFactorProductivity_SSP1, L201.TotalFactorProductivity_SSP2, L201.TotalFactorProductivity_SSP3, L201.TotalFactorProductivity_SSP4, L201.TotalFactorProductivity_SSP5,
                L201.GDP_GCAM3, L201.TotalFactorProductivity_GCAM3, L201.Pop_GCAM3)
  } else {
    stop("Unknown command")
  }
}
