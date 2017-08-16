#' module_socioeconomics_L201.Pop_GDP_scenarios
#'
#' Labor productivity and population by scenario and region.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.InterestRate}, \code{L201.LaborForceFillout}, \code{L201.PPPConvert}, \code{L201.BaseGDP_Scen}, \code{L201.Pop_gSSP1}, \code{L201.Pop_gSSP2}, \code{L201.Pop_gSSP3}, \code{L201.Pop_gSSP4}, \code{L201.Pop_gSSP5}, \code{L201.Pop_SSP1}, \code{L201.Pop_SSP2}, \code{L201.Pop_SSP3}, \code{L201.Pop_SSP4}, \code{L201.Pop_SSP5}, \code{L201.LaborProductivity_gSSP1}, \code{L201.LaborProductivity_gSSP2}, \code{L201.LaborProductivity_gSSP3}, \code{L201.LaborProductivity_gSSP4}, \code{L201.LaborProductivity_gSSP5}, \code{L201.LaborProductivity_SSP1}, \code{L201.LaborProductivity_SSP2}, \code{L201.LaborProductivity_SSP3}, \code{L201.LaborProductivity_SSP4}, and \code{L201.LaborProductivity_SSP5}. The corresponding file in the
#' original data system was \code{L201.Pop_GDP_scenarios.R} (socioeconomics level2).
#' @details Produces default interest rate by region, historical and future population by region and SSP scenario,
#' and uses per-capita GDP to calculate labor productivity by region and scenario.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author HM & RH June 2017
#' @export
module_socioeconomics_L201.Pop_GDP_scenarios <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             "L101.Pop_thous_R_Yh",
             "L101.Pop_thous_Scen_R_Yfut",
             "L102.gdp_mil90usd_Scen_R_Y",
             "L102.pcgdp_thous90USD_Scen_R_Y",
             "L102.PPP_MER_R",
             "L101.Pop_thous_GCAM3_R_Y",
             "L102.gdp_mil90usd_GCAM3_R_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.InterestRate",
             "L201.BaseGDP_Scen",
             "L201.LaborForceFillout",
             "L201.PPPConvert",
             paste0("L201.Pop_gSSP", seq(1, 5)),
             paste0("L201.Pop_SSP", seq(1, 5)),
             paste0("L201.LaborProductivity_gSSP", seq(1, 5)),
             paste0("L201.LaborProductivity_SSP", seq(1, 5)),
             "L201.BaseGDP_GCAM3",
             "L201.LaborProductivity_GCAM3",
             "L201.Pop_GCAM3"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    . <- GCAM_region_ID <- L201.LaborProductivity_SSP1 <- L201.LaborProductivity_SSP2 <-
      L201.LaborProductivity_SSP3 <- L201.LaborProductivity_SSP4 <- L201.LaborProductivity_SSP5 <-
      L201.LaborProductivity_gSSP1 <- L201.LaborProductivity_gSSP2 <- L201.LaborProductivity_gSSP3 <-
      L201.LaborProductivity_gSSP4 <- L201.LaborProductivity_gSSP5 <- L201.Pop_SSP1 <- L201.Pop_SSP2 <-
      L201.Pop_SSP3 <- L201.Pop_SSP4 <- L201.Pop_SSP5 <- L201.Pop_gSSP1 <- L201.Pop_gSSP2 <-
      L201.Pop_gSSP3 <- L201.Pop_gSSP4 <- L201.Pop_gSSP5 <- PPPConvert <- PPP_MER <- baseGDP <-
      constRatio <- curr_table <- lag_pcgdp <- rate_pcgdp <- ratio_pcgdp <- region <- scenario <-
      timesteps <- totalPop <- value <- year <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    L101.Pop_thous_R_Yh <- get_data(all_data, "L101.Pop_thous_R_Yh")
    L101.Pop_thous_Scen_R_Yfut <- get_data(all_data, "L101.Pop_thous_Scen_R_Yfut")
    L102.gdp_mil90usd_Scen_R_Y <- get_data(all_data, "L102.gdp_mil90usd_Scen_R_Y")
    L102.pcgdp_thous90USD_Scen_R_Y <- get_data(all_data, "L102.pcgdp_thous90USD_Scen_R_Y")
    L102.PPP_MER_R <- get_data(all_data, "L102.PPP_MER_R")
    L101.Pop_thous_GCAM3_R_Y <- get_data(all_data, "L101.Pop_thous_GCAM3_R_Y")
    L102.gdp_mil90usd_GCAM3_R_Y <- get_data(all_data,"L102.gdp_mil90usd_GCAM3_R_Y")

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
      mutate(totalPop = round(value, socioeconomics.POP_DIGITS)) %>%
      filter(year %in% MODEL_YEARS) # delete unused years

    # L201.BaseGDP_Scen: Base GDP for all scenarios
    # Get base GDP in start year
    L201.BaseGDP_Scen <- L102.gdp_mil90usd_Scen_R_Y %>%
      filter(scenario == BASE_GDP_SCENARIO) %>% # use the standard scenario
      filter(year == min(BASE_YEARS)) %>% # find the first year
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(baseGDP = round(value, socioeconomics.GDP_DIGITS)) %>%
      select(region, baseGDP)

    # L201.LaborForceFillout: Labor force participation and productivity for all scenarios
    # NOTE: No model of labor force used; labor force participation set to a constant
    # Simply fill out default rate
    L201.LaborForceFillout <- GCAM_region_names %>%
      select(region) %>%
      mutate(year.fillout = min(BASE_YEARS),
             laborforce = socioeconomics.DEFAULT_LABORFORCE)

    # Labor productivity growth is calculated from the change in per-capita GDP ratio in each time period
    L201.pcgdpGrowth_Scen_R_Y <- L102.pcgdp_thous90USD_Scen_R_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(year %in% MODEL_YEARS) %>%
      group_by(scenario, GCAM_region_ID) %>%
      mutate(timesteps = year - lag(year, n = 1L, order_by = c(GCAM_region_ID))) %>% # calculate time step
      mutate(lag_pcgdp = lag(value, n = 1L, order_by = c(GCAM_region_ID))) %>% # last period pcgdp
      mutate(ratio_pcgdp = value / lag_pcgdp) %>% # ratio of this year to last year
      filter(year != min(BASE_YEARS)) %>% # drop first period with NA ratio
      mutate(rate_pcgdp = round(ratio_pcgdp ^ (1 / timesteps) - 1, socioeconomics.LABOR_PRODUCTIVITY_DIGITS)) %>% # Annualize the ratios to return annual growth rates
      ungroup() %>%
      select(scenario, region, year, laborproductivity = rate_pcgdp)

    # Write out the PPP:MER conversion factors (purely used for reporting)
    # Define local constant: default is to keep PPP ratio constant
    constantPPPratio <- 1

    L201.PPPConvert <- L102.PPP_MER_R %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(constRatio = constantPPPratio) %>%
      mutate(PPPConvert = round(PPP_MER, socioeconomics.LABOR_PRODUCTIVITY_DIGITS)) %>%
      select(region, constRatio, PPPConvert)

    # Split by scenario and remove scenario column from each tibble
    L201.pcgdpGrowth_Scen_R_Y_split <- L201.pcgdpGrowth_Scen_R_Y %>%
      split(.$scenario) %>%
      lapply(function(df) {
        select(df, -scenario) %>%
          add_units("Unitless (annual rate of growth)") %>%
          add_comments("Per capita GDP growth rate is used for labor productivity growth rate, by scenario") %>%
          add_precursors("common/GCAM_region_names", "L102.pcgdp_thous90USD_Scen_R_Y")
      })
    # Assign each tibble in list
    for(i in names(L201.pcgdpGrowth_Scen_R_Y_split)) {
      assign(paste0("L201.LaborProductivity_", i), L201.pcgdpGrowth_Scen_R_Y_split[[i]] %>%
               add_title(paste0("Labor productivity: ", i)) %>%
               add_legacy_name(paste0("L201.LaborProductivity_", i)))
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
      select(region, year, totalPop = value)

    # L201.BaseGDP_GCAM3: Base GDP for GCAM 3.0 core scenario
    L201.BaseGDP_GCAM3 <- L102.gdp_mil90usd_GCAM3_R_Y %>%
      filter(year == MODEL_YEARS[1]) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(value = round(value, socioeconomics.GDP_DIGITS)) %>%
      select(region, baseGDP = value)

    # Labor productivity growth is calculated from the change in per-capita GDP ratio in each time period
    # For the GCAM 3.0 scenario, calculate the per-capita GDP
    L201.pcgdp_GCAM3_R_Y <- L102.gdp_mil90usd_GCAM3_R_Y %>%
      filter(year %in% MODEL_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join_error_no_match(L201.Pop_GCAM3, by = c("region", "year")) %>%
      transmute(region, year, value = value / totalPop) %>%
      group_by(region) %>%
      # Calculate the growth rate in per-capita GDP
      mutate(timesteps = year - lag(year),
             ratio_pcgdp = value / lag(value)) %>%
      ungroup() %>%
      # drop first period with NA ratio
      filter(year != min(BASE_YEARS)) %>%
      # Annualize the ratios to return annual growth rates
      mutate(rate_pcgdp = round(ratio_pcgdp ^ (1 / timesteps) - 1, socioeconomics.LABOR_PRODUCTIVITY_DIGITS)) %>%
      select(region, year, laborproductivity = rate_pcgdp)


    # ===================================================

    # Produce outputs
    L201.InterestRate %>%
      add_title("Interest Rate by region") %>%
      add_units("Unitless") %>%
      add_comments("Default interest rate applied to all regions") %>%
      add_legacy_name("L201.InterestRate") %>%
      add_precursors("common/GCAM_region_names") ->
      L201.InterestRate
    L201.LaborForceFillout %>%
      add_title("Labor force participation and productivity for all scenarios") %>%
      add_units("Unitless") %>%
      add_comments("Constant used for all regions") %>%
      add_legacy_name("L201.LaborForceFillout") %>%
      add_precursors("common/GCAM_region_names")  ->
      L201.LaborForceFillout
    L201.PPPConvert %>%
      add_title("Conversion factor from MER to PPP") %>%
      add_units("PPP/MER") %>%
      add_comments("Uses division of PPP by MER performed in L102.PPP_MER_R") %>%
      add_legacy_name("L201.PPPConvert") %>%
      add_precursors("common/GCAM_region_names", "L102.PPP_MER_R") ->
      L201.PPPConvert
    L201.BaseGDP_Scen %>%
      add_title("GDP in base scenario and year") %>%
      add_units("Million 1990USD") %>%
      add_comments(paste("Base scenario is", BASE_GDP_SCENARIO)) %>%
      add_comments(paste("Base year is", min(BASE_YEARS))) %>%
      add_legacy_name("L201.BaseGDP_Scen") %>%
      add_precursors("common/GCAM_region_names",  "L102.gdp_mil90usd_Scen_R_Y") ->
      L201.BaseGDP_Scen

    L201.BaseGDP_GCAM3 %>%
      add_title("GCAM3 Model Base Year GDP by Region") %>%
      add_units("Million 1990 USD") %>%
      add_comments("Filtered years, rounded values, and renamed columns in L102.gdp_mil90usd_GCAM3_R_Y") %>%
      add_legacy_name("L201.BaseGDP_GCAM3") %>%
      add_precursors("common/GCAM_region_names",
                     "L102.gdp_mil90usd_GCAM3_R_Y")  ->
      L201.BaseGDP_GCAM3

    L201.LaborProductivity_GCAM3 %>%
      add_title("GCAM3 Labor Productivity") %>%
      add_units("Unitless (annual rate of growth)") %>%
      add_comments("Per capita GDP growth rate is used for labor productivity growth rate") %>%
      add_legacy_name("L201.LaborProductivity_GCAM3") %>%
      add_precursors("common/GCAM_region_names", "L101.Pop_thous_GCAM3_R_Y", "L102.gdp_mil90usd_GCAM3_R_Y") ->
      L201.LaborProductivity_GCAM3

    L201.Pop_GCAM3 %>%
      add_title("GCAM3 Population") %>%
      add_units("thousand persons") %>%
      add_comments("Filtered years and renamed columns in L101.Pop_thous_GCAM3_R_Y") %>%
      add_legacy_name("L201.Pop_GCAM3") %>%
      add_precursors("common/GCAM_region_names",  "L101.Pop_thous_GCAM3_R_Y") ->
      L201.Pop_GCAM3


    return_data(L201.InterestRate, L201.LaborForceFillout, L201.PPPConvert, L201.BaseGDP_Scen,
                L201.Pop_gSSP1, L201.Pop_gSSP2, L201.Pop_gSSP3, L201.Pop_gSSP4, L201.Pop_gSSP5,
                L201.Pop_SSP1, L201.Pop_SSP2, L201.Pop_SSP3, L201.Pop_SSP4, L201.Pop_SSP5,
                L201.LaborProductivity_gSSP1, L201.LaborProductivity_gSSP2, L201.LaborProductivity_gSSP3, L201.LaborProductivity_gSSP4, L201.LaborProductivity_gSSP5,
                L201.LaborProductivity_SSP1, L201.LaborProductivity_SSP2, L201.LaborProductivity_SSP3, L201.LaborProductivity_SSP4, L201.LaborProductivity_SSP5,
                L201.BaseGDP_GCAM3, L201.LaborProductivity_GCAM3, L201.Pop_GCAM3)
  } else {
    stop("Unknown command")
  }
}
