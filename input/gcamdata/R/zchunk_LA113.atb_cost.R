# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA113.atb_cost
#'
#' Generates power sector cost input files based on 2019 ATB data.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L113.globaltech_capital_ATB}, \code{L113.globaltech_capital_ATB_adv},
#' \code{L113.globaltech_capital_ATB_low}, \code{L113.elecS_globaltech_capital_battery_ATB},
#' \code{L113.globaltech_OMfixed_ATB}, \code{L113.globaltech_OMvar_ATB}.
#' There was no corresponding file in the original data system.
#' @details Includes ATB capital cost data as starting point, improvement rate and improvement max data generated based on cost pathway.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select pull left_join anti_join bind_rows arrange rename
#' @author AJS March 2019
module_energy_LA113.atb_cost <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A23.globaltech_capital",
             FILE = "energy/A23.globaltech_capital_adv",
             FILE = "energy/A23.globaltech_capital_low",
             FILE = "energy/A23.globaltech_OMfixed",
             FILE = "energy/A23.globaltech_OMvar",
             FILE = "gcam-usa/A23.elecS_globaltech_non_energy_inputs",
             FILE = "energy/Muratori_globaltech_capital",
             FILE = "energy/Muratori_globaltech_capital_adv",
             FILE = "energy/Muratori_globaltech_capital_low",
             FILE = "energy/Muratori_globaltech_OMfixed",
             FILE = "energy/Muratori_globaltech_OMvar",
             FILE = "energy/NREL_ATB_capital",
             FILE = "energy/NREL_ATB_OMfixed",
             FILE = "energy/NREL_ATB_OMvar",
             FILE = "energy/mappings/atb_gcam_mapping"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L113.globaltech_capital_ATB",
             "L113.globaltech_capital_ATB_adv",
             "L113.globaltech_capital_ATB_low",
             "L113.elecS_globaltech_capital_battery_ATB",
             "L113.globaltech_OMfixed_ATB",
             "L113.globaltech_OMvar_ATB"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence global package checks
    supplysector <- subsector <- technology <- minicam.energy.input <- improvement.max <- improvement.rate <-
      improvement.shadow.technology <- `input-capital` <- fixed.charge.rate <- period <- lifetime <-
      steepness <- half.life <- variable.om <- fixed.om <- fcr <- capacity.factor <- atb.technology <-
      `OM-fixed` <- `OM-var` <- capital <- capital.cost <- case  <- conversion <- cost_end_year <- cost_ratio <-
      final_ATB_cost <- improvement.rate.base <- inferred <- initial_ATB_cost <- input <- input.OM.fixed <-
      input.OM.var <- long_term_improvement <- mid_ATB_cost <- shadow_tech <- shadow_tech_cost <-
      target_ATB_cost <- tech_detail <- tech_type <- uniroot <- value <- year <- NULL

    # Load required inputs
    # A23. shell files
    A23.globaltech_capital <- get_data(all_data, "energy/A23.globaltech_capital", strip_attributes = TRUE)
    A23.globaltech_capital_adv <- get_data(all_data, "energy/A23.globaltech_capital_adv", strip_attributes = TRUE)
    A23.globaltech_capital_low <- get_data(all_data, "energy/A23.globaltech_capital_low", strip_attributes = TRUE)
    A23.elecS_globaltech_non_energy_inputs <- get_data(all_data, "gcam-usa/A23.elecS_globaltech_non_energy_inputs",
                                                       strip_attributes = TRUE)
    A23.globaltech_OMfixed <- get_data(all_data, "energy/A23.globaltech_OMfixed", strip_attributes = TRUE)
    A23.globaltech_OMvar <- get_data(all_data, "energy/A23.globaltech_OMvar", strip_attributes = TRUE)

    # Legacy (Muratori) assumptions
    Muratori_globaltech_capital <- get_data(all_data, "energy/Muratori_globaltech_capital")
    Muratori_globaltech_capital_adv <- get_data(all_data, "energy/Muratori_globaltech_capital_adv")
    Muratori_globaltech_capital_low <- get_data(all_data, "energy/Muratori_globaltech_capital_low")
    Muratori_globaltech_OMfixed <- get_data(all_data, "energy/Muratori_globaltech_OMfixed")
    Muratori_globaltech_OMvar <- get_data(all_data, "energy/Muratori_globaltech_OMvar")

    # NREL files
    NREL_ATB_capital <- get_data(all_data, "energy/NREL_ATB_capital") %>%
      gather_years()
    NREL_ATB_OMfixed <- get_data(all_data, "energy/NREL_ATB_OMfixed") %>%
      gather_years()
    NREL_ATB_OMvar <- get_data(all_data, "energy/NREL_ATB_OMvar") %>%
      gather_years()

    # Mapping
    atb_gcam_mapping <- get_data(all_data, "energy/mappings/atb_gcam_mapping") %>%
      select(-inferred)

    # ===================================================

    # Pre-process data so capital & OM costs can be processed together
    # Start with NREL cost data
    # For capital costs, we want central, advanced tech (low cost), and low tech (high / constant cost) scenarios
    NREL_ATB_capital %>%
      # For O&M costs, we want central scenario only
      bind_rows(NREL_ATB_OMfixed %>%
                  filter(case == energy.COSTS_MID_CASE),
                NREL_ATB_OMvar %>%
                  filter(case == energy.COSTS_MID_CASE)) %>%
      # clean up cost case part of technology names, since mapping file doesn't include this detail
      mutate(tech_detail = gsub(" - Mid", "", tech_detail),
             tech_detail = gsub(" - Low", "", tech_detail),
             tech_detail = gsub(" - Constant", "", tech_detail),
             tech_detail = gsub(" - High", "", tech_detail), # only natural gas technologies in ATB
             tech_detail = gsub("-Mid", "", tech_detail),
             tech_detail = gsub("-Low", "", tech_detail),
             tech_detail = gsub("-Constant", "", tech_detail)) -> NREL_ATB_cost_assumptions

    # Pre-process legacy (Muratori et al.) cost data so capital & OM costs can be processed together
    Muratori_globaltech_capital %>%
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      rename(input = 'input-capital') %>%
      mutate(case = energy.COSTS_MID_CASE) %>%
      bind_rows(Muratori_globaltech_capital_adv %>%
                  fill_exp_decay_extrapolate(MODEL_YEARS) %>%
                  rename(input = 'input-capital') %>%
                  mutate(case = energy.COSTS_ADV_CASE),
                Muratori_globaltech_capital_low %>%
                  fill_exp_decay_extrapolate(MODEL_YEARS) %>%
                  rename(input = 'input-capital') %>%
                  mutate(case = energy.COSTS_LOW_CASE),
                Muratori_globaltech_OMfixed %>%
                  fill_exp_decay_extrapolate(MODEL_YEARS) %>%
                  rename(input = input.OM.fixed) %>%
                  mutate(case = energy.COSTS_MID_CASE),
                Muratori_globaltech_OMvar %>%
                  fill_exp_decay_extrapolate(MODEL_YEARS) %>%
                  rename(input = input.OM.var) %>%
                  mutate(case = energy.COSTS_MID_CASE)) -> GCAM_legacy_cost_assumptions

    # Establish some year constants for data processing
    ATB_years <- unique(NREL_ATB_cost_assumptions$year)

    A23_years <- A23.globaltech_capital %>%
      gather_years() %>%
      distinct(year)
    A23_years <- unique(A23_years$year)

    # Some GCAM technologies are not included in the ATB database
    # For these, we calculate the cost ratio from the previous GCAM cost assumptions
    # and apply these ratios to the costs for technologies that are in the ATB database.
    # Below are the relevant technologies and the ratios used to infer their costs.
    # This info can also be found in energy/mappings/atb_gcam_mapping
    # CSP ratio = CSP / CSP_storage
    # biomass (IGCC) ratio = biomass (IGCC) / biomass (conv)
    # biomass (conv CCS) ratio = biomass (conv CCS) / biomass (conv)
    # biomass (IGCC CCS) ratio = biomass (IGCC CCS) / biomass (IGCC)
    # coal (IGCC CCS) ratio = coal (IGCC CCS) / coal (IGCC)
    # refined liquids (steam/CT) ratio = refined liquids (steam/CT) / gas (steam/CT)
    # refined liquids (CC) ratio = refined liquids (CC) / gas (CC)
    # refined liquids (CC CCS) ratio = refined liquids (CC CCS) / gas (CC CCS)

    # Isolate technologies which require a shadow technology to infer costs from ATB data set.
    atb_gcam_mapping %>%
      filter(!is.na(shadow_tech)) %>%
      select(technology, shadow_tech) -> atb_gcam_mapping_ratios

    # Build a list of costs for shadow techologies
    GCAM_legacy_cost_assumptions %>%
      # filter for technologies that are considered shadow techs
      semi_join(atb_gcam_mapping_ratios, by = c("technology" = "shadow_tech")) %>%
      select(shadow_tech = technology, year, shadow_tech_cost = value, input, case)  -> L113.cost_shadow

    # Calculate cost ratios for technologies which require a shadow tech to infer costs from ATB data
    GCAM_legacy_cost_assumptions %>%
      # filter for technologies which need a shadow tech to calculate a cost ratio
      semi_join(atb_gcam_mapping_ratios, by = "technology") %>%
      left_join_error_no_match(atb_gcam_mapping_ratios, by = "technology") %>%
      left_join_error_no_match(L113.cost_shadow, by = c("shadow_tech", "year", "input", "case")) %>%
      mutate(cost_ratio = value / shadow_tech_cost) %>%
      select(technology, year, cost_ratio, input, case) %>%
      # fill out for all ATB years
      complete(nesting(technology, input, case), year = c(ATB_years)) %>%
      group_by(technology, input, case) %>%
      mutate(cost_ratio = approx_fun(year, cost_ratio)) %>%
      ungroup() -> L113.cost_shadow_ratio

    NREL_ATB_cost_assumptions %>%
      # isolate technologies which map to GCAM technologies
      semi_join(atb_gcam_mapping, by = c("tech_type", "tech_detail")) %>%
      # Convert to 1975$.  2015-2016 costs are in 2015$; 2017-2050 costs are in 2017$.
      mutate(value = if_else(year %in% energy.ATB_2017_YEARS, value * gdp_deflator(1975, 2015), value * gdp_deflator(1975, 2017))) %>%
      # some techs, like battery, don't have costs back to 2015
      # use approx_fun rule 2 to carry nearest year values backwards
      group_by(tech_type, tech_detail, input, case) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup %>%
      # join is intended to duplicate rows - some GCAM tech costs are composites of multiple ATB techs
      # LJENM throws an error, left_join is used
      left_join(atb_gcam_mapping, by = c("tech_type", "tech_detail")) %>%
      # not every technology requires a shadow technology to compute costs from the ATB data set
      # LJENM errors because of NAs (not all techs are in RHS), NAs are dealt with below, left_join is used
      left_join(L113.cost_shadow_ratio, by = c("technology", "year", "input", "case")) %>%
      mutate(conversion = if_else(is.na(conversion), cost_ratio, conversion),
             value = value * conversion) %>%
      group_by(technology, year, input, case) %>%
      summarise(value = sum(value)) %>%
      ungroup() -> L113.costs_ATB

    # Calculate the improvement rate from 2015 to 2035 and from 2035 to 2050
    L113.costs_ATB %>%
      group_by(technology, input, case) %>%
      # calculate simple near term and mid term annual improvement rates as % reduction / # years
      mutate(initial_ATB_cost = value[year==energy.ATB_BASE_YEAR],
             mid_ATB_cost = value[year==energy.ATB_MID_YEAR],
             final_ATB_cost = value[year==max(year)],
             target_ATB_cost = value[year==energy.ATB_TARGET_YEAR],
             near_term_improvement = ((initial_ATB_cost - mid_ATB_cost) / energy.ATB_BASE_YEAR) / (energy.ATB_MID_YEAR - energy.ATB_BASE_YEAR),
             long_term_improvement = ((mid_ATB_cost - final_ATB_cost) / energy.ATB_MID_YEAR) / (max(year) - energy.ATB_MID_YEAR),
             # calculate 2100 value based on extending long-term improvement rate
             cost_end_year = final_ATB_cost - (final_ATB_cost * (long_term_improvement * (max(FUTURE_YEARS) - max(year)))),
             # calculate maximum improvement based on extrapolated 2100 cost : 2015 cost
             improvement.max = cost_end_year / value[year==energy.ATB_BASE_YEAR],
             # calculate baseline improvement rate values assuming average linear reduction
             # this is just a starting point for the exponential function below
             improvement.rate.base = (initial_ATB_cost - cost_end_year) / (max(year) - energy.ATB_BASE_YEAR) / initial_ATB_cost) %>%
      ungroup() %>%
      distinct(technology, input, case, initial_ATB_cost, target_ATB_cost, improvement.max, improvement.rate.base) %>%
      # filter out techs with no cost in ATB - nuclear adv / low tech capital costs, CSP variable O&M
      filter(!is.na(initial_ATB_cost)) %>%
      # techs with no variable O&M have NAN improvement.max & improvement.rate.base - set to 0
      replace_na(list(improvement.max = 0, improvement.rate.base = 0)) %>%
      # for technologies where costs increase (very marginal increases), reset this
      mutate(target_ATB_cost = if_else(target_ATB_cost > initial_ATB_cost, initial_ATB_cost, target_ATB_cost),
             improvement.max = if_else(improvement.max > 1, 1, improvement.max),
             improvement.rate.base = if_else(improvement.rate.base < 0, 0, improvement.rate.base)) -> L113.costs_ATB_params

    # Setting up a functional form that replicates the function structure of cost generation. This has the following main components:
    # initial_ATB_cost: the base (2015) ATB value
    # improvement.max: maximum improvement from 2015 - 2100
    # The target year (2035) ATB value - the year deemed the inflection point between near and long-term improvement rates
    # improvement.rate = a rate of improvement such that the function gets as close to the target year ATB value as possible
    # This needs to be calculated for all technologies

    # Create a function for all technologies to determine the improvement rate that will
    # yield the desired capital cost (target_ATB_cost) in energy.ATB_TARGET_YEAR (default = 2035)
    calc_improvement_rate <- function(improvement.rate, tech, component, level) {
      L113.costs_ATB_params %>%
        filter(technology == tech,
               input == component,
               case == level) %>%
        mutate(check = initial_ATB_cost * improvement.max + (initial_ATB_cost - initial_ATB_cost * improvement.max) *
                 (1.0 - improvement.rate) ^ (energy.ATB_TARGET_YEAR - energy.ATB_BASE_YEAR) - target_ATB_cost) %>%
        pull(check) -> check
      check
    }

    # Loop the function for all technologies / input costs (component) / cases (level),
    # using uniroot to ensure that the correct improvement rate is found and entered in the main table
    for (rn in rownames(L113.costs_ATB_params)) {
      L113.costs_ATB_params %>%
        filter(row_number() == rn) -> L113.TEMP

      tech <- L113.TEMP$technology
      component <- L113.TEMP$input
      level <- L113.TEMP$case

      solved.improvement.rate <- uniroot(calc_improvement_rate, c(0, 1), tech, component, level)

      L113.costs_ATB_params %>%
        mutate(improvement.rate.base = replace(improvement.rate.base,
                                               technology == tech & input == component & case == level,
                                               solved.improvement.rate$root)) -> L113.costs_ATB_params

    }

    # Clean up the ATB table in order to properly merge with the old globaltech_capital table.
    L113.costs_ATB_params %>%
      select(technology, input, case, value = initial_ATB_cost, improvement.max, improvement.rate = improvement.rate.base) %>%
      mutate(year = energy.ATB_BASE_YEAR,
             improvement.max = round(improvement.max, energy.DIGITS_CAPACITY_FACTOR),
             improvement.rate = round(improvement.rate, energy.DIGITS_CAPACITY_FACTOR)) -> L113.globaltech_cost_atb

    # Extract technologies from the original A23 file that have a valid 2015 data (i.e. not NAs).
    # These technologies will take priority over ATB technologies if there's a conflict.
    A23.globaltech_capital %>%
      rename(input = 'input-capital') %>%
      mutate(case = energy.COSTS_MID_CASE) %>%
      bind_rows(A23.globaltech_capital_adv  %>%
                  rename(input = 'input-capital') %>%
                  mutate(case = energy.COSTS_ADV_CASE),
                A23.globaltech_capital_low  %>%
                  rename(input = 'input-capital') %>%
                  mutate(case = energy.COSTS_LOW_CASE),
                A23.globaltech_OMfixed  %>%
                  rename(input = input.OM.fixed) %>%
                  mutate(case = energy.COSTS_MID_CASE),
                A23.globaltech_OMvar %>%
                  rename(input = input.OM.var) %>%
                  mutate(case = energy.COSTS_MID_CASE)) %>%
      gather_years() -> A23.globaltech_cost

    A23.globaltech_cost %>%
      group_by(technology, input, case) %>%
      filter(!is.na(value[year==energy.ATB_BASE_YEAR])) %>%
      ungroup() -> A23.globaltech_cost_keep

    # Filter out capital cost data from ATB if already present in A23.globaltech_capital_keep.
    # We also filter out battery technology because that will only be used in GCAM-USA.
    L113.globaltech_cost_atb %>%
      anti_join(A23.globaltech_cost_keep, by = c("technology", "input", "case")) -> L113.globaltech_cost_atb_temp

    # Merge ATB data with the original globaltech_capital file. Populate historical years with 2015 ATB data.
    # Proprerly format the column order and row order.
    A23.globaltech_cost %>%
      gather_years() %>%
      filter(year==energy.ATB_BASE_YEAR) %>%
      group_by(technology, input, case) %>%
      # get rid of values that will be overridden by user defined values in A23 files
      filter(is.na(value[year==energy.ATB_BASE_YEAR])) %>%
      ungroup() %>%
      select(-year, -value, -improvement.max, -improvement.rate) %>%
      left_join_error_no_match(L113.globaltech_cost_atb_temp, by = c("technology", "input", "case")) %>%
      # add in user defined values from A23 files
      bind_rows(A23.globaltech_cost_keep) %>%
      # fill out for all years in original A23. file
      complete(nesting(supplysector, subsector, technology, input, case, fixed.charge.rate,
                       improvement.shadow.technology, improvement.max, improvement.rate),
               year = c(A23_years)) %>%
      group_by(technology, input, case) %>%
      # copy 2015 costs backwards
      mutate(value = if_else(is.na(value), value[year==energy.ATB_BASE_YEAR], value)) %>%
      ungroup() %>%
      arrange(subsector, technology, input, case) %>%
      spread(year, value) -> L113.globaltech_cost

    L113.globaltech_cost %>%
      # Filter for correct cost case
      filter(input == energy.CAPITAL_INPUT,
             case == energy.COSTS_MID_CASE) %>%
      rename('input-capital' = input) %>%
      select(-case) -> L113.globaltech_capital_ATB

    L113.globaltech_cost  %>%
      # Filter for correct cost case
      filter(input == energy.CAPITAL_INPUT,
             case == energy.COSTS_ADV_CASE) %>%
      rename('input-capital' = input) %>%
      select(-case) -> L113.globaltech_capital_ATB_adv

    L113.globaltech_cost %>%
      # Filter for correct cost case
      filter(input == energy.CAPITAL_INPUT,
             case == energy.COSTS_LOW_CASE) %>%
      rename('input-capital' = input) %>%
      select(-case) -> L113.globaltech_capital_ATB_low

    L113.globaltech_cost %>%
      # Filter for correct cost case
      filter(input == energy.OM_FIXED_INPUT,
             case == energy.COSTS_MID_CASE) %>%
      rename(input.OM.fixed = input) %>%
      select(-case) -> L113.globaltech_OMfixed_ATB

    L113.globaltech_cost %>%
      # Filter for correct cost case
      filter(input == energy.OM_VAR_INPUT,
             case == energy.COSTS_MID_CASE) %>%
      rename(input.OM.var = input) %>%
      select(-case) -> L113.globaltech_OMvar_ATB

    # Filter out battery technology data
    L113.globaltech_cost_atb %>%
      filter(technology == gcamusa.STORAGE_TECH,
             input %in% c(energy.CAPITAL_INPUT, energy.OM_FIXED_INPUT, energy.OM_VAR_INPUT),
             case == energy.COSTS_MID_CASE) %>%
      # Prepare the table in the format which can be used by the exponential decay function
      # in order to generate a full set of cost forecasts
      complete(nesting(technology, input, case, improvement.max, improvement.rate),
               year = c(A23_years)) %>%
      group_by(technology, input, case) %>%
      # copy 2015 costs backwards
      mutate(value = if_else(is.na(value), value[year==energy.ATB_BASE_YEAR], value)) %>%
      ungroup() %>%
      # Use exponential decay function to extrapolate costs till 2100
      fill_exp_decay_extrapolate(MODEL_YEARS) %>%
      # NAs are from variable OM, which is zero in all time periods
      replace_na(list(value = 0)) %>%
      rename(period = year) %>%
      select(technology, period, input, value) %>%
      spread(input, value) %>%
      rename(capital.cost = capital,
             fixed.om = `OM-fixed`,
             variable.om = `OM-var`) %>%
      mutate(capital.cost = round(capital.cost, energy.DIGITS_CAPITAL),
             fixed.om = round(fixed.om, energy.DIGITS_OM),
             variable.om = round(variable.om, energy.DIGITS_OM)) -> A23.globaltech_capital_atb_battery

    # Merge cost data with GCAM_USA battery cost structure file
    A23.elecS_globaltech_non_energy_inputs %>%
      select(-capital.cost, -fixed.om, -variable.om) %>%
      left_join_error_no_match(A23.globaltech_capital_atb_battery,
                               by = c("technology", "period")) -> L113.elecS_globaltech_capital_battery_ATB


    # ===================================================
    # Produce outputs

    L113.globaltech_capital_ATB %>%
      add_title("2019 ATB-based capital cost structure for GCAM") %>%
      add_units("1975$/kW") %>%
      add_comments("Will be used in place of A23.globaltech_capital by relevant chunks") %>%
      add_precursors("energy/A23.globaltech_capital",
                     "energy/Muratori_globaltech_capital",
                     "energy/NREL_ATB_capital",
                     "energy/mappings/atb_gcam_mapping") ->
      L113.globaltech_capital_ATB

    L113.globaltech_capital_ATB_adv %>%
      add_title("2019 ATB-based capital cost structure for GCAM - Advanced improvement scenario") %>%
      add_units("1975$/kW") %>%
      add_comments("Will be used in place of A23.globaltech_capital_adv by relevant chunks") %>%
      add_precursors("energy/A23.globaltech_capital_adv",
                     "energy/Muratori_globaltech_capital_adv",
                     "energy/NREL_ATB_capital",
                     "energy/mappings/atb_gcam_mapping") ->
      L113.globaltech_capital_ATB_adv

    L113.globaltech_capital_ATB_low %>%
      add_title("2019 ATB-based capital cost structure for GCAM - Low improvement scenario") %>%
      add_units("1975$/kW") %>%
      add_comments("Will be used in place of A23.globaltech_capital_low by relevant chunks") %>%
      add_precursors("energy/A23.globaltech_capital_low",
                     "energy/Muratori_globaltech_capital_low",
                     "energy/NREL_ATB_capital",
                     "energy/mappings/atb_gcam_mapping") ->
      L113.globaltech_capital_ATB_low

    L113.elecS_globaltech_capital_battery_ATB %>%
      add_title("Battery technology costs to be used by GCAM USA") %>%
      add_units("1975$/kW") %>%
      add_comments("Will be used in place of A23.elecS_globaltech_non_energy_inputs by relevant chunks") %>%
      add_precursors("gcam-usa/A23.elecS_globaltech_non_energy_inputs",
                     "energy/NREL_ATB_capital",
                     "energy/mappings/atb_gcam_mapping") ->
      L113.elecS_globaltech_capital_battery_ATB

    L113.globaltech_OMfixed_ATB %>%
      add_title("2019 ATB-based capital cost structure for GCAM - Advanced improvement scenario") %>%
      add_units("1975$/kW") %>%
      add_comments("Will be used in place of A23.globaltech_capital_adv by relevant chunks") %>%
      add_precursors("energy/A23.globaltech_OMfixed",
                     "energy/Muratori_globaltech_OMfixed",
                     "energy/NREL_ATB_OMfixed",
                     "energy/mappings/atb_gcam_mapping") ->
      L113.globaltech_OMfixed_ATB

    L113.globaltech_OMvar_ATB %>%
      add_title("2019 ATB-based capital cost structure for GCAM - Advanced improvement scenario") %>%
      add_units("1975$/kW") %>%
      add_comments("Will be used in place of A23.globaltech_capital_adv by relevant chunks") %>%
      add_precursors("energy/A23.globaltech_OMvar",
                     "energy/Muratori_globaltech_OMvar",
                     "energy/NREL_ATB_OMvar",
                     "energy/mappings/atb_gcam_mapping") ->
      L113.globaltech_OMvar_ATB

    return_data(L113.globaltech_capital_ATB,
                L113.globaltech_capital_ATB_adv,
                L113.globaltech_capital_ATB_low,
                L113.elecS_globaltech_capital_battery_ATB,
                L113.globaltech_OMfixed_ATB,
                L113.globaltech_OMvar_ATB)
  } else {
    stop("Unknown command")
  }
}
