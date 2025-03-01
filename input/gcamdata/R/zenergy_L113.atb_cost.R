# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L113.atb_cost
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
#' @importFrom purrr reduce
#' @author AJS March 2019
module_energy_L113.atb_cost <- function(command, ...) {
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
             # Add in separate files for each separate ATB data year
             FILE = "energy/NREL_ATB_capital_2017",
             FILE = "energy/NREL_ATB_capital_2019",
             FILE = "energy/NREL_ATB_capital_2021",
             FILE = "energy/NREL_ATB_capital_2022",
             FILE = "energy/NREL_ATB_OMfixed_2017",
             FILE = "energy/NREL_ATB_OMfixed_2019",
             FILE = "energy/NREL_ATB_OMfixed_2021",
             FILE = "energy/NREL_ATB_OMfixed_2022",
             FILE = "energy/NREL_ATB_OMvar_2017",
             FILE = "energy/NREL_ATB_OMvar_2019",
             FILE = "energy/NREL_ATB_OMvar_2021",
             FILE = "energy/NREL_ATB_OMvar_2022",
             FILE = "energy/mappings/ATB_tech_mapping",
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

    # Mapping
    atb_tech_mapping <- get_data(all_data, "energy/mappings/ATB_tech_mapping")
    atb_gcam_mapping <- get_data(all_data, "energy/mappings/atb_gcam_mapping") %>%
      select(-inferred)

    # ===================================================
    # ============ NREL ATB Processing ==================
    # ===================================================
    # NREL ATB file processing
    # Load in the ATB files flexibly in list that will be bound together
    # Initialize list of data for each case (NREL_ATB_capital, NREL_OMvar, NREL_OMfixed)
    NREL_ATB_capital_all_years = list()
    NREL_ATB_OMfixed_all_years = list()
    NREL_ATB_OMvar_all_years = list()
    # Data converted to $1975
    NREL_ATB_capital_all_years_1975 = list()
    NREL_OMvar_all_years_1975 = list()
    NREL_OMfixed_all_years_1975 = list()
    # Scaled NREL ATB data with time series interpolation applied
    NREL_ATB_capital_scaled = list()
    NREL_ATB_OMvar_scaled = list()
    NREL_ATB_OMfixed_scaled = list()


  # Load in the ATB onto the original file:
  # Naming convention is NREL_ATB_%input%_%year%
  # For mapping, we will use the naming conventions of the most recent ATB dataset:
    for(year in energy.ATB_HISTORICAL_YEARS){
      # Get index in loop
      ind <- which(year==energy.ATB_HISTORICAL_YEARS)
      # Assign variable names for each NREL ATB file, indexing by year and input (Capital, OMvar, OMfixed)
      assign(paste0("NREL_ATB_capital_",year), get_data(all_data, paste0("energy/NREL_ATB_capital_", year)))
      assign(paste0("NREL_ATB_OMfixed_", year),get_data(all_data, paste0("energy/NREL_ATB_OMfixed_", year)))
      assign(paste0("NREL_ATB_OMvar_", year), get_data(all_data, paste0("energy/NREL_ATB_OMvar_", year)))

      # Combine with mapping file to ensure that datasets are consistent across ATB datasets:
      # Do not perform this operation if we are at the final ATB year:
      if(year != max(energy.ATB_HISTORICAL_YEARS)){
      atb_tech_colname <- paste0('tech_detail_','ATB',year)
      atb_tech_mapping %>%
        rename(tech_detail = atb_tech_colname) %>%
        select(tech_type,tech_detail,tech_detail_standard) -> atb_technology_mapping

      Capital_yr_data_filt <- eval(parse(text=paste0("NREL_ATB_capital_",year))) %>%
        # Some technologies don't exist in earlier ATB's so we cannot use LJENM here
        left_join(atb_technology_mapping, by = c('tech_type','tech_detail')) %>%
        # Filter out unmapped ATB data:
        filter(!is.na(tech_detail_standard)) %>%
        mutate(tech_detail = tech_detail_standard) %>%
        select(-tech_detail_standard)

      OMvar_yr_data_filt <- eval(parse(text=paste0("NREL_ATB_OMvar_",year))) %>%
        left_join(atb_technology_mapping, by = c('tech_type','tech_detail')) %>%
        # Filter out unmapped ATB data:
        filter(!is.na(tech_detail_standard)) %>%
        mutate(tech_detail = tech_detail_standard) %>%
        select(-tech_detail_standard)

      OMfixed_yr_data_filt <- eval(parse(text=paste0("NREL_ATB_OMfixed_",year))) %>%
        left_join(atb_technology_mapping, by = c('tech_type','tech_detail')) %>%
        # Filter out unmapped ATB data:
        filter(!is.na(tech_detail_standard)) %>%
        mutate(tech_detail = tech_detail_standard) %>%
        select(-tech_detail_standard)
      } else {
        Capital_yr_data_filt <- eval(parse(text=paste0("NREL_ATB_capital_",year)))
        OMvar_yr_data_filt <- eval(parse(text=paste0("NREL_ATB_OMvar_",year)))
        OMfixed_yr_data_filt <- eval(parse(text=paste0("NREL_ATB_OMfixed_",year)))
      }

      # Append files to list
      NREL_ATB_capital_all_years[[ind]] <- Capital_yr_data_filt
      NREL_ATB_OMvar_all_years[[ind]] <- OMvar_yr_data_filt
      NREL_ATB_OMfixed_all_years[[ind]] <- OMfixed_yr_data_filt
    }

    # Need to scale the data to smooth discontinuities between different ATB dataset costs

    # First, adjust all ATB costs to $1975 using function:
    # Define function to determine appropriate gdp price deflator and scales data proportionally
    # This cost is equivalent to the degree of deflation in the first ATB year of that dataset:
    # The function takes in a long format ATB dataset:
    Adjust_price_deflation_1975 <- function(data) {
      # Define list which the new dataframes will be appended to
      # Note this assumes the $ year in each ATB is the first year in the data (always true so far)
      yearDeflator <- min(data$year)
      # Scale using GDP deflator:
      data %>%
        mutate(value = value * gdp_deflator(1975,yearDeflator)) -> costs_adjusted
      return(costs_adjusted)
    }

    # Adjust prices from all three inputs to $1975
    # Doing the price adjustment here centralizes this operation and assures its consistent for all ATB files
    for(year in energy.ATB_HISTORICAL_YEARS) {
      ind <- which(year == energy.ATB_HISTORICAL_YEARS)
      NREL_ATB_capital_all_years[[ind]] %>%
        gather_years() %>%
        Adjust_price_deflation_1975() -> NREL_ATB_capital_all_years_1975[[ind]]
      NREL_ATB_OMvar_all_years[[ind]] %>%
        gather_years() %>%
        Adjust_price_deflation_1975() -> NREL_OMvar_all_years_1975[[ind]]
      NREL_ATB_OMfixed_all_years[[ind]] %>%
        gather_years() %>%
        Adjust_price_deflation_1975() -> NREL_OMfixed_all_years_1975[[ind]]
    }



    # Need to pre-scale the data in order to smooth discontinuities prior to implementing time series
    # Define function to calculate the alpha factor used to extend time series
    # Alpha factors will be used as inputs for extending ATB data back to first ATB year
    harmonize_datasets <- function(atb1, atbnext) {
      # Determine overlap year between the datasets:
      # This will occur at the first year of the second dataset:
      overlap_year <- min(atbnext$year)
      # Choose year to interpolate from (first year of first dataset):
      first_year <- min(atb1$year)

      # For battery storage technology, the first year of available data
      # is 2018, so we need to copy back this cost
      BATTERY_STORAGE_YEAR <- 2018

      # Perform time series only on the central case:
      atb1 %>% filter(case == energy.COSTS_MID_CASE) -> atb1
      atbnext %>% filter(case == energy.COSTS_MID_CASE) -> atbnext

      # Now combine the datasets and determine what the ratio is at the overlap year
      # We will do this only for central case:
      # Apply the time series development only to central case. Adv and low techs
      # have the same costs for historical years (< energy.ATB_LATEST_YEAR)
      atbnext %>%
        filter(year == overlap_year) %>%
               # !tech_type %in% nonexist_techs) %>%
        rename(newVal = value) %>%
        # Use left_join because different ATB years do not contain the same technologies
        left_join(atb1 %>% filter(year == overlap_year),
                 by = c('tech_type', 'tech_detail','input','case','year')) %>%
        mutate(ratio = newVal / value) %>%
        mutate(ratio = if_else(is.na(ratio),1,ratio)) %>%
        select(-newVal, -value,-year) %>%
        mutate(check_overlap_constraint = if_else(abs(ratio -1) > energy.ATB_OVERLAP_CONSTRAINT,TRUE,FALSE))-> overlap_ratio_tab
      # Apply interpolation between the min year in atb1 and the overlap year
      # We want to stitch together 2 time series:
      # 1. The interpolated dataset for atb1 up to overlap_year
      # 2. The data for atb2 passed overlap_year
      atb1 %>%
        # Cannot use LJENM because some technologies don't yet exist in the ATB:
        left_join(overlap_ratio_tab, by = c('tech_type','tech_detail',
                                                           'input','case')) %>%
        mutate(ratio = if_else(year == min(year),1,ratio)) %>%
        filter(year <= overlap_year) %>%
        # Need to clear the intermediate years to apply approx_fun between the
        # first year and the overlap year. We assign an arbitrarily high number (of 100000)
        # so we can then replace with NA values
        mutate(ratio = if_else(!year %in% c(first_year,overlap_year),100000,ratio)) %>%
        # For battery technology, there is no data before 2018 so we want to make
        # sure we are interpolating between 2018 and the overlap year
        mutate(ratio = if_else(tech_type == 'Storage' & year <= BATTERY_STORAGE_YEAR,
                               1,ratio)) %>%
        mutate(ratio = dplyr::na_if(ratio,100000)) %>%
        group_by(tech_type,tech_detail,input,case) %>%
        mutate(ratio = approx_fun(year, ratio, rule = 2)) %>%
        ungroup() %>%
        # Set condition if the ratio of costs at the overlap year > 0.3 where we just copy back values
        # from the overlap year:
        group_by(tech_type,tech_detail,input,case) %>%
        mutate(value = value * ratio) %>%
        # Now if the ratio is greater than 0.3, we copy back from the overlap_year
        mutate(value = if_else(check_overlap_constraint == TRUE, value[year==overlap_year],value)) %>%
        select(-ratio,-check_overlap_constraint) %>%
        bind_rows(atbnext %>% filter(year >= overlap_year)) %>%
        ungroup() %>%
        filter(year <= energy.ATB_LATEST_YEAR) %>%
        distinct()-> timeseries_built
      return(timeseries_built)
    }


    scale_time_series <- function(atbList) {
      # We will loop through each technology and consecutively harmonize between datasets
      # up to the final dataset
      working_timeseries <- atbList[[1]]
      for (index in seq(from = 1, to=length(atbList)-1)){
        harmonize_datasets(working_timeseries, atbList[[index+1]]) -> working_timeseries
      }
    # Next, make sure that the low and advanced tech cases are kept in order to build the
    # full time series. For historical year < 2019, we will just make these the same as
    # the central case:
    working_timeseries %>%
      # First, if there is missing data, copy back from latest available data using approx_fun():
      group_by(tech_type, tech_detail) %>%
      mutate(value = approx_fun(year,value,rule=2)) %>%
      ungroup() %>%
      select(-case) %>%
      repeat_add_columns(tibble(case = unique(tail(atbList,n=1)[[1]]$case))) %>%
      bind_rows(tail(atbList,n=1)[[1]] %>% filter(year > energy.ATB_LATEST_YEAR)) %>%
      # Certain technologies only have central case defined so here, we will ensure that
      # i.e. Nuclear
      # advanced and low tech cases have the same costs:
      group_by(tech_type,tech_detail,input,year) %>%
      mutate(value = if_else(is.na(value),value[energy.COSTS_MID_CASE],value)) %>%
      ungroup()-> scaled_timeseries

    return(scaled_timeseries)
    }

    NREL_ATB_capital_all_years_1975 %>%
      scale_time_series() -> NREL_ATB_capital

    NREL_OMfixed_all_years_1975 %>%
      scale_time_series() -> NREL_ATB_OMfixed

    NREL_OMvar_all_years_1975 %>%
      scale_time_series -> NREL_ATB_OMvar

    NREL_ATB_cost_assumptions <- bind_rows(NREL_ATB_capital,
                                           NREL_ATB_OMfixed,
                                           NREL_ATB_OMvar)

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
                  mutate(case = energy.COSTS_MID_CASE)) %>%
      # We want these to diverge at the latest ATB year so we will set them to the central case
      # before the costs diverge. Then we use the shadow tech ratios:
            group_by(supplysector,subsector,technology,input,year) %>%
            mutate(value = if_else(year <= energy.ATB_LATEST_YEAR,value[case==energy.COSTS_MID_CASE],
                                   value)) %>%
            ungroup() -> GCAM_legacy_cost_assumptions

   # Establish some year constants for data processing
    ATB_years <- unique(NREL_ATB_cost_assumptions$year)

    A23_years <- A23.globaltech_capital %>%
      gather_years() %>%
      distinct(year)
    # Captures all the historical years
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
      select(shadow_tech = technology, year, shadow_tech_cost = value, input, case) %>%
      # For historical years, we want to normalize the shadow tech cost so we will
      # set to the central trajectory:
      group_by(shadow_tech,input,year) %>%
      mutate(shadow_tech_cost = if_else(year <= energy.ATB_LATEST_YEAR,
                                        shadow_tech_cost[case==energy.COSTS_MID_CASE],shadow_tech_cost)) %>%
      ungroup() -> L113.cost_shadow

    # Check here that all the shadow technologies are mapped:
    if(length(setdiff(unique(atb_gcam_mapping_ratios$shadow_tech),unique(L113.cost_shadow$shadow_tech)))>0) {
      stop('A shadow technology is missing from the dataset. Please check your ATB mapping')
    }

    # Calculate cost ratios for technologies which require a shadow tech to infer costs from ATB data
    GCAM_legacy_cost_assumptions %>%
      # filter for technologies which need a shadow tech to calculate a cost ratio
      semi_join(atb_gcam_mapping_ratios, by = "technology") %>%
      left_join_error_no_match(atb_gcam_mapping_ratios, by = "technology") %>%
      left_join_error_no_match(L113.cost_shadow, by = c("shadow_tech", "year", "input", "case")) %>%
      mutate(cost_ratio = value / shadow_tech_cost) %>%
      # technologies with cost = 0 for tech & shadow tech (e.g. CSP OM-var) return NAN... reset to cost_ratio = 1
      mutate(cost_ratio = if_else(is.nan(cost_ratio), 1, cost_ratio)) %>%
      select(technology, year, cost_ratio, input, case) %>%
      # fill out for all ATB years
      complete(nesting(technology, input, case), year = c(ATB_years)) %>%
      group_by(technology, input, case) %>%
      mutate(cost_ratio = approx_fun(year, cost_ratio)) %>%
      ungroup() -> L113.cost_shadow_ratio

    NREL_ATB_cost_assumptions %>%
      # isolate technologies which map to GCAM technologies
      semi_join(atb_gcam_mapping, by = c("tech_type", "tech_detail")) %>%
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
      distinct() %>%
      group_by(technology, year, input, case) %>%
      summarise(value = sum(value)) %>%
      # When shadow tech ratio is applied, we need to make sure that we retain the adv and low tech cases:
      ungroup() %>%
      group_by(technology,year,input) %>%
      mutate(value = if_else(is.na(value),value[case == energy.COSTS_MID_CASE],value)) %>%
      ungroup() -> L113.costs_ATB

    # Calculate the improvement rate from the latest ATB year to energy.ATB_MID_YEAR and from energy.ATB_MID_YEAR to 2050
    L113.costs_ATB %>%
      group_by(technology, input, case) %>%
      # calculate simple near term and mid term annual improvement rates as % reduction / # years
      mutate(initial_ATB_cost = value[year==energy.ATB_LATEST_YEAR],
             mid_ATB_cost = value[year==energy.ATB_MID_YEAR],
             final_ATB_cost = value[year==max(year)],
             target_ATB_cost = value[year==energy.ATB_TARGET_YEAR],
             near_term_improvement = ((initial_ATB_cost - mid_ATB_cost) / energy.ATB_LATEST_YEAR) / (energy.ATB_MID_YEAR - energy.ATB_LATEST_YEAR),
             long_term_improvement = ((mid_ATB_cost - final_ATB_cost) / energy.ATB_MID_YEAR) / (max(year) - energy.ATB_MID_YEAR),
             # calculate 2100 value based on extending long-term improvement rate
             cost_end_year = final_ATB_cost - (final_ATB_cost * (long_term_improvement * (max(FUTURE_YEARS) - max(year)))),
             # calculate maximum improvement based on extrapolated 2100 cost : 2015 cost
             improvement.max = cost_end_year / value[year==energy.ATB_LATEST_YEAR],
             # calculate baseline improvement rate values assuming average linear reduction
             # this is just a starting point for the exponential function below
             improvement.rate.base = (initial_ATB_cost - cost_end_year) / (max(year) - energy.ATB_LATEST_YEAR) / initial_ATB_cost) %>%
      ungroup() %>%
      distinct(technology, input, case, initial_ATB_cost, target_ATB_cost, improvement.max, improvement.rate.base,cost_end_year) %>%
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
                 (1.0 - improvement.rate) ^ (energy.ATB_TARGET_YEAR - energy.ATB_LATEST_YEAR) - target_ATB_cost) %>%
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
      improvement.max <- L113.TEMP$improvement.max
      end_yr_cost <- L113.TEMP$cost_end_year
      target_ATB_cost <- L113.TEMP$target_ATB_cost

      if(end_yr_cost != target_ATB_cost) {
        solved.improvement.rate.calc <- uniroot(calc_improvement_rate, c(0, 1), tech, component, level)
        solved.improvement.rate <- solved.improvement.rate.calc$root
      } else {
        # print(L113.TEMP$technology)
        solved.improvement.rate <- improvement.max
      }

      L113.costs_ATB_params %>%
        mutate(improvement.rate.base = replace(improvement.rate.base,
                                               technology == tech & input == component & case == level,
                                               solved.improvement.rate)) -> L113.costs_ATB_params

    }

    # Clean up the ATB table in order to properly merge with the old globaltech_capital table.
    # Add in the historical time series to provide selectivity for ATB base year:
    # Isolate the historical time series before the atb base year:
    L113.costs_ATB %>%
      filter(year < energy.ATB_LATEST_YEAR) -> L113.costs_ATB_historical

    L113.costs_ATB_params %>%
      select(technology, input, case, value = initial_ATB_cost, improvement.max, improvement.rate = improvement.rate.base) %>%
      # Improvement rates taken from the latest ATB year
      mutate(year = energy.ATB_LATEST_YEAR,
             improvement.max = round(improvement.max, energy.DIGITS_CAPACITY_FACTOR),
             improvement.rate = round(improvement.rate, energy.DIGITS_CAPACITY_FACTOR)) %>%
      bind_rows(L113.costs_ATB %>% filter(year < energy.ATB_LATEST_YEAR)) %>%
      filter(year <= energy.ATB_LATEST_YEAR) %>%
      ungroup() %>%
      group_by(technology,input,case) %>%
      mutate(improvement.rate = if_else(is.na(improvement.rate),improvement.rate[year==energy.ATB_LATEST_YEAR],improvement.rate),
             improvement.max = if_else(is.na(improvement.max),improvement.max[year==energy.ATB_LATEST_YEAR],improvement.max)) %>%
      ungroup() -> L113.globaltech_cost_atb


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

    # Make sure df contains ATB base year
    if ( max(A23.globaltech_cost$year) < energy.ATB_BASE_YEAR) {
      A23.globaltech_cost <- A23.globaltech_cost %>%
        dplyr::filter(year == max(A23.globaltech_cost$year)) %>%
        dplyr::mutate(year = energy.ATB_BASE_YEAR) %>%
        dplyr::bind_rows(A23.globaltech_cost)
    }

    A23.globaltech_cost %>%
      group_by(technology, input, case) %>%
      filter(!is.na(value[year==energy.ATB_BASE_YEAR])) %>%
      ungroup() -> A23.globaltech_cost_keep

    # Filter out capital cost data from ATB if already present in A23.globaltech_capital_keep.
    # We also filter out battery technology because that will only be used in GCAM-USA.
    L113.globaltech_cost_atb %>%
      anti_join(A23.globaltech_cost_keep, by = c("technology", "input", "case")) -> L113.globaltech_cost_atb_temp

    # Merge ATB data with the original globaltech_capital file. Populate historical years with 2015 ATB data.
    # Properly format the column order and row order.
    # Here is where we allow for base-year selectivity. If energy.ATB_BASE_YEAR < energy.ATB_LATEST_YEAR,
    # then we filter for years < energy.ATB_LATEST_YEAR. We still, however, use the tech change parameters
    # from the latest ATB year
    A23.globaltech_cost %>%
      gather_years() %>%
      group_by(technology, input, case) %>%
      # get rid of values that will be overridden by user defined values in A23 files
      filter(is.na(value[year==energy.ATB_LATEST_YEAR])) %>%
      ungroup() %>%
      select(-value, -improvement.max, -improvement.rate) %>%
      distinct() %>%
      # Need to retain the values from the time series
      # Need to use left_join because the A23 file doesn't have all combinations
      left_join(L113.globaltech_cost_atb_temp, by = c("technology", "input", "case","year")) %>%
      # add in user defined values from A23 files
      bind_rows(A23.globaltech_cost_keep) %>%
      group_by(supplysector, subsector, technology, input, case) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      mutate(improvement.max = if_else(is.na(improvement.max),improvement.max[year == energy.ATB_LATEST_YEAR],improvement.max),
             improvement.rate = if_else(is.na(improvement.rate), improvement.rate[year == energy.ATB_LATEST_YEAR],improvement.rate)) %>%
      ungroup() %>%
      arrange(subsector, technology, input, case) %>%
      # Filter for costs < the users specified ATB base year
      filter(year <= energy.ATB_BASE_YEAR) %>%
      spread(year, value) -> L113.globaltech_cost

    # At this point, check to make sure that all of the technologies that are mapped to GCAM are included in this table:
    # Battery technology is separated out so we will exclude this
    if(length(setdiff(unique((atb_gcam_mapping %>% filter(technology != 'battery'))$technology),unique(L113.globaltech_cost$technology))) != 0) {
      stop('Missing ATB technologies. Check mapping in atb_gcam_mapping')
    }

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
               year = A23_years) %>%
      # Copy historical prices back for historical years:
      group_by(technology,input,case) %>%
      mutate(value = approx_fun(year,value,rule=2)) %>%
      # Keep only costs <= energy.ATB_BASE_YEAR (defined in constants.R)
      filter(year <= energy.ATB_BASE_YEAR) %>%
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
                     "energy/NREL_ATB_capital_2017",
                     "energy/NREL_ATB_capital_2019",
                     "energy/NREL_ATB_capital_2021",
                     "energy/NREL_ATB_capital_2022",
                     "energy/mappings/atb_gcam_mapping") ->
      L113.globaltech_capital_ATB

    L113.globaltech_capital_ATB_adv %>%
      add_title("2019 ATB-based capital cost structure for GCAM - Advanced improvement scenario") %>%
      add_units("1975$/kW") %>%
      add_comments("Will be used in place of A23.globaltech_capital_adv by relevant chunks") %>%
      add_precursors("energy/A23.globaltech_capital_adv",
                     "energy/Muratori_globaltech_capital_adv",
                     "energy/NREL_ATB_capital_2017",
                     "energy/NREL_ATB_capital_2019",
                     "energy/NREL_ATB_capital_2021",
                     "energy/NREL_ATB_capital_2022",
                     "energy/mappings/atb_gcam_mapping") ->
      L113.globaltech_capital_ATB_adv

    L113.globaltech_capital_ATB_low %>%
      add_title("2019 ATB-based capital cost structure for GCAM - Low improvement scenario") %>%
      add_units("1975$/kW") %>%
      add_comments("Will be used in place of A23.globaltech_capital_low by relevant chunks") %>%
      add_precursors("energy/A23.globaltech_capital_low",
                     "energy/Muratori_globaltech_capital_low",
                     "energy/NREL_ATB_capital_2017",
                     "energy/NREL_ATB_capital_2019",
                     "energy/NREL_ATB_capital_2021",
                     "energy/NREL_ATB_capital_2022",
                     "energy/mappings/atb_gcam_mapping",
                     "energy/mappings/ATB_tech_mapping") ->
      L113.globaltech_capital_ATB_low

    L113.elecS_globaltech_capital_battery_ATB %>%
      add_title("Battery technology costs to be used by GCAM USA") %>%
      add_units("1975$/kW") %>%
      add_comments("Will be used in place of A23.elecS_globaltech_non_energy_inputs by relevant chunks") %>%
      add_precursors("gcam-usa/A23.elecS_globaltech_non_energy_inputs",
                     "energy/NREL_ATB_capital_2017",
                     "energy/NREL_ATB_capital_2019",
                     "energy/NREL_ATB_capital_2021",
                     "energy/NREL_ATB_capital_2022",
                     "energy/mappings/atb_gcam_mapping",
                     "energy/mappings/ATB_tech_mapping") ->
      L113.elecS_globaltech_capital_battery_ATB

    L113.globaltech_OMfixed_ATB %>%
      add_title("2019 ATB-based capital cost structure for GCAM - Advanced improvement scenario") %>%
      add_units("1975$/kW") %>%
      add_comments("Will be used in place of A23.globaltech_capital_adv by relevant chunks") %>%
      add_precursors("energy/A23.globaltech_OMfixed",
                     "energy/Muratori_globaltech_OMfixed",
                     "energy/NREL_ATB_OMfixed_2017",
                     "energy/NREL_ATB_OMfixed_2019",
                     "energy/NREL_ATB_OMfixed_2021",
                     "energy/NREL_ATB_OMfixed_2022",
                     "energy/mappings/atb_gcam_mapping",
                     "energy/mappings/ATB_tech_mapping") ->
      L113.globaltech_OMfixed_ATB

    L113.globaltech_OMvar_ATB %>%
      add_title("2019 ATB-based capital cost structure for GCAM - Advanced improvement scenario") %>%
      add_units("1975$/kW") %>%
      add_comments("Will be used in place of A23.globaltech_capital_adv by relevant chunks") %>%
      add_precursors("energy/A23.globaltech_OMvar",
                     "energy/Muratori_globaltech_OMvar",
                     "energy/NREL_ATB_OMvar_2017",
                     "energy/NREL_ATB_OMvar_2019",
                     "energy/NREL_ATB_OMvar_2021",
                     "energy/NREL_ATB_OMvar_2022",
                     "energy/mappings/atb_gcam_mapping",
                     "energy/mappings/ATB_tech_mapping") ->
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
