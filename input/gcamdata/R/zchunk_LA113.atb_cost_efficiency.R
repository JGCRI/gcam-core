# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA113.atb_cost_efficiency
#'
#' Generates costs and efficiency input files based on 2019 ATB data and calibrated inputs and outputs for the electricity sector.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L113.Globaltech_capital_ATB}, \code{L113.Globaltech_capital_ATB_low}, \code{L113.Globaltech_capital_ATB_adv},
#' \code{L113.elecS_globaltech_battery_capital}, \code{L113.Globaltech_eff_ATB}. There was no corresponding file in the
#' original data system.
#' @details Includes ATB capital cost data as starting point, improvement rate and improvement max data generated based on cost pathway.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select pull left_join anti_join bind_rows arrange rename
#' @author AJS March 2019
module_energy_LA113.atb_cost_efficiency <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( FILE = "energy/A23.globaltech_capital_atb_raw",
              FILE = "energy/A23.globaltech_capital_atb_used",
              FILE = "energy/A23.globaltech_capital",
			  FILE = "energy/A23.globaltech_capital_old",
              FILE = "energy/A23.globaltech_capital_low",
              FILE = "energy/A23.globaltech_capital_adv",
              FILE = "energy/A23.globaltech_capital_adv_old",
              FILE = "energy/A23.globaltech_eff",
              FILE = "energy/A23.globaltech_eff_old",
			  FILE = "energy/A23.globaltech_eff_raw",
              FILE = "gcam-usa/A23.elecS_globaltech_non_energy_inputs",
              FILE = "energy/mappings/atb_gcam_mapping"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L113.Globaltech_capital_ATB",
             "L113.Globaltech_capital_ATB_low",
             "L113.Globaltech_capital_ATB_adv",
             "L113.elecS_globaltech_battery_capital",
             "L113.Globaltech_eff_ATB"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence global package checks
    supplysector <- subsector <- technology <- minicam.energy.input <- `1971` <- `1990` <- `2005` <- `2010` <- `2015` <- `2100` <- improvement.max <- improvement.rate <- improvement.shadow.technology <-
	`input-capital` <- fixed.charge.rate <- period <- lifetime <- steepness <- half.life <- variable.om <- fixed.om <- fcr <- capacity.factor <- atb.technology <- `2020` <- `2025` <- `230` <- `2035` <-
	`2040` <- `2045` <- `2050` <- NULL

    # Load required inputs
    A23.globaltech_capital_atb_raw <- get_data(all_data, "energy/A23.globaltech_capital_atb_raw")
    A23.globaltech_capital_atb_used <- get_data(all_data, "energy/A23.globaltech_capital_atb_used")
    A23.globaltech_capital <- get_data(all_data, "energy/A23.globaltech_capital")
	A23.globaltech_capital_old <- get_data(all_data, "energy/A23.globaltech_capital_old")
    A23.globaltech_capital_low <- get_data(all_data, "energy/A23.globaltech_capital_low")
    A23.globaltech_capital_adv <- get_data(all_data, "energy/A23.globaltech_capital_adv")
	A23.globaltech_capital_adv_old <- get_data(all_data, "energy/A23.globaltech_capital_adv_old")
    A23.globaltech_eff <- get_data(all_data, "energy/A23.globaltech_eff")
    A23.globaltech_eff_old <- get_data(all_data, "energy/A23.globaltech_eff_old")
	A23.globaltech_eff_raw <- get_data(all_data, "energy/A23.globaltech_eff_raw")
    A23.elecS_globaltech_non_energy_inputs <- get_data(all_data, "gcam-usa/A23.elecS_globaltech_non_energy_inputs")
    atb_gcam_mapping <- get_data(all_data, "energy/mappings/atb_gcam_mapping")

    # ===================================================

	# First we process the ATB baseline costs

		# Get appopriate technologies that will be used from all available ATB technologies
			A23.globaltech_capital_atb_used %>% 
				filter(Scenario == medium) -> A23.globaltech_capital_atb_used_medium
			A23.globaltech_capital_atb_raw %>%
				semi_join(A23.globaltech_capital_atb_used_medium, by=c("Technology","Resource"="Used_Resource")) -> A23.globaltech_capital_atb_raw_medium

		 # Now to do technology-specific calculations. To make these easier, we turn technologies into columns
			A23.globaltech_capital_atb_raw_medium %>% 
				select(-Resource) %>%
				gather(year,value,-Technology) %>%
				spread(Technology,value) -> A23.globaltech_capital_atb_raw_medium_spread

		 # Generate old costs for every technology in GCAM
			A23.globaltech_capital_old %>%
				fill_exp_decay_extrapolate(MODEL_YEARS) %>%
				rename(sector.name = supplysector, subsector.name = subsector, capital.overnight = value, input.capital = `input-capital`) %>%
				mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL)) %>%
				select(technology,year,capital.overnight) %>%
				filter(year %in% ATB_years) %>%
				spread(technology,capital.overnight) -> A23.globaltech_capital_old_spread
				
		 # Generate appropriate cost ratios from old data, which will be used to generate ATB approximations whenever a technology is absent in ATB
			A23.globaltech_capital_old_spread %>%
				mutate(CSP_ratio = CSP/CSP_storage,
					   Biomass_IGCC_ratio = `biomass (IGCC)`/`biomass (conv)`,
					   Biomass_CCS_ratio = `biomass (conv CCS)`/`biomass (conv)`,
					   Biomass_IGCC_CCS_ratio = `biomass (IGCC CCS)`/`biomass (IGCC)`,
					   Coal_IGCC_CCS_ratio = `coal (IGCC CCS)`/`coal (IGCC)`,
					   Refined_CC_ratio = `refined liquids (CC)`/ `gas (CC)`,
					   Refined_CT_ratio = `refined liquids (steam/CT)`/ `gas (steam/CT)`,
					   Refined_CC_CCS_ratio= `refined liquids (CC CCS)`/ `gas (CC CCS)`) -> A23.globaltech_capital_old_ratios

		 # Final Cost Calculations  
			# a. Battery = Quadruple storage costs in order to match lifetime and storage duration assumptions with GCAM
			A23.globaltech_capital_atb_raw_medium_spread %>%
				mutate(battery = storage_multiplier * Storage) -> A23.globaltech_capital_atb_raw_medium_a
			# b. Rooftop PV costs are the average of ATB Residential and Commercial costs
			A23.globaltech_capital_atb_raw_medium_a %>%
				mutate(rooftop_pv = Rooftop_Average * (`Solar - PV Dist. Comm` + `Solar - PV Dist. Res`)) -> A23.globaltech_capital_atb_raw_medium_b
			# c. PV technology costs are in DC and AC/DC conversion must be accounted for
			A23.globaltech_capital_atb_raw_medium_b %>%
				mutate(PV = ACDC_conversion * `Solar - Utility PV`, rooftop_pv = ACDC_conversion * rooftop_pv) -> A23.globaltech_capital_atb_raw_medium_c
			# d. CSP costs to be estimated based on the ratio of Old GCAM CSP and CSP Storage costs
			A23.globaltech_capital_atb_raw_medium_c %>%
				mutate(CSP =  A23.globaltech_capital_old_ratios$CSP_ratio * `Solar - CSP`) -> A23.globaltech_capital_atb_raw_medium_d
			# e. Biomass IGCC costs to be estimated based on the ratio of Old GCAM Biomass IGCC and Biomass conv costs
				A23.globaltech_capital_atb_raw_medium_d %>%
				mutate(`biomass (IGCC)` =  A23.globaltech_capital_old_ratios$Biomass_IGCC_ratio * Biopower) -> A23.globaltech_capital_atb_raw_medium_e
			# f. Biomass CCS costs to be estimated based on the ratio of Old GCAM Biomass CCS and Biomass conv costs
				A23.globaltech_capital_atb_raw_medium_e %>%
				mutate(`biomass (conv CCS)` =  A23.globaltech_capital_old_ratios$Biomass_CCS_ratio * Biopower) -> A23.globaltech_capital_atb_raw_medium_f
			# g. Biomass IGCC CCS costs to be estimated based on the ratio of Old GCAM Biomass IGCC CCS and Biomass IGCC costs
				A23.globaltech_capital_atb_raw_medium_f %>%
				mutate(`biomass (IGCC CCS)` =  A23.globaltech_capital_old_ratios$Biomass_IGCC_CCS_ratio * `biomass (IGCC)`) -> A23.globaltech_capital_atb_raw_medium_g
			# h. Coal IGCC CCS costs to be estimated based on the ratio of Old GCAM Coal IGCC CCS and Coal IGCC costs
				A23.globaltech_capital_atb_raw_medium_g %>%
				mutate(`coal (IGCC CCS)` =  A23.globaltech_capital_old_ratios$Coal_IGCC_CCS_ratio * `Coal IGCC`) -> A23.globaltech_capital_atb_raw_medium_h
			# i. Refided CC costs to be estimated based on the ratio of Old GCAM Refined CC and Gas CC costs
				A23.globaltech_capital_atb_raw_medium_h %>%
				mutate(`refined liquids (CC)` =  A23.globaltech_capital_old_ratios$Refined_CC_ratio * `Gas CC`) -> A23.globaltech_capital_atb_raw_medium_i
			# j. Refined CT to be estimated based on the ratio of Old GCAM Refined CT and Gas CT costs
				A23.globaltech_capital_atb_raw_medium_i %>%
				mutate(`refined liquids (steam/CT)` =  A23.globaltech_capital_old_ratios$Refined_CT_ratio * `Gas CT`) -> A23.globaltech_capital_atb_raw_medium_j
			# k. Refined CC CCS to be estimated based on the ratio of Old GCAM Refined CC CCS and Gas CC CCS costs
				A23.globaltech_capital_atb_raw_medium_j %>%
				mutate(`refined liquids (CC CCS)` =  A23.globaltech_capital_old_ratios$Refined_CC_CCS_ratio * `Gas CC CCS`) -> A23.globaltech_capital_atb_raw_medium_k
			# i. Calculate PV storage cost from PV cost and battery cost
				A23.globaltech_capital_atb_raw_medium_k %>%
				mutate(PV_storage =  PV + battery) -> A23.globaltech_capital_atb_raw_medium_l
			# j. Calculate Wind (onshore) storage cost from Wind onshore cost and battery cost
				A23.globaltech_capital_atb_raw_medium_l %>%
				mutate(wind_storage =  `Land-Based Wind` + battery) -> A23.globaltech_capital_atb_raw_medium_m
			
		 # Data formatting and mapping ATB technology names to GCAM ones 
				A23.globaltech_capital_atb_raw_medium_m %>%
				gather(atb.technology, value, - year) %>%
				right_join(atb_gcam_mapping, by="atb.technology") %>%
				filter(!is.na(year)) %>%
				select(technology,year,value) %>%
				spread(year,value) -> A23.globaltech_atb_capital

		# From ATB file - calculate the improvement rate from 2015 to 2035 and from 2035 to 2050
		A23.globaltech_atb_capital %>%
			mutate(improvement3515 = (`2035` - `2015`)/`2015`, improvement5035 = (`2050`- `2035`)/`2035`) -> A23.globaltech_atb_capital
		# Estimate 2100 value from 2050 value on the basis of the 2035-2050 improvement rate, and then calculate the 2015-2100 improvement rate
		A23.globaltech_atb_capital %>%
			mutate(`2100`  = `2050`* (1+improvement5035), improvement0015 = abs((`2100` - `2015`)/`2015`)) -> A23.globaltech_atb_capital
		# Set up a list of technologies that will be used for looping purposes
		A23.globaltech_atb_capital %>%
			select(technology) %>%
			pull() -> technology_list
		 # Setting up a functional form that replicates the function structure of cost generation. This has the following main components
		 # The 2015 ATB value
		 # improvement.max = improvement0015
		 # The 2035 ATB value, the last year of high improvement
		 # improvement.rate = a rate of improvement such that the function gets as close to the 2035 ATB value as possible.
		 # This needs to be set up for all technologies
		 # Specifying the improvement max as a fraction of the original (2015) value that can be attained through improvement
		A23.globaltech_atb_capital %>%
			mutate(improvement.max = 1 - improvement0015) -> A23.globaltech_atb_capital
		# Create baseline improvement rate values assuming average linear reduction
		A23.globaltech_atb_capital %>%
			mutate(improvement.rate.base = (`2015`- `2100`)/number_of_periods/`2015`) -> A23.globaltech_atb_capital
		# Create a function to replace the baseline improvement rate values with the updated ones
			replace_fraction <- function(data, tech, new_fraction) {
			  data %>%
				mutate(improvement.rate.base = replace(improvement.rate.base, technology == tech,
										  new_fraction))
			}
		# Create a function for all technologies to determine the improvement rate given the 2100 target that will lead to the correct capital cost in 2035
		# Year_Target is 2035, Year_Base is 2015.
		 f<- function(improvement.rate, tech) {
		A23.globaltech_atb_capital %>%
			filter(technology == tech) %>%
			mutate(check = `2015` * improvement.max + (`2015` - `2015` * improvement.max) *
									 (1.0 - improvement.rate) ^ (year_target - year_base) - `2035`) %>%
			pull(check) -> check

		check
		}
		# Loop the function for all technologies, using uniroot to ensure that the correct improvement rate is  found and then is replaced in the main table
		for (i in seq_along(technology_list)) {
				tech <- technology_list[i]

		solved.improvement.rate <- uniroot(f, c(0,1), tech)
		A23.globaltech_atb_capital %>%
			replace_fraction(tech, solved.improvement.rate$root) -> A23.globaltech_atb_capital

		}

		# Clean up the ATB table in order to properly merge with the old globaltech_capital table. Convert 2015 value to 1975 dollars.
		A23.globaltech_atb_capital %>%
			select(technology,`2015`,improvement.max,improvement.rate=improvement.rate.base) %>%
			mutate(improvement.max=round(improvement.max,energy.DIGITS_CAPACITY_FACTOR), improvement.rate=round(improvement.rate,energy.DIGITS_CAPACITY_FACTOR), `2015`= `2015`*gdp_deflator(1975, 2015)) -> A23.globaltech_atb_capital

		# Extract technologies from the original A23 file that have a valid 2015 data (i.e. not NAs). These technologies will take priority over ATB technologies if there's a conflict.
		A23.globaltech_capital %>%
			filter(!is.na(`2015`)) -> A23.globaltech_capital_keep

		# Filter out capital cost data from ATB if already present in A23.globaltech_capital_keep. We also filter out battery technology because that will only be used in GCAM-USA.
		A23.globaltech_atb_capital %>%
			anti_join(A23.globaltech_capital_keep, by="technology") %>%
			filter(technology != battery) -> A23.globaltech_atb_capital_medium

		# Merge ATB data with the globaltech_capital file. Populate historical years with 2015 ATB data. Proprerly format the column order and row order.
		A23.globaltech_capital %>%
			select(-`2015`,-improvement.max,-improvement.rate) %>%
			left_join(A23.globaltech_atb_capital_medium, by="technology") %>%
			bind_rows(A23.globaltech_capital_keep) %>%
			filter(!is.na(`2015`)) %>%
			mutate(`2010` = `2015`, `2005` = `2015`, `1990` = `2005`, `1971` = `2005`, improvement.max=as.numeric(improvement.max), improvement.rate=as.numeric(improvement.rate)) %>%
			select(supplysector,subsector,technology,`input-capital`,fixed.charge.rate,`1971`,`1990`,`2005`,`2010`,`2015`,improvement.max,improvement.rate,improvement.shadow.technology) %>%
			arrange(subsector,technology) -> L113.Globaltech_capital_ATB

	# Next we process the ATB advanced technology costs
	
	  # Get appopriate technologies that will be used from all available ATB technologies
	    A23.globaltech_capital_atb_used %>% 
		    filter(Scenario == low) -> A23.globaltech_capital_atb_used_low
	    A23.globaltech_capital_atb_raw %>%
		    semi_join(A23.globaltech_capital_atb_used_low, by=c("Technology","Resource"="Used_Resource")) -> A23.globaltech_capital_atb_raw_adv

	 # Now to do technology-specific calculations. To make these easier, we turn technologies into columns
		A23.globaltech_capital_atb_raw_adv %>% 
			select(-Resource) %>%
			gather(year,value,-Technology) %>%
			spread(Technology,value) -> A23.globaltech_capital_atb_raw_adv_spread

	 # Generate old costs for every technology in GCAM
		A23.globaltech_capital_adv_old %>%
			fill_exp_decay_extrapolate(MODEL_YEARS) %>%
			rename(sector.name = supplysector, subsector.name = subsector, capital.overnight = value, input.capital = `input-capital`) %>%
			mutate(capital.overnight = round(capital.overnight, energy.DIGITS_CAPITAL)) %>%
			select(technology,year,capital.overnight) %>%
			filter(year %in% ATB_years) %>%
			spread(technology,capital.overnight) -> A23.globaltech_capital_adv_old_spread
			
	 # Generate appropriate cost ratios from old data, which will be used to generate ATB approximations whenever a technology is absent in ATB
		A23.globaltech_capital_adv_old_spread %>%
			mutate(CSP_ratio = CSP/CSP_storage,
				   Biomass_IGCC_ratio = `biomass (IGCC)`/`biomass (conv)`,
				   Biomass_CCS_ratio = `biomass (conv CCS)`/`biomass (conv)`,
				   Biomass_IGCC_CCS_ratio = `biomass (IGCC CCS)`/`biomass (IGCC)`,
				   Coal_IGCC_CCS_ratio = `coal (IGCC CCS)`/`coal (IGCC)`,
				   Refined_CC_ratio = `refined liquids (CC)`/ `gas (CC)`,
				   Refined_CT_ratio = `refined liquids (steam/CT)`/ `gas (steam/CT)`,
				   Refined_CC_CCS_ratio= `refined liquids (CC CCS)`/ `gas (CC CCS)`) -> A23.globaltech_capital_adv_old_ratios

	 # Final Cost Calculations  
		# a. Battery = Quadruple storage costs in order to match lifetime and storage duration assumptions with GCAM
		A23.globaltech_capital_atb_raw_adv_spread %>%
			mutate(battery = storage_multiplier * Storage) -> A23.globaltech_capital_atb_raw_adv_a
		# b. Rooftop PV costs are the average of ATB Residential and Commercial costs
		A23.globaltech_capital_atb_raw_adv_a %>%
			mutate(rooftop_pv = Rooftop_Average * (`Solar - PV Dist. Comm` + `Solar - PV Dist. Res`)) -> A23.globaltech_capital_atb_raw_adv_b
		# c. PV technology costs are in DC and AC/DC conversion must be accounted for
		A23.globaltech_capital_atb_raw_adv_b %>%
			mutate(PV = ACDC_conversion * `Solar - Utility PV`, rooftop_pv = ACDC_conversion * rooftop_pv) -> A23.globaltech_capital_atb_raw_adv_c
		# d. CSP costs to be estimated based on the ratio of Old GCAM CSP and CSP Storage costs
		A23.globaltech_capital_atb_raw_adv_c %>%
			mutate(CSP =  A23.globaltech_capital_adv_old_ratios$CSP_ratio * `Solar - CSP`) -> A23.globaltech_capital_atb_raw_adv_d
		# e. Biomass IGCC costs to be estimated based on the ratio of Old GCAM Biomass IGCC and Biomass conv costs
			A23.globaltech_capital_atb_raw_adv_d %>%
			mutate(`biomass (IGCC)` =  A23.globaltech_capital_adv_old_ratios$Biomass_IGCC_ratio * Biopower) -> A23.globaltech_capital_atb_raw_adv_e
		# f. Biomass CCS costs to be estimated based on the ratio of Old GCAM Biomass CCS and Biomass conv costs
			A23.globaltech_capital_atb_raw_adv_e %>%
			mutate(`biomass (conv CCS)` =  A23.globaltech_capital_adv_old_ratios$Biomass_CCS_ratio * Biopower) -> A23.globaltech_capital_atb_raw_adv_f
		# g. Biomass IGCC CCS costs to be estimated based on the ratio of Old GCAM Biomass IGCC CCS and Biomass IGCC costs
			A23.globaltech_capital_atb_raw_adv_f %>%
			mutate(`biomass (IGCC CCS)` =  A23.globaltech_capital_adv_old_ratios$Biomass_IGCC_CCS_ratio * `biomass (IGCC)`) -> A23.globaltech_capital_atb_raw_adv_g
		# h. Coal IGCC CCS costs to be estimated based on the ratio of Old GCAM Coal IGCC CCS and Coal IGCC costs
			A23.globaltech_capital_atb_raw_adv_g %>%
			mutate(`coal (IGCC CCS)` =  A23.globaltech_capital_adv_old_ratios$Coal_IGCC_CCS_ratio * `Coal IGCC`) -> A23.globaltech_capital_atb_raw_adv_h
		# i. Refided CC costs to be estimated based on the ratio of Old GCAM Refined CC and Gas CC costs
			A23.globaltech_capital_atb_raw_adv_h %>%
			mutate(`refined liquids (CC)` =  A23.globaltech_capital_adv_old_ratios$Refined_CC_ratio * `Gas CC`) -> A23.globaltech_capital_atb_raw_adv_i
		# j. Refined CT to be estimated based on the ratio of Old GCAM Refined CT and Gas CT costs
			A23.globaltech_capital_atb_raw_adv_i %>%
			mutate(`refined liquids (steam/CT)` =  A23.globaltech_capital_adv_old_ratios$Refined_CT_ratio * `Gas CT`) -> A23.globaltech_capital_atb_raw_adv_j
		# k. Refined CC CCS to be estimated based on the ratio of Old GCAM Refined CC CCS and Gas CC CCS costs
			A23.globaltech_capital_atb_raw_adv_j %>%
			mutate(`refined liquids (CC CCS)` =  A23.globaltech_capital_adv_old_ratios$Refined_CC_CCS_ratio * `Gas CC CCS`) -> A23.globaltech_capital_atb_raw_adv_k
		# i. Calculate PV storage cost from PV cost and battery cost
			A23.globaltech_capital_atb_raw_adv_k %>%
			mutate(PV_storage =  PV + battery) -> A23.globaltech_capital_atb_raw_adv_l
		# j. Calculate Wind (onshore) storage cost from Wind onshore cost and battery cost
			A23.globaltech_capital_atb_raw_adv_l %>%
			mutate(wind_storage =  `Land-Based Wind` + battery) -> A23.globaltech_capital_atb_raw_adv_m
		
	 # Data formatting and mapping ATB technology names to GCAM ones 
			A23.globaltech_capital_atb_raw_adv_m %>%
			gather(atb.technology, value, - year) %>%
			right_join(atb_gcam_mapping, by="atb.technology") %>%
			filter(!is.na(year)) %>%
			select(technology,year,value) %>%
			spread(year,value) -> A23.globaltech_atb_capital_adv

		# From ATB file - calculate the improvement rate from 2015 to 2035 and from 2035 to 2050
		A23.globaltech_atb_capital_adv %>%
			mutate(improvement3515 = (`2035` - `2015`)/`2015`, improvement5035 = (`2050`- `2035`)/`2035`) -> A23.globaltech_atb_capital_adv
		# Estimate 2100 value from 2050 value on the basis of the 2035-2050 improvement rate, and then calculate the 2015-2100 improvement rate
		A23.globaltech_atb_capital_adv %>%
			mutate(`2100`  = `2050`* (1+improvement5035), improvement0015 = abs((`2100` - `2015`)/`2015`)) -> A23.globaltech_atb_capital_adv
		# Set up a list of technologies that will be used for looping purposes
		A23.globaltech_atb_capital_adv %>%
			select(technology) %>%
			pull() -> technology_list
		 # Setting up a functional form that replicates the function structure of cost generation. This has the following main components
		 # The 2015 ATB value
		 # improvement.max = improvement0015
		 # The 2035 ATB value, the last year of high improvement
		 # improvement.rate = a rate of improvement such that the function gets as close to the 2035 ATB value as possible.
		 # This needs to be set up for all technologies
		 # Specifying the improvement max as a fraction of the original (2015) value that can be attained through improvement
		A23.globaltech_atb_capital_adv %>%
			mutate(improvement.max = 1 - improvement0015) -> A23.globaltech_atb_capital_adv
		# Create baseline improvement rate values assuming average linear reduction
		A23.globaltech_atb_capital_adv %>%
			mutate(improvement.rate.base = (`2015`- `2100`)/number_of_periods/`2015`) -> A23.globaltech_atb_capital_adv
		# Create a function to replace the baseline improvement rate values with the updated ones
			replace_fraction <- function(data, tech, new_fraction) {
			  data %>%
				mutate(improvement.rate.base = replace(improvement.rate.base, technology== tech,
										  new_fraction))
			}
		# Create a function for all technologies to determine the improvement rate given the 2100 target that will lead to the correct capital cost in 2035
		# Year_Target is 2035, Year_Base is 2015.
		 f<- function(improvement.rate, tech) {
		A23.globaltech_atb_capital_adv %>%
			filter(technology== tech) %>%
			mutate(check = `2015` * improvement.max + (`2015` - `2015` * improvement.max) *
									 (1.0 - improvement.rate) ^ (year_target - year_base) - `2035`) %>%
			pull(check) -> check

		check
		}
		# Loop the function for all technologies, using uniroot to ensure that the correct improvement rate is  found and then is replaced in the main table
		for (i in seq_along(technology_list)) {
				tech <- technology_list[i]

		solved.improvement.rate <- uniroot(f, c(0,1), tech)
		A23.globaltech_atb_capital_adv %>%
			replace_fraction(tech, solved.improvement.rate$root) -> A23.globaltech_atb_capital_adv

		}

		# Clean up the ATB table in order to properly merge with the old globaltech_capital table. Convert 2015 value to 1975 dollars.
		A23.globaltech_atb_capital_adv %>%
			select(technology,`2015`,improvement.max,improvement.rate=improvement.rate.base) %>%
			mutate(improvement.max=round(improvement.max,energy.DIGITS_CAPACITY_FACTOR), improvement.rate=round(improvement.rate,energy.DIGITS_CAPACITY_FACTOR), `2015`= `2015`*gdp_deflator(1975, 2015)) -> A23.globaltech_atb_capital_adv

		# Extract technologies from the original A23 file that have a valid improvement rate and other data (i.e. not NAs). These technologies will take priority over ATB technologies if there's a conflict.
		A23.globaltech_capital_adv %>%
			filter(!is.na(`2015`)) -> A23.globaltech_capital_adv_keep

		# Filter out capital cost data from ATB if already present in A23.globaltech_capital_adv_keep. We also filter out battery technology because that will only be used in GCAM-USA.
		A23.globaltech_atb_capital_adv %>%
			anti_join(A23.globaltech_capital_adv_keep, by="technology") %>%
			filter(technology != battery)-> A23.globaltech_atb_capital_adv

		# Merge ATB data with the globaltech_capital file. Populate historical years with 2015 ATB data. Proprerly format the column order and row order.
		A23.globaltech_capital_adv %>%
			select(-`2015`,-improvement.max,-improvement.rate) %>%
			left_join(A23.globaltech_atb_capital_adv, by="technology") %>%
			bind_rows(A23.globaltech_capital_adv_keep) %>%
			filter(!is.na(`2015`)) %>%
			mutate(`2010` = `2015`, `2005` = `2015`, `1990` = `2005`, `1971` = `2005`, improvement.max=as.numeric(improvement.max), improvement.rate=as.numeric(improvement.rate)) %>%
			select(supplysector,subsector,technology,`input-capital`,fixed.charge.rate,`1971`,`1990`,`2005`,`2010`,`2015`,improvement.max,improvement.rate,improvement.shadow.technology) %>%
			arrange(subsector,technology) -> L113.Globaltech_capital_ATB_adv

	# Then we process the ATB low technology costs. These are simply the 2015 ATB values paired with the improvement rate and improvement max from the older low technology scenario.

		# Extract technologies from the original A23 file that have a valid improvement rate and other data (i.e. not NAs). These technologies will take priority over ATB technologies if there's a conflict.
		A23.globaltech_capital_low %>%
			filter(!is.na(`2015`)) -> A23.globaltech_capital_low_keep

		# Filter out capital cost data from ATB if already present in A23.globaltech_capital_low_keep. We also filter out battery technology because that will only be used in GCAM-USA.
		A23.globaltech_atb_capital %>%
			anti_join(A23.globaltech_capital_low_keep, by="technology") %>%
			filter(technology != battery) -> A23.globaltech_atb_capital_low

		# Merge ATB data with the globaltech_capital file. Populate historical years with 2015 ATB data. Proprerly format the column order and row order.
		A23.globaltech_capital_low %>%
			select(-`2015`) %>%
			left_join(A23.globaltech_atb_capital_low, by="technology") %>%
			bind_rows(A23.globaltech_capital_low_keep) %>%
			filter(!is.na(`2015`)) %>%
			mutate(`2010` = `2015`, `2005` = `2015`, `1990` = `2005`, `1971` = `2005`, improvement.max=as.numeric(improvement.max), improvement.rate=as.numeric(improvement.rate)) %>%
			select(supplysector,subsector,technology,`input-capital`,fixed.charge.rate,`1971`,`1990`,`2005`,`2010`,`2015`,improvement.max,improvement.rate,improvement.shadow.technology) %>%
			arrange(subsector,technology) -> L113.Globaltech_capital_ATB_low

	# Next we process the GCAM_USA battery costs. This is taken from ATB Baseline cost scenario as there's only set of battery costs that are used as GCAM-USA inputs

		# Filter out battery technology data
			A23.globaltech_atb_capital %>%
				filter(technology == battery) -> A23.globaltech_atb_battery

		# Prepare the table in the format which can be used by the exponential decay function in order to generate a full set of cost forecasts.
			A23.globaltech_atb_battery %>%
				mutate(`2010` = `2015`, `2005` = `2015`, `1990` = `2005`, `1971` = `2005`) -> A23.globaltech_capital_atb_battery

		# Use exponential decay function to extrapolate costs till 2100
			A23.globaltech_capital_atb_battery %>%
				fill_exp_decay_extrapolate(MODEL_YEARS) %>%
				rename(period = year, capital.cost = value) %>%
				select(technology,period,capital.cost) %>%
				mutate(capital.cost = round(capital.cost, energy.DIGITS_CAPITAL)) -> A23.globaltech_capital_atb_battery

		# Merge cost data with GCAM_USA battery cost structure file
			A23.elecS_globaltech_non_energy_inputs %>%
				select(-capital.cost) %>%
				left_join_error_no_match(A23.globaltech_capital_atb_battery, by =c("technology","period")) -> L113.elecS_globaltech_battery_capital

	# Finally we process the ATB Efficiencies
	
		# To do technology-specific calculations. To make these easier, we turn technologies into columns
		A23.globaltech_eff_raw %>% 
			gather(year,value,-atb.technology) %>%
			spread(atb.technology,value) -> A23.globaltech_eff_raw_spread
			
		 # Generate old efficiency for every fuel-based technology in GCAM. Filter out 2015 and 2100 data only.
			A23.globaltech_eff_old %>%
				filter(is.na(`2100`)) %>%
				fill_exp_decay_extrapolate(MODEL_YEARS) %>%
				rename(sector.name = supplysector, subsector.name = subsector, efficiency = value) %>%
				mutate(efficiency = round(efficiency, energy.DIGITS_EFFICIENCY)) %>%
				select(technology,year,efficiency) %>%
				filter(year %in% ATB_eff_years) %>%
				spread(technology,efficiency) -> A23.globaltech_eff_old_spread
				
		 # Generate appropriate efficiency ratios from old data, which will be used to generate approximations for other technologies
			A23.globaltech_eff_old_spread %>%
				mutate(Coal_IGCC_ratio = `coal (IGCC)`/`coal (conv pul)`,
					   Coal_CCS_ratio = `coal (conv pul CCS)`/`coal (conv pul)`,
					   Coal_IGCC_CCS_ratio = `coal (IGCC CCS)`/`coal (IGCC)`,
					   gas_CT_ratio = `gas (steam/CT)`/ `gas (CC)`,
					   gas_CC_CCS_ratio = `gas (CC CCS)`/ `gas (CC)`,
					   Biomass_ratio = `biomass (conv)`/ `coal (conv pul)`,
					   Biomass_IGCC_ratio = `biomass (IGCC)`/`biomass (conv)`,
					   Biomass_CCS_ratio = `biomass (conv CCS)`/`biomass (conv)`,
					   Biomass_IGCC_CCS_ratio = `biomass (IGCC CCS)`/`biomass (IGCC)`,
					   Refined_CC_ratio = `refined liquids (CC)`/ `gas (CC)`,
					   Refined_CT_ratio = `refined liquids (steam/CT)`/ `gas (steam/CT)`,
					   Refined_CC_CCS_ratio= `refined liquids (CC CCS)`/ `gas (CC CCS)`) -> A23.globaltech_eff_old_ratios

		 # Final Efficiency Calculations  
			# a. Coal IGCC efficiency to be estimated based on the ratio of Old GCAM Coal IGCC and Coal conv efficiency
			A23.globaltech_eff_raw_spread %>%
				mutate(`coal (IGCC)` =  A23.globaltech_eff_old_ratios$Coal_IGCC_ratio * `Coal new`) -> A23.globaltech_eff_raw_a
			# b. Coal  CCS efficiency to be estimated based on the ratio of Old GCAM Coal CCS and Coal conv efficiency
			A23.globaltech_eff_raw_a %>%
				mutate(`coal (conv pul CCS)` =  A23.globaltech_eff_old_ratios$Coal_CCS_ratio * `Coal new`) -> A23.globaltech_eff_raw_b
			# c. Coal IGCC CCS efficiency to be estimated based on the ratio of Old GCAM Coal IGCC CCS and Coal IGCC efficiency
				A23.globaltech_eff_raw_b %>%
				mutate(`coal (IGCC CCS)` =  A23.globaltech_eff_old_ratios$Coal_IGCC_CCS_ratio * `coal (IGCC)`) -> A23.globaltech_eff_raw_c
			# d. Gas CT to be estimated based on the ratio of Old GCAM Gas CT and Gas CC efficiency
			A23.globaltech_eff_raw_c %>%
				mutate(`gas (steam/CT)` =  A23.globaltech_eff_old_ratios$gas_CT_ratio * `Gas CC`) -> A23.globaltech_eff_raw_d
			# e. Gas CCS to be estimated based on the ratio of Old GCAM Gas CCS and Gas CC efficiency
			A23.globaltech_eff_raw_d %>%
				mutate(`gas (CC CCS)` =  A23.globaltech_eff_old_ratios$gas_CC_CCS_ratio * `Gas CC`) -> A23.globaltech_eff_raw_e
			# f. Biomass conv efficiency to be estimated based on the ratio of Old GCAM Biomass conv and Coal conv efficiency
			A23.globaltech_eff_raw_e %>%
				mutate(`biomass (conv)` =  A23.globaltech_eff_old_ratios$Biomass_ratio * `Coal new`) -> A23.globaltech_eff_raw_f
			# g. Biomass IGCC efficiency to be estimated based on the ratio of Old GCAM Biomass IGCC and Biomass conv efficiency
				A23.globaltech_eff_raw_f %>%
				mutate(`biomass (IGCC)` =  A23.globaltech_eff_old_ratios$Biomass_IGCC_ratio * `biomass (conv)`) -> A23.globaltech_eff_raw_g
			# h. Biomass CCS efficiency to be estimated based on the ratio of Old GCAM Biomass CCS and Biomass conv efficiency
				A23.globaltech_eff_raw_g %>%
				mutate(`biomass (conv CCS)` =  A23.globaltech_eff_old_ratios$Biomass_CCS_ratio * `biomass (conv)`) -> A23.globaltech_eff_raw_h
			# i. Biomass IGCC CCS efficiency to be estimated based on the ratio of Old GCAM Biomass IGCC CCS and Biomass IGCC efficiency
				A23.globaltech_eff_raw_h %>%
				mutate(`biomass (IGCC CCS)` =  A23.globaltech_eff_old_ratios$Biomass_IGCC_CCS_ratio * `biomass (IGCC)`) -> A23.globaltech_eff_raw_i
			# j. Refided CC efficiency to be estimated based on the ratio of Old GCAM Refined CC and Gas CC efficiency
				A23.globaltech_eff_raw_i %>%
				mutate(`refined liquids (CC)` =  A23.globaltech_eff_old_ratios$Refined_CC_ratio * `Gas CC`) -> A23.globaltech_eff_raw_j
			# k. Refined CT to be estimated based on the ratio of Old GCAM Refined CT and Gas CT efficiency
				A23.globaltech_eff_raw_j %>%
				mutate(`refined liquids (steam/CT)` =  A23.globaltech_eff_old_ratios$Refined_CT_ratio * `gas (steam/CT)`) -> A23.globaltech_eff_raw_k
			# l. Refined CC CCS to be estimated based on the ratio of Old GCAM Refined CC CCS and Gas CC CCS efficiency
				A23.globaltech_eff_raw_k %>%
				mutate(`refined liquids (CC CCS)` =  A23.globaltech_eff_old_ratios$Refined_CC_CCS_ratio * `gas (CC CCS)`) -> A23.globaltech_eff_raw_l

		 # Data formatting and mapping ATB technology names to GCAM ones 
				A23.globaltech_eff_raw_l %>%
				gather(atb.technology, value, - year) %>%
				left_join(atb_gcam_mapping, by="atb.technology") %>%
				select(technology,year,value) %>%
				spread(year,value) -> A23.globaltech_atb_eff
	
		# Set up a list of technologies
		A23.globaltech_atb_eff %>%
			select(technology) %>% pull() -> technology_list
		# Separate non-fossil/biomass technologies
		A23.globaltech_eff %>%
			filter (technology %in% technology_list) -> A23.globaltech_eff_fossil
		A23.globaltech_eff %>%
			filter (!technology %in% technology_list) -> A23.globaltech_eff_nonfossil
		# For fossils/biomass, get rid of shadow technologies, and create aggregate improvement max for CCS technologies
		A23.globaltech_eff_fossil %>%
			select(technology,improvement.max,improvement.shadow.technology) -> A23.globaltech_eff_fossil1

		A23.globaltech_eff_fossil1 %>%
			filter(is.na(improvement.shadow.technology)) %>%
			select(-improvement.shadow.technology) %>%
			rename(improvement.shadow.technology = technology) -> A23.globaltech_eff_fossil2

		A23.globaltech_eff_fossil1 %>%
			left_join(A23.globaltech_eff_fossil2, by="improvement.shadow.technology") %>%
			replace(., is.na(.), 0) %>%
			mutate(improvement.max = abs(improvement.max.x) + improvement.max.y) %>%
			select(technology,improvement.max) -> A23.globaltech_eff_fossil3

		A23.globaltech_eff_fossil %>%
			select(-improvement.max,-improvement.shadow.technology) %>%
			left_join_error_no_match(A23.globaltech_eff_fossil3, by="technology") %>%
			mutate(improvement.shadow.technology = NA) -> A23.globaltech_eff_fossil

		# We need to set up a functional form that replicates the function structure of cost generation. This has the following main components
		 # The 2015 value
		 # improvement.max = from fossil fuel table
		 # The 2100 value, the last projected year
		 # improvement.rate = a rate of improvement such that the function yields the correct 2100 value.
		# This needs to be set up for all technologies
		A23.globaltech_eff_fossil %>%
		  select(technology,improvement.max) %>%
		  right_join(A23.globaltech_atb_eff, by="technology") -> A23.globaltech_atb_eff
		# Create baseline improvement rate values assuming average linear reduction
		A23.globaltech_atb_eff %>%
			mutate(improvement.rate.base = abs((`2015` - `2100`)/number_of_periods/`2015`)) -> A23.globaltech_atb_eff
		# Create a function to replace the baseline improvement rate values with the updated ones
			replace_fraction <- function(data, tech, new_fraction) {
			  data %>%
				mutate(improvement.rate.base = replace(improvement.rate.base, technology == tech,
										  new_fraction))
			}
		# Create a function for all technologies to determine the improvement rate given the 2100 target that will lead to the correct efficiency in 2100
		 f<- function(improvement.rate, tech) {
		A23.globaltech_atb_eff %>%
			filter(technology == tech) %>%
			mutate(check = `2015` * improvement.max + (`2015` - `2015` * improvement.max) *
									 (1.0 - improvement.rate) ^ (year_target_eff - year_base) - `2100`) %>%
			pull(check) -> check

		check
		}
		# Loop the function for all technologies, using uniroot to ensure that the correct improvement rate is  found and then is replaced in the main table.
		# There are some cases when the root is too difficult so solve. So in those cases we resort to the default values.
		for (i in seq_along(technology_list)) {
				tech <- technology_list[i]

		solved.improvement.rate <- tryCatch(uniroot(f, c(0,1), tech), error= function(e) list(root=pull(subset(A23.globaltech_eff_fossil, technology==tech, select=improvement.rate))))
		A23.globaltech_atb_eff %>%
			replace_fraction(tech, solved.improvement.rate$root) -> A23.globaltech_atb_eff

		}

		# Clean up the file in order to properly merge with the globaltech_eff_fossil file
		A23.globaltech_atb_eff %>%
			rename(improvement.rate=improvement.rate.base) %>%
			mutate(improvement.rate=round(improvement.rate,energy.DIGITS_CAPACITY_FACTOR)) -> A23.globaltech_atb_eff

		# Extract fossi fuel technologies from the original A23 file that have a valid 2015 data (i.e. not NAs). These technologies will take priority over ATB technologies if there's a conflict.
		A23.globaltech_eff_fossil %>%
			filter(!is.na(`2015`)) -> A23.globaltech_eff_fossil_keep

		# Filter out capital cost data from ATB if already present in A23.globaltech_eff_fossil_keep.
		A23.globaltech_atb_eff %>%
			anti_join(A23.globaltech_eff_fossil_keep, by="technology") -> A23.globaltech_atb_eff

		# Merge new information with the globaltech_eff_fossil file
		A23.globaltech_eff_fossil %>%
			select(-`2015`,-`2100`,-improvement.rate,-improvement.max) %>%
			left_join_error_no_match(A23.globaltech_atb_eff, by="technology") -> A23.globaltech_eff_fossil_revised

		# Combine with the non-fossil file
		A23.globaltech_eff_fossil_revised %>%
			bind_rows(A23.globaltech_eff_nonfossil) -> A23.globaltech_eff

		# Replace NAs and fix numerical columns being read as characters

		A23.globaltech_eff %>%
			replace(.,is.na(.)," ") %>%
			mutate(improvement.max=as.numeric(improvement.max), improvement.rate=as.numeric(improvement.rate))-> L113.Globaltech_eff_ATB

    # Produce outputs

    L113.Globaltech_capital_ATB %>%
      add_title("2019 ATB-based capital cost structure for GCAM") %>%
      add_units("1975$/kW") %>%
      add_comments("Will be used in place of A23.globaltech_capital by relevant chunks") %>%
      add_precursors("energy/A23.globaltech_capital", "energy/A23.globaltech_capital_atb_raw", "energy/A23.globaltech_capital_atb_used", "energy/A23.globaltech_capital_old", "energy/mappings/atb_gcam_mapping") ->
      L113.Globaltech_capital_ATB

    L113.Globaltech_capital_ATB_low %>%
      add_title("2019 ATB-based capital cost structure for GCAM - Low improvement scenario") %>%
      add_units("1975$/kW") %>%
      add_comments("Will be used in place of A23.globaltech_capital_low by relevant chunks") %>%
      add_precursors("energy/A23.globaltech_capital_low", "energy/A23.globaltech_capital_atb_raw", "energy/A23.globaltech_capital_atb_used", "energy/A23.globaltech_capital_old") ->
      L113.Globaltech_capital_ATB_low

    L113.Globaltech_capital_ATB_adv %>%
      add_title("2019 ATB-based capital cost structure for GCAM - Advanced improvement scenario") %>%
      add_units("1975$/kW") %>%
      add_comments("Will be used in place of A23.globaltech_capital_adv by relevant chunks") %>%
      add_precursors("energy/A23.globaltech_capital_adv", "energy/A23.globaltech_capital_atb_raw", "energy/A23.globaltech_capital_atb_used", "energy/A23.globaltech_capital_adv_old", "energy/mappings/atb_gcam_mapping") ->
      L113.Globaltech_capital_ATB_adv

    L113.elecS_globaltech_battery_capital %>%
      add_title("Battery technology costs to be used by GCAM USA") %>%
      add_units("1975$/kW") %>%
      add_comments("Will be used in place of A23.elecS_globaltech_non_energy_inputs by relevant chunks") %>%
      add_precursors("energy/A23.globaltech_capital_atb_raw", "energy/A23.globaltech_capital_atb_used", "gcam-usa/A23.elecS_globaltech_non_energy_inputs") ->
      L113.elecS_globaltech_battery_capital

    L113.Globaltech_eff_ATB %>%
      add_title("Updated technology efficiency numbers partially based on ATB for GCAM") %>%
      add_units("unitless") %>%
      add_comments("Will be used in place of A23.globaltech_eff by relevant chunks") %>%
      add_precursors("energy/A23.globaltech_eff_raw", "energy/A23.globaltech_eff", "energy/A23.globaltech_eff_old", "energy/mappings/atb_gcam_mapping") ->
      L113.Globaltech_eff_ATB

    return_data(L113.Globaltech_capital_ATB, L113.Globaltech_capital_ATB_low, L113.Globaltech_capital_ATB_adv, L113.elecS_globaltech_battery_capital, L113.Globaltech_eff_ATB)
  } else {
    stop("Unknown command")
  }
}
