#' module_gcamusa_L272.elc_nonghg_USA
#'
#' Calculate Non-GHG emissions parameters for electricity technologies in the USA
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2721.nonghg_elec_tech_coeff_USA},\code{L2721.elec_MAC_CSAPR}. The corresponding file in the
#' original data system was \code{L272.elc_nonghg_USA.R} (gcam-usa level2).
#' @details This chunk calculates Non-GHG emissions parameters for electricity technologies in the USA and the electricity MAC curve
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AJS and BY August 2019, MAW March 2022
  module_gcamusa_L272.elc_nonghg_USA <- function(command, ...) {
    if(command == driver.DECLARE_INPUTS) {
      return(c(FILE = "gcam-usa/emissions/BC_OC_assumptions",
               FILE = "gcam-usa/emissions/BCOC_PM25_ratios",
               FILE = "gcam-usa/emissions/EPA_state_egu_emission_factors_ktPJ_post2015",
               FILE = "gcam-usa/emissions/EPA_state_egu_emission_factors_ktPJ",
               FILE = "gcam-usa/states_subregions",
			         "L123.in_EJ_state_elec_F",
			         "L270.nonghg_tg_state_elec_F_Yb",
			         FILE = "gcam-usa/A23.elecS_tech_mapping_cool",
			         FILE = "gcam-usa/A23.elecS_tech_availability",
			         "L2234.StubTechMarket_elecS_USA",
			         "L2233.StubTechProd_elecS_cool_USA"))
    } else if(command == driver.DECLARE_OUTPUTS) {
      return(c("L272.nonghg_elec_tech_coeff_USA"))
    } else if(command == driver.MAKE) {

    # silence package check
    supplysector <- subsector <- technology <- elec_supplysector <- elec_subsector <-
	elec_technology <- BC_fraction <- OC_fraction <- Non.CO2 <- emiss.coef <-
	State <- ElectricityFuel <- Reduction <- Cost <- state <- state_name <-
	grid_region <- CSAPR <- sector <- fuel <- year <- value <- output <- input <-
	stub.technology <- region <- input.emissions <- fuel_input <- emiss.coef.x <-
	emiss.coef.y <- supplysector.y <- subsector.y <- emiss.coef <- NULL


  all_data <- list(...)[[1]]

  # Load required inputs
  BC_OC_assumptions <- get_data(all_data, "gcam-usa/emissions/BC_OC_assumptions", strip_attributes = TRUE)
  BCOC_PM25_ratios <- get_data(all_data, "gcam-usa/emissions/BCOC_PM25_ratios") %>%
    # removing columns we do not use, and sectors that aren't mapped to GCAM sectors
    select( -c( "Region", "Gains_Sector" ) ) %>%
    filter( !is.na( sector ) )
  EPA_state_egu_emission_factors_ktPJ_post2015 <- get_data(all_data, "gcam-usa/emissions/EPA_state_egu_emission_factors_ktPJ_post2015", strip_attributes = TRUE)
  EPA_state_egu_emission_factors_ktPJ <- get_data(all_data, "gcam-usa/emissions/EPA_state_egu_emission_factors_ktPJ", strip_attributes = TRUE)
  states_subregions <- get_data(all_data, "gcam-usa/states_subregions", strip_attributes = TRUE)
  L123.in_EJ_state_elec_F <- get_data(all_data, "L123.in_EJ_state_elec_F", strip_attributes = TRUE)
  L270.nonghg_tg_state_elec_F_Yb <- get_data(all_data, "L270.nonghg_tg_state_elec_F_Yb", strip_attributes = TRUE)
  L2234.StubTechMarket_elecS_USA <- get_data(all_data, 'L2234.StubTechMarket_elecS_USA', strip_attributes = TRUE)
  L2233.StubTechProd_elecS_cool_USA <- get_data(all_data, 'L2233.StubTechProd_elecS_cool_USA', strip_attributes = TRUE)
  A23.elecS_tech_mapping_cool <- get_data(all_data, 'gcam-usa/A23.elecS_tech_mapping_cool', strip_attributes = TRUE)
  A23.elecS_tech_availability <- get_data(all_data, 'gcam-usa/A23.elecS_tech_availability', strip_attributes = TRUE)

  load_segments <- unique(A23.elecS_tech_mapping_cool$Electric.sector)

  #Electric s/s/t mapping file
  A23.elecS_tech_mapping_cool %>%
    anti_join(A23.elecS_tech_availability,
              by = c("Electric.sector.technology" = "stub.technology")) %>%
    mutate(Electric.sector = factor(Electric.sector, levels = load_segments),
           Electric.sector = as.character(Electric.sector)) %>%
    filter( Electric.sector != "elect_td_bld" ) ->
    A23.elecS_tech_mapping

  ### Electricity technology emission coefficients in base years

  # First clean up the fuel inputs
  agg_fuel_input_elec_Yb <- L123.in_EJ_state_elec_F %>%
    # filter for the base years, which NEI input emissions are being applied for
	  filter(year %in% MODEL_BASE_YEARS) %>%
  	rename(fuel_input = value)

	# Take the table containing the emissions, match on the fuel inputs, and compute the emission coefficients
	agg_nonghg_elec_emiss_coeffs_F_Yb <- L270.nonghg_tg_state_elec_F_Yb %>%
	  # filter for the base years
	  filter(year %in% MODEL_BASE_YEARS) %>%
		rename(input.emissions = value) %>%
		left_join_error_no_match(agg_fuel_input_elec_Yb, by = c("state", "fuel", "year")) %>%
		mutate(emiss.coef = input.emissions / fuel_input) %>%
		select(state, fuel, Non.CO2, year, emiss.coef, fuel_input)
		###MISSING VALUES: NAs and Infs generated. Keep these for now

	# Assign emission coefficients to the electricity technologies in the base years
	L272.nonghg_elec_tech_coeff_Yb_USA.NAs <- L2233.StubTechProd_elecS_cool_USA %>%
	  select(-c(calOutputValue, share.weight.year, subs.share.weight, tech.share.weight)) %>%
	  filter(year %in% MODEL_BASE_YEARS,
	         # only retain entries for subsectors that we have fuel consumption for
	         subsector0 %in% agg_fuel_input_elec_Yb$fuel) %>%
	  # Add pollutant column
		repeat_add_columns(tibble::tibble(Non.CO2=unique(L270.nonghg_tg_state_elec_F_Yb$Non.CO2))) %>%
  	# Match on emission factors, must use left_join as there are several NAs - technologies that do not have emissions
		left_join(agg_nonghg_elec_emiss_coeffs_F_Yb, by = c("region" = "state", "subsector0" = "fuel", "Non.CO2", "year" ) ) %>%
	  rename( stub.technology = technology ) %>%
		select(region, supplysector, subsector0, subsector, stub.technology, year, Non.CO2, emiss.coef)

	# Generate national median emissions factors for base years
	# Remove NAs so as to not skew the median
	L272.nonghg_elec_tech_coeff_Yb_USA.median.true <- L272.nonghg_elec_tech_coeff_Yb_USA.NAs %>%
	  filter(!is.na(emiss.coef)) %>%
	  group_by(year, Non.CO2, supplysector, subsector0, subsector, stub.technology) %>%
	  summarise(emiss.coef = median(emiss.coef)) %>%
	  ungroup() %>%
	  rename(nationalEF = emiss.coef)

	# Some year / pollutant / sector / subsector / tech are NA for all entries, and should be set to 0
	L272.nonghg_elec_tech_coeff_Yb_USA.median.skewed <- L272.nonghg_elec_tech_coeff_Yb_USA.NAs %>%
	  replace_na(list(emiss.coef = 0)) %>%
	  group_by(year, Non.CO2, supplysector, subsector0, subsector, stub.technology) %>%
	  summarise(emiss.coef = median(emiss.coef)) %>%
	  ungroup() %>%
	  rename(nationalEF = emiss.coef)

	# We want to join these tables so that only the entries not in median.true are retained from median.skewed
	# These all have EFs of 0
	L272.nonghg_elec_tech_coeff_Yb_USA.median <- L272.nonghg_elec_tech_coeff_Yb_USA.median.skewed %>%
	  anti_join( L272.nonghg_elec_tech_coeff_Yb_USA.median.true, by=c("year", "Non.CO2", "supplysector", "subsector0", "subsector", "stub.technology") ) %>%
	  # rebind to median.true
	  bind_rows(L272.nonghg_elec_tech_coeff_Yb_USA.median.true)

	# Replace all emissions factors above a given value (20 * median) or that are NAs with the national median emissions factor for that year, non.CO2, and technology
	L272.nonghg_elec_tech_coeff_Yb_USA.noBCOC <- L272.nonghg_elec_tech_coeff_Yb_USA.NAs %>%
	  left_join_error_no_match(L272.nonghg_elec_tech_coeff_Yb_USA.median, by = c("year", "Non.CO2", "supplysector", "subsector0", "subsector", "stub.technology")) %>%
	  # create a new column that has the threshold value
	  mutate( threshold = nationalEF * 20,
	          emiss.coef = if_else(emiss.coef > threshold | is.na(emiss.coef), nationalEF, emiss.coef)) %>%
	  select(region, Non.CO2, year, supplysector, subsector0, subsector, stub.technology, emiss.coef)

	# Use fractions of PM2.5 to calculate BC/OC emissions.
	# We need to modify the BC_OC_assumptions table, as the BCOC_PM25_ratios table has updated values that are time dependent
	# If there are sector/subsector/tech combos that are in BCOC_PM25_ratios, we want to replace those entries in
	# the BC_OC_assumptions table. We also need to extend the data.
	# Extrapolate the data to future model years, and format the table
	BCOC_PM25_ratios_ext <- BCOC_PM25_ratios %>%
	  gather_years() %>%
	  complete(nesting(Parameter,sector,subsector,technology), year = MODEL_YEARS) %>%
	  # extrapolate missing years
	  group_by(Parameter,sector,subsector,technology) %>%
	  mutate(value = approx_fun(year, value, rule = 2)) %>%
	  ungroup() %>%
	  spread( Parameter, value ) %>%
	  rename( "BC_fraction" = `BC Fraction`,
	          "OC_fraction" = `OC Fraction`)

	BC_OC_assumptions_ext <- BC_OC_assumptions %>%
	  mutate( year = min( MODEL_BASE_YEARS ) ) %>%
	  complete(nesting(sector,subsector,technology, BC_fraction, OC_fraction), year = MODEL_YEARS)

	# Join the tables, keeping values from BC_OC_assumptions_ext that do not appear in BCOC_PM25_ratios
	BC_OC_assumptions_years <- BC_OC_assumptions_ext %>%
	  anti_join( BCOC_PM25_ratios_ext, by = c("sector", "subsector", "technology", "year") ) %>%
	  # bind to BCOC_PM25_assumptions
	  bind_rows( BCOC_PM25_ratios_ext )

	# Compute BC and OC EFs based off of PM2.5
	L272.nonghg_elec_tech_coeff_Yb_USA_all_techs <- compute_BC_OC_elc(L272.nonghg_elec_tech_coeff_Yb_USA.noBCOC, BC_OC_assumptions_years) %>%
	# at this point we have some NA EFs due to renewable technologies not having BC/OC emissions
	# we can omit these
	na.omit()

	# Remove IGCC and CCS technologies in the base years and filter out 1975
	L272.nonghg_elec_tech_coeff_Yb_USA <- L272.nonghg_elec_tech_coeff_Yb_USA_all_techs %>%
	  filter( !grepl( 'IGCC|CCS', subsector ),
	          year > min( MODEL_BASE_YEARS ) )


	### Electricity technology emission coefficients in future vintages

	# Create a table containing future emission factors, starting with exhaustive list of all technologies
	L272.nonghg_elec_tech_coeff_Yf_USA.No.NH3 <- L2233.StubTechProd_elecS_cool_USA %>%
	  select(-c(calOutputValue, share.weight.year, subs.share.weight, tech.share.weight, year)) %>%
	  # only retain entries for subsectors that we have fuel consumption for
	  filter(subsector0 %in% agg_fuel_input_elec_Yb$fuel) %>%
	  # add future years, we only need the minimum future year, as the EF will pass forward in the model
	  repeat_add_columns(tibble::tibble(year = unique(min(MODEL_FUTURE_YEARS)))) %>%
	  # add pollutants
		repeat_add_columns(tibble::tibble(Non.CO2 = unique(L270.nonghg_tg_state_elec_F_Yb$Non.CO2))) %>%
	  # must use left_join as there are several NAs - technologies that do not have emissions
	  left_join(A23.elecS_tech_mapping, by = c("supplysector" = "Electric.sector",
	                                           "subsector0" = "subsector",
	                                           "subsector" = "Electric.sector.technology",
	                                           "technology" = "to.technology")) %>%
	  # emission factors from EPA for electric power plants built post-2015
		left_join(EPA_state_egu_emission_factors_ktPJ_post2015, by=c("subsector0" = "fuel", "technology.y" = "technology", "Non.CO2")) %>%
	  rename(emiss.coef = emiss.coeff,
	         stub.technology = technology) %>%
	  select( c( region, supplysector, subsector0, subsector, stub.technology, year, Non.CO2, emiss.coef ) )

	###Filter out all NH3 NA emission factors which will replaced by EPA-ORD data, and are not included in the post2015 table
	L272.nonghg_elec_tech_coeff_Yf_USA.NA.NH3 <- L272.nonghg_elec_tech_coeff_Yf_USA.No.NH3 %>%
		filter(is.na(emiss.coef) & Non.CO2=="NH3")

	#Get NH3 emission factors from EPA-ORD
	L272.nonghg_elec_tech_coeff_Yf_USA_NH3_EPA <- EPA_state_egu_emission_factors_ktPJ %>%
	  # Convert to long format
	  tidyr::gather(variable, value, -state_name, -fuel) %>%
	  separate(variable, into = c("year", "Non.CO2"), sep = "_") %>%
	  mutate(year = as.numeric(year),
	  # NOTE: for now change oil to refined liquids
	         fuel = gsub("oil", "refined liquids", fuel)) %>%
	  # state code & select relevant columns
	  left_join_error_no_match(states_subregions %>% select(state, state_name),
	                           by = c("state_name")) %>%
	  mutate(sector = "elec_heat") %>%
	  filter(year %in% MODEL_FUTURE_YEARS,
	         Non.CO2 == "NH3") %>%
	  select(state, sector, fuel, Non.CO2, year, value)

	L272.nonghg_elec_tech_coeff_Yf_USA.NH3 <- L272.nonghg_elec_tech_coeff_Yf_USA.NA.NH3 %>%
	  # must use left_join as there are several NAs - technologies that do not have emissions
		left_join(L272.nonghg_elec_tech_coeff_Yf_USA_NH3_EPA, by=c("region" = "state", "subsector0" = "fuel" ,"Non.CO2", "year")) %>%
		select(-emiss.coef, -sector) %>%
		rename(emiss.coef = value)

	###Add these back to the L272.nonghg_elec_tech_coeff_Yf_USA.No.NH3 table
	L272.nonghg_elec_tech_coeff_Yf_USA.NAs <- L272.nonghg_elec_tech_coeff_Yf_USA.No.NH3 %>%
	  #filter out NH3 because this dataframe has EFs as NA for all NH3 entries
		filter(Non.CO2 != "NH3") %>%
	  #bind to table that has EFs for NH3
		bind_rows(L272.nonghg_elec_tech_coeff_Yf_USA.NH3)

	# Generate national median emissions factors for future years
	# Remove NAs so as to not skew the median
	L272.nonghg_elec_tech_coeff_Yf_USA.median.true <- L272.nonghg_elec_tech_coeff_Yf_USA.NAs %>%
	  filter(!is.na(emiss.coef)) %>%
	  group_by(year, Non.CO2, supplysector, subsector0, subsector, stub.technology) %>%
	  summarise(emiss.coef = median(emiss.coef)) %>%
	  ungroup() %>%
	  rename(nationalEF = emiss.coef)

	# Some year / pollutant / sector / subsector / tech are NA for all entries, and should be set to 0
	L272.nonghg_elec_tech_coeff_Yf_USA.median.skewed <- L272.nonghg_elec_tech_coeff_Yf_USA.NAs %>%
	  replace_na(list(emiss.coef = 0)) %>%
	  group_by(year, Non.CO2, supplysector, subsector0, subsector, stub.technology) %>%
	  summarise(emiss.coef = median(emiss.coef)) %>%
	  ungroup() %>%
	  rename(nationalEF = emiss.coef)

	# We want to join these tables so that only the entries not in median.true are retained from median.skewed
	# These all have EFs of 0
	L272.nonghg_elec_tech_coeff_Yf_USA.median <- 	L272.nonghg_elec_tech_coeff_Yf_USA.median.skewed %>%
	  anti_join( 	L272.nonghg_elec_tech_coeff_Yf_USA.median.true, by=c("year", "Non.CO2", "supplysector", "subsector0", "subsector", "stub.technology") ) %>%
	  # rebind to median.true
	  bind_rows(	L272.nonghg_elec_tech_coeff_Yf_USA.median.true)

	# Replace all emissions factors above a given value (20 * median) or that are NAs with the national median emissions factor for that year, non.CO2, and technology
	L272.nonghg_elec_tech_coeff_Yf_USA.noBCOC <- L272.nonghg_elec_tech_coeff_Yf_USA.NAs %>%
	  left_join_error_no_match(L272.nonghg_elec_tech_coeff_Yf_USA.median, by = c("year", "Non.CO2", "supplysector", "subsector0", "subsector", "stub.technology")) %>%
	  # create a new column that has the threshold value
	  mutate( threshold = nationalEF * 20,
	          emiss.coef = if_else(emiss.coef > threshold | is.na(emiss.coef), nationalEF, emiss.coef)) %>%
	  select(region, Non.CO2, year, supplysector, subsector0, subsector, stub.technology, emiss.coef) %>%
	  mutate(emiss.coef = if_else(is.infinite(emiss.coef), 1, emiss.coef))

	 # Use fractions of PM2.5 to calculate BC/OC emissions.
	L272.nonghg_elec_tech_coeff_Yf_USA <- compute_BC_OC_elc(L272.nonghg_elec_tech_coeff_Yf_USA.noBCOC, BC_OC_assumptions_years) %>%
	   # at this point we have some NA EFs due to renewable technologies not having BC/OC emissions
	   # we can omit these
	   na.omit()

	### Electricity emission factors in all years, for all technologies
	L272.nonghg_elec_tech_coeff_USA_all_techs <- bind_rows(L272.nonghg_elec_tech_coeff_Yb_USA, L272.nonghg_elec_tech_coeff_Yf_USA) %>%
		distinct(region, supplysector, subsector0, subsector, stub.technology, year, Non.CO2, emiss.coef) %>%
	  select(region, supplysector, subsector0, subsector, stub.technology, year, Non.CO2, emiss.coef)

	# Create a table that has region/supplysector/subsector0/subsector/technology
	# that should be kept for vintage and retire subsectors
	# Remove technologies from both base and future years
	# that have a calOutput = 0 in ALL base years, for vintage and retire subsectors
	nonzero_cal_output <- L2233.StubTechProd_elecS_cool_USA %>%
	  # filtering for the retire and vintage subsectors (these contain 19XX or 20XX)
	  filter( grepl( '19|20', subsector )) %>%
	  # making a sum column, if sum = 0, that region/supplysector/subsector0/subsector/technology should be removed
	  group_by( region, supplysector, subsector0, subsector, technology ) %>%
	  mutate( sum = sum( calOutputValue ) ) %>%
	  ungroup() %>%
	  # do not keep the entries that have sum = 0
	  filter( sum > 0 ) %>%
	  select( -c( sum, year, share.weight.year, subs.share.weight, tech.share.weight, calOutputValue ) ) %>%
	  distinct( region, supplysector, subsector0, subsector, technology )

	# Subset the vintage and retire subsectors
	L272.vintage_retire_techs <- L272.nonghg_elec_tech_coeff_USA_all_techs %>%
	  # filtering for the retire and vintage subsectors (these contain 19XX or 20XX)
	  filter( grepl( '19|20', subsector ) ) %>%
	  rename( technology = stub.technology )

	# Subset the non vintage and retire subsectors
	L272.regular_techs <- L272.nonghg_elec_tech_coeff_USA_all_techs %>%
	  # filtering out the retire and vintage subsectors (these contain 19XX or 20XX)
	  filter( !grepl( '19|20', subsector ) ) %>%
	  rename( technology = stub.technology )

	# Keep only the region/supplysector/subsector0/subsector/technology with nonzero calOutput
	L272.vintage_retire_techs_nonzero <- nonzero_cal_output %>%
	  left_join( L272.vintage_retire_techs, by = c("supplysector", "subsector0", "subsector", "region", "technology"))

	# Bind the two technologies together back together
	L272.nonghg_elec_tech_coeff_USA_no_driver <- bind_rows( L272.regular_techs, L272.vintage_retire_techs_nonzero )

	# Add an input name column to drive emissions
	L272.nonghg_elec_tech_coeff_USA <- L272.nonghg_elec_tech_coeff_USA_no_driver %>%
	  # L2234.StubTechMarket_elecS has the fuel inputs that should be used to drive emissions
	  left_join_error_no_match( L2234.StubTechMarket_elecS_USA %>% select( c( "supplysector", "subsector", "stub.technology",
	                                                                             "minicam.energy.input" ) ) %>%
	                              distinct( supplysector, subsector, minicam.energy.input ),
	                            by = c("supplysector", "subsector0" = "subsector") ) %>%
	  #rename to input.name to match header
	  rename( input.name = minicam.energy.input) %>%
	  #change SO2 to SO2_1
	  mutate( Non.CO2 = gsub( "SO2", "SO2_1", Non.CO2 ) ) %>%
	  # remove 1975
	  filter( year > min(MODEL_BASE_YEARS) ) %>%
	  # distinct to remove any potential duplicate rows
	  distinct()


	# Produce outputs

    L272.nonghg_elec_tech_coeff_USA %>%
      add_title("Non-GHG emissions parameters for electricity technologies in the USA") %>%
      add_units("Tg/EJ") %>%
      add_comments("Non-GHG emissions parameters for electricity technologies in the USA") %>%
      add_legacy_name("L272.nonghg_elec_tech_coeff_USA") %>%
      add_precursors("gcam-usa/emissions/BC_OC_assumptions",
                     "gcam-usa/emissions/BCOC_PM25_ratios",
                     "gcam-usa/emissions/EPA_state_egu_emission_factors_ktPJ_post2015",
                     "gcam-usa/emissions/EPA_state_egu_emission_factors_ktPJ",
                     "gcam-usa/states_subregions",
                     "L123.in_EJ_state_elec_F",
                     "L270.nonghg_tg_state_elec_F_Yb",
                     "gcam-usa/A23.elecS_tech_mapping_cool",
                     "gcam-usa/A23.elecS_tech_availability",
                     "L2234.StubTechMarket_elecS_USA",
                     "L2233.StubTechProd_elecS_cool_USA") ->
      L272.nonghg_elec_tech_coeff_USA

    return_data(L272.nonghg_elec_tech_coeff_USA)
  } else {
    stop("Unknown command")
  }
}
