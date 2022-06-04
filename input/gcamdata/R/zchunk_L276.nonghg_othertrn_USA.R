#' module_gcamusa_L276.nonghg_othertrn_USA
#'
#' Generates GCAM-USA input files of Non-GHG input emissions parameters for domestic aviation, ships, and rail.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L276.nonghg_othertrn_tech_coeff_USA}. The corresponding file in the
#' original data system was \code{L276.othertrn_nonghg_USA.R} (gcam-usa level2).
#' @details This chunk generates Non-GHG input emissions parameters for domestic aviation, ships, and rail in the USA.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author MAW March 2022
module_gcamusa_L276.nonghg_othertrn_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-usa/emissions/BC_OC_assumptions",
             FILE="gcam-usa/emissions/BCOC_PM25_ratios",
             FILE ="gcam-usa/emissions/IMO_Shipping_EF",
             "L270.nonghg_tg_state_othertrn_F_Yb",
             "L254.StubTranTech_USA",
             "L254.StubTranTechCalInput_USA",
             "L201.en_pol_emissions"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L276.nonghg_othertrn_tech_coeff_USA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region <- state <- stub.technology <- Non.CO2 <- subsector <- X2010 <- BC_fraction <- sector <- minicam.energy.input <- emiss.coeff <-
      OC_fraction <- tranSubsector <- year <- calibrated.value <- share.weight.year <- subs.share.weight <- tech.share.weight <-
	total.calibrated.value <- supplysector <- tech_share <- value <- input.emissions <- fuel_input <- emiss.coef_new <- emiss.coef <- NULL # Silence package notes

    # Load required inputs
    BC_OC_assumptions <- get_data(all_data, "gcam-usa/emissions/BC_OC_assumptions")
    BCOC_PM25_ratios <- get_data(all_data, "gcam-usa/emissions/BCOC_PM25_ratios") %>%
      # removing columns we do not use, and sectors that aren't mapped to GCAM sectors
      select( -c( "Region", "Gains_Sector" ) ) %>%
      filter( !is.na( sector ) )
    IMO_Shipping_EF <- get_data(all_data, "gcam-usa/emissions/IMO_Shipping_EF")
    L270.nonghg_tg_state_othertrn_F_Yb <- get_data(all_data, "L270.nonghg_tg_state_othertrn_F_Yb", strip_attributes = TRUE)
    L254.StubTranTech_USA <- get_data(all_data, "L254.StubTranTech_USA", strip_attributes = TRUE)
    L254.StubTranTechCalInput_USA <- get_data(all_data, "L254.StubTranTechCalInput_USA", strip_attributes = TRUE)
    L201.en_pol_emissions <- get_data(all_data, "L201.en_pol_emissions")

    # Build tables for CSVs

    # Rail
    # Compute technology shares based on level of detail available in NEI emissions data (tech/fuel)
    # In the states, rail fuel input is separated into freight and passenger in GCAM.
    # The emissions are given for the "rail" sector in NEI, so they must be shared out to the technologies.
    # There is no NG fuel input data for these technologies, so omit these emissions for now.
    agg_CalInput_rail_F_Yb <- L254.StubTranTechCalInput_USA %>%
      filter(year %in% MODEL_BASE_YEARS & grepl("Rail",tranSubsector) &
               stub.technology %in% L270.nonghg_tg_state_othertrn_F_Yb$fuel) %>%
      mutate(sector = "trn_rail") %>%
      group_by(region,sector,stub.technology,year) %>%
      summarise(total.calibrated.value = sum(calibrated.value)) %>%
	  ungroup()

    rail_fuel_input_tech_shares <- L254.StubTranTechCalInput_USA %>%
      filter(year %in% MODEL_BASE_YEARS & grepl("Rail",tranSubsector) &
               stub.technology %in% L270.nonghg_tg_state_othertrn_F_Yb$fuel) %>%
      mutate(sector = "trn_rail") %>%
      left_join_error_no_match(agg_CalInput_rail_F_Yb,by=c("region","stub.technology","sector", "year")) %>%
      mutate(tech_share = calibrated.value/total.calibrated.value) %>%
      select(region,supplysector,tranSubsector,stub.technology,year,sector,tech_share)

    # Share out NEI emissions to the rail technologies using the technology shares
    # Start with all possible permutations in the states
    L276.rail_nonghg_tech_coeff_Yb_USA <- L254.StubTranTech_USA %>%
      filter(grepl("Rail",tranSubsector) & stub.technology %in% L270.nonghg_tg_state_othertrn_F_Yb$fuel) %>%
      select( -sce ) %>%
      #Add on a column containing the model base years
      repeat_add_columns(tibble::tibble(year=MODEL_BASE_YEARS)) %>%
      #Add the pollutants contained in the NEI data
      repeat_add_columns(tibble::tibble(Non.CO2=unique(L270.nonghg_tg_state_othertrn_F_Yb$Non.CO2))) %>%
      #Match on the tech shares
      left_join_error_no_match(rail_fuel_input_tech_shares,by = c("region","tranSubsector","supplysector","stub.technology","year")) %>%
      #Match on the NEI input emissions
      inner_join(L270.nonghg_tg_state_othertrn_F_Yb, by =c("region"="state","sector","stub.technology" = "fuel","Non.CO2","year")) %>%
      mutate(input.emissions = tech_share * value) %>%
      ###MISSING VALUES: Some 0 emissions for various pollutants in TX and HI. Leave these in for now.
      select(-sector,-tech_share,-value) %>%
      distinct() %>%
      # Compute emission tech coefficients: divide emissions by fuel use
      #Add on fuel inputs to the emissions table
      left_join_error_no_match(L254.StubTranTechCalInput_USA, by=c("region","supplysector","tranSubsector","stub.technology","year")) %>%
      mutate(fuel_input = calibrated.value) %>%
      #Compute tech coefficients
      mutate(emiss.coeff = input.emissions / fuel_input) %>%
      select( -sce )

    # Domestic Aviation
    # Domestic Aviation should be one national EF assigned to every US state.
    # We will aggregate emissions and fuel consumption from all states, and
    # calculate a national EF based off of these values
    L276.air_nonghg_tech_coeff_Yb_USA <- L254.StubTranTech_USA %>%
      filter(tranSubsector %in% c("Domestic Aviation") & stub.technology %in% L270.nonghg_tg_state_othertrn_F_Yb$fuel) %>%
      select( -sce ) %>%
      repeat_add_columns(tibble::tibble(year=MODEL_BASE_YEARS)) %>%
      repeat_add_columns(tibble::tibble(Non.CO2=unique(L270.nonghg_tg_state_othertrn_F_Yb$Non.CO2))) %>%
      mutate(sector = "trn_domestic air") %>%
      #Match on the emissions
      inner_join(L270.nonghg_tg_state_othertrn_F_Yb, by =c("region"="state","sector","stub.technology" = "fuel","Non.CO2","year")) %>%
      mutate(input.emissions = value) %>%
      ###MISSING VALUES: many permutations. Leave them in for now
      select(-sector,-value) %>%
      distinct() %>%
      #aggregate emissions by year, fuel, and Non.CO2
      group_by( year, stub.technology, Non.CO2 ) %>%
      mutate( input.emissions = sum( input.emissions ) ) %>%
    # Compute emission tech coefficients: divide emissions by fuel use
      #Add on fuel inputs to the emissions table, first aggregating fuel use
      left_join_error_no_match(L254.StubTranTechCalInput_USA %>%
                                 filter( tranSubsector == "Domestic Aviation") %>%
                                 group_by( year ) %>%
                                 mutate( calibrated.value = sum( calibrated.value ) ),
                               by=c("region","supplysector","tranSubsector","stub.technology","year")) %>%
      #Compute tech coefficients
      mutate(emiss.coeff = input.emissions / calibrated.value) %>%
      select( -sce )

    # International Shipping and Aviation
    # International Shipping and Aviation will have national EFs assigned to every US state.
    # We have emissions at the national level and fuel consumption from all states, and
    # will calculate national EFs based off of these values
    # Emissions
    L276.int_nonghg_tech_coeff_Yb_USA_emissions <- L254.StubTranTech_USA %>%
      filter(tranSubsector %in% c("International Aviation", "International Ship") ) %>%
      repeat_add_columns(tibble::tibble(year=MODEL_BASE_YEARS)) %>%
      repeat_add_columns(tibble::tibble(Non.CO2=unique(L270.nonghg_tg_state_othertrn_F_Yb$Non.CO2))) %>%
      select( -sce ) %>%
      distinct() %>%
      # Change PM2.5 and PM10 to BC and OC so LJENM works, as this is what we have information for
      mutate( Non.CO2 = gsub( "PM2.5", "BC", Non.CO2 ),
              Non.CO2 = gsub( "PM10", "OC", Non.CO2 ) ) %>%
      # Join with the emissions
      # Note: we will have "future only" technologies such as FCEV or BEV which will cause
      # NAs when joined.  But that is ok as they should little/no emissions anyways
      left_join( L201.en_pol_emissions %>% filter( region == "USA" ) %>% select( -region ) %>% mutate( Non.CO2 = gsub( "SO2_1", "SO2", Non.CO2 ) ),
                 by = c("supplysector", "tranSubsector" = "subsector", "stub.technology", "Non.CO2", "year") ) %>%
      filter(!is.na(input.emissions))

    # Fuel
    L276.int_nonghg_tech_coeff_Yb_USA_fuel <- L254.StubTranTechCalInput_USA %>%
      filter(tranSubsector %in% c("International Aviation", "International Ship") ) %>%
      # Aggregate fuel consumption nationally
      group_by( supplysector, tranSubsector, stub.technology, year, minicam.energy.input ) %>%
      mutate( calibrated.value = sum( calibrated.value ) ) %>%
      distinct( supplysector, tranSubsector, stub.technology, year, minicam.energy.input, calibrated.value ) %>%
      # ungroup so the emiss.coeff values are not based on grouped emissions
      ungroup()

    # Emission Factors
    L276.int_nonghg_tech_coeff_Yb_USA <- L276.int_nonghg_tech_coeff_Yb_USA_emissions %>%
      # join with the fuel table
      left_join_error_no_match( L276.int_nonghg_tech_coeff_Yb_USA_fuel, by = c( "supplysector", "tranSubsector", "stub.technology", "year", "input.name" = "minicam.energy.input" ) ) %>%
      mutate(emiss.coeff = input.emissions / calibrated.value)


    # Bind the air, rail, and international shipping and aviation EFs into a single table and remove unnecessary columns
    L276.nonghg_othertrn_tech_coeff_Yb_USA.NAs <- bind_rows(L276.rail_nonghg_tech_coeff_Yb_USA, L276.air_nonghg_tech_coeff_Yb_USA, L276.int_nonghg_tech_coeff_Yb_USA) %>%
      select( c( region, supplysector, tranSubsector, stub.technology, year, Non.CO2, emiss.coeff ) ) %>%
      ungroup()

    # Generate national median emissions factors for base years
    # Remove NAs so as to not skew the median
    L276.nonghg_othertrn_tech_coeff_Yb_USA.median.true <- L276.nonghg_othertrn_tech_coeff_Yb_USA.NAs %>%
      filter(!is.na(emiss.coeff)) %>%
      group_by(year, Non.CO2, supplysector, tranSubsector, stub.technology) %>%
      summarise(emiss.coeff = median(emiss.coeff)) %>%
      ungroup() %>%
      rename(nationalEF = emiss.coeff)

    # Some year / pollutant / sector / subsector / tech are NA for all entries, and should be set to 0
    L276.nonghg_othertrn_tech_coeff_Yb_USA.median.skewed <- L276.nonghg_othertrn_tech_coeff_Yb_USA.NAs %>%
      replace_na(list(emiss.coeff = 0)) %>%
      group_by(year, Non.CO2, supplysector, tranSubsector, stub.technology) %>%
      summarise(emiss.coeff = median(emiss.coeff)) %>%
      ungroup() %>%
      rename(nationalEF = emiss.coeff)

    # We want to join these tables so that only the entries not in median.true are retained from median.skewed
    # These all have EFs of 0
    L276.nonghg_othertrn_tech_coeff_Yb_USA.median <- L276.nonghg_othertrn_tech_coeff_Yb_USA.median.skewed %>%
      anti_join( L276.nonghg_othertrn_tech_coeff_Yb_USA.median.true, by=c("year", "Non.CO2", "supplysector", "tranSubsector", "stub.technology") ) %>%
      # rebind to median.true
      bind_rows(L276.nonghg_othertrn_tech_coeff_Yb_USA.median.true)

    # Replace all emissions factors above a given value (20 * median) or that are NAs with the national median emissions factor for that year, non.CO2, and technology
    L276.nonghg_othertrn_tech_coeff_Yb_USA.noBCOC <- L276.nonghg_othertrn_tech_coeff_Yb_USA.NAs %>%
      left_join_error_no_match(L276.nonghg_othertrn_tech_coeff_Yb_USA.median, by = c("year", "Non.CO2", "supplysector","tranSubsector", "stub.technology")) %>%
      # create a new column that has the threshold value
      mutate( threshold = nationalEF * 20,
              emiss.coeff = if_else(emiss.coeff > threshold | is.na(emiss.coeff), nationalEF, emiss.coeff)) %>%
      select(region, Non.CO2, year, supplysector, tranSubsector, stub.technology, emiss.coeff) %>%
      mutate(emiss.coeff = if_else(is.infinite(emiss.coeff), 1, emiss.coeff)) %>%
      rename( emiss.coef = emiss.coeff)

  # Domestic Shipping
  # GCAM fuel consumption is not accurate for domestic shipping, so we can't calculate accurate EFs using that data
  # Instead, we will use EFs from the International Maritime Organization
  shipping_EF_Tg_per_EJ <- IMO_Shipping_EF %>%
    tidyr::gather( "year", "EF", -c(pollutant, CEDS_Fuel)) %>%
    # convert kg/kg to Tg/EJ
    mutate( EF = EF * CONV_KG_TO_TG,
            new_EF = if_else( CEDS_Fuel == "heavy_oil", EF / CONV_KG_T / gcamusa.CONVERSIONFACTOR_RESIDUALOIL_GJ_PER_T_NET / CONV_GJ_EJ,
                                 if_else( CEDS_Fuel == "diesel_oil", EF / CONV_KG_T / gcamusa.CONVERSIONFACTOR_DIESEL_GJ_PER_T_NET / CONV_GJ_EJ,
                                                                     EF / CONV_KG_T / gcamusa.CONVERSIONFACTOR_NATURALGAS_GJ_PER_T_NET / CONV_GJ_EJ ) ),
            pollutant = gsub( "SOx", "SO2", pollutant ),
            # PM is total particulate matter, so can be interpreted as PM10
            pollutant = gsub( "\\PM\\>", "PM10", pollutant ) )

  # The shipping EFs are given for three fuels, while there is only one fuel in GCAM - Liquids
  # Weight the EFs to create "Liquids"- 60% residual and 40% diesel
  shipping_EF_Tg_per_EJ_liquids <- shipping_EF_Tg_per_EJ %>%
    # remove NG as it is not included to calculate liquids EF
    filter( CEDS_Fuel != "natural_gas" ) %>%
    mutate( partial_EF = if_else( CEDS_Fuel == "heavy_oil", new_EF * 0.6,
                                           new_EF * 0.4 ) ) %>%
    # calculate weighted EF from "partial" EFs
    group_by( pollutant, year ) %>%
    mutate( emiss.coef = sum( partial_EF ) ) %>%
    distinct( pollutant, year, emiss.coef ) %>%
    rename( "Non.CO2" = "pollutant" ) %>%
    ungroup() %>%
    mutate( stub.technology = "Liquids" )

  # Add NG back in
  shipping_EF_Tg_per_EJ_All <- shipping_EF_Tg_per_EJ %>%
    filter( CEDS_Fuel == "natural_gas" ) %>%
    rename( emiss.coef = new_EF,
            Non.CO2 = pollutant ) %>%
    select( -c( "EF", "CEDS_Fuel" ) ) %>%
    mutate( stub.technology = "NG" ) %>%
    bind_rows( shipping_EF_Tg_per_EJ_liquids )

  # convert year from character to numeric
  shipping_EF_Tg_per_EJ_All$year <- as.numeric(shipping_EF_Tg_per_EJ_All$year)

  # split EFs into three tables, one for 2015, one for the previous base years, one for future years
  # This is done in order to apply the 2012 EFs to the earlier base years, and 2018 to future years
  shipping_EF_Tg_per_EJ_Yb <- shipping_EF_Tg_per_EJ_All %>%
    filter( year == min( year ) ) %>%
    # deselect year column so that these EFs are mapped to all years
    select( -year )

  shipping_EF_Tg_per_EJ_2015 <- shipping_EF_Tg_per_EJ_All %>%
    filter( year == max( MODEL_BASE_YEARS ) )

  shipping_EF_Tg_per_EJ_Yf <- shipping_EF_Tg_per_EJ_All %>%
    filter( year == max( year ) ) %>%
    # deselect year column so that these EFs are mapped to all years
    select( -year )


  # Apply 2012 EFs to historical years
  # NOTE: In the future, use a different source for earlier base year EFs since domestic shipping has changed in the last decade
  L276.nonghg_othertrn_tech_coeff_USA_marine_Yb <- L254.StubTranTech_USA %>%
    select( -sce ) %>%
    filter(tranSubsector %in% c("Domestic Ship") & stub.technology %in% L270.nonghg_tg_state_othertrn_F_Yb$fuel) %>%
    repeat_add_columns( tibble::tibble( year = MODEL_BASE_YEARS ) ) %>%
    # remove final base year
    filter( year != max( MODEL_BASE_YEARS ) ) %>%
    repeat_add_columns( tibble::tibble( Non.CO2 = unique( L270.nonghg_tg_state_othertrn_F_Yb$Non.CO2 ) ) ) %>%
    left_join( shipping_EF_Tg_per_EJ_Yb, by = c( "Non.CO2", "stub.technology" ) ) %>%
    # remove NH3, as we do not have EFs for it
    na.omit()

  # Apply current base years EFs to the current base year
  L276.nonghg_othertrn_tech_coeff_USA_marine_2015 <- L254.StubTranTech_USA %>%
    select( -sce ) %>%
    filter(tranSubsector %in% c("Domestic Ship") & stub.technology %in% L270.nonghg_tg_state_othertrn_F_Yb$fuel) %>%
    repeat_add_columns( tibble::tibble( year = max( MODEL_BASE_YEARS ) ) ) %>%
    repeat_add_columns( tibble::tibble( Non.CO2 = unique( L270.nonghg_tg_state_othertrn_F_Yb$Non.CO2 ) ) ) %>%
    left_join( shipping_EF_Tg_per_EJ_2015, by = c( "year", "Non.CO2", "stub.technology" ) ) %>%
    # remove NH3, as we do not have EFs for it
    na.omit()

  # Apply 2018 EFs to future years (only need to apply to first future year as this will be carried forward)
  L276.nonghg_othertrn_tech_coeff_USA_marine_Yf <- L254.StubTranTech_USA %>%
    select( -sce ) %>%
    filter(tranSubsector %in% c("Domestic Ship") & stub.technology %in% L270.nonghg_tg_state_othertrn_F_Yb$fuel) %>%
    repeat_add_columns( tibble::tibble( year = min( MODEL_FUTURE_YEARS ) ) ) %>%
    # remove final base year
    filter( year != max( MODEL_BASE_YEARS ) ) %>%
    repeat_add_columns( tibble::tibble( Non.CO2 = unique( L270.nonghg_tg_state_othertrn_F_Yb$Non.CO2 ) ) ) %>%
    left_join( shipping_EF_Tg_per_EJ_Yf, by = c( "Non.CO2", "stub.technology" ) ) %>%
    # remove NH3, as we do not have EFs for it
    na.omit()

	# Calculate SO2 emission coefficient for 2015 forward
	# Update usa domestic shipping energy consumption using data from US EPA GHG inventory
  # since EIA and IEA data is incomplete or unavailable for this sector.

	# We do this from the EPA GHG inventory data, which is the best source for domestic
	# shipping fuel consumption (IEA is incomplete), approximately 60% of domestic
	# shipping fuel is residual and 40% diesel. The diesel standard past 2012 is 15
	# ppm sulfur, and as per US EPA 2009 EPA Category 3 marine engines have a sulfur
	# limit of 1,000 ppm (0.1%) for marine fuels produced and/or sold for use within
	# an Emissions Control Area (ECA) - so this should include all domestic
	# shipping. Effectively, the sulfur limit for domestic navigation can be estimated with
	# the residual fraction and a 0.1% sulfur content. The emissions factor
	# [Tg/EJ] is, therefore, 0.1% * 2 * EC_resid, where the factor of 2 converts S
	# to SO2 and EC_resid = energy content of residual oil, in units of Tg/EJ.
	# We calculate the energy content of residual oil from readily available data.

  L276.nonghg_othertrn_tech_coeff_USA_marine_new_SO2 <- L276.nonghg_othertrn_tech_coeff_USA_marine_2015 %>%
    bind_rows( L276.nonghg_othertrn_tech_coeff_USA_marine_Yf ) %>%
    mutate( emiss.coef = if_else( Non.CO2 == "SO2",
                                  SO2_SHIP_LIMIT_POLICY_MULTIPLIER * CONV_KG_TO_TG / CONV_KG_T / gcamusa.CONVERSIONFACTOR_DIESEL_GJ_PER_T_NET / CONV_GJ_EJ,
                                  emiss.coef ) )

  # bind EFs for shipping from all years with the table of other subsector EFs
  L276.nonghg_all_othertrn_tech_coeff_Yb_USA.noBCOC <- bind_rows(L276.nonghg_othertrn_tech_coeff_USA_marine_Yb,
                                                             L276.nonghg_othertrn_tech_coeff_USA_marine_new_SO2,
                                                             L276.nonghg_othertrn_tech_coeff_Yb_USA.noBCOC)

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
    mutate( year = 1990 ) %>%
    complete(nesting(sector,subsector,technology, BC_fraction, OC_fraction), year = MODEL_YEARS)

  # Join the tables, keeping values from BC_OC_assumptions_ext that do not appear in BCOC_PM25_ratios
  BC_OC_assumptions_years <- BC_OC_assumptions_ext %>%
    anti_join( BCOC_PM25_ratios_ext, by = c("sector", "subsector", "technology", "year") ) %>%
    # bind to BCOC_PM25_assumptions
    bind_rows( BCOC_PM25_ratios_ext ) %>%
    rename(tranSubsector = subsector)

  # There is no data for BC/OC in the base year, so use fractions of PM2.5 to calculate BC/OC emission coefficients.
  L276.nonghg_othertrn_tech_coeff_USA_no_driver_needPM <- compute_BC_OC_transport(L276.nonghg_all_othertrn_tech_coeff_Yb_USA.noBCOC, BC_OC_assumptions_years)

  # For International Shipping and Aviation, we have BC and OC but no PM.
  # We can use our BC and OC assumptions to calculate PM2.5.
  L276.nonghg_othertrn_tech_coeff_USA_no_driver <- L276.nonghg_othertrn_tech_coeff_USA_no_driver_needPM %>%
    filter( grepl( "International", tranSubsector ),
            Non.CO2 == "BC" ) %>%
    # join with the assumption table
    left_join_error_no_match( BC_OC_assumptions_years, by = c( "supplysector" = "sector", "tranSubsector", "stub.technology" = "technology", "year" ) ) %>%
    # calculate PM2.5
    mutate( emiss.coef = emiss.coef / BC_fraction,
            Non.CO2 = "PM2.5" ) %>%
    # remove unneeded columns
    select( -c( BC_fraction, OC_fraction ) ) %>%
    # bind back to the table containing all other EFs
    bind_rows( L276.nonghg_othertrn_tech_coeff_USA_no_driver_needPM )

  # For International Ship, we need PM10. We can use PM2.5 to PM10 ratio from EPA to derive this.
  L276.nonghg_othertrn_tech_coeff_USA_no_driver_hasPM <- L276.nonghg_othertrn_tech_coeff_USA_no_driver %>%
    # filter for International Ship, PM2.5
    filter( tranSubsector == "International Ship", Non.CO2 == "PM2.5" ) %>%
    # calculate the PM10 emission factors based on PM2.5 and EPA ratio
    mutate( emiss.coef = emiss.coef / gcamusa.INTL_SHIP_PM_RATIO,
            Non.CO2 = "PM10" ) %>%
    # bind back to the table with all other EFs
    bind_rows( L276.nonghg_othertrn_tech_coeff_USA_no_driver )

  # Add an input name column to drive emissions
  L276.nonghg_othertrn_tech_coeff_USA <- L276.nonghg_othertrn_tech_coeff_USA_no_driver_hasPM %>%
    # L254.StubTranTechCalInput_USA has the fuel inputs that should be used to drive emissions
    left_join_error_no_match( L254.StubTranTechCalInput_USA %>%
                                filter( grepl( "Rail|Aviation|Ship", tranSubsector ) ) %>%
                                select( c( "supplysector", "tranSubsector", "stub.technology",
                                                                                 "minicam.energy.input" ) ) %>%
                                distinct( supplysector, tranSubsector, stub.technology, minicam.energy.input ),
                              by = c("supplysector", "tranSubsector", "stub.technology") ) %>%
    # rename to input.name to match header
    rename( input.name = minicam.energy.input ) %>%
    # change SO2 to SO2_1
    mutate( Non.CO2 = gsub( "SO2", "SO2_1", Non.CO2 ) ) %>%
    # remove 1975
    filter( year > min(MODEL_BASE_YEARS) ) %>%
    # distinct to remove any potential duplicate rows
    distinct()

    # Produce outputs
    L276.nonghg_othertrn_tech_coeff_USA %>%
      add_title("Non-GHG input emissions parameters for non-road transportation sector") %>%
      add_units("Tg/EJ") %>%
      add_comments("Non-GHG input emissions parameters for non-road transportation sector") %>%
      add_legacy_name("L276.nonghg_othertrn_tech_coeff_USA") %>%
      add_precursors("gcam-usa/emissions/BC_OC_assumptions",
                     "gcam-usa/emissions/BCOC_PM25_ratios",
                     "gcam-usa/emissions/IMO_Shipping_EF",
                     "L270.nonghg_tg_state_othertrn_F_Yb",
                     "L254.StubTranTech_USA",
                     "L254.StubTranTechCalInput_USA",
                     "L201.en_pol_emissions") ->
      L276.nonghg_othertrn_tech_coeff_USA



    return_data(L276.nonghg_othertrn_tech_coeff_USA)
  } else {
    stop("Unknown command")
  }
}
