#' module_gcamusa_L169.nonghg_NEI_scaling_USA
#'
#' Further processes the NEI data to get a full timeseries for 1990 - 2017.
#' The NEI data is scaled to EPA Tier 1 for HIGHWAY VEHICLES, OFF-HIGHWAY, and FUEL COMB. ELEC. UTIL.
#' It is linearly interpolated between NEI years (2008 - 2017), and extrapolated back to 1990 based on
#' the 2008 sector and fuels shares.
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L169.NEI_1990_2017_GCAM_sectors_unscaled}
#' @details Non-CO2 emissions from NEI interpolated and scaled to EPA Tier 1
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select group_by
#' @importFrom tidyr gather spread
#' @author MAW December 2021

module_gcamusa_L169.nonghg_NEI_scaling_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE="gcam-usa/emissions/state_tier1_caps",
             FILE="gcam-usa/emissions/NEI_emissions_to_CEDS_adj_2008",
             FILE="gcam-usa/emissions/NEI_emissions_to_CEDS_adj_2011",
             FILE="gcam-usa/emissions/NEI_emissions_to_CEDS_adj_2014",
             FILE="gcam-usa/emissions/NEI_emissions_to_CEDS_adj_2017",
             FILE="gcam-usa/emissions/CEDS_to_USEPA_Tier1_Mapping",
             FILE="gcam-usa/emissions/CEDS_to_GCAM_sector_mapping",
             FILE="gcam-usa/emissions/NEI_pollutant_mapping",
             "L102.ceds_GFED_nonco2_tg_C_S_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L169.NEI_1990_2017_GCAM_sectors_unscaled"))
  } else if(command == driver.MAKE) {


    # Silence package check.
    X <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    state_tier1_caps <- get_data(all_data, "gcam-usa/emissions/state_tier1_caps", strip_attributes = T)
    NEI_emissions_to_CEDS_adj_2008 <- get_data(all_data, "gcam-usa/emissions/NEI_emissions_to_CEDS_adj_2008", strip_attributes = T) %>%
      mutate(year = 2008)
    NEI_emissions_to_CEDS_adj_2011 <- get_data(all_data, "gcam-usa/emissions/NEI_emissions_to_CEDS_adj_2011", strip_attributes = T) %>%
      mutate(year = 2011)
    NEI_emissions_to_CEDS_adj_2014 <- get_data(all_data, "gcam-usa/emissions/NEI_emissions_to_CEDS_adj_2014", strip_attributes = T) %>%
      mutate(year = 2014)
    NEI_emissions_to_CEDS_adj_2017 <- get_data(all_data, "gcam-usa/emissions/NEI_emissions_to_CEDS_adj_2017", strip_attributes = T) %>%
      mutate(year = 2017)
    CEDS_to_USEPA_Tier1_Mapping <- get_data(all_data, "gcam-usa/emissions/CEDS_to_USEPA_Tier1_Mapping", strip_attributes = T)
    CEDS_to_GCAM_sector_mapping <- get_data(all_data, "gcam-usa/emissions/CEDS_to_GCAM_sector_mapping", strip_attributes = T)
    NEI_pollutant_mapping <- get_data(all_data, "gcam-usa/emissions/NEI_pollutant_mapping", strip_attributes = T)
    L102.ceds_GFED_nonco2_tg_C_S_F <- get_data(all_data, "L102.ceds_GFED_nonco2_tg_C_S_F")

    # Combine NEI data
    NEI_emissions_to_CEDS_adj_all <- NEI_emissions_to_CEDS_adj_2008 %>%
      bind_rows(NEI_emissions_to_CEDS_adj_2011,NEI_emissions_to_CEDS_adj_2014,NEI_emissions_to_CEDS_adj_2017)

    # -----------------------------------------------------------------------------
    # ======================================
    # ======================================
    # A. Interpolating 2008 - 2017
    # ======================================
    # ======================================

    # ======================================
    # 1. data and mapping preparation
    # ======================================
    # 1.1 preparing EPA Tier1 state level data
    state_tier1_caps %>%
      # changing units of the EPA Tier1 data to match NEI (Thousand Tons to Tons)
      dplyr::mutate_if( is.numeric, list(~ . * 1000) ) -> state_tier1_tons

    # NA entries become 0 in order for emissions sums to work (otherwise NA is returned)
    state_tier1_tons[is.na(state_tier1_tons)] <- 0

    # creating a dataframe that saves the three tier1 categories that NEI will be scaled to
    state_tier1_tons %>%
      filter( grepl( 'HIGHWAY VEHICLES|OFF-HIGHWAY|FUEL COMB. ELEC. UTIL.', tier1_description ) ) -> state_tier1_to_scale_to

    # 1.2 preparing NEI data
    NEI_emissions_to_CEDS_adj_all %>%
        # remove pollutants not in tier1 data
        filter( !grepl( 'Carbon Dioxide|Methane|Nitrous Oxide|PM10 Filterable|PM2.5 Filterable', pollutant ) ) %>%
        # remove regions not in tier1 data
        filter( state %in% gcamusa.STATES ) -> NEI_refined

      # Separating combustion and noncombustion emissions (broadly) and assigning "Process" fuel to noncombustion
      NEI_refined_combustion <- NEI_refined %>%
        # CEDS sectors that begin with "1A" are the combustion sectors
        filter( grepl( "^1A", CEDS_Sector ) )

      NEI_refined_noncombustion <- NEI_refined %>%
        filter( !grepl( "^1A", CEDS_Sector ) ) %>%
        mutate( CEDS_Fuel = "Process" ) %>%
        # sum emissions by state, sector, year, and pollutant now that they all have the same fuel
        group_by( state, pollutant, year, CEDS_Sector, CEDS_Fuel, unit ) %>%
        mutate( emissions = sum( emissions ) ) %>%
        distinct()

      NEI_refined_all <- NEI_refined_combustion %>%
        bind_rows( NEI_refined_noncombustion )

    # ======================================
    # 2. "OTHER" Tier 1 interpolation
    # ======================================
    # Linearly interpolating the Tier 1 categories that are not being scaled

    # 2.1 preparing the data for scaling and interpolation
      # mapping the CEDS extended sectors to Tier 1 categories
      NEI_refined_all %>%
        left_join_error_no_match( CEDS_to_USEPA_Tier1_Mapping, by = c( "CEDS_Sector" ) ) -> NEI_tier1

      NEI_tier1 %>%
        # removing the three Tier1 categories that are not being linearly interpolated
        filter( !grepl( 'HIGHWAY VEHICLES|OFF-HIGHWAY|FUEL COMB. ELEC. UTIL.', tier1_description ) ) -> NEI_othertier1

    # NAs must become 0 here, or else values will later be interpolated for sectors that did not previously exist for some years
    NEI_othertier1[is.na(NEI_othertier1)] <- 0

    # Interpolate data linearly between years
    NEI_othertier1 %>%
      complete( nesting( state, pollutant, CEDS_Sector, CEDS_Fuel, tier1_description, unit ), year = 2008:2017 ) %>%
      group_by( state, pollutant, CEDS_Sector, CEDS_Fuel, tier1_description, unit ) %>%
      mutate( emissions = approx_fun(year, emissions, rule = 2) ) %>%
      ungroup() %>%
      filter( !is.na(emissions)) -> NEI_othertier1_2008_2017

    # ======================================
    # 3 NEI to Tier 1 Scaling
    # ======================================
    # 3.1 preparing the data for scaling and interpolation
    # creating a dataframe that only include the three tier1 categories of interest (referred to as "hw tiers")
      NEI_tier1 %>%
        # keeping the three Tier1 categories that are being scaled
        filter( grepl( 'HIGHWAY VEHICLES|OFF-HIGHWAY|FUEL COMB. ELEC. UTIL.', tier1_description ) ) -> NEI_to_scale

    # 3.2 making adjustments to onroad sector names
    NEI_to_scale[is.na(NEI_to_scale)] <- 0 #NA entries become 0 in order for emissions sums to work (otherwise NA is returned)

    # Make dataframe long, remove onroad sectors (these will be processed differently), and interpolate.
    NEI_to_scale %>%
      filter( !grepl("1A3b", CEDS_Sector ) ) %>%
      complete( nesting( state, pollutant, CEDS_Sector, CEDS_Fuel, tier1_description, unit ), year = 2008:2017 ) %>%
      group_by( state, pollutant, CEDS_Sector, CEDS_Fuel, tier1_description, unit ) %>%
      mutate( emissions = approx_fun(year, emissions, rule = 2) ) %>%
      ungroup() %>%
      filter( !is.na(emissions)) -> NEI_to_scale_nonroad_long

    # Any CEDS Sector that begins with "1A3b" needs to be renamed to just "1A3bi_Road", and the emissions summed.
    # This is because there are discrepancies in road sector names between NEI years that result in double counting
    # when we interpolate. The specific CEDS sectors for Road are not used later, so this does not impact anything.
    # User can change whether or not to include dust in constants.R
    if( gcamusa.DUST == FALSE ){

      NEI_to_scale_renamed_road <- NEI_to_scale %>%
        filter( CEDS_Fuel != "Dust",
                grepl("1A3bi", CEDS_Sector ) ) %>%
        group_by( state, year, pollutant, CEDS_Fuel, tier1_description, unit ) %>%
        mutate( emissions = sum(emissions) ) %>%
        distinct() %>%
        mutate( "CEDS_Sector" = "1A3bi_Road" ) %>%
        complete( nesting( state, pollutant, CEDS_Sector, CEDS_Fuel, tier1_description, unit ), year = 2008:2017 ) %>%
        distinct()

    } else {

      # create a dataframe that has only onroad combustion emissions
      NEI_to_scale_renamed_road_no_dust <- NEI_to_scale %>%
        filter( grepl("1A3bi", CEDS_Sector ) ) %>%
        group_by( state, year, pollutant, CEDS_Fuel, tier1_description, unit ) %>%
        mutate( emissions = sum(emissions) ) %>%
        ungroup() %>%
        mutate( "CEDS_Sector" = "1A3bi_Road" ) %>%
        complete( nesting( state, pollutant, CEDS_Sector, CEDS_Fuel, tier1_description, unit ), year = 2008:2017 ) %>%
        distinct()

      # create a dataframe that has only dust emissions, which will be assigned fuel shares
      NEI_to_scale_renamed_road_dust <- NEI_to_scale %>%
        filter( CEDS_Fuel == "Dust",
                # there are some non-PM pollutants that have 0 emissions, remove these
                grepl( "PM", pollutant ) ) %>%
        group_by( state, year, pollutant, CEDS_Fuel, tier1_description, unit ) %>%
        mutate( emissions = sum(emissions) ) %>%
        ungroup() %>%
        mutate( "CEDS_Sector" = "1A3bi_Road" ) %>%
        distinct()

      # calculate fuel shares from the combustion onroad emissions
      # Note: Dan (EPA-ORD) suggested miles based if possible, but that can't be done at this step
      NEI_onroad_fuel_shares <- NEI_to_scale_renamed_road_no_dust %>%
        filter( grepl( "PM", pollutant ),
                # remove NA emissions. The shares will be applied to years we have, and emissions interpolatedv later
                !is.na( emissions ) ) %>%
        group_by( state, pollutant, year ) %>%
        mutate( agg_emissions = sum( emissions ),
                fuel_share = emissions / agg_emissions ) %>%
        select( c( state, pollutant, CEDS_Fuel, year, fuel_share ) ) %>%
        ungroup()

      NEI_dust_emissions_to_fuels <- NEI_to_scale_renamed_road_dust %>%
        # removing CEDS fuel since this will be replaces
        select( -CEDS_Fuel ) %>%
        # can't use left_join_error_no_match because we are adding rows due to adding fuels
        left_join( NEI_onroad_fuel_shares, by = c( "state", "pollutant", "year" ) ) %>%
        # sharing out the emissions
        mutate( emissions = emissions * fuel_share ) %>%
        select( -fuel_share ) %>%
        complete( nesting( state, pollutant, CEDS_Sector, CEDS_Fuel, tier1_description, unit ), year = 2008:2017 ) %>%
        distinct()

      # combine the combustion and dust dataframes, and aggregate emissions
      # Now, PM emissions include dust
      NEI_to_scale_renamed_road <- NEI_to_scale_renamed_road_no_dust %>%
        bind_rows( NEI_dust_emissions_to_fuels ) %>%
        group_by( state, pollutant, CEDS_Sector, CEDS_Fuel, tier1_description, unit, year ) %>%
        mutate( emissions = sum( emissions ) ) %>%
        ungroup() %>%
        distinct()

    }

    NEI_to_scale_renamed_road %>%
      group_by( state, pollutant, CEDS_Sector, CEDS_Fuel, tier1_description, unit ) %>%
      mutate( emissions = approx_fun(year, emissions, rule = 2) ) %>%
      ungroup() %>%
      distinct() -> NEI_to_scale_renamed_road_NAs

    # There are NAs in some instances (1A3bi_Road emissions for ND natural gas (all pollutants),
    # ID Dust (EV emissions) (CO, NOx, PM2.5, PM10), CT natural gas (all), and CA natural gas (SO2) ) for all but one year.
    # In this case, using approx_fun results in all NAs for that given grouping, removing emissions from
    # the year we have data for. We need to apply the value we have for all years or else we get emissions
    # sum issues down the line.
    NEI_to_scale_renamed_road_NAs %>%
      # these instances can be found where emissions are NA
      filter( is.na( emissions ) ) %>%
    # we want to save only the unique state / pollutant / sector / fuel combinations from here, and then
    # pull them from NEI_to_scale, retaining the values we do have
      distinct( state, pollutant, CEDS_Sector, CEDS_Fuel ) %>%
      left_join( NEI_to_scale_renamed_road, by = c( "state", "pollutant", "CEDS_Sector", "CEDS_Fuel" ) ) -> NEI_road_all_NAs

    # Remove these entries from NEI_to_scale_renamed_road_NAs
    NEI_to_scale_renamed_road_NAs %>%
      anti_join( NEI_road_all_NAs,
                 by = c("state", "pollutant", "CEDS_Sector", "CEDS_Fuel",
                        "tier1_description", "unit", "year") ) -> NEI_to_scale_renamed_road_no_NAs

    # To the dataframe that has several NAs, we want to use the approx_fun with the "constant" method
    NEI_road_all_NAs %>%
      group_by( state, pollutant, CEDS_Sector, CEDS_Fuel, tier1_description, unit ) %>%
      mutate( emissions = approx_fun_constant(year, emissions, rule = 2) ) %>%
      ungroup() -> NEI_to_scale_renamed_road_constant


    # bind the two road and one nonroad tables
    NEI_to_scale_renamed_road_no_NAs %>%
      bind_rows( NEI_to_scale_renamed_road_constant, NEI_to_scale_nonroad_long ) -> NEI_to_scale_all

    # 3.3 Scaling
    # calculate shares for NEI years by year, state, pollutant, and tier1 description
    NEI_to_scale_all %>%
      group_by( state, tier1_description, pollutant, year ) %>%
      mutate( share = emissions/sum( emissions ) ) %>%
      ungroup() -> NEI_shares

    # change NaN shares to 0- this happens when an entry has 0 emissions
    NEI_shares$share[is.na(NEI_shares$share)] <- 0

    # process the state level emissions data so that we can multiply total state emissions by NEI share values
    # this will be done by state, year, pollutant, and tier1 description
    state_tier1_to_scale_to %>%
      group_by( state, tier1_description, pollutant_code ) %>%
      # getting a total emissions value for each year, by group
      mutate( total_emissions2008 = sum( emissions08 ),
                     total_emissions2009 = sum( emissions09 ),
                     total_emissions2010 = sum( emissions10 ),
                     total_emissions2011 = sum( emissions11 ),
                     total_emissions2012 = sum( emissions12 ),
                     total_emissions2013 = sum( emissions13 ),
                     total_emissions2014 = sum( emissions14 ),
                     total_emissions2015 = sum( emissions15 ),
                     total_emissions2016 = sum( emissions16 ),
                     total_emissions2017 = sum( emissions17 ) ) %>%
      select( c(state, tier1_description, pollutant_code, total_emissions2008, total_emissions2009, total_emissions2010, total_emissions2011,
                       total_emissions2012, total_emissions2013, total_emissions2014, total_emissions2015, total_emissions2016, total_emissions2017 ) ) %>%
      gather( id, total_emissions, -c( state, tier1_description, pollutant_code ) ) %>%
      ungroup() %>%
      separate( id, into = c("id", "year"), sep = "emissions" ) %>%
      select( -id ) %>%
      mutate( year = as.integer( year ) ) -> state_tier1_pollutant_emissions

    # map the emissions from EPA state Tier 1 to NEI shares, then multiply scale emissions
    NEI_shares %>%
      left_join_error_no_match( NEI_pollutant_mapping, by = c( "pollutant" = "NEI_pollutant" ) ) %>%
      mutate( Non.CO2 = gsub( "NMVOC", "VOC", Non.CO2 ),
                     Non.CO2 = gsub( "NOx", "NOX", Non.CO2 ),
                     Non.CO2 = gsub( "PM2.5", "PM25", Non.CO2 ) ) %>%
      left_join_error_no_match( state_tier1_pollutant_emissions, by = c( "state", "tier1_description", "year", "Non.CO2" = "pollutant_code" ) ) %>%
      mutate( emissions = ( total_emissions * share ) ) %>%
      select( -c( share, total_emissions, Non.CO2 ) ) -> NEI_scaled_2008_2017

    # ======================================
    # 4. Combining Tables and Formatting
    # ======================================
    # rebinding all tiers
    NEI_othertier1_2008_2017 %>%
      bind_rows( NEI_scaled_2008_2017 ) -> NEI_2008_2017

    # Map to GCAM sectors
    NEI_2008_2017 %>%
      # use left_join because some CEDS_Sectors map to NA GCAM sectors, as these emissions are not included in GCAM.
      # Note: could map to "not used in GCAM" instead so LJENM works, and then filter out
      left_join( CEDS_to_GCAM_sector_mapping, by = "CEDS_Sector" ) %>%
      select( -c( CEDS_sector_code, IPCC_sector_code, IPCC_sector_description, Notes ) ) %>%
      ungroup() -> NEI_2008_2017_GCAM

    # assign Process CEDS_Fuel to process sectors
    NEI_2008_2017_GCAM %>%
      filter( !grepl( "^1A", CEDS_Sector ) ) %>%
      mutate( CEDS_Fuel = "Process" ) -> GCAM_Process_2008_2017

    NEI_2008_2017_GCAM %>%
      filter( grepl( "^1A", CEDS_Sector ) ) -> GCAM_Combustion_2008_2017

    GCAM_Process_2008_2017 %>%
      bind_rows( GCAM_Combustion_2008_2017 ) %>%
      distinct( state, year, pollutant, CEDS_Sector, CEDS_Fuel, tier1_description, unit, emissions, GCAM_sector ) -> NEI_2008_2017_final


    # ======================================
    # ======================================
    # B. Extrapolating & Scaling 1990 - 2007
    # ======================================
    # ======================================

    # ======================================
    # 1. data and mapping preparation
    # ======================================
    # 1.1 preparing CEDS data
    # Converting from Tg to short ton to match NEI units
    L102.ceds_GFED_nonco2_tg_C_S_F %>%
      mutate( emissions = emissions / CONV_TST_TG * 1000 ) %>%
      mutate( units = "TON" ) -> CEDS_ton

    # 1.2 preparing the NEI data
    NEI_2008_2017_final %>%
      # filtering for 2008, as the sector / fuel / pollutant distribution from this year will be carried back
      filter( year == 2008,
      # calculate shares for state and tier1 description
      # because the state tier1 caps don't include domestic shipping and other natural (other natural is not used in GCAM),
      # we have to remove these for the shares to work properly
      # other natural is not used so this is fine, domestic shipping is addressed later
              !grepl( "DOMESTIC SHIPPING|OTHER NATURAL", tier1_description ) ) %>%
      group_by( state, pollutant, tier1_description ) %>%
      mutate( share = emissions/sum( emissions ) ) %>%
      ungroup() -> NEI_2008_tier1_shares

    # 1.3 preparing the EPA Tier 1 data
    # now we need to process the state level emissions data, so that we can multiply EPA Tier 1 emissions by NEI share values
    # this will be done by state and tier1 description
    # we need a value of total emissions, by state, by year, by tier1 description
    # state_tier1 caps include Wildfires and Prescribed Fires, which is not included in the NEI output
    # we are also removing storage & transport for now, as no CEDS sectors are mapped to it
    # so we need to remove these from the state tier1 dataframe
    state_tier1_tons %>%
      filter( !grepl( "WILDFIRES|PRESCRIBED FIRES|STORAGE & TRANSPORT", tier1_description ) ) %>%
      group_by( state, pollutant_code, tier1_description ) %>%
      mutate( total_emissions2008 = sum( emissions08 ),
                     total_emissions2007 = sum( emissions07 ),
                     total_emissions2006 = sum( emissions06 ),
                     total_emissions2005 = sum( emissions05 ),
                     total_emissions2004 = sum( emissions04 ),
                     total_emissions2003 = sum( emissions03 ),
                     total_emissions2002 = sum( emissions02 ),
                     total_emissions2001 = sum( emissions01 ),
                     total_emissions2000 = sum( emissions00 ),
                     total_emissions1999 = sum( emissions99 ),
                     total_emissions1998 = sum( emissions98 ),
                     total_emissions1997 = sum( emissions97 ),
                     total_emissions1996 = sum( emissions96 ),
                     total_emissions1990 = sum( emissions90 )) %>%
      distinct( state, pollutant_code, tier1_description, .keep_all = T ) %>%
      select( c(state, pollutant_code, tier1_description, total_emissions2007, total_emissions2006, total_emissions2005,
                       total_emissions2004, total_emissions2003, total_emissions2002, total_emissions2001, total_emissions2000,
                       total_emissions1999, total_emissions1998, total_emissions1997, total_emissions1996, total_emissions1990 ) ) %>%
      gather( id, total_emissions, -c( state, pollutant_code, tier1_description ) ) %>%
      ungroup() %>%
      separate( id, into = c("id", "year"), sep = "emissions" ) %>%
      select( -id ) %>%
      mutate( year = as.integer( year ) ) %>%
      # interpolate emissions for missing years
      complete( nesting( state, pollutant_code, tier1_description ), year = 1990:2007 ) %>%
      group_by( state, pollutant_code, tier1_description ) %>%
      mutate( total_emissions = approx_fun(year, total_emissions, rule = 2) ) %>%
      ungroup() %>%
      # make data long again
      spread( year, total_emissions ) %>%
      # changing some pollutant names for mapping purposes
      mutate( pollutant_code = gsub( "NOX", "NOx", pollutant_code ),
              pollutant_code = gsub( "PM25", "PM2.5", pollutant_code ),
              pollutant_code = gsub( "VOC", "NMVOC", pollutant_code ) ) %>%
      # map to NEI pollutant names
      left_join_error_no_match( NEI_pollutant_mapping, by = c( "pollutant_code" = "Non.CO2" ) ) %>%
      select( -pollutant_code ) -> state_tier1_total_emissions

    # ======================================
    # 2. Scaling
    # ======================================
    # 2.1 scaling the NEI data to EPA Tier 1
    # map the total emissions value to NEI_tier1_shares, then multiply total_emissions by the share
    NEI_2008_tier1_shares %>%
      left_join_error_no_match( state_tier1_total_emissions, by = c( "state", "pollutant" = "NEI_pollutant", "tier1_description" ) ) %>%
      # now we need to multiply every EPA year column by the NEI share value from 2008
      mutate( `emissions2007` = ( `2007` * share ),
                     `emissions2006` = ( `2006` * share ),
                     `emissions2005` = ( `2005` * share ),
                     `emissions2004` = ( `2004` * share ),
                     `emissions2003` = ( `2003` * share ),
                     `emissions2002` = ( `2002` * share ),
                     `emissions2001` = ( `2001` * share ),
                     `emissions2000` = ( `2000` * share ),
                     `emissions1999` = ( `1999` * share ),
                     `emissions1998` = ( `1998` * share ),
                     `emissions1997` = ( `1997` * share ),
                     `emissions1996` = ( `1996` * share ),
                     `emissions1995` = ( `1995` * share ),
                     `emissions1994` = ( `1994` * share ),
                     `emissions1993` = ( `1993` * share ),
                     `emissions1992` = ( `1992` * share ),
                     `emissions1991` = ( `1991` * share ),
                     `emissions1990` = ( `1990` * share ) ) %>%
      # deselect columns no longer needed
      select( -c( "year", "emissions", "tier1_description", "share", "1990", "1991", "1992", "1993", "1994", "1995",
                  "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007"  ) ) -> NEI_scaled_to_state_emissions_NAs

    # TODO: scaling issue: some states/tier1 descriptions have emissions that do not have emissions in NEI
    # At this point, we will remove all NAs from the table, but the scaling issue should be remediated in the future

    # Entries with NA emissions in 2007 are removed. If there are NAs in one year, they are in all years.
    NEI_scaled_to_state_emissions_NAs %>%
      filter(!is.na(emissions2007),
             # remove NA GCAM_sectors. This is CEDS 5C_Open-burning-land-clearing, which has no assocaited GCAM sector.
             !is.na(GCAM_sector)) -> NEI_scaled_to_state_emissions

    #Other NA entries (emissions) become 0 in order for emissions sums to work (otherwise NA is returned)
    NEI_scaled_to_state_emissions[is.na(NEI_scaled_to_state_emissions)] <- 0

    # 2.1 scaling NEI Domestic Shipping to CEDS
    # We have emissions from domestic shipping from NEI for 2008 - 2017
    # It is not included in the Tier1 state level data, so we scale to CEDS emissions instead
    # Filter the CEDS data for domestic shipping, the years we need, and when there are non-zero emissions
    CEDS_ton %>%
      filter( sector == "1A3dii_Domestic-navigation",
              year >= 1990,
              year <= 2007,
              emissions > 0 ) %>%
      group_by( year, Non.CO2 ) %>%
      mutate( emissions = sum(emissions) ) %>%
      ungroup() %>%
      distinct( year, Non.CO2, emissions ) -> CEDS_domestic_ship

    # Calculate shares of Domestic Shipping emissions by pollutant
    # This way we can share out the CEDS emissions by pollutant to each state
    NEI_2008_2017_final %>%
      filter( year == 2008 ) %>%
      left_join_error_no_match( NEI_pollutant_mapping, by = c( "pollutant" = "NEI_pollutant" ) ) %>%
      filter( CEDS_Sector == "1A3dii_Domestic-navigation (shipping)" ) %>%
      group_by( Non.CO2 ) %>%
      mutate( sum = sum(emissions),
              share = emissions/sum ) %>%
      select( -c( emissions, sum, year ) ) -> NEI_2008_ship

    # Join the two tables, so that total US emissions from CEDS are matched for every share
    CEDS_domestic_ship %>%
      # calculate PM2.5 emissions based on BC and OC
      spread( Non.CO2, emissions ) %>%
      mutate( "PM2.5" =  ( BC + OC * gcamusa.OC_TO_OM ) * gcamusa.PM1_TO_PM2.5 ) %>%
      gather( Non.CO2, emissions, -year ) %>%
      # Full join and removing NA "share" and NA "emissions" because we only can scale in instances when we have pollutants in both datasets
      # The NA share removes BC, OC, CH4, and N2O (not in NEI)
      # The NA emissions removes PM10 (not in CEDS)
      full_join( NEI_2008_ship, by = c( "Non.CO2" ) ) %>%
      filter( !is.na( emissions ),
              !is.na( share ) ) %>%
      mutate( emissions = emissions * share ) %>%
      select( -c( "tier1_description", "share", "Non.CO2" ) ) -> domestic_ship_1990_2007

    # ======================================
    # 4. Combining Tables and Formatting
    # ======================================
    NEI_scaled_to_state_emissions %>%
      gather( id, emissions, -c( state, pollutant, CEDS_Sector, CEDS_Fuel, unit, GCAM_sector ) ) %>%
      separate( id, into = c("id", "year"), sep = "emissions" ) %>%
      select( -id ) %>%
      mutate( year = as.integer( year ) ) -> NEI_1990_2007_no_domestic_ship

    # assign Process CEDS_Fuel to process sectors
    NEI_1990_2007_no_domestic_ship %>%
      filter( !grepl( "^1A", CEDS_Sector ) ) %>%
      mutate( CEDS_Fuel = "Process" ) -> GCAM_Process_1990_2007

    NEI_1990_2007_no_domestic_ship %>%
      filter( grepl( "^1A", CEDS_Sector ) ) -> GCAM_Combustion_1990_2007

    GCAM_Process_1990_2007 %>%
      bind_rows( GCAM_Combustion_1990_2007 ) %>%
      distinct( state, year, pollutant, CEDS_Sector, CEDS_Fuel, unit, emissions, GCAM_sector ) %>%
      # add domestic shipping
      bind_rows( domestic_ship_1990_2007 ) -> NEI_1990_2007_final

    # Bind all of the historic year tables together
   NEI_1990_2007_final %>%
      bind_rows( NEI_2008_2017_final %>% select( -c( tier1_description ) ) ) %>%
      # make sure emissions are grouped by state, year, CEDS_sector, CEDS_Fuel and pollutant
      group_by( state, pollutant, CEDS_Sector, CEDS_Fuel, year ) %>%
      mutate( emissions = sum( emissions ) ) %>%
      distinct( state, pollutant, GCAM_sector, CEDS_Sector, CEDS_Fuel, unit, year, emissions ) %>%
      ungroup() -> NEI_1990_2017_GCAM_sectors

    # ===================================================
    # Produce outputs
   NEI_1990_2017_GCAM_sectors %>%
      add_title("Non-CO2 emissions by US state / CEDS Sector / CEDS Fuel / GCAM sector / pollutant / year") %>%
      add_units("TON") %>%
      add_comments("Psuedo-NEI emissions that have been interpolated and scaled to state level EPA Tier 1 data") %>%
      add_precursors("gcam-usa/emissions/state_tier1_caps",
                     "gcam-usa/emissions/NEI_emissions_to_CEDS_adj_2008",
                     "gcam-usa/emissions/NEI_emissions_to_CEDS_adj_2011",
                     "gcam-usa/emissions/NEI_emissions_to_CEDS_adj_2014",
                     "gcam-usa/emissions/NEI_emissions_to_CEDS_adj_2017",
                     "gcam-usa/emissions/CEDS_to_USEPA_Tier1_Mapping",
                     "gcam-usa/emissions/CEDS_to_GCAM_sector_mapping",
                     "gcam-usa/emissions/NEI_pollutant_mapping",
                     "L102.ceds_GFED_nonco2_tg_C_S_F") -> L169.NEI_1990_2017_GCAM_sectors_unscaled

    return_data(L169.NEI_1990_2017_GCAM_sectors_unscaled)
  } else {
    stop("Unknown command")
  }
}
