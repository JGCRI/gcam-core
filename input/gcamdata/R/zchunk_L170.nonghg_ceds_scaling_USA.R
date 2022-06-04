#' module_gcamusa_L170.nonghg_ceds_scaling_USA
#'
#' Scales pre-processed NEI non-CO2 emissions to CEDS at the national level
#' The NEI data is processed exogenously. It is scaled to EPA Tier 1, interpolated
#' between NEI years, extrapolated back to 1990, and mapped to GCAM sectors,
#' CEDS sectors, and CEDS fuels
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L170.NEI_1990_2017_GCAM_sectors}, \code{L170.NEI_CEDS_scaling_diagnostic}
#' @details Non-CO2 emissions from NEI scaled to CEDS
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select group_by
#' @importFrom tidyr gather spread
#' @author MAW February 2021

module_gcamusa_L170.nonghg_ceds_scaling_USA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE="gcam-usa/emissions/NEI_pollutant_mapping",
             FILE="gcam-usa/emissions/CEDS_to_CEDS_sector_mapping",
             "L169.NEI_1990_2017_GCAM_sectors_unscaled",
             "L102.ceds_GFED_nonco2_tg_C_S_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L170.NEI_1990_2017_GCAM_sectors",
             "L170.NEI_CEDS_scaling_diagnostic"))
  } else if(command == driver.MAKE) {

    # Silence package check.
    X <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    NEI_pollutant_mapping <- get_data(all_data, "gcam-usa/emissions/NEI_pollutant_mapping")
    CEDS_to_CEDS_sector_mapping <- get_data(all_data, "gcam-usa/emissions/CEDS_to_CEDS_sector_mapping")
    L102.ceds_GFED_nonco2_tg_C_S_F <- get_data(all_data, "L102.ceds_GFED_nonco2_tg_C_S_F")
    L169.NEI_1990_2017_GCAM_sectors_unscaled <- get_data(all_data, "L169.NEI_1990_2017_GCAM_sectors_unscaled", strip_attributes = T)
    # -----------------------------------------------------------------------------

    # Perform computations

 # Getting mapping file in order
    # Remove Notes column
    CEDS_to_CEDS <- CEDS_to_CEDS_sector_mapping %>%
      select( -Notes )

    # Remove Core_CEDS column from NEI mapping and NA entries so that we can use left_join_error_no_match
    US_SCC_CEDS_to_Agg <- CEDS_to_CEDS %>%
      select( -Core_CEDS ) %>%
      filter( !is.na( SCC_CEDS_extended ) )

    # Some Core_CEDS sectors have duplicate entries due to being further disaggregated in SCC_CEDS_extended sectors
    # ex. Core_CEDS = 1A1bc_Other-transformation maps to three SCC_CEDS_extended sectors
    # to remediate this, we need to remove the SCC_CEDS_extended column for the CEDS_to_CEDS mapping
    Core_CEDS_to_Agg <- CEDS_to_CEDS %>%
      select( -SCC_CEDS_extended ) %>%
      distinct( Core_CEDS, Agg_CEDS_for_scaling )

    # Convert CEDS from Tg to US Ton
    ceds_TON <- L102.ceds_GFED_nonco2_tg_C_S_F %>%
      mutate( emissions = emissions / CONV_TST_TG * 1000,
              units = "TON" )

 # ========== ROAD
 # The Road sector has a discontinuity in emissions, so we use a different scaling approach for BC, OC, PM2.5
    # From 2008 forward, we will scale CEDS to NEI
    # From 2007 back to 1990, we will scale CEDS using the 2008 scaling factor and linearly interpolate it to

    # Create a table that contains all of the NEI PM2.5 Road emissions
    NEI_road <- L169.NEI_1990_2017_GCAM_sectors_unscaled %>%
      left_join_error_no_match( US_SCC_CEDS_to_Agg, by = c( "CEDS_Sector" = "SCC_CEDS_extended" ) ) %>%
      left_join_error_no_match( NEI_pollutant_mapping, by = c( "pollutant" = "NEI_pollutant" ) ) %>%
      filter( Agg_CEDS_for_scaling == "1A3b_Road",
              Non.CO2 == "PM2.5",
              !is.na( CEDS_Fuel) )

    # 2008 forward
    # Sum NEI emissions by year, pollutant, and CEDS_Fuel
    NEI_PM2.5_road_sums_2008 <- NEI_road %>%
      filter( Agg_CEDS_for_scaling == "1A3b_Road",
              Non.CO2 == "PM2.5",
              year > 2007 ) %>%
      group_by( year, Non.CO2, CEDS_Fuel ) %>%
      mutate( NEI_sum = sum(emissions) ) %>%
      distinct( year, Non.CO2, NEI_sum, CEDS_Fuel )

    # Sum CEDS emissions by year, pollutant, and fuel
    CEDS_PM2.5_road_sums_2008 <- ceds_TON %>%
      filter( sector == "1A3b_Road",
              Non.CO2 == "BC" | Non.CO2 == "OC",
              year > 2007 ) %>%
      group_by( year, Non.CO2, fuel ) %>%
      mutate( CEDS_sum = sum(emissions) ) %>%
      distinct( year, Non.CO2, CEDS_sum, fuel ) %>%
      # make wide to calculate PM2.5
      spread( Non.CO2, CEDS_sum ) %>%
      # because CEDS does not have PM2.5, convert sum of BC + OC to PM2.5
      mutate( "PM2.5" =  ( BC + OC * gcamusa.OC_TO_OM ) * gcamusa.PM1_TO_PM2.5 ) %>%
      # make long again
      tidyr::gather( Non.CO2, CEDS_sum, -c( year, fuel ) ) %>%
      filter( Non.CO2 == "PM2.5" )

    # Join the tables to get scaling factors to apply to PM2.5, OC, and BC
    road_scaling <- NEI_PM2.5_road_sums_2008 %>%
      left_join( CEDS_PM2.5_road_sums_2008, by = c( "year", "Non.CO2", "CEDS_Fuel" = "fuel" ) ) %>%
      mutate( scaling_factor = CEDS_sum / NEI_sum ) %>%
      ungroup() %>%
      select( year, CEDS_Fuel, scaling_factor )

    # Scale CEDS BC and OC using this factor
    CEDS_road_scaled_2008 <- ceds_TON %>%
      filter( iso == "usa",
              sector == "1A3b_Road",
              Non.CO2 == "BC" | Non.CO2 == "OC",
              year > 2007 ) %>%
      left_join( road_scaling, by = c( "year", "fuel" = "CEDS_Fuel" ) ) %>%
      mutate( emissions = emissions / scaling_factor ) %>%
      # filter out NAs (these are years there isn't NEI data for, and also fuels with 0 emissions that become NA)
      filter( !is.na( emissions ) ) %>%
      select( -scaling_factor )

    # 2007 back
    # use the 2008 scaling factor, and linearly interpolate it to a constant in 1990
    road_scaling_1990_BC <- road_scaling %>%
      filter( year == 2008 ) %>%
      mutate( year = 1990,
              Non.CO2 = "BC",
              scaling_factor = gcamusa.BC_1990_ONROAD_SCALING_FACTOR )

    road_scaling_1990_BC_OC <- road_scaling_1990_BC %>%
      mutate( Non.CO2 = "OC",
              scaling_factor = gcamusa.OC_1990_ONROAD_SCALING_FACTOR ) %>%
      bind_rows( road_scaling_1990_BC)

    road_scaling_2008 <- road_scaling %>%
      filter( year == 2008 ) %>%
      repeat_add_columns(tibble::tibble(Non.CO2=unique(road_scaling_1990_BC_OC$Non.CO2)))

    road_scaling_2007_back <- road_scaling_2008 %>%
      bind_rows( road_scaling_1990_BC_OC ) %>%
      complete( nesting( CEDS_Fuel, Non.CO2 ), year = min(road_scaling_1990_BC_OC$year):max(road_scaling_2008$year)) %>%
      group_by( CEDS_Fuel, Non.CO2 ) %>%
      mutate( scaling_factor = approx_fun(year, scaling_factor, rule = 2) ) %>%
      unique()

    CEDS_road_scaled_2007_back <- ceds_TON %>%
      filter( iso == "usa",
              sector == "1A3b_Road",
              Non.CO2 == "BC" | Non.CO2 == "OC",
              year < 2008 ) %>%
      left_join( road_scaling_2007_back, by = c( "year", "fuel" = "CEDS_Fuel", "Non.CO2" ) ) %>%
      mutate( emissions = emissions / scaling_factor ) %>%
      select( -scaling_factor ) %>%
      # several NAs for fuels that have 0 emissions (brown_coal, coal_coke, etc.)
      # filter out NAs
      filter( !is.na( emissions ) )

    # Bind CEDS tables back together
    BC_OC_Road_CEDS <- CEDS_road_scaled_2007_back %>%
      bind_rows( CEDS_road_scaled_2008 ) %>%
      filter( emissions != 0 )

    # Table with NEI Road PM 2.5 for 2007 and before, to be scaled to CEDS nationally
    NEI_PM2.5_road_2007 <- NEI_road %>%
      filter( Agg_CEDS_for_scaling == "1A3b_Road",
              Non.CO2 == "PM2.5",
              year < 2008 )

    # Calculating PM2.5 equivalents for 2007 and before using CEDS BC and OC
    CEDS_PM2.5_road_sums_2007 <- BC_OC_Road_CEDS %>%
      filter( iso == "usa",
              sector == "1A3b_Road",
              Non.CO2 == "BC" | Non.CO2 == "OC",
              year < 2008 ) %>%
      # remove NA emissions in order for "sum" to work
      filter( !is.na( emissions ) ) %>%
      group_by( year, Non.CO2, fuel ) %>%
      mutate( CEDS_sum = sum(emissions) ) %>%
      distinct( year, Non.CO2, CEDS_sum, fuel ) %>%
      # make wide to calculate PM2.5
      spread( Non.CO2, CEDS_sum ) %>%
      mutate( "PM2.5" =  ( BC + OC * gcamusa.OC_TO_OM ) * gcamusa.PM1_TO_PM2.5 ) %>%
      # make long again
      tidyr::gather( Non.CO2, CEDS_sum, -c( year, fuel ) ) %>%
      filter( Non.CO2 == "PM2.5" )

    # Join the CEDS sums with the NEI PM2.5 values
    NEI_PM2.5_road_scaled_2007 <- NEI_PM2.5_road_2007 %>%
      # group emissions by year and fuel to get NEI_sum
      group_by( year, CEDS_Fuel ) %>%
      mutate( NEI_sum = sum( emissions ) ) %>%
      left_join( CEDS_PM2.5_road_sums_2007, by = c( "year", "Non.CO2", "CEDS_Fuel" = "fuel" ) ) %>%
      mutate( scaling_factor = NEI_sum / CEDS_sum,
              emissions = emissions/ scaling_factor ) %>%
      select( -c( NEI_sum, CEDS_sum, scaling_factor ) ) %>%
      # natural gas is all NAs since not in CEDS
      filter( !is.na(emissions) )

  # BIND AND FORMAT
    # Bind NEI PM2.5 2008 and later with NEI scaled PM2.5 2007 and before
    NEI_PM2.5_road <- NEI_road %>%
      filter( Agg_CEDS_for_scaling == "1A3b_Road",
              Non.CO2 == "PM2.5",
              year > 2007 ) %>%
      bind_rows( NEI_PM2.5_road_scaled_2007 ) %>%
      select( -pollutant )

  # Get CEDS BC and OC at state level
    # take CEDS national BC OC PM2.5 ratio and apply that to the states by CEDS fuel (diesel vs gasoline)
    fuel_ratio <- BC_OC_Road_CEDS %>%
      group_by( year, Non.CO2, fuel ) %>%
      mutate( CEDS_sum = sum(emissions) ) %>%
      # select relevant columns
      select( c( year, Non.CO2, CEDS_sum, fuel ) ) %>%
      # make wide to calculate PM2.5
      spread( Non.CO2, CEDS_sum ) %>%
      mutate( "PM2.5" =  ( BC + OC * gcamusa.OC_TO_OM ) * gcamusa.PM1_TO_PM2.5,
      # calculate BC and OC shares as a fraction of PM2.5
              "BC_frac" = BC / PM2.5,
              "OC_frac" = OC / PM2.5 ) %>%
      # select relevant columns
      select( -c( BC, OC, PM2.5 ) )

    # apply the fractions to the state level emissions data, basing BC and OC emissions
    # off of PM2.5 and the ratios calculated from CEDS
    road <- NEI_PM2.5_road %>%
      # rename "emissions" column PM2.5, as these are PM2.5 emissions
      rename( PM2.5 = emissions ) %>%
      left_join( fuel_ratio, by = c( "year", "CEDS_Fuel" = "fuel" ) ) %>%
      # calculate BC and OC emissions at the state level using the ratios
      mutate( BC = PM2.5 * BC_frac,
              OC = PM2.5 * OC_frac ) %>%
      # deselct the ratio columns, and gather the pollutant columns
      select( -c( BC_frac, OC_frac ) ) %>%
      tidyr::gather( key = "Non.CO2", value = "emissions", "PM2.5", "BC", "OC" ) %>%
      filter( !is.na( emissions ) )


  # ========== NON-ROAD
  # Biodiesel and Ethanol Production do not have emissions in CEDS, so these
  # sectors will be filtered out and we will use emissions straight from NEI
  # Petroleum Refining is in CEDS, but we will use emissions straight from NEI
  # because the scaling reduced these emissions too much
    biodiesel_ethanol_prod_petr_refining <- L169.NEI_1990_2017_GCAM_sectors_unscaled %>%
      left_join_error_no_match( US_SCC_CEDS_to_Agg, by = c( "CEDS_Sector" = "SCC_CEDS_extended" ) ) %>%
      left_join_error_no_match( NEI_pollutant_mapping, by = c( "pollutant" = "NEI_pollutant" ) ) %>%
      select( -c( pollutant ) ) %>%
      filter( Agg_CEDS_for_scaling == "1A1bc_Other-transformation" |
              CEDS_Sector == "1A1b_Pet-refining")


  # For all other sectors / pollutants other than Road PM2.5, we will scale to CEDS
    # First, bind mapping files to NEI table so this only has to be done once
    NEI_all_unscaled_no_road <- L169.NEI_1990_2017_GCAM_sectors_unscaled %>%
      left_join_error_no_match( US_SCC_CEDS_to_Agg, by = c( "CEDS_Sector" = "SCC_CEDS_extended" ) ) %>%
      left_join_error_no_match( NEI_pollutant_mapping, by = c( "pollutant" = "NEI_pollutant" ) ) %>%
      select( -c( pollutant ) ) %>%
      # filter out PM2.5 from the road sectors
      filter( Non.CO2 != "PM2.5" | Agg_CEDS_for_scaling != "1A3b_Road",
              Agg_CEDS_for_scaling != "1A1bc_Other-transformation",
              CEDS_Sector != "1A1b_Pet-refining" )

    # Create a table that has CEDS sums by year, aggregate sector, and Non.CO2
    # This will be compared to NEI sums by year, aggregate sector, and Non.CO2
    CEDS_sums <- ceds_TON %>%
      filter( iso == "usa",
              # filter out Road BC and OC
              sector != "1A3b_Road" | Non.CO2 != "BC" & Non.CO2 != "OC" ) %>%
      left_join_error_no_match( Core_CEDS_to_Agg, by = c( "sector" = "Core_CEDS") ) %>%
      select( -c( sector, CEDS_agg_sector, CEDS_agg_fuel ) ) %>%
      # group by year and pollutant, and sum emissions
      dplyr::group_by( year, Non.CO2, Agg_CEDS_for_scaling ) %>%
      dplyr::mutate( CEDS_emissions = sum( emissions ) ) %>%
      distinct( year, CEDS_emissions, Non.CO2, Agg_CEDS_for_scaling )


    # Create a table that has NEI sums by year, aggregate sector, and Non.CO2
    NEI_sums <- NEI_all_unscaled_no_road %>%
      dplyr::group_by( year, Non.CO2, Agg_CEDS_for_scaling ) %>%
      dplyr::mutate( NEI_emissions = sum( emissions ) ) %>%
      distinct( year, NEI_emissions, Non.CO2, Agg_CEDS_for_scaling ) %>%
      select( c( "year", "NEI_emissions", "Agg_CEDS_for_scaling", "Non.CO2" ) )

    # calculate an NEI to CEDS scaling factor for each year to multiply NEI emissions by
    NEI_CEDS_scaling <- NEI_sums %>%
      # can't use left_join_error_no_match because NEI has pollutants CEDS doesn't and vice versa
      # CEDS has BC, CH4, OC, N20, H2
      # NEI has PM2.5, PM10
      # pollutants in both: NH3, CO, NOx, SO2, NMVOC
      left_join( CEDS_sums, by = c( "year", "Non.CO2", "Agg_CEDS_for_scaling" ) ) %>%
      filter( Agg_CEDS_for_scaling != "not in gcam",
              Non.CO2 == "NH3" | Non.CO2 == "CO" | Non.CO2 == "NOx" | Non.CO2 == "SO2" | Non.CO2 == "NMVOC" ) %>%
      mutate( scaling_factor = NEI_emissions/CEDS_emissions )
      # scaling factors are "Inf" when NEI has emissions that are 0 in CEDS, NaN when emissions are 0 in both,
      # and 0 when CEDS has emissions not in NEI

    # diagnostic table to check scaling factors
    NEI_CEDS_scaling_diagnostic <- NEI_CEDS_scaling %>%
      select( -c( NEI_emissions, CEDS_emissions ) ) %>%
      filter( Non.CO2 != "PM2.5" & Non.CO2 != "PM10" ) %>%
      spread( key = year, value = scaling_factor ) %>%
      ungroup()

    # CEDS doesn't have PM2.5 or PM10, so these pollutants will be filtered out of NEI and not scaled
    NEI_noPM <- NEI_all_unscaled_no_road %>%
      filter( Non.CO2 != "PM2.5" & Non.CO2 != "PM10",
              Agg_CEDS_for_scaling != "not in gcam" ) %>%
      left_join( NEI_CEDS_scaling, by = c( "year", "Non.CO2", "Agg_CEDS_for_scaling" ) ) %>%
      mutate( emissions = emissions / scaling_factor ) %>%
      select( -c( NEI_emissions, CEDS_emissions, scaling_factor ) )

    # Table containing PM2.5 and PM10 emissions, bound with other emissions that were scaled to CEDS
    NEI_1990_2017_GCAM_sectors <- NEI_all_unscaled_no_road %>%
      filter( Non.CO2 == "PM2.5" | Non.CO2 == "PM10" ) %>%
      bind_rows( NEI_noPM ) %>%
      mutate( unit = "TON" ) %>%
      # add back the Road, biodiesel and ethanol production, and petroleum refining emissions
      bind_rows( road, biodiesel_ethanol_prod_petr_refining ) %>%
      # 5C_Open-burning-land-clearing are not used in GCAM, and not mapped to a GCAM Sector
      filter( !is.na( GCAM_sector ), GCAM_sector != 0 )

    # NA entries (emissions) become 0 in order for emissions sums to work (otherwise NA is returned)
    # The NAs are in Agg_CEDS_for_scaling "3B_Manure-management", for CO, NOx, and SO2 in 2014 - 2017
    # where the scaling factor is NaN or 0
    NEI_1990_2017_GCAM_sectors <- as.data.frame(NEI_1990_2017_GCAM_sectors)
    NEI_1990_2017_GCAM_sectors[is.na(NEI_1990_2017_GCAM_sectors)] <- 0
    NEI_1990_2017_GCAM_sectors <- as_tibble(NEI_1990_2017_GCAM_sectors)

    # Process into gcam datasystem format
    NEI_1990_2017_GCAM_sectors_final <- NEI_1990_2017_GCAM_sectors %>%
      left_join_error_no_match( NEI_pollutant_mapping, by = c( "Non.CO2" ) ) %>%
      rename( "pollutant" = "NEI_pollutant" ) %>%
      select( -c( Non.CO2, Agg_CEDS_for_scaling, CEDS_Sector ) )

    # ===================================================
    # Produce outputs
    L170.NEI_1990_2017_GCAM_sectors <- NEI_1990_2017_GCAM_sectors_final %>%
      add_title("Non-CO2 emissions by US state / GCAM sector / fuel / pollutant / year") %>%
      add_units("TON") %>%
      add_comments("Psuedo-NEI emissions that have been scaled to CEDS at the national level, after being scaled to state level tier1 data in an exogenous script") %>%
      add_precursors("gcam-usa/emissions/NEI_pollutant_mapping",
                     "gcam-usa/emissions/CEDS_to_CEDS_sector_mapping",
                     "L102.ceds_GFED_nonco2_tg_C_S_F",
                     "L169.NEI_1990_2017_GCAM_sectors_unscaled")

    L170.NEI_CEDS_scaling_diagnostic <- NEI_CEDS_scaling_diagnostic %>%
      add_title("Scaling factors used to scale NEI emissions to match CEDS nationally (USA) by aggregate sector") %>%
      add_units("Unitless") %>%
      add_comments("These factors can be used in post processing, particularly for 1A3aii_Domestic-aviation and 1A3dii_Domestic-navigation (shipping)") %>%
      add_comments("These sectors include different things between NEI and CEDS, so can be used to convert back to what is in NEI. Factor is for NEI/CEDS") %>%
      add_precursors("gcam-usa/emissions/NEI_pollutant_mapping",
                     "gcam-usa/emissions/CEDS_to_CEDS_sector_mapping",
                     "L102.ceds_GFED_nonco2_tg_C_S_F",
                     "L169.NEI_1990_2017_GCAM_sectors_unscaled")


    return_data(L170.NEI_1990_2017_GCAM_sectors, L170.NEI_CEDS_scaling_diagnostic)
  } else {
    stop("Unknown command")
  }
}
