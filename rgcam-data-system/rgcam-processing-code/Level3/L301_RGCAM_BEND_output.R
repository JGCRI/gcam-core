# L301_RGCAM_BEND_output.R
# GCAM OUTPUT FOR ENERGY AND EFFICIENCY
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L301_RGCAM_BEND_output.R" )
printlog( "RGCAM_BEND output aggregated to BEND categories for energy consumption and energy efficiency" )

# -----------------------------------------------------------------------------
# 1. Read files
state_region_ind <- readmap( "state_region_ind" )
A_BEND_technology <- readdata( "A_BEND_technology" )
A_BEND_Tprteff <- readdata( "A_BEND_Tprteff" )
BEND_energy <- readdata( "BEND_energy" )
RGCAM_bld_energy <- readdata( "RGCAM_bld_energy" )
RGCAM_bld_service <- readdata( "RGCAM_bld_service" )

# -----------------------------------------------------------------------------
# 2. Aggregations and calculations
# 2a. ENERGY CONSUMPTION BY BEND TECHNOLOGY
printlog( "Energy consumption: Matching BEND state and technology names into GCAM energy consumption output" )
RGCAM_bld_energy$state_name <- state_region_ind$state_name[ match( RGCAM_bld_energy$region, state_region_ind$state ) ]
RGCAM_bld_energy[ BEND_techID ] <- A_BEND_technology[
      match( paste( RGCAM_bld_energy$sector, RGCAM_bld_energy$technology ),
             paste( A_BEND_technology$supplysector, A_BEND_technology$technology ) ),
      BEND_techID ]

#Aggregate by state and BEND tech names
printlog( "Energy consumption: Aggregating GCAM output by BEND technology categories" )
L301_RGCAM_BEND_energy_kbtu <- aggregate( ( RGCAM_bld_energy[ X_GCAM_out_years ] / conv_kbtu_EJ ),
      list( state_name = RGCAM_bld_energy$state_name, BEND_sector = RGCAM_bld_energy$BEND_sector, BEND_service = RGCAM_bld_energy$BEND_service,
            BEND_fuel = RGCAM_bld_energy$BEND_fuel, BEND_technology = RGCAM_bld_energy$BEND_technology ),
      sum )
L301_RGCAM_BEND_energy_kbtu[ X_GCAM_out_years ] <- round( L301_RGCAM_BEND_energy_kbtu[ X_GCAM_out_years ], BEND_kbtu_digits )


#Remove biomass, coal, non-building energy, and fuel inputs to district services (all techs not in BEND)
printlog( "Energy consumption: Removing non-applicable technologies and sorting" )
L301_RGCAM_BEND_energy_kbtu <- L301_RGCAM_BEND_energy_kbtu[
      paste( L301_RGCAM_BEND_energy_kbtu$BEND_sector, L301_RGCAM_BEND_energy_kbtu$BEND_service,
             L301_RGCAM_BEND_energy_kbtu$BEND_fuel, L301_RGCAM_BEND_energy_kbtu$BEND_technology ) %in%
      paste( BEND_energy$BEND_sector, BEND_energy$BEND_service,
             BEND_energy$BEND_fuel, BEND_energy$BEND_technology ), ]

L301_RGCAM_BEND_energy_kbtu <- L301_RGCAM_BEND_energy_kbtu[ order( L301_RGCAM_BEND_energy_kbtu$state_name,
      L301_RGCAM_BEND_energy_kbtu$BEND_sector, L301_RGCAM_BEND_energy_kbtu$BEND_service, L301_RGCAM_BEND_energy_kbtu$BEND_fuel ), ]

# 2b. ENERGY EFFICIENCY BY BEND TECHNOLOGY
printlog( "Energy efficiency: Matching BEND state and technology names into GCAM output for service output" )
RGCAM_bld_service$state_name <- state_region_ind$state_name[ match( RGCAM_bld_service$region, state_region_ind$state ) ]
RGCAM_bld_service[ BEND_techID ] <- A_BEND_technology[
      match( paste( RGCAM_bld_service$sector, RGCAM_bld_service$technology ),
             paste( A_BEND_technology$supplysector, A_BEND_technology$technology ) ),
      BEND_techID ]

# subset relevant technologies from both energy and service tables
printlog( "Energy efficiency: Subsetting only technologies whose energy efficiency rating is being tracked" )
L301_RGCAM_BEND_energy_tech <- RGCAM_bld_energy[
      paste( RGCAM_bld_energy$BEND_sector, RGCAM_bld_energy$BEND_service, RGCAM_bld_energy$BEND_fuel, RGCAM_bld_energy$BEND_technology ) %in%
      paste( BEND_efficiency$BEND_sector, BEND_efficiency$BEND_service, BEND_efficiency$BEND_fuel, BEND_efficiency$BEND_technology ), ]
L301_RGCAM_BEND_service_tech <- RGCAM_bld_service[
      paste( RGCAM_bld_service$BEND_sector, RGCAM_bld_service$BEND_service, RGCAM_bld_service$BEND_fuel, RGCAM_bld_service$BEND_technology ) %in%
      paste( BEND_efficiency$BEND_sector, BEND_efficiency$BEND_service, BEND_efficiency$BEND_fuel, BEND_efficiency$BEND_technology ), ]

#Aggregate by state and BEND technology
printlog( "Energy efficiency: Aggregating energy inputs and service outputs by technology category" )
L301_RGCAM_BEND_energy <- aggregate( L301_RGCAM_BEND_energy_tech[ X_GCAM_out_years ],
      list( state_name = L301_RGCAM_BEND_energy_tech$state_name, BEND_sector = L301_RGCAM_BEND_energy_tech$BEND_sector,
            BEND_service = L301_RGCAM_BEND_energy_tech$BEND_service, BEND_fuel = L301_RGCAM_BEND_energy_tech$BEND_fuel,
            BEND_technology = L301_RGCAM_BEND_energy_tech$BEND_technology ), sum )
L301_RGCAM_BEND_service <- aggregate( L301_RGCAM_BEND_service_tech[ X_GCAM_out_years ],
      list( state_name = L301_RGCAM_BEND_service_tech$state_name, BEND_sector = L301_RGCAM_BEND_service_tech$BEND_sector,
            BEND_service = L301_RGCAM_BEND_service_tech$BEND_service, BEND_fuel = L301_RGCAM_BEND_service_tech$BEND_fuel,
            BEND_technology = L301_RGCAM_BEND_service_tech$BEND_technology ), sum )

#Check to make sure that the technology mapping corresponds exactly between these dataframes
if( any( L301_RGCAM_BEND_service[ c( "state_name", BEND_techID ) ] != L301_RGCAM_BEND_energy[ c( "state_name", BEND_techID ) ] ) )
   { printlog( "ERROR: Mismatch between service output and energy consumption" )
     stop() }

#Calculate efficiency in a new table as service output divided by energy consumption
printlog( "Energy efficiency: Calculating efficiency as aggregated output divided by aggregated input" )
L301_RGCAM_BEND_efficiency <- L301_RGCAM_BEND_service
L301_RGCAM_BEND_efficiency[ X_GCAM_out_years ] <- round(
      L301_RGCAM_BEND_service[ X_GCAM_out_years ] / L301_RGCAM_BEND_energy[ X_GCAM_out_years ],
      BEND_efficiency_digits )

L301_RGCAM_BEND_efficiency <- L301_RGCAM_BEND_efficiency[ order( L301_RGCAM_BEND_efficiency$state_name,
      L301_RGCAM_BEND_efficiency$BEND_sector, L301_RGCAM_BEND_efficiency$BEND_service, L301_RGCAM_BEND_efficiency$BEND_fuel ), ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L301_RGCAM_BEND_energy_kbtu <- c( "Energy consumption by state / sector / service / fuel / technology","kBtu/yr" )
comments.L301_RGCAM_BEND_efficiency <- c( "Energy efficiency by state / sector / service / fuel / technology","Unitless COP" )

#write tables as CSV files
writedata( L301_RGCAM_BEND_energy_kbtu,fn="L301_RGCAM_BEND_energy_kbtu", comments=comments.L301_RGCAM_BEND_energy_kbtu )
writedata( L301_RGCAM_BEND_efficiency,fn="L301_RGCAM_BEND_efficiency", comments=comments.L301_RGCAM_BEND_efficiency )

# Every script should finish with this line
logstop()





