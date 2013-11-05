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
RGCAM_BEND_technology <- readdata( "RGCAM_BEND_technology" )
RGCAM_bld_energy <- readdata( "RGCAM_bld_energy" )
RGCAM_bld_service <- readdata( "RGCAM_bld_service" )

# -----------------------------------------------------------------------------
# 2. Aggregations and calculations
# 2a. ENERGY CONSUMPTION BY BEND TECHNOLOGY
printlog( "Energy consumption: Matching BEND state and technology names into GCAM energy consumption output" )
RGCAM_bld_energy$state_name <- state_region_ind$state_name[ match( RGCAM_bld_energy$region, state_region_ind$state ) ]
RGCAM_bld_energy[ BEND_techID ] <- RGCAM_BEND_technology[
      match( paste( RGCAM_bld_energy$sector, RGCAM_bld_energy$technology ),
             paste( RGCAM_BEND_technology$supplysector, RGCAM_BEND_technology$technology ) ),
      BEND_techID ]

#Aggregate by state and BEND tech names
printlog( "Energy consumption: Aggregating GCAM output by BEND technology categories" )
L301_RGCAM_BEND_energy_Tbtu <- aggregate( RGCAM_bld_energy[ X_GCAM_out_years ] / conv_Tbtu_EJ,
      by=as.list( RGCAM_bld_energy[ c( "state_name", BEND_techID ) ] ), sum )
L301_RGCAM_BEND_energy_Tbtu[ X_GCAM_out_years ] <- round( L301_RGCAM_BEND_energy_Tbtu[ X_GCAM_out_years ], BEND_Tbtu_digits )

L301_RGCAM_BEND_energy_Tbtu <- L301_RGCAM_BEND_energy_Tbtu[ order( L301_RGCAM_BEND_energy_Tbtu$state_name,
      L301_RGCAM_BEND_energy_Tbtu$BEND_sector, L301_RGCAM_BEND_energy_Tbtu$BEND_service, L301_RGCAM_BEND_energy_Tbtu$BEND_fuel ), ]

# 2b. ENERGY EFFICIENCY BY BEND TECHNOLOGY
printlog( "Energy efficiency: Aggregate service and energy by the technologies being considered. Then divide" )
RGCAM_bld_service$state_name <- state_region_ind$state_name[ match( RGCAM_bld_service$region, state_region_ind$state ) ]
RGCAM_bld_service[ BEND_techID ] <- RGCAM_BEND_technology[
      match( paste( RGCAM_bld_service$sector, RGCAM_bld_service$technology ),
             paste( RGCAM_BEND_technology$supplysector, RGCAM_BEND_technology$technology ) ),
      BEND_techID ]
L301_RGCAM_BEND_service_Tbtu <- aggregate( RGCAM_bld_service[ X_GCAM_out_years ] / conv_Tbtu_EJ,
      by=as.list( RGCAM_bld_service[ c( "state_name", BEND_techID ) ] ), sum )
L301_RGCAM_BEND_service_Tbtu <- L301_RGCAM_BEND_service_Tbtu[ order( L301_RGCAM_BEND_service_Tbtu$state_name,
      L301_RGCAM_BEND_service_Tbtu$BEND_sector, L301_RGCAM_BEND_service_Tbtu$BEND_service, L301_RGCAM_BEND_service_Tbtu$BEND_fuel ), ]

#Check to make sure that the technology mapping corresponds exactly between these dataframes
if( any( L301_RGCAM_BEND_service_Tbtu[ c( "state_name", BEND_techID ) ] != L301_RGCAM_BEND_energy_Tbtu[ c( "state_name", BEND_techID ) ] ) )
   { printlog( "ERROR: Mismatch between service output and energy consumption" )
     stop() }

#Calculate efficiency in a new table as service output divided by energy consumption
printlog( "Energy efficiency: Calculating efficiency as aggregated output divided by aggregated input" )
L301_RGCAM_BEND_efficiency <- L301_RGCAM_BEND_service_Tbtu
L301_RGCAM_BEND_efficiency[ X_GCAM_out_years ] <- round(
      L301_RGCAM_BEND_service_Tbtu[ X_GCAM_out_years ] / L301_RGCAM_BEND_energy_Tbtu[ X_GCAM_out_years ],
      BEND_efficiency_digits )

#Calculate service output shares by technology within service
printlog( "Service shares: portion of each service supplied by fuel and technology, within state and sector" )
L301_RGCAM_BEND_service_Tbtu_serv <- aggregate( RGCAM_bld_service[ X_GCAM_out_years ] / conv_Tbtu_EJ,
      by=as.list( RGCAM_bld_service[ c( "state_name", BEND_serviceID ) ] ), sum )

L301_RGCAM_BEND_shares <- L301_RGCAM_BEND_service_Tbtu
L301_RGCAM_BEND_shares[ X_GCAM_out_years ] <- L301_RGCAM_BEND_service_Tbtu[ X_GCAM_out_years ] / L301_RGCAM_BEND_service_Tbtu_serv[
      match( paste( L301_RGCAM_BEND_service_Tbtu$state_name, L301_RGCAM_BEND_service_Tbtu$BEND_sector, L301_RGCAM_BEND_service_Tbtu$BEND_service ), 
             paste( L301_RGCAM_BEND_service_Tbtu_serv$state_name, L301_RGCAM_BEND_service_Tbtu_serv$BEND_sector, L301_RGCAM_BEND_service_Tbtu_serv$BEND_service ) ),
      X_GCAM_out_years ]

#Sort so that common techs are grouped together
L301_RGCAM_BEND_shares <- L301_RGCAM_BEND_shares[ order( L301_RGCAM_BEND_shares$state_name, L301_RGCAM_BEND_shares$BEND_sector,
      L301_RGCAM_BEND_shares$BEND_service, L301_RGCAM_BEND_shares$BEND_fuel ), ]

#Final step: write out electricity and fossil energy consumption by state and sector and service, with heating/cooling disaggregated
L301_RGCAM_BEND_energy_Tbtu_2005 <- aggregate( L301_RGCAM_BEND_energy_Tbtu[ "X2005" ],
      by=as.list( L301_RGCAM_BEND_energy_Tbtu[ c( "state_name", "BEND_sector", "BEND_fuel" ) ] ), sum )
L301_RGCAM_BEND_energy_Tbtu_2005$BEND_service <- "All"

L301_RGCAM_BEND_energy_Tbtu_2005HVAC <- aggregate( L301_RGCAM_BEND_energy_Tbtu[
         L301_RGCAM_BEND_energy_Tbtu$BEND_service %in% c( "Heating", "Cooling" ), ][ "X2005" ],
      by=as.list( L301_RGCAM_BEND_energy_Tbtu[
         L301_RGCAM_BEND_energy_Tbtu$BEND_service %in% c( "Heating", "Cooling" ), c( "state_name", "BEND_sector", "BEND_service", "BEND_fuel" ) ] ), sum )

L301_RGCAM_BEND_energy_Tbtu_2005 <- rbind( L301_RGCAM_BEND_energy_Tbtu_2005HVAC, L301_RGCAM_BEND_energy_Tbtu_2005 )
L301_RGCAM_BEND_energy_Tbtu_2005 <- cast( L301_RGCAM_BEND_energy_Tbtu_2005, state_name + BEND_sector + BEND_service ~ BEND_fuel, value = "X2005" )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L301_RGCAM_BEND_energy_Tbtu <- c( "Energy consumption by state / sector / service / fuel / technology","Trillion btu/yr" )
comments.L301_RGCAM_BEND_efficiency <- c( "Energy efficiency by state / sector / service / fuel / technology","Unitless COP" )
comments.L301_RGCAM_BEND_shares <- c( "Service shares by fuel / technology within state / sector / service","Unitless portion" )
comments.L301_RGCAM_BEND_energy_Tbtu_2005 <- c( "Energy consumption by state / sector / selected service / fuel","Trillion btu/yr" )

#write tables as CSV files
writedata( L301_RGCAM_BEND_energy_Tbtu,fn="L301_RGCAM_BEND_energy_Tbtu", comments=comments.L301_RGCAM_BEND_energy_Tbtu )
writedata( L301_RGCAM_BEND_efficiency,fn="L301_RGCAM_BEND_efficiency", comments=comments.L301_RGCAM_BEND_efficiency )
writedata( L301_RGCAM_BEND_shares,fn="L301_RGCAM_BEND_shares", comments=comments.L301_RGCAM_BEND_shares )
writedata( L301_RGCAM_BEND_energy_Tbtu_2005,fn="L301_RGCAM_BEND_energy_Tbtu_2005", comments=comments.L301_RGCAM_BEND_energy_Tbtu_2005 )

# Every script should finish with this line
logstop()
