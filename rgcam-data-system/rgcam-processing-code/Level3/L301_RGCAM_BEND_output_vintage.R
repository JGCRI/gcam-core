# L301_RGCAM_BEND_output_vintage.R
# GCAM OUTPUT FOR ENERGY AND EFFICIENCY
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L301_RGCAM_BEND_output_vintage.R" )
printlog( "RGCAM_BEND output aggregated to BEND categories for energy consumption and energy efficiency" )

# -----------------------------------------------------------------------------
# 1. Read files
state_region_ind <- readmap( "state_region_ind" )
RGCAM_BEND_technology <- readdata( "RGCAM_BEND_technology" )
RGCAM_bld_energy_vintage <- readdata( "RGCAM_bld_energy_vintage" )
RGCAM_bld_service_vintage <- readdata( "RGCAM_bld_service_vintage" )

# -----------------------------------------------------------------------------
# 2. Aggregations and calculations
# 2a. ENERGY CONSUMPTION BY BEND TECHNOLOGY
#String split the technology name into technology and vintage
RGCAM_bld_energy_vintage$vintage <- substr( RGCAM_bld_energy_vintage$technology, nchar( RGCAM_bld_energy_vintage$technology ) - 3, nchar( RGCAM_bld_energy_vintage$technology ) )
RGCAM_bld_energy_vintage$technology <- substr( RGCAM_bld_energy_vintage$technology, 1, regexpr( ",", RGCAM_bld_energy_vintage$technology, fixed=TRUE ) - 1 )
RGCAM_bld_service_vintage$vintage <- substr( RGCAM_bld_service_vintage$technology, nchar( RGCAM_bld_service_vintage$technology ) - 3, nchar( RGCAM_bld_service_vintage$technology ) )
RGCAM_bld_service_vintage$technology <- substr( RGCAM_bld_service_vintage$technology, 1, regexpr( ",", RGCAM_bld_service_vintage$technology, fixed=TRUE ) - 1 )

#Drop 1990 manually because it is not necessary for this study
RGCAM_bld_energy_vintage <- subset( RGCAM_bld_energy_vintage, vintage != 1990 )
RGCAM_bld_service_vintage <- subset( RGCAM_bld_service_vintage, vintage != 1990 )

printlog( "Energy consumption: Matching BEND state and technology names into GCAM energy consumption output" )
RGCAM_bld_energy_vintage$state_name <- state_region_ind$state_name[ match( RGCAM_bld_energy_vintage$region, state_region_ind$state ) ]
RGCAM_bld_energy_vintage[ BEND_techID ] <- RGCAM_BEND_technology[
      match( paste( RGCAM_bld_energy_vintage$sector, RGCAM_bld_energy_vintage$technology ),
             paste( RGCAM_BEND_technology$supplysector, RGCAM_BEND_technology$technology ) ),
      BEND_techID ]

#Aggregate by state and BEND tech names
printlog( "Energy consumption: Aggregating GCAM output by BEND technology categories" )
L301_RGCAM_BEND_energy_vintage_Tbtu <- aggregate( RGCAM_bld_energy_vintage[ X_GCAM_out_years ] / conv_Tbtu_EJ,
      by=as.list( RGCAM_bld_energy_vintage[ c( "state_name", BEND_techID, "vintage" ) ] ), sum )
L301_RGCAM_BEND_energy_vintage_Tbtu[ X_GCAM_out_years ] <- round( L301_RGCAM_BEND_energy_vintage_Tbtu[ X_GCAM_out_years ], BEND_Tbtu_digits )

L301_RGCAM_BEND_energy_vintage_Tbtu <- L301_RGCAM_BEND_energy_vintage_Tbtu[ order( L301_RGCAM_BEND_energy_vintage_Tbtu$state_name,
      L301_RGCAM_BEND_energy_vintage_Tbtu$BEND_sector, L301_RGCAM_BEND_energy_vintage_Tbtu$BEND_service, L301_RGCAM_BEND_energy_vintage_Tbtu$BEND_fuel ), ]

# 2b. ENERGY EFFICIENCY BY BEND TECHNOLOGY AND VINTAGE
printlog( "Energy efficiency: Aggregate service and energy by the technologies being considered. Then divide" )
RGCAM_bld_service_vintage$state_name <- state_region_ind$state_name[ match( RGCAM_bld_service_vintage$region, state_region_ind$state ) ]
RGCAM_bld_service_vintage[ BEND_techID ] <- RGCAM_BEND_technology[
      match( paste( RGCAM_bld_service_vintage$sector, RGCAM_bld_service_vintage$technology ),
             paste( RGCAM_BEND_technology$supplysector, RGCAM_BEND_technology$technology ) ),
      BEND_techID ]
L301_RGCAM_BEND_service_vintage_Tbtu <- aggregate( RGCAM_bld_service_vintage[ X_GCAM_out_years ] / conv_Tbtu_EJ,
      by=as.list( RGCAM_bld_service_vintage[ c( "state_name", BEND_techID, "vintage" ) ] ), sum )
L301_RGCAM_BEND_service_vintage_Tbtu <- L301_RGCAM_BEND_service_vintage_Tbtu[ order( L301_RGCAM_BEND_service_vintage_Tbtu$state_name,
      L301_RGCAM_BEND_service_vintage_Tbtu$BEND_sector, L301_RGCAM_BEND_service_vintage_Tbtu$BEND_service, L301_RGCAM_BEND_service_vintage_Tbtu$BEND_fuel ), ]

#Check to make sure that the technology mapping corresponds exactly between these dataframes
if( any( L301_RGCAM_BEND_service_vintage_Tbtu[ c( "state_name", BEND_techID, "vintage" ) ] !=
         L301_RGCAM_BEND_energy_vintage_Tbtu[ c( "state_name", BEND_techID, "vintage" ) ] ) )
   { printlog( "ERROR: Mismatch between service output and energy consumption" )
     stop() }

#Calculate efficiency in a new table as service output divided by energy consumption
printlog( "Energy efficiency: Calculating efficiency as aggregated output divided by aggregated input" )
L301_RGCAM_BEND_efficiency_vintage <- L301_RGCAM_BEND_service_vintage_Tbtu
L301_RGCAM_BEND_efficiency_vintage[ X_GCAM_out_years ] <- round(
      L301_RGCAM_BEND_service_vintage_Tbtu[ X_GCAM_out_years ] / L301_RGCAM_BEND_energy_vintage_Tbtu[ X_GCAM_out_years ],
      BEND_efficiency_digits )

#Subset this to include only the new investment in each time period. This is the table to write out.
L301_RGCAM_BEND_efficiency_new.melt <- melt( L301_RGCAM_BEND_efficiency_vintage, measure.vars = X_GCAM_out_years )
L301_RGCAM_BEND_efficiency_new.melt <- subset( L301_RGCAM_BEND_efficiency_new.melt, vintage == substr( variable, 2, 5 ) )
L301_RGCAM_BEND_efficiency_new <- cast( L301_RGCAM_BEND_efficiency_new.melt, state_name + BEND_sector + BEND_service + BEND_fuel + BEND_technology ~ variable )

#Calculate service output shares by technology within service
printlog( "Service shares: portion of each service supplied by fuel and technology and vintage, within state and sector" )
printlog( "Service shares are only computed for new investment in each time period")
L301_RGCAM_BEND_service_vintage_Tbtu.melt <- melt( L301_RGCAM_BEND_service_vintage_Tbtu, measure.vars = X_GCAM_out_years )
L301_RGCAM_BEND_service_new_Tbtu.melt <- subset( L301_RGCAM_BEND_service_vintage_Tbtu.melt, vintage == substr( variable, 2, 5 ) )
L301_RGCAM_BEND_service_new_Tbtu <- cast( L301_RGCAM_BEND_service_new_Tbtu.melt,
      state_name + BEND_sector + BEND_service + BEND_fuel + BEND_technology ~ variable )

L301_RGCAM_BEND_service_new_Tbtu_serv <- aggregate( L301_RGCAM_BEND_service_new_Tbtu[ X_GCAM_out_years ],
      by=as.list( L301_RGCAM_BEND_service_new_Tbtu[ c( "state_name", BEND_serviceID ) ] ), sum )

L301_RGCAM_BEND_shares_new <- L301_RGCAM_BEND_service_new_Tbtu
L301_RGCAM_BEND_shares_new[ X_GCAM_out_years ] <- L301_RGCAM_BEND_service_new_Tbtu[ X_GCAM_out_years ] / L301_RGCAM_BEND_service_new_Tbtu_serv[
      match( paste( L301_RGCAM_BEND_service_new_Tbtu$state_name, L301_RGCAM_BEND_service_new_Tbtu$BEND_sector,
                    L301_RGCAM_BEND_service_new_Tbtu$BEND_service ), 
             paste( L301_RGCAM_BEND_service_new_Tbtu_serv$state_name, L301_RGCAM_BEND_service_new_Tbtu_serv$BEND_sector,
                    L301_RGCAM_BEND_service_new_Tbtu_serv$BEND_service ) ),
      X_GCAM_out_years ]

#Sort so that common techs are grouped together
L301_RGCAM_BEND_shares_new <- L301_RGCAM_BEND_shares_new[ order( L301_RGCAM_BEND_shares_new$state_name, L301_RGCAM_BEND_shares_new$BEND_sector,
      L301_RGCAM_BEND_shares_new$BEND_service, L301_RGCAM_BEND_shares_new$BEND_fuel ), ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L301_RGCAM_BEND_energy_vintage_Tbtu <- c( "Energy consumption by state / sector / service / fuel / technology / vintage","kBtu/yr" )
comments.L301_RGCAM_BEND_efficiency_new <- c( "Energy efficiency of new installations by state / sector / service / fuel / technology","Unitless COP" )
comments.L301_RGCAM_BEND_shares_new <- c( "New installations by fuel / technology within state / sector / service","Unitless portion" )

#write tables as CSV files
writedata( L301_RGCAM_BEND_energy_vintage_Tbtu,fn="L301_RGCAM_BEND_energy_vintage_Tbtu", comments=comments.L301_RGCAM_BEND_energy_vintage_Tbtu )
writedata( L301_RGCAM_BEND_efficiency_new,fn="L301_RGCAM_BEND_efficiency_new", comments=comments.L301_RGCAM_BEND_efficiency_new )
writedata( L301_RGCAM_BEND_shares_new,fn="L301_RGCAM_BEND_shares_new", comments=comments.L301_RGCAM_BEND_shares_new )

# Every script should finish with this line
logstop()
