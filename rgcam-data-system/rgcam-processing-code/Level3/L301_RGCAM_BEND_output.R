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
RGCAM_indtrn_elec <- readdata( "RGCAM_indtrn_elec" )

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

#Non-buildings electricity use. Need to write out the comm non-building, transportation, and industry. Keep separate for now
L301_RGCAM_nonbld_elec <- subset( RGCAM_bld_energy, sector == "comm non-building" & subsector == "electricity" )
L301_RGCAM_nonbld_elec$BEND_sector <- "Commercial non-building"
L301_RGCAM_nonbld_elec$BEND_fuel <- "electricity"
L301_RGCAM_nonbld_elec <- L301_RGCAM_nonbld_elec[ c( "state_name", "BEND_sector", "BEND_fuel", X_GCAM_out_years ) ]

#For industry and transportation, first need to remove the repeated pass-through sectors to avoid double counting
RGCAM_indtrn_elec <- subset( RGCAM_indtrn_elec, !sector %in% c( "elect_td_ind", "elect_td_trn" ) )
RGCAM_indtrn_elec$state_name <- state_region_ind$state_name[ match( RGCAM_indtrn_elec$region, state_region_ind$state ) ]
RGCAM_indtrn_elec$BEND_sector[ grepl( "trn", RGCAM_indtrn_elec$sector ) ] <- "Transportation"
RGCAM_indtrn_elec$BEND_sector[ !grepl( "trn", RGCAM_indtrn_elec$sector ) ] <- "Industry"
RGCAM_indtrn_elec$BEND_fuel <- "Electricity"
L301_RGCAM_indtrn_elec <- aggregate( RGCAM_indtrn_elec[ X_GCAM_out_years ],
      by=as.list( RGCAM_indtrn_elec[ c( "state_name", "BEND_sector", "BEND_fuel" ) ] ), sum )

#Merge these tables (commercial non-building, industry, and transportation) and convert the unit
L301_RGCAM_BEND_other_elec <- rbind( L301_RGCAM_nonbld_elec, L301_RGCAM_indtrn_elec )
L301_RGCAM_BEND_other_elec[ X_GCAM_out_years ] <- round( L301_RGCAM_BEND_other_elec[ X_GCAM_out_years ] / conv_Tbtu_EJ, BEND_Tbtu_digits )

#Write out the service demands, aggregated by state and sector and service. Heating and cooling only.
L301_RGCAM_BEND_service_agg_Tbtu <- aggregate( L301_RGCAM_BEND_service_Tbtu[ X_GCAM_out_years ],
      by=as.list( L301_RGCAM_BEND_service_Tbtu[ c( "state_name", "BEND_sector", "BEND_service" ) ] ), sum )
L301_RGCAM_BEND_service_agg_Tbtu <- subset( L301_RGCAM_BEND_service_agg_Tbtu, BEND_service %in% c( "Heating", "Cooling", "Ventilation" ) )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L301_RGCAM_BEND_energy_Tbtu <- c( "Energy consumption by state / sector / service / fuel / technology","Trillion btu/yr" )
comments.L301_RGCAM_BEND_efficiency <- c( "Energy efficiency by state / sector / service / fuel / technology","Unitless COP" )
comments.L301_RGCAM_BEND_shares <- c( "Service shares by fuel / technology within state / sector / service","Unitless portion" )
comments.L301_RGCAM_BEND_energy_Tbtu_2005 <- c( "Energy consumption by state / sector / selected service / fuel","Trillion btu/yr" )
comments.L301_RGCAM_BEND_other_elec <- c( "Electricity consumption by state / non-building sector","Trillion btu/yr" )
comments.L301_RGCAM_BEND_service_agg_Tbtu <- c( "Service demand by state / sector / selected service","Trillion btu/yr" )

#write tables as CSV files
writedata( L301_RGCAM_BEND_energy_Tbtu,fn="L301_RGCAM_BEND_energy_Tbtu", comments=comments.L301_RGCAM_BEND_energy_Tbtu )
writedata( L301_RGCAM_BEND_efficiency,fn="L301_RGCAM_BEND_efficiency", comments=comments.L301_RGCAM_BEND_efficiency )
writedata( L301_RGCAM_BEND_shares,fn="L301_RGCAM_BEND_shares", comments=comments.L301_RGCAM_BEND_shares )
writedata( L301_RGCAM_BEND_other_elec,fn="L301_RGCAM_BEND_other_elec", comments=comments.L301_RGCAM_BEND_other_elec )
writedata( L301_RGCAM_BEND_service_agg_Tbtu,fn="L301_RGCAM_BEND_service_agg_Tbtu", comments=comments.L301_RGCAM_BEND_service_agg_Tbtu )

# Every script should finish with this line
logstop()
