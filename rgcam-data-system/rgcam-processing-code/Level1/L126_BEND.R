# L126_BEND_.R
# PROCESSING OF ENERGY AND EFFICIENCY DATA FROM BEND
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L126_BEND.R" )
printlog( "Processing of energy and efficiency data from BEND" )

# -----------------------------------------------------------------------------
# 1. Read files

state_region_ind <- readmap( "state_region_ind" )
A_BEND_technology <- readdata( "A_BEND_technology" )
A_BEND_distserv <- readdata( "A_BEND_distserv" )
BEND_energy <- readdata( "BEND_energy" )
EIA_distheat <- readdata( "EIA_distheat" )
BEND_efficiency <- readdata( "BEND_efficiency" )
L104_in_EJ_state_bld_F <- readdata( "L104_in_EJ_state_bld_F" )
L122_in_EJ_state_commext_elec <- readdata( "L122_in_EJ_state_commext_elec" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2A: ENERGY CONSUMPTION
printlog( "Initial processing of BEND energy data" )
#Melt energy table from BEND and match in state names
L126_BEND_energy.melt <- melt( BEND_energy, id.vars = BEND_techID )
#Spaces in state names are indicated in this database with periods. Replace this.
L126_BEND_energy.melt$state_name <- gsub( "."," ", L126_BEND_energy.melt$variable, fixed=T )
L126_BEND_energy.melt$state <- state_region_ind$state[
      match( L126_BEND_energy.melt$state_name, state_region_ind$state_name ) ]

#Add in a vector for "BEND_consumer" to allow differentiation between district services sector and buildings sector
L126_BEND_energy.melt$BEND_consumer <- L126_BEND_energy.melt$BEND_sector

#Convert energy units
L126_BEND_energy.melt$in_EJ <- L126_BEND_energy.melt$value * conv_kbtu_EJ

#DISTRICT HEATING INPUTS AND OUTPUTS BY STATE
printlog( "Disaggregating district service consumption in order to back out the fuel inputs to district services" )
#Subset district services to compute the implied fuel inputs to district services
L126_in_EJ_distserv_BEND <- L126_BEND_energy.melt[
      paste( L126_BEND_energy.melt$BEND_sector, L126_BEND_energy.melt$BEND_service, L126_BEND_energy.melt$BEND_fuel, L126_BEND_energy.melt$BEND_technology ) %in%
      paste( A_BEND_distserv$BEND_sector, A_BEND_distserv$BEND_service, A_BEND_distserv$BEND_fuel, A_BEND_distserv$BEND_technology ),
      c( "state", "BEND_consumer", BEND_techID, "in_EJ" ) ]

#Match in the sector and service names
#NOTE: For district services, we need a distinction between the "consumer" (resid, comm) and the producing "sector" (district services)
L126_in_EJ_distserv_BEND$GCAM_consumer <- A_BEND_technology$GCAM_sector[
      match( L126_in_EJ_distserv_BEND$BEND_sector, A_BEND_technology$BEND_sector ) ]
L126_in_EJ_distserv_BEND$GCAM_sector <- A_BEND_distserv$GCAM_sector[
      match( paste( L126_in_EJ_distserv_BEND$BEND_sector, L126_in_EJ_distserv_BEND$BEND_service, L126_in_EJ_distserv_BEND$BEND_fuel, L126_in_EJ_distserv_BEND$BEND_technology ),
             paste( A_BEND_distserv$BEND_sector, A_BEND_distserv$BEND_service, A_BEND_distserv$BEND_fuel, A_BEND_distserv$BEND_technology ) ) ]
L126_in_EJ_distserv_BEND$service <- A_BEND_distserv$service[
      match( paste( L126_in_EJ_distserv_BEND$BEND_sector, L126_in_EJ_distserv_BEND$BEND_service, L126_in_EJ_distserv_BEND$BEND_fuel, L126_in_EJ_distserv_BEND$BEND_technology ),
             paste( A_BEND_distserv$BEND_sector, A_BEND_distserv$BEND_service, A_BEND_distserv$BEND_fuel, A_BEND_distserv$BEND_technology ) ) ]

#Specify fuel names, and match in the efficiencies and shares to calculate inputs and outputs
#Repeat by number of fuels in EIA_distheat table
printlog( "Deriving fuel inputs to district services using exogenous efficiencies per service and fuel" )
L126_in_EJ_state_distserv_F_U <- L126_in_EJ_distserv_BEND[ rep( 1:nrow( L126_in_EJ_distserv_BEND ),
      times = length( unique( EIA_distheat$fuel ) ) ), ]
L126_in_EJ_state_distserv_F_U$fuel <- sort( rep( unique( EIA_distheat$fuel ), length.out = nrow( L126_in_EJ_state_distserv_F_U ) ) )
L126_in_EJ_state_distserv_F_U$share <- EIA_distheat$share[
      match( paste( L126_in_EJ_state_distserv_F_U$service, L126_in_EJ_state_distserv_F_U$fuel ),
             paste( EIA_distheat$service, EIA_distheat$fuel ) ) ]
L126_in_EJ_state_distserv_F_U$efficiency <- EIA_distheat$efficiency[
      match( paste( L126_in_EJ_state_distserv_F_U$service, L126_in_EJ_state_distserv_F_U$fuel ),
             paste( EIA_distheat$service, EIA_distheat$fuel ) ) ]
L126_in_EJ_state_distserv_F_U$dist_out_EJ <- L126_in_EJ_state_distserv_F_U$in_EJ * L126_in_EJ_state_distserv_F_U$share
L126_in_EJ_state_distserv_F_U$dist_in_EJ <- L126_in_EJ_state_distserv_F_U$dist_out_EJ / L126_in_EJ_state_distserv_F_U$efficiency

#Aggregate inputs and outputs by state, GCAM sector, and fuel
#NOTE: This step assumes that there will only be one district services sector per state, shared by all consumers (res/comm) and service output (heat/cooling/hot water)
#  Differences in service provision will be reflected in the calculated efficiencies, as the different services (and fuels) have different efficiencies
printlog( "NOTE: only modeling one district service sector per state" )
L126_in_EJ_state_distserv_F <- aggregate( L126_in_EJ_state_distserv_F_U[ c( "dist_out_EJ", "dist_in_EJ" ) ],
      list( state = L126_in_EJ_state_distserv_F_U$state, GCAM_sector = L126_in_EJ_state_distserv_F_U$GCAM_sector,
            GCAM_fuel = L126_in_EJ_state_distserv_F_U$fuel ), sum )
L126_in_EJ_state_distserv_F$efficiency <- L126_in_EJ_state_distserv_F$dist_out_EJ / L126_in_EJ_state_distserv_F$dist_in_EJ

#Compile just the efficiency as a table to be written out
printlog( "Calculating average efficiency of district service provision by state and fuel" )
L126_eff_state_distserv_F <- L126_in_EJ_state_distserv_F[ c( state_S_F, "efficiency" ) ] 

#Aggregate fuel inputs, keeping res and comm separate
L126_in_EJ_state_S_distserv_F <- aggregate( L126_in_EJ_state_distserv_F_U[ "dist_in_EJ" ],
      list( state = L126_in_EJ_state_distserv_F_U$state, GCAM_consumer = L126_in_EJ_state_distserv_F_U$GCAM_consumer,
            GCAM_sector = L126_in_EJ_state_distserv_F_U$GCAM_sector, GCAM_fuel = L126_in_EJ_state_distserv_F_U$fuel ), sum )

#Match in BEND district heating technology names to this dataframe
L126_in_EJ_state_S_distserv_F$BEND_consumer <- A_BEND_technology$BEND_sector[
      match( L126_in_EJ_state_S_distserv_F$GCAM_consumer, A_BEND_technology$GCAM_sector ) ]
L126_in_EJ_state_S_distserv_F$BEND_sector <- A_BEND_technology$BEND_sector[
      match( L126_in_EJ_state_S_distserv_F$GCAM_sector, A_BEND_technology$GCAM_sector ) ]
L126_in_EJ_state_S_distserv_F$BEND_service <- A_BEND_technology$BEND_service[
      match( L126_in_EJ_state_S_distserv_F$GCAM_sector, A_BEND_technology$GCAM_sector ) ]
L126_in_EJ_state_S_distserv_F$BEND_fuel <- A_BEND_technology$BEND_fuel[
      match( paste( L126_in_EJ_state_S_distserv_F$GCAM_sector, L126_in_EJ_state_S_distserv_F$GCAM_fuel ),
             paste( A_BEND_technology$GCAM_sector, A_BEND_technology$GCAM_fuel ) ) ]
L126_in_EJ_state_S_distserv_F$BEND_technology <- A_BEND_technology$BEND_technology[
      match( paste( L126_in_EJ_state_S_distserv_F$GCAM_sector, L126_in_EJ_state_S_distserv_F$GCAM_fuel ),
             paste( A_BEND_technology$GCAM_sector, A_BEND_technology$GCAM_fuel ) ) ]

#Build a new dataframe with BEND names for the district heat supply sector
L126_in_EJ_distservsupply_BEND <- L126_in_EJ_state_S_distserv_F[ c( "state", "BEND_consumer", BEND_techID ) ]
L126_in_EJ_distservsupply_BEND$in_EJ <- L126_in_EJ_state_S_distserv_F$dist_in_EJ

#NON-BUILDING ELECTRICITY USE
printlog( "Adding in non-building electricity use that is not included in the BEND dataset" )
L126_in_EJ_nonbld_BEND <- data.frame(
      state = L122_in_EJ_state_commext_elec$state,
      BEND_consumer = A_BEND_technology$BEND_sector[ match( L122_in_EJ_state_commext_elec$service, A_BEND_technology$service ) ],
      BEND_sector = A_BEND_technology$BEND_sector[ match( L122_in_EJ_state_commext_elec$service, A_BEND_technology$service ) ],
      BEND_service = A_BEND_technology$BEND_service[ match( L122_in_EJ_state_commext_elec$service, A_BEND_technology$service ) ],
      BEND_fuel = A_BEND_technology$BEND_fuel[ match( L122_in_EJ_state_commext_elec$service, A_BEND_technology$service ) ],
      BEND_technology = A_BEND_technology$BEND_fuel[ match( L122_in_EJ_state_commext_elec$service, A_BEND_technology$service ) ],
      in_EJ = L122_in_EJ_state_commext_elec$X2005 )

#DIRECT ENERGY CONSUMPTION INPUTS BY STATE AND SECTOR
L126_in_EJ_direct_BEND <- L126_BEND_energy.melt[
      paste( L126_BEND_energy.melt$BEND_sector, L126_BEND_energy.melt$BEND_service, L126_BEND_energy.melt$BEND_fuel, L126_BEND_energy.melt$BEND_technology ) %!in%
      paste( A_BEND_distserv$BEND_sector, A_BEND_distserv$BEND_service, A_BEND_distserv$BEND_fuel, A_BEND_distserv$BEND_technology ),
      c( "state", "BEND_consumer", BEND_techID, "in_EJ" ) ]

#DIRECT ENERGY CONSUMPTION OF FUELS NOT INCLUDED IN BEND DATA (BIOMASS, COAL)
printlog( "Adding in buildings sector use of fuels that are not included in the BEND dataset (biomass, coal)" )
L126_nonBEND_technology <- A_BEND_technology[ A_BEND_technology$BEND_fuel %!in% L126_BEND_energy.melt$BEND_fuel,
      c( BEND_techID, "GCAM_sector", "GCAM_fuel" ) ]
L126_nonBEND_technology$BEND_consumer <- L126_nonBEND_technology$BEND_sector
L126_in_EJ_direct_nonBEND <- L126_nonBEND_technology[ rep( 1:nrow( L126_nonBEND_technology ), times = length( states ) ), ]
L126_in_EJ_direct_nonBEND$state <- sort( rep( states, length.out = nrow( L126_in_EJ_direct_nonBEND ) ) )

#Paste in energy consumption from the SEDS-based table
L126_in_EJ_direct_nonBEND$in_EJ <- L104_in_EJ_state_bld_F$X2005[
      match( paste( L126_in_EJ_direct_nonBEND$state, L126_in_EJ_direct_nonBEND$GCAM_sector, L126_in_EJ_direct_nonBEND$GCAM_fuel ),
             paste( L104_in_EJ_state_bld_F$state, L104_in_EJ_state_bld_F$GCAM_sector, L104_in_EJ_state_bld_F$GCAM_fuel ) ) ]

#Re-order columns
L126_in_EJ_direct_nonBEND <- L126_in_EJ_direct_nonBEND[ c( "state", "BEND_consumer", BEND_techID, "in_EJ" ) ]

#Combine (rbind) dataframes to calculate the end-use allocations to services, by state, consumer (res/comm), and fuel
L126_in_EJ_BEND <- rbind( L126_in_EJ_direct_BEND, L126_in_EJ_distservsupply_BEND, L126_in_EJ_nonbld_BEND, L126_in_EJ_direct_nonBEND )
L126_in_EJ_BEND_state_S_F <- aggregate( L126_in_EJ_BEND[ "in_EJ" ],
      list( state = L126_in_EJ_BEND$state, BEND_consumer = L126_in_EJ_BEND$BEND_consumer,
            BEND_fuel = L126_in_EJ_BEND$BEND_fuel ), sum )

#Match these totals back to the full table to calculate end-use proportions
printlog( "Calculating portional allocations of BEND energy to services, by state, sector (res/comm), and fuel" )
L126_in_EJ_BEND$in_EJ_state_S_F <- L126_in_EJ_BEND_state_S_F$in_EJ[
      match( paste( L126_in_EJ_BEND$state, L126_in_EJ_BEND$BEND_consumer, L126_in_EJ_BEND$BEND_fuel ),
             paste( L126_in_EJ_BEND_state_S_F$state, L126_in_EJ_BEND_state_S_F$BEND_consumer, L126_in_EJ_BEND_state_S_F$BEND_fuel ) ) ]
L126_in_EJ_BEND$in_pct <- L126_in_EJ_BEND$in_EJ / L126_in_EJ_BEND$in_EJ_state_S_F

#Reset non-BEND fuel divide by zeroes to 1
L126_in_EJ_BEND$in_pct[ is.na( L126_in_EJ_BEND$in_pct ) & L126_in_EJ_BEND$BEND_fuel %in% L126_nonBEND_technology$BEND_fuel ] <- 1

#For any other fuels, divide by zeroes are a problem as there will be no way to disaggregate the energy among services.
if( any( is.na( L126_in_EJ_BEND ) ) )
   { printlog( "BEND database has no energy data for the following states and fuels" )
     print( L126_in_EJ_BEND[ is.na( L126_in_EJ_BEND$in_pct ), ] )
     stop () }

#Use these end-use proportions to calculate energy consumption by state, BEND sector, BEND service, BEND fuel, and BEND technology
#First, generate consistent mapping between the SEDS-based data and the BEND table
printlog( "Multiplying service portional allocations by SEDS-based energy consumption by state, sector, and fuel" )
L126_in_EJ_BEND$GCAM_sector <- A_BEND_technology$GCAM_sector[
      match( L126_in_EJ_BEND$BEND_consumer, A_BEND_technology$BEND_sector ) ]
L126_in_EJ_BEND$GCAM_fuel <- A_BEND_technology$GCAM_fuel[
      match( L126_in_EJ_BEND$BEND_fuel, A_BEND_technology$BEND_fuel ) ]

#Match in the appropriate energy consumption quantities in the GCAM base years, and multiply by the derived percentage
L126_in_EJ_BEND[ X_GCAM_base_years ] <- L104_in_EJ_state_bld_F [
      match( paste( L126_in_EJ_BEND$state, L126_in_EJ_BEND$GCAM_sector, L126_in_EJ_BEND$GCAM_fuel ),
             paste( L104_in_EJ_state_bld_F$state, L104_in_EJ_state_bld_F$GCAM_sector, L104_in_EJ_state_bld_F$GCAM_fuel ) ),
      X_GCAM_base_years ] * L126_in_EJ_BEND$in_pct

#Subset only the relevant columns; this is BEND-based energy consumption of fuels (not incl dist service consumption)
L126_in_EJ_BEND_RGCAM_fuel <- L126_in_EJ_BEND[ c( "state", "BEND_consumer", BEND_techID, X_GCAM_base_years ) ]

#The above table includes all energy consumption typically classified as "buildings sector"
#However, it doesn't track the transformation from energy to district services, and subsequent consumption by sectors and services

#DISTRICT SERVICE TRANSFORMATION AND CONSUMPTION
printlog( "Calculating the (scaled) output of district services, using efficiencies calculated above" )
#First, compile state- and fuel-specific efficiencies by BEND technology names
L126_eff_BEND_RGCAM_distserv <- data.frame(
      state = L126_in_EJ_state_distserv_F$state,
      BEND_sector = A_BEND_technology$BEND_sector[ match( L126_in_EJ_state_distserv_F$GCAM_sector, A_BEND_technology$GCAM_sector ) ],
      BEND_service = A_BEND_technology$BEND_service[ match( L126_in_EJ_state_distserv_F$GCAM_sector, A_BEND_technology$GCAM_sector ) ],
      BEND_fuel = A_BEND_technology$BEND_fuel[
            match( paste( L126_in_EJ_state_distserv_F$GCAM_sector, L126_in_EJ_state_distserv_F$GCAM_fuel ),
                   paste( A_BEND_technology$GCAM_sector, A_BEND_technology$GCAM_fuel ) ) ],
      BEND_technology = A_BEND_technology$BEND_technology[
            match( paste( L126_in_EJ_state_distserv_F$GCAM_sector, L126_in_EJ_state_distserv_F$GCAM_fuel ),
                   paste( A_BEND_technology$GCAM_sector, A_BEND_technology$GCAM_fuel ) ) ],
      efficiency = L126_in_EJ_state_distserv_F$efficiency )

#Subset fuel inputs to district services from the table of buildings sector fuel consumption
L126_in_EJ_distserv_state_S_F <- L126_in_EJ_BEND_RGCAM_fuel[ L126_in_EJ_BEND_RGCAM_fuel$BEND_sector %in% L126_eff_BEND_RGCAM_distserv$BEND_sector, ]

#Aggregate sectors (resid/comm)
L126_in_EJ_distserv_state_F <- aggregate( L126_in_EJ_distserv_state_S_F[ X_GCAM_base_years ],
      list( state = L126_in_EJ_distserv_state_S_F$state, BEND_sector = L126_in_EJ_distserv_state_S_F$BEND_sector,
            BEND_service = L126_in_EJ_distserv_state_S_F$BEND_service, BEND_fuel = L126_in_EJ_distserv_state_S_F$BEND_fuel,
            BEND_technology = L126_in_EJ_distserv_state_S_F$BEND_technology ), sum )

#Match in the efficiencies and calculate the output of district services by state and fuel
L126_in_EJ_distserv_state_F$efficiency <- L126_eff_BEND_RGCAM_distserv$efficiency[
      match( paste( L126_in_EJ_distserv_state_F$state, L126_in_EJ_distserv_state_F$BEND_sector, L126_in_EJ_distserv_state_F$BEND_fuel ),
             paste( L126_eff_BEND_RGCAM_distserv$state, L126_eff_BEND_RGCAM_distserv$BEND_sector, L126_eff_BEND_RGCAM_distserv$BEND_fuel ) ) ]

L126_out_EJ_distserv_state_F <- data.frame(
      L126_in_EJ_distserv_state_F[ c( "state", BEND_techID ) ],
      L126_in_EJ_distserv_state_F[ X_GCAM_base_years ] * L126_in_EJ_distserv_state_F$efficiency )

#Aggregate fuels to get the whole-sector output to be partitioned among the consumers and services
L126_out_EJ_distserv_state <- aggregate( L126_out_EJ_distserv_state_F[ X_GCAM_base_years ],
      list( state = L126_out_EJ_distserv_state_F$state, BEND_sector = L126_out_EJ_distserv_state_F$BEND_sector,
            BEND_service = L126_out_EJ_distserv_state_F$BEND_service ), sum )

#Calculate portional allocation of dist service consumption by sector and service
printlog( "Calculating portional allocations of district service demand by sector and service, within each state" )
L126_in_EJ_distserv_state <- aggregate( L126_in_EJ_distserv_BEND[ "in_EJ" ],
      list( state = L126_in_EJ_distserv_BEND$state ), sum )
L126_in_EJ_distserv_BEND$in_EJ_state <- L126_in_EJ_distserv_state$in_EJ[
      match( L126_in_EJ_distserv_BEND$state, L126_in_EJ_distserv_state$state ) ]
L126_in_EJ_distserv_BEND$in_pct <- L126_in_EJ_distserv_BEND$in_EJ / L126_in_EJ_distserv_BEND$in_EJ_state

#Match in the total service output by state, and calculate the amount of district service consumed by each state, sector, and end use
printlog( "Partitioning state-wise district service output to end uses and sectors according to these portional allocations" )
L126_in_EJ_distserv_BEND[ X_GCAM_base_years ] <- L126_out_EJ_distserv_state[
      match( L126_in_EJ_distserv_BEND$state, L126_out_EJ_distserv_state$state ), X_GCAM_base_years ]
L126_in_EJ_BEND_RGCAM_distserv <- data.frame(
      L126_in_EJ_distserv_BEND[ c( "state", "BEND_consumer", BEND_techID ) ],
      L126_in_EJ_distserv_BEND[ X_GCAM_base_years ] * L126_in_EJ_distserv_BEND$in_pct )

#Combine (rbind) district service consumption with table of energy consumption by all buildings.
#This still has the temporary ID vector of "BEND_consumer"
L126_in_EJ_BEND_RGCAM_cons <- rbind( L126_in_EJ_BEND_RGCAM_fuel, L126_in_EJ_BEND_RGCAM_distserv )

#Aggregate BEND_consumer for final table to be written out
L126_in_EJ_BEND_RGCAM <- aggregate( L126_in_EJ_BEND_RGCAM_cons[ X_GCAM_base_years ],
      list( state = L126_in_EJ_BEND_RGCAM_cons$state, BEND_sector = L126_in_EJ_BEND_RGCAM_cons$BEND_sector,
            BEND_service = L126_in_EJ_BEND_RGCAM_cons$BEND_service, BEND_fuel = L126_in_EJ_BEND_RGCAM_cons$BEND_fuel,
            BEND_technology = L126_in_EJ_BEND_RGCAM_cons$BEND_technology ), sum )
L126_in_EJ_BEND_RGCAM <- L126_in_EJ_BEND_RGCAM[ order( L126_in_EJ_BEND_RGCAM$state, L126_in_EJ_BEND_RGCAM$BEND_sector,
      L126_in_EJ_BEND_RGCAM$BEND_service, L126_in_EJ_BEND_RGCAM$BEND_fuel ), ]

# 2B: EFFICIENCY
printlog( "Processing data on energy efficiency from BEND" )
#Melt energy table from BEND and match in state names
L126_BEND_efficiency.melt <- melt( BEND_efficiency, id.vars = BEND_techID )

#Repeated state names are indicated with a number that needs to be removed
#Making a function to remove all of the numerical values appended to state names
tmp_from <- c('.10','.11','.1','.2','.3','.4','.5','.6','.7','.8','.9')
tmp_to <- c('','','','','','','','','','','')
gsub2 <- function(pattern, replacement, x, ...) {
      for(i in 1:length(pattern))
      x <- gsub(pattern[i], replacement[i], x, ...)
      x
}

L126_BEND_efficiency.melt$state_name <- gsub2( tmp_from, tmp_to, L126_BEND_efficiency.melt$variable )
L126_BEND_efficiency.melt$state_name <- gsub2( ".", " ", L126_BEND_efficiency.melt$state_name, fixed=T )

#Match in the state code
L126_BEND_efficiency.melt$state <- state_region_ind$state[
      match( substr( L126_BEND_efficiency.melt$state_name, 1, nchar( L126_BEND_efficiency.melt$state_name ) -2 ),
             substr( state_region_ind$state_name, 1, nchar( state_region_ind$state_name ) -2 ) ) ]

#Remove any missing values, and average the different climate zones within each state
L126_BEND_efficiency.melt <- L126_BEND_efficiency.melt[ !is.na( L126_BEND_efficiency.melt$value ), ]

L126_eff_state_S_F_BEND <- aggregate( L126_BEND_efficiency.melt[ "value" ],
      list( state = L126_BEND_efficiency.melt$state, BEND_sector = L126_BEND_efficiency.melt$BEND_sector,
            BEND_service = L126_BEND_efficiency.melt$BEND_service, BEND_fuel = L126_BEND_efficiency.melt$BEND_fuel,
            BEND_technology = L126_BEND_efficiency.melt$BEND_technology ),
      mean )
      
#4-9-2012: Most of the values in the BEND database do not have noticeable inter-state variation, 
# and of the 10 technologies available, 5 have efficiencies that are outside the range of the values in the GCAM technology database.
# Since most of these are for fuels that BEND doesn't focus on anyway, they are being dropped here.
# The exceptions are air conditioning and heat pumps (resid / comm), which are both within the range of the techs in GCAM, and use electricity.

printlog( "NOTE: Removing all non-electric technologies due to inconsistencies with the GCAM technology set" )
#tmp_min <- aggregate( L126_eff_state_S_F_BEND[ "value" ], list( L126_eff_state_S_F_BEND$BEND_sector, L126_eff_state_S_F_BEND$BEND_service,
#      L126_eff_state_S_F_BEND$BEND_fuel, L126_eff_state_S_F_BEND$BEND_technology ), min )
#tmp_max <- aggregate( L126_eff_state_S_F_BEND[ "value" ], list( L126_eff_state_S_F_BEND$BEND_sector, L126_eff_state_S_F_BEND$BEND_service,
#      L126_eff_state_S_F_BEND$BEND_fuel, L126_eff_state_S_F_BEND$BEND_technology ), max )

L126_eff_state_S_F_BEND <- L126_eff_state_S_F_BEND[ L126_eff_state_S_F_BEND$BEND_fuel == "Electricity", ]
names( L126_eff_state_S_F_BEND )[ names ( L126_eff_state_S_F_BEND ) == "value" ] <- "efficiency"

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L126_eff_state_distserv_F <- c( "Average efficiency of district service production by state and fuel","Unitless" )
comments.L126_in_EJ_BEND_RGCAM <- c( "BEND-based building energy consumption by state / sector / fuel / service","Unit = EJ" )
comments.L126_eff_state_S_F_BEND <- c( "BEND-based efficiency for selected technologies by state / sector / fuel / service","Unitless" )

#write tables as CSV files
writedata( L126_eff_state_distserv_F,fn="L126_eff_state_distserv_F", comments=comments.L126_eff_state_distserv_F )
writedata( L126_in_EJ_BEND_RGCAM,fn="L126_in_EJ_BEND_RGCAM", comments=comments.L126_in_EJ_BEND_RGCAM )
writedata( L126_eff_state_S_F_BEND,fn="L126_eff_state_S_F_BEND", comments=comments.L126_eff_state_S_F_BEND )

# Every script should finish with this line
logstop()