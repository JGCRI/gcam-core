# L202_RGCAM_BEND_bld_input.R
# MODEL INPUT FILES BASED ON RESIDENTIAL AND COMMERCIAL ENERGY CONSUMPTION AND EFFICIENCY FROM BEND
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L202_RGCAM_BEND_bld_input.R" )
printlog( "Buildings input file from BEND" )
                  
# -----------------------------------------------------------------------------
# 1. Read files

A_BEND_Delete <- readdata( "A_BEND_Delete" )
A_BEND_demand <- readdata( "A_BEND_demand" )
A_BEND_service <- readdata( "A_BEND_service" )
A_BEND_shell_conductance <- readdata( "A_BEND_shell_conductance" )
A_BEND_subs_logit <- readdata( "A_BEND_subs_logit" )
A_BEND_tech_logit <- readdata( "A_BEND_tech_logit" )
A_BEND_tech_cost_Y <- readdata( "A_BEND_tech_cost_Y" )
A_BEND_tech_eff_Y <- readdata( "A_BEND_tech_eff_Y" )
A_BEND_tech_interp <- readdata( "A_BEND_tech_interp" )
A_BEND_tech_intgains <- readdata( "A_BEND_tech_intgains" )
A_BEND_tech_retirement <- readdata( "A_BEND_tech_retirement" )
A_BEND_technology <- readdata( "A_BEND_technology" )
A_BEND_Tdiff_shares <- readdata( "A_BEND_Tdiff_shares" )
A_BEND_Tprteff <- readdata( "A_BEND_Tprteff" )
A_fuel_markets <- readdata( "A_fuel_markets" )
Census_pop_hist <- readdata( "Census_pop_hist" )
CDD_His_Fut <- readdata( "CDD_His_Fut" )
HDD_His_Fut <- readdata( "HDD_His_Fut" )
L111_flsp_bm2_state_res <- readdata( "L111_flsp_bm2_state_res" )
L121_flsp_bm2_state_comm <- readdata( "L121_flsp_bm2_state_comm" )
L126_eff_state_distserv_F <- readdata( "L126_eff_state_distserv_F" )
L126_eff_state_S_F_BEND <- readdata( "L126_eff_state_S_F_BEND" )
L126_in_EJ_BEND_RGCAM <- readdata( "L126_in_EJ_BEND_RGCAM" )

# -----------------------------------------------------------------------------
# 2. Build model input files
printlog( "L202_BldDelete: DELETE EXISTING US BUILDINGS DEMAND SECTOR" )
L202_BldDelete <- A_BEND_Delete 

printlog( "L202_PopShare: SUBREGIONAL POPULATION SHARE" )
L202_PopShare <- data.frame( gcam_consumer = A_BEND_demand$gcam_consumer )
L202_PopShare[ c( X_GCAM_model_period0, X_GCAM_years ) ] <- 1

printlog( "L202_IncShare: SUBREGIONAL INCOME SHARE" )
L202_IncShare <- data.frame( gcam_consumer = A_BEND_demand$gcam_consumer )
L202_IncShare[ c( X_GCAM_model_period0, X_GCAM_years ) ] <- 1

printlog( "L202_BldDemandFn: BUILDING SERVICE DEMAND FUNCTION TYPES" )
L202_BldDemandFn1 <- data.frame( gcam_consumer = A_BEND_demand$gcam_consumer,
      nodeInput = A_BEND_demand$nodeInput,
      prodDmdFnType = "building-function" )

L202_BldDemandFn2 <- data.frame( A_BEND_demand[ bld_node_names ],
      prodDmdFnType = "building-service-function" )

printlog( "L202_BldFlsp: BASE-YEAR FLOORSPACE AND SATIATION DEMAND LEVELS" )
#Combine residential and commercial tables
L202_flsp_bm2_state_bld <- rbind( L111_flsp_bm2_state_res, L121_flsp_bm2_state_comm )
L202_flsp_bm2_state_bld <- L202_flsp_bm2_state_bld[ order( L202_flsp_bm2_state_bld$GCAM_sector ), ]

#  Repeat demands by the number of states
A_BEND_demand_repstate <- A_BEND_demand[ rep( 1:nrow( A_BEND_demand ), times = length( states ) ), ]
A_BEND_demand_repstate <- A_BEND_demand_repstate[ order( A_BEND_demand_repstate$gcam_consumer ), ]
L202_BldFlsp <- data.frame( region = L202_flsp_bm2_state_bld$state,
      A_BEND_demand_repstate[ bld_node_names ],
      round( L202_flsp_bm2_state_bld[ X_GCAM_base_years ], flsp_digits ),
      price_exponent = P_elas_flsp,
      satiation_level = 0,
      satiation_adder = 0,
      internal_gains_market_name = A_BEND_demand_repstate$internal_gains_market_name,
      internal_gains_unit = "EJ" )
      
#  Need to read in the satiation level, in billion square meters per thousand persons. Equal to fixed multiplier times the base year floorspace
#  The satiation level can not be less than the per-capita floorspace in 1990, so make sure to set that as a minimum threshold level in each state/sector      
L202_flsp_bm2_state_bld$pop_1990 <- Census_pop_hist$X1990[ match( L202_flsp_bm2_state_bld$state, Census_pop_hist$state ) ]
L202_flsp_bm2_state_bld$pop_2005 <- Census_pop_hist$X2005[ match( L202_flsp_bm2_state_bld$state, Census_pop_hist$state ) ]

L202_flsp_bm2_state_bld$pcflsp_Mm2_1990 <- L202_flsp_bm2_state_bld$X1990 * 1e3 / L202_flsp_bm2_state_bld$pop_1990
L202_flsp_bm2_state_bld$pcflsp_Mm2_2005 <- L202_flsp_bm2_state_bld$X2005 * 1e3 / L202_flsp_bm2_state_bld$pop_2005

#  TODO: The fixed multiplier should be calculated as a function of per-capita income and floorspace income elasticity
L202_flsp_bm2_state_bld$satiation_level <- pmax( L202_flsp_bm2_state_bld$pcflsp_Mm2_2005 * satiation_flsp_mult,
                                                 L202_flsp_bm2_state_bld$pcflsp_Mm2_1990 * 1.01 )

L202_BldFlsp$satiation_level <- round( L202_flsp_bm2_state_bld$satiation_level[
      match( paste( L202_BldFlsp$region, L202_BldFlsp$gcam_consumer ),
             paste( L202_flsp_bm2_state_bld$state, L202_flsp_bm2_state_bld$GCAM_sector ) ) ],
      flsp_sat_digits )

printlog( "L202_BldServSatn and L202_BldThermalServSatn: BUILDING SERVICE SATIATION LEVELS" )
L202_BldServSatn <- data.frame( A_BEND_service[ 
      A_BEND_service$building_service_input %!in% thermal_services, 
      c( bld_node_names, "building_service_input", "satiation_base_year_increase", "satiation_adder" ) ] )

L202_BldThermalServSatn <- data.frame( A_BEND_service[ 
      A_BEND_service$building_service_input %in% thermal_services, 
      c( bld_node_names, "building_service_input", "internal_gains_scaler", "satiation_base_year_increase", "satiation_adder" ) ] )
names( L202_BldThermalServSatn )[ names( L202_BldThermalServSatn ) == "building_service_input" ] <- "thermal_building_service_input"

printlog( "L202_DegreeDays: HEATING DEGREE DAYS, COOLING DEGREE DAYS" )
#First, build tables with the service names
L202_heating_services <- A_BEND_service[ 
      A_BEND_service$building_service_input %in% heating_services, c( bld_node_names, "building_service_input" ) ]
L202_cooling_services <- A_BEND_service[ 
      A_BEND_service$building_service_input %in% cooling_services, c( bld_node_names, "building_service_input" ) ]

#Repeat by number of states, and sort by service
L202_heating_services_repstate <- L202_heating_services[ rep( 1:nrow( L202_heating_services ), times = length( states ) ), ]
L202_heating_services_repstate <- L202_heating_services_repstate[ order( L202_heating_services_repstate$gcam_consumer ), ]
L202_cooling_services_repstate <- L202_cooling_services[ rep( 1:nrow( L202_cooling_services ), times = length( states ) ), ]
L202_cooling_services_repstate <- L202_cooling_services_repstate[ order( L202_cooling_services_repstate$gcam_consumer ), ]

L202_HDD <- data.frame(
      region = rep( HDD_His_Fut$state, length.out = nrow( L202_heating_services_repstate ) ),
      L202_heating_services_repstate,
      HDD_His_Fut[ c( X_GCAM_model_period0, X_GCAM_years ) ] )
      
L202_CDD <- data.frame(
      region = rep( CDD_His_Fut$state, length.out = nrow( L202_cooling_services_repstate ) ),
      L202_cooling_services_repstate,
      CDD_His_Fut[ c( X_GCAM_model_period0, X_GCAM_years ) ] )

L202_DegreeDays <- rbind( L202_HDD, L202_CDD )
L202_DegreeDays[ c( X_GCAM_model_period0, X_GCAM_years ) ] <- 
      round( L202_DegreeDays[ c( X_GCAM_model_period0, X_GCAM_years ) ], HDDCDD_digits )

printlog( "L202_CalInput: BASE YEAR ENERGY CONSUMPTION BY TECHNOLOGY" )
L202_in_EJ_BEND_RGCAM <- L126_in_EJ_BEND_RGCAM

#Match in GCAM supplysector/subsector/input names to energy input table
L202_in_EJ_BEND_RGCAM[ sup_sub_input ] <- A_BEND_technology[
      match( paste( L202_in_EJ_BEND_RGCAM$BEND_sector, L202_in_EJ_BEND_RGCAM$BEND_service, L202_in_EJ_BEND_RGCAM$BEND_fuel ),
             paste( A_BEND_technology$BEND_sector, A_BEND_technology$BEND_service, A_BEND_technology$BEND_fuel ) ),
      sup_sub_input ]

#Subset "differentiated" technologies where similar service/fuels are partitioned into separate technologies, according to technology types (incandescent and fluorescent lighting)
L202_in_EJ_BEND_RGCAM_Tdiff <- L202_in_EJ_BEND_RGCAM[
      paste( L202_in_EJ_BEND_RGCAM$BEND_sector, L202_in_EJ_BEND_RGCAM$BEND_service,
             L202_in_EJ_BEND_RGCAM$BEND_fuel, L202_in_EJ_BEND_RGCAM$BEND_technology ) %in%
      paste( A_BEND_Tdiff_shares$BEND_sector, A_BEND_Tdiff_shares$BEND_service,
             A_BEND_Tdiff_shares$BEND_fuel, A_BEND_Tdiff_shares$BEND_technology ), ]

#Match in the technology names
L202_in_EJ_BEND_RGCAM_Tdiff$technology1 <- A_BEND_Tdiff_shares$technology1[
      match( paste( L202_in_EJ_BEND_RGCAM_Tdiff$supplysector, L202_in_EJ_BEND_RGCAM_Tdiff$subsector ),
             paste( A_BEND_Tdiff_shares$supplysector, A_BEND_Tdiff_shares$subsector ) ) ]
L202_in_EJ_BEND_RGCAM_Tdiff$technology2 <- A_BEND_Tdiff_shares$technology2[
      match( paste( L202_in_EJ_BEND_RGCAM_Tdiff$supplysector, L202_in_EJ_BEND_RGCAM_Tdiff$subsector ),
             paste( A_BEND_Tdiff_shares$supplysector, A_BEND_Tdiff_shares$subsector ) ) ]

#Subset "partitioned efficiency" BEND technologies (partitioned on the basis of efficiency)
L202_in_EJ_BEND_RGCAM_Tprteff <- L202_in_EJ_BEND_RGCAM[
      paste( L202_in_EJ_BEND_RGCAM$BEND_sector, L202_in_EJ_BEND_RGCAM$BEND_service,
             L202_in_EJ_BEND_RGCAM$BEND_fuel, L202_in_EJ_BEND_RGCAM$BEND_technology ) %in%
      paste( A_BEND_Tprteff$BEND_sector, A_BEND_Tprteff$BEND_service,
             A_BEND_Tprteff$BEND_fuel, A_BEND_Tprteff$BEND_technology ), ]

#Match in the technology names
L202_in_EJ_BEND_RGCAM_Tprteff$technology1 <- A_BEND_Tprteff$technology1[
      match( paste( L202_in_EJ_BEND_RGCAM_Tprteff$supplysector, L202_in_EJ_BEND_RGCAM_Tprteff$subsector ),
             paste( A_BEND_Tprteff$supplysector, A_BEND_Tprteff$subsector ) ) ]
L202_in_EJ_BEND_RGCAM_Tprteff$technology2 <- A_BEND_Tprteff$technology2[
      match( paste( L202_in_EJ_BEND_RGCAM_Tprteff$supplysector, L202_in_EJ_BEND_RGCAM_Tprteff$subsector ),
             paste( A_BEND_Tprteff$supplysector, A_BEND_Tprteff$subsector ) ) ]

#Subset the remaining technologies into a separate table
L202_in_EJ_BEND_RGCAM_Tgen <- L202_in_EJ_BEND_RGCAM[
      paste( L202_in_EJ_BEND_RGCAM$BEND_sector, L202_in_EJ_BEND_RGCAM$BEND_service,
             L202_in_EJ_BEND_RGCAM$BEND_fuel, L202_in_EJ_BEND_RGCAM$BEND_technology ) %!in%
      c( paste( L202_in_EJ_BEND_RGCAM_Tprteff$BEND_sector, L202_in_EJ_BEND_RGCAM_Tprteff$BEND_service,
                L202_in_EJ_BEND_RGCAM_Tprteff$BEND_fuel, L202_in_EJ_BEND_RGCAM_Tprteff$BEND_technology ),
         paste( L202_in_EJ_BEND_RGCAM_Tdiff$BEND_sector, L202_in_EJ_BEND_RGCAM_Tdiff$BEND_service,
                L202_in_EJ_BEND_RGCAM_Tdiff$BEND_fuel, L202_in_EJ_BEND_RGCAM_Tdiff$BEND_technology ) ), ]

#Match in the technology names, using the full BEND identifiers
L202_in_EJ_BEND_RGCAM_Tgen$technology <- A_BEND_technology$technology[
      match( paste( L202_in_EJ_BEND_RGCAM_Tgen$BEND_sector, L202_in_EJ_BEND_RGCAM_Tgen$BEND_service,
                    L202_in_EJ_BEND_RGCAM_Tgen$BEND_fuel, L202_in_EJ_BEND_RGCAM_Tgen$BEND_technology ),
             paste( A_BEND_technology$BEND_sector, A_BEND_technology$BEND_service,
                    A_BEND_technology$BEND_fuel, A_BEND_technology$BEND_technology ) ) ]

#Map in differentiated (exogenous) technology shares
L202_in_EJ_BEND_RGCAM_Tdiff$share_tech1 <- A_BEND_Tdiff_shares$share_tech1[
      match( paste( L202_in_EJ_BEND_RGCAM_Tdiff$supplysector, L202_in_EJ_BEND_RGCAM_Tdiff$subsector, L202_in_EJ_BEND_RGCAM_Tdiff$technology1 ),
             paste( A_BEND_Tdiff_shares$supplysector, A_BEND_Tdiff_shares$subsector, A_BEND_Tdiff_shares$technology1 ) ) ]
L202_in_EJ_BEND_RGCAM_Tdiff$share_tech2 <- 1 - L202_in_EJ_BEND_RGCAM_Tdiff$share_tech1

#Partitioned efficiency technology shares are derived according to stock average efficiency compared with the efficiency of tech1 and tech2
A_BEND_Tprteff$share_tech1 <- with( A_BEND_Tprteff, ( stockavg - eff_tech2 ) / ( eff_tech1 - eff_tech2 ) )
L202_in_EJ_BEND_RGCAM_Tprteff$share_tech1 <- A_BEND_Tprteff$share_tech1[
      match( paste( L202_in_EJ_BEND_RGCAM_Tprteff$supplysector, L202_in_EJ_BEND_RGCAM_Tprteff$subsector ),
             paste( A_BEND_Tprteff$supplysector, A_BEND_Tprteff$subsector ) ) ]
L202_in_EJ_BEND_RGCAM_Tprteff$share_tech2 <- 1 - L202_in_EJ_BEND_RGCAM_Tprteff$share_tech1

# Where efficiencies are specified by BEND, over-write the default values.
# Match GCAM supplysector/subsector/input names into the BEND table of efficiencies
L202_eff_state_S_F_BEND <- L126_eff_state_S_F_BEND
L202_eff_state_S_F_BEND[ sup_sub_input ] <- A_BEND_technology[
      match( paste( L202_eff_state_S_F_BEND$BEND_sector, L202_eff_state_S_F_BEND$BEND_service, L202_eff_state_S_F_BEND$BEND_fuel ),
             paste( A_BEND_technology$BEND_sector, A_BEND_technology$BEND_service, A_BEND_technology$BEND_fuel ) ),
      sup_sub_input ]

#Match in the tech1 and tech2 efficiencies, and calculate the shares to return the observed BEND efficiencies
L202_eff_state_S_F_BEND$eff_tech1 <- A_BEND_Tprteff$eff_tech1[
      match( paste( L202_eff_state_S_F_BEND$supplysector, L202_eff_state_S_F_BEND$subsector ),
             paste( A_BEND_Tprteff$supplysector, A_BEND_Tprteff$subsector ) ) ]
L202_eff_state_S_F_BEND$eff_tech2 <- A_BEND_Tprteff$eff_tech2[
      match( paste( L202_eff_state_S_F_BEND$supplysector, L202_eff_state_S_F_BEND$subsector ),
             paste( A_BEND_Tprteff$supplysector, A_BEND_Tprteff$subsector ) ) ]
L202_eff_state_S_F_BEND$share_tech1 <- with( L202_eff_state_S_F_BEND, ( efficiency - eff_tech2 ) / ( eff_tech1 - eff_tech2 ) )

#Generate ID vectors and match these shares back into the full table
L202_in_EJ_BEND_RGCAM_Tprteff$ID <- paste( L202_in_EJ_BEND_RGCAM_Tprteff$state, L202_in_EJ_BEND_RGCAM_Tprteff$supplysector, L202_in_EJ_BEND_RGCAM_Tprteff$subsector )
L202_eff_state_S_F_BEND$ID <- paste( L202_eff_state_S_F_BEND$state, L202_eff_state_S_F_BEND$supplysector, L202_eff_state_S_F_BEND$subsector )
L202_in_EJ_BEND_RGCAM_Tprteff$share_tech1[ L202_in_EJ_BEND_RGCAM_Tprteff$ID %in% L202_eff_state_S_F_BEND$ID ] <- L202_eff_state_S_F_BEND$share_tech1[
      match( L202_in_EJ_BEND_RGCAM_Tprteff$ID[ L202_in_EJ_BEND_RGCAM_Tprteff$ID %in% L202_eff_state_S_F_BEND$ID ],
             L202_eff_state_S_F_BEND$ID ) ]
L202_in_EJ_BEND_RGCAM_Tprteff$share_tech2 <- 1 - L202_in_EJ_BEND_RGCAM_Tprteff$share_tech1             

#These shares may need to be further modified for technologies that are both differentiated AND partitioned (e.g. electric heat pumps)
if( any( paste( L202_in_EJ_BEND_RGCAM_Tprteff$supplysector, L202_in_EJ_BEND_RGCAM_Tprteff$subsector, L202_in_EJ_BEND_RGCAM_Tprteff$technology1 ) %in%
             paste( L202_in_EJ_BEND_RGCAM_Tdiff$supplysector, L202_in_EJ_BEND_RGCAM_Tdiff$subsector, L202_in_EJ_BEND_RGCAM_Tdiff$technology1 ) ) )
   { L202_in_EJ_BEND_RGCAM_Tprteff$share_adj <- L202_in_EJ_BEND_RGCAM_Tdiff$share_tech1[
      match( paste( L202_in_EJ_BEND_RGCAM_Tprteff$supplysector, L202_in_EJ_BEND_RGCAM_Tprteff$subsector, L202_in_EJ_BEND_RGCAM_Tprteff$technology1 ) %in%
             paste( L202_in_EJ_BEND_RGCAM_Tdiff$supplysector, L202_in_EJ_BEND_RGCAM_Tdiff$subsector, L202_in_EJ_BEND_RGCAM_Tdiff$technology1 ) ) ]
   } else { L202_in_EJ_BEND_RGCAM_Tprteff$share_adj <- 1 }

#Build calibrated input table for generic technologies
L202_CalInput_Bld_GenTech <- data.frame(
      region = L202_in_EJ_BEND_RGCAM_Tgen$state,
      supplysector = L202_in_EJ_BEND_RGCAM_Tgen$supplysector,
      subsector = L202_in_EJ_BEND_RGCAM_Tgen$subsector,
      stub_technology = L202_in_EJ_BEND_RGCAM_Tgen$technology,
      minicam_energy_input = L202_in_EJ_BEND_RGCAM_Tgen$minicam_energy_input,
      L202_in_EJ_BEND_RGCAM_Tgen[ X_GCAM_base_years ] )

#Build calibrated input tables for differentiated technologies
L202_CalInput_Bld_DiffTech1 <- data.frame(
      region = L202_in_EJ_BEND_RGCAM_Tdiff$state,
      supplysector = L202_in_EJ_BEND_RGCAM_Tdiff$supplysector,
      subsector = L202_in_EJ_BEND_RGCAM_Tdiff$subsector,
      stub_technology = L202_in_EJ_BEND_RGCAM_Tdiff$technology1,
      minicam_energy_input = L202_in_EJ_BEND_RGCAM_Tdiff$minicam_energy_input,
      L202_in_EJ_BEND_RGCAM_Tdiff[ X_GCAM_base_years ] * L202_in_EJ_BEND_RGCAM_Tdiff$share_tech1 )

L202_CalInput_Bld_DiffTech2 <- data.frame(
      region = L202_in_EJ_BEND_RGCAM_Tdiff$state,
      supplysector = L202_in_EJ_BEND_RGCAM_Tdiff$supplysector,
      subsector = L202_in_EJ_BEND_RGCAM_Tdiff$subsector,
      stub_technology = L202_in_EJ_BEND_RGCAM_Tdiff$technology2,
      minicam_energy_input = L202_in_EJ_BEND_RGCAM_Tdiff$minicam_energy_input,
      L202_in_EJ_BEND_RGCAM_Tdiff[ X_GCAM_base_years ] * L202_in_EJ_BEND_RGCAM_Tdiff$share_tech2 )

#Build calibrated input tables for efficiency-partitioned technologies
L202_CalInput_Bld_PrtTech1 <- data.frame(
      region = L202_in_EJ_BEND_RGCAM_Tprteff$state,
      supplysector = L202_in_EJ_BEND_RGCAM_Tprteff$supplysector,
      subsector = L202_in_EJ_BEND_RGCAM_Tprteff$subsector,
      stub_technology = L202_in_EJ_BEND_RGCAM_Tprteff$technology1,
      minicam_energy_input = L202_in_EJ_BEND_RGCAM_Tprteff$minicam_energy_input,
      L202_in_EJ_BEND_RGCAM_Tprteff[ X_GCAM_base_years ] * L202_in_EJ_BEND_RGCAM_Tprteff$share_tech1 * L202_in_EJ_BEND_RGCAM_Tprteff$share_adj )

L202_CalInput_Bld_PrtTech2 <- data.frame(
      region = L202_in_EJ_BEND_RGCAM_Tprteff$state,
      supplysector = L202_in_EJ_BEND_RGCAM_Tprteff$supplysector,
      subsector = L202_in_EJ_BEND_RGCAM_Tprteff$subsector,
      stub_technology = L202_in_EJ_BEND_RGCAM_Tprteff$technology2,
      minicam_energy_input = L202_in_EJ_BEND_RGCAM_Tprteff$minicam_energy_input,
      L202_in_EJ_BEND_RGCAM_Tprteff[ X_GCAM_base_years ] * L202_in_EJ_BEND_RGCAM_Tprteff$share_tech2 * L202_in_EJ_BEND_RGCAM_Tprteff$share_adj )

#Combine these into a single table, round to specified number of digits, add shareweight columns, and sort.
L202_CalInput_Bld <- rbind( L202_CalInput_Bld_GenTech,
      L202_CalInput_Bld_PrtTech1, L202_CalInput_Bld_PrtTech2,
      L202_CalInput_Bld_DiffTech1, L202_CalInput_Bld_DiffTech2 ) 
L202_CalInput_Bld[ X_GCAM_base_years ] <- round( L202_CalInput_Bld[ X_GCAM_base_years ], CalInput_digits )
L202_CalInput_Bld[ GCAM_base_shareweights ] <- as.numeric( L202_CalInput_Bld[ X_GCAM_base_years ] > 0 )
L202_CalInput_Bld <- L202_CalInput_Bld[ order( L202_CalInput_Bld$region, as.character( L202_CalInput_Bld$supplysector ),
                                               as.character( L202_CalInput_Bld$subsector ) ), ]

printlog( "L202_BldServOut and L202_BldThermalServOut: BASE SERVICE BY STATE AND BUILDING SERVICE" )
#Computation of output by service - map in the efficiencies
L202_eff_state_bld_tech <- L202_CalInput_Bld[ R_sup_sub_stubtech_input ]
L202_eff_state_bld_tech[ X_GCAM_base_years ] <- A_BEND_tech_eff_Y[
      match( paste( L202_eff_state_bld_tech$supplysector, L202_eff_state_bld_tech$stub_technology ),
             paste( A_BEND_tech_eff_Y$supplysector, A_BEND_tech_eff_Y$technology ) ),
      X_GCAM_base_years ]
      
L202_out_EJ_state_bld_tech <- L202_CalInput_Bld[ R_sup_sub_stubtech_input ]
L202_out_EJ_state_bld_tech[ X_GCAM_base_years ] <- L202_CalInput_Bld[ X_GCAM_base_years ] * L202_eff_state_bld_tech[ X_GCAM_base_years ]

#Aggregate by service
L202_out_EJ_state_bld_serv <- aggregate( L202_out_EJ_state_bld_tech[ X_GCAM_base_years ],
          list( region = L202_out_EJ_state_bld_tech$region, building_service_input = L202_out_EJ_state_bld_tech$supplysector ), sum )

#Match in the building node names, and round to a specified number of decimal places
L202_out_EJ_state_bld_serv[ bld_node_names ] <- A_BEND_service[
      match( L202_out_EJ_state_bld_serv$building_service_input, A_BEND_service$building_service_input ),
      bld_node_names ] 

#Remove rows that do not map to a final consumer -- this removes the outputs from the district service production sector
L202_out_EJ_state_bld_serv <- na.omit( L202_out_EJ_state_bld_serv )
L202_out_EJ_state_bld_serv[ X_GCAM_base_years ] <- round( L202_out_EJ_state_bld_serv[ X_GCAM_base_years ], CalInput_digits )

#Subset to build the tables of service output by state and service (non-thermal services and thermal services)
L202_BldServOut <- L202_out_EJ_state_bld_serv[ L202_out_EJ_state_bld_serv$building_service_input %!in% thermal_services,
      c( "region", bld_node_names, "building_service_input", X_GCAM_base_years ) ]

L202_BldThermalServOut <- L202_out_EJ_state_bld_serv[ L202_out_EJ_state_bld_serv$building_service_input %in% thermal_services,
      c( "region", bld_node_names, "building_service_input", X_GCAM_base_years ) ]

printlog( "L202_BldShell: SHELL CONDUCTANCE AND FLOORSPACE TO SURFACE AREA RATIO" )
A_BEND_shell_conductance.melt <- melt( A_BEND_shell_conductance, id.vars = "gcam_consumer" )
A_BEND_shell_conductance.melt$year <- as.numeric( substr( A_BEND_shell_conductance.melt$variable, 2, 5 ) )

L202_BldShell <- A_BEND_demand[ rep( 1:nrow( A_BEND_demand ), times = length( c( GCAM_model_period0, GCAM_years ) ) ), bld_node_names ]
L202_BldShell <- L202_BldShell[ order( L202_BldShell$gcam_consumer ), ]
L202_BldShell$shell_conductance_year <- rep( c( GCAM_model_period0, GCAM_years ), length.out = nrow( L202_BldShell ) )
L202_BldShell$shell_conductance <- A_BEND_shell_conductance.melt$value[
      match( paste( L202_BldShell$gcam_consumer, L202_BldShell$shell_conductance_year ),
             paste( A_BEND_shell_conductance.melt$gcam_consumer, A_BEND_shell_conductance.melt$year ) ) ]
L202_BldShell$floor_to_surface_year <- L202_BldShell$shell_conductance_year
L202_BldShell$floor_to_surface_ratio <- floor_to_surface

printlog( "L202_Supplysector: SUPPLYSECTOR UNITS AND LOGIT EXPONENT" )
L202_Supplysector <- data.frame(
      supplysector = A_BEND_subs_logit$supplysector,
      output_unit = "EJ",
      input_unit = "EJ",
      price_unit = "1975$/GJ",
      price = 1,
      final_energy_keyword = "building",
      logit_exponent = A_BEND_subs_logit$subs_logit )
      
printlog( "L202_Subsector: SUBSECTOR INTERPOLATION AND LOGIT EXPONENT" )
L202_Subsector <- data.frame(
      supplysector = A_BEND_tech_logit$supplysector,
      subsector = A_BEND_tech_logit$subsector,
      share_weight = 1,
      apply_to = "share-weight",
      from_year = final_cal_year,
      to_year = 9999,
      interpolation_function = "fixed",
      logit_exponent = A_BEND_tech_logit$tech_logit, stringsAsFactors = FALSE )     

#Reset coal shareweights to a linear phase-out
L202_Subsector$to_year[ L202_Subsector$subsector == "coal" ] <- coal_phaseout_year
L202_Subsector$interpolation_function[ L202_Subsector$subsector == "coal" ] <- "linear"

printlog( "L202_SubsectorPhaseout: SUBSECTOR PHASEOUT (FOR COAL)" )
L202_SubsectorPhaseout <- data.frame(
      supplysector = A_BEND_technology$supplysector[ A_BEND_technology$subsector == "coal" ],
      subsector = "coal",
      share_weight_year = coal_phaseout_year,
      share_weight = 0 )

printlog( "L202_GlobalTech: GLOBAL TECHNOLOGY INTERPOLATION" )
#Build table of technologies included in BEND
L202_GlobalTech <- data.frame(
      sector_name = A_BEND_technology$supplysector,
      subsector_name = A_BEND_technology$subsector,
      technology = A_BEND_technology$technology,
      apply_to = "share-weight",
      from_year = final_cal_year,
      to_year = 9999,
      interpolation_function = "fixed", stringsAsFactors = FALSE )
      
#For specified technologies, use an s-curve function to a specified year
#First, generate a temporary ID vector
L202_GlobalTech$ID <- paste( L202_GlobalTech$sector_name, L202_GlobalTech$subsector_name, L202_GlobalTech$technology )
A_BEND_tech_interp$ID <- paste( A_BEND_tech_interp$supplysector, A_BEND_tech_interp$subsector, A_BEND_tech_interp$technology )
L202_GlobalTech$from_year[ L202_GlobalTech$ID %in% A_BEND_tech_interp$ID ] <- A_BEND_tech_interp$from_year[
      match( L202_GlobalTech$ID[ L202_GlobalTech$ID %in% A_BEND_tech_interp$ID ], A_BEND_tech_interp$ID ) ] 
L202_GlobalTech$to_year[ L202_GlobalTech$ID %in% A_BEND_tech_interp$ID ] <- A_BEND_tech_interp$to_year[
      match( L202_GlobalTech$ID[ L202_GlobalTech$ID %in% A_BEND_tech_interp$ID ], A_BEND_tech_interp$ID ) ] 
L202_GlobalTech$interpolation_function[ L202_GlobalTech$ID %in% A_BEND_tech_interp$ID ] <- A_BEND_tech_interp$interpolation_function[
      match( L202_GlobalTech$ID[ L202_GlobalTech$ID %in% A_BEND_tech_interp$ID ], A_BEND_tech_interp$ID ) ] 
L202_GlobalTech <- L202_GlobalTech[ names( L202_GlobalTech ) != "ID" ]      
      
printlog( "L202_GlobalTechEff: GLOBAL TECHNOLOGY EFFICIENCIES" )
L202_GlobalTechEff <- data.frame(
      sector_name = A_BEND_tech_eff_Y$supplysector,
      subsector_name = A_BEND_tech_eff_Y$subsector,
      technology = A_BEND_tech_eff_Y$technology,
      minicam_energy_input = A_BEND_tech_eff_Y$minicam_energy_input,
      A_BEND_tech_eff_Y[ X_GCAM_model_period0 ],
      round( A_BEND_tech_eff_Y[ X_GCAM_years ], CalInput_digits ) )
      
printlog( "L202_TechEff: STUB TECHNOLOGY EFFICIENCIES FOR DISTRICT SERVICES" )
L202_eff_state_distserv_F <- L126_eff_state_distserv_F
L202_eff_state_distserv_F[ sup_sub_tech_input ] <- A_BEND_technology[
      match( paste( L202_eff_state_distserv_F$GCAM_sector, L202_eff_state_distserv_F$GCAM_fuel ),
             paste( A_BEND_technology$GCAM_sector, A_BEND_technology$GCAM_fuel ) ),
      sup_sub_tech_input ]

L202_TechEff_DistServ <- data.frame(
      region = L202_eff_state_distserv_F$state,
      supplysector = L202_eff_state_distserv_F$supplysector,
      subsector = L202_eff_state_distserv_F$subsector,
      stub_technology = L202_eff_state_distserv_F$technology,
      minicam_energy_input = L202_eff_state_distserv_F$minicam_energy_input )
L202_TechEff_DistServ[ X_GCAM_years ] <- round( L202_eff_state_distserv_F$efficiency, CalInput_digits ) 

printlog( "L202_GlobalTechCost: GLOBAL TECHNOLOGY NON-ENERGY COSTS" )
L202_GlobalTechCost <- data.frame(
      sector_name = A_BEND_tech_cost_Y$supplysector,
      subsector_name = A_BEND_tech_cost_Y$subsector,
      technology = A_BEND_tech_cost_Y$technology,
      round( A_BEND_tech_cost_Y[ X_GCAM_years ], cost_digits ) )

printlog( "L202_GlobalTechIntgains: GLOBAL TECHNOLOGY INTERNAL GAINS" )
#  Start with efficiencies table, and map in the input ratios (heat out per unit of energy in)
L202_GlobalTechIntgains <- L202_GlobalTechEff[ c( globaltech_ID, X_GCAM_years ) ]
L202_GlobalTechIntgains$input_ratio <- A_BEND_tech_intgains$input_ratio[
      match( paste( L202_GlobalTechIntgains$sector_name, L202_GlobalTechIntgains$subsector_name, L202_GlobalTechIntgains$technology ),
             paste( A_BEND_tech_intgains$supplysector, A_BEND_tech_intgains$subsector, A_BEND_tech_intgains$technology ) ) ] 
#Divide input ratio by efficiency, drop the "input_ratio" column from the table, and drop rows with any "NA" values
L202_GlobalTechIntgains[ X_GCAM_years ] <- round(
      L202_GlobalTechIntgains$input_ratio / L202_GlobalTechEff[ X_GCAM_years ], intgains_digits )
L202_GlobalTechIntgains <- L202_GlobalTechIntgains[ !is.na( L202_GlobalTechIntgains$input_ratio ), ]
L202_GlobalTechIntgains <- L202_GlobalTechIntgains[ names( L202_GlobalTechIntgains ) != "input_ratio" ]

printlog( "L202_GlobalTechIntgainsMkt: GLOBAL TECHNOLOGY INTERNAL GAINS MARKET NAME" )
L202_GlobalTechIntgainsMkt <- L202_GlobalTechIntgains[ globaltech_ID ]
#NOTE: Using first three characters of service names to match to final-demand sector names
L202_GlobalTechIntgainsMkt$internal_gains_market_name <- A_BEND_demand$internal_gains_market_name[
      match( substr( L202_GlobalTechIntgainsMkt$sector_name, 1, 3 ),
             substr( A_BEND_demand$gcam_consumer, 1, 3 ) ) ]

printlog( "L202_GlobalTechRetirement_Stock: GLOBAL TECHNOLOGY RETIREMENT, EXISTING STOCK" )
L202_GlobalTechRetirement_Stock <- data.frame(
      L202_GlobalTechEff[ globaltech_ID ],
      period = final_cal_year )
L202_GlobalTechRetirement_Stock$lifetime <- A_BEND_tech_retirement$lifetime[
      match( L202_GlobalTechRetirement_Stock$sector_name, A_BEND_tech_retirement$supplysector ) ]      
L202_GlobalTechRetirement_Stock <- L202_GlobalTechRetirement_Stock[ !is.na( L202_GlobalTechRetirement_Stock$lifetime ), ]
L202_GlobalTechRetirement_Stock$half_life <- A_BEND_tech_retirement$half_life_stock[
      match( L202_GlobalTechRetirement_Stock$sector_name, A_BEND_tech_retirement$supplysector ) ]
L202_GlobalTechRetirement_Stock$steepness <- round( A_BEND_tech_retirement$steepness_stock[
      match( L202_GlobalTechRetirement_Stock$sector_name, A_BEND_tech_retirement$supplysector ) ], retirement_fn_digits )

printlog( "L202_GlobalTechRetirement_New: GLOBAL TECHNOLOGY RETIREMENT, NEW INVESTMENT" )
L202_GlobalTechRetirement_New <- L202_GlobalTechRetirement_Stock[ rep( 1:nrow( L202_GlobalTechRetirement_Stock ),
      times = length( GCAM_future_years ) ), ]
L202_GlobalTechRetirement_New$period <- sort( rep( GCAM_future_years, length.out = nrow( L202_GlobalTechRetirement_New ) ) )
L202_GlobalTechRetirement_New$half_life <- A_BEND_tech_retirement$half_life_new[
      match( L202_GlobalTechRetirement_New$sector_name, A_BEND_tech_retirement$supplysector ) ]
L202_GlobalTechRetirement_New$steepness <- round( A_BEND_tech_retirement$steepness_new[
      match( L202_GlobalTechRetirement_New$sector_name, A_BEND_tech_retirement$supplysector ) ], retirement_fn_digits )

printlog( "L202_GlobalTechBaseYearShareweight: BASE YEAR SHAREWEIGHTS FOR TECHS NOT IN THE CALIBRATED INPUT SET" )
L202_GlobalTechBaseYearShareweight <- A_BEND_technology[
      paste( A_BEND_technology$supplysector, A_BEND_technology$subsector, A_BEND_technology$technology ) %!in%
      paste( L202_CalInput_Bld$supplysector, L202_CalInput_Bld$subsector, L202_CalInput_Bld$stub_technology ),
      sup_sub_tech ]
L202_GlobalTechBaseYearShareweight$period <- "base periods"
L202_GlobalTechBaseYearShareweight$share.weight <- 0
names( L202_GlobalTechBaseYearShareweight )[ names( L202_GlobalTechBaseYearShareweight ) == "supplysector" ] <- "sector_name"
names( L202_GlobalTechBaseYearShareweight )[ names( L202_GlobalTechBaseYearShareweight ) == "subsector" ] <- "subsector_name"

printlog( "L202_GlobalTechFutureShareweight: FUTURE SHAREWEIGHTS FOR TECHS WITH SPECIFIED INTERPOLATION FUNCTIONS" )
L202_GlobalTechFutureShareweight <- data.frame(
      sector_name = A_BEND_tech_interp$supplysector,
      subsector_name = A_BEND_tech_interp$subsector,
      technology = A_BEND_tech_interp$technology,
      period = "future periods",
      share.weight = A_BEND_tech_interp$share.weight )

printlog( "L202_MarketName: MARKET NAMES FOR THE INPUT FUELS" )
L202_MarketName <- data.frame(
      region = sort( rep( states, times = nrow( A_BEND_tech_eff_Y ) ) ),
      A_BEND_tech_eff_Y[ sup_sub_tech_input ] )
L202_MarketName$market_name <- A_fuel_markets$market[
      match( paste( L202_MarketName$region, L202_MarketName$minicam_energy_input ),
             paste( A_fuel_markets$state, A_fuel_markets$minicam_energy_input ) ) ]

# Re-set district services to local markets. These are missing values here.
L202_MarketName <- L202_MarketName[ !is.na( L202_MarketName$market_name ), ]             


# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L202_BldDelete, "BldDelete", "L202_BldDelete", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_PopShare, "PopShare", "L202_PopShare", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_IncShare, "IncShare", "L202_IncShare", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_BldDemandFn1, "BldDemandFn1", "L202_BldDemandFn1", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_BldDemandFn2, "BldDemandFn2", "L202_BldDemandFn2", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_BldFlsp, "BldFlsp", "L202_BldFlsp", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_BldServSatn, "BldServSatn", "L202_BldServSatn", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_BldThermalServSatn, "BldThermalServSatn", "L202_BldThermalServSatn", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_DegreeDays, "DegreeDays", "L202_DegreeDays", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_BldServOut, "BldServOut", "L202_BldServOut", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_BldThermalServOut, "BldThermalServOut", "L202_BldThermalServOut", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_BldShell, "BldShell", "L202_BldShell", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_Supplysector, "Supplysector", "L202_Supplysector", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_Subsector, "Subsector", "L202_Subsector", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_SubsectorPhaseout, "SubsectorPhaseout", "L202_SubsectorPhaseout", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_GlobalTech, "GlobalTech", "L202_GlobalTech", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_GlobalTechEff, "GlobalTechEff", "L202_GlobalTechEff", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_TechEff_DistServ, "TechEff", "L202_TechEff_DistServ", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_GlobalTechCost, "GlobalTechCost", "L202_GlobalTechCost", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_GlobalTechIntgainsMkt, "GlobalTechIntgainsMkt", "L202_GlobalTechIntgainsMkt", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_GlobalTechIntgains, "GlobalTechIntgains", "L202_GlobalTechIntgains", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_GlobalTechRetirement_Stock, "GlobalTechRetirement", "L202_GlobalTechRetirement_Stock", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_GlobalTechRetirement_New, "GlobalTechRetirement", "L202_GlobalTechRetirement_New", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_GlobalTechBaseYearShareweight, "GlobalTechBaseYearShareweight", "L202_GlobalTechBaseYearShareweight", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_GlobalTechFutureShareweight, "GlobalTechFutureShareweight", "L202_GlobalTechFutureShareweight", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_MarketName, "MarketName", "L202_MarketName", "batch_rgcam_bend_input_base.xml" ) 
write_mi_data( L202_CalInput_Bld, "CalInput", "L202_CalInput_Bld", "batch_rgcam_bend_input_base.xml" ) 

insert_file_into_batchxml( "batch_rgcam_bend_input_base.xml", "rgcam_bend_input_base.xml", "", xml_tag="outFile" )

logstop()