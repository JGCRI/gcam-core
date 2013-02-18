# L201_RGCAM_bld_input.R
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L201_RGCAM_bld_input.R" )
printlog( "RGCAM Buildings sector model input file" )

# -----------------------------------------------------------------------------
# 1. Read files

A_bld_demand <- readdata( "A_bld_demand" )
A_bld_subs_logit <- readdata( "A_bld_subs_logit" )
A_bld_tech_logit <- readdata( "A_bld_tech_logit" )
A_bld_service <- readdata( "A_bld_service" )
A_bld_shell_conductance <- readdata( "A_bld_shell_conductance" )
A_bld_shell_conductance_adv <- readdata( "A_bld_shell_conductance_adv" )
A_bld_tech_cost_Y <- readdata( "A_bld_tech_cost_Y" )
A_bld_tech_cost_Y_adv <- readdata( "A_bld_tech_cost_Y_adv" )
A_bld_tech_eff_Y <- readdata( "A_bld_tech_eff_Y" )
A_bld_tech_eff_Y_adv <- readdata( "A_bld_tech_eff_Y_adv" )
A_bld_tech_interp <- readdata( "A_bld_tech_interp" )
A_bld_tech_intgains <- readdata( "A_bld_tech_intgains" )
A_bld_tech_retirement <- readdata( "A_bld_tech_retirement" )
A_bld_technology <- readdata( "A_bld_technology" )
Census_pop_hist <- readdata( "Census_pop_hist" )
A_bld_Delete <- readdata( "A_bld_Delete" )
A_bld_Tprteff <- readdata( "A_bld_Tprteff" )
A_bld_Tdiff_shares <- readdata( "A_bld_Tdiff_shares" )
A_fuel_markets <- readdata( "A_fuel_markets" )
CDD_His_Fut <- readdata( "CDD_His_Fut" )
HDD_His_Fut <- readdata( "HDD_His_Fut" )
L111_flsp_bm2_state_res <- readdata( "L111_flsp_bm2_state_res" )
L115_in_EJ_state_res_F_U <- readdata( "L115_in_EJ_state_res_F_U" )
L121_flsp_bm2_state_comm <- readdata( "L121_flsp_bm2_state_comm" )
L125_in_EJ_state_comm_F_U <- readdata( "L125_in_EJ_state_comm_F_U" )

# -----------------------------------------------------------------------------
# 2. Perform computations and build model input files
printlog( "L201_BldDelete: DELETE EXISTING US BUILDINGS DEMAND SECTOR" )
L201_BldDelete <- A_bld_Delete 

printlog( "L201_PopShare: SUBREGIONAL POPULATION SHARE" )
L201_PopShare <- data.frame( gcam_consumer = A_bld_demand$gcam_consumer )
L201_PopShare[ c( X_GCAM_model_period0, X_GCAM_years ) ] <- 1

printlog( "L201_IncShare: SUBREGIONAL INCOME SHARE" )
L201_IncShare <- data.frame( gcam_consumer = A_bld_demand$gcam_consumer )
L201_IncShare[ c( X_GCAM_model_period0, X_GCAM_years ) ] <- 1

printlog( "L201_BldDemandFn: BUILDING SERVICE DEMAND FUNCTION TYPES" )
L201_BldDemandFn1 <- data.frame( gcam_consumer = A_bld_demand$gcam_consumer,
      nodeInput = A_bld_demand$nodeInput,
      prodDmdFnType = "building-function" )

L201_BldDemandFn2 <- data.frame( A_bld_demand[ bld_node_names ],
      prodDmdFnType = "building-service-function" )

printlog( "L201_BldFlsp: BASE-YEAR FLOORSPACE AND SATIATION DEMAND LEVELS" )
#Combine residential and commercial tables
L201_flsp_bm2_state_bld <- rbind( L111_flsp_bm2_state_res, L121_flsp_bm2_state_comm )
L201_flsp_bm2_state_bld <- L201_flsp_bm2_state_bld[ order( L201_flsp_bm2_state_bld$GCAM_sector ), ]

#  Repeat demands by the number of states
A_bld_demand_repstate <- A_bld_demand[ rep( 1:nrow( A_bld_demand ), times = length( states ) ), ]
A_bld_demand_repstate <- A_bld_demand_repstate[ order( A_bld_demand_repstate$gcam_consumer ), ]
L201_BldFlsp <- data.frame( region = L201_flsp_bm2_state_bld$state,
      A_bld_demand_repstate[ bld_node_names ],
      round( L201_flsp_bm2_state_bld[ X_GCAM_base_years ], flsp_digits ),
      price_exponent = P_elas_flsp,
      satiation_level = 0,
      satiation_adder = 0,
      internal_gains_market_name = A_bld_demand_repstate$internal_gains_market_name,
      internal_gains_unit = "EJ" )
      
#  Need to read in the satiation level, in billion square meters per thousand persons. Equal to fixed multiplier times the base year floorspace
#  The satiation level can not be less than the per-capita floorspace in 1990, so make sure to set that as a minimum threshold level in each state/sector      
L201_flsp_bm2_state_bld$pop_1990 <- Census_pop_hist$X1990[ match( L201_flsp_bm2_state_bld$state, Census_pop_hist$state ) ]
L201_flsp_bm2_state_bld$pop_2005 <- Census_pop_hist$X2005[ match( L201_flsp_bm2_state_bld$state, Census_pop_hist$state ) ]

L201_flsp_bm2_state_bld$pcflsp_Mm2_1990 <- L201_flsp_bm2_state_bld$X1990 * 1e3 / L201_flsp_bm2_state_bld$pop_1990
L201_flsp_bm2_state_bld$pcflsp_Mm2_2005 <- L201_flsp_bm2_state_bld$X2005 * 1e3 / L201_flsp_bm2_state_bld$pop_2005

#  TODO: The fixed multiplier should be calculated as a function of per-capita income and floorspace income elasticity
L201_flsp_bm2_state_bld$satiation_level <- pmax( L201_flsp_bm2_state_bld$pcflsp_Mm2_2005 * satiation_flsp_mult,
                                                 L201_flsp_bm2_state_bld$pcflsp_Mm2_1990 * 1.01 )

L201_BldFlsp$satiation_level <- round( L201_flsp_bm2_state_bld$satiation_level[
      match( paste( L201_BldFlsp$region, L201_BldFlsp$gcam_consumer ),
             paste( L201_flsp_bm2_state_bld$state, L201_flsp_bm2_state_bld$GCAM_sector ) ) ],
      flsp_sat_digits )

printlog( "L201_BldServSatn and L201_BldThermalServSatn: BUILDING SERVICE SATIATION LEVELS" )
L201_BldServSatn <- data.frame( A_bld_service[ 
      A_bld_service$building_service_input %!in% thermal_services, 
      c( bld_node_names, "building_service_input", "satiation_base_year_increase", "satiation_adder" ) ] )

L201_BldThermalServSatn <- data.frame( A_bld_service[ 
      A_bld_service$building_service_input %in% thermal_services, 
      c( bld_node_names, "building_service_input", "internal_gains_scaler", "satiation_base_year_increase", "satiation_adder" ) ] )
names( L201_BldThermalServSatn )[ names( L201_BldThermalServSatn ) == "building_service_input" ] <- "thermal_building_service_input"

printlog( "L201_DegreeDays: HEATING DEGREE DAYS, COOLING DEGREE DAYS" )
#First, build tables with the service names
L201_heating_services <- A_bld_service[ 
      A_bld_service$building_service_input %in% heating_services, c( bld_node_names, "building_service_input" ) ]
L201_cooling_services <- A_bld_service[ 
      A_bld_service$building_service_input %in% cooling_services, c( bld_node_names, "building_service_input" ) ]

#Repeat by number of states, and sort by service
L201_heating_services_repstate <- L201_heating_services[ rep( 1:nrow( L201_heating_services ), times = length( states ) ), ]
L201_heating_services_repstate <- L201_heating_services_repstate[ order( L201_heating_services_repstate$gcam_consumer ), ]
L201_cooling_services_repstate <- L201_cooling_services[ rep( 1:nrow( L201_cooling_services ), times = length( states ) ), ]
L201_cooling_services_repstate <- L201_cooling_services_repstate[ order( L201_cooling_services_repstate$gcam_consumer ), ]

#Note the base assumption for heating and cooling degree days is to fix them 
#to the final base year value for all years.
#Changing degree days in the future come from a scenario which should thus
#be matched to the scenario being run, i.e. A2 or B2.
L201_HDD_Fixed <- HDD_His_Fut
L201_HDD_Fixed[, c( X_GCAM_model_period0, X_GCAM_years ) ] <- HDD_His_Fut[, X_GCAM_base_years[2] ]
L201_HDD <- data.frame(
      region = rep( L201_HDD_Fixed$state, length.out = nrow( L201_heating_services_repstate ) ),
      L201_heating_services_repstate,
      L201_HDD_Fixed[ c( X_GCAM_model_period0, X_GCAM_years ) ] )
      
L201_CDD_Fixed <- CDD_His_Fut
L201_CDD_Fixed[, c( X_GCAM_model_period0, X_GCAM_years ) ] <- CDD_His_Fut[, X_GCAM_base_years[2] ]
L201_CDD <- data.frame(
      region = rep( L201_CDD_Fixed$state, length.out = nrow( L201_cooling_services_repstate ) ),
      L201_cooling_services_repstate,
      L201_CDD_Fixed[ c( X_GCAM_model_period0, X_GCAM_years ) ] )

L201_DegreeDays <- rbind( L201_HDD, L201_CDD )
L201_DegreeDays[ c( X_GCAM_model_period0, X_GCAM_years ) ] <- 
      round( L201_DegreeDays[ c( X_GCAM_model_period0, X_GCAM_years ) ], HDDCDD_digits )

printlog( "L201_CalInput: BASE YEAR ENERGY CONSUMPTION BY TECHNOLOGY" )
#Combine (rbind) tables of residential and commercial energy by state / fuel / service
L201_in_EJ_state_bld_F_U <- rbind( L115_in_EJ_state_res_F_U, L125_in_EJ_state_comm_F_U )

#Match in GCAM supplysector/subsector/technology names to energy input table
L201_in_EJ_state_bld_F_U$supplysector <- A_bld_technology$supplysector[
      match( L201_in_EJ_state_bld_F_U$service, A_bld_technology$service ) ]
L201_in_EJ_state_bld_F_U$subsector <- A_bld_technology$subsector[
      match( L201_in_EJ_state_bld_F_U$GCAM_fuel, A_bld_technology$GCAM_fuel ) ]
L201_in_EJ_state_bld_F_U$minicam_energy_input <- A_bld_technology$minicam_energy_input[
      match( paste( L201_in_EJ_state_bld_F_U$service, L201_in_EJ_state_bld_F_U$GCAM_fuel ),
             paste( A_bld_technology$service, A_bld_technology$GCAM_fuel ) ) ]

#Subset "differentiated" technologies where similar service/fuels are partitioned into separate technologies, according to technology types (e.g. incandescent and fluorescent lighting)
L201_in_EJ_state_bld_F_U_Tdiff <- L201_in_EJ_state_bld_F_U[
      paste( L201_in_EJ_state_bld_F_U$supplysector, L201_in_EJ_state_bld_F_U$subsector ) %in%
      paste( A_bld_Tdiff_shares$supplysector, A_bld_Tdiff_shares$subsector ), ]
#Match in the technology names
L201_in_EJ_state_bld_F_U_Tdiff$technology1 <- A_bld_Tdiff_shares$technology1[
      match( paste( L201_in_EJ_state_bld_F_U_Tdiff$supplysector, L201_in_EJ_state_bld_F_U_Tdiff$subsector ),
             paste( A_bld_Tdiff_shares$supplysector, A_bld_Tdiff_shares$subsector ) ) ]
L201_in_EJ_state_bld_F_U_Tdiff$technology2 <- A_bld_Tdiff_shares$technology2[
      match( paste( L201_in_EJ_state_bld_F_U_Tdiff$supplysector, L201_in_EJ_state_bld_F_U_Tdiff$subsector ),
             paste( A_bld_Tdiff_shares$supplysector, A_bld_Tdiff_shares$subsector ) ) ]

#Subset "partitioned efficiency" subsectors where similar service/fuels are partitioned into separate technologies, according to efficiencies
L201_in_EJ_state_bld_F_U_Tprteff <- L201_in_EJ_state_bld_F_U[
      paste( L201_in_EJ_state_bld_F_U$supplysector, L201_in_EJ_state_bld_F_U$subsector ) %in%
      paste( A_bld_Tprteff$supplysector, A_bld_Tprteff$subsector ), ]
#Match in the technology names
L201_in_EJ_state_bld_F_U_Tprteff$technology1 <- A_bld_Tprteff$technology1[
      match( paste( L201_in_EJ_state_bld_F_U_Tprteff$supplysector, L201_in_EJ_state_bld_F_U_Tprteff$subsector ),
             paste( A_bld_Tprteff$supplysector, A_bld_Tprteff$subsector ) ) ]
L201_in_EJ_state_bld_F_U_Tprteff$technology2 <- A_bld_Tprteff$technology2[
      match( paste( L201_in_EJ_state_bld_F_U_Tprteff$supplysector, L201_in_EJ_state_bld_F_U_Tprteff$subsector ),
             paste( A_bld_Tprteff$supplysector, A_bld_Tprteff$subsector ) ) ]

#Subset the remaining technologies into a separate table
L201_in_EJ_state_bld_F_U_Tgen <- L201_in_EJ_state_bld_F_U[
      paste( L201_in_EJ_state_bld_F_U$supplysector, L201_in_EJ_state_bld_F_U$subsector ) %!in%
      c( paste( L201_in_EJ_state_bld_F_U_Tprteff$supplysector, L201_in_EJ_state_bld_F_U_Tprteff$subsector ),
         paste( L201_in_EJ_state_bld_F_U_Tdiff$supplysector, L201_in_EJ_state_bld_F_U_Tdiff$subsector ) ), ]
L201_in_EJ_state_bld_F_U_Tgen$technology <- A_bld_technology$technology[
      match( paste( L201_in_EJ_state_bld_F_U_Tgen$supplysector, L201_in_EJ_state_bld_F_U_Tgen$subsector ),
             paste( A_bld_technology$supplysector, A_bld_technology$subsector ) ) ]

#Map in differentiated (exogenous) technology shares
L201_in_EJ_state_bld_F_U_Tdiff$share_tech1 <- A_bld_Tdiff_shares$share_tech1[
      match( paste( L201_in_EJ_state_bld_F_U_Tdiff$supplysector, L201_in_EJ_state_bld_F_U_Tdiff$subsector, L201_in_EJ_state_bld_F_U_Tdiff$technology1 ),
             paste( A_bld_Tdiff_shares$supplysector, A_bld_Tdiff_shares$subsector, A_bld_Tdiff_shares$technology1 ) ) ]
L201_in_EJ_state_bld_F_U_Tdiff$share_tech2 <- 1 - L201_in_EJ_state_bld_F_U_Tdiff$share_tech1

#Partitioned efficiency technology shares are derived according to stock average efficiency compared with the efficiency of tech1 and tech2
A_bld_Tprteff$share_tech1 <- with( A_bld_Tprteff, ( stockavg - eff_tech2 ) / ( eff_tech1 - eff_tech2 ) )
L201_in_EJ_state_bld_F_U_Tprteff$share_tech1 <- A_bld_Tprteff$share_tech1[
      match( paste( L201_in_EJ_state_bld_F_U_Tprteff$supplysector, L201_in_EJ_state_bld_F_U_Tprteff$subsector ),
             paste( A_bld_Tprteff$supplysector, A_bld_Tprteff$subsector ) ) ]
L201_in_EJ_state_bld_F_U_Tprteff$share_tech2 <- 1 - L201_in_EJ_state_bld_F_U_Tprteff$share_tech1

#These shares may need to be further modified for technologies that are both differentiated AND partitioned (e.g. electric heat pumps)
if( any( paste( L201_in_EJ_state_bld_F_U_Tprteff$supplysector, L201_in_EJ_state_bld_F_U_Tprteff$subsector, L201_in_EJ_state_bld_F_U_Tprteff$technology1 ) %in%
             paste( L201_in_EJ_state_bld_F_U_Tdiff$supplysector, L201_in_EJ_state_bld_F_U_Tdiff$subsector, L201_in_EJ_state_bld_F_U_Tdiff$technology1 ) ) )
   { L201_in_EJ_state_bld_F_U_Tprteff$share_adj <- L201_in_EJ_state_bld_F_U_Tdiff$share_tech1[
      match( paste( L201_in_EJ_state_bld_F_U_Tprteff$supplysector, L201_in_EJ_state_bld_F_U_Tprteff$subsector, L201_in_EJ_state_bld_F_U_Tprteff$technology1 ) %in%
             paste( L201_in_EJ_state_bld_F_U_Tdiff$supplysector, L201_in_EJ_state_bld_F_U_Tdiff$subsector, L201_in_EJ_state_bld_F_U_Tdiff$technology1 ) ) ]
   } else { L201_in_EJ_state_bld_F_U_Tprteff$share_adj <- 1 }

#Build calibrated input table for generic technologies
L201_CalInput_Bld_GenTech <- data.frame(
      region = L201_in_EJ_state_bld_F_U_Tgen$state,
      supplysector = L201_in_EJ_state_bld_F_U_Tgen$supplysector,
      subsector = L201_in_EJ_state_bld_F_U_Tgen$subsector,
      stub_technology = L201_in_EJ_state_bld_F_U_Tgen$technology,
      minicam_energy_input = L201_in_EJ_state_bld_F_U_Tgen$minicam_energy_input,
      L201_in_EJ_state_bld_F_U_Tgen[ X_GCAM_base_years ] )

#Build calibrated input tables for differentiated technologies
L201_CalInput_Bld_DiffTech1 <- data.frame(
      region = L201_in_EJ_state_bld_F_U_Tdiff$state,
      supplysector = L201_in_EJ_state_bld_F_U_Tdiff$supplysector,
      subsector = L201_in_EJ_state_bld_F_U_Tdiff$subsector,
      stub_technology = L201_in_EJ_state_bld_F_U_Tdiff$technology1,
      minicam_energy_input = L201_in_EJ_state_bld_F_U_Tdiff$minicam_energy_input,
      L201_in_EJ_state_bld_F_U_Tdiff[ X_GCAM_base_years ] * L201_in_EJ_state_bld_F_U_Tdiff$share_tech1 )

L201_CalInput_Bld_DiffTech2 <- data.frame(
      region = L201_in_EJ_state_bld_F_U_Tdiff$state,
      supplysector = L201_in_EJ_state_bld_F_U_Tdiff$supplysector,
      subsector = L201_in_EJ_state_bld_F_U_Tdiff$subsector,
      stub_technology = L201_in_EJ_state_bld_F_U_Tdiff$technology2,
      minicam_energy_input = L201_in_EJ_state_bld_F_U_Tdiff$minicam_energy_input,
      L201_in_EJ_state_bld_F_U_Tdiff[ X_GCAM_base_years ] * L201_in_EJ_state_bld_F_U_Tdiff$share_tech2 )

#Build calibrated input tables for efficiency-partitioned technologies
L201_CalInput_Bld_PrtTech1 <- data.frame(
      region = L201_in_EJ_state_bld_F_U_Tprteff$state,
      supplysector = L201_in_EJ_state_bld_F_U_Tprteff$supplysector,
      subsector = L201_in_EJ_state_bld_F_U_Tprteff$subsector,
      stub_technology = L201_in_EJ_state_bld_F_U_Tprteff$technology1,
      minicam_energy_input = L201_in_EJ_state_bld_F_U_Tprteff$minicam_energy_input,
      L201_in_EJ_state_bld_F_U_Tprteff[ X_GCAM_base_years ] * L201_in_EJ_state_bld_F_U_Tprteff$share_tech1 * L201_in_EJ_state_bld_F_U_Tprteff$share_adj )

L201_CalInput_Bld_PrtTech2 <- data.frame(
      region = L201_in_EJ_state_bld_F_U_Tprteff$state,
      supplysector = L201_in_EJ_state_bld_F_U_Tprteff$supplysector,
      subsector = L201_in_EJ_state_bld_F_U_Tprteff$subsector,
      stub_technology = L201_in_EJ_state_bld_F_U_Tprteff$technology2,
      minicam_energy_input = L201_in_EJ_state_bld_F_U_Tprteff$minicam_energy_input,
      L201_in_EJ_state_bld_F_U_Tprteff[ X_GCAM_base_years ] * L201_in_EJ_state_bld_F_U_Tprteff$share_tech2 * L201_in_EJ_state_bld_F_U_Tprteff$share_adj )

#Combine these into a single table, round to specified number of digits, add shareweight columns, and sort.
L201_CalInput_Bld <- rbind( L201_CalInput_Bld_GenTech,
      L201_CalInput_Bld_PrtTech1, L201_CalInput_Bld_PrtTech2,
      L201_CalInput_Bld_DiffTech1, L201_CalInput_Bld_DiffTech2 ) 
L201_CalInput_Bld[ X_GCAM_base_years ] <- round( L201_CalInput_Bld[ X_GCAM_base_years ], CalInput_digits )
L201_CalInput_Bld[ GCAM_base_shareweights ] <- as.numeric( L201_CalInput_Bld[ X_GCAM_base_years ] > 0 )
L201_CalInput_Bld <- L201_CalInput_Bld[ order( L201_CalInput_Bld$region, as.character( L201_CalInput_Bld$supplysector ),
                                               as.character( L201_CalInput_Bld$subsector ) ), ]

printlog( "L201_BldServOut and L201_BldThermalServOut: BASE SERVICE BY STATE AND BUILDING SERVICE" )
#Computation of output by service - map in the efficiencies
L201_eff_state_bld_tech <- L201_CalInput_Bld[ c( R_sup_sub_stubtech_input ) ]
L201_eff_state_bld_tech[ X_GCAM_base_years ] <- A_bld_tech_eff_Y[
      match( paste( L201_eff_state_bld_tech$supplysector, L201_eff_state_bld_tech$stub_technology ),
             paste( A_bld_tech_eff_Y$supplysector, A_bld_tech_eff_Y$technology ) ),
      X_GCAM_base_years ]
      
L201_out_EJ_state_bld_tech <- L201_CalInput_Bld[ c( R_sup_sub_stubtech_input ) ]
L201_out_EJ_state_bld_tech[ X_GCAM_base_years ] <- L201_CalInput_Bld[ X_GCAM_base_years ] * L201_eff_state_bld_tech[ X_GCAM_base_years ]

#Aggregate by service
L201_out_EJ_state_bld_serv <- aggregate( L201_out_EJ_state_bld_tech[ X_GCAM_base_years ],
          list( region = L201_out_EJ_state_bld_tech$region, building_service_input = L201_out_EJ_state_bld_tech$supplysector ), sum )

#Match in the building node names, and round to a specified number of decimal places
L201_out_EJ_state_bld_serv[ bld_node_names ] <- A_bld_service[
      match( L201_out_EJ_state_bld_serv$building_service_input, A_bld_service$building_service_input ),
      bld_node_names ] 
L201_out_EJ_state_bld_serv[ X_GCAM_base_years ] <- round( L201_out_EJ_state_bld_serv[ X_GCAM_base_years ], CalInput_digits )

#Subset to build the tables of service output by state and service (non-thermal services and thermal services)
L201_BldServOut <- L201_out_EJ_state_bld_serv[ L201_out_EJ_state_bld_serv$building_service_input %!in% thermal_services,
      c( "region", bld_node_names, "building_service_input", X_GCAM_base_years ) ]

L201_BldThermalServOut <- L201_out_EJ_state_bld_serv[ L201_out_EJ_state_bld_serv$building_service_input %in% thermal_services,
      c( "region", bld_node_names, "building_service_input", X_GCAM_base_years ) ]

printlog( "L201_BldShell: SHELL CONDUCTANCE AND FLOORSPACE TO SURFACE AREA RATIO" )
A_bld_shell_conductance.melt <- melt( A_bld_shell_conductance, id.vars = "gcam_consumer" )
A_bld_shell_conductance.melt$year <- as.numeric( substr( A_bld_shell_conductance.melt$variable, 2, 5 ) )

L201_BldShell <- A_bld_demand[ rep( 1:nrow( A_bld_demand ), times = length( c( GCAM_model_period0, GCAM_years ) ) ), bld_node_names ]
L201_BldShell <- L201_BldShell[ order( L201_BldShell$gcam_consumer ), ]
L201_BldShell$shell_conductance_year <- rep( c( GCAM_model_period0, GCAM_years ), length.out = nrow( L201_BldShell ) )
L201_BldShell$shell_conductance <- A_bld_shell_conductance.melt$value[
      match( paste( L201_BldShell$gcam_consumer, L201_BldShell$shell_conductance_year ),
             paste( A_bld_shell_conductance.melt$gcam_consumer, A_bld_shell_conductance.melt$year ) ) ]
L201_BldShell$floor_to_surface_year <- L201_BldShell$shell_conductance_year
L201_BldShell$floor_to_surface_ratio <- floor_to_surface

printlog( "L201_Supplysector: SUPPLYSECTOR UNITS AND LOGIT EXPONENT" )
L201_Supplysector <- data.frame(
      supplysector = A_bld_subs_logit$supplysector,
      output_unit = "EJ",
      input_unit = "EJ",
      price_unit = "1975$/GJ",
      price = 1,
      final_energy_keyword = "building",
      logit_exponent = A_bld_subs_logit$subs_logit )
      
printlog( "L201_Subsector: SUBSECTOR INTERPOLATION AND LOGIT EXPONENT" )
L201_Subsector <- data.frame(
      supplysector = A_bld_tech_logit$supplysector,
      subsector = A_bld_tech_logit$subsector,
      share_weight = 1,
      apply_to = "share-weight",
      from_year = final_cal_year,
      to_year = 9999,
      interpolation_function = "fixed",
      logit_exponent = A_bld_tech_logit$tech_logit, stringsAsFactors = FALSE )     

#Reset coal shareweights to a linear phase-out
L201_Subsector$to_year[ L201_Subsector$subsector == "coal" ] <- coal_phaseout_year
L201_Subsector$interpolation_function[ L201_Subsector$subsector == "coal" ] <- "linear"

printlog( "L201_SubsectorPhaseout: SUBSECTOR PHASEOUT (FOR COAL)" )
L201_SubsectorPhaseout <- data.frame(
      supplysector = A_bld_technology$supplysector[ A_bld_technology$subsector == "coal" ],
      subsector = "coal",
      share_weight_year = coal_phaseout_year,
      share_weight = 0 )

printlog( "L201_GlobalTech: GLOBAL TECHNOLOGY INTERPOLATION" )
L201_GlobalTech <- data.frame(
      sector_name = A_bld_technology$supplysector,
      subsector_name = A_bld_technology$subsector,
      technology = A_bld_technology$technology,
      apply_to = "share-weight",
      from_year = final_cal_year,
      to_year = 9999,
      interpolation_function = "fixed", stringsAsFactors = FALSE )
      
#For specified technologies, use an s-curve function to a specified year
#First, generate a temporary ID vector
L201_GlobalTech$ID <- paste( L201_GlobalTech$sector_name, L201_GlobalTech$subsector_name, L201_GlobalTech$technology )
A_bld_tech_interp$ID <- paste( A_bld_tech_interp$supplysector, A_bld_tech_interp$subsector, A_bld_tech_interp$technology )
L201_GlobalTech$from_year[ L201_GlobalTech$ID %in% A_bld_tech_interp$ID ] <- A_bld_tech_interp$from_year[
      match( L201_GlobalTech$ID[ L201_GlobalTech$ID %in% A_bld_tech_interp$ID ], A_bld_tech_interp$ID ) ] 
L201_GlobalTech$to_year[ L201_GlobalTech$ID %in% A_bld_tech_interp$ID ] <- A_bld_tech_interp$to_year[
      match( L201_GlobalTech$ID[ L201_GlobalTech$ID %in% A_bld_tech_interp$ID ], A_bld_tech_interp$ID ) ] 
L201_GlobalTech$interpolation_function[ L201_GlobalTech$ID %in% A_bld_tech_interp$ID ] <- A_bld_tech_interp$interpolation_function[
      match( L201_GlobalTech$ID[ L201_GlobalTech$ID %in% A_bld_tech_interp$ID ], A_bld_tech_interp$ID ) ] 
L201_GlobalTech <- L201_GlobalTech[ names( L201_GlobalTech ) != "ID" ]      
      
printlog( "L201_GlobalTechEff: GLOBAL TECHNOLOGY EFFICIENCIES" )
L201_GlobalTechEff <- data.frame(
      sector_name = A_bld_tech_eff_Y$supplysector,
      subsector_name = A_bld_tech_eff_Y$subsector,
      technology = A_bld_tech_eff_Y$technology,
      minicam_energy_input = A_bld_tech_eff_Y$minicam_energy_input,
      A_bld_tech_eff_Y[ X_GCAM_model_period0 ],
      round( A_bld_tech_eff_Y[ X_GCAM_years ], CalInput_digits ) )
      
printlog( "L201_GlobalTechCost: GLOBAL TECHNOLOGY NON-ENERGY COSTS" )
L201_GlobalTechCost <- data.frame(
      sector_name = A_bld_tech_cost_Y$supplysector,
      subsector_name = A_bld_tech_cost_Y$subsector,
      technology = A_bld_tech_cost_Y$technology,
      round( A_bld_tech_cost_Y[ X_GCAM_years ], cost_digits ) )

printlog( "L201_GlobalTechIntgains: GLOBAL TECHNOLOGY INTERNAL GAINS" )
#  Start with efficiencies table, and map in the input ratios (heat out per unit of energy in)
L201_GlobalTechIntgains <- L201_GlobalTechEff[ c( globaltech_ID, X_GCAM_years ) ]
L201_GlobalTechIntgains$input_ratio <- A_bld_tech_intgains$input_ratio[
      match( paste( L201_GlobalTechIntgains$sector_name, L201_GlobalTechIntgains$subsector_name, L201_GlobalTechIntgains$technology ),
             paste( A_bld_tech_intgains$supplysector, A_bld_tech_intgains$subsector, A_bld_tech_intgains$technology ) ) ] 
#Divide input ratio by efficiency, drop the "input_ratio" column from the table, and drop rows with any "NA" values
L201_GlobalTechIntgains[ X_GCAM_years ] <- round(
      L201_GlobalTechIntgains$input_ratio / L201_GlobalTechEff[ X_GCAM_years ], intgains_digits )
L201_GlobalTechIntgains <- L201_GlobalTechIntgains[ !is.na( L201_GlobalTechIntgains$input_ratio ), ]
L201_GlobalTechIntgains <- L201_GlobalTechIntgains[ names( L201_GlobalTechIntgains ) != "input_ratio" ]

printlog( "L201_GlobalTechIntgainsMkt: GLOBAL TECHNOLOGY INTERNAL GAINS MARKET NAME" )
L201_GlobalTechIntgainsMkt <- L201_GlobalTechIntgains[ globaltech_ID ]
#NOTE: Using first three characters of service names to match to final-demand sector names
L201_GlobalTechIntgainsMkt$internal_gains_market_name <- A_bld_demand$internal_gains_market_name[
      match( substr( L201_GlobalTechIntgainsMkt$sector_name, 1, 3 ),
             substr( A_bld_demand$gcam_consumer, 1, 3 ) ) ]

printlog( "L201_GlobalTechRetirement_Stock: GLOBAL TECHNOLOGY RETIREMENT, EXISTING STOCK" )
L201_GlobalTechRetirement_Stock <- data.frame(
      L201_GlobalTechEff[ globaltech_ID ],
      period = final_cal_year )
L201_GlobalTechRetirement_Stock$lifetime <- A_bld_tech_retirement$lifetime[
      match( L201_GlobalTechRetirement_Stock$sector_name, A_bld_tech_retirement$supplysector ) ]      
L201_GlobalTechRetirement_Stock <- L201_GlobalTechRetirement_Stock[ !is.na( L201_GlobalTechRetirement_Stock$lifetime ), ]
L201_GlobalTechRetirement_Stock$half_life <- A_bld_tech_retirement$half_life_stock[
      match( L201_GlobalTechRetirement_Stock$sector_name, A_bld_tech_retirement$supplysector ) ]
L201_GlobalTechRetirement_Stock$steepness <- round( A_bld_tech_retirement$steepness_stock[
      match( L201_GlobalTechRetirement_Stock$sector_name, A_bld_tech_retirement$supplysector ) ], retirement_fn_digits )

printlog( "L201_GlobalTechRetirement_New: GLOBAL TECHNOLOGY RETIREMENT, NEW INVESTMENT" )
L201_GlobalTechRetirement_New <- L201_GlobalTechRetirement_Stock[ rep( 1:nrow( L201_GlobalTechRetirement_Stock ),
      times = length( GCAM_future_years ) ), ]
L201_GlobalTechRetirement_New$period <- sort( rep( GCAM_future_years, length.out = nrow( L201_GlobalTechRetirement_New ) ) )
L201_GlobalTechRetirement_New$half_life <- A_bld_tech_retirement$half_life_new[
      match( L201_GlobalTechRetirement_New$sector_name, A_bld_tech_retirement$supplysector ) ]
L201_GlobalTechRetirement_New$steepness <- round( A_bld_tech_retirement$steepness_new[
      match( L201_GlobalTechRetirement_New$sector_name, A_bld_tech_retirement$supplysector ) ], retirement_fn_digits )

printlog( "L201_GlobalTechBaseYearShareweight: BASE YEAR SHAREWEIGHTS FOR TECHS NOT IN THE CALIBRATED INPUT SET" )
L201_GlobalTechBaseYearShareweight <- A_bld_technology[
      paste( A_bld_technology$supplysector, A_bld_technology$subsector, A_bld_technology$technology ) %!in%
      paste( L201_CalInput_Bld$supplysector, L201_CalInput_Bld$subsector, L201_CalInput_Bld$stub_technology ),
      sup_sub_tech ]
L201_GlobalTechBaseYearShareweight$period <- "base periods"
L201_GlobalTechBaseYearShareweight$share.weight <- 0
names( L201_GlobalTechBaseYearShareweight )[ names( L201_GlobalTechBaseYearShareweight ) == "supplysector" ] <- "sector_name"
names( L201_GlobalTechBaseYearShareweight )[ names( L201_GlobalTechBaseYearShareweight ) == "subsector" ] <- "subsector_name"

printlog( "L201_GlobalTechFutureShareweight: FUTURE SHAREWEIGHTS FOR TECHS WITH SPECIFIED INTERPOLATION FUNCTIONS" )
L201_GlobalTechFutureShareweight <- data.frame(
      sector_name = A_bld_tech_interp$supplysector,
      subsector_name = A_bld_tech_interp$subsector,
      technology = A_bld_tech_interp$technology,
      period = "future periods",
      share.weight = A_bld_tech_interp$share.weight )

printlog( "L201_MarketName: MARKET NAMES FOR THE INPUT FUELS" )
L201_MarketName <- data.frame(
      region = sort( rep( states, times = nrow( A_bld_tech_eff_Y ) ) ),
      A_bld_tech_eff_Y[ sup_sub_tech_input ] )
L201_MarketName$market_name <- A_fuel_markets$market[
      match( paste( L201_MarketName$region, L201_MarketName$minicam_energy_input ),
             paste( A_fuel_markets$state, A_fuel_markets$minicam_energy_input ) ) ]

# ADVANCED SCENARIO TABLES
printlog( "L201_BldShell_adv: ADVANCED SCENARIO SHELL CONDUCTANCE" )
A_bld_shell_conductance_adv.melt <- melt( A_bld_shell_conductance_adv, id.vars = "gcam_consumer" )
A_bld_shell_conductance_adv.melt$year <- as.numeric( substr( A_bld_shell_conductance_adv.melt$variable, 2, 5 ) )

L201_BldShell_adv <- A_bld_demand[ rep( 1:nrow( A_bld_demand ), times = length( c( GCAM_model_period0, GCAM_years ) ) ), bld_node_names ]
L201_BldShell_adv <- L201_BldShell_adv[ order( L201_BldShell_adv$gcam_consumer ), ]
L201_BldShell_adv$shell_conductance_year <- rep( c( GCAM_model_period0, GCAM_years ), length.out = nrow( L201_BldShell_adv ) )
L201_BldShell_adv$shell_conductance <- A_bld_shell_conductance_adv.melt$value[
      match( paste( L201_BldShell_adv$gcam_consumer, L201_BldShell_adv$shell_conductance_year ),
             paste( A_bld_shell_conductance_adv.melt$gcam_consumer, A_bld_shell_conductance_adv.melt$year ) ) ]
L201_BldShell_adv$floor_to_surface_year <- L201_BldShell_adv$shell_conductance_year
L201_BldShell_adv$floor_to_surface_ratio <- floor_to_surface

printlog( "L201_GlobalTechEff_adv: ADVANCED SCENARIO GLOBAL TECHNOLOGY EFFICIENCIES" )
L201_GlobalTechEff_adv <- data.frame(
      sector_name = A_bld_tech_eff_Y_adv$supplysector,
      subsector_name = A_bld_tech_eff_Y_adv$subsector,
      technology = A_bld_tech_eff_Y_adv$technology,
      minicam_energy_input = A_bld_tech_eff_Y_adv$minicam_energy_input,
      A_bld_tech_eff_Y_adv[ X_GCAM_model_period0 ],
      round( A_bld_tech_eff_Y_adv[ X_GCAM_years ], CalInput_digits ) )
      
printlog( "L201_GlobalTechCost_adv: ADVANCED SCENARIO GLOBAL TECHNOLOGY NON-ENERGY COSTS" )
L201_GlobalTechCost_adv <- data.frame(
      sector_name = A_bld_tech_cost_Y_adv$supplysector,
      subsector_name = A_bld_tech_cost_Y_adv$subsector,
      technology = A_bld_tech_cost_Y_adv$technology,
      round( A_bld_tech_cost_Y_adv[ X_GCAM_years ], cost_digits ) )

# FROZEN TECHNOLOGY SCENARIO TABLES
printlog( "L201_BldShell_frz: FROZEN TECH SCENARIO SHELL CONDUCTANCE" )
A_bld_shell_conductance_frz <- A_bld_shell_conductance
A_bld_shell_conductance_frz[ X_GCAM_future_years ] <- A_bld_shell_conductance_frz[ X_final_cal_year ]
A_bld_shell_conductance_frz.melt <- melt( A_bld_shell_conductance_frz, id.vars = "gcam_consumer" )
A_bld_shell_conductance_frz.melt$year <- as.numeric( substr( A_bld_shell_conductance_frz.melt$variable, 2, 5 ) )

L201_BldShell_frz <- A_bld_demand[ rep( 1:nrow( A_bld_demand ), times = length( c( GCAM_model_period0, GCAM_years ) ) ), bld_node_names ]
L201_BldShell_frz <- L201_BldShell_frz[ order( L201_BldShell_frz$gcam_consumer ), ]
L201_BldShell_frz$shell_conductance_year <- rep( c( GCAM_model_period0, GCAM_years ), length.out = nrow( L201_BldShell_frz ) )
L201_BldShell_frz$shell_conductance <- A_bld_shell_conductance_frz.melt$value[
      match( paste( L201_BldShell_frz$gcam_consumer, L201_BldShell_frz$shell_conductance_year ),
             paste( A_bld_shell_conductance_frz.melt$gcam_consumer, A_bld_shell_conductance_frz.melt$year ) ) ]
L201_BldShell_frz$floor_to_surface_year <- L201_BldShell_frz$shell_conductance_year
L201_BldShell_frz$floor_to_surface_ratio <- floor_to_surface

printlog( "L201_GlobalTechEff_frz: FROZEN TECH SCENARIO GLOBAL TECHNOLOGY EFFICIENCIES" )
L201_GlobalTechEff_frz <- L201_GlobalTechEff
L201_GlobalTechEff_frz[ X_GCAM_future_years ] <- L201_GlobalTechEff_frz[ X_final_cal_year ]      

printlog( "L201_GlobalTechCost_adv: ADVANCED SCENARIO GLOBAL TECHNOLOGY NON-ENERGY COSTS" )
L201_GlobalTechCost_frz <- L201_GlobalTechCost
L201_GlobalTechCost_frz[ X_GCAM_future_years ] <- L201_GlobalTechCost_frz[ X_final_cal_year ]      


# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L201_BldDelete, "BldDelete", "L201_BldDelete", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_PopShare, "PopShare", "L201_PopShare", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_IncShare, "IncShare", "L201_IncShare", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_BldDemandFn1, "BldDemandFn1", "L201_BldDemandFn1", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_BldDemandFn2, "BldDemandFn2", "L201_BldDemandFn2", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_BldFlsp, "BldFlsp", "L201_BldFlsp", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_BldServSatn, "BldServSatn", "L201_BldServSatn", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_BldThermalServSatn, "BldThermalServSatn", "L201_BldThermalServSatn", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_DegreeDays, "DegreeDays", "L201_DegreeDays", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_BldServOut, "BldServOut", "L201_BldServOut", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_BldThermalServOut, "BldThermalServOut", "L201_BldThermalServOut", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_BldShell, "BldShell", "L201_BldShell", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_Supplysector, "Supplysector", "L201_Supplysector", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_Subsector, "Subsector", "L201_Subsector", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_SubsectorPhaseout, "SubsectorPhaseout", "L201_SubsectorPhaseout", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_GlobalTech, "GlobalTech", "L201_GlobalTech", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_GlobalTechEff, "GlobalTechEff", "L201_GlobalTechEff", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_GlobalTechCost, "GlobalTechCost", "L201_GlobalTechCost", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_GlobalTechIntgainsMkt, "GlobalTechIntgainsMkt", "L201_GlobalTechIntgainsMkt", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_GlobalTechIntgains, "GlobalTechIntgains", "L201_GlobalTechIntgains", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_GlobalTechRetirement_Stock, "GlobalTechRetirement", "L201_GlobalTechRetirement_Stock", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_GlobalTechRetirement_New, "GlobalTechRetirement", "L201_GlobalTechRetirement_New", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_GlobalTechBaseYearShareweight, "GlobalTechBaseYearShareweight", "L201_GlobalTechBaseYearShareweight", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_GlobalTechFutureShareweight, "GlobalTechFutureShareweight", "L201_GlobalTechFutureShareweight", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_MarketName, "MarketName", "L201_MarketName", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_CalInput_Bld, "CalInput", "L201_CalInput_Bld", "batch_rgcam_bld_input_base.xml" ) 
write_mi_data( L201_BldShell_adv, "BldShell", "L201_BldShell_adv", "batch_rgcam_bld_adv.xml" ) 
write_mi_data( L201_GlobalTechEff_adv, "GlobalTechEff", "L201_GlobalTechEff_adv", "batch_rgcam_bld_adv.xml" ) 
write_mi_data( L201_GlobalTechCost_adv, "GlobalTechCost", "L201_GlobalTechCost_adv", "batch_rgcam_bld_adv.xml" ) 
write_mi_data( L201_BldShell_frz, "BldShell", "L201_BldShell_frz", "batch_rgcam_bld_frz.xml" ) 
write_mi_data( L201_GlobalTechEff_frz, "GlobalTechEff", "L201_GlobalTechEff_frz", "batch_rgcam_bld_frz.xml" ) 
write_mi_data( L201_GlobalTechCost_frz, "GlobalTechCost", "L201_GlobalTechCost_frz", "batch_rgcam_bld_frz.xml" ) 
write_mi_data( L201_GlobalTechEff_frz, "GlobalTechEff", "L201_GlobalTechEff_frz", "batch_rgcam_bld_frz.xml" ) 
write_mi_data( L201_GlobalTechCost_frz, "GlobalTechCost", "L201_GlobalTechCost_frz", "batch_rgcam_bld_frz.xml" ) 

insert_file_into_batchxml( "batch_rgcam_bld_input_base.xml", "rgcam_bld_input_base.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "batch_rgcam_bld_adv.xml", "rgcam_bld_adv.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "batch_rgcam_bld_frz.xml", "rgcam_bld_frz.xml", "", xml_tag="outFile" )

logstop()
