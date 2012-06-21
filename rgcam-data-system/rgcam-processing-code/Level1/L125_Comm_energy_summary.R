# L125_Res_energy_summary.R
# COMMERCIAL SECTOR ENERGY CONSUMPTION BY STATE, FUEL, SERVICE, AND BASE YEAR
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L125_Comm_energy_summary.R" )
printlog( "Commercial sector energy consumption by state / fuel / service" )
                  
# -----------------------------------------------------------------------------
# 1. Read files

L104_in_EJ_state_bld_F <- readdata( "L104_in_EJ_state_bld_F" )
L122_in_EJ_state_comm_elec_U <- readdata( "L122_in_EJ_state_comm_elec_U" )
L123_in_EJ_state_comm_gas_U <- readdata( "L123_in_EJ_state_comm_gas_U" )
L124_in_EJ_state_comm_oil_U <- readdata( "L124_in_EJ_state_comm_oil_U" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. ASSIGNMENT OF COMMERCIAL BIOMASS AND COAL TO SERVICES
#Assign all biomass in the commercial sector to space heating
L125_in_EJ_state_comm_bio <- L104_in_EJ_state_bld_F[ L104_in_EJ_state_bld_F$GCAM_sector == "comm" &
      L104_in_EJ_state_bld_F$GCAM_fuel == "biomass", ]
L125_in_EJ_state_comm_bio_U <- data.frame( L125_in_EJ_state_comm_bio[ state_S_F ], service = "comm heating",
      L125_in_EJ_state_comm_bio[ X_base_years ] )

#Assign all coal in the commercial sector to space heating
L125_in_EJ_state_comm_coal <- L104_in_EJ_state_bld_F[ L104_in_EJ_state_bld_F$GCAM_sector == "comm" &
      L104_in_EJ_state_bld_F$GCAM_fuel == "coal", ]
L125_in_EJ_state_comm_coal_U <- data.frame( L125_in_EJ_state_comm_coal[ state_S_F ], service = "comm heating",
      L125_in_EJ_state_comm_coal[ X_base_years ] )

# 2b. COMBINE ALL TABLES FROM PREVIOUS FILES
#Rbind all tables of commercial energy consumption, and sort
L125_in_EJ_state_comm_F_U <- rbind( L122_in_EJ_state_comm_elec_U, L123_in_EJ_state_comm_gas_U,
      L124_in_EJ_state_comm_oil_U, L125_in_EJ_state_comm_bio_U, L125_in_EJ_state_comm_coal_U )
L125_in_EJ_state_comm_F_U <- L125_in_EJ_state_comm_F_U[ order( L125_in_EJ_state_comm_F_U$state, L125_in_EJ_state_comm_F_U$service ), ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L125_in_EJ_state_comm_F_U <- c( "Commercial building energy consumption by state / fuel / service","Unit = EJ" )

#write tables as CSV files
writedata( L125_in_EJ_state_comm_F_U,fn="L125_in_EJ_state_comm_F_U", comments=comments.L125_in_EJ_state_comm_F_U )

# Every script should finish with this line
logstop()