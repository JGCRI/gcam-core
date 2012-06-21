# L115_Res_energy_summary.R
# RESIDENTIAL SECTOR ENERGY CONSUMPTION BY STATE, FUEL, SERVICE, AND BASE YEAR
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L115_Res_energy_summary.R" )
printlog( "Residential sector energy consumption by state / fuel / service" )
                  
# -----------------------------------------------------------------------------
# 1. Read files

L104_in_EJ_state_bld_F <- readdata( "L104_in_EJ_state_bld_F" )
L112_in_EJ_state_res_elec_U <- readdata( "L112_in_EJ_state_res_elec_U" )
L113_in_EJ_state_res_gas_U <- readdata( "L113_in_EJ_state_res_gas_U" )
L114_in_EJ_state_res_oil_U <- readdata( "L114_in_EJ_state_res_oil_U" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. ASSIGNMENT OF RESIDENTIAL BIOMASS AND COAL TO SERVICES
#Assign all biomass in the residential sector to space heating
L115_in_EJ_state_res_bio <- L104_in_EJ_state_bld_F[ L104_in_EJ_state_bld_F$GCAM_sector == "resid" &
      L104_in_EJ_state_bld_F$GCAM_fuel == "biomass", ]
L115_in_EJ_state_res_bio_U <- data.frame( L115_in_EJ_state_res_bio[ state_S_F ], service = "resid heating",
      L115_in_EJ_state_res_bio[ X_base_years ] )

#Assign all coal in the residential sector to space heating
L115_in_EJ_state_res_coal <- L104_in_EJ_state_bld_F[ L104_in_EJ_state_bld_F$GCAM_sector == "resid" &
      L104_in_EJ_state_bld_F$GCAM_fuel == "coal", ]
L115_in_EJ_state_res_coal_U <- data.frame( L115_in_EJ_state_res_coal[ state_S_F ], service = "resid heating",
      L115_in_EJ_state_res_coal[ X_base_years ] )

# 2b. COMBINE ALL TABLES FROM PREVIOUS FILES
#Rbind all tables of residential energy consumption, and sort
L115_in_EJ_state_res_F_U <- rbind( L112_in_EJ_state_res_elec_U, L113_in_EJ_state_res_gas_U,
      L114_in_EJ_state_res_oil_U, L115_in_EJ_state_res_bio_U, L115_in_EJ_state_res_coal_U )
L115_in_EJ_state_res_F_U <- L115_in_EJ_state_res_F_U[ order( L115_in_EJ_state_res_F_U$state, L115_in_EJ_state_res_F_U$service ), ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L115_in_EJ_state_res_F_U <- c( "Residential building energy consumption by state / fuel / service","Unit = EJ" )

#write tables as CSV files
writedata( L115_in_EJ_state_res_F_U,fn="L115_in_EJ_state_res_F_U", comments=comments.L115_in_EJ_state_res_F_U )

# Every script should finish with this line
logstop()