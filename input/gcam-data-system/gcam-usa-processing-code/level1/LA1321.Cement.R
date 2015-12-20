# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "GCAMUSAPROC_DIR" ) ){
    if( Sys.getenv( "GCAMUSAPROC" ) != "" ){
        GCAMUSAPROC_DIR <- Sys.getenv( "GCAMUSAPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var GCAMUSAPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
logstart( "LA1321.Cement.R" )
printlog( "Cement production, IO coefficients, and energy consumption by state and fuel" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
Census_ind_VoS_state <- readdata( "GCAMUSA_LEVEL0_DATA", "Census_ind_VoS_state" )
L1321.out_Mt_R_cement_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1321.out_Mt_R_cement_Yh" )
L1321.IO_GJkg_R_cement_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1321.IO_GJkg_R_cement_F_Yh" )
L1321.in_EJ_R_cement_F_Y <- readdata( "ENERGY_LEVEL1_DATA", "L1321.in_EJ_R_cement_F_Y" )


# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Assigning national cement production to states on the basis of value of shipments of NAICS 3273 by state" )
# Note: The NAICS code 3273 includes cement and concrete product manufacturing. Includes cement, ready-mix concrete,
# concrete pipe, brick, and block, and other concrete products
L1321.VoS_share_state_cement <- subset( Census_ind_VoS_state, NAICS_code == 3273)
L1321.VoS_share_state_cement[ X_historical_years ] <- L1321.VoS_share_state_cement$VoS_thousUSD / sum( L1321.VoS_share_state_cement$VoS_thousUSD )
L1321.VoS_share_state_cement$sector <- "cement"
L1321.out_Mt_state_cement_Yh <- apportion_to_states(
      nation_data = subset( L1321.out_Mt_R_cement_Yh, GCAM_region_ID == USA_regID ),
      state_share_data = L1321.VoS_share_state_cement[ c( state_S, X_historical_years ) ] )

printlog( "Assuming all states have the same IO coefficients for heat, electricity, and limestone" )
L1321.IO_GJkg_state_cement_F_Yh <- repeat_and_add_vector( subset( L1321.IO_GJkg_R_cement_F_Yh, GCAM_region_ID == USA_regID ),
      state, unique( L1321.out_Mt_state_cement_Yh$state ) )[ c( state_S_F, X_historical_years ) ]

printlog( "Calculating inputs to cement production by state")
printlog( "NOTE: assuming the same fuel blend in all states")
L1321.VoS_share_state_cement_repF <- repeat_and_add_vector( L1321.VoS_share_state_cement, "fuel", unique( L1321.in_EJ_R_cement_F_Y$fuel ) )
L1321.in_EJ_state_cement_F_Y <- apportion_to_states(
      nation_data = subset( L1321.in_EJ_R_cement_F_Y, GCAM_region_ID == USA_regID ),
      state_share_data = L1321.VoS_share_state_cement_repF[ c( state_S_F, X_historical_years ) ],
      match_vectors = S_F )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L1321.out_Mt_state_cement_Yh <- c( "Cement production by state / historical year","Unit = Mt" )
comments.L1321.IO_GJkg_state_cement_F_Yh <- c( "Input-output coefficients of cement production by state / input / historical year","Unit = GJ/kg and kg/kg" )
comments.L1321.in_EJ_state_cement_F_Y <- c( "Energy inputs to cement production by state / fuel / historical year","Unit = EJ/yr" )

#write tables as CSV files
writedata( L1321.out_Mt_state_cement_Yh, domain="GCAMUSA_LEVEL1_DATA", fn="L1321.out_Mt_state_cement_Yh", comments=comments.L1321.out_Mt_state_cement_Yh )
writedata( L1321.IO_GJkg_state_cement_F_Yh, domain="GCAMUSA_LEVEL1_DATA", fn="L1321.IO_GJkg_state_cement_F_Yh", comments=comments.L1321.IO_GJkg_state_cement_F_Yh )
writedata( L1321.in_EJ_state_cement_F_Y, domain="GCAMUSA_LEVEL1_DATA", fn="L1321.in_EJ_state_cement_F_Y", comments=comments.L1321.in_EJ_state_cement_F_Y )

# Every script should finish with this line
logstop()

