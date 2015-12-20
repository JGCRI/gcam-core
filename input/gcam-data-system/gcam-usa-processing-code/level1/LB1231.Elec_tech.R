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
logstart( "LB1231.Elec_tech.R" )

printlog( "Electricity sector inputs and outputs by state, fuel, and technology" )

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
L123.in_EJ_R_elec_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L123.in_EJ_R_elec_F_Yh" )
L123.out_EJ_R_elec_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L123.out_EJ_R_elec_F_Yh" )
L1231.in_EJ_R_elec_F_tech_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1231.in_EJ_R_elec_F_tech_Yh" )
L1231.out_EJ_R_elec_F_tech_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1231.out_EJ_R_elec_F_tech_Yh" )
L123.in_EJ_state_elec_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L123.in_EJ_state_elec_F" )
L123.out_EJ_state_elec_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L123.out_EJ_state_elec_F" )

# -----------------------------------------------------------------------------

# 2. Perform computations
# Downscaling of electricity by fuel to fuel and technology
printlog( "Computing nation-level shares of technology within fuel")
L1231.share_elec_F_tech <- subset( L1231.out_EJ_R_elec_F_tech_Yh, GCAM_region_ID == USA_regID )
L1231.share_elec_F_tech[ X_historical_years ] <- L1231.share_elec_F_tech[ X_historical_years ] / L123.out_EJ_R_elec_F_Yh[
      match( vecpaste( L1231.share_elec_F_tech[ c( "GCAM_region_ID", "fuel" ) ] ),
             vecpaste( L123.out_EJ_R_elec_F_Yh[ c( "GCAM_region_ID", "fuel" ) ] ) ),
      X_historical_years ]
L1231.share_elec_F_tech[ is.na( L1231.share_elec_F_tech ) ] <- 0

#Repeat by states. Also create a second data frame with only the fuels that use "inputs"
L1231.share_elec_F_tech_repstate <- repeat_and_add_vector( L1231.share_elec_F_tech, state, states )
L1231.share_elec_Fin_tech_repstate <- subset( L1231.share_elec_F_tech_repstate, fuel %in% L1231.in_EJ_R_elec_F_tech_Yh$fuel )

#Multiply the tech shares by the input and output by state and fuel
L1231.in_EJ_state_elec_F_tech <- L1231.share_elec_Fin_tech_repstate[ c( state_S_F, "technology", X_historical_years ) ]
L1231.in_EJ_state_elec_F_tech[ X_historical_years ] <- L1231.share_elec_Fin_tech_repstate[ X_historical_years ] * L123.in_EJ_state_elec_F[
      match( vecpaste( L1231.share_elec_Fin_tech_repstate[ state_S_F ] ),
             vecpaste( L123.in_EJ_state_elec_F[ state_S_F ] ) ),
      X_historical_years ]
L1231.out_EJ_state_elec_F_tech <- L1231.share_elec_F_tech_repstate[ c( state_S_F, "technology", X_historical_years ) ]
L1231.out_EJ_state_elec_F_tech[ X_historical_years ] <- L1231.share_elec_F_tech_repstate[ X_historical_years ] * L123.out_EJ_state_elec_F[
      match( vecpaste( L1231.share_elec_F_tech_repstate[ state_S_F ] ),
             vecpaste( L123.out_EJ_state_elec_F[ state_S_F ] ) ),
      X_historical_years ]
# -----------------------------------------------------------------------------

# 3. Output

#Add comments for each table
comments.L1231.in_EJ_state_elec_F_tech <- c( "Electricity sector energy consumption by state / fuel / technology","Unit = EJ" )
comments.L1231.out_EJ_state_elec_F_tech <- c( "Electricity generation by state / fuel / technology","Unit = EJ" )

#write tables as CSV files
writedata( L1231.in_EJ_state_elec_F_tech, domain="GCAMUSA_LEVEL1_DATA", fn="L1231.in_EJ_state_elec_F_tech", comments=comments.L1231.in_EJ_state_elec_F_tech )
writedata( L1231.out_EJ_state_elec_F_tech, domain="GCAMUSA_LEVEL1_DATA", fn="L1231.out_EJ_state_elec_F_tech", comments=comments.L1231.out_EJ_state_elec_F_tech )

# Every script should finish with this line
logstop()