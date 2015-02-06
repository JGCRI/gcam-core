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
logstart( "LA1322.Fert.R" )
printlog( "Fertilizer production and IO coefficients by state" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
Census_ind_VoS_state <- readdata( "GCAMUSA_LEVEL0_DATA", "Census_ind_VoS_state" )
L1322.Fert_Prod_MtN_R_F_Y <- readdata( "ENERGY_LEVEL1_DATA", "L1322.Fert_Prod_MtN_R_F_Y" )
L1322.IO_R_Fert_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1322.IO_R_Fert_F_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Assigning national fertilizer production to states on the basis of value of shipments of NAICS 3273 by state" )
# Note: The NAICS code 3253 includes fertilizers (nitrogenous, phosphatic, and mixed), pesticides, and other agricultural chemicals
L1322.VoS_share_state_Fert <- subset( Census_ind_VoS_state, NAICS_code == 3253)
L1322.VoS_share_state_Fert[ X_historical_years ] <- L1322.VoS_share_state_Fert$VoS_thousUSD / sum( L1322.VoS_share_state_Fert$VoS_thousUSD )
L1322.VoS_share_state_Fert$sector <- Fert_name

#Note that the only relevant fuel in the USA is gas
L1322.VoS_share_state_Fert$fuel <- "gas"
L1322.out_Mt_state_Fert_Yh <- apportion_to_states(
      nation_data = subset( L1322.Fert_Prod_MtN_R_F_Y, GCAM_region_ID == USA_regID ),
      state_share_data = L1322.VoS_share_state_Fert[ c( state_S_F, X_historical_years ) ],
      match_vectors = S_F )

printlog( "Assuming all states have the same IO coefficients" )
L1322.IO_GJkg_state_Fert_F_Yh <- repeat_and_add_vector(
      subset( L1322.IO_R_Fert_F_Yh, GCAM_region_ID == USA_regID & fuel == "gas" ),
      state, unique( L1322.out_Mt_state_Fert_Yh$state ) )[
       c( state_S_F, X_historical_years ) ]

L1322.in_EJ_state_Fert_Yh <- L1322.IO_GJkg_state_Fert_F_Yh
L1322.in_EJ_state_Fert_Yh[ X_historical_years ] <- L1322.IO_GJkg_state_Fert_F_Yh[ X_historical_years ] * L1322.out_Mt_state_Fert_Yh[
      match( L1322.in_EJ_state_Fert_Yh$state, L1322.out_Mt_state_Fert_Yh$state ),
      X_historical_years ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L1322.out_Mt_state_Fert_Yh <- c( "Fert production by state / historical year","Unit = Mt" )
comments.L1322.IO_GJkg_state_Fert_F_Yh <- c( "Input-output coefficients of Fert production by state / input / historical year","Unit = GJ/kg and kg/kg" )
comments.L1322.in_EJ_state_Fert_Yh <- c( "Energy inputs to Fertilizer production by state / input / historical year","Unit = GJ/kg and kg/kg" )

#write tables as CSV files
writedata( L1322.out_Mt_state_Fert_Yh, domain="GCAMUSA_LEVEL1_DATA", fn="L1322.out_Mt_state_Fert_Yh", comments=comments.L1322.out_Mt_state_Fert_Yh )
writedata( L1322.IO_GJkg_state_Fert_F_Yh, domain="GCAMUSA_LEVEL1_DATA", fn="L1322.IO_GJkg_state_Fert_F_Yh", comments=comments.L1322.IO_GJkg_state_Fert_F_Yh )
writedata( L1322.in_EJ_state_Fert_Yh, domain="GCAMUSA_LEVEL1_DATA", fn="L1322.in_EJ_state_Fert_Yh", comments=comments.L1322.in_EJ_state_Fert_Yh )

# Every script should finish with this line
logstop()

