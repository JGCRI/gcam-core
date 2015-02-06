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
logstart( "LB1232.Elec_subregions.R" )
printlog( "Electricity sector inputs and outputs by state, fuel, and technology" )
# -----------------------------------------------------------------------------

# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
L1231.in_EJ_state_elec_F_tech <- readdata( "GCAMUSA_LEVEL1_DATA", "L1231.in_EJ_state_elec_F_tech" )
L1231.out_EJ_state_elec_F_tech <- readdata( "GCAMUSA_LEVEL1_DATA", "L1231.out_EJ_state_elec_F_tech" )
# -----------------------------------------------------------------------------

# 2. Perform computations
# Downscaling of electricity by fuel to fuel and technology
printlog( "Aggregating states to electricity subregions")
L1231.out_EJ_state_elec_F_tech$grid_region <- states_subregions$grid_region[
      match( L1231.out_EJ_state_elec_F_tech$state, states_subregions$state ) ]
L1232.out_EJ_sR_elec <- aggregate( L1231.out_EJ_state_elec_F_tech[ X_historical_years ],
      by=as.list( L1231.out_EJ_state_elec_F_tech[ c( "grid_region", "sector" ) ] ), sum )

# -----------------------------------------------------------------------------

# 3. Output

#Add comments for each table

comments.L1232.out_EJ_sR_elec <- c( "Electricity generation by FERC region / fuel / technology","Unit = EJ" )

#write tables as CSV files
writedata( L1232.out_EJ_sR_elec, domain="GCAMUSA_LEVEL1_DATA", fn="L1232.out_EJ_sR_elec", comments=comments.L1232.out_EJ_sR_elec )

# Every script should finish with this line
logstop()