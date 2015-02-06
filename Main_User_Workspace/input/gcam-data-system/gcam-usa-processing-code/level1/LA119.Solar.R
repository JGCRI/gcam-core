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
logstart( "LA119.Solar.R" )
printlog( "Solar capacity factors by state" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
NREL_us_re_capacity_factors <- readdata( "GCAMUSA_LEVEL0_DATA", "NREL_us_re_capacity_factors" )
NREL_us_re_technical_potential <- readdata( "GCAMUSA_LEVEL0_DATA", "NREL_us_re_technical_potential" )

# -----------------------------------------------------------------------------

# 2. Perform computations
NREL_us_re_capacity_factors$PV_scaler <- NREL_us_re_capacity_factors$Urban_Utility_scale_PV /
      NREL_us_re_capacity_factors$Urban_Utility_scale_PV[ NREL_us_re_capacity_factors$State == "Average" ]
NREL_us_re_capacity_factors$CSP_scaler <- NREL_us_re_capacity_factors$CSP /
      NREL_us_re_capacity_factors$CSP[ NREL_us_re_capacity_factors$State == "Average" ]
NREL_us_re_capacity_factors$state <- states_subregions$state[ match( NREL_us_re_capacity_factors$State, states_subregions$state_name ) ]
L119.CapFacScaler_PV_state <- data.frame(
      state = states,
      sector = "electricity generation",
      fuel = "solar PV",
      scaler = NREL_us_re_capacity_factors$PV_scaler[ match( states, NREL_us_re_capacity_factors$state ) ] )
L119.CapFacScaler_CSP_state <- data.frame(
      state = states,
      sector = "electricity generation",
      fuel = "solar CSP",
      scaler = NREL_us_re_capacity_factors$CSP_scaler[ match( states, NREL_us_re_capacity_factors$state ) ] )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L119.CapFacScaler_PV_state <- c( "Solar PV capacity factor adjustment by state","Unitless" )
comments.L119.CapFacScaler_CSP_state <- c( "Solar PV capacity factor adjustment by state","Unitless" )

#write tables as CSV files
writedata( L119.CapFacScaler_PV_state, domain="GCAMUSA_LEVEL1_DATA", fn="L119.CapFacScaler_PV_state", comments=comments.L119.CapFacScaler_PV_state )
writedata( L119.CapFacScaler_CSP_state, domain="GCAMUSA_LEVEL1_DATA", fn="L119.CapFacScaler_CSP_state", comments=comments.L119.CapFacScaler_CSP_state )

# Every script should finish with this line
logstop()
