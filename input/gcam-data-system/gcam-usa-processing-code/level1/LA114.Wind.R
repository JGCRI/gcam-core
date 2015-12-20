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
logstart( "LA114.Wind.R" )
printlog( "Wind resources by state" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_elec_data", extension = ".R" )
us_state_wind <- readdata( "GCAMUSA_GIS_DATA", "us_state_wind" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
A23.globaltech_capital <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_capital" )
A23.globaltech_OMfixed <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_OMfixed" )
A23.globaltech_OMvar <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_OMvar" )

# -----------------------------------------------------------------------------

# 2. Perform computations
printlog( "Interpolating the cost tables as necessary to get the costs in the assumed wind base cost year" )
X_wind_base_cost_year <- paste0( "X", wind_base_cost_year )
A23.globaltech_capital <- gcam_interp( A23.globaltech_capital, wind_base_cost_year )
A23.globaltech_OMfixed <- gcam_interp( A23.globaltech_OMfixed, wind_base_cost_year )
A23.globaltech_OMvar <- gcam_interp( A23.globaltech_OMvar, wind_base_cost_year )

#Extract the costs. These are in 1975$
L114.CapCost <- A23.globaltech_capital[[ X_wind_base_cost_year ]][ A23.globaltech_capital$technology == "wind" ]
L114.FixedChargeRate <- A23.globaltech_capital[[ "fixed.charge.rate" ]][ A23.globaltech_capital$technology == "wind" ]
L114.OMFixedCost <- A23.globaltech_OMfixed[[ X_wind_base_cost_year ]][ A23.globaltech_OMfixed$technology == "wind" ]
L114.OMVarCost <- A23.globaltech_OMvar[[ X_wind_base_cost_year ]][ A23.globaltech_OMvar$technology == "wind" ]

#Convert the 2007$/kwh to 1975$/GJ
us_state_wind$base_cost_75USDGJ <- us_state_wind$base_cost * conv_2007_1975_USD / conv_kwh_GJ

#Calculate the capacity factor for the base wind turbine in each state
us_state_wind$capacity_factor <- ( L114.CapCost * L114.FixedChargeRate + L114.OMFixedCost ) /
      ( conv_kwh_GJ * conv_year_hours ) / ( us_state_wind$base_cost_75USDGJ - ( L114.OMVarCost / ( 1000 * conv_kwh_GJ ) ) )

L114.CapacityFactor_wind_state <- data.frame(
      state = us_state_wind$region,
      sector = "electricity generation",
      fuel = "wind",
      capacity.factor = us_state_wind$capacity_factor )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L114.CapacityFactor_wind_state <- c( "Cost adjustment (adder) by state for wind","Unit = 1975$/GJ" )

#write tables as CSV files
writedata( L114.CapacityFactor_wind_state, domain="GCAMUSA_LEVEL1_DATA", fn="L114.CapacityFactor_wind_state", comments=comments.L114.CapacityFactor_wind_state )

# Every script should finish with this line
logstop()

