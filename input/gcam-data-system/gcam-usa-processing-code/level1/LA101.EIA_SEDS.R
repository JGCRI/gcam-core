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
logstart( "LA101.EIA_SEDS.R" )
printlog( "EIA State Energy Data Systems data aggregated by GCAM fuel and sector" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
EIA_SEDS_fuels <- readdata( "GCAMUSA_MAPPINGS", "EIA_SEDS_fuels" )
EIA_SEDS_sectors <- readdata( "GCAMUSA_MAPPINGS", "EIA_SEDS_sectors" )
EIA_use_all_Bbtu <- readdata( "GCAMUSA_LEVEL0_DATA", "EIA_use_all_Bbtu" )
A_fuel_conv <- readdata( "GCAMUSA_ASSUMPTIONS", "A_fuel_conv" )

# -----------------------------------------------------------------------------
# 2. Perform computations

#Add columns for EIA fuels and sectors, using the substrings of the MSN code
EIA_use_all_Bbtu$EIA_fuel <- substr( EIA_use_all_Bbtu$MSN, 1, 2 )
EIA_use_all_Bbtu$EIA_sector <- substr( EIA_use_all_Bbtu$MSN, 3, 4 )

printlog( "Removing the whole USA category, and matching in intermediate fuel and sector names" )
L101.EIA_use_all_Bbtu <- EIA_use_all_Bbtu[ EIA_use_all_Bbtu$State %in% states, ]
names( L101.EIA_use_all_Bbtu ) <- sub( "State", "state", names( L101.EIA_use_all_Bbtu ) )
L101.EIA_use_all_Bbtu$sector <- EIA_SEDS_sectors$GCAM_sector[ match( L101.EIA_use_all_Bbtu$EIA_sector, EIA_SEDS_sectors$EIA_sector ) ]
L101.EIA_use_all_Bbtu$fuel <- EIA_SEDS_fuels$GCAM_fuel[ match( L101.EIA_use_all_Bbtu$EIA_fuel, EIA_SEDS_fuels$EIA_fuel ) ]

#Collapse dataset by sector and fuel. Subset specified years
printlog( "Aggregating EIA state energy data by GCAM sector and fuel" )
L101.inEIA_Bbtu_state_S_F <- aggregate( L101.EIA_use_all_Bbtu[ X_historical_years ],
      by=as.list( L101.EIA_use_all_Bbtu[ state_S_F ] ), sum )
      
#Convert to EJ with fuel-specific multipliers
printlog( "Converting from Bbtu to EJ" )
L101.inEIA_Bbtu_state_S_F$conv <- A_fuel_conv$conv_Bbtu_EJ[ match( L101.inEIA_Bbtu_state_S_F$fuel, A_fuel_conv$fuel ) ]
L101.inEIA_EJ_state_S_F <- data.frame( L101.inEIA_Bbtu_state_S_F[ state_S_F ],
                           L101.inEIA_Bbtu_state_S_F[ X_historical_years ] * L101.inEIA_Bbtu_state_S_F$conv )

#Getting rid of missing values: prior to 1980, lots are missing. Because these data are only used for state-wise allocations
if( any( historical_years < 1980 ) ){
	for( i in length( historical_years[ historical_years < 1980 ] ) : 1 ){
		L101.EIA_use_all_Bbtu[ is.na( L101.EIA_use_all_Bbtu[[ X_historical_years[i] ]] ), X_historical_years[i] ] <-
		L101.EIA_use_all_Bbtu[ is.na( L101.EIA_use_all_Bbtu[[ X_historical_years[i] ]] ), X_historical_years[i+1] ]
	}
}


# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L101.EIA_use_all_Bbtu <- c( "EIA state energy database by EIA sector / fuel / year","Unit = EJ" )
comments.L101.inEIA_EJ_state_S_F <- c( "EIA state energy database by GCAM sector / fuel / year","Unit = EJ" )

#write tables as CSV files
writedata( L101.EIA_use_all_Bbtu, domain="GCAMUSA_LEVEL1_DATA", fn="L101.EIA_use_all_Bbtu", comments=comments.L101.EIA_use_all_Bbtu )
writedata( L101.inEIA_EJ_state_S_F, domain="GCAMUSA_LEVEL1_DATA", fn="L101.inEIA_EJ_state_S_F", comments=comments.L101.inEIA_EJ_state_S_F )

# Every script should finish with this line
logstop()