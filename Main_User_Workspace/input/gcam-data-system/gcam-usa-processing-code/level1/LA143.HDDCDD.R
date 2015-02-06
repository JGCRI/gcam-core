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
logstart( "LA143.HDDCDD.R" )
printlog( "State-level heating and cooling degree days as compared with surrounding subregion9 and subregion13" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
Census_pop_hist <- readdata( "GCAMUSA_LEVEL0_DATA", "Census_pop_hist" )
CDD_His <- readdata( "GCAMUSA_GIS_DATA", "CDD_His" )
HDD_His <- readdata( "GCAMUSA_GIS_DATA", "HDD_His" )

# 1b. reading HDDCDD files in a loop as we do not know their names or how many there are
GISfilepath <- readdomainpathmap()["GCAMUSA_GIS_DATA"][[1]]
DD_scenario_files <- list.files( GISfilepath )[ grepl( "DD", list.files( GISfilepath ) ) & !grepl( "_His", list.files( GISfilepath ) ) ]
DD_scenario_files <- sub( ".csv", "", DD_scenario_files )
DD_scenario_files.list <- list()
for( i in DD_scenario_files){
  index <- which( DD_scenario_files == i )
  DD_scenario_files.list[[index]] <- readdata( "GCAMUSA_GIS_DATA", i )
}
names(DD_scenario_files.list) <- DD_scenario_files
HDDCDD_data <- do.call( rbind, DD_scenario_files.list )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Generate estimates of heating and cooling degree days by subregion13 (for RECS) and subregion9 (for CBECS)
# 2a. add lookup vectors to CDD, HDD, and population tables
CDD_His$subregion9 <- states_subregions$subregion9[ match( CDD_His$state, states_subregions$state ) ]
CDD_His$subregion13 <- states_subregions$subregion13[ match( CDD_His$state, states_subregions$state ) ]
HDD_His$subregion9 <- states_subregions$subregion9[ match( HDD_His$state, states_subregions$state ) ]
HDD_His$subregion13 <- states_subregions$subregion13[ match( HDD_His$state, states_subregions$state ) ]
Census_pop_hist$subregion9 <- states_subregions$subregion9[ match( Census_pop_hist$state, states_subregions$state ) ]
Census_pop_hist$subregion13 <- states_subregions$subregion13[ match( Census_pop_hist$state, states_subregions$state ) ]

# 2b. order the states in all data frames to ensure one-to-one correspondence. Also fill out any missing historical years
L143.CDD_state <- gcam_interp( CDD_His[ order( CDD_His$state ), ], historical_years, rule = 2 )
L143.HDD_state <- gcam_interp( HDD_His[ order( HDD_His$state ), ], historical_years, rule = 2 )
Census_pop_hist <- Census_pop_hist[ order( Census_pop_hist$state ), ]

# 2c. multiply HDD and CDD by population in new tables
printlog( "Multiplying state-level population by HDD and CDD" )
L143.Pop_CDD_state <- data.frame(
      L143.CDD_state[ c( "state", "subregion9", "subregion13" ) ] ,
      L143.CDD_state[ X_historical_years ] * Census_pop_hist[ X_historical_years ] )

L143.Pop_HDD_state <- data.frame(
      L143.HDD_state[ c( "state", "subregion9", "subregion13" ) ] ,
      L143.HDD_state[ X_historical_years ] * Census_pop_hist[ X_historical_years ] )

# 2d. aggregate population and pop_HDD/pop_CDD by subregion9 and subregion13

printlog( "Aggregating into subregions to calculate weighted average HDD and CDD" )

L143.Pop_CDD_sR9 <- aggregate( L143.Pop_CDD_state[ X_historical_years ], by=as.list( L143.Pop_CDD_state[ "subregion9" ] ), sum )
L143.Pop_CDD_sR13 <- aggregate( L143.Pop_CDD_state[ X_historical_years ], by=as.list( L143.Pop_CDD_state[ "subregion13" ] ), sum )

L143.Pop_HDD_sR9 <- aggregate( L143.Pop_HDD_state[ X_historical_years ], by=as.list( L143.Pop_HDD_state[ "subregion9" ] ), sum )
L143.Pop_HDD_sR13 <- aggregate( L143.Pop_HDD_state[ X_historical_years ], by=as.list( L143.Pop_HDD_state[ "subregion13" ] ), sum )

# 2e. calculate each state's share of its subregion's population x HDD and CDD
L143.share_state_Pop_CDD_sR9 <- data.frame( L143.Pop_CDD_state[ c( state, "subregion9" ) ], variable = "CDD",
      L143.Pop_CDD_state[ X_historical_years ] / L143.Pop_CDD_sR9[
         match( L143.Pop_CDD_state$subregion9, L143.Pop_CDD_sR9$subregion9 ),
         X_historical_years ] )
L143.share_state_Pop_CDD_sR13 <- data.frame( L143.Pop_CDD_state[ c( state, "subregion13" ) ], variable = "CDD",
      L143.Pop_CDD_state[ X_historical_years ] / L143.Pop_CDD_sR13[
         match( L143.Pop_CDD_state$subregion13, L143.Pop_CDD_sR13$subregion13 ),
         X_historical_years ] )
L143.share_state_Pop_HDD_sR9 <- data.frame( L143.Pop_HDD_state[ c( state, "subregion9" ) ], variable = "HDD",
      L143.Pop_HDD_state[ X_historical_years ] / L143.Pop_HDD_sR9[
         match( L143.Pop_HDD_state$subregion9, L143.Pop_HDD_sR9$subregion9 ),
         X_historical_years ] )
L143.share_state_Pop_HDD_sR13 <- data.frame( L143.Pop_HDD_state[ c( state, "subregion13" ) ], variable = "HDD",
      L143.Pop_HDD_state[ X_historical_years ] / L143.Pop_HDD_sR13[
         match( L143.Pop_HDD_state$subregion13, L143.Pop_HDD_sR13$subregion13 ),
         X_historical_years ] )

# 2b. Processing of future scenarios
L143.HDDCDD_scen_state <- gcam_interp( HDDCDD_data, c( final_historical_year, future_years ), rule = 2 )
L143.HDDCDD_scen_state[ c( "variable", "GCM", "Scen" ) ] <- str_split_fixed( row.names( L143.HDDCDD_scen_state ), "_", 3 )
L143.HDDCDD_scen_state$Scen <- substr( L143.HDDCDD_scen_state$Scen, 1, regexpr( ".", L143.HDDCDD_scen_state$Scen, fixed = T ) - 1 )
row.names( L143.HDDCDD_scen_state ) <- 1:nrow( L143.HDDCDD_scen_state )
#Set aside the scenario's final historical year because this will be used in DD normalization later
L143.HDDCDD_scen_state$Scen_final_historical_year <- L143.HDDCDD_scen_state[[ X_final_historical_year ]]

#Interpolate and extrapolate the historical years from the historical data
CDD_His <- gcam_interp( CDD_His, historical_years, rule = 2 )
CDD_His$variable <- "CDD"
HDD_His <- gcam_interp( HDD_His, historical_years, rule = 2 )
HDD_His$variable <- "HDD"
DD_His <- rbind( CDD_His, HDD_His )

L143.HDDCDD_scen_state[ X_historical_years ] <- DD_His[
      match( vecpaste( L143.HDDCDD_scen_state[ c( "state", "variable" ) ] ),
             vecpaste( DD_His[ c( "state", "variable" ) ] ) ),
      X_historical_years ]

printlog( "FINAL STEP: normalizing to the base year. Necessary for large discrepancies between model output and observed historical data" )
L143.HDDCDD_scen_state[ X_future_years ] <- L143.HDDCDD_scen_state[[ X_final_historical_year ]] * 
      L143.HDDCDD_scen_state[ X_future_years ] / L143.HDDCDD_scen_state[[ "Scen_final_historical_year" ]]
L143.HDDCDD_scen_state <- L143.HDDCDD_scen_state[ c( "state", "Scen", "GCM", "variable", X_historical_years, X_future_years ) ]


# -----------------------------------------------------------------------------
# 3. Output

#Add comments for each table
comments.L143.share_state_Pop_CDD_sR9 <- c( "State-level share of person cooling degree days within census division (subregion9)","Unitless" )
comments.L143.share_state_Pop_CDD_sR13 <- c( "State-level share of person cooling degree days within subregion13","Unitless" )
comments.L143.share_state_Pop_HDD_sR9 <- c( "State-level share of person heating degree days within census division (subregion9)","Unitless" )
comments.L143.share_state_Pop_HDD_sR13 <- c( "State-level share of person heating degree days within subregion13","Unitless" )
comments.L143.HDDCDD_scen_state <- c( "Heating and cooling degree days by state and scenario","Degree F days" )

#write tables as CSV files
writedata( L143.share_state_Pop_CDD_sR9, domain="GCAMUSA_LEVEL1_DATA", fn="L143.share_state_Pop_CDD_sR9", comments=comments.L143.share_state_Pop_CDD_sR9 )
writedata( L143.share_state_Pop_CDD_sR13, domain="GCAMUSA_LEVEL1_DATA", fn="L143.share_state_Pop_CDD_sR13", comments=comments.L143.share_state_Pop_CDD_sR13 )
writedata( L143.share_state_Pop_HDD_sR9, domain="GCAMUSA_LEVEL1_DATA", fn="L143.share_state_Pop_HDD_sR9", comments=comments.L143.share_state_Pop_HDD_sR9 )
writedata( L143.share_state_Pop_HDD_sR13, domain="GCAMUSA_LEVEL1_DATA", fn="L143.share_state_Pop_HDD_sR13", comments=comments.L143.share_state_Pop_HDD_sR13 )
writedata( L143.HDDCDD_scen_state, domain="GCAMUSA_LEVEL1_DATA", fn="L143.HDDCDD_scen_state", comments=comments.L143.HDDCDD_scen_state )

# Every script should finish with this line
logstop()