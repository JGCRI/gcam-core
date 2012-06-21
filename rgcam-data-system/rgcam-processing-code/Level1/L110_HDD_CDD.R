# L110_HDD_CDD.R
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L110_HDD_CDD.R" )
printlog( "State-level heating and cooling service demands divided by population-weighted averages by subregion9 and subregion13" )

# -----------------------------------------------------------------------------
# 1. Read files

Census_state_division <- readmap( "Census_state_division" )
Census_pop_hist <- readdata( "Census_pop_hist" )
CDD_His <- readdata( "CDD_His" )
HDD_His <- readdata( "HDD_His" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Generate estimates of heating and cooling degree days by subregion13 (for RECS) and subregion9 (for CBECS)
# 2a. add lookup vectors to CDD, HDD, and population tables
CDD_His$subregion9 <- Census_state_division$subregion9[ match( CDD_His$state, Census_state_division$state ) ]
CDD_His$subregion13 <- Census_state_division$subregion13[ match( CDD_His$state, Census_state_division$state ) ]
HDD_His$subregion9 <- Census_state_division$subregion9[ match( HDD_His$state, Census_state_division$state ) ]
HDD_His$subregion13 <- Census_state_division$subregion13[ match( HDD_His$state, Census_state_division$state ) ]
Census_pop_hist$subregion9 <- Census_state_division$subregion9[ match( Census_pop_hist$state, Census_state_division$state ) ]
Census_pop_hist$subregion13 <- Census_state_division$subregion13[ match( Census_pop_hist$state, Census_state_division$state ) ]

# 2b. order the states in all data frames to ensure one-to-one correspondence
L110_CDD_state <- CDD_His[ order( CDD_His$state ), ]
L110_HDD_state <- HDD_His[ order( HDD_His$state ), ]
Census_pop_hist <- Census_pop_hist[ order( Census_pop_hist$state ), ]

# 2c. multiply HDD and CDD by population in new tables
printlog( "Multiplying state-level population by HDD and CDD" )
L110_Pop_CDD_state <- data.frame( L110_CDD_state[ c( "state", "subregion9", "subregion13" ) ] ,
      L110_CDD_state[ X_base_years ] * Census_pop_hist[ X_base_years ] )
L110_Pop_HDD_state <- data.frame( L110_HDD_state[ c( "state", "subregion9", "subregion13" ) ] ,
      L110_HDD_state[ X_base_years ] * Census_pop_hist[ X_base_years ] )

# 2d. aggregate population and pop_HDD/pop_CDD by subregion9 and subregion13
printlog( "Aggregating into subregions to calculate weighted average HDD and CDD" )
L110_Pop_CDD_sR9 <- aggregate( L110_Pop_CDD_state[ X_base_years ], list( subregion9 = L110_Pop_CDD_state$subregion9 ), sum )
L110_Pop_CDD_sR13 <- aggregate( L110_Pop_CDD_state[ X_base_years ], list( subregion13 = L110_Pop_CDD_state$subregion13 ), sum )

L110_Pop_HDD_sR9 <- aggregate( L110_Pop_HDD_state[ X_base_years ], list( subregion9 = L110_Pop_HDD_state$subregion9 ), sum )
L110_Pop_HDD_sR13 <- aggregate( L110_Pop_HDD_state[ X_base_years ], list( subregion13 = L110_Pop_HDD_state$subregion13 ), sum )

L110_Pop_sR9 <- aggregate( Census_pop_hist[ X_base_years ], list( subregion9 = Census_pop_hist$subregion9 ), sum )
L110_Pop_sR13 <- aggregate( Census_pop_hist[ X_base_years ], list( subregion13 = Census_pop_hist$subregion13 ), sum )

# 2e. calculate population-weighted average HDD and CDD by subregion9 and subregion13
L110_CDD_sR9 <- data.frame( subregion9 = L110_Pop_CDD_sR9$subregion9, L110_Pop_CDD_sR9[ X_base_years ] / L110_Pop_sR9[ X_base_years ] )
L110_CDD_sR13 <- data.frame( subregion13 = L110_Pop_CDD_sR13$subregion13, L110_Pop_CDD_sR13[ X_base_years ] / L110_Pop_sR13[ X_base_years ] )

L110_HDD_sR9 <- data.frame( subregion9 = L110_Pop_HDD_sR9$subregion9, L110_Pop_HDD_sR9[ X_base_years ] / L110_Pop_sR9[ X_base_years ] )
L110_HDD_sR13 <- data.frame( subregion13 = L110_Pop_HDD_sR13$subregion13, L110_Pop_HDD_sR13[ X_base_years ] / L110_Pop_sR13[ X_base_years ] )

#2f. Calculate each state's adjustment factor as compared with its subregion9 or subregion13
#    This factor estimates the heating / cooling service required in each state as compared with its subregion's average
#    melt all tables so that the year is a column (not including the unused ID vectors)
L110_CDD_state.melt <- melt( CDD_His[ c( "state", X_base_years ) ], id.vars = "state" )
L110_HDD_state.melt <- melt( HDD_His[ c( "state", X_base_years ) ], id.vars = "state" )

L110_CDD_sR9.melt <- melt( L110_CDD_sR9[ c( "subregion9", X_base_years ) ], id.vars = "subregion9" )
L110_CDD_sR13.melt <- melt( L110_CDD_sR13[ c( "subregion13", X_base_years ) ], id.vars = "subregion13" )

L110_HDD_sR9.melt <- melt( L110_HDD_sR9[ c( "subregion9", X_base_years ) ], id.vars = "subregion9" )
L110_HDD_sR13.melt <- melt( L110_HDD_sR13[ c( "subregion13", X_base_years ) ], id.vars = "subregion13" )

#    match in subregion9 and subregion13 into the state-wise tables
L110_CDD_state.melt$subregion9 <- Census_state_division$subregion9[ match( L110_CDD_state.melt$state, Census_state_division$state ) ]
L110_CDD_state.melt$subregion13 <- Census_state_division$subregion13[ match( L110_CDD_state.melt$state, Census_state_division$state ) ]
L110_HDD_state.melt$subregion9 <- Census_state_division$subregion9[ match( L110_HDD_state.melt$state, Census_state_division$state ) ]
L110_HDD_state.melt$subregion13 <- Census_state_division$subregion13[ match( L110_HDD_state.melt$state, Census_state_division$state ) ]

#    match in the population-weighted average CDD and HDD by subregion9 and subregion13
L110_CDD_state.melt$CDD_sR9 <- L110_CDD_sR9.melt$value[
      match( paste( L110_CDD_state.melt$subregion9, L110_CDD_state.melt$variable ),
             paste( L110_CDD_sR9.melt$subregion9, L110_CDD_sR9.melt$variable ) ) ]
L110_CDD_state.melt$CDD_sR13 <- L110_CDD_sR13.melt$value[
      match( paste( L110_CDD_state.melt$subregion13, L110_CDD_state.melt$variable ),
             paste( L110_CDD_sR13.melt$subregion13, L110_CDD_sR13.melt$variable ) ) ]
L110_HDD_state.melt$HDD_sR9 <- L110_HDD_sR9.melt$value[
      match( paste( L110_HDD_state.melt$subregion9, L110_HDD_state.melt$variable ),
             paste( L110_HDD_sR9.melt$subregion9, L110_HDD_sR9.melt$variable ) ) ]
L110_HDD_state.melt$HDD_sR13 <- L110_HDD_sR13.melt$value[
      match( paste( L110_HDD_state.melt$subregion13, L110_HDD_state.melt$variable ),
             paste( L110_HDD_sR13.melt$subregion13, L110_HDD_sR13.melt$variable ) ) ]

#    calculate the multipliers to go from the subregion to the state
L110_CDD_state.melt$sR9_adj <- L110_CDD_state.melt$value / L110_CDD_state.melt$CDD_sR9
L110_CDD_state.melt$sR13_adj <- L110_CDD_state.melt$value / L110_CDD_state.melt$CDD_sR13
L110_HDD_state.melt$sR9_adj <- L110_HDD_state.melt$value / L110_HDD_state.melt$HDD_sR9
L110_HDD_state.melt$sR13_adj <- L110_HDD_state.melt$value / L110_HDD_state.melt$HDD_sR13

#    reset the names of the column called "value" so that the cast function doesn't use it
names( L110_CDD_state.melt )[ names( L110_CDD_state.melt ) == "value" ] <- "CDD"
names( L110_HDD_state.melt )[ names( L110_HDD_state.melt ) == "value" ] <- "HDD"

#    re-cast the data with years as columns, dropping unnecessary ID vectors
L110_CDD_state_sR9_adj <- cast( L110_CDD_state.melt, state ~ variable, value = "sR9_adj" )
L110_CDD_state_sR13_adj <- cast( L110_CDD_state.melt, state ~ variable, value = "sR13_adj" )
L110_HDD_state_sR9_adj <- cast( L110_HDD_state.melt, state ~ variable, value = "sR9_adj" )
L110_HDD_state_sR13_adj <- cast( L110_HDD_state.melt, state ~ variable, value = "sR13_adj" )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L110_CDD_state_sR9_adj <- c( "State-level cooling adjustment factor from population-weighted average of census division (subregion9)","Unitless" )
comments.L110_CDD_state_sR13_adj <- c( "State-level cooling adjustment factor from population-weighted average of census division plus large states (subregion13)","Unitless" )
comments.L110_HDD_state_sR9_adj <- c( "State-level heating adjustment factor from population-weighted average of census division (subregion9)","Unitless" )
comments.L110_HDD_state_sR13_adj <- c( "State-level heating adjustment factor from population-weighted average of census division plus large states (subregion13)","Unitless" )

#write tables as CSV files
writedata( L110_CDD_state_sR9_adj,fn="L110_CDD_state_sR9_adj", comments=comments.L110_CDD_state_sR9_adj )
writedata( L110_CDD_state_sR13_adj,fn="L110_CDD_state_sR13_adj", comments=comments.L110_CDD_state_sR13_adj )
writedata( L110_HDD_state_sR9_adj,fn="L110_HDD_state_sR9_adj", comments=comments.L110_HDD_state_sR9_adj )
writedata( L110_HDD_state_sR13_adj,fn="L110_HDD_state_sR13_adj", comments=comments.L110_HDD_state_sR13_adj )

# Every script should finish with this line
logstop()