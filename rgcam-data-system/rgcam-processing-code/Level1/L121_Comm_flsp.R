# L121_Comm_flsp.R
# COMMERCIAL SECTOR FLOORSPACE BY STATE AND BASE YEAR
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L121_Comm_flsp.R" )
printlog( "Commercial sector floorspace and minor adjustments to CBECS databases" )

# -----------------------------------------------------------------------------
# 1. Read files

Census_state_division <- readmap( "Census_state_division" )
CBECS_1979_1983 <- readdata( "CBECS_1979_1983" )
CBECS_1986 <- readdata( "CBECS_1986" )
CBECS_1989 <- readdata( "CBECS_1989" )
CBECS_1992 <- readdata( "CBECS_1992" )
CBECS_1995 <- readdata( "CBECS_1995" )
CBECS_1999 <- readdata( "CBECS_1999" )
CBECS_2003 <- readdata( "CBECS_2003" )
Census_pop_hist <- readdata( "Census_pop_hist" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#2a. PREPARATION AND CLEANING OF CBECS DATABASES
#Create modified RECS objects
L121_CBECS_1979_1983 <- CBECS_1979_1983
L121_CBECS_1986 <- CBECS_1986
L121_CBECS_1989 <- CBECS_1989
L121_CBECS_1992 <- CBECS_1992
L121_CBECS_1995 <- CBECS_1995
L121_CBECS_1999 <- CBECS_1999
L121_CBECS_2003 <- CBECS_2003

#Add vector for GCAM subregion9 (census divisions)
L121_CBECS_1986$subregion9 <- Census_state_division$subregion9[ match( L121_CBECS_1986$CENDIV3, Census_state_division$DIVISION ) ]
L121_CBECS_1989$subregion9 <- Census_state_division$subregion9[ match( L121_CBECS_1989$CENDIV4, Census_state_division$DIVISION ) ]
L121_CBECS_1992$subregion9 <- Census_state_division$subregion9[ match( L121_CBECS_1992$CENDIV5, Census_state_division$DIVISION ) ]
L121_CBECS_1995$subregion9 <- Census_state_division$subregion9[ match( L121_CBECS_1995$CENDIV6, Census_state_division$DIVISION ) ]
L121_CBECS_1999$subregion9 <- Census_state_division$subregion9[ match( L121_CBECS_1999$CENDIV7, Census_state_division$DIVISION ) ]
L121_CBECS_2003$subregion9 <- Census_state_division$subregion9[ match( L121_CBECS_2003$CENDIV8, Census_state_division$DIVISION ) ]

#Convert all missing values to 0 in years with energy consumption data
L121_CBECS_1992[ is.na( L121_CBECS_1992 ) ] <- 0
L121_CBECS_1995[ L121_CBECS_1995 == 1e14 ] <- 0
L121_CBECS_2003[ is.na( L121_CBECS_2003 ) ] <- 0

#Subset only the columns that are relevant for floorspace in each table, and calculate weighted floorspace for aggregation (in bm2)
printlog( "Multiplying floorspace in CBECS by sampling weights to get floorspace by census division (subregion9)" )
L121_flsp_1979_CBECS <- data.frame( subregion4 = L121_CBECS_1979_1983$REGION, flsp_bm2 = L121_CBECS_1979_1983$SQFT1 * conv_milft2_bm2 )
L121_flsp_1983_CBECS <- data.frame( subregion4 = L121_CBECS_1979_1983$REGION, flsp_bm2 = L121_CBECS_1979_1983$SQFT2 * conv_milft2_bm2 )
L121_flsp_1986_CBECS <- data.frame( subregion4 = L121_CBECS_1986$REGION3, subregion9 = L121_CBECS_1986$subregion9,
      flsp_bm2 = L121_CBECS_1986$ADJWT3 * L121_CBECS_1986$SQFT3 * conv_ft2_bm2 )
L121_flsp_1989_CBECS <- data.frame( subregion9 = L121_CBECS_1989$subregion9,
      flsp_bm2 = L121_CBECS_1989$ADJWT4 * L121_CBECS_1989$SQFT4 * conv_ft2_bm2 )
L121_flsp_1992_CBECS <- data.frame( subregion9 = L121_CBECS_1992$subregion9,
      flsp_bm2 = L121_CBECS_1992$ADJWT5 * L121_CBECS_1992$SQFT5 * conv_ft2_bm2 )
L121_flsp_1995_CBECS <- data.frame( subregion9 = L121_CBECS_1995$subregion9,
      flsp_bm2 = L121_CBECS_1995$ADJWT6 * L121_CBECS_1995$SQFT6 * conv_ft2_bm2 )
L121_flsp_1999_CBECS <- data.frame( subregion9 = L121_CBECS_1999$subregion9,
      flsp_bm2 = L121_CBECS_1999$ADJWT7 * L121_CBECS_1999$SQFT7 * conv_ft2_bm2 )
L121_flsp_2003_CBECS <- data.frame( subregion9 = L121_CBECS_2003$subregion9,
      flsp_bm2 = L121_CBECS_2003$ADJWT8 * L121_CBECS_2003$SQFT8 * conv_ft2_bm2 )

#2b. EXTENSION OF SUBREGION9 TO DATASETS WHOSE AGGREGATION LEVEL IS SUBREGION4
#Aggregate weighted floorspace by 4 subregions in 1986 dataset, in order to calculate scalers for 9 subregions in prior years
printlog( "NOTE: Assuming that census divisions (subregion9) follow their census regions (subregion4) in years when not available" )
L121_flsp_bm2_sR4_comm_1986 <- aggregate( L121_flsp_1986_CBECS$flsp_bm2, list( subregion4 = L121_flsp_1986_CBECS$subregion4 ), sum )

#Calculate scaler to go from 1986 to 1979
L121_flsp_sR4_comm_1979_1986_scaler <- data.frame( subregion4 = L121_flsp_1979_CBECS$subregion4,
      scaler = L121_flsp_1979_CBECS$flsp_bm2 / L121_flsp_bm2_sR4_comm_1986$x )

#1986 to 1983 scaler
L121_flsp_sR4_comm_1983_1986_scaler <- data.frame( subregion4 = L121_flsp_1983_CBECS$subregion4,
      scaler = L121_flsp_1983_CBECS$flsp_bm2 / L121_flsp_bm2_sR4_comm_1986$x )

#Aggregate weighted floorspace by 9 subregions in 1986 - 2003
L121_flsp_bm2_sR9_comm_1986 <- aggregate( L121_flsp_1986_CBECS$flsp_bm2, list( subregion9 = L121_flsp_1986_CBECS$subregion9 ), sum )
L121_flsp_bm2_sR9_comm_1989 <- aggregate( L121_flsp_1989_CBECS$flsp_bm2, list( subregion9 = L121_flsp_1989_CBECS$subregion9 ), sum )
L121_flsp_bm2_sR9_comm_1992 <- aggregate( L121_flsp_1992_CBECS$flsp_bm2, list( subregion9 = L121_flsp_1992_CBECS$subregion9 ), sum )
L121_flsp_bm2_sR9_comm_1995 <- aggregate( L121_flsp_1995_CBECS$flsp_bm2, list( subregion9 = L121_flsp_1995_CBECS$subregion9 ), sum )
L121_flsp_bm2_sR9_comm_1999 <- aggregate( L121_flsp_1999_CBECS$flsp_bm2, list( subregion9 = L121_flsp_1999_CBECS$subregion9 ), sum )
L121_flsp_bm2_sR9_comm_2003 <- aggregate( L121_flsp_2003_CBECS$flsp_bm2, list( subregion9 = L121_flsp_2003_CBECS$subregion9 ), sum )

#Rename data columns to the given year
names( L121_flsp_bm2_sR9_comm_1986 )[ names( L121_flsp_bm2_sR9_comm_1986 ) == "x"] <- "X1986"
names( L121_flsp_bm2_sR9_comm_1989 )[ names( L121_flsp_bm2_sR9_comm_1989 ) == "x"] <- "X1989"
names( L121_flsp_bm2_sR9_comm_1992 )[ names( L121_flsp_bm2_sR9_comm_1992 ) == "x"] <- "X1992"
names( L121_flsp_bm2_sR9_comm_1995 )[ names( L121_flsp_bm2_sR9_comm_1995 ) == "x"] <- "X1995"
names( L121_flsp_bm2_sR9_comm_1999 )[ names( L121_flsp_bm2_sR9_comm_1999 ) == "x"] <- "X1999"
names( L121_flsp_bm2_sR9_comm_2003 )[ names( L121_flsp_bm2_sR9_comm_2003 ) == "x"] <- "X2003"

#Map in a vector for 9 subregions to the 1993 dataset, and match in the scalers to get the 1990 and 1984 output
#Map in a vector for 4 subregions to the 1986 dataset, and match in the scalers to get the 1983 and 1979 output
L121_flsp_bm2_sR9_comm_1979_1983 <- L121_flsp_bm2_sR9_comm_1986
L121_flsp_bm2_sR9_comm_1979_1983$subregion4 <- Census_state_division$REGION[
      match( L121_flsp_bm2_sR9_comm_1979_1983$subregion9, Census_state_division$subregion9 ) ]
L121_flsp_bm2_sR9_comm_1979_1983$scaler_1979 <- L121_flsp_sR4_comm_1979_1986_scaler$scaler[
      match( L121_flsp_bm2_sR9_comm_1979_1983$subregion4, L121_flsp_sR4_comm_1979_1986_scaler$subregion4 ) ]
L121_flsp_bm2_sR9_comm_1979_1983$X1979 <- L121_flsp_bm2_sR9_comm_1979_1983$X1986 * L121_flsp_bm2_sR9_comm_1979_1983$scaler_1979
L121_flsp_bm2_sR9_comm_1979_1983$scaler_1983 <- L121_flsp_sR4_comm_1983_1986_scaler$scaler[
      match( L121_flsp_bm2_sR9_comm_1979_1983$subregion4, L121_flsp_sR4_comm_1983_1986_scaler$subregion4 ) ]
L121_flsp_bm2_sR9_comm_1979_1983$X1983 <- L121_flsp_bm2_sR9_comm_1979_1983$X1986 * L121_flsp_bm2_sR9_comm_1979_1983$scaler_1983

#2c. CALCULATION OF FLOORSPACE IN SPECIFIED BASE YEARS
#Combine all years into a single data table. Interpolate model base years.
printlog( "NOTE: Using linear interpolation to calculate floorspace in base years between CBECS years" )
L121_flsp_bm2_sR9_comm_RECS <- data.frame( subregion9 = L121_flsp_bm2_sR9_comm_1979_1983$subregion9,
      X1979 = L121_flsp_bm2_sR9_comm_1979_1983$X1979, X1983 = L121_flsp_bm2_sR9_comm_1979_1983$X1983,
      X1986 = L121_flsp_bm2_sR9_comm_1986$X1986, X1989 = L121_flsp_bm2_sR9_comm_1989$X1989,
      X1992 = L121_flsp_bm2_sR9_comm_1992$X1992, X1995 = L121_flsp_bm2_sR9_comm_1995$X1995,
      X1999 = L121_flsp_bm2_sR9_comm_1999$X1999, X2003 = L121_flsp_bm2_sR9_comm_2003$X2003 )
L121_flsp_bm2_sR9_comm <- gcam_interp( L121_flsp_bm2_sR9_comm_RECS, base_years )
L121_flsp_bm2_sR9_comm <- L121_flsp_bm2_sR9_comm[ c( "subregion9", X_base_years ) ]

#Use population data to get floorspace by subregion in years outside the CBECS time series (1975, 2005, 2008)
#Aggregate population data by subregion
printlog( "NOTE: Assuming constant per-capita floorspace in years outside of CBECS years" )
Census_pop_hist$subregion9 <- Census_state_division$subregion9[ match( Census_pop_hist$state, Census_state_division$state ) ]
L121_pop_hist_sR9 <- aggregate( Census_pop_hist[ X_base_years ],list( subregion9 = Census_pop_hist$subregion9 ), sum )

#Use these ratios to get floorspace in the missing years
L121_flsp_bm2_sR9_comm$X1975 <- L121_flsp_bm2_sR9_comm$X1980 * L121_pop_hist_sR9$X1975 / L121_pop_hist_sR9$X1980
L121_flsp_bm2_sR9_comm$X2005 <- L121_flsp_bm2_sR9_comm$X2000 * L121_pop_hist_sR9$X2005 / L121_pop_hist_sR9$X2000
L121_flsp_bm2_sR9_comm$X2008 <- L121_flsp_bm2_sR9_comm$X2000 * L121_pop_hist_sR9$X2008 / L121_pop_hist_sR9$X2000

#Use per-capita floorspace by subregion and state population to disaggregate floorspace to states
printlog( "Disaggregating floorspace from subregion9 to individual states" )
printlog( "NOTE: Assuming constant per-capita floorspace by state within census division" )
L121_pcflsp_m2_sR9_comm <- data.frame( subregion9 = L121_flsp_bm2_sR9_comm$subregion9,
      L121_flsp_bm2_sR9_comm[ X_base_years ] * 1e9 / L121_pop_hist_sR9[ X_base_years ] )

#Melt tables of population and per-capita floorspace. Multiply to get total floorspace by state.
L121_pcflsp_m2_sR9_comm.melt <- melt( L121_pcflsp_m2_sR9_comm, id.vars = "subregion9" )
L121_Census_pop_hist.melt <- melt( Census_pop_hist, id.vars = c( "state", "subregion9" ) )
names( L121_Census_pop_hist.melt )[ names( L121_Census_pop_hist.melt ) == "value" ] <- "population"
L121_Census_pop_hist.melt$pcflsp_m2 <- L121_pcflsp_m2_sR9_comm.melt$value[
      match( paste( L121_Census_pop_hist.melt$subregion9, L121_Census_pop_hist.melt$variable ),
             paste( L121_pcflsp_m2_sR9_comm.melt$subregion9, L121_pcflsp_m2_sR9_comm.melt$variable ) ) ]
L121_Census_pop_hist.melt$flsp_bm2 <- L121_Census_pop_hist.melt$population * L121_Census_pop_hist.melt$pcflsp_m2 * 1e-9
L121_Census_pop_hist.melt$GCAM_sector <- "comm"

#Recast the table so that years are columns
L121_flsp_bm2_state_comm <- cast( L121_Census_pop_hist.melt, state + GCAM_sector ~ variable, value = "flsp_bm2" )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L121_CBECS_1992 <- "1992 Commercial Building Energy Consumption Survey"
comments.L121_CBECS_1995 <- "1995 Commercial Building Energy Consumption Survey"
comments.L121_CBECS_2003 <- "2003 Commercial Building Energy Consumption Survey"
comments.L121_flsp_bm2_state_comm <- c( "Commercial floorspace by state","Unit = billion m2" )

#write tables as CSV files
writedata( L121_CBECS_1992,fn="L121_CBECS_1992", comments=comments.L121_CBECS_1992 )
writedata( L121_CBECS_1995,fn="L121_CBECS_1995", comments=comments.L121_CBECS_1995 )
writedata( L121_CBECS_2003,fn="L121_CBECS_2003", comments=comments.L121_CBECS_2003 )
writedata( L121_flsp_bm2_state_comm,fn="L121_flsp_bm2_state_comm", comments=comments.L121_flsp_bm2_state_comm )

# Every script should finish with this line
logstop()