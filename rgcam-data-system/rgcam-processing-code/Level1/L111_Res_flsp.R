# L111_Res_flsp.R
# RESIDENTIAL SECTOR FLOORSPACE BY STATE AND BASE YEAR
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L111_Res_flsp.R" )
printlog( "Residential sector floorspace and minor adjustments to RECS databases" )

# -----------------------------------------------------------------------------
# 1. Read files

Census_state_division <- readmap( "Census_state_division" )
Census_pop_hist <- readdata( "Census_pop_hist" )
RECS_1979 <- readdata( "RECS_1979" )
RECS_1984 <- readdata( "RECS_1984" )
RECS_1990 <- readdata( "RECS_1990" )
RECS_1993 <- readdata( "RECS_1993" )
RECS_1997 <- readdata( "RECS_1997" )
RECS_2001 <- readdata( "RECS_2001" )
RECS_2005 <- readdata( "RECS_2005" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#2a. PREPARATION AND CLEANING OF RECS DATABASES
#Create modified RECS objects
L111_RECS_1979 <- RECS_1979
L111_RECS_1984 <- RECS_1984
L111_RECS_1990 <- RECS_1990
L111_RECS_1993 <- RECS_1993
L111_RECS_1997 <- RECS_1997
L111_RECS_2001 <- RECS_2001
L111_RECS_2005 <- RECS_2005

#Add vector for GCAM subregion9 (census divisions)
L111_RECS_1979$subregion9 <- Census_state_division$subregion9[ match( L111_RECS_1979$DIVISION, Census_state_division$DIVISION ) ]
L111_RECS_1984$subregion9 <- Census_state_division$subregion9[ match( L111_RECS_1984$DIVISION, Census_state_division$DIVISION ) ]
L111_RECS_1990$subregion9 <- Census_state_division$subregion9[ match( L111_RECS_1990$DIVISION, Census_state_division$DIVISION ) ]
L111_RECS_1993$subregion9 <- Census_state_division$subregion9[ match( L111_RECS_1993$DIVISION, Census_state_division$DIVISION ) ]
L111_RECS_1997$subregion9 <- Census_state_division$subregion9[ match( L111_RECS_1997$DIVISION, Census_state_division$DIVISION ) ]
L111_RECS_2001$subregion9 <- Census_state_division$subregion9[ match( L111_RECS_2001$DIVISION, Census_state_division$DIVISION ) ]
L111_RECS_2005$subregion9 <- Census_state_division$subregion9[ match( L111_RECS_2005$DIVISION, Census_state_division$DIVISION ) ]

#Add vector for subregion13 in 1993-2005 editions (census divisions with four large states disaggregated)
L111_RECS_1993$subregion13 <- Census_state_division$subregion13[ match( L111_RECS_1993$LRGSTATE, Census_state_division$LRGSTATE ) ]
L111_RECS_1993$subregion13[ L111_RECS_1993$LRGSTATE == 0 ] <- L111_RECS_1993$subregion9[ L111_RECS_1993$LRGSTATE == 0 ]
L111_RECS_1997$subregion13 <- Census_state_division$subregion13[ match( L111_RECS_1997$LRGSTATE, Census_state_division$LRGSTATE ) ]
L111_RECS_1997$subregion13[ L111_RECS_1997$LRGSTATE == 0 ] <- L111_RECS_1997$subregion9[ L111_RECS_1997$LRGSTATE == 0 ]
L111_RECS_2001$subregion13 <- Census_state_division$subregion13[ match( L111_RECS_2001$LRGSTATE, Census_state_division$LRGSTATE ) ]
L111_RECS_2001$subregion13[ L111_RECS_2001$LRGSTATE == 0 ] <- L111_RECS_2001$subregion9[ L111_RECS_2001$LRGSTATE == 0 ]
L111_RECS_2005$subregion13 <- Census_state_division$subregion13[ match( L111_RECS_2005$LRGSTATE, Census_state_division$LRGSTATE ) ]
L111_RECS_2005$subregion13[ L111_RECS_2005$LRGSTATE == 0 ] <- L111_RECS_2005$subregion9[ L111_RECS_2005$LRGSTATE == 0 ]

#Convert all 9999999 values to 0 in 1990 and 2005 RECS databases. Also convert missing values in 2005 RECS.
L111_RECS_1990[ L111_RECS_1990 == 9999999 ] <- 0
L111_RECS_2005[ L111_RECS_2005 == 9999999 ] <- 0
L111_RECS_2005[ is.na( L111_RECS_2005 ) ] <- 0

#Subset only the columns that are relevant for floorspace in each table, and calculate weighted floorspace for aggregation (in bm2)
printlog( "Multiplying floorspace in RECS databases by sampling weights to get sub-regional estimates" )
L111_flsp_1984_RECS <- data.frame( subregion9 = L111_RECS_1984$subregion9,
      flsp_bm2 = L111_RECS_1984$HOUSEHOLDS * L111_RECS_1984$HEATED * conv_milft2_bm2 )
L111_flsp_1990_RECS <- data.frame( subregion9 = L111_RECS_1990$subregion9,
      flsp_bm2 = L111_RECS_1990$NWEIGHT * L111_RECS_1990$HEATED * conv_ft2_bm2 )
L111_flsp_1993_RECS <- data.frame( subregion9 = L111_RECS_1993$subregion9, subregion13 = L111_RECS_1993$subregion13,
      flsp_bm2 = L111_RECS_1993$NWEIGHT * L111_RECS_1993$HEATED * conv_ft2_bm2 )
L111_flsp_1997_RECS <- data.frame( subregion9 = L111_RECS_1997$subregion9, subregion13 = L111_RECS_1997$subregion13,
      flsp_bm2 = L111_RECS_1997$NWEIGHT * L111_RECS_1997$SQFTREG * conv_ft2_bm2 )
L111_flsp_2001_RECS <- data.frame( subregion9 = L111_RECS_2001$subregion9, subregion13 = L111_RECS_2001$subregion13,
      flsp_bm2 = L111_RECS_2001$NWEIGHT * L111_RECS_2001$TOTHSQFT * conv_ft2_bm2 )
L111_flsp_2005_RECS <- data.frame( subregion9 = L111_RECS_2005$subregion9, subregion13 = L111_RECS_2005$subregion13,
      flsp_bm2 = L111_RECS_2005$NWEIGHT * L111_RECS_2005$TOTHSQFT * conv_ft2_bm2 )

#2b. EXTENSION OF SUBREGION13 TO DATASETS WHOSE AGGREGATION LEVEL IS SUBREGION9
#Aggregate weighted floorspace by 9 subregions in 1990 and 1993 datasets, in order to calculate scalers for 13 subregions
printlog( "NOTE: Assuming that large states follow their census divisions in years when not available" )
L111_flsp_bm2_sR9_res_1990 <- aggregate( L111_flsp_1990_RECS$flsp_bm2, list( subregion9 = L111_flsp_1990_RECS$subregion9 ), sum )
L111_flsp_bm2_sR9_res_1993 <- aggregate( L111_flsp_1993_RECS$flsp_bm2, list( subregion9 = L111_flsp_1993_RECS$subregion9 ), sum )

#Calculate scalers for subregion13 within subregion9, from 1993 to 1984
L111_flsp_bm2_sR9_res_1984 <- L111_flsp_1984_RECS[ order( L111_flsp_1984_RECS$subregion9 ), ]
L111_flsp_sR9_res_1984_1993_scaler <- data.frame( subregion9 = L111_flsp_bm2_sR9_res_1984$subregion9,
      scaler = L111_flsp_bm2_sR9_res_1984$flsp_bm2 / L111_flsp_bm2_sR9_res_1993$x )

#1993 to 1990 scaler
L111_flsp_sR9_res_1990_1993_scaler <- data.frame( subregion9 = L111_flsp_bm2_sR9_res_1990$subregion9,
      scaler = L111_flsp_bm2_sR9_res_1990$x / L111_flsp_bm2_sR9_res_1993$x )

#Aggregate weighted floorspace by subregion13 in 1993, 1997, 2001, and 2005
L111_flsp_bm2_sR13_res_1993 <- aggregate( L111_flsp_1993_RECS$flsp_bm2, list( subregion13 = L111_flsp_1993_RECS$subregion13 ), sum )
L111_flsp_bm2_sR13_res_1997 <- aggregate( L111_flsp_1997_RECS$flsp_bm2, list( subregion13 = L111_flsp_1997_RECS$subregion13 ), sum )
L111_flsp_bm2_sR13_res_2001 <- aggregate( L111_flsp_2001_RECS$flsp_bm2, list( subregion13 = L111_flsp_2001_RECS$subregion13 ), sum )
L111_flsp_bm2_sR13_res_2005 <- aggregate( L111_flsp_2005_RECS$flsp_bm2, list( subregion13 = L111_flsp_2005_RECS$subregion13 ), sum )

#Rename data columns to the given year
names( L111_flsp_bm2_sR13_res_1993 )[ names( L111_flsp_bm2_sR13_res_1993 ) == "x"] <- "X1993"
names( L111_flsp_bm2_sR13_res_1997 )[ names( L111_flsp_bm2_sR13_res_1997 ) == "x"] <- "X1997"
names( L111_flsp_bm2_sR13_res_2001 )[ names( L111_flsp_bm2_sR13_res_2001 ) == "x"] <- "X2001"
names( L111_flsp_bm2_sR13_res_2005 )[ names( L111_flsp_bm2_sR13_res_2005 ) == "x"] <- "X2005"

#Map in a vector for 9 subregions to the 1993 dataset, and match in the scalers to get the 1990 and 1984 output
L111_flsp_bm2_sR13_res_1984_1990 <- L111_flsp_bm2_sR13_res_1993
L111_flsp_bm2_sR13_res_1984_1990$subregion9 <- Census_state_division$subregion9[
      match( L111_flsp_bm2_sR13_res_1984_1990$subregion13, Census_state_division$subregion13 ) ]
L111_flsp_bm2_sR13_res_1984_1990$scaler_1984 <- L111_flsp_sR9_res_1984_1993_scaler$scaler[
      match( L111_flsp_bm2_sR13_res_1984_1990$subregion9, L111_flsp_sR9_res_1984_1993_scaler$subregion9 ) ]
L111_flsp_bm2_sR13_res_1984_1990$X1984 <- L111_flsp_bm2_sR13_res_1984_1990$X1993 * L111_flsp_bm2_sR13_res_1984_1990$scaler_1984
L111_flsp_bm2_sR13_res_1984_1990$scaler_1990 <- L111_flsp_sR9_res_1990_1993_scaler$scaler[
      match( L111_flsp_bm2_sR13_res_1984_1990$subregion9, L111_flsp_sR9_res_1990_1993_scaler$subregion9 ) ]
L111_flsp_bm2_sR13_res_1984_1990$X1990 <- L111_flsp_bm2_sR13_res_1984_1990$X1993 * L111_flsp_bm2_sR13_res_1984_1990$scaler_1990

#2c. CALCULATION OF FLOORSPACE IN SPECIFIED BASE YEARS
#Combine all years into a single data table. Interpolate model base years.
printlog( "NOTE: Using linear interpolation to calculate floorspace in base years between RECS years" )
L111_flsp_bm2_sR13_res_RECS <- data.frame( subregion13 = L111_flsp_bm2_sR13_res_1984_1990$subregion13,
      X1984 = L111_flsp_bm2_sR13_res_1984_1990$X1984, X1990 = L111_flsp_bm2_sR13_res_1984_1990$X1990,
      X1993 = L111_flsp_bm2_sR13_res_1993$X1993, X1997 = L111_flsp_bm2_sR13_res_1997$X1997,
      X2001 = L111_flsp_bm2_sR13_res_2001$X2001, X2005 = L111_flsp_bm2_sR13_res_2005$X2005 )
L111_flsp_bm2_sR13_res <- gcam_interp( L111_flsp_bm2_sR13_res_RECS, base_years )
L111_flsp_bm2_sR13_res <- L111_flsp_bm2_sR13_res[ c( "subregion13", X_base_years ) ]

#Use population data to get floorspace by subregion in years outside the RECS time series (1975, 1980, 2008)
#Aggregate population data by subregion
printlog( "NOTE: Assuming constant per-capita floorspace in years outside of RECS years" )
Census_pop_hist$subregion13 <- Census_state_division$subregion13[ match( Census_pop_hist$state, Census_state_division$state ) ]
L111_pop_hist_sR13 <- aggregate( Census_pop_hist[ X_base_years ],list( subregion13 = Census_pop_hist$subregion13 ), sum )

#Use these ratios to get floorspace in the missing years
L111_flsp_bm2_sR13_res$X1975 <- L111_flsp_bm2_sR13_res$X1985 * L111_pop_hist_sR13$X1975 / L111_pop_hist_sR13$X1985
L111_flsp_bm2_sR13_res$X1980 <- L111_flsp_bm2_sR13_res$X1985 * L111_pop_hist_sR13$X1980 / L111_pop_hist_sR13$X1985
L111_flsp_bm2_sR13_res$X2008 <- L111_flsp_bm2_sR13_res$X2005 * L111_pop_hist_sR13$X2008 / L111_pop_hist_sR13$X2005

#Use per-capita floorspace by subregion and population by state to calculate floorspace by state
printlog( "Disaggregating floorspace from subregion13 to individual states" )
printlog( "NOTE: Assuming constant per-capita floorspace by state within census division" )
L111_pcflsp_m2_sR13_res <- data.frame( subregion13 = L111_flsp_bm2_sR13_res$subregion13,
      L111_flsp_bm2_sR13_res[ X_base_years ] * 1e9 / L111_pop_hist_sR13[ X_base_years ] )

#Melt tables of population and per-capita floorspace. Multiply to get total floorspace by state.
L111_pcflsp_m2_sR13_res.melt <- melt( L111_pcflsp_m2_sR13_res, id.vars = "subregion13" )
L111_Census_pop_hist.melt <- melt( Census_pop_hist, id.vars = c( "state", "subregion13" ) )
names( L111_Census_pop_hist.melt )[ names( L111_Census_pop_hist.melt ) == "value" ] <- "population"
L111_Census_pop_hist.melt$pcflsp_m2 <- L111_pcflsp_m2_sR13_res.melt$value[
      match( paste( L111_Census_pop_hist.melt$subregion13, L111_Census_pop_hist.melt$variable ),
             paste( L111_pcflsp_m2_sR13_res.melt$subregion13, L111_pcflsp_m2_sR13_res.melt$variable ) ) ]
L111_Census_pop_hist.melt$flsp_bm2 <- L111_Census_pop_hist.melt$population * L111_Census_pop_hist.melt$pcflsp_m2 * 1e-9
L111_Census_pop_hist.melt$GCAM_sector <- "resid"

#Recast the table so that years are columns
L111_flsp_bm2_state_res <- cast( L111_Census_pop_hist.melt, state + GCAM_sector ~ variable, value = "flsp_bm2" )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L111_RECS_1979 <- "1979 Residential Energy Consumption Survey"
comments.L111_RECS_1984 <- "1984 Residential Energy Consumption Survey"
comments.L111_RECS_1990 <- "1990 Residential Energy Consumption Survey"
comments.L111_RECS_1993 <- "1993 Residential Energy Consumption Survey"
comments.L111_RECS_1997 <- "1997 Residential Energy Consumption Survey"
comments.L111_RECS_2001 <- "2001 Residential Energy Consumption Survey"
comments.L111_RECS_2005 <- "2005 Residential Energy Consumption Survey"
comments.L111_flsp_bm2_state_res <- c( "Residential floorspace by state","Unit = billion m2" )

#write tables as CSV files
writedata( L111_RECS_1979,fn="L111_RECS_1979", comments=comments.L111_RECS_1979 )
writedata( L111_RECS_1984,fn="L111_RECS_1984", comments=comments.L111_RECS_1984 )
writedata( L111_RECS_1990,fn="L111_RECS_1990", comments=comments.L111_RECS_1990 )
writedata( L111_RECS_1993,fn="L111_RECS_1993", comments=comments.L111_RECS_1993 )
writedata( L111_RECS_1997,fn="L111_RECS_1997", comments=comments.L111_RECS_1997 )
writedata( L111_RECS_2001,fn="L111_RECS_2001", comments=comments.L111_RECS_2001 )
writedata( L111_RECS_2005,fn="L111_RECS_2005", comments=comments.L111_RECS_2005 )
writedata( L111_flsp_bm2_state_res,fn="L111_flsp_bm2_state_res", comments=comments.L111_flsp_bm2_state_res )

# Every script should finish with this line
logstop()