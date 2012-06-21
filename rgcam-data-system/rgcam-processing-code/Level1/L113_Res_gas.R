# L113_Res_gas.R
# RESIDENTIAL SECTOR GAS CONSUMPTION BY STATE, SERVICE, AND BASE YEAR
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L113_Res_gas.R" )
printlog( "Residential sector gas consumption by state and service" )
                  
# -----------------------------------------------------------------------------
# 1. Read files

Census_state_division <- readmap( "Census_state_division" )
GCAM_res_services <- readmap( "GCAM_res_services" )
USA_res_in <- readdata( "USA_res_in" )
L104_in_EJ_state_bld_F <- readdata( "L104_in_EJ_state_bld_F" )
L110_HDD_state_sR13_adj <- readdata( "L110_HDD_state_sR13_adj" )
L111_RECS_1979 <- readdata( "L111_RECS_1979" )
L111_RECS_1990 <- readdata( "L111_RECS_1990" )
L111_RECS_1993 <- readdata( "L111_RECS_1993" )
L111_RECS_1997 <- readdata( "L111_RECS_1997" )
L111_RECS_2001 <- readdata( "L111_RECS_2001" )
L111_RECS_2005 <- readdata( "L111_RECS_2005" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. PREPARATION OF RECS DATABASES FOR AGGREGATION
printlog( "Multiplying RECS gas consumption by end use by building sampling weights" )
#Calculate gas demands by available end uses in each RECS
#1979 is converted from Tbtu to EJ
L113_in_EJ_res_gas_U_1979_RECS <- data.frame(
      subregion9 = L111_RECS_1979$subregion9,
      Xyear = "X1979",
      sph = L111_RECS_1979$TBTUNGSPH * conv_Tbtu_EJ,
      wth = L111_RECS_1979$TBTUNGWTH * conv_Tbtu_EJ,
      apl_other = L111_RECS_1979$TBTUNGAPL * conv_Tbtu_EJ )

#1990 through 2005 databases are multiplied by sampling weights and then converted from kbtu to EJ
L113_in_EJ_res_gas_U_1990_RECS <- data.frame(
      subregion9 = L111_RECS_1990$subregion9,
      Xyear = "X1990",
      sph = L111_RECS_1990$BTUNGSPH * conv_kbtu_EJ * L111_RECS_1990$NWEIGHT,
      wth = L111_RECS_1990$BTUNGWTH * conv_kbtu_EJ * L111_RECS_1990$NWEIGHT,
      apl_other = L111_RECS_1990$BTUNGAPL * conv_kbtu_EJ * L111_RECS_1990$NWEIGHT )

L113_in_EJ_res_gas_U_1993_RECS <- data.frame(
      subregion13 = L111_RECS_1993$subregion13,
      Xyear = "X1993",
      sph = L111_RECS_1993$BTUNGSPH * conv_kbtu_EJ * L111_RECS_1993$NWEIGHT,
      wth = L111_RECS_1993$BTUNGWTH * conv_kbtu_EJ * L111_RECS_1993$NWEIGHT,
      apl_other = L111_RECS_1993$BTUNGAPL * conv_kbtu_EJ * L111_RECS_1993$NWEIGHT )

L113_in_EJ_res_gas_U_1997_RECS <- data.frame(
      subregion13 = L111_RECS_1997$subregion13,
      Xyear = "X1997",
      sph = L111_RECS_1997$BTUNGSPH * conv_kbtu_EJ * L111_RECS_1997$NWEIGHT,
      wth = L111_RECS_1997$BTUNGWTH * conv_kbtu_EJ * L111_RECS_1997$NWEIGHT,
      apl_other = L111_RECS_1997$BTUNGAPL * conv_kbtu_EJ * L111_RECS_1997$NWEIGHT )

L113_in_EJ_res_gas_U_2001_RECS <- data.frame(
      subregion13 = L111_RECS_2001$subregion13,
      Xyear = "X2001",
      sph = L111_RECS_2001$BTUNGSPH * conv_kbtu_EJ * L111_RECS_2001$NWEIGHT,
      wth = L111_RECS_2001$BTUNGWTH * conv_kbtu_EJ * L111_RECS_2001$NWEIGHT,
      apl_other = L111_RECS_2001$BTUNGAPL * conv_kbtu_EJ * L111_RECS_2001$NWEIGHT )

L113_in_EJ_res_gas_U_2005_RECS <- data.frame(
      subregion13 = L111_RECS_2005$subregion13,
      Xyear = "X2005",
      sph = L111_RECS_2005$BTUNGSPH * conv_kbtu_EJ * L111_RECS_2005$NWEIGHT,
      wth = L111_RECS_2005$BTUNGWTH * conv_kbtu_EJ * L111_RECS_2005$NWEIGHT,
      apl_other = L111_RECS_2005$BTUNGAPL * conv_kbtu_EJ * L111_RECS_2005$NWEIGHT )

# 2b. DISAGGREGATION OF RECS "APPLIANCE AND OTHER" ENERGY INTO SPECIFIED GCAM SERVICES
#Calculate national percentage allocations of "appliance and other" gas to large appliances and other
L113_in_EJ_USA_res_gas_othU <- USA_res_in[ USA_res_in$service %in% GCAM_res_otherU & USA_res_in$fuel == "delivered gas", ] 
L113_in_pct_USA_res_gas_othU <- data.frame( L113_in_EJ_USA_res_gas_othU[ c( "service", "fuel" ) ],
      sweep( L113_in_EJ_USA_res_gas_othU[ names( L113_in_EJ_USA_res_gas_othU ) %in% X_RECS_years ], 2,
             colSums( L113_in_EJ_USA_res_gas_othU[ names( L113_in_EJ_USA_res_gas_othU ) %in% X_RECS_years ] ), "/" ) )

#Add years corresponding to RECS data that isn't available in provided table
L113_in_pct_USA_res_gas_othU$X1979 = L113_in_pct_USA_res_gas_othU$X1990
L113_in_pct_USA_res_gas_othU$X2008 = L113_in_pct_USA_res_gas_othU$X2005

#Melt dataframe so that the years are in a single column
L113_in_pct_USA_res_gas_othU.melt <- melt( L113_in_pct_USA_res_gas_othU, id.vars = c( "service", "fuel" ) )

#Rbind RECS databases into two dataframes (those with subregion13, and those without)
L113_in_EJ_res_gas_U_RECS_sR9 <- rbind( L113_in_EJ_res_gas_U_1979_RECS, L113_in_EJ_res_gas_U_1990_RECS )
L113_in_EJ_res_gas_U_RECS_sR13 <- rbind( L113_in_EJ_res_gas_U_1993_RECS, L113_in_EJ_res_gas_U_1997_RECS,
      L113_in_EJ_res_gas_U_2001_RECS, L113_in_EJ_res_gas_U_2005_RECS )

#Disaggregate RECS "appliances and other" into specified services in each of the binded RECS databases
L113_in_EJ_res_gas_U_RECS_sR9$apl <- L113_in_EJ_res_gas_U_RECS_sR9$apl_other * L113_in_pct_USA_res_gas_othU.melt$value[
      match( paste( "resid appliances", L113_in_EJ_res_gas_U_RECS_sR9$Xyear ),
             paste( L113_in_pct_USA_res_gas_othU.melt$service, L113_in_pct_USA_res_gas_othU.melt$variable ) ) ]
L113_in_EJ_res_gas_U_RECS_sR9$oth <- L113_in_EJ_res_gas_U_RECS_sR9$apl_other * L113_in_pct_USA_res_gas_othU.melt$value[
      match( paste( "resid other", L113_in_EJ_res_gas_U_RECS_sR9$Xyear ),
             paste( L113_in_pct_USA_res_gas_othU.melt$service, L113_in_pct_USA_res_gas_othU.melt$variable ) ) ]

L113_in_EJ_res_gas_U_RECS_sR13$apl <- L113_in_EJ_res_gas_U_RECS_sR13$apl_other * L113_in_pct_USA_res_gas_othU.melt$value[
      match( paste( "resid appliances", L113_in_EJ_res_gas_U_RECS_sR13$Xyear ),
             paste( L113_in_pct_USA_res_gas_othU.melt$service, L113_in_pct_USA_res_gas_othU.melt$variable ) ) ]
L113_in_EJ_res_gas_U_RECS_sR13$oth <- L113_in_EJ_res_gas_U_RECS_sR13$apl_other * L113_in_pct_USA_res_gas_othU.melt$value[
      match( paste( "resid other", L113_in_EJ_res_gas_U_RECS_sR13$Xyear ),
             paste( L113_in_pct_USA_res_gas_othU.melt$service, L113_in_pct_USA_res_gas_othU.melt$variable ) ) ]

# 2c. CALCULATION OF END USE PROPORTIONS BY AVAILABLE SUBREGIONS AND RECS YEAR
#Aggregate by subregion (9 or 13)
printlog( "Aggregating RECS data by available subregions to calculate end-use proportions" )
L113_in_EJ_sR9_res_gas_U_1979_1990 <- aggregate( L113_in_EJ_res_gas_U_RECS_sR9[ RECS_services_gas ],
      list( subregion9 = L113_in_EJ_res_gas_U_RECS_sR9$subregion9, Xyear = L113_in_EJ_res_gas_U_RECS_sR9$Xyear ), sum )
L113_in_EJ_sR13_res_gas_U_1993_2005 <- aggregate( L113_in_EJ_res_gas_U_RECS_sR13[ RECS_services_gas ],
      list( subregion13 = L113_in_EJ_res_gas_U_RECS_sR13$subregion13, Xyear = L113_in_EJ_res_gas_U_RECS_sR13$Xyear ), sum )

#Calculate the end-use proportions from each of these databases
L113_in_pct_sR9_res_gas_U_1979_1990 <- data.frame(
      subregion9 = L113_in_EJ_sR9_res_gas_U_1979_1990$subregion9,
      Xyear = L113_in_EJ_sR9_res_gas_U_1979_1990$Xyear,
      sweep( L113_in_EJ_sR9_res_gas_U_1979_1990[ RECS_services_gas ], 1, rowSums( L113_in_EJ_sR9_res_gas_U_1979_1990[ RECS_services_gas ] ), "/" ) )
L113_in_pct_sR13_res_gas_U_1993_2005 <- data.frame(
      subregion13 = L113_in_EJ_sR13_res_gas_U_1993_2005$subregion13, Xyear = L113_in_EJ_sR13_res_gas_U_1993_2005$Xyear,
      sweep( L113_in_EJ_sR13_res_gas_U_1993_2005[ RECS_services_gas ], 1, rowSums( L113_in_EJ_sR13_res_gas_U_1993_2005[ RECS_services_gas ] ), "/" ) )

# 2d. EXPANSION OF END USE PROPORTIONS TO LARGE STATES (SUBREGION13) IN EARLY RECS EDITIONS
#Expand the 1979-1990 table from subregion9 to subregion13
printlog( "NOTE: Applying end-use proportions by subregion9 to subregion13 in early RECS editions" )
L113_in_pct_sR13_res_gas_U_1979_1990 <- data.frame(
      subregion13 = rep( unique( Census_state_division$subregion13 ), times = length( unique( L113_in_pct_sR9_res_gas_U_1979_1990$Xyear ) ) ),
      Xyear = sort( rep( unique( L113_in_pct_sR9_res_gas_U_1979_1990$Xyear ), times = length( unique( Census_state_division$subregion13 ) ) ) ) )      
L113_in_pct_sR13_res_gas_U_1979_1990$subregion9 <- Census_state_division$subregion9[
      match( L113_in_pct_sR13_res_gas_U_1979_1990$subregion13, Census_state_division$subregion13 ) ]
L113_in_pct_sR13_res_gas_U_1979_1990[ RECS_services_gas ] <- L113_in_pct_sR9_res_gas_U_1979_1990[ 
      match( paste( L113_in_pct_sR13_res_gas_U_1979_1990$subregion9, L113_in_pct_sR13_res_gas_U_1979_1990$Xyear ),
             paste( L113_in_pct_sR9_res_gas_U_1979_1990$subregion9, L113_in_pct_sR9_res_gas_U_1979_1990$Xyear ) ),
      RECS_services_gas ]

#Melt the tables so that services are a column
printlog( "Combining all RECS years into a single table with years as columns" )
L113_in_pct_sR13_res_gas_U_1979_1990 <- L113_in_pct_sR13_res_gas_U_1979_1990[ c( "subregion13", "Xyear", RECS_services_gas ) ]
L113_in_pct_sR13_res_gas_U_1979_1990.melt <- melt( L113_in_pct_sR13_res_gas_U_1979_1990, id.vars = c( "subregion13", "Xyear" ) )
L113_in_pct_sR13_res_gas_U_1993_2005.melt <- melt( L113_in_pct_sR13_res_gas_U_1993_2005, id.vars = c( "subregion13", "Xyear" ) )

#Combine the two tables (rbind)
L113_in_pct_sR13_res_gas_U_Yrecs.melt <- rbind( L113_in_pct_sR13_res_gas_U_1979_1990.melt, L113_in_pct_sR13_res_gas_U_1993_2005.melt )

#Cast so that years are columns
L113_in_pct_sR13_res_gas_U_Yrecs <- cast( L113_in_pct_sR13_res_gas_U_Yrecs.melt, subregion13 + variable ~ Xyear )
names( L113_in_pct_sR13_res_gas_U_Yrecs )[ names( L113_in_pct_sR13_res_gas_U_Yrecs ) == "variable" ] <- "RECS_service"

# 2e. EXPANSION OF END USE PROPORTIONS TO ALL BASE YEARS
#Interpolate to the specified base years
printlog( "NOTE: Interpolating (linearly) from available RECS years to specified base years" )
L113_in_pct_sR13_res_gas_U <- gcam_interp( L113_in_pct_sR13_res_gas_U_Yrecs, base_years )

#For years outside the RECS, use the closest year within the database
printlog( "NOTE: Using closest available year for specified base years outside the range of RECS years" )
L113_in_pct_sR13_res_gas_U$X1975 <- L113_in_pct_sR13_res_gas_U$X1979
L113_in_pct_sR13_res_gas_U$X2008 <- L113_in_pct_sR13_res_gas_U$X2005

#Re-order columns and drop the years that aren't part of the specified base years
L113_in_pct_sR13_res_gas_U <- L113_in_pct_sR13_res_gas_U[ c( "subregion13", "RECS_service", X_base_years ) ]

#Revert "casted" data frame to a normal data frame
#  see https://stat.ethz.ch/pipermail/r-help/2009-January/185755.html
L113_in_pct_sR13_res_gas_U <- as.data.frame( L113_in_pct_sR13_res_gas_U )

#Apply percentages to states, using percentages of the respective subregions
printlog( "Applying subregion-level service proportions to constituent states" )
# Melt table of percentages by subregion13, service, and year
L113_in_pct_sR13_res_gas_U.melt <- melt( L113_in_pct_sR13_res_gas_U, id.vars = c( "subregion13", "RECS_service" ) )

# 2f. END USE PROPORTIONS BY STATE
#Generate table with all combinations of state, RECS_service, and Xyear
L113_in_pct_state_res_gas_U.melt <- data.frame(
      state = rep( states, times = length( RECS_services_gas ) * length( X_base_years ) ),
      RECS_service = rep( sort( rep( RECS_services_gas, times = length( states ) ) ), times = length( X_base_years ) ), 
      Xyear = sort( rep( X_base_years, times = length( states ) * length( RECS_services_gas ) ) ),
      value = 0 )
L113_in_pct_state_res_gas_U.melt$subregion13 <- Census_state_division$subregion13[
      match( L113_in_pct_state_res_gas_U.melt$state, Census_state_division$state ) ]

#Match in the end-use proportion generic to the subregion13
L113_in_pct_state_res_gas_U.melt$value <- L113_in_pct_sR13_res_gas_U.melt$value[
      match( paste( L113_in_pct_state_res_gas_U.melt$subregion13, L113_in_pct_state_res_gas_U.melt$RECS_service,
                    L113_in_pct_state_res_gas_U.melt$Xyear ),
             paste( L113_in_pct_sR13_res_gas_U.melt$subregion13, L113_in_pct_sR13_res_gas_U.melt$RECS_service,
                    L113_in_pct_sR13_res_gas_U.melt$variable ) ) ]

# 2g. ADJUSTMENT AND RE-NORMALIZATION OF OF END USE PROPORTIONS BY STATE FOR HEATING AND COOLING DEGREE DAYS
#Cast the table so that years are columns. This will build a data frame that can be multiplied by the HDD adjustment table.
printlog( "Adjusting service proportions by states within subregions according to relative HDD differences" )
L113_in_pct_state_res_gas_U <- cast( L113_in_pct_state_res_gas_U.melt, state + RECS_service ~ Xyear )

#Subset the heating percentages for adjustment from subregion13 to the specific state
L113_in_pct_state_res_gas_sph <- L113_in_pct_state_res_gas_U[ L113_in_pct_state_res_gas_U$RECS_service == "sph", ]

#Multiply these percentages by the state-specific adjustment factors for heating
L113_in_pct_state_res_gas_sph_adj <- data.frame( L113_in_pct_state_res_gas_sph[ c( "state", "RECS_service" ) ],
      L113_in_pct_state_res_gas_sph[ X_base_years ] * L110_HDD_state_sR13_adj[ X_base_years ] )

#Match these adjusted portional allocations into the original table, which will no longer have its service shares normalized
L113_in_pct_state_res_gas_U_adj_unnorm <- L113_in_pct_state_res_gas_U
L113_in_pct_state_res_gas_U_adj_unnorm[ L113_in_pct_state_res_gas_U_adj_unnorm$RECS_service == "sph", X_base_years ] <-
      L113_in_pct_state_res_gas_sph_adj[ X_base_years ]

#Aggregate by services to calculate state-specific divisors for re-normalizing allocations
printlog( "Re-normalizing end-use proportions" )
L113_in_pctnorm_state_res_gas_U <- aggregate( L113_in_pct_state_res_gas_U_adj_unnorm[ X_base_years ],
      list( state = L113_in_pct_state_res_gas_U_adj_unnorm$state ), sum )
L113_in_pctnorm_state_res_gas_U_repU <- L113_in_pctnorm_state_res_gas_U[ rep( 1:nrow( L113_in_pctnorm_state_res_gas_U ),
      length.out = nrow( L113_in_pct_state_res_gas_U_adj_unnorm ) ), ]
L113_in_pctnorm_state_res_gas_U_repU <- L113_in_pctnorm_state_res_gas_U_repU[ order( L113_in_pctnorm_state_res_gas_U_repU$state ), ]

#Re-normalize to return the final service allocations by state
L113_in_pct_state_res_gas_U_adj <- data.frame( L113_in_pct_state_res_gas_U_adj_unnorm[ c( "state", "RECS_service" ) ],
      L113_in_pct_state_res_gas_U_adj_unnorm[ X_base_years ] / L113_in_pctnorm_state_res_gas_U_repU[ X_base_years ] )
L113_in_pct_state_res_gas_U_adj$service <- GCAM_res_services$service[ match( L113_in_pct_state_res_gas_U_adj$RECS_service, GCAM_res_services$RECS_service ) ]      

#2h. FINAL ENERGY BY STATE, END USE, AND BASE YEAR
#Final energy table: repeat gas consumption by number of services, and multiply by portional allocations
printlog( "Multiplying adjusted end-use proportions by state-level residential gas (final table)" )
L113_in_EJ_state_res_gas <- L104_in_EJ_state_bld_F[ L104_in_EJ_state_bld_F$GCAM_sector == "resid" & L104_in_EJ_state_bld_F$GCAM_fuel == "gas", ]
L113_in_EJ_state_res_gas_repU <- L113_in_EJ_state_res_gas[ rep( 1:nrow( L113_in_EJ_state_res_gas ), times = length( RECS_services_gas ) ), ]
L113_in_EJ_state_res_gas_repU <- L113_in_EJ_state_res_gas_repU[ order( L113_in_EJ_state_res_gas_repU$state ), ]
L113_in_EJ_state_res_gas_U <- data.frame( L113_in_EJ_state_res_gas_repU[ state_S_F ],
      service = L113_in_pct_state_res_gas_U_adj$service,
      L113_in_pct_state_res_gas_U_adj[ X_base_years ] * L113_in_EJ_state_res_gas_repU[ X_base_years ] )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L113_in_EJ_state_res_gas_U <- c( "Residential building gas consumption by state and service","Unit = EJ" )

#write tables as CSV files
writedata( L113_in_EJ_state_res_gas_U,fn="L113_in_EJ_state_res_gas_U", comments=comments.L113_in_EJ_state_res_gas_U )

# Every script should finish with this line
logstop()