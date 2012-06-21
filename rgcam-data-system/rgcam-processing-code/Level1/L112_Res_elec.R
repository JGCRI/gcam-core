# L112_Res_elec.R
# RESIDENTIAL SECTOR ELECTRICITY CONSUMPTION BY STATE, SERVICE, AND BASE YEAR
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L112_Res_elec.R" )
printlog( "Residential sector electricity consumption by state and service" )
                  
# -----------------------------------------------------------------------------
# 1. Read files

Census_state_division <- readmap( "Census_state_division" )
GCAM_res_services <- readmap( "GCAM_res_services" )
USA_res_in <- readdata( "USA_res_in" )
L104_in_EJ_state_bld_F <- readdata( "L104_in_EJ_state_bld_F" )
L110_CDD_state_sR13_adj <- readdata( "L110_CDD_state_sR13_adj" )
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
printlog( "Multiplying RECS electricity consumption by end use by building sampling weights" )
#Calculate electricity demands by available end uses in each RECS
#1979 is converted from Tbtu to EJ
L112_in_EJ_res_elec_U_1979_RECS <- data.frame(
      subregion9 = L111_RECS_1979$subregion9,
      Xyear = "X1979",
      sph = L111_RECS_1979$TBTUELSPH * conv_Tbtu_EJ,
      col = L111_RECS_1979$TBTUELCOL * conv_Tbtu_EJ,
      wth = L111_RECS_1979$TBTUELWTH * conv_Tbtu_EJ,
      apl_other = L111_RECS_1979$TBTUELAPL * conv_Tbtu_EJ )

#1990 through 2005 databases are multiplied by sampling weights and then converted from kbtu to EJ
L112_in_EJ_res_elec_U_1990_RECS <- data.frame(
      subregion9 = L111_RECS_1990$subregion9,
      Xyear = "X1990",
      sph = L111_RECS_1990$BTUELSPH * conv_kbtu_EJ * L111_RECS_1990$NWEIGHT,
      col = L111_RECS_1990$BTUELCOL * conv_kbtu_EJ * L111_RECS_1990$NWEIGHT,
      wth = L111_RECS_1990$BTUELWTH * conv_kbtu_EJ * L111_RECS_1990$NWEIGHT,
      apl_other = ( L111_RECS_1990$BTUELAPL + L111_RECS_1990$BTUELRFG ) * conv_kbtu_EJ * L111_RECS_1990$NWEIGHT )

L112_in_EJ_res_elec_U_1993_RECS <- data.frame(
      subregion13 = L111_RECS_1993$subregion13,
      Xyear = "X1993",
      sph = L111_RECS_1993$BTUELSPH * conv_kbtu_EJ * L111_RECS_1993$NWEIGHT,
      col = L111_RECS_1993$BTUELCOL * conv_kbtu_EJ * L111_RECS_1993$NWEIGHT,
      wth = L111_RECS_1993$BTUELWTH * conv_kbtu_EJ * L111_RECS_1993$NWEIGHT,
      apl_other = ( L111_RECS_1993$BTUELAPL + L111_RECS_1993$BTUELRFG ) * conv_kbtu_EJ * L111_RECS_1993$NWEIGHT )

L112_in_EJ_res_elec_U_1997_RECS <- data.frame(
      subregion13 = L111_RECS_1997$subregion13,
      Xyear = "X1997",
      sph = L111_RECS_1997$BTUELSPH * conv_kbtu_EJ * L111_RECS_1997$NWEIGHT,
      col = L111_RECS_1997$BTUELCOL * conv_kbtu_EJ * L111_RECS_1997$NWEIGHT,
      wth = L111_RECS_1997$BTUELWTH * conv_kbtu_EJ * L111_RECS_1997$NWEIGHT,
      apl_other = ( L111_RECS_1997$BTUELAPL + L111_RECS_1997$BTUELRFG ) * conv_kbtu_EJ * L111_RECS_1997$NWEIGHT )

L112_in_EJ_res_elec_U_2001_RECS <- data.frame(
      subregion13 = L111_RECS_2001$subregion13,
      Xyear = "X2001",
      sph = L111_RECS_2001$BTUELSPH * conv_kbtu_EJ * L111_RECS_2001$NWEIGHT,
      col = L111_RECS_2001$BTUELCOL * conv_kbtu_EJ * L111_RECS_2001$NWEIGHT,
      wth = L111_RECS_2001$BTUELWTH * conv_kbtu_EJ * L111_RECS_2001$NWEIGHT,
      apl_other = ( L111_RECS_2001$BTUELAPL + L111_RECS_2001$BTUELRFG ) * conv_kbtu_EJ * L111_RECS_2001$NWEIGHT )

L112_in_EJ_res_elec_U_2005_RECS <- data.frame(
      subregion13 = L111_RECS_2005$subregion13,
      Xyear = "X2005",
      sph = L111_RECS_2005$BTUELSPH * conv_kbtu_EJ * L111_RECS_2005$NWEIGHT,
      col = L111_RECS_2005$BTUELCOL * conv_kbtu_EJ * L111_RECS_2005$NWEIGHT,
      wth = L111_RECS_2005$BTUELWTH * conv_kbtu_EJ * L111_RECS_2005$NWEIGHT,
      apl_other = ( L111_RECS_2005$BTUELAPL + L111_RECS_2005$BTUELRFG ) * conv_kbtu_EJ * L111_RECS_2005$NWEIGHT )

# 2b. DISAGGREGATION OF RECS "APPLIANCE AND OTHER" ENERGY INTO SPECIFIED GCAM SERVICES
#Calculate national percentage allocations of "appliance and other" electricity to lighting, large appliances, other appliances, and other
printlog( "Disaggregating appliances and other energy into specified services (4)" )
printlog( "NOTE: Assuming constant national shares for this disaggregation" )
L112_in_EJ_USA_res_elec_othU <- USA_res_in[ USA_res_in$service %in% GCAM_res_otherU & USA_res_in$fuel == "elect_td_bld", ] 
L112_in_pct_USA_res_elec_othU <- data.frame( L112_in_EJ_USA_res_elec_othU[ c( "service", "fuel" ) ],
      sweep( L112_in_EJ_USA_res_elec_othU[ names( L112_in_EJ_USA_res_elec_othU ) %in% X_RECS_years ], 2,
             colSums( L112_in_EJ_USA_res_elec_othU[ names( L112_in_EJ_USA_res_elec_othU ) %in% X_RECS_years ] ), "/" ) )

#Add years corresponding to RECS data that isn't available in provided table
L112_in_pct_USA_res_elec_othU$X1979 = L112_in_pct_USA_res_elec_othU$X1990
L112_in_pct_USA_res_elec_othU$X2008 = L112_in_pct_USA_res_elec_othU$X2005

#Melt dataframe so that the years are in a single column
L112_in_pct_USA_res_elec_othU.melt <- melt( L112_in_pct_USA_res_elec_othU, id.vars = c( "service", "fuel" ) )

#Rbind RECS databases into two dataframes (those with subregion13, and those without)
L112_in_EJ_res_elec_U_RECS_sR9 <- rbind( L112_in_EJ_res_elec_U_1979_RECS, L112_in_EJ_res_elec_U_1990_RECS )
L112_in_EJ_res_elec_U_RECS_sR13 <- rbind( L112_in_EJ_res_elec_U_1993_RECS, L112_in_EJ_res_elec_U_1997_RECS,
      L112_in_EJ_res_elec_U_2001_RECS, L112_in_EJ_res_elec_U_2005_RECS )

#Disaggregate RECS "appliances and other" into specified services in each of the binded RECS databases
L112_in_EJ_res_elec_U_RECS_sR9$lgt <- L112_in_EJ_res_elec_U_RECS_sR9$apl_other * L112_in_pct_USA_res_elec_othU.melt$value[
      match( paste( "resid lighting", L112_in_EJ_res_elec_U_RECS_sR9$Xyear ),
             paste( L112_in_pct_USA_res_elec_othU.melt$service, L112_in_pct_USA_res_elec_othU.melt$variable ) ) ]
L112_in_EJ_res_elec_U_RECS_sR9$apl <- L112_in_EJ_res_elec_U_RECS_sR9$apl_other * L112_in_pct_USA_res_elec_othU.melt$value[
      match( paste( "resid appliances", L112_in_EJ_res_elec_U_RECS_sR9$Xyear ),
             paste( L112_in_pct_USA_res_elec_othU.melt$service, L112_in_pct_USA_res_elec_othU.melt$variable ) ) ]
L112_in_EJ_res_elec_U_RECS_sR9$othapl <- L112_in_EJ_res_elec_U_RECS_sR9$apl_other * L112_in_pct_USA_res_elec_othU.melt$value[
      match( paste( "resid other appliances", L112_in_EJ_res_elec_U_RECS_sR9$Xyear ),
             paste( L112_in_pct_USA_res_elec_othU.melt$service, L112_in_pct_USA_res_elec_othU.melt$variable ) ) ]
L112_in_EJ_res_elec_U_RECS_sR9$oth <- L112_in_EJ_res_elec_U_RECS_sR9$apl_other * L112_in_pct_USA_res_elec_othU.melt$value[
      match( paste( "resid other", L112_in_EJ_res_elec_U_RECS_sR9$Xyear ),
             paste( L112_in_pct_USA_res_elec_othU.melt$service, L112_in_pct_USA_res_elec_othU.melt$variable ) ) ]

L112_in_EJ_res_elec_U_RECS_sR13$lgt <- L112_in_EJ_res_elec_U_RECS_sR13$apl_other * L112_in_pct_USA_res_elec_othU.melt$value[
      match( paste( "resid lighting", L112_in_EJ_res_elec_U_RECS_sR13$Xyear ),
             paste( L112_in_pct_USA_res_elec_othU.melt$service, L112_in_pct_USA_res_elec_othU.melt$variable ) ) ]
L112_in_EJ_res_elec_U_RECS_sR13$apl <- L112_in_EJ_res_elec_U_RECS_sR13$apl_other * L112_in_pct_USA_res_elec_othU.melt$value[
      match( paste( "resid appliances", L112_in_EJ_res_elec_U_RECS_sR13$Xyear ),
             paste( L112_in_pct_USA_res_elec_othU.melt$service, L112_in_pct_USA_res_elec_othU.melt$variable ) ) ]
L112_in_EJ_res_elec_U_RECS_sR13$othapl <- L112_in_EJ_res_elec_U_RECS_sR13$apl_other * L112_in_pct_USA_res_elec_othU.melt$value[
      match( paste( "resid other appliances", L112_in_EJ_res_elec_U_RECS_sR13$Xyear ),
             paste( L112_in_pct_USA_res_elec_othU.melt$service, L112_in_pct_USA_res_elec_othU.melt$variable ) ) ]
L112_in_EJ_res_elec_U_RECS_sR13$oth <- L112_in_EJ_res_elec_U_RECS_sR13$apl_other * L112_in_pct_USA_res_elec_othU.melt$value[
      match( paste( "resid other", L112_in_EJ_res_elec_U_RECS_sR13$Xyear ),
             paste( L112_in_pct_USA_res_elec_othU.melt$service, L112_in_pct_USA_res_elec_othU.melt$variable ) ) ]

# 2c. CALCULATION OF END USE PROPORTIONS BY AVAILABLE SUBREGIONS AND RECS YEAR
#Aggregate by subregion (9 or 13)
printlog( "Aggregating RECS data by available subregions to calculate end-use proportions" )
#   1979 table is already aggregated; just need to drop the "appliances and other" column
L112_in_EJ_sR9_res_elec_U_1979_1990 <- aggregate( L112_in_EJ_res_elec_U_RECS_sR9[ RECS_services_elec ],
      list( subregion9 = L112_in_EJ_res_elec_U_RECS_sR9$subregion9, Xyear = L112_in_EJ_res_elec_U_RECS_sR9$Xyear ), sum )
L112_in_EJ_sR13_res_elec_U_1993_2005 <- aggregate( L112_in_EJ_res_elec_U_RECS_sR13[ RECS_services_elec ],
      list( subregion13 = L112_in_EJ_res_elec_U_RECS_sR13$subregion13, Xyear = L112_in_EJ_res_elec_U_RECS_sR13$Xyear ), sum )

#Calculate the end-use proportions from each of these databases
L112_in_pct_sR9_res_elec_U_1979_1990 <- data.frame(
      subregion9 = L112_in_EJ_sR9_res_elec_U_1979_1990$subregion9,
      Xyear = L112_in_EJ_sR9_res_elec_U_1979_1990$Xyear,
      sweep( L112_in_EJ_sR9_res_elec_U_1979_1990[ RECS_services_elec ], 1, rowSums( L112_in_EJ_sR9_res_elec_U_1979_1990[ RECS_services_elec ] ), "/" ) )
L112_in_pct_sR13_res_elec_U_1993_2005 <- data.frame(
      subregion13 = L112_in_EJ_sR13_res_elec_U_1993_2005$subregion13, Xyear = L112_in_EJ_sR13_res_elec_U_1993_2005$Xyear,
      sweep( L112_in_EJ_sR13_res_elec_U_1993_2005[ RECS_services_elec ], 1, rowSums( L112_in_EJ_sR13_res_elec_U_1993_2005[ RECS_services_elec ] ), "/" ) )

# 2d. EXPANSION OF END USE PROPORTIONS TO LARGE STATES (SUBREGION13) IN EARLY RECS EDITIONS
#Expand the 1979-1990 table from subregion9 to subregion13
printlog( "NOTE: Applying end-use proportions by subregion9 to subregion13 in early RECS editions" )
L112_in_pct_sR13_res_elec_U_1979_1990 <- data.frame(
      subregion13 = rep( unique( Census_state_division$subregion13 ), times = length( unique( L112_in_pct_sR9_res_elec_U_1979_1990$Xyear ) ) ),
      Xyear = sort( rep( unique( L112_in_pct_sR9_res_elec_U_1979_1990$Xyear ), times = length( unique( Census_state_division$subregion13 ) ) ) ) )      
L112_in_pct_sR13_res_elec_U_1979_1990$subregion9 <- Census_state_division$subregion9[
      match( L112_in_pct_sR13_res_elec_U_1979_1990$subregion13, Census_state_division$subregion13 ) ]
L112_in_pct_sR13_res_elec_U_1979_1990[ RECS_services_elec ] <- L112_in_pct_sR9_res_elec_U_1979_1990[ 
      match( paste( L112_in_pct_sR13_res_elec_U_1979_1990$subregion9, L112_in_pct_sR13_res_elec_U_1979_1990$Xyear ),
             paste( L112_in_pct_sR9_res_elec_U_1979_1990$subregion9, L112_in_pct_sR9_res_elec_U_1979_1990$Xyear ) ),
      RECS_services_elec ]

#Melt the tables so that services are a column
printlog( "Combining all RECS years into a single table with years as columns" )
L112_in_pct_sR13_res_elec_U_1979_1990 <- L112_in_pct_sR13_res_elec_U_1979_1990[ c( "subregion13", "Xyear", RECS_services_elec ) ]
L112_in_pct_sR13_res_elec_U_1979_1990.melt <- melt( L112_in_pct_sR13_res_elec_U_1979_1990, id.vars = c( "subregion13", "Xyear" ) )
L112_in_pct_sR13_res_elec_U_1993_2005.melt <- melt( L112_in_pct_sR13_res_elec_U_1993_2005, id.vars = c( "subregion13", "Xyear" ) )

#Combine the two tables (rbind)
L112_in_pct_sR13_res_elec_U_Yrecs.melt <- rbind( L112_in_pct_sR13_res_elec_U_1979_1990.melt, L112_in_pct_sR13_res_elec_U_1993_2005.melt )

#Cast so that years are columns
L112_in_pct_sR13_res_elec_U_Yrecs <- cast( L112_in_pct_sR13_res_elec_U_Yrecs.melt, subregion13 + variable ~ Xyear )
names( L112_in_pct_sR13_res_elec_U_Yrecs )[ names( L112_in_pct_sR13_res_elec_U_Yrecs ) == "variable" ] <- "RECS_service"

# 2e. EXPANSION OF END USE PROPORTIONS TO ALL BASE YEARS
#Interpolate to the specified base years
printlog( "NOTE: Interpolating (linearly) from available RECS years to specified base years" )
L112_in_pct_sR13_res_elec_U <- gcam_interp( L112_in_pct_sR13_res_elec_U_Yrecs, base_years )

#For years outside the RECS, use the closest year within the database
printlog( "NOTE: Using closest available year for specified base years outside the range of RECS years" )
L112_in_pct_sR13_res_elec_U$X1975 <- L112_in_pct_sR13_res_elec_U$X1979
L112_in_pct_sR13_res_elec_U$X2008 <- L112_in_pct_sR13_res_elec_U$X2005

#Re-order columns and drop the years that aren't part of the specified base years
L112_in_pct_sR13_res_elec_U <- L112_in_pct_sR13_res_elec_U[ c( "subregion13", "RECS_service", X_base_years ) ]

#Revert "casted" data frame to a normal data frame
#  see https://stat.ethz.ch/pipermail/r-help/2009-January/185755.html
L112_in_pct_sR13_res_elec_U <- as.data.frame( L112_in_pct_sR13_res_elec_U )

#Apply percentages to states, using percentages of the respective subregions
printlog( "Applying subregion-level service proportions to constituent states" )
# Melt table of percentages by subregion13, service, and year
L112_in_pct_sR13_res_elec_U.melt <- melt( L112_in_pct_sR13_res_elec_U, id.vars = c( "subregion13", "RECS_service" ) )

# 2f. END USE PROPORTIONS BY STATE
#Generate table with all combinations of state, RECS_service, and Xyear
L112_in_pct_state_res_elec_U.melt <- data.frame(
      state = rep( states, times = length( RECS_services_elec ) * length( X_base_years ) ),
      RECS_service = rep( sort( rep( RECS_services_elec, times = length( states ) ) ), times = length( X_base_years ) ), 
      Xyear = sort( rep( X_base_years, times = length( states ) * length( RECS_services_elec ) ) ),
      value = 0 )
L112_in_pct_state_res_elec_U.melt$subregion13 <- Census_state_division$subregion13[
      match( L112_in_pct_state_res_elec_U.melt$state, Census_state_division$state ) ]

#Match in the end-use proportion generic to the subregion13
L112_in_pct_state_res_elec_U.melt$value <- L112_in_pct_sR13_res_elec_U.melt$value[
      match( paste( L112_in_pct_state_res_elec_U.melt$subregion13, L112_in_pct_state_res_elec_U.melt$RECS_service,
                    L112_in_pct_state_res_elec_U.melt$Xyear ),
             paste( L112_in_pct_sR13_res_elec_U.melt$subregion13, L112_in_pct_sR13_res_elec_U.melt$RECS_service,
                    L112_in_pct_sR13_res_elec_U.melt$variable ) ) ]

# 2g. ADJUSTMENT AND RE-NORMALIZATION OF OF END USE PROPORTIONS BY STATE FOR HEATING AND COOLING DEGREE DAYS
#Cast the table so that years are columns. This will build a data frame that can be multiplied by the HDD/CDD adjustment tables.
printlog( "Adjusting service proportions by states within subregions according to relative HDD and CDD differences" )
L112_in_pct_state_res_elec_U <- cast( L112_in_pct_state_res_elec_U.melt, state + RECS_service ~ Xyear )

#Subset the heating and cooling percentages for adjustment from subregion13 to the specific state
L112_in_pct_state_res_elec_sph <- L112_in_pct_state_res_elec_U[ L112_in_pct_state_res_elec_U$RECS_service == "sph", ]
L112_in_pct_state_res_elec_col <- L112_in_pct_state_res_elec_U[ L112_in_pct_state_res_elec_U$RECS_service == "col", ]

#Multiply these percentages by the state-wise adjustment factors for heating and cooling
L112_in_pct_state_res_elec_sph_adj <- data.frame( L112_in_pct_state_res_elec_sph[ c( "state", "RECS_service" ) ],
      L112_in_pct_state_res_elec_sph[ X_base_years ] * L110_HDD_state_sR13_adj[ X_base_years ] )
L112_in_pct_state_res_elec_col_adj <- data.frame( L112_in_pct_state_res_elec_col[ c( "state", "RECS_service" ) ],
      L112_in_pct_state_res_elec_col[ X_base_years ] * L110_CDD_state_sR13_adj[ X_base_years ] )

#Match these adjusted portional allocations into the original table, which will no longer have its service shares normalized
L112_in_pct_state_res_elec_U_adj_unnorm <- L112_in_pct_state_res_elec_U
L112_in_pct_state_res_elec_U_adj_unnorm[ L112_in_pct_state_res_elec_U_adj_unnorm$RECS_service == "sph", X_base_years ] <-
      L112_in_pct_state_res_elec_sph_adj[ X_base_years ]
L112_in_pct_state_res_elec_U_adj_unnorm[ L112_in_pct_state_res_elec_U_adj_unnorm$RECS_service == "col", X_base_years ] <-
      L112_in_pct_state_res_elec_col_adj[ X_base_years ]

#Aggregate by services to calculate state-specific divisors for re-normalizing allocations
printlog( "Re-normalizing end-use proportions" )
L112_in_pctnorm_state_res_elec_U <- aggregate( L112_in_pct_state_res_elec_U_adj_unnorm[ X_base_years ],
      list( state = L112_in_pct_state_res_elec_U_adj_unnorm$state ), sum )
L112_in_pctnorm_state_res_elec_U_repU <- L112_in_pctnorm_state_res_elec_U[ rep( 1:nrow( L112_in_pctnorm_state_res_elec_U ),
      length.out = nrow( L112_in_pct_state_res_elec_U_adj_unnorm ) ), ]
L112_in_pctnorm_state_res_elec_U_repU <- L112_in_pctnorm_state_res_elec_U_repU[ order( L112_in_pctnorm_state_res_elec_U_repU$state ), ]

#Re-normalize to return the final service allocations by state
L112_in_pct_state_res_elec_U_adj <- data.frame( L112_in_pct_state_res_elec_U_adj_unnorm[ c( "state", "RECS_service" ) ],
      L112_in_pct_state_res_elec_U_adj_unnorm[ X_base_years ] / L112_in_pctnorm_state_res_elec_U_repU[ X_base_years ] )
L112_in_pct_state_res_elec_U_adj$service <- GCAM_res_services$service[ match( L112_in_pct_state_res_elec_U_adj$RECS_service, GCAM_res_services$RECS_service ) ]      

#2h. FINAL ENERGY BY STATE, END USE, AND BASE YEAR
#Final energy table: repeat electricity consumption by number of services, and multiply by portional allocations
printlog( "Multiplying adjusted end-use proportions by state-level residential electricity (final table)" )
L112_in_EJ_state_res_elec <- L104_in_EJ_state_bld_F[ L104_in_EJ_state_bld_F$GCAM_sector == "resid" & L104_in_EJ_state_bld_F$GCAM_fuel == "electricity", ]
L112_in_EJ_state_res_elec_repU <- L112_in_EJ_state_res_elec[ rep( 1:nrow( L112_in_EJ_state_res_elec ), times = length( RECS_services_elec ) ), ]
L112_in_EJ_state_res_elec_repU <- L112_in_EJ_state_res_elec_repU[ order( L112_in_EJ_state_res_elec_repU$state ), ]
L112_in_EJ_state_res_elec_U <- data.frame( L112_in_EJ_state_res_elec_repU[ state_S_F ],
      service = L112_in_pct_state_res_elec_U_adj$service,
      L112_in_pct_state_res_elec_U_adj[ X_base_years ] * L112_in_EJ_state_res_elec_repU[ X_base_years ] )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L112_in_EJ_state_res_elec_U <- c( "Residential building electricity consumption by state and service","Unit = EJ" )

#write tables as CSV files
writedata( L112_in_EJ_state_res_elec_U,fn="L112_in_EJ_state_res_elec_U", comments=comments.L112_in_EJ_state_res_elec_U )

# Every script should finish with this line
logstop()