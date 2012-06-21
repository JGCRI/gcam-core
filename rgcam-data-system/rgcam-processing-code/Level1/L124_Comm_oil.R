# L124_Comm_oil.R
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L124_Comm_oil.R" )
printlog( "Commercial sector oil consumption by state and service" )

# -----------------------------------------------------------------------------
# 1. Read files

Census_state_division <- readmap( "Census_state_division" )
GCAM_comm_services <- readmap( "GCAM_comm_services" )
Census_pop_hist <- readdata( "Census_pop_hist" )
EIA_distheat <- readdata( "EIA_distheat" )
L104_in_EJ_state_bld_F <- readdata( "L104_in_EJ_state_bld_F" )
L110_HDD_state_sR9_adj <- readdata( "L110_HDD_state_sR9_adj" )
L121_CBECS_1992 <- readdata( "L121_CBECS_1992" )
L121_CBECS_1995 <- readdata( "L121_CBECS_1995" )
L121_CBECS_2003 <- readdata( "L121_CBECS_2003" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. PREPARATION OF CBECS DATABASES FOR AGGREGATION
printlog( "Multiplying CBECS oil consumption by end use by building sampling weights" )
#Calculate oil demands by available end uses in each CBECS
# Note that energy use in CBECS is in kbtu, not MMbtu as indicated in the documentation
# Differentiating between direct-fired and district-provided services (sph and wth only)
L124_in_EJ_comm_oil_U_1992_CBECS <- data.frame(
      subregion9 = L121_CBECS_1992$subregion9,
      Xyear = "X1992",
      sph_dir = L121_CBECS_1992$FKHTBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5,
      wth_dir = L121_CBECS_1992$FKWTBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5,
      othint = L121_CBECS_1992$FKMSBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5 )

#Calculate district-provided services, backing the energy out to the fuel inputs to the district heat plants
printlog( "Adding fuel inputs to district services to the respective services" )
printlog( "NOTE: Assuming constant national fuel shares and efficiencies for district services" )
L124_in_EJ_comm_oil_U_1992_CBECS$sph_dh <- L121_CBECS_1992$DHHTBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5 *
      EIA_distheat$share[ EIA_distheat$fuel == "refined liquids" & EIA_distheat$service == "heating" ] /
      EIA_distheat$efficiency[ EIA_distheat$fuel=="refined liquids" & EIA_distheat$service == "heating" ]
L124_in_EJ_comm_oil_U_1992_CBECS$wth_dh <- L121_CBECS_1992$DHHTBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5 *
      EIA_distheat$share[ EIA_distheat$fuel == "refined liquids" & EIA_distheat$service == "hot water" ] /
      EIA_distheat$efficiency[ EIA_distheat$fuel=="refined liquids" & EIA_distheat$service == "hot water" ]

#Add fuel inputs to district services to the relevant services
L124_in_EJ_comm_oil_U_1992_CBECS$sph <- L124_in_EJ_comm_oil_U_1992_CBECS$sph_dir + L124_in_EJ_comm_oil_U_1992_CBECS$sph_dh
L124_in_EJ_comm_oil_U_1992_CBECS$wth <- L124_in_EJ_comm_oil_U_1992_CBECS$wth_dir + L124_in_EJ_comm_oil_U_1992_CBECS$wth_dh

#Repeat for 1995 CBECS
L124_in_EJ_comm_oil_U_1995_CBECS <- data.frame(
      subregion9 = L121_CBECS_1995$subregion9,
      Xyear = "X1995",
      sph_dir = L121_CBECS_1995$FKHTBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6,
      wth_dir = L121_CBECS_1995$FKWTBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6,
      othint = ( L121_CBECS_1995$FKCLBTU6 + L121_CBECS_1995$FKCKBTU6 + L121_CBECS_1995$FKMSBTU6 ) * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6 )

L124_in_EJ_comm_oil_U_1995_CBECS$sph_dh <- L121_CBECS_1995$DHHTBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6 *
      EIA_distheat$share[ EIA_distheat$fuel == "refined liquids" & EIA_distheat$service == "heating" ] /
      EIA_distheat$efficiency[ EIA_distheat$fuel=="refined liquids" & EIA_distheat$service == "heating" ]
L124_in_EJ_comm_oil_U_1995_CBECS$wth_dh <- L121_CBECS_1995$DHHTBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6 *
      EIA_distheat$share[ EIA_distheat$fuel == "refined liquids" & EIA_distheat$service == "hot water" ] /
      EIA_distheat$efficiency[ EIA_distheat$fuel=="refined liquids" & EIA_distheat$service == "hot water" ]

L124_in_EJ_comm_oil_U_1995_CBECS$sph <- L124_in_EJ_comm_oil_U_1995_CBECS$sph_dir + L124_in_EJ_comm_oil_U_1995_CBECS$sph_dh
L124_in_EJ_comm_oil_U_1995_CBECS$wth <- L124_in_EJ_comm_oil_U_1995_CBECS$wth_dir + L124_in_EJ_comm_oil_U_1995_CBECS$wth_dh

#Repeat for 2003 CBECS
L124_in_EJ_comm_oil_U_2003_CBECS <- data.frame(
      subregion9 = L121_CBECS_2003$subregion9,
      Xyear = "X2003",
      sph_dir = L121_CBECS_2003$FKHTBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8,
      wth_dir = L121_CBECS_2003$FKWTBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8,
      othint = ( L121_CBECS_2003$FKCLBTU8 + L121_CBECS_2003$FKCKBTU8 + L121_CBECS_2003$FKMSBTU8 )* conv_kbtu_EJ * L121_CBECS_2003$ADJWT8 )

L124_in_EJ_comm_oil_U_2003_CBECS$sph_dh <- L121_CBECS_2003$DHHTBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8 *
      EIA_distheat$share[ EIA_distheat$fuel == "refined liquids" & EIA_distheat$service == "heating" ] /
      EIA_distheat$efficiency[ EIA_distheat$fuel=="refined liquids" & EIA_distheat$service == "heating" ]
L124_in_EJ_comm_oil_U_2003_CBECS$wth_dh <- L121_CBECS_2003$DHHTBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8 *
      EIA_distheat$share[ EIA_distheat$fuel == "refined liquids" & EIA_distheat$service == "hot water" ] /
      EIA_distheat$efficiency[ EIA_distheat$fuel=="refined liquids" & EIA_distheat$service == "hot water" ]

L124_in_EJ_comm_oil_U_2003_CBECS$sph <- L124_in_EJ_comm_oil_U_2003_CBECS$sph_dir + L124_in_EJ_comm_oil_U_2003_CBECS$sph_dh
L124_in_EJ_comm_oil_U_2003_CBECS$wth <- L124_in_EJ_comm_oil_U_2003_CBECS$wth_dir + L124_in_EJ_comm_oil_U_2003_CBECS$wth_dh

#Rbind CBECS databases into one dataframe
L124_in_EJ_comm_oil_U_CBECS_sR9 <- rbind( L124_in_EJ_comm_oil_U_1992_CBECS, L124_in_EJ_comm_oil_U_1995_CBECS,
      L124_in_EJ_comm_oil_U_2003_CBECS )

# 2b. CALCULATION OF END USE PROPORTIONS BY SUBREGION9 IN CBECS YEARS
#Aggregate full database by subregion9
printlog( "Aggregating CBECS data by available subregions to calculate end-use proportions" )
L124_in_EJ_sR9_comm_oil_U_Ycbecs <- aggregate( L124_in_EJ_comm_oil_U_CBECS_sR9[ CBECS_services_oil ],
      list( subregion9 = L124_in_EJ_comm_oil_U_CBECS_sR9$subregion9,
            Xyear = L124_in_EJ_comm_oil_U_CBECS_sR9$Xyear ), sum )

#Calculate the end-use proportions for commercial buildings energy use
L124_in_pct_sR9_comm_oil_U_Ycbecs <- data.frame(
      subregion9 = L124_in_EJ_sR9_comm_oil_U_Ycbecs$subregion9,
      Xyear = L124_in_EJ_sR9_comm_oil_U_Ycbecs$Xyear,
      sweep( L124_in_EJ_sR9_comm_oil_U_Ycbecs[ CBECS_services_oil ], 1,
             rowSums( L124_in_EJ_sR9_comm_oil_U_Ycbecs[ CBECS_services_oil ] ), "/" ) )

# 2c. EXPANSION OF END USE PROPORTIONS TO ALL BASE YEARS
#Melt and re-cast so that years are columns
L124_in_pct_sR9_comm_oil_U_Ycbecs.melt <- melt( L124_in_pct_sR9_comm_oil_U_Ycbecs, id.vars = c( "subregion9", "Xyear" ) )
L124_in_pct_sR9_comm_oil_U_Ycbecs <- cast( L124_in_pct_sR9_comm_oil_U_Ycbecs.melt, subregion9 + variable ~ Xyear )
names( L124_in_pct_sR9_comm_oil_U_Ycbecs )[ names( L124_in_pct_sR9_comm_oil_U_Ycbecs ) == "variable" ] <- "CBECS_service"

#Interpolate to the specified base years
printlog( "NOTE: Interpolating (linearly) from available CBECS years to specified base years" )
#First, add in 1975 and 2008 to bracket the range
printlog( "NOTE: Using closest available year for specified base years outside the range of CBECS years" )
L124_in_pct_sR9_comm_oil_U_Ycbecs$X1975 <- L124_in_pct_sR9_comm_oil_U_Ycbecs$X1992
L124_in_pct_sR9_comm_oil_U_Ycbecs$X2008 <- L124_in_pct_sR9_comm_oil_U_Ycbecs$X2003
L124_in_pct_sR9_comm_oil_U <- gcam_interp( L124_in_pct_sR9_comm_oil_U_Ycbecs, base_years )

#Re-order columns and drop the years that aren't part of the specified base years
L124_in_pct_sR9_comm_oil_U <- L124_in_pct_sR9_comm_oil_U[ c( "subregion9", "CBECS_service", X_base_years ) ]

#Revert "casted" data frame to a normal data frame
L124_in_pct_sR9_comm_oil_U <- as.data.frame( L124_in_pct_sR9_comm_oil_U )

#Apply percentages to states, using percentages of the respective subregions
printlog( "Applying subregion-level service proportions to constituent states" )
# Melt table of percentages by subregion13, service, and year
L124_in_pct_sR9_comm_oil_U.melt <- melt( L124_in_pct_sR9_comm_oil_U, id.vars = c( "subregion9", "CBECS_service" ) )

# 2d. END USE PROPORTIONS BY STATE
#Generate table with all combinations of state, CBECS_service, and Xyear
L124_in_pct_state_comm_oil_U.melt <- data.frame(
      state = rep( states, times = length( CBECS_services_oil ) * length( X_base_years ) ),
      CBECS_service = rep( sort( rep( CBECS_services_oil, times = length( states ) ) ), times = length( X_base_years ) ), 
      Xyear = sort( rep( X_base_years, times = length( states ) * length( CBECS_services_oil ) ) ),
      value = 0 )
L124_in_pct_state_comm_oil_U.melt$subregion9 <- Census_state_division$subregion9[
      match( L124_in_pct_state_comm_oil_U.melt$state, Census_state_division$state ) ]

#Match in the end-use proportion generic to the subregion9
L124_in_pct_state_comm_oil_U.melt$value <- L124_in_pct_sR9_comm_oil_U.melt$value[
      match( paste( L124_in_pct_state_comm_oil_U.melt$subregion9, L124_in_pct_state_comm_oil_U.melt$CBECS_service,
                    L124_in_pct_state_comm_oil_U.melt$Xyear ),
             paste( L124_in_pct_sR9_comm_oil_U.melt$subregion9, L124_in_pct_sR9_comm_oil_U.melt$CBECS_service,
                    L124_in_pct_sR9_comm_oil_U.melt$variable ) ) ]

# 2e. ADJUSTMENT AND RE-NORMALIZATION OF OF END USE PROPORTIONS BY STATE FOR HEATING AND COOLING DEGREE DAYS
#Cast the table so that years are columns. This will build a data frame that can be multiplied by the HDD/CDD adjustment tables.
printlog( "Adjusting service proportions by states within subregions according to relative HDD and CDD differences" )
L124_in_pct_state_comm_oil_U <- cast( L124_in_pct_state_comm_oil_U.melt, state + CBECS_service ~ Xyear )

#Subset the heating percentages for adjustment from subregion13 to the specific state
L124_in_pct_state_comm_oil_sph <- L124_in_pct_state_comm_oil_U[ L124_in_pct_state_comm_oil_U$CBECS_service == "sph", ]

#Multiply these percentages by the state-wise adjustment factors for heating
L124_in_pct_state_comm_oil_sph_adj <- data.frame( L124_in_pct_state_comm_oil_sph[ c( "state", "CBECS_service" ) ],
      L124_in_pct_state_comm_oil_sph[ X_base_years ] * L110_HDD_state_sR9_adj[ X_base_years ] )

#Match these adjusted portional allocations into the original table, which will no longer have its service shares normalized
L124_in_pct_state_comm_oil_U_adj_unnorm <- L124_in_pct_state_comm_oil_U
L124_in_pct_state_comm_oil_U_adj_unnorm[ L124_in_pct_state_comm_oil_U_adj_unnorm$CBECS_service == "sph", X_base_years ] <-
      L124_in_pct_state_comm_oil_sph_adj[ X_base_years ]

#Aggregate by services to calculate state-specific divisors for re-normalizing allocations
printlog( "Re-normalizing end-use proportions" )
L124_in_pctnorm_state_comm_oil_U <- aggregate( L124_in_pct_state_comm_oil_U_adj_unnorm[ X_base_years ],
      list( state = L124_in_pct_state_comm_oil_U_adj_unnorm$state ), sum )
L124_in_pctnorm_state_comm_oil_U_repU <- L124_in_pctnorm_state_comm_oil_U[ rep( 1:nrow( L124_in_pctnorm_state_comm_oil_U ),
      length.out = nrow( L124_in_pct_state_comm_oil_U_adj_unnorm ) ), ]
L124_in_pctnorm_state_comm_oil_U_repU <- L124_in_pctnorm_state_comm_oil_U_repU[ order( L124_in_pctnorm_state_comm_oil_U_repU$state ), ]

#Re-normalize to return the final service allocations by state
L124_in_pct_state_comm_oil_U_adj <- data.frame( L124_in_pct_state_comm_oil_U_adj_unnorm[ c( "state", "CBECS_service" ) ],
      L124_in_pct_state_comm_oil_U_adj_unnorm[ X_base_years ] / L124_in_pctnorm_state_comm_oil_U_repU[ X_base_years ] )
L124_in_pct_state_comm_oil_U_adj$service <- GCAM_comm_services$service[
      match( L124_in_pct_state_comm_oil_U_adj$CBECS_service, GCAM_comm_services$CBECS_service ) ]      

# 2f. FINAL ENERGY BY STATE, END USE, AND BASE YEAR
#Final energy table: repeat electricity consumption by number of services, and multiply by portional allocations
printlog( "Multiplying adjusted end-use proportions by state-level commercial sector oil consumption (final table)" )
L124_in_EJ_state_comm_oil <- L104_in_EJ_state_bld_F[ L104_in_EJ_state_bld_F$GCAM_sector == "comm" & L104_in_EJ_state_bld_F$GCAM_fuel == "refined liquids", ]
L124_in_EJ_state_comm_oil_repU <- L124_in_EJ_state_comm_oil[ rep( 1:nrow( L124_in_EJ_state_comm_oil ), times = length( CBECS_services_oil ) ), ]
L124_in_EJ_state_comm_oil_repU <- L124_in_EJ_state_comm_oil_repU[ order( L124_in_EJ_state_comm_oil_repU$state ), ]
L124_in_EJ_state_comm_oil_U <- data.frame( L124_in_EJ_state_comm_oil_repU[ state_S_F ],
      service = L124_in_pct_state_comm_oil_U_adj$service,
      L124_in_pct_state_comm_oil_U_adj[ X_base_years ] * L124_in_EJ_state_comm_oil_repU[ X_base_years ] )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L124_in_EJ_state_comm_oil_U <- c( "Commercial building oil consumption by state and service","Unit = EJ" )

#write tables as CSV files
writedata( L124_in_EJ_state_comm_oil_U,fn="L124_in_EJ_state_comm_oil_U", comments=comments.L124_in_EJ_state_comm_oil_U )

# Every script should finish with this line
logstop()