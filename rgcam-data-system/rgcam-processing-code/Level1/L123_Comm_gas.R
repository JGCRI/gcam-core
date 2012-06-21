# L123_Comm_gas.R
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L123_Comm_gas.R" )
printlog( "Commercial sector gas consumption by state and service" )

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
printlog( "Multiplying CBECS gas consumption by end use by building sampling weights" )
#Calculate gas demands by available end uses in each CBECS
# Note that energy use in CBECS is in kbtu, not MMbtu as indicated in the documentation
# Differentiating between direct-fired and district-provided services (sph and wth only)
L123_in_EJ_comm_gas_U_1992_CBECS <- data.frame(
      subregion9 = L121_CBECS_1992$subregion9,
      Xyear = "X1992",
      sph_dir = L121_CBECS_1992$NGHTBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5,
      col = 0,
      wth_dir = L121_CBECS_1992$NGWTBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5,
      ckg = L121_CBECS_1992$NGCKBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5,
      othint = L121_CBECS_1992$NGMSBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5 )

#Calculate district-provided services, backing the energy out to the fuel inputs to the district heat plants
printlog( "Adding fuel inputs to district services to the respective services" )
printlog( "NOTE: Assuming constant national fuel shares and efficiencies for district services" )
L123_in_EJ_comm_gas_U_1992_CBECS$sph_dh <- L121_CBECS_1992$DHHTBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5 *
      EIA_distheat$share[ EIA_distheat$fuel == "gas" & EIA_distheat$service == "heating" ] /
      EIA_distheat$efficiency[ EIA_distheat$fuel=="gas" & EIA_distheat$service == "heating" ]
L123_in_EJ_comm_gas_U_1992_CBECS$wth_dh <- L121_CBECS_1992$DHHTBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5 *
      EIA_distheat$share[ EIA_distheat$fuel == "gas" & EIA_distheat$service == "hot water" ] /
      EIA_distheat$efficiency[ EIA_distheat$fuel=="gas" & EIA_distheat$service == "hot water" ]

#Add fuel inputs to district services to the relevant services
L123_in_EJ_comm_gas_U_1992_CBECS$sph <- L123_in_EJ_comm_gas_U_1992_CBECS$sph_dir + L123_in_EJ_comm_gas_U_1992_CBECS$sph_dh
L123_in_EJ_comm_gas_U_1992_CBECS$wth <- L123_in_EJ_comm_gas_U_1992_CBECS$wth_dir + L123_in_EJ_comm_gas_U_1992_CBECS$wth_dh

#Repeat for 1995 CBECS
L123_in_EJ_comm_gas_U_1995_CBECS <- data.frame(
      subregion9 = L121_CBECS_1995$subregion9,
      Xyear = "X1995",
      sph_dir = L121_CBECS_1995$NGHTBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6,
      col = L121_CBECS_1995$NGCLBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6,
      wth_dir = L121_CBECS_1995$NGWTBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6,
      ckg = L121_CBECS_1995$NGCKBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6,
      othint = L121_CBECS_1995$NGMSBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6 )

L123_in_EJ_comm_gas_U_1995_CBECS$sph_dh <- L121_CBECS_1995$DHHTBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6 *
      EIA_distheat$share[ EIA_distheat$fuel == "gas" & EIA_distheat$service == "heating" ] /
      EIA_distheat$efficiency[ EIA_distheat$fuel=="gas" & EIA_distheat$service == "heating" ]
L123_in_EJ_comm_gas_U_1995_CBECS$wth_dh <- L121_CBECS_1995$DHHTBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6 *
      EIA_distheat$share[ EIA_distheat$fuel == "gas" & EIA_distheat$service == "hot water" ] /
      EIA_distheat$efficiency[ EIA_distheat$fuel=="gas" & EIA_distheat$service == "hot water" ]

L123_in_EJ_comm_gas_U_1995_CBECS$sph <- L123_in_EJ_comm_gas_U_1995_CBECS$sph_dir + L123_in_EJ_comm_gas_U_1995_CBECS$sph_dh
L123_in_EJ_comm_gas_U_1995_CBECS$wth <- L123_in_EJ_comm_gas_U_1995_CBECS$wth_dir + L123_in_EJ_comm_gas_U_1995_CBECS$wth_dh

#Repeat for 2003 CBECS
L123_in_EJ_comm_gas_U_2003_CBECS <- data.frame(
      subregion9 = L121_CBECS_2003$subregion9,
      Xyear = "X2003",
      sph_dir = L121_CBECS_2003$NGHTBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8,
      col = L121_CBECS_2003$NGCLBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8,
      wth_dir = L121_CBECS_2003$NGWTBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8,
      ckg = L121_CBECS_2003$NGCKBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8,
      othint = L121_CBECS_2003$NGMSBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8 )

L123_in_EJ_comm_gas_U_2003_CBECS$sph_dh <- L121_CBECS_2003$DHHTBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8 *
      EIA_distheat$share[ EIA_distheat$fuel == "gas" & EIA_distheat$service == "heating" ] /
      EIA_distheat$efficiency[ EIA_distheat$fuel=="gas" & EIA_distheat$service == "heating" ]
L123_in_EJ_comm_gas_U_2003_CBECS$wth_dh <- L121_CBECS_2003$DHHTBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8 *
      EIA_distheat$share[ EIA_distheat$fuel == "gas" & EIA_distheat$service == "hot water" ] /
      EIA_distheat$efficiency[ EIA_distheat$fuel=="gas" & EIA_distheat$service == "hot water" ]

L123_in_EJ_comm_gas_U_2003_CBECS$sph <- L123_in_EJ_comm_gas_U_2003_CBECS$sph_dir + L123_in_EJ_comm_gas_U_2003_CBECS$sph_dh
L123_in_EJ_comm_gas_U_2003_CBECS$wth <- L123_in_EJ_comm_gas_U_2003_CBECS$wth_dir + L123_in_EJ_comm_gas_U_2003_CBECS$wth_dh

#Rbind CBECS databases into one dataframe
L123_in_EJ_comm_gas_U_CBECS_sR9 <- rbind( L123_in_EJ_comm_gas_U_1992_CBECS, L123_in_EJ_comm_gas_U_1995_CBECS,
      L123_in_EJ_comm_gas_U_2003_CBECS )

# 2b. CALCULATION OF END USE PROPORTIONS BY SUBREGION9 IN CBECS YEARS
#Aggregate full database by subregion9
printlog( "Aggregating CBECS data by available subregions to calculate end-use proportions" )
L123_in_EJ_sR9_comm_gas_U_Ycbecs <- aggregate( L123_in_EJ_comm_gas_U_CBECS_sR9[ CBECS_services_gas ],
      list( subregion9 = L123_in_EJ_comm_gas_U_CBECS_sR9$subregion9,
            Xyear = L123_in_EJ_comm_gas_U_CBECS_sR9$Xyear ), sum )

#Calculate the end-use proportions for commercial buildings energy use
L123_in_pct_sR9_comm_gas_U_Ycbecs <- data.frame(
      subregion9 = L123_in_EJ_sR9_comm_gas_U_Ycbecs$subregion9,
      Xyear = L123_in_EJ_sR9_comm_gas_U_Ycbecs$Xyear,
      sweep( L123_in_EJ_sR9_comm_gas_U_Ycbecs[ CBECS_services_gas ], 1,
             rowSums( L123_in_EJ_sR9_comm_gas_U_Ycbecs[ CBECS_services_gas ] ), "/" ) )

# 2c. EXPANSION OF END USE PROPORTIONS TO ALL BASE YEARS
#Melt and re-cast so that years are columns
L123_in_pct_sR9_comm_gas_U_Ycbecs.melt <- melt( L123_in_pct_sR9_comm_gas_U_Ycbecs, id.vars = c( "subregion9", "Xyear" ) )
L123_in_pct_sR9_comm_gas_U_Ycbecs <- cast( L123_in_pct_sR9_comm_gas_U_Ycbecs.melt, subregion9 + variable ~ Xyear )
names( L123_in_pct_sR9_comm_gas_U_Ycbecs )[ names( L123_in_pct_sR9_comm_gas_U_Ycbecs ) == "variable" ] <- "CBECS_service"

#Interpolate to the specified base years
printlog( "NOTE: Interpolating (linearly) from available CBECS years to specified base years" )
#First, add in 1975 and 2008 to bracket the range
printlog( "NOTE: Using closest available year for specified base years outside the range of CBECS years" )
L123_in_pct_sR9_comm_gas_U_Ycbecs$X1975 <- L123_in_pct_sR9_comm_gas_U_Ycbecs$X1992
L123_in_pct_sR9_comm_gas_U_Ycbecs$X2008 <- L123_in_pct_sR9_comm_gas_U_Ycbecs$X2003
L123_in_pct_sR9_comm_gas_U <- gcam_interp( L123_in_pct_sR9_comm_gas_U_Ycbecs, base_years )

#Re-order columns and drop the years that aren't part of the specified base years
L123_in_pct_sR9_comm_gas_U <- L123_in_pct_sR9_comm_gas_U[ c( "subregion9", "CBECS_service", X_base_years ) ]

#Revert "casted" data frame to a normal data frame
L123_in_pct_sR9_comm_gas_U <- as.data.frame( L123_in_pct_sR9_comm_gas_U )

#Apply percentages to states, using percentages of the respective subregions
printlog( "Applying subregion-level service proportions to constituent states" )
# Melt table of percentages by subregion13, service, and year
L123_in_pct_sR9_comm_gas_U.melt <- melt( L123_in_pct_sR9_comm_gas_U, id.vars = c( "subregion9", "CBECS_service" ) )

# 2d. END USE PROPORTIONS BY STATE
#Generate table with all combinations of state, CBECS_service, and Xyear
L123_in_pct_state_comm_gas_U.melt <- data.frame(
      state = rep( states, times = length( CBECS_services_gas ) * length( X_base_years ) ),
      CBECS_service = rep( sort( rep( CBECS_services_gas, times = length( states ) ) ), times = length( X_base_years ) ), 
      Xyear = sort( rep( X_base_years, times = length( states ) * length( CBECS_services_gas ) ) ),
      value = 0 )
L123_in_pct_state_comm_gas_U.melt$subregion9 <- Census_state_division$subregion9[
      match( L123_in_pct_state_comm_gas_U.melt$state, Census_state_division$state ) ]

#Match in the end-use proportion generic to the subregion9
L123_in_pct_state_comm_gas_U.melt$value <- L123_in_pct_sR9_comm_gas_U.melt$value[
      match( paste( L123_in_pct_state_comm_gas_U.melt$subregion9, L123_in_pct_state_comm_gas_U.melt$CBECS_service,
                    L123_in_pct_state_comm_gas_U.melt$Xyear ),
             paste( L123_in_pct_sR9_comm_gas_U.melt$subregion9, L123_in_pct_sR9_comm_gas_U.melt$CBECS_service,
                    L123_in_pct_sR9_comm_gas_U.melt$variable ) ) ]

# 2e. ADJUSTMENT AND RE-NORMALIZATION OF OF END USE PROPORTIONS BY STATE FOR HEATING AND COOLING DEGREE DAYS
#Cast the table so that years are columns. This will build a data frame that can be multiplied by the HDD/CDD adjustment tables.
printlog( "Adjusting service proportions by states within subregions according to relative HDD and CDD differences" )
L123_in_pct_state_comm_gas_U <- cast( L123_in_pct_state_comm_gas_U.melt, state + CBECS_service ~ Xyear )

#Subset the heating percentages for adjustment from subregion13 to the specific state
L123_in_pct_state_comm_gas_sph <- L123_in_pct_state_comm_gas_U[ L123_in_pct_state_comm_gas_U$CBECS_service == "sph", ]

#Multiply these percentages by the state-wise adjustment factors for heating
L123_in_pct_state_comm_gas_sph_adj <- data.frame( L123_in_pct_state_comm_gas_sph[ c( "state", "CBECS_service" ) ],
      L123_in_pct_state_comm_gas_sph[ X_base_years ] * L110_HDD_state_sR9_adj[ X_base_years ] )

#Match these adjusted portional allocations into the original table, which will no longer have its service shares normalized
L123_in_pct_state_comm_gas_U_adj_unnorm <- L123_in_pct_state_comm_gas_U
L123_in_pct_state_comm_gas_U_adj_unnorm[ L123_in_pct_state_comm_gas_U_adj_unnorm$CBECS_service == "sph", X_base_years ] <-
      L123_in_pct_state_comm_gas_sph_adj[ X_base_years ]

#Aggregate by services to calculate state-specific divisors for re-normalizing allocations
printlog( "Re-normalizing end-use proportions" )
L123_in_pctnorm_state_comm_gas_U <- aggregate( L123_in_pct_state_comm_gas_U_adj_unnorm[ X_base_years ],
      list( state = L123_in_pct_state_comm_gas_U_adj_unnorm$state ), sum )
L123_in_pctnorm_state_comm_gas_U_repU <- L123_in_pctnorm_state_comm_gas_U[ rep( 1:nrow( L123_in_pctnorm_state_comm_gas_U ),
      length.out = nrow( L123_in_pct_state_comm_gas_U_adj_unnorm ) ), ]
L123_in_pctnorm_state_comm_gas_U_repU <- L123_in_pctnorm_state_comm_gas_U_repU[ order( L123_in_pctnorm_state_comm_gas_U_repU$state ), ]

#Re-normalize to return the final service allocations by state
L123_in_pct_state_comm_gas_U_adj <- data.frame( L123_in_pct_state_comm_gas_U_adj_unnorm[ c( "state", "CBECS_service" ) ],
      L123_in_pct_state_comm_gas_U_adj_unnorm[ X_base_years ] / L123_in_pctnorm_state_comm_gas_U_repU[ X_base_years ] )
L123_in_pct_state_comm_gas_U_adj$service <- GCAM_comm_services$service[
      match( L123_in_pct_state_comm_gas_U_adj$CBECS_service, GCAM_comm_services$CBECS_service ) ]      

# 2f. FINAL ENERGY BY STATE, END USE, AND BASE YEAR
#Final energy table: repeat electricity consumption by number of services, and multiply by portional allocations
printlog( "Multiplying adjusted end-use proportions by state-level commercial gas (final table)" )
L123_in_EJ_state_comm_gas <- L104_in_EJ_state_bld_F[ L104_in_EJ_state_bld_F$GCAM_sector == "comm" & L104_in_EJ_state_bld_F$GCAM_fuel == "gas", ]
L123_in_EJ_state_comm_gas_repU <- L123_in_EJ_state_comm_gas[ rep( 1:nrow( L123_in_EJ_state_comm_gas ), times = length( CBECS_services_gas ) ), ]
L123_in_EJ_state_comm_gas_repU <- L123_in_EJ_state_comm_gas_repU[ order( L123_in_EJ_state_comm_gas_repU$state ), ]
L123_in_EJ_state_comm_gas_U <- data.frame( L123_in_EJ_state_comm_gas_repU[ state_S_F ],
      service = L123_in_pct_state_comm_gas_U_adj$service,
      L123_in_pct_state_comm_gas_U_adj[ X_base_years ] * L123_in_EJ_state_comm_gas_repU[ X_base_years ] )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L123_in_EJ_state_comm_gas_U <- c( "Commercial building gas consumption by state and service","Unit = EJ" )

#write tables as CSV files
writedata( L123_in_EJ_state_comm_gas_U,fn="L123_in_EJ_state_comm_gas_U", comments=comments.L123_in_EJ_state_comm_gas_U )

# Every script should finish with this line
logstop()