# L122_Comm_elec.R
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L122_Comm_elec.R" )
printlog( "Commercial sector electricity consumption by state and service" )

# -----------------------------------------------------------------------------
# 1. Read files

Census_state_division <- readmap( "Census_state_division" )
GCAM_comm_services <- readmap( "GCAM_comm_services" )
EIA_distheat <- readdata( "EIA_distheat" )
PNNL_commext_elec <- readdata( "PNNL_commext_elec" )
L104_in_EJ_state_bld_F <- readdata( "L104_in_EJ_state_bld_F" )
L110_CDD_state_sR9_adj <- readdata( "L110_CDD_state_sR9_adj" )
L110_HDD_state_sR9_adj <- readdata( "L110_HDD_state_sR9_adj" )
L121_CBECS_1992 <- readdata( "L121_CBECS_1992" )
L121_CBECS_1995 <- readdata( "L121_CBECS_1995" )
L121_CBECS_2003 <- readdata( "L121_CBECS_2003" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. PREPARATION OF CBECS DATABASES FOR AGGREGATION
printlog( "Multiplying CBECS electricity consumption by end use by building sampling weights" )
#Calculate electricity demands by available end uses in each CBECS
# Note that energy use in CBECS is in kbtu, not MMbtu as indicated in the documentation
# Differentiating between direct-fired and district-provided services (sph and wth only)
L122_in_EJ_comm_elec_U_1992_CBECS <- data.frame(
      subregion9 = L121_CBECS_1992$subregion9,
      Xyear = "X1992",
      sph_dir = L121_CBECS_1992$ELHTBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5,
      col = L121_CBECS_1992$ELCLBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5,
      wth_dir = L121_CBECS_1992$ELWTBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5,
      vnt = L121_CBECS_1992$ELVNBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5,
      ckg = L121_CBECS_1992$ELCKBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5,
      lgt = L121_CBECS_1992$ELLTBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5,
      rfg = L121_CBECS_1992$ELRFBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5,
      ofc = L121_CBECS_1992$ELOFBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5,
      othint = L121_CBECS_1992$ELMSBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5 )

#Calculate district-provided services, backing the energy out to the fuel inputs to the district heat plants
printlog( "Adding fuel inputs to district services to the respective services" )
printlog( "NOTE: Assuming constant national fuel shares and efficiencies for district services" )
L122_in_EJ_comm_elec_U_1992_CBECS$sph_dh <- L121_CBECS_1992$DHHTBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5 *
      EIA_distheat$share[ EIA_distheat$fuel == "electricity" & EIA_distheat$service == "heating" ] /
      EIA_distheat$efficiency[ EIA_distheat$fuel=="electricity" & EIA_distheat$service == "heating" ]
L122_in_EJ_comm_elec_U_1992_CBECS$wth_dh <- L121_CBECS_1992$DHHTBTU5 * conv_kbtu_EJ * L121_CBECS_1992$ADJWT5 *
      EIA_distheat$share[ EIA_distheat$fuel == "electricity" & EIA_distheat$service == "hot water" ] /
      EIA_distheat$efficiency[ EIA_distheat$fuel=="electricity" & EIA_distheat$service == "hot water" ]

#Add fuel inputs to district services to the relevant services
L122_in_EJ_comm_elec_U_1992_CBECS$sph <- L122_in_EJ_comm_elec_U_1992_CBECS$sph_dir + L122_in_EJ_comm_elec_U_1992_CBECS$sph_dh
L122_in_EJ_comm_elec_U_1992_CBECS$wth <- L122_in_EJ_comm_elec_U_1992_CBECS$wth_dir + L122_in_EJ_comm_elec_U_1992_CBECS$wth_dh

#Repeat for 1995 CBECS
L122_in_EJ_comm_elec_U_1995_CBECS <- data.frame(
      subregion9 = L121_CBECS_1995$subregion9,
      Xyear = "X1995",
      sph_dir = L121_CBECS_1995$ELHTBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6,
      col = L121_CBECS_1995$ELCLBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6,
      wth_dir = L121_CBECS_1995$ELWTBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6,
      vnt = L121_CBECS_1995$ELVNBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6,
      ckg = L121_CBECS_1995$ELCKBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6,
      lgt = L121_CBECS_1995$ELLTBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6,
      rfg = L121_CBECS_1995$ELRFBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6,
      ofc = L121_CBECS_1995$ELOFBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6,
      othint = L121_CBECS_1995$ELMSBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6 )

L122_in_EJ_comm_elec_U_1995_CBECS$sph_dh <- L121_CBECS_1995$DHHTBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6 *
      EIA_distheat$share[ EIA_distheat$fuel == "electricity" & EIA_distheat$service == "heating" ] /
      EIA_distheat$efficiency[ EIA_distheat$fuel=="electricity" & EIA_distheat$service == "heating" ]
L122_in_EJ_comm_elec_U_1995_CBECS$wth_dh <- L121_CBECS_1995$DHHTBTU6 * conv_kbtu_EJ * L121_CBECS_1995$ADJWT6 *
      EIA_distheat$share[ EIA_distheat$fuel == "electricity" & EIA_distheat$service == "hot water" ] /
      EIA_distheat$efficiency[ EIA_distheat$fuel=="electricity" & EIA_distheat$service == "hot water" ]

L122_in_EJ_comm_elec_U_1995_CBECS$sph <- L122_in_EJ_comm_elec_U_1995_CBECS$sph_dir + L122_in_EJ_comm_elec_U_1995_CBECS$sph_dh
L122_in_EJ_comm_elec_U_1995_CBECS$wth <- L122_in_EJ_comm_elec_U_1995_CBECS$wth_dir + L122_in_EJ_comm_elec_U_1995_CBECS$wth_dh

#Repeat for 2003 CBECS
L122_in_EJ_comm_elec_U_2003_CBECS <- data.frame(
      subregion9 = L121_CBECS_2003$subregion9,
      Xyear = "X2003",
      sph_dir = L121_CBECS_2003$ELHTBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8,
      col = L121_CBECS_2003$ELCLBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8,
      wth_dir = L121_CBECS_2003$ELWTBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8,
      vnt = L121_CBECS_2003$ELVNBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8,
      ckg = L121_CBECS_2003$ELCKBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8,
      lgt = L121_CBECS_2003$ELLTBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8,
      rfg = L121_CBECS_2003$ELRFBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8,
      ofc = ( L121_CBECS_2003$ELOFBTU8 + L121_CBECS_2003$ELPCBTU8 ) * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8,
      othint = L121_CBECS_2003$ELMSBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8 )

L122_in_EJ_comm_elec_U_2003_CBECS$sph_dh <- L121_CBECS_2003$DHHTBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8 *
      EIA_distheat$share[ EIA_distheat$fuel == "electricity" & EIA_distheat$service == "heating" ] /
      EIA_distheat$efficiency[ EIA_distheat$fuel=="electricity" & EIA_distheat$service == "heating" ]
L122_in_EJ_comm_elec_U_2003_CBECS$wth_dh <- L121_CBECS_2003$DHHTBTU8 * conv_kbtu_EJ * L121_CBECS_2003$ADJWT8 *
      EIA_distheat$share[ EIA_distheat$fuel == "electricity" & EIA_distheat$service == "hot water" ] /
      EIA_distheat$efficiency[ EIA_distheat$fuel=="electricity" & EIA_distheat$service == "hot water" ]

L122_in_EJ_comm_elec_U_2003_CBECS$sph <- L122_in_EJ_comm_elec_U_2003_CBECS$sph_dir + L122_in_EJ_comm_elec_U_2003_CBECS$sph_dh
L122_in_EJ_comm_elec_U_2003_CBECS$wth <- L122_in_EJ_comm_elec_U_2003_CBECS$wth_dir + L122_in_EJ_comm_elec_U_2003_CBECS$wth_dh

#Rbind CBECS databases into one dataframe
L122_in_EJ_comm_elec_U_CBECS_sR9 <- rbind( L122_in_EJ_comm_elec_U_1992_CBECS, L122_in_EJ_comm_elec_U_1995_CBECS,
      L122_in_EJ_comm_elec_U_2003_CBECS )

# 2b. CALCULATION OF END USE PROPORTIONS BY SUBREGION9 IN CBECS YEARS
#Aggregate full database by subregion9
printlog( "Aggregating CBECS data by available subregions to calculate end-use proportions" )
L122_in_EJ_sR9_commint_elec_U_Ycbecs <- aggregate( L122_in_EJ_comm_elec_U_CBECS_sR9[ CBECS_services_elec ],
      list( subregion9 = L122_in_EJ_comm_elec_U_CBECS_sR9$subregion9,
            Xyear = L122_in_EJ_comm_elec_U_CBECS_sR9$Xyear ), sum )

#Calculate the end-use proportions for commercial buildings energy use
L122_in_pct_sR9_commint_elec_U_Ycbecs <- data.frame(
      subregion9 = L122_in_EJ_sR9_commint_elec_U_Ycbecs$subregion9,
      Xyear = L122_in_EJ_sR9_commint_elec_U_Ycbecs$Xyear,
      sweep( L122_in_EJ_sR9_commint_elec_U_Ycbecs[ CBECS_services_elec ], 1,
             rowSums( L122_in_EJ_sR9_commint_elec_U_Ycbecs[ CBECS_services_elec ] ), "/" ) )

# 2c. EXPANSION OF END USE PROPORTIONS TO ALL BASE YEARS
#Melt and re-cast so that years are columns
L122_in_pct_sR9_commint_elec_U_Ycbecs.melt <- melt( L122_in_pct_sR9_commint_elec_U_Ycbecs, id.vars = c( "subregion9", "Xyear" ) )
L122_in_pct_sR9_commint_elec_U_Ycbecs <- cast( L122_in_pct_sR9_commint_elec_U_Ycbecs.melt, subregion9 + variable ~ Xyear )
names( L122_in_pct_sR9_commint_elec_U_Ycbecs )[ names( L122_in_pct_sR9_commint_elec_U_Ycbecs ) == "variable" ] <- "CBECS_service"

#Interpolate to the specified base years
printlog( "NOTE: Interpolating (linearly) from available CBECS years to specified base years" )
#First, add in 1975 and 2008 to bracket the range
printlog( "NOTE: Using closest available year for specified base years outside the range of CBECS years" )
L122_in_pct_sR9_commint_elec_U_Ycbecs$X1975 <- L122_in_pct_sR9_commint_elec_U_Ycbecs$X1992
L122_in_pct_sR9_commint_elec_U_Ycbecs$X2008 <- L122_in_pct_sR9_commint_elec_U_Ycbecs$X2003
L122_in_pct_sR9_commint_elec_U <- gcam_interp( L122_in_pct_sR9_commint_elec_U_Ycbecs, base_years )

#Re-order columns and drop the years that aren't part of the specified base years
L122_in_pct_sR9_commint_elec_U <- L122_in_pct_sR9_commint_elec_U[ c( "subregion9", "CBECS_service", X_base_years ) ]

#Revert "casted" data frame to a normal data frame
L122_in_pct_sR9_commint_elec_U <- as.data.frame( L122_in_pct_sR9_commint_elec_U )

#Apply percentages to states, using percentages of the respective subregions
printlog( "Applying subregion-level service proportions to constituent states" )
# Melt table of percentages by subregion13, service, and year
L122_in_pct_sR9_commint_elec_U.melt <- melt( L122_in_pct_sR9_commint_elec_U, id.vars = c( "subregion9", "CBECS_service" ) )

# 2d. END USE PROPORTIONS BY STATE
#Generate table with all combinations of state, CBECS_service, and Xyear
L122_in_pct_state_commint_elec_U.melt <- data.frame(
      state = rep( states, times = length( CBECS_services_elec ) * length( X_base_years ) ),
      CBECS_service = rep( sort( rep( CBECS_services_elec, times = length( states ) ) ), times = length( X_base_years ) ), 
      Xyear = sort( rep( X_base_years, times = length( states ) * length( CBECS_services_elec ) ) ),
      value = 0 )
L122_in_pct_state_commint_elec_U.melt$subregion9 <- Census_state_division$subregion9[
      match( L122_in_pct_state_commint_elec_U.melt$state, Census_state_division$state ) ]

#Match in the end-use proportion generic to the subregion9
L122_in_pct_state_commint_elec_U.melt$value <- L122_in_pct_sR9_commint_elec_U.melt$value[
      match( paste( L122_in_pct_state_commint_elec_U.melt$subregion9, L122_in_pct_state_commint_elec_U.melt$CBECS_service,
                    L122_in_pct_state_commint_elec_U.melt$Xyear ),
             paste( L122_in_pct_sR9_commint_elec_U.melt$subregion9, L122_in_pct_sR9_commint_elec_U.melt$CBECS_service,
                    L122_in_pct_sR9_commint_elec_U.melt$variable ) ) ]

# 2e. ADJUSTMENT AND RE-NORMALIZATION OF OF END USE PROPORTIONS BY STATE FOR HEATING AND COOLING DEGREE DAYS
#Cast the table so that years are columns. This will build a data frame that can be multiplied by the HDD/CDD adjustment tables.
printlog( "Adjusting service proportions by states within subregions according to relative HDD and CDD differences" )
L122_in_pct_state_commint_elec_U <- cast( L122_in_pct_state_commint_elec_U.melt, state + CBECS_service ~ Xyear )

#Subset the heating and cooling percentages for adjustment from subregion13 to the specific state
L122_in_pct_state_commint_elec_sph <- L122_in_pct_state_commint_elec_U[ L122_in_pct_state_commint_elec_U$CBECS_service == "sph", ]
L122_in_pct_state_commint_elec_col <- L122_in_pct_state_commint_elec_U[ L122_in_pct_state_commint_elec_U$CBECS_service == "col", ]

#Multiply these percentages by the state-wise adjustment factors for heating and cooling
L122_in_pct_state_commint_elec_sph_adj <- data.frame( L122_in_pct_state_commint_elec_sph[ c( "state", "CBECS_service" ) ],
      L122_in_pct_state_commint_elec_sph[ X_base_years ] * L110_HDD_state_sR9_adj[ X_base_years ] )
L122_in_pct_state_commint_elec_col_adj <- data.frame( L122_in_pct_state_commint_elec_col[ c( "state", "CBECS_service" ) ],
      L122_in_pct_state_commint_elec_col[ X_base_years ] * L110_CDD_state_sR9_adj[ X_base_years ] )

#Match these adjusted portional allocations into the original table, which will no longer have its service shares normalized
L122_in_pct_state_commint_elec_U_adj_unnorm <- L122_in_pct_state_commint_elec_U
L122_in_pct_state_commint_elec_U_adj_unnorm[ L122_in_pct_state_commint_elec_U_adj_unnorm$CBECS_service == "sph", X_base_years ] <-
      L122_in_pct_state_commint_elec_sph_adj[ X_base_years ]
L122_in_pct_state_commint_elec_U_adj_unnorm[ L122_in_pct_state_commint_elec_U_adj_unnorm$CBECS_service == "col", X_base_years ] <-
      L122_in_pct_state_commint_elec_col_adj[ X_base_years ]

#Aggregate by services to calculate state-specific divisors for re-normalizing allocations
printlog( "Re-normalizing end-use proportions" )
L122_in_pctnorm_state_commint_elec_U <- aggregate( L122_in_pct_state_commint_elec_U_adj_unnorm[ X_base_years ],
      list( state = L122_in_pct_state_commint_elec_U_adj_unnorm$state ), sum )
L122_in_pctnorm_state_commint_elec_U_repU <- L122_in_pctnorm_state_commint_elec_U[ rep( 1:nrow( L122_in_pctnorm_state_commint_elec_U ),
      length.out = nrow( L122_in_pct_state_commint_elec_U_adj_unnorm ) ), ]
L122_in_pctnorm_state_commint_elec_U_repU <- L122_in_pctnorm_state_commint_elec_U_repU[ order( L122_in_pctnorm_state_commint_elec_U_repU$state ), ]

#Re-normalize to return the final service allocations by state
L122_in_pct_state_commint_elec_U_adj <- data.frame( L122_in_pct_state_commint_elec_U_adj_unnorm[ c( "state", "CBECS_service" ) ],
      L122_in_pct_state_commint_elec_U_adj_unnorm[ X_base_years ] / L122_in_pctnorm_state_commint_elec_U_repU[ X_base_years ] )
L122_in_pct_state_commint_elec_U_adj$service <- GCAM_comm_services$service[
      match( L122_in_pct_state_commint_elec_U_adj$CBECS_service, GCAM_comm_services$CBECS_service ) ]      

# 2f. SPECIFICATION OF COMMERCIAL NON-BUILDING ENERGY USE IN THE BASE YEARS
printlog( "Building dataset of national non-building commercial energy use in the base years" )
#These data are from Belzer 2004, Estimates of U.S. Commercial Building Electricity Trends, Section 3
#They are used to generate an estimate of commercial sector electricity use that is not in the CBECS surveys
L122_in_TWh_USA_commext_elec.melt <- PNNL_commext_elec
if( is.numeric( L122_in_TWh_USA_commext_elec.melt$Year ) )
      L122_in_TWh_USA_commext_elec.melt$Xyear <- paste ("X", L122_in_TWh_USA_commext_elec.melt$Year, sep = "" )
L122_in_TWh_USA_commext_elec.melt$Total_EJ <- apply( L122_in_TWh_USA_commext_elec.melt[ , 3:8 ], 1, sum ) * conv_TWh_EJ
L122_in_TWh_USA_commext_elec.melt$service <- "comm other"

#Translate table so that years are columns, and subset the GCAM years
L122_in_EJ_USA_commext_elec <- cast( L122_in_TWh_USA_commext_elec.melt, service ~ Xyear, value = "Total_EJ" )
L122_in_EJ_USA_commext_elec <- L122_in_EJ_USA_commext_elec[ names( L122_in_EJ_USA_commext_elec ) %in% c( "service", X_base_years ) ]

#Use population ratios to get the years outside of the range in the Belzer report, and re-sort
L122_in_EJ_USA_commext_elec$X1975 <- L122_in_EJ_USA_commext_elec$X1985 * sum( Census_pop_hist$X1975 ) / sum( Census_pop_hist$X1985 )
L122_in_EJ_USA_commext_elec$X1980 <- L122_in_EJ_USA_commext_elec$X1985 * sum( Census_pop_hist$X1980 ) / sum( Census_pop_hist$X1985 )
L122_in_EJ_USA_commext_elec$X2008 <- L122_in_EJ_USA_commext_elec$X2005 * sum( Census_pop_hist$X2008 ) / sum( Census_pop_hist$X2005 )
L122_in_EJ_USA_commext_elec <- L122_in_EJ_USA_commext_elec[ c( "service", X_base_years ) ]

printlog( "NOTE: Assigning non-building commercial electricity use to states on the basis of population" )
L122_poppct_state <- data.frame(
      state = Census_pop_hist$state,
      sweep( Census_pop_hist[ X_base_years ], 2, colSums( Census_pop_hist[ X_base_years ] ) , "/" ) )
L122_in_EJ_USA_commext_elec_repstate <- L122_in_EJ_USA_commext_elec[ rep( 1, times = nrow( L122_poppct_state ) ), ]
L122_in_EJ_state_commext_elec <- data.frame( state = L122_poppct_state$state, service = "comm other",
      L122_poppct_state[ X_base_years ] * L122_in_EJ_USA_commext_elec_repstate[ X_base_years ] )

#2g. FINAL ENERGY BY STATE, END USE, AND BASE YEAR
#Deduct non-building commercial electricity (commext) from state-level commercial electricity consumption
L122_in_EJ_state_comm_elec <- L104_in_EJ_state_bld_F[ L104_in_EJ_state_bld_F$GCAM_sector == "comm" & L104_in_EJ_state_bld_F$GCAM_fuel == "electricity", ]
L122_in_EJ_state_commint_elec <- data.frame( L122_in_EJ_state_comm_elec[ state_S_F ],
      L122_in_EJ_state_comm_elec[ X_base_years ] - L122_in_EJ_state_commext_elec[ X_base_years ] )

#Final energy table: repeat interior electricity consumption by number of services, and multiply by portional allocations
printlog( "Multiplying adjusted end-use proportions by state-level commercial building electricity" )
L122_in_EJ_state_commint_elec_repU <- L122_in_EJ_state_commint_elec[ rep( 1:nrow( L122_in_EJ_state_commint_elec ), times = length( CBECS_services_elec ) ), ]
L122_in_EJ_state_commint_elec_repU <- L122_in_EJ_state_commint_elec_repU[ order( L122_in_EJ_state_commint_elec_repU$state ), ]
L122_in_EJ_state_commint_elec_U <- data.frame( L122_in_EJ_state_commint_elec_repU[ state_S_F ],
      service = L122_in_pct_state_commint_elec_U_adj$service,
      L122_in_pct_state_commint_elec_U_adj[ X_base_years ] * L122_in_EJ_state_commint_elec_repU[ X_base_years ] )
                                                             
#Add in the exterior other to the interior other
printlog( "Adding in non-building electricity (final table)" )
L122_in_EJ_state_comm_elec_U <- L122_in_EJ_state_commint_elec_U
L122_in_EJ_state_comm_elec_U[ L122_in_EJ_state_comm_elec_U$service == "comm other", X_base_years ] <-
      L122_in_EJ_state_commint_elec_U[ L122_in_EJ_state_commint_elec_U$service == "comm other", X_base_years] +
      L122_in_EJ_state_commext_elec[ X_base_years ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L122_in_EJ_state_comm_elec_U <- c( "Commercial sector electricity consumption by state and service","Unit = EJ" )
#Write out the exterior building energy use for the BEND project
comments.L122_in_EJ_state_commext_elec <- c( "Commercial non-building electricity consumption by state and service","Unit = EJ" )

#write tables as CSV files
writedata( L122_in_EJ_state_comm_elec_U,fn="L122_in_EJ_state_comm_elec_U", comments=comments.L122_in_EJ_state_comm_elec_U )
writedata( L122_in_EJ_state_commext_elec,fn="L122_in_EJ_state_commext_elec", comments=comments.L122_in_EJ_state_commext_elec )

# Every script should finish with this line
logstop()