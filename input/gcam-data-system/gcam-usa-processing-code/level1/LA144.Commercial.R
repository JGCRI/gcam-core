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
logstart( "LA144.Commercial.R" )
printlog( "Commercial sector floorspace by state and energy consumption by state, fuel, and service" )

# -----------------------------------------------------------------------------

# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
Census_pop_hist <- readdata( "GCAMUSA_LEVEL0_DATA", "Census_pop_hist" )
CBECS_variables <- readdata( "GCAMUSA_MAPPINGS", "CBECS_variables" )
EIA_AEO_Tab5 <- readdata( "GCAMUSA_LEVEL0_DATA", "EIA_AEO_Tab5" )
EIA_distheat <- readdata( "GCAMUSA_LEVEL0_DATA", "EIA_distheat" )
PNNL_Commext_elec <- readdata( "GCAMUSA_LEVEL0_DATA", "PNNL_Commext_elec" )
CBECS_1979_1983 <- readdata( "GCAMUSA_LEVEL0_DATA", "CBECS_1979_1983" )
CBECS_1986 <- readdata( "GCAMUSA_LEVEL0_DATA", "CBECS_1986" )
CBECS_1989 <- readdata( "GCAMUSA_LEVEL0_DATA", "CBECS_1989" )
CBECS_1992 <- readdata( "GCAMUSA_LEVEL0_DATA", "CBECS_1992" )
CBECS_1995 <- readdata( "GCAMUSA_LEVEL0_DATA", "CBECS_1995" )
CBECS_1999 <- readdata( "GCAMUSA_LEVEL0_DATA", "CBECS_1999" )
CBECS_2003 <- readdata( "GCAMUSA_LEVEL0_DATA", "CBECS_2003" )
L142.in_EJ_state_bld_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L142.in_EJ_state_bld_F" )
L143.share_state_Pop_CDD_sR9 <- readdata( "GCAMUSA_LEVEL1_DATA", "L143.share_state_Pop_CDD_sR9" )
L143.share_state_Pop_HDD_sR9 <- readdata( "GCAMUSA_LEVEL1_DATA", "L143.share_state_Pop_HDD_sR9" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#2a. PREPARATION AND CLEANING OF CBECS DATABASES
# The 1979 and 1983 only have floorspace by census region; they aren't microdata and don't have energy information
L144.CBECS_all <- list( CBECS_1986, CBECS_1989, CBECS_1992, CBECS_1995, CBECS_1999, CBECS_2003 )
names( L144.CBECS_all ) <- paste0( "CBECS", c( 1986, 1989, 1992, 1995, 1999, 2003 ) )

#In order to be able to work with these data across years, the "edition" number needs to be removed from all
# variable names. E.g., re-naming square footage from "SQFT3" in 1986 and "SQFT4" in 1989 to "SQFT" in all.
for( i in seq( L144.CBECS_all ) ){
	for( j in seq( names( L144.CBECS_all[[i]] ) ) ){
		names( L144.CBECS_all[[i]] )[j] <- sub( "[0-9]{1}", "", names( L144.CBECS_all[[i]] )[j] )
	}
}

#Add a vector specifying the census region (subregion4) and census division (subregion9)
# Census regions (subregion4) are used for 1979-1986 floorspace, as the first editions didn't have census divisions (subregion9)
for( i in seq( L144.CBECS_all ) ){
	L144.CBECS_all[[i]][[ "subregion4" ]] <- states_subregions$subregion4[
	  match( L144.CBECS_all[[i]][[ "REGION" ]], states_subregions$REGION ) ]
	L144.CBECS_all[[i]][[ "subregion9" ]] <- states_subregions$subregion9[
	  match( L144.CBECS_all[[i]][[ "CENDIV" ]], states_subregions$DIVISION ) ]
}

#Convert all missing value strings to 0 in all databases
L144.CBECS_all[["CBECS1992"]][is.na( L144.CBECS_all[["CBECS1992"]] ) ] <- 0
L144.CBECS_all[["CBECS1995"]][L144.CBECS_all[["CBECS1995"]] == 1e14] <- 0
L144.CBECS_all[["CBECS2003"]][is.na( L144.CBECS_all[["CBECS2003"]] ) ] <- 0

#Aggregate population to the subregion9 and subregion9 levels for calculation of per-capita values
Census_pop_hist[ c( "subregion4", "subregion9" ) ] <- states_subregions[
      match( Census_pop_hist$state, states_subregions$state ),
      c( "subregion4", "subregion9" ) ]
L144.pop_sR4 <- aggregate( Census_pop_hist[ X_historical_years ], by=as.list( Census_pop_hist[ "subregion4" ] ), sum )
L144.pop_sR9 <- aggregate( Census_pop_hist[ X_historical_years ], by=as.list( Census_pop_hist[ "subregion9" ] ), sum )

#2b. FLOORSPACE BY STATE AND YEAR
printlog( "Estimating total floorspace by census region (subregion4) and division (subregion9)")
CBECS_1979_1983$subregion4 <- states_subregions$subregion4[
      match( CBECS_1979_1983$REGION, states_subregions$REGION ) ]
CBECS_1979_1983$Pop1 <- L144.pop_sR4$X1979[ match( CBECS_1979_1983$subregion4, L144.pop_sR4$subregion4 ) ]
CBECS_1979_1983$Pop2 <- L144.pop_sR4$X1983[ match( CBECS_1979_1983$subregion4, L144.pop_sR4$subregion4 ) ]
CBECS_1979_1983$pcflsp_m2_1979 <- CBECS_1979_1983$SQFT1 / CBECS_1979_1983$Pop1 * conv_milft2_m2
CBECS_1979_1983$pcflsp_m2_1983 <- CBECS_1979_1983$SQFT2 / CBECS_1979_1983$Pop2 * conv_milft2_m2
L144.flsp_bm2_sR4_CBECS1983 <- data.frame( )
for( i in seq( L144.CBECS_all ) ){
	if( i == 1 ){
	 	data_year <- substr( names( L144.CBECS_all )[i], 6, 9 )
	 	object <- aggregate_recs(
	 	   L144.CBECS_all[[i]], variables = "SQFT", year = paste0( "X", data_year ),
	 	   weight_variable = "ADJWT", region_variable = "subregion4", unit_conv = conv_ft2_m2  )
	 	object$pcflsp_m2 <- object[[ "SQFT" ]] / L144.pop_sR4[[ paste0( "X", data_year ) ]]
	 	objectname <- paste0( "L144.flsp_bm2_sR4_", names( L144.CBECS_all )[i] )
	 	assign( objectname, object )
	}
	 	data_year <- substr( names( L144.CBECS_all )[i], 6, 9 )
	 	object <- aggregate_recs(
	 	   L144.CBECS_all[[i]], variables = "SQFT", year = paste0( "X", data_year ),
	 	   weight_variable = "ADJWT", region_variable = "subregion9", unit_conv = conv_ft2_m2  )
	 	object$pcflsp_m2 <- object[[ "SQFT" ]] / L144.pop_sR9[[ paste0( "X", data_year ) ]]
	 	objectname <- paste0( "L144.flsp_bm2_sR9_", names( L144.CBECS_all )[i] )
	 	assign( objectname, object )
}

#Downscale 1983 and 1979 floorspace to subregion9, using the ratios of per-capita floorspace
L144.flsp_conv_4_9 <- L144.flsp_bm2_sR9_CBECS1986
L144.flsp_conv_4_9$subregion4 <- states_subregions$subregion4[ match( L144.flsp_conv_4_9$subregion9, states_subregions$subregion9 ) ]
L144.flsp_conv_4_9$pcflsp_m2_4 <- L144.flsp_bm2_sR4_CBECS1986$pcflsp_m2[ match( L144.flsp_conv_4_9$subregion4, L144.flsp_bm2_sR4_CBECS1986$subregion4 ) ]
L144.flsp_conv_4_9$conv_4_9 <- L144.flsp_conv_4_9$pcflsp_m2 / L144.flsp_conv_4_9$pcflsp_m2_4

#Multiplying the per-capita floorspace ratios from subregion4 to subregion9, to expand from 4 to 9
L144.flsp_bm2_sR9_CBECS1983 <- data.frame(
      L144.flsp_conv_4_9[ "subregion9" ], year = "X1983",
      pcflsp_m2 = CBECS_1979_1983$pcflsp_m2_1983[
         match( L144.flsp_conv_4_9$subregion4, CBECS_1979_1983$subregion4 ) ] * L144.flsp_conv_4_9$conv_4_9 )
L144.flsp_bm2_sR9_CBECS1979 <- data.frame(
      L144.flsp_conv_4_9[ "subregion9" ], year = "X1979",
      pcflsp_m2 = CBECS_1979_1983$pcflsp_m2_1979[
         match( L144.flsp_conv_4_9$subregion4, CBECS_1979_1983$subregion4 ) ] * L144.flsp_conv_4_9$conv_4_9 )

#Merge all tables, and cast by year
flsp_cols <- c( "subregion9", "year", "pcflsp_m2" )
L144.pcflsp_m2_sR9_CBECS.melt <- rbind( L144.flsp_bm2_sR9_CBECS1979[ flsp_cols ], L144.flsp_bm2_sR9_CBECS1983[ flsp_cols ],
      L144.flsp_bm2_sR9_CBECS1986[ flsp_cols ], L144.flsp_bm2_sR9_CBECS1989[ flsp_cols ], L144.flsp_bm2_sR9_CBECS1992[ flsp_cols ],
      L144.flsp_bm2_sR9_CBECS1995[ flsp_cols ], L144.flsp_bm2_sR9_CBECS1999[ flsp_cols ], L144.flsp_bm2_sR9_CBECS2003[ flsp_cols ] )
L144.pcflsp_m2_sR9_CBECS <- dcast( L144.pcflsp_m2_sR9_CBECS.melt, subregion9 ~ year, value.var = "pcflsp_m2" )
L144.pcflsp_m2_sR9_CBECS <- gcam_interp( L144.pcflsp_m2_sR9_CBECS, historical_years, rule = 2 )
L144.pcflsp_m2_sR9_comm <- L144.pcflsp_m2_sR9_CBECS[ c( "subregion9", X_historical_years ) ]

#Expand to states: multiply per-capita floorspace in each subregion9 times the population of each state
L144.flsp_bm2_state_comm <- data.frame(
      Census_pop_hist[ c( state, "subregion9" ) ],
      sector = "comm",
      Census_pop_hist[ X_historical_years ] * L144.pcflsp_m2_sR9_comm[
          match( Census_pop_hist$subregion9, L144.pcflsp_m2_sR9_comm$subregion9 ), X_historical_years ] /
          conv_bm2_m2 )

printlog( "NOTE: we are scaling aggregated CBECS floorspace to match AEO base year estimates from 1999 to 2010" )
printlog( "The main reason for this step is that the most recent CBECS edition is a decade old (2003)" )
AEO_Tab5_yearcols <- names( EIA_AEO_Tab5 )[ grepl( "X[0-9]{4}", names( EIA_AEO_Tab5 ) ) ]
AEO_USA_flsp_bm2 <- EIA_AEO_Tab5[ EIA_AEO_Tab5$variable == "Floorspace", AEO_Tab5_yearcols ] * conv_ft2_m2
AEO_USA_flsp_bm2_scalers <- AEO_USA_flsp_bm2 / apply( L144.flsp_bm2_state_comm[ AEO_Tab5_yearcols ], 2, sum )
L144.flsp_bm2_state_comm[ AEO_Tab5_yearcols ] <- L144.flsp_bm2_state_comm[ AEO_Tab5_yearcols ] *
      AEO_USA_flsp_bm2_scalers[ rep( 1, times = nrow( L144.flsp_bm2_state_comm ) ), ]

#2c: ENERGY CONSUMPTION BY STATE, SERVICE, AND YEAR
printlog( "Aggregating energy consumption by sampling weights" )
#Aggregate in a for loop
for( i in names( L144.CBECS_all ) ){
	if( any( grepl( "BTU", names( L144.CBECS_all[[i]] ) ) ) ){
		object.cast <- aggregate_recs(
	       L144.CBECS_all[[ i ]],
	       variables = names( L144.CBECS_all[[ i ]] )[ names( L144.CBECS_all[[ i ]] ) %in% CBECS_variables$variable ],
	       year = paste0("X", substr( i, 6, 9 ) ),
	       weight_variable = "ADJWT", region_variable = "subregion9", unit_conv = conv_kbtu_EJ )
		object <- melt( object.cast, id.vars = c( "subregion9", "year" ) )
		objectname <- paste0( "L144.in_EJ_sR9_comm_", substr( i, 6, 9 ) )
		assign( objectname, object )
	}	
}

#Rbind these data frames, match in the fuel and service, and aggregate (this will get rid of the different liquid fuels)
L144.in_EJ_sR9_CBECS_F_U_Y <- rbind( L144.in_EJ_sR9_comm_1992, L144.in_EJ_sR9_comm_1995, L144.in_EJ_sR9_comm_2003 )
L144.in_EJ_sR9_CBECS_F_U_Y[ F_U ] <- CBECS_variables[ match( L144.in_EJ_sR9_CBECS_F_U_Y$variable, CBECS_variables$variable ), F_U ]

#District services are backed out to their fuel inputs here
printlog( "NOTE: in GCAM-USA, district services consumed by buildings are indicated by the fuel inputs to the district service plants" )
L144.in_EJ_sR9_CBECS_dist_U_Y <- subset( L144.in_EJ_sR9_CBECS_F_U_Y, fuel == "district services" )
L144.in_EJ_sR9_CBECS_Fdist_U_Y <- repeat_and_add_vector( L144.in_EJ_sR9_CBECS_dist_U_Y, "fuel", unique( EIA_distheat$fuel ) )
L144.in_EJ_sR9_CBECS_Fdist_U_Y$value <- L144.in_EJ_sR9_CBECS_Fdist_U_Y$value *
      EIA_distheat$share[
         match( vecpaste( L144.in_EJ_sR9_CBECS_Fdist_U_Y[ F_U ] ), vecpaste( EIA_distheat[ F_U ] ) ) ] /
      EIA_distheat$efficiency[
         match( vecpaste( L144.in_EJ_sR9_CBECS_Fdist_U_Y[ F_U ] ), vecpaste( EIA_distheat[ F_U ] ) ) ]

#Merge this back into the initial table, but with the district services fuel removed
L144.in_EJ_sR9_CBECS_F_U_Y <- rbind(
      subset( L144.in_EJ_sR9_CBECS_F_U_Y, fuel != "district services" ),
      L144.in_EJ_sR9_CBECS_Fdist_U_Y )

#Aggregate by fuel and service, cast by year, and interpolate/extrapolate
L144.in_EJ_sR9_comm_F_U_Y.melt <- aggregate( L144.in_EJ_sR9_CBECS_F_U_Y[ "value" ], by=as.list( L144.in_EJ_sR9_CBECS_F_U_Y[ c( "subregion9", F_U, "year" ) ] ), sum )
L144.in_EJ_sR9_comm_F_U_Y <- dcast( L144.in_EJ_sR9_comm_F_U_Y.melt, subregion9 + fuel + service ~ year )
L144.in_EJ_sR9_comm_F_U_Y <- gcam_interp( L144.in_EJ_sR9_comm_F_U_Y, historical_years, rule = 2 )[
      c( "subregion9", F_U, X_historical_years ) ]

#At this point, we have a table of energy by subregion9, fuel, and end use that needs to be (a) apportioned to states, and (b) scaled
# The reason for apportioning to states first is that the heating and cooling energy will be modified by pop-weighted HDD and CDD
# prior to calculating energy shares
printlog( "Downscaling heating and cooling energy to states according to person-HDD and -CDD" )
L144.in_EJ_state_comm_F_heating_Y <- data.frame(
      repeat_and_add_vector( states_subregions[ c( state, "subregion9" ) ], "fuel", CBECS_heating_fuels ),
      service = "comm heating" )
L144.in_EJ_state_comm_F_heating_Y[ X_historical_years ] <-
      L144.in_EJ_sR9_comm_F_U_Y[
         match( vecpaste( L144.in_EJ_state_comm_F_heating_Y[ c( "subregion9", F_U ) ] ),
                vecpaste( L144.in_EJ_sR9_comm_F_U_Y[ c( "subregion9", F_U ) ] ) ),
         X_historical_years ] *
      L143.share_state_Pop_HDD_sR9[
         match( L144.in_EJ_state_comm_F_heating_Y$state, L143.share_state_Pop_HDD_sR9$state ),
         X_historical_years ]

L144.in_EJ_state_comm_F_cooling_Y <- data.frame(
      repeat_and_add_vector( states_subregions[ c( state, "subregion9" ) ], "fuel", CBECS_cooling_fuels ),
      service = "comm cooling" )
L144.in_EJ_state_comm_F_cooling_Y[ X_historical_years ] <-
      L144.in_EJ_sR9_comm_F_U_Y[
         match( vecpaste( L144.in_EJ_state_comm_F_cooling_Y[ c( "subregion9", F_U ) ] ),
                vecpaste( L144.in_EJ_sR9_comm_F_U_Y[ c( "subregion9", F_U ) ] ) ),
         X_historical_years ] *
      L143.share_state_Pop_CDD_sR9[
         match( L144.in_EJ_state_comm_F_cooling_Y$state, L143.share_state_Pop_CDD_sR9$state ),
         X_historical_years ]

printlog( "Downscaling all remaining services to states according to floorspace" )
#Calculate the share of each state within its census division's (subregion9's) floorspace
L144.flsp_bm2_sR9_comm <- aggregate( L144.flsp_bm2_state_comm[ X_historical_years ],
      by=as.list( L144.flsp_bm2_state_comm[ "subregion9" ] ), sum )
L144.flsp_state_share_sR9 <- data.frame( 
      L144.flsp_bm2_state_comm[ c( state, "subregion9" ) ],
      L144.flsp_bm2_state_comm[ X_historical_years ] / L144.flsp_bm2_sR9_comm[
         match( L144.flsp_bm2_state_comm$subregion9, L144.flsp_bm2_sR9_comm$subregion9 ),
         X_historical_years ] )

#Start with a table of the service x fuel combinations that are being represented in each state
L144.in_EJ_sR9_comm_F_Uoth_Y <- subset(L144.in_EJ_sR9_comm_F_U_Y, !service %in% c( "comm heating", "comm cooling" ) )
L144.F_Uoth <- unique( L144.in_EJ_sR9_comm_F_Uoth_Y[ F_U] )
L144.in_EJ_state_comm_F_Uoth_Y <- repeat_and_add_vector( L144.F_Uoth, state, states )
L144.in_EJ_state_comm_F_Uoth_Y$subregion9 <- states_subregions$subregion9[ match( L144.in_EJ_state_comm_F_Uoth_Y$state, states_subregions$state ) ]
L144.in_EJ_state_comm_F_Uoth_Y[ X_historical_years ] <-
      L144.flsp_state_share_sR9[ match( L144.in_EJ_state_comm_F_Uoth_Y$state, L144.flsp_state_share_sR9$state ),
      X_historical_years ] *
      L144.in_EJ_sR9_comm_F_Uoth_Y[
         match( vecpaste( L144.in_EJ_state_comm_F_Uoth_Y[ c( "subregion9", F_U ) ] ),
                vecpaste( L144.in_EJ_sR9_comm_F_Uoth_Y[ c( "subregion9", F_U ) ] ) ),
      X_historical_years ]

printlog( "Assembling unscaled energy consumption by state, fuel, and service" )
L144.in_EJ_state_comm_F_U_Y_unscaled <- rbind(
      L144.in_EJ_state_comm_F_heating_Y, L144.in_EJ_state_comm_F_cooling_Y, L144.in_EJ_state_comm_F_Uoth_Y )

printlog( "Calculating shares of energy consumption by each service, within each state and fuel")
L144.in_EJ_state_comm_F_Y_unscaled <- aggregate( L144.in_EJ_state_comm_F_U_Y_unscaled[ X_historical_years ],
      by=as.list( L144.in_EJ_state_comm_F_U_Y_unscaled[ state_F ] ), sum )
L144.pct_state_comm_F_U_Y <- L144.in_EJ_state_comm_F_U_Y_unscaled
L144.pct_state_comm_F_U_Y[ X_historical_years ] <- L144.in_EJ_state_comm_F_U_Y_unscaled[ X_historical_years ] / L144.in_EJ_state_comm_F_Y_unscaled[
      match( vecpaste( L144.in_EJ_state_comm_F_U_Y_unscaled[ state_F ] ),
             vecpaste( L144.in_EJ_state_comm_F_Y_unscaled[ state_F ] ) ),
      X_historical_years ]
L144.pct_state_comm_F_U_Y$sector <- "comm"

# At this point we can disaggregate the state-level energy consumption by sector and fuel to the specific end uses
printlog( "Non-building electricity use by state is estimated separately, and deducted from state-wide commercial electricity consumption" )
printlog( "National non-building electricity use is disaggregated to states according to population shares" )
L144.in_TWh_USA_commext_elec.melt <- melt( PNNL_Commext_elec, id.vars = c( "Year", "unit" ) )
L144.in_TWh_USA_commext_elec.melt$year <- paste0( "X", L144.in_TWh_USA_commext_elec.melt$Year )
L144.in_TWh_USA_commext_elec.melt$service <- "comm non-building"
L144.in_TWh_USA_commext_elec.melt$value_EJ <- L144.in_TWh_USA_commext_elec.melt$value * conv_TWh_EJ
L144.in_EJ_USA_commext_elec <- dcast( L144.in_TWh_USA_commext_elec.melt, service ~ year, value.var = "value_EJ", fun.aggregate = sum )

#This dataset needs to be expanded to all historical years. Use population ratios
first_year_commext <- paste0( "X", min( PNNL_Commext_elec$Year ) )
pre_commext_years <- paste0( "X", historical_years[ historical_years < min( PNNL_Commext_elec$Year ) ] )
last_year_commext <- paste0( "X", max( PNNL_Commext_elec$Year ) )
post_commext_years <- paste0( "X", historical_years[ historical_years > max( PNNL_Commext_elec$Year ) ] )
L144.in_EJ_USA_commext_elec[ pre_commext_years ] <- L144.in_EJ_USA_commext_elec[[first_year_commext]] *
      colSums( Census_pop_hist[ pre_commext_years ] ) / sum( Census_pop_hist[[first_year_commext]])
L144.in_EJ_USA_commext_elec[ post_commext_years ] <- L144.in_EJ_USA_commext_elec[[last_year_commext]] *
      colSums( Census_pop_hist[ post_commext_years ] ) / sum( Census_pop_hist[[last_year_commext]])
L144.in_EJ_USA_commext_elec <- L144.in_EJ_USA_commext_elec[ c( "service", X_historical_years ) ]

#Table of population ratios
L144.pct_state_commext_elec_Y <- data.frame(
      Census_pop_hist[ state ], sector = "comm", fuel = "electricity", service = "comm non-building",
      sweep( Census_pop_hist[ X_historical_years ], 2, colSums( Census_pop_hist[ X_historical_years ] ), "/" ) )
L144.in_EJ_state_commext_F_U_Y <- apportion_to_states(
      nation_data = L144.in_EJ_USA_commext_elec,
      state_share_data = L144.pct_state_commext_elec_Y,
      match_vectors = "service" )

printlog( "Commercial non-building electricity is not scaled; it is deducted from the top-down estimate of commercial electricity use" )
L144.in_EJ_state_comm_elec <- subset( L142.in_EJ_state_bld_F, sector == "comm" & fuel == "electricity" )
L144.in_EJ_state_commint_elec <- L144.in_EJ_state_comm_elec
L144.in_EJ_state_commint_elec[ X_historical_years ] <- L144.in_EJ_state_comm_elec[ X_historical_years ] -
      L144.in_EJ_state_commext_F_U_Y[ match( L144.in_EJ_state_commint_elec$state, L144.in_EJ_state_commext_F_U_Y$state ),
      X_historical_years ]

#Bind this back to the initial table of commercial energy use by state and fuel
L144.in_EJ_state_commint_F <- rbind(
      subset( L142.in_EJ_state_bld_F, sector == "comm" & fuel != "electricity" ),
      L144.in_EJ_state_commint_elec )

#This energy can now be apportioned to the end-use services
L144.in_EJ_state_commint_F_U_Y <- apportion_to_states(
      nation_data = L144.in_EJ_state_commint_F,
      state_share_data = L144.pct_state_comm_F_U_Y,
      match_vectors = state_S_F )[ c( state, S_F_U, X_historical_years ) ]

#Bind the building (interior) and non-building (exterior) energy use tables
L144.in_EJ_state_comm_F_U_Y <- rbind( L144.in_EJ_state_commint_F_U_Y, L144.in_EJ_state_commext_F_U_Y )

#This table needs to have coal and biomass added; just assign these to heating
L144.in_EJ_state_comm_FnoCBECS <- subset( L142.in_EJ_state_bld_F, sector == "comm" & !fuel %in% L144.in_EJ_state_comm_F_U_Y$fuel )
L144.in_EJ_state_comm_FnoCBECS$service <- "comm heating"
L144.in_EJ_state_comm_FnoCBECS <- L144.in_EJ_state_comm_FnoCBECS[ c( state, S_F_U, X_historical_years ) ]
L144.in_EJ_state_comm_F_U_Y <- rbind( L144.in_EJ_state_comm_FnoCBECS, L144.in_EJ_state_comm_F_U_Y )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L144.flsp_bm2_state_comm <- c( "commential floorspace by state","Unit = billion m2" )
comments.L144.in_EJ_state_comm_F_U_Y <- c( "commential energy consumption by state / fuel / end use","Unit = EJ/yr" )

#write tables as CSV files
writedata( L144.flsp_bm2_state_comm, domain="GCAMUSA_LEVEL1_DATA", fn="L144.flsp_bm2_state_comm", comments=comments.L144.flsp_bm2_state_comm )
writedata( L144.in_EJ_state_comm_F_U_Y, domain="GCAMUSA_LEVEL1_DATA", fn="L144.in_EJ_state_comm_F_U_Y", comments=comments.L144.in_EJ_state_comm_F_U_Y )

# Every script should finish with this line
logstop()