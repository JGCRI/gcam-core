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
logstart( "LA144.Residential.R" )
printlog( "Residential floorspace by state and energy consumption by state, fuel, and service" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
RECS_variables <- readdata( "GCAMUSA_MAPPINGS", "RECS_variables" )
EIA_AEO_fuels <- readdata( "GCAMUSA_MAPPINGS", "EIA_AEO_fuels" )
EIA_AEO_services <- readdata( "GCAMUSA_MAPPINGS", "EIA_AEO_services" )
Census_pop_hist <- readdata( "GCAMUSA_LEVEL0_DATA", "Census_pop_hist" )
EIA_AEO_Tab4 <- readdata( "GCAMUSA_LEVEL0_DATA", "EIA_AEO_Tab4" )
RECS_1979 <- readdata( "GCAMUSA_LEVEL0_DATA", "RECS_1979" )
RECS_1984 <- readdata( "GCAMUSA_LEVEL0_DATA", "RECS_1984" )
RECS_1990 <- readdata( "GCAMUSA_LEVEL0_DATA", "RECS_1990" )
RECS_1993 <- readdata( "GCAMUSA_LEVEL0_DATA", "RECS_1993" )
RECS_1997 <- readdata( "GCAMUSA_LEVEL0_DATA", "RECS_1997" )
RECS_2001 <- readdata( "GCAMUSA_LEVEL0_DATA", "RECS_2001" )
RECS_2005 <- readdata( "GCAMUSA_LEVEL0_DATA", "RECS_2005" )
RECS_2009 <- readdata( "GCAMUSA_LEVEL0_DATA", "RECS_2009" )
L142.in_EJ_state_bld_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L142.in_EJ_state_bld_F" )
L143.share_state_Pop_CDD_sR13 <- readdata( "GCAMUSA_LEVEL1_DATA", "L143.share_state_Pop_CDD_sR13" )
L143.share_state_Pop_HDD_sR13 <- readdata( "GCAMUSA_LEVEL1_DATA", "L143.share_state_Pop_HDD_sR13" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#2a. PREPARATION AND CLEANING OF RECS DATABASES
L144.RECS_all <- list( RECS_1979, RECS_1984, RECS_1990, RECS_1993, RECS_1997, RECS_2001, RECS_2005, RECS_2009 )
names( L144.RECS_all ) <- paste0( "RECS", c( 1979, 1984, 1990, 1993, 1997, 2001, 2005, 2009 ) )
#Add a vector specifying the census division (subregion9)
for( i in seq( L144.RECS_all ) ){
	L144.RECS_all[[i]][[ "subregion9" ]] <- states_subregions$subregion9[
	  match( L144.RECS_all[[i]][[ "DIVISION" ]], states_subregions$DIVISION ) ]
}
#note that the 2009 RECS has different census divisions (thanks)
L144.RECS_all[[ "RECS2009" ]][[ "subregion9" ]] <- states_subregions$subregion9[
      match( L144.RECS_all[[ "RECS2009" ]][[ "DIVISION" ]], states_subregions$DIVISION2009 ) ]

#Add a vector specifying the census division plus four large states (subregion13)
for( i in seq( L144.RECS_all ) ){
	if( "LRGSTATE" %in% names( L144.RECS_all[[i]] ) ){
	 	L144.RECS_all[[i]][[ "subregion13" ]] <- states_subregions$subregion13[
	      match( L144.RECS_all[[i]][[ "LRGSTATE" ]], states_subregions$LRGSTATE ) ]
		L144.RECS_all[[i]][[ "subregion13" ]][ L144.RECS_all[[i]][[ "LRGSTATE" ]] == 0 ] <-
		  L144.RECS_all[[i]][[ "subregion9" ]][ L144.RECS_all[[i]][[ "LRGSTATE" ]] == 0 ]
	}
}

#the 2009 RECS uses 27 "reportable domains", which consist of 16 single states and 11 small clusters of states.
# while this would enhance the geographic specificity of the RECS data, the small sample sizes cause obviously incorrect
# data on aggregation (e.g., states with < 300 ft2 of residential floorspace per capita)
# For this reason, only the four most populous states are used in addition to the census divisions, consistent with the 1993-2005 RECS
printlog( "NOTE: using subregion13 in the 2009 RECS rather than reportable domains (subregion27) due to sample size issues" )
L144.RECS_all[[ "RECS2009" ]][[ "subregion13" ]] <- states_subregions$subregion13[
      match( L144.RECS_all[[ "RECS2009" ]][[ "REPORTABLE_DOMAIN" ]], states_subregions$REPORTABLE_DOMAIN ) ]

#Convert all missing value strings to 0 in all databases
L144.RECS_all[["RECS1990"]][L144.RECS_all[["RECS1990"]] == 9999999] <- 0
L144.RECS_all[["RECS2005"]][L144.RECS_all[["RECS2005"]] == 9999999] <- 0
L144.RECS_all[["RECS2005"]][is.na( L144.RECS_all[["RECS2005"]] ) ] <- 0

#Aggregate population to the subregion9 and subregion13 levels for calculation of per-capita values
Census_pop_hist[ c( "subregion9", "subregion13" ) ] <- states_subregions[
      match( Census_pop_hist$state, states_subregions$state ),
      c( "subregion9", "subregion13" ) ]
L144.pop_sR13 <- aggregate( Census_pop_hist[ X_historical_years ], by=as.list( Census_pop_hist[ "subregion13" ] ), sum )
L144.pop_sR9 <- aggregate( Census_pop_hist[ X_historical_years ], by=as.list( Census_pop_hist[ "subregion9" ] ), sum )

#2b. FLOORSPACE BY STATE AND YEAR
printlog( "Estimating total floorspace by census division (subregion9) and subregion13 if available, in each RECS year where floorspace is available")
#The variable names differ by edition, but in no case does a variable name mean one thing in one edition and another thing in a later edition
# HOUSEHOLDS is only in the 1984 edition. 1984 also has a different unit on the weight
for( i in seq( L144.RECS_all ) ){
	if( "HOUSEHOLDS" %in% names( L144.RECS_all[[i]] ) ){
	 	data_year <- substr( names( L144.RECS_all )[i], 5, 8 )
	 	flsp_var <- names( L144.RECS_all[[i]] )[ names( L144.RECS_all[[i]] ) %in% c( "UNHEATED", "HOMEAREA", "SQFTREG", "TOTSQFT" ) ]
	 	object <- aggregate_recs(
	 	   L144.RECS_all[[i]], variables = flsp_var, year = paste0( "X", data_year ),
	 	   weight_variable = "HOUSEHOLDS", region_variable = "subregion9", unit_conv = conv_milft2_m2  )
	 	object$pcflsp_m2 <- object[[ flsp_var ]] / L144.pop_sR9[[ paste0( "X", data_year ) ]]
	 	objectname <- paste0( "L144.flsp_bm2_sR9_", names( L144.RECS_all )[i] )
	 	assign( objectname, object )
	}
	if( "NWEIGHT" %in% names( L144.RECS_all[[i]] ) ){
	 	data_year <- substr( names( L144.RECS_all )[i], 5, 8 )
	 	flsp_var <- names( L144.RECS_all[[i]] )[ names( L144.RECS_all[[i]] ) %in% c( "UNHEATED", "HOMEAREA", "SQFTREG", "TOTSQFT" ) ]
	 	object <- aggregate_recs(
	 	   L144.RECS_all[[i]], variables = flsp_var, year = paste0( "X", data_year ),
	 	   weight_variable = "NWEIGHT", region_variable = "subregion9", unit_conv = conv_ft2_m2  )
	 	object$pcflsp_m2 <- object[[ flsp_var ]] / L144.pop_sR9[[ paste0( "X", data_year ) ]]
	 	objectname <- paste0( "L144.flsp_bm2_sR9_", names( L144.RECS_all )[i] )
	 	assign( objectname, object )
	}
	if( "subregion13" %in% names( L144.RECS_all[[i]] ) ){
	 	data_year <- substr( names( L144.RECS_all )[i], 5, 8 )
	 	flsp_var <- names( L144.RECS_all[[i]] )[ names( L144.RECS_all[[i]] ) %in% c( "UNHEATED", "HOMEAREA", "SQFTREG", "TOTSQFT" ) ]
	 	object <- aggregate_recs(
	 	   L144.RECS_all[[i]], variables = flsp_var, year = paste0( "X", data_year ),
	 	   weight_variable = "NWEIGHT", region_variable = "subregion13", unit_conv = conv_ft2_m2  )
	 	object$pcflsp_m2 <- object[[ flsp_var ]] / L144.pop_sR13[[ paste0( "X", data_year ) ]]
	 	objectname <- paste0( "L144.flsp_bm2_sR13_", names( L144.RECS_all )[i] )
	 	assign( objectname, object )
	}
}

#Downscale 1990 and 1984 floorspace to subregion13, using the ratios of per-capita floorspace
L144.flsp_conv_9_13 <- L144.flsp_bm2_sR13_RECS1993
L144.flsp_conv_9_13$subregion9 <- states_subregions$subregion9[ match( L144.flsp_conv_9_13$subregion13, states_subregions$subregion13 ) ]
L144.flsp_conv_9_13$pcflsp_m2_9 <- L144.flsp_bm2_sR9_RECS1993$pcflsp_m2[ match( L144.flsp_conv_9_13$subregion9, L144.flsp_bm2_sR9_RECS1993$subregion9 ) ]
L144.flsp_conv_9_13$conv_9_13 <- L144.flsp_conv_9_13$pcflsp_m2 / L144.flsp_conv_9_13$pcflsp_m2_9

#Multiplying the per-capita floorspace ratios from subregion9 to subregion13, to expand from 9 to 13
L144.flsp_bm2_sR13_RECS1990 <- data.frame(
      L144.flsp_conv_9_13[ "subregion13" ], year = "X1990",
      pcflsp_m2 = L144.flsp_bm2_sR9_RECS1990$pcflsp_m2[
         match( L144.flsp_conv_9_13$subregion9, L144.flsp_bm2_sR9_RECS1990$subregion9 ) ] * L144.flsp_conv_9_13$conv_9_13 )
L144.flsp_bm2_sR13_RECS1984 <- data.frame(
      L144.flsp_conv_9_13[ "subregion13" ], year = "X1984",
      pcflsp_m2 = L144.flsp_bm2_sR9_RECS1984$pcflsp_m2[
         match( L144.flsp_conv_9_13$subregion9, L144.flsp_bm2_sR9_RECS1984$subregion9 ) ] * L144.flsp_conv_9_13$conv_9_13 )

#Merge all tables, and cast by year
flsp_cols <- c( "subregion13", "year", "pcflsp_m2" )
L144.pcflsp_m2_sR13_RECS.melt <- rbind( L144.flsp_bm2_sR13_RECS1984[ flsp_cols ], L144.flsp_bm2_sR13_RECS1990[ flsp_cols ],
      L144.flsp_bm2_sR13_RECS1993[ flsp_cols ], L144.flsp_bm2_sR13_RECS1997[ flsp_cols ], L144.flsp_bm2_sR13_RECS2001[ flsp_cols ],
      L144.flsp_bm2_sR13_RECS2005[ flsp_cols ], L144.flsp_bm2_sR13_RECS2009[ flsp_cols ] )
L144.pcflsp_m2_sR13_RECS <- dcast( L144.pcflsp_m2_sR13_RECS.melt, subregion13 ~ year, value.var = "pcflsp_m2" )
L144.pcflsp_m2_sR13_RECS <- gcam_interp( L144.pcflsp_m2_sR13_RECS, historical_years, rule = 2 )
L144.pcflsp_m2_sR13_res <- L144.pcflsp_m2_sR13_RECS[ c( "subregion13", X_historical_years ) ]

#Expand to states: multiply per-capita floorspace in each subregion13 times the population of each state
L144.flsp_bm2_state_res <- data.frame(
      Census_pop_hist[ c( state, "subregion13" ) ],
      sector = "resid",
      Census_pop_hist[ X_historical_years ] * L144.pcflsp_m2_sR13_res[
          match( Census_pop_hist$subregion13, L144.pcflsp_m2_sR13_res$subregion13 ), X_historical_years ] /
          conv_bm2_m2 )

#2c: ENERGY CONSUMPTION BY STATE, SERVICE, AND YEAR
printlog( "Aggregating energy consumption by sampling weights" )
#Aggregate in a for loop
#First, do 1990 and 1993 by subregion9
for( i in c( "RECS1990", "RECS1993" ) ){
	object.cast <- aggregate_recs(
	   L144.RECS_all[[ i ]],
	   variables = names( L144.RECS_all[[ i ]] )[ names( L144.RECS_all[[ i ]] ) %in% RECS_variables$variable ],
	   year = paste0("X", substr( i, 5, 8 ) ),
	   weight_variable = "NWEIGHT", region_variable = "subregion9", unit_conv = conv_kbtu_EJ )
	object <- melt( object.cast, id.vars = c( "subregion9", "year" ) )
	objectname <- paste0( "L144.in_EJ_sR9_res_", substr( i, 5, 8 ) )
	assign( objectname, object )
}

#Then, do the same for 1993 to 2009 in at the subregion13 level
for( i in c( "RECS1993", "RECS1997", "RECS2001", "RECS2005", "RECS2009" ) ){
	object.cast <- aggregate_recs(
	   L144.RECS_all[[ i ]],
	   variables = names( L144.RECS_all[[ i ]] )[ names( L144.RECS_all[[ i ]] ) %in% RECS_variables$variable ],
	   year = paste0("X", substr( i, 5, 8 ) ),
	   weight_variable = "NWEIGHT", region_variable = "subregion13", unit_conv = conv_kbtu_EJ )
	object <- melt( object.cast, id.vars = c( "subregion13", "year" ) )
	objectname <- paste0( "L144.in_EJ_sR13_res_", substr( i, 5, 8 ) )
	assign( objectname, object )
}

#Rbind these data frames, match in the fuel and service, and aggregate (this will get rid of the different liquid fuels)
L144.in_EJ_sR9_res_F_Urecs_Y <- rbind( L144.in_EJ_sR9_res_1990, L144.in_EJ_sR9_res_1993 )
L144.in_EJ_sR9_res_F_Urecs_Y[ F_U ] <- RECS_variables[ match( L144.in_EJ_sR9_res_F_Urecs_Y$variable, RECS_variables$variable ), F_U ]
L144.in_EJ_sR9_res_F_U_Y.melt <- aggregate( L144.in_EJ_sR9_res_F_Urecs_Y[ "value" ], by=as.list( L144.in_EJ_sR9_res_F_Urecs_Y[ c( "subregion9", F_U, "year" ) ] ), sum )

L144.in_EJ_sR13_res_F_Urecs_Y <- rbind( L144.in_EJ_sR13_res_1993, L144.in_EJ_sR13_res_1997, L144.in_EJ_sR13_res_2001,
      L144.in_EJ_sR13_res_2005, L144.in_EJ_sR13_res_2009 )
L144.in_EJ_sR13_res_F_Urecs_Y[ F_U ] <- RECS_variables[ match( L144.in_EJ_sR13_res_F_Urecs_Y$variable, RECS_variables$variable ), F_U ]
L144.in_EJ_sR13_res_F_U_Y.melt <- aggregate( L144.in_EJ_sR13_res_F_Urecs_Y[ "value" ], by=as.list( L144.in_EJ_sR13_res_F_Urecs_Y[ c( "subregion13", F_U, "year" ) ] ), sum )

#Cast by year, add 1979 to the 9-region table, and use the 9-subregion data to scale the 13-subregion data back to 1990 and 1979
RECS_1979$subregion9 <- states_subregions$subregion9[ match( RECS_1979$DIVISION, states_subregions$DIVISION ) ]
L144.RECS_1979.melt <- melt( RECS_1979, id.vars = c( "DIVISION", "subregion9" ) )
L144.RECS_1979.melt$value <- L144.RECS_1979.melt$value * conv_Tbtu_EJ
L144.RECS_1979.melt$variable <- sub( "TBTU", "BTU", L144.RECS_1979.melt$variable )
L144.RECS_1979.melt[ F_U ] <- RECS_variables[ match( L144.RECS_1979.melt$variable, RECS_variables$variable ), F_U ]

L144.in_EJ_sR9_res_F_U_Y <- dcast( L144.in_EJ_sR9_res_F_U_Y.melt, subregion9 + fuel + service ~ year )
L144.in_EJ_sR9_res_F_U_Y$X1979 <- L144.RECS_1979.melt$value[
      match( vecpaste( L144.in_EJ_sR9_res_F_U_Y[ c( "subregion9", F_U ) ] ),
             vecpaste( L144.RECS_1979.melt[ c( "subregion9", F_U ) ] ) ) ]
L144.in_EJ_sR13_res_F_U_Y <- dcast( L144.in_EJ_sR13_res_F_U_Y.melt, subregion13 + fuel + service ~ year )
L144.in_EJ_sR13_res_F_U_Y$subregion9 <- states_subregions$subregion9[ match( L144.in_EJ_sR13_res_F_U_Y$subregion13, states_subregions$subregion13 ) ]
L144.in_EJ_sR13_res_F_U_Y$X1990 <- L144.in_EJ_sR13_res_F_U_Y$X1993 * L144.in_EJ_sR9_res_F_U_Y$X1990[
      match( vecpaste( L144.in_EJ_sR13_res_F_U_Y[ c( "subregion9", F_U ) ] ),
             vecpaste( L144.in_EJ_sR9_res_F_U_Y[ c( "subregion9", F_U ) ] ) ) ] /
      L144.in_EJ_sR9_res_F_U_Y$X1993[
      match( vecpaste( L144.in_EJ_sR13_res_F_U_Y[ c( "subregion9", F_U ) ] ),
             vecpaste( L144.in_EJ_sR9_res_F_U_Y[ c( "subregion9", F_U ) ] ) ) ]
L144.in_EJ_sR13_res_F_U_Y$X1979 <- L144.in_EJ_sR13_res_F_U_Y$X1990 * L144.in_EJ_sR9_res_F_U_Y$X1979[
      match( vecpaste( L144.in_EJ_sR13_res_F_U_Y[ c( "subregion9", F_U ) ] ),
             vecpaste( L144.in_EJ_sR9_res_F_U_Y[ c( "subregion9", F_U ) ] ) ) ] /
      L144.in_EJ_sR9_res_F_U_Y$X1990[
      match( vecpaste( L144.in_EJ_sR13_res_F_U_Y[ c( "subregion9", F_U ) ] ),
             vecpaste( L144.in_EJ_sR9_res_F_U_Y[ c( "subregion9", F_U ) ] ) ) ]

# Interpolate and extrapolate all missing years
L144.in_EJ_sR13_res_F_U_Y <- gcam_interp( L144.in_EJ_sR13_res_F_U_Y, historical_years, rule = 2 )[
      c( "subregion13", F_U, X_historical_years ) ]

# Next, disaggregate "appliances and other" for each fuel to all relevant services that are being modeled
printlog( "Using EIA AEO Table 4 from 1996-2013 editions to disaggregate residential appliances and other to more specific end uses")
printlog( "NOTE: The national averages of appliances and other energy are assumed constant in all states" )
printlog( "NOTE: The national estimate for lighting energy increased substantially from 1999 to 2000 because a different method was used")
printlog( "NOTE: We double the lighting energy in the prior years, deducting the balance from Other Uses" )
X_EIA_AEO_years <- paste0( "X", 1993:2010 )
X_lgt_adj_years <- paste0( "X", 1993:1999 )
L144.EIA_AEO_Tab4 <- EIA_AEO_Tab4
L144.EIA_AEO_Tab4[ L144.EIA_AEO_Tab4$Fuel == "Electricity" & L144.EIA_AEO_Tab4$Service == "Other Uses", X_lgt_adj_years ] <- 
      L144.EIA_AEO_Tab4[ L144.EIA_AEO_Tab4$Fuel == "Electricity" & L144.EIA_AEO_Tab4$Service == "Other Uses", X_lgt_adj_years ] -
      L144.EIA_AEO_Tab4[ L144.EIA_AEO_Tab4$Fuel == "Electricity" & L144.EIA_AEO_Tab4$Service == "Lighting", X_lgt_adj_years ]
L144.EIA_AEO_Tab4[ L144.EIA_AEO_Tab4$Fuel == "Electricity" & L144.EIA_AEO_Tab4$Service == "Lighting", X_lgt_adj_years ] <- 
      L144.EIA_AEO_Tab4[ L144.EIA_AEO_Tab4$Fuel == "Electricity" & L144.EIA_AEO_Tab4$Service == "Lighting", X_lgt_adj_years ] * 2

printlog( "NOTE: Television energy was not estimated prior to 1995. Copying the 1995 output to prior years" )
L144.EIA_AEO_Tab4[ L144.EIA_AEO_Tab4$Fuel == "Electricity" & L144.EIA_AEO_Tab4$Service == "Other Uses", c( "X1993", "X1994" ) ] <- 
      L144.EIA_AEO_Tab4[ L144.EIA_AEO_Tab4$Fuel == "Electricity" & L144.EIA_AEO_Tab4$Service == "Other Uses", c( "X1993", "X1994" ) ] -
      L144.EIA_AEO_Tab4$X1995[ L144.EIA_AEO_Tab4$Fuel == "Electricity" & L144.EIA_AEO_Tab4$Service == "Color Televisions" ]
L144.EIA_AEO_Tab4[ L144.EIA_AEO_Tab4$Fuel == "Electricity" & L144.EIA_AEO_Tab4$Service == "Color Televisions", c( "X1993", "X1994" ) ] <- 
      L144.EIA_AEO_Tab4$X1995[ L144.EIA_AEO_Tab4$Fuel == "Electricity" & L144.EIA_AEO_Tab4$Service == "Color Televisions" ]

#Match in GCAM services and fuels
L144.EIA_AEO_Tab4$fuel <- EIA_AEO_fuels$fuel[ match( L144.EIA_AEO_Tab4$Fuel, EIA_AEO_fuels$EIA_fuel ) ]
L144.EIA_AEO_Tab4$service <- EIA_AEO_services $service[ match( L144.EIA_AEO_Tab4$Service, EIA_AEO_services$EIA_service ) ]

#Compute shares of "appliances and other" energy
appl_other_services <- unique( EIA_AEO_services$service )[ !unique( EIA_AEO_services$service ) %in% RECS_variables$service ]
L144.EIA_AEO_appl_other_F <- aggregate( L144.EIA_AEO_Tab4[ L144.EIA_AEO_Tab4$service %in% appl_other_services, X_EIA_AEO_years ],
      by=as.list( L144.EIA_AEO_Tab4[ L144.EIA_AEO_Tab4$service %in% appl_other_services, F_U ] ), sum )
L144.EIA_AEO_appl_other <- aggregate( L144.EIA_AEO_appl_other_F[ X_EIA_AEO_years ], by=as.list( L144.EIA_AEO_appl_other_F[ "fuel" ] ), sum )

#Compute shares. These will be used in all states
L144.shares_appl_other_F <- L144.EIA_AEO_appl_other_F
L144.shares_appl_other_F[ X_EIA_AEO_years ] <- L144.EIA_AEO_appl_other_F[ X_EIA_AEO_years ] / L144.EIA_AEO_appl_other[
      match( L144.shares_appl_other_F$fuel, L144.EIA_AEO_appl_other$fuel ),
      X_EIA_AEO_years ]

#Extrapolate to all historical years
L144.shares_appl_other_F <- gcam_interp( L144.shares_appl_other_F, historical_years, rule = 2 )[ c( "service", "fuel", X_historical_years ) ]

#At this point, we have a table of energy by subregion13, fuel, and end use that needs to be (a) apportioned to states, and (b) scaled
# The reason for apportioning to states first is that the heating and cooling energy will be modified by pop-weighted HDD and CDD
# prior to calculating energy shares
printlog( "Downscaling heating and cooling energy to states according to person-HDD and -CDD" )
L144.in_EJ_state_res_F_heating_Y <- data.frame(
      repeat_and_add_vector( states_subregions[ c( state, "subregion13" ) ], "fuel", RECS_heating_fuels ),
      service = "resid heating" )
L144.in_EJ_state_res_F_heating_Y[ X_historical_years ] <-
      L144.in_EJ_sR13_res_F_U_Y[
         match( vecpaste( L144.in_EJ_state_res_F_heating_Y[ c( "subregion13", F_U ) ] ),
                vecpaste( L144.in_EJ_sR13_res_F_U_Y[ c( "subregion13", F_U ) ] ) ),
         X_historical_years ] *
      L143.share_state_Pop_HDD_sR13[
         match( L144.in_EJ_state_res_F_heating_Y$state, L143.share_state_Pop_HDD_sR13$state ),
         X_historical_years ]

L144.in_EJ_state_res_F_cooling_Y <- data.frame(
      repeat_and_add_vector( states_subregions[ c( state, "subregion13" ) ], "fuel", RECS_cooling_fuels ),
      service = "resid cooling" )
L144.in_EJ_state_res_F_cooling_Y[ X_historical_years ] <-
      L144.in_EJ_sR13_res_F_U_Y[
         match( vecpaste( L144.in_EJ_state_res_F_cooling_Y[ c( "subregion13", F_U ) ] ),
                vecpaste( L144.in_EJ_sR13_res_F_U_Y[ c( "subregion13", F_U ) ] ) ),
         X_historical_years ] *
      L143.share_state_Pop_CDD_sR13[
         match( L144.in_EJ_state_res_F_cooling_Y$state, L143.share_state_Pop_CDD_sR13$state ),
         X_historical_years ]

printlog( "Downscaling water heating energy to states according to population" )
L144.state_pop_share_sR13 <- Census_pop_hist[ c( state, "subregion13" ) ]
L144.state_pop_share_sR13[ X_historical_years ] <- Census_pop_hist[ X_historical_years ] / L144.pop_sR13[
      match( L144.state_pop_share_sR13$subregion13, L144.pop_sR13$subregion13 ),
      X_historical_years ]
L144.in_EJ_state_res_F_hotwater_Y <- data.frame(
      repeat_and_add_vector( states_subregions[ c( state, "subregion13" ) ], "fuel", RECS_hotwater_fuels ),
      service = "resid hot water" )
L144.in_EJ_state_res_F_hotwater_Y[ X_historical_years ] <-
      L144.in_EJ_sR13_res_F_U_Y[
         match( vecpaste( L144.in_EJ_state_res_F_hotwater_Y[ c( "subregion13", F_U ) ] ),
                vecpaste( L144.in_EJ_sR13_res_F_U_Y[ c( "subregion13", F_U ) ] ) ),
         X_historical_years ] *
      L144.state_pop_share_sR13[ match( L144.in_EJ_state_res_F_hotwater_Y$state, L144.state_pop_share_sR13$state ),
         X_historical_years ]

printlog( "Downscaling appliances and other energy to states according to floorspace" )
L144.in_EJ_sR13_res_F_apploth_Y <- subset(L144.in_EJ_sR13_res_F_U_Y, service == "resid appliances and other" )
L144.flsp_bm2_sR13_res <- aggregate( L144.flsp_bm2_state_res[ X_historical_years ],
      by=as.list( L144.flsp_bm2_state_res[ "subregion13" ] ), sum )
L144.flsp_state_share_sR13 <- data.frame( 
      L144.flsp_bm2_state_res[ c( state, "subregion13" ) ],
      L144.flsp_bm2_state_res[ X_historical_years ] / L144.flsp_bm2_sR13_res[
         match( L144.flsp_bm2_state_res$subregion13, L144.flsp_bm2_sR13_res$subregion13 ),
         X_historical_years ] )
L144.in_EJ_state_res_F_apploth_Y <- repeat_and_add_vector( L144.shares_appl_other_F, state, states )
L144.in_EJ_state_res_F_apploth_Y$subregion13 <- states_subregions$subregion13[
      match( L144.in_EJ_state_res_F_apploth_Y$state, states_subregions$state ) ]
L144.in_EJ_state_res_F_apploth_Y[ X_historical_years ] <-
      L144.in_EJ_state_res_F_apploth_Y[ X_historical_years ] *         #Start with the shares of specific end use within appl_other and fuel
      L144.in_EJ_sR13_res_F_apploth_Y[                                 #Multiply by appl_other energy consumption by subregion13 and fuel
         match( vecpaste( L144.in_EJ_state_res_F_apploth_Y[ c( "subregion13", "fuel" ) ] ),
                vecpaste( L144.in_EJ_sR13_res_F_apploth_Y[ c( "subregion13", "fuel" ) ] ) ),
         X_historical_years ] *
      L144.flsp_state_share_sR13[ match( L144.in_EJ_state_res_F_apploth_Y$state, L144.flsp_state_share_sR13$state ), #Finish by state-within-subregion13 floorspace shares
         X_historical_years ]

printlog( "Assembling unscaled energy consumption by state, fuel, and service" )
L144.in_EJ_state_res_F_U_Y_unscaled <- rbind(
      L144.in_EJ_state_res_F_heating_Y, L144.in_EJ_state_res_F_cooling_Y, L144.in_EJ_state_res_F_hotwater_Y, L144.in_EJ_state_res_F_apploth_Y )

printlog( "Calculating shares of energy consumption by each service, within each state and fuel")
L144.in_EJ_state_res_F_Y_unscaled <- aggregate( L144.in_EJ_state_res_F_U_Y_unscaled[ X_historical_years ],
      by=as.list( L144.in_EJ_state_res_F_U_Y_unscaled[ state_F ] ), sum )
L144.pct_state_res_F_U_Y <- L144.in_EJ_state_res_F_U_Y_unscaled
L144.pct_state_res_F_U_Y[ X_historical_years ] <- L144.in_EJ_state_res_F_U_Y_unscaled[ X_historical_years ] / L144.in_EJ_state_res_F_Y_unscaled[
      match( vecpaste( L144.in_EJ_state_res_F_U_Y_unscaled[ state_F ] ),
             vecpaste( L144.in_EJ_state_res_F_Y_unscaled[ state_F ] ) ),
      X_historical_years ]
L144.pct_state_res_F_U_Y$sector <- "resid"

# At this point we can disaggregate the state-level energy consumption by sector and fuel to the specific end uses
L144.in_EJ_state_res_F_U_Y <- apportion_to_states(
      nation_data = L142.in_EJ_state_bld_F,
      state_share_data = L144.pct_state_res_F_U_Y,
      match_vectors = state_S_F )[ c( state, S_F_U, X_historical_years ) ]

#This table needs to have coal and biomass added; just assign these to heating
L144.in_EJ_state_bld_Fnorecs <- subset( L142.in_EJ_state_bld_F, sector == "resid" & !fuel %in% L144.in_EJ_state_res_F_U_Y$fuel )
L144.in_EJ_state_bld_Fnorecs$service <- "resid heating"
L144.in_EJ_state_bld_Fnorecs <- L144.in_EJ_state_bld_Fnorecs[ c( state, S_F_U, X_historical_years ) ]
L144.in_EJ_state_res_F_U_Y <- rbind( L144.in_EJ_state_bld_Fnorecs, L144.in_EJ_state_res_F_U_Y )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L144.flsp_bm2_state_res <- c( "Residential floorspace by state","Unit = billion m2" )
comments.L144.in_EJ_state_res_F_U_Y <- c( "Residential energy consumption by state / fuel / end use","Unit = EJ/yr" )

#write tables as CSV files
writedata( L144.flsp_bm2_state_res, domain="GCAMUSA_LEVEL1_DATA", fn="L144.flsp_bm2_state_res", comments=comments.L144.flsp_bm2_state_res )
writedata( L144.in_EJ_state_res_F_U_Y, domain="GCAMUSA_LEVEL1_DATA", fn="L144.in_EJ_state_res_F_U_Y", comments=comments.L144.in_EJ_state_res_F_U_Y )

# Every script should finish with this line

logstop()