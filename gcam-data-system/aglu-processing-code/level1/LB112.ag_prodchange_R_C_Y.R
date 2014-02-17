# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "AGLUPROC_DIR" ) ){
    if( Sys.getenv( "AGLUPROC" ) != "" ){
        AGLUPROC_DIR <- Sys.getenv( "AGLUPROC" )
    } else {
        stop("Could not determine location of aglu processing scripts, please set the R var AGLUPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
logstart( "LB112.ag_prodchange_R_C_Y.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Agricultural productivity change from FAO CROSIT model, 2005-2050" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
AGLU_ctry <- readdata( "AGLU_MAPPINGS", "AGLU_ctry" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
FAO_ag_CROSIT <- readdata( "AGLU_LEVEL0_DATA", "FAO_ag_CROSIT" )
GTAP_ag_Prod_t <- readdata( "AGLU_LEVEL0_DATA", "GTAP_ag_Prod_t" )
GTAP_ag_HA_ha <- readdata( "AGLU_LEVEL0_DATA", "GTAP_ag_HA_ha" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#2a. Initial preparation of CROSIT database - replace country and crop IDs with names, and add X to the year (for casting)
FAO_ag_CROSIT$CROSIT_ctry <- AGLU_ctry$CROSIT_ctry[ match( FAO_ag_CROSIT$country_ID, AGLU_ctry$CROSIT_country_ID ) ]
FAO_ag_CROSIT$CROSIT_crop <- FAO_ag_items_PRODSTAT$CROSIT_crop[ match( FAO_ag_CROSIT$crop_ID, FAO_ag_items_PRODSTAT$CROSIT_cropID ) ]
FAO_ag_CROSIT$Xyear <- paste( "X", FAO_ag_CROSIT$year, sep = "" )

#Intial data clean-up: some regions have 0 production with positive harvested area and a reported yield. Re-calculate production where yields are available.
FAO_ag_CROSIT$Prod_kt[ FAO_ag_CROSIT$Prod_kt == 0 & FAO_ag_CROSIT$HA_kha != 0 ] <-
      FAO_ag_CROSIT$HA_kha[ FAO_ag_CROSIT$Prod_kt == 0 & FAO_ag_CROSIT$HA_kha != 0 ] * 
      FAO_ag_CROSIT$Yield_kgHa[ FAO_ag_CROSIT$Prod_kt == 0 & FAO_ag_CROSIT$HA_kha != 0 ] * 0.001
#Where no yields are given, re-set harvested area to 0
FAO_ag_CROSIT$HA_kha[ FAO_ag_CROSIT$Prod_kt == 0 & FAO_ag_CROSIT$HA_kha != 0 ] <- 0

#Cast by year
printlog( "NOTE: Only using total production and harvested area from CROSIT (not separating rainfed and irrigated)")
crs_ID <- c( "CROSIT_ctry", "CROSIT_crop" )
L112.ag_Yield_kgHa_Rcrs_Ccrs_Y <- dcast( FAO_ag_CROSIT[ c( crs_ID, "Xyear", "Yield_kgHa") ], CROSIT_ctry + CROSIT_crop ~ Xyear, value.var = "Yield_kgHa" )

#Interpolate to specified agricultural productivity years, and drop any years not specified
L112.ag_Yield_kgHa_Rcrs_Ccrs_Y <- gcam_interp( L112.ag_Yield_kgHa_Rcrs_Ccrs_Y, spec_ag_prod_years )

#Order the columns
L112.ag_Yield_kgHa_Rcrs_Ccrs_Y <- L112.ag_Yield_kgHa_Rcrs_Ccrs_Y[ c( crs_ID, X_spec_ag_prod_years ) ]

#Calculate yield multipliers from the base year
printlog( "Calculating CROSIT multipliers from the base year to all specified years, for yield")
L112.ag_Yieldmult_Rcrs_Ccrs_Y <- data.frame(
      L112.ag_Yield_kgHa_Rcrs_Ccrs_Y[ crs_ID ],
      L112.ag_Yield_kgHa_Rcrs_Ccrs_Y[ X_spec_ag_prod_years ] / L112.ag_Yield_kgHa_Rcrs_Ccrs_Y[[ X_spec_ag_prod_years[ 1 ] ]] )

#Drop the NaN's (crops with zero base year production / harvested area)
L112.ag_Yieldmult_Rcrs_Ccrs_Y <- na.omit( L112.ag_Yieldmult_Rcrs_Ccrs_Y )

#Melt the multipliers and specify the year
L112.ag_Yieldmult_Rcrs_Ccrs_Y.melt <- melt( L112.ag_Yieldmult_Rcrs_Ccrs_Y, id.vars = crs_ID )
L112.ag_Yieldmult_Rcrs_Ccrs_Y.melt$year <- as.numeric( substr( L112.ag_Yieldmult_Rcrs_Ccrs_Y.melt$variable, 2, 5 ) )

#These multipliers are now ready to be matched into the GTAP tables of all country x crop x AEZ production and harvested area in the base year
#First, match in the CROSIT region and commodity names for aggregation
printlog( "Aggregating GTAP databases for production and harvested area by CROSIT region and crop" )
L112.GTAP_ag_Prod_t <- GTAP_ag_Prod_t
L112.GTAP_ag_Prod_t$CROSIT_ctry <- AGLU_ctry$CROSIT_ctry[ match( L112.GTAP_ag_Prod_t$ctry, AGLU_ctry$iso ) ]
L112.GTAP_ag_Prod_t$CROSIT_crop <- FAO_ag_items_PRODSTAT$CROSIT_crop[ match( L112.GTAP_ag_Prod_t$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L112.GTAP_ag_Prod_t <- na.omit( L112.GTAP_ag_Prod_t )

L112.GTAP_ag_HA_ha <- GTAP_ag_HA_ha
L112.GTAP_ag_HA_ha$CROSIT_ctry <- AGLU_ctry$CROSIT_ctry[ match( L112.GTAP_ag_HA_ha$ctry, AGLU_ctry$iso ) ]
L112.GTAP_ag_HA_ha$CROSIT_crop <- FAO_ag_items_PRODSTAT$CROSIT_crop[ match( L112.GTAP_ag_HA_ha$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L112.GTAP_ag_HA_ha <- na.omit( L112.GTAP_ag_HA_ha )

#Aggregate by CROSIT region and commodity
L112.ag_Prod_t_Rcrs_Ccrs_AEZ <- aggregate( L112.GTAP_ag_Prod_t[ AEZs ], by=as.list( L112.GTAP_ag_Prod_t[ crs_ID ] ), sum )
L112.ag_HA_ha_Rcrs_Ccrs_AEZ <- aggregate( L112.GTAP_ag_HA_ha[ AEZs ], by=as.list( L112.GTAP_ag_HA_ha[ crs_ID ] ), sum )

#Drop all crop x commodity combinations not present in the CROSIT yield multiplier data
L112.ag_Prod_t_Rcrs_Ccrs_AEZ <- L112.ag_Prod_t_Rcrs_Ccrs_AEZ[
      vecpaste( L112.ag_Prod_t_Rcrs_Ccrs_AEZ[ crs_ID ] ) %in%
      vecpaste( L112.ag_Yieldmult_Rcrs_Ccrs_Y[ crs_ID ] ), ]

L112.ag_HA_ha_Rcrs_Ccrs_AEZ <- L112.ag_HA_ha_Rcrs_Ccrs_AEZ[
      vecpaste( L112.ag_HA_ha_Rcrs_Ccrs_AEZ[ crs_ID ] ) %in%
      vecpaste( L112.ag_Yieldmult_Rcrs_Ccrs_Y[ crs_ID ] ), ]

#Repeat by the number of years in the specified agricultural productivity set, and add a year vector
printlog( "Repeating GTAP AEZ production / harvested area data by number of model timesteps" )
L112.ag_Prod_t_Rcrs_Ccrs_Ysy_AEZ <- repeat_and_add_vector( L112.ag_Prod_t_Rcrs_Ccrs_AEZ, Y, spec_ag_prod_years )
L112.ag_HA_ha_Rcrs_Ccrs_Ysy_AEZ <- repeat_and_add_vector( L112.ag_HA_ha_Rcrs_Ccrs_AEZ, Y, spec_ag_prod_years )

#Match in the productivity / harvested area multipliers
printlog( "Matching CROSIT production and harvested area multipliers into GTAP AEZ production database" )
#Use yield ratio for changes in production, and hold harvested area fixed. This removes bias from changes in composition of commodities
L112.ag_Prod_t_Rcrs_Ccrs_Ysy_AEZ$Mult <- L112.ag_Yieldmult_Rcrs_Ccrs_Y.melt$value[
      match( vecpaste( L112.ag_Prod_t_Rcrs_Ccrs_Ysy_AEZ[ c( crs_ID, Y ) ] ),
             vecpaste( L112.ag_Yieldmult_Rcrs_Ccrs_Y.melt[ c( crs_ID, Y ) ] ) ) ]
L112.ag_HA_ha_Rcrs_Ccrs_Ysy_AEZ$Mult <- 1

#Multiply through to get the projected production (assuming constant land area)
printlog( "Calculating the adjusted production / harvested area in each time period, for each CROSIT country / crop / AEZ" )
#NOTE: Need to start from full GTAP tables for composite regions and commodities in the CROSIT database
L112.ag_Prod_t_ctry_crop_Ysy <- repeat_and_add_vector( L112.GTAP_ag_Prod_t, Y, spec_ag_prod_years )
L112.ag_HA_ha_ctry_crop_Ysy <- repeat_and_add_vector( L112.GTAP_ag_HA_ha, Y, spec_ag_prod_years )

#Match in multipliers (from CROSIT database), GCAM regions, and GCAM commodities
L112.ag_Prod_t_ctry_crop_Ysy$Mult <- L112.ag_Prod_t_Rcrs_Ccrs_Ysy_AEZ$Mult[
      match( vecpaste( L112.ag_Prod_t_ctry_crop_Ysy[ c( crs_ID, Y ) ] ),
             vecpaste( L112.ag_Prod_t_Rcrs_Ccrs_Ysy_AEZ[ c( crs_ID, Y ) ] ) ) ]
L112.ag_Prod_t_ctry_crop_Ysy <- na.omit( L112.ag_Prod_t_ctry_crop_Ysy )
L112.ag_Prod_t_ctry_crop_Ysy$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L112.ag_Prod_t_ctry_crop_Ysy$ctry, iso_GCAM_regID$iso ) ]
L112.ag_Prod_t_ctry_crop_Ysy$GCAM_commodity <- FAO_ag_items_PRODSTAT$GCAM_commodity[ match( L112.ag_Prod_t_ctry_crop_Ysy$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]

L112.ag_HA_ha_ctry_crop_Ysy$Mult <- L112.ag_HA_ha_Rcrs_Ccrs_Ysy_AEZ$Mult[
      match( vecpaste( L112.ag_HA_ha_ctry_crop_Ysy[ c( crs_ID, Y ) ] ),
             vecpaste( L112.ag_HA_ha_Rcrs_Ccrs_Ysy_AEZ[ c( crs_ID, Y ) ] ) ) ]
L112.ag_HA_ha_ctry_crop_Ysy <- na.omit( L112.ag_HA_ha_ctry_crop_Ysy )
L112.ag_HA_ha_ctry_crop_Ysy$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L112.ag_HA_ha_ctry_crop_Ysy$ctry, iso_GCAM_regID$iso ) ]
L112.ag_HA_ha_ctry_crop_Ysy$GCAM_commodity <- FAO_ag_items_PRODSTAT$GCAM_commodity[ match( L112.ag_HA_ha_ctry_crop_Ysy$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]

#NOTE: Do not read these lines iteratively
L112.ag_Prod_t_ctry_crop_Ysy[ AEZs ] <- L112.ag_Prod_t_ctry_crop_Ysy[ AEZs ] * L112.ag_Prod_t_ctry_crop_Ysy$Mult
L112.ag_HA_ha_ctry_crop_Ysy[ AEZs ] <- L112.ag_HA_ha_ctry_crop_Ysy[ AEZs ] * L112.ag_HA_ha_ctry_crop_Ysy$Mult

#Aggregate by GCAM region and commodity
printlog( "Aggregating by GCAM region / commodity / year to calculate change in productivity from the base year" )
L112.ag_Prod_t_R_C_Ysy_AEZ <- aggregate( L112.ag_Prod_t_ctry_crop_Ysy[ AEZs ], by=as.list( L112.ag_Prod_t_ctry_crop_Ysy[ R_C_Y ] ), sum )
L112.ag_HA_ha_R_C_Ysy_AEZ <- aggregate( L112.ag_HA_ha_ctry_crop_Ysy[ AEZs ], by=as.list( L112.ag_HA_ha_ctry_crop_Ysy[ R_C_Y ] ), sum )

#Calculate the yield as production divided by harvested area
L112.ag_Yield_tha_R_C_Ysy_AEZ <- L112.ag_Prod_t_R_C_Ysy_AEZ
L112.ag_Yield_tha_R_C_Ysy_AEZ[ AEZs ] <- L112.ag_Prod_t_R_C_Ysy_AEZ[ AEZs ] / L112.ag_HA_ha_R_C_Ysy_AEZ[ 
      match( vecpaste( L112.ag_Yield_tha_R_C_Ysy_AEZ[ R_C_Y ] ),
             vecpaste( L112.ag_HA_ha_R_C_Ysy_AEZ[ R_C_Y ] ) ), AEZs ]
L112.ag_Yield_tha_R_C_Ysy_AEZ[ is.na( L112.ag_Yield_tha_R_C_Ysy_AEZ ) ] <- 0

#Calculate productivity multipliers from the first time period
L112.ag_Yield_tha_R_C_Yfby_AEZ <- subset( L112.ag_Yield_tha_R_C_Ysy_AEZ, year == min( spec_ag_prod_years ) )
L112.ag_YieldRatio_R_C_Ysy_AEZ <- L112.ag_Yield_tha_R_C_Ysy_AEZ
L112.ag_YieldRatio_R_C_Ysy_AEZ[ AEZs ] <-  L112.ag_Yield_tha_R_C_Ysy_AEZ[ AEZs ] / L112.ag_Yield_tha_R_C_Yfby_AEZ[
      match( vecpaste( L112.ag_Yield_tha_R_C_Ysy_AEZ[ R_C ] ), vecpaste( L112.ag_Yield_tha_R_C_Yfby_AEZ[ R_C ] ) ),
      AEZs ]
L112.ag_YieldRatio_R_C_Ysy_AEZ[ is.na( L112.ag_YieldRatio_R_C_Ysy_AEZ ) ] <- 1

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L112.ag_Yield_tha_R_C_Ysy_AEZ <- c( "Baseline yield by GCAM region / commodity / future year (specified)","Unit: tonnes per hectare" )
comments.L112.ag_YieldRatio_R_C_Ysy_AEZ <- c( "Yield change ratios by GCAM region / commodity (specified) / future year (specified)","Unitless" )

#write tables as CSV files
writedata( L112.ag_Yield_tha_R_C_Ysy_AEZ, domain="AGLU_LEVEL1_DATA",fn="L112.ag_Yield_tha_R_C_Ysy_AEZ", comments=comments.L112.ag_Yield_tha_R_C_Ysy_AEZ )
writedata( L112.ag_YieldRatio_R_C_Ysy_AEZ, domain="AGLU_LEVEL1_DATA",fn="L112.ag_YieldRatio_R_C_Ysy_AEZ", comments=comments.L112.ag_YieldRatio_R_C_Ysy_AEZ )

# Every script should finish with this line
logstop()
