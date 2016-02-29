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
logstart( "LB153.ag_prodchange_R_C_Y_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Agricultural productivity change from FAO CROSIT model, 2005-2050" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
AGLU_ctry <- readdata( "AGLU_MAPPINGS", "AGLU_ctry" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
FAO_ag_CROSIT <- readdata( "AGLU_LEVEL0_DATA", "FAO_ag_CROSIT" )
FAO_ag_items_PRODSTAT <- readdata( "AGLU_MAPPINGS", "FAO_ag_items_PRODSTAT" )
L151.GTAP_ag_irrProd_t <- readdata( "AGLU_LEVEL1_DATA", "L151.GTAP_ag_irrProd_t" )
L151.GTAP_ag_rfdProd_t <- readdata( "AGLU_LEVEL1_DATA", "L151.GTAP_ag_rfdProd_t" )
L151.GTAP_ag_irrHA_ha <- readdata( "AGLU_LEVEL1_DATA", "L151.GTAP_ag_irrHA_ha" )
L151.GTAP_ag_rfdHA_ha <- readdata( "AGLU_LEVEL1_DATA", "L151.GTAP_ag_rfdHA_ha" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#2a. Initial preparation of CROSIT database - replace country and crop IDs with names, and add X to the year (for casting)
FAO_ag_CROSIT$CROSIT_ctry <- AGLU_ctry$CROSIT_ctry[ match( FAO_ag_CROSIT$country_ID, AGLU_ctry$CROSIT_country_ID ) ]
FAO_ag_CROSIT$CROSIT_crop <- FAO_ag_items_PRODSTAT$CROSIT_crop[ match( FAO_ag_CROSIT$crop_ID, FAO_ag_items_PRODSTAT$CROSIT_cropID ) ]
FAO_ag_CROSIT$Xyear <- paste( "X", FAO_ag_CROSIT$year, sep = "" )

#Intial data clean-up: some regions have 0 production with positive harvested area and a reported yield. Re-calculate production where yields are available.
FAO_ag_CROSIT$Prod_kt_irrigated[ FAO_ag_CROSIT$Prod_kt_irrigated == 0 & FAO_ag_CROSIT$HA_kha_irrigated != 0 ] <-
      FAO_ag_CROSIT$HA_kha_irrigated[ FAO_ag_CROSIT$Prod_kt_irrigated == 0 & FAO_ag_CROSIT$HA_kha_irrigated != 0 ] * 
      FAO_ag_CROSIT$Yield_kgHa_irrigated[ FAO_ag_CROSIT$Prod_kt_irrigated == 0 & FAO_ag_CROSIT$HA_kha_irrigated != 0 ] * 0.001

FAO_ag_CROSIT$Prod_kt_rainfed[ FAO_ag_CROSIT$Prod_kt_rainfed == 0 & FAO_ag_CROSIT$HA_kha_rainfed != 0 ] <-
      FAO_ag_CROSIT$HA_kha_rainfed[ FAO_ag_CROSIT$Prod_kt_rainfed == 0 & FAO_ag_CROSIT$HA_kha_rainfed != 0 ] * 
      FAO_ag_CROSIT$Yield_kgHa_rainfed[ FAO_ag_CROSIT$Prod_kt_rainfed == 0 & FAO_ag_CROSIT$HA_kha_rainfed != 0 ] * 0.001

#Where no yields are given, re-set harvested area to 0
FAO_ag_CROSIT$HA_kha_irrigated[ FAO_ag_CROSIT$Prod_kt_irrigated == 0 & FAO_ag_CROSIT$HA_kha_irrigated != 0 ] <- 0
FAO_ag_CROSIT$HA_kha_rainfed[ FAO_ag_CROSIT$Prod_kt_rainfed == 0 & FAO_ag_CROSIT$HA_kha_rainfed != 0 ] <- 0

#Cast by year
printlog( "NOTE: We are separating rainfed and irrigated production from the CROSIT data")
crs_ID <- c( "CROSIT_ctry", "CROSIT_crop" )
L153.ag_irrYield_kgHa_Rcrs_Ccrs_Y <- dcast( FAO_ag_CROSIT, CROSIT_ctry + CROSIT_crop ~ Xyear, value.var = "Yield_kgHa_irrigated" )
L153.ag_rfdYield_kgHa_Rcrs_Ccrs_Y <- dcast( FAO_ag_CROSIT, CROSIT_ctry + CROSIT_crop ~ Xyear, value.var = "Yield_kgHa_rainfed" )

#Interpolate to specified agricultural productivity years, and drop any years not specified
L153.ag_irrYield_kgHa_Rcrs_Ccrs_Y <- gcam_interp( L153.ag_irrYield_kgHa_Rcrs_Ccrs_Y, spec_ag_prod_years )
L153.ag_rfdYield_kgHa_Rcrs_Ccrs_Y <- gcam_interp( L153.ag_rfdYield_kgHa_Rcrs_Ccrs_Y, spec_ag_prod_years )

#Order the columns
L153.ag_irrYield_kgHa_Rcrs_Ccrs_Y <- L153.ag_irrYield_kgHa_Rcrs_Ccrs_Y[ c( crs_ID, X_spec_ag_prod_years ) ]
L153.ag_rfdYield_kgHa_Rcrs_Ccrs_Y <- L153.ag_rfdYield_kgHa_Rcrs_Ccrs_Y[ c( crs_ID, X_spec_ag_prod_years ) ]

#Calculate yield multipliers from the base year
printlog( "Calculating CROSIT multipliers from the base year to all specified years, for yield")
L153.ag_irrYieldmult_Rcrs_Ccrs_Y <- data.frame(
      L153.ag_irrYield_kgHa_Rcrs_Ccrs_Y[ crs_ID ],
      L153.ag_irrYield_kgHa_Rcrs_Ccrs_Y[ X_spec_ag_prod_years ] / L153.ag_irrYield_kgHa_Rcrs_Ccrs_Y[[ X_spec_ag_prod_years[ 1 ] ]] )
L153.ag_rfdYieldmult_Rcrs_Ccrs_Y <- data.frame(
      L153.ag_rfdYield_kgHa_Rcrs_Ccrs_Y[ crs_ID ],
      L153.ag_rfdYield_kgHa_Rcrs_Ccrs_Y[ X_spec_ag_prod_years ] / L153.ag_rfdYield_kgHa_Rcrs_Ccrs_Y[[ X_spec_ag_prod_years[ 1 ] ]] )

#Drop the NaN's (crops with zero base year production / harvested area)
L153.ag_irrYieldmult_Rcrs_Ccrs_Y <- na.omit( L153.ag_irrYieldmult_Rcrs_Ccrs_Y )
L153.ag_rfdYieldmult_Rcrs_Ccrs_Y <- na.omit( L153.ag_rfdYieldmult_Rcrs_Ccrs_Y )

#Melt the multipliers and specify the year
L153.ag_irrYieldmult_Rcrs_Ccrs_Y.melt <- melt( L153.ag_irrYieldmult_Rcrs_Ccrs_Y, id.vars = crs_ID, value.name = "Mult" )
L153.ag_irrYieldmult_Rcrs_Ccrs_Y.melt$year <- as.numeric( substr( L153.ag_irrYieldmult_Rcrs_Ccrs_Y.melt$variable, 2, 5 ) )
L153.ag_rfdYieldmult_Rcrs_Ccrs_Y.melt <- melt( L153.ag_rfdYieldmult_Rcrs_Ccrs_Y, id.vars = crs_ID, value.name = "Mult" )
L153.ag_rfdYieldmult_Rcrs_Ccrs_Y.melt$year <- as.numeric( substr( L153.ag_rfdYieldmult_Rcrs_Ccrs_Y.melt$variable, 2, 5 ) )

#These multipliers are now ready to be matched into the GTAP tables of all country x crop x AEZ production and harvested area in the base year
#First, match in the CROSIT region and commodity names for aggregation
printlog( "Aggregating GTAP databases for production and harvested area by CROSIT region and crop" )
L153.GTAP_ag_irrProd_t <- L151.GTAP_ag_irrProd_t
L153.GTAP_ag_irrProd_t$CROSIT_ctry <- AGLU_ctry$CROSIT_ctry[ match( L153.GTAP_ag_irrProd_t$ctry, AGLU_ctry$iso ) ]
L153.GTAP_ag_irrProd_t$CROSIT_crop <- FAO_ag_items_PRODSTAT$CROSIT_crop[ match( L153.GTAP_ag_irrProd_t$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L153.GTAP_ag_irrProd_t <- na.omit( L153.GTAP_ag_irrProd_t )

L153.GTAP_ag_rfdProd_t <- L151.GTAP_ag_rfdProd_t
L153.GTAP_ag_rfdProd_t$CROSIT_ctry <- AGLU_ctry$CROSIT_ctry[ match( L153.GTAP_ag_rfdProd_t$ctry, AGLU_ctry$iso ) ]
L153.GTAP_ag_rfdProd_t$CROSIT_crop <- FAO_ag_items_PRODSTAT$CROSIT_crop[ match( L153.GTAP_ag_rfdProd_t$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L153.GTAP_ag_rfdProd_t <- na.omit( L153.GTAP_ag_rfdProd_t )

L153.GTAP_ag_irrHA_ha <- L151.GTAP_ag_irrHA_ha
L153.GTAP_ag_irrHA_ha$CROSIT_ctry <- AGLU_ctry$CROSIT_ctry[ match( L153.GTAP_ag_irrHA_ha$ctry, AGLU_ctry$iso ) ]
L153.GTAP_ag_irrHA_ha$CROSIT_crop <- FAO_ag_items_PRODSTAT$CROSIT_crop[ match( L153.GTAP_ag_irrHA_ha$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L153.GTAP_ag_irrHA_ha <- na.omit( L153.GTAP_ag_irrHA_ha )

L153.GTAP_ag_rfdHA_ha <- L151.GTAP_ag_rfdHA_ha
L153.GTAP_ag_rfdHA_ha$CROSIT_ctry <- AGLU_ctry$CROSIT_ctry[ match( L153.GTAP_ag_rfdHA_ha$ctry, AGLU_ctry$iso ) ]
L153.GTAP_ag_rfdHA_ha$CROSIT_crop <- FAO_ag_items_PRODSTAT$CROSIT_crop[ match( L153.GTAP_ag_rfdHA_ha$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]
L153.GTAP_ag_rfdHA_ha <- na.omit( L153.GTAP_ag_rfdHA_ha )

#Aggregate by CROSIT region and commodity
L153.ag_irrProd_t_Rcrs_Ccrs_AEZ <- aggregate( L153.GTAP_ag_irrProd_t[ AEZs ],
      by = list( CROSIT_ctry = L153.GTAP_ag_irrProd_t$CROSIT_ctry, CROSIT_crop = L153.GTAP_ag_irrProd_t$CROSIT_crop ), sum )
L153.ag_rfdProd_t_Rcrs_Ccrs_AEZ <- aggregate( L153.GTAP_ag_rfdProd_t[ AEZs ],
      by = list( CROSIT_ctry = L153.GTAP_ag_rfdProd_t$CROSIT_ctry, CROSIT_crop = L153.GTAP_ag_rfdProd_t$CROSIT_crop ), sum )

L153.ag_irrHA_ha_Rcrs_Ccrs_AEZ <- aggregate( L153.GTAP_ag_irrHA_ha[ AEZs ],
      by = list( CROSIT_ctry = L153.GTAP_ag_irrHA_ha$CROSIT_ctry, CROSIT_crop = L153.GTAP_ag_irrHA_ha$CROSIT_crop ), sum )
L153.ag_rfdHA_ha_Rcrs_Ccrs_AEZ <- aggregate( L153.GTAP_ag_rfdHA_ha[ AEZs ],
      by = list( CROSIT_ctry = L153.GTAP_ag_rfdHA_ha$CROSIT_ctry, CROSIT_crop = L153.GTAP_ag_rfdHA_ha$CROSIT_crop ), sum )

#Drop all crop x commodity combinations not present in the CROSIT yield area multiplier data
L153.ag_irrProd_t_Rcrs_Ccrs_AEZ <- L153.ag_irrProd_t_Rcrs_Ccrs_AEZ[
      vecpaste( L153.ag_irrProd_t_Rcrs_Ccrs_AEZ[ crs_ID ] ) %in%
      vecpaste( L153.ag_irrYieldmult_Rcrs_Ccrs_Y[ crs_ID ] ), ]

L153.ag_rfdProd_t_Rcrs_Ccrs_AEZ <- L153.ag_rfdProd_t_Rcrs_Ccrs_AEZ[
      vecpaste( L153.ag_rfdProd_t_Rcrs_Ccrs_AEZ[ crs_ID ] ) %in%
      vecpaste( L153.ag_rfdYieldmult_Rcrs_Ccrs_Y[ crs_ID ] ), ]

L153.ag_irrHA_ha_Rcrs_Ccrs_AEZ <- L153.ag_irrHA_ha_Rcrs_Ccrs_AEZ[
      vecpaste( L153.ag_irrHA_ha_Rcrs_Ccrs_AEZ[ crs_ID ] ) %in%
      vecpaste( L153.ag_irrYieldmult_Rcrs_Ccrs_Y.melt[ crs_ID ] ), ]

L153.ag_rfdHA_ha_Rcrs_Ccrs_AEZ <- L153.ag_rfdHA_ha_Rcrs_Ccrs_AEZ[
      vecpaste( L153.ag_rfdHA_ha_Rcrs_Ccrs_AEZ[ crs_ID ] ) %in%
      vecpaste( L153.ag_rfdYieldmult_Rcrs_Ccrs_Y.melt[ crs_ID ] ), ]

#Repeat by the number of years in the specified agricultural productivity set, and add a year vector
printlog( "Repeating GTAP AEZ production / harvested area data by number of model timesteps" )
L153.ag_irrProd_t_Rcrs_Ccrs_Ysy_AEZ <- repeat_and_add_vector( L153.ag_irrProd_t_Rcrs_Ccrs_AEZ, "year", spec_ag_prod_years )
L153.ag_irrHA_ha_Rcrs_Ccrs_Ysy_AEZ <- repeat_and_add_vector( L153.ag_irrHA_ha_Rcrs_Ccrs_AEZ, "year", spec_ag_prod_years )
L153.ag_rfdProd_t_Rcrs_Ccrs_Ysy_AEZ <- repeat_and_add_vector( L153.ag_rfdProd_t_Rcrs_Ccrs_AEZ, "year", spec_ag_prod_years )
L153.ag_rfdHA_ha_Rcrs_Ccrs_Ysy_AEZ <- repeat_and_add_vector( L153.ag_rfdHA_ha_Rcrs_Ccrs_AEZ, "year", spec_ag_prod_years )

#Match in the productivity / harvested area multipliers
#Match in the production multipliers from the IMPACT yield data for the given scenario, IMPACT region, IMPACT commodity, and year
printlog( "Matching CROSIT production and harvested area multipliers into GTAP AEZ production database" )
L153.ag_irrProd_t_Rcrs_Ccrs_Ysy_AEZ$Mult <- L153.ag_irrYieldmult_Rcrs_Ccrs_Y.melt$Mult[
      match( vecpaste( L153.ag_irrProd_t_Rcrs_Ccrs_Ysy_AEZ[ c( crs_ID, "year" )] ),
             vecpaste( L153.ag_irrYieldmult_Rcrs_Ccrs_Y.melt[ c( crs_ID, "year" )] ) ) ]
L153.ag_irrHA_ha_Rcrs_Ccrs_Ysy_AEZ$Mult <- 1

L153.ag_rfdProd_t_Rcrs_Ccrs_Ysy_AEZ$Mult <- L153.ag_rfdYieldmult_Rcrs_Ccrs_Y.melt$Mult[
      match( vecpaste( L153.ag_rfdProd_t_Rcrs_Ccrs_Ysy_AEZ[ c( crs_ID, "year" )] ),
             vecpaste( L153.ag_rfdYieldmult_Rcrs_Ccrs_Y.melt[ c( crs_ID, "year" )] ) ) ]
L153.ag_rfdHA_ha_Rcrs_Ccrs_Ysy_AEZ$Mult <- 1

#Multiply through to get the projected production (assuming constant land area)
printlog( "Calculating the adjusted production / harvested area in each time period, for each CROSIT country / crop / AEZ" )
#NOTE: Need to start from full GTAP tables for composite regions and commodities in the CROSIT database
L153.ag_irrProd_t_ctry_crop_Ysy <- repeat_and_add_vector( L153.GTAP_ag_irrProd_t, "year", spec_ag_prod_years )
L153.ag_irrHA_ha_ctry_crop_Ysy <- repeat_and_add_vector( L153.GTAP_ag_irrHA_ha, "year", spec_ag_prod_years )

L153.ag_rfdProd_t_ctry_crop_Ysy <- repeat_and_add_vector( L153.GTAP_ag_rfdProd_t, "year", spec_ag_prod_years )
L153.ag_rfdHA_ha_ctry_crop_Ysy <- repeat_and_add_vector( L153.GTAP_ag_rfdHA_ha, "year", spec_ag_prod_years )

#Match in multipliers (from CROSIT database), GCAM regions, and GCAM commodities
L153.ag_irrProd_t_ctry_crop_Ysy$Mult <- L153.ag_irrProd_t_Rcrs_Ccrs_Ysy_AEZ$Mult[
      match( vecpaste( L153.ag_irrProd_t_ctry_crop_Ysy[ c( crs_ID, "year" ) ] ),
             vecpaste( L153.ag_irrProd_t_Rcrs_Ccrs_Ysy_AEZ[ c( crs_ID, "year" ) ] ) ) ]
L153.ag_irrProd_t_ctry_crop_Ysy <- na.omit( L153.ag_irrProd_t_ctry_crop_Ysy )
L153.ag_irrProd_t_ctry_crop_Ysy[[R]] <- iso_GCAM_regID[[R]][ match( L153.ag_irrProd_t_ctry_crop_Ysy$ctry, iso_GCAM_regID$iso ) ]
L153.ag_irrProd_t_ctry_crop_Ysy[[C]] <- FAO_ag_items_PRODSTAT[[C]][ match( L153.ag_irrProd_t_ctry_crop_Ysy$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]

L153.ag_rfdProd_t_ctry_crop_Ysy$Mult <- L153.ag_rfdProd_t_Rcrs_Ccrs_Ysy_AEZ$Mult[
      match( vecpaste( L153.ag_rfdProd_t_ctry_crop_Ysy[ c( crs_ID, "year" ) ] ),
             vecpaste( L153.ag_rfdProd_t_Rcrs_Ccrs_Ysy_AEZ[ c( crs_ID, "year" ) ] ) ) ]
L153.ag_rfdProd_t_ctry_crop_Ysy <- na.omit( L153.ag_rfdProd_t_ctry_crop_Ysy )
L153.ag_rfdProd_t_ctry_crop_Ysy[[R]] <- iso_GCAM_regID[[R]][ match( L153.ag_rfdProd_t_ctry_crop_Ysy$ctry, iso_GCAM_regID$iso ) ]
L153.ag_rfdProd_t_ctry_crop_Ysy[[C]] <- FAO_ag_items_PRODSTAT[[C]][ match( L153.ag_rfdProd_t_ctry_crop_Ysy$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]

L153.ag_irrHA_ha_ctry_crop_Ysy$Mult <- L153.ag_irrHA_ha_Rcrs_Ccrs_Ysy_AEZ$Mult[
      match( vecpaste( L153.ag_irrHA_ha_ctry_crop_Ysy[ c( crs_ID, "year" ) ] ),
             vecpaste( L153.ag_irrHA_ha_Rcrs_Ccrs_Ysy_AEZ[ c( crs_ID, "year" ) ] ) ) ]
L153.ag_irrHA_ha_ctry_crop_Ysy <- na.omit( L153.ag_irrHA_ha_ctry_crop_Ysy )
L153.ag_irrHA_ha_ctry_crop_Ysy[[R]] <- iso_GCAM_regID[[R]][ match( L153.ag_irrHA_ha_ctry_crop_Ysy$ctry, iso_GCAM_regID$iso ) ]
L153.ag_irrHA_ha_ctry_crop_Ysy[[C]] <- FAO_ag_items_PRODSTAT[[C]][ match( L153.ag_irrHA_ha_ctry_crop_Ysy$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]

L153.ag_rfdHA_ha_ctry_crop_Ysy$Mult <- L153.ag_rfdHA_ha_Rcrs_Ccrs_Ysy_AEZ$Mult[
      match( vecpaste( L153.ag_rfdHA_ha_ctry_crop_Ysy[ c( crs_ID, "year" ) ] ),
             vecpaste( L153.ag_rfdHA_ha_Rcrs_Ccrs_Ysy_AEZ[ c( crs_ID, "year" ) ] ) ) ]
L153.ag_rfdHA_ha_ctry_crop_Ysy <- na.omit( L153.ag_rfdHA_ha_ctry_crop_Ysy )
L153.ag_rfdHA_ha_ctry_crop_Ysy[[R]] <- iso_GCAM_regID[[R]][ match( L153.ag_rfdHA_ha_ctry_crop_Ysy$ctry, iso_GCAM_regID$iso ) ]
L153.ag_rfdHA_ha_ctry_crop_Ysy[[C]] <- FAO_ag_items_PRODSTAT[[C]][ match( L153.ag_rfdHA_ha_ctry_crop_Ysy$GTAP_crop, FAO_ag_items_PRODSTAT$GTAP_crop ) ]

#NOTE: Do not read these lines iteratively
L153.ag_irrProd_t_ctry_crop_Ysy[ AEZs ] <- L153.ag_irrProd_t_ctry_crop_Ysy[ AEZs ] * L153.ag_irrProd_t_ctry_crop_Ysy$Mult
L153.ag_irrHA_ha_ctry_crop_Ysy[ AEZs ] <- L153.ag_irrHA_ha_ctry_crop_Ysy[ AEZs ] * L153.ag_irrHA_ha_ctry_crop_Ysy$Mult
L153.ag_rfdProd_t_ctry_crop_Ysy[ AEZs ] <- L153.ag_rfdProd_t_ctry_crop_Ysy[ AEZs ] * L153.ag_rfdProd_t_ctry_crop_Ysy$Mult
L153.ag_rfdHA_ha_ctry_crop_Ysy[ AEZs ] <- L153.ag_rfdHA_ha_ctry_crop_Ysy[ AEZs ] * L153.ag_rfdHA_ha_ctry_crop_Ysy$Mult

#Aggregate by GCAM region and commodity
printlog( "Aggregating by GCAM region / commodity / year to calculate change in productivity from the base year" )
L153.ag_irrProd_t_R_C_Ysy_AEZ <- aggregate( L153.ag_irrProd_t_ctry_crop_Ysy[ AEZs ],
      by=as.list( L153.ag_irrProd_t_ctry_crop_Ysy[ R_C_Y ] ), sum )
L153.ag_irrHA_ha_R_C_Ysy_AEZ <- aggregate( L153.ag_irrHA_ha_ctry_crop_Ysy[ AEZs ],
      by=as.list( L153.ag_irrHA_ha_ctry_crop_Ysy[ R_C_Y ] ), sum )
L153.ag_rfdProd_t_R_C_Ysy_AEZ <- aggregate( L153.ag_rfdProd_t_ctry_crop_Ysy[ AEZs ],
      by=as.list( L153.ag_rfdProd_t_ctry_crop_Ysy[ R_C_Y ] ), sum )
L153.ag_rfdHA_ha_R_C_Ysy_AEZ <- aggregate( L153.ag_rfdHA_ha_ctry_crop_Ysy[ AEZs ],
      by=as.list( L153.ag_rfdHA_ha_ctry_crop_Ysy[ R_C_Y ] ), sum )

#Calculate the yield as production divided by harvested area
L153.ag_irrYield_tha_R_C_Ysy_AEZ <- L153.ag_irrProd_t_R_C_Ysy_AEZ
L153.ag_irrYield_tha_R_C_Ysy_AEZ[ AEZs ] <- L153.ag_irrProd_t_R_C_Ysy_AEZ[ AEZs ] / L153.ag_irrHA_ha_R_C_Ysy_AEZ[ 
      match( vecpaste( L153.ag_irrYield_tha_R_C_Ysy_AEZ[ R_C_Y ] ),
             vecpaste( L153.ag_irrHA_ha_R_C_Ysy_AEZ[ R_C_Y ] ) ), AEZs ]
L153.ag_irrYield_tha_R_C_Ysy_AEZ[ is.na( L153.ag_irrYield_tha_R_C_Ysy_AEZ ) ] <- 0

L153.ag_rfdYield_tha_R_C_Ysy_AEZ <- L153.ag_rfdProd_t_R_C_Ysy_AEZ
L153.ag_rfdYield_tha_R_C_Ysy_AEZ[ AEZs ] <- L153.ag_rfdProd_t_R_C_Ysy_AEZ[ AEZs ] / L153.ag_rfdHA_ha_R_C_Ysy_AEZ[ 
      match( vecpaste( L153.ag_rfdYield_tha_R_C_Ysy_AEZ[ R_C_Y ] ),
             vecpaste( L153.ag_rfdHA_ha_R_C_Ysy_AEZ[ R_C_Y ] ) ), AEZs ]
L153.ag_rfdYield_tha_R_C_Ysy_AEZ[ is.na( L153.ag_rfdYield_tha_R_C_Ysy_AEZ ) ] <- 0

#Calculate productivity multipliers from the first time period

L153.ag_irrYield_tha_R_C_Yfby_AEZ <- subset( L153.ag_irrYield_tha_R_C_Ysy_AEZ, year == min( spec_ag_prod_years ) )
L153.ag_irrYieldRatio_R_C_Ysy_AEZ <- L153.ag_irrYield_tha_R_C_Ysy_AEZ
L153.ag_irrYieldRatio_R_C_Ysy_AEZ[ AEZs ] <-  L153.ag_irrYield_tha_R_C_Ysy_AEZ[ AEZs ] / L153.ag_irrYield_tha_R_C_Yfby_AEZ[
      match( vecpaste( L153.ag_irrYield_tha_R_C_Ysy_AEZ[ R_C ] ), vecpaste( L153.ag_irrYield_tha_R_C_Yfby_AEZ[ R_C ] ) ),
      AEZs ]
L153.ag_irrYieldRatio_R_C_Ysy_AEZ[ is.na( L153.ag_irrYieldRatio_R_C_Ysy_AEZ ) ] <- 1

L153.ag_rfdYield_tha_R_C_Yfby_AEZ <- subset( L153.ag_rfdYield_tha_R_C_Ysy_AEZ, year == min( spec_ag_prod_years ) )
L153.ag_rfdYieldRatio_R_C_Ysy_AEZ <- L153.ag_rfdYield_tha_R_C_Ysy_AEZ
L153.ag_rfdYieldRatio_R_C_Ysy_AEZ[ AEZs ] <-  L153.ag_rfdYield_tha_R_C_Ysy_AEZ[ AEZs ] / L153.ag_rfdYield_tha_R_C_Yfby_AEZ[
      match( vecpaste( L153.ag_rfdYield_tha_R_C_Ysy_AEZ[ R_C ] ), vecpaste( L153.ag_rfdYield_tha_R_C_Yfby_AEZ[ R_C ] ) ),
      AEZs ]
L153.ag_rfdYieldRatio_R_C_Ysy_AEZ[ is.na( L153.ag_rfdYieldRatio_R_C_Ysy_AEZ ) ] <- 1

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L153.ag_irrYield_tha_R_C_Ysy_AEZ <- c( "Baseline yield for irrigated crops by GCAM region / commodity / future year (specified)","Unit: tonnes per hectare" )
comments.L153.ag_irrYieldRatio_R_C_Ysy_AEZ <- c( "Yield ratio from base year for irrigated crops by GCAM region / commodity (specified) / future year (specified)","Unitless ratio" )
comments.L153.ag_rfdYield_tha_R_C_Ysy_AEZ <- c( "Baseline yield for rainfed crops by GCAM region / commodity / future year (specified)","Unit: tonnes per hectare" )
comments.L153.ag_rfdYieldRatio_R_C_Ysy_AEZ <- c( "Yield ratio from base year for rainfed crops by GCAM region / commodity (specified) / future year (specified)","Unitless ratio" )

#write tables as CSV files
writedata( L153.ag_irrYield_tha_R_C_Ysy_AEZ, domain="AGLU_LEVEL1_DATA",fn="L153.ag_irrYield_tha_R_C_Ysy_AEZ", comments=comments.L153.ag_irrYield_tha_R_C_Ysy_AEZ )
writedata( L153.ag_irrYieldRatio_R_C_Ysy_AEZ, domain="AGLU_LEVEL1_DATA",fn="L153.ag_irrYieldRatio_R_C_Ysy_AEZ", comments=comments.L153.ag_irrYieldRatio_R_C_Ysy_AEZ )
writedata( L153.ag_rfdYield_tha_R_C_Ysy_AEZ, domain="AGLU_LEVEL1_DATA",fn="L153.ag_rfdYield_tha_R_C_Ysy_AEZ", comments=comments.L153.ag_rfdYield_tha_R_C_Ysy_AEZ )
writedata( L153.ag_rfdYieldRatio_R_C_Ysy_AEZ, domain="AGLU_LEVEL1_DATA",fn="L153.ag_rfdYieldRatio_R_C_Ysy_AEZ", comments=comments.L153.ag_rfdYieldRatio_R_C_Ysy_AEZ )

# Every script should finish with this line
logstop()
