# Before we can load headers we need some paths defined. They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "AGLUPROC_DIR" ) ){
    if( Sys.getenv( "AGLUPROC" ) != "" ){
        AGLUPROC_DIR <- Sys.getenv( "AGLUPROC" )
    } else {
        stop("Could not determine location of aglu data system. Please set the R var AGLUPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
logstart( "LA103.ag_R_C_Y_AEZ.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Primary agricultural data assigned to GCAM region / commodity / year / AEZ" )

# -----------------------------------------------------------------------------
# 1. Read data from previous files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
L101.ag_Prod_Mt_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L101.ag_Prod_Mt_R_C_Y" )
L101.ag_HA_bm2_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L101.ag_HA_bm2_R_C_Y" )
L102.ag_Prod_Mt_R_C_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L102.ag_Prod_Mt_R_C_AEZ" )
L102.ag_HA_bm2_R_C_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L102.ag_HA_bm2_R_C_AEZ" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Combine FAO and GTAP: create tables with crop production and harvested area by AEZ and historical year
#production ( Mt )
printlog( "Disaggregating FAO production and harvested area of all crops to AEZs using GTAP data" )
L103.ag_Prod_frac_R_C_AEZ <- L102.ag_Prod_Mt_R_C_AEZ
L103.ag_Prod_frac_R_C_AEZ[ AEZs ] <- L102.ag_Prod_Mt_R_C_AEZ [ AEZs ] /
     rowSums(L102.ag_Prod_Mt_R_C_AEZ[ AEZs ] )
#Replace NaN values with 0
L103.ag_Prod_frac_R_C_AEZ[ is.na( L103.ag_Prod_frac_R_C_AEZ ) ] <- 0     

L103.ag_HA_frac_R_C_AEZ <- L102.ag_HA_bm2_R_C_AEZ
L103.ag_HA_frac_R_C_AEZ[ AEZs ] <- L102.ag_HA_bm2_R_C_AEZ [ AEZs ] /
     rowSums(L102.ag_HA_bm2_R_C_AEZ[ AEZs ] )
#Replace NaN values with 0
L103.ag_HA_frac_R_C_AEZ[ is.na( L103.ag_HA_frac_R_C_AEZ ) ] <- 0     

#Melt fraction tables so that they can be applied to each historical year
L103.ag_Prod_frac_R_C_AEZ.melt <- melt( L103.ag_Prod_frac_R_C_AEZ, id.vars = R_C, variable_name = AEZ )
L103.ag_HA_frac_R_C_AEZ.melt <- melt( L103.ag_HA_frac_R_C_AEZ, id.vars = R_C, variable_name = AEZ )

#Repeat the FAO historical production and harvested area tables by the AEZs, and multiply by AEZ-wise shares
L103.ag_Prod_Mt_R_C_Y_AEZ <- repeat_and_add_vector( L101.ag_Prod_Mt_R_C_Y, AEZ, AEZs )[ c( R_C_AEZ, X_AGLU_historical_years ) ]
L103.ag_Prod_Mt_R_C_Y_AEZ[ X_AGLU_historical_years ] <- L103.ag_Prod_Mt_R_C_Y_AEZ[ X_AGLU_historical_years ] * L103.ag_Prod_frac_R_C_AEZ.melt$value[
      match( vecpaste( L103.ag_Prod_Mt_R_C_Y_AEZ[ R_C_AEZ ] ),
             vecpaste( L103.ag_Prod_frac_R_C_AEZ.melt[ R_C_AEZ ] ) ) ]

L103.ag_HA_bm2_R_C_Y_AEZ <- repeat_and_add_vector( L101.ag_HA_bm2_R_C_Y, AEZ, AEZs )[ c( R_C_AEZ, X_AGLU_historical_years ) ]
L103.ag_HA_bm2_R_C_Y_AEZ[ X_AGLU_historical_years ] <- L103.ag_HA_bm2_R_C_Y_AEZ[ X_AGLU_historical_years ] * L103.ag_HA_frac_R_C_AEZ.melt$value[
      match( vecpaste( L103.ag_HA_bm2_R_C_Y_AEZ[ R_C_AEZ ] ),
             vecpaste( L103.ag_HA_frac_R_C_AEZ.melt[ R_C_AEZ ] ) ) ]

#calculate yield in kilograms per square meter
printlog( "Calculating initial yield estimates by region, crop, year, and AEZ" )
L103.ag_Yield_kgm2_R_C_Y_AEZ <- L103.ag_Prod_Mt_R_C_Y_AEZ
L103.ag_Yield_kgm2_R_C_Y_AEZ[ X_AGLU_historical_years ] <- L103.ag_Prod_Mt_R_C_Y_AEZ[ X_AGLU_historical_years ] / L103.ag_HA_bm2_R_C_Y_AEZ[
      match( vecpaste( L103.ag_Prod_Mt_R_C_Y_AEZ[ R_C_AEZ ] ), vecpaste( L103.ag_HA_bm2_R_C_Y_AEZ[ R_C_AEZ ] ) ),
      X_AGLU_historical_years ]
L103.ag_Yield_kgm2_R_C_Y_AEZ[ is.na( L103.ag_Yield_kgm2_R_C_Y_AEZ ) ] <- 0     

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L103.ag_Prod_frac_R_C_AEZ <- c( "AEZ-wise production fraction by GCAM region / commodity","Unit = decimal" )
comments.L103.ag_Prod_Mt_R_C_Y_AEZ <- c( "Unadjusted crop production by GCAM region / commodity / year / AEZ","Unit = Mt" )
comments.L103.ag_HA_bm2_R_C_Y_AEZ <- c( "Harvested area by GCAM region / commodity / year / AEZ","Unit = bm2" )
comments.L103.ag_Yield_kgm2_R_C_Y_AEZ <- c( "Unadjusted agronomic yield by GCAM region / commodity / year / AEZ","Unit = kg.m2" )

#write out final tables
writedata( L103.ag_Prod_frac_R_C_AEZ,domain="AGLU_LEVEL1_DATA",fn="L103.ag_Prod_frac_R_C_AEZ",comments=comments.L103.ag_Prod_frac_R_C_AEZ )
writedata( L103.ag_Prod_Mt_R_C_Y_AEZ,domain="AGLU_LEVEL1_DATA",fn="L103.ag_Prod_Mt_R_C_Y_AEZ",comments=comments.L103.ag_Prod_Mt_R_C_Y_AEZ )
writedata( L103.ag_HA_bm2_R_C_Y_AEZ,domain="AGLU_LEVEL1_DATA",fn="L103.ag_HA_bm2_R_C_Y_AEZ",comments=comments.L103.ag_HA_bm2_R_C_Y_AEZ )
writedata( L103.ag_Yield_kgm2_R_C_Y_AEZ,domain="AGLU_LEVEL1_DATA",fn="L103.ag_Yield_kgm2_R_C_Y_AEZ",comments=comments.L103.ag_Yield_kgm2_R_C_Y_AEZ )

# Every script should finish with this line
logstop()
