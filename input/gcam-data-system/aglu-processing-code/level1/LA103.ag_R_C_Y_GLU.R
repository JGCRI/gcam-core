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
logstart( "LA103.ag_R_C_Y_GLU.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Primary agricultural data assigned to GCAM region / commodity / year / GLU" )

# -----------------------------------------------------------------------------
# 1. Read data from prevrious files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
L101.ag_Prod_Mt_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L101.ag_Prod_Mt_R_C_Y" )
L101.ag_HA_bm2_R_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L101.ag_HA_bm2_R_C_Y" )
L102.ag_Prod_Mt_R_C_GLU <- readdata( "AGLU_LEVEL1_DATA", "L102.ag_Prod_Mt_R_C_GLU" )
L102.ag_HA_bm2_R_C_GLU <- readdata( "AGLU_LEVEL1_DATA", "L102.ag_HA_bm2_R_C_GLU" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Combine FAO and GTAP: create tables with crop production and harvested area by geographic land unit (GLU) and historical year
#production ( Mt )
printlog( "Disaggregating FAO production of all crops to GLUs using GTAP/LDS data" )
L103.ag_Prod_Mt_R_C <- aggregate( L102.ag_Prod_Mt_R_C_GLU[ "value" ],
                                  by = L102.ag_Prod_Mt_R_C_GLU[ R_C ], sum )
L103.ag_Prod_frac_R_C_GLU <- L102.ag_Prod_Mt_R_C_GLU
L103.ag_Prod_frac_R_C_GLU$value <- L103.ag_Prod_frac_R_C_GLU$value / L103.ag_Prod_Mt_R_C$value[
  match( vecpaste( L103.ag_Prod_frac_R_C_GLU[ R_C ] ),
         vecpaste( L103.ag_Prod_Mt_R_C[ R_C ] ) ) ]
#Replace NaN values with 0. This step doesn't seem to be necessary b/c the LDS doesn't out zeroes for missing values; leaving it anyway
L103.ag_Prod_frac_R_C_GLU[ is.na( L103.ag_Prod_frac_R_C_GLU ) ] <- 0     

printlog( "Disaggregating FAO harvested area of all crops to GLUs using GTAP/LDS data" )
L103.ag_HA_bm2_R_C <- aggregate( L102.ag_HA_bm2_R_C_GLU[ "value" ],
                                by = L102.ag_HA_bm2_R_C_GLU[ R_C ], sum )
L103.ag_HA_frac_R_C_GLU <- L102.ag_HA_bm2_R_C_GLU
L103.ag_HA_frac_R_C_GLU$value <- L103.ag_HA_frac_R_C_GLU$value / L103.ag_HA_bm2_R_C$value[
  match( vecpaste( L103.ag_HA_frac_R_C_GLU[ R_C ] ),
         vecpaste( L103.ag_HA_bm2_R_C[ R_C ] ) ) ]
#Replace NaN values with 0. This step doesn't seem to be necessary b/c the LDS doesn't out zeroes for missing values; leaving it anyway
L103.ag_HA_frac_R_C_GLU[ is.na( L103.ag_HA_frac_R_C_GLU ) ] <- 0     

#Multiply historical production by these shares in order to downscale to GLU
#NOTE: There are a few region x crop combinations in the FAO data that aren't in the aggregated gridded dataset, presumably because
# production was zero in around 2000, the base year for the gridded dataset. In analysis of these missing values, the quantities are tiny
# (zero in most years, <0.01 Mt in all/most others) and dropping them should not have any consequences
L103.ag_Prod_Mt_R_C_Y_GLU <- L103.ag_Prod_frac_R_C_GLU[ R_C_GLU ]
L103.ag_Prod_Mt_R_C_Y_GLU[ X_historical_years ] <- L103.ag_Prod_frac_R_C_GLU$value * L101.ag_Prod_Mt_R_C_Y[
  match( vecpaste( L103.ag_Prod_frac_R_C_GLU[ R_C ] ),
         vecpaste( L101.ag_Prod_Mt_R_C_Y[ R_C ] ) ),
  X_historical_years ]

printlog( "Removing crops from the written-out data that are zero in all years" )
# This is part of the "pruning" process of not creating XML tags for land use types that are non-applicable
L103.ag_Prod_Mt_R_C_Y_GLU <- L103.ag_Prod_Mt_R_C_Y_GLU[ rowSums( L103.ag_Prod_Mt_R_C_Y_GLU[ X_historical_years ] ) != 0, ]

L103.ag_HA_bm2_R_C_Y_GLU <- L103.ag_HA_frac_R_C_GLU[ R_C_GLU ]
L103.ag_HA_bm2_R_C_Y_GLU[ X_historical_years ] <- L103.ag_HA_frac_R_C_GLU$value * L101.ag_HA_bm2_R_C_Y[
  match( vecpaste( L103.ag_HA_frac_R_C_GLU[ R_C ] ),
         vecpaste( L101.ag_HA_bm2_R_C_Y[ R_C ] ) ),
  X_historical_years ]

# prune it
L103.ag_HA_bm2_R_C_Y_GLU <- L103.ag_HA_bm2_R_C_Y_GLU[ rowSums( L103.ag_HA_bm2_R_C_Y_GLU[ X_historical_years ] ) != 0, ]

#calculate yield in kilograms per square meter
printlog( "Calculating initial yield estimates by region, crop, year, and GLU" )
L103.ag_Yield_kgm2_R_C_Y_GLU <- L103.ag_Prod_Mt_R_C_Y_GLU
L103.ag_Yield_kgm2_R_C_Y_GLU[ X_AGLU_historical_years ] <- L103.ag_Prod_Mt_R_C_Y_GLU[ X_AGLU_historical_years ] / L103.ag_HA_bm2_R_C_Y_GLU[
      match( vecpaste( L103.ag_Prod_Mt_R_C_Y_GLU[ R_C_GLU ] ),
             vecpaste( L103.ag_HA_bm2_R_C_Y_GLU[ R_C_GLU ] ) ),
      X_AGLU_historical_years ]
L103.ag_Yield_kgm2_R_C_Y_GLU[ is.na( L103.ag_Yield_kgm2_R_C_Y_GLU ) ] <- 0     

#Aggregate through GLUs to get production by region/crop/year; different from L101 production in that we have now dropped
# some observations that weren't available in the GTAP-based gridded inventories
L103.ag_Prod_Mt_R_C_Y <- aggregate( L103.ag_Prod_Mt_R_C_Y_GLU[ X_historical_years ],
                                    by = L103.ag_Prod_Mt_R_C_Y_GLU[ R_C ], sum )
L103.ag_Prod_Mt_R_C_Y <- translate_to_full_table( L103.ag_Prod_Mt_R_C_Y,
                                                  R, unique( iso_GCAM_regID[[R]] ),
                                                  C, unique( L103.ag_Prod_Mt_R_C_Y[[C]] ) )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L103.ag_Prod_Mt_R_C_Y_GLU <- c( "Crop production by GCAM region / commodity / year / GLU","Unit = Mt" )
comments.L103.ag_Prod_Mt_R_C_Y <- c( "Crop production by GCAM region / commodity / year","Unit = Mt" )
comments.L103.ag_HA_bm2_R_C_Y_GLU <- c( "Harvested area by GCAM region / commodity / year / GLU","Unit = bm2" )
comments.L103.ag_Yield_kgm2_R_C_Y_GLU <- c( "Unadjusted agronomic yield by GCAM region / commodity / year / GLU","Unit = kg.m2" )

#write out final tables
writedata( L103.ag_Prod_Mt_R_C_Y_GLU,domain="AGLU_LEVEL1_DATA",fn="L103.ag_Prod_Mt_R_C_Y_GLU",comments=comments.L103.ag_Prod_Mt_R_C_Y_GLU )
writedata( L103.ag_Prod_Mt_R_C_Y,domain="AGLU_LEVEL1_DATA",fn="L103.ag_Prod_Mt_R_C_Y",comments=comments.L103.ag_Prod_Mt_R_C_Y )
writedata( L103.ag_HA_bm2_R_C_Y_GLU,domain="AGLU_LEVEL1_DATA",fn="L103.ag_HA_bm2_R_C_Y_GLU",comments=comments.L103.ag_HA_bm2_R_C_Y_GLU )
writedata( L103.ag_Yield_kgm2_R_C_Y_GLU,domain="AGLU_LEVEL1_DATA",fn="L103.ag_Yield_kgm2_R_C_Y_GLU",comments=comments.L103.ag_Yield_kgm2_R_C_Y_GLU )

# Every script should finish with this line
logstop()
