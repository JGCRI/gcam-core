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
logstart( "LB161.ag_R_C_Y_GLU_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Primary agricultural data assigned to GCAM region / commodity / year / GLU" )

# -----------------------------------------------------------------------------
# 1. Read data from previous files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
L103.ag_Prod_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L103.ag_Prod_Mt_R_C_Y_GLU" )
L103.ag_HA_bm2_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L103.ag_HA_bm2_R_C_Y_GLU" )
L152.ag_irrProd_Mt_R_C_GLU <- readdata( "AGLU_LEVEL1_DATA", "L152.ag_irrProd_Mt_R_C_GLU" )
L152.ag_irrHA_bm2_R_C_GLU <- readdata( "AGLU_LEVEL1_DATA", "L152.ag_irrHA_bm2_R_C_GLU" )
L152.ag_rfdProd_Mt_R_C_GLU <- readdata( "AGLU_LEVEL1_DATA", "L152.ag_rfdProd_Mt_R_C_GLU" )
L152.ag_rfdHA_bm2_R_C_GLU <- readdata( "AGLU_LEVEL1_DATA", "L152.ag_rfdHA_bm2_R_C_GLU" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Compute fraction of production that is irrigated for each GCAM region / commodity / GLU" )
L161.ag_irrProd_frac_R_C_GLU <- merge( L152.ag_irrProd_Mt_R_C_GLU, L152.ag_rfdProd_Mt_R_C_GLU, all = T)
L161.ag_irrProd_frac_R_C_GLU$irrProd_frac <- with( L161.ag_irrProd_frac_R_C_GLU, irrProd / ( irrProd + rfdProd ) )

printlog( "Compute fraction of harvested area that is irrigated for each GCAM region / commodity / GLU" )
L161.ag_irrHA_frac_R_C_GLU <- merge( L152.ag_irrHA_bm2_R_C_GLU, L152.ag_rfdHA_bm2_R_C_GLU, all = T)
L161.ag_irrHA_frac_R_C_GLU$irrHA_frac <- with( L161.ag_irrHA_frac_R_C_GLU, irrHA / ( irrHA + rfdHA ) )

#Combine FAO and GTAP: create tables with crop production and harvested area by GLU and model base years.
#production ( Mt )
printlog( "Disaggregating FAO production and harvested area of all crops to GLUs using GTAP data" )
#Repeat fraction table by number of historical years, and multiply fraction table by total annual production
L161.ag_irrProd_frac_R_C_Y_GLU <- repeat_and_add_vector( L161.ag_irrProd_frac_R_C_GLU, "year", historical_years )

#Melt production table to prepare for further calculation
L103.ag_Prod_Mt_R_C_Y_GLU.melt <- melt( L103.ag_Prod_Mt_R_C_Y_GLU, id.vars = R_C_GLU, variable.name = "Xyear" )
L103.ag_Prod_Mt_R_C_Y_GLU.melt$year <- as.numeric( substr( L103.ag_Prod_Mt_R_C_Y_GLU.melt$Xyear, 2, 5 ) )

# Calculate irrigated (rainfed) production by multiplying total by fraction irrigated (rainfed)
L161.ag_irrProd_Mt_R_C_Y_GLU.melt <- L103.ag_Prod_Mt_R_C_Y_GLU.melt
L161.ag_irrProd_Mt_R_C_Y_GLU.melt$value <- L103.ag_Prod_Mt_R_C_Y_GLU.melt$value *
      L161.ag_irrProd_frac_R_C_Y_GLU$irrProd_frac[
          match( vecpaste(L161.ag_irrProd_Mt_R_C_Y_GLU.melt[ R_C_Y_GLU ] ),
                 vecpaste(L161.ag_irrProd_frac_R_C_Y_GLU[ R_C_Y_GLU ] ) ) ]

L161.ag_rfdProd_Mt_R_C_Y_GLU.melt <- L103.ag_Prod_Mt_R_C_Y_GLU.melt
L161.ag_rfdProd_Mt_R_C_Y_GLU.melt$value <- L103.ag_Prod_Mt_R_C_Y_GLU.melt$value *
      ( 1 - L161.ag_irrProd_frac_R_C_Y_GLU$irrProd_frac[
         match( vecpaste( L161.ag_rfdProd_Mt_R_C_Y_GLU.melt[ R_C_Y_GLU ] ),
                vecpaste(L161.ag_irrProd_frac_R_C_Y_GLU[ R_C_Y_GLU ] ) ) ] )

#Repeat fraction table by number of historical years, and multiply fraction table by total annual production
L161.ag_irrHA_frac_R_C_Y_GLU <- repeat_and_add_vector( L161.ag_irrHA_frac_R_C_GLU, "year", historical_years )

#Melt tables to prepare for further calculation
L161.ag_HA_bm2_R_C_Y_GLU.melt <- melt( L103.ag_HA_bm2_R_C_Y_GLU, id.vars = R_C_GLU, variable.name = "Xyear" )
L161.ag_HA_bm2_R_C_Y_GLU.melt$year <- as.numeric( substr( L161.ag_HA_bm2_R_C_Y_GLU.melt$Xyear, 2, 5 ) )

# Calculate irrigated (rainfed) production by multiplying total by fraction irrigated (rainfed)
L161.ag_irrHA_bm2_R_C_Y_GLU.melt <- L161.ag_HA_bm2_R_C_Y_GLU.melt
L161.ag_irrHA_bm2_R_C_Y_GLU.melt$value <- L161.ag_HA_bm2_R_C_Y_GLU.melt$value *
      L161.ag_irrHA_frac_R_C_Y_GLU$irrHA_frac[
         match( vecpaste(L161.ag_irrHA_bm2_R_C_Y_GLU.melt[ R_C_Y_GLU ] ),
                vecpaste(L161.ag_irrHA_frac_R_C_Y_GLU[ R_C_Y_GLU ] ) ) ]

L161.ag_rfdHA_bm2_R_C_Y_GLU.melt <- L161.ag_HA_bm2_R_C_Y_GLU.melt
L161.ag_rfdHA_bm2_R_C_Y_GLU.melt$value <- L161.ag_HA_bm2_R_C_Y_GLU.melt$value *
      (1 - L161.ag_irrHA_frac_R_C_Y_GLU$irrHA_frac[
          match( vecpaste(L161.ag_rfdHA_bm2_R_C_Y_GLU.melt[ R_C_Y_GLU ]),
                 vecpaste( L161.ag_irrHA_frac_R_C_Y_GLU[ R_C_Y_GLU ]))])

# Cast by year
L161.ag_irrProd_Mt_R_C_Y_GLU <- dcast( L161.ag_irrProd_Mt_R_C_Y_GLU.melt, GCAM_region_ID + GCAM_commodity + GLU ~ Xyear, value.var = "value" )
L161.ag_rfdProd_Mt_R_C_Y_GLU <- dcast( L161.ag_rfdProd_Mt_R_C_Y_GLU.melt, GCAM_region_ID + GCAM_commodity + GLU ~ Xyear, value.var = "value" )
L161.ag_irrHA_bm2_R_C_Y_GLU <- dcast( L161.ag_irrHA_bm2_R_C_Y_GLU.melt, GCAM_region_ID + GCAM_commodity + GLU ~ Xyear, value.var = "value" )
L161.ag_rfdHA_bm2_R_C_Y_GLU <- dcast( L161.ag_rfdHA_bm2_R_C_Y_GLU.melt, GCAM_region_ID + GCAM_commodity + GLU ~ Xyear, value.var = "value" )

#calculate yield in kilograms per square meter
printlog( "Calculating yield estimates by region, crop, year, and GLU" )
L161.ag_irrYield_kgm2_R_C_Y_GLU <- L161.ag_irrProd_Mt_R_C_Y_GLU
L161.ag_irrYield_kgm2_R_C_Y_GLU[ X_AGLU_historical_years ] <- L161.ag_irrProd_Mt_R_C_Y_GLU[ X_AGLU_historical_years ] /
  L161.ag_irrHA_bm2_R_C_Y_GLU[
    match( vecpaste( L161.ag_irrYield_kgm2_R_C_Y_GLU[ R_C_GLU ] ),
           vecpaste( L161.ag_irrHA_bm2_R_C_Y_GLU[ R_C_GLU ] ) ),
    X_AGLU_historical_years ]
L161.ag_irrYield_kgm2_R_C_Y_GLU[is.na(L161.ag_irrYield_kgm2_R_C_Y_GLU)]<-0   

L161.ag_rfdYield_kgm2_R_C_Y_GLU <- L161.ag_rfdProd_Mt_R_C_Y_GLU
L161.ag_rfdYield_kgm2_R_C_Y_GLU[ X_AGLU_historical_years ] <- L161.ag_rfdProd_Mt_R_C_Y_GLU[ X_AGLU_historical_years ] /
  L161.ag_rfdHA_bm2_R_C_Y_GLU[
    match( vecpaste( L161.ag_rfdYield_kgm2_R_C_Y_GLU[ R_C_GLU ] ),
           vecpaste( L161.ag_rfdHA_bm2_R_C_Y_GLU[ R_C_GLU ] ) ),
    X_AGLU_historical_years ]
L161.ag_rfdYield_kgm2_R_C_Y_GLU[is.na(L161.ag_rfdYield_kgm2_R_C_Y_GLU)]<-0    

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L161.ag_irrProd_Mt_R_C_Y_GLU <- c( "GLU-wise irrigated production by GCAM region / commodity","Unit = Mt" )
comments.L161.ag_rfdProd_Mt_R_C_Y_GLU <- c( "GLU-wise rainfed production by GCAM region / commodity","Unit = Mt" )
comments.L161.ag_irrHA_bm2_R_C_Y_GLU <- c( "Irrigated harvested area by GCAM region / commodity / year / GLU","Unit = bm2" )
comments.L161.ag_rfdHA_bm2_R_C_Y_GLU <- c( "Rainfed harvested area by GCAM region / commodity / year / GLU","Unit = bm2" )
comments.L161.ag_irrYield_kgm2_R_C_Y_GLU <- c( "Unadjusted irrigated agronomic yield by GCAM region / commodity / year / GLU","Unit = kg.m2" )
comments.L161.ag_rfdYield_kgm2_R_C_Y_GLU <- c( "Unadjusted rainfed agronomic yield by GCAM region / commodity / year / GLU","Unit = kg.m2" )
comments.L161.ag_irrHA_frac_R_C_GLU <- c( "Fraction of harvested area that is irigated by GCAM region / commodity / year / GLU", "Unit = fract" )

#write out final tables
writedata( L161.ag_irrProd_Mt_R_C_Y_GLU,domain="AGLU_LEVEL1_DATA",fn="L161.ag_irrProd_Mt_R_C_Y_GLU",comments=comments.L161.ag_irrProd_Mt_R_C_Y_GLU )
writedata( L161.ag_rfdProd_Mt_R_C_Y_GLU,domain="AGLU_LEVEL1_DATA",fn="L161.ag_rfdProd_Mt_R_C_Y_GLU",comments=comments.L161.ag_rfdProd_Mt_R_C_Y_GLU )
writedata( L161.ag_irrHA_bm2_R_C_Y_GLU,domain="AGLU_LEVEL1_DATA",fn="L161.ag_irrHA_bm2_R_C_Y_GLU",comments=comments.L161.ag_irrHA_bm2_R_C_Y_GLU )
writedata( L161.ag_rfdHA_bm2_R_C_Y_GLU,domain="AGLU_LEVEL1_DATA",fn="L161.ag_rfdHA_bm2_R_C_Y_GLU",comments=comments.L161.ag_rfdHA_bm2_R_C_Y_GLU )
writedata( L161.ag_irrYield_kgm2_R_C_Y_GLU,domain="AGLU_LEVEL1_DATA",fn="L161.ag_irrYield_kgm2_R_C_Y_GLU",comments=comments.L161.ag_irrYield_kgm2_R_C_Y_GLU )
writedata( L161.ag_rfdYield_kgm2_R_C_Y_GLU,domain="AGLU_LEVEL1_DATA",fn="L161.ag_rfdYield_kgm2_R_C_Y_GLU",comments=comments.L161.ag_rfdYield_kgm2_R_C_Y_GLU )
writedata( L161.ag_irrHA_frac_R_C_GLU,domain="AGLU_LEVEL1_DATA",fn="L161.ag_irrHA_frac_R_C_GLU",comments=comments.L161.ag_irrHA_frac_R_C_GLU )

# Every script should finish with this line
logstop()
