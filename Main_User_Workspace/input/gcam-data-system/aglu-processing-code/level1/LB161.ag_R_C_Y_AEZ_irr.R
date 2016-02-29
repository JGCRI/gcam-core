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
logstart( "LB161.ag_R_C_Y_AEZ_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Primary agricultural data assigned to GCAM region / commodity / year / AEZ" )

# -----------------------------------------------------------------------------
# 1. Read data from previous files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
L104.ag_Prod_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L104.ag_Prod_Mt_R_C_Y_AEZ" )
L103.ag_HA_bm2_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L103.ag_HA_bm2_R_C_Y_AEZ" )
L152.ag_irrProd_Mt_R_C_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L152.ag_irrProd_Mt_R_C_AEZ" )
L152.ag_irrHA_bm2_R_C_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L152.ag_irrHA_bm2_R_C_AEZ" )
L152.ag_rfdProd_Mt_R_C_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L152.ag_rfdProd_Mt_R_C_AEZ" )
L152.ag_rfdHA_bm2_R_C_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L152.ag_rfdHA_bm2_R_C_AEZ" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Compute fraction of production that is irrigated by GCAM region / commodity / AEZ
printlog( "Compute fraction of production that is irrigated for each GCAM region / commodity / AEZ" )
L152.ag_irrProd_Mt_R_C_AEZ[ is.na( L152.ag_irrProd_Mt_R_C_AEZ ) ] <- 0
L152.ag_irrProd_Mt_R_C_AEZ[ AEZs ] <- sapply(L152.ag_irrProd_Mt_R_C_AEZ[ AEZs ], as.numeric )
L152.ag_rfdProd_Mt_R_C_AEZ[ is.na( L152.ag_rfdProd_Mt_R_C_AEZ ) ] <- 0
L152.ag_rfdProd_Mt_R_C_AEZ[ AEZs ] <- sapply(L152.ag_rfdProd_Mt_R_C_AEZ[ AEZs ], as.numeric )

L161.ag_irrProd_frac_R_C_AEZ <- L152.ag_irrProd_Mt_R_C_AEZ
L161.ag_irrProd_frac_R_C_AEZ[ AEZs ] <- L152.ag_irrProd_Mt_R_C_AEZ[ AEZs ] / (L152.ag_irrProd_Mt_R_C_AEZ[ AEZs ] + L152.ag_rfdProd_Mt_R_C_AEZ[ AEZs ] )
L161.ag_irrProd_frac_R_C_AEZ[ is.na( L161.ag_irrProd_frac_R_C_AEZ ) ] <- 0

#Compute fraction of harvested area that is irrigated by GCAM region / commodity / AEZ
printlog( "Compute fraction of harvested area that is irrigated for each GCAM region / commodity / AEZ" )
L161.ag_irrHA_frac_R_C_AEZ <- L152.ag_irrHA_bm2_R_C_AEZ
L161.ag_irrHA_frac_R_C_AEZ[ AEZs ] <- L152.ag_irrHA_bm2_R_C_AEZ[ AEZs ] / (L152.ag_irrHA_bm2_R_C_AEZ[ AEZs ] + L152.ag_rfdHA_bm2_R_C_AEZ[ AEZs ] )
L161.ag_irrHA_frac_R_C_AEZ[ is.na( L161.ag_irrHA_frac_R_C_AEZ ) ] <- 0

#Combine FAO and GTAP: create tables with crop production and harvested area by AEZ and model base years.
#production ( Mt )
printlog( "Disaggregating FAO production and harvested area of all crops to AEZs using GTAP data" )
#Repeat fraction table by number of historical years, and multiply fraction table by total annual production
L161.ag_irrProd_frac_R_C_Y_AEZ <- repeat_and_add_vector( L161.ag_irrProd_frac_R_C_AEZ, "year", historical_years )

#Melt tables to prepare for further calculation
L161.ag_irrProd_frac_R_C_Y_AEZ.melt <- melt( L161.ag_irrProd_frac_R_C_Y_AEZ, id.vars = R_C_Y, variable.name = AEZ )
L104.ag_Prod_Mt_R_C_Y_AEZ.melt <- melt(L104.ag_Prod_Mt_R_C_Y_AEZ, id.vars = R_C_AEZ, variable.name = "Xyear" )
L104.ag_Prod_Mt_R_C_Y_AEZ.melt$year <- as.numeric( substr( L104.ag_Prod_Mt_R_C_Y_AEZ.melt$Xyear, 2, 5 ) )

# Calculate irrigated (rainfed) production by multiplying total by fraction irrigated (rainfed)
L161.ag_irrProd_Mt_R_C_Y_AEZ.melt <- L104.ag_Prod_Mt_R_C_Y_AEZ.melt
L161.ag_irrProd_Mt_R_C_Y_AEZ.melt$value <- L104.ag_Prod_Mt_R_C_Y_AEZ.melt$value[
      match( vecpaste( L161.ag_irrProd_Mt_R_C_Y_AEZ.melt[ R_C_Y_AEZ ] ),
             vecpaste( L104.ag_Prod_Mt_R_C_Y_AEZ.melt[ R_C_Y_AEZ ]))] *
      L161.ag_irrProd_frac_R_C_Y_AEZ.melt$value[
          match( vecpaste(L161.ag_irrProd_Mt_R_C_Y_AEZ.melt[ R_C_Y_AEZ ] ),
                 vecpaste(L161.ag_irrProd_frac_R_C_Y_AEZ.melt[ R_C_Y_AEZ ] ) ) ]

L161.ag_rfdProd_Mt_R_C_Y_AEZ.melt <- L104.ag_Prod_Mt_R_C_Y_AEZ.melt
L161.ag_rfdProd_Mt_R_C_Y_AEZ.melt$value <- L104.ag_Prod_Mt_R_C_Y_AEZ.melt$value[
      match( vecpaste(L161.ag_rfdProd_Mt_R_C_Y_AEZ.melt[ R_C_Y_AEZ ] ),
             vecpaste(L104.ag_Prod_Mt_R_C_Y_AEZ.melt[ R_C_Y_AEZ ] ) ) ] *
      (1 - L161.ag_irrProd_frac_R_C_Y_AEZ.melt$value[
         match( vecpaste( L161.ag_rfdProd_Mt_R_C_Y_AEZ.melt[ R_C_Y_AEZ ] ),
                vecpaste(L161.ag_irrProd_frac_R_C_Y_AEZ.melt[ R_C_Y_AEZ ] ) ) ] )

#Repeat fraction table by number of historical years, and multiply fraction table by total annual production
L161.ag_irrHA_frac_R_C_Y_AEZ <- repeat_and_add_vector( L161.ag_irrHA_frac_R_C_AEZ, "year", historical_years )

#Melt tables to prepare for further calculation
L161.ag_irrHA_frac_R_C_Y_AEZ.melt <- melt( L161.ag_irrHA_frac_R_C_Y_AEZ, id.vars = R_C_Y, variable.name = AEZ )
L161.ag_HA_bm2_R_C_Y_AEZ.melt <- melt( L103.ag_HA_bm2_R_C_Y_AEZ, id.vars = R_C_AEZ, variable.name = "Xyear" )
L161.ag_HA_bm2_R_C_Y_AEZ.melt$year <- as.numeric( substr( L161.ag_HA_bm2_R_C_Y_AEZ.melt$Xyear, 2, 5 ) )

# Calculate irrigated (rainfed) production by multiplying total by fraction irrigated (rainfed)
L161.ag_irrHA_bm2_R_C_Y_AEZ.melt <- L161.ag_HA_bm2_R_C_Y_AEZ.melt
L161.ag_irrHA_bm2_R_C_Y_AEZ.melt$value <- L161.ag_HA_bm2_R_C_Y_AEZ.melt$value[
      match( vecpaste(L161.ag_irrHA_bm2_R_C_Y_AEZ.melt[ R_C_Y_AEZ ] ),
             vecpaste(L161.ag_HA_bm2_R_C_Y_AEZ.melt[ R_C_Y_AEZ ] ) ) ] *
      L161.ag_irrHA_frac_R_C_Y_AEZ.melt$value[
         match( vecpaste(L161.ag_irrHA_bm2_R_C_Y_AEZ.melt[ R_C_Y_AEZ ] ),
                vecpaste(L161.ag_irrHA_frac_R_C_Y_AEZ.melt[ R_C_Y_AEZ ] ) ) ]

L161.ag_rfdHA_bm2_R_C_Y_AEZ.melt <- L161.ag_HA_bm2_R_C_Y_AEZ.melt
L161.ag_rfdHA_bm2_R_C_Y_AEZ.melt$value <- L161.ag_HA_bm2_R_C_Y_AEZ.melt$value[
      match( vecpaste(L161.ag_rfdHA_bm2_R_C_Y_AEZ.melt[ R_C_Y_AEZ ]),
             vecpaste(L161.ag_HA_bm2_R_C_Y_AEZ.melt[ R_C_Y_AEZ ]))] *
      (1 - L161.ag_irrHA_frac_R_C_Y_AEZ.melt$value[
          match( vecpaste(L161.ag_rfdHA_bm2_R_C_Y_AEZ.melt[ R_C_Y_AEZ ]),
                 vecpaste( L161.ag_irrHA_frac_R_C_Y_AEZ.melt[ R_C_Y_AEZ ]))])

# Cast by AEZ
L161.ag_irrProd_Mt_R_C_Y_AEZ <- dcast( L161.ag_irrProd_Mt_R_C_Y_AEZ.melt, GCAM_region_ID + GCAM_commodity + year ~ AEZ, value.var = "value" )
L161.ag_rfdProd_Mt_R_C_Y_AEZ <- dcast( L161.ag_rfdProd_Mt_R_C_Y_AEZ.melt, GCAM_region_ID + GCAM_commodity + year ~ AEZ, value.var = "value" )
L161.ag_irrHA_bm2_R_C_Y_AEZ <- dcast( L161.ag_irrHA_bm2_R_C_Y_AEZ.melt, GCAM_region_ID + GCAM_commodity + year ~ AEZ, value.var = "value" )
L161.ag_rfdHA_bm2_R_C_Y_AEZ <- dcast( L161.ag_rfdHA_bm2_R_C_Y_AEZ.melt, GCAM_region_ID + GCAM_commodity + year ~ AEZ, value.var = "value" )

# Sort
L161.ag_irrProd_Mt_R_C_Y_AEZ <- L161.ag_irrProd_Mt_R_C_Y_AEZ[
      order( L161.ag_irrProd_Mt_R_C_Y_AEZ$year, L161.ag_irrProd_Mt_R_C_Y_AEZ$GCAM_commodity, L161.ag_irrProd_Mt_R_C_Y_AEZ$GCAM_region_ID), ]
L161.ag_rfdProd_Mt_R_C_Y_AEZ <- L161.ag_rfdProd_Mt_R_C_Y_AEZ[
      order( L161.ag_rfdProd_Mt_R_C_Y_AEZ$year, L161.ag_rfdProd_Mt_R_C_Y_AEZ$GCAM_commodity, L161.ag_rfdProd_Mt_R_C_Y_AEZ$GCAM_region_ID), ]
L161.ag_irrHA_bm2_R_C_Y_AEZ <- L161.ag_irrHA_bm2_R_C_Y_AEZ[
      order( L161.ag_irrHA_bm2_R_C_Y_AEZ$year, L161.ag_irrHA_bm2_R_C_Y_AEZ$GCAM_commodity, L161.ag_irrHA_bm2_R_C_Y_AEZ$GCAM_region_ID), ]
L161.ag_rfdHA_bm2_R_C_Y_AEZ <- L161.ag_rfdHA_bm2_R_C_Y_AEZ[
      order( L161.ag_rfdHA_bm2_R_C_Y_AEZ$year, L161.ag_rfdHA_bm2_R_C_Y_AEZ$GCAM_commodity, L161.ag_rfdHA_bm2_R_C_Y_AEZ$GCAM_region_ID), ]

#calculate yield in kilograms per square meter
printlog( "Calculating yield estimates by region, crop, year, and AEZ" )
L161.ag_irrYield_kgm2_R_C_Y_AEZ <- L161.ag_irrProd_Mt_R_C_Y_AEZ
L161.ag_irrYield_kgm2_R_C_Y_AEZ[ AEZs ] <- L161.ag_irrProd_Mt_R_C_Y_AEZ[ AEZs ] / ( L161.ag_irrHA_bm2_R_C_Y_AEZ[ AEZs ] + 1e-9 )
L161.ag_irrYield_kgm2_R_C_Y_AEZ[is.na(L161.ag_irrYield_kgm2_R_C_Y_AEZ)]<-0   

L161.ag_rfdYield_kgm2_R_C_Y_AEZ <- L161.ag_rfdProd_Mt_R_C_Y_AEZ
L161.ag_rfdYield_kgm2_R_C_Y_AEZ[ AEZs ] <- L161.ag_rfdProd_Mt_R_C_Y_AEZ[ AEZs ] / ( L161.ag_rfdHA_bm2_R_C_Y_AEZ[ AEZs ] + 1e-9 )
L161.ag_rfdYield_kgm2_R_C_Y_AEZ[is.na(L161.ag_rfdYield_kgm2_R_C_Y_AEZ)]<-0    

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L161.ag_irrProd_Mt_R_C_Y_AEZ <- c( "AEZ-wise irrigated production by GCAM region / commodity","Unit = Mt" )
comments.L161.ag_rfdProd_Mt_R_C_Y_AEZ <- c( "AEZ-wise rainfed production by GCAM region / commodity","Unit = Mt" )
comments.L161.ag_irrHA_bm2_R_C_Y_AEZ <- c( "Irrigated harvested area by GCAM region / commodity / year / AEZ","Unit = bm2" )
comments.L161.ag_rfdHA_bm2_R_C_Y_AEZ <- c( "Rainfed harvested area by GCAM region / commodity / year / AEZ","Unit = bm2" )
comments.L161.ag_irrYield_kgm2_R_C_Y_AEZ <- c( "Unadjusted irrigated agronomic yield by GCAM region / commodity / year / AEZ","Unit = kg.m2" )
comments.L161.ag_rfdYield_kgm2_R_C_Y_AEZ <- c( "Unadjusted rainfed agronomic yield by GCAM region / commodity / year / AEZ","Unit = kg.m2" )
comments.L161.ag_irrHA_frac_R_C_AEZ <- c( "Fraction of harvested area that is irigated by GCAM region / commodity / year / AEZ", "Unit = fract" )

#write out final tables
writedata( L161.ag_irrProd_Mt_R_C_Y_AEZ,domain="AGLU_LEVEL1_DATA",fn="L161.ag_irrProd_Mt_R_C_Y_AEZ",comments=comments.L161.ag_irrProd_Mt_R_C_Y_AEZ )
writedata( L161.ag_rfdProd_Mt_R_C_Y_AEZ,domain="AGLU_LEVEL1_DATA",fn="L161.ag_rfdProd_Mt_R_C_Y_AEZ",comments=comments.L161.ag_rfdProd_Mt_R_C_Y_AEZ )
writedata( L161.ag_irrHA_bm2_R_C_Y_AEZ,domain="AGLU_LEVEL1_DATA",fn="L161.ag_irrHA_bm2_R_C_Y_AEZ",comments=comments.L161.ag_irrHA_bm2_R_C_Y_AEZ )
writedata( L161.ag_rfdHA_bm2_R_C_Y_AEZ,domain="AGLU_LEVEL1_DATA",fn="L161.ag_rfdHA_bm2_R_C_Y_AEZ",comments=comments.L161.ag_rfdHA_bm2_R_C_Y_AEZ )
writedata( L161.ag_irrYield_kgm2_R_C_Y_AEZ,domain="AGLU_LEVEL1_DATA",fn="L161.ag_irrYield_kgm2_R_C_Y_AEZ",comments=comments.L161.ag_irrYield_kgm2_R_C_Y_AEZ )
writedata( L161.ag_rfdYield_kgm2_R_C_Y_AEZ,domain="AGLU_LEVEL1_DATA",fn="L161.ag_rfdYield_kgm2_R_C_Y_AEZ",comments=comments.L161.ag_rfdYield_kgm2_R_C_Y_AEZ )
writedata( L161.ag_irrHA_frac_R_C_AEZ,domain="AGLU_LEVEL1_DATA",fn="L161.ag_irrHA_frac_R_C_AEZ",comments=comments.L161.ag_irrHA_frac_R_C_AEZ )

# Every script should finish with this line
logstop()
