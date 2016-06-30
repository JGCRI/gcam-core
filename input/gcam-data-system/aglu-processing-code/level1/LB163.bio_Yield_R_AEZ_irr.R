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
logstart( "LB163.bio_Yield_R_AEZ_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Biomass yield by region / AEZ, base year" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
EPIC_bio_Yield <- readdata( "AGLU_LEVEL0_DATA", "EPIC_bio_Yield" )
L161.ag_rfdHA_bm2_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_rfdHA_bm2_R_C_Y_AEZ" )
L161.ag_rfdProd_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_rfdProd_Mt_R_C_Y_AEZ" )
L161.ag_irrHA_bm2_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrHA_bm2_R_C_Y_AEZ" )
L161.ag_irrProd_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrProd_Mt_R_C_Y_AEZ" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "2a. Compute rainfed bioenergy yields by region and AEZ" )
#Subset only the relevant (cellulosic) crops from the production and harvested area databases
printlog( "Aggregating yield data for crops being used for computing regional bioenergy indices" )
L163.ag_rfdProd_Mt_R_Ccell_fby_AEZ <- L161.ag_rfdProd_Mt_R_C_Y_AEZ[
      L161.ag_rfdProd_Mt_R_C_Y_AEZ$GCAM_commodity %in% cellulosic_crops & L161.ag_rfdProd_Mt_R_C_Y_AEZ$year == max( historical_years ),
      c( R_C_Y, AEZs) ]
L163.ag_rfdHA_bm2_R_Ccell_fby_AEZ <- L161.ag_rfdHA_bm2_R_C_Y_AEZ[
      L161.ag_rfdHA_bm2_R_C_Y_AEZ$GCAM_commodity %in% cellulosic_crops & L161.ag_rfdHA_bm2_R_C_Y_AEZ$year == max( historical_years ),
      c( R_C_Y, AEZs) ]
L163.ag_rfdYield_tm2_R_Ccell_fby_AEZ <- L163.ag_rfdProd_Mt_R_Ccell_fby_AEZ
L163.ag_rfdYield_tm2_R_Ccell_fby_AEZ[ AEZs ] <- L163.ag_rfdYield_tm2_R_Ccell_fby_AEZ[ AEZs ] / L163.ag_rfdHA_bm2_R_Ccell_fby_AEZ[ AEZs ]

#Regional index calculation using cellulosic crops
L163.ag_rfdProd_Mt_R_Ccell_fby_AEZ <- L163.ag_rfdProd_Mt_R_Ccell_fby_AEZ[ order( L163.ag_rfdProd_Mt_R_Ccell_fby_AEZ$GCAM_commodity,
      L163.ag_rfdProd_Mt_R_Ccell_fby_AEZ$GCAM_region_ID ) , ]
L163.ag_rfdHA_bm2_R_Ccell_fby_AEZ <- L163.ag_rfdHA_bm2_R_Ccell_fby_AEZ[ order( L163.ag_rfdHA_bm2_R_Ccell_fby_AEZ$GCAM_commodity,
      L163.ag_rfdHA_bm2_R_Ccell_fby_AEZ$GCAM_region_ID ) , ]
L163.ag_Index_R_Ccell <- L163.ag_rfdProd_Mt_R_Ccell_fby_AEZ[ c( R_C_Y ) ]
L163.ag_Index_R_Ccell$Prod_Mt <- rowSums( L163.ag_rfdProd_Mt_R_Ccell_fby_AEZ[ AEZs ] )
L163.ag_Index_R_Ccell$HA_bm2 <- rowSums( L163.ag_rfdHA_bm2_R_Ccell_fby_AEZ[ AEZs ] )
L163.ag_Index_R_Ccell$Yield_kgm2 <- L163.ag_Index_R_Ccell$Prod_Mt / L163.ag_Index_R_Ccell$HA_bm2
L163.ag_Index_R_Ccell$Yield_kgm2[ is.na( L163.ag_Index_R_Ccell$Yield_kgm2 ) ] <- 0

#Index each region's yield to region 1, and weight by the production
USA_regID <- iso_GCAM_regID$GCAM_region_ID[ iso_GCAM_regID$iso == "usa" ]
L163.ag_Index_USA_Ccell <- subset( L163.ag_Index_R_Ccell, GCAM_region_ID == USA_regID )
L163.ag_Index_R_Ccell$Yield_USA <- L163.ag_Index_USA_Ccell$Yield_kgm2[
      match( L163.ag_Index_R_Ccell$GCAM_commodity, L163.ag_Index_USA_Ccell$GCAM_commodity ) ]
L163.ag_Index_R_Ccell$Index <- L163.ag_Index_R_Ccell$Yield_kgm2 / L163.ag_Index_R_Ccell$Yield_USA
L163.ag_Index_R_Ccell$ProdxIndex <- L163.ag_Index_R_Ccell$Index * L163.ag_Index_R_Ccell$Prod_Mt
#Aggregate by crop to compute regional indices (to USA region)
L163.ag_bioYieldIndex_R <- aggregate( L163.ag_Index_R_Ccell[ c( "Prod_Mt", "ProdxIndex" ) ],
      list( GCAM_region_ID = L163.ag_Index_R_Ccell$GCAM_region_ID ), sum )
#Set regional index to a maximum of 1 (no region can be > USA)
L163.ag_bioYieldIndex_R$bioYieldIndex <- pmin( 1, L163.ag_bioYieldIndex_R$ProdxIndex / L163.ag_bioYieldIndex_R$Prod_Mt )

#In regions with no crops of the relevant type, just use a default factor
default_bioYieldIndex <- 0.5
L163.ag_bioYieldIndex_R$bioYieldIndex[ is.na( L163.ag_bioYieldIndex_R$bioYieldIndex ) ] <- default_bioYieldIndex

#AEZ indexing using cellolosic crops
printlog( "Aggregating yield data for crops being used for computing AEZ bioenergy indices" )
L163.ag_rfdProd_Mt_R_Ccell_fby_AEZ <- L161.ag_rfdProd_Mt_R_C_Y_AEZ[
      L161.ag_rfdProd_Mt_R_C_Y_AEZ$GCAM_commodity %in% cellulosic_crops & L161.ag_rfdProd_Mt_R_C_Y_AEZ$year == max( historical_years ),
      c( R_C_Y, AEZs) ]
L163.ag_rfdHA_bm2_R_Ccell_fby_AEZ <- L161.ag_rfdHA_bm2_R_C_Y_AEZ[ L161.ag_rfdHA_bm2_R_C_Y_AEZ$GCAM_commodity %in% cellulosic_crops
      & L161.ag_rfdHA_bm2_R_C_Y_AEZ$year == max( historical_years ),
      c( R_C_Y, AEZs) ]

#Aggregate by region and melt the tables
L163.ag_rfdProd_Mt_Ccell_fby_AEZ <- aggregate( L163.ag_rfdProd_Mt_R_Ccell_fby_AEZ[ AEZs ],
      list( GCAM_commodity = L163.ag_rfdProd_Mt_R_Ccell_fby_AEZ$GCAM_commodity ), sum )
L163.ag_rfdHA_bm2_Ccell_fby_AEZ <- aggregate( L163.ag_rfdHA_bm2_R_Ccell_fby_AEZ[ AEZs ],
      list( GCAM_commodity = L163.ag_rfdHA_bm2_R_Ccell_fby_AEZ$GCAM_commodity ), sum )

L163.ag_rfdProd_Mt_Ccell_fby_AEZ.melt <- melt( L163.ag_rfdProd_Mt_Ccell_fby_AEZ, id.vars="GCAM_commodity" ) 
L163.ag_rfdHA_bm2_Ccell_fby_AEZ.melt <- melt( L163.ag_rfdHA_bm2_Ccell_fby_AEZ, id.vars="GCAM_commodity" ) 

#Create a single table for computation of the indices
printlog( "Calculating AEZ bioenergy indices" )
L163.ag_Index_Ccell_AEZ <- data.frame(
      GCAM_commodity = L163.ag_rfdProd_Mt_Ccell_fby_AEZ.melt$GCAM_commodity,
      AEZ = L163.ag_rfdProd_Mt_Ccell_fby_AEZ.melt$variable,
      Prod_Mt = L163.ag_rfdProd_Mt_Ccell_fby_AEZ.melt$value )
L163.ag_Index_Ccell_AEZ$HA_bm2 <- L163.ag_rfdHA_bm2_Ccell_fby_AEZ.melt$value
L163.ag_Index_Ccell_AEZ$Yield_kgm2 <- L163.ag_Index_Ccell_AEZ$Prod_Mt / L163.ag_Index_Ccell_AEZ$HA_bm2
L163.ag_Index_Ccell_AEZ$Yield_kgm2[ is.na( L163.ag_Index_Ccell_AEZ$Yield_kgm2 ) ] <- 0

L163.ag_Index_Ccell_IndexAEZ <- L163.ag_Index_Ccell_AEZ[ L163.ag_Index_Ccell_AEZ$AEZ == Index_AEZ, ]
L163.ag_Index_Ccell_AEZ$Yield_IndexAEZ <- L163.ag_Index_Ccell_IndexAEZ$Yield_kgm2[
      match( L163.ag_Index_Ccell_AEZ$GCAM_commodity, L163.ag_Index_Ccell_IndexAEZ$GCAM_commodity ) ]
L163.ag_Index_Ccell_AEZ$Index <- L163.ag_Index_Ccell_AEZ$Yield_kgm2 / L163.ag_Index_Ccell_AEZ$Yield_IndexAEZ
L163.ag_Index_Ccell_AEZ$ProdxIndex <- L163.ag_Index_Ccell_AEZ$Index * L163.ag_Index_Ccell_AEZ$Prod_Mt

#Aggregate by AEZ to compute each AEZ's production-weighted index
L163.ag_bioYieldIndex_AEZ <- aggregate( L163.ag_Index_Ccell_AEZ[ c( "Prod_Mt", "ProdxIndex" ) ],
      list( AEZ = L163.ag_Index_Ccell_AEZ$AEZ ), sum )
L163.ag_bioYieldIndex_AEZ$Index_Ccell <- L163.ag_bioYieldIndex_AEZ$ProdxIndex / L163.ag_bioYieldIndex_AEZ$Prod_Mt
L163.ag_bioYieldIndex_AEZ$Index_Ccell[ is.na( L163.ag_bioYieldIndex_AEZ$Index_Ccell ) ] <- 0

#NOTE: Setting AEZ11's AEZ index equal to AEZ10
L163.ag_bioYieldIndex_AEZ$Index_Ccell[ L163.ag_bioYieldIndex_AEZ$AEZ == "AEZ11" ] <-
      L163.ag_bioYieldIndex_AEZ$Index_Ccell[ L163.ag_bioYieldIndex_AEZ$AEZ == "AEZ10" ]
      
#Cast by AEZs and repeat by number of regions
L163.ag_bioYieldIndex_AEZ.cast <- as.data.frame( t( L163.ag_bioYieldIndex_AEZ$Index_Ccell ) )
names( L163.ag_bioYieldIndex_AEZ.cast ) <- AEZs
L163.ag_bioYieldIndex_AEZ.cast_repR <- L163.ag_bioYieldIndex_AEZ.cast[ rep( 1, times = length( unique( iso_GCAM_regID[[R]] ) ) ) , ]

#Build table of yield indices by region and AEZ
printlog( "Multiplying regional bioenergy indices by EPIC-adjusted AEZ bioenergy indices" )
L163.ag_bioYieldIndex_AEZ_repR <- data.frame( GCAM_region_ID = sort( unique( iso_GCAM_regID[[R]] ) ), L163.ag_bioYieldIndex_AEZ.cast_repR )
L163.ag_bioYieldIndex_R_AEZ <- L163.ag_bioYieldIndex_AEZ_repR
L163.ag_bioYieldIndex_R_AEZ[ AEZs ] <- L163.ag_bioYieldIndex_R$bioYieldIndex * L163.ag_bioYieldIndex_AEZ.cast_repR[ AEZs ]

#Convert units and adjust yields in EPIC yield table
printlog( "Converting units from EPIC and adjusting upwards for consistency with literature estimates" )
EPIC_bio_Yield$MEAN_kgm2 <- EPIC_bio_Yield$MEAN * bio_GJt / conv_Ha_m2
EPIC_bio_Yield$MEAN_SD_kgm2 <- ( EPIC_bio_Yield$MEAN + EPIC_bio_Yield$STD ) * bio_GJt / conv_Ha_m2
EPIC_bio_Yield$MEAN_mult_kgm2 <- EPIC_bio_Yield$MEAN * bio_yield_mult * bio_GJt / conv_Ha_m2

#Multiply table of indices by region and AEZ by base yield in index AEZ. Arid AEZs use a different base yield.
printlog( "Multiplying base year yields by region and AEZ bioenergy indices for two scenarios" )
L163.ag_rfdBioYield_GJm2_R_AEZ_ref <- L163.ag_bioYieldIndex_R_AEZ
L163.ag_rfdBioYield_GJm2_R_AEZ_ref[ AEZs ] <-
      L163.ag_rfdBioYield_GJm2_R_AEZ_ref[ AEZs ] * EPIC_bio_Yield$MEAN_SD_kgm2[ EPIC_bio_Yield$AEZ == Index_AEZ ]
L163.ag_rfdBioYield_GJm2_R_AEZ_ref[ AEZs_arid ] <-
      L163.ag_rfdBioYield_GJm2_R_AEZ_ref[ AEZs_arid ] * EPIC_bio_Yield$MEAN_kgm2[ EPIC_bio_Yield$AEZ == Index_AEZ ]

#For "hi" scenario, arid AEZs use the same yield base
L163.ag_rfdBioYield_GJm2_R_AEZ_hi <- L163.ag_bioYieldIndex_R_AEZ
# comment out code that included the b-yield-mult term -maw 6/17/2014
#L163.ag_rfdBioYield_GJm2_R_AEZ_hi[ AEZs ] <-
#      L163.ag_bioYieldIndex_R_AEZ[ AEZs ] * EPIC_bio_Yield$MEAN_mult_kgm2[ EPIC_bio_Yield$AEZ == Index_AEZ ]
L163.ag_rfdBioYield_GJm2_R_AEZ_hi[ AEZs ] <-
  L163.ag_bioYieldIndex_R_AEZ[ AEZs ] * EPIC_bio_Yield$MEAN_kgm2[ EPIC_bio_Yield$AEZ == Index_AEZ ]

printlog( "2b. Compute ratio of irrigated to rainfed yield")
printlog( "Aggregating yield data for crops being used for computing AEZ bioenergy indices" )
L163.ag_irrProd_Mt_R_Ccell_fby_AEZ <- L161.ag_irrProd_Mt_R_C_Y_AEZ[
      L161.ag_irrProd_Mt_R_C_Y_AEZ$GCAM_commodity %in% cellulosic_crops & L161.ag_irrProd_Mt_R_C_Y_AEZ$year == max( historical_years ),
      c( R_C_Y, AEZs) ]
L163.ag_irrHA_bm2_R_Ccell_fby_AEZ <- L161.ag_irrHA_bm2_R_C_Y_AEZ[ L161.ag_irrHA_bm2_R_C_Y_AEZ$GCAM_commodity %in% cellulosic_crops
      & L161.ag_irrHA_bm2_R_C_Y_AEZ$year == max( historical_years ),
      c( R_C_Y, AEZs) ]
L163.ag_irrYield_tm2_R_Ccell_fby_AEZ <- L163.ag_irrProd_Mt_R_Ccell_fby_AEZ
L163.ag_irrYield_tm2_R_Ccell_fby_AEZ[ AEZs ] <- L163.ag_irrYield_tm2_R_Ccell_fby_AEZ[ AEZs ] / L163.ag_irrHA_bm2_R_Ccell_fby_AEZ[ AEZs ]

L163.ag_YieldRatio_tm2_R_Ccell_fby_AEZ <- L163.ag_irrYield_tm2_R_Ccell_fby_AEZ
L163.ag_YieldRatio_tm2_R_Ccell_fby_AEZ[ AEZs ] <- L163.ag_irrYield_tm2_R_Ccell_fby_AEZ[ AEZs ] / L163.ag_rfdYield_tm2_R_Ccell_fby_AEZ[ AEZs ]
L163.ag_YieldRatio_tm2_R_Ccell_fby_AEZ[ is.na( L163.ag_YieldRatio_tm2_R_Ccell_fby_AEZ ) ] <- 1
L163.ag_YieldRatio_tm2_R_Ccell_fby_AEZ[ L163.ag_YieldRatio_tm2_R_Ccell_fby_AEZ == Inf ] <- 1

#TODO: Use a weighted average yield ratio
L163.ag_YieldRatio_tm2_R_AEZ <- L163.ag_YieldRatio_tm2_R_Ccell_fby_AEZ[ L163.ag_YieldRatio_tm2_R_Ccell_fby_AEZ$GCAM_commodity == "Wheat", ]
max_irrYieldRatio <- 10
L163.ag_YieldRatio_tm2_R_AEZ[ AEZs ][ L163.ag_YieldRatio_tm2_R_AEZ[ AEZs ] > max_irrYieldRatio ] <- max_irrYieldRatio

# APPLY TO BIOENERGY
L163.ag_irrBioYield_GJm2_R_AEZ_ref <- L163.ag_rfdBioYield_GJm2_R_AEZ_ref
L163.ag_irrBioYield_GJm2_R_AEZ_ref[ AEZs ] <- L163.ag_irrBioYield_GJm2_R_AEZ_ref[ AEZs ] * L163.ag_YieldRatio_tm2_R_AEZ[ AEZs ]

L163.ag_irrBioYield_GJm2_R_AEZ_hi <- L163.ag_rfdBioYield_GJm2_R_AEZ_hi
L163.ag_irrBioYield_GJm2_R_AEZ_hi[ AEZs ] <- L163.ag_irrBioYield_GJm2_R_AEZ_hi[ AEZs ] * L163.ag_YieldRatio_tm2_R_AEZ[ AEZs ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L163.ag_rfdBioYield_GJm2_R_AEZ_ref <- c( "Reference base year bioenergy yields for rainfed crops by GCAM region / AEZ","Unit = GJ.m2" )
comments.L163.ag_rfdBioYield_GJm2_R_AEZ_hi <- c( "High base year bioenergy yields for rainfed crops by GCAM region / AEZ","Unit = GJ.m2" )
comments.L163.ag_irrBioYield_GJm2_R_AEZ_ref <- c( "Reference base year bioenergy yields for irrigated crops by GCAM region / AEZ","Unit = GJ.m2" )
comments.L163.ag_irrBioYield_GJm2_R_AEZ_hi <- c( "High base year bioenergy yields for irrigated crops by GCAM region / AEZ","Unit = GJ.m2" )

writedata( L163.ag_rfdBioYield_GJm2_R_AEZ_ref, domain="AGLU_LEVEL1_DATA", fn="L163.ag_rfdBioYield_GJm2_R_AEZ_ref", comments=comments.L163.ag_rfdBioYield_GJm2_R_AEZ_ref )
writedata( L163.ag_rfdBioYield_GJm2_R_AEZ_hi, domain="AGLU_LEVEL1_DATA", fn="L163.ag_rfdBioYield_GJm2_R_AEZ_hi", comments=comments.L163.ag_rfdBioYield_GJm2_R_AEZ_hi )
writedata( L163.ag_irrBioYield_GJm2_R_AEZ_ref, domain="AGLU_LEVEL1_DATA", fn="L163.ag_irrBioYield_GJm2_R_AEZ_ref", comments=comments.L163.ag_irrBioYield_GJm2_R_AEZ_ref )
writedata( L163.ag_irrBioYield_GJm2_R_AEZ_hi, domain="AGLU_LEVEL1_DATA", fn="L163.ag_irrBioYield_GJm2_R_AEZ_hi", comments=comments.L163.ag_irrBioYield_GJm2_R_AEZ_hi )

# Every script should finish with this line
logstop()
