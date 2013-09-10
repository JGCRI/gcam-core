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
logstart( "LB113.bio_Yield_R_AEZ.R" )
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
L103.ag_HA_bm2_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L103.ag_HA_bm2_R_C_Y_AEZ" )
L104.ag_Prod_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L104.ag_Prod_Mt_R_C_Y_AEZ" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Subset only the relevant (cellulosic) crops from the production and harvested area databases
printlog( "Aggregating yield data for crops being used for computing regional bioenergy indices" )
L113.ag_Prod_Mt_R_Ccell_fby_AEZ <- L104.ag_Prod_Mt_R_C_Y_AEZ[ L104.ag_Prod_Mt_R_C_Y_AEZ$GCAM_commodity %in% cellulosic_crops, c( R_C_AEZ, X_bio_yield_year ) ]
L113.ag_Prod_Mt_R_Ccell_fby <- aggregate( L113.ag_Prod_Mt_R_Ccell_fby_AEZ[ X_bio_yield_year ], by=as.list( L113.ag_Prod_Mt_R_Ccell_fby_AEZ[ R_C ] ), sum )

L113.ag_HA_bm2_R_Ccell_fby_AEZ <- L103.ag_HA_bm2_R_C_Y_AEZ[ L103.ag_HA_bm2_R_C_Y_AEZ$GCAM_commodity %in% cellulosic_crops, c( R_C_AEZ, X_bio_yield_year ) ]
L113.ag_HA_bm2_R_Ccell_fby <- aggregate( L113.ag_HA_bm2_R_Ccell_fby_AEZ[ X_bio_yield_year ], by=as.list( L113.ag_HA_bm2_R_Ccell_fby_AEZ[ R_C ] ), sum )

#Regional index calculation using cellulosic crops
L113.ag_Index_R_Ccell <- data.frame( L113.ag_Prod_Mt_R_Ccell_fby[ R_C ],
      Prod_Mt = L113.ag_Prod_Mt_R_Ccell_fby[[ X_bio_yield_year ]],
      HA_bm2 = L113.ag_HA_bm2_R_Ccell_fby[[ X_bio_yield_year ]],
      Yield_kgm2 = L113.ag_Prod_Mt_R_Ccell_fby[[ X_bio_yield_year ]] / L113.ag_HA_bm2_R_Ccell_fby[[ X_bio_yield_year ]] )

#Index each region's yield to the USA, and weight by the production
USA_regID <- iso_GCAM_regID$GCAM_region_ID[ iso_GCAM_regID$iso == "usa" ]
L113.ag_Index_R_Ccell$Index <- L113.ag_Index_R_Ccell$Yield_kgm2 / L113.ag_Index_R_Ccell$Yield_kgm2[
      match( paste( USA_regID, L113.ag_Index_R_Ccell$GCAM_commodity ),
             paste( L113.ag_Index_R_Ccell$GCAM_region_ID, L113.ag_Index_R_Ccell$GCAM_commodity ) ) ]
L113.ag_Index_R_Ccell$ProdxIndex <- L113.ag_Index_R_Ccell$Index * L113.ag_Index_R_Ccell$Prod_Mt
#Aggregate by crop to compute regional indices (to region 1)
L113.ag_bioYieldIndex_R <- aggregate( L113.ag_Index_R_Ccell[ c( "Prod_Mt", "ProdxIndex" ) ],
      by=as.list( L113.ag_Index_R_Ccell[ R ] ), sum )
#Set regional index to a maximum of 1 (no region can be > USA)
L113.ag_bioYieldIndex_R$bioYieldIndex <- pmin( 1, L113.ag_bioYieldIndex_R$ProdxIndex / L113.ag_bioYieldIndex_R$Prod_Mt )

#AEZ indexing using cellolosic crops
printlog( "Aggregating yield data for crops being used for computing AEZ bioenergy indices" )
L113.ag_Prod_Mt_Ccell_fby_AEZ <- aggregate( L113.ag_Prod_Mt_R_Ccell_fby_AEZ[  X_bio_yield_year ],
      by=as.list( L113.ag_Prod_Mt_R_Ccell_fby_AEZ[ C_AEZ ] ), sum )
L113.ag_HA_bm2_Ccell_fby_AEZ <- aggregate( L113.ag_HA_bm2_R_Ccell_fby_AEZ[ X_bio_yield_year ],
      by=as.list( L113.ag_HA_bm2_R_Ccell_fby_AEZ[ C_AEZ ] ), sum )

#Create a single table for computation of the indices
printlog( "Calculating AEZ bioenergy indices" )
L113.ag_Index_Ccell_AEZ <- data.frame(
      L113.ag_Prod_Mt_Ccell_fby_AEZ[ C_AEZ ],
      Prod_Mt = L113.ag_Prod_Mt_Ccell_fby_AEZ[[ X_bio_yield_year ]],
      HA_bm2 = L113.ag_HA_bm2_Ccell_fby_AEZ[[ X_bio_yield_year ]],
      Yield_kgm2 = L113.ag_Prod_Mt_Ccell_fby_AEZ[[ X_bio_yield_year ]] / L113.ag_HA_bm2_Ccell_fby_AEZ[[ X_bio_yield_year ]] )
L113.ag_Index_Ccell_AEZ$Yield_kgm2[ is.na( L113.ag_Index_Ccell_AEZ$Yield_kgm2 ) ] <- 0

#Index each AEZ's yield to the "index AEZ", and weight by production
L113.ag_Index_Ccell_AEZ$Index <- L113.ag_Index_Ccell_AEZ$Yield_kgm2 / L113.ag_Index_Ccell_AEZ$Yield_kgm2[
      match( paste( L113.ag_Index_Ccell_AEZ$GCAM_commodity, Index_AEZ ),
             paste( L113.ag_Index_Ccell_AEZ$GCAM_commodity, L113.ag_Index_Ccell_AEZ$AEZ ) ) ]
L113.ag_Index_Ccell_AEZ$ProdxIndex <- L113.ag_Index_Ccell_AEZ$Index * L113.ag_Index_Ccell_AEZ$Prod_Mt

#Aggregate by AEZ to compute each AEZ's production-weighted index
L113.ag_bioYieldIndex_AEZ <- aggregate( L113.ag_Index_Ccell_AEZ[ c( "Prod_Mt", "ProdxIndex" ) ], by=as.list( L113.ag_Index_Ccell_AEZ[ AEZ ] ), sum )
L113.ag_bioYieldIndex_AEZ$Index_Ccell <- L113.ag_bioYieldIndex_AEZ$ProdxIndex / L113.ag_bioYieldIndex_AEZ$Prod_Mt
L113.ag_bioYieldIndex_AEZ$Index_Ccell[ is.na( L113.ag_bioYieldIndex_AEZ$Index_Ccell ) ] <- 0

#NOTE: Setting AEZ11's AEZ index equal to AEZ10
L113.ag_bioYieldIndex_AEZ$Index_Ccell[ L113.ag_bioYieldIndex_AEZ$AEZ == "AEZ11" ] <-
      L113.ag_bioYieldIndex_AEZ$Index_Ccell[ L113.ag_bioYieldIndex_AEZ$AEZ == "AEZ10" ]
      
#Cast by AEZs and repeat by number of regions
L113.ag_bioYieldIndex_R_AEZ <- repeat_and_add_vector( L113.ag_bioYieldIndex_AEZ, R, sort( unique( iso_GCAM_regID$GCAM_region_ID ) ) )
L113.ag_bioYieldIndex_R_AEZ <- cast( L113.ag_bioYieldIndex_R_AEZ, GCAM_region_ID ~ AEZ, value = "Index_Ccell" )

#Build table of yield indices by region and AEZ
printlog( "Multiplying regional bioenergy indices by EPIC-adjusted AEZ bioenergy indices" )
L113.ag_bioYieldIndex_R_AEZ[ AEZs ] <- L113.ag_bioYieldIndex_R$bioYieldIndex * L113.ag_bioYieldIndex_R_AEZ[ AEZs ]

#Convert units and adjust yields in EPIC yield table
printlog( "Converting units from EPIC and adjusting upwards for consistency with literature estimates" )
EPIC_bio_Yield$MEAN_kgm2 <- EPIC_bio_Yield$MEAN * bio_GJt / conv_Ha_m2
EPIC_bio_Yield$MEAN_SD_kgm2 <- ( EPIC_bio_Yield$MEAN + EPIC_bio_Yield$STD ) * bio_GJt / conv_Ha_m2
EPIC_bio_Yield$MEAN_mult_kgm2 <- EPIC_bio_Yield$MEAN * bio_yield_mult * bio_GJt / conv_Ha_m2

#Multiply table of indices by region and AEZ by base yield in index AEZ. Arid AEZs use a different base yield.
printlog( "Multiplying base year yields by region and AEZ bioenergy indices for two scenarios" )
L113.ag_bioYield_GJm2_R_AEZ_ref <- L113.ag_bioYieldIndex_R_AEZ
L113.ag_bioYield_GJm2_R_AEZ_ref[ AEZs ] <-
      L113.ag_bioYieldIndex_R_AEZ[ AEZs ] * EPIC_bio_Yield$MEAN_SD_kgm2[ EPIC_bio_Yield$AEZ == Index_AEZ ]
L113.ag_bioYield_GJm2_R_AEZ_ref[ AEZs_arid ] <-
      L113.ag_bioYieldIndex_R_AEZ[ AEZs_arid ] * EPIC_bio_Yield$MEAN_kgm2[ EPIC_bio_Yield$AEZ == Index_AEZ ]
#Set yields to zero in regions with no agricultural data
L113.ag_bioYield_GJm2_R_AEZ_ref[ is.na( L113.ag_bioYield_GJm2_R_AEZ_ref ) ] <- 0

#For "hi" scenario, arid AEZs use the same yield base
L113.ag_bioYield_GJm2_R_AEZ_hi <- L113.ag_bioYieldIndex_R_AEZ
L113.ag_bioYield_GJm2_R_AEZ_hi[ AEZs ] <-
      L113.ag_bioYieldIndex_R_AEZ[ AEZs ] * EPIC_bio_Yield$MEAN_mult_kgm2[ EPIC_bio_Yield$AEZ == Index_AEZ ]
L113.ag_bioYield_GJm2_R_AEZ_hi[ is.na( L113.ag_bioYield_GJm2_R_AEZ_hi ) ] <- 0

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L113.ag_bioYield_GJm2_R_AEZ_ref <- c( "Reference base year bioenergy yields by GCAM region / AEZ","Unit = GJ.m2" )
comments.L113.ag_bioYield_GJm2_R_AEZ_hi <- c( "High base year bioenergy yields by GCAM region / AEZ","Unit = GJ.m2" )

writedata( L113.ag_bioYield_GJm2_R_AEZ_ref, domain="AGLU_LEVEL1_DATA", fn="L113.ag_bioYield_GJm2_R_AEZ_ref", comments=comments.L113.ag_bioYield_GJm2_R_AEZ_ref )
writedata( L113.ag_bioYield_GJm2_R_AEZ_hi, domain="AGLU_LEVEL1_DATA", fn="L113.ag_bioYield_GJm2_R_AEZ_hi", comments=comments.L113.ag_bioYield_GJm2_R_AEZ_hi )

# Every script should finish with this line
logstop()
