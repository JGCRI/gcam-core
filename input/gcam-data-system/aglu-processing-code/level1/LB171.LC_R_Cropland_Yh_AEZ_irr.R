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
logstart( "LB171.LC_R_Cropland_Yh_AEZ_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Cropland cover (irrigated and rainfed) by region / crop / historical year / AEZ" )

# -----------------------------------------------------------------------------
# 1. Read data

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
L122.LC_bm2_R_HarvCropLand_C_Yh_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L122.LC_bm2_R_HarvCropLand_C_Yh_AEZ" )
L161.ag_irrHA_bm2_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrHA_bm2_R_C_Y_AEZ" )
L161.ag_rfdHA_bm2_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_rfdHA_bm2_R_C_Y_AEZ" )
L161.ag_irrProd_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrProd_Mt_R_C_Y_AEZ" )
L161.ag_rfdProd_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_rfdProd_Mt_R_C_Y_AEZ" )
L161.ag_irrHA_frac_R_C_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrHA_frac_R_C_AEZ" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Downscaling cropland by region, crop, and AEZ to irrigated/rainfed according to irrigated/rainfed shares in base year")
printlog( "NOTE: Assuming the same irrigated:rainfed share in all historical periods (due to lack of data indicating otherwise)" )
L171.ag_irrHA_frac_R_C_AEZ.melt <- melt( L161.ag_irrHA_frac_R_C_AEZ, id.vars = R_C, variable.name = "AEZ", value.name = "irr_share" )
L171.ag_irrHA_frac_R_C_AEZ.melt$rfd_share <- 1 - L171.ag_irrHA_frac_R_C_AEZ.melt$irr_share

L171.LC_bm2_R_irrHarvCropLand_C_Yh_AEZ <- L122.LC_bm2_R_HarvCropLand_C_Yh_AEZ
L171.LC_bm2_R_irrHarvCropLand_C_Yh_AEZ[ X_land_cover_years ] <- L122.LC_bm2_R_HarvCropLand_C_Yh_AEZ[ X_land_cover_years ] *
      L171.ag_irrHA_frac_R_C_AEZ.melt$irr_share[
         match( vecpaste( L171.LC_bm2_R_irrHarvCropLand_C_Yh_AEZ[ R_C_AEZ ] ),
                vecpaste( L171.ag_irrHA_frac_R_C_AEZ.melt[ R_C_AEZ ] ) ) ]

L171.LC_bm2_R_rfdHarvCropLand_C_Yh_AEZ <- L122.LC_bm2_R_HarvCropLand_C_Yh_AEZ
L171.LC_bm2_R_rfdHarvCropLand_C_Yh_AEZ[ X_land_cover_years ] <- L122.LC_bm2_R_HarvCropLand_C_Yh_AEZ[ X_land_cover_years ] *
      L171.ag_irrHA_frac_R_C_AEZ.melt$rfd_share[
         match( vecpaste( L171.LC_bm2_R_rfdHarvCropLand_C_Yh_AEZ[ R_C_AEZ ] ),
                vecpaste( L171.ag_irrHA_frac_R_C_AEZ.melt[ R_C_AEZ ] ) ) ]

printlog( "Calculating economic yields as production divided by cropland" )
L171.ag_irrProd_Mt_R_C_Y_AEZ.melt <- melt( L161.ag_irrProd_Mt_R_C_Y_AEZ, measure.vars = AEZs, variable.name = AEZ )
L171.ag_irrProd_Mt_R_C_Y_AEZ.melt$Xyear <- paste0( "X", L171.ag_irrProd_Mt_R_C_Y_AEZ.melt$year )
L171.ag_irrProd_Mt_R_C_Y_AEZ <- dcast( L171.ag_irrProd_Mt_R_C_Y_AEZ.melt, GCAM_region_ID + GCAM_commodity + AEZ ~ Xyear )

L171.ag_irrEcYield_kgm2_R_C_Y_AEZ <- L171.ag_irrProd_Mt_R_C_Y_AEZ
L171.ag_irrEcYield_kgm2_R_C_Y_AEZ[ X_historical_years ] <- L171.ag_irrProd_Mt_R_C_Y_AEZ[ X_historical_years ] /
      L171.LC_bm2_R_irrHarvCropLand_C_Yh_AEZ[
         match( vecpaste( L171.ag_irrEcYield_kgm2_R_C_Y_AEZ[ R_C_AEZ ] ),
                vecpaste( L171.LC_bm2_R_irrHarvCropLand_C_Yh_AEZ[ R_C_AEZ ] ) ),
         X_historical_years ]
L171.ag_irrEcYield_kgm2_R_C_Y_AEZ[ is.na( L171.ag_irrEcYield_kgm2_R_C_Y_AEZ ) ] <- 0

L171.ag_rfdProd_Mt_R_C_Y_AEZ.melt <- melt( L161.ag_rfdProd_Mt_R_C_Y_AEZ, measure.vars = AEZs, variable.name = AEZ )
L171.ag_rfdProd_Mt_R_C_Y_AEZ.melt$Xyear <- paste0( "X", L171.ag_rfdProd_Mt_R_C_Y_AEZ.melt$year )
L171.ag_rfdProd_Mt_R_C_Y_AEZ <- dcast( L171.ag_rfdProd_Mt_R_C_Y_AEZ.melt, GCAM_region_ID + GCAM_commodity + AEZ ~ Xyear )

L171.ag_rfdEcYield_kgm2_R_C_Y_AEZ <- L171.ag_rfdProd_Mt_R_C_Y_AEZ
L171.ag_rfdEcYield_kgm2_R_C_Y_AEZ[ X_historical_years ] <- L171.ag_rfdProd_Mt_R_C_Y_AEZ[ X_historical_years ] /
      L171.LC_bm2_R_rfdHarvCropLand_C_Yh_AEZ[
         match( vecpaste( L171.ag_rfdEcYield_kgm2_R_C_Y_AEZ[ R_C_AEZ ] ),
                vecpaste( L171.LC_bm2_R_rfdHarvCropLand_C_Yh_AEZ[ R_C_AEZ ] ) ),
         X_historical_years ]
L171.ag_rfdEcYield_kgm2_R_C_Y_AEZ[ is.na( L171.ag_rfdEcYield_kgm2_R_C_Y_AEZ ) ] <- 0

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L171.LC_bm2_R_irrHarvCropLand_C_Yh_AEZ <- c( "Irrigated harvested cropland cover by GCAM region / commodity / year / AEZ","Unit = bm2" )
comments.L171.LC_bm2_R_rfdHarvCropLand_C_Yh_AEZ <- c( "Rainfed harvested cropland cover by GCAM region / commodity / year / AEZ","Unit = bm2" )
comments.L171.ag_irrEcYield_kgm2_R_C_Y_AEZ <- c( "Adjusted economic yield for irrigated crops by GCAM region / commodity / year / AEZ","Unit = kg.m2" )
comments.L171.ag_rfdEcYield_kgm2_R_C_Y_AEZ <- c( "Adjusted economic yield for rainfed crops by GCAM region / commodity / year / AEZ","Unit = kg.m2" )

writedata( L171.LC_bm2_R_irrHarvCropLand_C_Yh_AEZ, domain="AGLU_LEVEL1_DATA", fn="L171.LC_bm2_R_irrHarvCropLand_C_Yh_AEZ", comments=comments.L171.LC_bm2_R_irrHarvCropLand_C_Yh_AEZ )
writedata( L171.LC_bm2_R_rfdHarvCropLand_C_Yh_AEZ, domain="AGLU_LEVEL1_DATA", fn="L171.LC_bm2_R_rfdHarvCropLand_C_Yh_AEZ", comments=comments.L171.LC_bm2_R_rfdHarvCropLand_C_Yh_AEZ )
writedata( L171.ag_irrEcYield_kgm2_R_C_Y_AEZ, domain="AGLU_LEVEL1_DATA", fn="L171.ag_irrEcYield_kgm2_R_C_Y_AEZ", comments=comments.L171.ag_irrEcYield_kgm2_R_C_Y_AEZ )
writedata( L171.ag_rfdEcYield_kgm2_R_C_Y_AEZ, domain="AGLU_LEVEL1_DATA", fn="L171.ag_rfdEcYield_kgm2_R_C_Y_AEZ", comments=comments.L171.ag_rfdEcYield_kgm2_R_C_Y_AEZ )

# Every script should finish with this line
logstop()
