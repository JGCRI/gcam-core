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
logstart( "LB171.LC_R_Cropland_Yh_GLU_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Cropland cover (irrigated and rainfed) by region / crop / historical year / GLU" )

# -----------------------------------------------------------------------------
# 1. Read data

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
L122.LC_bm2_R_HarvCropLand_C_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU" )
L161.ag_irrHA_bm2_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrHA_bm2_R_C_Y_GLU" )
L161.ag_rfdHA_bm2_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_rfdHA_bm2_R_C_Y_GLU" )
L161.ag_irrProd_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrProd_Mt_R_C_Y_GLU" )
L161.ag_rfdProd_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_rfdProd_Mt_R_C_Y_GLU" )
L161.ag_irrHA_frac_R_C_GLU <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrHA_frac_R_C_GLU" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Downscaling cropland by region, crop, and GLU to irrigated/rainfed according to irrigated/rainfed shares in base year")
printlog( "NOTE: Assuming the same irrigated:rainfed share in all historical periods (due to lack of data indicating otherwise)" )
L171.ag_irrHA_frac_R_C_GLU <- L161.ag_irrHA_frac_R_C_GLU[ c( R_C_GLU, "irrHA_frac" ) ]
L171.ag_irrHA_frac_R_C_GLU$rfd_share <- 1 - L171.ag_irrHA_frac_R_C_GLU$irrHA_frac

L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU <- L122.LC_bm2_R_HarvCropLand_C_Yh_GLU
L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU[ X_land_cover_years ] <- L122.LC_bm2_R_HarvCropLand_C_Yh_GLU[ X_land_cover_years ] *
      L171.ag_irrHA_frac_R_C_GLU$irrHA_frac[
         match( vecpaste( L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU[ R_C_GLU ] ),
                vecpaste( L171.ag_irrHA_frac_R_C_GLU[ R_C_GLU ] ) ) ]

# Where values are missing, assume rainfed. This is left as a check; there are none currently (5/10/16)
L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU[ is.na( L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU ) ] <- 0

L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU <- L122.LC_bm2_R_HarvCropLand_C_Yh_GLU
L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU[ X_land_cover_years ] <- L122.LC_bm2_R_HarvCropLand_C_Yh_GLU[ X_land_cover_years ] *
      L171.ag_irrHA_frac_R_C_GLU$rfd_share[
         match( vecpaste( L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU[ R_C_GLU ] ),
                vecpaste( L171.ag_irrHA_frac_R_C_GLU[ R_C_GLU ] ) ) ]

# For the rainfed cropland table, missing values default to cropland quantities (presently all zero anyway).
L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU[ !complete.cases( L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU ), X_land_cover_years ] <-
  L122.LC_bm2_R_HarvCropLand_C_Yh_GLU[ !complete.cases( L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU ), X_land_cover_years ]

printlog( "Calculating economic yields as production divided by cropland" )
L171.ag_irrEcYield_kgm2_R_C_Y_GLU <- L161.ag_irrProd_Mt_R_C_Y_GLU
L171.ag_irrEcYield_kgm2_R_C_Y_GLU[ X_historical_years ] <- L161.ag_irrProd_Mt_R_C_Y_GLU[ X_historical_years ] /
      L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU[
         match( vecpaste( L171.ag_irrEcYield_kgm2_R_C_Y_GLU[ R_C_GLU ] ),
                vecpaste( L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU[ R_C_GLU ] ) ),
         X_historical_years ]
L171.ag_irrEcYield_kgm2_R_C_Y_GLU[ is.na( L171.ag_irrEcYield_kgm2_R_C_Y_GLU ) ] <- 0

L171.ag_rfdEcYield_kgm2_R_C_Y_GLU <- L161.ag_rfdProd_Mt_R_C_Y_GLU
L171.ag_rfdEcYield_kgm2_R_C_Y_GLU[ X_historical_years ] <- L161.ag_rfdProd_Mt_R_C_Y_GLU[ X_historical_years ] /
      L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU[
         match( vecpaste( L171.ag_rfdEcYield_kgm2_R_C_Y_GLU[ R_C_GLU ] ),
                vecpaste( L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU[ R_C_GLU ] ) ),
         X_historical_years ]
L171.ag_rfdEcYield_kgm2_R_C_Y_GLU[ is.na( L171.ag_rfdEcYield_kgm2_R_C_Y_GLU ) ] <- 0

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU <- c( "Irrigated harvested cropland cover by GCAM region / commodity / year / GLU","Unit = bm2" )
comments.L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU <- c( "Rainfed harvested cropland cover by GCAM region / commodity / year / GLU","Unit = bm2" )
comments.L171.ag_irrEcYield_kgm2_R_C_Y_GLU <- c( "Adjusted economic yield for irrigated crops by GCAM region / commodity / year / GLU","Unit = kg.m2" )
comments.L171.ag_rfdEcYield_kgm2_R_C_Y_GLU <- c( "Adjusted economic yield for rainfed crops by GCAM region / commodity / year / GLU","Unit = kg.m2" )

writedata( L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU, domain="AGLU_LEVEL1_DATA", fn="L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU", comments=comments.L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU )
writedata( L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU, domain="AGLU_LEVEL1_DATA", fn="L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU", comments=comments.L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU )
writedata( L171.ag_irrEcYield_kgm2_R_C_Y_GLU, domain="AGLU_LEVEL1_DATA", fn="L171.ag_irrEcYield_kgm2_R_C_Y_GLU", comments=comments.L171.ag_irrEcYield_kgm2_R_C_Y_GLU )
writedata( L171.ag_rfdEcYield_kgm2_R_C_Y_GLU, domain="AGLU_LEVEL1_DATA", fn="L171.ag_rfdEcYield_kgm2_R_C_Y_GLU", comments=comments.L171.ag_rfdEcYield_kgm2_R_C_Y_GLU )

# Every script should finish with this line
logstop()
