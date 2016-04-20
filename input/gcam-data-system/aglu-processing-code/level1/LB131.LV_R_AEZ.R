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
logstart( "LB131.LV_R_AEZ.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Land value by region and AEZ" )

# -----------------------------------------------------------------------------
# 1. Read data

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
L100.GTAP_LV_milUSD <- readdata( "AGLU_LEVEL1_DATA", "L100.GTAP_LV_milUSD" )
L122.LC_bm2_R_HarvCropLand_Yh_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L122.LC_bm2_R_HarvCropLand_Yh_AEZ" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Calculate total value of each AEZ
printlog( "Calculating total economic output by AEZ and region, excluding animal production" )
L100.GTAP_LV_milUSD$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L100.GTAP_LV_milUSD$iso, iso_GCAM_regID$iso ) ]
L131.LV_milUSD_R_AEZ <- aggregate( L100.GTAP_LV_milUSD[ AEZs ], by=as.list( L100.GTAP_LV_milUSD[ R ] ), sum )

#Convert 2001$ to 1975$
L131.LV_milUSD75_R_AEZ <- L131.LV_milUSD_R_AEZ
L131.LV_milUSD75_R_AEZ[ AEZs ] <- L131.LV_milUSD_R_AEZ[ AEZs ] * conv_2001_1975_USD

printlog( "Calculating land areas for land value calculations" )
printlog( "NOTE: Only cropland is used in deriving land values" )
GTAP_base_year <- 2000
X_GTAP_base_year <- paste( "X", GTAP_base_year, sep = "" )

L131.LC_bm2_R_HarvCropLand_AEZ <- L122.LC_bm2_R_HarvCropLand_Yh_AEZ[ c( R_AEZ, X_GTAP_base_year ) ]
L131.LC_bm2_R_HarvCropLand_AEZ <- dcast( L131.LC_bm2_R_HarvCropLand_AEZ, GCAM_region_ID ~ AEZ, value.var = X_GTAP_base_year )

#Calculate land value as economic output divided by land cover
L131.LV_USD75_m2_R_AEZ <- L131.LV_milUSD75_R_AEZ
L131.LV_USD75_m2_R_AEZ[ AEZs ] <-
      L131.LV_milUSD75_R_AEZ[ AEZs ] * 0.001 / L131.LC_bm2_R_HarvCropLand_AEZ[ AEZs ]
L131.LV_USD75_m2_R_AEZ[ is.na( L131.LV_USD75_m2_R_AEZ ) ] <- 0
L131.LV_USD75_m2_R_AEZ[ AEZs ][ L131.LV_USD75_m2_R_AEZ[ AEZs ] == Inf ] <- 0

#L131.LV_USD75_m2_R_AEZ[ L131.LV_USD75_m2_R_AEZ$GCAM_region_ID == 14, AEZs ] <-
#  L131.LV_USD75_m2_R_AEZ[ L131.LV_USD75_m2_R_AEZ$GCAM_region_ID == 14, AEZs ] * 10

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L131.LV_USD75_m2_R_AEZ <- c( "Land value by GCAM region / AEZ","Unit = 1975$/m2" )

writedata( L131.LV_USD75_m2_R_AEZ, domain="AGLU_LEVEL1_DATA", fn="L131.LV_USD75_m2_R_AEZ", comments=comments.L131.LV_USD75_m2_R_AEZ )

# Every script should finish with this line
logstop()
