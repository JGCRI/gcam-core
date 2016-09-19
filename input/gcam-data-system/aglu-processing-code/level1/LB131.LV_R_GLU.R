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
logstart( "LB131.LV_R_GLU.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Land value by region and GLU" )

# -----------------------------------------------------------------------------
# 1. Read data

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
L100.GTAP_LV_milUSD <- readdata( "AGLU_LEVEL1_DATA", "L100.GTAP_LV_milUSD" )
L122.LC_bm2_R_HarvCropLand_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L122.LC_bm2_R_HarvCropLand_Yh_GLU" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Calculate total value of each GLU
printlog( "Calculating total economic output by GLU and region, excluding animal production" )
L100.GTAP_LV_milUSD[[R]] <- iso_GCAM_regID[[R]][ match( L100.GTAP_LV_milUSD$iso, iso_GCAM_regID$iso ) ]
# convert to 1975$ at this stage
L131.LV_milUSD75_R_GLU <- aggregate( L100.GTAP_LV_milUSD[ "value" ]  * conv_2001_1975_USD,
                                   by = L100.GTAP_LV_milUSD[ R_GLU ], sum )

printlog( "Calculating land areas for land value calculations" )
printlog( "NOTE: Only cropland is used in deriving land values" )
X_GTAP_base_year <- paste( "X", 2000, sep = "" )

#Calculate land value as economic output divided by land cover
L131.LV_USD75_m2_R_GLU <- L131.LV_milUSD75_R_GLU
names( L131.LV_USD75_m2_R_GLU )[ names( L131.LV_USD75_m2_R_GLU ) == "value" ] <- "LV_milUSD75"
L131.LV_USD75_m2_R_GLU$HarvCropLand_bm2 <- L122.LC_bm2_R_HarvCropLand_Yh_GLU[[X_GTAP_base_year]][
  match( vecpaste( L131.LV_USD75_m2_R_GLU[ R_GLU ] ),
         vecpaste( L122.LC_bm2_R_HarvCropLand_Yh_GLU[ R_GLU ] ) ) ]
L131.LV_USD75_m2_R_GLU$LV_USD75_m2 <- with( L131.LV_USD75_m2_R_GLU, LV_milUSD75 * conv_mil_bil / HarvCropLand_bm2 )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L131.LV_USD75_m2_R_GLU <- c( "Land value by GCAM region / GLU","Unit = 1975$/m2" )

writedata( L131.LV_USD75_m2_R_GLU, domain="AGLU_LEVEL1_DATA", fn="L131.LV_USD75_m2_R_GLU", comments=comments.L131.LV_USD75_m2_R_GLU )

# Every script should finish with this line
logstop()
