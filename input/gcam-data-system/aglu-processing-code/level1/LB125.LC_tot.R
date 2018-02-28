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
logstart( "LB125.LC_tot.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Total land area and area check" )

# -----------------------------------------------------------------------------
# 1. Read data

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
L120.LC_bm2_R_UrbanLand_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L120.LC_bm2_R_UrbanLand_Yh_GLU" )
L120.LC_bm2_R_Tundra_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L120.LC_bm2_R_Tundra_Yh_GLU" )
L120.LC_bm2_R_RckIceDsrt_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L120.LC_bm2_R_RckIceDsrt_Yh_GLU" )
L122.LC_bm2_R_HarvCropLand_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L122.LC_bm2_R_HarvCropLand_Yh_GLU" )
L122.LC_bm2_R_OtherArableLand_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L122.LC_bm2_R_OtherArableLand_Yh_GLU" )
L123.LC_bm2_R_MgdPast_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L123.LC_bm2_R_MgdPast_Yh_GLU" )
L123.LC_bm2_R_MgdFor_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L123.LC_bm2_R_MgdFor_Yh_GLU" )
L124.LC_bm2_R_Shrub_Yh_GLU_adj <- readdata( "AGLU_LEVEL1_DATA", "L124.LC_bm2_R_Shrub_Yh_GLU_adj" )
L124.LC_bm2_R_Grass_Yh_GLU_adj <- readdata( "AGLU_LEVEL1_DATA", "L124.LC_bm2_R_Grass_Yh_GLU_adj" )
L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj <- readdata( "AGLU_LEVEL1_DATA", "L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj" )
L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj <- readdata( "AGLU_LEVEL1_DATA", "L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Create a table with all land types
#NOTE: If protected lands are included, uncomment the text below
L125.LC_bm2_R_LT_Yh_GLU.list <- list( L120.LC_bm2_R_UrbanLand_Yh_GLU, L120.LC_bm2_R_Tundra_Yh_GLU, L120.LC_bm2_R_RckIceDsrt_Yh_GLU,
      L122.LC_bm2_R_HarvCropLand_Yh_GLU, L122.LC_bm2_R_OtherArableLand_Yh_GLU, L123.LC_bm2_R_MgdPast_Yh_GLU, L123.LC_bm2_R_MgdFor_Yh_GLU,
      L124.LC_bm2_R_Shrub_Yh_GLU_adj, L124.LC_bm2_R_Grass_Yh_GLU_adj, L124.LC_bm2_R_UnMgdPast_Yh_GLU_adj, L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj )
L125.LC_bm2_R_LT_Yh_GLU <- do.call( rbind, L125.LC_bm2_R_LT_Yh_GLU.list )

printlog( "Adding up total land area by region, year, and GLU" )
L125.LC_bm2_R_Yh_GLU <- aggregate( L125.LC_bm2_R_LT_Yh_GLU[ X_land_cover_years ],
                                   by = L125.LC_bm2_R_LT_Yh_GLU[ R_GLU ], sum )

#Land cover should not change between periods. Check to make sure that this is the case
printlog( "Checking that total land area does not fluctuate between time periods in any region/GLU" )
L125.LC_check <- L125.LC_bm2_R_Yh_GLU
for( i in 2:ncol( L125.LC_check[ X_land_cover_years ] ) ){
	L125.LC_check[ X_land_cover_years ][i] <- L125.LC_bm2_R_Yh_GLU[ X_land_cover_years ][i] / L125.LC_bm2_R_Yh_GLU[ X_land_cover_years ][i-1]
}
L125.LC_check[ is.na( L125.LC_check ) ] <- 1
L125.LC_check[X_land_cover_years][1] <- 1

if( any( L125.LC_check[ X_land_cover_years ] < (1 - LandTolerance) |
      L125.LC_check[ X_land_cover_years ] > (1 + LandTolerance) ) )
      { stop( "ERROR: Interannual fluctuation in global land cover exceeds tolerance threshold" ) }

#Write out the totals, by region and by region x GLU
L125.LC_bm2_R_GLU <- aggregate( L125.LC_bm2_R_Yh_GLU[[ X_land_cover_years[1] ]],
                            by = L125.LC_bm2_R_Yh_GLU[ R_GLU ], sum )
names( L125.LC_bm2_R_GLU )[ names( L125.LC_bm2_R_GLU ) == "x" ] <- "LC_bm2"

L125.LC_bm2_R <- aggregate( L125.LC_bm2_R_Yh_GLU[[ X_land_cover_years[1] ]],
                            by = L125.LC_bm2_R_Yh_GLU[ R ], sum )
names( L125.LC_bm2_R )[ names( L125.LC_bm2_R ) == "x" ] <- "LC_bm2"
L125.LC_bm2_R$LC_bm2 <- round( L125.LC_bm2_R$LC_bm2, digits_land_total )

#Land cover totals differentiated by land use types: round totals to specified number of digits
L125.LC_bm2_R_LT_Yh_GLU[ X_land_cover_years ] <- round( L125.LC_bm2_R_LT_Yh_GLU[ X_land_cover_years ], digits_land_use )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L125.LC_bm2_R <- c( "Total land cover by GCAM region","Unit = bm2" )
comments.L125.LC_bm2_R_GLU <- c( "Total land cover by GCAM region and GLU","Unit = bm2" )
comments.L125.LC_bm2_R_LT_Yh_GLU <- c( "Total land cover by GCAM region / land type / historical year / GLU","Unit = bm2" )

writedata( L125.LC_bm2_R, domain="AGLU_LEVEL1_DATA", fn="L125.LC_bm2_R", comments=comments.L125.LC_bm2_R )
writedata( L125.LC_bm2_R_GLU, domain="AGLU_LEVEL1_DATA", fn="L125.LC_bm2_R_GLU", comments=comments.L125.LC_bm2_R_GLU )
writedata( L125.LC_bm2_R_LT_Yh_GLU, domain="AGLU_LEVEL1_DATA", fn="L125.LC_bm2_R_LT_Yh_GLU", comments=comments.L125.LC_bm2_R_LT_Yh_GLU )

# Every script should finish with this line
logstop()
