# Before we can load headers we need some paths defined.  They
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
logstart( "LA100.IMAGE_downscale_ctry_yr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Downscaling of IMAGE animal production characteristics to country, and writing out to all historical years" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
AGLU_ctry <- readdata( "AGLU_MAPPINGS", "AGLU_ctry" )
IMAGE_an_Feedfrac_Rimg_C_Sys_Fd_Y <- readdata( "AGLU_LEVEL0_DATA", "IMAGE_an_Feedfrac_Rimg_C_Sys_Fd_Y" )
IMAGE_an_FeedIO_Rimg_C_Sys_Y <- readdata( "AGLU_LEVEL0_DATA", "IMAGE_an_FeedIO_Rimg_C_Sys_Y" )
IMAGE_an_Prodmixfrac_Rimg_C_Y <- readdata( "AGLU_LEVEL0_DATA", "IMAGE_an_Prodmixfrac_Rimg_C_Y" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Write out each IMAGE table to all historical years
L100.IMAGE_an_Feedfrac_Rimg_C_Sys_Fd_Y <- interpolate_IMAGE_years( IMAGE_an_Feedfrac_Rimg_C_Sys_Fd_Y, idvars = c( "commodity", Sys, "input", Y ), AGLU_historical_years )
L100.IMAGE_an_FeedIO_Rimg_C_Sys_Y <- interpolate_IMAGE_years( IMAGE_an_FeedIO_Rimg_C_Sys_Y, idvars = c( "commodity", Sys, Y ), AGLU_historical_years )
L100.IMAGE_an_Prodmixfrac_Rimg_C_Y <- interpolate_IMAGE_years( IMAGE_an_Prodmixfrac_Rimg_C_Y, idvars = c( "commodity", Y ), AGLU_historical_years )

#Downscale IMAGE region-level data to all countries
L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y <- downscale_IMAGE_regions( L100.IMAGE_an_Feedfrac_Rimg_C_Sys_Fd_Y, idvars = c( "IMAGE_region_ID", "commodity", Sys, "input" ) )
L100.IMAGE_an_FeedIO_ctry_C_Sys_Y <- downscale_IMAGE_regions( L100.IMAGE_an_FeedIO_Rimg_C_Sys_Y, idvars = c( "IMAGE_region_ID", "commodity", Sys ) )
L100.IMAGE_an_Prodmixfrac_ctry_C_Y <- downscale_IMAGE_regions( L100.IMAGE_an_Prodmixfrac_Rimg_C_Y, idvars = c( "IMAGE_region_ID", "commodity" ) )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y <- c( "IMAGE feed fractions by country / commodity / system / feed type / year","Unitless" )
comments.L100.IMAGE_an_FeedIO_ctry_C_Sys_Y <- c( "IMAGE input-output coefficients by country / commodity / system / year","Unitless" )
comments.L100.IMAGE_an_Prodmixfrac_ctry_C_Y <- c( "IMAGE mixed fractions by country / commodity / year","Unitless" )

#write tables as CSV files
writedata( L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y, domain="AGLU_LEVEL1_DATA", fn="L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y", comments=comments.L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y )
writedata( L100.IMAGE_an_FeedIO_ctry_C_Sys_Y, domain="AGLU_LEVEL1_DATA", fn="L100.IMAGE_an_FeedIO_ctry_C_Sys_Y", comments=comments.L100.IMAGE_an_FeedIO_ctry_C_Sys_Y )
writedata( L100.IMAGE_an_Prodmixfrac_ctry_C_Y, domain="AGLU_LEVEL1_DATA", fn="L100.IMAGE_an_Prodmixfrac_ctry_C_Y", comments=comments.L100.IMAGE_an_Prodmixfrac_ctry_C_Y )

# Every script should finish with this line
logstop()
