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
logstart( "L107.an_IMAGE_R_C_Sys_Fd_Y.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Animal product data from IMAGE, assigned to GCAM region / commodity / system / feed / year" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
L100.IMAGE_an_Prodmixfrac_ctry_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L100.IMAGE_an_Prodmixfrac_ctry_C_Y" )
L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y <- readdata( "AGLU_LEVEL1_DATA", "L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y" )
L100.IMAGE_an_FeedIO_ctry_C_Sys_Y <- readdata( "AGLU_LEVEL1_DATA", "L100.IMAGE_an_FeedIO_ctry_C_Sys_Y" )
L105.an_Prod_Mt_ctry_C_Y <- readdata( "AGLU_LEVEL1_DATA", "L105.an_Prod_Mt_ctry_C_Y" )

# -----------------------------------------------------------------------------
# 2. Perform computations

#calculate mixed production as total prod times mixed fraction. Use this to build a table disaggregated by system.
printlog( "Calculating animal production by country, commodity, and system" )
#Multiply the total production by the fraction mixed, for the relevant commodities
L107.an_Prod_Mt_ctry_C_Y <- subset( L105.an_Prod_Mt_ctry_C_Y, GCAM_commodity %in% L100.IMAGE_an_Prodmixfrac_ctry_C_Y$commodity )
L107.an_Prod_Mt_ctry_C_mix_Y <- L107.an_Prod_Mt_ctry_C_Y
L107.an_Prod_Mt_ctry_C_mix_Y[ X_AGLU_historical_years ] <- L107.an_Prod_Mt_ctry_C_Y[ X_AGLU_historical_years ] * L100.IMAGE_an_Prodmixfrac_ctry_C_Y[
      match( vecpaste( L107.an_Prod_Mt_ctry_C_Y[ c( "iso", C ) ] ),
             vecpaste( L100.IMAGE_an_Prodmixfrac_ctry_C_Y[ c( "iso", "commodity" ) ] ) ),
      X_AGLU_historical_years ]
L107.an_Prod_Mt_ctry_C_mix_Y$system <- "Mixed"
L107.an_Prod_Mt_ctry_C_past_Y <- L107.an_Prod_Mt_ctry_C_Y
L107.an_Prod_Mt_ctry_C_past_Y[ X_AGLU_historical_years ] <- L107.an_Prod_Mt_ctry_C_Y[ X_AGLU_historical_years ] -
      L107.an_Prod_Mt_ctry_C_mix_Y[ X_AGLU_historical_years ]
L107.an_Prod_Mt_ctry_C_past_Y$system <- "Pastoral"
L107.an_Prod_Mt_ctry_C_Sys_Y <- rbind( L107.an_Prod_Mt_ctry_C_mix_Y, L107.an_Prod_Mt_ctry_C_past_Y )

printlog( "Calculating animal production by country, commodity, system, and feed type" )
L107.an_Prod_Mt_ctry_C_Sys_Fd_Y <- repeat_and_add_vector( L107.an_Prod_Mt_ctry_C_Sys_Y, Fd, unique( L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y$input ) )
L107.an_Prod_Mt_ctry_C_Sys_Fd_Y[ X_AGLU_historical_years ] <- L107.an_Prod_Mt_ctry_C_Sys_Fd_Y[ X_AGLU_historical_years ] * L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y[
      match( vecpaste( L107.an_Prod_Mt_ctry_C_Sys_Fd_Y[ c( "iso", C_Sys_Fd ) ]),
             vecpaste( L100.IMAGE_an_Feedfrac_ctry_C_Sys_Fd_Y[ c( "iso", "commodity", Sys, "input" ) ] ) ),
      X_AGLU_historical_years ]
L107.an_Prod_Mt_ctry_C_Sys_Fd_Y <- na.omit( L107.an_Prod_Mt_ctry_C_Sys_Fd_Y )

printlog( "Calculating inputs to animal production by country, commodity, system, and feed type" )
L107.an_Feed_Mt_ctry_C_Sys_Fd_Y <- L107.an_Prod_Mt_ctry_C_Sys_Fd_Y
L107.an_Feed_Mt_ctry_C_Sys_Fd_Y[ X_AGLU_historical_years ] <- L107.an_Prod_Mt_ctry_C_Sys_Fd_Y[ X_AGLU_historical_years ] * L100.IMAGE_an_FeedIO_ctry_C_Sys_Y[
      match( vecpaste( L107.an_Prod_Mt_ctry_C_Sys_Fd_Y[ c( "iso", C_Sys ) ] ),
             vecpaste( L100.IMAGE_an_FeedIO_ctry_C_Sys_Y[ c( "iso", "commodity", Sys ) ] ) ),
      X_AGLU_historical_years ]

printlog( "Aggregating animal production and feed inputs to GCAM regions" )
L107.an_Prod_Mt_ctry_C_Sys_Fd_Y$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L107.an_Prod_Mt_ctry_C_Sys_Fd_Y$iso, iso_GCAM_regID$iso)]
L107.an_Prod_Mt_R_C_Sys_Fd_Y <- aggregate( L107.an_Prod_Mt_ctry_C_Sys_Fd_Y[ X_AGLU_historical_years ], by=as.list( L107.an_Prod_Mt_ctry_C_Sys_Fd_Y[ R_C_Sys_Fd ] ), sum )

L107.an_Feed_Mt_ctry_C_Sys_Fd_Y$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L107.an_Feed_Mt_ctry_C_Sys_Fd_Y$iso, iso_GCAM_regID$iso)]
L107.an_Feed_Mt_R_C_Sys_Fd_Y <- aggregate( L107.an_Feed_Mt_ctry_C_Sys_Fd_Y[ X_AGLU_historical_years ], by=as.list( L107.an_Feed_Mt_ctry_C_Sys_Fd_Y[ R_C_Sys_Fd ] ), sum )

printlog( "Calculating weighted average feed input-output coefficients by region, commodity, system, and feed type" )
L107.an_FeedIO_R_C_Sys_Fd_Y <- L107.an_Feed_Mt_R_C_Sys_Fd_Y
L107.an_FeedIO_R_C_Sys_Fd_Y[ X_AGLU_historical_years ] <- L107.an_Feed_Mt_R_C_Sys_Fd_Y[ X_AGLU_historical_years ] / L107.an_Prod_Mt_R_C_Sys_Fd_Y[
      match( vecpaste( L107.an_Feed_Mt_R_C_Sys_Fd_Y[ R_C_Sys_Fd ] ), vecpaste( L107.an_Prod_Mt_R_C_Sys_Fd_Y[ R_C_Sys_Fd ] ) ),
      X_AGLU_historical_years ]

#Setting input-output coefs of non-existent combinations to 100
L107.an_FeedIO_R_C_Sys_Fd_Y[ is.na( L107.an_FeedIO_R_C_Sys_Fd_Y ) ] <- 100

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L107.an_Prod_Mt_R_C_Sys_Fd_Y <- c( "Animal production by GCAM region / commodity / system / feed type / year","Unit = Mt" )
comments.L107.an_Feed_Mt_R_C_Sys_Fd_Y <- c( "Animal feed consumption by GCAM region / commodity / system / feed type / year","Unit = Mt" )
comments.L107.an_FeedIO_R_C_Sys_Fd_Y <- c( "Animal production input-output coefficients by GCAM region / commodity / system / feed type / year","Unitless" )

#write tables as CSV files
writedata( L107.an_Prod_Mt_R_C_Sys_Fd_Y, domain="AGLU_LEVEL1_DATA",fn="L107.an_Prod_Mt_R_C_Sys_Fd_Y", comments=comments.L107.an_Prod_Mt_R_C_Sys_Fd_Y )
writedata( L107.an_Feed_Mt_R_C_Sys_Fd_Y, domain="AGLU_LEVEL1_DATA",fn="L107.an_Feed_Mt_R_C_Sys_Fd_Y", comments=comments.L107.an_Feed_Mt_R_C_Sys_Fd_Y )
writedata( L107.an_FeedIO_R_C_Sys_Fd_Y, domain="AGLU_LEVEL1_DATA",fn="L107.an_FeedIO_R_C_Sys_Fd_Y", comments=comments.L107.an_FeedIO_R_C_Sys_Fd_Y )

# Every script should finish with this line
logstop()
