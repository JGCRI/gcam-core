# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "ENERGYPROC_DIR" ) ){
    if( Sys.getenv( "ENERGYPROC" ) != "" ){
        ENERGYPROC_DIR <- Sys.getenv( "ENERGYPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var ENERGYPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
logstart( "LA100.CDIAC_downscale_ctry.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Historical CO2 emissions from CDIAC downscaled to country" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
CDIAC_CO2_by_nation <- readdata( "EMISSIONS_LEVEL0_DATA", "CDIAC_CO2_by_nation" )
CDIAC_Cseq_by_nation <- readdata( "EMISSIONS_LEVEL0_DATA", "CDIAC_Cseq_by_nation" )
CDIAC_nation <- readdata( "EMISSIONS_MAPPINGS", "CDIAC_nation" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Merge the sequestration and emissions datasets
CDIAC_Cseq_by_nation$nation <- CDIAC_nation$nation[ match( CDIAC_Cseq_by_nation$UN_code, CDIAC_nation$UN_code ) ]
CDIAC_CO2_by_nation$liquids.sequestration <- abs( CDIAC_Cseq_by_nation$liquids.sequestration[
      match( paste( CDIAC_CO2_by_nation$nation, CDIAC_CO2_by_nation$year ),
             paste( CDIAC_Cseq_by_nation$nation, CDIAC_Cseq_by_nation$year ) ) ] )
CDIAC_CO2_by_nation$liquids.sequestration[ is.na( CDIAC_CO2_by_nation$liquids.sequestration  ) ] <- 0             

#Subset only the years being processed
L100.CDIAC_CO2_ctry_hist <- subset( CDIAC_CO2_by_nation, year %in% CO2_historical_years )

#Generate time series of Former Soviet Union by country, using national shares in the first available year
L100.CO2_ctry_noUSSR_hist <- subset( L100.CDIAC_CO2_ctry_hist, nation != "USSR" )
L100.CO2_USSR_hist <- subset( L100.CDIAC_CO2_ctry_hist, nation == "USSR" )
USSR_years <- unique( L100.CO2_USSR_hist$year )
L100.CO2_USSR_hist_repCtry <- repeat_and_add_vector( L100.CO2_USSR_hist, "iso", CDIAC_nation$iso[ CDIAC_nation$nation == "USSR" ] )

L100.CO2_ctry_noUSSR_hist$iso <- CDIAC_nation$iso[ match( L100.CO2_ctry_noUSSR_hist$nation, CDIAC_nation$nation ) ]
L100.CO2_ctry_noUSSR_hist <- na.omit( L100.CO2_ctry_noUSSR_hist )
L100.CO2_postUSSR_hist <- subset( L100.CO2_ctry_noUSSR_hist, iso %in% L100.CO2_USSR_hist_repCtry$iso & year == max( USSR_years ) + 1 )
datacols <- names( L100.CDIAC_CO2_ctry_hist )[ names( L100.CDIAC_CO2_ctry_hist ) %!in% c( "nation", "year", "iso" ) ]
L100.CO2_postUSSR_hist_shares <- L100.CO2_postUSSR_hist
L100.CO2_postUSSR_hist_shares[ datacols ] <- sweep( L100.CO2_postUSSR_hist[ datacols ],
      2, colSums( L100.CO2_postUSSR_hist[ datacols ] ), "/" )

L100.CO2_FSU_hist <- L100.CO2_USSR_hist_repCtry
L100.CO2_FSU_hist[ datacols ] <- L100.CO2_USSR_hist_repCtry[ datacols ] *
      L100.CO2_postUSSR_hist_shares[ match( L100.CO2_FSU_hist$iso, L100.CO2_postUSSR_hist_shares$iso ),
      datacols ]

#Repeat for Yugoslavia
L100.CO2_ctry_noUSSR_Yug_hist <- subset( L100.CO2_ctry_noUSSR_hist, nation != "YUGOSLAVIA (FORMER SOCIALIST FEDERAL REPUBLIC)" )
L100.CO2_Yug_hist <- subset( L100.CO2_ctry_noUSSR_hist, nation == "YUGOSLAVIA (FORMER SOCIALIST FEDERAL REPUBLIC)" )
Yug_years <- unique( L100.CO2_Yug_hist$year )
L100.CO2_Yug_hist_repCtry <- repeat_and_add_vector( L100.CO2_Yug_hist, "iso",
      CDIAC_nation$iso[ CDIAC_nation$nation == "YUGOSLAVIA (FORMER SOCIALIST FEDERAL REPUBLIC)" ] )

L100.CO2_ctry_noUSSR_Yug_hist$iso <- CDIAC_nation$iso[ match( L100.CO2_ctry_noUSSR_Yug_hist$nation, CDIAC_nation$nation ) ]
L100.CO2_ctry_noUSSR_Yug_hist <- na.omit( L100.CO2_ctry_noUSSR_Yug_hist )
L100.CO2_postYug_hist <- subset( L100.CO2_ctry_noUSSR_Yug_hist, iso %in% L100.CO2_Yug_hist_repCtry$iso & year == max( Yug_years ) + 1 )
datacols <- names( L100.CDIAC_CO2_ctry_hist )[ names( L100.CDIAC_CO2_ctry_hist ) %!in% c( "nation", "year", "iso" ) ]
L100.CO2_postYug_hist_shares <- L100.CO2_postYug_hist
L100.CO2_postYug_hist_shares[ datacols ] <- sweep( L100.CO2_postYug_hist[ datacols ],
      2, colSums( L100.CO2_postYug_hist[ datacols ] ), "/" )

L100.CO2_FYug_hist <- L100.CO2_Yug_hist_repCtry
L100.CO2_FYug_hist[ datacols ] <- L100.CO2_Yug_hist_repCtry[ datacols ] *
      L100.CO2_postYug_hist_shares[ match( L100.CO2_FYug_hist$iso, L100.CO2_postYug_hist_shares$iso ),
      datacols ]
L100.CO2_FYug_hist[ datacols ][ is.na( L100.CO2_FYug_hist[ datacols ] ) ] <- 0

L100.CDIAC_CO2_ctry_hist <- rbind( L100.CO2_ctry_noUSSR_Yug_hist, L100.CO2_FSU_hist, L100.CO2_FYug_hist )
L100.CDIAC_CO2_ctry_hist <- L100.CDIAC_CO2_ctry_hist[ order( L100.CDIAC_CO2_ctry_hist$iso, L100.CDIAC_CO2_ctry_hist$year ), c( "iso", "year", datacols ) ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L100.CDIAC_CO2_ctry_hist <- c( "CO2 emissions by country / fuel type / historical year","Unit = kt C" )

#write tables as CSV files
writedata( L100.CDIAC_CO2_ctry_hist, domain="ENERGY_LEVEL1_DATA", fn="L100.CDIAC_CO2_ctry_hist", comments=comments.L100.CDIAC_CO2_ctry_hist )

# Every script should finish with this line
logstop()
