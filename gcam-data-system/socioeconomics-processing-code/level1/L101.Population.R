if( !exists( "SOCIOPROC_DIR" ) ){
    if( Sys.getenv( "SOCIOPROC" ) != "" ){
        SOCIOPROC_DIR <- Sys.getenv( "SOCIOPROC" )
    } else {
        stop("Could not determine location of socioeconomics processing scripts, please set the R var SOCIOPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(SOCIOPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(SOCIOPROC_DIR,"/../_common/headers/SOCIO_header.R",sep=""))
logstart( "L101.Population.R" )
adddep(paste(SOCIOPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(SOCIOPROC_DIR,"/../_common/headers/SOCIO_header.R",sep=""))
printlog( "Historical and future population by GCAM region" )

# -----------------------------------------------------------------------------
# 1. Read data

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "SOCIO_ASSUMPTIONS", "A_socioeconomics_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
GCAM3_population <- readdata( "SOCIO_LEVEL0_DATA", "GCAM3_population" )
L100.Pop_thous_ctry_Yh <- readdata( "SOCIO_LEVEL1_DATA", "L100.Pop_thous_ctry_Yh" )
L100.Pop_thous_SSP_ctry_Yfut <- readdata( "SOCIO_LEVEL1_DATA", "L100.Pop_thous_SSP_ctry_Yfut" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Historical population by region
L100.Pop_thous_ctry_Yh[[R]] <- iso_GCAM_regID[[R]][ match( L100.Pop_thous_ctry_Yh$iso, iso_GCAM_regID$iso ) ]
L101.Pop_thous_R_Yh <- aggregate( L100.Pop_thous_ctry_Yh[ c( X_Maddison_historical_years, X_UN_historical_years ) ],
      by=as.list( L100.Pop_thous_ctry_Yh[ R ] ), sum )

#Future population in the SSP scenarios
L100.Pop_thous_SSP_ctry_Yfut[[R]] <- iso_GCAM_regID[[R]][ match( L100.Pop_thous_SSP_ctry_Yfut$iso, iso_GCAM_regID$iso ) ]
L101.Pop_thous_SSP_R_Yfut <- aggregate( L100.Pop_thous_SSP_ctry_Yfut[ c( X_future_years ) ],
      by=as.list( L100.Pop_thous_SSP_ctry_Yfut[ Scen_R ] ), sum )

printlog( "Downscaling GCAM 3.0 population to country on the basis of UN historical data and base SSP in future years")
#Population by GCAM 3.0 region - downscale to country according to actual shares in the historical periods, and SSPbase in the future periods
L101.Pop_thous_ctry_Y <- L100.Pop_thous_ctry_Yh
L101.Pop_thous_ctry_Y[ X_future_years ] <- L100.Pop_thous_SSP_ctry_Yfut[
      match( paste( L101.Pop_thous_ctry_Y$iso, base_pop_scen ),
             paste( L100.Pop_thous_SSP_ctry_Yfut$iso, L100.Pop_thous_SSP_ctry_Yfut[[Scen]] ) ),
      X_future_years ]
L101.Pop_thous_ctry_Y$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( L101.Pop_thous_ctry_Y$iso, iso_GCAM_regID$iso ) ]
L101.Pop_thous_SSPbase_RG3_Y <- aggregate( L101.Pop_thous_ctry_Y[ c( X_historical_years, X_future_years ) ],
      by=as.list( L101.Pop_thous_ctry_Y[ "region_GCAM3"] ), sum )

#Calculate shares of each country within its region over the historical time series
L101.Popshares_ctryRG3_Y <- L101.Pop_thous_ctry_Y[ c( "iso", "region_GCAM3", X_historical_years, X_future_years ) ]
L101.Popshares_ctryRG3_Y[ c( X_historical_years, X_future_years ) ] <-
      L101.Pop_thous_ctry_Y[ c( X_historical_years, X_future_years ) ] / L101.Pop_thous_SSPbase_RG3_Y[
         match( L101.Pop_thous_ctry_Y$region_GCAM3, L101.Pop_thous_SSPbase_RG3_Y$region_GCAM3 ),
      c( X_historical_years, X_future_years ) ]

#Interpolate the GCAM population data to all historical and future years
L101.Pop_thous_GCAM3_RG3_Y <- gcam_interp( GCAM3_population, c( historical_years, future_years ) )[ c( "region_GCAM3", X_historical_years, X_future_years ) ]

if( "X2100" %in% X_future_years ){
	printlog( "Extending GCAM 3.0 scenario to 2100 using SSPbase population ratios by GCAM 3.0 region")
	L101.Pop_thous_GCAM3_RG3_Y$X2100 <- L101.Pop_thous_GCAM3_RG3_Y$X2095 *
	   L101.Pop_thous_SSPbase_RG3_Y$X2100[ match( L101.Pop_thous_GCAM3_RG3_Y$region_GCAM3, L101.Pop_thous_SSPbase_RG3_Y$region_GCAM3 ) ] /
	   L101.Pop_thous_SSPbase_RG3_Y$X2095[ match( L101.Pop_thous_GCAM3_RG3_Y$region_GCAM3, L101.Pop_thous_SSPbase_RG3_Y$region_GCAM3 ) ]
}

#Multiply these population numbers by the shares of each country within GCAM region
L101.Pop_thous_GCAM3_ctry_Y <- L101.Popshares_ctryRG3_Y
L101.Pop_thous_GCAM3_ctry_Y[ c( X_historical_years, X_future_years ) ] <- 
      L101.Popshares_ctryRG3_Y[ c( X_historical_years, X_future_years ) ] * L101.Pop_thous_GCAM3_RG3_Y[
         match( L101.Popshares_ctryRG3_Y$region_GCAM3, L101.Pop_thous_GCAM3_RG3_Y$region_GCAM3 ),
      c( X_historical_years, X_future_years ) ]

printlog( "Aggregating by GCAM regions")
L101.Pop_thous_GCAM3_ctry_Y[[R]] <- iso_GCAM_regID[[R]][ match( L101.Pop_thous_GCAM3_ctry_Y$iso, iso_GCAM_regID$iso ) ]
L101.Pop_thous_GCAM3_R_Y <- aggregate( L101.Pop_thous_GCAM3_ctry_Y[ c( X_historical_years, X_future_years ) ],
      by=as.list( L101.Pop_thous_GCAM3_ctry_Y[ R ] ), sum )
L101.Pop_thous_GCAM3_ctry_Y <- L101.Pop_thous_GCAM3_ctry_Y[ c( "iso", X_historical_years, X_future_years ) ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L101.Pop_thous_R_Yh <- c( "Population by region over the historical time period","Unit = million persons" )
comments.L101.Pop_thous_SSP_R_Yfut <- c( "Population by region and SSP in future periods","Unit = million persons" )
comments.L101.Pop_thous_GCAM3_R_Y <- c( "GCAM 3.0 population by region in historical and future years","Unit = thousand persons" )
comments.L101.Pop_thous_GCAM3_ctry_Y <- c( "GCAM 3.0 population by country in historical and future years","Unit = thousand persons" )

writedata( L101.Pop_thous_R_Yh, domain="SOCIO_LEVEL1_DATA", fn="L101.Pop_thous_R_Yh", comments=comments.L101.Pop_thous_R_Yh )
writedata( L101.Pop_thous_SSP_R_Yfut, domain="SOCIO_LEVEL1_DATA", fn="L101.Pop_thous_SSP_R_Yfut", comments=comments.L101.Pop_thous_SSP_R_Yfut )
writedata( L101.Pop_thous_GCAM3_R_Y, domain="SOCIO_LEVEL1_DATA", fn="L101.Pop_thous_GCAM3_R_Y", comments=comments.L101.Pop_thous_GCAM3_R_Y )
writedata( L101.Pop_thous_GCAM3_ctry_Y, domain="SOCIO_LEVEL1_DATA", fn="L101.Pop_thous_GCAM3_ctry_Y", comments=comments.L101.Pop_thous_GCAM3_ctry_Y )

# Every script should finish with this line
logstop()
