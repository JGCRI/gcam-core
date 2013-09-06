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
logstart( "L102.GDP.R" )
adddep(paste(SOCIOPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(SOCIOPROC_DIR,"/../_common/headers/SOCIO_header.R",sep=""))
printlog( "Historical and future GDP by GCAM region" )

# -----------------------------------------------------------------------------
# 1. Read data

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "SOCIO_ASSUMPTIONS", "A_socioeconomics_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
SSP_GDP_PPP <- readdata( "SOCIO_LEVEL0_DATA", "SSP_GDP_PPP" )
GCAM3_GDP <- readdata( "SOCIO_LEVEL0_DATA", "GCAM3_GDP" )
L100.gdp_mil90usd_ctry_Yh <- readdata( "SOCIO_LEVEL1_DATA", "L100.gdp_mil90usd_ctry_Yh" )
L101.Pop_thous_R_Yh <- readdata( "SOCIO_LEVEL1_DATA", "L101.Pop_thous_R_Yh" )
L101.Pop_thous_SSP_R_Yfut <- readdata( "SOCIO_LEVEL1_DATA", "L101.Pop_thous_SSP_R_Yfut" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "NOTE: The SSPs do not include Taiwan. This needs to be added for the processing" )
printlog( "Taiwan will be assigned the same growth rate as China in future time periods" )
TWN_GDP_PPP <- data.frame(
      model = gdp_model,
      Scenario = unique( SSP_GDP_PPP$Scenario[ SSP_GDP_PPP$model == gdp_model ] ),
      Region = "TWN",
      Variable = unique( SSP_GDP_PPP$Variable), Unit = unique( SSP_GDP_PPP$Unit ) )
TWN_GDP_PPP[ X_historical_years[ X_historical_years %in% names( SSP_GDP_PPP ) ] ] <- L100.gdp_mil90usd_ctry_Yh[
      L100.gdp_mil90usd_ctry_Yh$iso == "twn",
      X_historical_years[ X_historical_years %in% names( SSP_GDP_PPP ) ] ] * conv_1990_2005_USD * conv_mil_bil

#NOTE: Not worrying about PPP:MER conversion factors here. This is crapola anyway and will be replaced when the SSP group adds Taiwan.
X_SSP_data_years <- names( SSP_GDP_PPP )[ grep( "X[0-9]{4}", names( SSP_GDP_PPP ) ) ]
X_SSP_data_years_base <- X_SSP_data_years[ X_SSP_data_years %in% names( TWN_GDP_PPP ) ]
X_SSP_data_years_fut <- X_SSP_data_years[ !X_SSP_data_years %in% names( TWN_GDP_PPP ) ]
X_SSP_data_years_fby <- X_SSP_data_years_base[ length( X_SSP_data_years_base ) ]
CHN_GDP_PPP <- subset( SSP_GDP_PPP, Region=="CHN" & model == gdp_model )
TWN_GDP_PPP[ X_SSP_data_years_fut ] <- TWN_GDP_PPP[[ X_SSP_data_years_fby ]] * CHN_GDP_PPP[ X_SSP_data_years_fut ] / CHN_GDP_PPP[[ X_SSP_data_years_fby ]]

#Rbind the Taiwan GDP scenarios back to the full database
SSP_GDP_PPP <- rbind( SSP_GDP_PPP, TWN_GDP_PPP )

printlog( "Aggregating historical and future GDP by GCAM region")
L100.gdp_mil90usd_ctry_Yh[[R]] <- iso_GCAM_regID[[R]][ match( L100.gdp_mil90usd_ctry_Yh$iso, iso_GCAM_regID$iso ) ]
L102.gdp_mil90usd_R_Yh <- aggregate( L100.gdp_mil90usd_ctry_Yh[ X_historical_years ], by=as.list( L100.gdp_mil90usd_ctry_Yh[ R ] ), sum )

#Future GDP in the SSP scenarios
printlog( "NOTE: this point in the processing method defines the maximum number of countries that may be regions in GCAM (182)" )
L102.gdp_bilusd_ctry_Yfut <- subset( SSP_GDP_PPP, model == gdp_model )
L102.gdp_bilusd_ctry_Yfut$iso <- tolower( L102.gdp_bilusd_ctry_Yfut$Region )

#Romania is called "rou" in the SSP database. reset this
L102.gdp_bilusd_ctry_Yfut$iso[ L102.gdp_bilusd_ctry_Yfut$iso == "rou" ] <- "rom"

#Map in the region and scenario
L102.gdp_bilusd_ctry_Yfut[[R]] <- iso_GCAM_regID[[R]][ match( L102.gdp_bilusd_ctry_Yfut$iso, iso_GCAM_regID$iso ) ]
L102.gdp_bilusd_ctry_Yfut$scenario <- substr( L102.gdp_bilusd_ctry_Yfut$Scenario, 1, 4 )

#Specify which years in the SSP databases to use
X_SSP_years <- names( L102.gdp_bilusd_ctry_Yfut )[ names( L102.gdp_bilusd_ctry_Yfut ) %in% c( X_historical_years, X_future_years ) ]

#Find any common years between the historical data and the "future" data, for computation of ratios
X_common_years <- X_SSP_years[ X_SSP_years %in% names( L102.gdp_mil90usd_R_Yh ) ]

#Use the most recent year as the transition point between the two databases
printlog( "NOTE: using historical data through final historical year, and ratios from final historical year in projections" )
X_base_year <- X_common_years[ length( X_common_years ) ]

L102.gdp_mil90usd_SSP_R_Yfut <- aggregate( L102.gdp_bilusd_ctry_Yfut[ X_SSP_years ], by=as.list( L102.gdp_bilusd_ctry_Yfut[ Scen_R ] ), sum )

#Calculate the GDP ratios from the first year in the projections. Use this ratio to project GDP from historical dataset in final historical period
L102.gdpRatio_SSP_R_Yfut <- L102.gdp_mil90usd_SSP_R_Yfut[ c( Scen_R, X_future_years ) ]
L102.gdpRatio_SSP_R_Yfut[ X_future_years ] <- L102.gdp_mil90usd_SSP_R_Yfut[ X_future_years ] /  L102.gdp_mil90usd_SSP_R_Yfut[[ X_base_year ]]

#Use these ratios to build the GDP trajectories by SSP
L102.gdp_mil90usd_SSP_R_Y <- repeat_and_add_vector( L102.gdp_mil90usd_R_Yh, Scen, unique( L102.gdpRatio_SSP_R_Yfut[[Scen]] ) )
L102.gdp_mil90usd_SSP_R_Y[X_future_years] <- L102.gdp_mil90usd_SSP_R_Y[[X_base_year]] * L102.gdpRatio_SSP_R_Yfut[
      match( vecpaste( L102.gdp_mil90usd_SSP_R_Y[ Scen_R ] ), vecpaste( L102.gdpRatio_SSP_R_Yfut[ Scen_R ] ) ),
      X_future_years ]
L102.gdp_mil90usd_SSP_R_Y <- L102.gdp_mil90usd_SSP_R_Y[ c( Scen_R, X_historical_years, X_future_years ) ]

printlog( "Calculating per-capita GDP by GCAM region and historical year and SSP scenario" )
#First, merge the population datasets (historical and future/scenario)
L102.Pop_thous_SSP_R_Y <- L101.Pop_thous_SSP_R_Yfut
L102.Pop_thous_SSP_R_Y[ X_historical_years ] <- L101.Pop_thous_R_Yh[
      match( L102.Pop_thous_SSP_R_Y[[R]], L101.Pop_thous_R_Yh[[R]] ),
      X_historical_years ]
L102.Pop_thous_SSP_R_Y <- L102.Pop_thous_SSP_R_Y[ c( Scen_R, X_historical_years, X_future_years ) ]

#Calculate per-capita GDP
L102.pcgdp_thous90USD_SSP_R_Y <- L102.gdp_mil90usd_SSP_R_Y
L102.pcgdp_thous90USD_SSP_R_Y[ c( X_historical_years, X_future_years ) ] <-
      L102.gdp_mil90usd_SSP_R_Y[ c( X_historical_years, X_future_years ) ] / L102.Pop_thous_SSP_R_Y[
         match( vecpaste( L102.pcgdp_thous90USD_SSP_R_Y[ Scen_R ] ), vecpaste( L102.Pop_thous_SSP_R_Y[ Scen_R ] ) ),
         c( X_historical_years, X_future_years ) ]
         
#GDP by GCAM region from GCAM 3.0 GDPs.
printlog( "Downscaling GCAM 3.0 GDP by GCAM 3.0 region to countries, using SSP2 GDP scenario" )
#GDP by GCAM 3.0 region - downscale to country according to actual shares in the historical periods, and SSPbase in the future periods
L102.gdp_mil90usd_ctry_Yh <- subset( L100.gdp_mil90usd_ctry_Yh, iso %in% L102.gdp_bilusd_ctry_Yfut$iso )
L102.gdp_mil90usd_ctry_Yh[ X_future_years ] <- L102.gdp_bilusd_ctry_Yfut[
      match( paste( L102.gdp_mil90usd_ctry_Yh$iso, base_pop_scen ),
             paste( L102.gdp_bilusd_ctry_Yfut$iso, L102.gdp_bilusd_ctry_Yfut[[Scen]] ) ),
      X_future_years ]
L102.gdp_mil90usd_ctry_Yh$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( L102.gdp_mil90usd_ctry_Yh$iso, iso_GCAM_regID$iso ) ]
L102.gdp_mil90usd_SSPbase_RG3_Y <- aggregate( L102.gdp_mil90usd_ctry_Yh[ c( X_historical_years, X_future_years ) ],
      by=as.list( L102.gdp_mil90usd_ctry_Yh[ "region_GCAM3"] ), sum )

#Calculate shares of each country within its region over the historical time series
L102.gdpshares_ctryRG3_Y <- L102.gdp_mil90usd_ctry_Yh[ c( "iso", "region_GCAM3", X_historical_years, X_future_years ) ]
L102.gdpshares_ctryRG3_Y[ c( X_historical_years, X_future_years ) ] <-
      L102.gdp_mil90usd_ctry_Yh[ c( X_historical_years, X_future_years ) ] / L102.gdp_mil90usd_SSPbase_RG3_Y[
         match( L102.gdp_mil90usd_ctry_Yh$region_GCAM3, L102.gdp_mil90usd_SSPbase_RG3_Y$region_GCAM3 ),
      c( X_historical_years, X_future_years ) ]

#Interpolate the GCAM population data to all historical and future years
L102.gdp_mil90usd_GCAM3_RG3_Y <- gcam_interp( GCAM3_GDP, c( historical_years, future_years ) )[ c( "region_GCAM3", X_historical_years, X_future_years ) ]

if( "X2100" %in% X_future_years ){
	printlog( "Extending GCAM 3.0 scenario to 2100 using SSPbase GDP ratios by GCAM 3.0 region")
	L102.gdp_mil90usd_GCAM3_RG3_Y$X2100 <- L102.gdp_mil90usd_GCAM3_RG3_Y$X2095 *
	   L102.gdp_mil90usd_SSPbase_RG3_Y$X2100[ match( L102.gdp_mil90usd_GCAM3_RG3_Y$region_GCAM3, L102.gdp_mil90usd_SSPbase_RG3_Y$region_GCAM3 ) ] /
	   L102.gdp_mil90usd_SSPbase_RG3_Y$X2095[ match( L102.gdp_mil90usd_GCAM3_RG3_Y$region_GCAM3, L102.gdp_mil90usd_SSPbase_RG3_Y$region_GCAM3 ) ]
	L102.gdp_mil90usd_GCAM3_RG3_Y <- gcam_interp( L102.gdp_mil90usd_GCAM3_RG3_Y, c( historical_years, future_years ) )
}
GCAM_GDP_years <- as.numeric( substr( names( GCAM3_GDP ), 2, 5 )[ grepl( "X[0-9]{4}", names( GCAM3_GDP ) ) ] )
X_GCAM_GDP_years <- paste0( "X", GCAM_GDP_years )
X_first_GCAM_GDP_year <- paste0( "X", min( GCAM_GDP_years ) )
X_first_historical_year <- X_historical_years[1]
if( min( historical_years ) < min( GCAM_GDP_years ) ){
	printlog( "Extending GCAM 3.0 scenario to first historical year using historical GDP ratios by GCAM 3.0 region")
	L102.gdp_mil90usd_GCAM3_RG3_Y[[X_first_historical_year]] <- L102.gdp_mil90usd_GCAM3_RG3_Y[[X_first_GCAM_GDP_year]] *
	   L102.gdp_mil90usd_SSPbase_RG3_Y[[X_first_historical_year]][ match( L102.gdp_mil90usd_GCAM3_RG3_Y$region_GCAM3, L102.gdp_mil90usd_SSPbase_RG3_Y$region_GCAM3 ) ] /
	   L102.gdp_mil90usd_SSPbase_RG3_Y[[X_first_GCAM_GDP_year]][ match( L102.gdp_mil90usd_GCAM3_RG3_Y$region_GCAM3, L102.gdp_mil90usd_SSPbase_RG3_Y$region_GCAM3 ) ]
	L102.gdp_mil90usd_GCAM3_RG3_Y <- gcam_interp( L102.gdp_mil90usd_GCAM3_RG3_Y, c( historical_years, future_years ) )
}

#Multiply these GDP numbers by the shares of each country within GCAM region
L102.gdp_mil90usd_GCAM3_ctry_Y <- L102.gdpshares_ctryRG3_Y
L102.gdp_mil90usd_GCAM3_ctry_Y[ c( X_historical_years, X_future_years ) ] <- 
      L102.gdpshares_ctryRG3_Y[ c( X_historical_years, X_future_years ) ] * L102.gdp_mil90usd_GCAM3_RG3_Y[
         match( L102.gdpshares_ctryRG3_Y$region_GCAM3, L102.gdp_mil90usd_GCAM3_RG3_Y$region_GCAM3 ),
      c( X_historical_years, X_future_years ) ]

printlog( "Aggregating by GCAM regions")
L102.gdp_mil90usd_GCAM3_ctry_Y[[R]] <- iso_GCAM_regID[[R]][ match( L102.gdp_mil90usd_GCAM3_ctry_Y$iso, iso_GCAM_regID$iso ) ]
L102.gdp_mil90usd_GCAM3_R_Y <- aggregate( L102.gdp_mil90usd_GCAM3_ctry_Y[ c( X_historical_years, X_future_years ) ],
      by=as.list( L102.gdp_mil90usd_GCAM3_ctry_Y[ R ] ), sum )
L102.gdp_mil90usd_GCAM3_ctry_Y <- L102.gdp_mil90usd_GCAM3_ctry_Y[ c( "iso", X_historical_years, X_future_years ) ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L102.gdp_mil90usd_SSP_R_Y <- c( "GDP by SSP scenario and GCAM region (including historical time series)","Unit = billion 2005 USD" )
comments.L102.pcgdp_thous90USD_SSP_R_Y <- c( "per-capita GDP by SSP scenario and GCAM region (including historical time series)","Unit = 2005 USD" )
comments.L102.gdp_mil90usd_GCAM3_R_Y <- c( "Total GDP from GCAM 3.0 by GCAM region (including historical time series)","Unit = 1990 USD" )
comments.L102.gdp_mil90usd_GCAM3_ctry_Y <- c( "Total GDP from GCAM 3.0 by country (including historical time series)","Unit = 1990 USD" )

writedata( L102.gdp_mil90usd_SSP_R_Y, domain="SOCIO_LEVEL1_DATA", fn="L102.gdp_mil90usd_SSP_R_Y", comments=comments.L102.gdp_mil90usd_SSP_R_Y )
writedata( L102.pcgdp_thous90USD_SSP_R_Y, domain="SOCIO_LEVEL1_DATA", fn="L102.pcgdp_thous90USD_SSP_R_Y", comments=comments.L102.pcgdp_thous90USD_SSP_R_Y )
writedata( L102.gdp_mil90usd_GCAM3_R_Y, domain="SOCIO_LEVEL1_DATA", fn="L102.gdp_mil90usd_GCAM3_R_Y", comments=comments.L102.gdp_mil90usd_GCAM3_R_Y )
writedata( L102.gdp_mil90usd_GCAM3_ctry_Y, domain="SOCIO_LEVEL1_DATA", fn="L102.gdp_mil90usd_GCAM3_ctry_Y", comments=comments.L102.gdp_mil90usd_GCAM3_ctry_Y )

# Every script should finish with this line
logstop()
