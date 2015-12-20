# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "GCAMUSAPROC_DIR" ) ){
    if( Sys.getenv( "GCAMUSAPROC" ) != "" ){
        GCAMUSAPROC_DIR <- Sys.getenv( "GCAMUSAPROC" )
    } else {
        stop("Could not determine location of energy data system.please set the R var GCAMUSAPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
logstart( "LA100.Socioeconomics.R" )
printlog( "Historical GDP and per-capita GDP by state" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
BEA_pcGDP_09USD_state <- readdata( "GCAMUSA_LEVEL0_DATA", "BEA_pcGDP_09USD_state" )
BEA_pcGDP_97USD_state <- readdata( "GCAMUSA_LEVEL0_DATA", "BEA_pcGDP_97USD_state" )
Census_pop_hist <- readdata( "GCAMUSA_LEVEL0_DATA", "Census_pop_hist" )
PRIMA_pop <- readdata( "GCAMUSA_LEVEL0_DATA", "PRIMA_pop" )
L100.gdp_mil90usd_ctry_Yh <- readdata( "SOCIO_LEVEL1_DATA", "L100.gdp_mil90usd_ctry_Yh" )

# -----------------------------------------------------------------------------

# 2.perform computations
# Bind the two per-capita GDP data frames to get a continuous time series, and extrapolate
printlog( "Building historical per-capita GDP time series" )
printlog( "NOTE: only using these datasets to disaggregate national GDP totals, so no need to convert units or" )
printlog( "estimate what the actual per-capita GDP trends were in the pre-1987 years that have all missing values")
GDP97_years <- as.numeric( substr( names( BEA_pcGDP_97USD_state )[ names( BEA_pcGDP_97USD_state ) %in% X_historical_years ], 2, 5 ) )
BEA_pcGDP_97USD_state <- gcam_interp( BEA_pcGDP_97USD_state, GDP97_years, rule = 2 )
BEA_pcGDP_97USD_state$state <- states_subregions$state[ match( BEA_pcGDP_97USD_state$Area, states_subregions$state_name ) ]
L100.pcGDP_state_unscaled <- data.frame(
      BEA_pcGDP_97USD_state[ state ],
      BEA_pcGDP_97USD_state[ names( BEA_pcGDP_97USD_state  ) %in% X_historical_years & !names( BEA_pcGDP_97USD_state ) %in% names( BEA_pcGDP_09USD_state ) ],
      BEA_pcGDP_09USD_state[ names( BEA_pcGDP_09USD_state  ) %in% X_historical_years ] )
L100.GDP_state_unscaled <- L100.pcGDP_state_unscaled
L100.GDP_state_unscaled[ X_historical_years ] <- L100.pcGDP_state_unscaled[ X_historical_years ] * 1e-6 *    #avoiding "integer overflow" by dividing by a million
      Census_pop_hist[ match( L100.GDP_state_unscaled$state, Census_pop_hist$state ),
      X_historical_years ]
L100.GDPshare_state <- L100.GDP_state_unscaled
L100.GDPshare_state[ X_historical_years ] <- sweep( L100.GDP_state_unscaled[ X_historical_years ],
      2, colSums( L100.GDP_state_unscaled[ X_historical_years ] ), "/" )

#Multiply the country-level GDP by the state shares
L100.GDP_mil90usd_state <- apportion_to_states(
      nation_data = subset( L100.gdp_mil90usd_ctry_Yh, iso == "usa" ),
      state_share_data = L100.GDPshare_state )
L100.pcGDP_thous90usd_state <- L100.GDP_mil90usd_state
L100.pcGDP_thous90usd_state[ X_historical_years ] <-
      L100.GDP_mil90usd_state[ X_historical_years ] * conv_mil_thous / Census_pop_hist[
      match( L100.GDP_mil90usd_state$state, Census_pop_hist$state ),
      X_historical_years ]

#Future population by scenario. Right now just one scenario.
L100.Pop_ratio_state <- gcam_interp( PRIMA_pop, c( final_historical_year, future_years ) )
L100.Pop_ratio_state[ c( X_final_historical_year, X_future_years ) ] <-
      L100.Pop_ratio_state[ c( X_final_historical_year, X_future_years ) ] / 
      L100.Pop_ratio_state[[ X_final_historical_year ]]
L100.Pop_ratio_state$state <- states_subregions$state[ match( L100.Pop_ratio_state$state, states_subregions$state_name ) ]
L100.Pop_thous_state <- data.frame(
      Census_pop_hist[ "state" ],
      Census_pop_hist[ X_historical_years ] * conv_ones_thous )
L100.Pop_thous_state[ X_future_years ] <- L100.Pop_thous_state[[ X_final_historical_year ]] *
      L100.Pop_ratio_state[ match( L100.Pop_thous_state$state, L100.Pop_ratio_state$state ),
      X_future_years ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L100.pcGDP_thous90usd_state <- c( "Per-capita GDP by state","Unit = thousand 1990 USD per capita" )
comments.L100.GDP_mil90usd_state <- c( "GDP by state","Unit = million 1990 USD" )
comments.L100.Pop_thous_state <- c( "Population by state","Unit = thousand persons" )

#write tables as CSV files
writedata( L100.pcGDP_thous90usd_state, domain="GCAMUSA_LEVEL1_DATA", fn="L100.pcGDP_thous90usd_state", comments=comments.L100.pcGDP_thous90usd_state )
writedata( L100.GDP_mil90usd_state, domain="GCAMUSA_LEVEL1_DATA", fn="L100.GDP_mil90usd_state", comments=comments.L100.GDP_mil90usd_state )
writedata( L100.Pop_thous_state, domain="GCAMUSA_LEVEL1_DATA", fn="L100.Pop_thous_state", comments=comments.L100.Pop_thous_state )

# Every script should finish with this line
logstop()
