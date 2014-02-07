
#Population years - note that these sequences shouldn't have any overlap and should contain all historical years used by other modules
#Years for which to use Maddison data
Maddison_historical_years <- seq( 1700, 1900, 50 )
X_Maddison_historical_years <- paste0( "X", Maddison_historical_years )

#Years for which to use UN data
UN_historical_years <- c( 1950, 1971:2010 )
X_UN_historical_years <- paste0( "X", UN_historical_years )

pop_model <- "IIASA-WiC POP"
gdp_model <- "OECD Env-Growth"
base_pop_scen <- "SSP2"
base_gdp_scen <- "SSP2"

default_interest.rate <- 0.05
default_laborforce <- 0.5

digits_Pop <- 0
digits_LaborProductivity <- 5
digits_IncElas_ind <- 3
