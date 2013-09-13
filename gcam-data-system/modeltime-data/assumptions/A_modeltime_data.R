#---------------------
# Model base years
model_base_years <- c( 1971:2010 )
X_model_base_years <- paste0( "X", model_base_years )

# Model future years
model_future_years <- seq( 2015, 2100, 5 )
X_model_future_years <- paste0( "X", model_future_years )

#All model years
model_years <- c( model_base_years, model_future_years )
X_model_years <- paste0( "X", model_years )


#------------------------------------------------------------------------------------
#Climate model assumptions
Magicc_last_historical_year <- 2005
Magicc_bc_unit_forcing <- 0
Magicc_C_start_year <- 1705