#---------------------
# Model base years
model_base_years <- c( 1975, 1990, 2005, 2010 )
X_model_base_years <- paste0( "X", model_base_years )
final_model_base_year <- max( model_base_years )
X_final_model_base_year <- paste0( "X", final_model_base_year )

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
