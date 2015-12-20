# Specify the location of the module from the data system root directory
MODULE_PROC_ROOT		<- SOCIOPROC_DIR

# -----------------------------------------------------------------------------
#downscale_Maddison_country: function to downscale the countries that separated into multiple modern countries (e.g. USSR).
downscale_Maddison_country <- function( data, country_name, iso_codes, available_year, years = Maddison_years ){
	X_available_year <- paste( "X", available_year, sep = "" )
	ctry_years <- years[ years < available_year ]
	X_ctry_years <- paste( "X", ctry_years, sep = "" )
	data_ratio <- subset( data, Country == country_name )
	data_ratio[ c( X_ctry_years, X_available_year ) ] <- data_ratio[ c( X_ctry_years, X_available_year ) ] / data_ratio[[ X_available_year ]]
	data_ratio <- data_ratio[ rep( 1, times = length( iso_codes ) ), ]
	data_downscaled <- subset( data, iso %in% iso_codes )
	data_downscaled[ X_ctry_years ] <- data_downscaled[[ X_available_year ]] * data_ratio[ X_ctry_years ]
	data[ data$iso %in% iso_codes, ] <- data_downscaled
	return( data )
}

