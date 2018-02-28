# Specify the location of the module from the data system root directory
MODULE_PROC_ROOT		<- GCAMUSAPROC_DIR

#functions
#_______________________________________

#apportion_to_states: a function for apportioning a nation-level quantity to the 50 states on the basis of the shares of each state
apportion_to_states <- function( nation_data, state_share_data, years = X_historical_years, match_vectors = NULL ){
	data_final <- state_share_data
	if( is.null( match_vectors ) ){
		data_final[ years ] <- state_share_data[ years ] *
		     nation_data[ rep( 1:nrow( nation_data ), length.out = nrow( state_share_data ) ), years ]
	}
	if( !is.null( match_vectors ) ){
		data_final[ years ] <- state_share_data[ years ] *
			nation_data[
	       	   match( vecpaste( state_share_data[ match_vectors ] ),
	                  vecpaste( nation_data[ match_vectors ] ) ),
	    	years ]
	}
	return( data_final )
}


#aggregate_recs: function for aggregating stuff in the RECS and CBECS databases by multiplying by sampling weights and adding the year
# note that the first step is converting from integers to numeric variables, as necessary; leaving variables as integers will cause overflow problems
aggregate_recs <- function( recs_data, variables, year, weight_variable = "NWEIGHT", region_variable = "subregion13", unit_conv = 1 ){
	recs_data[ variables ] <- apply( recs_data[ variables ], 2, as.numeric )
	data_new <- aggregate( recs_data[ variables ] * recs_data[[ weight_variable ]] * unit_conv,
	                       by=as.list( recs_data[ region_variable ] ), sum )
	data_new$year <- year
	return( data_new )
}

# -----------------------------------------------------------------------------
# write_to_all_states: write out data to all states
write_to_all_states <- function( data, names ){
	if ( "logit.year.fillout" %in% names ) data$logit.year.fillout <- "start-year"
	if ( "price.exp.year.fillout" %in% names ) data$price.exp.year.fillout <- "start-year"
	data_new <- set_years( data )
	data_new <- repeat_and_add_vector( data_new, "region", states_subregions$state )
	return( data_new[ names ] ) 
}

