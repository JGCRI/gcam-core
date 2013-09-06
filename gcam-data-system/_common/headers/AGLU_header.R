# Specify the location of the module from the data system root directory
MODULE_PROC_ROOT		<- AGLUPROC_DIR

# -----------------------------------------------------------------------------
# rename_biocrops: a function for changing the names of "biomass" in selected region/AEZs
rename_biocrops <- function( data, lookup, data_matchvar, lookup_matchvar, data_var1, data_var2=NA, data_var3 = NA ){
     data_new <- data
     data_new$ID <- paste( data_new$region, data_new[[data_matchvar]] )
     lookup$ID <- paste( lookup$region, lookup[[lookup_matchvar]] )
     data_new[ data_new$ID %in% lookup$ID, data_var1 ] <- lookup[
      match( data_new$ID[ data_new$ID %in% lookup$ID ], lookup$ID ),
      data_var1 ]
     if( !is.na( data_var2 ) ) {
        data_new[ data_new$ID %in% lookup$ID, data_var2 ] <- lookup[
      match( data_new$ID[ data_new$ID %in% lookup$ID ], lookup$ID ),
      data_var2 ] }
     if( !is.na( data_var3 ) ) {
        data_new[ data_new$ID %in% lookup$ID, data_var3 ] <- lookup[
      match( data_new$ID[ data_new$ID %in% lookup$ID ], lookup$ID ),
      data_var3 ] }
     data_new <- data_new[ names( data_new ) != "ID" ]   
     return (data_new )
     }            

# -----------------------------------------------------------------------------
#downscale_FAO_country: function to downscale the countries that separated into multiple modern countries (e.g. USSR).
downscale_FAO_country <- function( data, country_name, dissolution_year, item_name = "item", element_name = "element" ){
	X_dissolution_year <- paste( "X", dissolution_year, sep = "" )
	ctry_years <- AGLU_historical_years[ AGLU_historical_years < dissolution_year ]
	X_ctry_years <- paste( "X", ctry_years, sep = "" )
	data_ratio <- aggregate( data[ c( X_ctry_years, X_dissolution_year ) ], by=as.list( data[ c( item_name, element_name ) ] ), sum )
	data_ratio[ c( X_ctry_years, X_dissolution_year ) ] <- data_ratio[ c( X_ctry_years, X_dissolution_year ) ] / data_ratio[[ X_dissolution_year ]]
	data_new <- subset( data, countries != country_name )
	data_new[ X_ctry_years ] <- data_new[[ X_dissolution_year ]] * data_ratio[
	   match( vecpaste( data_new[ c( item_name, element_name ) ] ), vecpaste( data_ratio[ c( item_name, element_name ) ] ) ), X_ctry_years ]
	data_new[ X_ctry_years ][ is.na( data_new[ X_ctry_years ] ) ] <- 0
	return( data_new )
}

#interpolate_IMAGE_years: function to interpolate IMAGE tables to all historical years
interpolate_IMAGE_years <- function( data, idvars, years = AGLU_historical_years ){
	data.melt <- melt( data, id.vars = idvars )
	data.melt$IMAGE_region_ID <- as.numeric( substr( as.character( data.melt$variable ), 2, nchar( as.character( data.melt$variable ) ) ) )
	data.melt$year <- paste( "X", data.melt$year, sep = "" )
	data_new <- cast( data.melt, IMAGE_region_ID + ... ~ year )
	data_new <- data_new[ names( data_new) != "variable" ]
	data_new$X1960 <- data_new$X1970
	data_new <- gcam_interp( data_new, years )
	return( data_new )
}

#downscale_IMAGE_regions: function to downscale the IMAGE regions to all countries( iso)
downscale_IMAGE_regions <- function( data, idvars, years = X_AGLU_historical_years ){
	data_new <- data[ data$IMAGE_region_ID==1, !names( data ) %in% years ]
	data_new <- repeat_and_add_vector( data_new, "iso", sort( unique( AGLU_ctry$iso ) ) )
	data_new$IMAGE_region_ID <- AGLU_ctry$IMAGE_region_ID[ match( data_new$iso, AGLU_ctry$iso ) ]
	data_new[ years ] <- data[
	      match( vecpaste( data_new[ idvars ] ),
	             vecpaste( data[ idvars ] ) ),
	      years ]
	data_new <- na.omit( data_new )
	data_new <- data_new[ c("iso", idvars, years ) ]
}

# -----------------------------------------------------------------------------
# write_to_all_regions_ag: write out ag table to all regions
write_to_all_regions_ag <- function( data, names ){
	data_new <- set_years( data )
	data_new <- repeat_and_add_vector( data_new, "GCAM_region_ID", GCAM_region_names$GCAM_region_ID )
	data_new <- add_region_name( data_new )
	if ("market.name" %in% names ) data_new$market.name <- data_new$region
	return( data_new[ names ] ) 
}

#remove non-existent AEZs
remove_AEZ_nonexist <- function( data, AEZcol = "AgSupplySubsector", AEZnonexist = L125.R_AEZ_nonexist ){
	AEZnonexist <- add_region_name( AEZnonexist )
	data[[AEZ]] <- substr( as.character( data[[AEZcol]] ), nchar( as.character( data[[AEZcol]] ) ) - 4, nchar( as.character( data[[AEZcol]] ) ) )
	data <- data[ !vecpaste( data[ c( reg, AEZ ) ] ) %in% vecpaste( AEZnonexist[ c( reg, AEZ ) ] ), ]
	data <- data[ names( data ) != AEZ ]
}

#add_agtech_names: function to use the commodity and AEZ names to create agsupplysectors, subsectors, technologies
add_agtech_names <- function( data ){
	data[[agsupp]] <- data[[C]]
	data[[agsubs]] <- paste( data[[C]], data[[AEZ]], sep = AEZ_delimiter )
	data[[agtech]] <- paste( data[[C]], data[[AEZ]], sep = AEZ_delimiter )
	return( data )
}

#append_AEZ: function to append AEZ to all specified variables
append_AEZ <- function( data, var1 = "LandNode1", var2 = NA, var3 = NA, var4 = NA, var5 = NA ){
	data[[var1]] <- paste( data[[var1]], data[[AEZ]], sep = AEZ_delimiter )
	if( !is.na( var2 ) ){ data[[var2]] <- paste( data[[var2]], data[[AEZ]], sep = AEZ_delimiter ) }
	if( !is.na( var3 ) ){ data[[var3]] <- paste( data[[var3]], data[[AEZ]], sep = AEZ_delimiter ) }
	if( !is.na( var4 ) ){ data[[var4]] <- paste( data[[var4]], data[[AEZ]], sep = AEZ_delimiter ) }
	if( !is.na( var5 ) ){ data[[var5]] <- paste( data[[var5]], data[[AEZ]], sep = AEZ_delimiter ) }
	return( data )
}

#add_node_leaf_names: function to match in the node and leaf names from a land nesting table
add_node_leaf_names <- function( data, nesting_table, leaf_name, LT_name = LT, LN1 = "LandNode1", LN2 = NA, LN3 = NA, LN4 = NA, append_AEZ = T ){
	data$LandAllocatorRoot <- "root"
	data[[LN1]] <- nesting_table[[LN1]][ match( data[[LT_name]], nesting_table[[leaf_name]] ) ]
	if( !is.na( LN2 ) ){ data[[LN2]] <- nesting_table[[LN2]][ match( data[[LT_name]], nesting_table[[leaf_name]] ) ] }
	if( !is.na( LN3 ) ){ data[[LN3]] <- nesting_table[[LN3]][ match( data[[LT_name]], nesting_table[[leaf_name]] ) ] }
	if( !is.na( LN4 ) ){ data[[LN4]] <- nesting_table[[LN4]][ match( data[[LT_name]], nesting_table[[leaf_name]] ) ] }
	data[[leaf_name]] <- data[[LT_name]]
	if( append_AEZ == T ){ data <- append_AEZ( data, var1 = leaf_name, var2 = LN1, var3 = LN2, var4 = LN3, var5 = LN4 ) }
	return( data )
}

#add_carbon_info: function to add in the carbon densities and mature ages from specified tables, matching on specified parameters
add_carbon_info <- function( data, veg_data, soil_data, age_data, LT_name = "Cdensity_LT" ){
	data$hist.veg.carbon.density <- veg_data$value[
      match( vecpaste( data[ c( "region", LT_name, AEZ ) ] ),
             vecpaste( veg_data[ c( "region", LT, AEZ ) ] ) ) ]
	data$hist.soil.carbon.density <- soil_data$value[
      match( vecpaste( data[ c( "region", LT_name, AEZ ) ] ),
             vecpaste( soil_data[ c( "region", LT, AEZ ) ] ) ) ]
	data$mature.age <- age_data$value[
      match( vecpaste( data[ c( "region", LT_name, AEZ ) ] ),
             vecpaste( age_data[ c( "region", LT, AEZ ) ] ) ) ]
	data$veg.carbon.density <- data$hist.veg.carbon.density
	data$soil.carbon.density <- data$hist.soil.carbon.density
	data$min.veg.carbon.density <- min.veg.carbon.density
	data$min.soil.carbon.density <- min.soil.carbon.density
	return( data )
}





