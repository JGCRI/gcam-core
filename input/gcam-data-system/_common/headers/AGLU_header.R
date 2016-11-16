# Specify the location of the module from the data system root directory
MODULE_PROC_ROOT		<- AGLUPROC_DIR

# -----------------------------------------------------------------------------
# rename_biocrops: a function for changing the names of "biomass" in selected region/AEZs
rename_biocrops <- function( data, lookup, data_matchvar, lookup_matchvar, data_var1, data_var2=NA, data_var3 = NA, data_var4 = NA ){
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
     if( !is.na( data_var4 ) ) {
        data_new[ data_new$ID %in% lookup$ID, data_var4 ] <- lookup[
      match( data_new$ID[ data_new$ID %in% lookup$ID ], lookup$ID ),
      data_var4 ] }
     data_new <- data_new[ names( data_new ) != "ID" ]   
     data_new <- data_new[ data_new[[ data_matchvar ]] != "delete", ]
     return (data_new )
     }            

# -----------------------------------------------------------------------------
#downscale_FAO_country: function to downscale the countries that separated into multiple modern countries (e.g. USSR).
downscale_FAO_country <- function( data, country_name, dissolution_year, item_name = "item",
                                   element_name = "element", years = AGLU_historical_years ){
	X_dissolution_year <- paste( "X", dissolution_year, sep = "" )
	ctry_years <- years[ years < dissolution_year ]
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
	data_new <- dcast( data.melt, IMAGE_region_ID + ... ~ year )
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
#append_GLU: function to append GLU to all specified variables
append_GLU <- function( data, var1 = "LandNode1", var2 = NA, var3 = NA, var4 = NA, var5 = NA ){
	data[[var1]] <- paste( data[[var1]], data[[GLU]], sep = LT_GLU_delimiter )
	if( !is.na( var2 ) ){ data[[var2]] <- paste( data[[var2]], data[[GLU]], sep = LT_GLU_delimiter ) }
	if( !is.na( var3 ) ){ data[[var3]] <- paste( data[[var3]], data[[GLU]], sep = LT_GLU_delimiter ) }
	if( !is.na( var4 ) ){ data[[var4]] <- paste( data[[var4]], data[[GLU]], sep = LT_GLU_delimiter ) }
	if( !is.na( var5 ) ){ data[[var5]] <- paste( data[[var5]], data[[GLU]], sep = LT_GLU_delimiter ) }
	return( data )
}

#substring_GLU: get the GLU from an appended column
substring_GLU <- function( data, from.var ){
  GLU_nchar <- sum( nchar( GLU_name_delimiter ), nchar( GLU ), GLU_ndigits, nchar( crop_GLU_delimiter ) )
  data[[GLU]] <- substr( data[[from.var]], nchar( data[[from.var]] ) - ( GLU_nchar - 1 - nchar( crop_GLU_delimiter ) ), nchar( data[[from.var]] ) )
  return( data )
}

#remove_GLU: function to remove the GLU name from all specified variables
remove_GLU <- function( data, var1 = "LandNode1", var2 = NA, var3 = NA, var4 = NA, var5 = NA ){
  GLU_nchar <- sum( nchar( GLU_name_delimiter ), nchar( GLU ), GLU_ndigits, nchar( crop_GLU_delimiter ) )
  data <- substring_GLU( data, from.var = var1 )
  data[[var1]] <- substr( data[[var1]], 1, nchar( data[[var1]] ) - GLU_nchar )
  if( !is.na( var2 ) ) data[[var2]] <- substr( data[[var2]], 1, nchar( data[[var2]] ) - GLU_nchar )
  if( !is.na( var3 ) ) data[[var3]] <- substr( data[[var3]], 1, nchar( data[[var3]] ) - GLU_nchar )
  if( !is.na( var4 ) ) data[[var4]] <- substr( data[[var4]], 1, nchar( data[[var4]] ) - GLU_nchar )
  if( !is.na( var5 ) ) data[[var5]] <- substr( data[[var5]], 1, nchar( data[[var5]] ) - GLU_nchar )
  return( data )
}

substring_irr <- function( data, from.var, to.lower = F ){
  data[[irr]] <- substr( data[[from.var]], nchar( data[[from.var]] ) - 2, nchar( data[[from.var]] ) )
  if( to.lower ) data[[irr]] <- tolower( data[[irr]] )
  return( data )
}

#add_node_leaf_names: function to match in the node and leaf names from a land nesting table
add_node_leaf_names <- function( data, nesting_table, leaf_name, LT_name = LT, LN1 = "LandNode1", LN2 = NA, LN3 = NA, LN4 = NA, append_GLU = T ){
	data$LandAllocatorRoot <- "root"
	data[[LN1]] <- nesting_table[[LN1]][ match( data[[LT_name]], nesting_table[[leaf_name]] ) ]
	if( !is.na( LN2 ) ){ data[[LN2]] <- nesting_table[[LN2]][ match( data[[LT_name]], nesting_table[[leaf_name]] ) ] }
	if( !is.na( LN3 ) ){ data[[LN3]] <- nesting_table[[LN3]][ match( data[[LT_name]], nesting_table[[leaf_name]] ) ] }
	if( !is.na( LN4 ) ){ data[[LN4]] <- nesting_table[[LN4]][ match( data[[LT_name]], nesting_table[[leaf_name]] ) ] }
	data[[leaf_name]] <- data[[LT_name]]
	if( append_GLU == T ){ data <- append_GLU( data, var1 = leaf_name, var2 = LN1, var3 = LN2, var4 = LN3, var5 = LN4 ) }
	if( irr %in% names( data ) ) { data[[leaf_name]] <- paste( data[[leaf_name]], data[[irr]], sep = irr_delimiter ) }
	return( data )
}

#add_carbon_info: function to translate from soil, veg, and mature age data (already in a table) to the required read-in model parameters
add_carbon_info <- function( data, carbon_info_table, carbon_info_table_names = c( "veg_c", "soil_c", "mature.age" ),
                             data_matchvars = c( R_GLU, "Cdensity_LT" ), carbon_info_matchvars = c( R_GLU, LT ) ){
	data[ c( "hist.veg.carbon.density", "hist.soil.carbon.density", "mature.age" ) ] <-
	  carbon_info_table[ match( vecpaste( data[ data_matchvars ] ),
	                            vecpaste( carbon_info_table[ carbon_info_matchvars ] ) ),
	                     carbon_info_table_names ]
	data[ c( "hist.veg.carbon.density", "hist.soil.carbon.density" ) ] <- round(
	  data[ c( "hist.veg.carbon.density", "hist.soil.carbon.density" ) ], digits_C_density )
	data$mature.age <- round( data$mature.age, digits_MatureAge )
	data$mature.age.year.fillout <- min( model_base_years )
	data$veg.carbon.density <- data$hist.veg.carbon.density
	data$soil.carbon.density <- data$hist.soil.carbon.density
	data$min.veg.carbon.density <- min.veg.carbon.density
	data$min.soil.carbon.density <- min.soil.carbon.density
	return( data )
}

remove_zero_output_land_leafs <- function( land, prod ) {
  prod <- subset( prod, calOutputValue > 0 )
  land$id <- paste( land$region, land$LandLeaf, land$year )
  prod$id <- paste( prod$region, prod$AgProductionTechnology, prod$year )
  land <- subset( land, id %in% unique( prod$id ) )
  land$id <- NULL
  
  return( land )
}