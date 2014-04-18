# Specify the location of the module from the data system root directory
MODULE_PROC_ROOT		<- EMISSPROC_DIR

# -----------------------------------------------------------------------------
# write_to_all_regions: write out data to all regions, and set traded names as required
write_to_all_regions <- function( data, names, has.traded=F, apply.to = "selected", set.market = F ){
	if ( "logit.year.fillout" %in% names ) data$logit.year.fillout <- "start-year"
	if ( "price.exp.year.fillout" %in% names ) data$price.exp.year.fillout <- "start-year"
	data_new <- set_years( data )
	data_new <- repeat_and_add_vector( data_new, "GCAM_region_ID", GCAM_region_names$GCAM_region_ID )
	data_new <- add_region_name( data_new )
	if( has.traded==T){
		if( set.market==T){
			data_new$market.name <- data_new$region
		}
		data_new <- set_traded_names( data_new, apply.to )
		}
	return( data_new[ names ] ) 
}

# -----------------------------------------------------------------------------
#remove non-existent AEZs
remove_AEZ_nonexist <- function( data, AEZcol = "AgSupplySubsector", AEZnonexist = L125.R_AEZ_nonexist ){
  AEZnonexist <- add_region_name( AEZnonexist )
  data[[AEZ]] <- substr( as.character( data[[AEZcol]] ), nchar( as.character( data[[AEZcol]] ) ) - 4, nchar( as.character( data[[AEZcol]] ) ) )
  data <- data[ !vecpaste( data[ c( reg, AEZ ) ] ) %in% vecpaste( AEZnonexist[ c( reg, AEZ ) ] ), ]
  data <- data[ names( data ) != AEZ ]
}

# -----------------------------------------------------------------------------
#rename SO2 to regional SO2
rename_SO2 <- function( data, so2_map, is.awb = FALSE ){
  data_so2 <- subset( data, data$Non.CO2 == "SO2" )
  data_notso2 <- subset( data, data$Non.CO2 != "SO2" )
  if ( is.awb ) {
    data_so2 <- subset( data, data$Non.CO2 == "SO2_AWB" )
    data_notso2 <- subset( data, data$Non.CO2 != "SO2_AWB" )  
  }
  
  if ( is.awb ) {
    so2_map$SO2_name <- paste( so2_map$SO2_name, "_AWB", sep="" )
  }
  data_so2$Non.CO2 <- so2_map$SO2_name[ match( data_so2$region, so2_map$region )]
  
  data_new <- rbind( data_so2, data_notso2 )  
  return (data_new )
}            

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
