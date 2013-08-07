# Specify the location of the module from the data system root directory
MODULE_PROC_ROOT		<- ENERGYPROC_DIR

# -----------------------------------------------------------------------------
# convert_rsrc_to_L2: convert a table with resource information to level 2 (csv to be read by model interface)
# NOTE: this formula expects the data to be written in a certain format
convert_rsrc_to_L2 <- function( data, resource_type, subresource_type=NA ) {
	if( resource_type == "depresource" ) {
		data_new <- data.frame(
		region = data$region,
		depresource = data$resource,
		subresource = data$subresource,
		grade = data$grade,
		available = round( data$available, digits_depresource ),
		extractioncost = data$extractioncost )
	}
	if( resource_type == "renewresource" & subresource_type == "smooth-renewable-subresource") {
		data_new <- data.frame(
		region = data$region,
		renewresource = data$resource,
		smooth.renewable.subresource = data$subresource,
		maxSubResource = round( data$maxSubResource, digits_maxSubResource ),
		mid.price = round( data$mid.price, digits_mid.price ),
		curve.exponent = round( data$curve.exponent, digits_curve.exponent ) )
	}
	if( resource_type == "renewresource" & subresource_type == "sub-renewable-resource") {
		data_new <- data.frame(
		region = data$region,
		renewresource = data$resource,
		sub.renewable.resource = data$subresource,
		grade = data$grade,
		available = round( data$available, digits_maxSubResource ),
		extractioncost = data$extractioncost )
	}
	if( !resource_type %in% c( "depresource", "renewresource" ) ) {
		stop( "Resource type specified is not one of the available options")
	}
	if( resource_type == "renewresource" & !subresource_type %in% c( "smooth-renewable-subresource", "sub-renewable-resource" ) ) {
		stop( "Subresource type specified is not one of the available options for renewresource" )
	}
	return( data_new )
	}
	
# -----------------------------------------------------------------------------
# set_traded_names: convert names of traded secondary goods to be contained within region 1, with region appended to subsector and tech names
set_traded_names <- function( data, apply.to="selected" ) {
	data_new <- data
	if( apply.to=="selected" ){
		if( "subsector" %in% names( data ) ){
			data_new$subsector[data$traded == 1] <- paste( data$region[data$traded == 1], data$subsector[data$traded == 1], sep = " " )
		} 
		if( "technology" %in% names( data ) ){
			data_new$technology[data$traded == 1] <- paste( data$region[data$traded == 1], data$technology[data$traded == 1], sep = " " )
		} 
		data_new$region[data$traded == 1] <- GCAM_region_names$region[1]
		return( data_new )
	}
	if(apply.to=="all" ){
		if( "subsector" %in% names( data ) ){
			data_new$subsector <- paste( data$region, data$subsector, sep = " " )
		} 
		if( "technology" %in% names( data ) ){
			data_new$technology <- paste( data$region, data$technology, sep = " " )
		} 
		data_new$region <- GCAM_region_names$region[1]
		return( data_new )
	}
}
	
# -----------------------------------------------------------------------------
# write_to_all_regions: write out data to all regions, and set traded names as required
write_to_all_regions <- function( data, names, has.traded=F, apply.to = "selected", set.market = F ){
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
# subset_inttechs: subset intermittent technologies into a new dataframe and change the tech column name
subset_inttechs <- function( data, inttech.table, sector.name= "supplysector", subsector.name="subsector" ){
	data_new <- subset( data, paste( sector.name, subsector.name, technology ) %in%
	            paste( inttech.table$supplysector, inttech.table$subsector, inttech.table$technology ) )
	names( data_new )[ names( data_new ) == "technology" ] <- "intermittent.technology"
	return( data_new ) 
}
	
# -----------------------------------------------------------------------------
# subset_techs: subset non-intermittent technologies into a new dataframe
subset_techs <- function( data, inttech.table, sector.name= "supplysector", subsector.name="subsector" ){
	data_new <- subset( data, paste( sector.name, subsector.name, technology ) %!in%
	            paste( inttech.table$supplysector, inttech.table$subsector, inttech.table$technology ) )
	return( data_new ) 
}

# -----------------------------------------------------------------------------
# set_subsector_shrwt: calculate subsector shareweights in calibration periods, where subsectors may have multiple technologies
set_subsector_shrwt <- function( data,
  value.name="calOutputValue", region.name="region", sector.name="supplysector", subsector.name="subsector", year.name="year",
  result.column.name="subs.share.weight" ){
	data_aggregated <- aggregate( data[value.name],
	      by=list( region = data[[region.name]], sector = data[[sector.name]], subsector = data[[subsector.name]], year = data[[year.name]] ),
	      FUN=sum )
	data_new <- data
	data_new[[result.column.name]] <- ifelse( data_aggregated[[value.name]][
	      match( paste( data_new[[region.name]], data_new[[sector.name]], data_new[[subsector.name]], data_new[[year.name]] ),
	             paste( data_aggregated$region, data_aggregated$sector, data_aggregated$subsector, data_aggregated$year ) ) ] > 0, 1, 0 )
	return( data_new ) 	
}

# -----------------------------------------------------------------------------
# derive_passthrough_inputs: given net energy use and table that includes all inputs, calculate the input to the passthrough sector
derive_passthrough_inputs <- function( net.data.frame=NA, input.data.frame=NA, region.name="GCAM_region_ID", sector.name="sector", fuel.name="fuel", yearcols=X_historical_years ){
	final.inputs <- aggregate( input.data.frame[ yearcols ],
	     by=list( GCAM_region_ID = input.data.frame[[region.name]]), sum)
	final.inputs[ c( sector.name, fuel.name ) ] <- net.data.frame[ c( sector.name, fuel.name ) ]
	final.inputs <- final.inputs[ c( region.name, sector.name, fuel.name, yearcols) ]
	return( final.inputs )
}

