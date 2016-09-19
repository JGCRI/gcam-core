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
		year.fillout = min( model_base_years ),
		maxSubResource = round( data$maxSubResource, digits_maxSubResource ),
		mid.price = round( data$mid.price, digits_mid.price ),
		curve.exponent = round( data$curve.exponent, digits_curve.exponent ) )
        if( any( names( data ) == "gdpSupplyElast" ) ) {
            data_new$gdpSupplyElast = round( data$gdpSupplyElast, digits_gdpSupplyElast )
        }
        if( any( names( data ) == "subResourceCapacityFactor" ) ) {
            data_new$subResourceCapacityFactor = round( data$subResourceCapacityFactor, digits_capacity.factor )
        }
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
# derive_passthrough_inputs: given net energy use and table that includes all inputs, calculate the input to the passthrough sector
derive_passthrough_inputs <- function( net.data.frame=NA, input.data.frame=NA, region.name="GCAM_region_ID", sector.name="sector", fuel.name="fuel", yearcols=X_historical_years ){
	final.inputs <- aggregate( input.data.frame[ yearcols ],
	     by=list( GCAM_region_ID = input.data.frame[[region.name]]), sum)
	final.inputs[ c( sector.name, fuel.name ) ] <- net.data.frame[ c( sector.name, fuel.name ) ]
	final.inputs <- final.inputs[ c( region.name, sector.name, fuel.name, yearcols) ]
	return( final.inputs )
}

