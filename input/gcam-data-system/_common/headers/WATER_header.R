# Specify the location of the module from the data system root directory
if( !exists( "MODULE_PROC_ROOT" ) | MODULE_PROC_ROOT == '' ) {
MODULE_PROC_ROOT        <- WATERPROC_DIR
}

# Get the appropriate minicam.energy.input name to use in the GCAM supplysector by
# looking up using a mapping to the water_sector and water_type.  The minicam.energy.input
# name to use will have to be some water mapping sector for water_types that are "mapped".
# d The data which contains a water_sector and water_type (and AEZ if the water sector is
#   irrigation) to determine the new input names.
# water_mapping The water mapping which gives us the name of the appropriate water mapping sector
#               to use for each water_sector.
# water_sector.col The name of the column in d that contains the "water_sector"
# water_type.col The name of the column in d that contains the "water_type"
# AEZ.col The name of the column in d that contains the "AEZ".  Note this is only needed if d contains
#         the water_sector Irrigation.
# Returns a vector of names that have been aprropriately mapped.
get_water_inputs_for_mapping <- function( d, water_mapping, water_sector.col=water_sector, water_type.col=water_type, AEZ.col="AEZ" ) {
    # The water_sector column must exist in the data
    stopifnot( any( names( d ) == water_sector.col ) )
    # The water_sector column must exist in the data
    stopifnot( any( names( d ) == water_type.col ) )

    cols_needed <- c( water_sector.col, water_type.col )
    use_AEZ <- F
    # If there is an irrigation sector with a mapped water_tpe then the AEZ
    # column is needed thus must be defined and exists in the data
    if( any( d[[water_sector.col]] == "Irrigation" & d[[water_type.col]] %in% mapped_water_types ) ) {
        stopifnot( any( names( d ) == AEZ.col ) )
        cols_needed <- c( cols_needed, AEZ.col )
        use_AEZ <- T
    }
    d_temp <- d[, cols_needed];

    # Add in the base mapped sector name and short water names so we can generate the
    # appropriate names for rows that need it.
    d_temp$base_name <- water_mapping[ match( d_temp[[water_sector.col]], water_mapping[[water_sector]]), supp ]
    d_temp[ d_temp[[water_type.col]] == mapped_water_types[1], "water_type_short" ] <- mapped_water_types_short[1]
    d_temp[ d_temp[[water_type.col]] == mapped_water_types[2], "water_type_short" ] <- mapped_water_types_short[2]

    # non-mapped water_types keep their names unchanged.
    d_temp[ d_temp[[water_type.col]] %!in% mapped_water_types, "new_name" ] <-
        d_temp[ d_temp[[water_type.col]] %!in% mapped_water_types, water_type.col ]

    # We must construct the name of the appropriate water mapping sector based on the water_sector and water_type
    d_temp[ d_temp[[water_sector.col]] != "Irrigation" & d_temp[[water_type.col]] %in% mapped_water_types, "new_name"] <- paste(
        d_temp[ d_temp[[water_sector.col]] != "Irrigation" & d_temp[[water_type.col]] %in% mapped_water_types, "base_name"],
        d_temp[ d_temp[[water_sector.col]] != "Irrigation" & d_temp[[water_type.col]] %in% mapped_water_types, "water_type_short"], sep="_" )
    if( use_AEZ ) {
        d_temp[ d_temp[[water_sector.col]] == "Irrigation" & d_temp[[water_type.col]] %in% mapped_water_types, "new_name"] <- paste(
                paste0(
                d_temp[ d_temp[[water_sector.col]] == "Irrigation" & d_temp[[water_type.col]] %in% mapped_water_types, "base_name"],
                sprintf( "%02d",
                    d_temp[ d_temp[[water_sector.col]] == "Irrigation" & d_temp[[water_type.col]] %in% mapped_water_types, AEZ.col ] ) ),
                d_temp[ d_temp[[water_sector.col]] == "Irrigation" & d_temp[[water_type.col]] %in% mapped_water_types, "water_type_short"], sep="_" )
    }
    return( d_temp$new_name )
}

