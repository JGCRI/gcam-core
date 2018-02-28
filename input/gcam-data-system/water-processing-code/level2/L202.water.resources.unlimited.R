# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "WATERPROC_DIR" ) ){
    if( Sys.getenv( "WATERPROC" ) != "" ){
        WATERPROC_DIR <- Sys.getenv( "WATERPROC" )
    } else {
        stop("Could not determine location of water data system. Please set the R var WATERPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(WATERPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(WATERPROC_DIR,"/../_common/headers/WATER_header.R",sep=""))
logstart( "L202.water.resources.unlimited.R" )
printlog( "Genereate water unlimited resource input files for region + water_type." )

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "WATER_ASSUMPTIONS", "A_water_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
L102.unlimited_water_price_R_W_Y_75USDm3 <- readdata( "WATER_LEVEL1_DATA", "L102.unlimited_water_price_R_W_Y_75USDm3" )

# -------------------------------------------------------------------------------

#2. Build tables
printlog( "L202.UnlimitRsrc: Create unlimited resource markets for water types" )
L202.UnlimitRsrc <- L102.unlimited_water_price_R_W_Y_75USDm3[, c( R, water_type ) ]
L202.UnlimitRsrc <- merge( L202.UnlimitRsrc, GCAM_region_names )
L202.UnlimitRsrc$market <- L202.UnlimitRsrc$region
names( L202.UnlimitRsrc )[ names( L202.UnlimitRsrc ) == water_type ] <- "unlimited.resource"
L202.UnlimitRsrc$output.unit <- water_units_quantity
L202.UnlimitRsrc$price.unit <- water_units_price
# Capacity factor is not used for water resources
L202.UnlimitRsrc$capacity.factor <- 1
L202.UnlimitRsrc <- L202.UnlimitRsrc[, names_UnlimitRsrc ]

# Remove water goods that are only used by ag technologies, in regions with no aglu module
L202.UnlimitRsrc <- subset( L202.UnlimitRsrc,
                            !region %in% no_aglu_regions | !unlimited.resource %in% ag_only_water_types )

printlog( "L202.UnlimitRsrcPrice: Read in fixed prices for water types." )
L202.unlimit_rsrc_price.melt <- melt( L102.unlimited_water_price_R_W_Y_75USDm3,
      id.vars=c( R, water_type ), measure.vars=X_model_years, variable.name="year", value.name="price" )
L202.unlimit_rsrc_price.melt$year <- as.integer( gsub('^X', '', L202.unlimit_rsrc_price.melt$year ) )
L202.UnlimitRsrcPrice <- add_region_name( L202.unlimit_rsrc_price.melt )
names(L202.UnlimitRsrcPrice)[names(L202.UnlimitRsrcPrice) == water_type] <- "unlimited.resource"
L202.UnlimitRsrcPrice <- L202.UnlimitRsrcPrice[, names_UnlimitRsrcPrice ]

L202.UnlimitRsrcPrice <- subset( L202.UnlimitRsrcPrice,
                                 !region %in% no_aglu_regions | !unlimited.resource %in% ag_only_water_types )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L202.UnlimitRsrc, "UnlimitRsrc", "WATER_LEVEL2_DATA", "L202.UnlimitRsrc", "WATER_XML_BATCH", "batch_unlimited_water_supply.xml")
write_mi_data( L202.UnlimitRsrcPrice, "UnlimitRsrcPrice", "WATER_LEVEL2_DATA", "L202.UnlimitRsrcPrice", "WATER_XML_BATCH", "batch_unlimited_water_supply.xml")
insert_file_into_batchxml( "WATER_XML_BATCH", "batch_unlimited_water_supply.xml", "WATER_XML_FINAL", "unlimited_water_supply.xml", "", "outFile" )

logstop()
