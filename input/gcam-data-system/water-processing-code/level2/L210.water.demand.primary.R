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
logstart( "L210.water.demand.primary.R" )
printlog( "Genereate primary energy water demands input files" )

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "WATER_ASSUMPTIONS", "A_water_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A03.sector <- readdata( "WATER_ASSUMPTIONS", "A03.sector" )
# TODO: we are driving primary energy water demands by consumption instead of production
A21.globaltech_coef <- readdata( "ENERGY_ASSUMPTIONS", "A21.globaltech_coef" )
A22.globaltech_coef <- readdata( "ENERGY_ASSUMPTIONS", "A22.globaltech_coef" )
L110.water_demand_primary_R_S_W_m3_GJ <- readdata( "WATER_LEVEL1_DATA", "L110.water_demand_primary_R_S_W_m3_GJ" )

# -------------------------------------------------------------------------------

#2. Build tables
printlog( "L210.TechCoef: Just read in water coefficients for all years" )
L210.TechCoef <- merge( L110.water_demand_primary_R_S_W_m3_GJ, rbind( A21.globaltech_coef[, c( supp, subs, tech ) ],
    A22.globaltech_coef[ grepl( 'nuclear', A22.globaltech_coef[[supp]] ), c( supp, subs, tech ) ] ) )
# Avoid double acounting unconventional oil in the regional oil sector as it is already
# accounted for in unconventional oil production.
L210.TechCoef <- L210.TechCoef[!( L210.TechCoef[[supp]] == "regional oil" & L210.TechCoef[[subs]] == "unconventional oil" ), ]
L210.TechCoef[[water_sector]] <- "Mining"
L210.TechCoef$minicam.energy.input <- get_water_inputs_for_mapping( L210.TechCoef, A03.sector )
L210.TechCoef <- merge( L210.TechCoef, GCAM_region_names )
L210.TechCoef$market.name <- L210.TechCoef[[reg]]
# Set the coef for all years
L210.orig_num_rows <- nrow( L210.TechCoef )
L210.TechCoef <- L210.TechCoef[ rep( 1:nrow( L210.TechCoef ), times=length( model_years ) ), ]
L210.TechCoef$year <- model_years[ sort( rep( 1:length( model_years ), times=L210.orig_num_rows ) ) ]
L210.TechCoef <- L210.TechCoef[, names_TechCoef ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L210.TechCoef, "TechCoef", "WATER_LEVEL2_DATA", "L210.TechCoef", "WATER_XML_BATCH", "batch_water_demand_primary.xml" )
insert_file_into_batchxml( "WATER_XML_BATCH", "batch_water_demand_primary.xml", "WATER_XML_FINAL", "water_demand_primary.xml", "", "outFile" )

logstop()
