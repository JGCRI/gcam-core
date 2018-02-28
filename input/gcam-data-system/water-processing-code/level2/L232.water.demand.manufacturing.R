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
logstart( "L232.water.demand.manufacturing.R" )
printlog( "Genereate manufacturing energy water demands input files" )

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "WATER_ASSUMPTIONS", "A_water_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A03.sector <- readdata( "WATER_ASSUMPTIONS", "A03.sector" )
A32.globaltech_coef <- readdata( "ENERGY_ASSUMPTIONS", "A32.globaltech_coef" )
L132.water_coef_manufacturing_R_W_m3_GJ <- readdata( "WATER_LEVEL1_DATA", "L132.water_coef_manufacturing_R_W_m3_GJ" )

# -------------------------------------------------------------------------------

#2. Build tables
printlog( "L232.TechCoef: Just read in water coefficients for all years" )
L232.TechCoef <- L132.water_coef_manufacturing_R_W_m3_GJ
# Just need the first row as the first and second just name different inputs.
L232.TechCoef[, c( supp, subs, tech ) ] <- A32.globaltech_coef[ 1, c( supp, subs, tech ) ]
L232.TechCoef[[water_sector]] <- "Manufacturing"
L232.TechCoef$minicam.energy.input <- get_water_inputs_for_mapping( L232.TechCoef, A03.sector )
L232.TechCoef <- merge( L232.TechCoef, GCAM_region_names )
L232.TechCoef$market.name <- L232.TechCoef[[reg]]
# Set the coef for all years
L232.orig_num_rows <- nrow( L232.TechCoef )
L232.TechCoef <- L232.TechCoef[ rep( 1:nrow( L232.TechCoef ), times=length( model_years ) ), ]
L232.TechCoef$year <- model_years[ sort( rep( 1:length( model_years ), times=L232.orig_num_rows ) ) ]
L232.TechCoef <- L232.TechCoef[, names_TechCoef ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L232.TechCoef, "TechCoef", "WATER_LEVEL2_DATA", "L232.TechCoef", "WATER_XML_BATCH", "batch_water_demand_industry.xml" )
insert_file_into_batchxml( "WATER_XML_BATCH", "batch_water_demand_industry.xml", "WATER_XML_FINAL", "water_demand_industry.xml", "", "outFile" )

logstop()
