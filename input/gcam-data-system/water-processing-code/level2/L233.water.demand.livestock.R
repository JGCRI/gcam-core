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
logstart( "L233.water.demand.livestock.R" )
printlog( "Genereate livestock water demands input files" )

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "WATER_ASSUMPTIONS", "A_water_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A03.sector <- readdata( "WATER_ASSUMPTIONS", "A03.sector" )
A_an_technology <- readdata( "AGLU_ASSUMPTIONS", "A_an_technology" )
L133.water_demand_livestock_R_C_W_km3_Mt <- readdata( "WATER_LEVEL1_DATA", "L133.water_demand_livestock_R_C_W_km3_Mt" )

# -------------------------------------------------------------------------------

#2. Build tables
printlog( "L233.TechCoef: Just read in water coefficients for all years" )
L233.TechCoef <- merge( L133.water_demand_livestock_R_C_W_km3_Mt, A_an_technology[, c( supp, subs, tech ) ], by.x="GCAM_commodity", by.y=supp )
names(L233.TechCoef)[names(L233.TechCoef) == "GCAM_commodity"] <- supp
L233.TechCoef[[water_sector]] <- "Livestock"
L233.TechCoef$minicam.energy.input <- get_water_inputs_for_mapping( L233.TechCoef, A03.sector )
L233.TechCoef <- merge( L233.TechCoef, GCAM_region_names )
L233.TechCoef$market.name <- L233.TechCoef[[reg]]
# Set the coef for all years
L233.orig_num_rows <- nrow( L233.TechCoef )
L233.TechCoef <- L233.TechCoef[ rep( 1:nrow( L233.TechCoef ), times=length( model_years ) ), ]
L233.TechCoef$year <- model_years[ sort( rep( 1:length( model_years ), times=L233.orig_num_rows ) ) ]
L233.TechCoef <- L233.TechCoef[, names_TechCoef ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L233.TechCoef, "TechCoef", "WATER_LEVEL2_DATA", "L233.TechCoef", "WATER_XML_BATCH", "batch_water_demand_livestock.xml" )
insert_file_into_batchxml( "WATER_XML_BATCH", "batch_water_demand_livestock.xml", "WATER_XML_FINAL", "water_demand_livestock.xml", "", "outFile" )

logstop()
