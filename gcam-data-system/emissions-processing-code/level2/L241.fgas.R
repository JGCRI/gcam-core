# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "EMISSPROC_DIR" ) ){
    if( Sys.getenv( "EMISSIONSPROC" ) != "" ){
        EMISSPROC_DIR <- Sys.getenv( "EMISSIONSPROC" )
    } else {
        stop("Could not determine location of emissions data system. Please set the R var EMISSPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
logstart( "L241.fgas.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions for industrial & urban processes" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A_region <- readdata( "EMISSIONS_ASSUMPTIONS", "A_regions" )
L142.fgas_tg_R_S_Yh <- readdata( "EMISSIONS_LEVEL1_DATA", "L142.fgas_tg_R_S_Yh" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
#Sulfur emissions
printlog( "L241.fgas: F-gas emissions for technologies in all regions" )
#Interpolate and add region name
L241.fgas <- L142.fgas_tg_R_S_Yh
L241.fgas <- melt( L241.fgas, id.vars = grep( "X[0-9]{4}", names( L241.fgas ), invert = T ) )
L241.fgas$year <- as.numeric( substr( L241.fgas$variable, 2, 5 ) )
L241.fgas <- subset( L241.fgas, L241.fgas$year %in% emiss_model_base_years )
L241.fgas <- add_region_name( L241.fgas )

#Format for csv file
L241.fgas_all <- L241.fgas[ c( names_StubTechYr, "Non.CO2" ) ]
L241.fgas_all$input.emissions <- round( L241.fgas$value, digits_emissions )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L241.fgas_all, "StbTechOutputEmissions", "EMISSIONS_LEVEL2_DATA", "L241.fgas_all", "EMISSIONS_XML_BATCH", "batch_fgas_emissions.xml" ) 

insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_fgas_emissions.xml", "EMISSIONS_XML_FINAL", "all_fgas_emissions.xml", "", xml_tag="outFile" )

logstop()
