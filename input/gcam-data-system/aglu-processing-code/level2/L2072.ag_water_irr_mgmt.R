#This file builds all tables necessarily for generating the model inputs for water
# Agricultural component
#  Water coefficients (km3 / Mt crop) for all regions, crops, AEZs, and years

# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "AGLUPROC_DIR" ) ){
    if( Sys.getenv( "AGLUPROC" ) != "" ){
        AGLUPROC_DIR <- Sys.getenv( "AGLUPROC" )
    } else {
        stop("Could not determine location of aglu processing scripts, please set the R var AGLUPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
logstart( "L2072.ag_water_irr_mgmt.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Model input for water withdrawals and consumption" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )

#Read the water coef files in a for loop
Ag_Water_Domain <- readdomainpathmap()["AGLU_LEVEL2_DATA"][[1]]
L2071.Ag_Water_filenames <- list.files( Ag_Water_Domain )[ grepl( "L2071.AgCoef", list.files( Ag_Water_Domain))]
L2071.Ag_Water_filenames <- sub( ".csv", "", L2071.Ag_Water_filenames )
L2071.Ag_Water_files.list <- list()
for( i in L2071.Ag_Water_filenames ){
  index <- which( L2071.Ag_Water_filenames == i )
  L2071.Ag_Water_files.list[[ index ]] <- readdata( "AGLU_LEVEL2_DATA", i, skip = 4)
}
names( L2071.Ag_Water_files.list ) <- L2071.Ag_Water_filenames

# -----------------------------------------------------------------------------
# 2. Build tables
L2072.Ag_Water_files.list <- L2071.Ag_Water_files.list
names( L2072.Ag_Water_files.list ) <- sub( "L2071", "L2072", names( L2072.Ag_Water_files.list ) )
for( i in 1:length( L2072.Ag_Water_files.list ) ){
	L2072.Ag_Water_files.list[[i]] <- repeat_and_add_vector( L2072.Ag_Water_files.list[[i]], lvl, c( "lo", "hi" ) )
	L2072.Ag_Water_files.list[[i]][[agtech]] <- paste( L2072.Ag_Water_files.list[[i]][[agtech]], L2072.Ag_Water_files.list[[i]][[lvl]], sep = mgmt_delimiter )
	L2072.Ag_Water_files.list[[i]] <- L2072.Ag_Water_files.list[[i]][ names_AgCoef ]	
}

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
for( i in 1:length( L2072.Ag_Water_files.list ) ){
	objectname <- paste0( names( L2072.Ag_Water_files.list[i] ), "_mgmt" )
	object <- L2072.Ag_Water_files.list[[i]]
	assign( objectname, object )
	write_mi_data( object, "AgCoef", "AGLU_LEVEL2_DATA", objectname, "AGLU_XML_BATCH", "batch_ag_water_input_IRR_MGMT.xml" )
}

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_water_input_IRR_MGMT.xml", "AGLU_XML_FINAL", "ag_water_input_IRR_MGMT.xml", "", xml_tag="outFile" )

logstop()
