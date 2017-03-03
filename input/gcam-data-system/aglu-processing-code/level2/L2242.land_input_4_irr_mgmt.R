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
logstart( "L2242.land_input_4_irr_mgmt.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Land cover and characteristics, node level 4, irrigated v rainfed, disaggregated to management techs" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
L2241.LN4_LogitTables <- read_logit_fn_tables( "AGLU_LEVEL2_DATA", "L2241.LN4_Logit_", skip=4, include.equiv.table=T )
L2241.LN4_Logit <- readdata( "AGLU_LEVEL2_DATA", "L2241.LN4_Logit", skip = 4 )
L2241.LN4_NodeGhostShare <- readdata( "AGLU_LEVEL2_DATA", "L2241.LN4_NodeGhostShare", skip = 4 )
L2241.LN4_NodeIsGhostShareRel <- readdata( "AGLU_LEVEL2_DATA", "L2241.LN4_NodeIsGhostShareRel", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables
printlog( "NOTE: There are no technologies that are disaggregated to irrigated and rainfed, but not to lo- and hi-input techs" )
printlog( "This code file only writes out the logit exponent for the irrigated/rainfed node competition" )
printlog( "L233.LN4_Logit: Logit exponent, fourth nest" )
# Note: The na.omit is intended to get rid of a tailing node-rename table that may be appended to level2 land alloator tables
L2242.LN4_Logit <- na.omit( L2241.LN4_Logit )

printlog( "New Node: irrigated and rainfed bioenergy, fourth nest" )
L2242.LN4_NodeGhostShare <- na.omit( L2241.LN4_NodeGhostShare )
L2242.LN4_NodeIsGhostShareRel <- na.omit( L2241.LN4_NodeIsGhostShareRel )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

for( curr_table_name in names( L2241.LN4_LogitTables ) ) {
    curr_header <- sub( 'L2241.', '', curr_table_name )
    write_mi_data( L2241.LN4_LogitTables[[ curr_table_name ]], curr_header, "AGLU_LEVEL2_DATA",
        paste0( "L2242.", curr_header ), "AGLU_XML_BATCH", "batch_land_input_4_IRR_MGMT.xml" )
}
write_mi_data( L2242.LN4_Logit, "LN4_Logit", "AGLU_LEVEL2_DATA", "L2242.LN4_Logit", "AGLU_XML_BATCH", "batch_land_input_4_IRR_MGMT.xml", node_rename = T )
write_mi_data( L2242.LN4_NodeGhostShare, "LN4_NodeGhostShare", "AGLU_LEVEL2_DATA", "L2242.LN4_NodeGhostShare", "AGLU_XML_BATCH", "batch_land_input_4_IRR_MGMT.xml" )
write_mi_data( L2242.LN4_NodeIsGhostShareRel, "LN4_NodeIsGhostShareRel", "AGLU_LEVEL2_DATA", "L2242.LN4_NodeIsGhostShareRel", "AGLU_XML_BATCH", "batch_land_input_4_IRR_MGMT.xml", node_rename = T )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_land_input_4_IRR_MGMT.xml", "AGLU_XML_FINAL", "land_input_4_IRR_MGMT.xml", "", xml_tag="outFile" )

logstop()
