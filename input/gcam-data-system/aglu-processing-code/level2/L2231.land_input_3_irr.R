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
logstart( "L2231.land_input_3_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Land cover and characteristics, node level 3, irrigated v rainfed" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )

#For files that don't change, simply change the name upon read-in to save a step
printlog( "Many files from L223 file are unchanged and written out")
L2231.LN3_LogitTables <- read_logit_fn_tables( "AGLU_LEVEL2_DATA", "L223.LN3_Logit_", skip=4, include.equiv.table=T )
L2231.LN3_Logit <- readdata( "AGLU_LEVEL2_DATA", "L223.LN3_Logit", skip = 4 )
L2231.LN3_HistUnmgdAllocation <- readdata( "AGLU_LEVEL2_DATA", "L223.LN3_HistUnmgdAllocation", skip = 4 )
L2231.LN3_UnmgdAllocation <- readdata( "AGLU_LEVEL2_DATA", "L223.LN3_UnmgdAllocation", skip = 4 )
L2231.NodeEquiv <- readdata( "AGLU_LEVEL2_DATA", "L223.NodeEquiv", skip = 4 )
L2231.LN3_NoEmissCarbon <- readdata( "AGLU_LEVEL2_DATA", "L223.LN3_NoEmissCarbon", skip = 4 )
L2231.LN3_NodeCarbon <- readdata( "AGLU_LEVEL2_DATA", "L223.LN3_NodeCarbon", skip = 4 )
L2231.LN3_HistMgdAllocation_noncrop <- readdata( "AGLU_LEVEL2_DATA", "L223.LN3_HistMgdAllocation_noncrop", skip = 4 )
L2231.LN3_MgdAllocation_noncrop <- readdata( "AGLU_LEVEL2_DATA", "L223.LN3_MgdAllocation_noncrop", skip = 4 )
L2231.LN3_UnmgdCarbon <-  readdata( "AGLU_LEVEL2_DATA", "L223.LN3_UnmgdCarbon", skip = 4 )
L2231.LN3_MgdCarbon_noncrop <-  readdata( "AGLU_LEVEL2_DATA", "L223.LN3_MgdCarbon_noncrop", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables
# nothing to do here, as all tables are just being written exactly as they were read in.
# However because of how missing values are handled, the whitespace columns in the no-emiss tables are interpreted
# as missing values. there also doesn't seem to be any way to over-ride this with the na.strings argument.
L2231.LN3_NoEmissCarbon$no.emiss.carbon.calc <- " "
L2231.LN3_NodeCarbon$node.carbon.calc <- " "

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
for( curr_table_name in names( L2231.LN3_LogitTables ) ) {
    curr_header <- sub( 'L223.', '', curr_table_name )
    write_mi_data( L2231.LN3_LogitTables[[ curr_table_name ]], curr_header, "AGLU_LEVEL2_DATA",
        paste0( "L2231.", curr_header ), "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml" )
}
write_mi_data( L2231.LN3_Logit, IDstring="LN3_Logit", domain="AGLU_LEVEL2_DATA", fn="L2231.LN3_Logit",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_land_input_3_IRR.xml" )
write_mi_data( L2231.LN3_HistUnmgdAllocation, "LN3_HistUnmgdAllocation", "AGLU_LEVEL2_DATA", "L2231.LN3_HistUnmgdAllocation", "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml" )
write_mi_data( L2231.LN3_UnmgdAllocation, "LN3_UnmgdAllocation", "AGLU_LEVEL2_DATA", "L2231.LN3_UnmgdAllocation", "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml" )
write_mi_data( L2231.NodeEquiv, "EQUIV_TABLE", "AGLU_LEVEL2_DATA", "L2231.NodeEquiv", "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml" )
write_mi_data( L2231.LN3_NoEmissCarbon, "LN3_NoEmissCarbon", "AGLU_LEVEL2_DATA", "L2231.LN3_NoEmissCarbon", "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml" )
write_mi_data( L2231.LN3_NodeCarbon, "LN3_NodeCarbon", "AGLU_LEVEL2_DATA", "L2231.LN3_NodeCarbon", "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml" )
write_mi_data( L2231.LN3_HistMgdAllocation_noncrop, "LN3_HistMgdAllocation", "AGLU_LEVEL2_DATA", "L2231.LN3_HistMgdAllocation_noncrop", "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml" )
write_mi_data( L2231.LN3_MgdAllocation_noncrop, "LN3_MgdAllocation", "AGLU_LEVEL2_DATA", "L2231.LN3_MgdAllocation_noncrop", "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml" )
write_mi_data( L2231.LN3_UnmgdCarbon, "LN3_UnmgdCarbon", "AGLU_LEVEL2_DATA", "L2231.LN3_UnmgdCarbon", "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml" )
write_mi_data( L2231.LN3_MgdCarbon_noncrop, "LN3_MgdCarbon", "AGLU_LEVEL2_DATA", "L2231.LN3_MgdCarbon_noncrop", "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml", node_rename=T )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml", "AGLU_XML_FINAL", "land_input_3_IRR.xml", "", xml_tag="outFile" )

logstop()
