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
logstart( "L2062.ag_Fert_irr_mgmt.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Model input for agricultural fertilizer consumption" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
L2061.AgCoef_Fert_ag_irr <- readdata( "AGLU_LEVEL2_DATA", "L2061.AgCoef_Fert_ag_irr", skip = 4 )
L2061.AgCoef_Fert_bio_irr <- readdata( "AGLU_LEVEL2_DATA", "L2061.AgCoef_Fert_bio_irr", skip = 4 )
L2061.AgCost_ag_irr_adj <- readdata( "AGLU_LEVEL2_DATA", "L2061.AgCost_ag_irr_adj", skip = 4 )
L2061.AgCost_bio_irr_adj <- readdata( "AGLU_LEVEL2_DATA", "L2061.AgCost_bio_irr_adj", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables
#Repeat to add in the lo and hi levels
L2062.AgCoef_Fert_ag_irr_mgmt <- repeat_and_add_vector( L2061.AgCoef_Fert_ag_irr, lvl, c( "lo", "hi" ) )
L2062.AgCoef_Fert_bio_irr_mgmt <- repeat_and_add_vector( L2061.AgCoef_Fert_bio_irr, lvl, c( "lo", "hi" ) )
L2062.AgCost_ag_irr_mgmt_adj <- repeat_and_add_vector( L2061.AgCost_ag_irr_adj, lvl, c( "lo", "hi" ) )
L2062.AgCost_bio_irr_mgmt_adj <- repeat_and_add_vector( L2061.AgCost_bio_irr_adj, lvl, c( "lo", "hi" ) )

#Paste to technology names
L2062.AgCoef_Fert_ag_irr_mgmt[[agtech]] <- paste( L2062.AgCoef_Fert_ag_irr_mgmt[[agtech]], L2062.AgCoef_Fert_ag_irr_mgmt[[lvl]], sep = mgmt_delimiter )
L2062.AgCoef_Fert_bio_irr_mgmt[[agtech]] <- paste( L2062.AgCoef_Fert_bio_irr_mgmt[[agtech]], L2062.AgCoef_Fert_bio_irr_mgmt[[lvl]], sep = mgmt_delimiter )
L2062.AgCost_ag_irr_mgmt_adj[[agtech]] <- paste( L2062.AgCost_ag_irr_mgmt_adj[[agtech]], L2062.AgCost_ag_irr_mgmt_adj[[lvl]], sep = mgmt_delimiter )
L2062.AgCost_bio_irr_mgmt_adj[[agtech]] <- paste( L2062.AgCost_bio_irr_mgmt_adj[[agtech]], L2062.AgCost_bio_irr_mgmt_adj[[lvl]], sep = mgmt_delimiter )

#Remove the column indicating the management level as it is appended to the technology name
L2062.AgCoef_Fert_ag_irr_mgmt[[lvl]] <- NULL
L2062.AgCoef_Fert_bio_irr_mgmt[[lvl]] <- NULL
L2062.AgCost_ag_irr_mgmt_adj[[lvl]] <- NULL
L2062.AgCost_bio_irr_mgmt_adj[[lvl]] <- NULL

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2062.AgCoef_Fert_ag_irr_mgmt, "AgCoef", "AGLU_LEVEL2_DATA", "L2062.AgCoef_Fert_ag_irr_mgmt", "AGLU_XML_BATCH", "batch_ag_Fert_IRR_MGMT.xml" )
write_mi_data( L2062.AgCoef_Fert_bio_irr_mgmt, "AgCoef", "AGLU_LEVEL2_DATA", "L2062.AgCoef_Fert_bio_irr_mgmt", "AGLU_XML_BATCH", "batch_ag_Fert_IRR_MGMT.xml" )
write_mi_data( L2062.AgCost_ag_irr_mgmt_adj, "AgCost", "AGLU_LEVEL2_DATA", "L2062.AgCost_ag_irr_mgmt_adj", "AGLU_XML_BATCH", "batch_ag_Fert_IRR_MGMT.xml" )
write_mi_data( L2062.AgCost_bio_irr_mgmt_adj, "AgCost", "AGLU_LEVEL2_DATA", "L2062.AgCost_bio_irr_mgmt_adj", "AGLU_XML_BATCH", "batch_ag_Fert_IRR_MGMT.xml" )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_Fert_IRR_MGMT.xml", "AGLU_XML_FINAL", "ag_Fert_IRR_MGMT.xml", "", xml_tag="outFile" )

logstop()
