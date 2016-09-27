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
logstart( "L2042.resbio_input_irr_mgmt.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Model input for residue biomass production from agriculture / forestry / milling" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
L2041.AgResBio_For <- readdata( "AGLU_LEVEL2_DATA", "L2041.AgResBio_For", skip = 4 )
L2041.GlobalResBio_Mill <- readdata( "AGLU_LEVEL2_DATA", "L2041.GlobalResBio_Mill", skip = 4 )
L2041.AgResBio_ag_irr <- readdata( "AGLU_LEVEL2_DATA", "L2041.AgResBio_ag_irr", skip = 4 )
L2041.AgResBioCurve_For <- readdata( "AGLU_LEVEL2_DATA", "L2041.AgResBioCurve_For", skip = 4 )
L2041.StubResBioCurve_Mill <- readdata( "AGLU_LEVEL2_DATA", "L2041.StubResBioCurve_Mill", skip = 4 )
L2041.AgResBioCurve_ag_irr <- readdata( "AGLU_LEVEL2_DATA", "L2041.AgResBioCurve_ag_irr", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables
printlog( "Forestry tables are copied with no changes" )
L2042.AgResBio_For <- L2041.AgResBio_For
L2042.GlobalResBio_Mill <- L2041.GlobalResBio_Mill
L2042.AgResBioCurve_For <- L2041.AgResBioCurve_For
L2042.StubResBioCurve_Mill <- L2041.StubResBioCurve_Mill

printlog( "Ag tables are repeated to add the management tech levels" )
L2042.AgResBio_ag_irr_mgmt <- repeat_and_add_vector( L2041.AgResBio_ag_irr, lvl, c( "lo", "hi" ) )
L2042.AgResBioCurve_ag_irr_mgmt <- repeat_and_add_vector( L2041.AgResBioCurve_ag_irr, lvl, c( "lo", "hi" ) )

L2042.AgResBio_ag_irr_mgmt[[agtech]] <- paste( L2042.AgResBio_ag_irr_mgmt[[agtech]], L2042.AgResBio_ag_irr_mgmt[[lvl]], sep = mgmt_delimiter )
L2042.AgResBioCurve_ag_irr_mgmt[[agtech]] <- paste( L2042.AgResBioCurve_ag_irr_mgmt[[agtech]], L2042.AgResBioCurve_ag_irr_mgmt[[lvl]], sep = mgmt_delimiter )

L2042.AgResBio_ag_irr_mgmt[[lvl]] <- NULL
L2042.AgResBioCurve_ag_irr_mgmt[[lvl]] <- NULL

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L2042.AgResBio_For, IDstring="AgResBio", domain="AGLU_LEVEL2_DATA", fn="L2042.AgResBio_For",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_resbio_input_IRR_MGMT.xml" )
write_mi_data( L2042.AgResBioCurve_For, "AgResBioCurve", "AGLU_LEVEL2_DATA", "L2042.AgResBioCurve_For", "AGLU_XML_BATCH", "batch_resbio_input_IRR_MGMT.xml" )

write_mi_data( L2042.GlobalResBio_Mill, "GlobalResBio", "AGLU_LEVEL2_DATA", "L2042.GlobalResBio_Mill", "AGLU_XML_BATCH", "batch_resbio_input_IRR_MGMT.xml" )
write_mi_data( L2042.StubResBioCurve_Mill, "StubResBioCurve", "AGLU_LEVEL2_DATA", "L2042.StubResBioCurve_Mill", "AGLU_XML_BATCH", "batch_resbio_input_IRR_MGMT.xml" )

write_mi_data( L2042.AgResBio_ag_irr_mgmt, "AgResBio", "AGLU_LEVEL2_DATA", "L2042.AgResBio_ag_irr_mgmt", "AGLU_XML_BATCH", "batch_resbio_input_IRR_MGMT.xml" )
write_mi_data( L2042.AgResBioCurve_ag_irr_mgmt, "AgResBioCurve", "AGLU_LEVEL2_DATA", "L2042.AgResBioCurve_ag_irr_mgmt", "AGLU_XML_BATCH", "batch_resbio_input_IRR_MGMT.xml" )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_resbio_input_IRR_MGMT.xml", "AGLU_XML_FINAL", "resbio_input_IRR_MGMT.xml", "", xml_tag="outFile" )

logstop()
