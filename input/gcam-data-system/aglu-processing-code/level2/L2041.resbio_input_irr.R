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
logstart( "L2041.resbio_input_IRR.R" )
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
L204.AgResBio_For <- readdata( "AGLU_LEVEL2_DATA", "L204.AgResBio_For", skip = 4 )
L204.GlobalResBio_Mill <- readdata( "AGLU_LEVEL2_DATA", "L204.GlobalResBio_Mill", skip = 4 )
L204.AgResBio_ag <- readdata( "AGLU_LEVEL2_DATA", "L204.AgResBio_ag", skip = 4 )
L204.AgResBioCurve_For <- readdata( "AGLU_LEVEL2_DATA", "L204.AgResBioCurve_For", skip = 4 )
L204.StubResBioCurve_Mill <- readdata( "AGLU_LEVEL2_DATA", "L204.StubResBioCurve_Mill", skip = 4 )
L204.AgResBioCurve_ag <- readdata( "AGLU_LEVEL2_DATA", "L204.AgResBioCurve_ag", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables
printlog( "Copying resbio tables from L204, and adding irrigation level detail where appropriate" )
L2041.AgResBio_For <- L204.AgResBio_For

L2041.GlobalResBio_Mill <- L204.GlobalResBio_Mill

L2041.AgResBio_ag_irr <- repeat_and_add_vector( L204.AgResBio_ag, irr, c( "IRR", "RFD" ) )
L2041.AgResBio_ag_irr[[agtech]] <- paste( L2041.AgResBio_ag_irr[[agtech]], L2041.AgResBio_ag_irr[[irr]], sep = irr_delimiter )
L2041.AgResBio_ag_irr[[irr]] <- NULL

L2041.AgResBioCurve_For <- L204.AgResBioCurve_For

L2041.StubResBioCurve_Mill <- L204.StubResBioCurve_Mill

L2041.AgResBioCurve_ag_irr <- repeat_and_add_vector( L204.AgResBioCurve_ag, irr, c( "IRR", "RFD" ) )
L2041.AgResBioCurve_ag_irr[[agtech]] <- paste( L2041.AgResBioCurve_ag_irr[[agsubs]], L2041.AgResBioCurve_ag_irr[[irr]], sep = irr_delimiter )
L2041.AgResBioCurve_ag_irr[[irr]] <- NULL

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L2041.AgResBio_For, IDstring="AgResBio", domain="AGLU_LEVEL2_DATA", fn="L2041.AgResBio_For",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_resbio_input_IRR.xml" )
write_mi_data( L2041.GlobalResBio_Mill, "GlobalResBio", "AGLU_LEVEL2_DATA", "L2041.GlobalResBio_Mill", "AGLU_XML_BATCH", "batch_resbio_input_IRR.xml" )
write_mi_data( L2041.AgResBio_ag_irr, "AgResBio", "AGLU_LEVEL2_DATA", "L2041.AgResBio_ag_irr", "AGLU_XML_BATCH", "batch_resbio_input_IRR.xml" )
write_mi_data( L2041.AgResBioCurve_For, "AgResBioCurve", "AGLU_LEVEL2_DATA", "L2041.AgResBioCurve_For", "AGLU_XML_BATCH", "batch_resbio_input_IRR.xml" )
write_mi_data( L2041.StubResBioCurve_Mill, "StubResBioCurve", "AGLU_LEVEL2_DATA", "L2041.StubResBioCurve_Mill", "AGLU_XML_BATCH", "batch_resbio_input_IRR.xml" )
write_mi_data( L2041.AgResBioCurve_ag_irr, "AgResBioCurve", "AGLU_LEVEL2_DATA", "L2041.AgResBioCurve_ag_irr", "AGLU_XML_BATCH", "batch_resbio_input_IRR.xml" )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_resbio_input_IRR.xml", "AGLU_XML_FINAL", "resbio_input_IRR.xml", "", xml_tag="outFile" )

logstop()
