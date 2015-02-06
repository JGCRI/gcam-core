if( !exists( "GCAMUSAPROC_DIR" ) ){
    if( Sys.getenv( "GCAMUSAPROC" ) != "" ){
        GCAMUSAPROC_DIR <- Sys.getenv( "GCAMUSAPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var GCAMUSAPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
logstart( "L225.hydrogen_USA.R" )
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
printlog( "Model input for state hydrogen sectors" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
L225.SubsectorLogit_h2 <- readdata( "ENERGY_LEVEL2_DATA", "L225.SubsectorLogit_h2", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Keeping hydrogen at the national level (no basis for inter-state competition)" )
printlog( "Deleting wind and solar subsectors as these resources do not exist at the national level in GCAM USA")
printlog( "Also removing electricity for now b/c this is not done at the nation level")
L225.DeleteSubsector_h2_USA <- subset( L225.SubsectorLogit_h2, region == "USA" & subsector %in% c( "wind", "solar", "electricity" ) )[ names_Subsector ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L225.DeleteSubsector_h2_USA, "DeleteSubsector", "GCAMUSA_LEVEL2_DATA", "L225.DeleteSubsector_h2_USA", "GCAMUSA_XML_BATCH", "batch_hydrogen_USA.xml" )

insert_file_into_batchxml( "GCAMUSA_XML_BATCH", "batch_hydrogen_USA.xml", "GCAMUSA_XML_FINAL", "hydrogen_USA.xml", "", xml_tag="outFile" )

logstop()
