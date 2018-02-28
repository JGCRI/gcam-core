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
logstart( "L2522.ag_MACC_IRR_MGMT.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Marginal Abatement Cost curves in the aglu system" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
A_MACC_TechChange <- readdata( "EMISSIONS_ASSUMPTIONS", "A_MACC_TechChange" )
L2521.MAC_an <- readdata( "EMISSIONS_LEVEL2_DATA", "L2521.MAC_an", skip = 4 )
L2521.AgMAC <- readdata( "EMISSIONS_LEVEL2_DATA", "L2521.AgMAC", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
L2522.MAC_an <- L2521.MAC_an
L2522.AgMAC <- repeat_and_add_vector( L2521.AgMAC, lvl, c( "lo", "hi" ) )
L2522.AgMAC[[agtech]] <- paste( L2522.AgMAC[[agtech]], L2522.AgMAC[[lvl]], sep = mgmt_delimiter )
L2522.AgMAC[[lvl]] <- NULL

printlog( "L2522.MAC_TC_SSP1: Tech Change on MACCs for SSP1" )
L2522.MAC_Ag_TC_SSP1 <- L2522.AgMAC[ names( L2522.AgMAC ) != "EPA_region" ]
L2522.MAC_Ag_TC_SSP1$tech.change <- A_MACC_TechChange$tech_change[ match( paste( "SSP1", L2522.MAC_Ag_TC_SSP1$mac.control ),
                                                                          paste( A_MACC_TechChange$scenario, A_MACC_TechChange$MAC ))]

L2522.MAC_An_TC_SSP1 <- L2522.MAC_an[ names( L2522.MAC_an ) != "EPA_region" ]
L2522.MAC_An_TC_SSP1$tech.change <- A_MACC_TechChange$tech_change[ match( paste( "SSP1", L2522.MAC_An_TC_SSP1$mac.control ),
                                                                          paste( A_MACC_TechChange$scenario, A_MACC_TechChange$MAC ))]

printlog( "L2522.MAC_TC_SSP2: Tech Change on MACCs for SSP2" )
L2522.MAC_Ag_TC_SSP2 <-  L2522.AgMAC[ names( L2522.AgMAC ) != "EPA_region" ]
L2522.MAC_Ag_TC_SSP2$tech.change <- A_MACC_TechChange$tech_change[ match( paste( "SSP2", L2522.MAC_Ag_TC_SSP1$mac.control ),
                                                                          paste( A_MACC_TechChange$scenario, A_MACC_TechChange$MAC ))]

L2522.MAC_An_TC_SSP2 <- L2522.MAC_an[ names( L2522.MAC_an ) != "EPA_region" ]
L2522.MAC_An_TC_SSP2$tech.change <- A_MACC_TechChange$tech_change[ match( paste( "SSP2", L2522.MAC_An_TC_SSP1$mac.control ),
                                                                          paste( A_MACC_TechChange$scenario, A_MACC_TechChange$MAC ))]

printlog( "L2522.MAC_TC_SSP5: Tech Change on MACCs for SSP5" )
L2522.MAC_Ag_TC_SSP5 <-  L2522.AgMAC[ names( L2522.AgMAC ) != "EPA_region" ]
L2522.MAC_Ag_TC_SSP5$tech.change <- A_MACC_TechChange$tech_change[ match( paste( "SSP5", L2522.MAC_Ag_TC_SSP1$mac.control ),
                                                                          paste( A_MACC_TechChange$scenario, A_MACC_TechChange$MAC ))]

L2522.MAC_An_TC_SSP5 <- L2522.MAC_an[ names( L2522.MAC_an ) != "EPA_region" ]
L2522.MAC_An_TC_SSP5$tech.change <- A_MACC_TechChange$tech_change[ match( paste( "SSP5", L2522.MAC_An_TC_SSP1$mac.control ),
                                                                          paste( A_MACC_TechChange$scenario, A_MACC_TechChange$MAC ))]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2522.MAC_an, "MAC", "EMISSIONS_LEVEL2_DATA", "L2522.MAC_an", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR_MGMT.xml" ) 
write_mi_data( L2522.AgMAC, "AgMAC", "EMISSIONS_LEVEL2_DATA", "L2522.AgMAC", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR_MGMT.xml" ) 

insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR_MGMT.xml", "EMISSIONS_XML_FINAL", "all_aglu_emissions_IRR_MGMT.xml", "", xml_tag="outFile" )

write_mi_data( L2522.MAC_Ag_TC_SSP1, "AgMACTC", "EMISSIONS_LEVEL2_DATA", "L2522.MAC_Ag_TC_SSP1", "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP1_IRR_MGMT.xml" ) 
write_mi_data( L2522.MAC_An_TC_SSP1, "MACTC", "EMISSIONS_LEVEL2_DATA", "L2522.MAC_An_TC_SSP1", "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP1_IRR_MGMT.xml" ) 
insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP1_IRR_MGMT.xml", "EMISSIONS_XML_FINAL", "MACC_TC_SSP1_IRR_MGMT.xml", "", xml_tag="outFile" )

write_mi_data( L2522.MAC_Ag_TC_SSP2, "AgMACTC", "EMISSIONS_LEVEL2_DATA", "L2522.MAC_Ag_TC_SSP2", "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP2_IRR_MGMT.xml" ) 
write_mi_data( L2522.MAC_An_TC_SSP2, "MACTC", "EMISSIONS_LEVEL2_DATA", "L2522.MAC_An_TC_SSP2", "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP2_IRR_MGMT.xml" ) 
insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP2_IRR_MGMT.xml", "EMISSIONS_XML_FINAL", "MACC_TC_SSP2_IRR_MGMT.xml", "", xml_tag="outFile" )

write_mi_data( L2522.MAC_Ag_TC_SSP5, "AgMACTC", "EMISSIONS_LEVEL2_DATA", "L2522.MAC_Ag_TC_SSP5", "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP5_IRR_MGMT.xml" ) 
write_mi_data( L2522.MAC_An_TC_SSP5, "MACTC", "EMISSIONS_LEVEL2_DATA", "L2522.MAC_An_TC_SSP5", "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP5_IRR_MGMT.xml" ) 
insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP5_IRR_MGMT.xml", "EMISSIONS_XML_FINAL", "MACC_TC_SSP5_IRR_MGMT.xml", "", xml_tag="outFile" )

logstop()
