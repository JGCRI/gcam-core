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
logstart( "L2521.MACC_IRR.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Marginal Abatement Cost Curves" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
A_MACC_TechChange <- readdata( "EMISSIONS_ASSUMPTIONS", "A_MACC_TechChange" )
L252.AgMAC <- readdata( "EMISSIONS_LEVEL2_DATA", "L252.AgMAC", skip = 4 )
L252.MAC_an <- readdata( "EMISSIONS_LEVEL2_DATA", "L252.MAC_an", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
printlog( "L2521.AgMAC: Ag MAC curves are simply copied by irr and rfd technologies" )
L2521.AgMAC <- repeat_and_add_vector( L252.AgMAC, irr, c( "IRR", "RFD" ) )
L2521.AgMAC[[agtech]] <- paste( L2521.AgMAC[[agsubs]], L2521.AgMAC[[irr]], sep = irr_delimiter )
L2521.AgMAC[[irr]] <- NULL

printlog( "L2521.MAC_an: Animal MAC curves are not modified" )
# these still need to be written out here
L2521.MAC_an <- L252.MAC_an

printlog( "L2521.MAC_TC_SSP1: Tech Change on MACCs for SSP1" )
L2521.MAC_Ag_TC_SSP1 <- L2521.AgMAC[ names( L2521.AgMAC ) != "EPA_region" ]
L2521.MAC_Ag_TC_SSP1$tech.change <- A_MACC_TechChange$tech_change[ match( paste( "SSP1", L2521.MAC_Ag_TC_SSP1$mac.control ),
                                                                         paste( A_MACC_TechChange$scenario, A_MACC_TechChange$MAC ))]

L2521.MAC_An_TC_SSP1 <- L2521.MAC_an[ names( L2521.MAC_an ) != "EPA_region" ]
L2521.MAC_An_TC_SSP1$tech.change <- A_MACC_TechChange$tech_change[ match( paste( "SSP1", L2521.MAC_An_TC_SSP1$mac.control ),
                                                                         paste( A_MACC_TechChange$scenario, A_MACC_TechChange$MAC ))]

printlog( "L2521.MAC_TC_SSP2: Tech Change on MACCs for SSP2" )
L2521.MAC_Ag_TC_SSP2 <-  L2521.AgMAC[ names( L2521.AgMAC ) != "EPA_region" ]
L2521.MAC_Ag_TC_SSP2$tech.change <- A_MACC_TechChange$tech_change[ match( paste( "SSP2", L2521.MAC_Ag_TC_SSP1$mac.control ),
                                                                         paste( A_MACC_TechChange$scenario, A_MACC_TechChange$MAC ))]

L2521.MAC_An_TC_SSP2 <- L2521.MAC_an[ names( L2521.MAC_an ) != "EPA_region" ]
L2521.MAC_An_TC_SSP2$tech.change <- A_MACC_TechChange$tech_change[ match( paste( "SSP2", L2521.MAC_An_TC_SSP1$mac.control ),
                                                                         paste( A_MACC_TechChange$scenario, A_MACC_TechChange$MAC ))]

printlog( "L2521.MAC_TC_SSP5: Tech Change on MACCs for SSP5" )
L2521.MAC_Ag_TC_SSP5 <-  L2521.AgMAC[ names( L2521.AgMAC ) != "EPA_region" ]
L2521.MAC_Ag_TC_SSP5$tech.change <- A_MACC_TechChange$tech_change[ match( paste( "SSP5", L2521.MAC_Ag_TC_SSP1$mac.control ),
                                                                         paste( A_MACC_TechChange$scenario, A_MACC_TechChange$MAC ))]

L2521.MAC_An_TC_SSP5 <- L2521.MAC_an[ names( L2521.MAC_an ) != "EPA_region" ]
L2521.MAC_An_TC_SSP5$tech.change <- A_MACC_TechChange$tech_change[ match( paste( "SSP5", L2521.MAC_An_TC_SSP1$mac.control ),
                                                                         paste( A_MACC_TechChange$scenario, A_MACC_TechChange$MAC ))]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2521.AgMAC, "AgMAC", "EMISSIONS_LEVEL2_DATA", "L2521.AgMAC", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 
write_mi_data( L2521.MAC_an, "MAC", "EMISSIONS_LEVEL2_DATA", "L2521.MAC_an", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 

#This is the last file that refers to agricultural emissions
insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml", "EMISSIONS_XML_FINAL", "all_aglu_emissions_IRR.xml", "", xml_tag="outFile" )

write_mi_data( L2521.MAC_Ag_TC_SSP1, "AgMACTC", "EMISSIONS_LEVEL2_DATA", "L2521.MAC_Ag_TC_SSP1", "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP1_IRR.xml" ) 
write_mi_data( L2521.MAC_An_TC_SSP1, "MACTC", "EMISSIONS_LEVEL2_DATA", "L2521.MAC_An_TC_SSP1", "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP1_IRR.xml" ) 
insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP1_IRR.xml", "EMISSIONS_XML_FINAL", "MACC_TC_SSP1_IRR.xml", "", xml_tag="outFile" )

write_mi_data( L2521.MAC_Ag_TC_SSP2, "AgMACTC", "EMISSIONS_LEVEL2_DATA", "L2521.MAC_Ag_TC_SSP2", "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP2_IRR.xml" ) 
write_mi_data( L2521.MAC_An_TC_SSP2, "MACTC", "EMISSIONS_LEVEL2_DATA", "L2521.MAC_An_TC_SSP2", "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP2_IRR.xml" ) 
insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP2_IRR.xml", "EMISSIONS_XML_FINAL", "MACC_TC_SSP2_IRR.xml", "", xml_tag="outFile" )

write_mi_data( L2521.MAC_Ag_TC_SSP5, "AgMACTC", "EMISSIONS_LEVEL2_DATA", "L2521.MAC_Ag_TC_SSP5", "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP5_IRR.xml" ) 
write_mi_data( L2521.MAC_An_TC_SSP5, "MACTC", "EMISSIONS_LEVEL2_DATA", "L2521.MAC_An_TC_SSP5", "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP5_IRR.xml" ) 
insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP5_IRR.xml", "EMISSIONS_XML_FINAL", "MACC_TC_SSP5_IRR.xml", "", xml_tag="outFile" )

logstop()
