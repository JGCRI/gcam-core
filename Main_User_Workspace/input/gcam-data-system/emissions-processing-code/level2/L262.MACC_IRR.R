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
logstart( "L262.MACC_IRR.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Marginal Abatement Cost Curves" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A_regions <- readdata( "EMISSIONS_ASSUMPTIONS", "A_regions" )
A_MACC_TechChange <- readdata( "EMISSIONS_ASSUMPTIONS", "A_MACC_TechChange" )
GCAM_sector_tech <- readdata( "EMISSIONS_MAPPINGS", "GCAM_sector_tech" )
L152.MAC_pct_R_S_Proc_EPA <- readdata( "EMISSIONS_LEVEL1_DATA", "L152.MAC_pct_R_S_Proc_EPA" )
L261.AGREmissions <- readdata( "EMISSIONS_LEVEL2_DATA", "L261.agr_emissions", skip = 4 )
L261.AnEmissions <- readdata( "EMISSIONS_LEVEL2_DATA", "L261.an_emissions", skip = 4 )
L261.AnNH3Emissions <- readdata( "EMISSIONS_LEVEL2_DATA", "L261.an_nh3_emissions", skip = 4 )
L261.AGRBio <- readdata( "EMISSIONS_LEVEL2_DATA", "L261.bio_emissions", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
printlog( "Prepare the table with all MAC curves for matching" )
L262.MAC_pct_R_S_Proc_EPA <- melt( L152.MAC_pct_R_S_Proc_EPA, id.vars = EPA_MACC_names, variable.name = "tax", value.name = "mac.reduction" )
L262.MAC_pct_R_S_Proc_EPA$tax <- as.numeric( sub( "X", "", L262.MAC_pct_R_S_Proc_EPA$tax ) )

printlog( "L262.AgMAC: Agricultural abatement (including bioenergy)" )
L262.AgMAC <- rbind(
      subset( L261.AGREmissions[ c( names_AgTechYr, "Non.CO2" ) ], year == min( L261.AGREmissions$year ) & Non.CO2 %in% ag_MACC_GHG_names ),
      subset( L261.AGRBio[ c( names_AgTechYr, "Non.CO2" ) ], year == min( L261.AGRBio$year ) & Non.CO2 %in% ag_MACC_GHG_names ) )
L262.AgMAC$mac.control <- GCAM_sector_tech$EPA_MACC_Sector[
      match( L262.AgMAC$AgSupplySector, GCAM_sector_tech$supplysector ) ]
#dropping jatropha b/c it isn't in the tech table, and wasn't considered in the epa analysis anyway
L262.AgMAC <- na.omit( L262.AgMAC )
L262.AgMAC <- repeat_and_add_vector( L262.AgMAC, "tax", MAC_taxes )
L262.AgMAC <- L262.AgMAC[ order( L262.AgMAC$region, L262.AgMAC$AgProductionTechnology ), ]
L262.AgMAC$mac.reduction <- NA #until we have the EPA region for matching
L262.AgMAC$EPA_region <- A_regions$MAC_region[ match( L262.AgMAC$region, A_regions$region ) ]
L262.AgMAC$mac.reduction <- round(
      L262.MAC_pct_R_S_Proc_EPA$mac.reduction[
         match( vecpaste( L262.AgMAC[ c( "EPA_region", "mac.control", "tax" ) ] ),
                vecpaste( L262.MAC_pct_R_S_Proc_EPA[ c( "EPA_region", "Process", "tax" ) ] ) ) ],
      digits_MACC )
L262.AgMAC <- na.omit( L262.AgMAC )
L262.AgMAC <- L262.AgMAC[ names( L262.AgMAC ) != "EPA_region" ]

printlog( "L262.MAC_an: Abatement from animal production" )
L262.AnEmissions <- rbind( L261.AnEmissions, L261.AnNH3Emissions )
L262.MAC_an <- subset( L262.AnEmissions[ c( names_StubTechYr, "Non.CO2" ) ], year == min( L262.AnEmissions$year ) & Non.CO2 %in% ag_MACC_GHG_names )
L262.MAC_an$mac.control <- GCAM_sector_tech$EPA_MACC_Sector[
      match( L262.MAC_an$supplysector, GCAM_sector_tech$supplysector ) ]
L262.MAC_an <- repeat_and_add_vector( L262.MAC_an, "tax", MAC_taxes )
L262.MAC_an <- L262.MAC_an[ order( L262.MAC_an$region, L262.MAC_an$supplysector, L262.MAC_an$subsector, L262.MAC_an$stub.technology, L262.MAC_an$Non.CO2 ), ]
L262.MAC_an$mac.reduction <- NA #until we have the EPA region for matching
L262.MAC_an$EPA_region <- A_regions$MAC_region[ match( L262.MAC_an$region, A_regions$region ) ]
L262.MAC_an$mac.reduction <- round(
      L262.MAC_pct_R_S_Proc_EPA$mac.reduction[
         match( vecpaste( L262.MAC_an[ c( "EPA_region", "mac.control", "tax" ) ] ),
                vecpaste( L262.MAC_pct_R_S_Proc_EPA[ c( "EPA_region", "Process", "tax" ) ] ) ) ],
      digits_MACC )
L262.MAC_an <- na.omit( L262.MAC_an )
L262.MAC_an <- L262.MAC_an[ names( L262.MAC_an ) != "EPA_region" ]

printlog( "L262.MAC_TC_SSP1: Tech Change on MACCs for SSP1" )
L262.MAC_Ag_TC_SSP1 <- L262.AgMAC
L262.MAC_Ag_TC_SSP1$tech.change <- A_MACC_TechChange$tech_change[ match( paste( "SSP1", L262.MAC_Ag_TC_SSP1$mac.control ),
                                                                         paste( A_MACC_TechChange$scenario, A_MACC_TechChange$MAC ))]

L262.MAC_An_TC_SSP1 <- L262.MAC_an
L262.MAC_An_TC_SSP1$tech.change <- A_MACC_TechChange$tech_change[ match( paste( "SSP1", L262.MAC_An_TC_SSP1$mac.control ),
                                                                         paste( A_MACC_TechChange$scenario, A_MACC_TechChange$MAC ))]

printlog( "L262.MAC_TC_SSP2: Tech Change on MACCs for SSP2" )
L262.MAC_Ag_TC_SSP2 <- L262.AgMAC
L262.MAC_Ag_TC_SSP2$tech.change <- A_MACC_TechChange$tech_change[ match( paste( "SSP2", L262.MAC_Ag_TC_SSP1$mac.control ),
                                                                         paste( A_MACC_TechChange$scenario, A_MACC_TechChange$MAC ))]

L262.MAC_An_TC_SSP2 <- L262.MAC_an
L262.MAC_An_TC_SSP2$tech.change <- A_MACC_TechChange$tech_change[ match( paste( "SSP2", L262.MAC_An_TC_SSP1$mac.control ),
                                                                         paste( A_MACC_TechChange$scenario, A_MACC_TechChange$MAC ))]

printlog( "L262.MAC_TC_SSP5: Tech Change on MACCs for SSP5" )
L262.MAC_Ag_TC_SSP5 <- L262.AgMAC
L262.MAC_Ag_TC_SSP5$tech.change <- A_MACC_TechChange$tech_change[ match( paste( "SSP5", L262.MAC_Ag_TC_SSP1$mac.control ),
                                                                         paste( A_MACC_TechChange$scenario, A_MACC_TechChange$MAC ))]

L262.MAC_An_TC_SSP5 <- L262.MAC_an
L262.MAC_An_TC_SSP5$tech.change <- A_MACC_TechChange$tech_change[ match( paste( "SSP5", L262.MAC_An_TC_SSP1$mac.control ),
                                                                         paste( A_MACC_TechChange$scenario, A_MACC_TechChange$MAC ))]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L262.AgMAC, "AgMAC", "EMISSIONS_LEVEL2_DATA", "L262.AgMAC", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 
write_mi_data( L262.MAC_an, "MAC", "EMISSIONS_LEVEL2_DATA", "L262.MAC_an", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml" ) 

#This is the last file that refers to agricultural emissions
insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions_IRR.xml", "EMISSIONS_XML_FINAL", "all_aglu_emissions_IRR.xml", "", xml_tag="outFile" )

write_mi_data( L262.MAC_Ag_TC_SSP1, "AgMACTC", "EMISSIONS_LEVEL2_DATA", "L262.MAC_Ag_TC_SSP1", "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP1_IRR.xml" ) 
write_mi_data( L262.MAC_An_TC_SSP1, "MACTC", "EMISSIONS_LEVEL2_DATA", "L262.MAC_An_TC_SSP1", "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP1_IRR.xml" ) 
insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP1_IRR.xml", "EMISSIONS_XML_FINAL", "MACC_TC_SSP1_IRR.xml", "", xml_tag="outFile" )

write_mi_data( L262.MAC_Ag_TC_SSP2, "AgMACTC", "EMISSIONS_LEVEL2_DATA", "L262.MAC_Ag_TC_SSP2", "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP2_IRR.xml" ) 
write_mi_data( L262.MAC_An_TC_SSP2, "MACTC", "EMISSIONS_LEVEL2_DATA", "L262.MAC_An_TC_SSP2", "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP2_IRR.xml" ) 
insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP2_IRR.xml", "EMISSIONS_XML_FINAL", "MACC_TC_SSP2_IRR.xml", "", xml_tag="outFile" )

write_mi_data( L262.MAC_Ag_TC_SSP5, "AgMACTC", "EMISSIONS_LEVEL2_DATA", "L262.MAC_Ag_TC_SSP5", "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP5_IRR.xml" ) 
write_mi_data( L262.MAC_An_TC_SSP5, "MACTC", "EMISSIONS_LEVEL2_DATA", "L262.MAC_An_TC_SSP5", "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP5_IRR.xml" ) 
insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_MACC_TC_SSP5_IRR.xml", "EMISSIONS_XML_FINAL", "MACC_TC_SSP5_IRR.xml", "", xml_tag="outFile" )

logstop()
