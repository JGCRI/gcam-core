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
logstart( "L252.MACC.R" )
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
GCAM_sector_tech <- readdata( "EMISSIONS_MAPPINGS", "GCAM_sector_tech" )
L152.MAC_pct_R_S_Proc_EPA <- readdata( "EMISSIONS_LEVEL1_DATA", "L152.MAC_pct_R_S_Proc_EPA" )
L201.ghg_res <- readdata( "EMISSIONS_LEVEL2_DATA", "L201.ghg_res", skip = 4 )
L211.agr_emissions <- readdata( "EMISSIONS_LEVEL2_DATA", "L211.agr_emissions", skip = 4 )
L211.an_emissions <- readdata( "EMISSIONS_LEVEL2_DATA", "L211.an_emissions", skip = 4 )
L211.bio_emissions <- readdata( "EMISSIONS_LEVEL2_DATA", "L211.bio_emissions", skip = 4 )
L232.nonco2_prc <- readdata( "EMISSIONS_LEVEL2_DATA", "L232.nonco2_prc", skip = 4 )
L241.hfc_all <- readdata( "EMISSIONS_LEVEL2_DATA", "L241.hfc_all", skip = 4 )
L241.pfc_all <- readdata( "EMISSIONS_LEVEL2_DATA", "L241.pfc_all", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
printlog( "Prepare the table with all MAC curves for matching" )
L252.MAC_pct_R_S_Proc_EPA <- melt( L152.MAC_pct_R_S_Proc_EPA, id.vars = EPA_MACC_names, variable.name = "tax", value.name = "mac.reduction" )
L252.MAC_pct_R_S_Proc_EPA$tax <- as.numeric( sub( "X", "", L252.MAC_pct_R_S_Proc_EPA$tax ) )

printlog( "L252.ResMAC_fos: Fossil resource MAC curves" )
printlog( "NOTE: only applying the fossil resource MAC curves to the CH4 emissions")
L252.ResMAC_fos <- subset( L201.ghg_res[ c( "region", "depresource", "Non.CO2" ) ], Non.CO2 == "CH4" )
L252.ResMAC_fos$mac.control <- GCAM_sector_tech$EPA_MACC_Sector[
      match( paste( "out_resources", L252.ResMAC_fos$depresource ),
             paste( GCAM_sector_tech$sector, GCAM_sector_tech$subsector ) ) ]
L252.ResMAC_fos <- repeat_and_add_vector( L252.ResMAC_fos, "tax", MAC_taxes )
L252.ResMAC_fos <- L252.ResMAC_fos[ order( L252.ResMAC_fos$region, L252.ResMAC_fos$depresource ), ]
L252.ResMAC_fos$mac.reduction <- NA #until we have the EPA region for matching
L252.ResMAC_fos$EPA_region <- A_regions$MAC_region[ match( L252.ResMAC_fos$region, A_regions$region ) ]
L252.ResMAC_fos$mac.reduction <- round(
      L252.MAC_pct_R_S_Proc_EPA$mac.reduction[
         match( vecpaste( L252.ResMAC_fos[ c( "EPA_region", "mac.control", "tax" ) ] ),
                vecpaste( L252.MAC_pct_R_S_Proc_EPA[ c( "EPA_region", "Process", "tax" ) ] ) ) ],
      digits_MACC )
L252.ResMAC_fos <- na.omit( L252.ResMAC_fos )
# Add column for market variable
L252.ResMAC_fos$market.name <- MAC_Market
# Remove EPA_Region - useful up to now for diagnostic, but not needed for csv->xml conversion
L252.ResMAC_fos <- L252.ResMAC_fos[, names( L252.ResMAC_fos ) != "EPA_region" ]

printlog( "L252.AgMAC: Agricultural abatement (including bioenergy)" )
L252.AgMAC <- rbind(
      subset( L211.agr_emissions[ c( names_AgTechYr, "Non.CO2" ) ], year == min( L211.agr_emissions$year ) & Non.CO2 %in% ag_MACC_GHG_names ),
      subset( L211.bio_emissions[ c( names_AgTechYr, "Non.CO2" ) ], year == min( L211.agr_emissions$year ) & Non.CO2 %in% ag_MACC_GHG_names ) )
L252.AgMAC$mac.control <- GCAM_sector_tech$EPA_MACC_Sector[
      match( L252.AgMAC$AgSupplySector, GCAM_sector_tech$supplysector ) ]
#dropping jatropha b/c it isn't in the tech table, and wasn't considered in the epa analysis anyway
L252.AgMAC <- na.omit( L252.AgMAC )
L252.AgMAC <- repeat_and_add_vector( L252.AgMAC, "tax", MAC_taxes )
L252.AgMAC <- L252.AgMAC[ order( L252.AgMAC$region, L252.AgMAC$AgProductionTechnology ), ]
L252.AgMAC$mac.reduction <- NA #until we have the EPA region for matching
L252.AgMAC$EPA_region <- A_regions$MAC_region[ match( L252.AgMAC$region, A_regions$region ) ]
L252.AgMAC$mac.reduction <- round(
      L252.MAC_pct_R_S_Proc_EPA$mac.reduction[
         match( vecpaste( L252.AgMAC[ c( "EPA_region", "mac.control", "tax" ) ] ),
                vecpaste( L252.MAC_pct_R_S_Proc_EPA[ c( "EPA_region", "Process", "tax" ) ] ) ) ],
      digits_MACC )
L252.AgMAC <- na.omit( L252.AgMAC )
# Add column for market variable
L252.AgMAC$market.name <- MAC_Market
# Remove EPA_Region - useful up to now for diagnostic, but not needed for csv->xml conversion
L252.AgMAC <- L252.AgMAC[, names( L252.AgMAC ) != "EPA_region" ]

printlog( "L252.MAC_an: Abatement from animal production" )
L252.MAC_an <- subset( L211.an_emissions[ c( names_StubTechYr, "Non.CO2" ) ], year == min( L211.an_emissions$year ) & Non.CO2 %in% ag_MACC_GHG_names )
L252.MAC_an$mac.control <- GCAM_sector_tech$EPA_MACC_Sector[
      match( L252.MAC_an$supplysector, GCAM_sector_tech$supplysector ) ]
L252.MAC_an <- repeat_and_add_vector( L252.MAC_an, "tax", MAC_taxes )
L252.MAC_an <- L252.MAC_an[ order( L252.MAC_an$region, L252.MAC_an$supplysector, L252.MAC_an$subsector, L252.MAC_an$stub.technology, L252.MAC_an$Non.CO2 ), ]
L252.MAC_an$mac.reduction <- NA #until we have the EPA region for matching
L252.MAC_an$EPA_region <- A_regions$MAC_region[ match( L252.MAC_an$region, A_regions$region ) ]
L252.MAC_an$mac.reduction <- round(
      L252.MAC_pct_R_S_Proc_EPA$mac.reduction[
         match( vecpaste( L252.MAC_an[ c( "EPA_region", "mac.control", "tax" ) ] ),
                vecpaste( L252.MAC_pct_R_S_Proc_EPA[ c( "EPA_region", "Process", "tax" ) ] ) ) ],
      digits_MACC )
L252.MAC_an <- na.omit( L252.MAC_an )
# Add column for market variable
L252.MAC_an$market.name <- MAC_Market
# Remove EPA_Region - useful up to now for diagnostic, but not needed for csv->xml conversion
L252.MAC_an <- L252.MAC_an[, names( L252.MAC_an ) != "EPA_region" ]

printlog( "L252.MAC_prc: Abatement from industrial and urban processes" )
L252.MAC_prc <- subset( L232.nonco2_prc[ c( names_StubTechYr, "Non.CO2" ) ], year == min( L232.nonco2_prc$year ) & Non.CO2 %in% GHG_names )
L252.MAC_prc$mac.control <- GCAM_sector_tech$EPA_MACC_Sector[
      match( vecpaste( L252.MAC_prc[ c( supp, subs, "stub.technology" ) ] ),
             vecpaste( GCAM_sector_tech[ c( supp, subs, "stub.technology" ) ] ) ) ]
L252.MAC_prc <- repeat_and_add_vector( L252.MAC_prc, "tax", MAC_taxes )
L252.MAC_prc <- L252.MAC_prc[ order( L252.MAC_prc$region, L252.MAC_prc$supplysector, L252.MAC_prc$subsector, L252.MAC_prc$stub.technology, L252.MAC_prc$Non.CO2 ), ]
L252.MAC_prc$mac.reduction <- NA #until we have the EPA region for matching
L252.MAC_prc$EPA_region <- A_regions$MAC_region[ match( L252.MAC_prc$region, A_regions$region ) ]
L252.MAC_prc$mac.reduction <- round(
      L252.MAC_pct_R_S_Proc_EPA$mac.reduction[
         match( vecpaste( L252.MAC_prc[ c( "EPA_region", "mac.control", "tax" ) ] ),
                vecpaste( L252.MAC_pct_R_S_Proc_EPA[ c( "EPA_region", "Process", "tax" ) ] ) ) ],
      digits_MACC )
L252.MAC_prc <- na.omit( L252.MAC_prc )
# Add column for market variable
L252.MAC_prc$market.name <- MAC_Market
# Remove EPA_Region - useful up to now for diagnostic, but not needed for csv->xml conversion
L252.MAC_prc <- L252.MAC_prc[, names( L252.MAC_prc ) != "EPA_region" ]

printlog( "L252.MAC_higwp: Abatement from HFCs, PFCs, and SF6" )
L252.MAC_higwp <- rbind(
      subset( L241.hfc_all[ c( names_StubTechYr, "Non.CO2" ) ], year == min( L241.hfc_all$year ) ),
      subset( L241.pfc_all[ c( names_StubTechYr, "Non.CO2" ) ], year == min( L241.pfc_all$year ) ) )
L252.MAC_higwp$mac.control <- GCAM_sector_tech$EPA_MACC_Sector[
      match( vecpaste( L252.MAC_higwp[ c( supp, subs, "stub.technology" ) ] ),
             vecpaste( GCAM_sector_tech[ c( supp, subs, "stub.technology" ) ] ) ) ]
L252.MAC_higwp <- repeat_and_add_vector( L252.MAC_higwp, "tax", MAC_taxes )
L252.MAC_higwp <- L252.MAC_higwp[ order( L252.MAC_higwp$region, L252.MAC_higwp$supplysector, L252.MAC_higwp$subsector, L252.MAC_higwp$stub.technology, L252.MAC_higwp$Non.CO2 ), ]
L252.MAC_higwp$mac.reduction <- NA #until we have the EPA region for matching
L252.MAC_higwp$EPA_region <- A_regions$MAC_region[ match( L252.MAC_higwp$region, A_regions$region ) ]
L252.MAC_higwp$mac.reduction <- round(
      L252.MAC_pct_R_S_Proc_EPA$mac.reduction[
         match( vecpaste( L252.MAC_higwp[ c( "EPA_region", "mac.control", "tax" ) ] ),
                vecpaste( L252.MAC_pct_R_S_Proc_EPA[ c( "EPA_region", "Process", "tax" ) ] ) ) ],
      digits_MACC )
L252.MAC_higwp <- na.omit( L252.MAC_higwp )
# Add column for market variable
L252.MAC_higwp$market.name <- MAC_Market
# Remove EPA_Region - useful up to now for diagnostic, but not needed for csv->xml conversion
L252.MAC_higwp <- L252.MAC_higwp[, names( L252.MAC_higwp ) != "EPA_region" ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L252.ResMAC_fos, "ResMAC", "EMISSIONS_LEVEL2_DATA", "L252.ResMAC_fos", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 
write_mi_data( L252.AgMAC, "AgMAC", "EMISSIONS_LEVEL2_DATA", "L252.AgMAC", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 
write_mi_data( L252.MAC_an, "MAC", "EMISSIONS_LEVEL2_DATA", "L252.MAC_an", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 
write_mi_data( L252.MAC_prc, "MAC", "EMISSIONS_LEVEL2_DATA", "L252.MAC_prc", "EMISSIONS_XML_BATCH", "batch_ind_urb_processing_sectors.xml" ) 
write_mi_data( L252.MAC_higwp, "MAC", "EMISSIONS_LEVEL2_DATA", "L252.MAC_higwp", "EMISSIONS_XML_BATCH", "batch_all_fgas_emissions.xml" ) 

#This is the last file that refers to agricultural emissions
insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml", "EMISSIONS_XML_FINAL", "all_aglu_emissions.xml", "", xml_tag="outFile" )

logstop()
