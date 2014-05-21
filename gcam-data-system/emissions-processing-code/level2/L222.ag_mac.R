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
logstart( "L222.ag_mac.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions in the aglu system" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )
A_biocrops_R_AEZ <- readdata( "AGLU_ASSUMPTIONS", "A_biocrops_R_AEZ" )
A_region <- readdata( "EMISSIONS_ASSUMPTIONS", "A_regions" )
GCAM_sector_tech <- readdata( "EMISSIONS_MAPPINGS", "GCAM_sector_tech" )
MM_MAC <- readdata( "EMISSIONS_LEVEL0_DATA", "MAC_ManureManagement" )
N2O_MAC <- readdata( "EMISSIONS_LEVEL0_DATA", "MAC_N2O" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
printlog( "Manure management" )
#Prepare MAC data for use
L222.MM_MAC <- melt ( MM_MAC, id.vars=c( "region" ) )
L222.MM_MAC$tax <- as.numeric( substr( L222.MM_MAC$variable, 2, 5 ) ) 

#Clean up negative tax information
L222.MM_MAC$tax[ L222.MM_MAC$variable == "X.20.00" ] <- -20.00
L222.MM_MAC$tax[ L222.MM_MAC$variable == "X.10.00" ] <- -10.00
L222.MM_MAC <- na.omit( L222.MM_MAC ) 

#Find any sector that uses the manure management MAC
L222.MM_MAC_S <- subset( GCAM_sector_tech, GCAM_sector_tech$MAC_type1 == "Manure Management"  ) 
L222.MM_MAC_S <- L222.MM_MAC_S[ names( L222.MM_MAC_S ) %in% c( "sector", "fuel", "technology" ) ]
names( L222.MM_MAC_S )[ names( L222.MM_MAC_S ) == "sector" ] <- "supplysector"
names( L222.MM_MAC_S )[ names( L222.MM_MAC_S ) == "fuel" ] <- "subsector"
names( L222.MM_MAC_S )[ names( L222.MM_MAC_S ) == "technology" ] <- "stub.technology"

L222.MM_MAC_S_R <- repeat_and_add_vector( L222.MM_MAC_S, "region", GCAM_region_names$region )
L222.MM_MAC_S_R_T <- repeat_and_add_vector( L222.MM_MAC_S_R, "tax", MAC_taxes)

#Map in MAC region name
L222.MM_MAC_S_R_T$MAC_region <- A_region$MAC_region[ match( L222.MM_MAC_S_R_T$region, A_region$region )]

#Map in MAC reduction
L222.MM_MAC_S_R_T$mac.reduction <- L222.MM_MAC$value[ match( vecpaste( L222.MM_MAC_S_R_T[ c( "MAC_region", "tax" ) ] ), vecpaste( L222.MM_MAC[ c( "region", "tax" ) ] ) )]

#Drop missing values
L222.MM_MAC_S_R_T <- na.omit( L222.MM_MAC_S_R_T )

#Add extra columns and re-order
L222.MM_MAC_S_R_T$year <- ctrl_base_year
L222.MM_MAC_S_R_T$Non.CO2 <- "CH4_AGR"
L222.MM_MAC_S_R_T$name <- "Manure Management"
L222.MM_MAC_S_R_T <- L222.MM_MAC_S_R_T[ c( names_StubTechYr, "Non.CO2", "name", "tax", "mac.reduction" )]

printlog( "N2O" )
#Prepare MAC data for use
L222.N2O_MAC <- melt ( N2O_MAC, id.vars=c( "region" ) )
L222.N2O_MAC$tax <- as.numeric( substr( L222.N2O_MAC $variable, 2, 5 ) ) 

#Clean up negative tax information
L222.N2O_MAC$tax[ L222.N2O_MAC$variable == "X.30.00" ] <- -30.00
L222.N2O_MAC$tax[ L222.N2O_MAC$variable == "X.15.00" ] <- -15.00
L222.N2O_MAC <- na.omit( L222.N2O_MAC ) 

#Find any sector that uses the manure management MAC
L222.N2O_MAC_S <- subset( GCAM_sector_tech, GCAM_sector_tech$MAC_type1 == "N2O"  ) 
L222.N2O_MAC_S <- L222.N2O_MAC_S[ names( L222.N2O_MAC_S) %in% c( "sector", "fuel", "technology" ) ]
names( L222.N2O_MAC_S )[ names( L222.N2O_MAC_S ) == "sector" ] <- "AgSupplySector"
names( L222.N2O_MAC_S )[ names( L222.N2O_MAC_S ) == "fuel" ] <- "AgSupplySubsector"
names( L222.N2O_MAC_S )[ names( L222.N2O_MAC_S ) == "technology" ] <- "AgProductionTechnology"

L222.N2O_MAC_S_R <- repeat_and_add_vector( L222.N2O_MAC_S, "region", GCAM_region_names$region )
L222.N2O_MAC_S_R_T <- repeat_and_add_vector( L222.N2O_MAC_S_R, "tax", MAC_taxes)

#Map in MAC region name
L222.N2O_MAC_S_R_T$MAC_region <- A_region$MAC_region[ match( L222.N2O_MAC_S_R_T$region, A_region$region )]

#Map in MAC reduction
L222.N2O_MAC_S_R_T$mac.reduction <- L222.N2O_MAC$value[ match( vecpaste( L222.N2O_MAC_S_R_T[ c( "MAC_region", "tax" ) ] ), vecpaste( L222.N2O_MAC[ c( "region", "tax" ) ] ) )]

#Drop missing values
L222.N2O_MAC_S_R_T <- na.omit( L222.N2O_MAC_S_R_T )

#Add extra columns and re-order
L222.N2O_MAC_S_R_T$year <- ctrl_base_year
L222.N2O_MAC_S_R_T$Non.CO2 <- "N2O_AGR"
L222.N2O_MAC_S_R_T$name <- "N2O"
L222.N2O_MAC_S_R_T <- L222.N2O_MAC_S_R_T[ c( names_AgTech, "year", "Non.CO2", "name", "tax", "mac.reduction" )]

L222.N2O_MAC_S_R_T <- rename_biocrops( L222.N2O_MAC_S_R_T, lookup = A_biocrops_R_AEZ, data_matchvar = "AgSupplySubsector",
                                lookup_matchvar = "old_AgSupplySubsector", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )

printlog( "Removing non-existent regions and AEZs from all tables")
L222.N2O_MAC_S_R_T <- remove_AEZ_nonexist( L222.N2O_MAC_S_R_T )
L222.MM_MAC_S_R_T <- subset( L222.MM_MAC_S_R_T, !region %in% no_aglu_regions )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L222.MM_MAC_S_R_T, "MAC", "EMISSIONS_LEVEL2_DATA", "L222.MM_MAC_S_R_T", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 
write_mi_data( L222.N2O_MAC_S_R_T, "AgMAC", "EMISSIONS_LEVEL2_DATA", "L222.N2O_MAC_S_R_T", "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml" ) 

insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_all_aglu_emissions.xml", "EMISSIONS_XML_FINAL", "all_aglu_emissions.xml", "", xml_tag="outFile" )

logstop()
