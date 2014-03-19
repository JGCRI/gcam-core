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
logstart( "L221.en_mac.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Marginal Abatement Cost Curves for the energy system" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A_region <- readdata( "EMISSIONS_ASSUMPTIONS", "A_regions" )
GCAM_sector_tech <- readdata( "EMISSIONS_MAPPINGS", "GCAM_sector_tech" )
MAC_AdipicAcid <- readdata( "EMISSIONS_LEVEL0_DATA", "MAC_AdipicAcid" )
MAC_NitricAcid <- readdata( "EMISSIONS_LEVEL0_DATA", "MAC_NitricAcid" )
MAC_NatGas <- readdata( "EMISSIONS_LEVEL0_DATA", "MAC_NatGas" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
printlog( "Adipic Acid" )
#Prepare MAC data for use
L222.MAC_AdipicAcid <- melt ( MAC_AdipicAcid, id.vars=c( "region" ) )
L222.MAC_AdipicAcid$tax <- as.numeric( substr( L222.MAC_AdipicAcid$variable, 2, 5 ) ) 

#Clean up negative tax information
L222.MAC_AdipicAcid$tax[ L222.MAC_AdipicAcid$variable == "X.20.00" ] <- -20.00
L222.MAC_AdipicAcid$tax[ L222.MAC_AdipicAcid$variable == "X.10.00" ] <- -10.00
L222.MAC_AdipicAcid <- na.omit( L222.MAC_AdipicAcid ) 

#Find any sector that uses the Adipic Acid MAC
L222.MAC_AdipicAcid_S <- subset( GCAM_sector_tech, GCAM_sector_tech$MAC_type1 == "AdipicAcid"  ) 
L222.MAC_AdipicAcid_S <- L222.MAC_AdipicAcid_S[ names( L222.MAC_AdipicAcid_S ) %in% c( "sector", "fuel", "technology" ) ]
names( L222.MAC_AdipicAcid_S )[ names( L222.MAC_AdipicAcid_S ) == "sector" ] <- "supplysector"
names( L222.MAC_AdipicAcid_S )[ names( L222.MAC_AdipicAcid_S ) == "fuel" ] <- "subsector"
names( L222.MAC_AdipicAcid_S )[ names( L222.MAC_AdipicAcid_S ) == "technology" ] <- "stub.technology"

L222.MAC_AdipicAcid_S_R <- repeat_and_add_vector( L222.MAC_AdipicAcid_S, "region", GCAM_region_names$region )
L222.MAC_AdipicAcid_S_R_T <- repeat_and_add_vector( L222.MAC_AdipicAcid_S_R, "tax", MAC_taxes)

#Map in MAC region name
L222.MAC_AdipicAcid_S_R_T$MAC_region <- A_region$MAC_region[ match( L222.MAC_AdipicAcid_S_R_T$region, A_region$region )]

#Map in MAC reduction
L222.MAC_AdipicAcid_S_R_T$mac.reduction <- L222.MAC_AdipicAcid$value[ match( vecpaste( L222.MAC_AdipicAcid_S_R_T[ c( "MAC_region", "tax" ) ] ), vecpaste( L222.MAC_AdipicAcid[ c( "region", "tax" ) ] ) )]

#Drop missing values
L222.MAC_AdipicAcid_S_R_T <- na.omit( L222.MAC_AdipicAcid_S_R_T )

#Add extra columns and re-order
L222.MAC_AdipicAcid_S_R_T$year <- final_emiss_year
L222.MAC_AdipicAcid_S_R_T$Non.CO2 <- "N2O"
L222.MAC_AdipicAcid_S_R_T$name <- "Adipic Acid"
L222.MAC_AdipicAcid_S_R_T <- L222.MAC_AdipicAcid_S_R_T[ c( names_StubTechYr, "Non.CO2", "name", "tax", "mac.reduction" )]

printlog( "Nitric Acid" )
#Prepare MAC data for use
L222.MAC_NitricAcid <- melt ( MAC_NitricAcid, id.vars=c( "region" ) )
L222.MAC_NitricAcid$tax <- as.numeric( substr( L222.MAC_NitricAcid$variable, 2, 5 ) ) 

#Clean up negative tax information
L222.MAC_NitricAcid$tax[ L222.MAC_NitricAcid$variable == "X.20.00" ] <- -20.00
L222.MAC_NitricAcid$tax[ L222.MAC_NitricAcid$variable == "X.10.00" ] <- -10.00
L222.MAC_NitricAcid <- na.omit( L222.MAC_NitricAcid ) 

#Find any sector that uses the Nitric Acid MAC
L222.MAC_NitricAcid_S <- subset( GCAM_sector_tech, GCAM_sector_tech$MAC_type1 == "NitricAcid"  ) 
L222.MAC_NitricAcid_S <- L222.MAC_NitricAcid_S[ names( L222.MAC_NitricAcid_S ) %in% c( "sector", "fuel", "technology" ) ]
names( L222.MAC_NitricAcid_S )[ names( L222.MAC_NitricAcid_S ) == "sector" ] <- "supplysector"
names( L222.MAC_NitricAcid_S )[ names( L222.MAC_NitricAcid_S ) == "fuel" ] <- "subsector"
names( L222.MAC_NitricAcid_S )[ names( L222.MAC_NitricAcid_S ) == "technology" ] <- "stub.technology"

L222.MAC_NitricAcid_S_R <- repeat_and_add_vector( L222.MAC_NitricAcid_S, "region", GCAM_region_names$region )
L222.MAC_NitricAcid_S_R_T <- repeat_and_add_vector( L222.MAC_NitricAcid_S_R, "tax", MAC_taxes)

#Map in MAC region name
L222.MAC_NitricAcid_S_R_T$MAC_region <- A_region$MAC_region[ match( L222.MAC_NitricAcid_S_R_T$region, A_region$region )]

#Map in MAC reduction
L222.MAC_NitricAcid_S_R_T$mac.reduction <- L222.MAC_NitricAcid$value[ match( vecpaste( L222.MAC_NitricAcid_S_R_T[ c( "MAC_region", "tax" ) ] ), vecpaste( L222.MAC_NitricAcid[ c( "region", "tax" ) ] ) )]

#Drop missing values
L222.MAC_NitricAcid_S_R_T <- na.omit( L222.MAC_NitricAcid_S_R_T )

#Add extra columns and re-order
L222.MAC_NitricAcid_S_R_T$year <- final_emiss_year
L222.MAC_NitricAcid_S_R_T$Non.CO2 <- "N2O"
L222.MAC_NitricAcid_S_R_T$name <- "Nitric Acid"
L222.MAC_NitricAcid_S_R_T <- L222.MAC_NitricAcid_S_R_T[ c( names_StubTechYr, "Non.CO2", "name", "tax", "mac.reduction" )]

printlog( "Natural Gas" )
#Prepare MAC data for use
L222.MAC_NatGas <- melt ( MAC_NatGas, id.vars=c( "region" ) )
L222.MAC_NatGas$tax <- as.numeric( substr( L222.MAC_NatGas$variable, 2, 5 ) ) 

#Clean up negative tax information
L222.MAC_NatGas$tax[ L222.MAC_NatGas$variable == "X.20.00" ] <- -20.00
L222.MAC_NatGas$tax[ L222.MAC_NatGas$variable == "X.10.00" ] <- -10.00
L222.MAC_NatGas <- na.omit( L222.MAC_NatGas ) 

#Find any sector that uses the Nitric Acid MAC
L222.MAC_NatGas_S <- subset( GCAM_sector_tech, GCAM_sector_tech$MAC_type1 == "NatGas"  ) 
L222.MAC_NatGas_S <- L222.MAC_NatGas_S[ names( L222.MAC_NatGas_S ) %in% c( "sector", "fuel", "technology" ) ]
names( L222.MAC_NatGas_S )[ names( L222.MAC_NatGas_S ) == "fuel" ] <- "depresource"

L222.MAC_NatGas_S_R <- repeat_and_add_vector( L222.MAC_NatGas_S, "region", GCAM_region_names$region )
L222.MAC_NatGas_S_R_T <- repeat_and_add_vector( L222.MAC_NatGas_S_R, "tax", MAC_taxes)

#Map in MAC region name
L222.MAC_NatGas_S_R_T$MAC_region <- A_region$MAC_region[ match( L222.MAC_NatGas_S_R_T$region, A_region$region )]

#Map in MAC reduction
L222.MAC_NatGas_S_R_T$mac.reduction <- L222.MAC_NatGas$value[ match( vecpaste( L222.MAC_NatGas_S_R_T[ c( "MAC_region", "tax" ) ] ), vecpaste( L222.MAC_NatGas[ c( "region", "tax" ) ] ) )]

#Drop missing values
L222.MAC_NatGas_S_R_T <- na.omit( L222.MAC_NatGas_S_R_T )

#Add extra columns and re-order
L222.MAC_NatGas_S_R_T$Non.CO2 <- "CH4"
L222.MAC_NatGas_S_R_T$name <- "Natural Gas"
L222.MAC_NatGas_S_R_T <- L222.MAC_NatGas_S_R_T[ c( "region", "depresource", "Non.CO2", "name", "tax", "mac.reduction" )]


# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L222.MAC_AdipicAcid_S_R_T, "MAC", "EMISSIONS_LEVEL2_DATA", "L222.MAC_AdipicAcid_S_R_T", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 
write_mi_data( L222.MAC_NitricAcid_S_R_T, "MAC", "EMISSIONS_LEVEL2_DATA", "L222.MAC_NitricAcid_S_R_T", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 
write_mi_data( L222.MAC_NatGas_S_R_T, "ResMAC", "EMISSIONS_LEVEL2_DATA", "L222.MAC_NatGas_S_R_T", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 


logstop()
