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
MAC_AdipicAcid <- readdata( "EMISSIONS_LEVEL0_DATA", "MAC-NitricAdipic2030" ) # Note: maintaining separate processing in case these are split again in the future
MAC_NitricAcid <- readdata( "EMISSIONS_LEVEL0_DATA", "MAC-NitricAdipic2030" ) # Note: maintaining separate processing in case these are split again in the future
MAC_NatGas <- readdata( "EMISSIONS_LEVEL0_DATA", "MAC-OilGas2030" ) # Note: maintaining separate processing in case these are split again in the future
MAC_Coal <- readdata( "EMISSIONS_LEVEL0_DATA", "MAC-Coal2030" )
MAC_Oil <- readdata( "EMISSIONS_LEVEL0_DATA", "MAC-OilGas2030" ) # Note: maintaining separate processing in case these are split again in the future
MAC_Landfill <- readdata( "EMISSIONS_LEVEL0_DATA", "MAC-Landfill2030" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
printlog( "Adipic Acid" )
#Prepare MAC data for use
L221.MAC_AdipicAcid <- melt ( MAC_AdipicAcid, id.vars=c( "region" ) )
L221.MAC_AdipicAcid$tax <- as.numeric( substr( L221.MAC_AdipicAcid$variable, 2, 5 ) ) 

#Clean up negative tax information
L221.MAC_AdipicAcid$tax[ L221.MAC_AdipicAcid$variable == "X.24" ] <- -24.00
L221.MAC_AdipicAcid$tax[ L221.MAC_AdipicAcid$variable == "X.12" ] <- -12.00
L221.MAC_AdipicAcid$tax[ L221.MAC_AdipicAcid$variable == "X.242" ] <- 243.00
L221.MAC_AdipicAcid <- na.omit( L221.MAC_AdipicAcid ) 

#Find any sector that uses the Adipic Acid MAC
L221.MAC_AdipicAcid_S <- subset( GCAM_sector_tech, GCAM_sector_tech$MAC_type1 == "AdipicAcid"  ) 
L221.MAC_AdipicAcid_S <- L221.MAC_AdipicAcid_S[ names( L221.MAC_AdipicAcid_S ) %in% c( "sector", "fuel", "technology" ) ]
names( L221.MAC_AdipicAcid_S )[ names( L221.MAC_AdipicAcid_S ) == "sector" ] <- "supplysector"
names( L221.MAC_AdipicAcid_S )[ names( L221.MAC_AdipicAcid_S ) == "fuel" ] <- "subsector"
names( L221.MAC_AdipicAcid_S )[ names( L221.MAC_AdipicAcid_S ) == "technology" ] <- "stub.technology"

L221.MAC_AdipicAcid_S_R <- repeat_and_add_vector( L221.MAC_AdipicAcid_S, "region", GCAM_region_names$region )
L221.MAC_AdipicAcid_S_R_T <- repeat_and_add_vector( L221.MAC_AdipicAcid_S_R, "tax", MAC_taxes)

#Map in MAC reduction
L221.MAC_AdipicAcid_S_R_T$mac.reduction <- L221.MAC_AdipicAcid$value[ match( vecpaste( L221.MAC_AdipicAcid_S_R_T[ c( "region", "tax" ) ] ), vecpaste( L221.MAC_AdipicAcid[ c( "region", "tax" ) ] ) )]

#Drop missing values
L221.MAC_AdipicAcid_S_R_T <- na.omit( L221.MAC_AdipicAcid_S_R_T )

#Add extra columns and re-order
L221.MAC_AdipicAcid_S_R_T$year <- ctrl_base_year
L221.MAC_AdipicAcid_S_R_T$Non.CO2 <- "N2O"
L221.MAC_AdipicAcid_S_R_T$name <- "Adipic Acid"
L221.MAC_AdipicAcid_S_R_T <- L221.MAC_AdipicAcid_S_R_T[ c( names_StubTechYr, "Non.CO2", "name", "tax", "mac.reduction" )]

printlog( "Nitric Acid" )
#Prepare MAC data for use
L221.MAC_NitricAcid <- melt ( MAC_NitricAcid, id.vars=c( "region" ) )
L221.MAC_NitricAcid$tax <- as.numeric( substr( L221.MAC_NitricAcid$variable, 2, 5 ) ) 

#Clean up negative tax information
L221.MAC_NitricAcid$tax[ L221.MAC_NitricAcid$variable == "X.24" ] <- -24.00
L221.MAC_NitricAcid$tax[ L221.MAC_NitricAcid$variable == "X.12" ] <- -12.00
L221.MAC_NitricAcid$tax[ L221.MAC_NitricAcid$variable == "X.242" ] <- 243.00
L221.MAC_NitricAcid <- na.omit( L221.MAC_NitricAcid ) 

#Find any sector that uses the Nitric Acid MAC
L221.MAC_NitricAcid_S <- subset( GCAM_sector_tech, GCAM_sector_tech$MAC_type1 == "NitricAcid"  ) 
L221.MAC_NitricAcid_S <- L221.MAC_NitricAcid_S[ names( L221.MAC_NitricAcid_S ) %in% c( "sector", "fuel", "technology" ) ]
names( L221.MAC_NitricAcid_S )[ names( L221.MAC_NitricAcid_S ) == "sector" ] <- "supplysector"
names( L221.MAC_NitricAcid_S )[ names( L221.MAC_NitricAcid_S ) == "fuel" ] <- "subsector"
names( L221.MAC_NitricAcid_S )[ names( L221.MAC_NitricAcid_S ) == "technology" ] <- "stub.technology"

L221.MAC_NitricAcid_S_R <- repeat_and_add_vector( L221.MAC_NitricAcid_S, "region", GCAM_region_names$region )
L221.MAC_NitricAcid_S_R_T <- repeat_and_add_vector( L221.MAC_NitricAcid_S_R, "tax", MAC_taxes)

#Map in MAC reduction
L221.MAC_NitricAcid_S_R_T$mac.reduction <- L221.MAC_NitricAcid$value[ match( vecpaste( L221.MAC_NitricAcid_S_R_T[ c( "region", "tax" ) ] ), vecpaste( L221.MAC_NitricAcid[ c( "region", "tax" ) ] ) )]

#Drop missing values
L221.MAC_NitricAcid_S_R_T <- na.omit( L221.MAC_NitricAcid_S_R_T )

#Add extra columns and re-order
L221.MAC_NitricAcid_S_R_T$year <- ctrl_base_year
L221.MAC_NitricAcid_S_R_T$Non.CO2 <- "N2O"
L221.MAC_NitricAcid_S_R_T$name <- "Nitric Acid"
L221.MAC_NitricAcid_S_R_T <- L221.MAC_NitricAcid_S_R_T[ c( names_StubTechYr, "Non.CO2", "name", "tax", "mac.reduction" )]

printlog( "Landfills" )
#Prepare MAC data for use
L221.MAC_Landfill <- melt ( MAC_Landfill, id.vars=c( "region" ) )
L221.MAC_Landfill$tax <- as.numeric( substr( L221.MAC_Landfill$variable, 2, 5 ) ) 

#Clean up negative tax information
L221.MAC_Landfill$tax[ L221.MAC_Landfill$variable == "X.24" ] <- -24.00
L221.MAC_Landfill$tax[ L221.MAC_Landfill$variable == "X.12" ] <- -12.00
L221.MAC_Landfill$tax[ L221.MAC_Landfill$variable == "X.242" ] <- 243.00
L221.MAC_Landfill <- na.omit( L221.MAC_Landfill ) 

#Find any sector that uses the Landfill MAC
L221.MAC_Landfill_S <- subset( GCAM_sector_tech, GCAM_sector_tech$MAC_type1 == "Landfill"  ) 
L221.MAC_Landfill_S <- L221.MAC_Landfill_S[ names( L221.MAC_Landfill_S ) %in% c( "sector", "fuel", "technology" ) ]
names( L221.MAC_Landfill_S )[ names( L221.MAC_Landfill_S ) == "sector" ] <- "supplysector"
names( L221.MAC_Landfill_S )[ names( L221.MAC_Landfill_S ) == "fuel" ] <- "subsector"
names( L221.MAC_Landfill_S )[ names( L221.MAC_Landfill_S ) == "technology" ] <- "stub.technology"

L221.MAC_Landfill_S_R <- repeat_and_add_vector( L221.MAC_Landfill_S, "region", GCAM_region_names$region )
L221.MAC_Landfill_S_R_T <- repeat_and_add_vector( L221.MAC_Landfill_S_R, "tax", MAC_taxes)

#Map in MAC reduction
L221.MAC_Landfill_S_R_T$mac.reduction <- L221.MAC_Landfill$value[ match( vecpaste( L221.MAC_Landfill_S_R_T[ c( "region", "tax" ) ] ), vecpaste( L221.MAC_Landfill[ c( "region", "tax" ) ] ) )]

#Drop missing values
L221.MAC_Landfill_S_R_T <- na.omit( L221.MAC_Landfill_S_R_T )

#Add extra columns and re-order
L221.MAC_Landfill_S_R_T$year <- ctrl_base_year
L221.MAC_Landfill_S_R_T$Non.CO2 <- "CH4"
L221.MAC_Landfill_S_R_T$name <- "Landfill"
L221.MAC_Landfill_S_R_T <- L221.MAC_Landfill_S_R_T[ c( names_StubTechYr, "Non.CO2", "name", "tax", "mac.reduction" )]

printlog( "Natural Gas" )
#Prepare MAC data for use
L221.MAC_NatGas <- melt ( MAC_NatGas, id.vars=c( "region" ) )
L221.MAC_NatGas$tax <- as.numeric( substr( L221.MAC_NatGas$variable, 2, 5 ) ) 

#Clean up negative tax information
L221.MAC_NatGas$tax[ L221.MAC_NatGas$variable == "X.24" ] <- -24.00
L221.MAC_NatGas$tax[ L221.MAC_NatGas$variable == "X.12" ] <- -12.00
L221.MAC_NatGas$tax[ L221.MAC_NatGas$variable == "X.242" ] <- 243.00
L221.MAC_NatGas <- na.omit( L221.MAC_NatGas ) 

#Find any sector that uses the Natural Gas MAC
L221.MAC_NatGas_S <- subset( GCAM_sector_tech, GCAM_sector_tech$MAC_type1 == "NatGas"  ) 
L221.MAC_NatGas_S <- L221.MAC_NatGas_S[ names( L221.MAC_NatGas_S ) %in% c( "sector", "fuel", "technology" ) ]
names( L221.MAC_NatGas_S )[ names( L221.MAC_NatGas_S ) == "fuel" ] <- "depresource"

L221.MAC_NatGas_S_R <- repeat_and_add_vector( L221.MAC_NatGas_S, "region", GCAM_region_names$region )
L221.MAC_NatGas_S_R_T <- repeat_and_add_vector( L221.MAC_NatGas_S_R, "tax", MAC_taxes)

#Map in MAC reduction
L221.MAC_NatGas_S_R_T$mac.reduction <- L221.MAC_NatGas$value[ match( vecpaste( L221.MAC_NatGas_S_R_T[ c( "region", "tax" ) ] ), vecpaste( L221.MAC_NatGas[ c( "region", "tax" ) ] ) )]

#Drop missing values
L221.MAC_NatGas_S_R_T <- na.omit( L221.MAC_NatGas_S_R_T )

#Add extra columns and re-order
L221.MAC_NatGas_S_R_T$Non.CO2 <- "CH4"
L221.MAC_NatGas_S_R_T$name <- "Natural Gas"
L221.MAC_NatGas_S_R_T <- L221.MAC_NatGas_S_R_T[ c( "region", "depresource", "Non.CO2", "name", "tax", "mac.reduction" )]

printlog( "Coal" )
#Prepare MAC data for use
L221.MAC_Coal <- melt ( MAC_Coal, id.vars=c( "region" ) )
L221.MAC_Coal$tax <- as.numeric( substr( L221.MAC_Coal$variable, 2, 5 ) ) 

#Clean up negative tax information
L221.MAC_Coal$tax[ L221.MAC_Coal$variable == "X.24" ] <- -24.00
L221.MAC_Coal$tax[ L221.MAC_Coal$variable == "X.12" ] <- -12.00
L221.MAC_Coal$tax[ L221.MAC_Coal$variable == "X.242" ] <- 243.00
L221.MAC_Coal <- na.omit( L221.MAC_Coal ) 

#Find any sector that uses the Coal MAC
L221.MAC_Coal_S <- subset( GCAM_sector_tech, GCAM_sector_tech$MAC_type1 == "Coal"  ) 
L221.MAC_Coal_S <- L221.MAC_Coal_S[ names( L221.MAC_Coal_S ) %in% c( "sector", "fuel", "technology" ) ]
names( L221.MAC_Coal_S )[ names( L221.MAC_Coal_S ) == "fuel" ] <- "depresource"

L221.MAC_Coal_S_R <- repeat_and_add_vector( L221.MAC_Coal_S, "region", GCAM_region_names$region )
L221.MAC_Coal_S_R_T <- repeat_and_add_vector( L221.MAC_Coal_S_R, "tax", MAC_taxes)

#Map in MAC reduction
L221.MAC_Coal_S_R_T$mac.reduction <- L221.MAC_Coal$value[ match( vecpaste( L221.MAC_Coal_S_R_T[ c( "region", "tax" ) ] ), vecpaste( L221.MAC_Coal[ c( "region", "tax" ) ] ) )]

#Drop missing values
L221.MAC_Coal_S_R_T <- na.omit( L221.MAC_Coal_S_R_T )

#Add extra columns and re-order
L221.MAC_Coal_S_R_T$Non.CO2 <- "CH4"
L221.MAC_Coal_S_R_T$name <- "Coal"
L221.MAC_Coal_S_R_T <- L221.MAC_Coal_S_R_T[ c( "region", "depresource", "Non.CO2", "name", "tax", "mac.reduction" )]

printlog( "Oil" )
#Prepare MAC data for use
L221.MAC_Oil <- melt ( MAC_Oil, id.vars=c( "region" ) )
L221.MAC_Oil$tax <- as.numeric( substr( L221.MAC_Oil$variable, 2, 5 ) ) 

#Clean up negative & greater than tax information
L221.MAC_Oil$tax[ L221.MAC_Oil$variable == "X.24" ] <- -24.00
L221.MAC_Oil$tax[ L221.MAC_Oil$variable == "X.12" ] <- -12.00
L221.MAC_Oil$tax[ L221.MAC_Oil$variable == "X.242" ] <- 243.00
L221.MAC_Oil <- na.omit( L221.MAC_Oil ) 

#Find any sector that uses the Coal MAC
L221.MAC_Oil_S <- subset( GCAM_sector_tech, GCAM_sector_tech$MAC_type1 == "Oil"  ) 
L221.MAC_Oil_S <- L221.MAC_Oil_S[ names( L221.MAC_Oil_S ) %in% c( "sector", "fuel", "technology" ) ]
names( L221.MAC_Oil_S )[ names( L221.MAC_Oil_S ) == "fuel" ] <- "depresource"

L221.MAC_Oil_S_R <- repeat_and_add_vector( L221.MAC_Oil_S, "region", GCAM_region_names$region )
L221.MAC_Oil_S_R_T <- repeat_and_add_vector( L221.MAC_Oil_S_R, "tax", MAC_taxes)

#Map in MAC reduction
L221.MAC_Oil_S_R_T$mac.reduction <- L221.MAC_Oil$value[ match( vecpaste( L221.MAC_Oil_S_R_T[ c( "region", "tax" ) ] ), vecpaste( L221.MAC_Oil[ c( "region", "tax" ) ] ) )]

#Drop missing values
L221.MAC_Oil_S_R_T <- na.omit( L221.MAC_Oil_S_R_T )

#Add extra columns and re-order
L221.MAC_Oil_S_R_T$Non.CO2 <- "CH4"
L221.MAC_Oil_S_R_T$name <- "Oil"
L221.MAC_Oil_S_R_T <- L221.MAC_Oil_S_R_T[ c( "region", "depresource", "Non.CO2", "name", "tax", "mac.reduction" )]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L221.MAC_AdipicAcid_S_R_T, "MAC", "EMISSIONS_LEVEL2_DATA", "L221.MAC_AdipicAcid_S_R_T", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 
write_mi_data( L221.MAC_NitricAcid_S_R_T, "MAC", "EMISSIONS_LEVEL2_DATA", "L221.MAC_NitricAcid_S_R_T", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 
write_mi_data( L221.MAC_Landfill_S_R_T, "MAC", "EMISSIONS_LEVEL2_DATA", "L221.MAC_Landfilllog_S_R_T", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 
write_mi_data( L221.MAC_NatGas_S_R_T, "ResMAC", "EMISSIONS_LEVEL2_DATA", "L221.MAC_NatGas_S_R_T", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 
write_mi_data( L221.MAC_Coal_S_R_T, "ResMAC", "EMISSIONS_LEVEL2_DATA", "L221.MAC_Coal_S_R_T", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 
write_mi_data( L221.MAC_Oil_S_R_T, "ResMAC", "EMISSIONS_LEVEL2_DATA", "L221.MAC_Oil_S_R_T", "EMISSIONS_XML_BATCH", "batch_all_energy_emissions.xml" ) 


logstop()
