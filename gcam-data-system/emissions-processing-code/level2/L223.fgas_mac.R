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
logstart( "L223.fgas_mac.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Marginal Abatement Cost Curves for F-Gases" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A_region <- readdata( "EMISSIONS_ASSUMPTIONS", "A_regions" )
GCAM_sector_tech <- readdata( "EMISSIONS_MAPPINGS", "gcam_fgas_tech" )
MAC_Aluminum <- readdata( "EMISSIONS_LEVEL0_DATA", "MAC-Aluminum2030" )
MAC_ElectTD <- readdata( "EMISSIONS_LEVEL0_DATA", "MAC-EPS2030" )
MAC_Foams <- readdata( "EMISSIONS_LEVEL0_DATA", "MAC-Foams2030" )
MAC_HFC23 <- readdata( "EMISSIONS_LEVEL0_DATA", "MAC-HFC232030" )
MAC_Refrigeration <- readdata( "EMISSIONS_LEVEL0_DATA", "MAC-Cooling2030" )
MAC_Semiconductors <- readdata( "EMISSIONS_LEVEL0_DATA", "MAC-Semi2030" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
printlog( "Aluminum" )
#Prepare MAC data for use
L223.MAC_Aluminum <- melt ( MAC_Aluminum, id.vars=c( "region" ) )
L223.MAC_Aluminum$tax <- as.numeric( substr( L223.MAC_Aluminum$variable, 2, 5 ) ) 

#Clean up negative tax information
L223.MAC_Aluminum$tax[ L223.MAC_Aluminum$variable == "X.24" ] <- -24.00
L223.MAC_Aluminum$tax[ L223.MAC_Aluminum$variable == "X.12" ] <- -12.00
L223.MAC_Aluminum$tax[ L223.MAC_Aluminum$variable == "X.242" ] <- 243.00
L223.MAC_Aluminum <- na.omit( L223.MAC_Aluminum ) 

#Find any sector that uses the Aluminum MAC
L223.MAC_Aluminum_S <- subset( GCAM_sector_tech, GCAM_sector_tech$MAC_type1 == "Aluminum"  ) 
L223.MAC_Aluminum_S <- L223.MAC_Aluminum_S[ names( L223.MAC_Aluminum_S ) %in% c( "supplysector", "subsector", "stub.technology" ) ]
L223.MAC_Aluminum_S_R <- repeat_and_add_vector( L223.MAC_Aluminum_S, "region", GCAM_region_names$region )
L223.MAC_Aluminum_S_R_T <- repeat_and_add_vector( L223.MAC_Aluminum_S_R, "tax", MAC_taxes)

#Map in MAC reduction
L223.MAC_Aluminum_S_R_T$mac.reduction <- L223.MAC_Aluminum$value[ match( vecpaste( L223.MAC_Aluminum_S_R_T[ c( "region", "tax" ) ] ), vecpaste( L223.MAC_Aluminum[ c( "region", "tax" ) ] ) )]

#Drop missing values
L223.MAC_Aluminum_S_R_T <- na.omit( L223.MAC_Aluminum_S_R_T )

#Add extra columns and re-order
L223.MAC_Aluminum_S_R_T$year <- ctrl_base_year
L223.MAC_Aluminum_S_R_T <- repeat_and_add_vector( L223.MAC_Aluminum_S_R_T, "Non.CO2", c( "CF4", "C2F6" ) )
L223.MAC_Aluminum_S_R_T$name <- "Aluminum"
L223.MAC_Aluminum_S_R_T <- L223.MAC_Aluminum_S_R_T[ c( names_StubTechYr, "Non.CO2", "name", "tax", "mac.reduction" )]

printlog( "ElectTD" )
#Prepare MAC data for use
L223.MAC_ElectTD <- melt ( MAC_ElectTD, id.vars=c( "region" ) )
L223.MAC_ElectTD$tax <- as.numeric( substr( L223.MAC_ElectTD$variable, 2, 5 ) ) 

#Clean up negative tax information
L223.MAC_ElectTD$tax[ L223.MAC_ElectTD$variable == "X.24" ] <- -24.00
L223.MAC_ElectTD$tax[ L223.MAC_ElectTD$variable == "X.12" ] <- -12.00
L223.MAC_ElectTD$tax[ L223.MAC_ElectTD$variable == "X.242" ] <- 243.00
L223.MAC_ElectTD <- na.omit( L223.MAC_ElectTD ) 

#Find any sector that uses the ElectTD MAC
L223.MAC_ElectTD_S <- subset( GCAM_sector_tech, GCAM_sector_tech$MAC_type1 == "ElectTD"  ) 
L223.MAC_ElectTD_S <- L223.MAC_ElectTD_S[ names( L223.MAC_ElectTD_S ) %in% c( "supplysector", "subsector", "stub.technology" ) ]
L223.MAC_ElectTD_S_R <- repeat_and_add_vector( L223.MAC_ElectTD_S, "region", GCAM_region_names$region )
L223.MAC_ElectTD_S_R_T <- repeat_and_add_vector( L223.MAC_ElectTD_S_R, "tax", MAC_taxes)

#Map in MAC reduction
L223.MAC_ElectTD_S_R_T$mac.reduction <- L223.MAC_ElectTD$value[ match( vecpaste( L223.MAC_ElectTD_S_R_T[ c( "region", "tax" ) ] ), vecpaste( L223.MAC_ElectTD[ c( "region", "tax" ) ] ) )]

#Drop missing values
L223.MAC_ElectTD_S_R_T <- na.omit( L223.MAC_ElectTD_S_R_T )

#Add extra columns and re-order
L223.MAC_ElectTD_S_R_T$year <- ctrl_base_year
L223.MAC_ElectTD_S_R_T$Non.CO2 <- "SF6"
L223.MAC_ElectTD_S_R_T$name <- "ElectTD"
L223.MAC_ElectTD_S_R_T <- L223.MAC_ElectTD_S_R_T[ c( names_StubTechYr, "Non.CO2", "name", "tax", "mac.reduction" )]

printlog( "Foams" )
#Prepare MAC data for use
L223.MAC_Foams <- melt ( MAC_Foams, id.vars=c( "region" ) )
L223.MAC_Foams$tax <- as.numeric( substr( L223.MAC_Foams$variable, 2, 5 ) ) 

#Clean up negative tax information
L223.MAC_Foams$tax[ L223.MAC_Foams$variable == "X.24" ] <- -24.00
L223.MAC_Foams$tax[ L223.MAC_Foams$variable == "X.12" ] <- -12.00
L223.MAC_Foams$tax[ L223.MAC_Foams$variable == "X.242" ] <- 243.00
L223.MAC_Foams <- na.omit( L223.MAC_Foams ) 

#Find any sector that uses the Foams MAC
L223.MAC_Foams_S <- subset( GCAM_sector_tech, GCAM_sector_tech$MAC_type1 == "Foams"  ) 
L223.MAC_Foams_S <- L223.MAC_Foams_S[ names( L223.MAC_Foams_S ) %in% c( "supplysector", "subsector", "stub.technology" ) ]
L223.MAC_Foams_S_R <- repeat_and_add_vector( L223.MAC_Foams_S, "region", GCAM_region_names$region )
L223.MAC_Foams_S_R_T <- repeat_and_add_vector( L223.MAC_Foams_S_R, "tax", MAC_taxes)

#Map in MAC reduction
L223.MAC_Foams_S_R_T$mac.reduction <- L223.MAC_Foams$value[ match( vecpaste( L223.MAC_Foams_S_R_T[ c( "region", "tax" ) ] ), vecpaste( L223.MAC_Foams[ c( "region", "tax" ) ] ) )]

#Drop missing values
L223.MAC_Foams_S_R_T <- na.omit( L223.MAC_Foams_S_R_T )

#Add extra columns and re-order
L223.MAC_Foams_S_R_T$year <- ctrl_base_year
L223.MAC_Foams_S_R_T <- repeat_and_add_vector( L223.MAC_Foams_S_R_T, "Non.CO2", c( "HFC125", "HFC134a", "HFC143a", "HFC23", "HFC236fa", "HFC32" ) )
L223.MAC_Foams_S_R_T$name <- "Foams"
L223.MAC_Foams_S_R_T <- L223.MAC_Foams_S_R_T[ c( names_StubTechYr, "Non.CO2", "name", "tax", "mac.reduction" )]

printlog( "HFC23" )
#Prepare MAC data for use
L223.MAC_HFC23 <- melt ( MAC_HFC23, id.vars=c( "region" ) )
L223.MAC_HFC23$tax <- as.numeric( substr( L223.MAC_HFC23$variable, 2, 5 ) ) 

#Clean up negative tax information
L223.MAC_HFC23$tax[ L223.MAC_HFC23$variable == "X.24" ] <- -24.00
L223.MAC_HFC23$tax[ L223.MAC_HFC23$variable == "X.12" ] <- -12.00
L223.MAC_HFC23$tax[ L223.MAC_HFC23$variable == "X.242" ] <- 243.00
L223.MAC_HFC23 <- na.omit( L223.MAC_HFC23 ) 

#Find any sector that uses the HFC23 MAC
L223.MAC_HFC23_S <- subset( GCAM_sector_tech, GCAM_sector_tech$MAC_type1 == "HFC23"  ) 
L223.MAC_HFC23_S <- L223.MAC_HFC23_S[ names( L223.MAC_HFC23_S ) %in% c( "supplysector", "subsector", "stub.technology" ) ]
L223.MAC_HFC23_S_R <- repeat_and_add_vector( L223.MAC_HFC23_S, "region", GCAM_region_names$region )
L223.MAC_HFC23_S_R_T <- repeat_and_add_vector( L223.MAC_HFC23_S_R, "tax", MAC_taxes)

#Map in MAC reduction
L223.MAC_HFC23_S_R_T$mac.reduction <- L223.MAC_HFC23$value[ match( vecpaste( L223.MAC_HFC23_S_R_T[ c( "region", "tax" ) ] ), vecpaste( L223.MAC_HFC23[ c( "region", "tax" ) ] ) )]

#Drop missing values
L223.MAC_HFC23_S_R_T <- na.omit( L223.MAC_HFC23_S_R_T )

#Add extra columns and re-order
L223.MAC_HFC23_S_R_T$year <- ctrl_base_year
L223.MAC_HFC23_S_R_T$Non.CO2 <- "HFC23"
L223.MAC_HFC23_S_R_T$name <- "HFC23"
L223.MAC_HFC23_S_R_T <- L223.MAC_HFC23_S_R_T[ c( names_StubTechYr, "Non.CO2", "name", "tax", "mac.reduction" )]

printlog( "Refrigeration" )
#Prepare MAC data for use
L223.MAC_Refrigeration <- melt ( MAC_Refrigeration, id.vars=c( "region" ) )
L223.MAC_Refrigeration$tax <- as.numeric( substr( L223.MAC_Refrigeration$variable, 2, 5 ) ) 

#Clean up negative tax information
L223.MAC_Refrigeration$tax[ L223.MAC_Refrigeration$variable == "X.24" ] <- -24.00
L223.MAC_Refrigeration$tax[ L223.MAC_Refrigeration$variable == "X.12" ] <- -12.00
L223.MAC_Refrigeration$tax[ L223.MAC_Refrigeration$variable == "X.242" ] <- 243.00
L223.MAC_Refrigeration <- na.omit( L223.MAC_Refrigeration ) 

#Find any sector that uses the Refrigeration MAC
L223.MAC_Refrigeration_S <- subset( GCAM_sector_tech, GCAM_sector_tech$MAC_type1 == "Refrigeration"  ) 
L223.MAC_Refrigeration_S <- L223.MAC_Refrigeration_S[ names( L223.MAC_Refrigeration_S ) %in% c( "supplysector", "subsector", "stub.technology" ) ]
L223.MAC_Refrigeration_S_R <- repeat_and_add_vector( L223.MAC_Refrigeration_S, "region", GCAM_region_names$region )
L223.MAC_Refrigeration_S_R_T <- repeat_and_add_vector( L223.MAC_Refrigeration_S_R, "tax", MAC_taxes)

#Map in MAC reduction
L223.MAC_Refrigeration_S_R_T$mac.reduction <- L223.MAC_Refrigeration$value[ match( vecpaste( L223.MAC_Refrigeration_S_R_T[ c( "region", "tax" ) ] ), vecpaste( L223.MAC_Refrigeration[ c( "region", "tax" ) ] ) )]

#Drop missing values
L223.MAC_Refrigeration_S_R_T <- na.omit( L223.MAC_Refrigeration_S_R_T )

#Add extra columns and re-order
L223.MAC_Refrigeration_S_R_T$year <- ctrl_base_year
L223.MAC_Refrigeration_S_R_T <- repeat_and_add_vector( L223.MAC_Refrigeration_S_R_T, "Non.CO2", c( "HFC125", "HFC134a", "HFC143a", "HFC236fa", "HFC32" ) )
L223.MAC_Refrigeration_S_R_T$name <- "Refrigeration"
L223.MAC_Refrigeration_S_R_T <- L223.MAC_Refrigeration_S_R_T[ c( names_StubTechYr, "Non.CO2", "name", "tax", "mac.reduction" )]

printlog( "Semiconductors" )
#Prepare MAC data for use
L223.MAC_Semiconductors <- melt ( MAC_Semiconductors, id.vars=c( "region" ) )
L223.MAC_Semiconductors$tax <- as.numeric( substr( L223.MAC_Semiconductors$variable, 2, 5 ) ) 

#Clean up negative tax information
L223.MAC_Semiconductors$tax[ L223.MAC_Semiconductors$variable == "X.24" ] <- -24.00
L223.MAC_Semiconductors$tax[ L223.MAC_Semiconductors$variable == "X.12" ] <- -12.00
L223.MAC_Semiconductors$tax[ L223.MAC_Semiconductors$variable == "X.242" ] <- 243.00
L223.MAC_Semiconductors <- na.omit( L223.MAC_Semiconductors ) 

#Find any sector that uses the Semiconductors MAC
L223.MAC_Semiconductors_S <- subset( GCAM_sector_tech, GCAM_sector_tech$MAC_type1 == "Semiconductors"  ) 
L223.MAC_Semiconductors_S <- L223.MAC_Semiconductors_S[ names( L223.MAC_Semiconductors_S ) %in% c( "supplysector", "subsector", "stub.technology" ) ]
L223.MAC_Semiconductors_S_R <- repeat_and_add_vector( L223.MAC_Semiconductors_S, "region", GCAM_region_names$region )
L223.MAC_Semiconductors_S_R_T <- repeat_and_add_vector( L223.MAC_Semiconductors_S_R, "tax", MAC_taxes)

#Map in MAC reduction
L223.MAC_Semiconductors_S_R_T$mac.reduction <- L223.MAC_Semiconductors$value[ match( vecpaste( L223.MAC_Semiconductors_S_R_T[ c( "region", "tax" ) ] ), vecpaste( L223.MAC_Semiconductors[ c( "region", "tax" ) ] ) )]

#Drop missing values
L223.MAC_Semiconductors_S_R_T <- na.omit( L223.MAC_Semiconductors_S_R_T )

#Add extra columns and re-order
L223.MAC_Semiconductors_S_R_T$year <- ctrl_base_year
L223.MAC_Semiconductors_S_R_T <- repeat_and_add_vector( L223.MAC_Semiconductors_S_R_T, "Non.CO2", c( "SF6", "CF4", "C2F6" ) )
L223.MAC_Semiconductors_S_R_T$name <- "Semiconductors"
L223.MAC_Semiconductors_S_R_T <- L223.MAC_Semiconductors_S_R_T[ c( names_StubTechYr, "Non.CO2", "name", "tax", "mac.reduction" )]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L223.MAC_Aluminum_S_R_T, "MAC", "EMISSIONS_LEVEL2_DATA", "L223.MAC_Aluminum_S_R_T", "EMISSIONS_XML_BATCH", "batch_all_fgas_emissions.xml" ) 
write_mi_data( L223.MAC_ElectTD_S_R_T, "MAC", "EMISSIONS_LEVEL2_DATA", "L223.MAC_ElectTD_S_R_T", "EMISSIONS_XML_BATCH", "batch_all_fgas_emissions.xml" ) 
write_mi_data( L223.MAC_Foams_S_R_T, "MAC", "EMISSIONS_LEVEL2_DATA", "L223.MAC_Foams_S_R_T", "EMISSIONS_XML_BATCH", "batch_all_fgas_emissions.xml" ) 
write_mi_data( L223.MAC_HFC23_S_R_T, "MAC", "EMISSIONS_LEVEL2_DATA", "L223.MAC_HFC23_S_R_T", "EMISSIONS_XML_BATCH", "batch_all_fgas_emissions.xml" ) 
write_mi_data( L223.MAC_Refrigeration_S_R_T, "MAC", "EMISSIONS_LEVEL2_DATA", "L223.MAC_Refrigeration_S_R_T", "EMISSIONS_XML_BATCH", "batch_all_fgas_emissions.xml" ) 
write_mi_data( L223.MAC_Semiconductors_S_R_T, "MAC", "EMISSIONS_LEVEL2_DATA", "L223.MAC_Semiconductors_S_R_T", "EMISSIONS_XML_BATCH", "batch_all_fgas_emissions.xml" ) 

logstop()
