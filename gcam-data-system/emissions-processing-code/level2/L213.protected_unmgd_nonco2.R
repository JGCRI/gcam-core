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
logstart( "L213.protected_unmgd_nonco2.R" )
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
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A_regions <- readdata( "EMISSIONS_ASSUMPTIONS", "A_regions" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )
L124.nonco2_tg_R_grass_Y_AEZ <- readdata( "EMISSIONS_LEVEL1_DATA", "L124.nonco2_tg_R_grass_Y_AEZ" )
L124.nonco2_tg_R_forest_Y_AEZ <- readdata( "EMISSIONS_LEVEL1_DATA", "L124.nonco2_tg_R_forest_Y_AEZ" )
L125.bcoc_tgbkm2_R_grass_2000 <- readdata( "EMISSIONS_LEVEL1_DATA", "L125.bcoc_tgbkm2_R_grass_2000" )
L125.bcoc_tgbkm2_R_forest_2000 <- readdata( "EMISSIONS_LEVEL1_DATA", "L125.bcoc_tgbkm2_R_forest_2000" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
#Unmanaged land sector info
printlog( "L213.Sector: Sector info for unmanaged land technology" )
L213.Sector <- GCAM_region_names[ names( GCAM_region_names ) == "region" ]
L213.Sector$AgSupplySector <- "UnmanagedLand"
L213.Sector$input.unit <- "thous km2"
L213.Sector$market <- L213.Sector$region

printlog( "L213.ItemName: Land item to relate emissions to" )
L213.LandItem <- L213.Sector[ names( L213.Sector ) %in% c( "region", "AgSupplySector" )]
L213.LandItem <- repeat_and_add_vector( L213.LandItem, "AgSupplySubsector", c( "ForestFires", "GrasslandFires" ) )
L213.LandItem$itemName[ L213.LandItem$AgSupplySubsector == "ForestFires" ] <- "UnmanagedForest" 
L213.LandItem$itemName[ L213.LandItem$AgSupplySubsector == "GrasslandFires" ] <- "Grassland" 
L213.LandItem <- repeat_and_add_vector( L213.LandItem, "AEZ", AEZs )
L213.LandItem$AgSupplySubsector <- paste( L213.LandItem$AgSupplySubsector, L213.LandItem$AEZ, sep="" )
L213.LandItem$UnmanagedLandTechnology <- L213.LandItem$AgSupplySubsector
L213.LandItem$itemName <- paste( L213.LandItem$itemName, L213.LandItem$AEZ, sep="" )
L213.LandItem <- repeat_and_add_vector( L213.LandItem, "year", emiss_model_base_years )
L213.LandItem <- L213.LandItem[ c( names_UnmgdTech, "year", "itemName" )]

L213.LandItemProtect <- L213.LandItem
L213.LandItemProtect$UnmanagedLandTechnology <- paste( "Protected", L213.LandItemProtect$UnmanagedLandTechnology, sep="")
L213.LandItemProtect$itemName <- paste( "Protected", L213.LandItemProtect$itemName, sep="")

#Grassland emissions
printlog( "L213.GrassEmissions: Grassland fire emissions in all regions" )
#Interpolate and add region name
L213.GRASS <- L124.nonco2_tg_R_grass_Y_AEZ[ names( L124.nonco2_tg_R_grass_Y_AEZ ) %!in% c( "X2009", "X2010") ]

#Note: interpolate takes ages, so I'm not using interpolate_and_melt
L213.GRASS <- melt( L213.GRASS, id.vars = grep( "X[0-9]{4}", names( L213.GRASS ), invert = T ) )
L213.GRASS <- na.omit( L213.GRASS )
L213.GRASS$year <- as.numeric( substr( L213.GRASS$variable, 2, 5 ) )
L213.GRASS <- subset( L213.GRASS, L213.GRASS$year %in% emiss_model_base_years )
L213.GRASS <- add_region_name( L213.GRASS )

#Add subsector and tech names
L213.GRASS$AgSupplySector <- "UnmanagedLand"
L213.GRASS$AgSupplySubsector <- paste( "GrasslandFires", L213.GRASS$AEZ, sep="" )
L213.GRASS$UnmanagedLandTechnology <- paste( "GrasslandFires", L213.GRASS$AEZ, sep="" )

#Format for csv file - unprotected
L213.GRASSEmissions <- L213.GRASS[ c( names_UnmgdTech, "year", "Non.CO2" ) ]
L213.GRASSEmissions$input.emissions <- L213.GRASS$value * ( 1 - protect_land_fract )
L213.GRASSEmissions <- na.omit( L213.GRASSEmissions )

#Format for csv file - protected
L213.GRASSEmissionsProtect <- L213.GRASS[ c( names_UnmgdTech, "year", "Non.CO2" ) ]
L213.GRASSEmissionsProtect$UnmanagedLandTechnology <- paste( "Protected", L213.GRASSEmissionsProtect$UnmanagedLandTechnology, sep="")
L213.GRASSEmissionsProtect$input.emissions <- L213.GRASS$value * protect_land_fract
L213.GRASSEmissionsProtect <- na.omit( L213.GRASSEmissionsProtect )

#Grassland emissions factors for BC/OC
printlog( "L213.GrassEmissions: Grassland fire emissions factors for BC/OC in all regions" )
L213.GRASSEF <- L125.bcoc_tgbkm2_R_grass_2000
L213.GRASSEF <- na.omit( L213.GRASSEF )
names( L213.GRASSEF )[ names( L213.GRASSEF ) == "X2000" ] <- "value"
L213.GRASSEF <- repeat_and_add_vector( L213.GRASSEF, "year", emiss_model_base_years )
L213.GRASSEF <- add_region_name( L213.GRASSEF )
L213.GRASSEF <- repeat_and_add_vector( L213.GRASSEF, "AEZ", AEZs )

#Add subsector and tech names
L213.GRASSEF$AgSupplySector <- "UnmanagedLand"
L213.GRASSEF$AgSupplySubsector <- paste( "GrasslandFires", L213.GRASSEF$AEZ, sep="" )
L213.GRASSEF$UnmanagedLandTechnology <- paste( "GrasslandFires", L213.GRASSEF$AEZ, sep="" )

#Format for csv file - unprotected
L213.GRASSEmissionsFactors <- L213.GRASSEF[ c( names_UnmgdTech, "year", "Non.CO2" ) ]
L213.GRASSEmissionsFactors$emiss.coef <- L213.GRASSEF$value
L213.GRASSEmissionsFactors <- na.omit( L213.GRASSEmissionsFactors )

#Format for csv file - protected
L213.GRASSEmissionsFactorsProtect <- L213.GRASSEF[ c( names_UnmgdTech, "year", "Non.CO2" ) ]
L213.GRASSEmissionsFactorsProtect$UnmanagedLandTechnology <- paste( "Protected", L213.GRASSEmissionsFactorsProtect$UnmanagedLandTechnology, sep="")
L213.GRASSEmissionsFactorsProtect$emiss.coef <- L213.GRASSEF$value
L213.GRASSEmissionsFactorsProtect <- na.omit( L213.GRASSEmissionsFactorsProtect )

#Forest fire emissions
printlog( "L213.ForestEmissions: Forest fire emissions in all regions" )
#Interpolate and add region name
L213.FOREST <- L124.nonco2_tg_R_forest_Y_AEZ[ names( L124.nonco2_tg_R_forest_Y_AEZ ) %!in% c( "X2009", "X2010") ]

#Note: interpolate takes ages, so I'm not using interpolate_and_melt
L213.FOREST <- melt( L213.FOREST, id.vars = grep( "X[0-9]{4}", names( L213.FOREST ), invert = T ) )
L213.FOREST <- na.omit( L213.FOREST )
L213.FOREST$year <- as.numeric( substr( L213.FOREST$variable, 2, 5 ) )
L213.FOREST <- subset( L213.FOREST, L213.FOREST$year %in% emiss_model_base_years )
L213.FOREST <- add_region_name( L213.FOREST )

#Add subsector and tech names
L213.FOREST$AgSupplySector <- "UnmanagedLand"
L213.FOREST$AgSupplySubsector <- paste( "ForestFires", L213.FOREST$AEZ, sep="" )
L213.FOREST$UnmanagedLandTechnology <- paste( "ForestFires", L213.FOREST$AEZ, sep="" )

#Format for csv file - unprotected
L213.FORESTEmissions <- L213.FOREST[ c( names_UnmgdTech, "year", "Non.CO2" ) ]
L213.FORESTEmissions$input.emissions <- L213.FOREST$value * ( 1 - protect_land_fract )
L213.FORESTEmissions <- na.omit( L213.FORESTEmissions )

#Format for csv file - protected
L213.FORESTEmissionsProtect <- L213.FOREST[ c( names_UnmgdTech, "year", "Non.CO2" ) ]
L213.FORESTEmissionsProtect$UnmanagedLandTechnology <- paste( "Protected", L213.FORESTEmissionsProtect$UnmanagedLandTechnology, sep="")
L213.FORESTEmissionsProtect$input.emissions <- L213.FOREST$value * protect_land_fract
L213.FORESTEmissionsProtect <- na.omit( L213.FORESTEmissionsProtect )

#Forest fire emissions factors for BC/OC
printlog( "L213.ForestEmissions: Forest fire emissions factors for BC/OC in all regions" )
#Interpolate and add region name
L213.FORESTEF <- L125.bcoc_tgbkm2_R_forest_2000
L213.FORESTEF <- na.omit( L213.FORESTEF )
names( L213.FORESTEF )[ names( L213.FORESTEF ) == "X2000" ] <- "value"
L213.FORESTEF <- repeat_and_add_vector( L213.FORESTEF, "year", emiss_model_base_years )
L213.FORESTEF <- add_region_name( L213.FORESTEF )
L213.FORESTEF <- repeat_and_add_vector( L213.FORESTEF, "AEZ", AEZs )

#Add subsector and tech names
L213.FORESTEF$AgSupplySector <- "UnmanagedLand"
L213.FORESTEF$AgSupplySubsector <- paste( "ForestFires", L213.FORESTEF$AEZ, sep="" )
L213.FORESTEF$UnmanagedLandTechnology <- paste( "ForestFires", L213.FORESTEF$AEZ, sep="" )

#Format for csv file - unprotected
L213.FORESTEmissionsFactors <- L213.FORESTEF[ c( names_UnmgdTech, "year", "Non.CO2" ) ]
L213.FORESTEmissionsFactors$emiss.coef <- L213.FORESTEF$value
L213.FORESTEmissionsFactors <- na.omit( L213.FORESTEmissionsFactors )

#Format for csv file - protected
L213.FORESTEmissionsFactorsProtect <- L213.FORESTEF[ c( names_UnmgdTech, "year", "Non.CO2" ) ]
L213.FORESTEmissionsFactorsProtect$UnmanagedLandTechnology <- paste( "Protected", L213.FORESTEmissionsFactorsProtect$UnmanagedLandTechnology, sep="" )
L213.FORESTEmissionsFactorsProtect$emiss.coef <- L213.FORESTEF$value
L213.FORESTEmissionsFactorsProtect <- na.omit( L213.FORESTEmissionsFactorsProtect )

printlog( "Removing non-existent regions and AEZs from all tables")
L213.Sector <- subset( L213.Sector, !region %in% no_aglu_regions )
L213.GRASSEmissions <- remove_AEZ_nonexist( L213.GRASSEmissions )
L213.GRASSEmissionsFactors <- remove_AEZ_nonexist( L213.GRASSEmissionsFactors )
L213.FORESTEmissions <- remove_AEZ_nonexist( L213.FORESTEmissions )
L213.FORESTEmissionsFactors <- remove_AEZ_nonexist( L213.FORESTEmissionsFactors )
L213.GRASSEmissionsProtect <- remove_AEZ_nonexist( L213.GRASSEmissionsProtect )
L213.GRASSEmissionsFactorsProtect <- remove_AEZ_nonexist( L213.GRASSEmissionsFactorsProtect )
L213.FORESTEmissionsProtect <- remove_AEZ_nonexist( L213.FORESTEmissionsProtect )
L213.FORESTEmissionsFactorsProtect <- remove_AEZ_nonexist( L213.FORESTEmissionsFactorsProtect )
L213.LandItem <- remove_AEZ_nonexist( L213.LandItem )
L213.LandItemProtect <- remove_AEZ_nonexist( L213.LandItemProtect )

printlog( "Rename to regional SO2" )
L213.GRASSEmissionsProtect <- rename_SO2( L213.GRASSEmissionsProtect, A_regions, FALSE )
L213.FORESTEmissionsProtect <- rename_SO2( L213.FORESTEmissionsProtect, A_regions, FALSE )
L213.GRASSEmissions <- rename_SO2( L213.GRASSEmissions, A_regions, FALSE )
L213.FORESTEmissions <- rename_SO2( L213.FORESTEmissions, A_regions, FALSE )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L213.Sector, "SectorUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.Sector", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.LandItem, "ItemNameUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.LandItem", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.LandItemProtect, "ItemNameUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.LandItemProtect", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.GRASSEmissions, "InputEmissionsUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.GRASSEmissions", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.FORESTEmissions, "InputEmissionsUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.FORESTEmissions", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.GRASSEmissionsFactors, "InputEmFactUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.GRASSEmissionsFactors", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.FORESTEmissionsFactors, "InputEmFactUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.FORESTEmissionsFactors", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.GRASSEmissionsProtect, "InputEmissionsUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.GRASSEmissionsProtect", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.FORESTEmissionsProtect, "InputEmissionsUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.FORESTEmissionsProtect", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.GRASSEmissionsFactorsProtect, "InputEmFactUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.GRASSEmissionsFactorsProtect", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.FORESTEmissionsFactorsProtect, "InputEmFactUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.FORESTEmissionsFactorsProtect", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 

insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml", "EMISSIONS_XML_FINAL", "all_protected_unmgd_emissions.xml", "", xml_tag="outFile" )

logstop()
