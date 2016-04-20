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
printlog( "Historical emissions from unmanaged land, when protected lands are included" )

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
L213.LandItem <- repeat_and_add_vector( L213.LandItem, "AgSupplySubsector", c( "ForestFire", "Deforest", "GrasslandFires" ) )
L213.LandItem$itemName[ L213.LandItem$AgSupplySubsector == "ForestFire" ] <- "UnmanagedForest" 
L213.LandItem$itemName[ L213.LandItem$AgSupplySubsector == "Deforest" ] <- "UnmanagedForest" 
L213.LandItem$itemName[ L213.LandItem$AgSupplySubsector == "GrasslandFires" ] <- "Grassland" 
L213.LandItem <- repeat_and_add_vector( L213.LandItem, "AEZ", AEZs )
L213.LandItem$AgSupplySubsector <- paste( L213.LandItem$AgSupplySubsector, L213.LandItem$AEZ, sep="" )
L213.LandItem$UnmanagedLandTechnology <- L213.LandItem$AgSupplySubsector
L213.LandItem$itemName <- paste( L213.LandItem$itemName, L213.LandItem$AEZ, sep="" )
L213.LandItem <- repeat_and_add_vector( L213.LandItem, "year", model_base_years )
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
L213.GRASSEF_BCOC <- L125.bcoc_tgbkm2_R_grass_2000
L213.GRASSEF_BCOC <- na.omit( L213.GRASSEF_BCOC )
names( L213.GRASSEF_BCOC )[ names( L213.GRASSEF_BCOC ) == "X2000" ] <- "value"
L213.GRASSEF_BCOC <- repeat_and_add_vector( L213.GRASSEF_BCOC, "year", emiss_model_base_years )
L213.GRASSEF_BCOC <- add_region_name( L213.GRASSEF_BCOC )
L213.GRASSEF_BCOC <- repeat_and_add_vector( L213.GRASSEF_BCOC, "AEZ", AEZs )

#Add subsector and tech names
L213.GRASSEF_BCOC$AgSupplySector <- "UnmanagedLand"
L213.GRASSEF_BCOC$AgSupplySubsector <- paste( "GrasslandFires", L213.GRASSEF_BCOC$AEZ, sep="" )
L213.GRASSEF_BCOC$UnmanagedLandTechnology <- paste( "GrasslandFires", L213.GRASSEF_BCOC$AEZ, sep="" )

#Format for csv file - unprotected
L213.GRASSEmissionsFactors_BCOC <- L213.GRASSEF_BCOC[ c( names_UnmgdTech, "year", "Non.CO2" ) ]
L213.GRASSEmissionsFactors_BCOC$emiss.coef <- L213.GRASSEF_BCOC$value
L213.GRASSEmissionsFactors_BCOC <- na.omit( L213.GRASSEmissionsFactors_BCOC )

#Format for csv file - protected
L213.GRASSEmissionsFactorsProtect_BCOC <- L213.GRASSEF_BCOC[ c( names_UnmgdTech, "year", "Non.CO2" ) ]
L213.GRASSEmissionsFactorsProtect_BCOC$UnmanagedLandTechnology <- paste( "Protected", L213.GRASSEmissionsFactorsProtect_BCOC$UnmanagedLandTechnology, sep="")
L213.GRASSEmissionsFactorsProtect_BCOC$emiss.coef <- L213.GRASSEF_BCOC$value
L213.GRASSEmissionsFactorsProtect_BCOC <- na.omit( L213.GRASSEmissionsFactorsProtect_BCOC )

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
L213.FOREST$AgSupplySubsector <- paste( L213.FOREST$technology, L213.FOREST$AEZ, sep="" )
L213.FOREST$UnmanagedLandTechnology <- paste( L213.FOREST$technology, L213.FOREST$AEZ, sep="" )

#Split into ForestFire & Deforest -- they have different drivers and this is easier to do now 
L213.FOREST_FF <- subset( L213.FOREST, technology == "ForestFire" )
L213.FOREST_D <- subset( L213.FOREST, technology == "Deforest" )

#Format for csv file - unprotected
L213.FORESTEmissions_FF <- L213.FOREST_FF[ c( names_UnmgdTech, "year", "Non.CO2" ) ]
L213.FORESTEmissions_FF$input.emissions <- L213.FOREST_FF$value * ( 1 - protect_land_fract )
L213.FORESTEmissions_FF <- na.omit( L213.FORESTEmissions_FF )

L213.FORESTEmissions_D <- L213.FOREST_D[ c( names_UnmgdTech, "year", "Non.CO2" ) ]
L213.FORESTEmissions_D$input.emissions <- L213.FOREST_D$value * ( 1 - protect_land_fract )
L213.FORESTEmissions_D <- na.omit( L213.FORESTEmissions_D )

#Format for csv file - protected
L213.FORESTEmissionsProtect_FF <- L213.FOREST_FF[ c( names_UnmgdTech, "year", "Non.CO2" ) ]
L213.FORESTEmissionsProtect_FF$UnmanagedLandTechnology <- paste( "Protected", L213.FORESTEmissionsProtect_FF$UnmanagedLandTechnology, sep="")
L213.FORESTEmissionsProtect_FF$input.emissions <- L213.FOREST_FF$value * protect_land_fract
L213.FORESTEmissionsProtect_FF <- na.omit( L213.FORESTEmissionsProtect_FF )

L213.FORESTEmissionsProtect_D <- L213.FOREST[ c( names_UnmgdTech, "year", "Non.CO2" ) ]
L213.FORESTEmissionsProtect_D$UnmanagedLandTechnology <- paste( "Protected", L213.FORESTEmissionsProtect_D$UnmanagedLandTechnology, sep="")
L213.FORESTEmissionsProtect_D$input.emissions <- L213.FOREST$value * protect_land_fract
L213.FORESTEmissionsProtect_D <- na.omit( L213.FORESTEmissionsProtect_D )

#Forest fire emissions factors for BC/OC
printlog( "L213.ForestEmissions: Forest fire emissions factors for BC/OC in all regions" )
#Interpolate and add region name
L213.FORESTEF_BCOC <- L125.bcoc_tgbkm2_R_forest_2000
L213.FORESTEF_BCOC <- na.omit( L213.FORESTEF_BCOC )
L213.FORESTEF_BCOC <- repeat_and_add_vector( L213.FORESTEF_BCOC, "year", emiss_model_base_years )
L213.FORESTEF_BCOC <- add_region_name( L213.FORESTEF_BCOC )
L213.FORESTEF_BCOC <- repeat_and_add_vector( L213.FORESTEF_BCOC, "AEZ", AEZs )

#Add subsector and tech names
L213.FORESTEF_BCOC$AgSupplySector <- "UnmanagedLand"
L213.FORESTEF_BCOC$AgSupplySubsector <- paste( L213.FORESTEF_BCOC$techonlogy, L213.FORESTEF_BCOC$AEZ, sep="" )
L213.FORESTEF_BCOC$UnmanagedLandTechnology <- paste( L213.FORESTEF_BCOC$technology, L213.FORESTEF_BCOC$AEZ, sep="" )

#Split into ForestFire & Deforest -- they have different drivers and this is easier to do now 
L213.FORESTEF_BCOC_FF <- subset( L213.FORESTEF_BCOC, technology == "ForestFire" )
L213.FORESTEF_BCOC_D <- subset( L213.FORESTEF_BCOC, technology == "Deforest" )

#Format for csv file - unprotected
L213.FORESTEmissionsFactors_BCOC_FF <- L213.FORESTEF_BCOC_FF[ c( names_UnmgdTech, "year", "Non.CO2" ) ]
L213.FORESTEmissionsFactors_BCOC_FF$emiss.coef <- L213.FORESTEF_BCOC_FF$em_fact
L213.FORESTEmissionsFactors_BCOC_FF <- na.omit( L213.FORESTEmissionsFactors_BCOC_FF )

L213.FORESTEmissionsFactors_BCOC_D <- L213.FORESTEF_BCOC_D[ c( names_UnmgdTech, "year", "Non.CO2" ) ]
L213.FORESTEmissionsFactors_BCOC_D$emiss.coef <- L213.FORESTEF_BCOC_D$em_fact
L213.FORESTEmissionsFactors_BCOC_D <- na.omit( L213.FORESTEmissionsFactors_BCOC_D )

#Format for csv file - protected
L213.FORESTEmissionsFactorsProtect_BCOC_FF <- L213.FORESTEF_BCOC_FF[ c( names_UnmgdTech, "year", "Non.CO2" ) ]
L213.FORESTEmissionsFactorsProtect_BCOC_FF$UnmanagedLandTechnology <- paste( "Protected", L213.FORESTEmissionsFactorsProtect_BCOC_FF$UnmanagedLandTechnology, sep="" )
L213.FORESTEmissionsFactorsProtect_BCOC_FF$emiss.coef <- L213.FORESTEF_BCOC_FF$em_fact
L213.FORESTEmissionsFactorsProtect_BCOC_FF <- na.omit( L213.FORESTEmissionsFactorsProtect_BCOC_FF )

L213.FORESTEmissionsFactorsProtect_BCOC_D <- L213.FORESTEF_BCOC_D[ c( names_UnmgdTech, "year", "Non.CO2" ) ]
L213.FORESTEmissionsFactorsProtect_BCOC_D$UnmanagedLandTechnology <- paste( "Protected", L213.FORESTEmissionsFactorsProtect_BCOC_D$UnmanagedLandTechnology, sep="" )
L213.FORESTEmissionsFactorsProtect_BCOC_D$emiss.coef <- L213.FORESTEF_BCOC_D$em_fact
L213.FORESTEmissionsFactorsProtect_BCOC_D <- na.omit( L213.FORESTEmissionsFactorsProtect_BCOC_D )

printlog( "Removing non-existent regions and AEZs from all tables")
L213.Sector <- subset( L213.Sector, !region %in% no_aglu_regions )
L213.GRASSEmissions <- remove_AEZ_nonexist( L213.GRASSEmissions )
L213.GRASSEmissionsFactors_BCOC <- remove_AEZ_nonexist( L213.GRASSEmissionsFactors_BCOC )
L213.FORESTEmissions_FF <- remove_AEZ_nonexist( L213.FORESTEmissions_FF )
L213.FORESTEmissions_D <- remove_AEZ_nonexist( L213.FORESTEmissions_D )
L213.FORESTEmissionsFactors_BCOC_FF <- remove_AEZ_nonexist( L213.FORESTEmissionsFactors_BCOC_FF )
L213.FORESTEmissionsFactors_BCOC_D <- remove_AEZ_nonexist( L213.FORESTEmissionsFactors_BCOC_D )
L213.GRASSEmissionsProtect <- remove_AEZ_nonexist( L213.GRASSEmissionsProtect )
L213.GRASSEmissionsFactorsProtect_BCOC <- remove_AEZ_nonexist( L213.GRASSEmissionsFactorsProtect_BCOC )
L213.FORESTEmissionsProtect_FF <- remove_AEZ_nonexist( L213.FORESTEmissionsProtect_FF )
L213.FORESTEmissionsProtect_D <- remove_AEZ_nonexist( L213.FORESTEmissionsProtect_D )
L213.FORESTEmissionsFactorsProtect_BCOC_FF <- remove_AEZ_nonexist( L213.FORESTEmissionsFactorsProtect_BCOC_FF )
L213.FORESTEmissionsFactorsProtect_BCOC_D <- remove_AEZ_nonexist( L213.FORESTEmissionsFactorsProtect_BCOC_D )
L213.LandItem <- remove_AEZ_nonexist( L213.LandItem )
L213.LandItemProtect <- remove_AEZ_nonexist( L213.LandItemProtect )

printlog( "Rename to regional SO2" )
L213.GRASSEmissionsProtect <- rename_SO2( L213.GRASSEmissionsProtect, A_regions, FALSE )
L213.FORESTEmissionsProtect_FF <- rename_SO2( L213.FORESTEmissionsProtect_FF, A_regions, FALSE )
L213.FORESTEmissionsProtect_D <- rename_SO2( L213.FORESTEmissionsProtect_D, A_regions, FALSE )
L213.GRASSEmissions <- rename_SO2( L213.GRASSEmissions, A_regions, FALSE )
L213.FORESTEmissions_FF <- rename_SO2( L213.FORESTEmissions_FF, A_regions, FALSE )
L213.FORESTEmissions_D <- rename_SO2( L213.FORESTEmissions_D, A_regions, FALSE )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L213.Sector, "SectorUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.Sector", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.LandItem, "ItemNameUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.LandItem", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.LandItemProtect, "ItemNameUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.LandItemProtect", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.GRASSEmissions, "InputEmissionsUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.GRASSEmissions", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.FORESTEmissions_FF, "InputEmissionsUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.FORESTEmissions_FF", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.FORESTEmissions_D, "OutputEmissionsUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.FORESTEmissions_D", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.GRASSEmissionsFactors_BCOC, "InputEmFactUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.GRASSEmissionsFactors_BCOC", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.FORESTEmissionsFactors_BCOC_FF, "InputEmFactUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.FORESTEmissionsFactors_BCOC_FF", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.FORESTEmissionsFactors_BCOC_D, "OutputEmFactUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.FORESTEmissionsFactors_BCOC_D", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.GRASSEmissionsProtect, "InputEmissionsUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.GRASSEmissionsProtect", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.FORESTEmissionsProtect_FF, "InputEmissionsUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.FORESTEmissionsProtect_FF", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.FORESTEmissionsProtect_D, "OutputEmissionsUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.FORESTEmissionsProtect_D", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.GRASSEmissionsFactorsProtect_BCOC, "InputEmFactUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.GRASSEmissionsFactorsProtect_BCOC", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.FORESTEmissionsFactorsProtect_BCOC_FF, "InputEmFactUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.FORESTEmissionsFactorsProtect_BCOC_FF", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L213.FORESTEmissionsFactorsProtect_BCOC_D, "OutputEmFactUnmgd", "EMISSIONS_LEVEL2_DATA", "L213.FORESTEmissionsFactorsProtect_BCOC_D", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 

insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml", "EMISSIONS_XML_FINAL", "all_protected_unmgd_emissions.xml", "", xml_tag="outFile" )

logstop()
