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
logstart( "L212.unmgd_nonco2.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions from unmanaged land" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
A_regions <- readdata( "EMISSIONS_ASSUMPTIONS", "A_regions" )
L124.nonco2_tg_R_grass_Y_GLU <- readdata( "EMISSIONS_LEVEL1_DATA", "L124.nonco2_tg_R_grass_Y_GLU" )
L124.nonco2_tg_R_forest_Y_GLU <- readdata( "EMISSIONS_LEVEL1_DATA", "L124.nonco2_tg_R_forest_Y_GLU" )
L125.bcoc_tgbkm2_R_grass_2000 <- readdata( "EMISSIONS_LEVEL1_DATA", "L125.bcoc_tgbkm2_R_grass_2000" )
L125.bcoc_tgbkm2_R_forest_2000 <- readdata( "EMISSIONS_LEVEL1_DATA", "L125.bcoc_tgbkm2_R_forest_2000" )

# -----------------------------------------------------------------------------
# 2. Build tables for CSVs
#Unmanaged land sector info
printlog( "L212.AgSupplySector: Sector info for unmanaged land technology" )
printlog( "NOTE: only making unmanaged land sectors in regions that have aglu related emissions")
# also note, not including the BC/OC tables here, which are from a different data source, and have no GLU detail
# if regions/LTs have default BC/OC emissions factors but don't have any other gases (from EDGAR), they are dropped
L212.regions <- sort( unique( c( L124.nonco2_tg_R_grass_Y_GLU[[R]], L124.nonco2_tg_R_forest_Y_GLU[[R]] ) ) )
L212.AgSupplySector <- data.frame(
  region = GCAM_region_names$region[ match( L212.regions, GCAM_region_names[[R]] ) ],
  AgSupplySector = "UnmanagedLand",
  output.unit = "none",
  input.unit = "thous km2",
  price.unit = "none",
  calPrice = -1 )
L212.AgSupplySector$market <- L212.AgSupplySector$region
L212.AgSupplySector$logit.year.fillout <- min( model_base_years )
L212.AgSupplySector$logit.exponent <- -3

printlog( "L212.ItemName: Land item to relate emissions to" )
L212.ItemNameBase <- data.frame( AgSupplySector = "UnmanagedLand",
                                 AgSupplySubsector = c( "ForestFire", "Deforest", "GrasslandFires"),
                                 itemName = c( "UnmanagedForest", "UnmanagedForest", "Grassland" ) )
#For merging, set the land type equal to the item name
L212.ItemNameBase[[LT]] <- L212.ItemNameBase$itemName

printlog( "Note: only making unmanaged land technologies in the regions/glus with the corresponding land use type available" )
L212.ItemName_R_LT_GLU <- rbind( unique( L124.nonco2_tg_R_grass_Y_GLU[ R_LT_GLU ] ),
                                 unique( L124.nonco2_tg_R_forest_Y_GLU[ R_LT_GLU ] ) )
L212.ItemName <- merge( L212.ItemNameBase, L212.ItemName_R_LT_GLU, all.x = T )

#append glu to make subsector and technology names, and repeat by the model base years
L212.ItemName <- add_region_name( L212.ItemName )
L212.ItemName$AgSupplySubsector <- with( L212.ItemName, paste( AgSupplySubsector, GLU, sep = LT_GLU_delimiter ) )
L212.ItemName$UnmanagedLandTechnology <- L212.ItemName$AgSupplySubsector
L212.ItemName <- repeat_and_add_vector( L212.ItemName, Y, model_base_years )
L212.ItemName$itemName <- with( L212.ItemName, paste( itemName, GLU, sep = LT_GLU_delimiter ) )
L212.ItemName <- L212.ItemName[ c( names_UnmgdTech, Y, "itemName" )]

#Grassland emissions
printlog( "L212.GrassEmissions: Grassland fire emissions in all regions" )
#Note: interpolate takes ages, so I'm not using interpolate_and_melt
L212.GRASS <- melt( L124.nonco2_tg_R_grass_Y_GLU, id.vars = c( R_LT_GLU, "Non.CO2" ),
                    measure.vars = X_emiss_model_base_years, variable.name = Y, value.name = "input.emissions" )
L212.GRASS$year <- as.numeric( sub( "X", "", L212.GRASS$year ) )
L212.GRASS$input.emissions <- round( L212.GRASS$input.emissions, digits_emissions )

#Add region, subsector and tech names
L212.GRASS <- add_region_name( L212.GRASS )
L212.GRASS$AgSupplySector <- "UnmanagedLand"
L212.GRASS$AgSupplySubsector <- paste( "GrasslandFires", L212.GRASS$GLU, sep = LT_GLU_delimiter )
L212.GRASS$UnmanagedLandTechnology <- L212.GRASS$AgSupplySubsector

#Format for csv file
L212.GRASSEmissions <- L212.GRASS[ c( names_UnmgdTech, "year", "Non.CO2", "input.emissions" ) ]

#Grassland emissions factors for BC/OC
printlog( "L212.GrassEmissions: Grassland fire emissions factors for BC/OC in all regions" )
L212.GRASSEmissionsFactors_BCOC <- unique( L212.GRASSEmissions[ c( names_UnmgdTech, "year" ) ] )
L212.GRASSEmissionsFactors_BCOC <- repeat_and_add_vector( L212.GRASSEmissionsFactors_BCOC, "Non.CO2", unique( L125.bcoc_tgbkm2_R_grass_2000$Non.CO2 ) )
L125.bcoc_tgbkm2_R_grass_2000 <- add_region_name( L125.bcoc_tgbkm2_R_grass_2000)
L212.GRASSEmissionsFactors_BCOC$emiss.coef <- round(
  L125.bcoc_tgbkm2_R_grass_2000$em_factor[
    match( vecpaste( L212.GRASSEmissionsFactors_BCOC[ c( "region", "Non.CO2" ) ] ),
           vecpaste( L125.bcoc_tgbkm2_R_grass_2000[ c( "region", "Non.CO2" ) ] ) ) ],
  digits_emissions )

#Forest fire emissions
printlog( "L212.ForestEmissions: Forest fire emissions in all regions" )
#Interpolate and add region name
L212.FOREST <- melt( L124.nonco2_tg_R_forest_Y_GLU, id.vars = c( R_LT_GLU, "technology", "Non.CO2" ),
                    measure.vars = X_emiss_model_base_years, variable.name = Y, value.name = "input.emissions" )
L212.FOREST$year <- as.numeric( sub( "X", "", L212.FOREST$year ) )
L212.FOREST$input.emissions <- round( L212.FOREST$input.emissions, digits_emissions )

#Add region, subsector and tech names
L212.FOREST <- add_region_name( L212.FOREST )
L212.FOREST$AgSupplySector <- "UnmanagedLand"
L212.FOREST$AgSupplySubsector <- paste( L212.FOREST$technology, L212.FOREST$GLU, sep = LT_GLU_delimiter )
L212.FOREST$UnmanagedLandTechnology <- L212.FOREST$AgSupplySubsector

#Split into ForestFire & Deforest -- they have different drivers and this is easier to do now 
L212.FOREST_FF <- subset( L212.FOREST, technology == "ForestFire" )
L212.FOREST_D <- subset( L212.FOREST, technology == "Deforest" )

#Format for csv file
L212.FORESTEmissions_FF <- L212.FOREST_FF[ c( names_UnmgdTech, "year", "Non.CO2", "input.emissions" ) ]
L212.FORESTEmissions_D <- L212.FOREST_D[ c( names_UnmgdTech, "year", "Non.CO2", "input.emissions" ) ]

#Forest fire emissions factors for BC/OC
printlog( "L212.ForestEmissions: Forest fire emissions factors for BC/OC in all regions" )
#Interpolate and add region name
L212.FORESTEmissionsFactors_BCOC <- unique( L212.FOREST[ c( names_UnmgdTech, "technology", "year" ) ] )
L212.FORESTEmissionsFactors_BCOC <- repeat_and_add_vector( L212.FORESTEmissionsFactors_BCOC, "Non.CO2", unique( L125.bcoc_tgbkm2_R_forest_2000$Non.CO2 ) )
L125.bcoc_tgbkm2_R_forest_2000 <- add_region_name( L125.bcoc_tgbkm2_R_forest_2000)
L212.FORESTEmissionsFactors_BCOC$emiss.coef <- round(
  L125.bcoc_tgbkm2_R_forest_2000$em_factor[
    match( vecpaste( L212.FORESTEmissionsFactors_BCOC[ c( "region", "technology", "Non.CO2" ) ] ),
           vecpaste( L125.bcoc_tgbkm2_R_forest_2000[ c( "region", "technology", "Non.CO2" ) ] ) ) ],
  digits_emissions )

#Split into ForestFire & Deforest -- they have different drivers and this is easier to do now 
L212.FORESTEmissionsFactors_BCOC_FF <- subset( L212.FORESTEmissionsFactors_BCOC, technology == "ForestFire" )[
  c( names_UnmgdTech, Y, "Non.CO2", "emiss.coef" ) ]
L212.FORESTEmissionsFactors_BCOC_D <- subset( L212.FORESTEmissionsFactors_BCOC, technology == "Deforest" )[
  c( names_UnmgdTech, Y, "Non.CO2", "emiss.coef" ) ]


printlog( "Rename to regional SO2" )
L212.GRASSEmissions <- rename_SO2( L212.GRASSEmissions, A_regions, FALSE )
L212.FORESTEmissions_D <- rename_SO2( L212.FORESTEmissions_D, A_regions, FALSE )
L212.FORESTEmissions_FF <- rename_SO2( L212.FORESTEmissions_FF, A_regions, FALSE )

# Add logit tags to avoid errors
L212.AgSupplySectorLogitType <- L212.AgSupplySector
L212.AgSupplySectorLogitType$logit.type <- NA
L212.AgSupplySectorLogitTables <- get_logit_fn_tables( L212.AgSupplySectorLogitType, names_AgSupplySectorLogitType,
    base.header="AgSupplySector_", include.equiv.table=T, write.all.regions=F )
# We do not actually care about the logit here but we need a value to avoid errors
L212.AgSupplySubsector <- L212.ItemName[ names( L212.ItemName ) %in% names_AgSupplySubsector ]
L212.AgSupplySubsector$logit.year.fillout <- min( model_base_years )
L212.AgSupplySubsector$logit.exponent <- -3
L212.AgSupplySubsectorLogitType <- L212.ItemName
L212.AgSupplySubsectorLogitType$logit.type <- NA
L212.SubsectorLogitTables <- get_logit_fn_tables( L212.AgSupplySubsectorLogitType, names_AgSupplySubsectorLogitType,
    base.header="AgSupplySubsector_", include.equiv.table=F, write.all.regions=F )

printlog( "Writing files with modified emissions for protected lands" )
# Note: the protected lands input file is intended to be read after the normal one,
# so rather than copy all of the duplicate information (e.g., supplysector and subsector, itemname),
# these files only read in the new technologies and modified emissions levels

printlog( "Land item: protected land use types now prefixed with protected; non-protected keeps the same name so no need to write out" )
#Land item: copy for prot and noprot, prefix the LTs with "Protected" in the prot file
L212.ItemName_prot <- L212.ItemName
L212.ItemName_prot$UnmanagedLandTechnology <- paste0( "Protected", L212.ItemName_prot$UnmanagedLandTechnology)
L212.ItemName_prot$itemName <- paste0( "Protected", L212.ItemName_prot$itemName)

printlog( "Emissions: names in protected files are prefixed, and emissions in both are multiplied by protected land fraction")
#Grassland emissions
L212.GRASSEmissions_prot <- L212.GRASSEmissions
L212.GRASSEmissions_prot$UnmanagedLandTechnology <- paste0( "Protected", L212.GRASSEmissions_prot$UnmanagedLandTechnology )
L212.GRASSEmissions_prot$input.emissions <- L212.GRASSEmissions$input.emissions * protect_land_fract

L212.GRASSEmissions_noprot <- L212.GRASSEmissions
L212.GRASSEmissions_noprot$input.emissions <- L212.GRASSEmissions$input.emissions * ( 1 - protect_land_fract )

#Forest fires
L212.FORESTEmissions_FF_prot <- L212.FORESTEmissions_FF
L212.FORESTEmissions_FF_prot$UnmanagedLandTechnology <- paste0( "Protected", L212.FORESTEmissions_FF_prot$UnmanagedLandTechnology )
L212.FORESTEmissions_FF_prot$input.emissions <- L212.FORESTEmissions_FF$input.emissions * protect_land_fract

L212.FORESTEmissions_FF_noprot <- L212.FORESTEmissions_FF
L212.FORESTEmissions_FF_noprot$input.emissions <- L212.FORESTEmissions_FF$input.emissions * ( 1 - protect_land_fract )

#Deforestation
L212.FORESTEmissions_D_prot <- L212.FORESTEmissions_D
L212.FORESTEmissions_D_prot$UnmanagedLandTechnology <- paste0( "Protected", L212.FORESTEmissions_D_prot$UnmanagedLandTechnology )
L212.FORESTEmissions_D_prot$input.emissions <- L212.FORESTEmissions_D$input.emissions * protect_land_fract

L212.FORESTEmissions_D_noprot <- L212.FORESTEmissions_D
L212.FORESTEmissions_D_noprot$input.emissions <- L212.FORESTEmissions_D$input.emissions * ( 1 - protect_land_fract )

printlog( "Emissions factors: names in protected files are prefixed, and factors are left unchanged" )
L212.GRASSEmissionsFactors_BCOC_prot <- L212.GRASSEmissionsFactors_BCOC
L212.GRASSEmissionsFactors_BCOC_prot$UnmanagedLandTechnology <- paste0( "Protected", L212.GRASSEmissionsFactors_BCOC_prot$UnmanagedLandTechnology )
L212.GRASSEmissionsFactors_BCOC_noprot <- L212.GRASSEmissionsFactors_BCOC

L212.FORESTEmissionsFactors_BCOC_FF_prot <- L212.FORESTEmissionsFactors_BCOC_FF
L212.FORESTEmissionsFactors_BCOC_FF_prot$UnmanagedLandTechnology <- paste0( "Protected", L212.FORESTEmissionsFactors_BCOC_FF_prot$UnmanagedLandTechnology )
L212.FORESTEmissionsFactors_BCOC_FF_noprot <- L212.FORESTEmissionsFactors_BCOC_FF

L212.FORESTEmissionsFactors_BCOC_D_prot <- L212.FORESTEmissionsFactors_BCOC_D
L212.FORESTEmissionsFactors_BCOC_D_prot$UnmanagedLandTechnology <- paste0( "Protected", L212.FORESTEmissionsFactors_BCOC_D_prot$UnmanagedLandTechnology )
L212.FORESTEmissionsFactors_BCOC_D_noprot <- L212.FORESTEmissionsFactors_BCOC_D

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
for( curr_table in names ( L212.AgSupplySectorLogitTables ) ) {
write_mi_data( L212.AgSupplySectorLogitTables[[ curr_table ]]$data, L212.AgSupplySectorLogitTables[[ curr_table ]]$header,
    "EMISSIONS_LEVEL2_DATA", paste0("L212.", L212.AgSupplySectorLogitTables[[ curr_table ]]$header ), "EMISSIONS_XML_BATCH",
    "batch_all_unmgd_emissions.xml" )
}
write_mi_data( L212.AgSupplySector, "AgSupplySector", "EMISSIONS_LEVEL2_DATA", "L212.AgSupplySector", "EMISSIONS_XML_BATCH", "batch_all_unmgd_emissions.xml" ) 
for( curr_table in names ( L212.SubsectorLogitTables ) ) {
write_mi_data( L212.SubsectorLogitTables[[ curr_table ]]$data, L212.SubsectorLogitTables[[ curr_table ]]$header,
    "EMISSIONS_LEVEL2_DATA", paste0("L212.", L212.SubsectorLogitTables[[ curr_table ]]$header ), "EMISSIONS_XML_BATCH",
    "batch_all_unmgd_emissions.xml" )
}
write_mi_data( L212.AgSupplySubsector, "AgSupplySubsector", "EMISSIONS_LEVEL2_DATA", "L212.AgSupplySubsector", "EMISSIONS_XML_BATCH", "batch_all_unmgd_emissions.xml" ) 
write_mi_data( L212.ItemName, "ItemName", "EMISSIONS_LEVEL2_DATA", "L212.ItemName", "EMISSIONS_XML_BATCH", "batch_all_unmgd_emissions.xml" ) 
write_mi_data( L212.GRASSEmissions, "InputEmissionsUnmgd", "EMISSIONS_LEVEL2_DATA", "L212.GRASSEmissions", "EMISSIONS_XML_BATCH", "batch_all_unmgd_emissions.xml" ) 
write_mi_data( L212.FORESTEmissions_FF, "InputEmissionsUnmgd", "EMISSIONS_LEVEL2_DATA", "L212.FORESTEmissions_FF", "EMISSIONS_XML_BATCH", "batch_all_unmgd_emissions.xml" ) 
write_mi_data( L212.FORESTEmissions_D, "OutputEmissionsUnmgd", "EMISSIONS_LEVEL2_DATA", "L212.FORESTEmissions_D", "EMISSIONS_XML_BATCH", "batch_all_unmgd_emissions.xml" ) 
write_mi_data( L212.GRASSEmissionsFactors_BCOC, "InputEmFactUnmgd", "EMISSIONS_LEVEL2_DATA", "L212.GRASSEmissionsFactors_BCOC", "EMISSIONS_XML_BATCH", "batch_all_unmgd_emissions.xml" ) 
write_mi_data( L212.FORESTEmissionsFactors_BCOC_FF, "InputEmFactUnmgd", "EMISSIONS_LEVEL2_DATA", "L212.FORESTEmissionsFactors_BCOC_FF", "EMISSIONS_XML_BATCH", "batch_all_unmgd_emissions.xml" ) 
write_mi_data( L212.FORESTEmissionsFactors_BCOC_D, "OutputEmFactUnmgd", "EMISSIONS_LEVEL2_DATA", "L212.FORESTEmissionsFactors_BCOC_D", "EMISSIONS_XML_BATCH", "batch_all_unmgd_emissions.xml" ) 

insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_all_unmgd_emissions.xml", "EMISSIONS_XML_FINAL", "all_unmgd_emissions.xml", "", xml_tag="outFile" )

#Protected lands files
write_mi_data( L212.ItemName_prot, "ItemName", "EMISSIONS_LEVEL2_DATA", "L212.ItemName_prot", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L212.GRASSEmissions_prot, "InputEmissionsUnmgd", "EMISSIONS_LEVEL2_DATA", "L212.GRASSEmissions_prot", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L212.GRASSEmissions_noprot, "InputEmissionsUnmgd", "EMISSIONS_LEVEL2_DATA", "L212.GRASSEmissions_noprot", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L212.FORESTEmissions_FF_prot, "InputEmissionsUnmgd", "EMISSIONS_LEVEL2_DATA", "L212.FORESTEmissions_FF_prot", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L212.FORESTEmissions_FF_noprot, "InputEmissionsUnmgd", "EMISSIONS_LEVEL2_DATA", "L212.FORESTEmissions_FF_noprot", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L212.FORESTEmissions_D_prot, "OutputEmissionsUnmgd", "EMISSIONS_LEVEL2_DATA", "L212.FORESTEmissions_D_prot", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L212.FORESTEmissions_D_noprot, "OutputEmissionsUnmgd", "EMISSIONS_LEVEL2_DATA", "L212.FORESTEmissions_D_noprot", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L212.GRASSEmissionsFactors_BCOC_prot, "InputEmFactUnmgd", "EMISSIONS_LEVEL2_DATA", "L212.GRASSEmissionsFactors_BCOC_prot", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L212.GRASSEmissionsFactors_BCOC_noprot, "InputEmFactUnmgd", "EMISSIONS_LEVEL2_DATA", "L212.GRASSEmissionsFactors_BCOC_noprot", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L212.FORESTEmissionsFactors_BCOC_FF_prot, "InputEmFactUnmgd", "EMISSIONS_LEVEL2_DATA", "L212.FORESTEmissionsFactors_BCOC_FF_prot", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L212.FORESTEmissionsFactors_BCOC_FF_noprot, "InputEmFactUnmgd", "EMISSIONS_LEVEL2_DATA", "L212.FORESTEmissionsFactors_BCOC_FF_noprot", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L212.FORESTEmissionsFactors_BCOC_D_prot, "OutputEmFactUnmgd", "EMISSIONS_LEVEL2_DATA", "L212.FORESTEmissionsFactors_BCOC_D_prot", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 
write_mi_data( L212.FORESTEmissionsFactors_BCOC_D_noprot, "OutputEmFactUnmgd", "EMISSIONS_LEVEL2_DATA", "L212.FORESTEmissionsFactors_BCOC_D_noprot", "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml" ) 

insert_file_into_batchxml( "EMISSIONS_XML_BATCH", "batch_all_protected_unmgd_emissions.xml", "EMISSIONS_XML_FINAL", "all_protected_unmgd_emissions.xml", "", xml_tag="outFile" )

logstop()
