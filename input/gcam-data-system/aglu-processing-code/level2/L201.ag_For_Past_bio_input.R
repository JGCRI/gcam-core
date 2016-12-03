# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "AGLUPROC_DIR" ) ){
    if( Sys.getenv( "AGLUPROC" ) != "" ){
        AGLUPROC_DIR <- Sys.getenv( "AGLUPROC" )
    } else {
        stop("Could not determine location of aglu processing scripts, please set the R var AGLUPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
logstart( "L201.ag_For_Past_bio_input.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Model input for production of primary agricultural products / pasture / forest products" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
basin_to_country_mapping <- readdata( "WATER_MAPPINGS", "basin_to_country_mapping" )
A_AgSupplySector <- readdata( "AGLU_ASSUMPTIONS", "A_agSupplySector" )
A_AgSupplySubsector <- readdata( "AGLU_ASSUMPTIONS", "A_agSupplySubsector" )
L103.ag_Prod_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L103.ag_Prod_Mt_R_C_Y_GLU", replace_GLU = T )
L113.ag_bioYield_GJm2_R_GLU <- readdata( "AGLU_LEVEL1_DATA", "L113.ag_bioYield_GJm2_R_GLU", replace_GLU = T )
L122.ag_HA_to_CropLand_R_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L122.ag_HA_to_CropLand_R_Y_GLU", replace_GLU = T )
L123.ag_Prod_Mt_R_Past_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L123.ag_Prod_Mt_R_Past_Y_GLU", replace_GLU = T )
L123.For_Prod_bm3_R_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L123.For_Prod_bm3_R_Y_GLU", replace_GLU = T )
L123.For_Yield_m3m2_R_GLU <- readdata( "AGLU_LEVEL1_DATA", "L123.For_Yield_m3m2_R_GLU", replace_GLU = T )
L132.ag_an_For_Prices <- readdata( "AGLU_LEVEL1_DATA", "L132.ag_an_For_Prices" )

# -----------------------------------------------------------------------------
# 2. Build tables
# Make template table of available region x commodity x glu
L201.R_C_GLU_ag <- L103.ag_Prod_Mt_R_C_Y_GLU[ R_C_GLU ]

# biograss: available anywhere that has any crop production at all
L201.R_C_GLU_biograss <- unique( L201.R_C_GLU_ag[ R_GLU ] )
L201.R_C_GLU_biograss[[C]] <- bio_grass_name

L201.R_C_GLU_For <- L123.For_Prod_bm3_R_Y_GLU[ R_C_GLU ]

# biotree: available anywhere that has any forest production at all
L201.R_C_GLU_biotree <- unique( L201.R_C_GLU_For[ R_GLU ] )
L201.R_C_GLU_biotree[[C]] <- bio_tree_name

# Pasture
L201.R_C_GLU_Past <- L123.ag_Prod_Mt_R_Past_Y_GLU[ R_C_GLU ]

#Bind them all together
L201.R_C_GLU <- rbind( L201.R_C_GLU_ag, L201.R_C_GLU_biograss, L201.R_C_GLU_For, L201.R_C_GLU_biotree, L201.R_C_GLU_Past )
L201.R_C_GLU <- L201.R_C_GLU[ order( L201.R_C_GLU[[R]], L201.R_C_GLU[[GLU]], L201.R_C_GLU[[C]] ), ]
L201.R_C_GLU <- add_region_name( L201.R_C_GLU )

printlog( "L201.AgSupplySector: Generic AgSupplySector characteristics (units, calprice, market, logit)" )
# At the supplysector (market) level, all regions get all supplysectors
L201.AgSectorLogitTables <- get_logit_fn_tables( A_AgSupplySector, names_AgSupplySectorLogitType,
                                                 base.header="AgSupplySector_", include.equiv.table=T, write.all.regions=T )

L201.AgSupplySector <- write_to_all_regions( A_AgSupplySector, names_AgSupplySector )
L201.AgSupplySector$calPrice <- L132.ag_an_For_Prices$calPrice[
  match( L201.AgSupplySector$AgSupplySector,
         L132.ag_an_For_Prices[[C]] ) ]
L201.AgSupplySector$calPrice[ L201.AgSupplySector$AgSupplySector == "biomass" ] <- 1   #value irrelevant
L201.AgSupplySector$market[ L201.AgSupplySector$market == "regional" ] <- L201.AgSupplySector$region[ L201.AgSupplySector$market == "regional" ]
L201.AgSupplySector <- L201.AgSupplySector[ names_AgSupplySector ]

printlog( "Removing no-aglu regions")
for( curr_table in names( L201.AgSectorLogitTables) ) {
  if( curr_table != "EQUIV_TABLE" ) {
    L201.AgSectorLogitTables[[ curr_table ]]$data <- subset( L201.AgSectorLogitTables[[ curr_table ]]$data,
                                                             !region %in% no_aglu_regions )
  }
}
L201.AgSupplySector <- subset( L201.AgSupplySector, !region %in% no_aglu_regions)

printlog( "L201.AgSupplySubsector: Generic AgSupplySubsector characteristics (none specified as competition is in the land allocator)" )
# At the subsector (production) level, only region x GLU combinations that actually exist are created
L201.AgSupplySubsector <- L201.R_C_GLU
L201.AgSupplySubsector[[agsupp]] <- A_AgSupplySubsector[[agsupp]][
  match( L201.AgSupplySubsector[[C]], A_AgSupplySubsector[[agsubs]] ) ]
# Subsector isn't just the supplysector & GLU for biomass crops, as this is where the grass/tree split is done
L201.AgSupplySubsector[[ agsubs ]] <- paste( L201.AgSupplySubsector[[C]], L201.AgSupplySubsector[[ GLU]], sep = crop_GLU_delimiter )
# We do not actually care about the logit here but we need a value to avoid errors
L201.AgSupplySubsector$logit.year.fillout <- min( model_base_years )
L201.AgSupplySubsector$logit.exponent <- -3
L201.AgSupplySubsector <- L201.AgSupplySubsector[ names_AgSupplySubsector ]

L201.AgSupplySubsectorLogitType <- L201.AgSupplySubsector
L201.AgSupplySubsectorLogitType$logit.type <- NA
L201.AgSubsectorLogitTables <- get_logit_fn_tables( L201.AgSupplySubsectorLogitType, names_AgSupplySubsectorLogitType,
                                                    base.header="AgSupplySubsector_", include.equiv.table=F, write.all.regions=F )

printlog( "L201.AgProduction_ag: Agricultural product calibrated output" )
#Start with all commodities, then filter just the ones under "ag"
L201.AgProduction <- L201.AgSupplySubsector
L201.AgProduction[[agtech]] <- L201.AgProduction[[agsubs]]
L201.AgProduction <- repeat_and_add_vector( L201.AgProduction, Y, model_base_years )
L201.AgProduction_ag <- L201.AgProduction[ L201.AgProduction[[agsupp]] %in% L103.ag_Prod_Mt_R_C_Y_GLU[[C]], ]

#Melt table of production
L201.ag_Prod_Mt_R_C_Y_GLU.melt <- interpolate_and_melt( L103.ag_Prod_Mt_R_C_Y_GLU,
                                                        years = model_base_years, value.name = "calOutputValue", digits = digits_calOutput )
L201.ag_Prod_Mt_R_C_Y_GLU.melt <- add_region_name( L201.ag_Prod_Mt_R_C_Y_GLU.melt )
L201.ag_Prod_Mt_R_C_Y_GLU.melt[[agtech]] <- paste( L201.ag_Prod_Mt_R_C_Y_GLU.melt[[C]], L201.ag_Prod_Mt_R_C_Y_GLU.melt[[GLU]], sep = crop_GLU_delimiter )

#Match in the production values
L201.AgProduction_ag$calOutputValue <- L201.ag_Prod_Mt_R_C_Y_GLU.melt$calOutputValue[
      match( vecpaste( L201.AgProduction_ag[ c( reg, agtech, Y ) ] ),
             vecpaste( L201.ag_Prod_Mt_R_C_Y_GLU.melt[ c( reg, agtech, Y ) ] ) ) ]

#Subsector and technology shareweights (subsector requires the year as well)
L201.AgProduction_ag$share.weight.year <- L201.AgProduction_ag[[Y]]
L201.AgProduction_ag$subs.share.weight <- ifelse( L201.AgProduction_ag$calOutputValue > 0, 1, 0 )
L201.AgProduction_ag$tech.share.weight <- ifelse( L201.AgProduction_ag$calOutputValue > 0, 1, 0 )
L201.AgProduction_ag <- L201.AgProduction_ag[ names_AgProduction ]

printlog( "L201.AgProduction_For: Forest product calibration (output)" )
#Melt production table
L201.For_Prod_bm3_R_Y_GLU.melt <- interpolate_and_melt( L123.For_Prod_bm3_R_Y_GLU,
                                                        years = model_base_years, value.name = "calOutputValue", digits = digits_calOutput )
L201.For_Prod_bm3_R_Y_GLU.melt <- add_region_name( L201.For_Prod_bm3_R_Y_GLU.melt )
L201.For_Prod_bm3_R_Y_GLU.melt[[agtech]] <- paste( L201.For_Prod_bm3_R_Y_GLU.melt[[C]], L201.For_Prod_bm3_R_Y_GLU.melt[[GLU]], sep = crop_GLU_delimiter )

#Subset only forest products from main table and paste in calibrated production, rounded
L201.AgProduction_For <- L201.AgProduction[ L201.AgProduction[[agsupp]] %in% L201.For_Prod_bm3_R_Y_GLU.melt[[C]], ]
L201.AgProduction_For$calOutputValue <- L201.For_Prod_bm3_R_Y_GLU.melt$calOutputValue[
      match( vecpaste( L201.AgProduction_For[ c( reg, agtech, Y ) ] ),
             vecpaste( L201.For_Prod_bm3_R_Y_GLU.melt[ c( reg, agtech, Y ) ] ) ) ]
L201.AgProduction_For$share.weight.year <- L201.AgProduction_For$year
L201.AgProduction_For$subs.share.weight <- ifelse( L201.AgProduction_For$calOutputValue > 0, 1, 0 )
L201.AgProduction_For$tech.share.weight <- ifelse( L201.AgProduction_For$calOutputValue > 0, 1, 0 )
L201.AgProduction_For <- L201.AgProduction_For[ names_AgProduction ]

printlog( "L201.AgProduction_Past: Pasture product calibration (output)" )
#Melt production table
L201.ag_Prod_Mt_R_Past_Y_GLU.melt <- interpolate_and_melt( L123.ag_Prod_Mt_R_Past_Y_GLU,
                                                           years = model_base_years, value.name = "calOutputValue", digits = digits_calOutput )
L201.ag_Prod_Mt_R_Past_Y_GLU.melt <- add_region_name( L201.ag_Prod_Mt_R_Past_Y_GLU.melt )
L201.ag_Prod_Mt_R_Past_Y_GLU.melt[[agtech]] <- paste( L201.ag_Prod_Mt_R_Past_Y_GLU.melt[[C]], L201.ag_Prod_Mt_R_Past_Y_GLU.melt[[GLU]], sep = crop_GLU_delimiter )

#Subset only pasture output from main table and paste in calibrated production, rounded
L201.AgProduction_Past <- L201.AgProduction[ L201.AgProduction$AgSupplySector %in% L201.ag_Prod_Mt_R_Past_Y_GLU.melt[[C]], ]
L201.AgProduction_Past$calOutputValue <- L201.ag_Prod_Mt_R_Past_Y_GLU.melt$calOutputValue[
      match( vecpaste( L201.AgProduction_Past[ c( reg, agtech, Y ) ] ),
             vecpaste( L201.ag_Prod_Mt_R_Past_Y_GLU.melt[ c( reg, agtech, Y ) ] ) ) ]
L201.AgProduction_Past$share.weight.year <- L201.AgProduction_Past$year
L201.AgProduction_Past$subs.share.weight <- ifelse( L201.AgProduction_Past$calOutputValue > 0, 1, 0 )
L201.AgProduction_Past$tech.share.weight <- ifelse( L201.AgProduction_Past$calOutputValue > 0, 1, 0 )
L201.AgProduction_Past <- L201.AgProduction_Past[ names_AgProduction ]

printlog( "L201.AgHAtoCL: Harvests per year" )
#Melt Harvested-area-to-cropland table
L201.ag_HA_to_CropLand_R_Y_GLU.melt <- interpolate_and_melt( L122.ag_HA_to_CropLand_R_Y_GLU,
                                                             years = model_years, value.name = "harvests.per.year", digits = digits_calOutput, rule = 2 )
L201.ag_HA_to_CropLand_R_Y_GLU.melt<- add_region_name( L201.ag_HA_to_CropLand_R_Y_GLU.melt )

#Paste in HA:CL to ag crops only
L201.AgHAtoCL <- subset( L201.AgProduction_ag, year == min( model_base_years ) )
L201.AgHAtoCL <- repeat_and_add_vector( L201.AgHAtoCL, Y, model_years )
L201.AgHAtoCL <- substring_GLU( L201.AgHAtoCL, from.var = agtech )
L201.AgHAtoCL$harvests.per.year <- L201.ag_HA_to_CropLand_R_Y_GLU.melt$harvests.per.year[
      match( vecpaste( L201.AgHAtoCL[ c( reg, GLU, Y ) ] ),
             vecpaste( L201.ag_HA_to_CropLand_R_Y_GLU.melt[ c( reg, GLU, Y ) ] ) ) ]
L201.AgHAtoCL <- L201.AgHAtoCL[ names_AgHAtoCL ]

printlog( "L201.AgYield_bio_grass: Base year biomass yields, grass bioenergy crops" )
L201.AgYield_bio_grass <- L201.AgProduction[ grepl( bio_grass_name, L201.AgProduction[[agsubs]] ), names_AgTechYr ]

#Add info to table on bioenergy grass yields
L201.ag_bioGrassYield_GJm2_R_GLU<- add_region_name( L113.ag_bioYield_GJm2_R_GLU )
L201.ag_bioGrassYield_GJm2_R_GLU[[C]] <- bio_grass_name
L201.ag_bioGrassYield_GJm2_R_GLU[[agsubs]] <- paste( L201.ag_bioGrassYield_GJm2_R_GLU[[C]], L201.ag_bioGrassYield_GJm2_R_GLU[[GLU]], sep = crop_GLU_delimiter )

#Match in the supply sector second, in case it's a different commodity (as opposed to a different subsector)
L201.ag_bioGrassYield_GJm2_R_GLU[[agsupp]] <- A_AgSupplySubsector[[agsupp]][ match( L201.ag_bioGrassYield_GJm2_R_GLU[[C]], A_AgSupplySubsector[[agsubs]] ) ]

#Biomass grass yields
L201.AgYield_bio_grass$yield <- round( L201.ag_bioGrassYield_GJm2_R_GLU$Yield_GJm2[
      match( vecpaste( L201.AgYield_bio_grass[ c( reg, agsupp, agsubs ) ] ),
             vecpaste( L201.ag_bioGrassYield_GJm2_R_GLU[ c( reg, agsupp, agsubs ) ] ) ) ],
      digits_calOutput )
L201.AgYield_bio_grass <- L201.AgYield_bio_grass[ names_AgYield ]

printlog( "L201.AgYield_bio_tree: Base year biomass yields, tree bioenergy crops" )
L201.AgYield_bio_tree <- L201.AgProduction[ grepl( bio_tree_name, L201.AgProduction[[agsubs]] ), names_AgTechYr ]

#Just use the grass yields where available. Where not available (i.e., where there is forest but not cropped land),
# use a default minimum as these are likely remote / unpopulated lands
L201.AgYield_bio_grass <- substring_GLU( L201.AgYield_bio_grass, from.var = agtech )
L201.AgYield_bio_tree$yield <- NA
L201.AgYield_bio_tree <- substring_GLU( L201.AgYield_bio_tree, from.var = agtech )
L201.AgYield_bio_tree$yield <- L201.AgYield_bio_grass$yield[
  match( vecpaste( L201.AgYield_bio_tree[ c( reg, GLU ) ] ),
         vecpaste( L201.AgYield_bio_grass[ c( reg, GLU ) ] ) ) ]
L201.AgYield_bio_tree$yield[ is.na( L201.AgYield_bio_tree$yield ) ] <-
  min( L201.AgYield_bio_tree$yield, na.rm = T )
L201.AgYield_bio_tree <- L201.AgYield_bio_tree[ names_AgYield ]
L201.AgYield_bio_grass <- L201.AgYield_bio_grass[ names_AgYield ]

# the method below, commented out, would use carbon density-derived assumptions of forest yields in each land use region
# the problem with this method is that it isn't indexed to the value of agricultural land, unlike the assumed unmanaged land
# values (and also unlike the bio grass crop yields, which are derived from ag crop yields).
# This means that in places where ag land (and therefore unmanaged land) has low value, the bio tree crop profit rates are
# comparatively very high, and gain very high market share.
# the code to set bio tree yields according to estimated forest primary productivity is kept in case this issue is addressed
if( FALSE ){
  #Add info to table on bioenergy tree yields
  L123.For_Yield_m3m2_R_GLU$yield <- L123.For_Yield_m3m2_R_GLU[[X_final_historical_year]]
  
  #This contains zeroes in places without any logging in the final historical year. replace with an assumed minimum value
  L123.For_Yield_m3m2_R_GLU$yield[ L123.For_Yield_m3m2_R_GLU$yield == 0 ] <-
    min( L123.For_Yield_m3m2_R_GLU$yield[ L123.For_Yield_m3m2_R_GLU$yield != 0 ] )
  
  #Also need to re-set yields in places where roundwood production compared with available forest land returns unrealistically high yields
  # could happen either from bad data, or from places with unsustainably high logging rates in the final historical year
  L123.For_Yield_m3m2_R_GLU$yield <- pmin( L123.For_Yield_m3m2_R_GLU$yield, quantile( L123.For_Yield_m3m2_R_GLU$yield, probs = 0.8 ) )
  L201.ag_bioTreeYield_GJm2_R_GLU <- L123.For_Yield_m3m2_R_GLU[ c( R_C_GLU, "yield" ) ]
  L201.ag_bioTreeYield_GJm2_R_GLU$Yield_GJm2 <- L201.ag_bioTreeYield_GJm2_R_GLU$yield * AvgWoodDensity_kgm3 * WoodEnergyContent_GJkg
  L201.ag_bioTreeYield_GJm2_R_GLU <- add_region_name( L201.ag_bioTreeYield_GJm2_R_GLU )
  L201.ag_bioTreeYield_GJm2_R_GLU[[C]] <- bio_tree_name
  L201.ag_bioTreeYield_GJm2_R_GLU[[agsubs]] <- paste( L201.ag_bioTreeYield_GJm2_R_GLU[[C]], L201.ag_bioTreeYield_GJm2_R_GLU[[GLU]], sep = crop_GLU_delimiter )
  
  #Match in the supply sector second, in case it's a different commodity (as opposed to a different subsector)
  L201.ag_bioTreeYield_GJm2_R_GLU[[agsupp]] <- A_AgSupplySubsector[[agsupp]][ match( L201.ag_bioTreeYield_GJm2_R_GLU[[C]], A_AgSupplySubsector[[agsubs]] ) ]
  
  #Biomass tree yields
  L201.AgYield_bio_tree$yield <- round( L201.ag_bioTreeYield_GJm2_R_GLU$Yield_GJm2[
    match( vecpaste( L201.AgYield_bio_tree[ c( reg, agsupp, agsubs ) ] ),
           vecpaste( L201.ag_bioTreeYield_GJm2_R_GLU[ c( reg, agsupp, agsubs ) ] ) ) ],
    digits_calOutput )
  L201.AgYield_bio_tree <- L201.AgYield_bio_tree[ names_AgYield ]
}

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

for( curr_table in names ( L201.AgSectorLogitTables) ) {
  write_mi_data( L201.AgSectorLogitTables[[ curr_table ]]$data, L201.AgSectorLogitTables[[ curr_table ]]$header,
                 "AGLU_LEVEL2_DATA", paste0("L201.", L201.AgSectorLogitTables[[ curr_table ]]$header ), "AGLU_XML_BATCH",
                 "batch_ag_For_Past_bio_base.xml" )
}
write_mi_data( L201.AgSupplySector, IDstring="AgSupplySector", domain="AGLU_LEVEL2_DATA", fn="L201.AgSupplySector",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_For_Past_bio_base.xml" ) 
for( curr_table in names ( L201.AgSubsectorLogitTables ) ) {
write_mi_data( L201.AgSubsectorLogitTables[[ curr_table ]]$data, L201.AgSubsectorLogitTables[[ curr_table ]]$header,
    "AGLU_LEVEL2_DATA", paste0("L201.", L201.AgSubsectorLogitTables[[ curr_table ]]$header ), "AGLU_XML_BATCH",
    "batch_ag_For_Past_bio_base.xml" )
}
write_mi_data( L201.AgSupplySubsector, "AgSupplySubsector", "AGLU_LEVEL2_DATA", "L201.AgSupplySubsector", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base.xml" ) 
write_mi_data( L201.AgProduction_ag, "AgProduction", "AGLU_LEVEL2_DATA", "L201.AgProduction_ag", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base.xml" ) 
write_mi_data( L201.AgProduction_For, "AgProduction", "AGLU_LEVEL2_DATA", "L201.AgProduction_For", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base.xml" ) 
write_mi_data( L201.AgProduction_Past, "AgProduction", "AGLU_LEVEL2_DATA", "L201.AgProduction_Past", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base.xml" ) 
write_mi_data( L201.AgHAtoCL, "AgHAtoCL", "AGLU_LEVEL2_DATA", "L201.AgHAtoCL", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base.xml" ) 
write_mi_data( L201.AgYield_bio_grass, "AgYield", "AGLU_LEVEL2_DATA", "L201.AgYield_bio_grass", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base.xml" ) 
write_mi_data( L201.AgYield_bio_tree, "AgYield", "AGLU_LEVEL2_DATA", "L201.AgYield_bio_tree", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base.xml" ) 

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base.xml", "AGLU_XML_FINAL", "ag_For_Past_bio_base.xml", "", xml_tag="outFile" )

logstop()
