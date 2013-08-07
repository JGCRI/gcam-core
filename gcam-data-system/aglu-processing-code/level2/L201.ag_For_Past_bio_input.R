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
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A_AgSupplySector <- readdata( "AGLU_ASSUMPTIONS", "A_agSupplySector" )
A_AgSupplySubsector <- readdata( "AGLU_ASSUMPTIONS", "A_agSupplySubsector" )
A_AgTechnology <- readdata( "AGLU_ASSUMPTIONS", "A_AgTechnology" )
A_bio_cost_yield <- readdata( "AGLU_ASSUMPTIONS", "A_bio_cost_yield" )
A_biocrops_R_AEZ <- readdata( "AGLU_ASSUMPTIONS", "A_biocrops_R_AEZ" )
L104.ag_Prod_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L104.ag_Prod_Mt_R_C_Y_AEZ" )
L113.ag_bioYield_GJm2_R_AEZ_ref <- readdata( "AGLU_LEVEL1_DATA", "L113.ag_bioYield_GJm2_R_AEZ_ref" )
L113.ag_bioYield_GJm2_R_AEZ_hi <- readdata( "AGLU_LEVEL1_DATA", "L113.ag_bioYield_GJm2_R_AEZ_hi" )
L122.ag_HA_to_CropLand_R_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L122.ag_HA_to_CropLand_R_Y_AEZ" )
L123.ag_Prod_Mt_R_Past_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L123.ag_Prod_Mt_R_Past_Y_AEZ" )
L123.For_Prod_bm3_R_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L123.For_Prod_bm3_R_Y_AEZ" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )
L132.ag_an_For_Prices <- readdata( "AGLU_LEVEL1_DATA", "L132.ag_an_For_Prices" )

# -----------------------------------------------------------------------------
# 2. Build tables
#Region by AgSupplysector base table
printlog( "L201.AgSupplySector: Generic AgSupplySector characteristics (units, calprice, market, logit)" )
L201.AgSupplySector <- write_to_all_regions_ag( A_AgSupplySector, names_AgSupplySector )
L201.AgSupplySector$calPrice <- L132.ag_an_For_Prices$calPrice[ match( L201.AgSupplySector$AgSupplySector, L132.ag_an_For_Prices$GCAM_commodity ) ]
L201.AgSupplySector$calPrice[ L201.AgSupplySector$AgSupplySector == "biomass" ] <- 0.7   #value irrelevant
L201.AgSupplySector$market[ L201.AgSupplySector$market == "regional" ] <- L201.AgSupplySector$region[ L201.AgSupplySector$market == "regional" ]
L201.AgSupplySector <- L201.AgSupplySector[ names_AgSupplySector ]

printlog( "L201.AgSupplySector_biomassOil: AgSupplySector characteristics for biomassOil (only in regions where applicable)" )
L201.biomassOil_R <- A_biocrops_R_AEZ[ A_biocrops_R_AEZ$AgSupplySector == "biomassOil" , ]
L201.AgSupplySector_biomassOil <- data.frame( region = unique( L201.biomassOil_R$region ) )
L201.AgSupplySector_biomassOil[ names( A_AgSupplySector ) ] <- subset( A_AgSupplySector, AgSupplySector == "biomass" )
L201.AgSupplySector_biomassOil$AgSupplySector <- "biomassOil"
L201.AgSupplySector_biomassOil$calPrice <- 0.7    #Value irrelevant
L201.AgSupplySector_biomassOil$market[ L201.AgSupplySector_biomassOil$market == "regional" ] <-
      L201.AgSupplySector_biomassOil$region[ L201.AgSupplySector_biomassOil$market == "regional" ]
L201.AgSupplySector_biomassOil <- L201.AgSupplySector_biomassOil[ names_AgSupplySector ]

printlog( "L201.AgSupplySubsector: Generic AgSupplySubsector characteristics (logit, shareweight interpolation)" )
A_AgSupplySubsector_repAEZ <- repeat_and_add_vector( A_AgSupplySubsector, AEZ, AEZs )
A_AgSupplySubsector_repAEZ$AgSupplySubsector <- with( A_AgSupplySubsector_repAEZ, paste( AgSupplySector, AEZ, sep = AEZ_delimiter ) )
L201.AgSupplySubsector <- write_to_all_regions_ag( A_AgSupplySubsector_repAEZ, names_AgSupplySubsector )

printlog( "L201.AgTechInterp: Generic AgProductionTechnology characteristics (share-weight interpolation)" )
A_AgTechnology_repAEZ <- repeat_and_add_vector( A_AgTechnology, AEZ, AEZs )
A_AgTechnology_repAEZ$AgSupplySubsector <- with( A_AgTechnology_repAEZ, paste( AgSupplySector, AEZ, sep = AEZ_delimiter ) )
A_AgTechnology_repAEZ$AgProductionTechnology <- with( A_AgTechnology_repAEZ, paste( AgSupplySector, AEZ, sep = AEZ_delimiter ) )
L201.AgTechInterp <- write_to_all_regions_ag( A_AgTechnology_repAEZ, names_AgTechInterp )

printlog( "L201.AgTechShrwt: AgProductionTechnology shareweights" )
L201.AgTechShrwt <- repeat_and_add_vector( L201.AgTechInterp[ names( L201.AgTechInterp ) %in% names_AgTechShrwt ], Y, c( model_base_years, model_future_years ) )
L201.AgTechShrwt$share.weight <- 1
L201.AgTechShrwt <- L201.AgTechShrwt[ names_AgTechShrwt ]

printlog( "L201.AgProduction_ag: Agricultural product calibrated output" )
L201.AgProduction <- L201.AgTechInterp[ names( L201.AgTechInterp ) %in% names_AgProduction ]
L201.AgProduction <- repeat_and_add_vector( L201.AgProduction, Y, model_base_years )
L201.AgProduction_ag <- L201.AgProduction[ L201.AgProduction$AgSupplySector %in% L104.ag_Prod_Mt_R_C_Y_AEZ[[C]], ]

#Melt table of production
L201.ag_Prod_Mt_R_C_Y_AEZ.melt <- melt( L104.ag_Prod_Mt_R_C_Y_AEZ, id.vars = R_C_AEZ, variable_name = Y )
L201.ag_Prod_Mt_R_C_Y_AEZ.melt[[Y]] <- sub( "X", "", L201.ag_Prod_Mt_R_C_Y_AEZ.melt[[Y]])
L201.ag_Prod_Mt_R_C_Y_AEZ.melt <- add_region_name( L201.ag_Prod_Mt_R_C_Y_AEZ.melt )
L201.ag_Prod_Mt_R_C_Y_AEZ.melt$AgProductionTechnology <- paste( L201.ag_Prod_Mt_R_C_Y_AEZ.melt[[C]], L201.ag_Prod_Mt_R_C_Y_AEZ.melt[[AEZ]], sep = AEZ_delimiter )

#Match in the production values
L201.AgProduction_ag$calOutputValue <- round( L201.ag_Prod_Mt_R_C_Y_AEZ.melt$value[
      match( vecpaste( L201.AgProduction_ag[ c( reg, agtech, Y ) ] ),
             vecpaste( L201.ag_Prod_Mt_R_C_Y_AEZ.melt[ c( reg, agtech, Y ) ] ) ) ],
      digits_calOutput )

#Subsector and technology shareweights (subsector requires the year as well)
L201.AgProduction_ag$share.weight.year <- L201.AgProduction_ag[[Y]]
L201.AgProduction_ag$subs.share.weight <- ifelse( L201.AgProduction_ag$calOutputValue > 0, 1, 0 )
L201.AgProduction_ag$tech.share.weight <- ifelse( L201.AgProduction_ag$calOutputValue > 0, 1, 0 )
L201.AgProduction_ag <- L201.AgProduction_ag[ names_AgProduction ]

printlog( "L201.AgProduction_For: Forest product calibration (output)" )
#Melt production table
L201.For_Prod_bm3_R_Y_AEZ.melt <- melt( L123.For_Prod_bm3_R_Y_AEZ, id.vars = R_C_AEZ, variable_name = Y )
L201.For_Prod_bm3_R_Y_AEZ.melt[[Y]] <- sub( "X", "", L201.For_Prod_bm3_R_Y_AEZ.melt[[Y]])
L201.For_Prod_bm3_R_Y_AEZ.melt <- add_region_name( L201.For_Prod_bm3_R_Y_AEZ.melt )
L201.For_Prod_bm3_R_Y_AEZ.melt$AgProductionTechnology <- paste( L201.For_Prod_bm3_R_Y_AEZ.melt[[C]], L201.For_Prod_bm3_R_Y_AEZ.melt[[AEZ]], sep = AEZ_delimiter )

#Subset only forest products from main table and paste in calibrated production, rounded
L201.AgProduction_For <- L201.AgProduction[ L201.AgProduction$AgSupplySector %in% L201.For_Prod_bm3_R_Y_AEZ.melt[[C]], ]
L201.AgProduction_For$calOutputValue <- round( L201.For_Prod_bm3_R_Y_AEZ.melt$value[
      match( vecpaste( L201.AgProduction_For[ c( reg, agtech, Y ) ] ),
             vecpaste( L201.For_Prod_bm3_R_Y_AEZ.melt[ c( reg, agtech, Y ) ] ) ) ],
      digits_calOutput )
L201.AgProduction_For$share.weight.year <- L201.AgProduction_For$year
L201.AgProduction_For$subs.share.weight <- ifelse( L201.AgProduction_For$calOutputValue > 0, 1, 0 )
L201.AgProduction_For$tech.share.weight <- ifelse( L201.AgProduction_For$calOutputValue > 0, 1, 0 )
L201.AgProduction_For <- L201.AgProduction_For[ names_AgProduction ]

printlog( "L201.AgProduction_Past: Pasture product calibration (output)" )
#Melt production table
L201.ag_Prod_Mt_R_Past_Y_AEZ.melt <- melt( L123.ag_Prod_Mt_R_Past_Y_AEZ, id.vars = R_C_AEZ, variable_name = Y )
L201.ag_Prod_Mt_R_Past_Y_AEZ.melt[[Y]] <- sub( "X", "", L201.ag_Prod_Mt_R_Past_Y_AEZ.melt[[Y]])
L201.ag_Prod_Mt_R_Past_Y_AEZ.melt <- add_region_name( L201.ag_Prod_Mt_R_Past_Y_AEZ.melt )
L201.ag_Prod_Mt_R_Past_Y_AEZ.melt$AgProductionTechnology <- paste( L201.ag_Prod_Mt_R_Past_Y_AEZ.melt[[C]], L201.ag_Prod_Mt_R_Past_Y_AEZ.melt[[AEZ]], sep = AEZ_delimiter )

#Subset only pasture output from main table and paste in calibrated production, rounded
L201.AgProduction_Past <- L201.AgProduction[ L201.AgProduction$AgSupplySector %in% L201.ag_Prod_Mt_R_Past_Y_AEZ.melt[[C]], ]
L201.AgProduction_Past$calOutputValue <- round( L201.ag_Prod_Mt_R_Past_Y_AEZ.melt$value[
      match( vecpaste( L201.AgProduction_Past[ c( reg, agtech, Y ) ] ),
             vecpaste( L201.ag_Prod_Mt_R_Past_Y_AEZ.melt[ c( reg, agtech, Y ) ] ) ) ],
      digits_calOutput )
L201.AgProduction_Past$share.weight.year <- L201.AgProduction_Past$year
L201.AgProduction_Past$subs.share.weight <- ifelse( L201.AgProduction_Past$calOutputValue > 0, 1, 0 )
L201.AgProduction_Past$tech.share.weight <- ifelse( L201.AgProduction_Past$calOutputValue > 0, 1, 0 )
L201.AgProduction_Past <- L201.AgProduction_Past[ names_AgProduction ]

printlog( "L201.AgHAtoCL: Harvests per year" )
#Melt Harvested-area-to-cropland table
L201.ag_HA_to_CropLand_R_Y_AEZ.melt <- melt( L122.ag_HA_to_CropLand_R_Y_AEZ, id.vars = R_AEZ, variable_name = Y )
L201.ag_HA_to_CropLand_R_Y_AEZ.melt[[Y]] <- sub( "X", "", L201.ag_HA_to_CropLand_R_Y_AEZ.melt[[Y]])
L201.ag_HA_to_CropLand_R_Y_AEZ.melt<- add_region_name( L201.ag_HA_to_CropLand_R_Y_AEZ.melt )

#Paste in HA:CL to ag crops table for base years (rounded)
L201.AgHAtoCL_base <- L201.AgProduction_ag[ names_AgTechYr ]
L201.AgHAtoCL_base[[AEZ]] <- with( L201.AgHAtoCL_base, substr( AgProductionTechnology, nchar( AgProductionTechnology ) - 4, nchar( AgProductionTechnology ) ) )
L201.AgHAtoCL_base$harvests.per.year <- round( L201.ag_HA_to_CropLand_R_Y_AEZ.melt$value[
      match( vecpaste( L201.AgHAtoCL_base[ c( reg, AEZ, Y ) ] ),
             vecpaste( L201.ag_HA_to_CropLand_R_Y_AEZ.melt[ c( reg, AEZ, Y ) ] ) ) ],
      digits_calOutput )

#For future years, repeat values in final base year by number of future periods
L201.AgHAtoCL_future <- repeat_and_add_vector( subset( L201.AgHAtoCL_base, year == max( year ) ), Y, model_future_years )

#Rbind the base and future year tables
L201.AgHAtoCL <- rbind( L201.AgHAtoCL_base, L201.AgHAtoCL_future )
L201.AgHAtoCL <- L201.AgHAtoCL[ names_AgHAtoCL ]

printlog( "L201.AgYield_bio_xxx: Base year biomass yields" )
L201.AgYield_bio <- L201.AgProduction[ L201.AgProduction$AgSupplySector == "biomass", names_AgTechYr ]
L201.AgYield_bio[[AEZ]] <- with( L201.AgYield_bio, substr( AgProductionTechnology, nchar( AgProductionTechnology ) - 4, nchar( AgProductionTechnology ) ) )

#Melt tables of base year biomass yields
L201.ag_bioYield_GJm2_R_AEZ_ref.melt <- melt( L113.ag_bioYield_GJm2_R_AEZ_ref, id.vars = R, variable_name = AEZ ) 
L201.ag_bioYield_GJm2_R_AEZ_ref.melt<- add_region_name( L201.ag_bioYield_GJm2_R_AEZ_ref.melt )

L201.ag_bioYield_GJm2_R_AEZ_hi.melt <- melt( L113.ag_bioYield_GJm2_R_AEZ_hi, id.vars = R, variable_name = AEZ ) 
L201.ag_bioYield_GJm2_R_AEZ_hi.melt<- add_region_name( L201.ag_bioYield_GJm2_R_AEZ_hi.melt )

#Reference scenario: paste in biomass yields
L201.AgYield_bio_ref <- L201.AgYield_bio
L201.AgYield_bio_ref$yield <- round( L201.ag_bioYield_GJm2_R_AEZ_ref.melt$value[
      match( vecpaste( L201.AgYield_bio_ref[ c( reg, AEZ ) ] ),
             vecpaste( L201.ag_bioYield_GJm2_R_AEZ_ref.melt[ c( reg, AEZ ) ] ) ) ],
      digits_calOutput )
L201.AgYield_bio_ref <- L201.AgYield_bio_ref[ names_AgYield ]

#High biomass yield scenario
L201.AgYield_bio_hi <- L201.AgYield_bio
L201.AgYield_bio_hi$yield <- round( L201.ag_bioYield_GJm2_R_AEZ_hi.melt$value[
      match( vecpaste( L201.AgYield_bio_hi[ c( reg, AEZ ) ] ),
             vecpaste( L201.ag_bioYield_GJm2_R_AEZ_hi.melt[ c( reg, AEZ ) ] ) ) ],
      digits_calOutput )
L201.AgYield_bio_hi <- L201.AgYield_bio_hi[ names_AgYield ]

printlog( "Renaming biomass crops in all tables to specified names" )
L201.AgSupplySubsector <- rename_biocrops( L201.AgSupplySubsector, lookup = A_biocrops_R_AEZ, data_matchvar = "AgSupplySubsector",
      lookup_matchvar = "old_AgSupplySubsector", "AgSupplySector", "AgSupplySubsector" )
L201.AgTechInterp <- rename_biocrops( L201.AgTechInterp, lookup = A_biocrops_R_AEZ, data_matchvar = "AgSupplySubsector",
      lookup_matchvar = "old_AgSupplySubsector", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )
L201.AgYield_bio_ref <- rename_biocrops( L201.AgYield_bio_ref, lookup = A_biocrops_R_AEZ, data_matchvar = "AgSupplySubsector",
      lookup_matchvar = "old_AgSupplySubsector", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )
L201.AgYield_bio_hi <- rename_biocrops( L201.AgYield_bio_hi, lookup = A_biocrops_R_AEZ, data_matchvar = "AgSupplySubsector",
      lookup_matchvar = "old_AgSupplySubsector", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )

#Reset yields for specified bioenergy crops
printlog( "NOTE: Using multipliers from maximum switchgrass yields for specified bioenergy crops in the base years" )
A_bio_cost_yield$yield_GJm2[ A_bio_cost_yield$GCAM_commodity == "biomass" ] <- max( L113.ag_bioYield_GJm2_R_AEZ_ref[ AEZs ] )
A_bio_cost_yield$yield_mult <- A_bio_cost_yield$yield_GJm2 / A_bio_cost_yield$yield_GJm2[ A_bio_cost_yield$GCAM_commodity == "biomass" ]

L201.AgYield_bio_ref[[C]] <- with( L201.AgYield_bio_ref, substr( AgSupplySubsector, 1, nchar( AgSupplySubsector ) - 5 ) )
L201.AgYield_bio_ref$yield_mult <- A_bio_cost_yield$yield_mult[ match( L201.AgYield_bio_ref[[C]], A_bio_cost_yield[[C]] ) ] 
L201.AgYield_bio_ref$yield <- round( L201.AgYield_bio_ref$yield * L201.AgYield_bio_ref$yield_mult, digits_calOutput )
L201.AgYield_bio_ref <- L201.AgYield_bio_ref[ names_AgYield ]

L201.AgYield_bio_hi$yield[ paste( L201.AgYield_bio_hi$region, L201.AgYield_bio_hi$AgSupplySubsector ) %in%
      paste( A_biocrops_R_AEZ$region, A_biocrops_R_AEZ$AgSupplySubsector ) ] <-
   L201.AgYield_bio_ref$yield[ paste( L201.AgYield_bio_ref$region, L201.AgYield_bio_ref$AgSupplySubsector ) %in%
      paste( A_biocrops_R_AEZ$region, A_biocrops_R_AEZ$AgSupplySubsector ) ]

printlog( "Removing non-existent AEZs from all tables")
L201.AgSupplySubsector <- remove_AEZ_nonexist( L201.AgSupplySubsector )
L201.AgTechInterp <- remove_AEZ_nonexist( L201.AgTechInterp )
L201.AgTechShrwt <- remove_AEZ_nonexist( L201.AgTechShrwt )
L201.AgProduction_ag <- remove_AEZ_nonexist( L201.AgProduction_ag )
L201.AgProduction_For <- remove_AEZ_nonexist( L201.AgProduction_For )
L201.AgProduction_Past <- remove_AEZ_nonexist( L201.AgProduction_Past )
L201.AgHAtoCL <- remove_AEZ_nonexist( L201.AgHAtoCL )
L201.AgYield_bio_ref <- remove_AEZ_nonexist( L201.AgYield_bio_ref )
L201.AgYield_bio_hi <- remove_AEZ_nonexist( L201.AgYield_bio_hi )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L201.AgSupplySector, IDstring="AgSupplySector", domain="AGLU_LEVEL2_DATA", fn="L201.AgSupplySector",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_For_Past_bio_base.xml" ) 
write_mi_data( L201.AgSupplySector_biomassOil, "AgSupplySector", "AGLU_LEVEL2_DATA", "L201.AgSupplySector_biomassOil", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base.xml" ) 
write_mi_data( L201.AgSupplySubsector, "AgSupplySubsector", "AGLU_LEVEL2_DATA", "L201.AgSupplySubsector", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base.xml" ) 
write_mi_data( L201.AgTechInterp, "AgTechInterp", "AGLU_LEVEL2_DATA", "L201.AgTechInterp", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base.xml" ) 
write_mi_data( L201.AgTechShrwt, "AgTechShrwt", "AGLU_LEVEL2_DATA", "L201.AgTechShrwt", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base.xml" ) 
write_mi_data( L201.AgProduction_ag, "AgProduction", "AGLU_LEVEL2_DATA", "L201.AgProduction_ag", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base.xml" ) 
write_mi_data( L201.AgProduction_For, "AgProduction", "AGLU_LEVEL2_DATA", "L201.AgProduction_For", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base.xml" ) 
write_mi_data( L201.AgProduction_Past, "AgProduction", "AGLU_LEVEL2_DATA", "L201.AgProduction_Past", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base.xml" ) 
write_mi_data( L201.AgHAtoCL, "AgHAtoCL", "AGLU_LEVEL2_DATA", "L201.AgHAtoCL", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base.xml" ) 
write_mi_data( L201.AgYield_bio_ref, "AgYield", "AGLU_LEVEL2_DATA", "L201.AgYield_bio_ref", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base.xml" ) 
write_mi_data( L201.AgYield_bio_hi, "AgYield", "AGLU_LEVEL2_DATA", "L201.AgYield_bio_hi", "AGLU_XML_BATCH", "batch_bio_hi.xml" ) 

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base.xml", "AGLU_XML_FINAL", "ag_For_Past_bio_base.xml", "", xml_tag="outFile" )

logstop()
