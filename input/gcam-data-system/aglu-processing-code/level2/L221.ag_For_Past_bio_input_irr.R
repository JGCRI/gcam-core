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
logstart( "L221.ag_For_Past_bio_input_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Model input for production of primary agricultural products / pasture / forest products with irrigated / rainfed split" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A_AgSupplySector <- readdata( "AGLU_ASSUMPTIONS", "A_agSupplySector" )
A_AgSupplySubsector <- readdata( "AGLU_ASSUMPTIONS", "A_agSupplySubsector" )
A_AgTechnology <- readdata( "AGLU_ASSUMPTIONS", "A_agTechnology" )
A_bio_cost_yield <- readdata( "AGLU_ASSUMPTIONS", "A_bio_cost_yield" )
A_biocrops_R_AEZ_irr <- readdata( "AGLU_ASSUMPTIONS", "A_biocrops_R_AEZ_irr" )
L104.ag_Prod_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L104.ag_Prod_Mt_R_C_Y_AEZ" )
L163.ag_irrBioYield_GJm2_R_AEZ_ref <- readdata( "AGLU_LEVEL1_DATA", "L163.ag_irrBioYield_GJm2_R_AEZ_ref" )
L163.ag_irrBioYield_GJm2_R_AEZ_hi <- readdata( "AGLU_LEVEL1_DATA", "L163.ag_irrBioYield_GJm2_R_AEZ_hi" )
L163.ag_rfdBioYield_GJm2_R_AEZ_ref <- readdata( "AGLU_LEVEL1_DATA", "L163.ag_rfdBioYield_GJm2_R_AEZ_ref" )
L163.ag_rfdBioYield_GJm2_R_AEZ_hi <- readdata( "AGLU_LEVEL1_DATA", "L163.ag_rfdBioYield_GJm2_R_AEZ_hi" )
L122.ag_HA_to_CropLand_R_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L122.ag_HA_to_CropLand_R_Y_AEZ" )
L123.ag_Prod_Mt_R_Past_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L123.ag_Prod_Mt_R_Past_Y_AEZ" )
L123.For_Prod_bm3_R_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L123.For_Prod_bm3_R_Y_AEZ" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )
L132.ag_an_For_Prices <- readdata( "AGLU_LEVEL1_DATA", "L132.ag_an_For_Prices" )
L161.ag_irrProd_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrProd_Mt_R_C_Y_AEZ" )
L161.ag_rfdProd_Mt_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_rfdProd_Mt_R_C_Y_AEZ" )
#L154_ag_subsidies <- readdata( "AGLU_LEVEL1_DATA", "L154_ag_subsidies" )

# -----------------------------------------------------------------------------
# 2. Build tables
printlog( "L221.AgSupplySector: Generic AgSupplySector characteristics (units, calprice, market, logit)" )
L221.AgSupplySector <- write_to_all_regions_ag( A_AgSupplySector, names_AgSupplySector )
L221.AgSupplySector$calPrice <- L132.ag_an_For_Prices$calPrice[ match( L221.AgSupplySector$AgSupplySector, L132.ag_an_For_Prices$GCAM_commodity ) ]
L221.AgSupplySector$calPrice[ L221.AgSupplySector$AgSupplySector == "biomass" ] <- 0.7   #value irrelevant
L221.AgSupplySector$market[ L221.AgSupplySector$market == "regional" ] <- L221.AgSupplySector$region[ L221.AgSupplySector$market == "regional" ]
L221.AgSupplySector <- L221.AgSupplySector[ names_AgSupplySector ]

printlog( "L221.AgSupplySector_biomassOil: AgSupplySector characteristics for biomassOil (only in regions where applicable)" )
L221.biomassOil_R <- A_biocrops_R_AEZ_irr[ A_biocrops_R_AEZ_irr$AgSupplySector == "biomassOil" , ]
L221.AgSupplySector_biomassOil <- data.frame( region = unique( L221.biomassOil_R$region ) )
L221.AgSupplySector_biomassOil[ names( A_AgSupplySector ) ] <- subset( A_AgSupplySector, AgSupplySector == "biomass" )
L221.AgSupplySector_biomassOil$AgSupplySector <- "biomassOil"
L221.AgSupplySector_biomassOil$calPrice <- 0.7    #Value irrelevant
L221.AgSupplySector_biomassOil$market[ L221.AgSupplySector_biomassOil$market == "regional" ] <-
      L221.AgSupplySector_biomassOil$region[ L221.AgSupplySector_biomassOil$market == "regional" ]
L221.AgSupplySector_biomassOil$logit.year.fillout <- min( model_base_years )
L221.AgSupplySector_biomassOil <- L221.AgSupplySector_biomassOil[ names_AgSupplySector ]

#printlog( "L221.AgSector: Regional Subsidies" )
#L221.AgSector_subs <- L221.AgSector[ c( "region", "AgSupplySector")]
#L221.AgSector_subs$subsidy <- L154_ag_subsidies$subsidy[
#      match( vecpaste( L221.AgSector[ c( "region", "AgSupplySector")] ),
#             vecpaste( L154_ag_subsidies[ c( "GCAM_region", "GCAM_commodity")] ) ) ]
#L221.AgSector_subs$subsidy[ is.na( L221.AgSector_subs$subsidy )] <- 0

printlog( "L221.AgSupplySubsector: Generic AgSupplySubsector characteristics (none specified as competition is in the land allocator)" )
A_AgSupplySubsector_repAEZ <- repeat_and_add_vector( A_AgSupplySubsector, AEZ, AEZs )
A_AgSupplySubsector_repAEZ$AgSupplySubsector <- with( A_AgSupplySubsector_repAEZ, paste( AgSupplySector, AEZ, sep = AEZ_delimiter ) )
L221.AgSupplySubsector <- write_to_all_regions_ag( A_AgSupplySubsector_repAEZ, names_AgSupplySubsector )
# We do not actually care about the logit here but we need a value to avoid errors
L221.AgSupplySubsector$logit.year.fillout <- min( model_base_years )
L221.AgSupplySubsector$logit.exponent <- -3
L221.AgSupplySubsectorLogitType <- L221.AgSupplySubsector
L221.AgSupplySubsectorLogitType$logit.type <- NA
L221.AgSubsectorLogitTables <- get_logit_fn_tables( L221.AgSupplySubsectorLogitType, names_AgSupplySubsectorLogitType,
                                                    base.header="AgSupplySubsector_", include.equiv.table=F, write.all.regions=F )


printlog( "L221.AgTechInterp: Generic AgProductionTechnology characteristics (share-weight interpolation)" )
A_AgTechnology_repAEZ <- repeat_and_add_vector( A_AgTechnology, AEZ, AEZs )
A_AgTechnology_repAEZ$AgSupplySubsector <- with( A_AgTechnology_repAEZ, paste( AgSupplySector, AEZ, sep = AEZ_delimiter ) )
non_irr_sectors <- c( "Forest", "Pasture" )
A_AgTechnology_repAEZ_irr <- repeat_and_add_vector( subset( A_AgTechnology_repAEZ, AgSupplySector %!in% non_irr_sectors ), "Irr_Rfd", c( "IRR", "RFD" ) )
A_AgTechnology_repAEZ_noirr <- data.frame(
      subset( A_AgTechnology_repAEZ, AgSupplySector %in% non_irr_sectors ),
      Irr_Rfd = "" )
A_AgTechnology_repAEZ_irr <- rbind( A_AgTechnology_repAEZ_irr, A_AgTechnology_repAEZ_noirr )
A_AgTechnology_repAEZ_irr$AgProductionTechnology <- with( A_AgTechnology_repAEZ_irr, paste( AgSupplySubsector, Irr_Rfd, sep = AEZ_delimiter ) )
L221.AgTechInterp <- write_to_all_regions_ag( A_AgTechnology_repAEZ_irr, names_AgTechInterp )

printlog( "L221.AgTechShrwt: AgProductionTechnology shareweights" )
L221.AgTechShrwt <- repeat_and_add_vector( L221.AgTechInterp[ names( L221.AgTechInterp ) %in% names_AgTechShrwt ], Y, model_years )
L221.AgTechShrwt$share.weight <- 1
L221.AgTechShrwt <- L221.AgTechShrwt[ names_AgTechShrwt ]

#Melt production tables
L161.ag_irrProd_Mt_R_C_Y_AEZ.melt <- melt( L161.ag_irrProd_Mt_R_C_Y_AEZ, id.vars = R_C_Y, variable.name = "AEZ" )
L161.ag_irrProd_Mt_R_C_Y_AEZ.melt$Irr_Rfd <- "IRR"
L161.ag_rfdProd_Mt_R_C_Y_AEZ.melt <- melt( L161.ag_rfdProd_Mt_R_C_Y_AEZ, id.vars = R_C_Y, variable.name = "AEZ" )
L161.ag_rfdProd_Mt_R_C_Y_AEZ.melt$Irr_Rfd <- "RFD"
L221.ag_Prod_Mt_R_C_Y_AEZ.melt <- rbind( L161.ag_irrProd_Mt_R_C_Y_AEZ.melt, L161.ag_rfdProd_Mt_R_C_Y_AEZ.melt )

L221.ag_Prod_Mt_R_C_Y_AEZ.melt <- add_region_name( L221.ag_Prod_Mt_R_C_Y_AEZ.melt )
L221.ag_Prod_Mt_R_C_Y_AEZ.melt$AgSupplySector <- L221.ag_Prod_Mt_R_C_Y_AEZ.melt$GCAM_commodity
L221.ag_Prod_Mt_R_C_Y_AEZ.melt$AgSupplySubsector <- paste(
      L221.ag_Prod_Mt_R_C_Y_AEZ.melt$GCAM_commodity,
      L221.ag_Prod_Mt_R_C_Y_AEZ.melt$AEZ, sep = AEZ_delimiter )
L221.ag_Prod_Mt_R_C_Y_AEZ.melt$AgProductionTechnology <- paste(
      L221.ag_Prod_Mt_R_C_Y_AEZ.melt$AgSupplySubsector,
      L221.ag_Prod_Mt_R_C_Y_AEZ.melt$Irr_Rfd, sep = AEZ_delimiter)
L221.ag_Prod_Mt_R_C_Y_AEZ.melt$SubsectorOutput <- L161.ag_irrProd_Mt_R_C_Y_AEZ.melt$value[
      match( vecpaste(L221.ag_Prod_Mt_R_C_Y_AEZ.melt[ R_C_Y_AEZ ]),
             vecpaste( L161.ag_irrProd_Mt_R_C_Y_AEZ.melt[ R_C_Y_AEZ ] ) ) ] +
    L161.ag_rfdProd_Mt_R_C_Y_AEZ.melt$value[
      match( vecpaste( L221.ag_Prod_Mt_R_C_Y_AEZ.melt[ R_C_Y_AEZ ] ),
             vecpaste( L161.ag_rfdProd_Mt_R_C_Y_AEZ.melt[ R_C_Y_AEZ ] ) ) ]
     
printlog( "L221.AgProduction_ag: Agricultural product calibrated output" )
L221.AgProduction_ag <- subset( L221.AgTechShrwt,
      AgSupplySector %in% L221.ag_Prod_Mt_R_C_Y_AEZ.melt$AgSupplySector &
      year %in% model_base_years )[ names_AgTechYr ]

L221.AgProduction_ag$calOutputValue <- round(
      L221.ag_Prod_Mt_R_C_Y_AEZ.melt$value[
         match( vecpaste( L221.AgProduction_ag[ names_AgTechYr ] ),
                vecpaste( L221.ag_Prod_Mt_R_C_Y_AEZ.melt[ names_AgTechYr ] ) ) ],
      digits_calOutput )
      
# Calculate subsector output.  Needed for subsector share weight.
L221.AgProduction_ag$SubsectorOutput <- round(
      L221.ag_Prod_Mt_R_C_Y_AEZ.melt$SubsectorOutput[
         match( vecpaste( L221.AgProduction_ag[ names_AgTechYr ] ),
                vecpaste( L221.ag_Prod_Mt_R_C_Y_AEZ.melt[ names_AgTechYr ] ) ) ],
      digits_calOutput )

#Subsector and technology shareweights (subsector requires the year as well)
L221.AgProduction_ag$share.weight.year <- L221.AgProduction_ag$year
L221.AgProduction_ag$subs.share.weight <- ifelse( L221.AgProduction_ag$SubsectorOutput > 0, 1, 0 )
L221.AgProduction_ag$tech.share.weight <- ifelse( L221.AgProduction_ag$calOutputValue > 0, 1, 0 )

#Remove extra columns
L221.AgProduction_ag <- L221.AgProduction_ag[ names_AgProduction ]

printlog( "L221.AgProduction_For: Forest product calibration (output)" )
#Melt production table
L221.For_Prod_bm3_R_Y_AEZ.melt <- melt( L123.For_Prod_bm3_R_Y_AEZ, id.vars = R_C_AEZ, variable.name = Y )
L221.For_Prod_bm3_R_Y_AEZ.melt[[Y]] <- sub( "X", "", L221.For_Prod_bm3_R_Y_AEZ.melt[[Y]])
L221.For_Prod_bm3_R_Y_AEZ.melt <- add_region_name( L221.For_Prod_bm3_R_Y_AEZ.melt )
L221.For_Prod_bm3_R_Y_AEZ.melt$AgProductionTechnology <- paste( L221.For_Prod_bm3_R_Y_AEZ.melt[[C]], L221.For_Prod_bm3_R_Y_AEZ.melt[[AEZ]], sep = AEZ_delimiter )

#Subset only forest products from main table and paste in calibrated production, rounded
L221.AgProduction_For <- subset( L221.AgTechShrwt,
      AgSupplySector == "Forest" &
      year %in% model_base_years )[ names_AgTechYr ]
L221.AgProduction_For$calOutputValue <- round( L221.For_Prod_bm3_R_Y_AEZ.melt$value[
      match( vecpaste( L221.AgProduction_For[ c( reg, agtech, Y ) ] ),
             vecpaste( L221.For_Prod_bm3_R_Y_AEZ.melt[ c( reg, agtech, Y ) ] ) ) ],
      digits_calOutput )
L221.AgProduction_For$share.weight.year <- L221.AgProduction_For$year
L221.AgProduction_For$subs.share.weight <- ifelse( L221.AgProduction_For$calOutputValue > 0, 1, 0 )
L221.AgProduction_For$tech.share.weight <- ifelse( L221.AgProduction_For$calOutputValue > 0, 1, 0 )
L221.AgProduction_For <- L221.AgProduction_For[ names_AgProduction ]

printlog( "L221.AgProduction_Past: Pasture product calibration (output)" )
#Melt production table
L221.ag_Prod_Mt_R_Past_Y_AEZ.melt <- melt( L123.ag_Prod_Mt_R_Past_Y_AEZ, id.vars = R_C_AEZ, variable.name = Y )
L221.ag_Prod_Mt_R_Past_Y_AEZ.melt[[Y]] <- sub( "X", "", L221.ag_Prod_Mt_R_Past_Y_AEZ.melt[[Y]])
L221.ag_Prod_Mt_R_Past_Y_AEZ.melt <- add_region_name( L221.ag_Prod_Mt_R_Past_Y_AEZ.melt )
L221.ag_Prod_Mt_R_Past_Y_AEZ.melt$AgProductionTechnology <- paste( L221.ag_Prod_Mt_R_Past_Y_AEZ.melt[[C]], L221.ag_Prod_Mt_R_Past_Y_AEZ.melt[[AEZ]], sep = AEZ_delimiter )

#Subset only pasture output from main table and paste in calibrated production, rounded
L221.AgProduction_Past <- subset( L221.AgTechShrwt,
      AgSupplySector == "Pasture" &
      year %in% model_base_years )[ names_AgTechYr ]
L221.AgProduction_Past$calOutputValue <- round( L221.ag_Prod_Mt_R_Past_Y_AEZ.melt$value[
      match( vecpaste( L221.AgProduction_Past[ c( reg, agtech, Y ) ] ),
             vecpaste( L221.ag_Prod_Mt_R_Past_Y_AEZ.melt[ c( reg, agtech, Y ) ] ) ) ],
      digits_calOutput )
L221.AgProduction_Past$share.weight.year <- L221.AgProduction_Past$year
L221.AgProduction_Past$subs.share.weight <- ifelse( L221.AgProduction_Past$calOutputValue > 0, 1, 0 )
L221.AgProduction_Past$tech.share.weight <- ifelse( L221.AgProduction_Past$calOutputValue > 0, 1, 0 )
L221.AgProduction_Past <- L221.AgProduction_Past[ names_AgProduction ]

printlog( "L221.AgHAtoCL: Harvests per year" )
#Melt Harvested-area-to-cropland table
L221.ag_HA_to_CropLand_R_Y_AEZ.melt <- melt( L122.ag_HA_to_CropLand_R_Y_AEZ, id.vars = R_AEZ, variable.name = Y )
L221.ag_HA_to_CropLand_R_Y_AEZ.melt[[Y]] <- sub( "X", "", L221.ag_HA_to_CropLand_R_Y_AEZ.melt[[Y]])
L221.ag_HA_to_CropLand_R_Y_AEZ.melt<- add_region_name( L221.ag_HA_to_CropLand_R_Y_AEZ.melt )

#Paste in HA:CL to ag crops table for base years (rounded)
L221.AgHAtoCL_base <- L221.AgProduction_ag[ names_AgTechYr ]
L221.AgHAtoCL_base[[AEZ]] <- with( L221.AgHAtoCL_base, substr( AgSupplySubsector, nchar( AgSupplySubsector ) - 4, nchar( AgSupplySubsector ) ) )
L221.AgHAtoCL_base$harvests.per.year <- round( L221.ag_HA_to_CropLand_R_Y_AEZ.melt$value[
      match( vecpaste( L221.AgHAtoCL_base[ c( reg, AEZ, Y ) ] ),
             vecpaste( L221.ag_HA_to_CropLand_R_Y_AEZ.melt[ c( reg, AEZ, Y ) ] ) ) ],
      digits_calOutput )

#For future years, repeat values in final base year by number of future periods
L221.AgHAtoCL_future <- repeat_and_add_vector( subset( L221.AgHAtoCL_base, year == max( year ) ), Y, model_future_years )

#Rbind the base and future year tables
L221.AgHAtoCL <- rbind( L221.AgHAtoCL_base, L221.AgHAtoCL_future )
L221.AgHAtoCL <- L221.AgHAtoCL[ names_AgHAtoCL ]

printlog( "L221.AgYield_bio_xxx: Base year biomass yields" )
L221.AgYield_bio <- subset( L221.AgTechShrwt,
      AgSupplySector == "biomass" &
      year %in% model_base_years )[ names_AgTechYr ]

#Melt tables of base year biomass yields
L221.ag_irrBioYield_GJm2_R_AEZ_ref.melt <- melt( L163.ag_irrBioYield_GJm2_R_AEZ_ref, id.vars = R, variable.name = AEZ ) 
L221.ag_irrBioYield_GJm2_R_AEZ_ref.melt<- add_region_name( L221.ag_irrBioYield_GJm2_R_AEZ_ref.melt )
L221.ag_irrBioYield_GJm2_R_AEZ_ref.melt$AgProductionTechnology <- paste( "biomass", L221.ag_irrBioYield_GJm2_R_AEZ_ref.melt$AEZ, "IRR", sep = AEZ_delimiter )

L221.ag_rfdBioYield_GJm2_R_AEZ_ref.melt <- melt( L163.ag_rfdBioYield_GJm2_R_AEZ_ref, id.vars = R, variable.name = AEZ ) 
L221.ag_rfdBioYield_GJm2_R_AEZ_ref.melt <- add_region_name( L221.ag_rfdBioYield_GJm2_R_AEZ_ref.melt )
L221.ag_rfdBioYield_GJm2_R_AEZ_ref.melt$AgProductionTechnology <- paste( "biomass", L221.ag_rfdBioYield_GJm2_R_AEZ_ref.melt$AEZ, "RFD", sep = AEZ_delimiter )

L221.ag_irrBioYield_GJm2_R_AEZ_hi.melt <- melt( L163.ag_irrBioYield_GJm2_R_AEZ_hi, id.vars = R, variable.name = AEZ ) 
L221.ag_irrBioYield_GJm2_R_AEZ_hi.melt<- add_region_name( L221.ag_irrBioYield_GJm2_R_AEZ_hi.melt )
L221.ag_irrBioYield_GJm2_R_AEZ_hi.melt$AgProductionTechnology <- paste( "biomass", L221.ag_irrBioYield_GJm2_R_AEZ_hi.melt$AEZ, "IRR", sep = AEZ_delimiter )

L221.ag_rfdBioYield_GJm2_R_AEZ_hi.melt <- melt( L163.ag_rfdBioYield_GJm2_R_AEZ_hi, id.vars = R, variable.name = AEZ ) 
L221.ag_rfdBioYield_GJm2_R_AEZ_hi.melt<- add_region_name( L221.ag_rfdBioYield_GJm2_R_AEZ_hi.melt )
L221.ag_rfdBioYield_GJm2_R_AEZ_hi.melt$AgProductionTechnology <- paste( "biomass", L221.ag_rfdBioYield_GJm2_R_AEZ_hi.melt$AEZ, "RFD", sep = AEZ_delimiter )

L221.ag_BioYield_GJm2_R_AEZ_ref.melt <- rbind( L221.ag_irrBioYield_GJm2_R_AEZ_ref.melt, L221.ag_rfdBioYield_GJm2_R_AEZ_ref.melt )
L221.ag_BioYield_GJm2_R_AEZ_hi.melt <- rbind( L221.ag_irrBioYield_GJm2_R_AEZ_hi.melt, L221.ag_rfdBioYield_GJm2_R_AEZ_hi.melt )

#Reference scenario: paste in biomass yields
L221.AgYield_bio_ref <- L221.AgYield_bio
L221.AgYield_bio_ref$yield <- round(
      L221.ag_BioYield_GJm2_R_AEZ_ref.melt$value[
          match( vecpaste( L221.AgYield_bio_ref[ c( reg, "AgProductionTechnology" ) ] ),
                 vecpaste( L221.ag_BioYield_GJm2_R_AEZ_ref.melt[ c( reg, "AgProductionTechnology" ) ] ) ) ],
      digits_calOutput )

#High biomass yield scenario
L221.AgYield_bio_hi <- L221.AgYield_bio
L221.AgYield_bio_hi$yield <- round(
L221.ag_BioYield_GJm2_R_AEZ_hi.melt$value[
          match( vecpaste( L221.AgYield_bio_hi[ c( reg, "AgProductionTechnology" ) ] ),
                 vecpaste( L221.ag_BioYield_GJm2_R_AEZ_hi.melt[ c( reg, "AgProductionTechnology" ) ] ) ) ],
      digits_calOutput )      

printlog( "Renaming biomass crops in all tables to specified names" )
L221.AgSupplySubsector <- rename_biocrops( L221.AgSupplySubsector, lookup = A_biocrops_R_AEZ_irr, data_matchvar = "AgSupplySubsector",
      lookup_matchvar = "old_AgSupplySubsector", "AgSupplySector", "AgSupplySubsector" )
L221.AgTechInterp <- rename_biocrops( L221.AgTechInterp, lookup = A_biocrops_R_AEZ_irr, data_matchvar = "AgProductionTechnology",
      lookup_matchvar = "old_AgProductionTechnology", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )
L221.AgTechShrwt <- rename_biocrops( L221.AgTechShrwt, lookup = A_biocrops_R_AEZ_irr, data_matchvar = "AgProductionTechnology",
      lookup_matchvar = "old_AgProductionTechnology", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )
L221.AgYield_bio_ref <- rename_biocrops( L221.AgYield_bio_ref, lookup = A_biocrops_R_AEZ_irr, data_matchvar = "AgProductionTechnology",
      lookup_matchvar = "old_AgProductionTechnology", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )
L221.AgYield_bio_hi <- rename_biocrops( L221.AgYield_bio_hi, lookup = A_biocrops_R_AEZ_irr, data_matchvar = "AgProductionTechnology",
      lookup_matchvar = "old_AgProductionTechnology", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology" )
      
#Reset yields for specified bioenergy crops
printlog( "NOTE: Using multipliers from switchgrass yields in the index AEZ in the USA to determine specified bioenergy crop yields" )
A_bio_cost_yield$yield_GJm2[ A_bio_cost_yield$GCAM_commodity == "biomass" ] <- max( L163.ag_rfdBioYield_GJm2_R_AEZ_ref[ AEZs ] )
A_bio_cost_yield$yield_mult <- A_bio_cost_yield$yield_GJm2 / A_bio_cost_yield$yield_GJm2[ A_bio_cost_yield$GCAM_commodity == "biomass" ]

L221.AgYield_bio_ref[[ C ]] <- with( L221.AgYield_bio_ref, substr( AgSupplySubsector, 1, nchar( AgSupplySubsector ) - 5 ) )
L221.AgYield_bio_ref$yield_mult <- A_bio_cost_yield$yield_mult[ match( L221.AgYield_bio_ref[[ C ]], A_bio_cost_yield[[ C ]] ) ] 
L221.AgYield_bio_ref$yield <- round( L221.AgYield_bio_ref$yield * L221.AgYield_bio_ref$yield_mult, digits_calOutput )
L221.AgYield_bio_ref <- L221.AgYield_bio_ref[ names_AgYield ]

L221.AgYield_bio_hi$yield[ paste( L221.AgYield_bio_hi$region, L221.AgYield_bio_hi$AgSupplySubsector ) %in%
      paste( A_biocrops_R_AEZ_irr$region, A_biocrops_R_AEZ_irr$AgSupplySubsector ) ] <-
    L221.AgYield_bio_ref$yield[ paste( L221.AgYield_bio_ref$region, L221.AgYield_bio_ref$AgSupplySubsector ) %in%
      paste( A_biocrops_R_AEZ_irr$region, A_biocrops_R_AEZ_irr$AgSupplySubsector ) ]

printlog( "Removing non-existent regions and AEZs from all tables")
L221.AgSupplySector <- subset( L221.AgSupplySector, !region %in% no_aglu_regions )
L221.AgSupplySector_biomassOil <- subset( L221.AgSupplySector_biomassOil, !region %in% no_aglu_regions )
L221.AgSupplySubsector <- remove_AEZ_nonexist( L221.AgSupplySubsector )
L221.AgTechInterp <- remove_AEZ_nonexist( L221.AgTechInterp )
L221.AgTechShrwt <- remove_AEZ_nonexist( L221.AgTechShrwt )
L221.AgProduction_ag <- remove_AEZ_nonexist( L221.AgProduction_ag )
L221.AgProduction_For <- remove_AEZ_nonexist( L221.AgProduction_For )
L221.AgProduction_Past <- remove_AEZ_nonexist( L221.AgProduction_Past )
L221.AgHAtoCL <- remove_AEZ_nonexist( L221.AgHAtoCL )
L221.AgYield_bio_ref <- remove_AEZ_nonexist( L221.AgYield_bio_ref )
L221.AgYield_bio_hi <- remove_AEZ_nonexist( L221.AgYield_bio_hi )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L221.AgSupplySector, IDstring="AgSupplySector", domain="AGLU_LEVEL2_DATA", fn="L221.AgSupplySector",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_For_Past_bio_base_IRR.xml" ) 
write_mi_data( L221.AgSupplySector_biomassOil, "AgSupplySector", "AGLU_LEVEL2_DATA", "L221.AgSupplySector_biomassOil", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR.xml" ) 
write_mi_data( L221.AgSupplySubsector, "AgSupplySubsector", "AGLU_LEVEL2_DATA", "L221.AgSupplySubsector", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR.xml" ) 
write_mi_data( L221.AgTechInterp, "AgTechInterp", "AGLU_LEVEL2_DATA", "L221.AgTechInterp", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR.xml" ) 
write_mi_data( L221.AgTechShrwt, "AgTechShrwt", "AGLU_LEVEL2_DATA", "L221.AgTechShrwt", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR.xml" ) 
write_mi_data( L221.AgProduction_ag, "AgProduction", "AGLU_LEVEL2_DATA", "L221.AgProduction_ag", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR.xml" ) 
write_mi_data( L221.AgProduction_For, "AgProduction", "AGLU_LEVEL2_DATA", "L221.AgProduction_For", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR.xml" ) 
write_mi_data( L221.AgProduction_Past, "AgProduction", "AGLU_LEVEL2_DATA", "L221.AgProduction_Past", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR.xml" ) 
write_mi_data( L221.AgHAtoCL, "AgHAtoCL", "AGLU_LEVEL2_DATA", "L221.AgHAtoCL", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR.xml" ) 
write_mi_data( L221.AgYield_bio_ref, "AgYield", "AGLU_LEVEL2_DATA", "L221.AgYield_bio_ref", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR.xml" ) 
write_mi_data( L221.AgYield_bio_hi, "AgYield", "AGLU_LEVEL2_DATA", "L221.AgYield_bio_hi", "AGLU_XML_BATCH", "batch_bio_hi_IRR.xml" ) 

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR.xml", "AGLU_XML_FINAL", "ag_For_Past_bio_base_IRR.xml", "", xml_tag="outFile" )

logstop()
