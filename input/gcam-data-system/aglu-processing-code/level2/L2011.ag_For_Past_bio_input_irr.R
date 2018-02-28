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
logstart( "L2011.ag_For_Past_bio_input_irr.R" )
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
basin_to_country_mapping <- readdata( "WATER_MAPPINGS", "basin_to_country_mapping" )
A_AgSupplySector <- readdata( "AGLU_ASSUMPTIONS", "A_agSupplySector" )
L161.ag_irrProd_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_irrProd_Mt_R_C_Y_GLU", replace_GLU = T )
L161.ag_rfdProd_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L161.ag_rfdProd_Mt_R_C_Y_GLU", replace_GLU = T )
L163.ag_irrBioYield_GJm2_R_GLU <- readdata( "AGLU_LEVEL1_DATA", "L163.ag_irrBioYield_GJm2_R_GLU", replace_GLU = T )
L163.ag_rfdBioYield_GJm2_R_GLU <- readdata( "AGLU_LEVEL1_DATA", "L163.ag_rfdBioYield_GJm2_R_GLU", replace_GLU = T )
L201.AgSupplySector <- readdata( "AGLU_LEVEL2_DATA", "L201.AgSupplySector", skip = 4 )
L201.AgSupplySubsector <- readdata( "AGLU_LEVEL2_DATA", "L201.AgSupplySubsector", skip = 4 )
L201.AgProduction_For <- readdata( "AGLU_LEVEL2_DATA", "L201.AgProduction_For", skip = 4 )
L201.AgProduction_Past <- readdata( "AGLU_LEVEL2_DATA", "L201.AgProduction_Past", skip = 4 )
L201.AgHAtoCL <- readdata( "AGLU_LEVEL2_DATA", "L201.AgHAtoCL", skip = 4 )
L201.AgYield_bio_grass <- readdata( "AGLU_LEVEL2_DATA", "L201.AgYield_bio_grass", skip = 4 )
L201.AgYield_bio_tree <- readdata( "AGLU_LEVEL2_DATA", "L201.AgYield_bio_tree", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables
printlog( "This file builds an XML input file that is stand-alone from (i.e., not read in after) ag_For_Past_bio_base.xml")
# Rather than process the information in parallel code, this file imports the output from the corresponding L201 file and
# makes modifications where necessary
L2011.AgSectorLogitTables <- get_logit_fn_tables( A_AgSupplySector, names_AgSupplySectorLogitType,
                                                  base.header="AgSupplySector_", include.equiv.table=T, write.all.regions=T )
printlog( "Removing no-aglu regions")
for( curr_table in names( L2011.AgSectorLogitTables) ) {
  if( curr_table != "EQUIV_TABLE" ) {
    L2011.AgSectorLogitTables[[ curr_table ]]$data <- subset( L2011.AgSectorLogitTables[[ curr_table ]]$data,
                                                             !region %in% no_aglu_regions )
  }
}

printlog( "L2011.AgSupplySector: no changes from L201" )
L2011.AgSupplySector <- L201.AgSupplySector

printlog( "L2011.AgSupplySubsector: no changes from L201" )
L2011.AgSupplySubsector <- L201.AgSupplySubsector

# We do not actually care about the logit here but we need a value to avoid errors
L2011.AgSupplySubsectorLogitType <- L2011.AgSupplySubsector
L2011.AgSupplySubsectorLogitType$logit.type <- NA
L2011.AgSubsectorLogitTables <- get_logit_fn_tables( L2011.AgSupplySubsectorLogitType, names_AgSupplySubsectorLogitType,
                                                    base.header="AgSupplySubsector_", include.equiv.table=F, write.all.regions=F )

printlog( "L2011.AgProduction_ag_irr: agricultural product calibrated output, with irr/rfd disaggregated" )
L2011.ag_irrProd_Mt_R_C_Y_GLU.melt <- interpolate_and_melt( L161.ag_irrProd_Mt_R_C_Y_GLU,
                                                            model_base_years, "calOutputValue", digits_calOutput )
L2011.ag_irrProd_Mt_R_C_Y_GLU.melt[[irr]] <- "IRR"
L2011.ag_rfdProd_Mt_R_C_Y_GLU.melt <- interpolate_and_melt( L161.ag_rfdProd_Mt_R_C_Y_GLU,
                                                            model_base_years, "calOutputValue", digits_calOutput )
L2011.ag_rfdProd_Mt_R_C_Y_GLU.melt[[irr]] <- "RFD"
L2011.AgProduction_ag_irr <- rbind( L2011.ag_irrProd_Mt_R_C_Y_GLU.melt, L2011.ag_rfdProd_Mt_R_C_Y_GLU.melt )

#Add region name and agtech names
L2011.AgProduction_ag_irr <- add_region_name( L2011.AgProduction_ag_irr )
L2011.AgProduction_ag_irr <- add_agtech_names( L2011.AgProduction_ag_irr )
L2011.AgProduction_ag_irr$share.weight.year <- L2011.AgProduction_ag_irr[[Y]]
L2011.AgProduction_ag_irr <- set_subsector_shrwt( L2011.AgProduction_ag_irr, sector.name = "AgSupplySector", subsector.name = "AgSupplySubsector" )
L2011.AgProduction_ag_irr$tech.share.weight <- ifelse( L2011.AgProduction_ag_irr$calOutputValue > 0, 1, 0 )
L2011.AgProduction_ag_irr <- L2011.AgProduction_ag_irr[ names_AgProduction ]

printlog( "L2011.AgProduction_For: no changes from L201" )
L2011.AgProduction_For <- L201.AgProduction_For

printlog( "L2011.AgProduction_Past: no changes from L201" )
L2011.AgProduction_Past <- L201.AgProduction_Past

printlog( "L2011.AgHAtoCL_irr: same data as L201; just copy by irrigation level" )
# First, make sure that we aren't including pasture or forest. even if these are typically assigned HA_CL values,
# it isn't necessary as it's just the model default value
L2011.AgHAtoCL_irr <- L201.AgHAtoCL[ L201.AgHAtoCL[[agsupp]] %in% L2011.AgProduction_ag_irr[[agsupp]], ]
L2011.AgHAtoCL_irr <- repeat_and_add_vector( L2011.AgHAtoCL_irr, irr, c( "IRR", "RFD" ) )
L2011.AgHAtoCL_irr[[agtech]] <- paste( L2011.AgHAtoCL_irr[[agtech]], L2011.AgHAtoCL_irr[[irr]], sep = irr_delimiter )
L2011.AgHAtoCL_irr[[irr]] <- NULL

printlog( "L2011.AgYield_bio_grass_irr: yields of bioenergy grass crops, with irrigated and rainfed split")
L163.ag_irrBioYield_GJm2_R_GLU[[irr]] <- "IRR"
L163.ag_rfdBioYield_GJm2_R_GLU[[irr]] <- "RFD"
L2011.ag_BioYield_GJm2_R_GLU_irr <- rbind( L163.ag_irrBioYield_GJm2_R_GLU, L163.ag_rfdBioYield_GJm2_R_GLU )
L2011.ag_BioYield_GJm2_R_GLU_irr <- add_region_name( L2011.ag_BioYield_GJm2_R_GLU_irr )
L2011.ag_BioYield_GJm2_R_GLU_irr[[agsubs]] <- paste( bio_grass_name, L2011.ag_BioYield_GJm2_R_GLU_irr[[GLU]], sep = crop_GLU_delimiter )
L2011.ag_BioYield_GJm2_R_GLU_irr[[agtech]] <- paste( L2011.ag_BioYield_GJm2_R_GLU_irr[[agsubs]], L2011.ag_BioYield_GJm2_R_GLU_irr[[irr]], sep = irr_delimiter )

# because the agsupplysector may have a different name than the subsectors, use these data to match into the prior (L201) table
L2011.AgYield_bio_grass_irr <- repeat_and_add_vector( L201.AgYield_bio_grass, irr, c( "IRR", "RFD" ) )
L2011.AgYield_bio_grass_irr[[agtech]] <- paste( L2011.AgYield_bio_grass_irr[[agtech]], L2011.AgYield_bio_grass_irr[[irr]], sep = irr_delimiter )
L2011.AgYield_bio_grass_irr$yield <- L2011.ag_BioYield_GJm2_R_GLU_irr$Yield_GJm2[
  match( vecpaste( L2011.AgYield_bio_grass_irr[ c( reg, agtech ) ] ),
         vecpaste( L2011.ag_BioYield_GJm2_R_GLU_irr[ c( reg, agtech ) ] ) ) ]

# Places with no irrigated crop production will return missing values here. May as well set these yields to 0 to ensure that
# they get no irrigated bioenergy production in the future periods (most are tropical areas with no need for irrigation)
L2011.AgYield_bio_grass_irr[ is.na( L2011.AgYield_bio_grass_irr ) ] <- 0
L2011.AgYield_bio_grass_irr[[irr]] <- NULL

printlog( "L2011.AgYield_bio_tree_irr: yield of bioenergy tree crops. Use the irr:rfd yield ratios from grass crops" )
# This method essentially copies prior versions of GCAM with irrigation, albeit in a different sequence. Before, we used
# the generic bioenergy crop yields by irr/rfd, multiplied by an assumed tree:grass yield conversion factor. Here, we start with
# the generic tree bioenergy crop yields in each land use region, and multiply by generic:irr and generic:rfd conversion factors.
L2011.AgYield_bio_tree_irr <- repeat_and_add_vector( L201.AgYield_bio_tree, irr, c( "IRR", "RFD" ) )
L2011.AgYield_bio_tree_irr[[agtech]] <- paste( L2011.AgYield_bio_tree_irr[[agtech]], L2011.AgYield_bio_tree_irr[[irr]], sep = irr_delimiter )

# Compile the generic:irr and generic:rfd conversion factors
L2011.irr_rfd_factors <- L2011.AgYield_bio_grass_irr
L2011.irr_rfd_factors$generic.yield <- L201.AgYield_bio_grass$yield[
  match( vecpaste( L2011.irr_rfd_factors[ c( reg, agsubs ) ] ),
         vecpaste( L201.AgYield_bio_grass[ c( reg, agsubs ) ] ) ) ]
L2011.irr_rfd_factors$factor <- with( L2011.irr_rfd_factors, yield / generic.yield )
L2011.irr_rfd_factors[[agtech]] <- sub( bio_grass_name, bio_tree_name, L2011.irr_rfd_factors[[agtech]] )

#Multiply generic tree crop yields by the conversion factors
# Not all regions that have tree crops also have grass crops (e.g., regions w forest but no ag production)
# Simply use the defaults for these, which are likely minor ag regions anyway
L2011.AgYield_bio_tree_irr$yield_irr <- L2011.AgYield_bio_tree_irr$yield * L2011.irr_rfd_factors$factor[
  match( vecpaste( L2011.AgYield_bio_tree_irr[ c( reg, agtech ) ] ),
         vecpaste( L2011.irr_rfd_factors[ c( reg, agtech ) ] ) ) ]
L2011.AgYield_bio_tree_irr$yield[ !is.na( L2011.AgYield_bio_tree_irr$yield_irr ) ] <-
  L2011.AgYield_bio_tree_irr$yield_irr[ !is.na( L2011.AgYield_bio_tree_irr$yield_irr ) ]
L2011.AgYield_bio_tree_irr$yield <- round( L2011.AgYield_bio_tree_irr$yield, digits_calOutput )
L2011.AgYield_bio_tree_irr <- L2011.AgYield_bio_tree_irr[ names_AgYield]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
for( curr_table in names ( L2011.AgSectorLogitTables) ) {
  write_mi_data( L2011.AgSectorLogitTables[[ curr_table ]]$data, L2011.AgSectorLogitTables[[ curr_table ]]$header,
                 "AGLU_LEVEL2_DATA", paste0("L2011.", L2011.AgSectorLogitTables[[ curr_table ]]$header ), "AGLU_XML_BATCH",
                 "batch_ag_For_Past_bio_base_IRR.xml" )
}
write_mi_data( L2011.AgSupplySector, IDstring="AgSupplySector", domain="AGLU_LEVEL2_DATA", fn="L2011.AgSupplySector",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_For_Past_bio_base_IRR.xml" ) 
for( curr_table in names ( L2011.AgSubsectorLogitTables ) ) {
  write_mi_data( L2011.AgSubsectorLogitTables[[ curr_table ]]$data, L2011.AgSubsectorLogitTables[[ curr_table ]]$header,
                 "AGLU_LEVEL2_DATA", paste0("L2011.", L2011.AgSubsectorLogitTables[[ curr_table ]]$header ), "AGLU_XML_BATCH",
                 "batch_ag_For_Past_bio_base_IRR.xml" )
}
write_mi_data( L2011.AgSupplySubsector, "AgSupplySubsector", "AGLU_LEVEL2_DATA", "L2011.AgSupplySubsector", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR.xml" ) 
write_mi_data( L2011.AgProduction_ag_irr, "AgProduction", "AGLU_LEVEL2_DATA", "L2011.AgProduction_ag_irr", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR.xml" ) 
write_mi_data( L2011.AgProduction_For, "AgProduction", "AGLU_LEVEL2_DATA", "L2011.AgProduction_For", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR.xml" ) 
write_mi_data( L2011.AgProduction_Past, "AgProduction", "AGLU_LEVEL2_DATA", "L2011.AgProduction_Past", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR.xml" ) 
write_mi_data( L2011.AgHAtoCL_irr, "AgHAtoCL", "AGLU_LEVEL2_DATA", "L2011.AgHAtoCL_irr", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR.xml" ) 
write_mi_data( L2011.AgYield_bio_grass_irr, "AgYield", "AGLU_LEVEL2_DATA", "L2011.AgYield_bio_grass_irr", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR.xml" ) 
write_mi_data( L2011.AgYield_bio_tree_irr, "AgYield", "AGLU_LEVEL2_DATA", "L2011.AgYield_bio_tree_irr", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR.xml" ) 

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR.xml", "AGLU_XML_FINAL", "ag_For_Past_bio_base_IRR.xml", "", xml_tag="outFile" )

logstop()
