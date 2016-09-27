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
logstart( "L2012.ag_For_Past_bio_input_irr_mgmt.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Model input for production of primary agricultural products / pasture / forest products with management levels split" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A_AgSupplySector <- readdata( "AGLU_ASSUMPTIONS", "A_agSupplySector" )
L181.ag_Prod_Mt_R_C_Y_GLU_irr_level <- readdata( "AGLU_LEVEL1_DATA", "L181.ag_Prod_Mt_R_C_Y_GLU_irr_level" )
L181.YieldMult_R_bio_GLU_irr <- readdata( "AGLU_LEVEL1_DATA", "L181.YieldMult_R_bio_GLU_irr" )
L2011.AgSupplySector <- readdata( "AGLU_LEVEL2_DATA", "L2011.AgSupplySector", skip = 4 )
L2011.AgSupplySubsector <- readdata( "AGLU_LEVEL2_DATA", "L2011.AgSupplySubsector", skip = 4 )
L2011.AgProduction_ag_irr <- readdata( "AGLU_LEVEL2_DATA", "L2011.AgProduction_ag_irr", skip = 4 )
L2011.AgProduction_For <- readdata( "AGLU_LEVEL2_DATA", "L2011.AgProduction_For", skip = 4 )
L2011.AgProduction_Past <- readdata( "AGLU_LEVEL2_DATA", "L2011.AgProduction_Past", skip = 4 )
L2011.AgHAtoCL_irr <- readdata( "AGLU_LEVEL2_DATA", "L2011.AgHAtoCL_irr", skip = 4 )
L2011.AgYield_bio_grass_irr <- readdata( "AGLU_LEVEL2_DATA", "L2011.AgYield_bio_grass_irr", skip = 4 )
L2011.AgYield_bio_tree_irr <- readdata( "AGLU_LEVEL2_DATA", "L2011.AgYield_bio_tree_irr", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables
printlog( "Writing out tables that are identical (supplysector, subsector, forestry, pasture)" )
L2012.AgSectorLogitTables <- get_logit_fn_tables( A_AgSupplySector, names_AgSupplySectorLogitType,
                                                  base.header="AgSupplySector_", include.equiv.table=T, write.all.regions=T )
printlog( "Removing no-aglu regions")
for( curr_table in names( L2012.AgSectorLogitTables) ) {
  if( curr_table != "EQUIV_TABLE" ) {
    L2012.AgSectorLogitTables[[ curr_table ]]$data <- subset( L2012.AgSectorLogitTables[[ curr_table ]]$data,
                                                              !region %in% no_aglu_regions )
  }
}

L2012.AgSupplySector <- L2011.AgSupplySector
L2012.AgSupplySubsector <- L2011.AgSupplySubsector

# We do not actually care about the logit here but we need a value to avoid errors
L2012.AgSupplySubsectorLogitType <- L2012.AgSupplySubsector
L2012.AgSupplySubsectorLogitType$logit.type <- NA
L2012.AgSubsectorLogitTables <- get_logit_fn_tables( L2012.AgSupplySubsectorLogitType, names_AgSupplySubsectorLogitType,
                                                     base.header="AgSupplySubsector_", include.equiv.table=F, write.all.regions=F )

L2012.AgProduction_For <- L2011.AgProduction_For
L2012.AgProduction_Past <- L2011.AgProduction_Past

printlog( "Ag HA:CL: repeat by mgmt techs and copy values with no replacement" )
L2012.AgHAtoCL_irr_mgmt <- repeat_and_add_vector( L2011.AgHAtoCL_irr, lvl, c( "lo", "hi" ) )
L2012.AgHAtoCL_irr_mgmt[[agtech]] <- paste( L2012.AgHAtoCL_irr_mgmt[[agtech]], L2012.AgHAtoCL_irr_mgmt[[lvl]], sep = mgmt_delimiter )
L2012.AgHAtoCL_irr_mgmt <- L2012.AgHAtoCL_irr_mgmt[ names_AgHAtoCL ]

printlog( "Agricultural production: match in the values from the management-partitioned data" )
L2012.AgProduction_ag_irr_mgmt <- repeat_and_add_vector( L2011.AgProduction_ag_irr, lvl, c( "lo", "hi" ) )
L2012.AgProduction_ag_irr_mgmt <- substring_irr( L2012.AgProduction_ag_irr_mgmt, from.var = agtech, to.lower = T )
L2012.AgProduction_ag_irr_mgmt <- substring_GLU( L2012.AgProduction_ag_irr_mgmt, from.var = agsubs )
L2012.AgProduction_ag_irr_mgmt[[agtech]] <- paste( L2012.AgProduction_ag_irr_mgmt[[agtech]], L2012.AgProduction_ag_irr_mgmt[[lvl]], sep = mgmt_delimiter )

L181.ag_Prod_Mt_R_C_Y_GLU_irr_level.melt <- melt( L181.ag_Prod_Mt_R_C_Y_GLU_irr_level,
                                                  measure.vars = X_model_base_years,
                                                  id.vars = R_C_GLU_irr_lvl,
                                                  variable.name = Y )
L181.ag_Prod_Mt_R_C_Y_GLU_irr_level.melt[[Y]] <- as.numeric( substr( as.character( L181.ag_Prod_Mt_R_C_Y_GLU_irr_level.melt[[Y]] ), 2, 5 ) )
L181.ag_Prod_Mt_R_C_Y_GLU_irr_level.melt <- add_region_name( L181.ag_Prod_Mt_R_C_Y_GLU_irr_level.melt )

L2012.AgProduction_ag_irr_mgmt[["calOutputValue"]] <- round(
      L181.ag_Prod_Mt_R_C_Y_GLU_irr_level.melt$value[
         match( vecpaste( L2012.AgProduction_ag_irr_mgmt[ c( reg, agsupp, GLU, irr, lvl, Y ) ] ),
                vecpaste( L181.ag_Prod_Mt_R_C_Y_GLU_irr_level.melt[ c( reg, C, GLU, irr, lvl, Y ) ] ) ) ],
      digits_calOutput )

printlog( "Bioenergy yields: Repeat by mgmt techs and replace values" )
#First, prepare a yield multiplier table, with the level as a column ID
L2012.YieldMult_R_bio_GLU_irr <- melt( L181.YieldMult_R_bio_GLU_irr,
                                       measure.vars = c( "yieldmult_lo", "yieldmult_hi" ),
                                       value.name = "yieldmult" )
L2012.YieldMult_R_bio_GLU_irr[[lvl]] <- substr( as.character( L2012.YieldMult_R_bio_GLU_irr$variable ),
                                                nchar( as.character( L2012.YieldMult_R_bio_GLU_irr$variable ) ) - 1,
                                                nchar( as.character( L2012.YieldMult_R_bio_GLU_irr$variable ) ) )
L2012.YieldMult_R_bio_GLU_irr <- add_region_name (L2012.YieldMult_R_bio_GLU_irr )

#Next, rbind the tree and grass tables, repeat by mgmt level, and match in the yield multipliers
# by region, GLU, irr, and mgmt level
L2012.AgYield_bio_ref <- repeat_and_add_vector( 
  rbind( L2011.AgYield_bio_grass_irr, L2011.AgYield_bio_tree_irr ),
  lvl, c( "lo", "hi" ) )
L2012.AgYield_bio_ref <- substring_irr( L2012.AgYield_bio_ref, from.var = agtech, to.lower = T )
L2012.AgYield_bio_ref <- substring_GLU( L2012.AgYield_bio_ref, from.var = agsubs )
L2012.AgYield_bio_ref$yieldmult <- L2012.YieldMult_R_bio_GLU_irr$yieldmult[
      match( vecpaste( L2012.AgYield_bio_ref[ c( reg, GLU, irr, lvl ) ] ),
             vecpaste( L2012.YieldMult_R_bio_GLU_irr[ c( reg, GLU, irr, lvl ) ] ) ) ]

#For minor region/GLUs that are missing from the ag data, set the multipliers to 1 (effectively fixing yields in all periods)
L2012.AgYield_bio_ref$yieldmult[ is.na( L2012.AgYield_bio_ref$yieldmult ) ] <- 1
L2012.AgYield_bio_ref[[agtech]] <- paste( L2012.AgYield_bio_ref[[agtech]], L2012.AgYield_bio_ref[[lvl]], sep = mgmt_delimiter )
L2012.AgYield_bio_ref[["yield"]] <- L2012.AgYield_bio_ref[["yield"]] * L2012.AgYield_bio_ref[["yieldmult"]]
L2012.AgYield_bio_ref <- L2012.AgYield_bio_ref[ names_AgYield ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
for( curr_table in names ( L2012.AgSectorLogitTables) ) {
  write_mi_data( L2012.AgSectorLogitTables[[ curr_table ]]$data, L2012.AgSectorLogitTables[[ curr_table ]]$header,
                 "AGLU_LEVEL2_DATA", paste0("L2012.", L2012.AgSectorLogitTables[[ curr_table ]]$header ), "AGLU_XML_BATCH",
                 "batch_ag_For_Past_bio_base_IRR_MGMT.xml" )
}
write_mi_data( L2012.AgSupplySector, IDstring="AgSupplySector", domain="AGLU_LEVEL2_DATA", fn="L2012.AgSupplySector",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_ag_For_Past_bio_base_IRR_MGMT.xml" ) 
for( curr_table in names ( L2012.AgSubsectorLogitTables ) ) {
  write_mi_data( L2012.AgSubsectorLogitTables[[ curr_table ]]$data, L2012.AgSubsectorLogitTables[[ curr_table ]]$header,
                 "AGLU_LEVEL2_DATA", paste0("L2012.", L2012.AgSubsectorLogitTables[[ curr_table ]]$header ), "AGLU_XML_BATCH",
                 "batch_ag_For_Past_bio_base_IRR_MGMT.xml" )
}
write_mi_data( L2012.AgSupplySubsector, "AgSupplySubsector", "AGLU_LEVEL2_DATA", "L2012.AgSupplySubsector", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR_MGMT.xml" ) 
write_mi_data( L2012.AgProduction_ag_irr_mgmt, "AgProduction", "AGLU_LEVEL2_DATA", "L2012.AgProduction_ag_irr_mgmt", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR_MGMT.xml" ) 
write_mi_data( L2012.AgProduction_For, "AgProduction", "AGLU_LEVEL2_DATA", "L2012.AgProduction_For", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR_MGMT.xml" ) 
write_mi_data( L2012.AgProduction_Past, "AgProduction", "AGLU_LEVEL2_DATA", "L2012.AgProduction_Past", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR_MGMT.xml" ) 
write_mi_data( L2012.AgHAtoCL_irr_mgmt, "AgHAtoCL", "AGLU_LEVEL2_DATA", "L2012.AgHAtoCL_irr_mgmt", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR_MGMT.xml" ) 
write_mi_data( L2012.AgYield_bio_ref, "AgYield", "AGLU_LEVEL2_DATA", "L2012.AgYield_bio_ref", "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR_MGMT.xml" ) 

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_For_Past_bio_base_IRR_MGMT.xml", "AGLU_XML_FINAL", "ag_For_Past_bio_base_IRR_MGMT.xml", "", xml_tag="outFile" )

logstop()
