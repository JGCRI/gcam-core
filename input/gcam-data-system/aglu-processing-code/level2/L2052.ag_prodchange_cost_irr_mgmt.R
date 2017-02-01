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
logstart( "L2052.ag_prodchange_cost_irr_mgmt.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Model input for agricultural costs and productivity change" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
L2051.AgCost_ag_irr <- readdata( "AGLU_LEVEL2_DATA", "L2051.AgCost_ag_irr", skip = 4 )
L2051.AgCost_bio_irr <- readdata( "AGLU_LEVEL2_DATA", "L2051.AgCost_bio_irr", skip = 4 )
L2051.AgCost_For <- readdata( "AGLU_LEVEL2_DATA", "L2051.AgCost_For", skip = 4 )
# 1b. reading AgProdChange files in a loop
Ag_ProdChange_Domain <- readdomainpathmap()["AGLU_LEVEL2_DATA"][[1]]
L2051.Ag_ProdChange_filenames <- list.files( Ag_ProdChange_Domain )[ grepl( "L2051.AgProdChange", list.files( Ag_ProdChange_Domain))]
L2051.Ag_ProdChange_filenames <- sub( ".csv", "", L2051.Ag_ProdChange_filenames )
L2051.Ag_ProdChange_files.list <- list()
for( i in L2051.Ag_ProdChange_filenames ){
  index <- which( L2051.Ag_ProdChange_filenames == i )
  L2051.Ag_ProdChange_files.list[[ index ]] <- readdata( "AGLU_LEVEL2_DATA", i, skip = 4)
}
names( L2051.Ag_ProdChange_files.list ) <- L2051.Ag_ProdChange_filenames

# -----------------------------------------------------------------------------
# 2. Build tables
L2052.AgCost_ag_irr_mgmt <- repeat_and_add_vector( L2051.AgCost_ag_irr, lvl, c( "lo", "hi" ) )
L2052.AgCost_ag_irr_mgmt[[agtech]] <- paste( L2052.AgCost_ag_irr_mgmt[[agtech]], L2052.AgCost_ag_irr_mgmt[[lvl]], sep = mgmt_delimiter )
L2052.AgCost_ag_irr_mgmt <- L2052.AgCost_ag_irr_mgmt[ names_AgCost ]

L2052.AgCost_bio_irr_mgmt <- repeat_and_add_vector( L2051.AgCost_bio_irr, lvl, c( "lo", "hi" ) )
L2052.AgCost_bio_irr_mgmt[[agtech]] <- paste( L2052.AgCost_bio_irr_mgmt[[agtech]], L2052.AgCost_bio_irr_mgmt[[lvl]], sep = mgmt_delimiter )
L2052.AgCost_bio_irr_mgmt <- L2052.AgCost_bio_irr_mgmt[ names_AgCost ]

L2052.AgCost_For <- L2051.AgCost_For

L2052.Ag_ProdChange_files.list <- L2051.Ag_ProdChange_files.list
names( L2052.Ag_ProdChange_files.list ) <- sub( "L2051", "L2052", names( L2052.Ag_ProdChange_files.list ) )

printlog( "Modified method - just apply the prod change to both hi and lo technologies equally" )
for( i in names( L2052.Ag_ProdChange_files.list ) ){
  L2052.Ag_ProdChange_files.list[[i]] <- repeat_and_add_vector( L2052.Ag_ProdChange_files.list[[i]], lvl, c( "lo", "hi" ) )
  L2052.Ag_ProdChange_files.list[[i]][[agtech]] <- paste( L2052.Ag_ProdChange_files.list[[i]][[agtech]],
                                                          L2052.Ag_ProdChange_files.list[[i]][[lvl]], sep = mgmt_delimiter )
  L2052.Ag_ProdChange_files.list[[i]] <- L2052.Ag_ProdChange_files.list[[i]][ names_AgProdChange ]
}

#Not writing the files out in a loop for now, since we want to change the names of the XML files from the object names here
L2052.AgProdChange_ag_irr_ref <- L2052.Ag_ProdChange_files.list[["L2052.AgProdChange_ag_irr_ref"]]
L2052.AgProdChange_bio_irr_ref <- L2052.Ag_ProdChange_files.list[["L2052.AgProdChange_bio_irr_ref"]]
L2052.AgProdChange_irr_high <- L2052.Ag_ProdChange_files.list[["L2052.AgProdChange_irr_high"]]
L2052.AgProdChange_irr_low <- L2052.Ag_ProdChange_files.list[["L2052.AgProdChange_irr_low"]]
L2052.AgProdChange_irr_ssp4 <- L2052.Ag_ProdChange_files.list[["L2052.AgProdChange_irr_ssp4"]]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L2052.AgCost_ag_irr_mgmt, "AgCost", "AGLU_LEVEL2_DATA", "L2052.AgCost_ag_irr_mgmt", "AGLU_XML_BATCH", "batch_ag_cost_IRR_MGMT.xml" ) 
write_mi_data( L2052.AgCost_bio_irr_mgmt, "AgCost", "AGLU_LEVEL2_DATA", "L2052.AgCost_bio_irr_mgmt", "AGLU_XML_BATCH", "batch_ag_cost_IRR_MGMT.xml" ) 
write_mi_data( L2052.AgCost_For, "AgCost", "AGLU_LEVEL2_DATA", "L2052.AgCost_For", "AGLU_XML_BATCH", "batch_ag_cost_IRR_MGMT.xml" ) 
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_cost_IRR_MGMT.xml", "AGLU_XML_FINAL", "ag_cost_IRR_MGMT.xml", "", xml_tag="outFile" )

write_mi_data( L2052.AgProdChange_ag_irr_ref, "AgProdChange", "AGLU_LEVEL2_DATA", "L2052.AgProdChange_ag_irr_ref", "AGLU_XML_BATCH", "batch_ag_prodchange_ref_IRR_MGMT.xml" ) 
write_mi_data( L2052.AgProdChange_bio_irr_ref, "AgProdChange", "AGLU_LEVEL2_DATA", "L2052.AgProdChange_bio_irr_ref", "AGLU_XML_BATCH", "batch_ag_prodchange_ref_IRR_MGMT.xml" ) 
write_mi_data( L2052.AgProdChange_irr_high, "AgProdChange", "AGLU_LEVEL2_DATA", "L2052.AgProdChange_irr_high", "AGLU_XML_BATCH", "batch_ag_prodchange_ssp1_IRR_MGMT.xml" ) 
write_mi_data( L2052.AgProdChange_ag_irr_ref, "AgProdChange", "AGLU_LEVEL2_DATA", "L2052.AgProdChange_ag_irr_ref", "AGLU_XML_BATCH", "batch_ag_prodchange_ssp2_IRR_MGMT.xml" ) 
write_mi_data( L2052.AgProdChange_irr_low, "AgProdChange", "AGLU_LEVEL2_DATA", "L2052.AgProdChange_irr_low", "AGLU_XML_BATCH", "batch_ag_prodchange_ssp3_IRR_MGMT.xml" ) 
write_mi_data( L2052.AgProdChange_irr_ssp4, "AgProdChange", "AGLU_LEVEL2_DATA", "L2052.AgProdChange_irr_ssp4", "AGLU_XML_BATCH", "batch_ag_prodchange_ssp4_IRR_MGMT.xml" ) 
write_mi_data( L2052.AgProdChange_irr_high, "AgProdChange", "AGLU_LEVEL2_DATA", "L2052.AgProdChange_irr_high", "AGLU_XML_BATCH", "batch_ag_prodchange_ssp5_IRR_MGMT.xml" ) 

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ref_IRR_MGMT.xml", "AGLU_XML_FINAL", "ag_prodchange_ref_IRR_MGMT.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp1_IRR_MGMT.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp1_IRR_MGMT.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp2_IRR_MGMT.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp2_IRR_MGMT.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp3_IRR_MGMT.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp3_IRR_MGMT.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp4_IRR_MGMT.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp4_IRR_MGMT.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_ag_prodchange_ssp5_IRR_MGMT.xml", "AGLU_XML_FINAL", "ag_prodchange_ssp5_IRR_MGMT.xml", "", xml_tag="outFile" )

logstop()