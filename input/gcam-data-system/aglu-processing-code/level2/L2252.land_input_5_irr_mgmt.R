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
logstart( "L2252.land_input_5_irr_mgmt.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Land cover and characteristics, node level 5 (management tech split)" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
L181.LandShare_R_bio_GLU_irr <- readdata( "AGLU_LEVEL1_DATA", "L181.LandShare_R_bio_GLU_irr" )
L181.LC_bm2_R_C_Yh_GLU_irr_level <- readdata( "AGLU_LEVEL1_DATA", "L181.LC_bm2_R_C_Yh_GLU_irr_level" )
L181.YieldMult_R_bio_GLU_irr <- readdata( "AGLU_LEVEL1_DATA", "L181.YieldMult_R_bio_GLU_irr" )
L2241.LN4_Logit <- readdata( "AGLU_LEVEL2_DATA", "L2241.LN4_Logit", skip = 4 )
L2241.LN4_HistMgdAllocation_crop <- readdata( "AGLU_LEVEL2_DATA", "L2241.LN4_HistMgdAllocation_crop", skip = 4 )
L2241.LN4_MgdAllocation_crop <- readdata( "AGLU_LEVEL2_DATA", "L2241.LN4_MgdAllocation_crop", skip = 4 )
L2241.LN4_HistMgdAllocation_bio <- readdata( "AGLU_LEVEL2_DATA", "L2241.LN4_HistMgdAllocation_bio", skip = 4 )
L2241.LN4_MgdAllocation_bio <- readdata( "AGLU_LEVEL2_DATA", "L2241.LN4_MgdAllocation_bio", skip = 4 )
L2241.LN4_MgdCarbon_crop <- readdata( "AGLU_LEVEL2_DATA", "L2241.LN4_MgdCarbon_crop", skip = 4 )
#Na.omit to drop the node rename table, if present
L2241.LN4_MgdCarbon_bio <- na.omit( readdata( "AGLU_LEVEL2_DATA", "L2241.LN4_MgdCarbon_bio", skip = 4 ) )
L2241.LN4_NewTech <- readdata( "AGLU_LEVEL2_DATA", "L2241.LN4_NewTech", skip = 4 )

# -----------------------------------------------------------------------------
#The methods in this code file will be to start with existing (landnode4) tables, and add another level of detail
# Some tables will require post-hoc adjustments, and for others the information simply needs to be passed down to another level
#1b. Write a function to carry LN4 information down to LN5
convert_LN4_to_LN5 <- function( data, names ){
	data_new <- repeat_and_add_vector( data, lvl, c( "lo", "hi" ) )
	data_new$LandNode5 <- data_new$LandLeaf
	data_new$LandLeaf <- paste( data_new$LandNode5, data_new[[lvl]], sep = mgmt_delimiter )
	data_new <- data_new[ names ]
	return( data_new )
}

# -----------------------------------------------------------------------------
# 2. Build tables
printlog( "L2252.LN5_Logit: Logit exponent between lo and hi managed techs")
# Keeping this assumption in this file for now; keeps changes from forcing re-builds of other files.
mgmt_logit <- 0.1
mgmt_logit_type <- "absolute-cost-logit"
L2252.LN5_Logit <- repeat_and_add_vector( L2241.LN4_Logit, irr, c( "IRR", "RFD" ) )
L2252.LN5_Logit$LandNode5 <- paste( L2252.LN5_Logit$LandNode4, L2252.LN5_Logit[[irr]], sep = irr_delimiter )
L2252.LN5_Logit$logit.exponent <- mgmt_logit
L2252.LN5_Logit$logit.type <- mgmt_logit_type

L2252.LN5_LogitTables <- get_logit_fn_tables( L2252.LN5_Logit, names_LN5_LogitType,
    base.header="LN5_Logit_", include.equiv.table=T, write.all.regions=F )
L2252.LN5_Logit <- L2252.LN5_Logit[ names_LN5_Logit ]

printlog( "L2252.LN5_HistMgdAllocation_crop: historical cropland allocation" )
L2252.LC_bm2_R_C_Yh_GLU_irr_mgmt <- add_region_name( melt( L181.LC_bm2_R_C_Yh_GLU_irr_level,
                                                            id.vars = R_C_GLU_irr_lvl, variable.name = Y ) )
L2252.LC_bm2_R_C_Yh_GLU_irr_mgmt$year <- as.numeric( substr( L2252.LC_bm2_R_C_Yh_GLU_irr_mgmt[[Y]], 2, 5 ) )
L2252.LC_bm2_R_C_Yh_GLU_irr_mgmt[[irr]] <- toupper( L2252.LC_bm2_R_C_Yh_GLU_irr_mgmt[[irr]] )
L2252.LC_bm2_R_C_Yh_GLU_irr_mgmt$LandLeaf <-
  paste( paste( paste( L2252.LC_bm2_R_C_Yh_GLU_irr_mgmt[[C]], L2252.LC_bm2_R_C_Yh_GLU_irr_mgmt[[GLU]], sep = crop_GLU_delimiter ),
                L2252.LC_bm2_R_C_Yh_GLU_irr_mgmt[[irr]], sep = irr_delimiter ),
         L2252.LC_bm2_R_C_Yh_GLU_irr_mgmt[[lvl]], sep = mgmt_delimiter )

L2252.LN5_HistMgdAllocation_crop <- convert_LN4_to_LN5( L2241.LN4_HistMgdAllocation_crop, names_LN5_HistMgdAllocation )
L2252.LN5_HistMgdAllocation_crop$allocation <- round(
      L2252.LC_bm2_R_C_Yh_GLU_irr_mgmt$value[
         match( vecpaste( L2252.LN5_HistMgdAllocation_crop[ c( reg, Y, "LandLeaf" ) ] ),
                vecpaste( L2252.LC_bm2_R_C_Yh_GLU_irr_mgmt[ c( reg, Y, "LandLeaf" ) ] ) ) ],
      digits_land_use )

printlog( "L2252.LN5_MgdAllocation_crop: cropland allocation" )
L2252.LN5_MgdAllocation_crop <- convert_LN4_to_LN5( L2241.LN4_MgdAllocation_crop, names_LN5_MgdAllocation )
L2252.LN5_MgdAllocation_crop$allocation <- round(
      L2252.LC_bm2_R_C_Yh_GLU_irr_mgmt$value[
         match( vecpaste( L2252.LN5_MgdAllocation_crop[ c( reg, Y, "LandLeaf" ) ] ),
                vecpaste( L2252.LC_bm2_R_C_Yh_GLU_irr_mgmt[ c( reg, Y, "LandLeaf" ) ] ) ) ],
      digits_land_use )


printlog( "L2252.LN5_HistMgdAllocation_bio: No changes (just switch the structure)" )
L2252.LN5_HistMgdAllocation_bio <- convert_LN4_to_LN5( L2241.LN4_HistMgdAllocation_bio, names_LN5_HistMgdAllocation )

printlog( "L2252.LN5_MgdAllocation_bio: No changes (just switch the structure)" )
L2252.LN5_MgdAllocation_bio <- convert_LN4_to_LN5( L2241.LN4_MgdAllocation_bio, names_LN5_HistMgdAllocation )

printlog( "L2252.LN5_MgdCarbon_crop: Structural change, and re-calculate carbon content from yields" )
L2252.LN5_MgdCarbon_crop <- convert_LN4_to_LN5( L2241.LN4_MgdCarbon_crop, names_LN5_MgdCarbon )

printlog( "L2252.LN5_MgdCarbon_bio: Structural change plus multiply the vegetative carbon content by the lo/hi yield multipliers")
L2252.YieldMult_R_bio_GLU_irr <- add_region_name( melt( L181.YieldMult_R_bio_GLU_irr,
                                                        measure.vars = c( "yieldmult_lo", "yieldmult_hi" ),
                                                        value.name = "yieldmult" ) )
L2252.YieldMult_R_bio_GLU_irr$variable <- as.character( L2252.YieldMult_R_bio_GLU_irr$variable )
L2252.YieldMult_R_bio_GLU_irr[[lvl]] <- with( L2252.YieldMult_R_bio_GLU_irr, substr( variable, nchar( variable ) - 1, nchar( variable ) ) )

L2252.LN5_MgdCarbon_bio <- convert_LN4_to_LN5( L2241.LN4_MgdCarbon_bio, names_LN5_MgdCarbon )
L2252.LN5_MgdCarbon_bio[[lvl]] <- with( L2252.LN5_MgdCarbon_bio, substr( LandLeaf, nchar( LandLeaf ) - 1, nchar( LandLeaf ) ) )
L2252.LN5_MgdCarbon_bio <- substring_irr( L2252.LN5_MgdCarbon_bio, from.var = "LandNode5", to.lower = T )
L2252.LN5_MgdCarbon_bio <- substring_GLU( L2252.LN5_MgdCarbon_bio, from.var = "LandNode4" )

#Multiply carbon contents by (matched in) yield multipliers
L2252.LN5_MgdCarbon_bio$hist.veg.carbon.density <-
  round( L2252.LN5_MgdCarbon_bio$veg.carbon.density * L2252.YieldMult_R_bio_GLU_irr$yieldmult[
    match( vecpaste( L2252.LN5_MgdCarbon_bio[ c( reg, GLU, irr, lvl ) ] ),
           vecpaste( L2252.YieldMult_R_bio_GLU_irr[ c( reg, GLU, irr, lvl ) ] ) ) ],
    digits_C_density )

#For places with no info, set equal to whatever it was before
L2252.LN5_MgdCarbon_bio$hist.veg.carbon.density[ is.na( L2252.LN5_MgdCarbon_bio$hist.veg.carbon.density ) ] <-
      L2252.LN5_MgdCarbon_bio$veg.carbon.density[ is.na( L2252.LN5_MgdCarbon_bio$hist.veg.carbon.density ) ]
L2252.LN5_MgdCarbon_bio$veg.carbon.density <- L2252.LN5_MgdCarbon_bio$hist.veg.carbon.density

printlog( "L2252.LN5_NewTech: Ghost share of the new landleaf (lo-input versus hi-input)" )
printlog( "NOTE: The ghost shares are inferred from average land shares allocated to hi-input versus lo-input, across all crops" )
L2252.LN5_NewTech <- convert_LN4_to_LN5( L2241.LN4_NewTech, c( names_LN5_NewTech, lvl ) )
L2252.LN5_NewTech <- substring_GLU( L2252.LN5_NewTech, "LandNode4" )
L2252.LN5_NewTech <- substring_irr( L2252.LN5_NewTech, "LandNode5", to.lower = T )

L2252.LandShare_R_bio_GLU_irr <- add_region_name( melt( L181.LandShare_R_bio_GLU_irr,
                                                        measure.vars = c( "landshare_lo", "landshare_hi" ),
                                                        value.name = "landshare" ) )
L2252.LandShare_R_bio_GLU_irr$variable <- as.character( L2252.LandShare_R_bio_GLU_irr$variable )
L2252.LandShare_R_bio_GLU_irr[[lvl]] <- with( L2252.LandShare_R_bio_GLU_irr, substr( variable, nchar( variable ) - 1, nchar( variable ) ) )

L2252.LN5_NewTech$ghost.share.leaf <-
  round( L2252.LandShare_R_bio_GLU_irr$landshare[
    match( vecpaste( L2252.LN5_NewTech[ c( reg, GLU, irr, lvl ) ] ),
           vecpaste( L2252.LandShare_R_bio_GLU_irr[ c( reg, GLU, irr, lvl ) ] ) ) ],
    digits_land_use )

#For bio techs with no ghost share info, set lo- and hi-input techs to 0.5
L2252.LN5_NewTech$ghost.share.leaf[ is.na( L2252.LN5_NewTech$ghost.share.leaf ) ] <- 0.5

printlog( "L2252.LN5_NewNode: Ghost share of the new nodes (irrigated versus rainfed)")
L2252.LN5_NewNode <- L2241.LN4_NewTech
names( L2252.LN5_NewNode ) <- sub( "LandLeaf", "LandNode5", names( L2252.LN5_NewNode ) )
names( L2252.LN5_NewNode ) <- sub( "ghost.share.leaf", "ghost.share.node", names( L2252.LN5_NewNode ) )
L2252.LN5_NewNode[["newTechStartYear"]] <- NULL

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

for( curr_table in L2252.LN5_LogitTables ) {
write_mi_data( curr_table$data, curr_table$header, "AGLU_LEVEL2_DATA", paste0( "L2252.", curr_table$header ), "AGLU_XML_BATCH", "batch_land_input_5_IRR_MGMT.xml" )
}
write_mi_data( L2252.LN5_Logit, IDstring="LN5_Logit", domain="AGLU_LEVEL2_DATA", fn="L2252.LN5_Logit",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_land_input_5_IRR_MGMT.xml" )
write_mi_data( L2252.LN5_HistMgdAllocation_crop, "LN5_HistMgdAllocation", "AGLU_LEVEL2_DATA", "L2252.LN5_HistMgdAllocation_crop", "AGLU_XML_BATCH", "batch_land_input_5_IRR_MGMT.xml" )
write_mi_data( L2252.LN5_MgdAllocation_crop, "LN5_MgdAllocation", "AGLU_LEVEL2_DATA", "L2252.LN5_MgdAllocation_crop", "AGLU_XML_BATCH", "batch_land_input_5_IRR_MGMT.xml" )
write_mi_data( L2252.LN5_HistMgdAllocation_bio, "LN5_HistMgdAllocation", "AGLU_LEVEL2_DATA", "L2252.LN5_HistMgdAllocation_bio", "AGLU_XML_BATCH", "batch_land_input_5_IRR_MGMT.xml" )
write_mi_data( L2252.LN5_MgdAllocation_bio, "LN5_MgdAllocation", "AGLU_LEVEL2_DATA", "L2252.LN5_MgdAllocation_bio", "AGLU_XML_BATCH", "batch_land_input_5_IRR_MGMT.xml" )
write_mi_data( L2252.LN5_MgdCarbon_crop, "LN5_MgdCarbon", "AGLU_LEVEL2_DATA", "L2252.LN5_MgdCarbon_crop", "AGLU_XML_BATCH", "batch_land_input_5_IRR_MGMT.xml" )
write_mi_data( L2252.LN5_MgdCarbon_bio, "LN5_MgdCarbon", "AGLU_LEVEL2_DATA", "L2252.LN5_MgdCarbon_bio", "AGLU_XML_BATCH", "batch_land_input_5_IRR_MGMT.xml" )
write_mi_data( L2252.LN5_NewTech, "LN5_NewTech", "AGLU_LEVEL2_DATA", "L2252.LN5_NewTech", "AGLU_XML_BATCH", "batch_land_input_5_IRR_MGMT.xml" )
write_mi_data( L2252.LN5_NewNode, "LN5_NewNode", "AGLU_LEVEL2_DATA", "L2252.LN5_NewNode", "AGLU_XML_BATCH", "batch_land_input_5_IRR_MGMT.xml", node_rename=T )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_land_input_5_IRR_MGMT.xml", "AGLU_XML_FINAL", "land_input_5_IRR_MGMT.xml", "", xml_tag="outFile" )

logstop()
