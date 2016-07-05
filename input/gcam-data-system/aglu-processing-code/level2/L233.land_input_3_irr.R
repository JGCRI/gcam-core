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
logstart( "L233.land_input_3_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Land cover and characteristics, node level 3, irrigated v rainfed" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
GCAMLandLeaf_CdensityLT <- readdata( "AGLU_MAPPINGS", "GCAMLandLeaf_CdensityLT" )
A_biocrops_R_AEZ_irr <- readdata( "AGLU_ASSUMPTIONS", "A_biocrops_R_AEZ_irr" )
A_LandNode_logit <- readdata( "AGLU_ASSUMPTIONS", "A_LandNode_logit" )
A_LandNode3 <- readdata( "AGLU_ASSUMPTIONS", "A_LandNode3" )
A_LandLeaf_Unmgd3 <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf_Unmgd3" )
A_LandLeaf3_irr <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf3_irr" )
L163.ag_rfdBioYield_GJm2_R_AEZ_ref <- readdata( "AGLU_LEVEL1_DATA", "L163.ag_rfdBioYield_GJm2_R_AEZ_ref" )
L163.ag_rfdBioYield_GJm2_R_AEZ_hi <- readdata( "AGLU_LEVEL1_DATA", "L163.ag_rfdBioYield_GJm2_R_AEZ_hi" )
L163.ag_irrBioYield_GJm2_R_AEZ_ref <- readdata( "AGLU_LEVEL1_DATA", "L163.ag_irrBioYield_GJm2_R_AEZ_ref" )
L163.ag_irrBioYield_GJm2_R_AEZ_hi <- readdata( "AGLU_LEVEL1_DATA", "L163.ag_irrBioYield_GJm2_R_AEZ_hi" )
L121.VegC_kgm2_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.VegC_kgm2_R_LT_AEZ" )
L121.SoilC_kgm2_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.SoilC_kgm2_R_LT_AEZ" )
L121.MatureAge_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.MatureAge_R_LT_AEZ" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )
L125.LC_bm2_R_LT_Yh_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L125.LC_bm2_R_LT_Yh_AEZ" )

# -----------------------------------------------------------------------------
# 2. Build tables
printlog( "L233.LN3_Logit: Logit exponent, third nest" )
L233.LN3_Logit <- data.frame(
      region = rep( GCAM_region_names$region, times = nrow( A_LandNode3 ) ),
      LandAllocatorRoot = "root",
      LandNode3 = sort( rep( A_LandNode3$LandNode3, times = length( GCAM_region_names$region ) ) ) )
L233.LN3_Logit[ c( "LandNode1", "LandNode2" ) ] <- A_LandNode3[
      match( L233.LN3_Logit$LandNode3, A_LandNode3$LandNode3 ),
      c( "LandNode1", "LandNode2" ) ]
L233.LN3_Logit <- repeat_and_add_vector( L233.LN3_Logit, AEZ, AEZs )

#Match in logit exponents based on the land node 2
L233.LN3_Logit$logit.year.fillout <- min( model_base_years )
L233.LN3_Logit$logit.exponent <- A_LandNode_logit$logit.exponent[ match( L233.LN3_Logit$LandNode3, A_LandNode_logit$LandNode ) ]
L233.LN3_Logit$logit.exponent[ L233.LN3_Logit$AEZ %in% AEZs_most_arid ] <- A_LandNode_logit$logit.exponent.arid[
      match( L233.LN3_Logit$LandNode3[ L233.LN3_Logit$AEZ %in% AEZs_most_arid ], A_LandNode_logit$LandNode ) ]

#Append AEZ names and keep only relevant columns
L233.LN3_Logit <- append_AEZ( L233.LN3_Logit, var1 = "LandNode1", var2 = "LandNode2", var3 = "LandNode3" )
L233.LN3_Logit <- L233.LN3_Logit[ names_LN3_Logit ]

#LAND USE HISTORY
#Unmanaged land
L233.LC_bm2_R_Unmgd3_Yh_AEZ <- subset( L125.LC_bm2_R_LT_Yh_AEZ, Land_Type %in% A_LandLeaf_Unmgd3$UnmanagedLandLeaf )
L233.LC_bm2_R_Unmgd3_Yh_AEZ.melt <- melt( L233.LC_bm2_R_Unmgd3_Yh_AEZ, id.vars = R_LT_AEZ, variable.name = "Xyear" )
L233.LC_bm2_R_Unmgd3_Yh_AEZ.melt <- add_region_name( L233.LC_bm2_R_Unmgd3_Yh_AEZ.melt )
L233.LC_bm2_R_Unmgd3_Yh_AEZ.melt[[Y]] <- sub( "X", "", L233.LC_bm2_R_Unmgd3_Yh_AEZ.melt$Xyear )
L233.LC_bm2_R_Unmgd3_Yh_AEZ.melt$allocation <- L233.LC_bm2_R_Unmgd3_Yh_AEZ.melt$value
L233.LC_bm2_R_Unmgd3_Yh_AEZ.melt <- add_node_leaf_names( L233.LC_bm2_R_Unmgd3_Yh_AEZ.melt,
       nesting_table = A_LandLeaf_Unmgd3, leaf_name = "UnmanagedLandLeaf", LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3" )

printlog( "L233.LN3_HistUnmgdAllocation: Historical land cover, unmanaged land in the third nest" )
L233.LN3_HistUnmgdAllocation <- L233.LC_bm2_R_Unmgd3_Yh_AEZ.melt[ L233.LC_bm2_R_Unmgd3_Yh_AEZ.melt[[Y]] %in% land_history_years, names_LN3_HistUnmgdAllocation ]

printlog( "L233.LN3_UnmgdAllocation: Model base period land cover, unmanaged land in the third nest" )
L233.LN3_UnmgdAllocation <- L233.LC_bm2_R_Unmgd3_Yh_AEZ.melt[ L233.LC_bm2_R_Unmgd3_Yh_AEZ.melt[[Y]] %in% model_base_years, names_LN3_UnmgdAllocation ]

#Managed land - non-crop
L233.LC_bm2_R_Mgd3_Yh_AEZ <- subset( L125.LC_bm2_R_LT_Yh_AEZ, Land_Type %in% A_LandLeaf3_irr$LandLeaf )
L233.LC_bm2_R_Mgd3_Yh_AEZ.melt <- melt( L233.LC_bm2_R_Mgd3_Yh_AEZ, id.vars = R_LT_AEZ, variable.name = "Xyear", value.name = "allocation" )
L233.LC_bm2_R_Mgd3_Yh_AEZ.melt <- add_region_name( L233.LC_bm2_R_Mgd3_Yh_AEZ.melt )
L233.LC_bm2_R_Mgd3_Yh_AEZ.melt[[Y]] <- sub( "X", "", L233.LC_bm2_R_Mgd3_Yh_AEZ.melt$Xyear )
L233.LC_bm2_R_Mgd3_Yh_AEZ.melt <- add_node_leaf_names( L233.LC_bm2_R_Mgd3_Yh_AEZ.melt,
       nesting_table = A_LandLeaf3_irr, leaf_name = "LandLeaf", LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3" )

printlog( "L233.LN3_HistMgdAllocation: Historical land cover, managed land in the third nest (noncrop)" )
L233.LN3_HistMgdAllocation <- L233.LC_bm2_R_Mgd3_Yh_AEZ.melt[ L233.LC_bm2_R_Mgd3_Yh_AEZ.melt[[Y]] %in% land_history_years, names_LN3_HistMgdAllocation ]

printlog( "L233.LN3_MgdAllocation: Model base period land cover, managed land in the third nest (noncrop)" )
L233.LN3_MgdAllocation <- L233.LC_bm2_R_Mgd3_Yh_AEZ.melt[ L233.LC_bm2_R_Mgd3_Yh_AEZ.melt[[Y]] %in% model_base_years, names_LN3_MgdAllocation ]

#CARBON CONTENTS AND MATURE AGES
#Melt tables with compiled carbon contents and mature ages
L233.VegC_kgm2_R_LT_AEZ.melt <- melt( L121.VegC_kgm2_R_LT_AEZ, id.vars = R_LT, variable.name = AEZ )
L233.SoilC_kgm2_R_LT_AEZ.melt <- melt( L121.SoilC_kgm2_R_LT_AEZ, id.vars = R_LT, variable.name = AEZ )
L233.MatureAge_R_LT_AEZ.melt <- melt( L121.MatureAge_R_LT_AEZ, id.vars = R_LT, variable.name = AEZ )

#Add region vector
L233.VegC_kgm2_R_LT_AEZ.melt <- add_region_name( L233.VegC_kgm2_R_LT_AEZ.melt )
L233.SoilC_kgm2_R_LT_AEZ.melt <- add_region_name( L233.SoilC_kgm2_R_LT_AEZ.melt )
L233.MatureAge_R_LT_AEZ.melt <- add_region_name( L233.MatureAge_R_LT_AEZ.melt )

printlog( "L233.LN3_UnmgdCarbon: Carbon content info, unmanaged land in the third nest" )
L233.LN3_UnmgdCarbon <- data.frame(
      region = rep( GCAM_region_names$region, times = nrow( A_LandLeaf_Unmgd3 ) ),
      Land_Type = sort( rep( A_LandLeaf_Unmgd3$UnmanagedLandLeaf, times = nrow( GCAM_region_names ) ) ) )
L233.LN3_UnmgdCarbon$Cdensity_LT <- GCAMLandLeaf_CdensityLT[[LT]][
      match( L233.LN3_UnmgdCarbon[[LT]], GCAMLandLeaf_CdensityLT$LandLeaf) ]
L233.LN3_UnmgdCarbon <- repeat_and_add_vector( L233.LN3_UnmgdCarbon, AEZ, AEZs )
L233.LN3_UnmgdCarbon <- add_node_leaf_names( L233.LN3_UnmgdCarbon,
      nesting_table = A_LandLeaf_Unmgd3, leaf_name = "UnmanagedLandLeaf", LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3" )

#Match in carbon densities and mature age and sort the columns to match the model interface header
L233.LN3_UnmgdCarbon <- add_carbon_info( L233.LN3_UnmgdCarbon,
      veg_data = L233.VegC_kgm2_R_LT_AEZ.melt, soil_data = L233.SoilC_kgm2_R_LT_AEZ.melt, age_data = L233.MatureAge_R_LT_AEZ.melt )
L233.LN3_UnmgdCarbon <- L233.LN3_UnmgdCarbon[ names_LN3_UnmgdCarbon ]

printlog( "L233.LN3_UnmgdNoEmissCarbon: Carbon content info with no-emissions walkaway" )
L233.LN3_UnmgdNoEmissCarbon <- subset( L233.LN3_UnmgdCarbon, grepl( "Forest", UnmanagedLandLeaf ) )
L233.LN3_UnmgdCarbon <- subset( L233.LN3_UnmgdCarbon, !grepl( "Forest", UnmanagedLandLeaf ) )

printlog( "L233.LN3_MgdNoEmissCarbon: Carbon content info, managed land in the third nest (noncrop)" )
#Managed carbon in the third nest (managed forest)
L233.LN3_MgdNoEmissCarbon <- data.frame(
      region = rep( GCAM_region_names$region, times = nrow( A_LandLeaf3_irr ) ),
      Land_Type = sort( rep( A_LandLeaf3_irr$LandLeaf, times = nrow( GCAM_region_names ) ) ) )
L233.LN3_MgdNoEmissCarbon$Cdensity_LT <- GCAMLandLeaf_CdensityLT[[LT]][
      match( L233.LN3_MgdNoEmissCarbon[[LT]], GCAMLandLeaf_CdensityLT$LandLeaf) ]
L233.LN3_MgdNoEmissCarbon <- repeat_and_add_vector( L233.LN3_MgdNoEmissCarbon, AEZ, AEZs )
L233.LN3_MgdNoEmissCarbon <- add_node_leaf_names( L233.LN3_MgdNoEmissCarbon,
      nesting_table = A_LandLeaf3_irr, leaf_name = "LandLeaf", LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3" )

#Match in carbon densities and mature age and sort the columns to match the model interface header
L233.LN3_MgdNoEmissCarbon <- add_carbon_info( L233.LN3_MgdNoEmissCarbon,
      veg_data = L233.VegC_kgm2_R_LT_AEZ.melt, soil_data = L233.SoilC_kgm2_R_LT_AEZ.melt, age_data = L233.MatureAge_R_LT_AEZ.melt )
L233.LN3_MgdNoEmissCarbon <- L233.LN3_MgdNoEmissCarbon[ names_LN3_MgdCarbon ]

# Put the node carbon calc in the node just above the leaves
printlog( "L233.LN3_NodeCarbon: Set the node-carbon-calc to drive the carbon calc between forest leaves" )
L233.LN3_NodeCarbon <- L233.LN3_MgdNoEmissCarbon[ c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf" ) ]
L233.LN3_NodeCarbon$LandLeaf <- NULL
L233.LN3_NodeCarbon <- unique( L233.LN3_NodeCarbon )
L233.LN3_NodeCarbon$node.carbon.calc <- ""
L233.LN3_NodeCarbon$extra <- "junk"

printlog( "Removing non-existent AEZs from all tables" )
L233.LN3_Logit <- remove_AEZ_nonexist( L233.LN3_Logit, AEZcol = "LandNode1" )
L233.LN3_HistUnmgdAllocation <- remove_AEZ_nonexist( L233.LN3_HistUnmgdAllocation, AEZcol = "LandNode1" )
L233.LN3_UnmgdAllocation <- remove_AEZ_nonexist( L233.LN3_UnmgdAllocation, AEZcol = "LandNode1" )
L233.LN3_HistMgdAllocation <- remove_AEZ_nonexist( L233.LN3_HistMgdAllocation, AEZcol = "LandNode1" )
L233.LN3_MgdAllocation <- remove_AEZ_nonexist( L233.LN3_MgdAllocation, AEZcol = "LandNode1" )
L233.LN3_UnmgdCarbon <- remove_AEZ_nonexist( L233.LN3_UnmgdCarbon, AEZcol = "LandNode1" )
L233.LN3_UnmgdNoEmissCarbon <- remove_AEZ_nonexist( L233.LN3_UnmgdNoEmissCarbon, AEZcol = "LandNode1" )
L233.LN3_MgdNoEmissCarbon <- remove_AEZ_nonexist( L233.LN3_MgdNoEmissCarbon, AEZcol = "LandNode1" )
L233.LN3_NodeCarbon <- remove_AEZ_nonexist( L233.LN3_NodeCarbon, AEZcol = "LandNode1" )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L233.LN3_Logit, IDstring="LN3_Logit", domain="AGLU_LEVEL2_DATA", fn="L233.LN3_Logit",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_land_input_3_IRR.xml" )
write_mi_data( L233.LN3_HistUnmgdAllocation, "LN3_HistUnmgdAllocation", "AGLU_LEVEL2_DATA", "L233.LN3_HistUnmgdAllocation", "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml" )
write_mi_data( L233.LN3_UnmgdAllocation, "LN3_UnmgdAllocation", "AGLU_LEVEL2_DATA", "L233.LN3_UnmgdAllocation", "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml" )
write_mi_data( L233.LN3_HistMgdAllocation, "LN3_HistMgdAllocation", "AGLU_LEVEL2_DATA", "L233.LN3_HistMgdAllocation", "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml" )
write_mi_data( L233.LN3_MgdAllocation, "LN3_MgdAllocation", "AGLU_LEVEL2_DATA", "L233.LN3_MgdAllocation", "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml" )
write_mi_data( L233.LN3_UnmgdCarbon, "LN3_UnmgdCarbon", "AGLU_LEVEL2_DATA", "L233.LN3_UnmgdCarbon", "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml" )
write_mi_data( L233.LN3_UnmgdNoEmissCarbon, "LN3_UnmgdNoEmissCarbon", "AGLU_LEVEL2_DATA", "L233.LN3_UnmgdNoEmissCarbon", "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml" )
write_mi_data( L233.LN3_MgdNoEmissCarbon, "LN3_MgdNoEmissCarbon", "AGLU_LEVEL2_DATA", "L233.LN3_MgdNoEmissCarbon", "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml" )
write_mi_data( L233.LN3_NodeCarbon, "LN3_NodeCarbon", "AGLU_LEVEL2_DATA", "L233.LN3_NodeCarbon", "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml", node_rename = T )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_land_input_3_IRR.xml", "AGLU_XML_FINAL", "land_input_3_IRR.xml", "", xml_tag="outFile" )

logstop()
