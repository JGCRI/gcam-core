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
logstart( "L212.land_input_2.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Land cover and characteristics, node level 2" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
GCAMLandLeaf_CdensityLT <- readdata( "AGLU_MAPPINGS", "GCAMLandLeaf_CdensityLT" )
A_LandNode_logit <- readdata( "AGLU_ASSUMPTIONS", "A_LandNode_logit" )
A_LandNode2 <- readdata( "AGLU_ASSUMPTIONS", "A_LandNode2" )
A_LandLeaf_Unmgd2 <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf_Unmgd2" )
A_LandLeaf2 <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf2" )
L121.VegC_kgm2_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.VegC_kgm2_R_LT_AEZ" )
L121.SoilC_kgm2_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.SoilC_kgm2_R_LT_AEZ" )
L121.MatureAge_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.MatureAge_R_LT_AEZ" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )
L125.LC_bm2_R_LT_Yh_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L125.LC_bm2_R_LT_Yh_AEZ" )

# -----------------------------------------------------------------------------
# 2. Build tables
printlog( "L212.LN2_Logit: Logit exponent of the third nest" )
L212.LN2_Logit <- data.frame(
      region = rep( GCAM_region_names$region, times = nrow( A_LandNode2 ) ),
      LandAllocatorRoot = "root",
      LandNode2 = sort( rep( A_LandNode2$LandNode2, times = length( GCAM_region_names$region ) ) ) )
L212.LN2_Logit$LandNode1 <- A_LandNode2$LandNode1[ match( L212.LN2_Logit$LandNode2, A_LandNode2$LandNode2 ) ]
L212.LN2_Logit <- repeat_and_add_vector( L212.LN2_Logit, AEZ, AEZs )

#Match in logit exponents based on the land node 2
L212.LN2_Logit$logit.year.fillout <- min( model_base_years )
L212.LN2_Logit$logit.exponent <- A_LandNode_logit$logit.exponent[ match( L212.LN2_Logit$LandNode2, A_LandNode_logit$LandNode ) ]
L212.LN2_Logit$logit.exponent[ L212.LN2_Logit$AEZ %in% AEZs_most_arid ] <- A_LandNode_logit$logit.exponent.arid[
      match( L212.LN2_Logit$LandNode2[ L212.LN2_Logit$AEZ %in% AEZs_most_arid ], A_LandNode_logit$LandNode ) ]

#Append AEZ names and keep only relevant columns
L212.LN2_Logit <- append_AEZ( L212.LN2_Logit, var1 = "LandNode1", var2 = "LandNode2" )
L212.LN2_Logit <- L212.LN2_Logit[ names_LN2_Logit ]

#LAND ALLOCATION AND LAND USE HISTORY
#Unmanaged land
L212.LC_bm2_R_Unmgd2_Yh_AEZ <- subset( L125.LC_bm2_R_LT_Yh_AEZ, Land_Type %in% A_LandLeaf_Unmgd2$UnmanagedLandLeaf )
L212.LC_bm2_R_Unmgd2_Yh_AEZ.melt <- melt( L212.LC_bm2_R_Unmgd2_Yh_AEZ, id.vars = R_LT_AEZ, variable_name = "Xyear" )
L212.LC_bm2_R_Unmgd2_Yh_AEZ.melt <- add_region_name( L212.LC_bm2_R_Unmgd2_Yh_AEZ.melt )
L212.LC_bm2_R_Unmgd2_Yh_AEZ.melt[[Y]] <- sub( "X", "", L212.LC_bm2_R_Unmgd2_Yh_AEZ.melt$Xyear )
L212.LC_bm2_R_Unmgd2_Yh_AEZ.melt$allocation <- L212.LC_bm2_R_Unmgd2_Yh_AEZ.melt$value
L212.LC_bm2_R_Unmgd2_Yh_AEZ.melt <- add_node_leaf_names( L212.LC_bm2_R_Unmgd2_Yh_AEZ.melt,
       nesting_table = A_LandLeaf_Unmgd2, leaf_name = "UnmanagedLandLeaf", LN1 = "LandNode1", LN2 = "LandNode2" )

printlog( "L212.LN2_HistUnmgdAllocation: Historical land cover, unmanaged land in the second nest" )
L212.LN2_HistUnmgdAllocation <- L212.LC_bm2_R_Unmgd2_Yh_AEZ.melt[
      L212.LC_bm2_R_Unmgd2_Yh_AEZ.melt[[Y]] %in% land_history_years, names_LN2_HistUnmgdAllocation ]

printlog( "L212.LN2_UnmgdAllocation: Model base period land cover, unmanaged land in the second nest" )
L212.LN2_UnmgdAllocation <- L212.LC_bm2_R_Unmgd2_Yh_AEZ.melt[
      L212.LC_bm2_R_Unmgd2_Yh_AEZ.melt[[Y]] %in% model_base_years, names_LN2_UnmgdAllocation ]

#Managed land
L212.LC_bm2_R_Mgd2_Yh_AEZ <- subset( L125.LC_bm2_R_LT_Yh_AEZ, Land_Type %in% A_LandLeaf2$LandLeaf )
L212.LC_bm2_R_Mgd2_Yh_AEZ.melt <- melt( L212.LC_bm2_R_Mgd2_Yh_AEZ, id.vars = R_LT_AEZ, variable_name = "Xyear" )
L212.LC_bm2_R_Mgd2_Yh_AEZ.melt <- add_region_name( L212.LC_bm2_R_Mgd2_Yh_AEZ.melt )
L212.LC_bm2_R_Mgd2_Yh_AEZ.melt[[Y]] <- sub( "X", "", L212.LC_bm2_R_Mgd2_Yh_AEZ.melt$Xyear )
L212.LC_bm2_R_Mgd2_Yh_AEZ.melt$allocation <- L212.LC_bm2_R_Mgd2_Yh_AEZ.melt$value
L212.LC_bm2_R_Mgd2_Yh_AEZ.melt <- add_node_leaf_names( L212.LC_bm2_R_Mgd2_Yh_AEZ.melt,
       nesting_table = A_LandLeaf2, leaf_name = "LandLeaf", LN1 = "LandNode1", LN2 = "LandNode2" )

printlog( "L212.LN2_HistMgdAllocation: Historical land cover, managed land in the second nest" )
L212.LN2_HistMgdAllocation <- L212.LC_bm2_R_Mgd2_Yh_AEZ.melt[ L212.LC_bm2_R_Mgd2_Yh_AEZ.melt[[Y]] %in% land_history_years, names_LN2_HistMgdAllocation ]

printlog( "L212.LN2_MgdAllocation: Model base period land cover, managed land in the second nest" )
L212.LN2_MgdAllocation <- L212.LC_bm2_R_Mgd2_Yh_AEZ.melt[ L212.LC_bm2_R_Mgd2_Yh_AEZ.melt[[Y]] %in% model_base_years, names_LN2_MgdAllocation ]

#CARBON CONTENTS AND MATURE AGES
#Melt tables with compiled carbon contents and mature ages
L212.VegC_kgm2_R_LT_AEZ.melt <- melt( L121.VegC_kgm2_R_LT_AEZ, id.vars = R_LT, variable_name = AEZ )
L212.SoilC_kgm2_R_LT_AEZ.melt <- melt( L121.SoilC_kgm2_R_LT_AEZ, id.vars = R_LT, variable_name = AEZ )
L212.MatureAge_R_LT_AEZ.melt <- melt( L121.MatureAge_R_LT_AEZ, id.vars = R_LT, variable_name = AEZ )

#Add region vector
L212.VegC_kgm2_R_LT_AEZ.melt <- add_region_name( L212.VegC_kgm2_R_LT_AEZ.melt )
L212.SoilC_kgm2_R_LT_AEZ.melt <- add_region_name( L212.SoilC_kgm2_R_LT_AEZ.melt )
L212.MatureAge_R_LT_AEZ.melt <- add_region_name( L212.MatureAge_R_LT_AEZ.melt )

printlog( "L212.LN2_UnmgdCarbon: Carbon content info, unmanaged land in the second nest" )
L212.LN2_UnmgdCarbon <- data.frame(
      region = rep( GCAM_region_names$region, times = nrow( A_LandLeaf_Unmgd2 ) ),
      Land_Type = sort( rep( A_LandLeaf_Unmgd2$UnmanagedLandLeaf, times = nrow( GCAM_region_names ) ) ) )
L212.LN2_UnmgdCarbon$Cdensity_LT <- GCAMLandLeaf_CdensityLT[[LT]][
      match( L212.LN2_UnmgdCarbon[[LT]], GCAMLandLeaf_CdensityLT$LandLeaf) ]
L212.LN2_UnmgdCarbon <- repeat_and_add_vector( L212.LN2_UnmgdCarbon, AEZ, AEZs )
L212.LN2_UnmgdCarbon <- add_node_leaf_names( L212.LN2_UnmgdCarbon,
      nesting_table = A_LandLeaf_Unmgd2, leaf_name = "UnmanagedLandLeaf", LN1 = "LandNode1", LN2 = "LandNode2" )

#Match in carbon densities and mature age and sort the columns to match the model interface header
L212.LN2_UnmgdCarbon <- add_carbon_info( L212.LN2_UnmgdCarbon,
      veg_data = L212.VegC_kgm2_R_LT_AEZ.melt, soil_data = L212.SoilC_kgm2_R_LT_AEZ.melt, age_data = L212.MatureAge_R_LT_AEZ.melt )
L212.LN2_UnmgdCarbon <- L212.LN2_UnmgdCarbon[ names_LN2_UnmgdCarbon ]

printlog( "L212.LN2_MgdCarbon: Carbon content info, managed land in the second nest" )
L212.LN2_MgdCarbon <- data.frame(
      region = rep( GCAM_region_names$region, times = nrow( A_LandLeaf2 ) ),
      Land_Type = sort( rep( A_LandLeaf2$LandLeaf, times = nrow( GCAM_region_names ) ) ) )
L212.LN2_MgdCarbon$Cdensity_LT <- GCAMLandLeaf_CdensityLT[[LT]][
      match( L212.LN2_MgdCarbon[[LT]], GCAMLandLeaf_CdensityLT$LandLeaf) ]
L212.LN2_MgdCarbon <- repeat_and_add_vector( L212.LN2_MgdCarbon, AEZ, AEZs )
L212.LN2_MgdCarbon <- add_node_leaf_names( L212.LN2_MgdCarbon,
      nesting_table = A_LandLeaf2, leaf_name = "LandLeaf", LN1 = "LandNode1", LN2 = "LandNode2" )

#Match in carbon densities and mature age and sort the columns to match the model interface header
L212.LN2_MgdCarbon <- add_carbon_info( L212.LN2_MgdCarbon,
      veg_data = L212.VegC_kgm2_R_LT_AEZ.melt, soil_data = L212.SoilC_kgm2_R_LT_AEZ.melt, age_data = L212.MatureAge_R_LT_AEZ.melt )
L212.LN2_MgdCarbon <- L212.LN2_MgdCarbon[ names_LN2_MgdCarbon ]

printlog( "Removing non-existent AEZs from all tables" )
L212.LN2_Logit <- remove_AEZ_nonexist( L212.LN2_Logit, AEZcol = "LandNode1" )
L212.LN2_HistUnmgdAllocation <- remove_AEZ_nonexist( L212.LN2_HistUnmgdAllocation, AEZcol = "LandNode1" )
L212.LN2_UnmgdAllocation <- remove_AEZ_nonexist( L212.LN2_UnmgdAllocation, AEZcol = "LandNode1" )
L212.LN2_HistMgdAllocation <- remove_AEZ_nonexist( L212.LN2_HistMgdAllocation, AEZcol = "LandNode1" )
L212.LN2_MgdAllocation <- remove_AEZ_nonexist( L212.LN2_MgdAllocation, AEZcol = "LandNode1" )
L212.LN2_UnmgdCarbon <- remove_AEZ_nonexist( L212.LN2_UnmgdCarbon, AEZcol = "LandNode1" )
L212.LN2_MgdCarbon <- remove_AEZ_nonexist( L212.LN2_MgdCarbon, AEZcol = "LandNode1" )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L212.LN2_Logit, IDstring="LN2_Logit", domain="AGLU_LEVEL2_DATA", fn="L212.LN2_Logit",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_land_input_2.xml" )
write_mi_data( L212.LN2_HistUnmgdAllocation, "LN2_HistUnmgdAllocation", "AGLU_LEVEL2_DATA", "L212.LN2_HistUnmgdAllocation", "AGLU_XML_BATCH", "batch_land_input_2.xml" )
write_mi_data( L212.LN2_UnmgdAllocation, "LN2_UnmgdAllocation", "AGLU_LEVEL2_DATA", "L212.LN2_UnmgdAllocation", "AGLU_XML_BATCH", "batch_land_input_2.xml" )
write_mi_data( L212.LN2_HistMgdAllocation, "LN2_HistMgdAllocation", "AGLU_LEVEL2_DATA", "L212.LN2_HistMgdAllocation", "AGLU_XML_BATCH", "batch_land_input_2.xml" )
write_mi_data( L212.LN2_MgdAllocation, "LN2_MgdAllocation", "AGLU_LEVEL2_DATA", "L212.LN2_MgdAllocation", "AGLU_XML_BATCH", "batch_land_input_2.xml" )
write_mi_data( L212.LN2_UnmgdCarbon, "LN2_UnmgdCarbon", "AGLU_LEVEL2_DATA", "L212.LN2_UnmgdCarbon", "AGLU_XML_BATCH", "batch_land_input_2.xml" )
write_mi_data( L212.LN2_MgdCarbon, "LN2_MgdCarbon", "AGLU_LEVEL2_DATA", "L212.LN2_MgdCarbon", "AGLU_XML_BATCH", "batch_land_input_2.xml", node_rename=T )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_land_input_2.xml", "AGLU_XML_FINAL", "land_input_2.xml", "", xml_tag="outFile" )

logstop()
