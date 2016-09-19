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
logstart( "L222.land_input_2.R" )
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
A_LandLeaf_Unmgd2 <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf_Unmgd2" )
A_LandLeaf2 <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf2" )
A_LT_Mapping <- readdata( "AGLU_ASSUMPTIONS", "A_LT_Mapping" )
L121.CarbonContent_kgm2_R_LT_GLU <- readdata( "AGLU_LEVEL1_DATA", "L121.CarbonContent_kgm2_R_LT_GLU" )
L125.LC_bm2_R_LT_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L125.LC_bm2_R_LT_Yh_GLU" )

# -----------------------------------------------------------------------------
# 2. Build tables
# Determine which node combinations are applicable at this level
L125.LC_bm2_R_LT_Yh_GLU[ c( "LandNode1", "LandNode2" ) ] <- A_LT_Mapping[
  match( L125.LC_bm2_R_LT_Yh_GLU[[LT]], A_LT_Mapping[[LT]] ),
  c( "LandNode1", "LandNode2" ) ]
L222.LN2 <- na.omit( unique( L125.LC_bm2_R_LT_Yh_GLU[ c( R, GLU, "LandNode1", "LandNode2" ) ] ) )

printlog( "L222.LN2_Logit: Logit exponent of the second nest" )
L222.LN2_Logit <- add_region_name( L222.LN2 )
L222.LN2_Logit$LandAllocatorRoot <- "root"

#Match in logit exponents based on the land node 2
L222.LN2_Logit$logit.year.fillout <- min( model_base_years )
L222.LN2_Logit$logit.exponent <- A_LandNode_logit$logit.exponent[ match( L222.LN2_Logit$LandNode2, A_LandNode_logit$LandNode ) ]

#Append GLU names and keep only relevant columns
L222.LN2_Logit <- append_GLU( L222.LN2_Logit, var1 = "LandNode1", var2 = "LandNode2" )
L222.LN2_Logit <- L222.LN2_Logit[ names_LN2_Logit ]

#LAND ALLOCATION AND LAND USE HISTORY
#Unmanaged land
L222.LC_bm2_R_Unmgd2_Yh_GLU <- L125.LC_bm2_R_LT_Yh_GLU[ L125.LC_bm2_R_LT_Yh_GLU[[LT]] %in% A_LandLeaf_Unmgd2$UnmanagedLandLeaf, ]
L222.LC_bm2_R_Unmgd2_Yh_GLU.melt <- interpolate_and_melt( L222.LC_bm2_R_Unmgd2_Yh_GLU,
                                                          unique( c( land_history_years, model_base_years ) ),
                                                          value.name = "allocation", digits = digits_land_use )
L222.LC_bm2_R_Unmgd2_Yh_GLU.melt <- add_region_name( L222.LC_bm2_R_Unmgd2_Yh_GLU.melt )
L222.LC_bm2_R_Unmgd2_Yh_GLU.melt <- add_node_leaf_names( L222.LC_bm2_R_Unmgd2_Yh_GLU.melt,
                                                         nesting_table = A_LandLeaf_Unmgd2, leaf_name = "UnmanagedLandLeaf",
                                                         LN1 = "LandNode1", LN2 = "LandNode2" )

printlog( "L222.LN2_HistUnmgdAllocation: Historical land cover, unmanaged land in the second nest" )
L222.LN2_HistUnmgdAllocation <- L222.LC_bm2_R_Unmgd2_Yh_GLU.melt[
  L222.LC_bm2_R_Unmgd2_Yh_GLU.melt[[Y]] %in% land_history_years, names_LN2_HistUnmgdAllocation ]

printlog( "L222.LN2_UnmgdAllocation: Model base period land cover, unmanaged land in the second nest" )
L222.LN2_UnmgdAllocation <- L222.LC_bm2_R_Unmgd2_Yh_GLU.melt[
  L222.LC_bm2_R_Unmgd2_Yh_GLU.melt[[Y]] %in% model_base_years, names_LN2_UnmgdAllocation ]

#Managed land
L222.LC_bm2_R_Mgd2_Yh_GLU <- L125.LC_bm2_R_LT_Yh_GLU[ L125.LC_bm2_R_LT_Yh_GLU[[LT]] %in% A_LandLeaf2$LandLeaf, ]
L222.LC_bm2_R_Mgd2_Yh_GLU.melt <- interpolate_and_melt( L222.LC_bm2_R_Mgd2_Yh_GLU,
                                                        unique( c( land_history_years, model_base_years ) ),
                                                        value.name = "allocation", digits = digits_land_use )
L222.LC_bm2_R_Mgd2_Yh_GLU.melt <- add_region_name( L222.LC_bm2_R_Mgd2_Yh_GLU.melt )
L222.LC_bm2_R_Mgd2_Yh_GLU.melt <- add_node_leaf_names( L222.LC_bm2_R_Mgd2_Yh_GLU.melt,
                                                       nesting_table = A_LandLeaf2, leaf_name = "LandLeaf",
                                                       LN1 = "LandNode1", LN2 = "LandNode2" )

printlog( "L222.LN2_HistMgdAllocation: Historical land cover, managed land in the second nest" )
L222.LN2_HistMgdAllocation <- L222.LC_bm2_R_Mgd2_Yh_GLU.melt[ L222.LC_bm2_R_Mgd2_Yh_GLU.melt[[Y]] %in% land_history_years, names_LN2_HistMgdAllocation ]

printlog( "L222.LN2_MgdAllocation: Model base period land cover, managed land in the second nest" )
L222.LN2_MgdAllocation <- L222.LC_bm2_R_Mgd2_Yh_GLU.melt[ L222.LC_bm2_R_Mgd2_Yh_GLU.melt[[Y]] %in% model_base_years, names_LN2_MgdAllocation ]

#CARBON CONTENTS AND MATURE AGES
printlog( "L222.LN2_UnmgdCarbon: Carbon content info, unmanaged land in the second nest" )
L222.LN2_UnmgdCarbon <- subset( L222.LC_bm2_R_Unmgd2_Yh_GLU.melt, year == max( model_base_years ) )[
  !names( L222.LC_bm2_R_Unmgd2_Yh_GLU.melt ) %in% c( "variable", "allocation", "year" ) ]
L222.LN2_UnmgdCarbon$Cdensity_LT <- GCAMLandLeaf_CdensityLT[[LT]][
      match( L222.LN2_UnmgdCarbon[[LT]], GCAMLandLeaf_CdensityLT$LandLeaf) ]
L222.LN2_UnmgdCarbon <- add_carbon_info( L222.LN2_UnmgdCarbon, carbon_info_table = L121.CarbonContent_kgm2_R_LT_GLU )
L222.LN2_UnmgdCarbon <- L222.LN2_UnmgdCarbon[ names_LN2_UnmgdCarbon ]

printlog( "L222.LN2_MgdCarbon: Carbon content info, managed land in the second nest" )
L222.LN2_MgdCarbon <- subset( L222.LC_bm2_R_Mgd2_Yh_GLU.melt, year == max( model_base_years ) )[
  !names( L222.LC_bm2_R_Unmgd2_Yh_GLU.melt ) %in% c( "variable", "allocation", "year" ) ]
L222.LN2_MgdCarbon$Cdensity_LT <- GCAMLandLeaf_CdensityLT[[LT]][
      match( L222.LN2_MgdCarbon[[LT]], GCAMLandLeaf_CdensityLT$LandLeaf) ]
L222.LN2_MgdCarbon <- add_carbon_info( L222.LN2_MgdCarbon, carbon_info_table = L121.CarbonContent_kgm2_R_LT_GLU )
L222.LN2_MgdCarbon <- L222.LN2_MgdCarbon[ names_LN2_MgdCarbon ]

printlog( "Writing out information on protected lands, where applicable" )
printlog( "L222.LN2_HistUnmgdAllocation_noprot: historical unmanaged land cover, no protect")
L222.LN2_HistUnmgdAllocation_noprot <- L222.LN2_HistUnmgdAllocation
L222.LN2_HistUnmgdAllocation_noprot$allocation <- L222.LN2_HistUnmgdAllocation$allocation * ( 1 - protect_land_fract )

printlog( "L222.LN2_HistUnmgdAllocation_noprot: unmanaged land cover, no protect")
L222.LN2_UnmgdAllocation_noprot <- L222.LN2_UnmgdAllocation
L222.LN2_UnmgdAllocation_noprot$allocation <- L222.LN2_UnmgdAllocation$allocation * ( 1 - protect_land_fract )

#no changes needed to carbon file for the non-protected lands

printlog( "Protected land files: modified land allocations, different names, different nesting structure" )
printlog( "L222.LN1_HistUnmgdAllocation_prot: unmanaged land cover, protected")
L222.LN1_HistUnmgdAllocation_prot <- L222.LN2_HistUnmgdAllocation
L222.LN1_HistUnmgdAllocation_prot$UnmanagedLandLeaf <- paste0( "Protected", L222.LN2_HistUnmgdAllocation$UnmanagedLandLeaf )
L222.LN1_HistUnmgdAllocation_prot$LandNode1 <- L222.LN1_HistUnmgdAllocation_prot$UnmanagedLandLeaf
L222.LN1_HistUnmgdAllocation_prot$LandNode2 <- NULL
L222.LN1_HistUnmgdAllocation_prot$allocation <- L222.LN2_HistUnmgdAllocation$allocation * protect_land_fract

printlog( "L222.LN1_UnmgdAllocation_prot: unmanaged land cover, protected")
L222.LN1_UnmgdAllocation_prot <- L222.LN2_UnmgdAllocation
L222.LN1_UnmgdAllocation_prot$UnmanagedLandLeaf <- paste0( "Protected", L222.LN1_UnmgdAllocation_prot$UnmanagedLandLeaf )
L222.LN1_UnmgdAllocation_prot$LandNode1 <- L222.LN1_UnmgdAllocation_prot$UnmanagedLandLeaf
L222.LN1_UnmgdAllocation_prot$LandNode2 <- NULL
L222.LN1_UnmgdAllocation_prot$allocation <- L222.LN2_UnmgdAllocation$allocation * protect_land_fract

printlog( "L222.LN1_UnmgdCarbon_prot: unmanaged carbon info, protected")
L222.LN1_UnmgdCarbon_prot <- L222.LN2_UnmgdCarbon
L222.LN1_UnmgdCarbon_prot$UnmanagedLandLeaf <- paste0( "Protected", L222.LN1_UnmgdCarbon_prot$UnmanagedLandLeaf )
L222.LN1_UnmgdCarbon_prot$LandNode1 <- L222.LN1_UnmgdCarbon_prot$UnmanagedLandLeaf
L222.LN1_UnmgdCarbon_prot$LandNode2 <- NULL

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L222.LN2_Logit, IDstring="LN2_Logit", domain="AGLU_LEVEL2_DATA", fn="L222.LN2_Logit",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_land_input_2.xml" )
write_mi_data( L222.LN2_HistUnmgdAllocation, "LN2_HistUnmgdAllocation", "AGLU_LEVEL2_DATA", "L222.LN2_HistUnmgdAllocation", "AGLU_XML_BATCH", "batch_land_input_2.xml" )
write_mi_data( L222.LN2_UnmgdAllocation, "LN2_UnmgdAllocation", "AGLU_LEVEL2_DATA", "L222.LN2_UnmgdAllocation", "AGLU_XML_BATCH", "batch_land_input_2.xml" )
write_mi_data( L222.LN2_HistMgdAllocation, "LN2_HistMgdAllocation", "AGLU_LEVEL2_DATA", "L222.LN2_HistMgdAllocation", "AGLU_XML_BATCH", "batch_land_input_2.xml" )
write_mi_data( L222.LN2_MgdAllocation, "LN2_MgdAllocation", "AGLU_LEVEL2_DATA", "L222.LN2_MgdAllocation", "AGLU_XML_BATCH", "batch_land_input_2.xml" )
write_mi_data( L222.LN2_UnmgdCarbon, "LN2_UnmgdCarbon", "AGLU_LEVEL2_DATA", "L222.LN2_UnmgdCarbon", "AGLU_XML_BATCH", "batch_land_input_2.xml" )
write_mi_data( L222.LN2_MgdCarbon, "LN2_MgdCarbon", "AGLU_LEVEL2_DATA", "L222.LN2_MgdCarbon", "AGLU_XML_BATCH", "batch_land_input_2.xml", node_rename=T )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_land_input_2.xml", "AGLU_XML_FINAL", "land_input_2.xml", "", xml_tag="outFile" )

#protected lands files
write_mi_data( L222.LN2_HistUnmgdAllocation_noprot, "LN2_HistUnmgdAllocation", "AGLU_LEVEL2_DATA", "L222.LN2_HistUnmgdAllocation_noprot", "AGLU_XML_BATCH", "batch_protected_land_input_2.xml" )
write_mi_data( L222.LN2_UnmgdAllocation_noprot, "LN2_UnmgdAllocation", "AGLU_LEVEL2_DATA", "L222.LN2_UnmgdAllocation_noprot", "AGLU_XML_BATCH", "batch_protected_land_input_2.xml" )
write_mi_data( L222.LN2_UnmgdCarbon, "LN2_UnmgdCarbon", "AGLU_LEVEL2_DATA", "L222.LN2_UnmgdCarbon", "AGLU_XML_BATCH", "batch_protected_land_input_2.xml" )

write_mi_data( L222.LN1_HistUnmgdAllocation_prot, "LN1_HistUnmgdAllocation", "AGLU_LEVEL2_DATA", "L222.LN1_HistUnmgdAllocation_prot", "AGLU_XML_BATCH", "batch_protected_land_input_2.xml" )
write_mi_data( L222.LN1_UnmgdAllocation_prot, "LN1_UnmgdAllocation", "AGLU_LEVEL2_DATA", "L222.LN1_UnmgdAllocation_prot", "AGLU_XML_BATCH", "batch_protected_land_input_2.xml" )
write_mi_data( L222.LN1_UnmgdCarbon_prot, "LN1_UnmgdCarbon", "AGLU_LEVEL2_DATA", "L222.LN1_UnmgdCarbon_prot", "AGLU_XML_BATCH", "batch_protected_land_input_2.xml", node_rename=T )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_protected_land_input_2.xml", "AGLU_XML_FINAL", "protected_land_input_2.xml", "", xml_tag="outFile" )

logstop()
