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
logstart( "L214.protected_land_input_2.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Land cover and characteristics, node level 2" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
GCAMLandLeaf_CdensityLT <- readdata( "AGLU_MAPPINGS", "GCAMLandLeaf_CdensityLT" )
A_LandNode2 <- readdata( "AGLU_ASSUMPTIONS", "A_LandNode2" )
A_LandLeaf_Unmgd2 <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf_Unmgd2" )
A_LandLeaf2 <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf2" )
L121.VegC_kgm2_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.VegC_kgm2_R_LT_AEZ" )
L121.SoilC_kgm2_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.SoilC_kgm2_R_LT_AEZ" )
L121.MatureAge_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.MatureAge_R_LT_AEZ" )
L125.LC_bm2_R_LT_Yh_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L125.LC_bm2_R_LT_Yh_AEZ" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )

# -----------------------------------------------------------------------------
# 2. Build tables
#LAND ALLOCATION AND LAND USE HISTORY
#Subset the relevant land types from table with land cover by all land types
#Unmanaged land
L214.LC_bm2_R_Unmgd2_Yh_AEZ <- subset( L125.LC_bm2_R_LT_Yh_AEZ, Land_Type %in% A_LandLeaf_Unmgd2$UnmanagedLandLeaf )
L214.LC_bm2_R_Unmgd2_Yh_AEZ.melt <- melt( L214.LC_bm2_R_Unmgd2_Yh_AEZ, id.vars = R_LT_AEZ, variable_name = "Xyear" )
L214.LC_bm2_R_Unmgd2_Yh_AEZ.melt <- add_region_name( L214.LC_bm2_R_Unmgd2_Yh_AEZ.melt )
L214.LC_bm2_R_Unmgd2_Yh_AEZ.melt[[Y]] <- sub( "X", "", L214.LC_bm2_R_Unmgd2_Yh_AEZ.melt$Xyear )
# Set the land allocation to the "unprotected value"
L214.LC_bm2_R_Unmgd2_Yh_AEZ.melt$allocation <- L214.LC_bm2_R_Unmgd2_Yh_AEZ.melt$value * ( 1 - protect_land_fract )
L214.LC_bm2_R_Unmgd2_Yh_AEZ.melt <- add_node_leaf_names( L214.LC_bm2_R_Unmgd2_Yh_AEZ.melt,
       nesting_table = A_LandLeaf_Unmgd2, leaf_name = "UnmanagedLandLeaf", LN1 = "LandNode1", LN2 = "LandNode2" )

printlog( "L214.LN2_HistUnmgdAllocation: Historical land cover, unmanaged land in the second nest" )
L214.LN2_HistUnmgdAllocation <- L214.LC_bm2_R_Unmgd2_Yh_AEZ.melt[
      L214.LC_bm2_R_Unmgd2_Yh_AEZ.melt[[Y]] %in% land_history_years, names_LN2_HistUnmgdAllocation ]

printlog( "L214.LN2_UnmgdAllocation: Model base period land cover, unmanaged land in the second nest" )
L214.LN2_UnmgdAllocation <- L214.LC_bm2_R_Unmgd2_Yh_AEZ.melt[
      L214.LC_bm2_R_Unmgd2_Yh_AEZ.melt[[Y]] %in% model_base_years, names_LN2_UnmgdAllocation ]

printlog( "L214.LN1_HistProtectAllocation: Historical land cover, protected land in the second nest" )
L214.LN1_AllProtectAllocation <- L214.LC_bm2_R_Unmgd2_Yh_AEZ.melt
# Set the land allocation to the "protected value"
L214.LN1_AllProtectAllocation$allocation <- L214.LN1_AllProtectAllocation$value * protect_land_fract
L214.LN1_AllProtectAllocation$UnmanagedLandLeaf <- paste( "Protected", L214.LN1_AllProtectAllocation$UnmanagedLandLeaf, sep="")
L214.LN1_AllProtectAllocation$LandNode1 <- L214.LN1_AllProtectAllocation$UnmanagedLandLeaf
L214.LN1_HistProtectAllocation <- L214.LN1_AllProtectAllocation[
      L214.LN1_AllProtectAllocation[[Y]] %in% land_history_years, names_LN1_UnmgdAllocation ]
                    
printlog( "L214.LN1_ProtectAllocation: Model base period land cover, protected land in the second nest" )
L214.LN1_ProtectAllocation <- L214.LN1_AllProtectAllocation[
      L214.LN1_AllProtectAllocation[[Y]] %in% model_base_years, names_LN1_UnmgdAllocation ]
                    
#CARBON CONTENTS AND MATURE AGES
#Melt tables with compiled carbon contents and mature ages
L214.VegC_kgm2_R_LT_AEZ.melt <- melt( L121.VegC_kgm2_R_LT_AEZ, id.vars = R_LT, variable_name = AEZ )
L214.SoilC_kgm2_R_LT_AEZ.melt <- melt( L121.SoilC_kgm2_R_LT_AEZ, id.vars = R_LT, variable_name = AEZ )
L214.MatureAge_R_LT_AEZ.melt <- melt( L121.MatureAge_R_LT_AEZ, id.vars = R_LT, variable_name = AEZ )

#Add region vector
L214.VegC_kgm2_R_LT_AEZ.melt <- add_region_name( L214.VegC_kgm2_R_LT_AEZ.melt )
L214.SoilC_kgm2_R_LT_AEZ.melt <- add_region_name( L214.SoilC_kgm2_R_LT_AEZ.melt )
L214.MatureAge_R_LT_AEZ.melt <- add_region_name( L214.MatureAge_R_LT_AEZ.melt )

printlog( "L214.LN2_UnmgdCarbon: Carbon content info, unmanaged land in the second nest" )
L214.LN2_UnmgdCarbon <- data.frame(
      region = rep( GCAM_region_names$region, times = nrow( A_LandLeaf_Unmgd2 ) ),
      Land_Type = sort( rep( A_LandLeaf_Unmgd2$UnmanagedLandLeaf, times = nrow( GCAM_region_names ) ) ) )
L214.LN2_UnmgdCarbon$Cdensity_LT <- GCAMLandLeaf_CdensityLT[[LT]][
      match( L214.LN2_UnmgdCarbon[[LT]], GCAMLandLeaf_CdensityLT$LandLeaf) ]
L214.LN2_UnmgdCarbon <- repeat_and_add_vector( L214.LN2_UnmgdCarbon, AEZ, AEZs )
L214.LN2_UnmgdCarbon <- add_node_leaf_names( L214.LN2_UnmgdCarbon,
      nesting_table = A_LandLeaf_Unmgd2, leaf_name = "UnmanagedLandLeaf", LN1 = "LandNode1", LN2 = "LandNode2" )

#Match in carbon densities and mature age and sort the columns to match the model interface header
L214.LN2_UnmgdCarbon <- add_carbon_info( L214.LN2_UnmgdCarbon,
      veg_data = L214.VegC_kgm2_R_LT_AEZ.melt, soil_data = L214.SoilC_kgm2_R_LT_AEZ.melt, age_data = L214.MatureAge_R_LT_AEZ.melt )
L214.LN2_UnmgdCarbon <- L214.LN2_UnmgdCarbon[ names_LN2_UnmgdCarbon ]

printlog( "L214.LN1_ProtectCarbon: Carbon content info, protected land in the second nest" )
L214.LN1_ProtectCarbon <- L214.LN2_UnmgdCarbon
L214.LN1_ProtectCarbon <- L214.LN1_ProtectCarbon[ names( L214.LN1_ProtectCarbon ) != "LandNode1" ]
names( L214.LN1_ProtectCarbon )[ names( L214.LN1_ProtectCarbon ) == "LandNode2" ] <- "LandNode1"
L214.LN1_ProtectCarbon$LandNode1 <- paste( "Protected", L214.LN1_ProtectCarbon$UnmanagedLandLeaf, sep="" )
L214.LN1_ProtectCarbon$UnmanagedLandLeaf <- paste( "Protected", L214.LN1_ProtectCarbon$UnmanagedLandLeaf, sep="" )
 
printlog( "Removing non-existent AEZs from all tables" )
L214.LN2_HistUnmgdAllocation <- remove_AEZ_nonexist( L214.LN2_HistUnmgdAllocation, AEZcol = "LandNode1" )
L214.LN2_UnmgdAllocation <- remove_AEZ_nonexist( L214.LN2_UnmgdAllocation, AEZcol = "LandNode1" )
L214.LN1_HistProtectAllocation <- remove_AEZ_nonexist( L214.LN1_HistProtectAllocation, AEZcol = "LandNode1" )
L214.LN1_ProtectAllocation <- remove_AEZ_nonexist( L214.LN1_ProtectAllocation, AEZcol = "LandNode1" )
L214.LN2_UnmgdCarbon <- remove_AEZ_nonexist( L214.LN2_UnmgdCarbon, AEZcol = "LandNode1" )
L214.LN1_ProtectCarbon <- remove_AEZ_nonexist( L214.LN1_ProtectCarbon, AEZcol = "LandNode1" )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L214.LN2_HistUnmgdAllocation, IDstring="LN2_HistUnmgdAllocation", domain="AGLU_LEVEL2_DATA", fn="L214.LN2_HistUnmgdAllocation",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_protected_land_input_2.xml" )
write_mi_data( L214.LN2_UnmgdAllocation, "LN2_UnmgdAllocation", "AGLU_LEVEL2_DATA", "L214.LN2_UnmgdAllocation","AGLU_XML_BATCH", "batch_protected_land_input_2.xml" )
write_mi_data( L214.LN1_HistProtectAllocation, "LN1_HistUnmgdAllocation", "AGLU_LEVEL2_DATA", "L214.LN1_HistProtectAllocation","AGLU_XML_BATCH", "batch_protected_land_input_2.xml" )
write_mi_data( L214.LN1_ProtectAllocation, "LN1_UnmgdAllocation", "AGLU_LEVEL2_DATA", "L214.LN1_ProtectAllocation","AGLU_XML_BATCH", "batch_protected_land_input_2.xml" )
write_mi_data( L214.LN2_UnmgdCarbon, "LN2_UnmgdCarbon", "AGLU_LEVEL2_DATA", "L214.LN2_UnmgdCarbon","AGLU_XML_BATCH", "batch_protected_land_input_2.xml" )
write_mi_data( L214.LN1_ProtectCarbon, "LN1_UnmgdCarbon", "AGLU_LEVEL2_DATA", "L214.LN1_ProtectCarbon","AGLU_XML_BATCH", "batch_protected_land_input_2.xml", node_rename=T  )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_protected_land_input_2.xml", "AGLU_XML_FINAL", "protected_land_input_2.xml", "", xml_tag="outFile" )

logstop()
