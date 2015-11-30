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
logstart( "L215.protected_land_input_3.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Land cover and characteristics, node level 3" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
GCAMLandLeaf_CdensityLT <- readdata( "AGLU_MAPPINGS", "GCAMLandLeaf_CdensityLT" )
A_LandNode3 <- readdata( "AGLU_ASSUMPTIONS", "A_LandNode3" )
A_LandLeaf_Unmgd3 <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf_Unmgd3" )
A_LandLeaf3 <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf3" )
L121.VegC_kgm2_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.VegC_kgm2_R_LT_AEZ" )
L121.SoilC_kgm2_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.SoilC_kgm2_R_LT_AEZ" )
L121.MatureAge_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.MatureAge_R_LT_AEZ" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )
L125.LC_bm2_R_LT_Yh_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L125.LC_bm2_R_LT_Yh_AEZ" )

# -----------------------------------------------------------------------------
# 2. Build tables
#LAND USE HISTORY
#Subset the relevant land types from table with all land types
#Unmanaged land
L215.LC_bm2_R_Unmgd3_Yh_AEZ <- subset( L125.LC_bm2_R_LT_Yh_AEZ, Land_Type %in% A_LandLeaf_Unmgd3$UnmanagedLandLeaf )
L215.LC_bm2_R_Unmgd3_Yh_AEZ.melt <- melt( L215.LC_bm2_R_Unmgd3_Yh_AEZ, id.vars = R_LT_AEZ, variable.name = "Xyear" )
L215.LC_bm2_R_Unmgd3_Yh_AEZ.melt <- add_region_name( L215.LC_bm2_R_Unmgd3_Yh_AEZ.melt )
L215.LC_bm2_R_Unmgd3_Yh_AEZ.melt[[Y]] <- sub( "X", "", L215.LC_bm2_R_Unmgd3_Yh_AEZ.melt$Xyear )
# Set the land allocation to the "unprotected value"
L215.LC_bm2_R_Unmgd3_Yh_AEZ.melt$allocation <- L215.LC_bm2_R_Unmgd3_Yh_AEZ.melt$value * ( 1 - protect_land_fract )
L215.LC_bm2_R_Unmgd3_Yh_AEZ.melt <- add_node_leaf_names( L215.LC_bm2_R_Unmgd3_Yh_AEZ.melt,
       nesting_table = A_LandLeaf_Unmgd3, leaf_name = "UnmanagedLandLeaf", LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3" )

#Remove other arable land since we don't want to protect it
L215.LC_bm2_R_Unmgd3_Yh_AEZ.melt <- subset( L215.LC_bm2_R_Unmgd3_Yh_AEZ.melt, Land_Type != "OtherArableLand" )

printlog( "L215.LN3_HistUnmgdAllocation: Historical land cover, unmanaged land in the third nest" )
L215.LN3_HistUnmgdAllocation <- L215.LC_bm2_R_Unmgd3_Yh_AEZ.melt[ L215.LC_bm2_R_Unmgd3_Yh_AEZ.melt[[Y]] %in% land_history_years,
       names_LN3_HistUnmgdAllocation ]

printlog( "L215.LN3_UnmgdAllocation: Model base year land cover, unmanaged land in the third nest" )
L215.LN3_UnmgdAllocation <- L215.LC_bm2_R_Unmgd3_Yh_AEZ.melt[ L215.LC_bm2_R_Unmgd3_Yh_AEZ.melt[[Y]] %in% model_base_years,
       names_LN3_HistUnmgdAllocation ]

printlog( "L215.LN1_HistProtectAllocation: Historical land cover, protected land in the third nest" )
L215.LN1_AllProtectAllocation <- L215.LC_bm2_R_Unmgd3_Yh_AEZ.melt
# Set the land allocation to the "protected value"
L215.LN1_AllProtectAllocation$allocation <- L215.LN1_AllProtectAllocation$value * protect_land_fract
L215.LN1_AllProtectAllocation$UnmanagedLandLeaf <- paste( "Protected", L215.LN1_AllProtectAllocation$UnmanagedLandLeaf, sep="")
L215.LN1_AllProtectAllocation$LandNode1 <- L215.LN1_AllProtectAllocation$UnmanagedLandLeaf
L215.LN1_HistProtectAllocation <- L215.LN1_AllProtectAllocation[
      L215.LN1_AllProtectAllocation[[Y]] %in% land_history_years, names_LN1_UnmgdAllocation ]

printlog( "L215.LN1_ProtectAllocation: Model base year land cover, protected land in the third nest" )
L215.LN1_ProtectAllocation <- L215.LN1_AllProtectAllocation[
      L215.LN1_AllProtectAllocation[[Y]] %in% model_base_years, names_LN1_UnmgdAllocation ]

#CARBON CONTENTS AND MATURE AGES
#Melt tables with compiled carbon contents and mature ages
L215.VegC_kgm2_R_LT_AEZ.melt <- melt( L121.VegC_kgm2_R_LT_AEZ, id.vars = R_LT, variable.name = AEZ )
L215.SoilC_kgm2_R_LT_AEZ.melt <- melt( L121.SoilC_kgm2_R_LT_AEZ, id.vars = R_LT, variable.name = AEZ )
L215.MatureAge_R_LT_AEZ.melt <- melt( L121.MatureAge_R_LT_AEZ, id.vars = R_LT, variable.name = AEZ )

#Add region vector
L215.VegC_kgm2_R_LT_AEZ.melt <- add_region_name( L215.VegC_kgm2_R_LT_AEZ.melt )
L215.SoilC_kgm2_R_LT_AEZ.melt <- add_region_name( L215.SoilC_kgm2_R_LT_AEZ.melt )
L215.MatureAge_R_LT_AEZ.melt <- add_region_name( L215.MatureAge_R_LT_AEZ.melt )

printlog( "L215.LN3_UnmgdCarbon: Carbon content info, unmanaged land in the third nest" )
L215.LN3_UnmgdCarbon <- data.frame(
      region = rep( GCAM_region_names$region, times = nrow( A_LandLeaf_Unmgd3 ) ),
      Land_Type = sort( rep( A_LandLeaf_Unmgd3$UnmanagedLandLeaf, times = nrow( GCAM_region_names ) ) ) )
L215.LN3_UnmgdCarbon$Cdensity_LT <- GCAMLandLeaf_CdensityLT[[LT]][
      match( L215.LN3_UnmgdCarbon[[LT]], GCAMLandLeaf_CdensityLT$LandLeaf) ]
L215.LN3_UnmgdCarbon <- repeat_and_add_vector( L215.LN3_UnmgdCarbon, AEZ, AEZs )
L215.LN3_UnmgdCarbon <- add_node_leaf_names( L215.LN3_UnmgdCarbon,
      nesting_table = A_LandLeaf_Unmgd3, leaf_name = "UnmanagedLandLeaf", LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3" )

#Remove other arable land since we don't want to protect it
L215.LN3_UnmgdCarbon <- subset( L215.LN3_UnmgdCarbon, Land_Type != "OtherArableLand" )

#Match in carbon densities and mature age and sort the columns to match the model interface header
L215.LN3_UnmgdCarbon <- add_carbon_info( L215.LN3_UnmgdCarbon,
      veg_data = L215.VegC_kgm2_R_LT_AEZ.melt, soil_data = L215.SoilC_kgm2_R_LT_AEZ.melt, age_data = L215.MatureAge_R_LT_AEZ.melt )
L215.LN3_UnmgdCarbon <- L215.LN3_UnmgdCarbon[ names_LN3_UnmgdCarbon ]

printlog( "L215.LN1_ProtectCarbon: Carbon content info, protected land in the second nest" )
L215.LN1_ProtectCarbon <- L215.LN3_UnmgdCarbon
L215.LN1_ProtectCarbon <- L215.LN1_ProtectCarbon[ names( L215.LN1_ProtectCarbon ) %!in% c( "LandNode1", "LandNode2" ) ]
names( L215.LN1_ProtectCarbon )[ names( L215.LN1_ProtectCarbon ) == "LandNode3" ] <- "LandNode1"
L215.LN1_ProtectCarbon$LandNode1 <- paste( "Protected", L215.LN1_ProtectCarbon$UnmanagedLandLeaf, sep="" )
L215.LN1_ProtectCarbon$UnmanagedLandLeaf <- paste( "Protected", L215.LN1_ProtectCarbon$UnmanagedLandLeaf, sep="" )

printlog( "Removing non-existent AEZs from all tables" )
L215.LN3_HistUnmgdAllocation <- remove_AEZ_nonexist( L215.LN3_HistUnmgdAllocation, AEZcol = "LandNode1" )
L215.LN3_UnmgdAllocation <- remove_AEZ_nonexist( L215.LN3_UnmgdAllocation, AEZcol = "LandNode1" )
L215.LN1_HistProtectAllocation <- remove_AEZ_nonexist( L215.LN1_HistProtectAllocation, AEZcol = "LandNode1" )
L215.LN1_ProtectAllocation <- remove_AEZ_nonexist( L215.LN1_ProtectAllocation, AEZcol = "LandNode1" )
L215.LN1_ProtectCarbon <- remove_AEZ_nonexist( L215.LN1_ProtectCarbon, AEZcol = "LandNode1" )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L215.LN3_HistUnmgdAllocation, IDstring="LN3_HistUnmgdAllocation", domain="AGLU_LEVEL2_DATA", fn="L215.LN3_HistUnmgdAllocation",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_protected_land_input_3.xml" )
write_mi_data( L215.LN3_UnmgdAllocation, "LN3_UnmgdAllocation", "AGLU_LEVEL2_DATA", "L215.LN3_UnmgdAllocation", "AGLU_XML_BATCH", "batch_protected_land_input_3.xml" )
# Note no need to write out managed carbon as that information is already specified elsewhere.
write_mi_data( L215.LN1_HistProtectAllocation, "LN1_HistUnmgdAllocation", "AGLU_LEVEL2_DATA", "L215.LN1_HistProtectAllocation","AGLU_XML_BATCH", "batch_protected_land_input_3.xml" )
write_mi_data( L215.LN1_ProtectAllocation, "LN1_UnmgdAllocation", "AGLU_LEVEL2_DATA", "L215.LN1_ProtectAllocation","AGLU_XML_BATCH", "batch_protected_land_input_3.xml" )
write_mi_data( L215.LN1_ProtectCarbon, "LN1_UnmgdCarbon", "AGLU_LEVEL2_DATA", "L215.LN1_ProtectCarbon","AGLU_XML_BATCH", "batch_protected_land_input_3.xml", node_rename=T  )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_protected_land_input_3.xml", "AGLU_XML_FINAL", "protected_land_input_3.xml", "", xml_tag="outFile" )

logstop()
