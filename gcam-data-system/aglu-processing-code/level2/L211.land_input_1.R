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
logstart( "L211.land_input_1.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Land cover and characteristics, node level 1" )

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
A_LandNode1 <- readdata( "AGLU_ASSUMPTIONS", "A_LandNode1" )
A_LandLeaf_Unmgd1 <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf_Unmgd1" )
A_soil_time_scale_R <- readdata( "AGLU_ASSUMPTIONS", "A_soil_time_scale_R" )
L121.VegC_kgm2_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.VegC_kgm2_R_LT_AEZ" )
L121.SoilC_kgm2_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.SoilC_kgm2_R_LT_AEZ" )
L121.MatureAge_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.MatureAge_R_LT_AEZ" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )
L125.LC_bm2_R_LT_Yh_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L125.LC_bm2_R_LT_Yh_AEZ" )
L125.LC_bm2_R <- readdata( "AGLU_LEVEL1_DATA", "L125.LC_bm2_R" )
L131.LV_USD75_m2_R_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L131.LV_USD75_m2_R_AEZ" )

# -----------------------------------------------------------------------------
# 2. Build tables
printlog( "L211.LN0_Logit: Logit exponent of the first land nest" )
L211.LN0_Logit <- data.frame(
      region = GCAM_region_names$region,
      LandAllocatorRoot = "root",
      logit.year.fillout = min( model_base_years ),
      logit.exponent = 0 )[ names_LN0_Logit ]

printlog( "L211.LN0_Land: Total regional land allocation" )
L211.LN0_Land <- add_region_name( L125.LC_bm2_R )
L211.LN0_Land$LandAllocatorRoot <- "root"
L211.LN0_Land$year.fillout <- min( model_base_years )
L211.LN0_Land$landAllocation <- L211.LN0_Land$LC_bm2
L211.LN0_Land <- L211.LN0_Land[ names_LN0_Land ]

printlog( "L211.LN0_SoilTimeScale: Soil time scale by region" )
#Note - the soil time scale should be set at the AEZ and not the (political) region. this will be changed at some point (in the model code)
A_soil_time_scale_R <- add_region_name( A_soil_time_scale_R )
L211.LN0_SoilTimeScale <- data.frame(
      region = GCAM_region_names$region,
      LandAllocatorRoot = "root" )
L211.LN0_SoilTimeScale$soilTimeScale <- A_soil_time_scale_R$soilTimeScale[ match( L211.LN0_SoilTimeScale$region, A_soil_time_scale_R$region ) ]
L211.LN0_SoilTimeScale <- L211.LN0_SoilTimeScale[ names_LN0_SoilTimeScale ]

printlog( "L211.LN1_ValueLogit: Unmanaged land value by region and AEZ, and logit exponent of second nest" )
L211.LV_USD75_m2_R_AEZ.melt <- melt( L131.LV_USD75_m2_R_AEZ, id.vars = R, variable.name = AEZ )
L211.LV_USD75_m2_R_AEZ.melt <- add_region_name( L211.LV_USD75_m2_R_AEZ.melt )
L211.LN1_ValueLogit <- data.frame(
      region = rep( GCAM_region_names$region, times = nrow( A_LandNode1 ) ),
      LandAllocatorRoot = "root",
      LandNode1 = sort( rep( A_LandNode1$LandNode1, times = length( GCAM_region_names$region ) ) ) )
L211.LN1_ValueLogit <- repeat_and_add_vector( L211.LN1_ValueLogit, AEZ, AEZs )
      
#Paste in values for unManagedLandValue using AEZ identifier, and set logit exponents using land type identifier
L211.LN1_ValueLogit$unManagedLandValue <- round( L211.LV_USD75_m2_R_AEZ.melt$value[
      match( vecpaste( L211.LN1_ValueLogit[ c( reg, AEZ ) ] ),
             vecpaste( L211.LV_USD75_m2_R_AEZ.melt[ c( reg, AEZ ) ] ) ) ] * conv_bm2_m2,
      digits_land_value )
L211.LN1_ValueLogit$logit.year.fillout <- min( model_base_years )
L211.LN1_ValueLogit$logit.exponent <- A_LandNode_logit$logit.exponent[
      match( L211.LN1_ValueLogit$LandNode1, A_LandNode_logit$LandNode ) ]
L211.LN1_ValueLogit$logit.exponent[ L211.LN1_ValueLogit$AEZ %in% AEZs_most_arid ] <- A_LandNode_logit$logit.exponent.arid[
      match( L211.LN1_ValueLogit$LandNode1[ L211.LN1_ValueLogit$AEZ %in% AEZs_most_arid ], A_LandNode_logit$LandNode ) ]

#Append AEZ to the LandNode name and only write out the columns specified in the header
L211.LN1_ValueLogit <- append_AEZ( L211.LN1_ValueLogit, "LandNode1" )
L211.LN1_ValueLogit <- L211.LN1_ValueLogit[ names_LN1_ValueLogit ]

#LAND USE HISTORY
L211.LC_bm2_R_Unmgd1_Yh_AEZ <- L125.LC_bm2_R_LT_Yh_AEZ[ L125.LC_bm2_R_LT_Yh_AEZ[[LT]] %in% A_LandLeaf_Unmgd1$UnmanagedLandLeaf, ]
L211.LC_bm2_R_Unmgd1_Yh_AEZ.melt <- melt( L211.LC_bm2_R_Unmgd1_Yh_AEZ, id.vars = R_LT_AEZ, variable.name = "Xyear" )
L211.LC_bm2_R_Unmgd1_Yh_AEZ.melt <- add_region_name( L211.LC_bm2_R_Unmgd1_Yh_AEZ.melt )
L211.LC_bm2_R_Unmgd1_Yh_AEZ.melt[[Y]] <- sub( "X", "", L211.LC_bm2_R_Unmgd1_Yh_AEZ.melt$Xyear )
L211.LC_bm2_R_Unmgd1_Yh_AEZ.melt$allocation <- L211.LC_bm2_R_Unmgd1_Yh_AEZ.melt$value
L211.LC_bm2_R_Unmgd1_Yh_AEZ.melt <- add_node_leaf_names( L211.LC_bm2_R_Unmgd1_Yh_AEZ.melt,
      nesting_table = A_LandLeaf_Unmgd1, leaf_name = "UnmanagedLandLeaf", LN1 = "LandNode1" )

printlog( "L211.LN1_HistUnmgdAllocation: Historical land cover, unmanaged land in the first nest" )
L211.LN1_HistUnmgdAllocation <- L211.LC_bm2_R_Unmgd1_Yh_AEZ.melt[ L211.LC_bm2_R_Unmgd1_Yh_AEZ.melt[[Y]] %in% land_history_years, names_LN1_HistUnmgdAllocation ]

printlog( "L211.LN1_UnmgdAllocation: Land cover in the model base periods, unmanaged land in the first nest" )
L211.LN1_UnmgdAllocation <- L211.LC_bm2_R_Unmgd1_Yh_AEZ.melt[ L211.LC_bm2_R_Unmgd1_Yh_AEZ.melt[[Y]] %in% model_base_years, names_LN1_UnmgdAllocation ]

#CARBON CONTENTS AND MATURE AGES
#Melt tables with compiled carbon contents and mature ages, and add region vectors
L211.VegC_kgm2_R_LT_AEZ.melt <- melt( L121.VegC_kgm2_R_LT_AEZ, id.vars = R_LT, variable.name = AEZ )
L211.SoilC_kgm2_R_LT_AEZ.melt <- melt( L121.SoilC_kgm2_R_LT_AEZ, id.vars = R_LT, variable.name = AEZ )
L211.MatureAge_R_LT_AEZ.melt <- melt( L121.MatureAge_R_LT_AEZ, id.vars = R_LT, variable.name = AEZ )

L211.VegC_kgm2_R_LT_AEZ.melt <- add_region_name( L211.VegC_kgm2_R_LT_AEZ.melt )
L211.SoilC_kgm2_R_LT_AEZ.melt <- add_region_name( L211.SoilC_kgm2_R_LT_AEZ.melt )
L211.MatureAge_R_LT_AEZ.melt <- add_region_name( L211.MatureAge_R_LT_AEZ.melt )

printlog( "L211.LN1_UnmgdCarbon: Carbon content info, unmanaged land in the first nest" )
L211.LN1_UnmgdCarbon <- data.frame(
      region = rep( GCAM_region_names$region, times = nrow( A_LandLeaf_Unmgd1 ) ),
      Land_Type = sort( rep( A_LandLeaf_Unmgd1$UnmanagedLandLeaf, times = nrow( GCAM_region_names ) ) ) )
L211.LN1_UnmgdCarbon$Cdensity_LT <- GCAMLandLeaf_CdensityLT[[LT]][
      match( L211.LN1_UnmgdCarbon[[LT]], GCAMLandLeaf_CdensityLT$LandLeaf) ]
L211.LN1_UnmgdCarbon <- repeat_and_add_vector( L211.LN1_UnmgdCarbon, AEZ, AEZs )
L211.LN1_UnmgdCarbon <- add_node_leaf_names( L211.LN1_UnmgdCarbon,
      nesting_table = A_LandLeaf_Unmgd1, leaf_name = "UnmanagedLandLeaf", LN1 = "LandNode1" )

#Match in carbon densities and mature age and sort the columns to match the model interface header
L211.LN1_UnmgdCarbon <- add_carbon_info( L211.LN1_UnmgdCarbon,
      veg_data = L211.VegC_kgm2_R_LT_AEZ.melt, soil_data = L211.SoilC_kgm2_R_LT_AEZ.melt, age_data = L211.MatureAge_R_LT_AEZ.melt )
L211.LN1_UnmgdCarbon <- L211.LN1_UnmgdCarbon[ names_LN1_UnmgdCarbon ]

printlog( "Removing non-existent AEZs from all tables" )
L211.LN0_Logit <- subset( L211.LN0_Logit, !region %in% no_aglu_regions )
L211.LN0_Land <- subset( L211.LN0_Land, !region %in% no_aglu_regions )
L211.LN0_SoilTimeScale <- subset( L211.LN0_SoilTimeScale, !region %in% no_aglu_regions )
L211.LN1_ValueLogit <- remove_AEZ_nonexist( L211.LN1_ValueLogit, AEZcol = "LandNode1" )
L211.LN1_HistUnmgdAllocation <- remove_AEZ_nonexist( L211.LN1_HistUnmgdAllocation, AEZcol = "LandNode1" )
L211.LN1_UnmgdAllocation <- remove_AEZ_nonexist( L211.LN1_UnmgdAllocation, AEZcol = "LandNode1" )
L211.LN1_UnmgdCarbon <- remove_AEZ_nonexist( L211.LN1_UnmgdCarbon, AEZcol = "LandNode1" )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L211.LN0_Logit, IDstring="LN0_Logit", domain="AGLU_LEVEL2_DATA", fn="L211.LN0_Logit",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_land_input_1.xml" )
write_mi_data( L211.LN0_Land, "LN0_Land", "AGLU_LEVEL2_DATA", "L211.LN0_Land", "AGLU_XML_BATCH", "batch_land_input_1.xml" )
write_mi_data( L211.LN0_SoilTimeScale, "LN0_SoilTimeScale", "AGLU_LEVEL2_DATA", "L211.LN0_SoilTimeScale", "AGLU_XML_BATCH", "batch_land_input_1.xml" )
write_mi_data( L211.LN1_ValueLogit, "LN1_ValueLogit", "AGLU_LEVEL2_DATA", "L211.LN1_ValueLogit", "AGLU_XML_BATCH", "batch_land_input_1.xml" )
write_mi_data( L211.LN1_HistUnmgdAllocation, "LN1_HistUnmgdAllocation", "AGLU_LEVEL2_DATA", "L211.LN1_HistUnmgdAllocation", "AGLU_XML_BATCH", "batch_land_input_1.xml" )
write_mi_data( L211.LN1_UnmgdAllocation, "LN1_UnmgdAllocation", "AGLU_LEVEL2_DATA", "L211.LN1_UnmgdAllocation", "AGLU_XML_BATCH", "batch_land_input_1.xml" )
write_mi_data( L211.LN1_UnmgdCarbon, "LN1_UnmgdCarbon", "AGLU_LEVEL2_DATA", "L211.LN1_UnmgdCarbon", "AGLU_XML_BATCH", "batch_land_input_1.xml", node_rename=T )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_land_input_1.xml", "AGLU_XML_FINAL", "land_input_1.xml", "", xml_tag="outFile"  )

logstop()
