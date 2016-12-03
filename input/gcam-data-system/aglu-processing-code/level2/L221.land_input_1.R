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
logstart( "L221.land_input_1.R" )
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
basin_to_country_mapping <- readdata( "WATER_MAPPINGS", "basin_to_country_mapping" )
GCAMLandLeaf_CdensityLT <- readdata( "AGLU_MAPPINGS", "GCAMLandLeaf_CdensityLT" )
A_LandNode_logit <- readdata( "AGLU_ASSUMPTIONS", "A_LandNode_logit" )
A_LandLeaf_Unmgd1 <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf_Unmgd1" )
A_LT_Mapping <- readdata( "AGLU_ASSUMPTIONS", "A_LT_Mapping" )
A_soil_time_scale_R <- readdata( "AGLU_ASSUMPTIONS", "A_soil_time_scale_R" )
L121.CarbonContent_kgm2_R_LT_GLU <- readdata( "AGLU_LEVEL1_DATA", "L121.CarbonContent_kgm2_R_LT_GLU", replace_GLU = T )
L125.LC_bm2_R_LT_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L125.LC_bm2_R_LT_Yh_GLU", replace_GLU = T )
L125.LC_bm2_R <- readdata( "AGLU_LEVEL1_DATA", "L125.LC_bm2_R" )
L131.LV_USD75_m2_R_GLU <- readdata( "AGLU_LEVEL1_DATA", "L131.LV_USD75_m2_R_GLU", replace_GLU = T )

# -----------------------------------------------------------------------------
# 2. Build tables
# Create template tables first, with available combinations of regions, GLUs, and land use types
L125.LC_bm2_R_LT_Yh_GLU$LandNode1 <- A_LT_Mapping$LandNode1[
  match( L125.LC_bm2_R_LT_Yh_GLU[[LT]], A_LT_Mapping[[LT]] ) ]
L221.LN1 <- na.omit( unique( L125.LC_bm2_R_LT_Yh_GLU[ c( R, GLU, "LandNode1" ) ] ) )

L125.LC_bm2_R <- add_region_name( L125.LC_bm2_R )

printlog( "L221.LN0_Logit: Logit exponent of the top-level (zero) land nest" )
L221.LN0_Logit <- data.frame(
      region = GCAM_region_names$region[ match( L125.LC_bm2_R[[R]], GCAM_region_names[[R]] ) ],
      LandAllocatorRoot = "root",
      logit.year.fillout = min( model_base_years ),
      logit.exponent = 0,
      logit.type = NA )

L221.LN0_LogitTables <- get_logit_fn_tables( L221.LN0_Logit, names_LN0_LogitType,
    base.header="LN0_Logit_", include.equiv.table=T, write.all.regions=F )

L221.LN0_Logit <- L221.LN0_Logit[, names_LN0_Logit ]

printlog( "L221.LN0_Land: Total regional land allocation" )
L221.LN0_Land <- L125.LC_bm2_R
L221.LN0_Land$LandAllocatorRoot <- "root"
L221.LN0_Land$year.fillout <- min( model_base_years )
L221.LN0_Land$landAllocation <- L221.LN0_Land$LC_bm2
L221.LN0_Land <- L221.LN0_Land[ names_LN0_Land ]

printlog( "L221.LN0_SoilTimeScale: Soil time scale by region" )
#Note - the soil time scale should be set at the AEZ and not the (political) region. this will be changed at some point (in the model code)
A_soil_time_scale_R <- add_region_name( A_soil_time_scale_R )
L221.LN0_SoilTimeScale <- data.frame(
      region = L125.LC_bm2_R$region,
      LandAllocatorRoot = "root" )
L221.LN0_SoilTimeScale$soilTimeScale <- A_soil_time_scale_R$soilTimeScale[ match( L221.LN0_SoilTimeScale$region, A_soil_time_scale_R$region ) ]
L221.LN0_SoilTimeScale <- L221.LN0_SoilTimeScale[ names_LN0_SoilTimeScale ]

printlog( "L221.LN1_ValueLogit: Unmanaged land value by region and AEZ, and logit exponent of first nest" )
# note that the model unit is 1975$ per thousand km2
L131.LV_USD75_m2_R_GLU$LV_USD75_bm2 <- round( L131.LV_USD75_m2_R_GLU$LV_USD75_m2 * conv_bm2_m2, digits_land_value )
# Note: setting a minimum threshold on the land values to ensure that no land use regions get a value of zero
min_LV_USD75_bm2 <- min( L131.LV_USD75_m2_R_GLU$LV_USD75_bm2[ L131.LV_USD75_m2_R_GLU$LV_USD75_bm2 > 0 ] )
L221.LN1_ValueLogit <- add_region_name( L221.LN1 )
L221.LN1_ValueLogit$LandAllocatorRoot <- "root"
L221.LN1_ValueLogit$logit.year.fillout <- min( model_base_years )
L221.LN1_ValueLogit$logit.exponent <- A_LandNode_logit$logit.exponent[
  match( L221.LN1_ValueLogit$LandNode1, A_LandNode_logit$LandNode ) ]
L221.LN1_ValueLogit$logit.type <- A_LandNode_logit$logit.type[
  match( L221.LN1_ValueLogit$LandNode1, A_LandNode_logit$LandNode ) ]
L221.LN1_ValueLogit$unManagedLandValue <- L131.LV_USD75_m2_R_GLU$LV_USD75_bm2[
    match( vecpaste( L221.LN1_ValueLogit[ R_GLU ] ),
           vecpaste( L131.LV_USD75_m2_R_GLU[ R_GLU ] ) ) ]

# replace zeroes and missing values with the minimum assumed land value
L221.LN1_ValueLogit$unManagedLandValue[ is.na( L221.LN1_ValueLogit$unManagedLandValue ) |
                                          L221.LN1_ValueLogit$unManagedLandValue == 0 ] <- min_LV_USD75_bm2

#Append GLU to the LandNode name and only write out the columns specified in the header.
# Also sort by region then GLU
L221.LN1_ValueLogit <- append_GLU( L221.LN1_ValueLogit, "LandNode1" )
L221.LN1_ValueLogit <- L221.LN1_ValueLogit[ order( L221.LN1_ValueLogit[[R]], L221.LN1_ValueLogit[[GLU]] ), ]

L221.LN1_LogitTables <- get_logit_fn_tables( L221.LN1_ValueLogit, names_LN1_LogitType,
    base.header="LN1_Logit_", include.equiv.table=F, write.all.regions=F )
L221.LN1_ValueLogit <- L221.LN1_ValueLogit[ names_LN1_ValueLogit ]

#LAND USE HISTORY
L221.LC_bm2_R_Unmgd1_Yh_GLU <- L125.LC_bm2_R_LT_Yh_GLU[
  L125.LC_bm2_R_LT_Yh_GLU[[LT]] %in% A_LandLeaf_Unmgd1$UnmanagedLandLeaf, ]
L221.LC_bm2_R_Unmgd1_Yh_GLU.melt <- interpolate_and_melt( L221.LC_bm2_R_Unmgd1_Yh_GLU,
                                                          unique( c( land_history_years, model_base_years ) ),
                                                          value.name = "allocation", digits = digits_land_use )
L221.LC_bm2_R_Unmgd1_Yh_GLU.melt <- add_region_name( L221.LC_bm2_R_Unmgd1_Yh_GLU.melt )
L221.LC_bm2_R_Unmgd1_Yh_GLU.melt <- add_node_leaf_names( L221.LC_bm2_R_Unmgd1_Yh_GLU.melt,
      nesting_table = A_LandLeaf_Unmgd1, leaf_name = "UnmanagedLandLeaf", LN1 = "LandNode1" )

printlog( "L221.LN1_HistUnmgdAllocation: Historical land cover, unmanaged land in the first nest" )
L221.LN1_HistUnmgdAllocation <- L221.LC_bm2_R_Unmgd1_Yh_GLU.melt[ L221.LC_bm2_R_Unmgd1_Yh_GLU.melt[[Y]] %in% land_history_years, names_LN1_HistUnmgdAllocation ]

printlog( "L221.LN1_UnmgdAllocation: Land cover in the model base periods, unmanaged land in the first nest" )
L221.LN1_UnmgdAllocation <- L221.LC_bm2_R_Unmgd1_Yh_GLU.melt[ L221.LC_bm2_R_Unmgd1_Yh_GLU.melt[[Y]] %in% model_base_years, names_LN1_UnmgdAllocation ]

#CARBON CONTENTS AND MATURE AGES
printlog( "L221.LN1_UnmgdCarbon: Carbon content info, unmanaged land in the first nest" )
L221.LN1_UnmgdCarbon <- subset( L221.LC_bm2_R_Unmgd1_Yh_GLU.melt, year == max( model_base_years ) )[
  !names( L221.LC_bm2_R_Unmgd1_Yh_GLU.melt ) %in% c( "variable", "allocation", "year" ) ]
L221.LN1_UnmgdCarbon$Cdensity_LT <- GCAMLandLeaf_CdensityLT[[LT]][
  match( L221.LN1_UnmgdCarbon[[LT]], GCAMLandLeaf_CdensityLT$LandLeaf) ]
L221.LN1_UnmgdCarbon <- add_carbon_info( L221.LN1_UnmgdCarbon, carbon_info_table = L121.CarbonContent_kgm2_R_LT_GLU )
L221.LN1_UnmgdCarbon <- L221.LN1_UnmgdCarbon[ names_LN1_UnmgdCarbon ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

for( curr_table in L221.LN0_LogitTables ) {
write_mi_data( curr_table$data, curr_table$header, "AGLU_LEVEL2_DATA", paste0("L221.", curr_table$header ), "AGLU_XML_BATCH", "batch_land_input_1.xml" )
}
write_mi_data( L221.LN0_Logit, IDstring="LN0_Logit", domain="AGLU_LEVEL2_DATA", fn="L221.LN0_Logit",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_land_input_1.xml" )
write_mi_data( L221.LN0_Land, "LN0_Land", "AGLU_LEVEL2_DATA", "L221.LN0_Land", "AGLU_XML_BATCH", "batch_land_input_1.xml" )
write_mi_data( L221.LN0_SoilTimeScale, "LN0_SoilTimeScale", "AGLU_LEVEL2_DATA", "L221.LN0_SoilTimeScale", "AGLU_XML_BATCH", "batch_land_input_1.xml" )
for( curr_table in L221.LN1_LogitTables ) {
write_mi_data( curr_table$data, curr_table$header, "AGLU_LEVEL2_DATA", paste0("L221.", curr_table$header ), "AGLU_XML_BATCH", "batch_land_input_1.xml" )
}
write_mi_data( L221.LN1_ValueLogit, "LN1_ValueLogit", "AGLU_LEVEL2_DATA", "L221.LN1_ValueLogit", "AGLU_XML_BATCH", "batch_land_input_1.xml" )
write_mi_data( L221.LN1_HistUnmgdAllocation, "LN1_HistUnmgdAllocation", "AGLU_LEVEL2_DATA", "L221.LN1_HistUnmgdAllocation", "AGLU_XML_BATCH", "batch_land_input_1.xml" )
write_mi_data( L221.LN1_UnmgdAllocation, "LN1_UnmgdAllocation", "AGLU_LEVEL2_DATA", "L221.LN1_UnmgdAllocation", "AGLU_XML_BATCH", "batch_land_input_1.xml" )
write_mi_data( L221.LN1_UnmgdCarbon, "LN1_UnmgdCarbon", "AGLU_LEVEL2_DATA", "L221.LN1_UnmgdCarbon", "AGLU_XML_BATCH", "batch_land_input_1.xml", node_rename=T )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_land_input_1.xml", "AGLU_XML_FINAL", "land_input_1.xml", "", xml_tag="outFile"  )

logstop()
