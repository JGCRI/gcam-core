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
logstart( "L242.ssp34_pasture.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Adjusting pasture intensity in SSP3 and 4" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
GCAMLandLeaf_CdensityLT <- readdata( "AGLU_MAPPINGS", "GCAMLandLeaf_CdensityLT" )
A_LandLeaf_Unmgd2 <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf_Unmgd2" )
A_LandLeaf2 <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf2" )
L125.LC_bm2_R_LT_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L125.LC_bm2_R_LT_Yh_GLU" )
L102.pcgdp_thous90USD_SSP_R_Y <- readdata( "SOCIO_LEVEL1_DATA", "L102.pcgdp_thous90USD_SSP_R_Y" )

# -----------------------------------------------------------------------------
# 2. Build tables
fract_unmgd_to_mgd <- 0.25

#LAND ALLOCATION AND LAND USE HISTORY
#Subset the relevant land types from table with land cover by all land types
#Unmanaged land
L242.LC_bm2_R_Unmgd2_Yh_GLU <- subset( L125.LC_bm2_R_LT_Yh_GLU, Land_Type %in% "UnmanagedPasture" )
L242.LC_bm2_R_Unmgd2_Yh_GLU.melt <- melt( L242.LC_bm2_R_Unmgd2_Yh_GLU, id.vars = R_LT_GLU, variable.name = "Xyear" )
L242.LC_bm2_R_Unmgd2_Yh_GLU.melt <- add_region_name( L242.LC_bm2_R_Unmgd2_Yh_GLU.melt )
L242.LC_bm2_R_Unmgd2_Yh_GLU.melt[[Y]] <- sub( "X", "", L242.LC_bm2_R_Unmgd2_Yh_GLU.melt$Xyear )
# Adjust land cover downwards so more pasture is in the managed category
L242.LC_bm2_R_Unmgd2_Yh_GLU.melt$allocation <- L242.LC_bm2_R_Unmgd2_Yh_GLU.melt$value * ( 1 - fract_unmgd_to_mgd )
L242.LC_bm2_R_Unmgd2_Yh_GLU.melt <- add_node_leaf_names( L242.LC_bm2_R_Unmgd2_Yh_GLU.melt,
       nesting_table = A_LandLeaf_Unmgd2, leaf_name = "UnmanagedLandLeaf", LN1 = "LandNode1", LN2 = "LandNode2" )

printlog( "L242.LN2_HistUnmgdAllocation: Historical land cover, unmanaged land in the second nest" )
L242.LN2_HistUnmgdAllocation_ALL <- L242.LC_bm2_R_Unmgd2_Yh_GLU.melt[
      L242.LC_bm2_R_Unmgd2_Yh_GLU.melt[[Y]] %in% land_history_years, names_LN2_HistUnmgdAllocation ]

printlog( "L242.LN2_UnmgdAllocation: Model base period land cover, unmanaged land in the second nest" )
L242.LN2_UnmgdAllocation_ALL <- L242.LC_bm2_R_Unmgd2_Yh_GLU.melt[
      L242.LC_bm2_R_Unmgd2_Yh_GLU.melt[[Y]] %in% model_base_years, names_LN2_UnmgdAllocation ]

#Managed land
L242.LC_bm2_R_Mgd2_Yh_GLU <- subset( L125.LC_bm2_R_LT_Yh_GLU, Land_Type %in% "Pasture" )
L242.LC_bm2_R_Mgd2_Yh_GLU.melt <- melt( L242.LC_bm2_R_Mgd2_Yh_GLU, id.vars = R_LT_GLU, variable.name = "Xyear" )
L242.LC_bm2_R_Mgd2_Yh_GLU.melt <- add_region_name( L242.LC_bm2_R_Mgd2_Yh_GLU.melt )
L242.LC_bm2_R_Mgd2_Yh_GLU.melt[[Y]] <- sub( "X", "", L242.LC_bm2_R_Mgd2_Yh_GLU.melt$Xyear )
L242.LC_bm2_R_Mgd2_Yh_GLU.melt$allocation <- L242.LC_bm2_R_Mgd2_Yh_GLU.melt$value + fract_unmgd_to_mgd  * 
  L242.LC_bm2_R_Unmgd2_Yh_GLU.melt$value[ match( vecpaste( L242.LC_bm2_R_Mgd2_Yh_GLU.melt[ R_Y_GLU ] ), 
                                                 vecpaste( L242.LC_bm2_R_Unmgd2_Yh_GLU.melt[ R_Y_GLU ] ) )]
L242.LC_bm2_R_Mgd2_Yh_GLU.melt <- add_node_leaf_names( L242.LC_bm2_R_Mgd2_Yh_GLU.melt,
                                                       nesting_table = A_LandLeaf2, leaf_name = "LandLeaf", LN1 = "LandNode1", LN2 = "LandNode2" )

printlog( "L242.LN2_HistMgdAllocation: Historical land cover, managed land in the second nest" )
L242.LN2_HistMgdAllocation_ALL <- L242.LC_bm2_R_Mgd2_Yh_GLU.melt[ L242.LC_bm2_R_Mgd2_Yh_GLU.melt[[Y]] %in% land_history_years, names_LN2_HistMgdAllocation ]

printlog( "L242.LN2_MgdAllocation: Model base period land cover, managed land in the second nest" )
L242.LN2_MgdAllocation_ALL <- L242.LC_bm2_R_Mgd2_Yh_GLU.melt[ L242.LC_bm2_R_Mgd2_Yh_GLU.melt[[Y]] %in% model_base_years, names_LN2_MgdAllocation ]

# Create SSP4 pasture inputs
L242.pcgdp_2010 <- subset( L102.pcgdp_thous90USD_SSP_R_Y, L102.pcgdp_thous90USD_SSP_R_Y$scenario == "SSP4" )
L242.pcgdp_2010 <- L242.pcgdp_2010[ names( L242.pcgdp_2010) %in% c( "GCAM_region_ID", "X2010" ) ]
L242.pcgdp_2010 <- add_region_name( L242.pcgdp_2010 )
L242.pcgdp_2010$X2010 <- L242.pcgdp_2010$X2010 * conv_1990_2010_USD
L242.low_reg <- L242.pcgdp_2010$region[ L242.pcgdp_2010$X2010 < lo_growth_pcgdp ]
L242.LN2_HistUnmgdAllocation_SSP34 <- subset( L242.LN2_HistUnmgdAllocation_ALL, region %in% L242.low_reg )
L242.LN2_UnmgdAllocation_SSP34 <- subset( L242.LN2_UnmgdAllocation_ALL, region %in% L242.low_reg )
L242.LN2_HistMgdAllocation_SSP34 <- subset( L242.LN2_HistMgdAllocation_ALL, region %in% L242.low_reg )
L242.LN2_MgdAllocation_SSP34 <- subset( L242.LN2_MgdAllocation_ALL, region %in% L242.low_reg )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file
write_mi_data( L242.LN2_HistUnmgdAllocation_SSP34, IDstring="LN2_HistUnmgdAllocation", domain="AGLU_LEVEL2_DATA", fn="L242.LN2_HistUnmgdAllocation_SSP34",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_pasture_ssp34.xml" )
write_mi_data( L242.LN2_UnmgdAllocation_SSP34, "LN2_UnmgdAllocation", "AGLU_LEVEL2_DATA", "L242.LN2_UnmgdAllocation_SSP34","AGLU_XML_BATCH", "batch_pasture_ssp34.xml" )
write_mi_data( L242.LN2_HistMgdAllocation_SSP34, "LN2_HistMgdAllocation", "AGLU_LEVEL2_DATA", "L242.LN2_HistMgdAllocation_SSP34", "AGLU_XML_BATCH", "batch_pasture_ssp34.xml" )
write_mi_data( L242.LN2_MgdAllocation_SSP34, "LN2_MgdAllocation", "AGLU_LEVEL2_DATA", "L242.LN2_MgdAllocation_SSP34", "AGLU_XML_BATCH", "batch_pasture_ssp34.xml", node_rename=T )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_pasture_ssp34.xml", "AGLU_XML_FINAL", "pasture_ssp34.xml", "", xml_tag="outFile" )

logstop()
