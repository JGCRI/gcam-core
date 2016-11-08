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
logstart( "L223.land_input_3.R" )
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
A_bio_ghost_share <- readdata( "AGLU_ASSUMPTIONS", "A_bio_ghost_share" )
A_Fodderbio_chars <- readdata( "AGLU_ASSUMPTIONS", "A_Fodderbio_chars" )
A_LT_Mapping <- readdata( "AGLU_ASSUMPTIONS", "A_LT_Mapping" )
A_LandNode_logit <- readdata( "AGLU_ASSUMPTIONS", "A_LandNode_logit" )
A_LandLeaf_Unmgd3 <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf_Unmgd3" )
A_LandLeaf3 <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf3" )
L111.ag_resbio_R_C <- readdata( "AGLU_LEVEL1_DATA", "L111.ag_resbio_R_C" )
L121.CarbonContent_kgm2_R_LT_GLU <- readdata( "AGLU_LEVEL1_DATA", "L121.CarbonContent_kgm2_R_LT_GLU" )
L122.ag_EcYield_kgm2_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L122.ag_EcYield_kgm2_R_C_Y_GLU" )
L122.LC_bm2_R_HarvCropLand_C_Yh_GLU <- readdata ( "AGLU_LEVEL1_DATA", "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU" )
L123.For_Yield_m3m2_R_GLU <- readdata( "AGLU_LEVEL1_DATA", "L123.For_Yield_m3m2_R_GLU" )
L125.LC_bm2_R_LT_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L125.LC_bm2_R_LT_Yh_GLU" )
L132.ag_an_For_Prices <- readdata( "AGLU_LEVEL1_DATA", "L132.ag_an_For_Prices" )
L133.ag_Cost_75USDkg_C <- readdata( "AGLU_LEVEL1_DATA", "L133.ag_Cost_75USDkg_C" )
L201.AgYield_bio_grass <- readdata( "AGLU_LEVEL2_DATA", "L201.AgYield_bio_grass", skip = 4 )
L201.AgYield_bio_tree <- readdata( "AGLU_LEVEL2_DATA", "L201.AgYield_bio_tree", skip = 4 )

# -----------------------------------------------------------------------------
# 2. Build tables
# Determine which node combinations are applicable at this level
L125.LC_bm2_R_LT_Yh_GLU[ c( "LandNode1", "LandNode2", "LandNode3" ) ] <- A_LT_Mapping[
  match( L125.LC_bm2_R_LT_Yh_GLU[[LT]], A_LT_Mapping[[LT]] ),
  c( "LandNode1", "LandNode2", "LandNode3" ) ]
L223.LN3 <- na.omit( unique( L125.LC_bm2_R_LT_Yh_GLU[ c( R, GLU, "LandNode1", "LandNode2", "LandNode3" ) ] ) )

#Biomass leaves and nodes - need to match the ones created in L201
L223.LN3_leaf_bio <- unique( rbind( L201.AgYield_bio_grass, L201.AgYield_bio_tree )[ c( reg, agtech ) ] )
L223.LN3_leaf_bio <- remove_GLU( L223.LN3_leaf_bio, var1 = agtech )
L223.LN3_leaf_bio[ c( "LandNode1", "LandNode2", "LandNode3" ) ] <- A_LT_Mapping[
  match( L223.LN3_leaf_bio[[agtech]], A_LT_Mapping[[LT]] ),
  c( "LandNode1", "LandNode2", "LandNode3" ) ]
L223.LN3_leaf_bio <- append_GLU( L223.LN3_leaf_bio, var1 = "LandNode1", var2 = "LandNode2", var3 = "LandNode3" ,var4 = agtech )
L223.LN3_leaf_bio$LandLeaf <- L223.LN3_leaf_bio[[agtech]]

printlog( "L223.LN3_Logit: Logit exponent of the third nest" )
L223.LN3_Logit <- add_region_name( L223.LN3 )
L223.LN3_Logit$LandAllocatorRoot <- "root"

#Match in logit exponents based on the land node 2
L223.LN3_Logit$logit.year.fillout <- min( model_base_years )
L223.LN3_Logit$logit.exponent <- A_LandNode_logit$logit.exponent[ match( L223.LN3_Logit$LandNode3, A_LandNode_logit$LandNode ) ]
L223.LN3_Logit$logit.type <- A_LandNode_logit$logit.type[ match( L223.LN3_Logit$LandNode3, A_LandNode_logit$LandNode ) ]

#Append GLU names and keep only relevant columns
L223.LN3_Logit <- append_GLU( L223.LN3_Logit, var1 = "LandNode1", var2 = "LandNode2", var3 = "LandNode3" )

L223.LN3_LogitTables <- get_logit_fn_tables( L223.LN3_Logit, names_LN3_LogitType,
    base.header="LN3_Logit_", include.equiv.table=T, write.all.regions=F )
L223.LN3_Logit <- L223.LN3_Logit[ names_LN3_Logit ]

printlog( "L223.LN3_LeafGhostShare: Default shares for new technologies in specified years" )
#Default shares do not interpolate in the model, so write it out in all model future years (starting with first bio year)
L223.LN3_LeafGhostShare <- L223.LN3_leaf_bio
L223.LN3_LeafGhostShare$LandAllocatorRoot <- "root"
L223.LN3_LeafGhostShare <- repeat_and_add_vector( L223.LN3_LeafGhostShare, Y, model_future_years[ model_future_years >= Bio_start_year ] )
L223.LN3_LeafGhostShare$ghost.unnormalized.share <- approx(
  x = A_bio_ghost_share$year,
  y = A_bio_ghost_share$ghost.share,
  xout = L223.LN3_LeafGhostShare$year, rule = 2 )$y
L223.LN3_LeafGhostShare <- L223.LN3_LeafGhostShare[ names_LN3_LeafGhostShare ]

#LAND USE HISTORY
#Unmanaged land
L223.LC_bm2_R_Unmgd3_Yh_GLU <- L125.LC_bm2_R_LT_Yh_GLU[ L125.LC_bm2_R_LT_Yh_GLU[[LT]] %in% A_LandLeaf_Unmgd3$UnmanagedLandLeaf, ]
L223.LC_bm2_R_Unmgd3_Yh_GLU.melt <- interpolate_and_melt( L223.LC_bm2_R_Unmgd3_Yh_GLU,
                                                          unique( c( land_history_years, model_base_years ) ),
                                                          value.name = "allocation", digits = digits_land_use )
L223.LC_bm2_R_Unmgd3_Yh_GLU.melt <- add_region_name( L223.LC_bm2_R_Unmgd3_Yh_GLU.melt )
L223.LC_bm2_R_Unmgd3_Yh_GLU.melt <- add_node_leaf_names( L223.LC_bm2_R_Unmgd3_Yh_GLU.melt,
                                                         nesting_table = A_LandLeaf_Unmgd3, leaf_name = "UnmanagedLandLeaf",
                                                         LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3" )

printlog( "L223.LN3_HistUnmgdAllocation: Historical land cover, unmanaged land in the third nest" )
L223.LN3_HistUnmgdAllocation <- L223.LC_bm2_R_Unmgd3_Yh_GLU.melt[
  L223.LC_bm2_R_Unmgd3_Yh_GLU.melt[[Y]] %in% land_history_years, names_LN3_HistUnmgdAllocation ]

printlog( "L223.LN3_UnmgdAllocation: Model base period land cover, unmanaged land in the third nest" )
L223.LN3_UnmgdAllocation <- L223.LC_bm2_R_Unmgd3_Yh_GLU.melt[
  L223.LC_bm2_R_Unmgd3_Yh_GLU.melt[[Y]] %in% model_base_years, names_LN3_UnmgdAllocation ]

#Managed land - non-crop (forest)
L223.LC_bm2_R_Mgd3_Yh_GLU <- L125.LC_bm2_R_LT_Yh_GLU[ L125.LC_bm2_R_LT_Yh_GLU[[LT]] %in% A_LandLeaf3$LandLeaf, ]
L223.LC_bm2_R_Mgd3_Yh_GLU.melt <- interpolate_and_melt( L223.LC_bm2_R_Mgd3_Yh_GLU,
                                                          unique( c( land_history_years, model_base_years ) ),
                                                          value.name = "allocation", digits = digits_land_use )
L223.LC_bm2_R_Mgd3_Yh_GLU.melt <- add_region_name( L223.LC_bm2_R_Mgd3_Yh_GLU.melt )
L223.LC_bm2_R_Mgd3_Yh_GLU.melt <- add_node_leaf_names( L223.LC_bm2_R_Mgd3_Yh_GLU.melt,
                                                         nesting_table = A_LandLeaf3, leaf_name = "LandLeaf",
                                                         LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3" )

printlog( "L223.LN3_HistMgdAllocation_noncrop: Historical land cover, managed land in the third nest (noncrop)" )
L223.LN3_HistMgdAllocation_noncrop <- L223.LC_bm2_R_Mgd3_Yh_GLU.melt[
  L223.LC_bm2_R_Mgd3_Yh_GLU.melt[[Y]] %in% land_history_years, names_LN3_HistMgdAllocation ]

printlog( "L223.LN3_MgdAllocation_noncrop: Model base period land cover, managed land in the third nest (noncrop)" )
L223.LN3_MgdAllocation_noncrop <- L223.LC_bm2_R_Mgd3_Yh_GLU.melt[
  L223.LC_bm2_R_Mgd3_Yh_GLU.melt[[Y]] %in% model_base_years, names_LN3_MgdAllocation ]

#Managed land - crop land
L223.LC_bm2_R_HarvCropLand_C_Yh_GLU <- L122.LC_bm2_R_HarvCropLand_C_Yh_GLU[
  rowSums( L122.LC_bm2_R_HarvCropLand_C_Yh_GLU[ X_historical_years ] ) != 0, ]
L223.LC_bm2_R_HarvCropLand_C_Yh_GLU.melt <- interpolate_and_melt( L223.LC_bm2_R_HarvCropLand_C_Yh_GLU,
                                                                  unique( c( land_history_years, model_base_years ) ),
                                                                  value.name = "allocation", digits = digits_land_use )
L223.LC_bm2_R_HarvCropLand_C_Yh_GLU.melt <- add_region_name( L223.LC_bm2_R_HarvCropLand_C_Yh_GLU.melt )
L223.LC_bm2_R_HarvCropLand_C_Yh_GLU.melt <- add_node_leaf_names( L223.LC_bm2_R_HarvCropLand_C_Yh_GLU.melt,
                                                                 nesting_table = A_LandLeaf3, leaf_name = "LandLeaf",
                                                                 LT_name = C, LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3" )

printlog( "L223.LN3_HistMgdAllocation_crop: Historical land cover, managed land in the third nest, cropland" )
L223.LN3_HistMgdAllocation_crop <- L223.LC_bm2_R_HarvCropLand_C_Yh_GLU.melt[
  L223.LC_bm2_R_HarvCropLand_C_Yh_GLU.melt[[Y]] %in% land_history_years, names_LN3_HistMgdAllocation ]

printlog( "L223.LN3_MgdAllocation_crop: Model base year land cover, managed land in the third nest, cropland" )
L223.LN3_MgdAllocation_crop <- L223.LC_bm2_R_HarvCropLand_C_Yh_GLU.melt[
  L223.LC_bm2_R_HarvCropLand_C_Yh_GLU.melt[[Y]] %in% model_base_years, names_LN3_MgdAllocation ]

#Managed land - biomass
L223.LC_bm2_R_bio_Yh_AEZ.melt <- L223.LN3_leaf_bio
L223.LC_bm2_R_bio_Yh_AEZ.melt$LandAllocatorRoot <- "root"

printlog( "L223.LN3_HistMgdAllocation_bio: Historical land cover, managed land in the third nest, bioenergy" )
L223.LN3_HistMgdAllocation_bio <- repeat_and_add_vector( L223.LC_bm2_R_bio_Yh_AEZ.melt, Y, land_history_years )
L223.LN3_HistMgdAllocation_bio$allocation <- 0
L223.LN3_HistMgdAllocation_bio <- L223.LN3_HistMgdAllocation_bio[ names_LN3_HistMgdAllocation ]

printlog( "L223.LN3_MgdAllocation_bio: Model base year land cover, managed land in the third nest, bioenergy" )
L223.LN3_MgdAllocation_bio <- repeat_and_add_vector( L223.LC_bm2_R_bio_Yh_AEZ.melt, Y, model_base_years )
L223.LN3_MgdAllocation_bio$allocation <- 0
L223.LN3_MgdAllocation_bio <- L223.LN3_MgdAllocation_bio[ names_LN3_MgdAllocation ]

#CARBON CONTENTS AND MATURE AGES
#Add region vector to carbon info table
L223.CarbonContent_kgm2_R_LT_GLU <- add_region_name( L121.CarbonContent_kgm2_R_LT_GLU )

printlog( "L223.LN3_UnmgdCarbon: Carbon content info, unmanaged land in the third nest" )
L223.LN3_UnmgdCarbon <- subset( L223.LC_bm2_R_Unmgd3_Yh_GLU.melt, year == max( model_base_years ) )
L223.LN3_UnmgdCarbon$Cdensity_LT <- GCAMLandLeaf_CdensityLT[[LT]][
  match( L223.LN3_UnmgdCarbon[[LT]], GCAMLandLeaf_CdensityLT$LandLeaf) ]
L223.LN3_UnmgdCarbon <- add_carbon_info( L223.LN3_UnmgdCarbon, carbon_info_table = L121.CarbonContent_kgm2_R_LT_GLU )

#If any regions are zero in all periods, they will return missing values here. The specific value doesn't matter because
# these land use types will be zero in all periods, but there is an assert in the code prohibiting mature age from being zero
L223.LN3_UnmgdCarbon[ is.na( L223.LN3_UnmgdCarbon ) ] <- 1
L223.LN3_UnmgdCarbon <- L223.LN3_UnmgdCarbon[ names_LN3_UnmgdCarbon ]

#Managed carbon in the third next. Ag and bio values will be over-written later
printlog( "L223.LN3_MgdCarbon_noncrop: Carbon content info, managed land in the third nest, non-crop" )
L223.LN3_MgdCarbon_noncrop <- subset( L223.LC_bm2_R_Mgd3_Yh_GLU.melt, year == max( model_base_years ) )
L223.LN3_MgdCarbon_noncrop$Cdensity_LT <- GCAMLandLeaf_CdensityLT[[LT]][
  match( L223.LN3_MgdCarbon_noncrop[[LT]], GCAMLandLeaf_CdensityLT$LandLeaf) ]
L223.LN3_MgdCarbon_noncrop <- add_carbon_info( L223.LN3_MgdCarbon_noncrop, carbon_info_table = L121.CarbonContent_kgm2_R_LT_GLU )
L223.LN3_MgdCarbon_noncrop <- L223.LN3_MgdCarbon_noncrop[ names_LN3_MgdCarbon ]

# Set Forests to use node-carbon-calc at the node level and no-emiss-carbon-calc at the leaf
# to allow them to switch between each other without lots of emissions+uptake
printlog( "L223.LN3_NoEmissCarbon: Set the no-emiss-carbon-calc as the type of carbon to use in forest leaves" )
L223.LN3_NoEmissCarbon <- subset( L223.LN3_UnmgdCarbon, grepl( "Forest", UnmanagedLandLeaf ),
    select=c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "UnmanagedLandLeaf" ) )
names( L223.LN3_NoEmissCarbon ) <- sub( 'Unmanaged', '', names( L223.LN3_NoEmissCarbon ) )
L223.LN3_NoEmissCarbon <- rbind( L223.LN3_NoEmissCarbon, subset( L223.LN3_MgdCarbon_noncrop,
    select=c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf" ) ) )
# Just need to create an empty tag, to do so we have to have a whitespace column and an
# extra column with extraneous data
L223.LN3_NoEmissCarbon$no.emiss.carbon.calc <- " "
L223.LN3_NoEmissCarbon$extra <- "junk"

# Put the node carbon calc in the node just above the leaves
printlog( "L223.LN3_NodeCarbon: Set the node-carbon-calc to drive the carbon calc between forest leaves" )
L223.LN3_NodeCarbon <- L223.LN3_NoEmissCarbon
L223.LN3_NodeCarbon$LandLeaf <- NULL
L223.LN3_NodeCarbon <- unique( L223.LN3_NodeCarbon )
names( L223.LN3_NodeCarbon ) <- sub( 'no.emiss', 'node', names( L223.LN3_NodeCarbon ) )

# Create node equivalence lists so that we don't have to subset and create a bunch of extra
# tables for reading in the *CarbonCalc data
# TODO: better place for these?  they are related to headers since they list tag names
printlog( "L223.NodeEquiv: Node tag equivalence list to minimize extra tables to read in same params" )
L223.NodeEquiv <- data.frame( group.name=c("Leaf"), tag1=c("LandLeaf"), tag2=c("UnmanagedLandLeaf"), stringsAsFactors=FALSE )
L223.NodeEquiv <- rbind( L223.NodeEquiv, c( "CarbonCalc", "land-carbon-densities", "no-emiss-carbon-calc") )

#CROPLAND VEGETATION CARBON DENSITY CALCULATION
printlog( "L223.LN3_MgdCarbon_crop: Carbon content info, managed land in the third nest, cropland" )
printlog( "Soil will use default values, but vegetation will be replaced by bottom-up estimates" )
L223.LN3_MgdCarbon_crop <- L223.LC_bm2_R_HarvCropLand_C_Yh_GLU.melt[ L223.LC_bm2_R_HarvCropLand_C_Yh_GLU.melt[[Y]] == max( model_base_years ),
                                                        names( L223.LN3_MgdAllocation_crop ) %!in% c( Y, "allocation" ) ]
L223.LN3_MgdCarbon_crop <- add_region_name( L223.LN3_MgdCarbon_crop )
L223.LN3_MgdCarbon_crop$LandAllocatorRoot <- "root"
L223.LN3_MgdCarbon_crop$Cdensity_LT <- GCAMLandLeaf_CdensityLT[[LT]][
  match( L223.LN3_MgdCarbon_crop[[C]], GCAMLandLeaf_CdensityLT$LandLeaf) ]
L223.LN3_MgdCarbon_crop <- add_carbon_info( L223.LN3_MgdCarbon_crop, carbon_info_table = L121.CarbonContent_kgm2_R_LT_GLU )

printlog( "Replacing missing values in places with harvested area and production but no assigned cropland" )
# If regions have harvested area and production but no cropland assigned in Hyde, we take "unmanaged" land and re-assign it to cropland.
# however this re-assignment is done after computing carbon contents, so such regions will have missing values at this point.
# because at this point cropland carbon contents are not derived from underlying vegetation, all are equal and we can just inherit the
# values from any other region. No need to do this w veg at this point b/c it will be replaced later
L223.LN3_MgdCarbon_crop[ is.na( L223.LN3_MgdCarbon_crop$soil.carbon.density ), c( "hist.soil.carbon.density", "soil.carbon.density" ) ] <-
  mean( L223.LN3_MgdCarbon_crop$soil.carbon.density, na.rm = T )
L223.LN3_MgdCarbon_crop$mature.age[ is.na( L223.LN3_MgdCarbon_crop$mature.age ) ] <-
  mean( L223.LN3_MgdCarbon_crop$mature.age, na.rm = T )

#Add region ID vector to residue biomass table
L223.ag_resbio_R_C <- add_region_name( L111.ag_resbio_R_C )

#Melt yield table in final calibration year, and paste in region ID vector
L223.ag_EcYield_kgm2_R_C_fby_GLU<- add_region_name( L122.ag_EcYield_kgm2_R_C_Y_GLU[ c( R_C_GLU, X_final_historical_year ) ] )

#Map in information for calculation of cropland vegetation carbon
crop_chars <- c( "HarvestIndex", "WaterContent", "Root_Shoot" )
L223.LN3_MgdCarbon_crop [ crop_chars ] <- L223.ag_resbio_R_C[
      match( vecpaste( L223.LN3_MgdCarbon_crop[ R_C ] ),
             vecpaste( L223.ag_resbio_R_C[ R_C ] ) ),
      crop_chars ]
L223.LN3_MgdCarbon_crop$Yield <- L223.ag_EcYield_kgm2_R_C_fby_GLU[[X_final_historical_year]][  
      match( vecpaste( L223.LN3_MgdCarbon_crop[ R_C_GLU ] ),
             vecpaste( L223.ag_EcYield_kgm2_R_C_fby_GLU[ R_C_GLU ] ) ) ]

#Match in harvest index, water content, and root:shoot for fiber and fodder crops based on exogenous assumptions
L223.LN3_MgdCarbon_crop[ L223.LN3_MgdCarbon_crop[[C]]%in% A_Fodderbio_chars[[C]], crop_chars ] <- A_Fodderbio_chars[
      match( L223.LN3_MgdCarbon_crop[[C]][ L223.LN3_MgdCarbon_crop[[C]] %in% A_Fodderbio_chars[[C]] ], A_Fodderbio_chars[[C]] ),
      crop_chars ]

#Calculate vegetation carbon density based on the yields and crop characteristics
L223.LN3_MgdCarbon_crop$hist.veg.carbon.density <- with( L223.LN3_MgdCarbon_crop,
      round( Yield / ( HarvestIndex ) * ( 1 + Root_Shoot ) *
             ( 1 - WaterContent ) * Ccontent_cellulose * Cconv_peak_avg,
      digits_C_density_crop )
)
#Replace missing values with the default values      
L223.LN3_MgdCarbon_crop$hist.veg.carbon.density[ is.na( L223.LN3_MgdCarbon_crop$hist.veg.carbon.density ) ] <-
      L223.LN3_MgdCarbon_crop$veg.carbon.density[ is.na( L223.LN3_MgdCarbon_crop$hist.veg.carbon.density ) ]
L223.LN3_MgdCarbon_crop$hist.veg.carbon.density[ L223.LN3_MgdCarbon_crop$hist.veg.carbon.density == Inf ] <-
      L223.LN3_MgdCarbon_crop$veg.carbon.density[ L223.LN3_MgdCarbon_crop$hist.veg.carbon.density == Inf ]
L223.LN3_MgdCarbon_crop$veg.carbon.density <- L223.LN3_MgdCarbon_crop$hist.veg.carbon.density
L223.LN3_MgdCarbon_crop <- L223.LN3_MgdCarbon_crop[ names_LN3_MgdCarbon ]

printlog( "L223.LN3_MgdCarbon_bio: Carbon content info, managed land in the third nest, bioenergy (veg C from base-year yields)" )
L223.LN3_MgdCarbon_bio <- rbind( L201.AgYield_bio_grass[ L201.AgYield_bio_grass[[Y]] == max( model_base_years ), ],
                                 L201.AgYield_bio_tree[ L201.AgYield_bio_tree[[Y]] == max( model_base_years ), ] )[
                                   c( reg, agtech, "yield" ) ]
L223.LN3_MgdCarbon_bio[[C]] <- L223.LN3_MgdCarbon_bio[[agtech]]
L223.LN3_MgdCarbon_bio <- remove_GLU( L223.LN3_MgdCarbon_bio, var1 = C )
L223.LN3_MgdCarbon_bio[ crop_chars ] <- A_Fodderbio_chars[
  match( L223.LN3_MgdCarbon_bio[[C]], A_Fodderbio_chars[[C]] ),
  crop_chars ]

L223.LN3_MgdCarbon_bio$Yield_kgm2 <- L223.LN3_MgdCarbon_bio$yield / ( bio_GJt * conv_kg_t )

#Set default carbon contents according to the specified corresponding land use type
L223.LN3_MgdCarbon_bio$Cdensity_LT <- GCAMLandLeaf_CdensityLT[[LT]][
  match( L223.LN3_MgdCarbon_bio[[C]], GCAMLandLeaf_CdensityLT$LandLeaf) ]
L223.LN3_MgdCarbon_bio[[R]] <- GCAM_region_names[[R]][ match( L223.LN3_MgdCarbon_bio[[reg]], GCAM_region_names[[reg]] ) ]
L223.LN3_MgdCarbon_bio <- add_carbon_info( L223.LN3_MgdCarbon_bio, carbon_info_table = L121.CarbonContent_kgm2_R_LT_GLU )

#Re-set missing values (places with no land of the assigned land use type from which carbon contents are derived). Just use the min of all other regions
L223.LN3_MgdCarbon_bio[ is.na( L223.LN3_MgdCarbon_bio$soil.carbon.density ), c( "hist.soil.carbon.density", "soil.carbon.density" ) ] <-
  min( L223.LN3_MgdCarbon_bio$soil.carbon.density[ !is.na( L223.LN3_MgdCarbon_bio$soil.carbon.density ) ] )
L223.LN3_MgdCarbon_bio$mature.age[ is.na( L223.LN3_MgdCarbon_bio$mature.age ) ] <-
  min( L223.LN3_MgdCarbon_bio$mature.age, na.rm = T )

#Calculate the veg carbon content. Assume that the crop is perennial so the root portion doesn't get multiplied by the peak->avg conversion
L223.LN3_MgdCarbon_bio$hist.veg.carbon.density <- with( L223.LN3_MgdCarbon_bio, 
      round( Yield_kgm2 / ( HarvestIndex + 1e-6 ) * ( 1 - WaterContent ) *
             Ccontent_cellulose * Cconv_peak_avg +
               Yield_kgm2 / ( HarvestIndex + 1e-6 ) * ( 1 - WaterContent ) *
             Ccontent_cellulose * Root_Shoot,
      digits_C_density_crop ) )
L223.LN3_MgdCarbon_bio$veg.carbon.density <- L223.LN3_MgdCarbon_bio$hist.veg.carbon.density

L223.LN3_MgdCarbon_bio <- add_node_leaf_names( L223.LN3_MgdCarbon_bio, A_LandLeaf3, leaf_name = "LandLeaf", LT_name = C,
                                               LN2 = "LandNode2", LN3 = "LandNode3" )

#Write out only the names to be written to XML
L223.LN3_MgdCarbon_bio <- L223.LN3_MgdCarbon_bio[ names_LN3_MgdCarbon ]

printlog( "Writing out information on protected lands, where applicable" )
# Note - protected land categories are added, and non-protected lands have their land allocation quantities modified.
# The non-protected land information (e.g., logit exponents, carbon densities) are already read in, so do not need to be duplicated
printlog( "L223.LN3_HistUnmgdAllocation_noprot: historical unmanaged land cover, no protect")
printlog( "NOTE: not protecting other arable land" )
remove_OtherArable <- function( data ){
  data_new <- subset( data, !grepl( "OtherArable", UnmanagedLandLeaf ) )
  return( data_new )
  }
L223.LN3_HistUnmgdAllocation_noprot <- remove_OtherArable( L223.LN3_HistUnmgdAllocation )
L223.LN3_HistUnmgdAllocation_noprot$allocation <- L223.LN3_HistUnmgdAllocation_noprot$allocation * ( 1 - protect_land_fract )

printlog( "L223.LN3_UnmgdAllocation_noprot: unmanaged land cover, no protect")
L223.LN3_UnmgdAllocation_noprot <- remove_OtherArable( L223.LN3_UnmgdAllocation )
L223.LN3_UnmgdAllocation_noprot$allocation <- L223.LN3_UnmgdAllocation_noprot$allocation * ( 1 - protect_land_fract )

printlog( "Protected land files: modified land allocations, different names, different nesting structure" )
printlog( "L223.LN1_HistUnmgdAllocation_prot: unmanaged land cover, protected")
L223.LN1_HistUnmgdAllocation_prot <- remove_OtherArable( L223.LN3_HistUnmgdAllocation )
L223.LN1_HistUnmgdAllocation_prot$UnmanagedLandLeaf <- paste0( "Protected", L223.LN1_HistUnmgdAllocation_prot$UnmanagedLandLeaf )
L223.LN1_HistUnmgdAllocation_prot$LandNode1 <- L223.LN1_HistUnmgdAllocation_prot$UnmanagedLandLeaf
L223.LN1_HistUnmgdAllocation_prot$LandNode2 <- NULL
L223.LN1_HistUnmgdAllocation_prot$LandNode3 <- NULL
L223.LN1_HistUnmgdAllocation_prot$allocation <- L223.LN1_HistUnmgdAllocation_prot$allocation * protect_land_fract

printlog( "L223.LN1_UnmgdAllocation_prot: unmanaged land cover, protected")
L223.LN1_UnmgdAllocation_prot <- remove_OtherArable( L223.LN3_UnmgdAllocation )
L223.LN1_UnmgdAllocation_prot$UnmanagedLandLeaf <- paste0( "Protected", L223.LN1_UnmgdAllocation_prot$UnmanagedLandLeaf )
L223.LN1_UnmgdAllocation_prot$LandNode1 <- L223.LN1_UnmgdAllocation_prot$UnmanagedLandLeaf
L223.LN1_UnmgdAllocation_prot$LandNode2 <- NULL
L223.LN1_UnmgdAllocation_prot$LandNode3 <- NULL
L223.LN1_UnmgdAllocation_prot$allocation <- L223.LN1_UnmgdAllocation_prot$allocation * protect_land_fract

printlog( "L223.LN1_UnmgdCarbon_prot: unmanaged carbon info, protected")
L223.LN1_UnmgdCarbon_prot <- remove_OtherArable( L223.LN3_UnmgdCarbon )
L223.LN1_UnmgdCarbon_prot$UnmanagedLandLeaf <- paste0( "Protected", L223.LN1_UnmgdCarbon_prot$UnmanagedLandLeaf )
L223.LN1_UnmgdCarbon_prot$LandNode1 <- L223.LN1_UnmgdCarbon_prot$UnmanagedLandLeaf
L223.LN1_UnmgdCarbon_prot$LandNode2 <- NULL
L223.LN1_UnmgdCarbon_prot$LandNode3 <- NULL

# Final step - need to calculate the bioenergy profit rate compared with the dominant crop, and
# multiply this ratio by the assumed ghost share for bioenergy in each node
printlog( "Calculating the (approximate) profit rate of bioenergy crops in each land use region in the final base year" )
L223.LN3_ProfitRate_bio_grass <- subset( L223.LN3_MgdAllocation_bio, year == final_model_base_year &
                                     grepl( "grass", LandLeaf ))[ c( reg, "LandLeaf", Y ) ]
L223.LN3_ProfitRate_bio_grass$Price <- bio_price_75USD_GJ

L223.LN3_ProfitRate_bio_grass$Cost <- bio_grass_Cost_75USD_GJ

L223.LN3_ProfitRate_bio_grass$Yield <- L201.AgYield_bio_grass$yield[
  match( vecpaste( L223.LN3_ProfitRate_bio_grass[ c( reg, "LandLeaf", Y ) ] ),
         vecpaste( L201.AgYield_bio_grass[ c( reg, agtech, Y ) ] ) ) ]

L223.LN3_ProfitRate_bio_grass$Profit <- with( L223.LN3_ProfitRate_bio_grass, ( Price - Cost ) * Yield )
L223.LN3_ProfitRate_bio_grass <- substring_GLU( L223.LN3_ProfitRate_bio_grass, from.var = "LandLeaf" )

printlog( "Calculating the (approximate) profit rate of the dominant crop of each land use region in the final base year" )
L223.LN3_ProfitRate_domcrop_tmp <- subset( L223.LN3_MgdAllocation_crop, year == final_model_base_year )
L223.LN3_ProfitRate_domcrop_tmp <- substring_GLU( L223.LN3_ProfitRate_domcrop_tmp, from.var = "LandLeaf" )

# Strange sequence here to avoid cases where two crops within a land use region have exactly the same land allocation
L223.LN3_ProfitRate_domcrop <- aggregate( L223.LN3_ProfitRate_domcrop_tmp[ "allocation" ],
                                          by = L223.LN3_ProfitRate_domcrop_tmp[ c( reg, Y, GLU ) ],
                                          max )
L223.LN3_ProfitRate_domcrop[[R]] <- GCAM_region_names[[R]][
  match( L223.LN3_ProfitRate_domcrop[[reg]], GCAM_region_names[[reg]] ) ]
L223.LN3_ProfitRate_domcrop$LandLeaf <- L223.LN3_ProfitRate_domcrop_tmp$LandLeaf[
  match( vecpaste( L223.LN3_ProfitRate_domcrop[ c( reg, Y, GLU, "allocation" ) ] ),
         vecpaste( L223.LN3_ProfitRate_domcrop_tmp[ c( reg, Y, GLU, "allocation" ) ] ) ) ]
L223.LN3_ProfitRate_domcrop[[C]] <- substr( L223.LN3_ProfitRate_domcrop$LandLeaf, 1,
                                            regexpr( "_GLU", L223.LN3_ProfitRate_domcrop$LandLeaf, fixed = T ) - 1 )

# Match in the prices, costs, and yields
L223.LN3_ProfitRate_domcrop$Price <- L132.ag_an_For_Prices$calPrice[
  match( L223.LN3_ProfitRate_domcrop[[C]], L132.ag_an_For_Prices[[C]] ) ]
L223.LN3_ProfitRate_domcrop$Cost <- L133.ag_Cost_75USDkg_C$Cost_75USDkg[
  match( L223.LN3_ProfitRate_domcrop[[C]], L133.ag_Cost_75USDkg_C[[C]] ) ]
L223.LN3_ProfitRate_domcrop$Yield <- L122.ag_EcYield_kgm2_R_C_Y_GLU[[X_final_model_base_year]][
  match( vecpaste( L223.LN3_ProfitRate_domcrop[ R_C_GLU ] ),
         vecpaste( L122.ag_EcYield_kgm2_R_C_Y_GLU[ R_C_GLU ] ) ) ]

#Calculate the profit, and match it into the bioenergy table
L223.LN3_ProfitRate_domcrop$Profit <- with( L223.LN3_ProfitRate_domcrop, ( Price - Cost ) * Yield )
L223.LN3_ProfitRate_bio_grass$Profit_domcrop <- L223.LN3_ProfitRate_domcrop$Profit[
  match( vecpaste( L223.LN3_ProfitRate_bio_grass[ c( reg, GLU ) ] ),
         vecpaste( L223.LN3_ProfitRate_domcrop[ c( reg, GLU ) ] ) ) ]
L223.LN3_ProfitRate_bio_grass$ProfitRatio <- with( L223.LN3_ProfitRate_bio_grass, Profit / Profit_domcrop )

#Repeat for bio tree
L223.LN3_ProfitRate_bio_tree <- subset( L223.LN3_MgdAllocation_bio, year == final_model_base_year &
                                          grepl( "tree", LandLeaf ))[ c( reg, "LandLeaf", Y ) ]
L223.LN3_ProfitRate_bio_tree[[R]] <- GCAM_region_names[[R]][
  match( L223.LN3_ProfitRate_bio_tree[[reg]], GCAM_region_names[[reg]] ) ]
L223.LN3_ProfitRate_bio_tree$Price <- bio_price_75USD_GJ
L223.LN3_ProfitRate_bio_tree$Cost <- bio_tree_Cost_75USD_GJ
L223.LN3_ProfitRate_bio_tree$Yield <- L201.AgYield_bio_tree$yield[
  match( vecpaste( L223.LN3_ProfitRate_bio_tree[ c( reg, "LandLeaf", Y ) ] ),
         vecpaste( L201.AgYield_bio_tree[ c( reg, agtech, Y ) ] ) ) ]

#Calculate profit
L223.LN3_ProfitRate_bio_tree$Profit <- with( L223.LN3_ProfitRate_bio_tree, ( Price - Cost ) * Yield )
L223.LN3_ProfitRate_bio_tree <- substring_GLU( L223.LN3_ProfitRate_bio_tree, from.var = "LandLeaf" )

#For forestry, the price and the cost are the same in all regions
L223.LN3_ProfitRate_bio_tree$Price_For <- L132.ag_an_For_Prices$calPrice[ L132.ag_an_For_Prices[[C]] == "Forest" ]
L223.LN3_ProfitRate_bio_tree$Cost_For <- cost_For_75USDm3
L223.LN3_ProfitRate_bio_tree$Yield_For <- L123.For_Yield_m3m2_R_GLU[[X_final_model_base_year ]][
  match( vecpaste( L223.LN3_ProfitRate_bio_tree[ R_GLU ] ),
         vecpaste( L123.For_Yield_m3m2_R_GLU[ R_GLU ] ) ) ]
L223.LN3_ProfitRate_bio_tree$Profit_For <- with( L223.LN3_ProfitRate_bio_tree, ( Price_For - Cost_For ) * Yield_For )
L223.LN3_ProfitRate_bio_tree$ProfitRatio <- with( L223.LN3_ProfitRate_bio_tree, Profit / Profit_For )

L223.LN3_ProfitRatio_bio <- rbind( L223.LN3_ProfitRate_bio_grass[ c( reg, "LandLeaf", "ProfitRatio" ) ],
                                   L223.LN3_ProfitRate_bio_tree[ c( reg, "LandLeaf", "ProfitRatio" ) ] )

printlog( "Multiplying the unnormalized ghost shares by the profit ratios, for grass and tree bioenergy crops" )
L223.LN3_LeafGhostShare$ghost.unnormalized.share <- L223.LN3_LeafGhostShare$ghost.unnormalized.share *
  L223.LN3_ProfitRatio_bio$ProfitRatio[
    match( vecpaste( L223.LN3_LeafGhostShare[ c( reg, "LandLeaf" ) ] ),
           vecpaste( L223.LN3_ProfitRatio_bio[ c( reg, "LandLeaf" ) ] ) ) ]

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

for( curr_table in L223.LN3_LogitTables ) {
write_mi_data( curr_table$data, curr_table$header, "AGLU_LEVEL2_DATA", paste0( "L223.", curr_table$header ), "AGLU_XML_BATCH", "batch_land_input_3.xml" )
}
write_mi_data( L223.LN3_Logit, IDstring="LN3_Logit", domain="AGLU_LEVEL2_DATA", fn="L223.LN3_Logit",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_land_input_3.xml" )
write_mi_data( L223.LN3_LeafGhostShare, "LN3_LeafGhostShare", "AGLU_LEVEL2_DATA", "L223.LN3_LeafGhostShare", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L223.LN3_HistUnmgdAllocation, "LN3_HistUnmgdAllocation", "AGLU_LEVEL2_DATA", "L223.LN3_HistUnmgdAllocation", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L223.LN3_UnmgdAllocation, "LN3_UnmgdAllocation", "AGLU_LEVEL2_DATA", "L223.LN3_UnmgdAllocation", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L223.NodeEquiv, "EQUIV_TABLE", "AGLU_LEVEL2_DATA", "L223.NodeEquiv", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L223.LN3_NoEmissCarbon, "LN3_NoEmissCarbon", "AGLU_LEVEL2_DATA", "L223.LN3_NoEmissCarbon", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L223.LN3_NodeCarbon, "LN3_NodeCarbon", "AGLU_LEVEL2_DATA", "L223.LN3_NodeCarbon", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L223.LN3_HistMgdAllocation_noncrop, "LN3_HistMgdAllocation", "AGLU_LEVEL2_DATA", "L223.LN3_HistMgdAllocation_noncrop", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L223.LN3_MgdAllocation_noncrop, "LN3_MgdAllocation", "AGLU_LEVEL2_DATA", "L223.LN3_MgdAllocation_noncrop", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L223.LN3_HistMgdAllocation_crop, "LN3_HistMgdAllocation", "AGLU_LEVEL2_DATA", "L223.LN3_HistMgdAllocation_crop", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L223.LN3_MgdAllocation_crop, "LN3_MgdAllocation", "AGLU_LEVEL2_DATA", "L223.LN3_MgdAllocation_crop", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L223.LN3_HistMgdAllocation_bio, "LN3_HistMgdAllocation", "AGLU_LEVEL2_DATA", "L223.LN3_HistMgdAllocation_bio", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L223.LN3_MgdAllocation_bio, "LN3_MgdAllocation", "AGLU_LEVEL2_DATA", "L223.LN3_MgdAllocation_bio", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L223.LN3_UnmgdCarbon, "LN3_UnmgdCarbon", "AGLU_LEVEL2_DATA", "L223.LN3_UnmgdCarbon", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L223.LN3_MgdCarbon_noncrop, "LN3_MgdCarbon", "AGLU_LEVEL2_DATA", "L223.LN3_MgdCarbon_noncrop", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L223.LN3_MgdCarbon_crop, "LN3_MgdCarbon", "AGLU_LEVEL2_DATA", "L223.LN3_MgdCarbon_crop", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L223.LN3_MgdCarbon_bio, "LN3_MgdCarbon", "AGLU_LEVEL2_DATA", "L223.LN3_MgdCarbon_bio", "AGLU_XML_BATCH", "batch_land_input_3.xml", node_rename=T )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_land_input_3.xml", "AGLU_XML_FINAL", "land_input_3.xml", "", xml_tag="outFile" )

#protected lands files
write_mi_data( L223.LN3_HistUnmgdAllocation_noprot, "LN3_HistUnmgdAllocation", "AGLU_LEVEL2_DATA", "L223.LN3_HistUnmgdAllocation_noprot", "AGLU_XML_BATCH", "batch_protected_land_input_3.xml" )
write_mi_data( L223.LN3_UnmgdAllocation_noprot, "LN3_UnmgdAllocation", "AGLU_LEVEL2_DATA", "L223.LN3_UnmgdAllocation_noprot", "AGLU_XML_BATCH", "batch_protected_land_input_3.xml" )

write_mi_data( L223.LN1_HistUnmgdAllocation_prot, "LN1_HistUnmgdAllocation", "AGLU_LEVEL2_DATA", "L223.LN1_HistUnmgdAllocation_prot", "AGLU_XML_BATCH", "batch_protected_land_input_3.xml" )
write_mi_data( L223.LN1_UnmgdAllocation_prot, "LN1_UnmgdAllocation", "AGLU_LEVEL2_DATA", "L223.LN1_UnmgdAllocation_prot", "AGLU_XML_BATCH", "batch_protected_land_input_3.xml" )
write_mi_data( L223.LN1_UnmgdCarbon_prot, "LN1_UnmgdCarbon", "AGLU_LEVEL2_DATA", "L223.LN1_UnmgdCarbon_prot", "AGLU_XML_BATCH", "batch_protected_land_input_3.xml", node_rename=T )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_protected_land_input_3.xml", "AGLU_XML_FINAL", "protected_land_input_3.xml", "", xml_tag="outFile" )

logstop()
