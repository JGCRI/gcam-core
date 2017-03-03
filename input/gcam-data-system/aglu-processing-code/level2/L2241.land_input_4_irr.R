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
logstart( "L2241.land_input_4_irr.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Land cover and characteristics, node level 4" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "level2_data_names", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
basin_to_country_mapping <- readdata( "WATER_MAPPINGS", "basin_to_country_mapping" )
A_Fodderbio_chars <- readdata( "AGLU_ASSUMPTIONS", "A_Fodderbio_chars" )
A_LandNode_logit_irr <- readdata( "AGLU_ASSUMPTIONS", "A_LandNode_logit_irr" )
A_LandNode4_irr <- readdata( "AGLU_ASSUMPTIONS", "A_LandNode4_irr" )
A_LandLeaf4_irr <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf4_irr" )
A_bio_ghost_share <- readdata( "AGLU_ASSUMPTIONS", "A_bio_ghost_share" )
GCAMLandLeaf_CdensityLT <- readdata( "AGLU_MAPPINGS", "GCAMLandLeaf_CdensityLT" )
L111.ag_resbio_R_C <- readdata( "AGLU_LEVEL1_DATA", "L111.ag_resbio_R_C" )
L121.CarbonContent_kgm2_R_LT_GLU <- readdata( "AGLU_LEVEL1_DATA", "L121.CarbonContent_kgm2_R_LT_GLU", replace_GLU = T )
L171.ag_irrEcYield_kgm2_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L171.ag_irrEcYield_kgm2_R_C_Y_GLU", replace_GLU = T )
L171.ag_rfdEcYield_kgm2_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L171.ag_rfdEcYield_kgm2_R_C_Y_GLU", replace_GLU = T )
L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU <- readdata ( "AGLU_LEVEL1_DATA", "L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU", replace_GLU = T )
L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU <- readdata ( "AGLU_LEVEL1_DATA", "L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU", replace_GLU = T )
L223.LN3_HistMgdAllocation_bio <- readdata( "AGLU_LEVEL2_DATA", "L223.LN3_HistMgdAllocation_bio", skip = 4 )
L223.LN3_MgdAllocation_bio <- readdata( "AGLU_LEVEL2_DATA", "L223.LN3_MgdAllocation_bio", skip = 4 )
L223.LN3_MgdAllocation_crop <- readdata( "AGLU_LEVEL2_DATA", "L223.LN3_MgdAllocation_crop", skip = 4 )
L223.LN3_LeafGhostShare <- readdata( "AGLU_LEVEL2_DATA", "L223.LN3_LeafGhostShare", skip = 4 )
L223.LN3_LeafIsGhostShareRel <- readdata( "AGLU_LEVEL2_DATA", "L223.LN3_LeafIsGhostShareRel", skip = 4 )
L2011.AgYield_bio_grass_irr <- readdata( "AGLU_LEVEL2_DATA", "L2011.AgYield_bio_grass_irr", skip = 4 )
L2011.AgYield_bio_tree_irr <- readdata( "AGLU_LEVEL2_DATA", "L2011.AgYield_bio_tree_irr", skip = 4 )


# -----------------------------------------------------------------------------
# 2. Build tables
printlog( "L2241.LN4: base table for land node 4" )
#Use the cropland and bioenergy allocation tables to establish which region/glu/node combinations are available
names_LN3_Leaf <- c( reg, "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf" )
L2241.LN4 <- rbind( unique( L223.LN3_MgdAllocation_crop[ names_LN3_Leaf ] ),
                          unique( L223.LN3_MgdAllocation_bio[ names_LN3_Leaf ] ) )
# What was a leaf for level3 is now a node, as it will have the 4th level nested under it
names( L2241.LN4 )[ names( L2241.LN4 ) == "LandLeaf" ] <- "LandNode4"
L2241.LN4 <- substring_GLU( L2241.LN4, "LandNode4" )

printlog( "L2241.LN4_Leaf: base table for land leaf in the fourth node" )
L2241.LN4_Leaf <- repeat_and_add_vector( L2241.LN4, irr, c( "IRR", "RFD" ) )
L2241.LN4_Leaf$LandLeaf <- paste( L2241.LN4_Leaf$LandNode4, L2241.LN4_Leaf[[irr]], sep = irr_delimiter )

printlog( "L2241.LN4_Logit: Logit exponent, fourth nest" )
L2241.LN4_Logit <- L2241.LN4
L2241.LN4_Logit$logit.year.fillout <- min( model_base_years )
L2241.LN4_Logit <- remove_GLU( L2241.LN4_Logit, "LandNode4" )
L2241.LN4_Logit$logit.exponent <- A_LandNode_logit_irr$logit.exponent[
  match( L2241.LN4_Logit$LandNode4, A_LandNode_logit_irr$LandNode ) ]
L2241.LN4_Logit$logit.type <- A_LandNode_logit_irr$logit.type[
  match( L2241.LN4_Logit$LandNode4, A_LandNode_logit_irr$LandNode ) ]
L2241.LN4_Logit <- append_GLU( L2241.LN4_Logit, var1 = "LandNode4" )

L2241.LN4_LogitTables <- get_logit_fn_tables( L2241.LN4_Logit, names_LN4_LogitType,
    base.header="LN4_Logit_", include.equiv.table=T, write.all.regions=F )
L2241.LN4_Logit <- L2241.LN4_Logit[ names_LN4_Logit ]

#LAND USE HISTORY
# crop land
L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU[[irr]] <- "IRR"
L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU[[irr]] <- "RFD"
L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irr <- rbind( L171.LC_bm2_R_irrHarvCropLand_C_Yh_GLU, L171.LC_bm2_R_rfdHarvCropLand_C_Yh_GLU )

L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irr.melt <- melt( L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irr,
                                                       id.vars = R_C_GLU_irr, measure.vars = unique( c( X_land_cover_years, X_model_base_years ) ),
                                                       variable.name = "Xyear", value.name = "allocation" )
L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irr.melt <- add_region_name( L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irr.melt )
L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irr.melt[[Y]] <- sub( "X", "", L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irr.melt$Xyear )
L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irr.melt$allocation <- round( L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irr.melt$allocation, digits_land_use )
L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irr.melt <- add_node_leaf_names( L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irr.melt,
                                                                      nesting_table = A_LandLeaf4_irr, leaf_name = "LandLeaf",
                                                                      LT_name = C, LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3", LN4 = "LandNode4" )

printlog( "L2241.LN4_HistMgdAllocation_crop: Historical land cover, managed land in the fourth nest, cropland" )
L2241.LN4_HistMgdAllocation_crop <- L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irr.melt[
  L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irr.melt[[Y]] %in% land_history_years, names_LN4_HistMgdAllocation ]

printlog( "L2241.LN4_MgdAllocation_crop: Model base year land cover, managed land in the fourth nest, cropland" )
L2241.LN4_MgdAllocation_crop <- L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irr.melt[
  L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irr.melt[[Y]] %in% model_base_years, names_LN4_MgdAllocation ]

# biomass
L2241.LN4_Leaf_bio <- subset( L2241.LN4_Leaf, grepl( bio_grass_name, LandLeaf ) | grepl( bio_tree_name, LandLeaf ) )
L2241.LN4_Leaf_bio[[C]] <- bio_grass_name
L2241.LN4_Leaf_bio[[C]][ grepl( bio_tree_name, L2241.LN4_Leaf_bio$LandLeaf ) ] <- bio_tree_name

printlog( "L2241.LN4_HistMgdAllocation_bio: Historical land cover, managed land in the third nest, bioenergy" )
L2241.LN4_HistMgdAllocation_bio <- repeat_and_add_vector( L2241.LN4_Leaf_bio, Y, land_history_years )
L2241.LN4_HistMgdAllocation_bio$allocation <- 0
L2241.LN4_HistMgdAllocation_bio <- L2241.LN4_HistMgdAllocation_bio[ names_LN4_HistMgdAllocation ]

printlog( "L2241.LN4_MgdAllocation_bio: Model base year land cover, managed land in the third nest, bioenergy" )
L2241.LN4_MgdAllocation_bio <- repeat_and_add_vector( L2241.LN4_Leaf_bio, Y, model_base_years )
L2241.LN4_MgdAllocation_bio$allocation <- 0
L2241.LN4_MgdAllocation_bio <- L2241.LN4_MgdAllocation_bio[ names_LN4_MgdAllocation ]

#CARBON CONTENTS AND MATURE AGES
#Add region vector to carbon info table
L2241.CarbonContent_kgm2_R_LT_GLU <- add_region_name( L121.CarbonContent_kgm2_R_LT_GLU )

printlog( "L2241.LN4_MgdCarbon_crop: Carbon content info, managed land in the fourth nest, cropland (no bio)" )
printlog( "Soil will use default values, but vegetation will be replaced by bottom-up estimates" )
L2241.LN4_MgdCarbon_crop <- L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irr.melt[
  L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irr.melt[[Y]] == max( model_base_years ),
  names( L2241.LN4_MgdAllocation_crop ) %!in% c( Y, "allocation" ) ]
L2241.LN4_MgdCarbon_crop <- add_region_name( L2241.LN4_MgdCarbon_crop )
L2241.LN4_MgdCarbon_crop$LandAllocatorRoot <- "root"
L2241.LN4_MgdCarbon_crop$Cdensity_LT <- GCAMLandLeaf_CdensityLT[[LT]][
  match( L2241.LN4_MgdCarbon_crop[[C]], GCAMLandLeaf_CdensityLT$LandLeaf) ]
L2241.LN4_MgdCarbon_crop <- add_carbon_info( L2241.LN4_MgdCarbon_crop, carbon_info_table = L121.CarbonContent_kgm2_R_LT_GLU )

printlog( "Replacing missing values in places with harvested area and production but no assigned cropland" )
# If regions have harvested area and production but no cropland assigned in Hyde, we take "unmanaged" land and re-assign it to cropland.
# however this re-assignment is done after computing carbon contents, so such regions will have missing values at this point.
# because at this point cropland carbon contents are not derived from underlying vegetation, all are equal and we can just inherit the
# values from any other region. No need to do this w veg at this point b/c it will be replaced later
L2241.LN4_MgdCarbon_crop[ is.na( L2241.LN4_MgdCarbon_crop$soil.carbon.density ), c( "hist.soil.carbon.density", "soil.carbon.density" ) ] <-
  mean( L2241.LN4_MgdCarbon_crop$soil.carbon.density, na.rm = T )
L2241.LN4_MgdCarbon_crop$mature.age[ is.na( L2241.LN4_MgdCarbon_crop$mature.age ) ] <-
  mean( L2241.LN4_MgdCarbon_crop$mature.age, na.rm = T )

#Add region ID vector to residue biomass table
L2241.ag_resbio_R_C <- add_region_name( L111.ag_resbio_R_C )

#Add region ID to table of ec yields
#Melt yield table in final calibration year, and paste in region ID vector
L171.ag_irrEcYield_kgm2_R_C_Y_GLU[[irr]] <- "IRR"
L171.ag_rfdEcYield_kgm2_R_C_Y_GLU[[irr]] <- "RFD"
L2241.ag_EcYield_kgm2_R_C_fby_GLU_irr <- rbind( L171.ag_irrEcYield_kgm2_R_C_Y_GLU, L171.ag_rfdEcYield_kgm2_R_C_Y_GLU )[
  c( R_C_GLU_irr, X_final_historical_year ) ]
L2241.ag_EcYield_kgm2_R_C_fby_GLU_irr <- add_region_name( L2241.ag_EcYield_kgm2_R_C_fby_GLU_irr )

#Map in information for calculation of cropland vegetation carbon
crop_chars <- c( "HarvestIndex", "WaterContent", "Root_Shoot" )
L2241.LN4_MgdCarbon_crop[ crop_chars ] <- L2241.ag_resbio_R_C[
  match( vecpaste( L2241.LN4_MgdCarbon_crop[ R_C ] ),
         vecpaste( L2241.ag_resbio_R_C[ R_C ] ) ),
  crop_chars ]
L2241.LN4_MgdCarbon_crop$yield <- L2241.ag_EcYield_kgm2_R_C_fby_GLU_irr[[X_final_historical_year]][  
  match( vecpaste( L2241.LN4_MgdCarbon_crop[ R_C_GLU_irr ] ),
         vecpaste( L2241.ag_EcYield_kgm2_R_C_fby_GLU_irr[ R_C_GLU_irr ] ) ) ]

#Match in harvest index, water content, and root:shoot for fiber and fodder crops based on exogenous assumptions
L2241.LN4_MgdCarbon_crop[ L2241.LN4_MgdCarbon_crop[[C]]%in% A_Fodderbio_chars[[C]], crop_chars ] <- A_Fodderbio_chars[
  match( L2241.LN4_MgdCarbon_crop[[C]][ L2241.LN4_MgdCarbon_crop[[C]] %in% A_Fodderbio_chars[[C]] ], A_Fodderbio_chars[[C]] ),
  crop_chars ]

#Calculate vegetation carbon density based on the yields and crop characteristics
L2241.LN4_MgdCarbon_crop$hist.veg.carbon.density <- with( L2241.LN4_MgdCarbon_crop,
                                                         round( yield / ( HarvestIndex ) * ( 1 + Root_Shoot ) *
                                                                  ( 1 - WaterContent ) * Ccontent_cellulose * Cconv_peak_avg,
                                                                digits_C_density_crop )
                                                         )

#Replace missing values with the default values      
L2241.LN4_MgdCarbon_crop$hist.veg.carbon.density[ is.na( L2241.LN4_MgdCarbon_crop$hist.veg.carbon.density ) ] <-
  L2241.LN4_MgdCarbon_crop$veg.carbon.density[ is.na( L2241.LN4_MgdCarbon_crop$hist.veg.carbon.density ) ]
L2241.LN4_MgdCarbon_crop$hist.veg.carbon.density[ L2241.LN4_MgdCarbon_crop$hist.veg.carbon.density == Inf ] <-
  L2241.LN4_MgdCarbon_crop$veg.carbon.density[ L2241.LN4_MgdCarbon_crop$hist.veg.carbon.density == Inf ]
L2241.LN4_MgdCarbon_crop$veg.carbon.density <- L2241.LN4_MgdCarbon_crop$hist.veg.carbon.density
L2241.LN4_MgdCarbon_crop <- L2241.LN4_MgdCarbon_crop[ names_LN4_MgdCarbon ]

printlog( "L2241.LN4_MgdCarbon_bio: biomass carbon information" )
L2241.LN4_MgdCarbon_bio <- L2241.LN4_Leaf_bio
L2241.LN4_MgdCarbon_bio[[R]] <- GCAM_region_names[[R]][ match( L2241.LN4_MgdCarbon_bio[[reg]], GCAM_region_names[[reg]] ) ]
L2241.LN4_MgdCarbon_bio$Cdensity_LT <- GCAMLandLeaf_CdensityLT[[LT]][
  match( L2241.LN4_MgdCarbon_bio[[C]], GCAMLandLeaf_CdensityLT$LandLeaf) ]
L2241.LN4_MgdCarbon_bio <- add_carbon_info( L2241.LN4_MgdCarbon_bio, carbon_info_table = L121.CarbonContent_kgm2_R_LT_GLU )

# There may missing values, where the assigned land type (LT) from which to get the carbon content didn't actually exist. Re-set to defaults.
L2241.LN4_MgdCarbon_bio$mature.age[ is.na( L2241.LN4_MgdCarbon_bio$mature.age ) ] <-
  mean( L2241.LN4_MgdCarbon_bio$mature.age, na.rm = T )
L2241.LN4_MgdCarbon_bio[ is.na( L2241.LN4_MgdCarbon_bio$soil.carbon.density ), c( "hist.soil.carbon.density", "soil.carbon.density" ) ] <-
  mean( L2241.LN4_MgdCarbon_bio$soil.carbon.density, na.rm = T )

#Match in biomass yield, converting from GJ/m2 to kg/m2
L2241.ag_bioYield_GJm2_R_GLU_irr_ref <- rbind( subset( L2011.AgYield_bio_grass_irr, year == max( model_base_years ) ),
                                               subset( L2011.AgYield_bio_tree_irr, year == max( model_base_years ) ) )
L2241.LN4_MgdCarbon_bio$yield <- L2241.ag_bioYield_GJm2_R_GLU_irr_ref$yield[  
      match( vecpaste( L2241.LN4_MgdCarbon_bio[ c( reg, "LandLeaf" ) ] ),
             vecpaste( L2241.ag_bioYield_GJm2_R_GLU_irr_ref[ c( reg, agtech ) ] ) ) ] /
      ( bio_GJt * conv_kg_t )

#Calculate the veg carbon content. Assume that the crop is perennial so the root portion doesn't get multiplied by the peak->avg conversion
L2241.LN4_MgdCarbon_bio[ crop_chars ] <- A_Fodderbio_chars[ 
  match( L2241.LN4_MgdCarbon_bio[[C]], A_Fodderbio_chars[[C]] ),
  crop_chars]
L2241.LN4_MgdCarbon_bio$hist.veg.carbon.density <-
  with( L2241.LN4_MgdCarbon_bio,
        round( yield * ( 1 - WaterContent ) / HarvestIndex * Ccontent_cellulose * Cconv_peak_avg +
                 yield * ( 1 - WaterContent ) * Ccontent_cellulose * Root_Shoot,
               digits_C_density_crop ) )
L2241.LN4_MgdCarbon_bio$veg.carbon.density <- L2241.LN4_MgdCarbon_bio$hist.veg.carbon.density

#Write out only the names to be written to XML
L2241.LN4_MgdCarbon_bio <- L2241.LN4_MgdCarbon_bio[ names_LN4_MgdCarbon ]

printlog( "L2241.LN4_LeafGhostShare: Specify the start year for purpose-grown biomass" )
#Start with a table that has not had its bioenergy crops re-named yet (will make assigning a land leaf easier)
L2241.LN4_LeafGhostShare <- L2241.LN4_Leaf_bio
L2241.LN4_LeafGhostShare$year <- Bio_start_year

printlog( "NOTE: Assigning irrigated bioenergy ghost shares based on the share of agricultural land that is irrigated by region and GLU" )
#Calculate fraction of cropland that is irrigated in the final historical year, for each region and GLU
#First, multiply the irrigated quantity of land by an exogenous multiplier whose purpose is to reduce its share
L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irrX <- L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irr[ c( R_C_GLU_irr, X_final_historical_year ) ]
L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irrX[[ X_final_historical_year ]][ grepl( "IRR", L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irrX [[irr ]] ) ] <-
      L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irrX[[ X_final_historical_year ]][ grepl( "IRR", L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irrX [[irr ]] ) ] *
      irrig_ghost_share_mult
L2241.LC_bm2_R_HarvCropLand_GLU_irr <- aggregate( L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irrX[ X_final_historical_year ],
                                                  by = L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irrX[ R_GLU_irr ], sum )
L2241.LC_bm2_R_HarvCropLand_GLU <- aggregate( L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irrX[ X_final_historical_year ],
                                              by = L2241.LC_bm2_R_HarvCropLand_C_Yh_GLU_irrX[ R_GLU ], sum )
L2241.GhostShare_R_GLU <- L2241.LC_bm2_R_HarvCropLand_GLU_irr
L2241.GhostShare_R_GLU$GhostShare <- L2241.LC_bm2_R_HarvCropLand_GLU_irr[[ X_final_historical_year ]] /
      L2241.LC_bm2_R_HarvCropLand_GLU[[ X_final_historical_year ]][
         match( vecpaste( L2241.LC_bm2_R_HarvCropLand_GLU_irr[ R_GLU ] ),
                vecpaste( L2241.LC_bm2_R_HarvCropLand_GLU[ R_GLU ] ) ) ]
L2241.GhostShare_R_GLU <- add_region_name( L2241.GhostShare_R_GLU )

#Set ghost.unnormalized.share to irrigated share
L2241.LN4_LeafGhostShare$ghost.unnormalized.share <- round(
    L2241.GhostShare_R_GLU$GhostShare[
      match( vecpaste( L2241.LN4_LeafGhostShare[ c( reg, GLU, irr ) ] ),
             vecpaste( L2241.GhostShare_R_GLU[ c( reg, GLU, irr ) ] ) ) ],
    digits_land_use )

#Regions / GLUs may exist but not have any cropland. In these cases, set the ghost.unnormalized.share to 1 for rainfed and 0 for irrigated
L2241.LN4_LeafGhostShare$ghost.unnormalized.share[ is.na( L2241.LN4_LeafGhostShare$ghost.unnormalized.share ) & L2241.LN4_LeafGhostShare[[irr]] == "IRR" ] <- 0
L2241.LN4_LeafGhostShare$ghost.unnormalized.share[ is.na( L2241.LN4_LeafGhostShare$ghost.unnormalized.share ) & L2241.LN4_LeafGhostShare[[irr]] == "RFD" ] <- 1
L2241.LN4_LeafGhostShare <- L2241.LN4_LeafGhostShare[ names_LN4_LeafGhostShare ]

printlog( "L2241.LN4_NodeGhostShare: Indicate that the bioenergy node is available in future years, and specify the ghost node share" )
# These are the same as the values that would have been set as ghost share in the leaves in land input 3.
# We can just copy that data frame and just rename the LandLeaf column to LandNode.
L2241.LN4_NodeGhostShare <- L223.LN3_LeafGhostShare
names( L2241.LN4_NodeGhostShare ) <- names_LN4_NodeGhostShare
L2241.LN4_NodeIsGhostShareRel <- L223.LN3_LeafIsGhostShareRel
names( L2241.LN4_NodeIsGhostShareRel ) <- names_LN4_NodeIsGhostShareRel

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

for( curr_table in L2241.LN4_LogitTables ) {
write_mi_data( curr_table$data, curr_table$header, "AGLU_LEVEL2_DATA", paste0( "L2241.", curr_table$header ), "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml" )
}
write_mi_data( L2241.LN4_Logit, IDstring="LN4_Logit", domain="AGLU_LEVEL2_DATA", fn="L2241.LN4_Logit",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_land_input_4_IRR.xml" )
write_mi_data( L2241.LN4_HistMgdAllocation_crop, "LN4_HistMgdAllocation", "AGLU_LEVEL2_DATA", "L2241.LN4_HistMgdAllocation_crop", "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml" )
write_mi_data( L2241.LN4_MgdAllocation_crop, "LN4_MgdAllocation", "AGLU_LEVEL2_DATA", "L2241.LN4_MgdAllocation_crop", "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml" )
write_mi_data( L2241.LN4_HistMgdAllocation_bio, "LN4_HistMgdAllocation", "AGLU_LEVEL2_DATA", "L2241.LN4_HistMgdAllocation_bio", "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml" )
write_mi_data( L2241.LN4_MgdAllocation_bio, "LN4_MgdAllocation", "AGLU_LEVEL2_DATA", "L2241.LN4_MgdAllocation_bio", "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml" )
write_mi_data( L2241.LN4_MgdCarbon_crop, "LN4_MgdCarbon", "AGLU_LEVEL2_DATA", "L2241.LN4_MgdCarbon_crop", "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml" )
write_mi_data( L2241.LN4_MgdCarbon_bio, "LN4_MgdCarbon", "AGLU_LEVEL2_DATA", "L2241.LN4_MgdCarbon_bio", "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml" )
write_mi_data( L2241.LN4_LeafGhostShare, "LN4_LeafGhostShare", "AGLU_LEVEL2_DATA", "L2241.LN4_LeafGhostShare", "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml" )
write_mi_data( L2241.LN4_NodeGhostShare, "LN4_NodeGhostShare", "AGLU_LEVEL2_DATA", "L2241.LN4_NodeGhostShare", "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml" )
write_mi_data( L2241.LN4_NodeIsGhostShareRel, "LN4_NodeIsGhostShareRel", "AGLU_LEVEL2_DATA", "L2241.LN4_NodeIsGhostShareRel", "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml", node_rename=T )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml", "AGLU_XML_FINAL", "land_input_4_IRR.xml", "", xml_tag="outFile" )

logstop()
