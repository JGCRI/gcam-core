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
logstart( "L213.land_input_3.R" )
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
A_bio_default_share <- readdata( "AGLU_ASSUMPTIONS", "A_bio_default_share" )
A_biocrops_R_AEZ <- readdata( "AGLU_ASSUMPTIONS", "A_biocrops_R_AEZ" )
A_Fodderbio_chars <- readdata( "AGLU_ASSUMPTIONS", "A_Fodderbio_chars" )
A_LandNode_logit <- readdata( "AGLU_ASSUMPTIONS", "A_LandNode_logit" )
A_LandNode3 <- readdata( "AGLU_ASSUMPTIONS", "A_LandNode3" )
A_LandLeaf_Unmgd3 <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf_Unmgd3" )
A_LandLeaf3 <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf3" )
L111.ag_resbio_R_C <- readdata( "AGLU_LEVEL1_DATA", "L111.ag_resbio_R_C" )
L113.ag_bioYield_GJm2_R_AEZ_ref <- readdata( "AGLU_LEVEL1_DATA", "L113.ag_bioYield_GJm2_R_AEZ_ref" )
L113.ag_bioYield_GJm2_R_AEZ_hi <- readdata( "AGLU_LEVEL1_DATA", "L113.ag_bioYield_GJm2_R_AEZ_hi" )
L121.VegC_kgm2_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.VegC_kgm2_R_LT_AEZ" )
L121.SoilC_kgm2_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.SoilC_kgm2_R_LT_AEZ" )
L121.MatureAge_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.MatureAge_R_LT_AEZ" )
L122.ag_EcYield_kgm2_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L122.ag_EcYield_kgm2_R_C_Y_AEZ" )
L122.LC_bm2_R_HarvCropLand_C_Yh_AEZ <- readdata ( "AGLU_LEVEL1_DATA", "L122.LC_bm2_R_HarvCropLand_C_Yh_AEZ" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )
L125.LC_bm2_R_LT_Yh_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L125.LC_bm2_R_LT_Yh_AEZ" )

# -----------------------------------------------------------------------------
# 2. Build tables
printlog( "L213.LN3_Logit: Logit exponent and default share in selected periods, third nest" )
L213.LN3_Logit <- data.frame(
      region = rep( GCAM_region_names$region, times = nrow( A_LandNode3 ) ),
      LandAllocatorRoot = "root",
      LandNode3 = sort( rep( A_LandNode3$LandNode3, times = length( GCAM_region_names$region ) ) ) )
L213.LN3_Logit[ c( "LandNode1", "LandNode2" ) ] <- A_LandNode3[
      match( L213.LN3_Logit$LandNode3, A_LandNode3$LandNode3 ),
      c( "LandNode1", "LandNode2" ) ]
L213.LN3_Logit <- repeat_and_add_vector( L213.LN3_Logit, AEZ, AEZs )

#Match in logit exponents based on the land node 2
L213.LN3_Logit$logit.year.fillout <- min( model_base_years )
L213.LN3_Logit$logit.exponent <- A_LandNode_logit$logit.exponent[ match( L213.LN3_Logit$LandNode3, A_LandNode_logit$LandNode ) ]
L213.LN3_Logit$logit.exponent[ L213.LN3_Logit$AEZ %in% AEZs_most_arid ] <- A_LandNode_logit$logit.exponent.arid[
      match( L213.LN3_Logit$LandNode3[ L213.LN3_Logit$AEZ %in% AEZs_most_arid ], A_LandNode_logit$LandNode ) ]

#Append AEZ names and keep only relevant columns
L213.LN3_Logit <- append_AEZ( L213.LN3_Logit, var1 = "LandNode1", var2 = "LandNode2", var3 = "LandNode3" )
L213.LN3_Logit <- L213.LN3_Logit[ names_LN3_Logit ]

printlog( "L213.LN3_DefaultShare: Default shares for new technologies in specified years" )
L213.LN3_DefaultShare <- L213.LN3_Logit[ names( L213.LN3_Logit ) %in% names_LN3_DefaultShare ]
#Default shares do not interpolate in the model, so write it out in all model future years (starting with first bio year)
L213.LN3_DefaultShare <- repeat_and_add_vector( L213.LN3_DefaultShare, Y, model_future_years[ model_future_years >= Bio_start_year ] )
L213.LN3_DefaultShare$default.share <- approx( x = A_bio_default_share$year, y = A_bio_default_share$default.share, xout = L213.LN3_DefaultShare$year, rule = 2 )$y

#LAND USE HISTORY
#Unmanaged land
L213.LC_bm2_R_Unmgd3_Yh_AEZ <- subset( L125.LC_bm2_R_LT_Yh_AEZ, Land_Type %in% A_LandLeaf_Unmgd3$UnmanagedLandLeaf )
L213.LC_bm2_R_Unmgd3_Yh_AEZ.melt <- melt( L213.LC_bm2_R_Unmgd3_Yh_AEZ, id.vars = R_LT_AEZ, variable.name = "Xyear" )
L213.LC_bm2_R_Unmgd3_Yh_AEZ.melt <- add_region_name( L213.LC_bm2_R_Unmgd3_Yh_AEZ.melt )
L213.LC_bm2_R_Unmgd3_Yh_AEZ.melt[[Y]] <- sub( "X", "", L213.LC_bm2_R_Unmgd3_Yh_AEZ.melt$Xyear )
L213.LC_bm2_R_Unmgd3_Yh_AEZ.melt$allocation <- L213.LC_bm2_R_Unmgd3_Yh_AEZ.melt$value
L213.LC_bm2_R_Unmgd3_Yh_AEZ.melt <- add_node_leaf_names( L213.LC_bm2_R_Unmgd3_Yh_AEZ.melt,
       nesting_table = A_LandLeaf_Unmgd3, leaf_name = "UnmanagedLandLeaf", LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3" )

printlog( "L213.LN3_HistUnmgdAllocation: Historical land cover, unmanaged land in the third nest" )
L213.LN3_HistUnmgdAllocation <- L213.LC_bm2_R_Unmgd3_Yh_AEZ.melt[ L213.LC_bm2_R_Unmgd3_Yh_AEZ.melt[[Y]] %in% land_history_years, names_LN3_HistUnmgdAllocation ]

printlog( "L213.LN3_UnmgdAllocation: Model base period land cover, unmanaged land in the third nest" )
L213.LN3_UnmgdAllocation <- L213.LC_bm2_R_Unmgd3_Yh_AEZ.melt[ L213.LC_bm2_R_Unmgd3_Yh_AEZ.melt[[Y]] %in% model_base_years, names_LN3_UnmgdAllocation ]

#Managed land - non-crop
L213.LC_bm2_R_Mgd3_Yh_AEZ <- subset( L125.LC_bm2_R_LT_Yh_AEZ, Land_Type %in% A_LandLeaf3$LandLeaf )
L213.LC_bm2_R_Mgd3_Yh_AEZ.melt <- melt( L213.LC_bm2_R_Mgd3_Yh_AEZ, id.vars = R_LT_AEZ, variable.name = "Xyear" )
L213.LC_bm2_R_Mgd3_Yh_AEZ.melt <- add_region_name( L213.LC_bm2_R_Mgd3_Yh_AEZ.melt )
L213.LC_bm2_R_Mgd3_Yh_AEZ.melt[[Y]] <- sub( "X", "", L213.LC_bm2_R_Mgd3_Yh_AEZ.melt$Xyear )
L213.LC_bm2_R_Mgd3_Yh_AEZ.melt$allocation <- L213.LC_bm2_R_Mgd3_Yh_AEZ.melt$value
L213.LC_bm2_R_Mgd3_Yh_AEZ.melt <- add_node_leaf_names( L213.LC_bm2_R_Mgd3_Yh_AEZ.melt,
       nesting_table = A_LandLeaf3, leaf_name = "LandLeaf", LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3" )

printlog( "L213.LN3_HistMgdAllocation_noncrop: Historical land cover, managed land in the third nest (noncrop)" )
L213.LN3_HistMgdAllocation_noncrop <- L213.LC_bm2_R_Mgd3_Yh_AEZ.melt[ L213.LC_bm2_R_Mgd3_Yh_AEZ.melt[[Y]] %in% land_history_years, names_LN3_HistMgdAllocation ]

printlog( "L213.LN3_MgdAllocation_noncrop: Model base period land cover, managed land in the third nest (noncrop)" )
L213.LN3_MgdAllocation_noncrop <- L213.LC_bm2_R_Mgd3_Yh_AEZ.melt[ L213.LC_bm2_R_Mgd3_Yh_AEZ.melt[[Y]] %in% model_base_years, names_LN3_MgdAllocation ]

#Managed land - crop land
L213.LC_bm2_R_HarvCropLand_C_Yh_AEZ.melt <- melt( L122.LC_bm2_R_HarvCropLand_C_Yh_AEZ, id.vars = R_C_AEZ, variable.name = "Xyear" )
L213.LC_bm2_R_HarvCropLand_C_Yh_AEZ.melt <- add_region_name( L213.LC_bm2_R_HarvCropLand_C_Yh_AEZ.melt )
L213.LC_bm2_R_HarvCropLand_C_Yh_AEZ.melt[[Y]] <- sub( "X", "", L213.LC_bm2_R_HarvCropLand_C_Yh_AEZ.melt$Xyear )
L213.LC_bm2_R_HarvCropLand_C_Yh_AEZ.melt$allocation <- round( L213.LC_bm2_R_HarvCropLand_C_Yh_AEZ.melt$value, digits_land_use )
L213.LC_bm2_R_HarvCropLand_C_Yh_AEZ.melt <- add_node_leaf_names( L213.LC_bm2_R_HarvCropLand_C_Yh_AEZ.melt,
       nesting_table = A_LandLeaf3, leaf_name = "LandLeaf", LT_name = C, LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3" )

printlog( "L213.LN3_HistMgdAllocation_crop: Historical land cover, managed land in the third nest, cropland" )
L213.LN3_HistMgdAllocation_crop <- L213.LC_bm2_R_HarvCropLand_C_Yh_AEZ.melt[ L213.LC_bm2_R_HarvCropLand_C_Yh_AEZ.melt[[Y]] %in% land_history_years, names_LN3_HistMgdAllocation ]

printlog( "L213.LN3_MgdAllocation_crop: Model base year land cover, managed land in the third nest, cropland" )
L213.LN3_MgdAllocation_crop <- L213.LC_bm2_R_HarvCropLand_C_Yh_AEZ.melt[ L213.LC_bm2_R_HarvCropLand_C_Yh_AEZ.melt[[Y]] %in% model_base_years, names_LN3_MgdAllocation ]

#Managed land - biomass
A_LandLeaf3_bio <- subset( A_LandLeaf3, LandLeaf == "biomass" )
L213.LC_bm2_R_bio_Yh_AEZ.melt <- data.frame( region = GCAM_region_names$region, LandAllocatorRoot = "root",
      A_LandLeaf3_bio[ rep( 1, times = length( GCAM_region_names$region ) ), ] )
L213.LC_bm2_R_bio_Yh_AEZ.melt <- repeat_and_add_vector( L213.LC_bm2_R_bio_Yh_AEZ.melt, Y, unique( c( land_history_years, model_base_years ) ) )
L213.LC_bm2_R_bio_Yh_AEZ.melt <- repeat_and_add_vector( L213.LC_bm2_R_bio_Yh_AEZ.melt, AEZ, AEZs )
L213.LC_bm2_R_bio_Yh_AEZ.melt <- append_AEZ( L213.LC_bm2_R_bio_Yh_AEZ.melt, var1 = "LandNode1", var2 = "LandNode2", var3 = "LandNode3", var4 = "LandLeaf" )
L213.LC_bm2_R_bio_Yh_AEZ.melt$allocation <- 0

printlog( "L213.LN3_HistMgdAllocation_bio: Historical land cover, managed land in the third nest, bioenergy" )
L213.LN3_HistMgdAllocation_bio <- L213.LC_bm2_R_bio_Yh_AEZ.melt[ L213.LC_bm2_R_bio_Yh_AEZ.melt[[Y]] %in% land_history_years, names_LN3_HistMgdAllocation ]

printlog( "L213.LN3_MgdAllocation_bio: Model base year land cover, managed land in the third nest, bioenergy" )
L213.LN3_MgdAllocation_bio <- L213.LC_bm2_R_bio_Yh_AEZ.melt[ L213.LC_bm2_R_bio_Yh_AEZ.melt[[Y]] %in% model_base_years, names_LN3_MgdAllocation ]

#CARBON CONTENTS AND MATURE AGES
#Melt tables with compiled carbon contents and mature ages
L213.VegC_kgm2_R_LT_AEZ.melt <- melt( L121.VegC_kgm2_R_LT_AEZ, id.vars = R_LT, variable.name = AEZ )
L213.SoilC_kgm2_R_LT_AEZ.melt <- melt( L121.SoilC_kgm2_R_LT_AEZ, id.vars = R_LT, variable.name = AEZ )
L213.MatureAge_R_LT_AEZ.melt <- melt( L121.MatureAge_R_LT_AEZ, id.vars = R_LT, variable.name = AEZ )

#Add region vector
L213.VegC_kgm2_R_LT_AEZ.melt <- add_region_name( L213.VegC_kgm2_R_LT_AEZ.melt )
L213.SoilC_kgm2_R_LT_AEZ.melt <- add_region_name( L213.SoilC_kgm2_R_LT_AEZ.melt )
L213.MatureAge_R_LT_AEZ.melt <- add_region_name( L213.MatureAge_R_LT_AEZ.melt )

printlog( "L213.LN3_UnmgdCarbon: Carbon content info, unmanaged land in the third nest" )
L213.LN3_UnmgdCarbon <- data.frame(
      region = rep( GCAM_region_names$region, times = nrow( A_LandLeaf_Unmgd3 ) ),
      Land_Type = sort( rep( A_LandLeaf_Unmgd3$UnmanagedLandLeaf, times = nrow( GCAM_region_names ) ) ) )
L213.LN3_UnmgdCarbon$Cdensity_LT <- GCAMLandLeaf_CdensityLT[[LT]][
      match( L213.LN3_UnmgdCarbon[[LT]], GCAMLandLeaf_CdensityLT$LandLeaf) ]
L213.LN3_UnmgdCarbon <- repeat_and_add_vector( L213.LN3_UnmgdCarbon, AEZ, AEZs )
L213.LN3_UnmgdCarbon <- add_node_leaf_names( L213.LN3_UnmgdCarbon,
      nesting_table = A_LandLeaf_Unmgd3, leaf_name = "UnmanagedLandLeaf", LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3" )

#Match in carbon densities and mature age and sort the columns to match the model interface header
L213.LN3_UnmgdCarbon <- add_carbon_info( L213.LN3_UnmgdCarbon,
      veg_data = L213.VegC_kgm2_R_LT_AEZ.melt, soil_data = L213.SoilC_kgm2_R_LT_AEZ.melt, age_data = L213.MatureAge_R_LT_AEZ.melt )
L213.LN3_UnmgdCarbon <- L213.LN3_UnmgdCarbon[ names_LN3_UnmgdCarbon ]

#Managed carbon in the third next. Ag and bio values will be over-written later
L213.LN3_MgdCarbon <- data.frame(
      region = rep( GCAM_region_names$region, times = nrow( A_LandLeaf3 ) ),
      Land_Type = sort( rep( A_LandLeaf3$LandLeaf, times = nrow( GCAM_region_names ) ) ) )
L213.LN3_MgdCarbon$Cdensity_LT <- GCAMLandLeaf_CdensityLT[[LT]][
      match( L213.LN3_MgdCarbon[[LT]], GCAMLandLeaf_CdensityLT$LandLeaf) ]
L213.LN3_MgdCarbon <- repeat_and_add_vector( L213.LN3_MgdCarbon, AEZ, AEZs )
L213.LN3_MgdCarbon <- add_node_leaf_names( L213.LN3_MgdCarbon,
      nesting_table = A_LandLeaf3, leaf_name = "LandLeaf", LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3" )

#Match in carbon densities and mature age and sort the columns to match the model interface header
L213.LN3_MgdCarbon <- add_carbon_info( L213.LN3_MgdCarbon,
      veg_data = L213.VegC_kgm2_R_LT_AEZ.melt, soil_data = L213.SoilC_kgm2_R_LT_AEZ.melt, age_data = L213.MatureAge_R_LT_AEZ.melt )

printlog( "L213.LN3_MgdCarbon_noncrop: Carbon content info, managed land in the third nest (noncrop)" )
L213.LN3_MgdCarbon_noncrop <- L213.LN3_MgdCarbon[ L213.LN3_MgdCarbon[[LT]] %!in% c( "biomass", L213.LC_bm2_R_HarvCropLand_C_Yh_AEZ.melt[[C]] ), names_LN3_MgdCarbon ]

printlog( "L213.LN3_MgdCarbon_crop: Carbon content info, managed land in the third nest, cropland" )
L213.LN3_MgdCarbon_crop <- L213.LN3_MgdCarbon[ L213.LN3_MgdCarbon[[LT]] %in% L213.LC_bm2_R_HarvCropLand_C_Yh_AEZ.melt[[C]], ]

# Set Forests to use node-carbon-calc at the node level and no-emiss-carbon-calc at the leaf
# to allow them to switch between each other without lots of emissions+uptake
printlog( "L213.LN3_NoEmissCarbon: Set the no-emiss-carbon-calc as the type of carbon to use in forest leaves" )
L213.LN3_NoEmissCarbon <- subset( L213.LN3_UnmgdCarbon, grepl( "Forest", UnmanagedLandLeaf ),
    select=c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "UnmanagedLandLeaf" ) )
names( L213.LN3_NoEmissCarbon ) <- sub( 'Unmanaged', '', names( L213.LN3_NoEmissCarbon ) )
L213.LN3_NoEmissCarbon <- rbind( L213.LN3_NoEmissCarbon, subset( L213.LN3_MgdCarbon, grepl( "Forest", Cdensity_LT ),
    select=c( "region", "LandAllocatorRoot", "LandNode1", "LandNode2", "LandNode3", "LandLeaf" ) ) )
# Just need to create an empty tag, to do so we have to have a whitespace column and an
# extra column with extraneous data
L213.LN3_NoEmissCarbon$no.emiss.carbon.calc <- " "
L213.LN3_NoEmissCarbon$extra <- "junk"

# Put the node carbon calc in the node just above the leaves
printlog( "L213.LN3_NodeCarbon: Set the node-carbon-calc to drive the carbon calc between forest leaves" )
L213.LN3_NodeCarbon <- L213.LN3_NoEmissCarbon
L213.LN3_NodeCarbon$LandLeaf <- NULL
L213.LN3_NodeCarbon <- unique( L213.LN3_NodeCarbon )
names( L213.LN3_NodeCarbon ) <- sub( 'no.emiss', 'node', names( L213.LN3_NodeCarbon ) )

# Create node equivalence lists so that we don't have to subset and create a bunch of extra
# tables for reading in the *CarbonCalc data
# TODO: better place for these?  they are related to headers since they list tag names
printlog( "L213.NodeEquiv: Node tag equivalence list to minimize extra tables to read in same params" )
L213.NodeEquiv <- data.frame( group.name=c("Leaf"), tag1=c("LandLeaf"), tag2=c("UnmanagedLandLeaf"), stringsAsFactors=FALSE )
L213.NodeEquiv <- rbind( L213.NodeEquiv, c( "CarbonCalc", "land-carbon-densities", "no-emiss-carbon-calc") )

#CROPLAND VEGETATION CARBON DENSITY CALCULATION
#Add region ID vector to residue biomass table
L213.ag_resbio_R_C <- add_region_name( L111.ag_resbio_R_C )

#Melt yield table in final calibration year, and paste in region ID vector
L213.ag_EcYield_kgm2_R_C_fby_AEZ <- L122.ag_EcYield_kgm2_R_C_Y_AEZ[ c( R_C_AEZ, X_final_historical_year ) ]
L213.ag_EcYield_kgm2_R_C_fby_AEZ<- add_region_name( L213.ag_EcYield_kgm2_R_C_fby_AEZ )

#Map in information for calculation of cropland vegetation carbon
crop_chars <- c( "HarvestIndex", "WaterContent", "Root_Shoot" )
L213.LN3_MgdCarbon_crop [ crop_chars ] <- L213.ag_resbio_R_C[
      match( vecpaste( L213.LN3_MgdCarbon_crop[ c( "region", LT ) ] ),
             vecpaste( L213.ag_resbio_R_C[ c( "region", C ) ] ) ),
      crop_chars ]
L213.LN3_MgdCarbon_crop$Yield <- L213.ag_EcYield_kgm2_R_C_fby_AEZ[[X_final_historical_year]][  
      match( vecpaste( L213.LN3_MgdCarbon_crop[ c( "region", LT, AEZ ) ] ),
             vecpaste( L213.ag_EcYield_kgm2_R_C_fby_AEZ[ c( "region", C, AEZ ) ] ) ) ]

#Match in harvest index, water content, and root:shoot for fiber and fodder crops based on exogenous assumptions
L213.LN3_MgdCarbon_crop[ L213.LN3_MgdCarbon_crop[[LT]]%in% A_Fodderbio_chars[[C]], crop_chars ] <- A_Fodderbio_chars[
      match( L213.LN3_MgdCarbon_crop[[LT]][ L213.LN3_MgdCarbon_crop[[LT]] %in% A_Fodderbio_chars[[C]] ], A_Fodderbio_chars[[C]] ),
      crop_chars ]

#Calculate vegetation carbon density based on the yields and crop characteristics
L213.LN3_MgdCarbon_crop$hist.veg.carbon.density <- 
      round( L213.LN3_MgdCarbon_crop$Yield / ( L213.LN3_MgdCarbon_crop$HarvestIndex ) * ( 1 + L213.LN3_MgdCarbon_crop$Root_Shoot ) *
             ( 1 - L213.LN3_MgdCarbon_crop$WaterContent ) * Ccontent_cellulose * Cconv_peak_avg,
      digits_C_density_crop )
#Replace missing values with the default values      
L213.LN3_MgdCarbon_crop$hist.veg.carbon.density[ is.na( L213.LN3_MgdCarbon_crop$hist.veg.carbon.density ) ] <-
      L213.LN3_MgdCarbon_crop$veg.carbon.density[ is.na( L213.LN3_MgdCarbon_crop$hist.veg.carbon.density ) ]
L213.LN3_MgdCarbon_crop$hist.veg.carbon.density[ L213.LN3_MgdCarbon_crop$hist.veg.carbon.density == Inf ] <-
      L213.LN3_MgdCarbon_crop$veg.carbon.density[ L213.LN3_MgdCarbon_crop$hist.veg.carbon.density == Inf ]
L213.LN3_MgdCarbon_crop$veg.carbon.density <- L213.LN3_MgdCarbon_crop$hist.veg.carbon.density
L213.LN3_MgdCarbon_crop <- L213.LN3_MgdCarbon_crop[ names_LN3_MgdCarbon ]

printlog( "L213.LN3_MgdCarbon_bio_ref: Carbon content info, managed land in the third nest, bioenergy (ref yields)" )
L213.LN3_MgdCarbon_bio_ref <- L213.LN3_MgdCarbon[ L213.LN3_MgdCarbon[[LT]] == "biomass", ]

printlog( "Renaming biocrops before calculating vegetative carbon contents" )
L213.LN3_MgdCarbon_bio_ref <- rename_biocrops( L213.LN3_MgdCarbon_bio_ref, lookup = A_biocrops_R_AEZ,
      data_matchvar = "LandLeaf", lookup_matchvar = "old_LandLeaf", "LandNode2", "LandNode3", "LandLeaf" )

#Biomass vegetation carbon density calculation
L213.LN3_MgdCarbon_bio_ref[ crop_chars ] <- A_Fodderbio_chars[ match( L213.LN3_MgdCarbon_bio_ref[[LT]], A_Fodderbio_chars[[C]] ), crop_chars ]

#Biomass yield
L213.ag_bioYield_GJm2_R_AEZ_ref.melt <- melt( L113.ag_bioYield_GJm2_R_AEZ_ref, id.vars = R, variable.name = AEZ )
L213.ag_bioYield_GJm2_R_AEZ_ref.melt <- add_region_name( L213.ag_bioYield_GJm2_R_AEZ_ref.melt )

#Match in biomass yield, converting from GJ/m2 to kg/m2
L213.LN3_MgdCarbon_bio_ref$Yield <- L213.ag_bioYield_GJm2_R_AEZ_ref.melt$value[  
      match( vecpaste( L213.LN3_MgdCarbon_bio_ref[ c( "region", AEZ ) ] ),
             vecpaste( L213.ag_bioYield_GJm2_R_AEZ_ref.melt[ c( "region", AEZ ) ] ) ) ] /
      ( bio_GJt * conv_kg_t )

#Calculate the veg carbon content. Assume that the crop is perennial so the root portion doesn't get multiplied by the peak->avg conversion
L213.LN3_MgdCarbon_bio_ref$hist.veg.carbon.density <-
      round( L213.LN3_MgdCarbon_bio_ref$Yield / ( L213.LN3_MgdCarbon_bio_ref$HarvestIndex + 1e-6 ) * ( 1 - L213.LN3_MgdCarbon_bio_ref$WaterContent ) *
             Ccontent_cellulose * Cconv_peak_avg +
             L213.LN3_MgdCarbon_bio_ref$Yield / ( L213.LN3_MgdCarbon_bio_ref$HarvestIndex + 1e-6 ) * ( 1 - L213.LN3_MgdCarbon_bio_ref$WaterContent ) *
             Ccontent_cellulose * L213.LN3_MgdCarbon_bio_ref$Root_Shoot,
      digits_C_density_crop ) 
L213.LN3_MgdCarbon_bio_ref$veg.carbon.density <- L213.LN3_MgdCarbon_bio_ref$hist.veg.carbon.density

printlog( "L213.LN3_MgdCarbon_bio_hi: Carbon content info, managed land in the third nest, bioenergy (hi yields)" )
L213.ag_bioYield_GJm2_R_AEZ_hi.melt <- melt( L113.ag_bioYield_GJm2_R_AEZ_hi, id.vars = R, variable.name = AEZ )
L213.ag_bioYield_GJm2_R_AEZ_hi.melt <- add_region_name( L213.ag_bioYield_GJm2_R_AEZ_hi.melt )

#Replace biomass yields in this table
L213.LN3_MgdCarbon_bio_hi <- L213.LN3_MgdCarbon_bio_ref
L213.LN3_MgdCarbon_bio_hi$Yield <-  L213.ag_bioYield_GJm2_R_AEZ_hi.melt$value[ 
      match( vecpaste( L213.LN3_MgdCarbon_bio_hi[ c( "region", AEZ ) ] ),
             vecpaste( L213.ag_bioYield_GJm2_R_AEZ_hi.melt[ c( "region", AEZ ) ] ) ) ] /
      ( bio_GJt * conv_kg_t )

#Recalculate vegetation carbon content, using the higher yields
L213.LN3_MgdCarbon_bio_hi$hist.veg.carbon.density <-
      round( L213.LN3_MgdCarbon_bio_hi$Yield / ( L213.LN3_MgdCarbon_bio_hi$HarvestIndex + 1e-6 ) * ( 1 - L213.LN3_MgdCarbon_bio_hi$WaterContent ) *
             Ccontent_cellulose * Cconv_peak_avg +
             L213.LN3_MgdCarbon_bio_hi$Yield / ( L213.LN3_MgdCarbon_bio_hi$HarvestIndex + 1e-6 ) * ( 1 - L213.LN3_MgdCarbon_bio_hi$WaterContent ) *
             Ccontent_cellulose * L213.LN3_MgdCarbon_bio_hi$Root_Shoot,
      digits_C_density_crop ) 
L213.LN3_MgdCarbon_bio_hi$veg.carbon.density <- L213.LN3_MgdCarbon_bio_hi$hist.veg.carbon.density

#Set minimum carbon densities for biomassOil technologies
L213.LN3_MgdCarbon_bio_ref$min.veg.carbon.density <- ifelse( grepl( "Jatropha", L213.LN3_MgdCarbon_bio_ref$LandLeaf ),  L213.LN3_MgdCarbon_bio_ref$veg.carbon.density, 0 )
L213.LN3_MgdCarbon_bio_ref$min.soil.carbon.density <- ifelse( grepl( "Jatropha", L213.LN3_MgdCarbon_bio_ref$LandLeaf ),  L213.LN3_MgdCarbon_bio_ref$soil.carbon.density, 0 )
L213.LN3_MgdCarbon_bio_hi$min.veg.carbon.density <- ifelse( grepl( "Jatropha", L213.LN3_MgdCarbon_bio_hi$LandLeaf ),  L213.LN3_MgdCarbon_bio_hi$veg.carbon.density, 0 )
L213.LN3_MgdCarbon_bio_hi$min.soil.carbon.density <- ifelse( grepl( "Jatropha", L213.LN3_MgdCarbon_bio_hi$LandLeaf ),  L213.LN3_MgdCarbon_bio_hi$soil.carbon.density, 0 )

#Write out only the names to be written to XML
L213.LN3_MgdCarbon_bio_ref <- L213.LN3_MgdCarbon_bio_ref[ names_LN3_MgdCarbon ]
L213.LN3_MgdCarbon_bio_hi <- L213.LN3_MgdCarbon_bio_hi[ names_LN3_MgdCarbon ]

printlog( "L213.LN3_NewTech: Specify the start year for purpose-grown biomass" )
L213.LN3_NewTech <- L213.LN3_MgdCarbon_bio_ref[ names( L213.LN3_MgdCarbon_bio_ref ) %in% names_LN3_NewTech ]
L213.LN3_NewTech$year.fillout <- Bio_start_year
L213.LN3_NewTech$isNewTechnology <- 1

printlog( "Re-designating biomass land nodes for specified bioenergy crops, where indicated" )
L213.LN3_HistMgdAllocation_bio <- rename_biocrops( L213.LN3_HistMgdAllocation_bio, lookup = A_biocrops_R_AEZ,
      data_matchvar = "LandLeaf", lookup_matchvar = "old_LandLeaf", "LandNode2", "LandNode3", "LandLeaf" )
L213.LN3_MgdAllocation_bio <- rename_biocrops( L213.LN3_MgdAllocation_bio, lookup = A_biocrops_R_AEZ,
      data_matchvar = "LandLeaf", lookup_matchvar = "old_LandLeaf", "LandNode2", "LandNode3", "LandLeaf" )
L213.LN3_NewTech <- rename_biocrops( L213.LN3_NewTech, lookup = A_biocrops_R_AEZ,
      data_matchvar = "LandLeaf", lookup_matchvar = "old_LandLeaf", "LandNode2", "LandNode3", "LandLeaf" )

printlog( "Removing non-existent AEZs from all tables" )
L213.LN3_Logit <- remove_AEZ_nonexist( L213.LN3_Logit, AEZcol = "LandNode1" )
L213.LN3_DefaultShare <- remove_AEZ_nonexist( L213.LN3_DefaultShare, AEZcol = "LandNode1" )
L213.LN3_HistUnmgdAllocation <- remove_AEZ_nonexist( L213.LN3_HistUnmgdAllocation, AEZcol = "LandNode1" )
L213.LN3_UnmgdAllocation <- remove_AEZ_nonexist( L213.LN3_UnmgdAllocation, AEZcol = "LandNode1" )
L213.LN3_HistMgdAllocation_noncrop <- remove_AEZ_nonexist( L213.LN3_HistMgdAllocation_noncrop, AEZcol = "LandNode1" )
L213.LN3_MgdAllocation_noncrop <- remove_AEZ_nonexist( L213.LN3_MgdAllocation_noncrop, AEZcol = "LandNode1" )
L213.LN3_HistMgdAllocation_crop <- remove_AEZ_nonexist( L213.LN3_HistMgdAllocation_crop, AEZcol = "LandNode1" )
L213.LN3_MgdAllocation_crop <- remove_AEZ_nonexist( L213.LN3_MgdAllocation_crop, AEZcol = "LandNode1" )
L213.LN3_HistMgdAllocation_bio <- remove_AEZ_nonexist( L213.LN3_HistMgdAllocation_bio, AEZcol = "LandNode1" )
L213.LN3_MgdAllocation_bio <- remove_AEZ_nonexist( L213.LN3_MgdAllocation_bio, AEZcol = "LandNode1" )
L213.LN3_UnmgdCarbon <- remove_AEZ_nonexist( L213.LN3_UnmgdCarbon, AEZcol = "LandNode1" )
L213.LN3_MgdCarbon_noncrop <- remove_AEZ_nonexist( L213.LN3_MgdCarbon_noncrop, AEZcol = "LandNode1" )
L213.LN3_MgdCarbon_crop <- remove_AEZ_nonexist( L213.LN3_MgdCarbon_crop, AEZcol = "LandNode1" )
L213.LN3_MgdCarbon_bio_ref <- remove_AEZ_nonexist( L213.LN3_MgdCarbon_bio_ref, AEZcol = "LandNode1" )
L213.LN3_MgdCarbon_bio_hi <- remove_AEZ_nonexist( L213.LN3_MgdCarbon_bio_hi, AEZcol = "LandNode1" )
L213.LN3_NewTech <- remove_AEZ_nonexist( L213.LN3_NewTech, AEZcol = "LandNode1" )
L213.LN3_NoEmissCarbon <- remove_AEZ_nonexist( L213.LN3_NoEmissCarbon, AEZcol="LandNode1" )
L213.LN3_NodeCarbon <- remove_AEZ_nonexist( L213.LN3_NodeCarbon, AEZcol="LandNode1" )

#May as well remove the nodes from "default share" table that don't have new technologies
L213.LN3_DefaultShare <- subset( L213.LN3_DefaultShare, paste( region, LandNode1, LandNode2, LandNode3 ) %in%
      paste( L213.LN3_NewTech$region, L213.LN3_NewTech$LandNode1, L213.LN3_NewTech$LandNode2, L213.LN3_NewTech$LandNode3 ) )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L213.LN3_Logit, IDstring="LN3_Logit", domain="AGLU_LEVEL2_DATA", fn="L213.LN3_Logit",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_land_input_3.xml" )
write_mi_data( L213.LN3_DefaultShare, "LN3_DefaultShare", "AGLU_LEVEL2_DATA", "L213.LN3_DefaultShare", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L213.LN3_HistUnmgdAllocation, "LN3_HistUnmgdAllocation", "AGLU_LEVEL2_DATA", "L213.LN3_HistUnmgdAllocation", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L213.LN3_UnmgdAllocation, "LN3_UnmgdAllocation", "AGLU_LEVEL2_DATA", "L213.LN3_UnmgdAllocation", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L213.NodeEquiv, "EQUIV_TABLE", "AGLU_LEVEL2_DATA", "L213.NodeEquiv", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L213.LN3_NoEmissCarbon, "LN3_NoEmissCarbon", "AGLU_LEVEL2_DATA", "L213.LN3_NoEmissCarbon", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L213.LN3_NodeCarbon, "LN3_NodeCarbon", "AGLU_LEVEL2_DATA", "L213.LN3_NodeCarbon", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L213.LN3_HistMgdAllocation_noncrop, "LN3_HistMgdAllocation", "AGLU_LEVEL2_DATA", "L213.LN3_HistMgdAllocation_noncrop", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L213.LN3_MgdAllocation_noncrop, "LN3_MgdAllocation", "AGLU_LEVEL2_DATA", "L213.LN3_MgdAllocation_noncrop", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L213.LN3_HistMgdAllocation_crop, "LN3_HistMgdAllocation", "AGLU_LEVEL2_DATA", "L213.LN3_HistMgdAllocation_crop", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L213.LN3_MgdAllocation_crop, "LN3_MgdAllocation", "AGLU_LEVEL2_DATA", "L213.LN3_MgdAllocation_crop", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L213.LN3_HistMgdAllocation_bio, "LN3_HistMgdAllocation", "AGLU_LEVEL2_DATA", "L213.LN3_HistMgdAllocation_bio", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L213.LN3_MgdAllocation_bio, "LN3_MgdAllocation", "AGLU_LEVEL2_DATA", "L213.LN3_MgdAllocation_bio", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L213.LN3_UnmgdCarbon, "LN3_UnmgdCarbon", "AGLU_LEVEL2_DATA", "L213.LN3_UnmgdCarbon", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L213.LN3_MgdCarbon_noncrop, "LN3_MgdCarbon", "AGLU_LEVEL2_DATA", "L213.LN3_MgdCarbon_noncrop", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L213.LN3_MgdCarbon_crop, "LN3_MgdCarbon", "AGLU_LEVEL2_DATA", "L213.LN3_MgdCarbon_crop", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L213.LN3_MgdCarbon_bio_ref, "LN3_MgdCarbon", "AGLU_LEVEL2_DATA", "L213.LN3_MgdCarbon_bio_ref", "AGLU_XML_BATCH", "batch_land_input_3.xml" )
write_mi_data( L213.LN3_MgdCarbon_bio_hi, "LN3_MgdCarbon", "AGLU_LEVEL2_DATA", "L213.LN3_MgdCarbon_bio_hi", "AGLU_XML_BATCH", "batch_bio_hi.xml", node_rename=T )
write_mi_data( L213.LN3_NewTech, "LN3_NewTech", "AGLU_LEVEL2_DATA", "L213.LN3_NewTech", "AGLU_XML_BATCH", "batch_land_input_3.xml", node_rename=T )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_land_input_3.xml", "AGLU_XML_FINAL", "land_input_3.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_bio_hi.xml", "AGLU_XML_FINAL", "bio_hi.xml", "", xml_tag="outFile" )

logstop()
