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
logstart( "L234.land_input_4_irr.R" )
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
GCAMLandLeaf_CdensityLT <- readdata( "AGLU_MAPPINGS", "GCAMLandLeaf_CdensityLT" )
A_biocrops_R_AEZ_irr <- readdata( "AGLU_ASSUMPTIONS", "A_biocrops_R_AEZ_irr" )
A_Fodderbio_chars <- readdata( "AGLU_ASSUMPTIONS", "A_Fodderbio_chars" )
A_LandNode_logit_irr <- readdata( "AGLU_ASSUMPTIONS", "A_LandNode_logit_irr" )
A_LandNode4_irr <- readdata( "AGLU_ASSUMPTIONS", "A_LandNode4_irr" )
A_LandLeaf4_irr <- readdata( "AGLU_ASSUMPTIONS", "A_LandLeaf4_irr" )
L111.ag_resbio_R_C <- readdata( "AGLU_LEVEL1_DATA", "L111.ag_resbio_R_C" )
L163.ag_rfdBioYield_GJm2_R_AEZ_ref <- readdata( "AGLU_LEVEL1_DATA", "L163.ag_rfdBioYield_GJm2_R_AEZ_ref" )
L163.ag_rfdBioYield_GJm2_R_AEZ_hi <- readdata( "AGLU_LEVEL1_DATA", "L163.ag_rfdBioYield_GJm2_R_AEZ_hi" )
L163.ag_irrBioYield_GJm2_R_AEZ_ref <- readdata( "AGLU_LEVEL1_DATA", "L163.ag_irrBioYield_GJm2_R_AEZ_ref" )
L163.ag_irrBioYield_GJm2_R_AEZ_hi <- readdata( "AGLU_LEVEL1_DATA", "L163.ag_irrBioYield_GJm2_R_AEZ_hi" )
L121.VegC_kgm2_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.VegC_kgm2_R_LT_AEZ" )
L121.SoilC_kgm2_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.SoilC_kgm2_R_LT_AEZ" )
L121.MatureAge_R_LT_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L121.MatureAge_R_LT_AEZ" )
L171.ag_irrEcYield_kgm2_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L171.ag_irrEcYield_kgm2_R_C_Y_AEZ" )
L171.ag_rfdEcYield_kgm2_R_C_Y_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L171.ag_rfdEcYield_kgm2_R_C_Y_AEZ" )
L171.LC_bm2_R_irrHarvCropLand_C_Yh_AEZ <- readdata ( "AGLU_LEVEL1_DATA", "L171.LC_bm2_R_irrHarvCropLand_C_Yh_AEZ" )
L171.LC_bm2_R_rfdHarvCropLand_C_Yh_AEZ <- readdata ( "AGLU_LEVEL1_DATA", "L171.LC_bm2_R_rfdHarvCropLand_C_Yh_AEZ" )
L125.R_AEZ_nonexist <- readdata( "AGLU_LEVEL1_DATA", "L125.R_AEZ_nonexist" )

# -----------------------------------------------------------------------------
# 2. Build tables
#Build node base tables. Match in upper-level nodes with lower-level nodes to ensure correspondence.
printlog( "L234.LN4_Logit: Logit exponent, fourth nest" )
L234.LN4_Logit <- data.frame(
      region = rep( GCAM_region_names$region, times = nrow( A_LandNode4_irr ) ),
      LandAllocatorRoot = "root",
      LandNode4 = sort( rep( A_LandNode4_irr$LandNode4, times = length( GCAM_region_names$region ) ) ) )
L234.LN4_Logit[ c( "LandNode1", "LandNode2", "LandNode3" ) ] <- A_LandNode4_irr[
      match( L234.LN4_Logit$LandNode4, A_LandNode4_irr$LandNode4 ),
      c( "LandNode1", "LandNode2", "LandNode3" ) ]
L234.LN4_Logit <- repeat_and_add_vector( L234.LN4_Logit, AEZ, AEZs )

#Match in logit exponents based on the land node 4
L234.LN4_Logit$logit.year.fillout <- min( model_base_years )
L234.LN4_Logit$logit.exponent <- A_LandNode_logit_irr$logit.exponent[ match( L234.LN4_Logit$LandNode4, A_LandNode_logit_irr$LandNode ) ]
L234.LN4_Logit$logit.exponent[ L234.LN4_Logit$AEZ %in% AEZs_most_arid ] <- A_LandNode_logit_irr$logit.exponent.arid[
      match( L234.LN4_Logit$LandNode4[ L234.LN4_Logit$AEZ %in% AEZs_most_arid ], A_LandNode_logit_irr$LandNode ) ]

#Append AEZ names and keep only relevant columns
L234.LN4_Logit <- append_AEZ( L234.LN4_Logit, var1 = "LandNode1", var2 = "LandNode2", var3 = "LandNode3", var4 = "LandNode4" )
L234.LN4_Logit <- L234.LN4_Logit[ names_LN4_Logit ]

#LAND USE HISTORY
#Managed land - crop land
L171.LC_bm2_R_irrHarvCropLand_C_Yh_AEZ[[irr]] <- "IRR"
L171.LC_bm2_R_rfdHarvCropLand_C_Yh_AEZ[[irr]] <- "RFD"
L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irr <- rbind( L171.LC_bm2_R_irrHarvCropLand_C_Yh_AEZ, L171.LC_bm2_R_rfdHarvCropLand_C_Yh_AEZ )

L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irr.melt <- melt( L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irr, id.vars = R_C_AEZ_irr,
      variable.name = "Xyear", value.name = "allocation" )
L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irr.melt <- add_region_name( L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irr.melt )
L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irr.melt[[Y]] <- sub( "X", "", L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irr.melt$Xyear )
L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irr.melt$allocation <- round( L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irr.melt$allocation, digits_land_use )
L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irr.melt <- add_node_leaf_names( L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irr.melt,
       nesting_table = A_LandLeaf4_irr, leaf_name = "LandLeaf", LT_name = C, LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3", LN4 = "LandNode4" )

printlog( "L234.LN4_HistMgdAllocation_crop: Historical land cover, managed land in the fourth nest, cropland" )
L234.LN4_HistMgdAllocation_crop <- L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irr.melt[ L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irr.melt[[Y]] %in% land_history_years, names_LN4_HistMgdAllocation ]

printlog( "L234.LN4_MgdAllocation_crop: Model base year land cover, managed land in the fourth nest, cropland" )
L234.LN4_MgdAllocation_crop <- L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irr.melt[ L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irr.melt[[Y]] %in% model_base_years, names_LN4_MgdAllocation ]

#Managed land - biomass
A_LandLeaf4_bio <- subset( A_LandLeaf4_irr, LandLeaf == "biomass" )
L234.LC_bm2_R_bio_Yh_AEZ_irr.melt <- data.frame( region = GCAM_region_names$region, LandAllocatorRoot = "root",
      A_LandLeaf4_bio[ rep( 1, times = length( GCAM_region_names$region ) ), ] )
L234.LC_bm2_R_bio_Yh_AEZ_irr.melt <- repeat_and_add_vector( L234.LC_bm2_R_bio_Yh_AEZ_irr.melt, Y, unique( c( land_history_years, model_base_years ) ) )
L234.LC_bm2_R_bio_Yh_AEZ_irr.melt <- repeat_and_add_vector( L234.LC_bm2_R_bio_Yh_AEZ_irr.melt, AEZ, AEZs )
L234.LC_bm2_R_bio_Yh_AEZ_irr.melt <- repeat_and_add_vector( L234.LC_bm2_R_bio_Yh_AEZ_irr.melt, irr, c( "IRR", "RFD" ) )
L234.LC_bm2_R_bio_Yh_AEZ_irr.melt <- append_AEZ( L234.LC_bm2_R_bio_Yh_AEZ_irr.melt,
      var1 = "LandNode1", var2 = "LandNode2", var3 = "LandNode3", var4 = "LandNode4", var5 = "LandLeaf" )
L234.LC_bm2_R_bio_Yh_AEZ_irr.melt$LandLeaf <- paste( L234.LC_bm2_R_bio_Yh_AEZ_irr.melt$LandLeaf, L234.LC_bm2_R_bio_Yh_AEZ_irr.melt[[irr]], sep = AEZ_delimiter )
L234.LC_bm2_R_bio_Yh_AEZ_irr.melt$allocation <- 0

printlog( "L234.LN4_HistMgdAllocation_bio: Historical land cover, managed land in the third nest, bioenergy" )
L234.LN4_HistMgdAllocation_bio <- L234.LC_bm2_R_bio_Yh_AEZ_irr.melt[ L234.LC_bm2_R_bio_Yh_AEZ_irr.melt[[Y]] %in% land_history_years, names_LN4_HistMgdAllocation ]

printlog( "L234.LN4_MgdAllocation_bio: Model base year land cover, managed land in the third nest, bioenergy" )
L234.LN4_MgdAllocation_bio <- L234.LC_bm2_R_bio_Yh_AEZ_irr.melt[ L234.LC_bm2_R_bio_Yh_AEZ_irr.melt[[Y]] %in% model_base_years, names_LN4_MgdAllocation ]

#CARBON CONTENTS AND MATURE AGES
#Melt tables with compiled carbon contents and mature ages
L234.VegC_kgm2_R_LT_AEZ.melt <- melt( L121.VegC_kgm2_R_LT_AEZ, id.vars = R_LT, variable.name = AEZ )
L234.SoilC_kgm2_R_LT_AEZ.melt <- melt( L121.SoilC_kgm2_R_LT_AEZ, id.vars = R_LT, variable.name = AEZ )
L234.MatureAge_R_LT_AEZ.melt <- melt( L121.MatureAge_R_LT_AEZ, id.vars = R_LT, variable.name = AEZ )

#Add region vector
L234.VegC_kgm2_R_LT_AEZ.melt <- add_region_name( L234.VegC_kgm2_R_LT_AEZ.melt )
L234.SoilC_kgm2_R_LT_AEZ.melt <- add_region_name( L234.SoilC_kgm2_R_LT_AEZ.melt )
L234.MatureAge_R_LT_AEZ.melt <- add_region_name( L234.MatureAge_R_LT_AEZ.melt )

printlog( "L234.LN4_MgdCarbon_crop: Carbon content info, managed land in the fourth nest, cropland" )
# Ag and bio vegetation carbon contents will be over-written later
L234.LN4_MgdCarbon <- data.frame(
      region = rep( GCAM_region_names$region, times = nrow( A_LandLeaf4_irr ) ),
      Land_Type = sort( rep( A_LandLeaf4_irr$LandLeaf, times = nrow( GCAM_region_names ) ) ) )
L234.LN4_MgdCarbon$Cdensity_LT <- GCAMLandLeaf_CdensityLT[[LT]][
      match( L234.LN4_MgdCarbon[[LT]], GCAMLandLeaf_CdensityLT$LandLeaf) ]
L234.LN4_MgdCarbon <- repeat_and_add_vector( L234.LN4_MgdCarbon, AEZ, AEZs )
L234.LN4_MgdCarbon <- add_node_leaf_names( L234.LN4_MgdCarbon,
      nesting_table = A_LandLeaf4_irr, leaf_name = "LandLeaf", LN1 = "LandNode1", LN2 = "LandNode2", LN3 = "LandNode3", LN4 = "LandNode4" )
L234.LN4_MgdCarbon <- repeat_and_add_vector( L234.LN4_MgdCarbon, irr, c( "IRR", "RFD" ) )
L234.LN4_MgdCarbon$LandLeaf <- paste( L234.LN4_MgdCarbon$LandLeaf, L234.LN4_MgdCarbon[[irr]], sep = AEZ_delimiter)

#Match in carbon densities and mature age and sort the columns to match the model interface header
L234.LN4_MgdCarbon <- add_carbon_info( L234.LN4_MgdCarbon,
      veg_data = L234.VegC_kgm2_R_LT_AEZ.melt, soil_data = L234.SoilC_kgm2_R_LT_AEZ.melt, age_data = L234.MatureAge_R_LT_AEZ.melt )

#CROPLAND VEGETATION CARBON DENSITY CALCULATION
#Add region ID vector to residue biomass table
L234.ag_resbio_R_C <- add_region_name( L111.ag_resbio_R_C )

#Melt yield table in final calibration year, and paste in region ID vector
L171.ag_irrEcYield_kgm2_R_C_Y_AEZ[[irr]] <- "IRR"
L171.ag_rfdEcYield_kgm2_R_C_Y_AEZ[[irr]] <- "RFD"
L234.ag_EcYield_kgm2_R_C_fby_AEZ_irr <- rbind( L171.ag_irrEcYield_kgm2_R_C_Y_AEZ, L171.ag_rfdEcYield_kgm2_R_C_Y_AEZ )[
      c( R_C_AEZ_irr, X_final_historical_year ) ]
L234.ag_EcYield_kgm2_R_C_fby_AEZ_irr <- add_region_name( L234.ag_EcYield_kgm2_R_C_fby_AEZ_irr )

printlog( "L234.LN4_MgdCarbon_crop: Carbon content info, managed land in the fourth nest, crops" )
#Map in information for calculation of cropland vegetation carbon
crop_chars <- c( "HarvestIndex", "WaterContent", "Root_Shoot" )
L234.LN4_MgdCarbon_crop <- L234.LN4_MgdCarbon[ L234.LN4_MgdCarbon[[LT]] %in% L234.ag_EcYield_kgm2_R_C_fby_AEZ_irr[[C]], ]
L234.LN4_MgdCarbon_crop [ crop_chars ] <- L234.ag_resbio_R_C[
      match( vecpaste( L234.LN4_MgdCarbon_crop[ c( "region", LT ) ] ),
             vecpaste( L234.ag_resbio_R_C[ c( "region", C ) ] ) ),
      crop_chars ]
L234.LN4_MgdCarbon_crop$Yield <- L234.ag_EcYield_kgm2_R_C_fby_AEZ_irr[[X_final_historical_year]][  
      match( vecpaste( L234.LN4_MgdCarbon_crop[ c( "region", LT, AEZ, irr ) ] ),
             vecpaste( L234.ag_EcYield_kgm2_R_C_fby_AEZ_irr[ c( "region", C, AEZ, irr ) ] ) ) ]

#Match in harvest index, water content, and root:shoot for fiber and fodder crops based on exogenous assumptions
L234.LN4_MgdCarbon_crop[ L234.LN4_MgdCarbon_crop[[LT]]%in% A_Fodderbio_chars[[C]], crop_chars ] <- A_Fodderbio_chars[
      match( L234.LN4_MgdCarbon_crop[[LT]][ L234.LN4_MgdCarbon_crop[[LT]] %in% A_Fodderbio_chars[[C]] ], A_Fodderbio_chars[[C]] ),
      crop_chars ]

#Calculate vegetation carbon density based on the yields and crop characteristics
L234.LN4_MgdCarbon_crop$hist.veg.carbon.density <- 
      round( L234.LN4_MgdCarbon_crop$Yield / ( L234.LN4_MgdCarbon_crop$HarvestIndex ) * ( 1 + L234.LN4_MgdCarbon_crop$Root_Shoot ) *
             ( 1 - L234.LN4_MgdCarbon_crop$WaterContent ) * Ccontent_cellulose * Cconv_peak_avg,
      digits_C_density_crop )
#Replace missing values with the default values      
L234.LN4_MgdCarbon_crop$hist.veg.carbon.density[ is.na( L234.LN4_MgdCarbon_crop$hist.veg.carbon.density ) ] <-
      L234.LN4_MgdCarbon_crop$veg.carbon.density[ is.na( L234.LN4_MgdCarbon_crop$hist.veg.carbon.density ) ]
L234.LN4_MgdCarbon_crop$hist.veg.carbon.density[ L234.LN4_MgdCarbon_crop$hist.veg.carbon.density == Inf ] <-
      L234.LN4_MgdCarbon_crop$veg.carbon.density[ L234.LN4_MgdCarbon_crop$hist.veg.carbon.density == Inf ]
L234.LN4_MgdCarbon_crop$veg.carbon.density <- L234.LN4_MgdCarbon_crop$hist.veg.carbon.density
L234.LN4_MgdCarbon_crop <- L234.LN4_MgdCarbon_crop[ names_LN4_MgdCarbon ]

printlog( "L234.LN4_MgdCarbon_bio_ref: Carbon content info, managed land in the fourth nest, bioenergy (ref yields)" )
L234.LN4_MgdCarbon_bio_ref <- L234.LN4_MgdCarbon[ L234.LN4_MgdCarbon[[LT]] == "biomass", ]

printlog( "Renaming biocrops before calculating vegetative carbon contents" )
L234.LN4_MgdCarbon_bio_ref <- rename_biocrops( L234.LN4_MgdCarbon_bio_ref, lookup = A_biocrops_R_AEZ_irr,
      data_matchvar = "LandLeaf", lookup_matchvar = "old_LandLeaf", "LandNode2", "LandNode3", "LandNode4", "LandLeaf" )

#Biomass vegetation carbon density calculation
L234.LN4_MgdCarbon_bio_ref[ crop_chars ] <- A_Fodderbio_chars[ match( L234.LN4_MgdCarbon_bio_ref[[LT]], A_Fodderbio_chars[[C]] ), crop_chars ]

#Biomass yield
L163.ag_irrBioYield_GJm2_R_AEZ_ref[[irr]] <- "IRR"
L163.ag_rfdBioYield_GJm2_R_AEZ_ref[[irr]] <- "RFD"
L234.ag_bioYield_GJm2_R_AEZ_irr_ref <- rbind( L163.ag_irrBioYield_GJm2_R_AEZ_ref, L163.ag_rfdBioYield_GJm2_R_AEZ_ref )
L234.ag_bioYield_GJm2_R_AEZ_irr_ref.melt <- melt( L234.ag_bioYield_GJm2_R_AEZ_irr_ref, id.vars = c( R, irr ), variable.name = AEZ )
L234.ag_bioYield_GJm2_R_AEZ_irr_ref.melt <- add_region_name( L234.ag_bioYield_GJm2_R_AEZ_irr_ref.melt )

#Match in biomass yield, converting from GJ/m2 to kg/m2
L234.LN4_MgdCarbon_bio_ref$Yield <- L234.ag_bioYield_GJm2_R_AEZ_irr_ref.melt$value[  
      match( vecpaste( L234.LN4_MgdCarbon_bio_ref[ c( "region", AEZ, irr ) ] ),
             vecpaste( L234.ag_bioYield_GJm2_R_AEZ_irr_ref.melt[ c( "region", AEZ, irr ) ] ) ) ] /
      ( bio_GJt * conv_kg_t )

#Calculate the veg carbon content. Assume that the crop is perennial so the root portion doesn't get multiplied by the peak->avg conversion
L234.LN4_MgdCarbon_bio_ref$hist.veg.carbon.density <-
      round( L234.LN4_MgdCarbon_bio_ref$Yield / ( L234.LN4_MgdCarbon_bio_ref$HarvestIndex + 1e-6 ) * ( 1 - L234.LN4_MgdCarbon_bio_ref$WaterContent ) *
             Ccontent_cellulose * Cconv_peak_avg +
             L234.LN4_MgdCarbon_bio_ref$Yield / ( L234.LN4_MgdCarbon_bio_ref$HarvestIndex + 1e-6 ) * ( 1 - L234.LN4_MgdCarbon_bio_ref$WaterContent ) *
             Ccontent_cellulose * L234.LN4_MgdCarbon_bio_ref$Root_Shoot,
      digits_C_density_crop ) 
L234.LN4_MgdCarbon_bio_ref$veg.carbon.density <- L234.LN4_MgdCarbon_bio_ref$hist.veg.carbon.density

printlog( "L234.LN4_MgdCarbon_bio_hi: Carbon content info, managed land in the third nest, bioenergy (hi yields)" )
L163.ag_irrBioYield_GJm2_R_AEZ_hi[[irr]] <- "IRR"
L163.ag_rfdBioYield_GJm2_R_AEZ_hi[[irr]] <- "RFD"
L234.ag_bioYield_GJm2_R_AEZ_irr_hi <- rbind( L163.ag_irrBioYield_GJm2_R_AEZ_hi, L163.ag_rfdBioYield_GJm2_R_AEZ_hi )
L234.ag_bioYield_GJm2_R_AEZ_irr_hi.melt <- melt( L234.ag_bioYield_GJm2_R_AEZ_irr_hi, id.vars = c( R, irr ), variable.name = AEZ )
L234.ag_bioYield_GJm2_R_AEZ_irr_hi.melt <- add_region_name( L234.ag_bioYield_GJm2_R_AEZ_irr_hi.melt )

#Replace biomass yields in this table
L234.LN4_MgdCarbon_bio_hi <- L234.LN4_MgdCarbon_bio_ref
L234.LN4_MgdCarbon_bio_hi$Yield <-  L234.ag_bioYield_GJm2_R_AEZ_irr_hi.melt$value[ 
      match( vecpaste( L234.LN4_MgdCarbon_bio_hi[ c( "region", AEZ, irr ) ] ),
             vecpaste( L234.ag_bioYield_GJm2_R_AEZ_irr_hi.melt[ c( "region", AEZ, irr ) ] ) ) ] /
      ( bio_GJt * conv_kg_t )

#Recalculate vegetation carbon content, using the higher yields
L234.LN4_MgdCarbon_bio_hi$hist.veg.carbon.density <-
      round( L234.LN4_MgdCarbon_bio_hi$Yield / ( L234.LN4_MgdCarbon_bio_hi$HarvestIndex + 1e-6 ) * ( 1 - L234.LN4_MgdCarbon_bio_hi$WaterContent ) *
             Ccontent_cellulose * Cconv_peak_avg +
             L234.LN4_MgdCarbon_bio_hi$Yield / ( L234.LN4_MgdCarbon_bio_hi$HarvestIndex + 1e-6 ) * ( 1 - L234.LN4_MgdCarbon_bio_hi$WaterContent ) *
             Ccontent_cellulose * L234.LN4_MgdCarbon_bio_hi$Root_Shoot,
      digits_C_density_crop ) 
L234.LN4_MgdCarbon_bio_hi$veg.carbon.density <- L234.LN4_MgdCarbon_bio_hi$hist.veg.carbon.density

#Write out only the names to be written to XML
L234.LN4_MgdCarbon_bio_ref <- L234.LN4_MgdCarbon_bio_ref[ names_LN4_MgdCarbon ]
L234.LN4_MgdCarbon_bio_hi <- L234.LN4_MgdCarbon_bio_hi[ names_LN4_MgdCarbon ]

printlog( "L234.LN4_NewTech: Specify the start year for purpose-grown biomass" )
#Start with a table that has not had its bioenergy crops re-named yet (will make assigning a land leaf easier)
L234.LN4_NewTech <- L234.LN4_MgdCarbon[ L234.LN4_MgdCarbon[[LT]] == "biomass",
      names( L234.LN4_MgdCarbon ) %in% names_LN4_NewTech ]
L234.LN4_NewTech$newTechStartYear <- Bio_start_year
L234.LN4_NewTech$isNewTechnology <- 1

printlog( "NOTE: Assigning irrigated bioenergy ghost shares based on the share of agricultural land that is irrigated by region and AEZ" )
#Calculate fraction of cropland that is irrigated in the final historical year, for each region and AEZ
#First, multiply the irrigated quantity of land by an exogenous multiplier whose purpose is to reduce its share
L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irrX <- L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irr[ c( R_C_AEZ_irr, X_final_historical_year ) ]
L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irrX[[ X_final_historical_year ]][ grepl( "IRR", L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irrX [[irr ]] ) ] <-
      L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irrX[[ X_final_historical_year ]][ grepl( "IRR", L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irrX [[irr ]] ) ] *
      irrig_ghost_share_mult
L234.LC_bm2_R_HarvCropLand_AEZ_irr <- aggregate( L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irrX[ X_final_historical_year ],
      by=as.list( L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irrX[ R_AEZ_irr ] ), sum )
L234.LC_bm2_R_HarvCropLand_AEZ <- aggregate( L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irrX[ X_final_historical_year ],
      by=as.list( L234.LC_bm2_R_HarvCropLand_C_Yh_AEZ_irrX[ R_AEZ ] ), sum )
L234.GhostShare_R_AEZ <- L234.LC_bm2_R_HarvCropLand_AEZ_irr
L234.GhostShare_R_AEZ$GhostShare <- L234.LC_bm2_R_HarvCropLand_AEZ_irr[[ X_final_historical_year ]] /
      L234.LC_bm2_R_HarvCropLand_AEZ[[ X_final_historical_year ]][
         match( vecpaste( L234.LC_bm2_R_HarvCropLand_AEZ_irr[ R_AEZ ] ),
                vecpaste( L234.LC_bm2_R_HarvCropLand_AEZ[ R_AEZ ] ) ) ]
L234.GhostShare_R_AEZ <- add_region_name( L234.GhostShare_R_AEZ )
L234.GhostShare_R_AEZ$LandLeaf <- paste( "biomass", L234.GhostShare_R_AEZ[[AEZ]], L234.GhostShare_R_AEZ[[irr]], sep = AEZ_delimiter )

#Set ghost share leaf to irrigated share
L234.LN4_NewTech$GhostShareLeaf <- round(
    L234.GhostShare_R_AEZ$GhostShare[
      match( vecpaste( L234.LN4_NewTech[ c( "region", "LandLeaf" ) ] ),
             vecpaste( L234.GhostShare_R_AEZ[ c( "region", "LandLeaf" ) ] ) ) ],
    digits_land_use )
#Regions / AEZs may exist but not have any cropland. In these cases, set the ghost share leaf to 1 for rainfed and 0 for irrigated
L234.LN4_NewTech$GhostShareLeaf[ is.na( L234.LN4_NewTech$GhostShareLeaf ) & grepl( "IRR", L234.LN4_NewTech$LandLeaf ) ] <- 0
L234.LN4_NewTech$GhostShareLeaf[ is.na( L234.LN4_NewTech$GhostShareLeaf ) & !grepl( "IRR", L234.LN4_NewTech$LandLeaf ) ] <- 1

printlog( "L234.LN4_NewNode: Create new biomass nodes" )
L234.LN4_NewNode <- subset( L234.LN4_Logit, grepl( "biomass", LandNode4 ) )[ names( L234.LN4_Logit ) %in% names_LN4_NewTech ]
L234.LN4_NewNode$isNewTechnology <- 1
L234.LN4_NewNode$GhostShareLeaf <- ghost_share_node

#Rename biocrops
L234.LN4_Logit <- rename_biocrops( L234.LN4_Logit, lookup = A_biocrops_R_AEZ_irr,
      data_matchvar = "LandNode4", lookup_matchvar = "old_LandNode4", "LandNode2", "LandNode3", "LandNode4" )
L234.LN4_HistMgdAllocation_bio <- rename_biocrops( L234.LN4_HistMgdAllocation_bio, lookup = A_biocrops_R_AEZ_irr,
      data_matchvar = "LandLeaf", lookup_matchvar = "old_LandLeaf", "LandNode2", "LandNode3", "LandNode4", "LandLeaf" )
L234.LN4_MgdAllocation_bio <- rename_biocrops( L234.LN4_MgdAllocation_bio, lookup = A_biocrops_R_AEZ_irr,
      data_matchvar = "LandLeaf", lookup_matchvar = "old_LandLeaf", "LandNode2", "LandNode3", "LandNode4", "LandLeaf" )
L234.LN4_NewTech <- rename_biocrops( L234.LN4_NewTech, lookup = A_biocrops_R_AEZ_irr,
      data_matchvar = "LandLeaf", lookup_matchvar = "old_LandLeaf", "LandNode2", "LandNode3", "LandNode4", "LandLeaf" )
L234.LN4_NewNode <- rename_biocrops( L234.LN4_NewNode, lookup = A_biocrops_R_AEZ_irr,
      data_matchvar = "LandNode4", lookup_matchvar = "old_LandNode4", "LandNode2", "LandNode3", "LandNode4" )

#Remove non-existent AEZs
L234.LN4_Logit <- remove_AEZ_nonexist( L234.LN4_Logit, AEZcol="LandNode1" )
L234.LN4_HistMgdAllocation_crop <- remove_AEZ_nonexist( L234.LN4_HistMgdAllocation_crop, AEZcol="LandNode1" )
L234.LN4_MgdAllocation_crop <- remove_AEZ_nonexist( L234.LN4_MgdAllocation_crop, AEZcol="LandNode1" )
L234.LN4_HistMgdAllocation_bio <- remove_AEZ_nonexist( L234.LN4_HistMgdAllocation_bio, AEZcol="LandNode1" )
L234.LN4_MgdAllocation_bio <- remove_AEZ_nonexist( L234.LN4_MgdAllocation_bio, AEZcol="LandNode1" )
L234.LN4_MgdCarbon_crop <- remove_AEZ_nonexist( L234.LN4_MgdCarbon_crop, AEZcol = "LandNode1" )
L234.LN4_MgdCarbon_bio_ref <- remove_AEZ_nonexist( L234.LN4_MgdCarbon_bio_ref, AEZcol = "LandNode1" )
L234.LN4_MgdCarbon_bio_hi <- remove_AEZ_nonexist( L234.LN4_MgdCarbon_bio_hi, AEZcol = "LandNode1" )
L234.LN4_NewTech <- remove_AEZ_nonexist( L234.LN4_NewTech, AEZcol = "LandNode1" )
L234.LN4_NewNode <- remove_AEZ_nonexist( L234.LN4_NewNode, AEZcol = "LandNode1" )

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L234.LN4_Logit, IDstring="LN4_Logit", domain="AGLU_LEVEL2_DATA", fn="L234.LN4_Logit",
               batch_XML_domain="AGLU_XML_BATCH", batch_XML_file="batch_land_input_4_IRR.xml" )
write_mi_data( L234.LN4_HistMgdAllocation_crop, "LN4_HistMgdAllocation", "AGLU_LEVEL2_DATA", "L234.LN4_HistMgdAllocation_crop", "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml" )
write_mi_data( L234.LN4_MgdAllocation_crop, "LN4_MgdAllocation", "AGLU_LEVEL2_DATA", "L234.LN4_MgdAllocation_crop", "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml" )
write_mi_data( L234.LN4_HistMgdAllocation_bio, "LN4_HistMgdAllocation", "AGLU_LEVEL2_DATA", "L234.LN4_HistMgdAllocation_bio", "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml" )
write_mi_data( L234.LN4_MgdAllocation_bio, "LN4_MgdAllocation", "AGLU_LEVEL2_DATA", "L234.LN4_MgdAllocation_bio", "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml" )
write_mi_data( L234.LN4_MgdCarbon_crop, "LN4_MgdCarbon", "AGLU_LEVEL2_DATA", "L234.LN4_MgdCarbon_crop", "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml" )
write_mi_data( L234.LN4_MgdCarbon_bio_ref, "LN4_MgdCarbon", "AGLU_LEVEL2_DATA", "L234.LN4_MgdCarbon_bio_ref", "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml" )
write_mi_data( L234.LN4_MgdCarbon_bio_hi, "LN4_MgdCarbon", "AGLU_LEVEL2_DATA", "L234.LN4_MgdCarbon_bio_hi", "AGLU_XML_BATCH", "batch_bio_hi_IRR.xml", node_rename=T )
write_mi_data( L234.LN4_NewTech, "LN4_NewTech", "AGLU_LEVEL2_DATA", "L234.LN4_NewTech", "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml" )
write_mi_data( L234.LN4_NewNode, "LN4_NewNode", "AGLU_LEVEL2_DATA", "L234.LN4_NewNode", "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml", node_rename=T )

insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_land_input_4_IRR.xml", "AGLU_XML_FINAL", "land_input_4_IRR.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", "batch_bio_hi_IRR.xml", "AGLU_XML_FINAL", "bio_hi_IRR.xml", "", xml_tag="outFile" )

logstop()
