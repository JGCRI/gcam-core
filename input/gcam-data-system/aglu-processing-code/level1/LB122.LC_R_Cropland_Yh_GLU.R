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
logstart( "LB122.LC_R_Cropland_Yh_GLU.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Cropland cover by region / crop / historical year / GLU" )

# -----------------------------------------------------------------------------
# 1. Read data

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
L100.FAO_fallowland_kha <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_fallowland_kha" )
L100.FAO_CL_kha <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_CL_kha" )
L100.FAO_harv_CL_kha <- readdata( "AGLU_LEVEL1_DATA", "L100.FAO_harv_CL_kha" )
L103.ag_HA_bm2_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L103.ag_HA_bm2_R_C_Y_GLU" )
L103.ag_Prod_Mt_R_C_Y_GLU <- readdata( "AGLU_LEVEL1_DATA", "L103.ag_Prod_Mt_R_C_Y_GLU" )
L120.LC_bm2_R_LT_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L120.LC_bm2_R_LT_Yh_GLU" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#take a subset of the land cover table: cropland, and only in aglu historical years
L122.LC_bm2_R_CropLand_Y_GLU <- L120.LC_bm2_R_LT_Yh_GLU[ L120.LC_bm2_R_LT_Yh_GLU[[ LT ]] == "Cropland", c( R_LT_GLU, X_AGLU_historical_years ) ]

# The harvested area/production tables (from Monfreda) may have R_GLUs not in the cropland table (from Hyde), and vice versa.
# Filling out the cropland table to include all R_GLUs in Monfreda
L122.Cropland_addtl <- unique( L103.ag_HA_bm2_R_C_Y_GLU[
  vecpaste( L103.ag_HA_bm2_R_C_Y_GLU[ R_GLU ] ) %!in% vecpaste( L122.LC_bm2_R_CropLand_Y_GLU[ R_GLU ] ),
  R_GLU ] )
L122.Cropland_addtl[[LT]] <- "Cropland"
L122.Cropland_addtl[ X_AGLU_historical_years ] <- 0  #set to zero for now, will be over-written later
L122.LC_bm2_R_CropLand_Y_GLU <- rbind( L122.LC_bm2_R_CropLand_Y_GLU, L122.Cropland_addtl )

#compile harvested area across all crops
L122.ag_HA_bm2_R_Y_GLU <- aggregate( L103.ag_HA_bm2_R_C_Y_GLU[ X_AGLU_historical_years ],
                                     by = L103.ag_HA_bm2_R_C_Y_GLU[ R_GLU ], sum )

#Calculate the average cropland, fallow land, and land in temporary crops from FAO RESOURCESTAT
#The time series is unreliable, so only using the last available year (and applying to all historical years)
X_fallowland_year <- X_AGLU_historical_years[ length( X_AGLU_historical_years ) ]

#compile the ratio of "temporary crops" to total arable land in each region
#make table with fallow land compared to total arable land
printlog ( "Calculating the average percent of cropland that is fallow in each region" )
L122.cropland_fallow_ctry <- data.frame( iso  = L100.FAO_CL_kha$iso, cropland = L100.FAO_CL_kha[[ X_fallowland_year ]] )
L122.cropland_fallow_ctry$fallow <- L100.FAO_fallowland_kha[[ X_fallowland_year ]][ match( L122.cropland_fallow_ctry$iso, L100.FAO_fallowland_kha$iso ) ]
L122.cropland_fallow_ctry <- na.omit( L122.cropland_fallow_ctry )
L122.cropland_fallow_ctry[[ R ]] <- iso_GCAM_regID [[ R ]][ match( L122.cropland_fallow_ctry$iso, iso_GCAM_regID$iso ) ]
L122.cropland_fallow_R <- aggregate( L122.cropland_fallow_ctry[ c( "cropland", "fallow" ) ],
      by=as.list( L122.cropland_fallow_ctry[ R ] ), sum )
L122.cropland_fallow_R$fallow_frac <- L122.cropland_fallow_R$fallow / L122.cropland_fallow_R$cropland

#make table with cropped land compared to total arable land
printlog ( "Calculating the average percent of cropland that is not in active crop rotations in each region" )
L122.cropland_cropped_ctry <- data.frame( iso  = L100.FAO_CL_kha$iso, cropland = L100.FAO_CL_kha[[ X_fallowland_year ]] )
L122.cropland_cropped_ctry$cropped <- L100.FAO_harv_CL_kha[[ X_fallowland_year ]][ match( L122.cropland_cropped_ctry$iso, L100.FAO_harv_CL_kha$iso ) ]
L122.cropland_cropped_ctry <- na.omit( L122.cropland_cropped_ctry )
L122.cropland_cropped_ctry[[ R ]] <- iso_GCAM_regID [[ R ]][ match( L122.cropland_cropped_ctry$iso, iso_GCAM_regID$iso ) ]
L122.cropland_cropped_R <- aggregate( L122.cropland_cropped_ctry[ c( "cropland", "cropped" ) ],
      by=as.list( L122.cropland_cropped_ctry[ R ] ), sum )
L122.cropland_cropped_R$cropped_frac <- L122.cropland_cropped_R$cropped / L122.cropland_cropped_R$cropland

#calculate the average amount of cropland that is not in production as the fallow land fraction, where available, or else the non-cropped cropland
printlog ( "NOTE: based on availability, using (1) fallow land fraction, (2) land not in crop rotations, or (3) 0" )
L122.nonharvested_cropland_R <- data.frame( GCAM_region_ID = sort( unique( iso_GCAM_regID[[ R ]] ) ),
										    Land_Type = "Cropland" )
L122.nonharvested_cropland_R$fallow_frac <- L122.cropland_fallow_R$fallow_frac[
      match( L122.nonharvested_cropland_R$GCAM_region_ID, L122.cropland_fallow_R$GCAM_region_ID ) ]
L122.nonharvested_cropland_R$uncropped_frac <- 1 - L122.cropland_cropped_R$cropped_frac[
      match( L122.nonharvested_cropland_R$GCAM_region_ID, L122.cropland_cropped_R$GCAM_region_ID ) ]
L122.nonharvested_cropland_R$nonharvested_frac <- ifelse(
      is.na( L122.nonharvested_cropland_R$fallow_frac ),
      L122.nonharvested_cropland_R$uncropped_frac,
      L122.nonharvested_cropland_R$fallow_frac )
#If any regions have no data on fallow land or temporary crop cover for any years, set the portion to 0
L122.nonharvested_cropland_R$nonharvested_frac[ is.na ( L122.nonharvested_cropland_R$nonharvested_frac ) ] <- 0

#make a table with fallow land, and the remaining cropland that is "available" for harvest, by region, year, and GLU
printlog( "NOTE: applying regional average fallow fractions to all GLUs within each region" )
L122.LC_bm2_R_FallowLand_Y_GLU <- data.frame( L122.LC_bm2_R_CropLand_Y_GLU[ c( R_LT_GLU ) ],
      L122.LC_bm2_R_CropLand_Y_GLU[ X_AGLU_historical_years ] * L122.nonharvested_cropland_R$nonharvested_frac[
          match( L122.LC_bm2_R_CropLand_Y_GLU[[ R ]], L122.nonharvested_cropland_R[[ R ]] ) ] )
L122.LC_bm2_R_FallowLand_Y_GLU[[ LT ]] <- "FallowLand"

L122.LC_bm2_R_AvailableCropLand_Y_GLU <- data.frame( L122.LC_bm2_R_CropLand_Y_GLU[ c( R_LT_GLU ) ],
      L122.LC_bm2_R_CropLand_Y_GLU[ X_AGLU_historical_years ] - L122.LC_bm2_R_FallowLand_Y_GLU[ X_AGLU_historical_years ] )

#Calculate the harvested to cropped land ratio for all crops, by region, year, and GLU
printlog( "NOTE: applying minimum and maximum harvested:cropped ratios" )
L122.ag_HA_to_CropLand_R_Y_GLU <- L122.ag_HA_bm2_R_Y_GLU
L122.ag_HA_to_CropLand_R_Y_GLU[ X_AGLU_historical_years ] <-  L122.ag_HA_bm2_R_Y_GLU[ X_AGLU_historical_years ] / L122.LC_bm2_R_AvailableCropLand_Y_GLU[
      match( vecpaste( L122.ag_HA_bm2_R_Y_GLU[ R_GLU ] ), vecpaste( L122.LC_bm2_R_AvailableCropLand_Y_GLU[ R_GLU ] ) ),
      X_AGLU_historical_years ]
L122.ag_HA_to_CropLand_R_Y_GLU[ X_AGLU_historical_years ][ L122.ag_HA_to_CropLand_R_Y_GLU[ X_AGLU_historical_years ] < min_HA_to_Cropland ] <- min_HA_to_Cropland

#Maximum harvested:cropped ratio may cause cropland to expand
#This additional cropland will need to be balanced by a deduction from other land types later on, so it is tracked below
if( any( L122.ag_HA_to_CropLand_R_Y_GLU[ X_AGLU_historical_years ] > max_HA_to_Cropland ) ){
	printlog( "NOTE: Cropland increased where maximum harvested:cropped ratio was exceeded" )
	L122.ag_HA_to_CropLand_R_Y_GLU[ X_AGLU_historical_years ][ L122.ag_HA_to_CropLand_R_Y_GLU[ X_AGLU_historical_years ] > max_HA_to_Cropland ] <- max_HA_to_Cropland
}

#The ag_HA_to_CropLand ratio is assumed to be a property of the region and GLU ( not individual crops ), as individual sites are planted with different crops
#Calculate cropland requirements of each crop as harvested area divided by regional ag_HA_to_CropLand ratio
printlog( "Calculating cropland requirements of each crop as harvested area divided by HA:CL" )
#Calculate land cover as harvested area divided by HA:CL
L122.LC_bm2_R_HarvCropLand_C_Y_GLU <- L103.ag_HA_bm2_R_C_Y_GLU[ R_C_GLU ]
L122.LC_bm2_R_HarvCropLand_C_Y_GLU[ X_AGLU_historical_years ] <- L103.ag_HA_bm2_R_C_Y_GLU[ X_AGLU_historical_years ] / L122.ag_HA_to_CropLand_R_Y_GLU[
      match( vecpaste( L103.ag_HA_bm2_R_C_Y_GLU[ R_GLU ] ), vecpaste( L122.ag_HA_to_CropLand_R_Y_GLU[ R_GLU ] ) ),
      X_AGLU_historical_years ]

#aggregate across crops to get harvested cropland area, by region/GLU/year
L122.LC_bm2_R_HarvCropLand_C_Y_GLU[[LT]] <- "HarvCropLand"
L122.LC_bm2_R_HarvCropLand_Y_GLU <- aggregate( L122.LC_bm2_R_HarvCropLand_C_Y_GLU[ X_AGLU_historical_years ],
      by = L122.LC_bm2_R_HarvCropLand_C_Y_GLU[ R_LT_GLU ], sum )

#Calculate economic yield by each crop as production divided by cropland. Write out this preliminary table.
L122.ag_EcYield_kgm2_R_C_Y_GLU <- L122.LC_bm2_R_HarvCropLand_C_Y_GLU[ R_C_GLU ]
L122.ag_EcYield_kgm2_R_C_Y_GLU[ X_AGLU_historical_years ] <- L103.ag_Prod_Mt_R_C_Y_GLU[
      match( vecpaste( L122.LC_bm2_R_HarvCropLand_C_Y_GLU[ R_C_GLU ] ), vecpaste( L103.ag_Prod_Mt_R_C_Y_GLU[ R_C_GLU ] ) ),
      X_AGLU_historical_years ] /
      L122.LC_bm2_R_HarvCropLand_C_Y_GLU[ X_AGLU_historical_years ]
L122.ag_EcYield_kgm2_R_C_Y_GLU[ is.na( L122.ag_EcYield_kgm2_R_C_Y_GLU ) ] <- 0

#The minimum threshold on HA:CL means that some cropland in Hyde will not be assigned to a crop. This is mapped to "other arable land"
#The maximum threshold on HA:CL means that cropland in Hyde in some ag regions may be less than cropland in GCAM
#In the latter case, land needs to be mapped to cropland from other uses.
#Calculate the residual land cover that is cropland (may be positive or negative)
L122.LC_bm2_R_ResidualCropLand_Y_GLU <- L122.LC_bm2_R_HarvCropLand_Y_GLU[ R_LT_GLU ]
L122.LC_bm2_R_ResidualCropLand_Y_GLU[[ LT ]] <- "ResidualCropLand"
L122.LC_bm2_R_ResidualCropLand_Y_GLU[ X_AGLU_historical_years ] <- L122.LC_bm2_R_HarvCropLand_Y_GLU[ X_AGLU_historical_years ] -
  L122.LC_bm2_R_AvailableCropLand_Y_GLU[
      match( vecpaste( L122.LC_bm2_R_HarvCropLand_Y_GLU[ R_GLU ] ),
             vecpaste( L122.LC_bm2_R_AvailableCropLand_Y_GLU[ R_GLU ] ) ),
      X_AGLU_historical_years ]

#Where residuals are negative, this is "unused" cropland that will be mapped to other arable land
printlog( "Calculating unused cropland; this is added with fallow land to calculate other arable land" )
L122.LC_bm2_R_UnusedCropLand_Y_GLU <- L122.LC_bm2_R_ResidualCropLand_Y_GLU
L122.LC_bm2_R_UnusedCropLand_Y_GLU[[ LT ]] <- "UnusedCropLand"
L122.LC_bm2_R_UnusedCropLand_Y_GLU[ X_AGLU_historical_years ][ L122.LC_bm2_R_UnusedCropLand_Y_GLU[ X_AGLU_historical_years ] > 0 ] <- 0
L122.LC_bm2_R_UnusedCropLand_Y_GLU[ X_AGLU_historical_years ] <- -1 * L122.LC_bm2_R_UnusedCropLand_Y_GLU[ X_AGLU_historical_years ]

#Where residuals are positive, this is "extra" land that will later be deducted from other categories.
printlog( "Calculating extra cropland; this will be balanced by a deduction from unmanaged lands" )
L122.LC_bm2_R_ExtraCropLand_Y_GLU <- L122.LC_bm2_R_ResidualCropLand_Y_GLU
L122.LC_bm2_R_ExtraCropLand_Y_GLU[[ LT ]] <- "ExtraCropLand"
L122.LC_bm2_R_ExtraCropLand_Y_GLU[ X_AGLU_historical_years ][ L122.LC_bm2_R_ExtraCropLand_Y_GLU[ X_AGLU_historical_years ] < 0 ] <- 0

printlog( "Calculating other arable land: known fallow plus discrepancy between sum of harvested area and cropland" )
L122.LC_bm2_R_OtherArableLand_Y_GLU <- L122.LC_bm2_R_FallowLand_Y_GLU
L122.LC_bm2_R_OtherArableLand_Y_GLU[[ LT ]] <- "OtherArableLand"
L122.LC_bm2_R_OtherArableLand_Y_GLU[ X_AGLU_historical_years ] <- L122.LC_bm2_R_FallowLand_Y_GLU[ X_AGLU_historical_years ] +
  L122.LC_bm2_R_UnusedCropLand_Y_GLU[
      match( vecpaste( L122.LC_bm2_R_FallowLand_Y_GLU[ R_GLU ] ),
             vecpaste( L122.LC_bm2_R_UnusedCropLand_Y_GLU[ R_GLU ] ) ),
      X_AGLU_historical_years ]

printlog( "Assigning cropland to other arable land wherever harvested area is zero" )
#If there are any land use regions with 0 harvested area (Monfreda) but positive cropland cover (Hyde), these are missing
# values in the above table, and all of this cropland should be assigned to other arable land.
L122.LC_bm2_R_OtherArableLand_Y_GLU[
  vecpaste( L122.LC_bm2_R_OtherArableLand_Y_GLU[ R_GLU ] ) %!in% vecpaste( L103.ag_HA_bm2_R_C_Y_GLU[ R_GLU ] ), X_AGLU_historical_years ] <-
  L122.LC_bm2_R_CropLand_Y_GLU[
    vecpaste( L122.LC_bm2_R_CropLand_Y_GLU[ R_GLU ] ) %!in% vecpaste( L103.ag_HA_bm2_R_C_Y_GLU[ R_GLU ] ), X_AGLU_historical_years ]

#Land use history
printlog( "Cropland quantities prior to the first AGLU historical year, for spinup of simple carbon cycle model" )
printlog( "NOTE: Simply assigning this to other arable land" )
# This method differs from what we had done in the past, where 1971 land cover quantities were rolled back on the basis of the cropland ratios
# between each land history year and 1971. The problem with this method is that zero values in 1971 can not match non-zero values in prior years.
# The current method instead will have apparent land use change in going from the land history years to the model base years, but due to equal
# soil carbon contents, and similar/small vegetative carbon contents, the net emissions signal should be negligible.
printlog( "Using historical cropland ratios to calculate land use history for other arable land" )
# First, make a table with cropland in the pre-aglu years
L122.LC_bm2_R_CropLand_Yhh_GLU <- L122.LC_bm2_R_CropLand_Y_GLU[ R_LT_GLU ]
L122.LC_bm2_R_CropLand_Yhh_GLU[ X_preAGLU_years ] <- L120.LC_bm2_R_LT_Yh_GLU[
  match( vecpaste( L122.LC_bm2_R_CropLand_Yhh_GLU[R_LT_GLU ] ),
         vecpaste( L120.LC_bm2_R_LT_Yh_GLU[ R_LT_GLU ] ) ),
  X_preAGLU_years ]

#Missing values are zeroes
L122.LC_bm2_R_CropLand_Yhh_GLU[ is.na( L122.LC_bm2_R_CropLand_Yhh_GLU ) ] <- 0

#These are now ready to be copied into the land history years of other arable land
L122.LC_bm2_R_OtherArableLand_Yh_GLU <- L122.LC_bm2_R_OtherArableLand_Y_GLU
L122.LC_bm2_R_OtherArableLand_Yh_GLU[ X_preAGLU_years ] <- L122.LC_bm2_R_CropLand_Yhh_GLU[ X_preAGLU_years ]

printlog( "All other cropland uses are zero in the pre-aglu years" )
L122.LC_bm2_R_ExtraCropLand_Yh_GLU <- L122.LC_bm2_R_ExtraCropLand_Y_GLU
L122.LC_bm2_R_ExtraCropLand_Yh_GLU[ X_preAGLU_years ] <- 0
L122.LC_bm2_R_ExtraCropLand_Yh_GLU <- L122.LC_bm2_R_ExtraCropLand_Yh_GLU[ c( R_LT_GLU, X_land_cover_years ) ]

#Harvested cropland history
printlog( "Using historical cropland ratios to calculate land use history for harvested cropland, by crop" )
L122.LC_bm2_R_HarvCropLand_C_Yh_GLU <- L122.LC_bm2_R_HarvCropLand_C_Y_GLU
L122.LC_bm2_R_HarvCropLand_C_Yh_GLU[ X_preAGLU_years ] <- 0

#Combine crop types to get land use history by all harvested cropland
L122.LC_bm2_R_HarvCropLand_Yh_GLU <- aggregate( L122.LC_bm2_R_HarvCropLand_C_Yh_GLU[ X_land_cover_years ],
      by = L122.LC_bm2_R_HarvCropLand_C_Yh_GLU[ R_LT_GLU ], sum )

#Only write out the relevant columns
L122.LC_bm2_R_HarvCropLand_C_Yh_GLU <- L122.LC_bm2_R_HarvCropLand_C_Yh_GLU[ c( R_C_GLU, X_land_cover_years ) ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L122.ag_HA_to_CropLand_R_Y_GLU <- c( "Harvested area to cropland ratio by GCAM region / year / GLU","Unitless ratio" )
comments.L122.ag_EcYield_kgm2_R_C_Y_GLU <- c( "Economic yield by GCAM region / commodity / year / GLU","Unit = kg.m2" )
comments.L122.LC_bm2_R_OtherArableLand_Yh_GLU <- c( "Other arable land cover by GCAM region / year / GLU","Unit = bm2" )
comments.L122.LC_bm2_R_ExtraCropLand_Yh_GLU <- c( "Extra cropland cover by GCAM region / year / GLU","Unit = bm2" )
comments.L122.LC_bm2_R_HarvCropLand_C_Yh_GLU <- c( "Harvested cropland cover by GCAM region / commodity / year / GLU","Unit = bm2" )
comments.L122.LC_bm2_R_HarvCropLand_Yh_GLU <- c( "Harvested cropland cover by GCAM region / year / GLU","Unit = bm2" )

writedata( L122.ag_HA_to_CropLand_R_Y_GLU, domain="AGLU_LEVEL1_DATA", fn="L122.ag_HA_to_CropLand_R_Y_GLU", comments=comments.L122.ag_HA_to_CropLand_R_Y_GLU )
writedata( L122.ag_EcYield_kgm2_R_C_Y_GLU, domain="AGLU_LEVEL1_DATA", fn="L122.ag_EcYield_kgm2_R_C_Y_GLU", comments=comments.L122.ag_EcYield_kgm2_R_C_Y_GLU )
writedata( L122.LC_bm2_R_OtherArableLand_Yh_GLU, domain="AGLU_LEVEL1_DATA", fn="L122.LC_bm2_R_OtherArableLand_Yh_GLU", comments=comments.L122.LC_bm2_R_OtherArableLand_Yh_GLU )
writedata( L122.LC_bm2_R_ExtraCropLand_Yh_GLU, domain="AGLU_LEVEL1_DATA", fn="L122.LC_bm2_R_ExtraCropLand_Yh_GLU", comments=comments.L122.LC_bm2_R_ExtraCropLand_Yh_GLU )
writedata( L122.LC_bm2_R_HarvCropLand_C_Yh_GLU, domain="AGLU_LEVEL1_DATA", fn="L122.LC_bm2_R_HarvCropLand_C_Yh_GLU", comments=comments.L122.LC_bm2_R_HarvCropLand_C_Yh_GLU )
writedata( L122.LC_bm2_R_HarvCropLand_Yh_GLU, domain="AGLU_LEVEL1_DATA", fn="L122.LC_bm2_R_HarvCropLand_Yh_GLU", comments=comments.L122.LC_bm2_R_HarvCropLand_Yh_GLU )

# Every script should finish with this line
logstop()
