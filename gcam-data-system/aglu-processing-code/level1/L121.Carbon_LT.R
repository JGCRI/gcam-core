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
logstart( "L121.Carbon_LT.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Carbon densities by region, land type, and AEZ" )

# -----------------------------------------------------------------------------
# 1. Read data
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS","iso_GCAM_regID" )
SAGE_LT <- readdata( "AGLU_MAPPINGS", "SAGE_LT" )
Various_VegC_tCha_LTsage_AEZ <- readdata( "AGLU_LEVEL0_DATA", "Various_VegC_tCha_LTsage_AEZ" )
Various_SoilC_tCha_LTsage_AEZ <- readdata( "AGLU_LEVEL0_DATA", "Various_SoilC_tCha_LTsage_AEZ" )
Various_MatureAge_LTsage_AEZ <- readdata( "AGLU_LEVEL0_DATA", "Various_MatureAge_LTsage_AEZ" )
L120.LC_bm2_R_LTsage_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L120.LC_bm2_R_LTsage_AEZ" )

# -----------------------------------------------------------------------------
# 2. Perform computations
GCAM_regions <- sort( unique( iso_GCAM_regID$GCAM_region_ID ) )

#First, convert characteristics by land type to correct units.
L121.VegC_kgCm2_LTsage_mgd_AEZ <- Various_VegC_tCha_LTsage_AEZ[ c( "LT_SAGE", AEZs ) ]
L121.VegC_kgCm2_LTsage_mgd_AEZ[ AEZs ] <- Various_VegC_tCha_LTsage_AEZ[ AEZs ] * conv_tha_kgm2
L121.SoilC_kgCm2_LTsage_mgd_AEZ <- Various_SoilC_tCha_LTsage_AEZ[ c( "LT_SAGE", AEZs ) ]
L121.SoilC_kgCm2_LTsage_mgd_AEZ[ AEZs ] <- Various_SoilC_tCha_LTsage_AEZ[ AEZs ] * conv_tha_kgm2
L121.MatureAge_LTsage_mgd_AEZ <- Various_MatureAge_LTsage_AEZ[ c( "LT_SAGE", AEZs ) ]

#Subset only the land types in the SAGE natural vegetation database
L121.VegC_kgCm2_LTsage_AEZ <- subset( L121.VegC_kgCm2_LTsage_mgd_AEZ, LT_SAGE %in% L120.LC_bm2_R_LTsage_AEZ$LT_SAGE )
L121.SoilC_kgCm2_LTsage_AEZ <- subset( L121.SoilC_kgCm2_LTsage_mgd_AEZ, LT_SAGE %in% L120.LC_bm2_R_LTsage_AEZ$LT_SAGE )
L121.MatureAge_LTsage_AEZ <- subset( L121.MatureAge_LTsage_mgd_AEZ, LT_SAGE %in% L120.LC_bm2_R_LTsage_AEZ$LT_SAGE )

#Repeat and add region vector
L121.VegC_kgCm2_R_LTsage_AEZ <- repeat_and_add_vector( L121.VegC_kgCm2_LTsage_AEZ, R, GCAM_regions )
L121.SoilC_kgCm2_R_LTsage_AEZ <- repeat_and_add_vector( L121.SoilC_kgCm2_LTsage_AEZ, R, GCAM_regions )
L121.MatureAge_R_LTsage_AEZ <- repeat_and_add_vector( L121.MatureAge_LTsage_AEZ, R, GCAM_regions )

#Next, sort land cover AND characteristics tables to ensure one-to-one mapping
L121.VegC_kgCm2_R_LTsage_AEZ <- L121.VegC_kgCm2_R_LTsage_AEZ[ order( L121.VegC_kgCm2_R_LTsage_AEZ$LT_SAGE ) , ]
L121.SoilC_kgCm2_R_LTsage_AEZ <- L121.SoilC_kgCm2_R_LTsage_AEZ[ order( L121.SoilC_kgCm2_R_LTsage_AEZ$LT_SAGE ) , ]
L121.MatureAge_R_LTsage_AEZ <- L121.MatureAge_R_LTsage_AEZ[ order( L121.MatureAge_R_LTsage_AEZ$LT_SAGE ) , ]
L121.LCnatveg_bm2_R_LTsage_AEZ <- L120.LC_bm2_R_LTsage_AEZ[ order( L120.LC_bm2_R_LTsage_AEZ$LT_SAGE ) , ]

#Multiply land cover by carbon content/mature age
printlog( "Multiplying carbon content and mature age by land cover, by SAGE land type, for weighting" )
L121.VegC_Tg_R_LTsage <- L121.LCnatveg_bm2_R_LTsage_AEZ
L121.VegC_Tg_R_LTsage[ AEZs ] <- L121.LCnatveg_bm2_R_LTsage_AEZ[ AEZs ] * L121.VegC_kgCm2_R_LTsage_AEZ[ AEZs ]
      
L121.SoilC_Tg_R_LTsage <- L121.LCnatveg_bm2_R_LTsage_AEZ
L121.SoilC_Tg_R_LTsage[ AEZs ] <- L121.LCnatveg_bm2_R_LTsage_AEZ[ AEZs ] * L121.SoilC_kgCm2_R_LTsage_AEZ[ AEZs ]

L121.MatureAge_bm2_R_LTsage <- L121.LCnatveg_bm2_R_LTsage_AEZ
L121.MatureAge_bm2_R_LTsage[ AEZs ] <- L121.LCnatveg_bm2_R_LTsage_AEZ[ AEZs ] * L121.MatureAge_R_LTsage_AEZ[ AEZs ]

#Map in GCAM land types  and GCAM regions into all four tables
printlog( "Aggregating by GCAM land type" )
L121.VegC_Tg_R_LTsage[[ LT ]] <- SAGE_LT[[ LT ]][ match( L121.VegC_Tg_R_LTsage$LT_SAGE, SAGE_LT$LT_SAGE ) ]
L121.SoilC_Tg_R_LTsage[[ LT ]] <- SAGE_LT[[ LT ]][ match( L121.SoilC_Tg_R_LTsage$LT_SAGE, SAGE_LT$LT_SAGE ) ]
L121.MatureAge_bm2_R_LTsage[[ LT ]] <- SAGE_LT[[ LT ]][ match( L121.MatureAge_bm2_R_LTsage$LT_SAGE, SAGE_LT$LT_SAGE ) ]
L121.LCnatveg_bm2_R_LTsage_AEZ[[ LT ]] <- SAGE_LT[[ LT ]][ match( L121.LCnatveg_bm2_R_LTsage_AEZ$LT_SAGE, SAGE_LT$LT_SAGE ) ]

#Aggregate by GCAM land type and region
L121.VegC_Tg_R_LTnatveg <- aggregate( L121.VegC_Tg_R_LTsage[ AEZs ], by=as.list( L121.SoilC_Tg_R_LTsage[ R_LT ] ), sum )
L121.SoilC_Tg_R_LTnatveg <- aggregate( L121.SoilC_Tg_R_LTsage[ AEZs ], by=as.list( L121.SoilC_Tg_R_LTsage[ R_LT ] ), sum )
L121.MatureAge_bm2_R_LTnatveg <- aggregate( L121.MatureAge_bm2_R_LTsage[ AEZs ], by=as.list( L121.MatureAge_bm2_R_LTsage[ R_LT ] ), sum )
L121.LCnatveg_bm2_LT <- aggregate( L121.LCnatveg_bm2_R_LTsage_AEZ[ AEZs ], by=as.list( L121.LCnatveg_bm2_R_LTsage_AEZ[ R_LT ] ), sum )

#Calculate average carbon content and mature age by GCAM land type
printlog( "Calculating weighted average carbon density and mature age by GCAM land type" )
L121.VegC_kgm2_R_LTnatveg_AEZ <- L121.VegC_Tg_R_LTnatveg
L121.VegC_kgm2_R_LTnatveg_AEZ[ AEZs ] <- L121.VegC_Tg_R_LTnatveg[ AEZs ] / L121.LCnatveg_bm2_LT[ AEZs ]

L121.SoilC_kgm2_R_LTnatveg_AEZ <- L121.SoilC_Tg_R_LTnatveg
L121.SoilC_kgm2_R_LTnatveg_AEZ[ AEZs ] <- L121.SoilC_Tg_R_LTnatveg[ AEZs ] / L121.LCnatveg_bm2_LT[ AEZs ]

L121.MatureAge_R_LTnatveg_AEZ <- L121.MatureAge_bm2_R_LTnatveg
L121.MatureAge_R_LTnatveg_AEZ[ AEZs ] <- L121.MatureAge_bm2_R_LTnatveg[ AEZs ] / L121.LCnatveg_bm2_LT[ AEZs ]

#Pasture carbon content and mature age
#Set pasture soil carbon content equal to aggregate grassland
printlog( "NOTE: Setting (unmanaged) pasture soil carbon content equal to grassland" )
L121.SoilC_kgm2_R_Past_AEZ <- L121.SoilC_kgm2_R_LTnatveg_AEZ[ L121.SoilC_kgm2_R_LTnatveg_AEZ[[ LT ]] == "Grassland" , ]
L121.SoilC_kgm2_R_Past_AEZ[[ LT ]] <- "Pasture"

printlog( "NOTE: AEZs can have pasture but no grassland. Using a default carbon content for AEZs with no grassland" )
#Build a matching dataframe of default values
L121.SoilC_kgCm2_Steppe_AEZ <- Various_SoilC_tCha_LTsage_AEZ[ Various_SoilC_tCha_LTsage_AEZ$LT_SAGE == "Grassland/Steppe", AEZs ] * conv_tha_kgm2
L121.SoilC_kgCm2_Steppe_AEZ <- L121.SoilC_kgCm2_Steppe_AEZ[ rep( 1, times = length( GCAM_regions ) ) , ]

#Replace the missing values
L121.SoilC_kgm2_R_Past_AEZ[ AEZs ][ is.na( L121.SoilC_kgm2_R_Past_AEZ[ AEZs ] ) ] <-
      L121.SoilC_kgCm2_Steppe_AEZ[ AEZs ][ is.na( L121.SoilC_kgm2_R_Past_AEZ[ AEZs ] ) ]

#Set vegetation carbon and mature age equal to SAGE-based "grassland/steppe"
L121.VegC_kgCm2_Steppe_AEZ <- Various_VegC_tCha_LTsage_AEZ[ Various_VegC_tCha_LTsage_AEZ$LT_SAGE == "Grassland/Steppe", AEZs ] * conv_tha_kgm2
L121.VegC_kgCm2_Steppe_AEZ_repR <- L121.VegC_kgCm2_Steppe_AEZ[ rep( 1, times = length( GCAM_regions ) ) , ]
L121.VegC_kgm2_R_Past_AEZ <- data.frame( GCAM_region_ID = GCAM_regions,
                                         Land_Type = "Pasture",
                                         L121.VegC_kgCm2_Steppe_AEZ_repR )
L121.MatureAge_Steppe_AEZ <- Various_MatureAge_LTsage_AEZ[ Various_MatureAge_LTsage_AEZ$LT_SAGE == "Grassland/Steppe", AEZs ]
L121.MatureAge_Steppe_AEZ_repR <- L121.MatureAge_Steppe_AEZ[ rep( 1, times = length( GCAM_regions ) ) , ]
L121.MatureAge_R_Past_AEZ <- data.frame( GCAM_region_ID = GCAM_regions,
										 Land_Type = "Pasture",
										 L121.MatureAge_Steppe_AEZ_repR )

#Urban land veg and soil carbon content, and mature age
printlog( "Reading in carbon contents and mature ages for urban land and cropland" )
L121.VegC_kgm2_Urban_AEZ <- L121.VegC_kgCm2_LTsage_mgd_AEZ[ L121.VegC_kgCm2_LTsage_mgd_AEZ$LT_SAGE == "Urbanland" , ]
L121.VegC_kgm2_Urban_AEZ_repR <- L121.VegC_kgm2_Urban_AEZ[ rep( 1:nrow( L121.VegC_kgm2_Urban_AEZ ), times = length( GCAM_regions) ) , ]
L121.VegC_kgm2_R_Urban_AEZ <- data.frame( GCAM_region_ID = GCAM_regions,
                                          Land_Type = "Urbanland",
                                          L121.VegC_kgm2_Urban_AEZ_repR[ AEZs ] )

L121.SoilC_kgm2_Urban_AEZ <- L121.SoilC_kgCm2_LTsage_mgd_AEZ[ L121.SoilC_kgCm2_LTsage_mgd_AEZ$LT_SAGE == "Urbanland" , ]
L121.SoilC_kgm2_Urban_AEZ_repR <- L121.SoilC_kgm2_Urban_AEZ[ rep( 1:nrow( L121.SoilC_kgm2_Urban_AEZ ), times = length( GCAM_regions) ) , ]
L121.SoilC_kgm2_R_Urban_AEZ <- data.frame( GCAM_region_ID = GCAM_regions,
										   Land_Type = "Urbanland",
										   L121.SoilC_kgm2_Urban_AEZ_repR[ AEZs ] )

L121.MatureAge_Urban_AEZ <- L121.MatureAge_LTsage_mgd_AEZ[ L121.MatureAge_LTsage_mgd_AEZ$LT_SAGE == "Urbanland" , ]
L121.MatureAge_Urban_AEZ_repR <- L121.MatureAge_Urban_AEZ[ rep( 1:nrow( L121.MatureAge_Urban_AEZ ), times = length( GCAM_regions) ) , ]
L121.MatureAge_R_Urban_AEZ <- data.frame( GCAM_region_ID = GCAM_regions,
										  Land_Type = "Urbanland",
										  L121.MatureAge_Urban_AEZ_repR[ AEZs ] )

#Cropland soil carbon content and mature age
L121.VegC_kgm2_Crop_AEZ <- L121.VegC_kgCm2_LTsage_mgd_AEZ[ L121.VegC_kgCm2_LTsage_mgd_AEZ$LT_SAGE == "Cropland" , ]
L121.VegC_kgm2_Crop_AEZ_repR <- L121.VegC_kgm2_Crop_AEZ[ rep( 1:nrow( L121.VegC_kgm2_Crop_AEZ ), times = length( GCAM_regions) ) , ]
L121.VegC_kgm2_R_Crop_AEZ <- data.frame( GCAM_region_ID = GCAM_regions,
										 Land_Type = "Cropland",
										 L121.VegC_kgm2_Crop_AEZ_repR[ AEZs ] )

L121.SoilC_kgm2_Crop_AEZ <- L121.SoilC_kgCm2_LTsage_mgd_AEZ[ L121.SoilC_kgCm2_LTsage_mgd_AEZ$LT_SAGE == "Cropland" , ]
L121.SoilC_kgm2_Crop_AEZ_repR <- L121.SoilC_kgm2_Crop_AEZ[ rep( 1:nrow( L121.SoilC_kgm2_Crop_AEZ ), times = length( GCAM_regions) ) , ]
L121.SoilC_kgm2_R_Crop_AEZ <- data.frame( GCAM_region_ID = GCAM_regions,
										  Land_Type = "Cropland",
										  L121.SoilC_kgm2_Crop_AEZ_repR[ AEZs ] )

L121.MatureAge_Crop_AEZ <- L121.MatureAge_LTsage_mgd_AEZ[ L121.MatureAge_LTsage_mgd_AEZ$LT_SAGE == "Cropland" , ]
L121.MatureAge_Crop_AEZ_repR <- L121.MatureAge_Crop_AEZ[ rep( 1:nrow( L121.MatureAge_Crop_AEZ ), times = length( GCAM_regions) ) , ]
L121.MatureAge_R_Crop_AEZ <- data.frame( GCAM_region_ID = GCAM_regions,
										 Land_Type = "Cropland",
										 L121.MatureAge_Crop_AEZ_repR[ AEZs ] )

#Rbind natveg, pasture, urbanland, and cropland tables
printlog( "Combining natural vegetation, pasture, urban land, and cropland into three tables" )
L121.VegC_kgm2_R_LT_AEZ <- rbind( L121.VegC_kgm2_R_LTnatveg_AEZ, L121.VegC_kgm2_R_Past_AEZ, L121.VegC_kgm2_R_Urban_AEZ, L121.VegC_kgm2_R_Crop_AEZ )
L121.SoilC_kgm2_R_LT_AEZ <- rbind( L121.SoilC_kgm2_R_LTnatveg_AEZ, L121.SoilC_kgm2_R_Past_AEZ, L121.SoilC_kgm2_R_Urban_AEZ, L121.SoilC_kgm2_R_Crop_AEZ )
L121.MatureAge_R_LT_AEZ <- rbind( L121.MatureAge_R_LTnatveg_AEZ, L121.MatureAge_R_Past_AEZ, L121.MatureAge_R_Urban_AEZ, L121.MatureAge_R_Crop_AEZ )

#Round all estimates to specified number of decimal places
printlog( "Rounding all estimates to specified number of decimal places" )
L121.VegC_kgm2_R_LT_AEZ[ AEZs ] <- round( L121.VegC_kgm2_R_LT_AEZ[ AEZs ], digits = digits_C_density )
L121.SoilC_kgm2_R_LT_AEZ[ AEZs ] <- round( L121.SoilC_kgm2_R_LT_AEZ[ AEZs ], digits = digits_C_density )
L121.MatureAge_R_LT_AEZ[ AEZs ] <- round( L121.MatureAge_R_LT_AEZ[ AEZs ], digits = digits_MatureAge )

#Remove all missing values
L121.VegC_kgm2_R_LT_AEZ[ is.na( L121.VegC_kgm2_R_LT_AEZ ) ] <- 0
L121.SoilC_kgm2_R_LT_AEZ[ is.na( L121.SoilC_kgm2_R_LT_AEZ ) ] <- 0
L121.MatureAge_R_LT_AEZ[ is.na( L121.MatureAge_R_LT_AEZ ) ] <- 0

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L121.VegC_kgm2_R_LT_AEZ <- c( "Vegetation carbon density by region and land type","Unit = kgC / m2" )
comments.L121.SoilC_kgm2_R_LT_AEZ <- c( "Soil carbon density by region and land type","Unit = kgC / m2" )
comments.L121.MatureAge_R_LT_AEZ <- c( "Mature age by region and land type","Unit = years" )

writedata( L121.VegC_kgm2_R_LT_AEZ, domain="AGLU_LEVEL1_DATA", fn="L121.VegC_kgm2_R_LT_AEZ", comments=comments.L121.VegC_kgm2_R_LT_AEZ )
writedata( L121.SoilC_kgm2_R_LT_AEZ, domain="AGLU_LEVEL1_DATA", fn="L121.SoilC_kgm2_R_LT_AEZ", comments=comments.L121.SoilC_kgm2_R_LT_AEZ )
writedata( L121.MatureAge_R_LT_AEZ, domain="AGLU_LEVEL1_DATA", fn="L121.MatureAge_R_LT_AEZ", comments=comments.L121.MatureAge_R_LT_AEZ )

# Every script should finish with this line
logstop()
      
