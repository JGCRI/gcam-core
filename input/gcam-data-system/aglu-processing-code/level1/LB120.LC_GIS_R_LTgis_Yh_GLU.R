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
logstart( "LB120.LC_GIS_R_LTgis_Yh_GLU.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Land cover by region / GIS land types / historical year / GLU" )

# -----------------------------------------------------------------------------
# 1. Read data from GIS mapping

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS","iso_GCAM_regID" )
LDS_land_types <- readdata( "AGLU_MAPPINGS","LDS_land_types" )
SAGE_LT <- readdata( "AGLU_MAPPINGS","SAGE_LT" )
L100.Land_type_area_ha <- readdata( "AGLU_LEVEL1_DATA", "L100.Land_type_area_ha" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Add vectors for GCAM region ID and GLU
L100.Land_type_area_ha$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L100.Land_type_area_ha$iso, iso_GCAM_regID$iso ) ]

#Add vectors for land type ( SAGE, HYDE, and WDPA )
printlog( "Mapping in land types from different GIS databases" )
L100.Land_type_area_ha[ c( "LT_SAGE", "LT_HYDE", "LT_WDPA" ) ] <- LDS_land_types[
      match( L100.Land_type_area_ha$land_code, LDS_land_types$Category ),
      c( "LT_SAGE", "LT_HYDE", "LT_WDPA" ) ]
L100.Land_type_area_ha$LT_SAGE_5 <- SAGE_LT$Land_Type[ match( L100.Land_type_area_ha$LT_SAGE, SAGE_LT$LT_SAGE ) ]

#Drop all rows with missing values ( bodies of water )
printlog( "Dropping inland water" )
L100.Land_type_area_ha <- na.omit( L100.Land_type_area_ha )

#Reset WDPA classification to "Non-protected" where HYDE classification is cropland, pasture, or urban land
L100.Land_type_area_ha$LT_WDPA[ L100.Land_type_area_ha$LT_HYDE!="Unmanaged" ] <- "Non-protected"

#These multi-tiered classifications will be used for C contents, but for all land cover processing, collapse into GCAM land types
printlog ( "Collapsing multi-tiered GIS land types into a single vector of aggregate land types" )
L100.Land_type_area_ha$Land_Type <- L100.Land_type_area_ha$LT_SAGE_5
L100.Land_type_area_ha$Land_Type[ L100.Land_type_area_ha$LT_HYDE=="Cropland" ] <- "Cropland"
L100.Land_type_area_ha$Land_Type[ L100.Land_type_area_ha$LT_HYDE=="Pasture" ] <- "Pasture"
L100.Land_type_area_ha$Land_Type[ L100.Land_type_area_ha$LT_HYDE=="UrbanLand" ] <- "UrbanLand"

#Add vector for area in thousand square kilometers (bm2)
L100.Land_type_area_ha$Area_bm2 <- L100.Land_type_area_ha$value * conv_Ha_bm2

#LAND COVER FOR LAND ALLOCATION
#Aggregate into GCAM regions and land types. This table is incomplete (missing non-existent combinations), indicated by LCi
printlog( "Part 1: Land cover by GCAM land category in all model history/base years" )
printlog( "Collapsing land cover into GCAM regions and aggregate land types" )
L120.LCi_bm2_R_LT_year_GLU <- aggregate( L100.Land_type_area_ha[ "Area_bm2" ], by=as.list( L100.Land_type_area_ha[ R_LT_Y_GLU ] ), sum )

#Cast by year and interpolate to include all desired years
L120.LCi_bm2_R_LT_year_GLU$Xyear <- paste( "X", L120.LCi_bm2_R_LT_year_GLU$year, sep = "" )
L120.LCi_bm2_R_LT_year_GLU.cast <- dcast( L120.LCi_bm2_R_LT_year_GLU, GCAM_region_ID + Land_Type + GLU ~ Xyear, value.var = "Area_bm2" )

#Missing values should be set to 0 before interpolation, so that in-between years are interpolated correctly
L120.LCi_bm2_R_LT_year_GLU.cast[ is.na( L120.LCi_bm2_R_LT_year_GLU.cast ) ] <- 0
L120.LC_bm2_R_LT_Yh_GLU <- gcam_interp( L120.LCi_bm2_R_LT_year_GLU.cast, land_cover_years )

#Reset to a data frame and re-sort the years
L120.LC_bm2_R_LT_Yh_GLU <- data.frame( L120.LC_bm2_R_LT_Yh_GLU[ R_LT_GLU ], L120.LC_bm2_R_LT_Yh_GLU[ X_land_cover_years ] )

#Subset the land types that are not further modified, and write them out
L120.LC_bm2_R_UrbanLand_Yh_GLU <- L120.LC_bm2_R_LT_Yh_GLU[ L120.LC_bm2_R_LT_Yh_GLU[[LT]] =="UrbanLand", ]
L120.LC_bm2_R_Tundra_Yh_GLU <- L120.LC_bm2_R_LT_Yh_GLU[ L120.LC_bm2_R_LT_Yh_GLU[[LT]] =="Tundra", ]
L120.LC_bm2_R_RckIceDsrt_Yh_GLU <- L120.LC_bm2_R_LT_Yh_GLU[ L120.LC_bm2_R_LT_Yh_GLU[[LT]] =="RockIceDesert", ]

#LAND COVER FOR CARBON CONTENT CALCULATION
# Note: not just using the final year, as some land use types may have gone to zero over the historical period.
# Instead, just use the median of the available years within our "historical" years
L120.LC_bm2_ctry_LTsage_GLU_Y <- subset( L100.Land_type_area_ha, LT_HYDE == "Unmanaged" )[
  c( "iso", R_GLU, "land_code", "LT_SAGE", LT, Y, "Area_bm2" ) ]
L120.LC_bm2_ctry_LTsage_GLU <- aggregate( L120.LC_bm2_ctry_LTsage_GLU_Y[ "Area_bm2" ],
                                          by = L120.LC_bm2_ctry_LTsage_GLU_Y[ c( "iso", R_GLU, "land_code", "LT_SAGE", LT ) ],
                                          mean )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments 
comments.L120.LC_bm2_R_LT_Yh_GLU <- c( "Land cover by GCAM region / aggregate land type / historical year / GLU","Unit = bm2" )
comments.L120.LC_bm2_R_UrbanLand_Yh_GLU <- c( "Urban land cover by GCAM region / historical year / GLU","bm2" )
comments.L120.LC_bm2_R_Tundra_Yh_GLU <- c( "Tundra land cover by GCAM region / historical year / GLU","bm2" )
comments.L120.LC_bm2_R_RckIceDsrt_Yh_GLU <- c( "Rock/ice/desert land cover by GCAM region / historical year / GLU","bm2" )
comments.L120.LC_bm2_ctry_LTsage_GLU <- c( "Land cover by country / SAGE15 land type / GLU","Unit = bm2" )

#Write it out
writedata( L120.LC_bm2_R_LT_Yh_GLU, domain="AGLU_LEVEL1_DATA", fn="L120.LC_bm2_R_LT_Yh_GLU", comments=comments.L120.LC_bm2_R_LT_Yh_GLU )
writedata( L120.LC_bm2_R_UrbanLand_Yh_GLU, domain="AGLU_LEVEL1_DATA", fn="L120.LC_bm2_R_UrbanLand_Yh_GLU", comments=comments.L120.LC_bm2_R_UrbanLand_Yh_GLU )
writedata( L120.LC_bm2_R_Tundra_Yh_GLU, domain="AGLU_LEVEL1_DATA", fn="L120.LC_bm2_R_Tundra_Yh_GLU", comments=comments.L120.LC_bm2_R_Tundra_Yh_GLU )
writedata( L120.LC_bm2_R_RckIceDsrt_Yh_GLU, domain="AGLU_LEVEL1_DATA", fn="L120.LC_bm2_R_RckIceDsrt_Yh_GLU", comments=comments.L120.LC_bm2_R_RckIceDsrt_Yh_GLU )
writedata( L120.LC_bm2_ctry_LTsage_GLU, domain="AGLU_LEVEL1_DATA", fn="L120.LC_bm2_ctry_LTsage_GLU", comments=comments.L120.LC_bm2_ctry_LTsage_GLU )

# Every script should finish with this line
logstop()
