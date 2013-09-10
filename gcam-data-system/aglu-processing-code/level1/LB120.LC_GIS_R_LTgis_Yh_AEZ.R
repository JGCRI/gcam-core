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
logstart( "LB120.LC_GIS_R_LTgis_Yh_AEZ.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Land cover by region / GIS land types / historical year / AEZ" )

# -----------------------------------------------------------------------------
# 1. Read data from GIS mapping

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS","iso_GCAM_regID" )
GIS_ctry_AEZ <- readdata( "AGLU_MAPPINGS","GIS_ctry_AEZ" )
GIS_land_types <- readdata( "AGLU_MAPPINGS","GIS_land_types" )
SAGE_LT <- readdata( "AGLU_MAPPINGS","SAGE_LT" )
Sage_Hyde15_Area <- readdata( "AGLU_GIS_DATA", "Sage_Hyde15_Area" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Add vectors for GCAM region ID and AEZ
Sage_Hyde15_Area$iso <- GIS_ctry_AEZ$iso[ match( Sage_Hyde15_Area$AEZ_ID, GIS_ctry_AEZ$AEZ_ID ) ]
Sage_Hyde15_Area$AEZ <- GIS_ctry_AEZ$AEZ[ match( Sage_Hyde15_Area$AEZ_ID, GIS_ctry_AEZ$AEZ_ID ) ]
Sage_Hyde15_Area$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( Sage_Hyde15_Area$iso, iso_GCAM_regID$iso ) ]

#Add vectors for land type ( SAGE, HYDE, and WDPA )
printlog( "Mapping in land types from different GIS databases" )
Sage_Hyde15_Area[ c( "LT_SAGE", "LT_HYDE", "LT_WDPA" ) ] <- GIS_land_types[
      match( Sage_Hyde15_Area$Category, GIS_land_types$Category ),
      c( "LT_SAGE", "LT_HYDE", "LT_WDPA" ) ]
Sage_Hyde15_Area$LT_SAGE_5 <- SAGE_LT$Land_Type[ match( Sage_Hyde15_Area$LT_SAGE, SAGE_LT$LT_SAGE ) ]

#Drop all rows with missing values ( bodies of water )
printlog( "Dropping inland water" )
Sage_Hyde15_Area <- na.omit( Sage_Hyde15_Area )

#Reset WDPA classification to "Non-protected" where HYDE classification is cropland, pasture, or urban land
Sage_Hyde15_Area$LT_WDPA[ Sage_Hyde15_Area$LT_HYDE!="Unmanaged" ] <- "Non-protected"

#These multi-tiered classifications will be used for C contents, but for all land cover processing, collapse into GCAM land types
printlog ( "Collapsing multi-tiered GIS land types into a single vector of aggregate land types" )
Sage_Hyde15_Area$Land_Type <- Sage_Hyde15_Area$LT_SAGE_5
#Need to add levels to new variable Land_Type
#NOTE: To include protected lands: Add a category called "Protected" to the list below
Sage_Hyde15_Area$Land_Type[ Sage_Hyde15_Area$LT_HYDE=="Cropland" ] <- "Cropland"
Sage_Hyde15_Area$Land_Type[ Sage_Hyde15_Area$LT_HYDE=="Pasture" ] <- "Pasture"
Sage_Hyde15_Area$Land_Type[ Sage_Hyde15_Area$LT_HYDE=="Urbanland" ] <- "UrbanLand"    #This is the only land type whose "land" is capitalized here
#NOTE: To include protected lands, uncomment the line below
#Sage_Hyde15_Area$Land_Type[ Sage_Hyde15_Area$LT_WDPA=="Protected" ] <- "Protected"

#Add vector for area in thousand square kilometers (bm2)
Sage_Hyde15_Area$Area_bm2 <- Sage_Hyde15_Area$Area.km2. * conv_km2_bm2

#LAND COVER FOR LAND ALLOCATION
#Aggregate into GCAM regions and land types. This table is incomplete (missing non-existent combinations), indicated by LCi
printlog( "Part 1: Land cover by GCAM land category in all model history/base years" )
printlog( "Collapsing land cover into GCAM regions and aggregate land types" )
names( Sage_Hyde15_Area )[ names( Sage_Hyde15_Area ) == "Year" ] <- Y
L120.LCi_bm2_R_LT_year_AEZ <- aggregate( Sage_Hyde15_Area[ "Area_bm2" ], by=as.list( Sage_Hyde15_Area[ R_LT_Y_AEZ ] ), sum )

#Cast by year and interpolate to include all desired years
L120.LCi_bm2_R_LT_year_AEZ$Xyear <- paste( "X", L120.LCi_bm2_R_LT_year_AEZ$year, sep = "" )
L120.LCi_bm2_R_LT_year_AEZ.cast <- cast( L120.LCi_bm2_R_LT_year_AEZ, GCAM_region_ID + Land_Type + AEZ ~ Xyear, value = "Area_bm2" )

###TEMPORARY STEP: SET 2010 EQUAL TO 2005 IN ABSENCE OF ANY REAL DATA FOR 2010
L120.LCi_bm2_R_LT_year_AEZ.cast$X2010 <- L120.LCi_bm2_R_LT_year_AEZ.cast$X2005
L120.LCi_bm2_R_LT_allyear_AEZ.cast <- gcam_interp( L120.LCi_bm2_R_LT_year_AEZ.cast, land_cover_years )

#Reset to a data frame, melt, and drop the "X" prefix from the year vector
L120.LCi_bm2_R_LT_allyear_AEZ.cast <- data.frame( L120.LCi_bm2_R_LT_allyear_AEZ.cast[ R_LT_AEZ ], L120.LCi_bm2_R_LT_allyear_AEZ.cast[ X_land_cover_years ] )

#Create "complete" table with all possible combinations of regions, land types, AEZs, and years
printlog( "Expanding table to include all possible combinations of region, land type, year, and AEZ" )
L120.LC_bm2_R_LT_Yh_AEZ <- translate_to_full_table( L120.LCi_bm2_R_LT_allyear_AEZ.cast,
      var1 = R, var1_values = sort( unique( iso_GCAM_regID$GCAM_region_ID ) ),
      var2 = LT, var2_values = unique( L120.LCi_bm2_R_LT_year_AEZ$Land_Type ),
      var3 = AEZ, var3_values = AEZs,
      datacols = X_land_cover_years )

#Subset the land types that are not further modified, and write them out
L120.LC_bm2_R_UrbanLand_Yh_AEZ <- L120.LC_bm2_R_LT_Yh_AEZ[ L120.LC_bm2_R_LT_Yh_AEZ[[LT]] =="UrbanLand", ]
L120.LC_bm2_R_Tundra_Yh_AEZ <- L120.LC_bm2_R_LT_Yh_AEZ[ L120.LC_bm2_R_LT_Yh_AEZ[[LT]] =="Tundra", ]
L120.LC_bm2_R_RckIceDsrt_Yh_AEZ <- L120.LC_bm2_R_LT_Yh_AEZ[ L120.LC_bm2_R_LT_Yh_AEZ[[LT]] =="RockIceDesert", ]
#NOTE: If protected lands are included, uncomment the text below
#LC_bm2_R_Protected_Yh_AEZ <- L120.LC_bm2_R_LT_Yh_AEZ[ L120.LC_bm2_R_LT_Yh_AEZ[[LT]] =="Protected", ]

#LAND COVER FOR CARBON CONTENT CALCULATION
printlog ( "Part 2: Land cover by Sage14 category for carbon content calculation" )
printlog ( "Collapsing land cover into GCAM regions and SAGE 14 land types" )
Sage_Hyde15_Area_by <- subset( Sage_Hyde15_Area, year == max( year ) )
L120.LCi_bm2_R_LTsage_AEZ <- aggregate( Sage_Hyde15_Area_by[ "Area_bm2" ],
      by=as.list( Sage_Hyde15_Area_by[ c( R, "LT_SAGE", AEZ ) ] ), sum )
L120.LCi_bm2_R_LTsage_AEZ.cast <- cast( L120.LCi_bm2_R_LTsage_AEZ, GCAM_region_ID + LT_SAGE ~ AEZ, value = "Area_bm2" )
L120.LCi_bm2_R_LTsage_AEZ.cast[ is.na( L120.LCi_bm2_R_LTsage_AEZ.cast ) ] <- 0

#Create "complete" table with all possible combinations of regions, land types, AEZs, and years
printlog( "Expanding table to include all possible combinations of region, land type, and AEZ" )
L120.LC_bm2_R_LTsage_AEZ <- translate_to_full_table( L120.LCi_bm2_R_LTsage_AEZ.cast,
      var1 = R, var1_values = sort( unique( iso_GCAM_regID$GCAM_region_ID ) ),
      var2 = "LT_SAGE", var2_values = unique( L120.LCi_bm2_R_LTsage_AEZ.cast$LT_SAGE ),
      datacols = AEZs  )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments 
comments.L120.LC_bm2_R_LT_Yh_AEZ <- c( "Land cover by GCAM region / aggregate land type / historical year / AEZ","Unit = bm2" )
comments.L120.LC_bm2_R_UrbanLand_Yh_AEZ <- c( "Urban land cover by GCAM region / historical year / AEZ","bm2" )
comments.L120.LC_bm2_R_Tundra_Yh_AEZ <- c( "Tundra land cover by GCAM region / historical year / AEZ","bm2" )
comments.L120.LC_bm2_R_RckIceDsrt_Yh_AEZ <- c( "Rock/ice/desert land cover by GCAM region / historical year / AEZ","bm2" )
#comments.LC_bm2_R_Protected_Yh_AEZ <- c( "Protected land cover by GCAM region / historical year / AEZ","bm2" )
comments.L120.LC_bm2_R_LTsage_AEZ <- c( "Land cover by GCAM region / SAGE15 land type / AEZ","Unit = bm2" )

#Write it out
writedata( L120.LC_bm2_R_LT_Yh_AEZ, domain="AGLU_LEVEL1_DATA", fn="L120.LC_bm2_R_LT_Yh_AEZ", comments=comments.L120.LC_bm2_R_LT_Yh_AEZ )
writedata( L120.LC_bm2_R_UrbanLand_Yh_AEZ, domain="AGLU_LEVEL1_DATA", fn="L120.LC_bm2_R_UrbanLand_Yh_AEZ", comments=comments.L120.LC_bm2_R_UrbanLand_Yh_AEZ )
writedata( L120.LC_bm2_R_Tundra_Yh_AEZ, domain="AGLU_LEVEL1_DATA", fn="L120.LC_bm2_R_Tundra_Yh_AEZ", comments=comments.L120.LC_bm2_R_Tundra_Yh_AEZ )
writedata( L120.LC_bm2_R_RckIceDsrt_Yh_AEZ, domain="AGLU_LEVEL1_DATA", fn="L120.LC_bm2_R_RckIceDsrt_Yh_AEZ", comments=comments.L120.LC_bm2_R_RckIceDsrt_Yh_AEZ )
#writedata( LC_bm2_R_Protected_Yh_AEZ, domain="AGLU_LEVEL1_DATA", fn="LC_bm2_R_Protected_Yh_AEZ", comments=comments.LC_bm2_R_Protected_Yh_AEZ )
writedata( L120.LC_bm2_R_LTsage_AEZ, domain="AGLU_LEVEL1_DATA", fn="L120.LC_bm2_R_LTsage_AEZ", comments=comments.L120.LC_bm2_R_LTsage_AEZ )

# Every script should finish with this line
logstop()
