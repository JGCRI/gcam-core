# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "EMISSPROC_DIR" ) ){
    if( Sys.getenv( "EMISSIONSPROC" ) != "" ){
        EMISSPROC_DIR <- Sys.getenv( "EMISSIONSPROC" )
    } else {
        stop("Could not determine location of emissions data system. Please set the R var EMISSPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
logstart( "L124.nonco2_unmgd_R_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions from unmanaged land by land cover type, computed from EDGAR emissions data" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
EDGAR_nation <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_nation" )
EDGAR_sector <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_sector" )
L124.LC_bm2_R_Grass_Yh_GLU_adj <- readdata( "AGLU_LEVEL1_DATA", "L124.LC_bm2_R_Grass_Yh_GLU_adj" )
L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj <- readdata( "AGLU_LEVEL1_DATA", "L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj" )
EDGAR_SO2 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_SO2" )
EDGAR_CO <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_CO" )
EDGAR_NOx <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NOx" )
EDGAR_VOC <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NMVOC" )
EDGAR_CH4 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_CH4" )
EDGAR_N2O <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_N2O" )

GFED_ForestFire_SO2 <- readdata( "EMISSIONS_LEVEL0_DATA", "GFED_ForestFire_SO2" )
GFED_Deforest_SO2 <- readdata( "EMISSIONS_LEVEL0_DATA", "GFED_Deforest_SO2" )
GFED_ForestFire_CO <- readdata( "EMISSIONS_LEVEL0_DATA", "GFED_ForestFire_CO" )
GFED_Deforest_CO <- readdata( "EMISSIONS_LEVEL0_DATA", "GFED_Deforest_CO" )
GFED_ForestFire_NOx <- readdata( "EMISSIONS_LEVEL0_DATA", "GFED_ForestFire_NOx" )
GFED_Deforest_NOx <- readdata( "EMISSIONS_LEVEL0_DATA", "GFED_Deforest_NOx" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Compute share of regional land area by technology" )
L124.grass_bm2_R_Y <- aggregate( L124.LC_bm2_R_Grass_Yh_GLU_adj[ X_historical_years ],
                                 by = L124.LC_bm2_R_Grass_Yh_GLU_adj[ R ], sum )
L124.grass_bm2_R_Y <- melt( L124.grass_bm2_R_Y, id.vars = R, variable.name = Y )
L124.LCshare_grass_R_Y_GLU.melt <- melt( L124.LC_bm2_R_Grass_Yh_GLU_adj,
                                     id.vars = R_LT_GLU, measure.vars = X_historical_years, variable.name = Y )
L124.LCshare_grass_R_Y_GLU.melt$total_land <- L124.grass_bm2_R_Y$value[
  match( vecpaste( L124.LCshare_grass_R_Y_GLU.melt[ R_Y ] ),
         vecpaste( L124.grass_bm2_R_Y[ R_Y ] ) ) ]
L124.LCshare_grass_R_Y_GLU.melt$land_share <- with( L124.LCshare_grass_R_Y_GLU.melt, value / total_land )

L124.forest_bm2_R_Y <- aggregate( L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj[ X_historical_years ],
                                  by = L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj[ R ], sum )
L124.forest_bm2_R_Y <- melt( L124.forest_bm2_R_Y, id.vars = R, variable.name = Y )
L124.LCshare_forest_R_Y_GLU.melt <- melt( L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj,
                                          id.vars = R_LT_GLU, measure.vars = X_historical_years, variable.name = Y )
L124.LCshare_forest_R_Y_GLU.melt$total_land <- L124.forest_bm2_R_Y$value[
  match( vecpaste( L124.LCshare_forest_R_Y_GLU.melt[ R_Y ] ),
         vecpaste( L124.forest_bm2_R_Y[ R_Y ] ) ) ]
L124.LCshare_forest_R_Y_GLU.melt$land_share <- with( L124.LCshare_forest_R_Y_GLU.melt, value / total_land )

printlog( "Compute share of forest fire vs deforestation from GFED")
GFED_ForestFire_SO2$Non.CO2 <- "SO2"
GFED_Deforest_SO2$Non.CO2 <- "SO2"
GFED_ForestFire_CO$Non.CO2 <- "CO"
GFED_Deforest_CO$Non.CO2 <- "CO"
GFED_ForestFire_NOx$Non.CO2 <- "NOx"
GFED_Deforest_NOx$Non.CO2 <- "NOx"

# NMVOC is split into lots of inventories, so using CO for now. TODO: Get NMVOC data
GFED_ForestFire_NMVOC <- GFED_ForestFire_CO
GFED_ForestFire_NMVOC$Non.CO2 <- "NMVOC"
GFED_Deforest_NMVOC <- GFED_Deforest_CO
GFED_Deforest_NMVOC$Non.CO2 <- "NMVOC"

L124.GFED_ForestFire <- rbind( GFED_ForestFire_SO2, GFED_ForestFire_CO, GFED_ForestFire_NOx, GFED_ForestFire_NMVOC )
L124.GFED_Deforest <- rbind( GFED_Deforest_SO2, GFED_Deforest_CO, GFED_Deforest_NOx, GFED_Deforest_NMVOC )

L124.GFED_ForestFire.melt <- melt( L124.GFED_ForestFire,
                                   id.vars=c( "Country", "Non.CO2" ), variable.name = Y, value.name = "ForestFire" )
L124.GFED_Deforest.melt <- melt( L124.GFED_Deforest,
                                 id.vars=c( "Country", "Non.CO2" ), variable.name = Y, value.name = "Deforest" )

L124.GFED_ALL.melt <- L124.GFED_ForestFire.melt
L124.GFED_ALL.melt$Deforest <- L124.GFED_Deforest.melt$Deforest[
  match( vecpaste( L124.GFED_ALL.melt[ c( "Country", "Non.CO2", Y )]),
         vecpaste( L124.GFED_Deforest.melt[ c( "Country", "Non.CO2", Y )] ))]

#Aggregate by region, gas, and year
L124.GFED_ALL.melt$iso <- EDGAR_nation$iso[ match( L124.GFED_ALL.melt$Country, EDGAR_nation$ISO_A3 )]
L124.GFED_ALL.melt$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L124.GFED_ALL.melt$iso, iso_GCAM_regID$iso )]   
L124.GFED_ALL.melt <- aggregate( L124.GFED_ALL.melt[ c( "ForestFire", "Deforest" )], 
                                 by = L124.GFED_ALL.melt[ c( "GCAM_region_ID", "Non.CO2", Y )], sum )

L124.GFED_ALL.melt$PctForestFire <- L124.GFED_ALL.melt$ForestFire / ( L124.GFED_ALL.melt$ForestFire + L124.GFED_ALL.melt$Deforest )
L124.GFED_ALL.melt$PctForestFire[ is.na( L124.GFED_ALL.melt$PctForestFire ) ] <- 1
L124.GFED_ALL.melt$PctDeforest <- 1 - L124.GFED_ALL.melt$PctForestFire

printlog( "Compute EDGAR emissions by region" )
EDGAR_SO2$Non.CO2 <- "SO2"
EDGAR_CO$Non.CO2 <- "CO"
EDGAR_NOx$Non.CO2 <- "NOx"
EDGAR_VOC$Non.CO2 <- "NMVOC"
EDGAR_CH4$Non.CO2 <- "CH4"
EDGAR_N2O$Non.CO2 <- "N2O"
EDGAR_VOC <- EDGAR_VOC[ names( EDGAR_VOC ) %!in% c( "X2009", "X2010" )]
L124.EDGAR <- rbind( EDGAR_SO2, EDGAR_CO, EDGAR_NOx, EDGAR_VOC, EDGAR_CH4, EDGAR_N2O )
L124.EDGAR$sector <- EDGAR_sector$agg_sector[ match( L124.EDGAR$IPCC, EDGAR_sector$IPCC ) ]
L124.EDGAR$iso <- EDGAR_nation$iso[ match( L124.EDGAR$ISO_A3, EDGAR_nation$ISO_A3 )]
L124.EDGAR$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L124.EDGAR$iso, iso_GCAM_regID$iso )]   

# Extrapolate using average of last five years --- 2010 is problematic here because of deforestation
L124.EDGAR$X2010 <- rowMeans( L124.EDGAR[ c( "X2004", "X2005", "X2006", "X2007", "X2008" ) ] )

#Drop unnecessary columns, aggregate by region, and melt
L124.EDGAR <- L124.EDGAR[ names( L124.EDGAR ) %!in% c( "IPCC.Annex", "World.Region", "ISO_A3", "iso", "Name", "IPCC", "IPCC_description" ) ]
L124.EDGAR <- na.omit( L124.EDGAR )
L124.EDGAR <- aggregate( L124.EDGAR[ c( X_EDGAR_historical_years, "X2010" ) ],
                         by = L124.EDGAR[ c( "GCAM_region_ID", "Non.CO2", "sector" ) ], sum)
L124.EDGAR.melt <- melt( L124.EDGAR, id.vars = c( "GCAM_region_ID", "Non.CO2", "sector" ), variable.name = Y )

#Convert to Tg
L124.EDGAR.melt$value <- L124.EDGAR.melt$value * conv_Gg_Tg

#Subset for grassland and forest
L124.EDGAR_grass.melt <- subset( L124.EDGAR.melt, L124.EDGAR.melt$sector == "grassland" )
L124.EDGAR_forest.melt <- subset( L124.EDGAR.melt, L124.EDGAR.melt$sector == "forest" )

printlog( "Compute grassland emissions by GCAM region and GLU" )
L124.nonco2_tg_R_grass_Y_GLU.melt <- L124.LCshare_grass_R_Y_GLU.melt[ c( R_LT_Y_GLU, "land_share" ) ]
L124.nonco2_tg_R_grass_Y_GLU.melt <- repeat_and_add_vector( L124.nonco2_tg_R_grass_Y_GLU.melt, "Non.CO2", unique( L124.EDGAR_grass.melt$Non.CO2 ) )
L124.nonco2_tg_R_grass_Y_GLU.melt$total_emiss <- L124.EDGAR_grass.melt$value[
  match( vecpaste( L124.nonco2_tg_R_grass_Y_GLU.melt[ c( R_Y, "Non.CO2" ) ] ),
         vecpaste( L124.EDGAR_grass.melt[ c( R_Y, "Non.CO2" ) ] ) ) ]
L124.nonco2_tg_R_grass_Y_GLU.melt$emissions <- with( L124.nonco2_tg_R_grass_Y_GLU.melt, total_emiss * land_share )
L124.nonco2_tg_R_grass_Y_GLU.melt <- na.omit( L124.nonco2_tg_R_grass_Y_GLU.melt) 

#Reshape
L124.nonco2_tg_R_grass_Y_GLU <- dcast( L124.nonco2_tg_R_grass_Y_GLU.melt, GCAM_region_ID + Land_Type + Non.CO2 + GLU ~ year, value.var = c( "emissions" ) )

printlog( "Compute forest emissions by GCAM region and GLU" )
L124.nonco2_tg_R_forest_Y_GLU.melt <- L124.LCshare_forest_R_Y_GLU.melt[ c( R_LT_Y_GLU, "land_share" )]
L124.nonco2_tg_R_forest_Y_GLU.melt <- repeat_and_add_vector( L124.nonco2_tg_R_forest_Y_GLU.melt, "Non.CO2", unique( L124.EDGAR_forest.melt$Non.CO2 ) )
L124.nonco2_tg_R_forest_Y_GLU.melt$total_emiss <- L124.EDGAR_forest.melt$value[
  match( vecpaste( L124.nonco2_tg_R_forest_Y_GLU.melt[ c( R_Y, "Non.CO2" ) ] ),
         vecpaste( L124.EDGAR_forest.melt[ c( R_Y, "Non.CO2" ) ] ) ) ]
L124.nonco2_tg_R_forest_Y_GLU.melt$emissions <- with( L124.nonco2_tg_R_forest_Y_GLU.melt, total_emiss * land_share )
L124.nonco2_tg_R_forest_Y_GLU.melt <- na.omit( L124.nonco2_tg_R_forest_Y_GLU.melt )

#Split into ForestFire and Deforest
L124.nonco2_tg_R_forest_Y_GLU.melt$PctForestFire <- L124.GFED_ALL.melt$PctForestFire[
  match( vecpaste( L124.nonco2_tg_R_forest_Y_GLU.melt[ c( R_Y, "Non.CO2" ) ] ),
         vecpaste( L124.GFED_ALL.melt[ c( R_Y, "Non.CO2" )] ))]
L124.nonco2_tg_R_forest_Y_GLU.melt$PctDeforest <- 1 - L124.nonco2_tg_R_forest_Y_GLU.melt$PctForestFire

# If data is missing, assume all emissions are assigned to forest fires. These are easier to calibrate in GCAM.
L124.nonco2_tg_R_forest_Y_GLU.melt$PctForestFire[ is.na( L124.nonco2_tg_R_forest_Y_GLU.melt$PctForestFire ) ] <- 1.0
L124.nonco2_tg_R_forest_Y_GLU.melt$PctDeforest[ is.na( L124.nonco2_tg_R_forest_Y_GLU.melt$PctDeforest ) ] <- 0.0

L124.nonco2_tg_R_forest_Y_GLU.melt$ForestFire <- with( L124.nonco2_tg_R_forest_Y_GLU.melt, emissions * PctForestFire )
L124.nonco2_tg_R_forest_Y_GLU.melt$Deforest <- with( L124.nonco2_tg_R_forest_Y_GLU.melt, emissions * PctDeforest )

L124.nonco2_tg_R_forest_Y_GLU.melt <- L124.nonco2_tg_R_forest_Y_GLU.melt[ c( R_LT_Y_GLU, "Non.CO2", "ForestFire", "Deforest" ) ]
L124.nonco2_tg_R_forest_Y_GLU.melt <- melt( L124.nonco2_tg_R_forest_Y_GLU.melt,
                                            measure.vars = c( "ForestFire", "Deforest" ), variable.name = "technology" )

#Reshape
L124.nonco2_tg_R_forest_Y_GLU <- dcast( L124.nonco2_tg_R_forest_Y_GLU.melt,
                                        GCAM_region_ID + Land_Type + Non.CO2 + GLU + technology ~ year, value.var = c( "value" ) )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L124.nonco2_tg_R_grass_Y_GLU <- c( "Grassland fire emissions by GCAM region / land cover type / historical year", "Unit = Tg" )
comments.L124.nonco2_tg_R_forest_Y_GLU <- c( "Forest fire emissions by GCAM region / land cover type / historical year", "Unit = Tg" )

#write tables as CSV files
writedata( L124.nonco2_tg_R_grass_Y_GLU, domain="EMISSIONS_LEVEL1_DATA", fn="L124.nonco2_tg_R_grass_Y_GLU", comments=comments.L124.nonco2_tg_R_grass_Y_GLU )
writedata( L124.nonco2_tg_R_forest_Y_GLU, domain="EMISSIONS_LEVEL1_DATA", fn="L124.nonco2_tg_R_forest_Y_GLU", comments=comments.L124.nonco2_tg_R_forest_Y_GLU )

# Every script should finish with this line
logstop()
