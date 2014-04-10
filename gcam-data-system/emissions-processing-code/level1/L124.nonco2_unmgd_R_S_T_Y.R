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
printlog( "Historical emissions by GCAM technology, computed from EDGAR emissions data and EPA emissions factors" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
EDGAR_nation <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_nation" )
EDGAR_sector <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_sector" )
L124.LC_bm2_R_Grass_Yh_AEZ_adj <- readdata( "AGLU_LEVEL1_DATA", "L124.LC_bm2_R_Grass_Yh_AEZ_adj" )
L124.LC_bm2_R_UnMgdFor_Yh_AEZ_adj <- readdata( "AGLU_LEVEL1_DATA", "L124.LC_bm2_R_UnMgdFor_Yh_AEZ_adj" )
EDGAR_SO2 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_SO2" )
EDGAR_CO <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_CO" )
EDGAR_NOx <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NOx" )
EDGAR_VOC <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_NMVOC" )
EDGAR_CH4 <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_CH4" )
EDGAR_N2O <- readdata( "EMISSIONS_LEVEL0_DATA", "EDGAR_N2O" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Compute share of regional land area by technology" )
L124.grass_bm2_R_Y <- aggregate( L124.LC_bm2_R_Grass_Yh_AEZ_adj[ X_historical_years ], by=as.list( L124.LC_bm2_R_Grass_Yh_AEZ_adj[ c( "GCAM_region_ID" ) ] ), sum )
L124.grass_bm2_R_Y <- melt( L124.grass_bm2_R_Y, id.vars=c( "GCAM_region_ID" ) )
L124.grass_pct_R_Y_AEZ.melt <- melt( L124.LC_bm2_R_Grass_Yh_AEZ_adj, id.vars=c( "GCAM_region_ID", "Land_Type", "AEZ" ))
L124.grass_pct_R_Y_AEZ.melt$total_land <- L124.grass_bm2_R_Y$value[ match( vecpaste( L124.grass_pct_R_Y_AEZ.melt[ c( "GCAM_region_ID", "variable" )]), vecpaste( L124.grass_bm2_R_Y[ c( "GCAM_region_ID", "variable" )]))]
L124.grass_pct_R_Y_AEZ.melt$land_share <- L124.grass_pct_R_Y_AEZ.melt$value / L124.grass_pct_R_Y_AEZ.melt$total_land
L124.grass_pct_R_Y_AEZ.melt <- subset( L124.grass_pct_R_Y_AEZ.melt, L124.grass_pct_R_Y_AEZ.melt$variable %in% X_historical_years )

L124.forest_bm2_R_Y <- aggregate( L124.LC_bm2_R_UnMgdFor_Yh_AEZ_adj[ X_historical_years ], by=as.list( L124.LC_bm2_R_UnMgdFor_Yh_AEZ_adj[ c( "GCAM_region_ID" ) ] ), sum )
L124.forest_bm2_R_Y <- melt( L124.forest_bm2_R_Y, id.vars=c( "GCAM_region_ID" ) )
L124.forest_pct_R_Y_AEZ.melt <- melt( L124.LC_bm2_R_UnMgdFor_Yh_AEZ_adj, id.vars=c( "GCAM_region_ID", "Land_Type", "AEZ" ))
L124.forest_pct_R_Y_AEZ.melt$total_land <- L124.forest_bm2_R_Y$value[ match( vecpaste( L124.forest_pct_R_Y_AEZ.melt[ c( "GCAM_region_ID", "variable" )]), vecpaste( L124.forest_bm2_R_Y[ c( "GCAM_region_ID", "variable" )]))]
L124.forest_pct_R_Y_AEZ.melt$land_share <- L124.forest_pct_R_Y_AEZ.melt$value / L124.forest_pct_R_Y_AEZ.melt$total_land
L124.forest_pct_R_Y_AEZ.melt <- subset( L124.forest_pct_R_Y_AEZ.melt, L124.forest_pct_R_Y_AEZ.melt$variable %in% X_historical_years )

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

#Drop unnecessary columns, aggregate by region, and melt
L124.EDGAR <- L124.EDGAR[ names( L124.EDGAR ) %!in% c( "IPCC.Annex", "World.Region", "ISO_A3", "iso", "Name", "IPCC", "IPCC_description" ) ]
L124.EDGAR <- na.omit( L124.EDGAR )
L124.EDGAR <- aggregate( L124.EDGAR[ X_EDGAR_historical_years ], by=as.list( L124.EDGAR[ c( "GCAM_region_ID", "Non.CO2", "sector")]), sum)
L124.EDGAR.melt <- melt( L124.EDGAR, id.vars = c( "GCAM_region_ID", "Non.CO2", "sector" ))

#Convert to Tg
L124.EDGAR.melt$value <- L124.EDGAR.melt$value / 1000.0 

#Subset for grassland and forest
L124.EDGAR_grass.melt <- subset( L124.EDGAR.melt, L124.EDGAR.melt$sector == "grassland" )
L124.EDGAR_forest.melt <- subset( L124.EDGAR.melt, L124.EDGAR.melt$sector == "forest" )

printlog( "Compute grassland emissions by GCAM region and AEZ" )
L124.nonco2_tg_R_grass_Y_AEZ.melt <- L124.grass_pct_R_Y_AEZ.melt[ names( L124.grass_pct_R_Y_AEZ.melt ) %in% c( "GCAM_region_ID", "AEZ", "variable", "land_share" )]
L124.nonco2_tg_R_grass_Y_AEZ.melt <- repeat_and_add_vector( L124.nonco2_tg_R_grass_Y_AEZ.melt, "Non.CO2", c( "CO", "NMVOC", "SO2", "NOx", "CH4", "N2O") )
L124.nonco2_tg_R_grass_Y_AEZ.melt$total_emiss <- L124.EDGAR_grass.melt$value[ match( vecpaste( L124.nonco2_tg_R_grass_Y_AEZ.melt[ c( "GCAM_region_ID", "Non.CO2", "variable" )]), vecpaste( L124.EDGAR_grass.melt[ c( "GCAM_region_ID", "Non.CO2", "variable" )] ) )]
L124.nonco2_tg_R_grass_Y_AEZ.melt$emissions <- L124.nonco2_tg_R_grass_Y_AEZ.melt$total_emiss * L124.nonco2_tg_R_grass_Y_AEZ.melt$land_share
L124.nonco2_tg_R_grass_Y_AEZ.melt <- na.omit( L124.nonco2_tg_R_grass_Y_AEZ.melt) 

#Reshape
L124.nonco2_tg_R_grass_Y_AEZ <- dcast( L124.nonco2_tg_R_grass_Y_AEZ.melt, GCAM_region_ID + Non.CO2 + AEZ ~ variable, value = c( "emissions" ) )

printlog( "Compute forest emissions by GCAM region and AEZ" )
L124.nonco2_tg_R_forest_Y_AEZ.melt <- L124.forest_pct_R_Y_AEZ.melt[ names( L124.forest_pct_R_Y_AEZ.melt ) %in% c( "GCAM_region_ID", "AEZ", "variable", "land_share" )]
L124.nonco2_tg_R_forest_Y_AEZ.melt <- repeat_and_add_vector( L124.nonco2_tg_R_forest_Y_AEZ.melt, "Non.CO2", c( "CO", "NMVOC", "SO2", "NOx", "CH4", "N2O") )
L124.nonco2_tg_R_forest_Y_AEZ.melt$total_emiss <- L124.EDGAR_forest.melt$value[ match( vecpaste( L124.nonco2_tg_R_forest_Y_AEZ.melt[ c( "GCAM_region_ID", "Non.CO2", "variable" )]), vecpaste( L124.EDGAR_forest.melt[ c( "GCAM_region_ID", "Non.CO2", "variable" )] ) )]
L124.nonco2_tg_R_forest_Y_AEZ.melt$emissions <- L124.nonco2_tg_R_forest_Y_AEZ.melt$total_emiss * L124.nonco2_tg_R_forest_Y_AEZ.melt$land_share
L124.nonco2_tg_R_forest_Y_AEZ.melt <- na.omit( L124.nonco2_tg_R_forest_Y_AEZ.melt )

#Reshape
L124.nonco2_tg_R_forest_Y_AEZ <- dcast( L124.nonco2_tg_R_forest_Y_AEZ.melt, GCAM_region_ID + Non.CO2 + AEZ ~ variable, value = c( "emissions" ) )

printlog( "Compute average emissions factors for forest and grassland")
#These emissions factors will be used from 2005 and beyond
L124.grass_bm2_Y <- aggregate( L124.grass_bm2_R_Y$value, by=as.list( L124.grass_bm2_R_Y[ c( "variable" ) ] ), sum  )
L124.forest_bm2_Y <- aggregate( L124.forest_bm2_R_Y$value, by=as.list( L124.forest_bm2_R_Y[ c( "variable" ) ] ), sum  )

L124.nonco2_tg_grass_Y.melt <- aggregate( L124.nonco2_tg_R_grass_Y_AEZ.melt$emissions, by=as.list( L124.nonco2_tg_R_grass_Y_AEZ.melt[ c( "Non.CO2", "variable" ) ] ), sum )
L124.nonco2_tg_forest_Y.melt <- aggregate( L124.nonco2_tg_R_forest_Y_AEZ.melt$emissions, by=as.list( L124.nonco2_tg_R_forest_Y_AEZ.melt[ c( "Non.CO2", "variable" ) ] ), sum )

L124.nonco2_tgbm2_grass_Y.melt <- L124.nonco2_tg_grass_Y.melt
names( L124.nonco2_tgbm2_grass_Y.melt )[ names( L124.nonco2_tgbm2_grass_Y.melt ) == "x" ] <- "emissions"
L124.nonco2_tgbm2_grass_Y.melt$land_area <- L124.grass_bm2_Y$x[ match( L124.nonco2_tgbm2_grass_Y.melt$variable, L124.grass_bm2_Y$variable )]
L124.nonco2_tgbm2_grass_Y.melt$land_type <- "grassland"
L124.nonco2_tgbm2_grass_Y.melt$em_fact <- L124.nonco2_tgbm2_grass_Y.melt$emissions / L124.nonco2_tgbm2_grass_Y.melt$land_area
L124.nonco2_tgbm2_grass_fy.melt <- subset( L124.nonco2_tgbm2_grass_Y.melt, L124.nonco2_tgbm2_grass_Y.melt$variable == paste( "X", final_emiss_year, sep="" ))
L124.nonco2_tgbm2_grass_fy.melt <- L124.nonco2_tgbm2_grass_fy.melt[ names( L124.nonco2_tgbm2_grass_fy.melt ) %in% c( "Non.CO2", "land_type", "em_fact" )]

L124.nonco2_tgbm2_forest_Y.melt <- L124.nonco2_tg_forest_Y.melt
names( L124.nonco2_tgbm2_forest_Y.melt )[ names( L124.nonco2_tgbm2_forest_Y.melt ) == "x" ] <- "emissions"
L124.nonco2_tgbm2_forest_Y.melt$land_area <- L124.forest_bm2_Y$x[ match( L124.nonco2_tgbm2_forest_Y.melt$variable, L124.forest_bm2_Y$variable )]
L124.nonco2_tgbm2_forest_Y.melt$land_type <- "forest"
L124.nonco2_tgbm2_forest_Y.melt$em_fact <- L124.nonco2_tgbm2_forest_Y.melt$emissions / L124.nonco2_tgbm2_forest_Y.melt$land_area
L124.nonco2_tgbm2_forest_fy.melt <- subset( L124.nonco2_tgbm2_forest_Y.melt, L124.nonco2_tgbm2_forest_Y.melt$variable == paste( "X", final_emiss_year, sep="" ))
L124.nonco2_tgbm2_forest_fy.melt <- L124.nonco2_tgbm2_forest_fy.melt[ names( L124.nonco2_tgbm2_forest_fy.melt ) %in% c( "Non.CO2", "land_type", "em_fact" )]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L124.nonco2_tg_R_grass_Y_AEZ <- c( "Grassland fire emissions by GCAM region / commodity / historical year", "Unit = Tg" )
comments.L124.nonco2_tg_R_forest_Y_AEZ <- c( "Forest fire emissions by GCAM region / commodity / historical year", "Unit = Tg" )
comments.L124.nonco2_tgbm2_grass_fy.melt <- c( "Average grassland fire emissions factors in the final historical year", "Unit = Tg / bm2" )
comments.L124.nonco2_tgbm2_forest_fy.melt <- c( "Average forest fire emissions factors in the final historical year", "Unit = Tg / bm2" )

#write tables as CSV files
writedata( L124.nonco2_tg_R_grass_Y_AEZ, domain="EMISSIONS_LEVEL1_DATA", fn="L124.nonco2_tg_R_grass_Y_AEZ", comments=comments.L124.nonco2_tg_R_grass_Y_AEZ )
writedata( L124.nonco2_tg_R_forest_Y_AEZ, domain="EMISSIONS_LEVEL1_DATA", fn="L124.nonco2_tg_R_forest_Y_AEZ", comments=comments.L124.nonco2_tg_R_forest_Y_AEZ )
writedata( L124.nonco2_tgbm2_grass_fy.melt, domain="EMISSIONS_LEVEL1_DATA", fn="L124.nonco2_tgbm2_grass_fy.melt", comments=comments.L124.nonco2_tgbm2_grass_fy.melt )
writedata( L124.nonco2_tgbm2_forest_fy.melt, domain="EMISSIONS_LEVEL1_DATA", fn="L124.nonco2_tgbm2_forest_fy.melt", comments=comments.L124.nonco2_tgbm2_forest_fy.melt )

# Every script should finish with this line
logstop()
