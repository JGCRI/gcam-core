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
logstart( "L125.bcoc_unmgd_R_S_T_Y.R" )
adddep(paste(EMISSPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(EMISSPROC_DIR,"/../_common/headers/EMISSIONS_header.R",sep=""))
printlog( "Historical emissions from unmanaged land by land cover type, computed from RCP emissions data" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "EMISSIONS_ASSUMPTIONS", "A_emissions_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
EDGAR_nation <- readdata( "EMISSIONS_MAPPINGS", "EDGAR_nation" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
L124.LC_bm2_R_Grass_Yh_AEZ_adj <- readdata( "AGLU_LEVEL1_DATA", "L124.LC_bm2_R_Grass_Yh_AEZ_adj" )
L124.LC_bm2_R_UnMgdFor_Yh_AEZ_adj <- readdata( "AGLU_LEVEL1_DATA", "L124.LC_bm2_R_UnMgdFor_Yh_AEZ_adj" )
RCP_BC_2000 <- readdata( "EMISSIONS_LEVEL0_DATA", "RCP_BC_2000" )
RCP_OC_2000 <- readdata( "EMISSIONS_LEVEL0_DATA", "RCP_OC_2000" )

GFED_ForestFire_BC <- readdata( "EMISSIONS_LEVEL0_DATA", "GFED_ForestFire_BC" )
GFED_Deforest_BC <- readdata( "EMISSIONS_LEVEL0_DATA", "GFED_Deforest_BC" )
GFED_ForestFire_OC <- readdata( "EMISSIONS_LEVEL0_DATA", "GFED_ForestFire_OC" )
GFED_Deforest_OC <- readdata( "EMISSIONS_LEVEL0_DATA", "GFED_Deforest_OC" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Compute share of regional land area by technology" )
L125.grass_bm2_R_Y <- aggregate( L124.LC_bm2_R_Grass_Yh_AEZ_adj[ X_historical_years ], by=as.list( L124.LC_bm2_R_Grass_Yh_AEZ_adj[ c( "GCAM_region_ID" ) ] ), sum )
L125.grass_bm2_R_Y <- melt( L125.grass_bm2_R_Y, id.vars=c( "GCAM_region_ID" ) )

L125.forest_bm2_R_Y <- aggregate( L124.LC_bm2_R_UnMgdFor_Yh_AEZ_adj[ X_historical_years ], by=as.list( L124.LC_bm2_R_UnMgdFor_Yh_AEZ_adj[ c( "GCAM_region_ID" ) ] ), sum )
L125.forest_bm2_R_Y <- melt( L125.forest_bm2_R_Y, id.vars=c( "GCAM_region_ID" ) )

printlog( "Compute fraction of emissions from deforestation vs forest fires" )
GFED_ForestFire_BC$Non.CO2 <- "BC"
GFED_Deforest_BC$Non.CO2 <- "BC"
GFED_ForestFire_OC$Non.CO2 <- "OC"
GFED_Deforest_OC$Non.CO2 <- "OC"

L125.GFED_ForestFire <- rbind( GFED_ForestFire_BC, GFED_ForestFire_OC )
L125.GFED_Deforest <- rbind( GFED_Deforest_BC, GFED_Deforest_OC )

L125.GFED_ForestFire <- L125.GFED_ForestFire[ names( L125.GFED_ForestFire) %in% c( "Country", "Non.CO2", "X2000" ) ]
names( L125.GFED_ForestFire )[ names( L125.GFED_ForestFire ) == "X2000" ] <- "ForestFire" 
L125.GFED_Deforest <- L125.GFED_Deforest[ names( L125.GFED_Deforest ) %in% c( "Country", "Non.CO2", "X2000" ) ]
names( L125.GFED_Deforest )[ names( L125.GFED_Deforest ) == "X2000" ] <- "Deforest"

L125.GFED_ALL <- L125.GFED_ForestFire
L125.GFED_ALL$Deforest <- L125.GFED_Deforest$Deforest[ match( vecpaste( L125.GFED_ALL[ c( "Country", "Non.CO2" )]),
                                                              vecpaste( L125.GFED_Deforest[ c( "Country", "Non.CO2" ) ]) )]
L125.GFED_ALL$iso <- EDGAR_nation$iso[ match( L125.GFED_ALL$Country, EDGAR_nation$ISO_A3 )]
L125.GFED_ALL$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L125.GFED_ALL$iso, iso_GCAM_regID$iso )]   
L125.GFED_ALL <- aggregate( L125.GFED_ALL[ c( "ForestFire", "Deforest")], by=as.list( L125.GFED_ALL[ c( "GCAM_region_ID", "Non.CO2")]), sum )
L125.GFED_ALL$PctForestFire <- L125.GFED_ALL$ForestFire / ( L125.GFED_ALL$ForestFire + L125.GFED_ALL$Deforest )
L125.GFED_ALL$PctForestFire[ is.na( L125.GFED_ALL$PctForestFire ) ] <- 1
L125.GFED_ALL$PctForestFire[ L125.GFED_ALL$PctForestFire == "Inf" ] <- 1

printlog( "Compute RCP emissions by region" )
RCP_BC_2000$Non.CO2 <- "BC"
RCP_OC_2000$Non.CO2 <- "OC"
L125.RCP <- rbind( RCP_BC_2000, RCP_OC_2000 )
L125.RCP$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L125.RCP$iso, iso_GCAM_regID$iso )]   

#Drop unnecessary columns, aggregate by region, and melt
L125.RCP <- L125.RCP[ names( L125.RCP ) %in% c( "GCAM_region_ID", "Non.CO2", "lcf", "sav" ) ]

#Convert to Tg
L125.RCP$lcf <- L125.RCP$lcf * kg_to_tg 
L125.RCP$sav <- L125.RCP$sav * kg_to_tg 

#Drop NAs and aggregate by region
L125.RCP <- na.omit( L125.RCP )
L125.RCP <- aggregate( L125.RCP[ c( "lcf", "sav")], by=as.list( L125.RCP[ c( "GCAM_region_ID", "Non.CO2" ) ] ), sum)

printlog( "Compute grassland emissions factors by GCAM region and AEZ" )
L125.bcoc_tgbkm2_R_grass_2000.melt <- L125.grass_bm2_R_Y
L125.bcoc_tgbkm2_R_grass_2000.melt <- subset( L125.bcoc_tgbkm2_R_grass_2000.melt, L125.bcoc_tgbkm2_R_grass_2000.melt$variable == "X2000" )
L125.bcoc_tgbkm2_R_grass_2000.melt <- repeat_and_add_vector( L125.bcoc_tgbkm2_R_grass_2000.melt, "Non.CO2", c( "BC", "OC" ) )
L125.bcoc_tgbkm2_R_grass_2000.melt$total_emiss <- L125.RCP$sav[ match( vecpaste( L125.bcoc_tgbkm2_R_grass_2000.melt[ c( "GCAM_region_ID", "Non.CO2" )]), vecpaste( L125.RCP[ c( "GCAM_region_ID", "Non.CO2" )] ) )]
L125.bcoc_tgbkm2_R_grass_2000.melt <- na.omit( L125.bcoc_tgbkm2_R_grass_2000.melt) 
L125.bcoc_tgbkm2_R_grass_2000.melt$em_factor <- L125.bcoc_tgbkm2_R_grass_2000.melt$total_emiss / L125.bcoc_tgbkm2_R_grass_2000.melt$value
L125.bcoc_tgbkm2_R_grass_2000.melt$em_factor[ L125.bcoc_tgbkm2_R_grass_2000.melt$em_factor == "Inf" ] <- 0
L125.bcoc_tgbkm2_R_grass_2000.melt$em_factor[ is.na( L125.bcoc_tgbkm2_R_grass_2000.melt$em_factor ) ] <- 0

#Reshape
L125.bcoc_tgbkm2_R_grass_2000 <- dcast( L125.bcoc_tgbkm2_R_grass_2000.melt, GCAM_region_ID + Non.CO2 ~ variable, value.var = c( "em_factor" ) )

printlog( "Compute forest fire emissions factors by GCAM region and AEZ" )
L125.bcoc_tgbkm2_R_forest_2000.melt <- L125.forest_bm2_R_Y
L125.bcoc_tgbkm2_R_forest_2000.melt <- subset( L125.bcoc_tgbkm2_R_forest_2000.melt, L125.bcoc_tgbkm2_R_forest_2000.melt$variable == "X2010" )
names( L125.bcoc_tgbkm2_R_forest_2000.melt )[ names( L125.bcoc_tgbkm2_R_forest_2000.melt) == "value" ] <- "land2000"
L125.bcoc_tgbkm2_R_forest_2000.melt <- L125.bcoc_tgbkm2_R_forest_2000.melt[ names( L125.bcoc_tgbkm2_R_forest_2000.melt ) != "variable" ]

# Deforestation emissions are driven by change in land area, so we'll need to compute that
L125.bcoc_tgbkm2_R_forest_2000.melt$land1999 <- L125.forest_bm2_R_Y$value[ match( paste( L125.bcoc_tgbkm2_R_forest_2000.melt$GCAM_region_ID, "X2009"),
                                                                                  paste( L125.forest_bm2_R_Y$GCAM_region_ID, L125.forest_bm2_R_Y$variable ))]
L125.bcoc_tgbkm2_R_forest_2000.melt$landdiff <- L125.bcoc_tgbkm2_R_forest_2000.melt$land1999 - L125.bcoc_tgbkm2_R_forest_2000.melt$land2000

# Calculate emissions
L125.bcoc_tgbkm2_R_forest_2000.melt <- repeat_and_add_vector( L125.bcoc_tgbkm2_R_forest_2000.melt, "Non.CO2", c( "BC", "OC" ) )
L125.bcoc_tgbkm2_R_forest_2000.melt$total_emiss <- L125.RCP$lcf[ match( vecpaste( L125.bcoc_tgbkm2_R_forest_2000.melt[ c( "GCAM_region_ID", "Non.CO2" )]), vecpaste( L125.RCP[ c( "GCAM_region_ID", "Non.CO2" )] ) )]
L125.bcoc_tgbkm2_R_forest_2000.melt$PctForestFire <- L125.GFED_ALL$PctForestFire[ match( vecpaste(L125.bcoc_tgbkm2_R_forest_2000.melt[ c( "GCAM_region_ID", "Non.CO2" )] ),
                                                                                         vecpaste( L125.GFED_ALL[ c( "GCAM_region_ID", "Non.CO2" )]) )]
L125.bcoc_tgbkm2_R_forest_2000.melt$PctForestFire[ is.na( L125.bcoc_tgbkm2_R_forest_2000.melt$PctForestFire ) ] <- 1 # Always overwrite missing data to ForestFire -- this has less problems
L125.bcoc_tgbkm2_R_forest_2000.melt <- na.omit( L125.bcoc_tgbkm2_R_forest_2000.melt) 
L125.bcoc_tgbkm2_R_forest_2000.melt$ForestFireEmiss <- L125.bcoc_tgbkm2_R_forest_2000.melt$total_emiss * L125.bcoc_tgbkm2_R_forest_2000.melt$PctForestFire
L125.bcoc_tgbkm2_R_forest_2000.melt$DeforestEmiss <- L125.bcoc_tgbkm2_R_forest_2000.melt$total_emiss - L125.bcoc_tgbkm2_R_forest_2000.melt$ForestFireEmiss

# Calculate emissions factors
L125.bcoc_tgbkm2_R_forest_2000.melt$ForestFire <- L125.bcoc_tgbkm2_R_forest_2000.melt$ForestFireEmiss / L125.bcoc_tgbkm2_R_forest_2000.melt$land2000
L125.bcoc_tgbkm2_R_forest_2000.melt$Deforest <- L125.bcoc_tgbkm2_R_forest_2000.melt$DeforestEmiss / L125.bcoc_tgbkm2_R_forest_2000.melt$landdiff

# We shouldn't have negative emissions from deforestation
L125.bcoc_tgbkm2_R_forest_2000.melt$Deforest[ L125.bcoc_tgbkm2_R_forest_2000.melt$Deforest < 0 ] <- 0

# Remove extra columns, reshape, remove strange values
L125.bcoc_tgbkm2_R_forest_2000.melt <- L125.bcoc_tgbkm2_R_forest_2000.melt[ names( L125.bcoc_tgbkm2_R_forest_2000.melt ) %in% c( "GCAM_region_ID", "Non.CO2", "ForestFire", "Deforest" )]
L125.bcoc_tgbkm2_R_forest_2000.melt <- melt( L125.bcoc_tgbkm2_R_forest_2000.melt, id.vars=c( "GCAM_region_ID", "Non.CO2" ))
names( L125.bcoc_tgbkm2_R_forest_2000.melt )[ names(L125.bcoc_tgbkm2_R_forest_2000.melt ) == "value" ] <- "em_factor"
names( L125.bcoc_tgbkm2_R_forest_2000.melt )[ names(L125.bcoc_tgbkm2_R_forest_2000.melt ) == "variable" ] <- "technology"
L125.bcoc_tgbkm2_R_forest_2000.melt$em_factor[ L125.bcoc_tgbkm2_R_forest_2000.melt$em_factor == "Inf" ] <- 0
L125.bcoc_tgbkm2_R_forest_2000.melt$em_factor[ is.na( L125.bcoc_tgbkm2_R_forest_2000.melt$em_factor ) ] <- 0
L125.bcoc_tgbkm2_R_forest_2000 <- L125.bcoc_tgbkm2_R_forest_2000.melt

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L125.bcoc_tgbkm2_R_grass_2000 <- c( "BC/OC grassland burning emissions factors by GCAM region / 2000", "Unit = Tg / Mt" )
comments.L125.bcoc_tgbkm2_R_forest_2000 <- c( "BC/OC forest fire emissions factors by GCAM region / 2000", "Unit = Tg / Mt" )

#write tables as CSV files
writedata( L125.bcoc_tgbkm2_R_grass_2000, domain="EMISSIONS_LEVEL1_DATA", fn="L125.bcoc_tgbkm2_R_grass_2000", comments=comments.L125.bcoc_tgbkm2_R_grass_2000 )
writedata( L125.bcoc_tgbkm2_R_forest_2000, domain="EMISSIONS_LEVEL1_DATA", fn="L125.bcoc_tgbkm2_R_forest_2000", comments=comments.L125.bcoc_tgbkm2_R_forest_2000 )

# Every script should finish with this line
logstop()
