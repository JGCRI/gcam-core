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
L124.LC_bm2_R_Grass_Yh_GLU_adj <- readdata( "AGLU_LEVEL1_DATA", "L124.LC_bm2_R_Grass_Yh_GLU_adj" )
L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj <- readdata( "AGLU_LEVEL1_DATA", "L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj" )
RCP_BC_2000 <- readdata( "EMISSIONS_LEVEL0_DATA", "RCP_BC_2000" )
RCP_OC_2000 <- readdata( "EMISSIONS_LEVEL0_DATA", "RCP_OC_2000" )

GFED_ForestFire_BC <- readdata( "EMISSIONS_LEVEL0_DATA", "GFED_ForestFire_BC" )
GFED_Deforest_BC <- readdata( "EMISSIONS_LEVEL0_DATA", "GFED_Deforest_BC" )
GFED_ForestFire_OC <- readdata( "EMISSIONS_LEVEL0_DATA", "GFED_ForestFire_OC" )
GFED_Deforest_OC <- readdata( "EMISSIONS_LEVEL0_DATA", "GFED_Deforest_OC" )

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Compute fraction of emissions from deforestation vs forest fires" )
GFED_ForestFire_BC$Non.CO2 <- "BC"
GFED_Deforest_BC$Non.CO2 <- "BC"
GFED_ForestFire_OC$Non.CO2 <- "OC"
GFED_Deforest_OC$Non.CO2 <- "OC"

L125.GFED_ForestFire <- rbind( GFED_ForestFire_BC, GFED_ForestFire_OC )
L125.GFED_Deforest <- rbind( GFED_Deforest_BC, GFED_Deforest_OC )

L125.GFED_ForestFire <- L125.GFED_ForestFire[ c( "Country", "Non.CO2", "X2000" ) ]
names( L125.GFED_ForestFire )[ names( L125.GFED_ForestFire ) == "X2000" ] <- "ForestFire" 
L125.GFED_Deforest <- L125.GFED_Deforest[ c( "Country", "Non.CO2", "X2000" ) ]
names( L125.GFED_Deforest )[ names( L125.GFED_Deforest ) == "X2000" ] <- "Deforest"

L125.GFED_ALL <- L125.GFED_ForestFire
L125.GFED_ALL$Deforest <- L125.GFED_Deforest$Deforest[
  match( vecpaste( L125.GFED_ALL[ c( "Country", "Non.CO2" )]),
         vecpaste( L125.GFED_Deforest[ c( "Country", "Non.CO2" ) ]) )]
L125.GFED_ALL$iso <- EDGAR_nation$iso[ match( L125.GFED_ALL$Country, EDGAR_nation$ISO_A3 )]
L125.GFED_ALL$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L125.GFED_ALL$iso, iso_GCAM_regID$iso )]   
L125.GFED_ALL <- aggregate( L125.GFED_ALL[ c( "ForestFire", "Deforest")],
                            by = L125.GFED_ALL[ c( "GCAM_region_ID", "Non.CO2")], sum )
L125.GFED_ALL$PctForestFire <- L125.GFED_ALL$ForestFire / ( L125.GFED_ALL$ForestFire + L125.GFED_ALL$Deforest )
L125.GFED_ALL$PctForestFire[ is.na( L125.GFED_ALL$PctForestFire ) ] <- 1

printlog( "Compute RCP emissions by region" )
RCP_BC_2000$Non.CO2 <- "BC"
RCP_OC_2000$Non.CO2 <- "OC"
L125.RCP <- rbind( RCP_BC_2000, RCP_OC_2000 )
L125.RCP$GCAM_region_ID <- iso_GCAM_regID$GCAM_region_ID[ match( L125.RCP$iso, iso_GCAM_regID$iso )]   

#Drop unnecessary columns, aggregate by region, and melt
L125.RCP <- L125.RCP[ c( "GCAM_region_ID", "Non.CO2", "lcf", "sav" ) ]

#Convert to Tg
L125.RCP[ c( "lcf", "sav" ) ] <- L125.RCP[ c( "lcf", "sav" ) ] * kg_to_tg 

# aggregate by region
L125.RCP <- aggregate( L125.RCP[ c( "lcf", "sav")],
                       by = L125.RCP[ c( "GCAM_region_ID", "Non.CO2" ) ], sum)

printlog( "Compute grassland emissions factors by GCAM region" )
printlog( "Because there is only one base year, we use constant emissions factors for BC and OC in all historical years" )
# Because grassland and forest fire emissions scale with land quantity, the coefs can be computed at the regional level
# and applied equally to all land use regions (GLUs) thereafter. However deforestation emissions are assigned from the region
# to the GLUs according to relative shares of deforested land, different from forest land cover.
L125.bcoc_tgbkm2_R_grass_2000 <- aggregate( L124.LC_bm2_R_Grass_Yh_GLU_adj[ "X2000" ],
                                            by = L124.LC_bm2_R_Grass_Yh_GLU_adj[ R_LT ], sum )
L125.bcoc_tgbkm2_R_grass_2000 <- repeat_and_add_vector( L125.bcoc_tgbkm2_R_grass_2000,
                                                        "Non.CO2", unique( L125.RCP$Non.CO2 ) )
L125.bcoc_tgbkm2_R_grass_2000$emissions <- L125.RCP$sav[
  match( vecpaste( L125.bcoc_tgbkm2_R_grass_2000[ c( R, "Non.CO2" ) ] ),
         vecpaste( L125.RCP[ c( R, "Non.CO2" ) ] ) ) ]
L125.bcoc_tgbkm2_R_grass_2000$em_factor <- with( L125.bcoc_tgbkm2_R_grass_2000, emissions / X2000 )

# Subset only relevant columns
L125.bcoc_tgbkm2_R_grass_2000 <- L125.bcoc_tgbkm2_R_grass_2000[c( R_LT, "Non.CO2", "em_factor" ) ]

printlog( "Compute forest fire emissions factors by GCAM region" )
L125.bcoc_tgbkm2_R_forestfire_2000 <- aggregate( L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj[ "X2000" ],
                                                 by = L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj[ R_LT ],
                                                 sum )
names( L125.bcoc_tgbkm2_R_forestfire_2000 )[ names( L125.bcoc_tgbkm2_R_forestfire_2000 ) == "X2000" ] <- "FF_driver"

L125.bcoc_tgbkm2_R_GLU_defor_2000 <- L124.LC_bm2_R_UnMgdFor_Yh_GLU_adj[ c( R_LT_GLU, X_Deforest_coef_years ) ]
L125.bcoc_tgbkm2_R_GLU_defor_2000$D_driver <- pmax(
  L125.bcoc_tgbkm2_R_GLU_defor_2000[[X_Deforest_coef_years[1] ]] - L125.bcoc_tgbkm2_R_GLU_defor_2000[[ X_Deforest_coef_years[2] ]],
  0 ) / ( Deforest_coef_years[2] - Deforest_coef_years[1] )
L125.bcoc_tgbkm2_R_defor_2000 <- aggregate( L125.bcoc_tgbkm2_R_GLU_defor_2000[ "D_driver" ],
                                            by = L125.bcoc_tgbkm2_R_GLU_defor_2000[ R_LT ],
                                            sum )
L125.bcoc_tgbkm2_R_forest_2000 <- merge( L125.bcoc_tgbkm2_R_forestfire_2000, L125.bcoc_tgbkm2_R_defor_2000 )

# Match in emissions and multiply by forest fire fraction
L125.bcoc_tgbkm2_R_forest_2000 <- repeat_and_add_vector( L125.bcoc_tgbkm2_R_forest_2000,
                                                             "Non.CO2", unique( L125.RCP$Non.CO2 ) )
L125.bcoc_tgbkm2_R_forest_2000$total_emiss <- L125.RCP$lcf[
  match( vecpaste( L125.bcoc_tgbkm2_R_forest_2000[ c( "GCAM_region_ID", "Non.CO2" )]),
         vecpaste( L125.RCP[ c( "GCAM_region_ID", "Non.CO2" )] ) )]
L125.bcoc_tgbkm2_R_forest_2000$PctForestFire <- L125.GFED_ALL$PctForestFire[
  match( vecpaste(L125.bcoc_tgbkm2_R_forest_2000[ c( "GCAM_region_ID", "Non.CO2" )] ),
         vecpaste( L125.GFED_ALL[ c( "GCAM_region_ID", "Non.CO2" )]) )]
L125.bcoc_tgbkm2_R_forest_2000$ForestFireEmiss <- with( L125.bcoc_tgbkm2_R_forest_2000, total_emiss * PctForestFire )
L125.bcoc_tgbkm2_R_forest_2000$DeforestEmiss <- with( L125.bcoc_tgbkm2_R_forest_2000, total_emiss - ForestFireEmiss )

printlog( "Calculating average default deforestation emissions factors, to be used in future periods" )
L125.deforest_coefs_bcoc <- aggregate( L125.bcoc_tgbkm2_R_forest_2000[ c( "D_driver", "DeforestEmiss" ) ],
                                       by = L125.bcoc_tgbkm2_R_forest_2000[ "Non.CO2" ],
                                       sum )
L125.deforest_coefs_bcoc$emiss.coef <- with( L125.deforest_coefs_bcoc, DeforestEmiss / D_driver )

# Calculate emissions factors
L125.bcoc_tgbkm2_R_forest_2000$ForestFire <- with( L125.bcoc_tgbkm2_R_forest_2000, ForestFireEmiss / FF_driver )
L125.bcoc_tgbkm2_R_forest_2000$Deforest <- with( L125.bcoc_tgbkm2_R_forest_2000, DeforestEmiss / D_driver )

# Remove extra columns, reshape, remove strange values
L125.bcoc_tgbkm2_R_forest_2000 <- melt( L125.bcoc_tgbkm2_R_forest_2000,
                                        measure.vars = c( "ForestFire", "Deforest" ),
                                        id.vars = c( R_LT, "Non.CO2" ),
                                        variable.name = "technology", value.name = "em_factor" )

L125.bcoc_tgbkm2_R_forest_2000$em_factor[ L125.bcoc_tgbkm2_R_forest_2000$em_factor == "Inf" ] <- 0
L125.bcoc_tgbkm2_R_forest_2000$em_factor[ is.na( L125.bcoc_tgbkm2_R_forest_2000$em_factor ) ] <- 0

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L125.bcoc_tgbkm2_R_grass_2000 <- c( "BC/OC grassland burning emissions factors by GCAM region / 2000", "Unit = Tg / bm2" )
comments.L125.bcoc_tgbkm2_R_forest_2000 <- c( "BC/OC forest fires and deforestation emissions factors by GCAM region / 2000", "Unit = Tg / bm2" )
comments.L125.deforest_coefs_bcoc <- c( "Default deforestation coefficients by BC and OC", "Unit = kg/m2/yr" )

#write tables as CSV files
writedata( L125.bcoc_tgbkm2_R_grass_2000, domain="EMISSIONS_LEVEL1_DATA", fn="L125.bcoc_tgbkm2_R_grass_2000", comments=comments.L125.bcoc_tgbkm2_R_grass_2000 )
writedata( L125.bcoc_tgbkm2_R_forest_2000, domain="EMISSIONS_LEVEL1_DATA", fn="L125.bcoc_tgbkm2_R_forest_2000", comments=comments.L125.bcoc_tgbkm2_R_forest_2000 )
writedata( L125.deforest_coefs_bcoc, domain="EMISSIONS_LEVEL1_DATA", fn="L125.deforest_coefs_bcoc", comments=comments.L125.deforest_coefs_bcoc )

# Every script should finish with this line
logstop()
