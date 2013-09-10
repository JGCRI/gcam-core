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
logstart( "LB124.LC_R_UnMgd_Yh_AEZ.R" )
adddep(paste(AGLUPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(AGLUPROC_DIR,"/../_common/headers/AGLU_header.R",sep=""))
printlog( "Grassland, shrubland, unmanaged pasture, and unmanaged forest by region / historical year / AEZ" )

# -----------------------------------------------------------------------------
# 1. Read data

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "AGLU_ASSUMPTIONS", "A_aglu_data", extension = ".R" )
L120.LC_bm2_R_LT_Yh_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L120.LC_bm2_R_LT_Yh_AEZ" )
L122.LC_bm2_R_ExtraCropLand_Yh_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L122.LC_bm2_R_ExtraCropLand_Yh_AEZ" )
L123.LC_bm2_R_MgdPast_Yh_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L123.LC_bm2_R_MgdPast_Yh_AEZ" )
L123.LC_bm2_R_MgdFor_Yh_AEZ <- readdata( "AGLU_LEVEL1_DATA", "L123.LC_bm2_R_MgdFor_Yh_AEZ" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Calculate initial estimates of shrubland, unmanaged pasture, and unmanaged forest
#shrubland and grassland are taken directly from SAGE minus HYDE and WDPA
printlog( "Subsetting unadjusted land cover of grassland and shrubland" )
L124.LC_bm2_R_Shrub_Yh_AEZ <- L120.LC_bm2_R_LT_Yh_AEZ[ L120.LC_bm2_R_LT_Yh_AEZ[[LT]] == "Shrubland", ]
L124.LC_bm2_R_Grass_Yh_AEZ <- L120.LC_bm2_R_LT_Yh_AEZ[ L120.LC_bm2_R_LT_Yh_AEZ[[LT]] == "Grassland", ]

#unmanaged pasture is equal to total pasture from Hyde minus managed pasture
printlog( "Calculating unmanaged pasture and forest as total minus managed" )
L124.LC_bm2_R_UnMgdPast_Yh_AEZ <- L120.LC_bm2_R_LT_Yh_AEZ[ L120.LC_bm2_R_LT_Yh_AEZ[[LT]] == "Pasture", ]
L124.LC_bm2_R_UnMgdPast_Yh_AEZ[ X_land_cover_years ] <- L124.LC_bm2_R_UnMgdPast_Yh_AEZ[ X_land_cover_years ] - L123.LC_bm2_R_MgdPast_Yh_AEZ[ X_land_cover_years ]
L124.LC_bm2_R_UnMgdPast_Yh_AEZ[[LT]]<- "UnmanagedPasture"

#unmanaged forest is equal to total forest from Sage/Hyde minus managed forest
L124.LC_bm2_R_UnMgdFor_Yh_AEZ <- L120.LC_bm2_R_LT_Yh_AEZ[ L120.LC_bm2_R_LT_Yh_AEZ[[LT]] == "Forest", ]
L124.LC_bm2_R_UnMgdFor_Yh_AEZ[ X_land_cover_years ] <- L124.LC_bm2_R_UnMgdFor_Yh_AEZ[ X_land_cover_years ] - L123.LC_bm2_R_MgdFor_Yh_AEZ[ X_land_cover_years ]
L124.LC_bm2_R_UnMgdFor_Yh_AEZ[[LT]]<- "UnmanagedForest"

#The above land categories all have land deducted to cover the "extra" cropland that came from setting a maximum harvested:cropped ratio.
#This deduction takes place according to the relative shares of land cover.
#Calculate these shares
printlog( "NOTE: Expansion in cropland is balanced by deduction from these unmanaged land types" )
printlog( "Calculating adjusted land cover of grassland, shrubland, unmanaged pasture, and unmanaged forest" )
L124.LC_bm2_R_LTunmgd_Yh_AEZ <- rbind(
      L124.LC_bm2_R_Shrub_Yh_AEZ, L124.LC_bm2_R_Grass_Yh_AEZ, L124.LC_bm2_R_UnMgdPast_Yh_AEZ, L124.LC_bm2_R_UnMgdFor_Yh_AEZ )
L124.LC_bm2_R_UnMgd_Yh_AEZ <- L124.LC_bm2_R_LTunmgd_Yh_AEZ
L124.LC_bm2_R_UnMgd_Yh_AEZ[[LT]]<- "All_Unmanaged"
L124.LC_bm2_R_UnMgd_Yh_AEZ <- aggregate( L124.LC_bm2_R_UnMgd_Yh_AEZ[ X_land_cover_years ], by=as.list( L124.LC_bm2_R_UnMgd_Yh_AEZ[ R_LT_AEZ ] ), sum )

#Calculate adjusted total unmanaged land as total unmanaged minus "extra" cropland
L124.LC_UnMgdAdj_R_Yh_AEZ <- L124.LC_bm2_R_UnMgd_Yh_AEZ
L124.LC_UnMgdAdj_R_Yh_AEZ[ X_land_cover_years ] <-
      ( L124.LC_bm2_R_UnMgd_Yh_AEZ[ X_land_cover_years ] - L122.LC_bm2_R_ExtraCropLand_Yh_AEZ[
        match( vecpaste( L124.LC_bm2_R_UnMgd_Yh_AEZ[ R_AEZ ] ), vecpaste( L122.LC_bm2_R_ExtraCropLand_Yh_AEZ[ R_AEZ] ) ),
        X_land_cover_years ] ) / 
      L124.LC_bm2_R_UnMgd_Yh_AEZ[ X_land_cover_years ]
L124.LC_UnMgdAdj_R_Yh_AEZ[ is.na( L124.LC_UnMgdAdj_R_Yh_AEZ ) ] <- 1
     
#Check to make sure that enough land is available for the cropland expansion in all regions/AEZs
if( any( L124.LC_UnMgdAdj_R_Yh_AEZ[ X_land_cover_years ] < 0 ) ) {
	stop( "Increase in cropland exceeds available unmanaged land")
}

#Apply the adjustment factor to the different unmanaged land types, and split into different tables
L124.LC_bm2_R_LTunmgd_Yh_AEZ_adj <- L124.LC_bm2_R_LTunmgd_Yh_AEZ
L124.LC_bm2_R_LTunmgd_Yh_AEZ_adj[ X_land_cover_years ] <- L124.LC_bm2_R_LTunmgd_Yh_AEZ_adj[ X_land_cover_years ] * L124.LC_UnMgdAdj_R_Yh_AEZ[
      match( vecpaste( L124.LC_bm2_R_LTunmgd_Yh_AEZ_adj[ R_AEZ ] ), vecpaste( L124.LC_UnMgdAdj_R_Yh_AEZ[ R_AEZ ] ) ),
      X_land_cover_years ]

L124.LC_bm2_R_Shrub_Yh_AEZ_adj <- L124.LC_bm2_R_LTunmgd_Yh_AEZ_adj[ L124.LC_bm2_R_LTunmgd_Yh_AEZ_adj[[LT]] == "Shrubland", ]
L124.LC_bm2_R_Grass_Yh_AEZ_adj <- L124.LC_bm2_R_LTunmgd_Yh_AEZ_adj[ L124.LC_bm2_R_LTunmgd_Yh_AEZ_adj[[LT]] == "Grassland", ]
L124.LC_bm2_R_UnMgdPast_Yh_AEZ_adj <- L124.LC_bm2_R_LTunmgd_Yh_AEZ_adj[ L124.LC_bm2_R_LTunmgd_Yh_AEZ_adj[[LT]] == "UnmanagedPasture", ]
L124.LC_bm2_R_UnMgdFor_Yh_AEZ_adj <- L124.LC_bm2_R_LTunmgd_Yh_AEZ_adj[ L124.LC_bm2_R_LTunmgd_Yh_AEZ_adj[[LT]] == "UnmanagedForest", ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L124.LC_bm2_R_Shrub_Yh_AEZ_adj <- c( "Shrub land cover by GCAM region / historical year / AEZ","bm2" )
comments.L124.LC_bm2_R_Grass_Yh_AEZ_adj <- c( "Grass land cover by GCAM region / historical year / AEZ","bm2" )
comments.L124.LC_bm2_R_UnMgdPast_Yh_AEZ_adj <- c( "Unmanaged pasture land cover by GCAM region / historical year / AEZ","bm2" )
comments.L124.LC_bm2_R_UnMgdFor_Yh_AEZ_adj <- c( "Unmanaged forest land cover by GCAM region / historical year / AEZ","bm2" )

writedata( L124.LC_bm2_R_Shrub_Yh_AEZ_adj, domain="AGLU_LEVEL1_DATA", fn="L124.LC_bm2_R_Shrub_Yh_AEZ_adj", comments=comments.L124.LC_bm2_R_Shrub_Yh_AEZ_adj )
writedata( L124.LC_bm2_R_Grass_Yh_AEZ_adj, domain="AGLU_LEVEL1_DATA", fn="L124.LC_bm2_R_Grass_Yh_AEZ_adj", comments=comments.L124.LC_bm2_R_Grass_Yh_AEZ_adj )
writedata( L124.LC_bm2_R_UnMgdPast_Yh_AEZ_adj, domain="AGLU_LEVEL1_DATA", fn="L124.LC_bm2_R_UnMgdPast_Yh_AEZ_adj", comments=comments.L124.LC_bm2_R_UnMgdPast_Yh_AEZ_adj )
writedata( L124.LC_bm2_R_UnMgdFor_Yh_AEZ_adj, domain="AGLU_LEVEL1_DATA", fn="L124.LC_bm2_R_UnMgdFor_Yh_AEZ_adj", comments=comments.L124.LC_bm2_R_UnMgdFor_Yh_AEZ_adj )

# Every script should finish with this line
logstop()
