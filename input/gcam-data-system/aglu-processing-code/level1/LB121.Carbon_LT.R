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
logstart( "LB121.Carbon_LT.R" )
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
Various_CarbonData_LTsage <- readdata( "AGLU_LEVEL0_DATA", "Various_CarbonData_LTsage" )
L120.LC_bm2_R_LT_Yh_GLU <- readdata( "AGLU_LEVEL1_DATA", "L120.LC_bm2_R_LT_Yh_GLU" )
L120.LC_bm2_ctry_LTsage_GLU <- readdata( "AGLU_LEVEL1_DATA", "L120.LC_bm2_ctry_LTsage_GLU" )
L120.LC_bm2_ctry_LTpast_GLU <- readdata( "AGLU_LEVEL1_DATA", "L120.LC_bm2_ctry_LTpast_GLU" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#First, convert characteristics by land type to correct units.
L121.Various_CarbonData_LTsage <- Various_CarbonData_LTsage[ names( Various_CarbonData_LTsage ) != "Source" ]
L121.Various_CarbonData_LTsage$value[ L121.Various_CarbonData_LTsage$unit == "tC/ha" ] <- 
  L121.Various_CarbonData_LTsage$value[ L121.Various_CarbonData_LTsage$unit == "tC/ha" ] * conv_tha_kgm2
L121.Various_CarbonData_LTsage$unit[ L121.Various_CarbonData_LTsage$unit == "tC/ha" ] <- "kg/m2"
L121.Various_CarbonData_LTsage.cast <- dcast( L121.Various_CarbonData_LTsage,
                                              LT_SAGE ~ variable, value.var = "value" )

# Need to get rid of the spaces in the table for matching
L121.Various_CarbonData_LTsage.cast$LT_SAGE <- gsub( " ", "", L121.Various_CarbonData_LTsage.cast$LT_SAGE )
Carbon_vars <- unique( c( "mature age", "soil_c", "veg_c" ) )

printlog( "Matching in carbon contents and mature age by land cover, by SAGE land type" )
L121.CarbonData_ctry_LTsage_GLU <- L120.LC_bm2_ctry_LTsage_GLU
L121.CarbonData_ctry_LTsage_GLU[ Carbon_vars ] <- L121.Various_CarbonData_LTsage.cast[
  match( L121.CarbonData_ctry_LTsage_GLU$LT_SAGE, L121.Various_CarbonData_LTsage.cast$LT_SAGE ),
  Carbon_vars ]

printlog( "Multiplying carbon characteristics by land area, for calculating weighted average" )
L121.CarbonData_ctry_LTsage_GLU[ Carbon_vars ] <- L121.CarbonData_ctry_LTsage_GLU[ Carbon_vars ] *
  L121.CarbonData_ctry_LTsage_GLU$Area_bm2

printlog( "Aggregating by GCAM region and GCAM land use type" )
L121.CarbonContent_kgm2_R_LTnatveg_GLU <- aggregate( L121.CarbonData_ctry_LTsage_GLU[ c( "Area_bm2", Carbon_vars ) ],
                                             by = L121.CarbonData_ctry_LTsage_GLU[ c( R_LT_GLU ) ], sum )
L121.CarbonContent_kgm2_R_LTnatveg_GLU[ Carbon_vars ] <- L121.CarbonContent_kgm2_R_LTnatveg_GLU[ Carbon_vars ] /
  L121.CarbonContent_kgm2_R_LTnatveg_GLU$Area_bm2
L121.CarbonContent_kgm2_R_LTnatveg_GLU$Area_bm2 <- NULL

printlog( "Urban land and cropland")
L121.CarbonContent_kgm2_R_LTmgd_GLU <- L120.LC_bm2_R_LT_Yh_GLU[
  L120.LC_bm2_R_LT_Yh_GLU[[LT]] %in% c( "Cropland", "UrbanLand" ), R_LT_GLU ]
L121.CarbonContent_kgm2_R_LTmgd_GLU[ Carbon_vars ] <- L121.Various_CarbonData_LTsage.cast[
  match( L121.CarbonContent_kgm2_R_LTmgd_GLU[[LT]], L121.Various_CarbonData_LTsage.cast[["LT_SAGE"]] ),
         Carbon_vars ]

printlog( "Pasture carbon contents" )
Pasture_vars <- c( Carbon_vars, "pasture_yield" )
L121.CarbonData_ctry_LTpast_GLU <- L120.LC_bm2_ctry_LTpast_GLU
L121.CarbonData_ctry_LTpast_GLU[ Pasture_vars ] <- L121.Various_CarbonData_LTsage.cast[
  match( L121.CarbonData_ctry_LTpast_GLU$LT_SAGE, L121.Various_CarbonData_LTsage.cast$LT_SAGE ),
  Pasture_vars ]

# pasture lands may involve clearing; it is unclear to what extent this takes place but we'll use the carbon
# contents of savanna as a maximum possible value for the vegetative carbon content of pasture lands
L121.CarbonData_ctry_LTpast_GLU$veg_c <- pmin( L121.CarbonData_ctry_LTpast_GLU$veg_c,
                                               L121.Various_CarbonData_LTsage.cast$veg_c[
                                                 L121.Various_CarbonData_LTsage.cast$LT_SAGE == "Savanna" ] )

printlog( "Multiplying carbon characteristics by land area, for calculating weighted average" )
L121.CarbonData_ctry_LTpast_GLU[ Pasture_vars ] <- L121.CarbonData_ctry_LTpast_GLU[ Pasture_vars ] *
  L121.CarbonData_ctry_LTpast_GLU$Area_bm2

printlog( "Aggregating by GCAM region and GCAM land use type" )
L121.CarbonContent_kgm2_R_LTpast_GLU <- aggregate( L121.CarbonData_ctry_LTpast_GLU[ c( "Area_bm2", Pasture_vars ) ],
                                                     by = L121.CarbonData_ctry_LTpast_GLU[ c( R_LT_GLU ) ], sum )
L121.CarbonContent_kgm2_R_LTpast_GLU[ Pasture_vars ] <- L121.CarbonContent_kgm2_R_LTpast_GLU[ Pasture_vars ] /
  L121.CarbonContent_kgm2_R_LTpast_GLU$Area_bm2
L121.CarbonContent_kgm2_R_LTpast_GLU$Area_bm2 <- NULL

printlog( "Combining natural vegetation and managed land use tables" )
L121.CarbonContent_kgm2_R_LT_GLU <- rbind( L121.CarbonContent_kgm2_R_LTnatveg_GLU,
                                           L121.CarbonContent_kgm2_R_LTpast_GLU[ names( L121.CarbonContent_kgm2_R_LTnatveg_GLU ) ],
                                           L121.CarbonContent_kgm2_R_LTmgd_GLU )
L121.CarbonContent_kgm2_R_LT_GLU <- L121.CarbonContent_kgm2_R_LT_GLU[
  order( L121.CarbonContent_kgm2_R_LT_GLU[[R]], L121.CarbonContent_kgm2_R_LT_GLU[[GLU]], L121.CarbonContent_kgm2_R_LT_GLU[[LT]] ), ]

printlog( "Writing out pasture yields separately" )
L121.Yield_kgm2_R_Past_GLU <- L121.CarbonContent_kgm2_R_LTpast_GLU[ c( R_LT_GLU, "pasture_yield" ) ]
L121.Yield_kgm2_R_Past_GLU$pasture_yield <- L121.Yield_kgm2_R_Past_GLU$pasture_yield / Ccontent_cellulose

# -----------------------------------------------------------------------------
# 3. Output
#Add comments to tables
comments.L121.CarbonContent_kgm2_R_LT_GLU <- c( "Vegetation carbon density by region and land type","Unit = kgC / m2" )
comments.L121.Yield_kgm2_R_Past_GLU <- c( "Pasture yield by region / GLU","Unit = kg / m2" )

writedata( L121.CarbonContent_kgm2_R_LT_GLU, domain="AGLU_LEVEL1_DATA", fn="L121.CarbonContent_kgm2_R_LT_GLU", comments=comments.L121.CarbonContent_kgm2_R_LT_GLU )
writedata( L121.Yield_kgm2_R_Past_GLU, domain="AGLU_LEVEL1_DATA", fn="L121.Yield_kgm2_R_Past_GLU", comments=comments.L121.Yield_kgm2_R_Past_GLU )

# Every script should finish with this line
logstop()
