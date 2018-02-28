# L116.geo.R

# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "ENERGYPROC_DIR" ) ){
    if( Sys.getenv( "ENERGYPROC" ) != "" ){
        ENERGYPROC_DIR <- Sys.getenv( "ENERGYPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var ENERGYPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
logstart( "LA116.geo.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Geothermal resource supply curves" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
Land_type_area_ha <- readdata( "AGLU_LDS_DATA", "Land_type_area_ha" )
A16.geo_curves <- readdata( "ENERGY_ASSUMPTIONS", "A16.geo_curves" )
A16.EGS_curves <- readdata( "ENERGY_ASSUMPTIONS", "A16.EGS_curves" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. Geothermal resource supply curves
printlog( "Downscaling GCAM 3.0 geothermal supply curves to countries on the basis of land area" )
#Calculate land cover shares of GCAM regions within region_GCAM3
L116.LC_bm2_ctry_LT_GLU <- subset( Land_type_area_ha, year == max( year ) )
L116.LC_bm2_ctry <- aggregate( L116.LC_bm2_ctry_LT_GLU[ "value" ] * conv_Ha_bm2,
      by = L116.LC_bm2_ctry_LT_GLU[ "iso" ], sum )

#Match in the GCAM 3.0 region and aggregate to compute shares of countries within GCAM 3.0 region
L116.LC_bm2_ctry$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( L116.LC_bm2_ctry$iso, iso_GCAM_regID$iso ) ]
L116.LC_bm2_RG3 <- aggregate( L116.LC_bm2_ctry[ "value" ], by = L116.LC_bm2_ctry[ "region_GCAM3" ], sum )
L116.LC_bm2_ctry$area_RG3 <- L116.LC_bm2_RG3$value[ match( L116.LC_bm2_ctry$region_GCAM3, L116.LC_bm2_RG3$region_GCAM3 ) ]
L116.LC_bm2_ctry$share <- L116.LC_bm2_ctry$value / L116.LC_bm2_ctry$area_RG3

#Repeat by number of grades, and match in the available quantities
#Hydrothermal
L116.Available_EJ_ctry_geo <- repeat_and_add_vector( L116.LC_bm2_ctry, "grade", unique( A16.geo_curves$grade ) )
L116.Available_EJ_ctry_geo$available_RG3 <- A16.geo_curves$available[
      match( vecpaste( L116.Available_EJ_ctry_geo[ c( "region_GCAM3", "grade" ) ] ),
             vecpaste( A16.geo_curves[ c( "region_GCAM3", "grade" ) ] ) ) ]
L116.Available_EJ_ctry_geo$available <- L116.Available_EJ_ctry_geo$share * L116.Available_EJ_ctry_geo$available_RG3

#EGS
L116.Available_EJ_ctry_EGS <- repeat_and_add_vector( L116.LC_bm2_ctry, "grade", unique( A16.EGS_curves$grade ) )
L116.Available_EJ_ctry_EGS$available_RG3 <- A16.EGS_curves$available[
      match( vecpaste( L116.Available_EJ_ctry_EGS[ c( "region_GCAM3", "grade" ) ] ),
             vecpaste( A16.EGS_curves[ c( "region_GCAM3", "grade" ) ] ) ) ]
L116.Available_EJ_ctry_EGS$available <- L116.Available_EJ_ctry_EGS$share * L116.Available_EJ_ctry_EGS$available_RG3

printlog( "Aggregating country-level supplies by GCAM regions")
#Hydrothermal
L116.Available_EJ_ctry_geo[[R]]<- iso_GCAM_regID[[R]][ match( L116.Available_EJ_ctry_geo$iso, iso_GCAM_regID$iso ) ]
L116.Available_EJ_R_geo <- aggregate( L116.Available_EJ_ctry_geo[ "available" ],
                                      by = L116.Available_EJ_ctry_geo[ c( R, "grade" ) ], sum )

#EGS
L116.Available_EJ_ctry_EGS[[R]]<- iso_GCAM_regID[[R]][ match( L116.Available_EJ_ctry_EGS$iso, iso_GCAM_regID$iso ) ]
L116.Available_EJ_R_EGS <- aggregate( L116.Available_EJ_ctry_EGS[ "available" ],
                                      by = L116.Available_EJ_ctry_EGS[ c( R, "grade" ) ], sum )

printlog( "Building hydrothermal supply curves")
L116.RsrcCurves_EJ_R_geo <- data.frame(
      GCAM_region_ID = sort( unique( iso_GCAM_regID[[R]] ) ),
      resource = unique( A16.geo_curves$resource ),
      subresource = unique( A16.geo_curves$subresource) )
L116.RsrcCurves_EJ_R_geo <- repeat_and_add_vector( L116.RsrcCurves_EJ_R_geo, "grade", unique(  A16.geo_curves$grade ) )
L116.RsrcCurves_EJ_R_geo$available <- L116.Available_EJ_R_geo$available[
      match( vecpaste( L116.RsrcCurves_EJ_R_geo[ c( R, "grade" ) ] ),
             vecpaste( L116.Available_EJ_R_geo[ c( R, "grade" ) ] ) ) ]
#NOTE: currently assuming that all regions have the same price points
L116.RsrcCurves_EJ_R_geo$extractioncost <- A16.geo_curves$extractioncost[
      match( L116.RsrcCurves_EJ_R_geo$grade, A16.geo_curves$grade ) ]

printlog( "Building EGS supply curves")
L116.RsrcCurves_EJ_R_EGS <- data.frame(
      GCAM_region_ID = sort( unique( iso_GCAM_regID[[R]] ) ),
      resource = unique( A16.EGS_curves$resource ),
      subresource = unique( A16.EGS_curves$subresource) )
L116.RsrcCurves_EJ_R_EGS <- repeat_and_add_vector( L116.RsrcCurves_EJ_R_EGS, "grade", unique(  A16.EGS_curves$grade ) )
L116.RsrcCurves_EJ_R_EGS$available <- L116.Available_EJ_R_EGS$available[
      match( vecpaste( L116.RsrcCurves_EJ_R_EGS[ c( R, "grade" ) ] ),
             vecpaste( L116.Available_EJ_R_EGS[ c( R, "grade" ) ] ) ) ]
#NOTE: currently assuming that all regions have the same price points
L116.RsrcCurves_EJ_R_EGS$extractioncost <- A16.EGS_curves$extractioncost[
      match( L116.RsrcCurves_EJ_R_EGS$grade, A16.EGS_curves$grade ) ]

# 2b. Historical biomass prices (determined at global level, so no level 1 processing necessary)

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L116.RsrcCurves_EJ_R_geo <- c( "Geothermal (hydrothermal) resources by GCAM region","Unit = EJ" )
comments.L116.RsrcCurves_EJ_R_EGS <- c( "EGS resources by GCAM region","Unit = EJ" )

#write tables as CSV files
writedata( L116.RsrcCurves_EJ_R_geo, domain="ENERGY_LEVEL1_DATA", fn="L116.RsrcCurves_EJ_R_geo", comments=comments.L116.RsrcCurves_EJ_R_geo )
writedata( L116.RsrcCurves_EJ_R_EGS, domain="ENERGY_LEVEL1_DATA", fn="L116.RsrcCurves_EJ_R_EGS", comments=comments.L116.RsrcCurves_EJ_R_EGS )

# Every script should finish with this line
logstop()
