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
logstart( "LA114.wind.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Wind resource supply curves" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
GIS_ctry_AEZ <- readdata( "AGLU_MAPPINGS", "GIS_ctry_AEZ" )
Sage_Hyde15_Area <- readdata( "AGLU_GIS_DATA", "Sage_Hyde15_Area" )
A14.wind_curves <- readdata( "ENERGY_ASSUMPTIONS", "A14.wind_curves" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. Wind resource supply curves
printlog( "Downscaling GCAM 3.0 wind supply curves to countries on the basis of land area" )
#Calculate land cover shares of GCAM regions within region_GCAM3
L114.LC_km2_ctry_LT_AEZ <- subset( Sage_Hyde15_Area, Year == max( Year ) )
L114.LC_km2_ctry_LT_AEZ$iso <- GIS_ctry_AEZ$iso[ match( L114.LC_km2_ctry_LT_AEZ$AEZ_ID, GIS_ctry_AEZ$AEZ_ID ) ]
L114.LC_km2_ctry <- aggregate( L114.LC_km2_ctry_LT_AEZ[ "Area.km2." ], by=as.list( L114.LC_km2_ctry_LT_AEZ[ "iso" ] ), sum )

##Adding in Taiwan
L114.LC_km2_ctry <- rbind( L114.LC_km2_ctry, data.frame( iso = "twn", Area.km2. = 36000) )

#Match in the GCAM 3.0 region and aggregate to compute shares of countries within GCAM 3.0 region
L114.LC_km2_ctry$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( L114.LC_km2_ctry$iso, iso_GCAM_regID$iso ) ]
L114.LC_km2_RG3 <- aggregate( L114.LC_km2_ctry[ "Area.km2." ], by=as.list( L114.LC_km2_ctry[ "region_GCAM3" ] ), sum )
L114.LC_km2_ctry$area_RG3 <- L114.LC_km2_RG3$Area.km2.[ match( L114.LC_km2_ctry$region_GCAM3, L114.LC_km2_RG3$region_GCAM3 ) ]
L114.LC_km2_ctry$share <- L114.LC_km2_ctry$Area.km2. / L114.LC_km2_ctry$area_RG3
L114.LC_km2_ctry$maxSubResource_RG3 <- A14.wind_curves$maxSubResource[ match( L114.LC_km2_ctry$region_GCAM3, A14.wind_curves$region_GCAM3 ) ]
L114.LC_km2_ctry$maxSubResource <- L114.LC_km2_ctry$share * L114.LC_km2_ctry$maxSubResource_RG3

printlog( "Aggregating country-level supplies by GCAM regions")
L114.LC_km2_ctry[[R]]<- iso_GCAM_regID[[R]][ match( L114.LC_km2_ctry$iso, iso_GCAM_regID$iso ) ]
L114.LC_km2_R <- aggregate( L114.LC_km2_ctry[ "maxSubResource" ], by=as.list( L114.LC_km2_ctry[ R ] ), sum )

L114.RsrcCurves_EJ_R_wind <- data.frame(
      GCAM_region_ID = sort( unique( iso_GCAM_regID[[R]] ) ),
      resource = unique( A14.wind_curves$resource ),
      subresource = unique( A14.wind_curves$subresource) )
L114.RsrcCurves_EJ_R_wind$maxSubResource <- L114.LC_km2_R$maxSubResource[ match( L114.RsrcCurves_EJ_R_wind[[R]], L114.LC_km2_R[[R]]) ]
L114.RsrcCurves_EJ_R_wind[ c( "mid.price", "curve.exponent" ) ] <- A14.wind_curves[
      rep( 1, times = nrow( L114.RsrcCurves_EJ_R_wind ) ),
      c( "mid.price", "curve.exponent" ) ]

# 2b. Historical biomass prices (determined at global level, so no level 1 processing necessary)

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L114.RsrcCurves_EJ_R_wind <- c( "Wind resource curves by GCAM region","Unit = EJ" )

#write tables as CSV files
writedata( L114.RsrcCurves_EJ_R_wind, domain="ENERGY_LEVEL1_DATA", fn="L114.RsrcCurves_EJ_R_wind", comments=comments.L114.RsrcCurves_EJ_R_wind )

# Every script should finish with this line
logstop()
