# L115.roofPV.R

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
logstart( "LA115.roofPV.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Rooftop PV resource supply curves" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
A15.roofPV_curves <- readdata( "ENERGY_ASSUMPTIONS", "A15.roofPV_curves" )
A15.roofPV_TechChange <- readdata( "ENERGY_ASSUMPTIONS", "A15.roofPV_TechChange" )
L100.Pop_thous_ctry_Yh <- readdata( "SOCIO_LEVEL1_DATA", "L100.Pop_thous_ctry_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. Geothermal resource supply curves
printlog( "Downscaling GCAM 3.0 rooftop PV supply curves to countries on the basis of population" )
L115.pop_ctry <- L100.Pop_thous_ctry_Yh[ c( "iso", X_final_historical_year ) ]
L115.pop_ctry$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( L115.pop_ctry$iso, iso_GCAM_regID$iso ) ]
L115.pop_RG3 <- aggregate( L115.pop_ctry[ X_final_historical_year ], by=as.list( L115.pop_ctry[ "region_GCAM3" ] ), sum )
L115.pop_ctry$pop_RG3 <- L115.pop_RG3[[X_final_historical_year]][ match( L115.pop_ctry$region_GCAM3, L115.pop_RG3$region_GCAM3 ) ]
L115.pop_ctry$share <- L115.pop_ctry[[X_final_historical_year]] / L115.pop_ctry$pop_RG3
L115.pop_ctry$maxSubResource_RG3 <- A15.roofPV_curves$maxSubResource[ match( L115.pop_ctry$region_GCAM3, A15.roofPV_curves$region_GCAM3 ) ]
L115.pop_ctry$maxSubResource <- L115.pop_ctry$share * L115.pop_ctry$maxSubResource_RG3

printlog( "Aggregating country-level supplies by GCAM regions")
L115.pop_ctry[[R]]<- iso_GCAM_regID[[R]][ match( L115.pop_ctry$iso, iso_GCAM_regID$iso ) ]
L115.roofPV_maxSubResource_R <- aggregate( L115.pop_ctry[ "maxSubResource" ], by=as.list( L115.pop_ctry[ R ] ), sum )

printlog( "Rooftop PV supply curves have region-specific mid-prices; using the median to translate from countries to regions" )
L115.pop_ctry$mid.price <- A15.roofPV_curves$mid.price[ match( L115.pop_ctry$region_GCAM3, A15.roofPV_curves$region_GCAM3 ) ]
L115.roofPV_midprice_R <- aggregate( L115.pop_ctry[ "mid.price" ], by=as.list( L115.pop_ctry[ R ] ), median )

#Build the supply curves from the available data
L115.RsrcCurves_EJ_R_roofPV <- data.frame(
      GCAM_region_ID = sort( unique( iso_GCAM_regID[[R]] ) ),
      resource = unique( A15.roofPV_curves$resource ),
      subresource = unique( A15.roofPV_curves$subresource) )
L115.RsrcCurves_EJ_R_roofPV$maxSubResource <- L115.roofPV_maxSubResource_R$maxSubResource[ match( L115.RsrcCurves_EJ_R_roofPV[[R]], L115.roofPV_maxSubResource_R[[R]] ) ]
L115.RsrcCurves_EJ_R_roofPV$mid.price <- L115.roofPV_midprice_R$mid.price[ match( L115.RsrcCurves_EJ_R_roofPV[[R]], L115.roofPV_midprice_R[[R]] ) ]
L115.RsrcCurves_EJ_R_roofPV$curve.exponent <- A15.roofPV_curves$curve.exponent[1]     #assuming that all regions have the same curve exponent
L115.RsrcCurves_EJ_R_roofPV$gdpSupplyElast <- A15.roofPV_curves$gdpSupplyElast[1]     #assuming that all regions have the same GDP supply elasticity
L115.RsrcCurves_EJ_R_roofPV$subResourceCapacityFactor <- A15.roofPV_curves$subResourceCapacityFactor[1]     #assuming that all regions have the same capacity factor

# 2b. Prices (assumed, no processing necessary)

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L115.RsrcCurves_EJ_R_roofPV <- c( "Rooftop PV resources by GCAM region","Unit = EJ" )

#write tables as CSV files
writedata( L115.RsrcCurves_EJ_R_roofPV, domain="ENERGY_LEVEL1_DATA", fn="L115.RsrcCurves_EJ_R_roofPV", comments=comments.L115.RsrcCurves_EJ_R_roofPV )

# Every script should finish with this line
logstop()
