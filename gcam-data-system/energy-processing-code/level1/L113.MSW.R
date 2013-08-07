# L113.MSW.R

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
logstart( "L113.MSW.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Municipal solid waste supply curves" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
L100.gdp_bilusd_ctry_Yh <- readdata( "SOCIO_LEVEL1_DATA", "L100.gdp_bilusd_ctry_Yh" )
A13.MSW_curves <- readdata( "ENERGY_ASSUMPTIONS", "A13.MSW_curves" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. Municipal solid waste supply curves
printlog( "Downscaling GCAM 3.0 MSW supply curves to countries on the basis of GDP" )
#Calculate GDP shares of GCAM regions within region_GCAM3
L113.GDP_ctry <- L100.gdp_bilusd_ctry_Yh[ c( "iso", X_final_historical_year ) ]
L113.GDP_ctry$region_GCAM3 <- iso_GCAM_regID$region_GCAM3[ match( L113.GDP_ctry$iso, iso_GCAM_regID$iso ) ]
L113.GDP_RG3 <- aggregate( L113.GDP_ctry[ X_final_historical_year ], by=as.list( L113.GDP_ctry[ "region_GCAM3" ] ), sum )
L113.GDP_ctry$share <- L113.GDP_ctry[[X_final_historical_year]] / L113.GDP_RG3[[X_final_historical_year]][
      match( L113.GDP_ctry$region_GCAM3, L113.GDP_RG3$region_GCAM3 ) ]
L113.GDP_ctry$maxSubResource_RG3 <- A13.MSW_curves$maxSubResource[
      match( L113.GDP_ctry$region_GCAM3, A13.MSW_curves$region_GCAM3 ) ]
L113.GDP_ctry$maxSubResource <- L113.GDP_ctry$share * L113.GDP_ctry$maxSubResource_RG3

printlog( "Aggregating country-level MSW resources to GCAM regions")
printlog( "NOTE: this method assumes that all regions have the same curve shape parameters")
L113.GDP_ctry[[R]] <- iso_GCAM_regID[[R]][ match( L113.GDP_ctry$iso, iso_GCAM_regID$iso ) ]
L113.MSW_maxSubResource <- aggregate( L113.GDP_ctry[ "maxSubResource" ], by=as.list( L113.GDP_ctry[ R ] ), sum )

printlog( "Writing the supply curves to all regions")
L113.RsrcCurves_EJ_R_MSW <- data.frame(
      GCAM_region_ID = sort( unique( iso_GCAM_regID[[R]] ) ),
      resource = unique( A13.MSW_curves$resource ),
      subresource = unique( A13.MSW_curves$subresource) )
L113.RsrcCurves_EJ_R_MSW$maxSubResource <- L113.MSW_maxSubResource$maxSubResource[
      match( L113.RsrcCurves_EJ_R_MSW[[R]], L113.MSW_maxSubResource[[R]] ) ]
L113.RsrcCurves_EJ_R_MSW[ c( "mid.price", "curve.exponent" ) ] <- A13.MSW_curves[
      rep( 1, times = nrow( L113.RsrcCurves_EJ_R_MSW ) ),
      c( "mid.price", "curve.exponent" ) ]

# 2b. Historical biomass prices (currently determined at global level, so no level 1 processing necessary)

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L113.RsrcCurves_EJ_R_MSW <- c( "MSW resource curves by GCAM region","Unit = EJ" )

#write tables as CSV files
writedata( L113.RsrcCurves_EJ_R_MSW, domain="ENERGY_LEVEL1_DATA", fn="L113.RsrcCurves_EJ_R_MSW", comments=comments.L113.RsrcCurves_EJ_R_MSW )

# Every script should finish with this line
logstop()
