# L117.geo.R

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
logstart( "LA117.tradbio.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Traditional biomass resource supply curves" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
A_regions <- readdata( "ENERGY_ASSUMPTIONS", "A_regions" )
iso_GCAM_regID <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID" )
L1011.en_bal_EJ_R_Si_Fi_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1011.en_bal_EJ_R_Si_Fi_Yh" )
A17.tradbio_curves <- readdata( "ENERGY_ASSUMPTIONS", "A17.tradbio_curves" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. Geothermal resource supply curves
printlog( "Calculating the max resource of tradbio as the maximum during the historical years" )
L117.TPES_EJ_R_bld_tradbio_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, fuel == "biomass_tradbio" & sector == "TPES" )
L117.TPES_EJ_R_bld_tradbio_Yh.melt <- melt( L117.TPES_EJ_R_bld_tradbio_Yh, id.vars = R_S_F, variable_name = "Xyear" )
L117.maxSubResource_tradbio <- aggregate( L117.TPES_EJ_R_bld_tradbio_Yh.melt[ "value" ],
      by=as.list( L117.TPES_EJ_R_bld_tradbio_Yh.melt[ c( R_S_F ) ] ), max )

printlog( "Writing the supply curves to all regions, multiplying maxSubResouce by quantity available at each grade")
L117.RsrcCurves_EJ_R_tradbio <- repeat_and_add_vector( A17.tradbio_curves, R, sort( unique( iso_GCAM_regID[[R]] ) ) )[ c( R, names( A17.tradbio_curves ) ) ]
L117.RsrcCurves_EJ_R_tradbio$available <- L117.RsrcCurves_EJ_R_tradbio$available * L117.maxSubResource_tradbio$value[
      match( L117.RsrcCurves_EJ_R_tradbio[[R]], L117.maxSubResource_tradbio[[R]] ) ]

printlog( "Removing resource curves in regions where this fuel does not apply" )
rm_tradbio_regions <- A_regions[[ R ]][ A_regions$tradbio_region == 0 ]
L117.RsrcCurves_EJ_R_tradbio <- L117.RsrcCurves_EJ_R_tradbio[ !L117.RsrcCurves_EJ_R_tradbio[[R]] %in% rm_tradbio_regions, ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L117.RsrcCurves_EJ_R_tradbio <- c( "Traditional biomass resources by GCAM region","Unit = EJ" )

#write tables as CSV files
writedata( L117.RsrcCurves_EJ_R_tradbio, domain="ENERGY_LEVEL1_DATA", fn="L117.RsrcCurves_EJ_R_tradbio", comments=comments.L117.RsrcCurves_EJ_R_tradbio )

# Every script should finish with this line
logstop()
