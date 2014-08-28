# L112.U.R

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
logstart( "LA112.U.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Uranium supply curves" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names" )
A12.U_curves <- readdata( "ENERGY_ASSUMPTIONS", "A12.U_curves" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. Uranium supply curves
# Currently not built up from inventory data; just using GCAM 3.0 values
#These were not disaggregated to regions in GCAM 3.0. Keeping this convention for now.
printlog( "NOTE: Assigning global uranium supply curve to GCAM_region_ID 1")
L112.RsrcCurves_Mt_R_U <- data.frame(
      GCAM_region_ID = 1,
      resource = A12.U_curves$resource,
      subresource = A12.U_curves$subresource,
      grade = A12.U_curves$grade,
      extractioncost = A12.U_curves$extractioncost,
      available = A12.U_curves$available )

# 2b. Historical uranium prices (currently assumed at global level, so no level 1 processing necessary)

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L112.RsrcCurves_Mt_R_U <- c( "Uranium resource curves by GCAM region","Unit = Mt" )

#write tables as CSV files
writedata( L112.RsrcCurves_Mt_R_U, domain="ENERGY_LEVEL1_DATA", fn="L112.RsrcCurves_Mt_R_U", comments=comments.L112.RsrcCurves_Mt_R_U )

# Every script should finish with this line
logstop()
