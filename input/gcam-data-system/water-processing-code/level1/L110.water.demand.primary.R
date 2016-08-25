# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "WATERPROC_DIR" ) ){
    if( Sys.getenv( "WATERPROC" ) != "" ){
        WATERPROC_DIR <- Sys.getenv( "WATERPROC" )
    } else {
        stop("Could not determine location of water data system. Please set the R var WATERPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(WATERPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(WATERPROC_DIR,"/../_common/headers/WATER_header.R",sep=""))
logstart( "L110.water.demand.primary.R" )
printlog( "Calculate water demand coefficients for primary energy by water type." )

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "WATER_ASSUMPTIONS", "A_water_data", extension = ".R" )
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
basin_ID <- readdata( "WATER_MAPPINGS", "basin_ID" )
A227.resource_water_coef_mapping <- readdata( "WATER_ASSUMPTIONS", "A227.resource_water_coef_mapping" )
resource_water_data <- readdata( "WATER_LEVEL0_DATA", "resource_water_data" )
# TODO: make this generic to the region definition
resource_water_share_32 <- readdata( "WATER_LEVEL0_DATA", "resource_water_share_32" )

# -----------------------------------------------------------------------------

# 2. Process data
printlog( "Just use the global average water coefficient.  We will use the fuels in the mapping to match the rows" )
L110.global_water_cons_coef <- merge( A227.resource_water_coef_mapping[, c( "fuel", supp ) ],
    resource_water_data[, c( "fuel", "water.coefficient.m3.per.TJ" ) ] )
# TODO: remove once mapping fixed
L110.global_water_cons_coef <- unique( L110.global_water_cons_coef )
L110.regional_water_coef <- data.frame( L110.global_water_cons_coef[ rep( 1:nrow( L110.global_water_cons_coef ), times=nrow( GCAM_region_names ) ), ] )
L110.regional_water_coef[, names( resource_water_share_32 ) ] <- resource_water_share_32[ sort( 
    rep( 1:nrow( resource_water_share_32 ), times=nrow( L110.global_water_cons_coef ) ) ), ]
L110.regional_water_coef$seawater <- L110.regional_water_coef$water.coefficient.m3.per.TJ * L110.regional_water_coef$fraction.salinewater / 1000
L110.regional_water_coef[[water_C]] <- L110.regional_water_coef$water.coefficient.m3.per.TJ * L110.regional_water_coef$fraction.freshwater / 1000
L110.regional_water_coef[[water_W]] <- L110.regional_water_coef[[water_C]] / L110.regional_water_coef$ratio.consumption.withdrawal.fresh
L110.regional_water_coef <- L110.regional_water_coef[, c( R, supp, water_C, water_W, "seawater" ) ]
L110.water_demand_primary_R_S_W_m3_GJ <- melt( L110.regional_water_coef, id.vars=c( R, supp ), variable.name=water_type, value.name="coefficient" )

# 3. Output

#Add comments for each table
comments.L110.water_demand_primary_R_S_W_m3_GJ <- c( "Primary energy water coefficients by region ID / supplysector / water type","Unit = m^3 / GJ" )

#write tables as CSV files
writedata( L110.water_demand_primary_R_S_W_m3_GJ, domain="WATER_LEVEL1_DATA", fn="L110.water_demand_primary_R_S_W_m3_GJ", comments=comments.L110.water_demand_primary_R_S_W_m3_GJ )

# Every script should finish with this line
logstop()
