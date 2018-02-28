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
logstart( "L145.water.demand.municipal.R" )
printlog( "Calculate water demand withdrawals, consumption coefficients, and base prices for municipal water use" )

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "WATER_ASSUMPTIONS", "A_water_data", extension = ".R" )
sourcedata( "MODELTIME_ASSUMPTIONS", "A_modeltime_data", extension = ".R" )
GCAM_region_iso <- readdata( "COMMON_MAPPINGS", "iso_GCAM_regID")
GCAM_region_names <- readdata( "COMMON_MAPPINGS", "GCAM_region_names")
AGLU_ctry <- readdata( "AGLU_MAPPINGS", "AGLU_ctry" )
FAO_municipal_water_AQUASTAT <- readdata( "WATER_LEVEL0_DATA", "FAO_municipal_water_AQUASTAT" )
IBNET_municipal_water_cost_USDm3 <- readdata( "WATER_LEVEL0_DATA", "IBNET_municipal_water_cost_USDm3" )
municipal_water_use_efficiency <- readdata( "WATER_LEVEL0_DATA", "municipal_water_use_efficiency" )
manufacturing_water_mapping <- readdata( "WATER_MAPPINGS", "manufacturing_water_mapping" )

# TODO: why won't this file read the header right? FIX!!
FAO_municipal_water_AQUASTAT$Year <- paste0( 'X', FAO_municipal_water_AQUASTAT$Year )
FAO_municipal_water_AQUASTAT$Value <- as.numeric( FAO_municipal_water_AQUASTAT$Value )

# -----------------------------------------------------------------------------

# 2. Process data

# The FAO_municipal_water_AQUASTAT is already in the right units, we just need to aggregate them to GCAM regions
# an fill out the years for missing data.
printlog( "Map FAO country to GCAM region ID" )
L145.FAO_ctry_GCAM_region_ID <- merge( GCAM_region_iso[, c("iso", R) ], AGLU_ctry[, c("iso", "FAO_country" ) ] )
L145.ctry_municipal_W <- merge( FAO_municipal_water_AQUASTAT, L145.FAO_ctry_GCAM_region_ID[, c( R, "FAO_country" ) ], by.x="Area", by.y="FAO_country" )

printlog( "Aggregate to regions and fill out missing years using rule=2" )
L145.municipal_water_R_W_Yh_km3 <- aggregate( Value ~ GCAM_region_ID + Year, L145.ctry_municipal_W, FUN=sum )
L145.municipal_water_R_W_Yh_km3 <- dcast( L145.municipal_water_R_W_Yh_km3, GCAM_region_ID ~ Year, value.var="Value" )
L145.municipal_water_R_W_Yh_km3 <- gcam_interp( L145.municipal_water_R_W_Yh_km3, historical_years, rule=2 )
L145.municipal_water_R_W_Yh_km3[[water_type]] <- water_W
L145.municipal_water_R_W_Yh_km3 <- L145.municipal_water_R_W_Yh_km3[, c( R, water_type, X_historical_years ) ]

# Come up with GCAM reginal average prices starting with the country level IBNET data.
# Note since the years are all over the place we will just use the average across years too.
printlog( "Come up with regional municipal prices from IBNET data" )
L145.ctry_municipal_prices <- merge( IBNET_municipal_water_cost_USDm3, L145.FAO_ctry_GCAM_region_ID, all.x=T, by.x="country", by.y="FAO_country" )
L145.ctry_municipal_prices$expenditure <- L145.ctry_municipal_prices$cost * L145.ctry_municipal_prices$consumption
L145.municipal_water_cost_R_75USD_m3 <- aggregate( L145.ctry_municipal_prices[, c( "expenditure", "consumption" ) ], by=as.list(
    L145.ctry_municipal_prices[, R, drop=F ] ), FUN=sum )
# TODO: what are the dollar units?
L145.municipal_water_cost_R_75USD_m3$input.cost <- L145.municipal_water_cost_R_75USD_m3$expenditure / L145.municipal_water_cost_R_75USD_m3$consumption
L145.municipal_water_cost_R_75USD_m3 <- L145.municipal_water_cost_R_75USD_m3[, c( R, "input.cost" ) ]
# The IBNET data is incomplete and so it is very possible that we have entire GCAM regions in which none
# of the contained countries had any reported data.
stopifnot( nrow( L145.municipal_water_cost_R_75USD_m3 ) == nrow( GCAM_region_names ) )

# Map water use efficiencies from the continent scale to GCAM regions
# Note names were changed to match manufacturing continent to avoid the need for an additional mapping
printlog( "Map municipal water use efficiencies to GCAM regions" )
L145.municipal_water_eff_R_Y <- merge( manufacturing_water_mapping, municipal_water_use_efficiency )
L145.municipal_water_eff_R_Y <- merge( L145.municipal_water_eff_R_Y, GCAM_region_names )

printlog( "Fillout the coefficients for all years using rule=2" )
L145.municipal_water_eff_R_Y <- interpolate_and_melt( L145.municipal_water_eff_R_Y, model_years, value.name="coefficient", rule=2 )
L145.municipal_water_eff_R_Y <- L145.municipal_water_eff_R_Y[, c( R, Y, "coefficient" ) ]

# 3. Output

#Add comments for each table
comments.L145.municipal_water_R_W_Yh_km3<- c( "Municipal water withdrawals by GCAM_region_ID for all historical years","Unit = km^3" )
comments.L145.municipal_water_cost_R_75USD_m3 <- c( "Municipal water base deleivery cost by GCAM_region_ID","Unit = 1975$/m^3" )
comments.L145.municipal_water_eff_R_Y <- c( "Municipal water use efficiency by GCAM_region_ID for all years","Unit = %" )

#write tables as CSV files
writedata( L145.municipal_water_R_W_Yh_km3, domain="WATER_LEVEL1_DATA", fn="L145.municipal_water_R_W_Yh_km3", comments=comments.L145.municipal_water_R_W_Yh_km3 )
writedata( L145.municipal_water_cost_R_75USD_m3, domain="WATER_LEVEL1_DATA", fn="L145.municipal_water_cost_R_75USD_m3", comments=comments.L145.municipal_water_cost_R_75USD_m3 )
writedata( L145.municipal_water_eff_R_Y, domain="WATER_LEVEL1_DATA", fn="L145.municipal_water_eff_R_Y", comments=comments.L145.municipal_water_eff_R_Y )

# Every script should finish with this line
logstop()
