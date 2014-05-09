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

# Coutry level supply curves used to buildup the regional supply curves
Zhou_wind_supply_ctry_EJ <- readdata( "ENERGY_LEVEL0_DATA", "Zhou_wind_supply_ctry_EJ" )
# GCAM3 supply curves used only for resource names
A14.wind_curves <- readdata( "ENERGY_ASSUMPTIONS", "A14.wind_curves" )

# Define the function which computes the smooth renewable resource function
# supply = (p - base.price) ^ curve.exponent / (mid.price ^ curve.exponent +
#    (p - base.price) ^ curve.exponent * maxSubResource
evaluate_smooth_res_curve <- function(curve.exponent, mid.price, base.price, maxSubResource, p) {
    p_pow_exp <- ( p - base.price ) ^ curve.exponent
    supply <- p_pow_exp / ( mid.price ^ curve.exponent + p_pow_exp ) * maxSubResource
    # zero out the supply where the price was less than the base.price
    supply[ p < base.price ] <- 0
    return( supply )
}

# Checks how well the given smooth renewable curve matches the given supply-points
# Note that the first argument is the one that is changed by optimize when trying to
# minimize the error
smooth_res_curve_approx_error <- function(curve.exponent, mid.price, base.price, maxSubResource, supply_points) {
    f_p <- evaluate_smooth_res_curve( curve.exponent, mid.price, base.price, maxSubResource, supply_points$price )
    error <- f_p - supply_points$supply
    return( crossprod( error, error ) )
}

# -----------------------------------------------------------------------------
# 2. Perform computations
printlog( "Attach region IDs and convert price units" )
L114.RsrcCurves_EJ_ctry_wind <- merge( Zhou_wind_supply_ctry_EJ, iso_GCAM_regID )
L114.RsrcCurves_EJ_ctry_wind$mid.price <- L114.RsrcCurves_EJ_ctry_wind$mid.price * conv_2007_1975_USD / conv_kwh_GJ
L114.RsrcCurves_EJ_ctry_wind$base.price <- L114.RsrcCurves_EJ_ctry_wind$base.price * conv_2007_1975_USD / conv_kwh_GJ
# Useful diagnostic to ensure we can replicate the GCAM3 supply curves
#L114.RsrcCurves_EJ_ctry_wind <- merge( A14.wind_curves, unique(iso_GCAM_regID[, c("region_GCAM3", "GCAM_region_ID")]) )

printlog( "Evaluate all supply curves by GCAM_region_ID to generate (price, supply) points" )
# Arbitrary params to determine how many price points to evaluate the supply curve which
# should cover the entire curve with sufficient density.
L114.price_inc <- 0.1
L114.supply_tol <- 0.01
L114.SupplyPoints <- data.frame()
for(region_ID in unique(L114.RsrcCurves_EJ_ctry_wind$GCAM_region_ID) ) {
    L114.curr_region_RsrcCurves <- subset( L114.RsrcCurves_EJ_ctry_wind, GCAM_region_ID == region_ID )
    L114.price <- min( L114.curr_region_RsrcCurves$base.price )
    # Evaluate the supply curve until the supply doesn't seem to be changing more than a tolerance
    # with a minimum number of evaluations to ensure we make progress into a very shallow curve as well.
    L114.prev_supply <- 0
    L114.min_calcs <- 100
    repeat {
        # Evaluate supply curves for all coutries in the region at the current price and sum up the supply
        L114.SupplyPoints.currR <- data.frame( GCAM_region_ID=region_ID, price=L114.price, supply=0 )
        for(i in 1:nrow(L114.curr_region_RsrcCurves) ) {
            L114.SupplyPoints.currR$supply <- L114.SupplyPoints.currR$supply + do.call( evaluate_smooth_res_curve,
                c( L114.curr_region_RsrcCurves[ i, c( "curve.exponent", "mid.price", "base.price", "maxSubResource" ) ],
                   L114.price ) )
        }
        L114.SupplyPoints <- rbind( L114.SupplyPoints, L114.SupplyPoints.currR )
        if(L114.min_calcs <= 0 & ( L114.SupplyPoints.currR$supply - L114.prev_supply ) < L114.supply_tol ) {
            # No longer making any difference in supply, we can stop
            break
        } else {
            # Supply is still changing, need more price points
            L114.min_calcs <- L114.min_calcs - 1
            L114.price <- L114.price + L114.price_inc
            L114.prev_supply <- L114.SupplyPoints.currR$supply
        }
    }
}

# Refit the supply curve to the supply-points we just calculated by evaulauting the ctry level supply curves
# so that we can aggregate them to the regional level.
printlog( "Refit the renewable resource curve to the supply-points we just calculated." )
L114.RsrcCurves_EJ_R_wind <- data.frame()
for(region_ID in unique(L114.RsrcCurves_EJ_ctry_wind$GCAM_region_ID) ) {
    L114.SupplyPoints.currR <- subset( L114.SupplyPoints, GCAM_region_ID == region_ID )
    L114.RsrcCurves_EJ_R_wind.currR <- data.frame( GCAM_region_ID=region_ID, base.price=min( L114.SupplyPoints.currR$price ),
        maxSubResource=max( L114.SupplyPoints.currR$supply ) )
    # The points were calculated in increasing price order with upward sloping curves.  Thus we can
    # approximate the mid.price by looking at the first price point of the supply that is 50% of the max.
    L114.RsrcCurves_EJ_R_wind.currR$mid.price <- L114.SupplyPoints.currR[ L114.SupplyPoints.currR$supply >= 0.5 *
        L114.RsrcCurves_EJ_R_wind.currR$maxSubResource, "price" ][1]
    # GCAM assumes that the base.price has already been taken out of the mid.price
    L114.RsrcCurves_EJ_R_wind.currR$mid.price <- with( L114.RsrcCurves_EJ_R_wind.currR, mid.price - base.price )

    # We must solve for the curve exponent that best fits the supply-points
    L114.error_min_curve.exp <- optimize( f=smooth_res_curve_approx_error, interval=c(1.0,15.0),
        L114.RsrcCurves_EJ_R_wind.currR$mid.price,
        L114.RsrcCurves_EJ_R_wind.currR$base.price,
        L114.RsrcCurves_EJ_R_wind.currR$maxSubResource,
        L114.SupplyPoints.currR )
    L114.RsrcCurves_EJ_R_wind.currR$curve.exponent <- L114.error_min_curve.exp$minimum

    L114.RsrcCurves_EJ_R_wind <- rbind( L114.RsrcCurves_EJ_R_wind, L114.RsrcCurves_EJ_R_wind.currR )
}

# Add resource names and clean up for writing out
L114.RsrcCurves_EJ_R_wind <- L114.RsrcCurves_EJ_R_wind[ order( L114.RsrcCurves_EJ_R_wind[[R]] ), ]
L114.RsrcCurves_EJ_R_wind$resource <- unique( A14.wind_curves$resource )
L114.RsrcCurves_EJ_R_wind$subresource <- unique( A14.wind_curves$subresource )
L114.RsrcCurves_EJ_R_wind <- L114.RsrcCurves_EJ_R_wind[, c( R, "resource", "subresource", "maxSubResource",
    "mid.price", "curve.exponent", "base.price" ) ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L114.RsrcCurves_EJ_R_wind <- c( "Wind resource curves by GCAM region","Unit = EJ" )

#write tables as CSV files
writedata( L114.RsrcCurves_EJ_R_wind, domain="ENERGY_LEVEL1_DATA", fn="L114.RsrcCurves_EJ_R_wind", comments=comments.L114.RsrcCurves_EJ_R_wind )

# Every script should finish with this line
logstop()
