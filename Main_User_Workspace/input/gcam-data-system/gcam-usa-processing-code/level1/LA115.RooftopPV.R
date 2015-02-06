# Before we can load headers we need some paths defined.  They
# may be provided by a system environment variable or just
# having already been set in the workspace
if( !exists( "GCAMUSAPROC_DIR" ) ){
    if( Sys.getenv( "GCAMUSAPROC" ) != "" ){
        GCAMUSAPROC_DIR <- Sys.getenv( "GCAMUSAPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var GCAMUSAPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(GCAMUSAPROC_DIR,"/../_common/headers/GCAMUSA_header.R",sep=""))
logstart( "LA115.RooftopPV.R" )
printlog( "Rooftop PV resources by state" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
NREL_Com_PV_supply_curve <- readdata( "GCAMUSA_LEVEL0_DATA", "NREL_Com_PV_supply_curve" )
NREL_Res_PV_supply_curve <- readdata( "GCAMUSA_LEVEL0_DATA", "NREL_Res_PV_supply_curve" )
A15.roofPV_TechChange <- readdata( "ENERGY_ASSUMPTIONS", "A15.roofPV_TechChange" )

# -----------------------------------------------------------------------------

# 2. Perform computations
# The first part of this file walks through a series of calculations to define the minimum levelized cost of electricity production
# by rooftop PV in the final historical year, based on a series of user-modifiable assumptions. Because this is the only code file that relies on these
# assumptions, all data assumptions are made here.
PV_derating_factor <- 0.77     #this includes a variety of factors, including inverters, transformers, mismatch, soiling, and others
PV_resid_installed_cost_2005USDkW <- 9500
PV_comm_installed_cost_2005USDkW <- 7290
PV_resid_OM_2005USDkWyr <- 100
PV_comm_OM_2005USDkWyr <- 40
PV_lifetime <- 30
PV_discount_rate <- 0.1
hours_per_year <- 8760
max_resid_capacity_factor <- NREL_Res_PV_supply_curve$MWh[ NREL_Res_PV_supply_curve$Relative_Cost == min( NREL_Res_PV_supply_curve$Relative_Cost ) ] /
      NREL_Res_PV_supply_curve$MW[ NREL_Res_PV_supply_curve$Relative_Cost == min( NREL_Res_PV_supply_curve$Relative_Cost ) ] /
      hours_per_year
max_comm_capacity_factor <- NREL_Com_PV_supply_curve$GWH[ NREL_Com_PV_supply_curve$Rel_Cost == min( NREL_Com_PV_supply_curve$Rel_Cost ) ] * conv_bil_mil /
      NREL_Com_PV_supply_curve$MW[ NREL_Com_PV_supply_curve$Rel_Cost == min( NREL_Com_PV_supply_curve$Rel_Cost ) ] /
      hours_per_year
PV_FCR <- ( PV_discount_rate * ( 1 + PV_discount_rate ) ^ PV_lifetime ) / ( ( PV_discount_rate + 1 ) ^ PV_lifetime - 1 )
PV_resid_min_LEC_2005USDkWh <- ( PV_resid_installed_cost_2005USDkW / PV_derating_factor ) * PV_FCR / ( max_resid_capacity_factor * hours_per_year ) +
                                 PV_resid_OM_2005USDkWyr / ( max_resid_capacity_factor * hours_per_year )
PV_resid_min_LEC_1975USDGJ <- PV_resid_min_LEC_2005USDkWh * conv_2005_1975_USD / conv_kwh_GJ

PV_comm_min_LEC_2005USDkWh <- ( PV_comm_installed_cost_2005USDkW / PV_derating_factor ) * PV_FCR / ( max_comm_capacity_factor * hours_per_year ) +
                                 PV_comm_OM_2005USDkWyr / ( max_comm_capacity_factor * hours_per_year )
PV_comm_min_LEC_1975USDGJ <- PV_comm_min_LEC_2005USDkWh * conv_2005_1975_USD / conv_kwh_GJ

smooth_res_curve_approx <- function( b, midp, p ) {
    p_pow_b <- p ^ b
    f_p <- p_pow_b / ( midp ^ b + p_pow_b )
}

# Computes the smooth renewable resource function with the given parameters
# at the price points from the actual supply curve.  Then return the
# error between the computed values and the actual values
smooth_res_curve_approx_error <- function( b, midp, supply_curve ) {
    f_p <- smooth_res_curve_approx( b, midp, supply_curve$p )
    error <- f_p - supply_curve$percent_cumul
    crossprod( error, error )
}

printlog( "Compute supply curves" )
# Change Res generation to GWh to be consistent with Com
L115.pv_sc <- with( NREL_Res_PV_supply_curve, data.frame( state_name=State, p=Relative_Cost * PV_resid_min_LEC_1975USDGJ, generation=MWh * conv_mil_bil ) )
L115.pv_sc <- with( NREL_Com_PV_supply_curve, rbind( L115.pv_sc,
    data.frame( state_name=State, p=Rel_Cost * PV_comm_min_LEC_1975USDGJ, generation=GWH ) ) )
L115.pv_sc <- merge( states_subregions[,c( "state", "state_name" ) ], L115.pv_sc )
L115.pv_sc <- L115.pv_sc[ order( L115.pv_sc$p ), ]
L115.pv_sc$p <- L115.pv_sc$p - min( L115.pv_sc$p )

#states <- c( "*" )
#states <- c( "*", "AR", "CA", "MD", "DC" )
L115.rsrc_state_rooftopPV <- data.frame()
for(L115.state in states ) {
# We want prices relative to the smallest cost
if( L115.state == "*" ) {
L115.pv_sc_State <- L115.pv_sc
} else {
L115.pv_sc_State <- L115.pv_sc[ L115.pv_sc$state == L115.state, ]
}
L115.pv_sc_State <- rbind( L115.pv_sc_State[ 1, ], L115.pv_sc_State )
L115.pv_sc_State[ 1, c( "p", "generation" ) ] <- cbind( 0, 0 )
#L115.base_price <- min( L115.pv_sc_State$p )
#L115.pv_sc_State$p <- L115.pv_sc_State$p - L115.base_price
L115.pv_sc_State$cumul <- 0
for (i in 1:nrow(L115.pv_sc_State)) {
    if (i == 1) {
        L115.pv_sc_State[ i, "cumul" ] <- L115.pv_sc_State[ i, "generation" ]
    } else {
        L115.pv_sc_State[ i, "cumul" ] <- L115.pv_sc_State[ i-1, "cumul" ] + L115.pv_sc_State[ i, "generation" ]
    }
}
L115.pv_sc_State$percent_cumul <- L115.pv_sc_State$cumul / sum( L115.pv_sc_State$generation )
# Find the mid price.  We do this by taking the first % that is greater
# than or equal to %50 which biases us slightly to the high side
# with(L115.pv_sc_State,which(abs(percent_cumul-0.5)==min(abs(percent_cumul-0.5)))
L115.mid_p <- L115.pv_sc_State[ L115.pv_sc_State$percent_cumul >= 0.5, "p" ][1]

#L115.error_min_b <- nlm( smooth_res_curve_approx_error, 4.24, L115.mid_p, L115.pv_sc_State )
# A return code greater than 2 indicates a failure to solve
#stopifnot( L115.error_min_b$code <= 2 )
L115.error_min_b <- optimize(f=smooth_res_curve_approx_error, interval=c(1.0,15.0), L115.mid_p, L115.pv_sc_State )
L115.rsrc_state_rooftopPV <- rbind( L115.rsrc_state_rooftopPV,
    data.frame( state=L115.state, mid_p=L115.mid_p, b_exp=L115.error_min_b$minimum ) )

#L115.plot <- rbind( data.frame( p=L115.pv_sc_State$p, fract=L115.pv_sc_State$percent_cumul, type="data" ),
#                    data.frame( p=L115.pv_sc_State$p, fract=smooth_res_curve_approx( 4.24, L115.mid_p, L115.pv_sc_State$p ), type="guess" ),
#                    data.frame( p=L115.pv_sc_State$p, fract=smooth_res_curve_approx( L115.error_min_b$minimum, L115.mid_p, L115.pv_sc_State$p ), type="solved" ) )
#print(
#ggplot() + layer(geom="point", data=L115.plot[L115.plot$type == "data",],mapping=aes(x=fract,y=p),color="black") +
#    layer(geom="line", data=L115.plot[L115.plot$type == "guess",],mapping=aes(x=fract,y=p),color="green") +
#    layer(geom="line", data=L115.plot[L115.plot$type == "solved",],mapping=aes(x=fract,y=p),color="red")
#)
#readline( prompt="Check it out" )
}

L115.max_gen <- aggregate( generation ~ state, data=L115.pv_sc, FUN=sum )
# convert to from GWh to EJ
L115.max_gen$generation <- L115.max_gen$generation * 0.0000036;
L115.rsrc_state_rooftopPV <- merge( L115.rsrc_state_rooftopPV, L115.max_gen )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L115.rsrc_state_rooftopPV <- c( "Resource curves for rooftop PV (commercial and residential together)","Unit = 1975$/GJ (mid-price) and EJ/yr (maxsubresource)" )

#write tables as CSV files
writedata( L115.rsrc_state_rooftopPV, domain="GCAMUSA_LEVEL1_DATA", fn="L115.rsrc_state_rooftopPV", comments=comments.L115.rsrc_state_rooftopPV )

# Every script should finish with this line
logstop()

