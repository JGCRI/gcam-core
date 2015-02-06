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
logstart( "LB1233.Elec_water.R" )

printlog( "Water consumption by state, fuel, technology, and cooling system type" )

# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
UCS_tech_names <- readdata( "GCAMUSA_MAPPINGS", "UCS_tech_names" )
UCS_water_types <- readdata( "GCAMUSA_MAPPINGS", "UCS_water_types" )
Macknick_elec_water_m3MWh <- readdata( "GCAMUSA_LEVEL0_DATA", "Macknick_elec_water_m3MWh" )
UCS_Database <- readdata( "GCAMUSA_LEVEL0_DATA", "UCS_Database", skip = 7 )
L1231.out_EJ_state_elec_F_tech <- readdata( "GCAMUSA_LEVEL1_DATA", "L1231.out_EJ_state_elec_F_tech" )

# -----------------------------------------------------------------------------

#1b. Upfront adjustments
# Michigan and Wisconsin each have some power plants using seawater. Reset this.
UCS_Database$Reported.Water.Source..Type.[
      UCS_Database$State %in% c( "MI", "WI" ) & UCS_Database$Reported.Water.Source..Type. == "Ocean" ] <- "Surface Water"

# 2. Perform computations
UCS_Database$state <- UCS_Database$State
UCS_Database$out_MWh <- UCS_Database[[ "Estimated.Generation..MWh." ]]
UCS_Database <- merge( UCS_Database, UCS_tech_names )
UCS_Database <- merge( UCS_Database, UCS_water_types )

#For cases with no cooling system, just set the water type to "none". Revert this for hydro and PV
UCS_Database$water_type[ UCS_Database$cooling_system == "none" ] <- "none"
UCS_Database$water_type[ UCS_Database$technology == "hydro" ] <- "fresh"
UCS_Database$water_type[ UCS_Database$technology == "PV" ] <- "fresh"

#Aggregate and compute shares for states
L1233.out_MWh_state_elec_F_tech <- aggregate( UCS_Database[ "out_MWh" ],
      by=as.list( UCS_Database[ c( state, S_F_tech ) ] ), sum )
L1233.out_MWh_state_elec_F_tech_cool <- aggregate( UCS_Database[ "out_MWh" ],
      by=as.list( UCS_Database[ c( state, S_F_tech, "cooling_system", "water_type" ) ] ), sum )
L1233.out_MWh_state_elec_F_tech_cool$share <- L1233.out_MWh_state_elec_F_tech_cool$out_MWh /
      L1233.out_MWh_state_elec_F_tech$out_MWh[
        match( vecpaste( L1233.out_MWh_state_elec_F_tech_cool[ c( state, S_F_tech ) ] ),
               vecpaste( L1233.out_MWh_state_elec_F_tech[ c( state, S_F_tech ) ] ) ) ]

#Aggregate and compute shares for FERC subregions, subsetting only the power plants built in the 2000s
UCS_Database_2000s <- subset( UCS_Database, First.Year.of.Operation > 1999 )
UCS_Database_2000s$grid_region <- states_subregions$grid_region[ match( UCS_Database_2000s$state, states_subregions$state ) ]
L1233.out_MWh_sR_elec_F_tech <- aggregate( UCS_Database_2000s[ "out_MWh" ],
      by=as.list( UCS_Database_2000s[ c( "grid_region", S_F_tech ) ] ), sum )
L1233.out_MWh_sR_elec_F_tech_cool <- aggregate( UCS_Database_2000s[ "out_MWh" ],
      by=as.list( UCS_Database_2000s[ c( "grid_region", S_F_tech, "cooling_system", "water_type" ) ] ), sum )
L1233.out_MWh_sR_elec_F_tech_cool$share <- L1233.out_MWh_sR_elec_F_tech_cool$out_MWh /
      L1233.out_MWh_sR_elec_F_tech$out_MWh[
        match( vecpaste( L1233.out_MWh_sR_elec_F_tech_cool[ c( "grid_region", S_F_tech ) ] ),
               vecpaste( L1233.out_MWh_sR_elec_F_tech[ c( "grid_region", S_F_tech ) ] ) ) ]

#Calculate the national averages, to be used as default values where data are missing
L1233.out_MWh_USA_elec_F_tech <- aggregate( UCS_Database[ "out_MWh" ],
      by=as.list( UCS_Database[ c( S_F_tech ) ] ), sum )
L1233.out_MWh_USA_elec_F_tech_cool <- aggregate( UCS_Database[ "out_MWh" ],
      by=as.list( UCS_Database[ c( S_F_tech, "cooling_system", "water_type" ) ] ), sum )
L1233.out_MWh_USA_elec_F_tech_cool$share <- L1233.out_MWh_USA_elec_F_tech_cool$out_MWh /
      L1233.out_MWh_USA_elec_F_tech$out_MWh[
        match( vecpaste( L1233.out_MWh_USA_elec_F_tech_cool[ S_F_tech ] ),
               vecpaste( L1233.out_MWh_USA_elec_F_tech[ S_F_tech ] ) ) ]

#Write out all possible combinations of power plants, cooling system types, and water types in all states
L1233.elec_tech_cool <- unique( L1233.out_MWh_state_elec_F_tech_cool[ c( S_F_tech, "cooling_system", "water_type" ) ] )
L1233.elec_tech_cool <- L1233.elec_tech_cool[ order( L1233.elec_tech_cool$fuel, L1233.elec_tech_cool$technology ), ]
L1233.out_EJ_state_elec_F_tech_cool <- repeat_and_add_vector( L1233.elec_tech_cool, state, states )
L1233.out_EJ_state_elec_F_tech_cool$share <- L1233.out_MWh_state_elec_F_tech_cool$share[
      match( vecpaste( L1233.out_EJ_state_elec_F_tech_cool[ c( state, S_F_tech, "cooling_system", "water_type" ) ] ),
             vecpaste( L1233.out_MWh_state_elec_F_tech_cool[ c( state, S_F_tech, "cooling_system", "water_type" ) ] ) ) ]

#A lot of the shares are NAs at this point. Most should be zeroes, but where a technology has all zeroes, we replace the zeroes with national averages
L1233.out_EJ_state_elec_F_tech_cool$share[ is.na( L1233.out_EJ_state_elec_F_tech_cool$share ) ] <- 0
L1233.state_elec_F_tech_default <- subset(
      aggregate( L1233.out_EJ_state_elec_F_tech_cool[ "share" ],
         by=as.list( L1233.out_EJ_state_elec_F_tech_cool[ c( state, S_F_tech ) ] ), sum ),
      share == 0 )

#Replace missing values with national default shares
L1233.out_EJ_state_elec_F_tech_cool$share[
      vecpaste( L1233.out_EJ_state_elec_F_tech_cool[ c( state, S_F_tech ) ] ) %in% vecpaste( L1233.state_elec_F_tech_default[ c( state, S_F_tech ) ] ) ] <-
      L1233.out_MWh_USA_elec_F_tech_cool$share[
         match( vecpaste( L1233.out_EJ_state_elec_F_tech_cool[
                          vecpaste( L1233.out_EJ_state_elec_F_tech_cool[ c( state, S_F_tech ) ] ) %in% vecpaste( L1233.state_elec_F_tech_default[ c( state, S_F_tech ) ] ),
                          c( S_F_tech, "cooling_system", "water_type" ) ] ),
                vecpaste( L1233.out_MWh_USA_elec_F_tech_cool[ c( S_F_tech, "cooling_system", "water_type" ) ] ) ) ]

#Multiply through by historical output
L1233.out_EJ_state_elec_F_tech_cool[ X_historical_years ] <- L1233.out_EJ_state_elec_F_tech_cool$share * L1231.out_EJ_state_elec_F_tech[
      match( vecpaste( L1233.out_EJ_state_elec_F_tech_cool[ c( state, S_F_tech ) ] ),
             vecpaste( L1231.out_EJ_state_elec_F_tech[ c( state, S_F_tech ) ] ) ),
      X_historical_years ]
L1233.out_EJ_state_elec_F_tech_cool <- L1233.out_EJ_state_elec_F_tech_cool[ c( state, S_F_tech, "cooling_system", "water_type", X_historical_years ) ]

#Next, we'll write out the shares by FERC subregion for the new investment in the future periods
L1233.share_sR_elec_F_tech_cool <- repeat_and_add_vector( L1233.elec_tech_cool, "grid_region", sort( unique( states_subregions$grid_region ) ) )
L1233.share_sR_elec_F_tech_cool$share <- L1233.out_MWh_sR_elec_F_tech_cool$share[
      match( vecpaste( L1233.share_sR_elec_F_tech_cool[ c( "grid_region", S_F_tech, "cooling_system", "water_type" ) ] ),
             vecpaste( L1233.out_MWh_sR_elec_F_tech_cool[ c( "grid_region", S_F_tech, "cooling_system", "water_type" ) ] ) ) ]

#A lot of the shares are NAs at this point. Most should be zeroes, but where a technology has all zeroes, we replace the zeroes with national averages
L1233.share_sR_elec_F_tech_cool$share[ is.na( L1233.share_sR_elec_F_tech_cool$share ) ] <- 0
L1233.sR_elec_F_tech_default <- subset(
      aggregate( L1233.share_sR_elec_F_tech_cool[ "share" ],
         by=as.list( L1233.share_sR_elec_F_tech_cool[ c( "grid_region", S_F_tech ) ] ), sum ),
      share == 0 )

#Replace missing values with national default shares
L1233.share_sR_elec_F_tech_cool$share[
      vecpaste( L1233.share_sR_elec_F_tech_cool[ c( "grid_region", S_F_tech ) ] ) %in% vecpaste( L1233.sR_elec_F_tech_default[ c( "grid_region", S_F_tech ) ] ) ] <-
      L1233.out_MWh_USA_elec_F_tech_cool$share[
         match( vecpaste( L1233.share_sR_elec_F_tech_cool[
                          vecpaste( L1233.share_sR_elec_F_tech_cool[ c( "grid_region", S_F_tech ) ] ) %in% vecpaste( L1233.sR_elec_F_tech_default[ c( "grid_region", S_F_tech ) ] ),
                          c( S_F_tech, "cooling_system", "water_type" ) ] ),
                vecpaste( L1233.out_MWh_USA_elec_F_tech_cool[ c( S_F_tech, "cooling_system", "water_type" ) ] ) ) ]

#Just re-sort the columns; there is no output to multiply through here
L1233.share_sR_elec_F_tech_cool <- L1233.share_sR_elec_F_tech_cool[ c( "grid_region", S_F_tech, "cooling_system", "water_type", "share" ) ]

#Calculate the withdrawals and consumption of water by the power sector in the recent years
# doesn't make sense to write this out prior to 2000, unless the power plants constructed subsequently are removed prior to the calculations
water_years <- 2000:2010
X_water_years <- paste0( "X", water_years )

#Withdrawals
L1233.wdraw_km3_state_elec_F_tech_cool <- L1233.out_EJ_state_elec_F_tech_cool[ c( state, S_F_tech, "cooling_system", "water_type", X_water_years ) ]
L1233.wdraw_km3_state_elec_F_tech_cool[ X_water_years ] <- L1233.out_EJ_state_elec_F_tech_cool[ X_water_years ] * Macknick_elec_water_m3MWh$water_withdrawals[
      match( vecpaste( L1233.wdraw_km3_state_elec_F_tech_cool[ c( S_F_tech, "cooling_system", "water_type" ) ] ),
             vecpaste( Macknick_elec_water_m3MWh[ c( S_F_tech, "cooling_system", "water_type" ) ] ) ) ] /
      conv_MWh_GJ
L1233.wdraw_km3_state_elec_F_tech_cool <- subset( L1233.wdraw_km3_state_elec_F_tech_cool, water_type != "none" )
L1233.wdraw_km3_state_elec <- aggregate( L1233.wdraw_km3_state_elec_F_tech_cool[ X_water_years ],
      by=as.list( L1233.wdraw_km3_state_elec_F_tech_cool[ c( state, "sector", "water_type" ) ] ), sum )

#Consumption
L1233.wcons_km3_state_elec_F_tech_cool <- L1233.out_EJ_state_elec_F_tech_cool[ c( state, S_F_tech, "cooling_system", "water_type", X_water_years ) ]
L1233.wcons_km3_state_elec_F_tech_cool[ X_water_years ] <- L1233.out_EJ_state_elec_F_tech_cool[ X_water_years ] * Macknick_elec_water_m3MWh$water_consumption[
      match( vecpaste( L1233.wcons_km3_state_elec_F_tech_cool[ c( S_F_tech, "cooling_system", "water_type" ) ] ),
             vecpaste( Macknick_elec_water_m3MWh[ c( S_F_tech, "cooling_system", "water_type" ) ] ) ) ] /
      conv_MWh_GJ
L1233.wcons_km3_state_elec_F_tech_cool <- subset( L1233.wcons_km3_state_elec_F_tech_cool, water_type != "none" )
L1233.wcons_km3_state_elec <- aggregate( L1233.wcons_km3_state_elec_F_tech_cool[ X_water_years ],
      by=as.list( L1233.wcons_km3_state_elec_F_tech_cool[ c( state, "sector", "water_type" ) ] ), sum )

# -----------------------------------------------------------------------------

# 3. Output

#Add comments for each table
comments.L1233.out_EJ_state_elec_F_tech_cool <- c( "Electricity output by state / fuel / technology / cooling system / water type","Unit = EJ" )
comments.L1233.share_sR_elec_F_tech_cool <- c( "Electricity generation shares by state / fuel / technology / cooling system / water type","Unit = EJ" )
comments.L1233.wdraw_km3_state_elec <- c( "Water withdrawals for electricity generation by state / water type","Unit = km3 (bm3)" )
comments.L1233.wcons_km3_state_elec <- c( "Water consumption for electricity generation by state / water type","Unit = km3 (bm3)" )

#write tables as CSV files
writedata( L1233.out_EJ_state_elec_F_tech_cool, domain="GCAMUSA_LEVEL1_DATA", fn="L1233.out_EJ_state_elec_F_tech_cool", comments=comments.L1233.out_EJ_state_elec_F_tech_cool )
writedata( L1233.share_sR_elec_F_tech_cool, domain="GCAMUSA_LEVEL1_DATA", fn="L1233.share_sR_elec_F_tech_cool", comments=comments.L1233.share_sR_elec_F_tech_cool )
writedata( L1233.wdraw_km3_state_elec, domain="GCAMUSA_LEVEL1_DATA", fn="L1233.wdraw_km3_state_elec", comments=comments.L1233.wdraw_km3_state_elec )
writedata( L1233.wcons_km3_state_elec, domain="GCAMUSA_LEVEL1_DATA", fn="L1233.wcons_km3_state_elec", comments=comments.L1233.wcons_km3_state_elec )

# Every script should finish with this line
logstop()