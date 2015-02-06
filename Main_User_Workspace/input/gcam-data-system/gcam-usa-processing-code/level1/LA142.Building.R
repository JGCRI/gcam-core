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
logstart( "LA142.Building.R" )

printlog( "Buildings sector energy consumption" )



# -----------------------------------------------------------------------------

# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
L142.in_EJ_R_bld_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L142.in_EJ_R_bld_F_Yh" )

L101.inEIA_EJ_state_S_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L101.inEIA_EJ_state_S_F" )



# -----------------------------------------------------------------------------

# 2. Perform computations

#Subset residential and commercial from the SEDS table, and only the fuels that are part of the GCAM buildings sector. Sort by fuel.

printlog( "Calculating each state and sector's (res/comm) shares of USA building energy use, by fuel" )

printlog( "NOTE: Using SEDS rather than IEA for nation-level disaggregation between residential and commercial")
L142.in_EJ_state_bld_F_unscaled <- subset( L101.inEIA_EJ_state_S_F, sector %in% c( "comm", "resid" ) & fuel %in% L142.in_EJ_R_bld_F_Yh$fuel )



#Aggregate by fuel to calculate each state and sector's portional allocation

L142.in_EJ_USA_bld_F_unscaled <- aggregate( L142.in_EJ_state_bld_F_unscaled[ X_historical_years ],

      by=as.list( L142.in_EJ_state_bld_F_unscaled[ "fuel" ] ), sum ) 



#Calculate the portional allocation of total US buildings energy use (by each fuel) to each state and sector

L142.in_pct_state_bld_F <- L142.in_EJ_state_bld_F_unscaled
L142.in_pct_state_bld_F[ X_historical_years ] <- L142.in_EJ_state_bld_F_unscaled[ X_historical_years ] /
      L142.in_EJ_USA_bld_F_unscaled[ match( L142.in_EJ_state_bld_F_unscaled$fuel, L142.in_EJ_USA_bld_F_unscaled$fuel ),
      X_historical_years ]

#Apportion nation-level energy by fuel to states and sectors
#First need to aggregate the buildings sectors in the whole-usa data
L142.in_EJ_R_bldtot_F_Yh <- aggregate( L142.in_EJ_R_bld_F_Yh[ X_historical_years ],
      by=as.list( L142.in_EJ_R_bld_F_Yh[ c( "GCAM_region_ID", "fuel") ] ), sum )
L142.in_EJ_state_bld_F <- apportion_to_states(
      nation_data = subset( L142.in_EJ_R_bldtot_F_Yh, GCAM_region_ID == USA_regID ),
      state_share_data = L142.in_pct_state_bld_F,
      match_vectors = "fuel" )


# -----------------------------------------------------------------------------

# 3. Output

#Add comments for each table

comments.L142.in_EJ_state_bld_F <- c( "Buildings energy consumption by state, sector (res/comm) and fuel","Unit = EJ" )



#write tables as CSV files

writedata( L142.in_EJ_state_bld_F, domain="GCAMUSA_LEVEL1_DATA", fn="L142.in_EJ_state_bld_F", comments=comments.L142.in_EJ_state_bld_F )



# Every script should finish with this line

logstop()