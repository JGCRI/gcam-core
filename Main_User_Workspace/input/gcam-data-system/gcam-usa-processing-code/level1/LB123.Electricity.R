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
logstart( "LB123.Electricity.R" )

printlog( "Electricity sector inputs and outputs, and electricity ownuse" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
states_subregions <- readdata( "GCAMUSA_MAPPINGS", "states_subregions" )
NREL_us_re_technical_potential <- readdata( "GCAMUSA_LEVEL0_DATA", "NREL_us_re_technical_potential" )
L123.in_EJ_R_elec_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L123.in_EJ_R_elec_F_Yh" )
L123.out_EJ_R_elec_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L123.out_EJ_R_elec_F_Yh" )
EIA_elect_td_ownuse_prices <- readdata( "GCAMUSA_LEVEL0_DATA", "EIA_elect_td_ownuse_prices" )
L126.in_EJ_R_elecownuse_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L126.in_EJ_R_elecownuse_F_Yh" )
L126.out_EJ_R_elecownuse_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L126.out_EJ_R_elecownuse_F_Yh" )
L101.inEIA_EJ_state_S_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L101.inEIA_EJ_state_S_F" )
L132.out_EJ_state_indchp_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L132.out_EJ_state_indchp_F" )

# -----------------------------------------------------------------------------
# SEDS indicates electricity generation technologies either in terms of fuel inputs or fuel outputs (not both)
#ELECTRICITY_INPUT: coal, gas, oil, biomass
#ELECTRICITY_OUTPUT: nuclear and renewables
L123.in_EJ_state_elec_F_unscaled <- subset( L101.inEIA_EJ_state_S_F, sector %in% c( "electricity_input", "electricity_output" ) )

#Aggregate by fuel compute each state's percentage, by fuel
L123.in_EJ_USA_elec_Fin_unscaled <- aggregate( L123.in_EJ_state_elec_F_unscaled[ X_historical_years ],
      by=as.list( L123.in_EJ_state_elec_F_unscaled[ S_F ]), sum)

L123.pct_state_elec_F <- L123.in_EJ_state_elec_F_unscaled
L123.pct_state_elec_F[ X_historical_years ] <- L123.in_EJ_state_elec_F_unscaled[ X_historical_years ] /
      L123.in_EJ_USA_elec_Fin_unscaled[ match( L123.pct_state_elec_F$fuel, L123.in_EJ_USA_elec_Fin_unscaled$fuel ),
      X_historical_years ]
L123.pct_state_elec_F[ is.na( L123.pct_state_elec_F ) ] <- 0

printlog( "NOTE: SEDS does not disaggregate PV and CSP. Using solar shares for PV, and NREL data for CSP" )
# Many states have zero CSP potential, and allocating production to these states will cause errors later on
L123.pct_state_elec_F$fuel[ L123.pct_state_elec_F$fuel == "solar" ] <- "solar PV"

NREL_us_re_technical_potential$state <- states_subregions$state[ match( NREL_us_re_technical_potential$State, states_subregions$state_name ) ]
state_CSP_shares <- data.frame( state = states, CSP_share = NREL_us_re_technical_potential$CSP_GWh[ match( states, NREL_us_re_technical_potential$state ) ] )
state_CSP_shares$CSP_share <- state_CSP_shares$CSP_share / sum( state_CSP_shares$CSP_share )

L123.pct_state_elec_CSP <- subset( L123.pct_state_elec_F, fuel == "solar PV" )
L123.pct_state_elec_CSP$fuel <- "solar CSP"
L123.pct_state_elec_CSP[ X_historical_years ] <- state_CSP_shares$CSP_share[
      match( L123.pct_state_elec_CSP$state, state_CSP_shares$state)]

L123.pct_state_elec_F <- rbind( L123.pct_state_elec_F, L123.pct_state_elec_CSP )

printlog( "Electricity generation inputs by fuel and state" )
#Note that fuel inputs are only available for selected fuels; state_share_data uses this relevant subset
L123.in_EJ_state_elec_F <- apportion_to_states(
      nation_data = subset( L123.in_EJ_R_elec_F_Yh, GCAM_region_ID == USA_regID ),
      state_share_data = subset( L123.pct_state_elec_F, fuel %in% L123.in_EJ_R_elec_F_Yh$fuel ),
      match_vectors = "fuel" )
L123.in_EJ_state_elec_F$sector <- "electricity generation"

printlog( "Electricity generation outputs by fuel and state" )
L123.out_EJ_state_elec_F <- apportion_to_states(
      nation_data = subset( L123.out_EJ_R_elec_F_Yh, GCAM_region_ID == USA_regID & fuel %in% L123.pct_state_elec_F$fuel ),
      state_share_data = L123.pct_state_elec_F,
      match_vectors = "fuel" )
L123.out_EJ_state_elec_F$sector <- "electricity generation"

#ELECTRICITY - OWNUSE
printlog( "Electricity own use by state" )
printlog( "NOTE: Electricity net own use energy is apportioned to states on the basis of EIA's direct use by state" )
#First calculate the national own use quantity
L123.in_EJ_USA_ownuse <- subset( L126.in_EJ_R_elecownuse_F_Yh, GCAM_region_ID == USA_regID )
L123.out_EJ_USA_ownuse <- subset( L126.out_EJ_R_elecownuse_F_Yh, GCAM_region_ID == USA_regID )
L123.net_EJ_USA_ownuse <- data.frame(
      L123.in_EJ_USA_ownuse[ R_S_F ],
      L123.in_EJ_USA_ownuse[ X_historical_years ] - L123.out_EJ_USA_ownuse[ X_historical_years ] )

#Then build table with each state's share of the national ownuse. Note that this is assumed invariant over time.
L123.net_pct_state_USA_ownuse_elec <- data.frame( state = states, sector = "electricity ownuse", fuel = "electricity" )
L123.net_pct_state_USA_ownuse_elec[ X_historical_years ] <- EIA_elect_td_ownuse_prices$DirectUse_MWh[
      match( L123.net_pct_state_USA_ownuse_elec$state, EIA_elect_td_ownuse_prices$State ) ]
L123.net_pct_state_USA_ownuse_elec[ X_historical_years ] <- sweep( L123.net_pct_state_USA_ownuse_elec[ X_historical_years ], 2,
      colSums( L123.net_pct_state_USA_ownuse_elec[ X_historical_years ] ), "/" )

#Net own use = national total by each state's share
L123.net_EJ_state_ownuse_elec <- apportion_to_states(
      nation_data = L123.net_EJ_USA_ownuse,
      state_share_data = L123.net_pct_state_USA_ownuse_elec,
      match_vectors = "sector" )

#The input of the electricity_net_ownuse sector is equal to sum of all generation (industrial CHP + electric sector)
L123.out_EJ_state_elecind_F <- rbind( L123.out_EJ_state_elec_F, L132.out_EJ_state_indchp_F )
L123.in_EJ_state_ownuse_elec <- aggregate( L123.out_EJ_state_elecind_F[ X_historical_years ],
      by=as.list( L123.out_EJ_state_elecind_F[ state ] ), sum )

L123.in_EJ_state_ownuse_elec$sector <- "electricity ownuse"
L123.in_EJ_state_ownuse_elec$fuel <- "electricity"
L123.in_EJ_state_ownuse_elec <- L123.in_EJ_state_ownuse_elec[ c( state_S_F, X_historical_years ) ]

#Output of electricity_net_ownuse sector is equal to input minus ownuse "net" energy
L123.out_EJ_state_ownuse_elec <- L123.in_EJ_state_ownuse_elec
L123.out_EJ_state_ownuse_elec[ X_historical_years ] <-
      L123.in_EJ_state_ownuse_elec[ X_historical_years ] - L123.net_EJ_state_ownuse_elec[ X_historical_years ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L123.in_EJ_state_elec_F <- c( "Electricity sector energy consumption by state and fuel","Unit = EJ" )
comments.L123.out_EJ_state_elec_F <- c( "Electricity generation by state and fuel","Unit = EJ" )
comments.L123.in_EJ_state_ownuse_elec <- c( "Input to electricity net ownuse by state","Unit = EJ" )
comments.L123.out_EJ_state_ownuse_elec <- c( "Output of electricity net ownuse by state","Unit = EJ" )

#write tables as CSV files
writedata( L123.in_EJ_state_elec_F, domain="GCAMUSA_LEVEL1_DATA", fn="L123.in_EJ_state_elec_F", comments=comments.L123.in_EJ_state_elec_F )
writedata( L123.out_EJ_state_elec_F, domain="GCAMUSA_LEVEL1_DATA", fn="L123.out_EJ_state_elec_F", comments=comments.L123.out_EJ_state_elec_F )
writedata( L123.in_EJ_state_ownuse_elec, domain="GCAMUSA_LEVEL1_DATA", fn="L123.in_EJ_state_ownuse_elec", comments=comments.L123.in_EJ_state_ownuse_elec )
writedata( L123.out_EJ_state_ownuse_elec, domain="GCAMUSA_LEVEL1_DATA", fn="L123.out_EJ_state_ownuse_elec", comments=comments.L123.out_EJ_state_ownuse_elec )

# Every script should finish with this line
logstop()