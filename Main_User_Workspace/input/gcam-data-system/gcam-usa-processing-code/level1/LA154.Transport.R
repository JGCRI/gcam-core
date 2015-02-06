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
logstart( "LA154.Transport.R" )
printlog( "Transportation sector energy consumption by state and mode (not service)" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
trnUCD_EIA_mapping <- readdata( "GCAMUSA_MAPPINGS", "trnUCD_EIA_mapping" )
L154.in_EJ_R_trn_m_sz_tech_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L154.in_EJ_R_trn_m_sz_tech_F_Yh" )
L154.out_mpkm_R_trn_nonmotor_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L154.out_mpkm_R_trn_nonmotor_Yh" )
L100.Pop_thous_state <- readdata( "GCAMUSA_LEVEL1_DATA", "L100.Pop_thous_state" )
L101.inEIA_EJ_state_S_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L101.inEIA_EJ_state_S_F" )
L101.EIA_use_all_Bbtu <- readdata( "GCAMUSA_LEVEL1_DATA", "L101.EIA_use_all_Bbtu" )

# -----------------------------------------------------------------------------

# 2. Perform computations

# Calculate the state-wise percentages for each of EIA's sector/fuel combinations that is relevant for disaggregating
# nation-level transportation energy to the states
# first, subset only the usa, and only values that are > 0 in the historical periods
L154.in_EJ_USA_trn_m_sz_tech_F_Yh <- subset( L154.in_EJ_R_trn_m_sz_tech_F_Yh,
      GCAM_region_ID == USA_regID & rowSums( L154.in_EJ_R_trn_m_sz_tech_F_Yh[ X_historical_years ] ) !=0 )
L154.in_EJ_USA_trn_m_sz_tech_F_Yh[ EIA_S_F ] <- trnUCD_EIA_mapping[
      match( vecpaste( L154.in_EJ_USA_trn_m_sz_tech_F_Yh[ c( "mode", "fuel" ) ] ),
             vecpaste( trnUCD_EIA_mapping[ c( "mode", "fuel" ) ] ) ),
      EIA_S_F ]

#next, extract the relevant EIA sector & fuel combinations from the full state database
L154.EIA_trn_Bbtu_state <- subset( L101.EIA_use_all_Bbtu,
      vecpaste( L101.EIA_use_all_Bbtu[ EIA_S_F ] ) %in%
      vecpaste( L154.in_EJ_USA_trn_m_sz_tech_F_Yh[ EIA_S_F ] ) )

#Aggregate states to compute each state's share
L154.EIA_trn_Bbtu_USA <- aggregate( L154.EIA_trn_Bbtu_state[ X_historical_years ],
      by=as.list( L154.EIA_trn_Bbtu_state[ EIA_S_F ] ), sum )
L154.EIA_trn_share_state <- L154.EIA_trn_Bbtu_state
L154.EIA_trn_share_state[ X_historical_years ] <- L154.EIA_trn_Bbtu_state[ X_historical_years ] /
      L154.EIA_trn_Bbtu_USA[
         match( vecpaste( L154.EIA_trn_Bbtu_state[ EIA_S_F ] ),
                vecpaste( L154.EIA_trn_Bbtu_USA[ EIA_S_F ] ) ),
      X_historical_years ]
L154.EIA_trn_share_state[ X_historical_years ][ is.na( L154.EIA_trn_share_state[ X_historical_years ] ) ] <- 0

# Match these percentages into a table of all transportation technologies that will be written out
L154.pct_state_trn_m_sz_tech_F_Yh <- repeat_and_add_vector( L154.in_EJ_USA_trn_m_sz_tech_F_Yh, state, states )
L154.pct_state_trn_m_sz_tech_F_Yh[ X_historical_years ] <- L154.EIA_trn_share_state[
      match( vecpaste( L154.pct_state_trn_m_sz_tech_F_Yh[ c( state, EIA_S_F ) ] ),
             vecpaste( L154.EIA_trn_share_state[ c( state, EIA_S_F ) ] ) ),
      X_historical_years ]

#Now, the full USA tran UCD database can be apportioned to the states
L154.in_EJ_state_trn_m_sz_tech_F <- apportion_to_states(
      nation_data = L154.in_EJ_USA_trn_m_sz_tech_F_Yh,
      state_share_data = L154.pct_state_trn_m_sz_tech_F_Yh,
      match_vectors = UCD_techID )
L154.in_EJ_state_trn_m_sz_tech_F <- L154.in_EJ_state_trn_m_sz_tech_F[ c( state, UCD_techID, "fuel", X_historical_years ) ]

#As a final step, aggregate by fuel and name the sector
L154.in_EJ_state_trn_F <- aggregate( L154.in_EJ_state_trn_m_sz_tech_F[ X_historical_years ],
      by=as.list( L154.in_EJ_state_trn_m_sz_tech_F[ state_F ] ), sum )
L154.in_EJ_state_trn_F$sector <- "transportation"
L154.in_EJ_state_trn_F <- L154.in_EJ_state_trn_F[ c( state_S_F, X_historical_years ) ]

#Apportion non-motorized energy consumption to states on the basis of population
L154.Pop_state_shares <- data.frame( L100.Pop_thous_state[ state ],
      sweep( L100.Pop_thous_state[ X_historical_years ], 2, colSums( L100.Pop_thous_state[ X_historical_years ] ), "/" ) )
L154.Pop_state_shares_mode <- repeat_and_add_vector( L154.Pop_state_shares, "mode", unique( L154.out_mpkm_R_trn_nonmotor_Yh$mode ) )[
      c( state, "mode", X_historical_years ) ]
L154.out_mpkm_state_trn_nonmotor_Yh <- apportion_to_states(
      nation_data = subset( L154.out_mpkm_R_trn_nonmotor_Yh, GCAM_region_ID == USA_regID ),
      state_share_data = L154.Pop_state_shares_mode )

# -----------------------------------------------------------------------------
# 3. Output

#Add comments for each table
comments.L154.in_EJ_state_trn_m_sz_tech_F <- c( "Transportation energy consumption by state, sector, mode, size class, and fuel","Unit = EJ" )
comments.L154.out_mpkm_state_trn_nonmotor_Yh <- c( "Transportation non-motorized travel by mode and state","Unit = million person-km" )
comments.L154.in_EJ_state_trn_F <- c( "Transportation energy consumption by state and fuel","Unit = EJ" )

#write tables as CSV files
writedata( L154.in_EJ_state_trn_m_sz_tech_F, domain = "GCAMUSA_LEVEL1_DATA", fn="L154.in_EJ_state_trn_m_sz_tech_F", comments=comments.L154.in_EJ_state_trn_m_sz_tech_F )
writedata( L154.out_mpkm_state_trn_nonmotor_Yh, domain = "GCAMUSA_LEVEL1_DATA", fn="L154.out_mpkm_state_trn_nonmotor_Yh", comments=comments.L154.out_mpkm_state_trn_nonmotor_Yh )
writedata( L154.in_EJ_state_trn_F, domain = "GCAMUSA_LEVEL1_DATA", fn="L154.in_EJ_state_trn_F", comments=comments.L154.in_EJ_state_trn_F )

# Every script should finish with this line
logstop()