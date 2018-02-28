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
logstart( "LB126.Gas_ElecTD.R" )
printlog( "Gas processing, gas pipeline, and electricity T&D" )

# -----------------------------------------------------------------------------
# 1. Read files

sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
L122.in_EJ_R_gasproc_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L122.in_EJ_R_gasproc_F_Yh" )
L122.out_EJ_R_gasproc_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L122.out_EJ_R_gasproc_F_Yh" )
L126.in_EJ_R_gaspipe_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L126.in_EJ_R_gaspipe_F_Yh" )
L126.out_EJ_R_gaspipe_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L126.out_EJ_R_gaspipe_F_Yh" )
L126.IO_R_electd_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L126.IO_R_electd_F_Yh" )
L101.EIA_use_all_Bbtu <- readdata( "GCAMUSA_LEVEL1_DATA", "L101.EIA_use_all_Bbtu" )
L101.inEIA_EJ_state_S_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L101.inEIA_EJ_state_S_F" )
L122.in_EJ_state_refining_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L122.in_EJ_state_refining_F" )
L123.out_EJ_state_elec_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L123.out_EJ_state_elec_F" )
L123.out_EJ_state_ownuse_elec <- readdata( "GCAMUSA_LEVEL1_DATA", "L123.out_EJ_state_ownuse_elec" )
L132.in_EJ_state_indchp_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L132.in_EJ_state_indchp_F" )
L132.in_EJ_state_indfeed_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L132.in_EJ_state_indfeed_F" )
L132.in_EJ_state_indnochp_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L132.in_EJ_state_indnochp_F" )
L1321.in_EJ_state_cement_F_Y <- readdata( "GCAMUSA_LEVEL1_DATA", "L1321.in_EJ_state_cement_F_Y" )
L1322.in_EJ_state_Fert_Yh <- readdata( "GCAMUSA_LEVEL1_DATA", "L1322.in_EJ_state_Fert_Yh" )
L142.in_EJ_state_bld_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L142.in_EJ_state_bld_F" )
L154.in_EJ_state_trn_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L154.in_EJ_state_trn_F" )

# -----------------------------------------------------------------------------
# 2. Perform computations

#PIPELINE ENERGY USE (NET)

printlog( "Deriving gas pipeline energy use (net) from national estimate and each state's share" )

L126.net_EJ_USA_pipeline <- subset( L126.in_EJ_R_gaspipe_F_Yh, GCAM_region_ID == USA_regID )

L126.net_EJ_USA_pipeline[ X_historical_years ] <-
      L126.in_EJ_R_gaspipe_F_Yh[ L126.in_EJ_R_gaspipe_F_Yh$GCAM_region_ID == USA_regID, X_historical_years ] -
      L126.out_EJ_R_gaspipe_F_Yh[ L126.out_EJ_R_gaspipe_F_Yh$GCAM_region_ID == USA_regID, X_historical_years ]

#Calculate state shares of pipeline energy use

L126.pct_state_pipeline_gas <- subset( L101.inEIA_EJ_state_S_F, sector == "gas pipeline" & fuel == "gas" )
L126.pct_state_pipeline_gas$sector <- "gas pipeline"
L126.pct_state_pipeline_gas[ X_historical_years ] <- sweep( L126.pct_state_pipeline_gas[ X_historical_years], 2,
      colSums( L126.pct_state_pipeline_gas[ X_historical_years ] ), "/" )

# Apportion to states
L126.net_EJ_state_pipeline_gas <- apportion_to_states(
      nation_data = L126.net_EJ_USA_pipeline,
      state_share_data = L126.pct_state_pipeline_gas,
      match_vectors = S_F )

#PIPELINE OUTPUT AND INPUT

#Pipeline energy use is represented in GCAM as a coefficient on regional fuel consumption: (consumption + pipeline energy use) / consumption

#Compile each state's total gas consumption by all sectors: elec, refining, bld, ind, trn. This is equal to pipeline "output"

printlog( "Computing total final energy consumption by all sectors in each state" )

L126.in_EJ_state_S_F <- rbind(
      L122.in_EJ_state_refining_F,
      L123.out_EJ_state_elec_F,
      L132.in_EJ_state_indchp_F,
      L132.in_EJ_state_indfeed_F,
      L132.in_EJ_state_indnochp_F,
      L1321.in_EJ_state_cement_F_Y,
      L1322.in_EJ_state_Fert_Yh,
      L142.in_EJ_state_bld_F,
      L154.in_EJ_state_trn_F )

L126.in_EJ_state_F <- aggregate( L126.in_EJ_state_S_F[ X_historical_years ],

      by=as.list( L126.in_EJ_state_S_F[ state_F ]), sum )

L126.in_EJ_state_gas <- subset( L126.in_EJ_state_F, fuel == "gas" )
#Specify the sector, and rename this table as pipeline output

L126.out_EJ_state_pipeline_gas <- L126.in_EJ_state_gas

L126.out_EJ_state_pipeline_gas$sector <- "gas pipeline"

L126.out_EJ_state_pipeline_gas <- L126.out_EJ_state_pipeline_gas[ c( state_S_F, X_historical_years ) ]

#Add in pipeline energy use. This the pipeline sector's input.

printlog( "Deriving inputs to gas pipelines as output plus pipeline energy use" )

L126.in_EJ_state_pipeline_gas <- data.frame(
      L126.out_EJ_state_pipeline_gas[ state_S_F ],

      L126.out_EJ_state_pipeline_gas[ X_historical_years ] + L126.net_EJ_state_pipeline_gas[ X_historical_years ] )

#GAS PROCESSING

printlog( "Gas processing by state and fuel type" )

printlog( "Coal gasification inputs and outputs" )

printlog( "NOTE: Coal gasification (town gas) is disaggregated to states on the basis of industrial coal consumption" )

L126.pct_state_gasproc_coal <- subset( L101.inEIA_EJ_state_S_F, sector == "industry" & fuel == "coal" )
L126.pct_state_gasproc_coal$sector <- "gas processing"
L126.pct_state_gasproc_coal[ X_historical_years ] <- sweep( L126.pct_state_gasproc_coal[ X_historical_years], 2,
      colSums( L126.pct_state_gasproc_coal[ X_historical_years ] ), "/" )

#Apportion nation-level data to states
L126.in_EJ_state_gasproc_coal <- apportion_to_states(
      nation_data = subset( L122.in_EJ_R_gasproc_F_Yh, GCAM_region_ID == USA_regID ),
      state_share_data = L126.pct_state_gasproc_coal,
      match_vectors = S_F )
L126.out_EJ_state_gasproc_coal <- apportion_to_states(
      nation_data = subset( L122.out_EJ_R_gasproc_F_Yh, GCAM_region_ID == USA_regID ),
      state_share_data = L126.pct_state_gasproc_coal,
      match_vectors = S_F )
      
printlog( "Biomass gasification inputs and outputs" )
printlog( "NOTE: Biomass gasification (biogas) is disaggregated to states on the basis of electric sector waste biomass consumption" )

#Subset the waste used by the electric sector in the specified base years, from the EIA SEDS table
L126.in_Bbtu_state_elec_ws <- subset( L101.EIA_use_all_Bbtu, EIA_sector == "EI" & EIA_fuel == "WS", c( "state", X_historical_years ) )

#Calculate each state's share

L126.in_pct_state_gasproc_bio <- data.frame(
      state = L126.in_Bbtu_state_elec_ws$state,
      sector = "gas processing",
      fuel = "biomass",

      sweep( L126.in_Bbtu_state_elec_ws[ X_historical_years ], 2, colSums( L126.in_Bbtu_state_elec_ws[ X_historical_years ] ), "/", ) )

#Multiply national totals by state shares to get each state's scaled biomass gasification outputs and inputs

#Output

L126.out_EJ_state_gasproc_bio <- apportion_to_states(
      nation_data = subset( L122.out_EJ_R_gasproc_F_Yh, GCAM_region_ID == USA_regID & fuel == "biomass" ),
      state_share_data = L126.in_pct_state_gasproc_bio,
      match_vectors = "fuel" )

#Input

L126.in_EJ_state_gasproc_bio <- apportion_to_states(
      nation_data = subset( L122.in_EJ_R_gasproc_F_Yh, GCAM_region_ID == USA_regID & fuel == "biomass" ),
      state_share_data = L126.in_pct_state_gasproc_bio,
      match_vectors = "fuel" )
printlog( "The remainder of each state's consumption of gas is assigned to the natural gas technology" )

L126.out_EJ_state_gasproc_gas <- data.frame(
      state = states,
      sector = "gas processing",
      fuel = "natural gas",

      L126.in_EJ_state_pipeline_gas[ X_historical_years ] - L126.out_EJ_state_gasproc_coal[ X_historical_years ] - L126.out_EJ_state_gasproc_bio[ X_historical_years ] )

L126.in_EJ_state_gasproc_gas <- L126.out_EJ_state_gasproc_gas

#Rbind the input and output tables of gas processing technologies, and write them out

L126.out_EJ_state_gasproc_F <- rbind( L126.out_EJ_state_gasproc_gas, L126.out_EJ_state_gasproc_bio, L126.out_EJ_state_gasproc_coal)

L126.in_EJ_state_gasproc_F <- rbind( L126.in_EJ_state_gasproc_gas, L126.in_EJ_state_gasproc_bio, L126.in_EJ_state_gasproc_coal)

#ELECTRICITY TRANSMISSION AND DISTRIBUTION

printlog( "Electricity transmission and distribution by state" )

#Compile each state's total elec consumption: refining, bld, ind, trn.

L126.in_EJ_state_elec <- subset( L126.in_EJ_state_F, fuel == "electricity" )

#Specify the sector, rename this table as elect_TD output, add ID vector, and write it out

printlog( "Deriving electricity T&D output as the sum of all tracked demands of electricity" )

L126.out_EJ_state_td_elec <- L126.in_EJ_state_elec

L126.out_EJ_state_td_elec$sector <- "elect_td"

L126.out_EJ_state_td_elec <- L126.out_EJ_state_td_elec[ c( state_S_F, X_historical_years ) ]

printlog( "Assigning all states the national average T&D coefficient" )

# Note: this function here is not actually apportioning a national total using a table of percentages
L126.in_EJ_state_td_elec <- apportion_to_states(
      nation_data = subset( L126.IO_R_electd_F_Yh, GCAM_region_ID == USA_regID ),
      state_share_data = L126.out_EJ_state_td_elec,
      match_vectors = "fuel" )
# -----------------------------------------------------------------------------

# 3. Output

#Add comments for each table

comments.L126.out_EJ_state_pipeline_gas <- c( "Output of gas pipeline sector by state","Unit = EJ" )

comments.L126.in_EJ_state_pipeline_gas <- c( "Input to gas pipeline sector by state","Unit = EJ" )

comments.L126.out_EJ_state_gasproc_F <- c( "Output of gas processing sector by state and technology","Unit = EJ" )

comments.L126.in_EJ_state_gasproc_F <- c( "Inputs to gas processing sector by state and technology","Unit = EJ" )

comments.L126.out_EJ_state_td_elec <- c( "Output of electricity T&D sector by state","Unit = EJ" )

comments.L126.in_EJ_state_td_elec <- c( "Input to electricity T&D sector by state","Unit = EJ" )

#write tables as CSV files

writedata( L126.out_EJ_state_pipeline_gas, domain="GCAMUSA_LEVEL1_DATA", fn="L126.out_EJ_state_pipeline_gas", comments=comments.L126.out_EJ_state_pipeline_gas )

writedata( L126.in_EJ_state_pipeline_gas, domain="GCAMUSA_LEVEL1_DATA", fn="L126.in_EJ_state_pipeline_gas", comments=comments.L126.in_EJ_state_pipeline_gas )

writedata( L126.out_EJ_state_gasproc_F, domain="GCAMUSA_LEVEL1_DATA", fn="L126.out_EJ_state_gasproc_F", comments=comments.L126.out_EJ_state_gasproc_F )

writedata( L126.in_EJ_state_gasproc_F, domain="GCAMUSA_LEVEL1_DATA", fn="L126.in_EJ_state_gasproc_F", comments=comments.L126.in_EJ_state_gasproc_F )

writedata( L126.out_EJ_state_td_elec, domain="GCAMUSA_LEVEL1_DATA", fn="L126.out_EJ_state_td_elec", comments=comments.L126.out_EJ_state_td_elec )

writedata( L126.in_EJ_state_td_elec, domain="GCAMUSA_LEVEL1_DATA", fn="L126.in_EJ_state_td_elec", comments=comments.L126.in_EJ_state_td_elec )

# Every script should finish with this line

logstop()