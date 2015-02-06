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
logstart( "LA132.Industry.R" )
printlog( "Refining sector inputs and outputs, industrial sector inputs, and industrial cogeneration output" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "GCAMUSA_ASSUMPTIONS", "A_GCAMUSA_data", extension = ".R" )
L101.inEIA_EJ_state_S_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L101.inEIA_EJ_state_S_F" )
L122.in_EJ_state_refining_F <- readdata( "GCAMUSA_LEVEL1_DATA", "L122.in_EJ_state_refining_F" )
L123.in_EJ_R_indchp_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L123.in_EJ_R_indchp_F_Yh" )
L123.out_EJ_R_indchp_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L123.out_EJ_R_indchp_F_Yh" )
printlog( "NOTE: using industrial energy after deductions for cement and fertilizer at the national level" )
L1322.in_EJ_R_indenergy_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1322.in_EJ_R_indenergy_F_Yh" )
L1322.in_EJ_R_indfeed_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1322.in_EJ_R_indfeed_F_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#INDUSTRIAL ENERGY USE - DEDUCTION FOR REFINING ENERGY USE
#Electricity and gas inputs to refineries are now deducted from industry.
#Subset industrial energy use from whole-US table
printlog( "Recalculating industrial energy use in SEDS database, deducting the energy that is used in refining" )
L132.in_EJ_state_ind_elecgas <- subset( L101.inEIA_EJ_state_S_F, sector == "industry" & fuel %in% c( "electricity", "gas" ) )

#Aggregate all refining techs' electricity and gas use by state
L132.in_EJ_state_refining_elecgas <- subset( L122.in_EJ_state_refining_F, fuel %in% c( "electricity", "gas" ) )
L132.in_EJ_state_refining_elecgas <- aggregate(
      L132.in_EJ_state_refining_elecgas[ X_historical_years ] * -1,
      by=as.list( L132.in_EJ_state_refining_elecgas[ state_F ] ), sum )

#Adjusted industrial fuel consumption = initial minus refinery fuel consumption
L132.in_EJ_state_ind_elecgas_adj <- subset( L101.inEIA_EJ_state_S_F, sector == "industry" & fuel %in% c( "electricity", "gas" ) )
L132.in_EJ_state_ind_elecgas_adj[ X_historical_years ] <- L132.in_EJ_state_ind_elecgas_adj[ X_historical_years ] +
      L132.in_EJ_state_refining_elecgas[
         match( vecpaste( L132.in_EJ_state_ind_elecgas_adj[ state_F ] ),
                vecpaste( L132.in_EJ_state_refining_elecgas[ state_F ] ) ),
         X_historical_years ]
#Replace any negative values with zeroes
L132.in_EJ_state_ind_elecgas_adj[ X_historical_years ][ L132.in_EJ_state_ind_elecgas_adj[ X_historical_years ] < 0 ] <- 0

#Put together a new table for industrial energy consumption. This will be used to calculate state-wise percentages of industrial energy use
# Only use the fuels that are considered in GCAM's "industrial energy use" sector
L132.in_EJ_state_indenergy_F_unscaled <- rbind(
      subset( L101.inEIA_EJ_state_S_F, sector == "industry" &
              fuel %!in% c( "electricity", "gas" ) &
              fuel %in% L1322.in_EJ_R_indenergy_F_Yh$fuel ),
      L132.in_EJ_state_ind_elecgas_adj )

#Aggregate by fuel to calculate each state's portion of the national industrial sector totals
L132.in_EJ_USA_ind_F_unscaled <- aggregate( L132.in_EJ_state_indenergy_F_unscaled[ X_historical_years ],
      by=as.list( L132.in_EJ_state_indenergy_F_unscaled[ S_F ] ), sum )

#Calculate the percentages of industrial energy use by state and fuel
L132.in_pct_state_ind_F <- L132.in_EJ_state_indenergy_F_unscaled[ state_S_F ]
L132.in_pct_state_ind_F[ X_historical_years ] <- L132.in_EJ_state_indenergy_F_unscaled[ X_historical_years ] /
      L132.in_EJ_USA_ind_F_unscaled[
         match( L132.in_EJ_state_indenergy_F_unscaled$fuel,
                L132.in_EJ_USA_ind_F_unscaled$fuel ),
      X_historical_years ]

#Apportion nation-level industrial energy consumption to states - NON-COGEN
printlog( "Industrial sector non-cogen input energy by state and fuel" )
L132.in_EJ_state_indnochp_F <- apportion_to_states(
      nation_data = subset( L1322.in_EJ_R_indenergy_F_Yh, GCAM_region_ID == USA_regID ),
      state_share_data = L132.in_pct_state_ind_F,
      match_vectors = "fuel" )
L132.in_EJ_state_indnochp_F$sector <- "industry_energy"

#Apportion nation-level industrial energy consumption to states - COGEN
printlog( "Industrial sector cogeneration input energy by state and fuel" )
#We only want fuels that are inputs to cogen systems, i.e. not electricity
L132.in_EJ_state_indchp_F <- apportion_to_states(
      nation_data = subset( L123.in_EJ_R_indchp_F_Yh, GCAM_region_ID == USA_regID ),
      state_share_data = subset( L132.in_pct_state_ind_F, fuel %in% L123.in_EJ_R_indchp_F_Yh$fuel ),
      match_vectors = "fuel" )
L132.in_EJ_state_indchp_F$sector <- "chp_elec"

#Apportion nation-level industrial cogen output to states
printlog( "Industrial sector electricity cogeneration by state" )
L132.out_EJ_state_indchp_F <- apportion_to_states(
      nation_data = subset( L123.out_EJ_R_indchp_F_Yh, GCAM_region_ID == USA_regID ),
      state_share_data = subset( L132.in_pct_state_ind_F, fuel %in% L123.out_EJ_R_indchp_F_Yh$fuel ),
      match_vectors = "fuel" )
L132.out_EJ_state_indchp_F$sector <- "chp_elec"

#INDUSTRIAL FEEDSTOCKS
#Only considering gas and liquid fuels; each will be treated separately.
# Liquid fuels apportioned to states by sum of petchem feed and asphalt
# Gas apportioned to states by petchem feed only
printlog( "Petroleum feedstocks by state" )
printlog( "NOTE: using petrochemical feedstocks and asphalt/road oil as basis for disaggregating petroleum feedstocks to states" )
#Split state-level data into feedstocks only, according to names of fuel inputs
L132.in_EJ_state_indfeed_liq_unscaled <- subset( L101.inEIA_EJ_state_S_F,
      sector == "industry" &
      fuel %in% c( "refined liquids (const feed)", "refined liquids (petchem feed)" ) )
L132.in_EJ_state_indfeed_liq_unscaled <- aggregate( L132.in_EJ_state_indfeed_liq_unscaled[ X_historical_years ],
      by=as.list( L132.in_EJ_state_indfeed_liq_unscaled[ state_S ] ), sum )
L132.in_EJ_state_indfeed_liq_unscaled$sector <- "industry_feedstocks"
L132.in_EJ_state_indfeed_liq_unscaled$fuel <- "refined liquids"

#Calculate the percentage-wise allocations, by state
L132.pct_state_indfeed_liq <- L132.in_EJ_state_indfeed_liq_unscaled[ state_S_F ]
L132.pct_state_indfeed_liq[ X_historical_years ] <- sweep(
      L132.in_EJ_state_indfeed_liq_unscaled[ X_historical_years ], 2,
      colSums( L132.in_EJ_state_indfeed_liq_unscaled[ X_historical_years ] ), "/" )

printlog( "Natural gas feedstocks by state" )
printlog( "NOTE: using petrochemical feedstocks as basis for disaggregating natural gas feedstocks to states" )
L132.in_EJ_state_indfeed_gas_unscaled <- subset( L101.inEIA_EJ_state_S_F,
      sector == "industry" &
      fuel == "refined liquids (petchem feed)" )
L132.in_EJ_state_indfeed_gas_unscaled$sector <- "industry_feedstocks"
L132.in_EJ_state_indfeed_gas_unscaled$fuel <- "gas"
L132.pct_state_indfeed_gas <- L132.in_EJ_state_indfeed_gas_unscaled[ state_S_F ]
L132.pct_state_indfeed_gas[ X_historical_years ] <- sweep(
      L132.in_EJ_state_indfeed_gas_unscaled[ X_historical_years ], 2,
      colSums( L132.in_EJ_state_indfeed_gas_unscaled[ X_historical_years ] ), "/" )

#Repeat this for coal as well; won't matter b/c coal feedstocks are 0 in all years in the USA
L132.pct_state_indfeed_coal <- L132.pct_state_indfeed_gas
L132.pct_state_indfeed_coal$fuel <- "coal"
L132.pct_state_indfeed_F <- rbind( L132.pct_state_indfeed_liq, L132.pct_state_indfeed_gas, L132.pct_state_indfeed_coal )

#Apportion to the states
L132.in_EJ_state_indfeed_F <- apportion_to_states(
      nation_data = subset( L1322.in_EJ_R_indfeed_F_Yh, GCAM_region_ID == USA_regID ),
      state_share_data = L132.pct_state_indfeed_F,
      match_vectors = "fuel" )

# -----------------------------------------------------------------------------

# 3. Output
#Add comments for each table
comments.L132.in_EJ_state_indnochp_F <- c( "Non-cogen industrial energy consumption by state and fuel","Unit = EJ" )
comments.L132.in_EJ_state_indchp_F <- c( "Industrial cogen energy consumption by state and fuel","Unit = EJ" )
comments.L132.out_EJ_state_indchp_F <- c( "Industrial electricity cogeneration by state and fuel","Unit = EJ" )
comments.L132.in_EJ_state_indfeed_F <- c( "Industrial feedstocks by state and fuel","Unit = EJ" )

#write tables as CSV files
writedata( L132.in_EJ_state_indnochp_F, domain="GCAMUSA_LEVEL1_DATA", fn="L132.in_EJ_state_indnochp_F", comments=comments.L132.in_EJ_state_indnochp_F )
writedata( L132.in_EJ_state_indchp_F, domain="GCAMUSA_LEVEL1_DATA", fn="L132.in_EJ_state_indchp_F", comments=comments.L132.in_EJ_state_indchp_F )
writedata( L132.out_EJ_state_indchp_F, domain="GCAMUSA_LEVEL1_DATA", fn="L132.out_EJ_state_indchp_F", comments=comments.L132.out_EJ_state_indchp_F )
writedata( L132.in_EJ_state_indfeed_F, domain="GCAMUSA_LEVEL1_DATA", fn="L132.in_EJ_state_indfeed_F", comments=comments.L132.in_EJ_state_indfeed_F )

# Every script should finish with this line
logstop()
