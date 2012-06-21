# L106_Gas_ElecTD.R
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L106_Gas_ElecTD.R" )
printlog( "Gas processing, gas pipeline, and electricity transmission and distribution sectors" )

# -----------------------------------------------------------------------------
# 1. Read files

USA_gas_pipeline_in <- readdata( "USA_gas_pipeline_in" )
USA_gas_pipeline_out <- readdata( "USA_gas_pipeline_out" )
USA_gas_processing_in <- readdata( "USA_gas_processing_in" )
USA_gas_processing_out <- readdata( "USA_gas_processing_out" )
USA_elec_td_net <- readdata( "USA_elec_td_net" )
L101_EIA_use_all_Bbtu <- readdata( "L101_EIA_use_all_Bbtu" )
L101_inEIA_EJ_state_S_F <- readdata( "L101_inEIA_EJ_state_S_F" )
L102_in_EJ_state_refining_F <- readdata( "L102_in_EJ_state_refining_F" )
L102_in_EJ_state_indchp_F <- readdata( "L102_in_EJ_state_indchp_F" )
L102_in_EJ_state_indfeed_F <- readdata( "L102_in_EJ_state_indfeed_F" )
L102_in_EJ_state_indnochp_F <- readdata( "L102_in_EJ_state_indnochp_F" )
L103_in_EJ_state_elec_F <- readdata( "L103_in_EJ_state_elec_F" )
L104_in_EJ_state_bld_F <- readdata( "L104_in_EJ_state_bld_F" )
L105_in_EJ_state_trn_F <- readdata( "L105_in_EJ_state_trn_F" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#PIPELINE ENERGY USE (NET)
printlog( "Deriving gas pipeline energy use (net) from national estimate and each state's share" )
L106_net_EJ_USA_pipeline <- USA_gas_pipeline_in
L106_net_EJ_USA_pipeline[ X_base_years ] <- USA_gas_pipeline_in[ X_base_years ] - USA_gas_pipeline_out[ X_base_years ]

#Calculate state shares of pipeline energy use
L106_net_EJ_state_pipeline_unscaled <- L101_inEIA_EJ_state_S_F[ L101_inEIA_EJ_state_S_F$GCAM_sector == "gas pipeline", ]
L106_net_pct_state_pipeline <- data.frame( state = states, GCAM_sector = "gas pipeline",
      sweep( L106_net_EJ_state_pipeline_unscaled[ X_base_years ], 2, colSums( L106_net_EJ_state_pipeline_unscaled[ X_base_years ] ), "/" ) )

#Multiply by national total to get each state's scaled pipeline energy use
L106_net_EJ_USA_pipeline_repstate <- L106_net_EJ_USA_pipeline[ rep( 1, times = length( states ) ), ]
L106_net_EJ_state_pipeline_gas <- data.frame( state = states,
      GCAM_sector = "gas pipeline", GCAM_fuel = "gas",
      L106_net_EJ_USA_pipeline_repstate[ X_base_years ] * L106_net_pct_state_pipeline[ X_base_years ] )

#PIPELINE OUTPUT AND INPUT
#Pipeline energy use is represented in GCAM as a coefficient on regional fuel consumption: (consumption + pipeline energy use) / consumption
#Compile each state's total gas consumption: elec, refining, bld, ind, trn. This is equal to pipeline "output"
printlog( "Deriving pipeline output in each state as the sum of all tracked demands of natural gas" )
L106_in_EJ_state_refining_gas <- L102_in_EJ_state_refining_F[ L102_in_EJ_state_refining_F$GCAM_fuel == "wholesale gas", ]
L106_in_EJ_state_refining_gas$GCAM_fuel <- "gas"
L106_in_EJ_state_indchp_gas <- L102_in_EJ_state_indchp_F[ L102_in_EJ_state_indchp_F$GCAM_fuel == "gas", ]
L106_in_EJ_state_indfeed_gas <- L102_in_EJ_state_indfeed_F[ L102_in_EJ_state_indfeed_F$GCAM_fuel == "gas", ]
L106_in_EJ_state_indnochp_gas <- L102_in_EJ_state_indnochp_F[ L102_in_EJ_state_indnochp_F$GCAM_fuel == "gas", ]
L106_in_EJ_state_elec_gas <- L103_in_EJ_state_elec_F[ L103_in_EJ_state_elec_F$GCAM_fuel == "gas", ]
L106_in_EJ_state_bld_gas <- L104_in_EJ_state_bld_F[ L104_in_EJ_state_bld_F$GCAM_fuel == "gas", ]
L106_in_EJ_state_trn_gas <- L105_in_EJ_state_trn_F[ L105_in_EJ_state_trn_F$GCAM_fuel == "gas", ]

#Put these tables into a single dataframe and aggregate
L106_in_EJ_state_S_gas <- rbind( L106_in_EJ_state_refining_gas, L106_in_EJ_state_indchp_gas, L106_in_EJ_state_indfeed_gas,
      L106_in_EJ_state_indnochp_gas, L106_in_EJ_state_elec_gas, L106_in_EJ_state_bld_gas, L106_in_EJ_state_trn_gas )
L106_in_EJ_state_total_gas <- aggregate( L106_in_EJ_state_S_gas[ X_base_years ],
      list( state = L106_in_EJ_state_S_gas$state, GCAM_fuel = L106_in_EJ_state_S_gas$GCAM_fuel), sum )

#Specify the sector, and rename this table as pipeline output
L106_out_EJ_state_pipeline_gas <- L106_in_EJ_state_total_gas
L106_out_EJ_state_pipeline_gas$GCAM_sector <- "gas pipeline"
L106_out_EJ_state_pipeline_gas <- L106_out_EJ_state_pipeline_gas[ c( state_S_F, X_base_years ) ]

#Add in pipeline energy use. This the pipeline sector's input.
printlog( "Deriving inputs to gas pipelines as output plus pipeline energy use" )
L106_in_EJ_state_pipeline_gas <- data.frame( L106_out_EJ_state_pipeline_gas[ state_S_F ],
      L106_out_EJ_state_pipeline_gas[ X_base_years ] + L106_net_EJ_state_pipeline_gas[ X_base_years ] )

#GAS PROCESSING
printlog( "Gas processing by state and technology" )
#Import data for gas production and fuel input by technology
L106_out_EJ_USA_gasproc_coal <- USA_gas_processing_out[ USA_gas_processing_out$subsector == "coal gasification", ]
L106_in_EJ_USA_gasproc_coal <- USA_gas_processing_in[ USA_gas_processing_in$subsector == "coal gasification", ]
L106_out_EJ_USA_gasproc_bio <- USA_gas_processing_out[ USA_gas_processing_out$subsector == "biomass gasification", ]
L106_in_EJ_USA_gasproc_bio <- USA_gas_processing_in[ USA_gas_processing_in$subsector == "biomass gasification", ]

printlog( "Coal gasification inputs and outputs" )
printlog( "NOTE: Coal gasification (coke oven gas) is disaggregated to states on the basis of industrial coal consumption" )
#Subset the coal used by the industrial sector in the specified base years, from the EIA SEDS table
L106_in_Bbtu_state_ind_coal <- L101_EIA_use_all_Bbtu[ L101_EIA_use_all_Bbtu$eia_sector == "IC" & L101_EIA_use_all_Bbtu$eia_fuel == "CL", c( "state", X_base_years ) ]

#Calculate each state's share
L106_in_pct_state_gasproc_coal <- data.frame( state = states, GCAM_sector = "coal gasification",
      sweep( L106_in_Bbtu_state_ind_coal[ X_base_years ], 2, colSums( L106_in_Bbtu_state_ind_coal[ X_base_years ] ), "/" ) )

#Multiply by national total to get each state's scaled coal gasification
#Output
L106_out_EJ_USA_gasproc_coal_repstate <- L106_out_EJ_USA_gasproc_coal[ rep( 1, times = length( states ) ), ]
L106_out_EJ_state_gasproc_coal <- data.frame( state = states,
      GCAM_sector = "coal gasification", GCAM_fuel = "gas",
      L106_out_EJ_USA_gasproc_coal_repstate[ X_base_years ] * L106_in_pct_state_gasproc_coal[ X_base_years ] )

#Input
L106_in_EJ_USA_gasproc_coal_repstate <- L106_in_EJ_USA_gasproc_coal[ rep( 1, times = length( states ) ), ]
L106_in_EJ_state_gasproc_coal <- data.frame( state = states,
      GCAM_sector = "coal gasification", GCAM_fuel = "coal",
      L106_in_EJ_USA_gasproc_coal_repstate[ X_base_years ] * L106_in_pct_state_gasproc_coal[ X_base_years ] )

printlog( "Biomass gasification inputs and outputs" )
printlog( "NOTE: Biomass gasification (biogas) is disaggregated to states on the basis of electric sector waste biomass consumption" )
#Subset the waste used by the electric sector in the specified base years, from the EIA SEDS table
L106_in_Bbtu_state_elec_ws <- L101_EIA_use_all_Bbtu[ L101_EIA_use_all_Bbtu$eia_sector == "EI" & L101_EIA_use_all_Bbtu$eia_fuel == "WS", c( "state", X_base_years ) ]

#Calculate each state's share
L106_in_pct_state_gasproc_bio <- data.frame( state = states, GCAM_sector = "biomass gasification",
      sweep( L106_in_Bbtu_state_elec_ws[ X_base_years ], 2, colSums( L106_in_Bbtu_state_elec_ws[ X_base_years ] ), "/", ) )

#Multiply by national total to get each state's scaled biomass gasification
#Output
L106_out_EJ_USA_gasproc_bio_repstate <- L106_out_EJ_USA_gasproc_bio[ rep( 1, times = length( states ) ), ]
L106_out_EJ_state_gasproc_bio <- data.frame( state = states,
      GCAM_sector = "biomass gasification", GCAM_fuel = "gas",
      L106_out_EJ_USA_gasproc_bio_repstate[ X_base_years ] * L106_in_pct_state_gasproc_bio[ X_base_years ] )

#Input
L106_in_EJ_USA_gasproc_bio_repstate <- L106_in_EJ_USA_gasproc_bio[ rep( 1, times = length( states ) ), ]
L106_in_EJ_state_gasproc_bio <- data.frame( state = states,
      GCAM_sector = "biomass gasification", GCAM_fuel = "biomass",
      L106_in_EJ_USA_gasproc_bio_repstate[ X_base_years ] * L106_in_pct_state_gasproc_bio[ X_base_years ] )

printlog( "NOTE: The remainder of each state's domestic supply of gas is assigned to the natural gas technology" )
L106_out_EJ_state_gasproc_gas <- data.frame( state = states, GCAM_sector = "natural gas", GCAM_fuel = "gas",
      L106_in_EJ_state_pipeline_gas[ X_base_years ] - L106_out_EJ_state_gasproc_coal[ X_base_years ] - L106_out_EJ_state_gasproc_bio[ X_base_years ] )
L106_in_EJ_state_gasproc_gas <- data.frame( state = states, GCAM_sector = "natural gas", GCAM_fuel = "natural gas",
      L106_out_EJ_state_gasproc_gas[ X_base_years ] )

#Rbind the input and output tables of gas processing technologies, and write them out
L106_out_EJ_state_gasproc_F <- rbind( L106_out_EJ_state_gasproc_gas, L106_out_EJ_state_gasproc_bio, L106_out_EJ_state_gasproc_coal)
L106_in_EJ_state_gasproc_F <- rbind( L106_in_EJ_state_gasproc_gas, L106_in_EJ_state_gasproc_bio, L106_in_EJ_state_gasproc_coal)

#ELECTRICITY TRANSMISSION AND DISTRIBUTION
printlog( "Electricity transmission and distribution by state" )
#Compile each state's total elec consumption: refining, bld, ind, trn.
L106_in_EJ_state_refining_elec <- L102_in_EJ_state_refining_F[ L102_in_EJ_state_refining_F$GCAM_fuel == "elect_td_ind", ]
L106_in_EJ_state_refining_elec$GCAM_fuel <- "electricity"
L106_in_EJ_state_indnochp_elec <- L102_in_EJ_state_indnochp_F[ L102_in_EJ_state_indnochp_F$GCAM_fuel == "electricity", ]
L106_in_EJ_state_bld_elec <- L104_in_EJ_state_bld_F[ L104_in_EJ_state_bld_F$GCAM_fuel == "electricity", ]
L106_in_EJ_state_trn_elec <- L105_in_EJ_state_trn_F[ L105_in_EJ_state_trn_F$GCAM_fuel == "electricity", ]

#Put these tables into a single dataframe and aggregate
L106_in_EJ_state_S_elec <- rbind( L106_in_EJ_state_refining_elec, L106_in_EJ_state_indnochp_elec, L106_in_EJ_state_bld_elec, L106_in_EJ_state_trn_elec )
L106_in_EJ_state_total_elec <- aggregate( L106_in_EJ_state_S_elec[ X_base_years ],
      list( state = L106_in_EJ_state_S_elec$state, GCAM_fuel = L106_in_EJ_state_S_elec$GCAM_fuel ), sum )

#Specify the sector, rename this table as elect_TD output, add ID vector, and write it out
printlog( "Deriving electricity T&D output as the sum of all tracked demands of electricity" )
L106_out_EJ_state_td_elec <- L106_in_EJ_state_total_elec
L106_out_EJ_state_td_elec$GCAM_sector <- "elect_td"
L106_out_EJ_state_td_elec <- L106_out_EJ_state_td_elec[ c( state_S_F, X_base_years ) ]

printlog( "Deriving electricity T&D losses in each state as the national estimate multiplied by each state's share of electricity demand" )
printlog( "NOTE: by this method, all states are assumed to have equal T&D coefficients" )
#Calculate state shares of total electricity demand
L106_pct_state_td_elec <- data.frame( L106_out_EJ_state_td_elec[ state_S_F ],
      sweep( L106_out_EJ_state_td_elec[ X_base_years ], 2, colSums( L106_out_EJ_state_td_elec[ X_base_years ] ), "/", ) )

#Multiply nation-level T&D losses by these shares to calculate each state's T&D losses
L106_net_EJ_USA_td_elec_repstate <- USA_elec_td_net[ rep( 1, times = length( states ) ), ]
L106_net_EJ_state_td_elec <- data.frame( L106_pct_state_td_elec[ state_S_F ],
      L106_pct_state_td_elec[ X_base_years ] * L106_net_EJ_USA_td_elec_repstate[ X_base_years ] )

#Add the net losses to the delivered electricity (output) to calculate the input to the T&D sector
L106_in_EJ_state_td_elec <- data.frame( L106_out_EJ_state_td_elec[ state_S_F ],
      L106_out_EJ_state_td_elec[ X_base_years ] + L106_net_EJ_state_td_elec[ X_base_years ] )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L106_out_EJ_state_pipeline_gas <- c( "Output of gas pipeline sector by state","Unit = EJ" )
comments.L106_in_EJ_state_pipeline_gas <- c( "Input to gas pipeline sector by state","Unit = EJ" )
comments.L106_out_EJ_state_gasproc_F <- c( "Output of gas processing sector by state and technology","Unit = EJ" )
comments.L106_in_EJ_state_gasproc_F <- c( "Inputs to gas processing sector by state and technology","Unit = EJ" )
comments.L106_out_EJ_state_td_elec <- c( "Output of electricity T&D sector by state","Unit = EJ" )
comments.L106_in_EJ_state_td_elec <- c( "Input to electricity T&D sector by state","Unit = EJ" )

#write tables as CSV files
writedata( L106_out_EJ_state_pipeline_gas,fn="L106_out_EJ_state_pipeline_gas", comments=comments.L106_out_EJ_state_pipeline_gas )
writedata( L106_in_EJ_state_pipeline_gas,fn="L106_in_EJ_state_pipeline_gas", comments=comments.L106_in_EJ_state_pipeline_gas )
writedata( L106_out_EJ_state_gasproc_F,fn="L106_out_EJ_state_gasproc_F", comments=comments.L106_out_EJ_state_gasproc_F )
writedata( L106_in_EJ_state_gasproc_F,fn="L106_in_EJ_state_gasproc_F", comments=comments.L106_in_EJ_state_gasproc_F )
writedata( L106_out_EJ_state_td_elec,fn="L106_out_EJ_state_td_elec", comments=comments.L106_out_EJ_state_td_elec )
writedata( L106_in_EJ_state_td_elec,fn="L106_in_EJ_state_td_elec", comments=comments.L106_in_EJ_state_td_elec )

# Every script should finish with this line
logstop()