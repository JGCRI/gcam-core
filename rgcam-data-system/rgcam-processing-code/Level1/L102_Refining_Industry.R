# L102_Refining_Industry.R
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L102_Refining_Industry.R" )
printlog( "Refining sector inputs and outputs, industrial sector inputs, and industrial cogeneration output" )

# -----------------------------------------------------------------------------
# 1. Read files

L101_inEIA_EJ_state_S_F <- readdata( "L101_inEIA_EJ_state_S_F" )
USA_refining_in <- readdata( "USA_refining_in" )
USA_refining_out <- readdata( "USA_refining_out" )
USA_ind_elec_refining <- readdata( "USA_ind_elec_refining" )
NE_ethanol_Mgal.yr <- readdata( "NE_ethanol_Mgal.yr" )
USA_final_in <- readdata( "USA_final_in" )
USA_elec_chp_out <- readdata( "USA_elec_chp_out" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#CRUDE OIL REFINING
printlog( "NOTE: using SEDS crude oil input to industry as basis for allocation of crude oil refining to states" )
#Crude oil consumption by industry is the energy used at refineries (input - output)
L102_net_EJ_state_cor_crude <- L101_inEIA_EJ_state_S_F[ L101_inEIA_EJ_state_S_F$GCAM_sector == "industry" &
      L101_inEIA_EJ_state_S_F$GCAM_fuel == "crude oil", ]
L102_net_EJ_state_cor_crude$GCAM_sector <- "crude oil refining"

#Calculate the percentages in each state
L102_pct_state_cor <- data.frame( state = L102_net_EJ_state_cor_crude$state,
      GCAM_sector = L102_net_EJ_state_cor_crude$GCAM_sector,
      sweep( L102_net_EJ_state_cor_crude[ X_base_years ], 2, colSums( L102_net_EJ_state_cor_crude[ X_base_years ] ), "/" ) )

printlog( "Crude oil refining output by state" )
#Calculate the output in each state as total US output times the state-wise percentage
#First, repeat the total national output by the number of states
L102_out_EJ_USA_cor <- USA_refining_out [USA_refining_out$technology == "crude oil refining", ]
L102_out_EJ_USA_cor_repstate <- L102_out_EJ_USA_cor[ rep( 1, times = length( states ) ), ]
L102_out_EJ_state_cor <- data.frame( state = states,
      GCAM_sector = L102_out_EJ_USA_cor_repstate$subsector,
      GCAM_fuel = L102_out_EJ_USA_cor_repstate$supplysector,
      L102_out_EJ_USA_cor_repstate[ X_base_years ] * L102_pct_state_cor[ X_base_years ] )

printlog( "Crude oil refining inputs by state and fuel" )
#Inputs to crude oil refining - same method, but built to handle multiple inputs
L102_in_EJ_USA_cor_F <- USA_refining_in [USA_refining_in$technology == "crude oil refining", ]

#At this point, the elec input includes the input to "refined liquids electricity", which needs to be deducted
printlog( "Deducting electricity input to refined liquids electricity (accounted under industrial energy)" )
L102_in_EJ_USA_cor_F[ L102_in_EJ_USA_cor_F$minicam.energy.input == "elect_td_ind", X_base_years ] <-
      L102_in_EJ_USA_cor_F[ L102_in_EJ_USA_cor_F$minicam.energy.input == "elect_td_ind", X_base_years ] - 
      USA_ind_elec_refining[ X_base_years ]

L102_in_EJ_USA_cor_F_repstate <- L102_in_EJ_USA_cor_F[ rep( 1:nrow( L102_in_EJ_USA_cor_F ), times = length( states ) ), ]
L102_in_EJ_USA_cor_F_repstate <- L102_in_EJ_USA_cor_F_repstate[ order( L102_in_EJ_USA_cor_F_repstate$minicam.energy.input ), ]

#Repeat percentage-wise table by number of fuel inputs
L102_pct_state_cor_repF <- L102_pct_state_cor[ rep( 1:nrow( L102_pct_state_cor ), times = nrow( L102_in_EJ_USA_cor_F ) ), ]

L102_in_EJ_state_cor_F <- data.frame( state = rep( states, times = nrow( L102_in_EJ_USA_cor_F ) ),
      GCAM_sector = L102_in_EJ_USA_cor_F_repstate$subsector,
      GCAM_fuel = L102_in_EJ_USA_cor_F_repstate$minicam.energy.input,
      L102_in_EJ_USA_cor_F_repstate[ X_base_years ] * L102_pct_state_cor_repF[ X_base_years ] )

#BIOMASS LIQUIDS
printlog( "NOTE: Using ethanol production by state to disaggregate all biomass liquids production to states" )
#Biomass liquids production is not in the state databases from EIA. Disaggregate US biomass liquids production to states.
#Percentage of ethanol production by state
NE_ethanol_Mgal.yr$pct <- NE_ethanol_Mgal.yr$Mgal.yr / sum( NE_ethanol_Mgal.yr$Mgal.yr )

#Build a table with these percentages by all states, in the base years
L102_pct_state_btl <- data.frame( state = states,
      GCAM_sector = "biomass liquids" )
L102_pct_state_btl$pct <- NE_ethanol_Mgal.yr$pct[ match( L102_pct_state_btl$state, NE_ethanol_Mgal.yr$state ) ]
L102_pct_state_btl$pct[ is.na( L102_pct_state_btl$pct ) ] <- 0

#Disaggregation of biomass liquids output for ethanol and biodiesel
printlog( "Corn ethanol output by state" )
L102_out_EJ_USA_btle <- USA_refining_out[ USA_refining_out$technology == "biomass liquids-ethanol (conv)", ]
L102_out_EJ_USA_btle_repstate <- L102_out_EJ_USA_btle[ rep( 1, times = length( states ) ), ]
L102_out_EJ_state_btle <- data.frame( state = states,
      GCAM_sector = L102_out_EJ_USA_btle_repstate$technology,
      GCAM_fuel = L102_out_EJ_USA_btle_repstate$supplysector,
      L102_out_EJ_USA_btle_repstate[ X_base_years ] * L102_pct_state_btl$pct )
      
printlog( "Corn ethanol inputs by state and fuel" )
L102_in_EJ_USA_btle_F <- USA_refining_in[ USA_refining_in$technology == "biomass liquids-ethanol (conv)", ]
L102_in_EJ_USA_btle_F_repstate <- L102_in_EJ_USA_btle_F[ rep( 1:nrow( L102_in_EJ_USA_btle_F ), times = length( states ) ), ]
L102_in_EJ_USA_btle_F_repstate <- L102_in_EJ_USA_btle_F_repstate[ order( L102_in_EJ_USA_btle_F_repstate$minicam.energy.input ), ]

#Repeat percentage-wise table by number of fuel inputs
L102_pct_state_btle_repF <- L102_pct_state_btl[ rep( 1:nrow( L102_pct_state_btl ),
      times = length( unique( L102_in_EJ_USA_btle_F$minicam.energy.input ) ) ), ]

L102_in_EJ_state_btle_F <- data.frame( state = rep( states, times = nrow( L102_in_EJ_USA_btle_F ) ),
      GCAM_sector = L102_in_EJ_USA_btle_F_repstate$technology,
      GCAM_fuel = L102_in_EJ_USA_btle_F_repstate$minicam.energy.input,
      L102_in_EJ_USA_btle_F_repstate[ X_base_years ] * L102_pct_state_btle_repF$pct )

printlog( "Biodiesel output by state" )
L102_out_EJ_USA_btlbd <- USA_refining_out[ USA_refining_out$technology == "biomass liquids-biodiesel (conv)", ]
L102_out_EJ_USA_btlbd_repstate <- L102_out_EJ_USA_btlbd[ rep( 1, times = length( states ) ), ]
L102_out_EJ_state_btlbd <- data.frame( state = states,
      GCAM_sector = L102_out_EJ_USA_btlbd_repstate$technology,
      GCAM_fuel = L102_out_EJ_USA_btlbd_repstate$supplysector,
      L102_out_EJ_USA_btlbd_repstate[ X_base_years ] * L102_pct_state_btl$pct )

printlog( "Biodiesel inputs by state and fuel" )
L102_in_EJ_USA_btlbd_F <- USA_refining_in[ USA_refining_in$technology == "biomass liquids-biodiesel (conv)", ]
L102_in_EJ_USA_btlbd_F_repstate <- L102_in_EJ_USA_btlbd_F[ rep( 1:nrow( L102_in_EJ_USA_btlbd_F ), times = length( states ) ), ]
L102_in_EJ_USA_btlbd_F_repstate <- L102_in_EJ_USA_btlbd_F_repstate[ order( L102_in_EJ_USA_btlbd_F_repstate$minicam.energy.input ), ]

#Repeat percentage-wise table by number of fuel inputs
L102_pct_state_btlbd_repF <- L102_pct_state_btl[ rep( 1:nrow( L102_pct_state_btl ),
      times = length( unique( L102_in_EJ_USA_btlbd_F$minicam.energy.input ) ) ), ]

L102_in_EJ_state_btlbd_F <- data.frame( state = rep( states, times = nrow( L102_in_EJ_USA_btlbd_F ) ),
      GCAM_sector = L102_in_EJ_USA_btlbd_F_repstate$technology,
      GCAM_fuel = L102_in_EJ_USA_btlbd_F_repstate$minicam.energy.input,
      L102_in_EJ_USA_btlbd_F_repstate[ X_base_years ] * L102_pct_state_btlbd_repF$pct )

#rbind the tables of inputs and outputs of all refineries by state in the base years
L102_in_EJ_state_refining_F <- rbind( L102_in_EJ_state_cor_F, L102_in_EJ_state_btle_F, L102_in_EJ_state_btlbd_F )
L102_out_EJ_state_refining_F <- rbind( L102_out_EJ_state_cor, L102_out_EJ_state_btle, L102_out_EJ_state_btlbd )

#INDUSTRIAL ENERGY USE - DEDUCTION FOR REFINING ENERGY USE
#Electricity and gas inputs to refineries are now deducted from industry.
#Subset industrial energy use from whole-US table
printlog( "Recalculating industrial energy use in SEDS database, deducting the energy that is used in refining" )
L102_in_EJ_state_ind_elec <- L101_inEIA_EJ_state_S_F[ L101_inEIA_EJ_state_S_F$GCAM_sector == "industry" &
      L101_inEIA_EJ_state_S_F$GCAM_fuel == "electricity", ]
L102_in_EJ_state_ind_gas <- L101_inEIA_EJ_state_S_F[ L101_inEIA_EJ_state_S_F$GCAM_sector == "industry" &
      L101_inEIA_EJ_state_S_F$GCAM_fuel == "gas", ]

#Electricity
#Aggregate all refining techs' electricity use by state
L102_in_EJ_state_refining_elec <- L102_in_EJ_state_refining_F[ L102_in_EJ_state_refining_F$GCAM_fuel == "elect_td_ind", ]
L102_in_EJ_state_refining_electot <- aggregate( L102_in_EJ_state_refining_elec[ X_base_years ],
      by=list( state = L102_in_EJ_state_refining_elec$state, GCAM_fuel = L102_in_EJ_state_refining_elec$GCAM_fuel ), sum )

#Adjusted industrial fuel consumption = initial minus refinery fuel consumption
L102_in_EJ_state_ind_elec_adj <- L102_in_EJ_state_ind_elec
L102_in_EJ_state_ind_elec_adj[ X_base_years ] <- L102_in_EJ_state_ind_elec[ X_base_years ] - L102_in_EJ_state_refining_electot[ X_base_years ]
#Replace any negative values with zeroes
L102_in_EJ_state_ind_elec_adj[ X_base_years ] <- apply( L102_in_EJ_state_ind_elec_adj[ X_base_years ],
      2, function( x ){ x[ x<0 ] = 0; x } )

#Natural gas
L102_in_EJ_state_refining_gas <- L102_in_EJ_state_refining_F[ L102_in_EJ_state_refining_F$GCAM_fuel == "wholesale gas", ]
L102_in_EJ_state_refining_gastot <- aggregate( L102_in_EJ_state_refining_gas[ X_base_years ],
      by=list( state = L102_in_EJ_state_refining_gas$state, GCAM_fuel = L102_in_EJ_state_refining_gas$GCAM_fuel ), sum )

L102_in_EJ_state_ind_gas_adj <- L102_in_EJ_state_ind_gas
L102_in_EJ_state_ind_gas_adj[ X_base_years ] <- L102_in_EJ_state_ind_gas[ X_base_years ] - L102_in_EJ_state_refining_gastot[ X_base_years ]
#Replace any negative values with zeroes
L102_in_EJ_state_ind_gas_adj[ X_base_years ] <- apply( L102_in_EJ_state_ind_gas_adj[ X_base_years ],
      2, function( x ){ x[ x<0 ] = 0; x } )

#Put together a new table for industrial energy consumption. This will be used to calculate state-wise percentages of industrial energy use
L102_in_EJ_state_ind_F_unscaled <- L101_inEIA_EJ_state_S_F[ L101_inEIA_EJ_state_S_F$GCAM_sector == "industry", ]
L102_in_EJ_state_ind_F_unscaled[ L102_in_EJ_state_ind_F_unscaled$GCAM_fuel == "electricity", X_base_years ] <- L102_in_EJ_state_ind_elec_adj[ X_base_years ]
L102_in_EJ_state_ind_F_unscaled[L102_in_EJ_state_ind_F_unscaled$GCAM_fuel=="gas", X_base_years ] <- L102_in_EJ_state_ind_gas_adj[ X_base_years ]

#Subset only those fuels that are considered part of "industrial energy use"
L102_in_EJ_USA_ind_F_tech <- USA_final_in[ USA_final_in$supplysector=="industrial energy use", ]
L102_in_EJ_state_indenergy_F_unscaled <- L102_in_EJ_state_ind_F_unscaled[ L102_in_EJ_state_ind_F_unscaled$GCAM_fuel %in% L102_in_EJ_USA_ind_F_tech$subsector, ]
L102_in_EJ_state_indenergy_F_unscaled$GCAM_sector <- "industrial energy use"

#Aggregate by fuel to calculate each state's portion of the national industrial sector totals
L102_in_EJ_USA_ind_F_unscaled <- aggregate( L102_in_EJ_state_indenergy_F_unscaled[X_base_years],
      list( GCAM_sector = L102_in_EJ_state_indenergy_F_unscaled$GCAM_sector, GCAM_fuel = L102_in_EJ_state_indenergy_F_unscaled$GCAM_fuel ), sum )

#Repeat by number of states and sort by fuel
L102_in_EJ_USA_ind_F_unscaled_repstate <- L102_in_EJ_USA_ind_F_unscaled[ rep( 1:nrow( L102_in_EJ_USA_ind_F_unscaled ), times = length( states ) ), ]
L102_in_EJ_USA_ind_F_unscaled_repstate <- L102_in_EJ_USA_ind_F_unscaled_repstate[ order( L102_in_EJ_USA_ind_F_unscaled_repstate$GCAM_fuel ), ]

#Calculate the percentages
L102_in_pct_state_ind_F <- data.frame(
      L102_in_EJ_state_indenergy_F_unscaled[ state_S_F ],
      L102_in_EJ_state_indenergy_F_unscaled[ X_base_years ] / L102_in_EJ_USA_ind_F_unscaled_repstate[ X_base_years ] )

#Apportion nation-level industrial energy consumption to states - NON-COGEN
printlog( "Industrial sector non-cogen input energy by state and fuel" )
L102_in_EJ_USA_indnochp_F <- L102_in_EJ_USA_ind_F_tech[ grep( "cogen", L102_in_EJ_USA_ind_F_tech$technology, invert = TRUE ), ]
L102_in_EJ_USA_indnochp_F_repstate <- L102_in_EJ_USA_indnochp_F[ rep( 1:nrow( L102_in_EJ_USA_indnochp_F ), times = length( states) ), ]
L102_in_EJ_USA_indnochp_F_repstate <- L102_in_EJ_USA_indnochp_F_repstate[ order( L102_in_EJ_USA_indnochp_F_repstate$subsector ), ]
L102_in_EJ_state_indnochp_F <- data.frame(
      L102_in_pct_state_ind_F[ state_S_F ],
      L102_in_EJ_USA_indnochp_F_repstate[ X_base_years ] * L102_in_pct_state_ind_F[ X_base_years ] )

#Apportion nation-level industrial energy consumption to states - COGEN
printlog( "Industrial sector cogeneration input energy by state and fuel" )
L102_in_EJ_USA_indchp_F <- L102_in_EJ_USA_ind_F_tech[ grep( "cogen", L102_in_EJ_USA_ind_F_tech$technology, invert = FALSE ), ]
L102_in_EJ_USA_indchp_F_repstate <- L102_in_EJ_USA_indchp_F[ rep( 1:nrow( L102_in_EJ_USA_indchp_F ), times = length( states) ), ]
L102_in_EJ_USA_indchp_F_repstate <- L102_in_EJ_USA_indchp_F_repstate[ order( L102_in_EJ_USA_indchp_F_repstate$subsector ), ]

#Remove non-applicable fuels (electricity) from the portional allocation table
L102_in_pct_state_indchp_F <- L102_in_pct_state_ind_F[ L102_in_pct_state_ind_F$GCAM_fuel %in% L102_in_EJ_USA_indchp_F$subsector, ]
L102_in_EJ_state_indchp_F <- data.frame(
      L102_in_pct_state_indchp_F[ state_S_F ],
      L102_in_EJ_USA_indchp_F_repstate[ X_base_years ] * L102_in_pct_state_indchp_F[ X_base_years ] )

#Apportion nation-level industrial cogen output to states
printlog( "Industrial sector electricity cogeneration by state" )
L102_out_EJ_USA_indchp_F_repstate <- USA_elec_chp_out[ rep( 1:nrow( USA_elec_chp_out ), times = length( states) ), ]
L102_out_EJ_USA_indchp_F_repstate <- L102_out_EJ_USA_indchp_F_repstate[ order( L102_out_EJ_USA_indchp_F_repstate$subsector ), ]
L102_out_EJ_state_indchp_F <- data.frame(
      L102_in_pct_state_indchp_F[ state_S_F ],
      L102_out_EJ_USA_indchp_F_repstate[ X_base_years ] * L102_in_pct_state_indchp_F[ X_base_years ] )

#INDUSTRIAL FEEDSTOCKS
#Only considering gas and liquid fuels; each will be treated separately.
# Liquid fuels apportioned to states by sum of petchem feed and asphalt
# Gas apportioned to states by petchem feed only
printlog( "Industrial feedstocks by state and fuel" )
printlog( "NOTE: using petrochememical feedstocks as basis for disaggregating natural gas feedstocks to states" )
L102_in_EJ_USA_indfeed_gas <- USA_final_in[ USA_final_in$supplysector=="industrial feedstocks" & USA_final_in$subsector == "gas", ]
L102_in_EJ_USA_indfeed_liq <- USA_final_in[ USA_final_in$supplysector=="industrial feedstocks" & USA_final_in$subsector == "refined liquids", ]

#Split state-level data into feedstocks only, according to names of fuel inputs
L102_in_EJ_state_indfeed_F_unscaled <- L102_in_EJ_state_ind_F_unscaled[ L102_in_EJ_state_ind_F_unscaled$GCAM_fuel %in%
      c( "refined liquids (const feed)", "refined liquids (petchem feed)" ), ]

#Aggregate the two feedstock types to calculate the liquid fuel state-wise portional allocations
L102_in_EJ_state_indfeed_liq_unscaled <- aggregate( L102_in_EJ_state_indfeed_F_unscaled[ X_base_years ],
      list( state = L102_in_EJ_state_indfeed_F_unscaled$state, GCAM_sector = L102_in_EJ_state_indfeed_F_unscaled$GCAM_sector ), sum )
L102_in_EJ_USA_indfeed_liq_unscaled <- aggregate( L102_in_EJ_state_indfeed_liq_unscaled[ X_base_years ],
      list( GCAM_sector = L102_in_EJ_state_indfeed_liq_unscaled$GCAM_sector ), sum )
L102_in_EJ_USA_indfeed_liq_unscaled_repstate <-
      L102_in_EJ_USA_indfeed_liq_unscaled[ rep( 1:nrow( L102_in_EJ_USA_indfeed_liq_unscaled ), times = length( states ) ), ]

#Calculate the state-wise allocations of liquid fuel feedstocks
L102_in_pct_state_indfeed_liq <- data.frame( state = states,
      GCAM_sector = "industrial feedstocks", GCAM_fuel = "refined liquids",
      L102_in_EJ_state_indfeed_liq_unscaled[ X_base_years ] / L102_in_EJ_USA_indfeed_liq_unscaled_repstate[ X_base_years ] )

#Multiply the state-wise portional allocation by the (repeated) national total
L102_in_EJ_USA_indfeed_liq_repstate <- L102_in_EJ_USA_indfeed_liq[ rep( 1:nrow( L102_in_EJ_USA_indfeed_liq ), times = length( states ) ), ]
L102_in_EJ_state_indfeed_liq <- data.frame( L102_in_pct_state_indfeed_liq[ state_S_F ],
      L102_in_pct_state_indfeed_liq[ X_base_years ] * L102_in_EJ_USA_indfeed_liq_repstate[ X_base_years ] )

#Repeat method for gas feedstocks, this time using only petchem feedstocks as the basis for the allocation
L102_in_EJ_state_indfeed_gas_unscaled <- L102_in_EJ_state_indfeed_F_unscaled[
      L102_in_EJ_state_indfeed_F_unscaled$GCAM_fuel == "refined liquids (petchem feed)", ]
L102_in_EJ_USA_indfeed_gas_unscaled <- aggregate( L102_in_EJ_state_indfeed_gas_unscaled[ X_base_years ],
      list( GCAM_sector = L102_in_EJ_state_indfeed_gas_unscaled$GCAM_sector ), sum )
L102_in_EJ_USA_indfeed_gas_unscaled_repstate <-
      L102_in_EJ_USA_indfeed_gas_unscaled[ rep( 1:nrow( L102_in_EJ_USA_indfeed_gas_unscaled ), times = length( states ) ), ]

#Calculate the state-wise allocations of petchem feedstocks
L102_in_pct_state_indfeed_gas <- data.frame( state = states,
      GCAM_sector = "industrial feedstocks", GCAM_fuel = "gas",
      L102_in_EJ_state_indfeed_gas_unscaled[ X_base_years ] / L102_in_EJ_USA_indfeed_gas_unscaled_repstate[ X_base_years ] )

#Multiply the state-wise portional allocation by the (repeated) national total
L102_in_EJ_USA_indfeed_gas_repstate <- L102_in_EJ_USA_indfeed_gas[ rep( 1:nrow( L102_in_EJ_USA_indfeed_gas ), times = length( states ) ), ]
L102_in_EJ_state_indfeed_gas <- data.frame( L102_in_pct_state_indfeed_gas[ state_S_F ],
      L102_in_pct_state_indfeed_gas[ X_base_years ] * L102_in_EJ_USA_indfeed_gas_repstate[ X_base_years ] )

#Combine the gas and liquid feedstocks tables and write it out
L102_in_EJ_state_indfeed_F <- rbind( L102_in_EJ_state_indfeed_gas, L102_in_EJ_state_indfeed_liq )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L102_in_EJ_state_refining_F <- c( "Refinery energy inputs by state, technology, and fuel","Unit = EJ" )
comments.L102_out_EJ_state_refining_F <- c( "Refinery output by state and technology","Unit = EJ" )
comments.L102_in_EJ_state_indnochp_F <- c( "Non-cogen industrial energy consumption by state and fuel","Unit = EJ" )
comments.L102_in_EJ_state_indchp_F <- c( "Industrial cogen energy consumption by state and fuel","Unit = EJ" )
comments.L102_out_EJ_state_indchp_F <- c( "Industrial electricity cogeneration by state and fuel","Unit = EJ" )
comments.L102_in_EJ_state_indfeed_F <- c( "Industrial feedstocks by state and fuel","Unit = EJ" )

#write tables as CSV files
writedata( L102_in_EJ_state_refining_F,fn="L102_in_EJ_state_refining_F", comments=comments.L102_in_EJ_state_refining_F )
writedata( L102_out_EJ_state_refining_F,fn="L102_out_EJ_state_refining_F", comments=comments.L102_out_EJ_state_refining_F )
writedata( L102_in_EJ_state_indnochp_F,fn="L102_in_EJ_state_indnochp_F", comments=comments.L102_in_EJ_state_indnochp_F )
writedata( L102_in_EJ_state_indchp_F,fn="L102_in_EJ_state_indchp_F", comments=comments.L102_in_EJ_state_indchp_F )
writedata( L102_out_EJ_state_indchp_F,fn="L102_out_EJ_state_indchp_F", comments=comments.L102_out_EJ_state_indchp_F )
writedata( L102_in_EJ_state_indfeed_F,fn="L102_in_EJ_state_indfeed_F", comments=comments.L102_in_EJ_state_indfeed_F )

# Every script should finish with this line
logstop()