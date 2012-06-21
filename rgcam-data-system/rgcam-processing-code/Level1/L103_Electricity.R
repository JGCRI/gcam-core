# L103_Electricity.R
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L103_Electricity.R" )
printlog( "Electricity sector inputs and outputs, and electricity ownuse" )

# -----------------------------------------------------------------------------
# 1. Read files

USA_elec_in <- readdata( "USA_elec_in" )
USA_elec_out <- readdata( "USA_elec_out" )
EIA_elect_td_ownuse_prices <- readdata( "EIA_elect_td_ownuse_prices" )
USA_elec_ownuse_in <- readdata( "USA_elec_ownuse_in" )
USA_elec_ownuse_out <- readdata( "USA_elec_ownuse_out" )
L101_inEIA_EJ_state_S_F <- readdata( "L101_inEIA_EJ_state_S_F" )
L102_out_EJ_state_indchp_F <- readdata( "L102_out_EJ_state_indchp_F" )

# -----------------------------------------------------------------------------
#ELECTRICITY_INPUT
#SEDS indicates electricity generation technologies either in terms of fuel inputs or fuel outputs (not both)
#These will need to be merged, but at first are handled separately
L103_in_EJ_state_elec_Fin_unscaled <- L101_inEIA_EJ_state_S_F[ L101_inEIA_EJ_state_S_F$GCAM_sector == "electricity_input", ]

#Aggregate by fuel compute each state's percentage, by fuel
L103_in_EJ_USA_elec_Fin_unscaled <- aggregate( L103_in_EJ_state_elec_Fin_unscaled[ X_base_years ],
      list(GCAM_sector = L103_in_EJ_state_elec_Fin_unscaled$GCAM_sector, GCAM_fuel = L103_in_EJ_state_elec_Fin_unscaled$GCAM_fuel), sum)
L103_in_EJ_USA_elec_Fin_unscaled_repstate <- L103_in_EJ_USA_elec_Fin_unscaled[ rep( 1:nrow( L103_in_EJ_USA_elec_Fin_unscaled ), times = length( states ) ), ]
L103_in_EJ_USA_elec_Fin_unscaled_repstate <- L103_in_EJ_USA_elec_Fin_unscaled_repstate[ order( L103_in_EJ_USA_elec_Fin_unscaled_repstate$GCAM_fuel ), ]

L103_in_pct_USA_elec_Fin <- data.frame(
      L103_in_EJ_state_elec_Fin_unscaled[ state_S_F ],
      L103_in_EJ_state_elec_Fin_unscaled[ X_base_years ] / L103_in_EJ_USA_elec_Fin_unscaled_repstate[ X_base_years ] )

#Subset only the fuels in the USA_elec_in table that are indicated on the basis of inputs
#First, rename the "oil" subsector as refined liquids for naming compatibility
printlog( "Electricity generation inputs, for technologies indicated in terms of input in SEDS" )
L103_in_EJ_USA_elec_Fin <- USA_elec_in
L103_in_EJ_USA_elec_Fin$subsector[ L103_in_EJ_USA_elec_Fin$subsector == "oil"] <- "refined liquids"
L103_in_EJ_USA_elec_Fin <- L103_in_EJ_USA_elec_Fin[ L103_in_EJ_USA_elec_Fin$subsector %in% L103_in_pct_USA_elec_Fin$GCAM_fuel, ]
#Sort to make sure subsectors are in the right order
L103_in_EJ_USA_elec_Fin <- L103_in_EJ_USA_elec_Fin[ order( L103_in_EJ_USA_elec_Fin$subsector ), ]

#Multiply the percentage-wise allocations by the fuel inputs to calculate the inputs to electricity generation
L103_in_EJ_USA_elec_Fin_repstate <- L103_in_EJ_USA_elec_Fin[ rep( 1:nrow( L103_in_EJ_USA_elec_Fin ), times = length( states ) ), ]
L103_in_EJ_USA_elec_Fin_repstate <- L103_in_EJ_USA_elec_Fin_repstate[ order( L103_in_EJ_USA_elec_Fin_repstate$subsector ), ]
L103_in_EJ_state_elec_Fin <- data.frame(
      L103_in_pct_USA_elec_Fin[ state_S_F ],
      L103_in_pct_USA_elec_Fin[ X_base_years ] * L103_in_EJ_USA_elec_Fin_repstate[ X_base_years ] )

printlog( "Electricity generation outputs, for technologies indicated in terms of input in SEDS" )
L103_out_EJ_USA_elec_Fin <- USA_elec_out
L103_out_EJ_USA_elec_Fin$subsector[ L103_out_EJ_USA_elec_Fin$subsector == "oil"] <- "refined liquids"
L103_out_EJ_USA_elec_Fin <- L103_out_EJ_USA_elec_Fin[ L103_out_EJ_USA_elec_Fin$subsector %in% L103_in_pct_USA_elec_Fin$GCAM_fuel, ]
#Sort to make sure subsectors are in the right order
L103_out_EJ_USA_elec_Fin <- L103_out_EJ_USA_elec_Fin[ order( L103_out_EJ_USA_elec_Fin$subsector ), ]

#Multiply the percentage-wise allocations by the fuel inputs to calculate the inputs to electricity generation
L103_out_EJ_USA_elec_Fin_repstate <- L103_out_EJ_USA_elec_Fin[ rep( 1:nrow( L103_out_EJ_USA_elec_Fin ), times = length( states ) ), ]
L103_out_EJ_USA_elec_Fin_repstate <- L103_out_EJ_USA_elec_Fin_repstate[ order( L103_out_EJ_USA_elec_Fin_repstate$subsector ), ]
L103_out_EJ_state_elec_Fin <- data.frame(
      L103_in_pct_USA_elec_Fin[ state_S_F ],
      L103_in_pct_USA_elec_Fin[ X_base_years ] * L103_out_EJ_USA_elec_Fin_repstate[ X_base_years ] )

#ELECTRICITY_OUTPUT
L103_in_EJ_state_elec_Fout_unscaled <- L101_inEIA_EJ_state_S_F[ L101_inEIA_EJ_state_S_F$GCAM_sector == "electricity_output", ]

#Aggregate for the whole nation and compute each state's percentage.
L103_in_EJ_USA_elec_Fout_unscaled <- aggregate( L103_in_EJ_state_elec_Fout_unscaled[ X_base_years ],
      list(GCAM_sector = L103_in_EJ_state_elec_Fout_unscaled$GCAM_sector, GCAM_fuel = L103_in_EJ_state_elec_Fout_unscaled$GCAM_fuel), sum)
L103_in_EJ_USA_elec_Fout_unscaled_repstate <- L103_in_EJ_USA_elec_Fout_unscaled[ rep( 1:nrow( L103_in_EJ_USA_elec_Fout_unscaled ), times = length( states ) ), ]
L103_in_EJ_USA_elec_Fout_unscaled_repstate <- L103_in_EJ_USA_elec_Fout_unscaled_repstate[ order( L103_in_EJ_USA_elec_Fout_unscaled_repstate$GCAM_fuel ), ]

L103_in_pct_USA_elec_Fout <- data.frame(
      L103_in_EJ_state_elec_Fout_unscaled[ state_S_F ],
      L103_in_EJ_state_elec_Fout_unscaled[ X_base_years ] / L103_in_EJ_USA_elec_Fout_unscaled_repstate[ X_base_years ] )
L103_in_pct_USA_elec_Fout[ is.na( L103_in_pct_USA_elec_Fout ) ] <- 0

#Subset only the fuels in the USA_elec_out table that are indicated on the basis of inputs
#First, rename the "oil" subsector as refined liquids for naming compatibility
printlog( "Electricity generation inputs, for technologies indicated in terms of output in SEDS" )
L103_in_EJ_USA_elec_Fout <- USA_elec_in
L103_in_EJ_USA_elec_Fout <- L103_in_EJ_USA_elec_Fout[ L103_in_EJ_USA_elec_Fout$subsector %in% L103_in_pct_USA_elec_Fout$GCAM_fuel, ]
#Sort to make sure subsectors are in the right order
L103_in_EJ_USA_elec_Fout <- L103_in_EJ_USA_elec_Fout[ order( L103_in_EJ_USA_elec_Fout$subsector ), ]

#Multiply the percentage-wise allocations by the fuel inputs to calculate the inputs to electricity generation
L103_in_EJ_USA_elec_Fout_repstate <- L103_in_EJ_USA_elec_Fout[ rep( 1:nrow( L103_in_EJ_USA_elec_Fout ), times = length( states ) ), ]
L103_in_EJ_USA_elec_Fout_repstate <- L103_in_EJ_USA_elec_Fout_repstate[ order( L103_in_EJ_USA_elec_Fout_repstate$subsector ), ]
L103_in_EJ_state_elec_Fout <- data.frame(
      L103_in_pct_USA_elec_Fout[ state_S_F ],
      L103_in_pct_USA_elec_Fout[ X_base_years ] * L103_in_EJ_USA_elec_Fout_repstate[ X_base_years ] )

printlog( "Electricity generation outputs, for technologies indicated in terms of output in SEDS" )
L103_out_EJ_USA_elec_Fout <- USA_elec_out
L103_out_EJ_USA_elec_Fout <- L103_out_EJ_USA_elec_Fout[ L103_out_EJ_USA_elec_Fout$subsector %in% L103_in_pct_USA_elec_Fout$GCAM_fuel, ]
#Sort to make sure subsectors are in the right order
L103_out_EJ_USA_elec_Fout <- L103_out_EJ_USA_elec_Fout[ order( L103_out_EJ_USA_elec_Fout$subsector ), ]

#Multiply the percentage-wise allocations by the fuel inputs to calculate the inputs to electricity generation
L103_out_EJ_USA_elec_Fout_repstate <- L103_out_EJ_USA_elec_Fout[ rep( 1:nrow( L103_out_EJ_USA_elec_Fout ), times = length( states ) ), ]
L103_out_EJ_USA_elec_Fout_repstate <- L103_out_EJ_USA_elec_Fout_repstate[ order( L103_out_EJ_USA_elec_Fout_repstate$subsector ), ]
L103_out_EJ_state_elec_Fout <- data.frame(
      L103_in_pct_USA_elec_Fout[ state_S_F ],
      L103_in_pct_USA_elec_Fout[ X_base_years ] * L103_out_EJ_USA_elec_Fout_repstate[ X_base_years ] )

#Combine tables of inputs and outputs
L103_in_EJ_state_elec_F <- rbind( L103_in_EJ_state_elec_Fin, L103_in_EJ_state_elec_Fout )
L103_in_EJ_state_elec_F$GCAM_sector <- "electricity"

L103_out_EJ_state_elec_F <- rbind( L103_out_EJ_state_elec_Fin, L103_out_EJ_state_elec_Fout )
L103_out_EJ_state_elec_F$GCAM_sector <- "electricity"

#ELECTRICITY - OWNUSE
printlog( "Electricity own use by state" )
printlog( "NOTE: Electricity net own use energy is apportioned to states on the basis of EIA's direct use by state" )
L103_net_EJ_USA_ownuse <- USA_elec_ownuse_in
L103_net_EJ_USA_ownuse[ X_base_years ] <- USA_elec_ownuse_in[ X_base_years ] - USA_elec_ownuse_out[ X_base_years ]

#Determine each state's share of the national ownuse
L103_net_pct_state_USA_ownuse_elec <- data.frame( state = states, DirectUse_MWh = 0, pct = 0 )
L103_net_pct_state_USA_ownuse_elec$DirectUse_MWh <- EIA_elect_td_ownuse_prices$DirectUse_MWh[
      match( L103_net_pct_state_USA_ownuse_elec$state, EIA_elect_td_ownuse_prices$State ) ]
L103_net_pct_state_USA_ownuse_elec$pct <-
      L103_net_pct_state_USA_ownuse_elec$DirectUse_MWh / sum( L103_net_pct_state_USA_ownuse_elec$DirectUse_MWh )

#Calculate the net ownuse in each state
L103_net_EJ_USA_ownuse_repstate <- L103_net_EJ_USA_ownuse[ rep( 1, times = length( states ) ), ]
L103_net_EJ_state_USA_ownuse_elec <- data.frame( state = states,
      GCAM_sector = "own use", GCAM_fuel = "electricity",
      L103_net_EJ_USA_ownuse_repstate[ X_base_years ] * L103_net_pct_state_USA_ownuse_elec$pct )

#The input of the electricity_net_ownuse sector is equal to sum of all generation (industrial CHP + electric sector)
#Electricity sector output
L103_out_EJ_state_elec <- aggregate( L103_out_EJ_state_elec_F[ X_base_years ],
      list( state = L103_out_EJ_state_elec_F$state, GCAM_sector = L103_out_EJ_state_elec_F$GCAM_sector ), sum )
L103_out_EJ_state_elec$GCAM_fuel <- "electricity"
L103_out_EJ_state_elec <- L103_out_EJ_state_elec[ c( state_S_F, X_base_years ) ]

#Industrial sector CHP output
L103_out_EJ_state_indchp <- aggregate( L102_out_EJ_state_indchp_F[ X_base_years ],
      list( state = L102_out_EJ_state_indchp_F$state, GCAM_sector = L102_out_EJ_state_indchp_F$GCAM_sector ), sum )
L103_out_EJ_state_indchp$GCAM_fuel <- "electricity"
L103_out_EJ_state_indchp <- L103_out_EJ_state_indchp[ c( state_S_F, X_base_years ) ]

#Add the two up. This is "electricity net ownuse" sector input
L103_in_EJ_state_ownuse_elec <- L103_out_EJ_state_elec
L103_in_EJ_state_ownuse_elec$GCAM_sector <- "own use"
L103_in_EJ_state_ownuse_elec[ X_base_years ] <- L103_out_EJ_state_elec[ X_base_years ] + L103_out_EJ_state_indchp[ X_base_years ]

#Output of electricity_net_ownuse sector is equal to input minus ownuse "net" energy
L103_out_EJ_state_ownuse_elec <- L103_in_EJ_state_ownuse_elec
L103_out_EJ_state_ownuse_elec[ X_base_years ] <-
      L103_in_EJ_state_ownuse_elec[ X_base_years ] - L103_net_EJ_state_USA_ownuse_elec[ X_base_years ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L103_in_EJ_state_elec_F <- c( "Electricity sector energy consumption by state and fuel","Unit = EJ" )
comments.L103_out_EJ_state_elec_F <- c( "Electricity generation by state and fuel","Unit = EJ" )
comments.L103_in_EJ_state_ownuse_elec <- c( "Input to electricity net ownuse by state","Unit = EJ" )
comments.L103_out_EJ_state_ownuse_elec <- c( "Output of electricity net ownuse by state","Unit = EJ" )

#write tables as CSV files
writedata( L103_in_EJ_state_elec_F,fn="L103_in_EJ_state_elec_F", comments=comments.L103_in_EJ_state_elec_F )
writedata( L103_out_EJ_state_elec_F,fn="L103_out_EJ_state_elec_F", comments=comments.L103_out_EJ_state_elec_F )
writedata( L103_in_EJ_state_ownuse_elec,fn="L103_in_EJ_state_ownuse_elec", comments=comments.L103_in_EJ_state_ownuse_elec )
writedata( L103_out_EJ_state_ownuse_elec,fn="L103_out_EJ_state_ownuse_elec", comments=comments.L103_out_EJ_state_ownuse_elec )

# Every script should finish with this line
logstop()