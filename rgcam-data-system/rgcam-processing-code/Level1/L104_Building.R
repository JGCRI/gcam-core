# L104_Building.R
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L104_Building.R" )
printlog( "Buildings sector energy consumption" )

# -----------------------------------------------------------------------------
# 1. Read files

USA_final_in <- readdata( "USA_final_in" )
L101_inEIA_EJ_state_S_F <- readdata( "L101_inEIA_EJ_state_S_F" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Subset the buildings sector from the final energy table
L104_in_EJ_USA_bld_F <- USA_final_in[ USA_final_in$supplysector=="building", ]

#Subset residential and commercial from the SEDS table, and only the fuels that are part of the GCAM buildings sector. Sort by fuel.
printlog( "Calculating each state and sector's (res/comm) shares of USA building energy use, by fuel" )
L104_in_EJ_state_bld_F_unscaled <- L101_inEIA_EJ_state_S_F[ L101_inEIA_EJ_state_S_F$GCAM_sector %in% c( "comm", "resid" ), ]
L104_in_EJ_state_bld_F_unscaled <- L104_in_EJ_state_bld_F_unscaled[ L104_in_EJ_state_bld_F_unscaled$GCAM_fuel %in% L104_in_EJ_USA_bld_F$subsector, ]
L104_in_EJ_state_bld_F_unscaled <- L104_in_EJ_state_bld_F_unscaled[ order( L104_in_EJ_state_bld_F_unscaled$GCAM_fuel ), ]

#Aggregate by fuel to calculate each state and sector's portional allocation
L104_in_EJ_USA_bld_F_unscaled <- aggregate( L104_in_EJ_state_bld_F_unscaled[ X_base_years ],
      list( GCAM_fuel = L104_in_EJ_state_bld_F_unscaled$GCAM_fuel ), sum ) 
L104_in_EJ_USA_bld_F_unscaled_repstatebld <- L104_in_EJ_USA_bld_F_unscaled[ rep( 1:nrow( L104_in_EJ_USA_bld_F_unscaled ),
      length.out = nrow( L104_in_EJ_state_bld_F_unscaled ) ), ]
L104_in_EJ_USA_bld_F_unscaled_repstatebld <- L104_in_EJ_USA_bld_F_unscaled_repstatebld[ order( L104_in_EJ_USA_bld_F_unscaled_repstatebld$GCAM_fuel ), ]

#Calculate the portional allocation of total US buildings energy use (by each fuel) to each state and sector
L104_in_pct_state_bld_F <- data.frame( L104_in_EJ_state_bld_F_unscaled[ state_S_F ],
      L104_in_EJ_state_bld_F_unscaled[ X_base_years ] / L104_in_EJ_USA_bld_F_unscaled_repstatebld[ X_base_years ] ) 

#Repeat the whole-USA table by the appropriate number of times, and sort by fuel
L104_in_EJ_USA_bld_F_repstatebld <- L104_in_EJ_USA_bld_F[ rep( 1:nrow( L104_in_EJ_USA_bld_F ), length.out = nrow( L104_in_EJ_state_bld_F_unscaled ) ), ]
L104_in_EJ_USA_bld_F_repstatebld <- L104_in_EJ_USA_bld_F_repstatebld[ order( L104_in_EJ_USA_bld_F_repstatebld$subsector ), ]

#Multiply the repeated whole-US buildings sector table by the portional allocation table to get building energy by state, sector, and fuel
printlog( "Multiplying USA building energy use by each state and sector's shares" )
L104_in_EJ_state_bld_F <- data.frame( L104_in_pct_state_bld_F[ state_S_F ],
      L104_in_pct_state_bld_F[ X_base_years ] * L104_in_EJ_USA_bld_F_repstatebld[ X_base_years ] )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L104_in_EJ_state_bld_F <- c( "Buildings energy consumption by state, sector (res/comm) and fuel","Unit = EJ" )

#write tables as CSV files
writedata( L104_in_EJ_state_bld_F,fn="L104_in_EJ_state_bld_F", comments=comments.L104_in_EJ_state_bld_F )

# Every script should finish with this line
logstop()