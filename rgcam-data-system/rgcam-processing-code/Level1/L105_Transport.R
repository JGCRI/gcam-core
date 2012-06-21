# L105_Transport.R
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L105_Transport.R" )
printlog( "Transportation sector energy consumption by state and mode (not service)" )

# -----------------------------------------------------------------------------
# 1. Read files

USA_final_in <- readdata( "USA_final_in" )
L101_inEIA_EJ_state_S_F <- readdata( "L101_inEIA_EJ_state_S_F" )
L101_EIA_use_all_Bbtu <- readdata( "L101_EIA_use_all_Bbtu" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#SHIPPING
printlog( "Energy consumption for international and domestic shipping" )
printlog( "NOTE: Shipping energy is disaggregated to states on the basis of SEDS residual fuel oil use by the transportation sector" )
#Subset shipping from the USA final energy table
L105_in_EJ_USA_trnship_liq <- USA_final_in[ USA_final_in$supplysector %in% c( "trn_domestic ship" , "trn_international ship" ) &
      USA_final_in$subsector == "refined liquids", ]
#Subset the residual fuel oil used by the transportation sector in the specified base years, from the EIA SEDS table
L105_in_Bbtu_state_trn_rf <- L101_EIA_use_all_Bbtu[ L101_EIA_use_all_Bbtu$eia_sector == "AC" & L101_EIA_use_all_Bbtu$eia_fuel == "RF", c( "state", X_base_years ) ]

#Calculate each state's share of residual fuel use by the transportation sector
L105_in_pct_state_trnshipping <- data.frame( state = states, GCAM_sector = "trn_shipping",
      sweep( L105_in_Bbtu_state_trn_rf[ X_base_years ], 2, colSums( L105_in_Bbtu_state_trn_rf[ X_base_years ] ), "/" ) )
      
#Calculate each state's international and domestic shipping
L105_in_pct_state_trnshipping_repship <- L105_in_pct_state_trnshipping[ rep( 1:nrow( L105_in_pct_state_trnshipping ), times = nrow( L105_in_EJ_USA_trnship_liq ) ), ]
L105_in_EJ_USA_trnship_liq_repstate <- L105_in_EJ_USA_trnship_liq[ rep( 1:nrow( L105_in_EJ_USA_trnship_liq ), times = length( states ) ), ]
L105_in_EJ_USA_trnship_liq_repstate <- L105_in_EJ_USA_trnship_liq_repstate[ order( L105_in_EJ_USA_trnship_liq_repstate$supplysector ), ]
L105_in_EJ_state_trnship_liq <- data.frame( state = rep( states, length.out = nrow( L105_in_EJ_USA_trnship_liq_repstate ) ),
      GCAM_sector = L105_in_EJ_USA_trnship_liq_repstate$supplysector, GCAM_fuel = "refined liquids",
      L105_in_EJ_USA_trnship_liq_repstate[ X_base_years ] * L105_in_pct_state_trnshipping_repship[ X_base_years ] )

#AVIATION
printlog( "Energy consumption for aviation" )
printlog( "NOTE: Aviation energy is disaggregated to states on the basis of SEDS jet fuel use by the transportation sector" )
#Subset aviation from the USA final energy table
L105_in_EJ_USA_trnair_liq <- USA_final_in[ USA_final_in$supplysector == "trn_air", ]

L105_in_Bbtu_state_trn_jf <- L101_EIA_use_all_Bbtu[ L101_EIA_use_all_Bbtu$eia_sector == "AC" & L101_EIA_use_all_Bbtu$eia_fuel == "JF", c( "state", X_base_years ) ]

#Calculate each state's share
L105_in_pct_state_trnair <- data.frame( state = states, GCAM_sector = "trn_air",
      sweep( L105_in_Bbtu_state_trn_jf[ X_base_years ], 2, colSums( L105_in_Bbtu_state_trn_jf[ X_base_years ] ), "/" ) )

#Calculate each state's aviation energy use
L105_in_EJ_USA_trnair_liq_repstate <- L105_in_EJ_USA_trnair_liq[ rep( 1, times = length( states ) ), ]
L105_in_EJ_state_trnair_liq <- data.frame( state = states,
      GCAM_sector = "trn_air", GCAM_fuel = "refined liquids",
      L105_in_EJ_USA_trnair_liq_repstate[ X_base_years ] * L105_in_pct_state_trnair[ X_base_years ] )

#SURFACE TRANSPORT MODES
# Road - Gas (CNG Buses)
printlog( "CNG bus energy consumption by state (this is an item in SEDS)" )
#Subset gas buses from the USA final energy table
L105_in_EJ_USA_trnroad_gas <- USA_final_in[ USA_final_in$supplysector == "trn_road" &
      USA_final_in$subsector == "gas", ]

#Subset gas buses from the EIA SEDS database
L105_in_Bbtu_state_trnroad_gas <- L101_EIA_use_all_Bbtu[ L101_EIA_use_all_Bbtu$eia_sector == "VH", c( "state", X_base_years ) ]

#Calculate each state's share
L105_in_pct_state_trnroad_gas <- data.frame( state = states, GCAM_sector = "trn_road",
      sweep( L105_in_Bbtu_state_trnroad_gas[ X_base_years ], 2, colSums( L105_in_Bbtu_state_trnroad_gas[ X_base_years ] ), "/" ) )
L105_in_pct_state_trnroad_gas[ is.na( L105_in_pct_state_trnroad_gas ) ] <- 0

#Calculate each state's use of natural gas for road transport (buses)
L105_in_EJ_USA_trnroad_gas_repstate <- L105_in_EJ_USA_trnroad_gas[ rep( 1, times = length( states ) ), ]
L105_in_EJ_state_trnroad_gas <- data.frame( state = states,
      GCAM_sector = "trn_road", GCAM_fuel = "gas",
      L105_in_EJ_USA_trnroad_gas_repstate[ X_base_years ] * L105_in_pct_state_trnroad_gas[ X_base_years ] )

# ELECTRIC RAIL
printlog( "Electric rail energy consumption" )
printlog( "NOTE: Electric rail energy is disaggregated to states on the basis of SEDS electricity use by the transportation sector" )
#Subset electric rail from the surface transport table
L105_in_EJ_USA_trnrail_elec <- USA_final_in[ USA_final_in$supplysector == "trn_rail" &
      USA_final_in$subsector == "electricity", ]

#Subset electric rail from the EIA SEDS database
L105_in_Bbtu_state_trnrail_elec <- L101_EIA_use_all_Bbtu[ L101_EIA_use_all_Bbtu$eia_sector == "AC" &
      L101_EIA_use_all_Bbtu$eia_fuel == "ES", c( "state", X_base_years ) ]

#Calculate each state's share
L105_in_pct_state_trnrail_elec <- data.frame( state = states, GCAM_sector = "trn_rail",
      sweep( L105_in_Bbtu_state_trnrail_elec[ X_base_years ], 2, colSums( L105_in_Bbtu_state_trnrail_elec[ X_base_years ] ), "/", ) )

#Calculate each state's use of electricity for rail transport
L105_in_EJ_USA_trnrail_elec_repstate <- L105_in_EJ_USA_trnrail_elec[ rep( 1, times = length( states ) ), ]
L105_in_EJ_state_trnrail_elec <- data.frame( state = states,
      GCAM_sector = "trn_rail", GCAM_fuel = "electricity",
      L105_in_EJ_USA_trnrail_elec_repstate[ X_base_years ] * L105_in_pct_state_trnrail_elec[ X_base_years ] )

#RAIL AND ROAD - LIQUID FUELS
printlog( "Liquid fuel consumption by rail and road" )
printlog( "NOTE: This is disaggregated to states on the basis of all liquid fuel consumption by transport, minus quantities already accounted" )
printlog( "NOTE: The remainder is assigned a constant rail:road share based on the shares in the whole US" )
#Subset all transport techs that use liquid fuels from the USA final energy table
L105_in_EJ_USA_trnall_liq <- USA_final_in[ grepl( "trn", USA_final_in$supplysector ) &
      USA_final_in$subsector == "refined liquids", ]
#Aggregate all modes
L105_in_EJ_USA_trn_liq <- aggregate( L105_in_EJ_USA_trnall_liq[ X_base_years ],
      list( L105_in_EJ_USA_trnall_liq$subsector ), sum )

#Subset the liquid fuels used by the transportation sector in the specified base years
L105_inEIA_EJ_state_trn_liq <- L101_inEIA_EJ_state_S_F[ L101_inEIA_EJ_state_S_F$GCAM_sector == "trn_all" &
      L101_inEIA_EJ_state_S_F$GCAM_fuel == "refined liquids", ]

#Calculate each state's share
L105_in_pct_state_trn_liq <- data.frame( state = states, GCAM_sector = "trn_all", GCAM_fuel = "refined liquids",
      sweep( L105_inEIA_EJ_state_trn_liq[ X_base_years ], 2, colSums( L105_inEIA_EJ_state_trn_liq[ X_base_years ] ), "/", ) )

#Calculate each state's total refined liquid fuel consumption by transport
L105_in_EJ_USA_trn_liq_repstate <- L105_in_EJ_USA_trn_liq[ rep( 1, times = length( states ) ), ]
L105_in_EJ_state_trn_liq <- data.frame( state = states,
      GCAM_sector = "trn_all", GCAM_fuel = "refined liquids",
      L105_in_EJ_USA_trn_liq_repstate[ X_base_years ] * L105_in_pct_state_trn_liq[ X_base_years ] )

#Deduct the energy consumption already specified (by other modes)
L105_in_EJ_state_trnsurface_liq <- data.frame( state = states,
      GCAM_sector = "trn_surface", GCAM_fuel = "refined liquids",
      L105_in_EJ_state_trn_liq[ X_base_years ] -  L105_in_EJ_state_trnair_liq[ X_base_years ] -
          L105_in_EJ_state_trnship_liq[ L105_in_EJ_state_trnship_liq$GCAM_sector == "trn_domestic ship", X_base_years ] -
          L105_in_EJ_state_trnship_liq[ L105_in_EJ_state_trnship_liq$GCAM_sector == "trn_international ship", X_base_years ] )

#Calculate the shares of road vs rail in the whole USA, in each time period
L105_share_USA_trnroad_surface <- L105_in_EJ_USA_trnall_liq[ L105_in_EJ_USA_trnall_liq$supplysector == "trn_road", ]
L105_share_USA_trnroad_surface[ X_base_years ] <- L105_in_EJ_USA_trnall_liq[ L105_in_EJ_USA_trnall_liq$supplysector == "trn_road", X_base_years ] /
                                                ( L105_in_EJ_USA_trnall_liq[ L105_in_EJ_USA_trnall_liq$supplysector == "trn_road", X_base_years ] +
                                                  L105_in_EJ_USA_trnall_liq[ L105_in_EJ_USA_trnall_liq$supplysector == "trn_rail", X_base_years ] )
L105_share_USA_trnroad_surface_repstate <- L105_share_USA_trnroad_surface[ rep( 1:nrow( L105_share_USA_trnroad_surface ), times = length( states ) ), ]

#Calculate road energy use as the surface total times the road share
L105_in_EJ_state_trnroad_liq <- data.frame( state = states,
      GCAM_sector = "trn_road", GCAM_fuel = "refined liquids",
      L105_in_EJ_state_trnsurface_liq[ X_base_years ] * L105_share_USA_trnroad_surface_repstate[ X_base_years ] )
L105_in_EJ_state_trnrail_liq <- data.frame( state = states,
      GCAM_sector = "trn_rail", GCAM_fuel = "refined liquids",
      L105_in_EJ_state_trnsurface_liq[ X_base_years ] - L105_in_EJ_state_trnroad_liq[ X_base_years ] )

#COMPILE ALL TRANSPORTATION TABLES
L105_in_EJ_state_trn_F <- rbind(  L105_in_EJ_state_trnair_liq, L105_in_EJ_state_trnship_liq, L105_in_EJ_state_trnrail_elec,
                                 L105_in_EJ_state_trnrail_liq, L105_in_EJ_state_trnroad_gas, L105_in_EJ_state_trnroad_liq )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L105_in_EJ_state_trn_F <- c( "Transportation energy consumption by state, mode (not service) and fuel","Unit = EJ" )

#write tables as CSV files
writedata( L105_in_EJ_state_trn_F,fn="L105_in_EJ_state_trn_F", comments=comments.L105_in_EJ_state_trn_F )

# Every script should finish with this line
logstop()