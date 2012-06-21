# L101_EIA_all_sectors.R
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L101_EIA_all_sectors.R" )
printlog( "EIA State Energy Data Systems data aggregated by GCAM fuel and sector" )

# -----------------------------------------------------------------------------
# 1. Read files

EIA_fuels <- readmap( "EIA_fuels" )
EIA_sectors <- readmap( "EIA_sectors" )
EIA_use_all_Bbtu <- readdata( "EIA_use_all_Bbtu" )
A_fuel_conv <- readdata( "A_fuel_conv" )

# -----------------------------------------------------------------------------
# 2. Perform computations

#Drop the whole US, add a 2008 column (if one is not already there), and add lookup vectors for sectors and fuels
printlog( "Removing the whole USA category, and using 2007 data as a proxy for 2008 if necessary" )
L101_EIA_use_all_Bbtu <- EIA_use_all_Bbtu[ EIA_use_all_Bbtu$state %in% states, ]
if( !exists( "L101_EIA_use_all_Bbtu$X2008" ) )
      L101_EIA_use_all_Bbtu$X2008 <- L101_EIA_use_all_Bbtu$X2007
L101_EIA_use_all_Bbtu$GCAM_sector <- EIA_sectors$GCAM_sector[ match( L101_EIA_use_all_Bbtu$eia_sector, EIA_sectors$eia_sector ) ]
L101_EIA_use_all_Bbtu$GCAM_fuel <- EIA_fuels$GCAM_fuel[ match( L101_EIA_use_all_Bbtu$eia_fuel, EIA_fuels$eia_fuel ) ]

#Collapse dataset by sector and fuel. Subset specified years
printlog( "Aggregating EIA state energy data by GCAM sector and fuel" )
L101_inEIA_Bbtu_state_S_F <- aggregate( L101_EIA_use_all_Bbtu[ X_base_years ],
      list( state = L101_EIA_use_all_Bbtu$state, GCAM_sector = L101_EIA_use_all_Bbtu$GCAM_sector,
            GCAM_fuel = L101_EIA_use_all_Bbtu$GCAM_fuel ), sum )
      
#Subset only the portion of the dataset with no "na" values (drop whole-US numbers as well)
L101_inEIA_Bbtu_state_S_F <- L101_inEIA_Bbtu_state_S_F[ L101_inEIA_Bbtu_state_S_F$GCAM_sector != "na" &
      L101_inEIA_Bbtu_state_S_F$GCAM_fuel != "na", ]

#Convert to EJ with fuel-specific multipliers
printlog( "Converting from Bbtu to EJ" )
L101_inEIA_Bbtu_state_S_F$conv <- A_fuel_conv$conv_Bbtu_EJ[ match( L101_inEIA_Bbtu_state_S_F$GCAM_fuel, A_fuel_conv$GCAM_fuel ) ]
L101_inEIA_EJ_state_S_F <- data.frame( L101_inEIA_Bbtu_state_S_F[ state_S_F ],
                           L101_inEIA_Bbtu_state_S_F[ X_base_years ] * L101_inEIA_Bbtu_state_S_F$conv )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L101_EIA_use_all_Bbtu <- c( "EIA state energy database by EIA sector / fuel / year","Unit = EJ" )
comments.L101_inEIA_EJ_state_S_F <- c( "EIA state energy database by GCAM sector / fuel / year","Unit = EJ" )

#write tables as CSV files
writedata( L101_EIA_use_all_Bbtu,fn="L101_EIA_use_all_Bbtu", comments=comments.L101_EIA_use_all_Bbtu )
writedata( L101_inEIA_EJ_state_S_F,fn="L101_inEIA_EJ_state_S_F", comments=comments.L101_inEIA_EJ_state_S_F )

# Every script should finish with this line
logstop()