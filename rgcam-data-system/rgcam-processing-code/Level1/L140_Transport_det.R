# L140_Transport_det.R
# Split transportation calibration data by detailed modes/technologies
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L140_Transport_det.R" )
printlog( "States detailed transportation" )

# -----------------------------------------------------------------------------
# 1. Read files

USA_trn_det_in <- readdata( "USA_trn_det_in" )
USA_TEDB_rail_road_Tbtu <- readdata( "TEDB_rail_road_Tbtu" )
L105_in_EJ_state_trn_F <- readdata( "L105_in_EJ_state_trn_F" )

# Shipping: add service vectors to existing tables
printlog( "Domestic and international shipping" )
L140_in_EJ_state_trnfreight_domship_F <- L105_in_EJ_state_trn_F[ L105_in_EJ_state_trn_F$GCAM_sector == "trn_domestic ship", ]
L140_in_EJ_state_trnfreight_domship_F$GCAM_sector <- as.factor( "trn_freight" )
L140_in_EJ_state_trnfreight_domship_F$GCAM_mode <- as.factor( "domestic ship" )
L140_in_EJ_state_trnfreight_domship_F <- L140_in_EJ_state_trnfreight_domship_F[,
    c( "state", "GCAM_sector", "GCAM_mode", "GCAM_fuel", X_base_years ) ]

L140_in_EJ_state_trnfreight_intship_F <- L105_in_EJ_state_trn_F[ L105_in_EJ_state_trn_F$GCAM_sector == "trn_international ship", ]
L140_in_EJ_state_trnfreight_intship_F$GCAM_sector <- as.factor( "trn_shipping_intl" )
L140_in_EJ_state_trnfreight_intship_F$GCAM_mode <- as.factor( "international ship" )
L140_in_EJ_state_trnfreight_intship_F <- L140_in_EJ_state_trnfreight_intship_F[,
    c( "state", "GCAM_sector", "GCAM_mode", "GCAM_fuel", X_base_years ) ]

# Aviation: assign aviation energy use by state to freight and passenger services using fixed shares
printlog( "Split aviation energy between freight and passenger services" )
L140_USA_trnair_freight <- USA_trn_det_in$X2005[ USA_trn_det_in$supplysector=="trn_freight" & USA_trn_det_in$tranSubsector=="air" ]
L140_USA_trnair_pass <- USA_trn_det_in$X2005[USA_trn_det_in$supplysector=="trn_passenger" & USA_trn_det_in$tranSubsector=="air"]
L140_USA_trnair_freightfrac <- L140_USA_trnair_freight / sum( L140_USA_trnair_freight, L140_USA_trnair_pass )

L140_in_EJ_state_all_air_F <- L105_in_EJ_state_trn_F[ L105_in_EJ_state_trn_F$GCAM_sector == "trn_air", ]
L140_in_EJ_state_trnfreight_air_F <- L140_in_EJ_state_all_air_F
L140_in_EJ_state_trnfreight_air_F$GCAM_sector <- as.factor( "trn_freight" )
L140_in_EJ_state_trnfreight_air_F$GCAM_mode <- as.factor( "air" )
L140_in_EJ_state_trnfreight_air_F <- L140_in_EJ_state_trnfreight_air_F[,
    c( "state", "GCAM_sector", "GCAM_mode", "GCAM_fuel", X_base_years ) ]
L140_in_EJ_state_trnfreight_air_F[,X_base_years] <- L140_in_EJ_state_all_air_F[,X_base_years] * L140_USA_trnair_freightfrac

L140_in_EJ_state_trnpass_air_F <- L140_in_EJ_state_trnfreight_air_F
L140_in_EJ_state_trnpass_air_F$GCAM_sector <- as.factor( "trn_passenger" )
L140_in_EJ_state_trnpass_air_F[,X_base_years]<-L140_in_EJ_state_all_air_F[,X_base_years] * (1 - L140_USA_trnair_freightfrac)

# CNG BUSES
printlog( "Categorize gas in road as CNG bus" )
L140_in_EJ_state_trnroad_bus_gas <- L105_in_EJ_state_trn_F[ L105_in_EJ_state_trn_F$GCAM_sector == "trn_road"
    & L105_in_EJ_state_trn_F$GCAM_fuel == "gas", ]
L140_in_EJ_state_trnroad_bus_gas$GCAM_sector <- as.factor( "trn_pass_road" )
L140_in_EJ_state_trnroad_bus_gas$GCAM_mode <- as.factor( "bus" )
L140_in_EJ_state_trnroad_bus_gas <- L140_in_EJ_state_trnroad_bus_gas[,
    c( "state", "GCAM_sector", "GCAM_mode", "GCAM_fuel", X_base_years ) ]

# SURFACE TRANSPORT (RAIL ONLY) - ELECTRICITY
printlog( "Categorize rail - electricity" )
L140_in_EJ_state_trnpass_rail_elec <- L105_in_EJ_state_trn_F[ L105_in_EJ_state_trn_F$GCAM_sector == "trn_rail"
    & L105_in_EJ_state_trn_F$GCAM_fuel == "electricity", ]
L140_in_EJ_state_trnpass_rail_elec$GCAM_sector <- as.factor( "trn_passenger" )
L140_in_EJ_state_trnpass_rail_elec$GCAM_mode <- as.factor( "rail" )
L140_in_EJ_state_trnpass_rail_elec <- L140_in_EJ_state_trnpass_rail_elec[,
    c( "state", "GCAM_sector", "GCAM_mode", "GCAM_fuel", X_base_years ) ]

# SURFACE TRANSPORT - RAIL - LIQUID FUELS
# Compute historical freight fraction of rail energy use
printlog( "Categorize rail - refined liquids" )
L140_USA_trnrail_Tbtu <- USA_TEDB_rail_road_Tbtu[ USA_TEDB_rail_road_Tbtu$tranSubsector=="rail", ]
L140_USA_trnrail_Tbtu_tot <- aggregate( L140_USA_trnrail_Tbtu[,X_base_years],
    list( tranSubsector=L140_USA_trnrail_Tbtu$tranSubsector ), sum )
L140_USA_trnrail_freightfrac <- L140_USA_trnrail_Tbtu_tot
L140_USA_trnrail_freightfrac[ ,X_base_years ] <- L140_USA_trnrail_Tbtu[
    L140_USA_trnrail_Tbtu$supplysector == "trn_freight", X_base_years ] / L140_USA_trnrail_Tbtu_tot[, X_base_years ]

# Repeat by number of states and multiply through
L140_in_EJ_state_trnrail_liq <- L105_in_EJ_state_trn_F[ L105_in_EJ_state_trn_F$GCAM_sector == "trn_rail"
    & L105_in_EJ_state_trn_F$GCAM_fuel == "refined liquids", ]
L140_state_trnrail_freightfrac <- L140_USA_trnrail_freightfrac[ rep( 1, times=( nrow( L140_in_EJ_state_trnrail_liq ) ) ), ]

L140_in_EJ_state_trnfreight_rail_F <- L140_in_EJ_state_trnrail_liq
L140_in_EJ_state_trnfreight_rail_F$GCAM_sector <- as.factor( "trn_freight" )
L140_in_EJ_state_trnfreight_rail_F$GCAM_mode <- as.factor( "rail" )
L140_in_EJ_state_trnfreight_rail_F <- L140_in_EJ_state_trnfreight_rail_F[,
    c( "state", "GCAM_sector", "GCAM_mode", "GCAM_fuel", X_base_years ) ]
L140_in_EJ_state_trnfreight_rail_F[, X_base_years ] <- L140_in_EJ_state_trnrail_liq[, X_base_years ] *
    L140_state_trnrail_freightfrac[, X_base_years ]

L140_in_EJ_state_trnpass_rail_liq <- L140_in_EJ_state_trnrail_liq
L140_in_EJ_state_trnpass_rail_liq$GCAM_sector <- as.factor( "trn_passenger" )
L140_in_EJ_state_trnpass_rail_liq$GCAM_mode <- as.factor( "rail" )
L140_in_EJ_state_trnpass_rail_liq <- L140_in_EJ_state_trnpass_rail_liq[,
    c( "state", "GCAM_sector", "GCAM_mode", "GCAM_fuel", X_base_years ) ]
L140_in_EJ_state_trnpass_rail_liq[, X_base_years ] <- L140_in_EJ_state_trnrail_liq[, X_base_years ] *
    ( 1 - L140_state_trnrail_freightfrac[, X_base_years ] )

# SURFACE TRANSPORT - ROAD - LIQUID FUELS
printlog( "Categorize road - refined liquids" )
L140_USA_trnroad_Tbtu <- USA_TEDB_rail_road_Tbtu[ USA_TEDB_rail_road_Tbtu$tranSubsector != "rail", ]
L140_USA_trnroad_Tbtu$tranSubsector2 <- as.factor( "road" )
L140_USA_trnroad_Tbtu_tot <- aggregate( L140_USA_trnroad_Tbtu[, X_base_years ],
    list( tranSubsector2=L140_USA_trnroad_Tbtu$tranSubsector2 ), sum )

# Freight
L140_USA_trnroad_freightfrac <- L140_USA_trnroad_Tbtu_tot
L140_USA_trnroad_freightfrac[, X_base_years ] <- L140_USA_trnroad_Tbtu[
    L140_USA_trnroad_Tbtu$supplysector == "trn_freight", X_base_years ] / L140_USA_trnroad_Tbtu_tot[, X_base_years ]

# Repeat by number of states and multiply through
L140_in_EJ_state_trnroad_liq <- L105_in_EJ_state_trn_F[ L105_in_EJ_state_trn_F$GCAM_sector == "trn_road"
    & L105_in_EJ_state_trn_F$GCAM_fuel == "refined liquids", ]
L140_state_trnroad_freightfrac <- L140_USA_trnroad_freightfrac[ rep( 1, times=( nrow( L140_in_EJ_state_trnroad_liq ) ) ), ]

L140_in_EJ_state_trnfreight_road_F <- L140_in_EJ_state_trnroad_liq
L140_in_EJ_state_trnfreight_road_F$GCAM_sector <- as.factor( "trn_freight" )
L140_in_EJ_state_trnfreight_road_F$GCAM_mode <- as.factor( "road" )
L140_in_EJ_state_trnfreight_road_F <- L140_in_EJ_state_trnfreight_road_F[,
    c( "state", "GCAM_sector", "GCAM_mode", "GCAM_fuel", X_base_years ) ]
L140_in_EJ_state_trnfreight_road_F[, X_base_years ] <- L140_in_EJ_state_trnroad_liq[, X_base_years ] *
    L140_state_trnroad_freightfrac[, X_base_years ]

# Bus
L140_USA_trnroad_busfrac <- L140_USA_trnroad_Tbtu_tot
L140_USA_trnroad_busfrac[, X_base_years ] <- L140_USA_trnroad_Tbtu[
    L140_USA_trnroad_Tbtu$tranSubsector == "bus", X_base_years ] / L140_USA_trnroad_Tbtu_tot[, X_base_years ]

# Repeat by number of states and multiply through
L140_state_trnroad_busfrac <- L140_USA_trnroad_busfrac[ rep( 1, times=( nrow( L140_in_EJ_state_trnroad_liq ) ) ), ]

L140_in_EJ_state_trnroad_bus_liq <- L140_in_EJ_state_trnroad_liq
L140_in_EJ_state_trnroad_bus_liq$GCAM_sector <- as.factor( "trn_pass_road" )
L140_in_EJ_state_trnroad_bus_liq$GCAM_mode <- as.factor( "bus" )
L140_in_EJ_state_trnroad_bus_liq <- L140_in_EJ_state_trnroad_bus_liq[,
    c( "state", "GCAM_sector", "GCAM_mode", "GCAM_fuel", X_base_years ) ]
L140_in_EJ_state_trnroad_bus_liq[, X_base_years ] <- L140_in_EJ_state_trnroad_liq[, X_base_years ] *
    L140_state_trnroad_busfrac[, X_base_years ]

# LDV
L140_USA_trnroad_LDVfrac <- L140_USA_trnroad_Tbtu_tot
L140_USA_trnroad_LDVfrac[, X_base_years ] <- L140_USA_trnroad_Tbtu[
    L140_USA_trnroad_Tbtu$tranSubsector == "LDV", X_base_years ] / L140_USA_trnroad_Tbtu_tot[, X_base_years ]

# Repeat by number of states and multiply through
L140_state_trnroad_LDVfrac <- L140_USA_trnroad_LDVfrac[ rep( 1, times=( nrow( L140_in_EJ_state_trnroad_liq ) ) ), ]

L140_in_EJ_state_trnroad_LDV_liq <- L140_in_EJ_state_trnroad_liq
L140_in_EJ_state_trnroad_LDV_liq$GCAM_sector <- as.factor( "trn_pass_road" )
L140_in_EJ_state_trnroad_LDV_liq$GCAM_mode <- as.factor( "LDV" )
L140_in_EJ_state_trnroad_LDV_liq <- L140_in_EJ_state_trnroad_LDV_liq[,
    c( "state", "GCAM_sector", "GCAM_mode", "GCAM_fuel", X_base_years ) ]
L140_in_EJ_state_trnroad_LDV_liq[, X_base_years ] <- L140_in_EJ_state_trnroad_liq[, X_base_years ] * 
    L140_state_trnroad_LDVfrac[, X_base_years ]

# COMPILE ALL TRANSPORTATION MODES AND FUELS INTO A SINGLE TABLE, ADD ID VECTOR, AND WRITE IT OUT
printlog( "Aggregate data from all modes and services into a single table" )
L140_in_EJ_state_trn_det_F <- rbind( L140_in_EJ_state_trnfreight_air_F,
                                     L140_in_EJ_state_trnfreight_domship_F,
                                     L140_in_EJ_state_trnfreight_rail_F,
                                     L140_in_EJ_state_trnfreight_road_F,
                                     L140_in_EJ_state_trnpass_air_F,
                                     L140_in_EJ_state_trnpass_rail_elec,
                                     L140_in_EJ_state_trnpass_rail_liq,
                                     L140_in_EJ_state_trnroad_bus_gas,
                                     L140_in_EJ_state_trnroad_bus_liq,
                                     L140_in_EJ_state_trnroad_LDV_liq,
                                     L140_in_EJ_state_trnfreight_intship_F )
L140_in_EJ_state_trn_det_F$ID_state_S_M_F <- paste( L140_in_EJ_state_trn_det_F$state, L140_in_EJ_state_trn_det_F$GCAM_sector,
    L140_in_EJ_state_trn_det_F$GCAM_mode, L140_in_EJ_state_trn_det_F$GCAM_fuel, sep="" )

# Aggregate by subregion
# TODO: note sure if this is necessary
#in_EJ_state_trn_det_F$GCAM_subregion<-states_subregions$subregion12[match(in_EJ_state_trn_det_F$state,states_subregions$state)]
#in_EJ_sR_trn_det_F<-aggregate(in_EJ_state_trn_det_F[,5:12],list(GCAM_subregion = in_EJ_state_trn_det_F$GCAM_subregion,GCAM_sector = in_EJ_state_trn_det_F$GCAM_sector,GCAM_mode = in_EJ_state_trn_det_F$GCAM_mode,GCAM_fuel = in_EJ_state_trn_det_F$GCAM_fuel),sum)
#in_EJ_sR_trn_det_F<-in_EJ_sR_trn_det_F[order(in_EJ_sR_trn_det_F$GCAM_subregion,in_EJ_sR_trn_det_F$GCAM_sector,in_EJ_sR_trn_det_F$GCAM_mode,in_EJ_sR_trn_det_F$GCAM_fuel),]
#in_EJ_sR_trn_det_F$ID_sR_S_M_F<-paste(in_EJ_sR_trn_det_F$GCAM_subregion,in_EJ_sR_trn_det_F$GCAM_sector,in_EJ_sR_trn_det_F$GCAM_mode,in_EJ_sR_trn_det_F$GCAM_fuel,sep="")
#write.table(in_EJ_sR_trn_det_F,file="Rdata_out/in_EJ_sR_trn_det_F.csv",sep=",",col.names=TRUE,row.names=FALSE)

# -----------------------------------------------------------------------------
# 3. Output
# Add comments for each table
comments.L140_in_EJ_state_trn_det_F<- c( "Transportation fuel by detailed service / fuel / year","Unit = EJ" )

# write tables as CSV files
writedata( L140_in_EJ_state_trn_det_F, fn="L140_in_EJ_state_trn_det_F", comments=comments.L140_in_EJ_state_trn_det_F )

# Every script should finish with this line
logstop()

