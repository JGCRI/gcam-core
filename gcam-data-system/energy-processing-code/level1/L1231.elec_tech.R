
if( !exists( "ENERGYPROC_DIR" ) ){
    if( Sys.getenv( "ENERGYPROC" ) != "" ){
        ENERGYPROC_DIR <- Sys.getenv( "ENERGYPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var ENERGYPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
logstart( "L1231.elec_tech.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Disaggregation of historical electricity production into specific technologies" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
A23.globaltech_eff <- readdata( "ENERGY_ASSUMPTIONS", "A23.globaltech_eff" )
calibrated_techs <- readdata( "ENERGY_MAPPINGS", "calibrated_techs" )
L123.in_EJ_R_elec_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L123.in_EJ_R_elec_F_Yh" )
L123.out_EJ_R_elec_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L123.out_EJ_R_elec_F_Yh" )
L123.eff_R_elec_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L123.eff_R_elec_F_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. Natural gas: Disaggregate to CC and CT/steam on the basis of assumed efficiencies
#Subset and melt the actual efficiencies of gas -> electricity
L1231.eff_R_elec_gas_Yh.melt <- interpolate_and_melt( subset( L123.eff_R_elec_F_Yh, fuel == "gas" ), historical_years, value.name = "efficiency" )

#Subset and melt the assumed efficiencies of gas technologies
L1231.eff_R_elec_gas_tech <- A23.globaltech_eff[ A23.globaltech_eff$subsector == "gas" &
      vecpaste( A23.globaltech_eff[ s_s_t] ) %in% vecpaste( calibrated_techs[ s_s_t ] ), ]
L1231.eff_R_elec_gas_tech.melt <- interpolate_and_melt( L1231.eff_R_elec_gas_tech, historical_years, value.name = "efficiency_tech" )

#Match the assumed tech efficiencies into the table of actual historical efficiencies
L1231.eff_R_elec_gas_Yh.melt$efficiency_tech1 <- L1231.eff_R_elec_gas_tech.melt$efficiency_tech[
      match( paste( L1231.eff_R_elec_gas_Yh.melt$year, unique( L1231.eff_R_elec_gas_tech.melt$technology[1] ) ),
             paste( L1231.eff_R_elec_gas_tech.melt$year, L1231.eff_R_elec_gas_tech.melt$technology ) ) ]
L1231.eff_R_elec_gas_Yh.melt$efficiency_tech2 <- L1231.eff_R_elec_gas_tech.melt$efficiency_tech[
      match( paste( L1231.eff_R_elec_gas_Yh.melt$year, unique( L1231.eff_R_elec_gas_tech.melt$technology[2] ) ),
             paste( L1231.eff_R_elec_gas_tech.melt$year, L1231.eff_R_elec_gas_tech.melt$technology ) ) ]

#Reset upper and lower bound efficiencies, as needed
##Where avg efficiency is outside of the range of the two technologies, set the upper or lower one to the average. This will take 100% of the market share.
L1231.eff_R_elec_gas_Yh.melt$efficiency_tech1[ L1231.eff_R_elec_gas_Yh.melt$efficiency < L1231.eff_R_elec_gas_Yh.melt$efficiency_tech1 ] <-
      L1231.eff_R_elec_gas_Yh.melt$efficiency[ L1231.eff_R_elec_gas_Yh.melt$efficiency < L1231.eff_R_elec_gas_Yh.melt$efficiency_tech1 ]
L1231.eff_R_elec_gas_Yh.melt$efficiency_tech2[ L1231.eff_R_elec_gas_Yh.melt$efficiency > L1231.eff_R_elec_gas_Yh.melt$efficiency_tech2 ] <-
      L1231.eff_R_elec_gas_Yh.melt$efficiency[ L1231.eff_R_elec_gas_Yh.melt$efficiency > L1231.eff_R_elec_gas_Yh.melt$efficiency_tech2 ]
      
#Cast efficiency table to final format
L1231.eff_R_elec_gas_tech1_Yh <- cast( L1231.eff_R_elec_gas_Yh.melt, GCAM_region_ID + sector + fuel ~ variable, value = "efficiency_tech1")
L1231.eff_R_elec_gas_tech1_Yh$technology <- unique( L1231.eff_R_elec_gas_tech.melt$technology[1] )
L1231.eff_R_elec_gas_tech2_Yh <- cast( L1231.eff_R_elec_gas_Yh.melt, GCAM_region_ID + sector + fuel ~ variable, value = "efficiency_tech2")
L1231.eff_R_elec_gas_tech2_Yh$technology <- unique( L1231.eff_R_elec_gas_tech.melt$technology[2] )
L1231.eff_R_elec_gas_tech_Yh <- rbind( L1231.eff_R_elec_gas_tech1_Yh, L1231.eff_R_elec_gas_tech2_Yh )[ c( R_S_F_tech, X_historical_years ) ]

#Solve for share of first technology
L1231.eff_R_elec_gas_Yh.melt$share_tech1 <- with( L1231.eff_R_elec_gas_Yh.melt,
      (efficiency - efficiency_tech2) / ( efficiency_tech1 - efficiency_tech2 ) )

#Multiply share by input
L1231.in_EJ_R_elec_gas_Yh.melt <- interpolate_and_melt( subset( L123.in_EJ_R_elec_F_Yh, fuel == "gas" ), historical_years, value.name = "in_EJ" )
L1231.in_EJ_R_elec_gas_Yh.melt$share_tech1 <- L1231.eff_R_elec_gas_Yh.melt$share_tech1[
      match( vecpaste( L1231.in_EJ_R_elec_gas_Yh.melt[ R_Y ] ), vecpaste( L1231.eff_R_elec_gas_Yh.melt[ R_Y ] ) ) ]
L1231.in_EJ_R_elec_gas_Yh.melt$in_EJ_tech1 <- L1231.in_EJ_R_elec_gas_Yh.melt$in_EJ * L1231.in_EJ_R_elec_gas_Yh.melt$share_tech1
L1231.in_EJ_R_elec_gas_Yh.melt$in_EJ_tech2 <- L1231.in_EJ_R_elec_gas_Yh.melt$in_EJ - L1231.in_EJ_R_elec_gas_Yh.melt$in_EJ_tech1

#Cast to final format
L1231.in_EJ_R_elec_gas_tech1_Yh <- cast( L1231.in_EJ_R_elec_gas_Yh.melt, GCAM_region_ID + sector + fuel ~ variable, value = "in_EJ_tech1" )
L1231.in_EJ_R_elec_gas_tech1_Yh$technology <- unique( L1231.eff_R_elec_gas_tech.melt$technology[1] )
L1231.in_EJ_R_elec_gas_tech2_Yh <- cast( L1231.in_EJ_R_elec_gas_Yh.melt, GCAM_region_ID + sector + fuel ~ variable, value = "in_EJ_tech2" )
L1231.in_EJ_R_elec_gas_tech2_Yh$technology <- unique( L1231.eff_R_elec_gas_tech.melt$technology[2] )
L1231.in_EJ_R_elec_gas_tech_Yh <- rbind( L1231.in_EJ_R_elec_gas_tech1_Yh, L1231.in_EJ_R_elec_gas_tech2_Yh)[ c( R_S_F_tech, X_historical_years ) ]

#2b. All other (non-gas) technologies are not disaggregated further (only one tech per fuel type)
L1231.eff_R_elec_Fnogas_tech_Yh <- subset( L123.eff_R_elec_F_Yh, fuel != "gas" )
L1231.eff_R_elec_Fnogas_tech_Yh$technology <- calibrated_techs$technology[
      match( vecpaste( L1231.eff_R_elec_Fnogas_tech_Yh[ S_F ] ), vecpaste( calibrated_techs[ S_F ] ) ) ]
L1231.eff_R_elec_F_tech_Yh <- rbind( L1231.eff_R_elec_gas_tech_Yh, L1231.eff_R_elec_Fnogas_tech_Yh[ c( R_S_F_tech, X_historical_years ) ] )
L1231.eff_R_elec_F_tech_Yh <- L1231.eff_R_elec_F_tech_Yh[
      order( L1231.eff_R_elec_F_tech_Yh$fuel, L1231.eff_R_elec_F_tech_Yh$technology, L1231.eff_R_elec_F_tech_Yh$GCAM_region_ID ), ]

L1231.in_EJ_R_elec_Fnogas_tech_Yh <- subset( L123.in_EJ_R_elec_F_Yh, fuel != "gas" )
L1231.in_EJ_R_elec_Fnogas_tech_Yh$technology <- calibrated_techs$technology[
      match( vecpaste( L1231.in_EJ_R_elec_Fnogas_tech_Yh[ S_F ] ), vecpaste( calibrated_techs[ S_F ] ) ) ]
L1231.in_EJ_R_elec_F_tech_Yh <- rbind( L1231.in_EJ_R_elec_gas_tech_Yh, L1231.in_EJ_R_elec_Fnogas_tech_Yh[ c( R_S_F_tech, X_historical_years ) ] )
L1231.in_EJ_R_elec_F_tech_Yh <- L1231.in_EJ_R_elec_F_tech_Yh[ order( L1231.in_EJ_R_elec_F_tech_Yh$fuel, L1231.in_EJ_R_elec_F_tech_Yh$technology ), ]

#Calculate output as input * efficiency
#This table only includes technologies with modeled efficiencies
L1231.out_EJ_R_elec_Fin_tech_Yh <- L1231.eff_R_elec_F_tech_Yh
L1231.out_EJ_R_elec_Fin_tech_Yh[ X_historical_years ] <- L1231.in_EJ_R_elec_F_tech_Yh[
      match( vecpaste( L1231.out_EJ_R_elec_Fin_tech_Yh[ R_S_F_tech ] ), vecpaste( L1231.in_EJ_R_elec_F_tech_Yh[ R_S_F_tech ] ) ), X_historical_years ] *
      L1231.eff_R_elec_F_tech_Yh[ X_historical_years ]

#Combine with technologies modeled by output only (e.g. nuclear, hydro, renewables)
L1231.out_EJ_R_elec_Fout_tech_Yh <- subset(L123.out_EJ_R_elec_F_Yh, !fuel %in% L1231.out_EJ_R_elec_Fin_tech_Yh$fuel )
L1231.out_EJ_R_elec_Fout_tech_Yh$technology <- calibrated_techs$technology[
      match( vecpaste( L1231.out_EJ_R_elec_Fout_tech_Yh[ S_F ] ), vecpaste( calibrated_techs[ S_F ] ) ) ]
L1231.out_EJ_R_elec_F_tech_Yh <- rbind( L1231.out_EJ_R_elec_Fin_tech_Yh, L1231.out_EJ_R_elec_Fout_tech_Yh )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L1231.in_EJ_R_elec_F_tech_Yh <- c( "Outputs of electricity sector by GCAM region / fuel / technology / historical year","Unit = EJ" )
comments.L1231.out_EJ_R_elec_F_tech_Yh <- c( "Inputs to electricity sector by GCAM region / fuel / technology / historical year","Unit = EJ" )
comments.L1231.eff_R_elec_F_tech_Yh <- c( "Electric sector efficiencies by GCAM region / fuel / technology / historical year","Unitless IO" )

#write tables as CSV files
writedata( L1231.in_EJ_R_elec_F_tech_Yh, domain="ENERGY_LEVEL1_DATA", fn="L1231.in_EJ_R_elec_F_tech_Yh", comments=comments.L1231.in_EJ_R_elec_F_tech_Yh )
writedata( L1231.out_EJ_R_elec_F_tech_Yh, domain="ENERGY_LEVEL1_DATA", fn="L1231.out_EJ_R_elec_F_tech_Yh", comments=comments.L1231.out_EJ_R_elec_F_tech_Yh )
writedata( L1231.eff_R_elec_F_tech_Yh, domain="ENERGY_LEVEL1_DATA", fn="L1231.eff_R_elec_F_tech_Yh", comments=comments.L1231.eff_R_elec_F_tech_Yh )

# Every script should finish with this line
logstop()
