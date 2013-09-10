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
logstart( "L123.electricity.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Historical inputs, outputs, and IO coefficients of electricity (incl. CHP)" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "COMMON_ASSUMPTIONS", "unit_conversions", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
enduse_fuel_aggregation <- readdata( "ENERGY_MAPPINGS", "enduse_fuel_aggregation" )
A23.chp_elecratio <- readdata( "ENERGY_ASSUMPTIONS", "A23.chp_elecratio" )
L1011.en_bal_EJ_R_Si_Fi_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L1011.en_bal_EJ_R_Si_Fi_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
# 2a. CENTRAL ELECTRICITY
printlog( "Electricity output (central only): aggregating intermediate fuels as specified in ", file_fqn( "ENERGY_MAPPINGS", "enduse_fuel_aggregation" ) )
L123.out_EJ_R_elec_F_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "out_electricity generation" )
L123.out_EJ_R_elec_F_Yh$sector <- sub( "out_", "", L123.out_EJ_R_elec_F_Yh$sector )
L123.out_EJ_R_elec_F_Yh$fuel <- enduse_fuel_aggregation$electricity[ match( L123.out_EJ_R_elec_F_Yh$fuel, enduse_fuel_aggregation$fuel ) ]
L123.out_EJ_R_elec_F_Yh <- aggregate( L123.out_EJ_R_elec_F_Yh[ X_historical_years ], by=as.list( L123.out_EJ_R_elec_F_Yh[ R_S_F ] ), sum )

printlog( "Fuel inputs to electricity: aggregating intermediate fuels" )
L123.in_EJ_R_elec_F_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "in_electricity generation" )
L123.in_EJ_R_elec_F_Yh$sector <- sub( "in_", "", L123.in_EJ_R_elec_F_Yh$sector )
L123.in_EJ_R_elec_F_Yh$fuel <- enduse_fuel_aggregation$electricity[ match( L123.in_EJ_R_elec_F_Yh$fuel, enduse_fuel_aggregation$fuel ) ]
L123.in_EJ_R_elec_F_Yh <- subset( L123.in_EJ_R_elec_F_Yh, fuel %in% electricity_input_fuels )
L123.in_EJ_R_elec_F_Yh <- aggregate( L123.in_EJ_R_elec_F_Yh[ X_historical_years ], by=as.list( L123.in_EJ_R_elec_F_Yh[ R_S_F ] ), sum )

printlog( "Calculating efficiencies whose inputs are considered")
L123.eff_R_elec_F_Yh <- L123.in_EJ_R_elec_F_Yh
L123.eff_R_elec_F_Yh[ X_historical_years ] <- L123.out_EJ_R_elec_F_Yh[
      match( vecpaste( L123.eff_R_elec_F_Yh[ R_F ] ), vecpaste( L123.out_EJ_R_elec_F_Yh[ R_F ] ) ), X_historical_years ] /
      L123.in_EJ_R_elec_F_Yh[ X_historical_years ]

#Re-set NaN (no input or output), 0 (input but no output), and "Inf" (output but no input) to default electric efficiency
##Only the change to the 0 efficiencies will have any impacts
L123.eff_R_elec_F_Yh[ is.na( L123.eff_R_elec_F_Yh ) ] <- default_electric_efficiency
L123.eff_R_elec_F_Yh[ L123.eff_R_elec_F_Yh == 0 ] <- default_electric_efficiency
L123.eff_R_elec_F_Yh[ L123.eff_R_elec_F_Yh == Inf ] <- default_electric_efficiency

#Re-calculate the output to take into account these changes in selected efficiencies
L123.out_EJ_R_elec_F_Yh[ vecpaste( L123.out_EJ_R_elec_F_Yh[ R_F ] ) %in% vecpaste( L123.eff_R_elec_F_Yh[ R_F ] ), X_historical_years ] <- 
      L123.eff_R_elec_F_Yh[ X_historical_years ] * L123.in_EJ_R_elec_F_Yh[ X_historical_years ]

# 2b. CHP
printlog( "Electricity output (CHP only): aggregating intermediate fuels as specified in ", file_fqn( "ENERGY_MAPPINGS", "enduse_fuel_aggregation" ) )
L123.out_EJ_R_indchp_F_Yh <- subset( L1011.en_bal_EJ_R_Si_Fi_Yh, sector == "out_chp_elec" )
L123.out_EJ_R_indchp_F_Yh$sector <- sub( "out_", "", L123.out_EJ_R_indchp_F_Yh$sector )
L123.out_EJ_R_indchp_F_Yh$fuel <- enduse_fuel_aggregation$industry[ match( L123.out_EJ_R_indchp_F_Yh$fuel, enduse_fuel_aggregation$fuel ) ]
L123.out_EJ_R_indchp_F_Yh$fuel[ L123.out_EJ_R_indchp_F_Yh$fuel %!in% enduse_fuel_aggregation$electricity ] <- NA
L123.out_EJ_R_indchp_F_Yh <- aggregate( L123.out_EJ_R_indchp_F_Yh[ X_historical_years ], by=as.list( L123.out_EJ_R_indchp_F_Yh[ R_S_F ] ), sum )

printlog( "Fuel inputs to CHP systems: calculated from electricity output using exogenous elec/fuel ratios" )
L123.in_EJ_R_indchp_F_Yh <- L123.out_EJ_R_indchp_F_Yh
L123.in_EJ_R_indchp_F_Yh[ X_historical_years ] <- L123.out_EJ_R_indchp_F_Yh[ X_historical_years ] / A23.chp_elecratio$elec_ratio[
      match( L123.in_EJ_R_indchp_F_Yh$fuel, A23.chp_elecratio$fuel ) ]

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L123.out_EJ_R_elec_F_Yh <- c( "Outputs of electricity sector by GCAM region / fuel / historical year","Unit = EJ" )
comments.L123.in_EJ_R_elec_F_Yh <- c( "Inputs to electricity sector by GCAM region / fuel / historical year","Unit = EJ" )
comments.L123.eff_R_elec_F_Yh <- c( "Electric sector efficiencies by GCAM region / fuel / historical year","Unitless IO" )
comments.L123.out_EJ_R_indchp_F_Yh <- c( "Industrial CHP electricity generation by GCAM region / fuel / historical year","Unit = EJ" )
comments.L123.in_EJ_R_indchp_F_Yh <- c( "Inputs to industrial CHP by GCAM region / fuel / historical year","Unit = EJ" )

#write tables as CSV files
writedata( L123.out_EJ_R_elec_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L123.out_EJ_R_elec_F_Yh", comments=comments.L123.out_EJ_R_elec_F_Yh )
writedata( L123.in_EJ_R_elec_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L123.in_EJ_R_elec_F_Yh", comments=comments.L123.in_EJ_R_elec_F_Yh )
writedata( L123.eff_R_elec_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L123.eff_R_elec_F_Yh", comments=comments.L123.eff_R_elec_F_Yh )
writedata( L123.out_EJ_R_indchp_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L123.out_EJ_R_indchp_F_Yh", comments=comments.L123.out_EJ_R_indchp_F_Yh )
writedata( L123.in_EJ_R_indchp_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L123.in_EJ_R_indchp_F_Yh", comments=comments.L123.in_EJ_R_indchp_F_Yh )

# Every script should finish with this line
logstop()
